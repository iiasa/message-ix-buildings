## FUTURE STOCK DYNAMICS
## this calculation is to be put within a loop on "i" (time steps)

library(dplyr)
library(tidyverse)
library(tibble)
library(tidyr)

# Rounding (number of decimals)
rnd <- 5

#' @param sector: sector to run, "resid" or "comm"
#' @param yrs: years to run, e.g. c(2015,2020,2025,2030,2035,2040,2045,2050)
#' @param i: index of the year to run
#' @param run: run number
#' @param geo_level: level for analysis, e.g. "region_bld", "region_gea"
#' @param geo_level_aggr: level for aggregation, e.g. "region_bld", "region_gea"
#' @param geo_levels: levels to keep track of, e.g. c("region_bld","region_gea")
#' @param bld_cases_eneff: building cases for energy efficiency,
#' @param bld_cases_fuel: building cases for fuel,
#' @param ct_bld_age: building age data
#' @param ct_fuel_comb: fuel combination data
#' @param hh_size: household size data, used for residential
#' @param floor_cap: floor space data, used for commercial
#' @param stock_aggr: aggregated building stock data
#' @param bld_det_age_i: building age data
#' @param prob_dem: probability of demolition data
#' @param rate_switch_fuel_heat: rate of switching fuel for heating
#' @param ms_new_i: new building stock
#' @param ms_ren_i: renovated building stock
#' @param rate_ren_i: renovation rate
#' @param ms_sw_i: switched building stock
#' @param en_m2_scen_heat: energy demand per m2 for heating
fun_stock_dyn <- function(sector,
                          yrs,
                          i,
                          run,
                          geo_level,
                          geo_level_aggr,
                          geo_levels,
                          bld_cases_fuel,
                          bld_cases_eneff,
                          ct_bld_age,
                          ct_fuel_comb,
                          hh_size,
                          floor_cap,
                          stock_aggr,
                          bld_det_age_i,
                          prob_dem,
                          rate_switch_fuel_heat,
                          ms_new_i,
                          ms_ren_i,
                          rate_ren_i,
                          ms_sw_i,
                          shr_need_heat,
                          en_m2_scen_heat,
                          en_m2_scen_cool,
                          en_hh_hw_scen,
                          en_m2_hw_scen,
                          en_m2_others,
                          mat_int,
                          report_var,
                          report,
                          shr_distr_heat = NULL) {


  print(paste0("Running stock turnover - scenario ", run, " - year ", yrs[i]))

  # what happen for last year?
  stp <- yrs[i] - yrs[i - 1]

  # Vintage category for the current year...
  #   to remove categories belonging to different periods.
  ct_bld_age_i <- ct_bld_age %>%
    filter(yrs[i] >= year_i & yrs[i] <= year_f) %>%
    select(mat, bld_age_id) %>%
    rename(bld_age = bld_age_id)

  # Stock Dynamics
  print(paste("Number of existing buildings during the previous time step is",
    round(sum(bld_det_age_i$n_units_fuel) / 10^6, 0), "million units."))

  # Calculate number of demolitions by detailed segment.
  dem_det_age_i <- bld_det_age_i %>%
    # Stock from previous year
    filter(year == yrs[i - 1]) %>%
    select(-year) %>%
    left_join(prob_dem %>%
      pivot_wider(names_from = "parameter", values_from = "prob_dem")) %>%
    # CDF: difference between two consecutive time steps
    mutate(pdem = pweibull(yrs[i] - yr_con, shape = shape, scale = scale) -
      pweibull(yrs[i - 1] - yr_con, shape = shape, scale = scale)) %>%
    mutate(n_dem = ifelse(n_units_fuel > 0,
      round(pdem * n_units_fuel, rnd), 0)) %>%
    rename(n_units_fuel_p = n_units_fuel) %>%
    select(-c(shape, scale, pdem))

  print(paste("Number of demolitions is",
    round(sum(dem_det_age_i$n_dem) / 10^6, 0), "million units.",
    "i.e. ", round(sum(dem_det_age_i$n_dem) /
      sum(bld_det_age_i$n_units_fuel) * 100, 1), "% of the existing stock."))

  # Calculate new constructions or abandoned buildings by aggregated segment.
  bld_aggr_i <- stock_aggr %>%
    filter(year == yrs[i]) %>%
    # Aggregate demolitions by aggregated segment.
    left_join(dem_det_age_i %>%
        group_by_at(intersect(names(dem_det_age_i), names(stock_aggr))) %>%
        summarise(n_dem = sum(n_dem)) %>%
        ungroup()) %>%
    mutate(n_dem = ifelse(var_aggr == n_units_aggr
      & is.na(n_dem), 0, n_dem)) %>%
    mutate(
      n_new = ifelse(var_aggr + n_dem >= 0, var_aggr + n_dem, 0),
      n_empty = ifelse(var_aggr + n_dem >= 0, 0, -var_aggr - n_dem)
    ) %>%
    mutate(
      n_dem = ifelse(abs(var_aggr) < 1e-9
        & abs(n_units_aggr) < 1e-9, 0, n_dem),
      n_new = ifelse(abs(var_aggr) < 1e-9
        & abs(n_units_aggr) < 1e-9, 0, n_new),
      n_empty = ifelse(abs(var_aggr) < 1e-9
        & abs(n_units_aggr) < 1e-9, 0, n_empty)
    )
  
  print(paste("Number of constructions is",
    round(sum(bld_aggr_i$n_new) / 10^6, 0), "million units.",
    "i.e. ", round(sum(bld_aggr_i$n_new) /
      sum(bld_det_age_i$n_units_fuel) * 100, 1), "% of the existing stock."))

  print(paste("Number of empty buildings is",
    round(sum(bld_aggr_i$n_empty) / 10^6, 0), "million units.",
    "i.e. ", round(sum(bld_aggr_i$n_empty) /
      sum(bld_det_age_i$n_units_fuel) * 100, 1), "% of the existing stock."))


  # Distribute abandoned buildings across vintage cohorts and archetypes
  # Problem: empty units exceeding n.units(previous stes) - n.units demolished
  # First calculate share of buildings by vintage in the previous timestep,
  #  to allocate empty buildings.
  dem_det_age_i <- dem_det_age_i %>%
    # Select all variables, except "eneff" and "n_dem" for grouping
    group_by_at(
        setdiff(intersect(names(dem_det_age_i), names(bld_aggr_i)),
        "n_dem")) %>%
    # Surviving units
    mutate(n_units_fuel_upd_tot = sum(n_units_fuel_p - n_dem)) %>%
    ungroup() %>%
    mutate(shr_aggr_age =
      (n_units_fuel_p - n_dem) / n_units_fuel_upd_tot) %>%
    left_join(bld_aggr_i %>%
      select(-c(year, n_units_aggr, var_aggr, n_dem, n_new))) %>%
    # Update number of empty buildings by vintage cohort
    mutate(n_empty = ifelse(shr_aggr_age > 0 & n_units_fuel_p > 0,
      round(n_empty * shr_aggr_age, rnd), 0)) %>%
    # Correct exceeding empty buildings
    mutate(n_empty = ifelse(n_units_fuel_p - n_dem - n_empty < 0, 0,
      n_empty)) %>%
    select(-c(n_units_fuel_upd_tot, shr_aggr_age))

  # Test: number of empty buildings
  if (round(sum(dem_det_age_i$n_empty), 0) !=
    round(sum(bld_aggr_i$n_empty), 0)) {
    print(paste("Test failed. Number of empty buildings is not consistent.",
      "Error is:",
      round((sum(dem_det_age_i$n_empty) - sum(bld_aggr_i$n_empty)) / 10^6, 0),
      "million units."))
  } else {
    print("Test passed. Empty buildings.")
  }
    
  # New buildings
  if (!is.null(shr_distr_heat)) {
    temp <- sum(ms_new_i$ms)
    ms_new_i <- ms_new_i %>%
      left_join(shr_distr_heat) %>%
      mutate(
        ms = ms * (1 - shr_distr_heat)) %>%
      bind_rows(
        ms_new_i %>%
          left_join(shr_distr_heat) %>%
            mutate(
              fuel_heat = "district_heat",
              ms = ms * shr_distr_heat
            )) %>%
      select(-shr_distr_heat) %>%
      filter(ms > 0)
    # Test if sum of ms_new_i is consistent with temp
    if (round(sum(ms_new_i$ms), 0) != round(temp, 0)) {
      print("Test failed. District heating.")
      }
    }

  # Test: ms_new_i aggregated on bld_aggr_i intersect names should be equal to 1
  test <- ms_new_i %>% 
    group_by_at(intersect(names(bld_aggr_i), names(ms_new_i))) %>%
    summarise(ms = sum(ms)) %>%
    ungroup()
  # All ms value of test should be equal to 1
  if (any(round(test$ms, 2) != 1)) {
    print("Test failed. Market shares.")
  } else {
    print("Test passed. Market shares.")
  }

  # Disaggregate results based on market shares
  new_det_age_i <- bld_aggr_i %>%
    filter(mat != "sub") %>%
    select(-c(n_units_aggr, var_aggr, n_dem, n_empty)) %>%
    add_column(yr_con = yrs[i], .after = "year") %>%
    left_join(ct_bld_age_i) %>%
    # Join shares of eneff and fuel_heat in new construction
    left_join(ms_new_i) %>%
    mutate(n_new = round(n_new * ms, rnd)) %>%
    filter(n_new > 0) %>%
    relocate(n_units_fuel = n_new, .after = last_col()) %>%
    select(-c(ms))


  if ("sub" %in% unique(bld_cases_eneff$mat)) {
    # Slum buildings
    new_det_slum_age_i <-
      bld_aggr_i %>%
      filter(mat == "sub") %>%
      select(-c(n_units_aggr, var_aggr, n_dem, n_empty)) %>%
      add_column(yr_con = yrs[i], .after = "year") %>%
      left_join(ct_bld_age_i) %>%
      # CHANGE THIS!!!!
      mutate(
        arch = "inf",
         eneff = "ns",
        fuel_heat = "electric",
        fuel_cool = "electricity"
      ) %>%
      filter(n_new > 0) %>%
      mutate(n_units_fuel = round(n_new, rnd), .after = last_col()) %>%
      select(-n_new)
  } else {
    new_det_slum_age_i <- data.frame(NULL)
  }

  # Test for new construction - total
  if (round(c(sum(new_det_age_i$n_units_fuel) +
      sum(new_det_slum_age_i$n_units_fuel)), 0) !=
      round(sum(bld_aggr_i$n_new), 0)) {
    print(paste("Test failed. New construction buildings.",
      "Error is:",
      round((sum(new_det_age_i$n_units_fuel) +
        sum(new_det_slum_age_i$n_units_fuel) -
        sum(bld_aggr_i$n_new)), 0),
      "units.")
    )
  } else {
    print("Test passed. New construction buildings.")
  }

  # Existing buildings - renovated
  ren_det_age_i <- dem_det_age_i %>%
    add_column(year = yrs[i], .before = "yr_con") %>%
    # Update results existing buildings (before renovation)
    mutate(n_units_fuel_exst = n_units_fuel_p - n_dem - n_empty) %>%
    # Account for renovations
    left_join(rate_ren_i) %>%
    mutate(rate_ren = ifelse(is.na(rate_ren), 0, rate_ren)) %>%
    left_join(ms_ren_i %>%
      rename(eneff = eneff_i, fuel_heat = fuel_heat_i)) %>%
    mutate(
      mod_decision = ifelse(is.na(ms_ren), 0, mod_decision),
      # take original eneff if is.na(ms_ren)
      eneff_f = ifelse(is.na(ms_ren), eneff, eneff_f),
      fuel_heat_f = ifelse(is.na(ms_ren), fuel_heat, fuel_heat_f)
    ) %>%
    # take original fuel heat if is.na(ms_ren)
    mutate(ms_ren = ifelse(is.na(ms_ren), 0, ms_ren)) %>%
    # calculate n.units - after renovation
    mutate(n_units_fuel =
      round(n_units_fuel_exst * rate_ren * stp * ms_ren, rnd)) %>%
    select(-c(
      n_units_fuel_p, n_dem, n_empty, n_units_fuel_exst, rate_ren,
      mod_decision, ms_ren
    ))
  print(
    paste("Renovated buildings:",
      round(sum(ren_det_age_i$n_units_fuel) / 10^6, 0),
      "million units in", stp, "years.",
      "i.e.",
      round(sum(ren_det_age_i$n_units_fuel) /
        sum(bld_det_age_i$n_units_fuel), 2) * 100 / stp,
      "percent per year."))

  # Existing buildings - non-renovated
  exst_det_age_i <- dem_det_age_i %>%
    left_join(ren_det_age_i %>%
      # Select all variables, except the ones indicated, for grouping
      group_by_at(setdiff(
        names(ren_det_age_i),
        c("eneff_f", "fuel_heat_f", "n_units_fuel"))) %>%
      summarise(n_ren = sum(n_units_fuel)) %>%
      ungroup()) %>%
    mutate(n_units_fuel_exst = n_units_fuel_p - n_dem - n_empty - n_ren) %>%
    select(-c(n_units_fuel_p, n_dem, n_empty, n_ren))

  # Format renovated buildings by renaming columns
  ren_det_age_i <- ren_det_age_i %>%
    filter(n_units_fuel != 0) %>%
    select(-c(eneff, fuel_heat)) %>%
    rename(eneff = eneff_f, fuel_heat = fuel_heat_f)


  # Test for existing buildings
  if (round(c(sum(exst_det_age_i$n_units_fuel_exst) +
      sum(ren_det_age_i$n_units_fuel)), 0) !=
      round(sum(dem_det_age_i$n_units_fuel_p -
      dem_det_age_i$n_dem - dem_det_age_i$n_empty), 0)) {
    print("Test failed. Existing buildings.")
  } else {
    print("Test passed. Existing buildings.")
  }

  # Existing buildings - non-renovated - fuel switch
  exst_sw_det_age_i <- exst_det_age_i %>%
    # left_join(bld_fuel_switch) %>%
    left_join(rate_switch_fuel_heat) %>%
    left_join(ms_sw_i) %>%
    # Fuel switches only over the minimum age of buildings
    mutate(rate_switch_fuel_heat =
      ifelse(year - yr_con > bld_age_min, rate_switch_fuel_heat, 0)) %>%
    # Calculate n.units - after renovation
    mutate(n_units_fuel =
      round(n_units_fuel_exst * (rate_switch_fuel_heat * stp) * ms, rnd)) %>%
    # No fuel switches with NAs
    filter(n_units_fuel > 0) %>%
    # Calculate renovation rate - mat level
    select(-c(
      n_units_fuel_exst, bld_age_min, rate_switch_fuel_heat, mod_decision, ms
    ))


  # Update Existing buildings - non-renovated - without fuel switch
  exst_det_age_i <- exst_det_age_i %>%
    left_join(exst_sw_det_age_i %>%
    # Select all variables, except the ones indicated, for grouping
      group_by_at(setdiff(names(exst_sw_det_age_i),
        c("fuel_heat_f", "n_units_fuel"))) %>%
    summarise(n_sw = sum(n_units_fuel)) %>%
    ungroup()) %>%
    mutate(n_sw = ifelse(is.na(n_sw), 0, n_sw)) %>%
    mutate(n_units_fuel = n_units_fuel_exst - n_sw) %>%
    select(-c(n_units_fuel_exst, n_sw))


  # Format DF non-renovated buildings - fuel switches
  exst_sw_det_age_i <- exst_sw_det_age_i %>%
    select(-fuel_heat) %>%
    rename(fuel_heat = fuel_heat_f)

  # Test for existing buildings
  print(paste(
    "Number of existing units (detailed):",
    round(c(sum(exst_det_age_i$n_units_fuel) +
      sum(exst_sw_det_age_i$n_units_fuel) +
      sum(ren_det_age_i$n_units_fuel)) / 10^6, 0),
    "million units."
  ))
  # Sum disaggregated buildings
  print(paste("Number of existing units (detailed):",
    round(sum(dem_det_age_i$n_units_fuel_p -
      dem_det_age_i$n_dem - dem_det_age_i$n_empty) / 10^6, 0),
      "million units."))
  # Sum aggregated buildings
  print(paste("Existing units (aggregated):",
    round(sum(bld_aggr_i$n_units_aggr - bld_aggr_i$n_new) / 10^6, 0),
    "million units."))


  ### TEST: TOTAL NUMBER OF HOUSING UNITS
  print(paste(
    "Total units (detailed):",
    round(c(sum(new_det_age_i$n_units_fuel) +
      sum(new_det_slum_age_i$n_units_fuel) +
      sum(exst_det_age_i$n_units_fuel) +
      sum(exst_sw_det_age_i$n_units_fuel) +
      sum(ren_det_age_i$n_units_fuel)) / 10^6, 0),
    "million units."
  )) # Sum disaggregated buildings
  print(paste("Total units (aggregated):",
    round(sum(stock_aggr %>%
                filter(year == yrs[i]) %>%
                select(n_units_aggr) %>%
                pull()) / 10^6, 0),
                "million units."))

  # Results

  # Bind all datasets - fuel level + vintage
  bld_det_age_i <- bind_rows(exst_det_age_i,
                            exst_sw_det_age_i,
                            ren_det_age_i,
                            new_det_age_i,
                            new_det_slum_age_i) %>%
    # Remove negative values (due to approximation)
    mutate(n_units_fuel = ifelse(n_units_fuel < 0, 0, n_units_fuel)) %>%
    # Aggregate to remove doubled categories
    group_by_at(setdiff(names(bld_det_age_i), c("n_units_fuel"))) %>%
    summarise(n_units_fuel = sum(n_units_fuel)) %>%
    ungroup()

  # Aggregate at fuel level for keeping track of the stock
  bld_det_i <- bld_det_age_i %>%
    # Select all variables, except the ones indicated, for grouping
    group_by_at(setdiff(names(bld_det_age_i),
      c("yr_con", "n_units_fuel", "n_dem", "n_empty"))) %>%
    summarise(
      n_units_fuel = sum(n_units_fuel)
    ) %>%
    ungroup()


  t <- en_stock_i %>% 

  # Report Energy Demand
  en_stock_i <- bld_cases_fuel %>%
    mutate(scenario = run) %>%
    mutate(year = yrs[i]) %>%
    # Issue matching periods of construction when definition is different!
    left_join(bld_det_i) %>%
    # Add "v_no_heat" category
    select(-c(mod_decision)) %>%
    pivot_wider(
      names_from = "fuel_heat",
      values_from = "n_units_fuel"
    ) %>%
    mutate(v_no_heat = 0) %>%
    pivot_longer(
      cols = c(sort(unique(ct_fuel_comb$fuel_heat)), "v_no_heat"),
      names_to = "fuel_heat",
      values_to = "n_units_fuel"
    ) %>%
    filter(!is.na(n_units_fuel)) %>%
    group_by_at(paste(c(geo_level,
      "urt", "inc_cl", "arch", "year", "clim", "eneff"))) %>%
    # Calculate n_units_eneff to account for buildings with no heating
    mutate(n_units_eneff = sum(n_units_fuel)) %>%
    ungroup()

  # Stock results - Energy
  t <- bld_det_i %>% group_by_at("eneff") %>% summarise(total = sum(n_units_fuel))

  if (sector == "resid") {
    en_stock_i <- en_stock_i %>%
      left_join(shr_need_heat) %>%
      # Rescale number of units based on fuel
      # Heating access not considered here!
      mutate(n_units_fuel = ifelse(fuel_heat == "v_no_heat",
        n_units_eneff * (1 - shr_need_heat),
        n_units_fuel * shr_need_heat
      )) %>%
      left_join(hh_size) %>%
      left_join(floor_cap) %>%
      # left_join(shr_acc_cool) %>%
      left_join(en_m2_scen_heat) %>%
      left_join(en_m2_scen_cool) %>%
      left_join(en_hh_hw_scen) %>%
      # convert n. units to millions
      mutate(floor_Mm2 = n_units_fuel / 1e6 * hh_size * floor_cap) %>%
      mutate(floor_heat_Mm2 = floor_Mm2) %>%
      # mutate(floor_heat_Mm2 = ifelse(acc_heat == 1, floor_Mm2, 0)) %>%
      mutate(floor_cool_Mm2 =
        ifelse(shr_acc_cool == 1, floor_Mm2 * shr_acc_cool, 0)) %>%
      # Converted from kWh to MJ (3.6).
      #  Houssing units are in million, so results are in TJ.
      mutate(heat_TJ = ifelse(fuel_heat == "v_no_heat", 0,
        en_dem_heat * n_units_fuel / 1e6 * hh_size * floor_cap * 3.6)) %>%
      # Converted from kWh to MJ (3.6).
      #  Houssing units are in million, so results are in TJ.
      mutate(cool_TJ = en_dem_cool * shr_acc_cool *
        n_units_fuel / 1e6 * hh_size * floor_cap * 3.6) %>%
      mutate(cool_ac_TJ = en_dem_c_ac * shr_acc_cool *
        n_units_fuel / 1e6 * hh_size * floor_cap * 3.6) %>%
      # Note:shr_acc_cool=1 for all cases (access calculated before)
      # Converted from kWh to MJ (3.6). 
      #   Houssing units are in million, so results are in TJ.
      mutate(cool_fans_TJ = en_dem_c_fans * shr_acc_cool *
        n_units_fuel / 1e6 * hh_size * floor_cap * 3.6) %>% 
      # converted from GJ/hh/yr to TJ
      mutate(hotwater_TJ = ifelse(fuel_heat == "v_no_heat", 0,
        en_dem_dhw * n_units_fuel / 1e3)) %>%
      # Other uses not covered for residential
      mutate(other_uses_TJ = 0) %>%
      mutate(stock_M = n_units_fuel / 1e6) %>%
      filter(stock_M > 0 & !is.na(stock_M)) %>%
      select_at(c(geo_levels, paste(c(
        "urt", "clim", "inc_cl", "arch", "mat",
        "eneff", "fuel_heat", "fuel_cool",
        "scenario", "year", "stock_M", "floor_Mm2",
        "heat_TJ", "cool_TJ", "cool_ac_TJ", "cool_fans_TJ",
        "hotwater_TJ", "other_uses_TJ"
      ))))
  }

  if (sector == "comm") {
    en_stock_i <- en_stock_i %>% # To avoid NAs with non-available combination of eneff - fuel (slums)
      # left_join(en_stock_i) %>%
      left_join(shr_need_heat) %>%
      mutate(n_units_fuel = ifelse(fuel_heat == "v_no_heat", # Rescale number of units based on fuel # Heating access not considered here!!
        n_units_eneff * (1 - shr_need_heat),
        n_units_fuel * shr_need_heat
      )) %>%
      # left_join(hh_size) %>%
      # left_join(floor) %>%
      # left_join(shr_acc_cool) %>%
      left_join(en_m2_scen_heat) %>%
      left_join(en_m2_scen_cool) %>%
      left_join(en_m2_hw_scen) %>%
      left_join(en_m2_others) %>%
      mutate(floor_Mm2 = n_units_fuel / 1e6) %>%
      mutate(floor_heat_Mm2 = floor_Mm2) %>%
      # mutate(floor_heat_Mm2 = ifelse(acc_heat == 1, floor_Mm2, 0)) %>%
      mutate(floor_cool_Mm2 = ifelse(shr_acc_cool == 1, floor_Mm2 * shr_acc_cool, 0)) %>%
      mutate(heat_TJ = ifelse(fuel_heat == "v_no_heat", 0, en_dem_heat * n_units_fuel / 1e6 * 3.6)) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(cool_TJ = en_dem_cool * shr_acc_cool * n_units_fuel / 1e6 * 3.6) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(cool_ac_TJ = en_dem_c_ac * shr_acc_cool * n_units_fuel / 1e6 * 3.6) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(cool_fans_TJ = en_dem_c_fans * shr_acc_cool * n_units_fuel / 1e6 * 3.6) %>% # Note:shr_acc_cool=1 for all cases (access calculated before) #converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      # mutate(hotwater_TJ = ifelse(fuel_heat == "v_no_heat", 0, en_dem_dhw * n_units_fuel / 1e6 * 3.6)) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(hotwater_TJ = ifelse(fuel_heat == "v_no_heat", 0, en_dem_dhw * n_units_fuel / 1e6 * 3.6)) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(other_uses_TJ = en_dem_others * n_units_fuel / 1e6 * 3.6) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(stock_M = n_units_fuel / 1e6) %>%
      filter(stock_M > 0 & !is.na(stock_M)) %>%
      select_at(c(geo_levels, paste(c(
        "urt", "clim", "inc_cl", "arch", "mat", "eneff", "fuel_heat", "fuel_cool",
        "scenario", # "ssp",
        "year", "stock_M", "floor_Mm2",
        # "floor_tot_heat_Mm2", "floor_tot_cool_Mm2",
        "heat_TJ", "cool_TJ", "cool_ac_TJ", "cool_fans_TJ", "hotwater_TJ", "other_uses_TJ"
      ))))
  }

  # Stock results - Material

  # Aggregate results at eneff level
  # demolitions
  dem_eneff_i <- dem_det_age_i %>%
    group_by_at(setdiff(names(dem_det_age_i), c(
      "bld_age", "fuel_heat", "fuel_cool", "n_units_fuel_p", "n_dem", "n_empty"
    ))) %>%
    summarise(
      n_dem = sum(n_dem)
    ) %>%
    ungroup()

  # new constructions
  new_eneff_i <- bind_rows(new_det_age_i, new_det_slum_age_i) %>%
    group_by_at(setdiff(names(new_det_age_i), c(
      "bld_age", "mod_decision",
      "fuel_heat", "fuel_cool", "n_units_fuel"
    ))) %>%
    summarise(n_new = sum(n_units_fuel)) %>%
    ungroup()

  # stock
  bld_eneff_i <- bld_det_age_i %>%
    group_by_at(setdiff(names(bld_det_age_i), c(
      "bld_age", "fuel_heat", "fuel_cool", "n_units_fuel"
    ))) %>%
    summarise(n_units = sum(n_units_fuel)) %>%
    ungroup()


  if ("material" %in% report_var) {
    if (sector == "resid") {
      # Calculate material stock
      mat_stock_i <- bld_cases_eneff %>%
        mutate(scenario = run) %>%
        # mutate(ssp = ssp_r) %>%
        mutate(year = yrs[i]) %>%
        left_join(hh_size) %>%
        left_join(floor_cap) %>%
        left_join(bld_eneff_i) %>%
        left_join(dem_eneff_i) %>%
        left_join(new_eneff_i) %>%
        mutate(
          n_units = ifelse(is.na(n_units), 0, n_units),
          n_new = ifelse(is.na(n_new), 0, n_new),
          n_dem = ifelse(is.na(n_dem), 0, n_dem)
        ) %>%
        filter(n_units + n_new + n_dem != 0) %>%
        filter(mat != "sub") %>% # Exclude slums (no material intensity data)
        left_join(mat_int) %>%
        # filter(arch != "inf") %>% # Materials not calculated for slums
        mutate(floor_tot_Mm2 = n_units * hh_size * floor_cap / 1e6) %>% # Mm2
        mutate(floor_new_Mm2 = n_new * hh_size * floor_cap / stp / 1e6) %>% # Mm2/yr
        mutate(floor_dem_Mm2 = n_dem * hh_size * floor_cap / stp / 1e6) %>% # Mm2/yr
        mutate(mat_stock_Mt = n_units * hh_size * floor_cap * mat_int / 1e3 / 1e6) %>% # Mt/y
        mutate(mat_demand_Mt = n_new * hh_size * floor_cap * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
        mutate(mat_scrap_Mt = n_dem * hh_size * floor_cap * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
        # select_at(c(geo_levels, paste(c("urt", "clim", "inc_cl", "arch", "mat", "eneff", "material", ##### CHECK yr_con
        #                                 "scenario", "ssp", "year",
        #                                 "floor_tot_Mm2",
        #                                 "floor_new_Mm2", "floor_dem_Mm2",
        #                                 "mat_int",
        #                                 "mat_stock_Mt",
        #                                 "mat_demand_Mt", "mat_scrap_Mt")))) %>%
        group_by_at(c(
          geo_levels,
          paste(c(
            "urt", "clim", "inc_cl", "arch", "mat", "eneff", "material", "scenario", # "ssp",
            "year"
          ))
        )) %>% # Drop yr_con dimension

        summarise(
          floor_tot_Mm2 = sum(floor_tot_Mm2),
          floor_new_Mm2 = sum(floor_new_Mm2),
          floor_dem_Mm2 = sum(floor_dem_Mm2),
          # mat_int = weighted.mean(mat_int, floor_tot_Mm2),
          mat_stock_Mt = sum(mat_stock_Mt),
          mat_demand_Mt = sum(mat_demand_Mt),
          mat_scrap_Mt = sum(mat_scrap_Mt)
        ) %>%
        ungroup() %>%
        mutate(mat_int = 1e3 * mat_stock_Mt / floor_tot_Mm2) # Recalculate average material intensity
    } else {
      mat_stock_i <- bld_cases_eneff %>%
        mutate(scenario = run) %>%
        # mutate(ssp = ssp_r) %>%
        mutate(year = yrs[i]) %>%
        # left_join(floor_cap) %>%
        left_join(bld_eneff_i) %>%
        left_join(dem_eneff_i) %>%
        left_join(new_eneff_i) %>%
        mutate(
          n_units = ifelse(is.na(n_units), 0, n_units),
          n_new = ifelse(is.na(n_new), 0, n_new),
          n_dem = ifelse(is.na(n_dem), 0, n_dem)
        ) %>%
        filter(n_units + n_new + n_dem != 0) %>%
        left_join(mat_int) %>%
        # filter(arch != "inf") %>% # Materials not calculated for slums
        mutate(floor_tot_Mm2 = n_units / 1e6) %>% # Mm2
        mutate(floor_new_Mm2 = n_new / stp / 1e6) %>% # Mm2/yr
        mutate(floor_dem_Mm2 = n_dem / stp / 1e6) %>% # Mm2/yr
        mutate(mat_stock_Mt = n_units * mat_int / 1e3 / 1e6) %>% # Mt
        mutate(mat_demand_Mt = n_new * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
        mutate(mat_scrap_Mt = n_dem * mat_int / stp / 1e3 / 1e6) %>% # Mt/y
        # select_at(c(geo_levels, paste(c("urt", "clim", "inc_cl", "arch", "mat", "eneff", "material",
        #                                 "scenario", "ssp", "year",
        #                                 "floor_tot_Mm2",
        #                                 "floor_new_Mm2", "floor_dem_Mm2",
        #                                 "mat_int",
        #                                 "mat_stock_Mt",
        #                                 "mat_demand_Mt", "mat_scrap_Mt"))))
        group_by_at(c(
          geo_levels,
          paste(c(
            "urt", "clim", "inc_cl", "arch", "mat", "eneff", "material", "scenario", # "ssp",
            "year"
          ))
        )) %>% # Drop yr_con dimension
        summarise(
          floor_tot_Mm2 = sum(floor_tot_Mm2),
          floor_new_Mm2 = sum(floor_new_Mm2),
          floor_dem_Mm2 = sum(floor_dem_Mm2),
          # mat_int = weighted.mean(mat_int, floor_tot_Mm2),
          mat_stock_Mt = sum(mat_stock_Mt),
          mat_demand_Mt = sum(mat_demand_Mt),
          mat_scrap_Mt = sum(mat_scrap_Mt)
        ) %>%
        ungroup() %>%
        mutate(mat_int = 1e3 * mat_stock_Mt / floor_tot_Mm2) # Recalculate average material intensity
    }

    ## Stock results - Material - Add Cement

    cement_content <- 0.15 ## Cement content in concrete

    mat_stock_cem_i <- mat_stock_i %>%
      filter(material == "concrete") %>%
      mutate(material = "cement") %>%
      mutate(
        mat_stock_Mt = mat_stock_Mt * cement_content,
        mat_demand_Mt = mat_demand_Mt * cement_content,
        mat_scrap_Mt = mat_scrap_Mt * cement_content
      )

    mat_stock_i <- rbind(mat_stock_i, mat_stock_cem_i)
  }

  # Report stock by eneff - vintage
  if ("vintage" %in% report_var) {
    # Stock by eneff and vintage (for reporting)
    report$bld_eneff_age <- bind_rows(
      report$bld_eneff_age,
      bld_det_age_i %>%
        group_by_at(setdiff(names(bld_det_age_i), c( # "arch",
          "fuel_heat", "fuel_cool", "n_units_fuel"
        ))) %>%
        summarise(n_units_eneff = sum(n_units_fuel)) %>%
        ungroup()
    )
  }

  # Update energy demand/material results (for reporting)
  if ("energy" %in% report_var) {
    report$en_stock <- bind_rows(report$en_stock, en_stock_i)
  }

  if ("material" %in% report_var) {
    report$mat_stock <- bind_rows(report$mat_stock, mat_stock_i)
  }

  output <- list(
    report = report,
    stock_aggr = stock_aggr,
    bld_det_age_i = bld_det_age_i
  )

  return(output)
}
