# Description: Stock dynamics for the STURM model
# Calculation of stock dynamics for one iteration

library(dplyr)
library(tidyverse)
library(tibble)
library(tidyr)

#' @param sector: sector to run, "resid" or "comm"
#' @param yrs: years to run, e.g. c(2015,2020,2025,2030,2035,2040,2045,2050)
#' @param i: index of the year to run
#' @param bld_cases_fuel: building cases for fuel,
#' @param ct_bld_age: building age data
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
fun_stock_dyn <- function(i,
                          yrs,
                          sector,
                          bld_cases_fuel,
                          ct_bld_age,
                          stock_aggr,
                          bld_det_age_i,
                          prob_dem,
                          rate_switch_fuel_heat,
                          ms_new_i,
                          ms_ren_i,
                          rate_ren_i,
                          ms_sw_i,
                          shr_distr_heat = NULL) {


  print(paste("Running stock turnover year:", yrs[i]))

  # what happen for the last year?
  stp <- yrs[i] - yrs[i - 1]

  # Vintage category for the current year...
  #   to remove categories belonging to different periods.
  ct_bld_age_i <- ct_bld_age %>%
    filter(yrs[i] >= year_i & yrs[i] <= year_f) %>%
    select(mat, bld_age_id) %>%
    rename(bld_age = bld_age_id)

  print(stock_aggr %>% group_by_at("year") %>% summarize(total = sum(n_units_aggr)))
  # Stock Dynamics
  print(paste("Number of existing buildings during the previous time step is",
    round(sum(bld_det_age_i$n_units_fuel) / 1e6, 0), "million units."))
  print(paste("Number of existing buildings for this step is",
    round(sum(filter(stock_aggr, year == yrs[i])$n_units_aggr) / 1e6, 0),
    "million units."))

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


  if ("sub" %in% unique(bld_cases_fuel$mat)) {
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
  if (round(sum(new_det_age_i$n_units_fuel) / 1e6 +
      sum(new_det_slum_age_i$n_units_fuel) / 1e6, 0) !=
      round(sum(bld_aggr_i$n_new) / 1e6, 0)) {
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
      ms_ren
    ))

  print(
    paste("Renovated buildings:",
      round(sum(ren_det_age_i$n_units_fuel) / 1e6, 0),
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
      n_units_fuel_exst, bld_age_min, rate_switch_fuel_heat, ms
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
      sum(ren_det_age_i$n_units_fuel)) / 1e6, 0),
    "million units."
  ))
  # Sum disaggregated buildings
  print(paste("Number of existing units (detailed):",
    round(sum(dem_det_age_i$n_units_fuel_p -
      dem_det_age_i$n_dem - dem_det_age_i$n_empty) / 1e6, 0),
      "million units."))
  # Sum aggregated buildings
  print(paste("Existing units (aggregated):",
    round(sum(bld_aggr_i$n_units_aggr - bld_aggr_i$n_new) / 1e6, 0),
    "million units."))


  ### TEST: TOTAL NUMBER OF HOUSING UNITS
  print(paste(
    "Total units (detailed):",
    round(c(
      sum(exst_det_age_i$n_units_fuel) +
      sum(exst_sw_det_age_i$n_units_fuel) +
      sum(ren_det_age_i$n_units_fuel) +
      sum(new_det_age_i$n_units_fuel) +
      sum(new_det_slum_age_i$n_units_fuel)) / 1e6, 0),
    "million units."))
  print(paste("Total units (aggregated)",
    round(sum(filter(stock_aggr, year == yrs[i])$n_units_aggr) / 1e6, 0),
    "million units."))

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

  print(paste("Number of units:",
    round(sum(bld_det_age_i$n_units_fuel) / 1e6, 0),
    "million units."))

  return(bld_det_age_i)
}
