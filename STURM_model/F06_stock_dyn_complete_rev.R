# Description: Stock dynamics for the STURM model
# Calculation of stock dynamics for one iteration

library(dplyr)
library(tidyverse)
library(tibble)
library(tidyr)


fun_stock_turnover_dyn <- function(i, yrs, bld_cases_fuel, ct_bld_age,
                                   stock_aggr, bld_det_ini, prob_dem,
                                   rate_dem_target = NULL,
                                   path_out = NULL) {

  stp <- yrs[i] - yrs[i - 1]

  # Vintage category for the current year...
  #   to remove categories belonging to different periods.
  ct_bld_age_i <- ct_bld_age %>%
    filter(yrs[i] >= year_i & yrs[i] <= year_f) %>%
    select(mat, bld_age_id) %>%
    rename(bld_age = bld_age_id)

  # Stock Dynamics
  print(paste("Number of existing buildings during the previous time step is",
    round(sum(bld_det_ini$n_units_fuel) / 1e6, 0), "million units."))
  print(paste("Number of existing buildings for this step is",
    round(sum(filter(stock_aggr, year == yrs[i])$n_units_aggr) / 1e6, 0),
    "million units."))

  # 1. Calculate number of demolitions by detailed segment.
  bld_det_i <- bld_det_ini %>%
    # Stock from previous year
    filter(year == yrs[i - 1]) %>%
    select(-year) %>%
    left_join(prob_dem %>%
      pivot_wider(names_from = "parameter", values_from = "prob_dem")) %>%
    # CDF: difference between two consecutive time steps
    mutate(pdem = pweibull(yrs[i] - yr_con, shape = shape, scale = scale) -
      pweibull(yrs[i - 1] - yr_con, shape = shape, scale = scale)) %>%
    # Removing renovated buildings from demolitions
    mutate(pdem = ifelse(eneff %in% c("adv", "std"), 0, pdem)) %>%
    mutate(n_dem = ifelse(n_units_fuel > 0,
      round(pdem * n_units_fuel, rnd), 0)) %>%
    rename(n_units_fuel_p = n_units_fuel) %>%
    select(-c(shape, scale, pdem))
  
  # If target demolition rate is provided, calculate correction factor
  if (!is.null(rate_dem_target)) {
    correction_factor <- bld_det_i %>%
      group_by_at(("region_bld")) %>%
      summarize(n_dem = sum(n_dem),
                n_units = sum(n_units_fuel_p)) %>%
      ungroup() %>%
      mutate(rate_dem = n_dem / n_units / 5) %>%
      mutate(rate_dem_target = rate_dem_target) %>%
      mutate(correction_factor = rate_dem_target / rate_dem) %>%
      select(c("region_bld", "correction_factor"))

    bld_det_i <- bld_det_i %>%
      left_join(correction_factor) %>%
      mutate(n_dem = n_dem * correction_factor) %>%
      select(-c(correction_factor))

      if (!is.null(path_out) && FALSE) {
      write.csv(correction_factor,
        paste(path_out, "calibration_demolition.csv", sep = ""))
    }
  }

  print(paste("Number of demolitions is",
    round(sum(bld_det_i$n_dem) / 1e6, 0), "million units.",
    "i.e. ", round(sum(bld_det_i$n_dem) /
      sum(bld_det_ini$n_units_fuel) * 100, 1), "% of the existing stock."))

  # 2. Calculate new constructions or abandoned buildings by aggregated segment.
  bld_aggr_i <- stock_aggr %>%
    filter(year == yrs[i]) %>%
    # Aggregate demolitions by aggregated segment.
    left_join(bld_det_i %>%
        group_by_at(intersect(names(bld_det_i), names(stock_aggr))) %>%
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
  
  # Check empty buildings in rural VS new buildings in urban -
  bld_aggr_i_check <- bld_aggr_i %>%
    group_by(region_bld, clim, arch, year) %>%
    summarise(n_new = sum(n_new),
              n_empty = sum(n_empty)) %>%
    mutate(diff = ifelse(n_empty>0 & n_new>0, min(n_new, n_empty),0)) %>%
    ungroup %>%
    select(-n_new,-n_empty)
    
  # Recalculate new construction and empty buildings based on assumed transition between rural and urban
  bld_aggr_i <- bld_aggr_i %>%
    left_join(bld_aggr_i_check) %>%
    mutate(n_empty = ifelse(n_empty > 0 & n_empty >= diff, n_empty - diff, n_empty)) %>%
    mutate(n_new = ifelse(n_new > 0 & diff > 0 & n_new >= diff, n_new - diff, n_new)) %>%
    mutate(n_units_aggr = ifelse(n_new > 0 & diff > 0 & n_new >= diff, n_units_aggr + diff, n_units_aggr)) %>%
    select(-diff)
  
  #rm(bld_aggr_i_check)
  
  print(paste("Number of constructions is",
    round(sum(bld_aggr_i$n_new) / 1e6, 0), "million units.",
    "i.e. ", round(sum(bld_aggr_i$n_new) /
      sum(bld_det_ini$n_units_fuel) * 100, 1), "% of the existing stock."))

  print(paste("Number of empty buildings is",
    round(sum(bld_aggr_i$n_empty) / 1e6, 0), "million units.",
    "i.e. ", round(sum(bld_aggr_i$n_empty) /
      sum(bld_det_ini$n_units_fuel) * 100, 1), "% of the existing stock."))

  # 3. Distribute abandoned and demolished buildings across vintage cohorts and archetypes
  bld_det_i <- bld_det_i %>%
    group_by_at(
        setdiff(intersect(names(bld_det_i), names(bld_aggr_i)),
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
    select(-c(n_units_fuel_upd_tot, shr_aggr_age)) %>%
    add_column(year = yrs[i], .before = "yr_con") %>%
    # Update results existing buildings (before renovation)
    mutate(n_units_fuel_exst = n_units_fuel_p - n_dem - n_empty)

  # Test: number of empty buildings
  if (round(sum(bld_det_i$n_empty), 0) !=
    round(sum(bld_aggr_i$n_empty), 0)) {
    print(paste("Number of empty buildings is not consistent.",
      "Error is:",
      round((sum(bld_det_i$n_empty) - sum(bld_aggr_i$n_empty)) / 1e6, 0),
      "million units."))
    stop('Test failed.')
  } else {
    print("Test passed. Empty buildings.")
  }

  report <- bld_det_i %>%
    group_by_at("region_bld") %>%
    summarise(n_dem = sum(n_dem),
      n_empty = sum(n_empty)) %>%
    ungroup()
  
  bld_det_i <- bld_det_i %>%
    select(-c(n_units_fuel_p, n_dem, n_empty))

  bld_aggr_i <- bld_aggr_i %>%
    select(-c(var_aggr, n_dem, n_empty)) %>%
    add_column(yr_con = yrs[i], .after = "year") %>%
    left_join(ct_bld_age_i)

  output <- list(
    bld_aggr_i = bld_aggr_i,
    bld_det_i = bld_det_i,
    report = report
  )
  return(output)
}


fun_stock_construction_dyn <- function(bld_aggr_i, ms_new_i, ct_fuel) {

  # Disaggregate results based on market shares
  new_det_i <- bld_aggr_i %>%
    select(-c(n_units_aggr)) %>%
    filter(mat != "sub") %>%
    # Join shares of eneff in new construction
    left_join(ms_new_i) %>%
    mutate(n_new = round(n_new * ms, rnd)) %>%
    filter(n_new > 0) %>%
    relocate(n_units_fuel = n_new, .after = last_col()) %>%
    select(-c(ms))


  if ("sub" %in% bld_aggr_i$mat) {
    # Slum buildings
    new_det_slum_age_i <- bld_aggr_i %>%
      filter(mat == "sub") %>%
      select(-c(n_units_aggr)) %>%
      mutate(
        arch = "inf",
        eneff = "ns",
        fuel_heat = "electricity",
        fuel_cool = "electricity"
      ) %>%
      filter(n_new > 0) %>%
      mutate(n_units_fuel = round(n_new, rnd), .after = last_col()) %>%
      select(-n_new)
  } else {
    new_det_slum_age_i <- data.frame(NULL)
  }

  # Test for new construction - total
  if (round(sum(new_det_i$n_units_fuel) / 1e6 +
      sum(new_det_slum_age_i$n_units_fuel) / 1e6, 0) !=
      round(sum(bld_aggr_i$n_new) / 1e6, 0)) {
    print(paste("Test failed. New construction buildings.",
      "Error is:",
      round((sum(new_det_i$n_units_fuel) +
        sum(new_det_slum_age_i$n_units_fuel) -
        sum(bld_aggr_i$n_new)), 0),
      "units.")
    )
  } else {
    print("Test passed. New construction buildings.")
  }

    # Bind all datasets - fuel level + vintage
  new_det_i <- bind_rows(new_det_i, new_det_slum_age_i) %>%
    # Remove negative values (due to approximation)
    mutate(n_units_fuel = ifelse(n_units_fuel < 0, 0, n_units_fuel)) %>%
    # Aggregate to remove doubled categories
    group_by_at(setdiff(names(new_det_i), c("n_units_fuel"))) %>%
    summarise(n_units_fuel = sum(n_units_fuel)) %>%
    ungroup()

  return(new_det_i)

}

#' @title Renovation dynamics
#' @description Renovation dynamics
#' @param bld_det_i Data frame with building stock
#' @param rate_ren_i Data frame with renovation rates
#' @param ms_ren_i Data frame with market shares of renovation
#' @param stp Number of years per time step
fun_stock_renovation_dyn <- function(bld_det_i,
                                     rate_ren_i, ms_ren_i, stp) {

  temp <- sum(bld_det_i$n_units_fuel_exst)

  # Existing buildings - renovated
  ren_det_i <- bld_det_i  %>%
    # Account for renovations
    left_join(rate_ren_i, relationship = "many-to-many") %>%
    mutate(rate_ren = ifelse(is.na(rate_ren), 0, rate_ren)) %>%
    left_join(ms_ren_i %>%
      rename(eneff = eneff_i, fuel_heat = fuel_heat_i),
      relationship = "many-to-many") %>%
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
      n_units_fuel_exst, rate_ren, ms_ren
    ))

  # Test market-share agg
  ms_agg <- ren_det_i %>%
    group_by_at(c("region_bld", "eneff_f")) %>%
    summarise(n_units_fuel = sum(n_units_fuel) / stp) %>%
    ungroup() %>%
    left_join(bld_det_i %>%
      group_by_at(c("region_bld")) %>%
      summarise(n_units_fuel_exst = sum(n_units_fuel_exst))) %>%
    mutate(rate = n_units_fuel / n_units_fuel_exst)

  print(
    paste("Renovated buildings:",
      round(sum(ren_det_i$n_units_fuel) / 1e6, 0),
      "million units in", stp, "years.",
      "i.e.",
      round(sum(ren_det_i$n_units_fuel) /
        sum(bld_det_i$n_units_fuel_exst), 2) * 100 / stp,
      "percent per year."))

  # Existing buildings - non-renovated
  woren_det_i <- bld_det_i %>%
    left_join(ren_det_i %>%
      # Select all variables, except the ones indicated, for grouping
      group_by_at(setdiff(
        names(ren_det_i),
        c("eneff_f", "fuel_heat_f", "n_units_fuel",
          "sub_ren_hh", "cost_invest_hh"))) %>%
      summarise(n_ren = sum(n_units_fuel)) %>%
      ungroup()) %>%
    mutate(n_units_fuel = n_units_fuel_exst - n_ren) %>%
    select(-c(n_ren, n_units_fuel_exst))

  # Format renovated buildings by renaming columns
  ren_det_i <- ren_det_i %>%
    filter(n_units_fuel != 0) %>%
    select(-c(eneff, fuel_heat)) %>%
    rename(eneff = eneff_f, fuel_heat = fuel_heat_f)

  # Test for existing buildings
  if (round((sum(woren_det_i$n_units_fuel) +
      sum(ren_det_i$n_units_fuel)) / 1e6, 0) !=
      round(temp / 1e6, 0)) {
    print("Test failed. Existing buildings.")
  } else {
    print("Test passed. Existing buildings.")
  }

  # Bind all datasets - fuel level + vintage
  bld_det_i <- bind_rows(woren_det_i,
      ren_det_i %>% select(-c("sub_ren_hh", "cost_invest_hh"))) %>%
    # Remove negative values (due to approximation)
    mutate(n_units_fuel = ifelse(n_units_fuel < 0, 0, n_units_fuel)) %>%
    # Aggregate to remove doubled categories
    group_by_at(setdiff(names(bld_det_i), c("n_units_fuel_exst"))) %>%
    summarise(n_units_fuel = sum(n_units_fuel)) %>%
    ungroup()

  output <- list(
    bld_det_i = bld_det_i,
    ren_det_i = ren_det_i
  )

  return(output)

}


fun_stock_switch_fuel_dyn <- function(bld_det_i,
                                      rate_switch_fuel_heat,
                                      ms_sw_i,
                                      ct_fuel,
                                      stp,
                                      year_run) {
  # Existing buildings - non-renovated - fuel switch
  temp <- round(sum(bld_det_i$n_units_fuel) / 1e6, 0)

  bld_det_i_sw <- bld_det_i %>%
    # left_join(bld_fuel_switch) %>%
    left_join(rate_switch_fuel_heat) %>%
    left_join(ms_sw_i) %>%
    # Fuel switches only over the minimum age of buildings
    mutate(rate_switch_fuel_heat =
      ifelse(year - yr_con > bld_age_min, rate_switch_fuel_heat * stp, 0)) %>%
    mutate(rate_switch_fuel_heat =
      ifelse(yr_con == year_run, 1, rate_switch_fuel_heat)) %>%
    # Calculate n.units - after renovation
    mutate(n_units_fuel =
      round(n_units_fuel * rate_switch_fuel_heat * ms, rnd)) %>%
    # No fuel switches with NAs
    filter(n_units_fuel > 0) %>%
    # Calculate renovation rate - mat level
    select(-c(bld_age_min, rate_switch_fuel_heat, ms))


  # Update Existing buildings - non-renovated - without fuel switch
  bld_det_i <- bld_det_i %>%
    left_join(bld_det_i_sw %>%
      group_by_at(setdiff(names(bld_det_i_sw),
        c("fuel_heat_f", "n_units_fuel", "cost_invest_heat", "sub_heat_hh"))) %>%
    summarise(n_sw = sum(n_units_fuel)) %>%
    ungroup()) %>%
    mutate(n_sw = ifelse(is.na(n_sw), 0, n_sw)) %>%
    mutate(n_units_fuel = n_units_fuel - n_sw) %>%
    filter(n_units_fuel > 1e-1) %>%
    select(-c(n_sw))

  # Format DF non-renovated buildings - fuel switches
  bld_det_i_sw <- bld_det_i_sw %>%
    select(-c("fuel_heat", "fuel", "fuel_cool")) %>%
    rename(fuel_heat = fuel_heat_f) %>%
    left_join(ct_fuel)

  if (temp != round((sum(bld_det_i$n_units_fuel) +
    sum(bld_det_i_sw$n_units_fuel)) / 1e6, 0)) {
    stop("Test failed. Error during switching fuel.")
   } else {
    print(paste("Test passed.", "Switch",
      round(sum(bld_det_i_sw$n_units_fuel) / 1e6, 0),
      "million units. i.e.", round(sum(bld_det_i_sw$n_units_fuel) /
        (temp * 1e6) * 100, 1), "% of the existing stock."))
   }

    # Bind all datasets - fuel level + vintage
  bld_det_i <- bind_rows(bld_det_i,
    bld_det_i_sw %>% select(-c("cost_invest_heat", "sub_heat_hh"))) %>%
    # Remove negative values (due to approximation)
    mutate(n_units_fuel = ifelse(n_units_fuel < 0, 0, n_units_fuel)) %>%
    # Aggregate to remove doubled categories
    group_by_at(setdiff(names(bld_det_i), c("n_units_fuel"))) %>%
    summarise(n_units_fuel = sum(n_units_fuel)) %>%
    ungroup()

  output <- list(
    bld_det_i = bld_det_i,
    bld_det_i_sw = bld_det_i_sw
  )

  return(output)

}