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
    mutate(diff = ifelse(n_empty > 0 & n_new > 0, min(n_new, n_empty), 0)) %>%
    ungroup() %>%
    select(-n_new, -n_empty)
    
  # Recalculate new construction and empty buildings based on assumed transition between rural and urban
  bld_aggr_i <- bld_aggr_i %>%
    left_join(bld_aggr_i_check) %>%
    mutate(n_empty = ifelse(n_empty > 0 & n_empty >= diff,
      n_empty - diff, n_empty)) %>%
    mutate(n_new = ifelse(n_new > 0 & diff > 0 & n_new >= diff,
      n_new - diff, n_new)) %>%
    mutate(n_units_aggr = ifelse(n_new > 0 & diff > 0 & n_new >= diff,
      n_units_aggr + diff, n_units_aggr)) %>%
    select(-diff)
  # LV: I think n_units_aggr should not be changed here
  
  # rm(bld_aggr_i_check)
  
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
  demolition_heater <- bld_det_i %>%
    group_by_at(c("region_bld", "fuel_heat")) %>%
    summarize(n_dem = sum(n_dem) + sum(n_empty)) %>%
    ungroup()

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
    demolition_heater = demolition_heater,
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
                                     rate_ren_i,
                                     ms_ren_i,
                                     stp,
                                     anticipate = NULL,
                                     verbose = TRUE) {


  # If renovation rate is empty
  if (nrow(rate_ren_i) == 0) {

    ren_det_i <- bld_det_i %>%
      mutate(sub_ren_hh = 0, cost_invest_hh = 0, n_units_fuel = 0)
    return(list(
      bld_det_i = bld_det_i,
      ren_det_i = ren_det_i
    ))
  }
  
  temp <- sum(bld_det_i$n_units_fuel_exst)

  # Create dummy variable for premature replacement if NULL
  if (is.null(anticipate)) {
    anticipate <- data.frame(
      region_bld = unique(bld_det_i$region_bld),
      anticipate = 0,
      payback = 0
    )
  } else {
    if ("n_units_fuel_exst" %in% colnames(anticipate)) {
      anticipate <- select(anticipate, -c("n_units_fuel_exst"))
    }
  }
  if ("n_units_fuel_exst" %in% colnames(rate_ren_i)) {
    rate_ren_i <- select(rate_ren_i, -c("n_units_fuel_exst"))
  }
  if ("n_units_fuel_exst" %in% colnames(ms_ren_i)) {
    ms_ren_i <- select(ms_ren_i, -c("n_units_fuel_exst"))
  }

  # Existing buildings - renovated
  ren_det_i <- bld_det_i  %>%
    # Account for renovations
    left_join(rate_ren_i) %>%
    filter(!is.na(rate_ren)) %>%
    # mutate(rate_ren = ifelse(is.na(rate_ren), 0, rate_ren)) %>%
    left_join(ms_ren_i %>%
      rename(eneff = eneff_i, fuel_heat = fuel_heat_i),
      relationship = "many-to-many") %>%
    mutate(
      eneff_f = ifelse(is.na(ms_ren), eneff, eneff_f),
      fuel_heat_f = ifelse(is.na(ms_ren), fuel_heat, fuel_heat_f)
    ) %>%
    mutate(ms_ren = ifelse(is.na(ms_ren), 0, ms_ren)) %>%
    # Calculate number of renovations
    mutate(n_units_renovation =
      round(n_units_fuel_exst * rate_ren * stp * ms_ren, rnd)) %>%
    # Anticipate renovation
    group_by_at(setdiff(names(bld_det_i), c("n_units_fuel_exst"))) %>%
    mutate(tot_renovation = sum(n_units_renovation)) %>%
    ungroup() %>%
    mutate(n_units_fuel_exst = n_units_fuel_exst - tot_renovation) %>%
    left_join(anticipate) %>%
    mutate(anticipate = ifelse(is.na(anticipate), 0, anticipate)) %>%
    mutate(n_units_renovation = ifelse(anticipate == 1,
      n_units_renovation + n_units_fuel_exst, n_units_renovation)) %>%
    select(-c(
      "n_units_fuel_exst", "rate_ren", "ms_ren", "payback", "anticipate",
      "tot_renovation"
    )) %>%
    rename(n_units_fuel = n_units_renovation)

  # Test market-share agg
  if (FALSE) {
    ms_agg <- ren_det_i %>%
      group_by_at(c("region_bld", "eneff_f")) %>%
      summarise(n_units_fuel = sum(n_units_fuel) / stp) %>%
      ungroup() %>%
      left_join(bld_det_i %>%
        group_by_at(c("region_bld")) %>%
        summarise(n_units_fuel_exst = sum(n_units_fuel_exst))) %>%
      mutate(rate = n_units_fuel / n_units_fuel_exst)
  }

  if (verbose) {
    print(
      paste("Number of renovated buildings is",
        round(sum(ren_det_i$n_units_fuel) / 1e6, 0),
        "million units in", stp, "years.",
        "i.e.",
        round(sum(ren_det_i$n_units_fuel) /
          sum(bld_det_i$n_units_fuel_exst), 2) * 100 / stp,
        "percent per year."))
  }

  # Existing non-renovated buildings
  woren_det_i <- bld_det_i %>%
    left_join(ren_det_i %>%
      group_by_at(setdiff(names(bld_det_i),
        c("n_units_fuel", "n_units_fuel_exst"))) %>%
      summarise(n_ren = sum(n_units_fuel)) %>%
      ungroup()) %>%
    mutate(n_ren = ifelse(is.na(n_ren), 0, n_ren)) %>%
    mutate(n_units_fuel = n_units_fuel_exst - n_ren) %>%
    select(-c(n_ren, n_units_fuel_exst))

  # Format renovated buildings by renaming columns
  ren_det_i <- ren_det_i %>%
    filter(n_units_fuel != 0) %>%
    select(-c(eneff, fuel_heat)) %>%
    rename(eneff = eneff_f, fuel_heat = fuel_heat_f)

  # Test for existing buildings
  if (verbose) {
    if (round((sum(woren_det_i$n_units_fuel) +
        sum(ren_det_i$n_units_fuel)) / 1e6, 0) !=
        round(temp / 1e6, 0)) {
      print("Test failed. Renovated buildings.")
    } else {
      print("Test passed. Renovated buildings.")
    }
  }

  # Bind all datasets
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

#' @title Fuel switch dynamics
#' @description Fuel switch dynamics
#' @param bld_det_i Data frame with building stock  
#' @param heater_vintage Data frame with heater vintage
#' @param ms_sw_i Data frame with market shares of fuel switch
#' @param ct_fuel Data frame with fuel costs
#' @param stp Number of years per time step
fun_stock_switch_fuel_dyn <- function(bld_det_i,
                                      heater_vintage,
                                      ms_sw_i,
                                      ct_fuel,
                                      stp,
                                      year_run,
                                      renovation,
                                      mandatory_switch = FALSE,
                                      premature = NULL,
                                      verbose = TRUE) {

  temp <- round(sum(bld_det_i$n_units_fuel) / 1e6, 0)

  renovation <- renovation %>%
    select(-c("sub_ren_hh", "cost_invest_hh")) %>%
    rename(n_renovation = n_units_fuel)

  # Create dummy variable for premature replacement if NULL
  if (is.null(premature)) {
    premature <- data.frame(
      region_bld = unique(bld_det_i$region_bld),
      premature = 0,
      n_units_fuel = 0,
      payback = 0
    )
  }

  heater_vintage <- heater_vintage %>%
    filter(vintage <= year_run) %>%
    group_by(region_bld, fuel_heat, lifetime_heater) %>%
    summarise(n_vintage = sum(n_vintage)) %>%
    ungroup()

  # Calculate fuel switch for existing and new buildings
  bld_det_i_sw <- bld_det_i %>%
    filter(n_units_fuel > 0) %>%
    left_join(heater_vintage) %>%
    group_by(region_bld, fuel_heat) %>%
    mutate(rate_switch_fuel_heat = n_vintage /
      sum(ifelse((yr_con < year_run), n_units_fuel, 0))) %>%
    ungroup() %>%
    left_join(renovation) %>%
    mutate(mandatory = mandatory_switch) %>%
    mutate(rate_switch_fuel_heat =
      ifelse(!is.na(n_renovation) &
            (eneff == "adv") &
            (fuel_heat %in% c("electricity", "gas", "oil", "coal")) &
            (mandatory == TRUE),
            n_renovation / n_units_fuel / stp,
            rate_switch_fuel_heat)) %>%
    select(-c("n_renovation", "mandatory")) %>%
    left_join(ms_sw_i) %>%
    # Construction building
    mutate(rate_switch_fuel_heat =
      ifelse(yr_con == year_run, 1, rate_switch_fuel_heat)) %>%
    # Calculate number of units to switch
    mutate(n_units_replacement =
      round(n_units_fuel * rate_switch_fuel_heat * ms, rnd)) %>%
    # Premature replacement
    group_by_at(setdiff(names(bld_det_i), c("n_units_fuel"))) %>%
    mutate(tot_replacement = sum(n_units_replacement)) %>%
    ungroup() %>%
    mutate(n_units_fuel = n_units_fuel - tot_replacement) %>%
    left_join(premature %>%
      select(-c("n_units_fuel"))) %>%
    mutate(premature = ifelse(is.na(premature), 0, premature)) %>%
    mutate(n_units_replacement = ifelse(premature == 1,
      n_units_replacement + n_units_fuel, n_units_replacement)) %>%
    mutate(n_premature = ifelse(premature == 1,
      n_units_fuel, 0)) %>%
    # Format data
    select(-c("n_vintage", "lifetime_heater", "rate_switch_fuel_heat",
      "ms", "n_units_fuel", "payback", "premature")) %>%
    rename(n_units_fuel = n_units_replacement) %>%
    filter(n_units_fuel > 0)

  premature <- bld_det_i_sw %>%
    group_by_at(c("region_bld", "fuel_heat")) %>%
    summarise(n_premature = sum(n_premature)) %>%
    ungroup() %>%
    filter(n_premature > 0)

  bld_det_i_sw <- bld_det_i_sw %>%
    select(-c("n_premature"))


  # Remove buildings that have replaced their heating system
  bld_det_i <- bld_det_i %>%
    left_join(bld_det_i_sw %>%
      group_by_at(setdiff(names(bld_det_i), c("n_units_fuel"))) %>%
      summarise(n_sw = sum(n_units_fuel)) %>%
      ungroup()) %>%
    mutate(n_sw = ifelse(is.na(n_sw), 0, n_sw)) %>%
    mutate(n_after = n_units_fuel - n_sw) %>%
    select(-c(n_sw, n_units_fuel)) %>%
    rename(n_units_fuel = n_after) %>%
    # Only keeping buildings with fuel
    filter(n_units_fuel > 1e-3)

  # Format buildings with fuel switch by renaming columns
  bld_det_i_sw <- bld_det_i_sw %>%
    select(-c("fuel_heat", "fuel", "fuel_cool")) %>%
    rename(fuel_heat = fuel_heat_f) %>%
    left_join(ct_fuel)

  if (verbose) {
    if (temp != round((sum(bld_det_i$n_units_fuel) +
      sum(bld_det_i_sw$n_units_fuel)) / 1e6, 0)) {
      stop("Test failed. Error during switching fuel.")
    } else {
      print("Test passed. Replacement boilers is consistent.")
      print(paste("Number of heater installation is",
        round(sum(bld_det_i_sw$n_units_fuel) / 1e6, 0),
        "million units in 5 years. i.e.", round(sum(bld_det_i_sw$n_units_fuel) /
          (temp * 1e6) * 100, 1), "% of the existing stock."))
    }
   }


  # Bind all datasets - fuel level + vintage
  bld_det_i <- bind_rows(bld_det_i,
    bld_det_i_sw %>% select(names(bld_det_i))) %>%
    # Aggregate to remove doubled categories
    group_by_at(setdiff(names(bld_det_i), c("n_units_fuel"))) %>%
    summarise(n_units_fuel = sum(n_units_fuel)) %>%
    ungroup()

  output <- list(
    bld_det_i = bld_det_i,
    bld_det_i_sw = bld_det_i_sw,
    premature = premature
  )

  return(output)

}