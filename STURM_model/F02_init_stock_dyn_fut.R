# Description: Initialize building stock for future scenarios

library(dplyr)

#' @param sector: sector to run, "resid" or "comm"
#' @param yrs: years to run, e.g. c(2015,2020,2025,2030,2035,2040,2045,2050)
#' @param geo_data: data with geographical information
#' @param bld_cases_eneff: building cases for energy efficiency,
#' @param pop: population data
#' @param hh_size: household size data, used for residential
#' @param floor_cap: floor space data, used for commercial
#' @param ct_eneff: energy efficiency data
#' @param ct_fuel_comb: fuel combination data
#' @param stock_arch_base: base building stock data
#' @param shr_mat: share of materials data
#' @param shr_arch: share of architecture data
#' @param report_var: report variable, e.g. "energy"
#' @return: data frame with results
#' @export fun_stock_init_fut
fun_stock_aggr <- function(sector,
                            yrs,
                            geo_data,
                            bld_cases_fuel,
                            pop,
                            hh_size,
                            floor_cap,
                            ct_eneff,
                            ct_fuel_comb,
                            shr_mat,
                            shr_arch,
                            path_out = NULL) {

  # Total number of residential building units:
  #  population / average household size
  if (sector == "resid") {
    bld_units <- pop %>%
      filter(year %in% yrs) %>%
      left_join(hh_size) %>%
      mutate(bld_units =
        round(1e6 * pop / hh_size, rnd)) %>%
      select(-c(pop, hh_size))
    try(if (nrow(bld_units) != nrow(distinct(bld_units)))
      stop("Error in aggregated households calculations! 
        Duplicated records in hh"))
  }

  # Total number of building units
  #  floor_cap times population
  if (sector == "comm") {
    bld_units <- pop %>%
      filter(year %in% yrs[-1]) %>%
      left_join(floor_cap) %>%
      # Convert from million units to units
      mutate(bld_units = round(1e6 * pop * floor_cap, rnd)) %>%
      select(-c(pop, floor_cap))

    try(if (nrow(bld_units) != nrow(distinct(bld_units)))
      stop("Error in aggregated households calculations!
        Duplicated records in hh"))
  }

  resolution_aggr <- union(names(bld_units %>% select(-bld_units)),
    c("mat", "arch", "region_gea")
  )

  # Stock aggregated: Number of units (Million) - "arch" level
  stock_aggr <- bld_cases_fuel %>%
    select_at(intersect(resolution_aggr, names(bld_cases_fuel))) %>%
    distinct() %>%
    # Years filtered here
    left_join(bld_units, relationship = "many-to-many") %>%
    left_join(shr_mat) %>%
    # Long format
    left_join(shr_arch) %>%
    mutate(n_units_aggr = bld_units * shr_mat * shr_arch) %>%
    select(-c(bld_units, shr_mat, shr_arch))

  temp <- stock_aggr %>%
    group_by_at(c("region_bld", "year")) %>%
    summarize(n = sum(n_units_aggr)) %>%
    ungroup()
  
  p <- pop %>%
    group_by_at(c("region_bld", "year")) %>%
    summarize(pop = sum(pop)) %>%
    ungroup()

  try(if(nrow(stock_aggr) !=
    nrow(distinct(stock_aggr %>% select(-c(n_units_aggr)))))
    stop("Error in aggregated stock dataset! 
      Multiple records for same combinations in stock_aggr"))

  # Add stock variation aggregated
  stock_aggr <- stock_aggr %>%
    mutate(var_aggr = n_units_aggr - lag(n_units_aggr)) %>%
    ungroup()

  try(if(nrow(stock_aggr) != nrow(distinct(stock_aggr %>%
    select(-c(n_units_aggr, var_aggr)))))
    stop("Error in aggregated stock dataset!
      Multiple records for same combinations in stock_aggr"))

  return(stock_aggr)
}


#' @title Initialize building stock
#' @description Initialize building stock for baseyear by adding information
#' @param sector: sector to be analysed, default is "resid"
#' @param stock_arch_base: stock baseyear
#' @param geo_data: geo data
#' @param ct_eneff: energy efficiency catagories
#' @param ct_fuel_comb: fuel combinations catagories
#' @param shr_fuel_heat_base: share of heat fuel in baseyear
#' @param ct_heat: heat catagories
#' @param report_var: variables to be reported,
fun_stock_det_ini <- function(sector,
                           stock_aggr,
                           stock_arch_base,
                           geo_data,
                           ct_eneff,
                           ct_fuel_comb,
                           ct_heat,
                           shr_fuel_heat_base,
                           shr_tenure_status,
                           report_var) {
  
  # Assumption: one intial eneff (the lower) per period of construction
  ct_eneff_ini <- ct_eneff %>%
    group_by(bld_age) %>%
    filter(rank(match(eneff, c("avg", "std", "adv"))) == 1) %>%
    ungroup()

  # Calculate share of additional dimensions in stock_arch_base
  shr_arch_base <- stock_arch_base %>%
    group_by_at(intersect(names(stock_aggr), names(stock_arch_base))) %>%
    mutate(share = stock_arch_base / sum(stock_arch_base)) %>%
    ungroup() %>%
    select(-stock_arch_base)

  bld_det_i <- stock_aggr %>%
    select(-var_aggr) %>%
    filter(year == 2015) %>%
    left_join(shr_arch_base, relationship =
      "many-to-many") %>%
    mutate(n_units_eneff = share * n_units_aggr) %>%
    select(-c(n_units_aggr, share)) %>%
    left_join(ct_eneff_ini)

  # TODO: Mapping fuel shares with building stock
  ct_heat <- filter(ct_heat, ct_heat == 1)

  target_fuel <- bld_det_i %>%
    group_by_at(setdiff(names(shr_fuel_heat_base),
      c("fuel_heat", "shr_fuel_heat_base"))) %>%
    summarise(n_units_eneff = sum(n_units_eneff)) %>%
    ungroup() %>%
    left_join(shr_fuel_heat_base) %>%
    mutate(n_fuels = n_units_eneff * shr_fuel_heat_base) %>%
    select(-c(n_units_eneff, shr_fuel_heat_base))

  shr_fuel_heat_constraint <- bld_det_i %>%
    left_join(ct_heat, relationship = "many-to-many") %>%
    mutate(n_units_eneff = ct_heat * n_units_eneff) %>%
    filter(ct_heat ==  1) %>%
    group_by_at(setdiff(names(target_fuel), c("n_fuels"))) %>%
    summarise(n_units_eneff = sum(n_units_eneff)) %>%
    ungroup() %>%
    left_join(target_fuel) %>%
    mutate(share = n_fuels / n_units_eneff) %>%
    mutate(share = ifelse(share > 1, 1, share)) %>%
    left_join(ct_heat) %>%
    select(-c(n_units_eneff, n_fuels, ct_heat))

  shr_fuel_heat <- shr_fuel_heat_base %>%
    filter(!fuel_heat %in% unique(ct_heat$fuel_heat)) %>%
    group_by_at(setdiff(names(shr_fuel_heat_base),
      c("fuel_heat", "shr_fuel_heat_base"))) %>%
    mutate(shr_fuel_heat_base =
      shr_fuel_heat_base / sum(shr_fuel_heat_base)) %>%
    ungroup()

  bld_det_i <- bld_det_i %>%
    left_join(ct_fuel_comb, relationship = "many-to-many") %>%
    left_join(shr_fuel_heat_constraint) %>%
    mutate(share = ifelse(is.na(share), 0, share)) %>%
    left_join(shr_fuel_heat) %>%
    mutate(shr_fuel_heat_base = ifelse(is.na(shr_fuel_heat_base),
      0, shr_fuel_heat_base)) %>%
    group_by_at(names(bld_det_i)) %>%
    mutate(shr_fuel_heat_base = (1 - sum(share)) * shr_fuel_heat_base) %>%
    ungroup() %>%
    mutate(shr_fuel_heat_base =
      ifelse(share > 0, share, shr_fuel_heat_base)) %>%
    mutate(n_units_fuel =
      round(n_units_eneff * shr_fuel_heat_base, rnd)) %>%
    mutate_cond(mat == "sub", n_units_fuel = n_units_eneff) %>%
    select(-c(share, shr_fuel_heat_base, n_units_eneff)) %>%
    filter(n_units_fuel != 0)

  # Test consistency with target fuels after distribution in the building stock
  test <- bld_det_i %>%
    group_by_at(setdiff(names(target_fuel), c("n_fuels"))) %>%
    summarise(n_units_fuel = sum(n_units_fuel)) %>%
    ungroup() %>%
    left_join(target_fuel)

  if (round(sum(test$n_units_fuel) / 1e6, 0) !=
    round(sum(test$n_fuels) / 1e6, 0)) {
    print("Error:
      Inconsistent fuel shares.")
  }

  if (sum(is.na(bld_det_i$n_units_fuel)) > 0) {
    print("NA value in bld_det_i")
  }

  if (round(sum(bld_det_i$n_units_fuel) / 1e6, 0) !=
      round(sum(filter(stock_aggr, year == 2015)$n_units_aggr) / 1e6, 0)) {
        print("Test failed.
          Problem during detailing builing stock for base year")
      } else {
    print("Test passed. Building stock detailing for base year successful")
  }

  if (!"inc_cl" %in% names(bld_det_i)) {
    # Subdivising equally between three classes of income (q1, q2, q3)
    # Assumption of no correlation with other dimension

    shr_inc <- data.frame(
        mat = "perm",
        inc_cl = c("q1", "q2", "q3"),
        share = 1 / 3)

    temp <- sum(bld_det_i$n_units_fuel)
    bld_det_i <- bld_det_i %>%
      left_join(shr_inc, relationship = "many-to-many") %>%
      mutate(n_units_fuel = n_units_fuel * share) %>%
      select(-share)

    # Test consistency.
    if (round(temp, 0) !=
      round(sum(bld_det_i$n_units_fuel), 0)) {
      print("Error:
        Inconsistent income shares.")
    }

  }

  if (!"tenr" %in% names(bld_det_i)) {

    temp <- sum(bld_det_i$n_units_fuel)
    bld_det_i <- bld_det_i %>%
      left_join(shr_tenure_status, relationship = "many-to-many") %>%
      mutate(n_units_fuel = n_units_fuel * hh_tenure) %>%
      select(-hh_tenure)
    
    # Test consistency.
    if (round(temp / 1e6, 0) !=
      round(sum(bld_det_i$n_units_fuel) / 1e6, 0)) {
      print("Error:
        Inconsistent income shares.")
    }
  }

  print(paste("Number of buildings for base year:",
    round(sum(bld_det_i$n_units_fuel) / 1e6, 0),
    "million units."))

  print(paste("Resolution of detailed building stock:",
    paste(names(bld_det_i), collapse = ", ")))

  report <- list()
  if ("energy" %in% report_var) {
    report <- append(report, list(en_stock = as.data.frame(NULL)))
    report <- append(report, list(agg_result =
      data.frame(
        region_bld = character(),
        year = numeric(),
        variable = character(),
        resolution = character(),
        value = numeric())))

  }
  if ("material" %in% report_var) {
    report <- append(report, list(mat_stock = as.data.frame(NULL)))
  }
  if ("vintage" %in% report_var) {
    # Report stock by eneff - vintage
    bld_eneff_age <- bld_det_i %>%
      group_by_at(setdiff(names(bld_det_i),
        c("fuel_heat", "fuel_cool", "n_units_fuel"))) %>%
      summarise(n_units_eneff = sum(n_units_fuel)) %>%
      ungroup()

    report <- append(report, list(bld_eneff_age = bld_eneff_age))
  }

  output <- list(
    bld_det_i = bld_det_i,
    report = report
  )

  return(output)
}
