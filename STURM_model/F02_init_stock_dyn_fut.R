### INITIALIZE BUILDING STOCK - FUTURE SCENARIOS ###

# add switch to select levels for calculation?
library(dplyr)

# rounding (number of decimals)
rnd <- 5



#' @title Initialize building stock
#' @description Initialize building stock for baseyear by adding information
#' @param sector: sector to be analysed, default is "resid"
#' @param stock_arch_base: stock baseyear
#' @param geo_data: geo data
#' @param geo_levels: levels to keep track of,
#' default is c("region_bld","region_gea")
#' @param ct_eneff: energy efficiency catagories
#' @param ct_fuel_comb: fuel combinations catagories
#' @param shr_fuel_heat_base: share of heat fuel in baseyear
#' @param shr_distr_heat: share of district heating
#' @param report_var: variables to be reported,
fun_stock_init <- function(sector,
                           stock_arch_base,
                           geo_data,
                           geo_levels,
                           ct_eneff,
                           ct_fuel_comb,
                           shr_fuel_heat_base,
                           shr_distr_heat,
                           report_var){


  # Initialize DF stock by vintage (baseyear) - detailed fuel level
  bld_det_age_i <- geo_data %>%
    select_at(geo_levels) %>%
    left_join(stock_arch_base) %>% # baseyear results
    left_join(ct_eneff) %>%
    left_join(ct_fuel_comb) %>%
    left_join(shr_fuel_heat_base) %>%
    left_join(shr_distr_heat) %>%
    # Assumption: one eneff per period of construction
    mutate(n_units_eneff = stock_arch_base) %>%
    # District heating
    mutate(n_units_fuel = ifelse(fuel_heat == "district_heat",
      round(n_units_eneff * shr_distr_heat, rnd),
      round(n_units_eneff * (1 - shr_distr_heat) * shr_fuel_heat_base, rnd)
    )) %>%
    # Other fuels (decentralized)
    mutate(n_units_fuel = round(n_units_fuel, rnd)) %>%
    # Sub-standard buildings - one fuel type only
    mutate_cond(mat == "sub", n_units_fuel = n_units_eneff) %>%
    select(-c(stock_arch_base, shr_fuel_heat_base,
      shr_distr_heat, n_units_eneff, mod_decision))

    report <- list()
  if ("energy" %in% report_var) {
    report <- append(report, list(en_stock = as.data.frame(NULL)))
  }
  if ("material" %in% report_var) {
    report <- append(report, list(mat_stock = as.data.frame(NULL)))
  }

  if ("vintage" %in% report_var) {
    # Report stock by eneff - vintage
    bld_eneff_age <- bld_det_age_i %>%
      group_by_at(setdiff(names(bld_det_age_i),
        c("fuel_heat", "fuel_cool", "n_units_fuel"))) %>%
      summarise(n_units_eneff = sum(n_units_fuel)) %>%
      ungroup()

    report <- append(report, list(bld_eneff_age = bld_eneff_age))
  }

  output <- list(
    bld_det_age_i = bld_det_age_i,
    report = report
  )

  return(output)
}




#' @param sector: sector to run, "resid" or "comm"
#' @param yrs: years to run, e.g. c(2015,2020,2025,2030,2035,2040,2045,2050)
#' @param geo_data: data with geographical information
#' @param geo_levels: levels to keep track of, e.g. c("region_bld","region_gea")
#' @param geo_level: level for analysis, e.g. "region_bld", "region_gea"
#' @param bld_cases_eneff: building cases for energy efficiency,
#' @param pop: population data
#' @param hh_size: household size data, used for residential
#' @param floor_cap: floor space data, used for commercial
#' @param ct_hh_inc: household income data
#' @param ct_eneff: energy efficiency data
#' @param ct_fuel_comb: fuel combination data
#' @param stock_arch_base: base building stock data
#' @param shr_mat: share of materials data
#' @param shr_arch: share of architecture data
#' @param report_var: report variable, e.g. "energy"
#' @return: data frame with results
#' @export fun_stock_init_fut
fun_stock_future <- function(sector,
                               yrs,
                               geo_data,
                               geo_levels,
                               geo_level,
                               bld_cases_eneff,
                               pop,
                               hh_size,
                               floor_cap,
                               ct_hh_inc,
                               ct_eneff,
                               ct_fuel_comb,
                               stock_arch_base,
                               shr_mat,
                               shr_arch
                               ) {

  # Total number of residential building units:
  #  population / average household size
  if (sector == "resid") {
    bld_units <- pop %>%
      filter(year %in% yrs[-1]) %>%
      left_join(hh_size) %>%
      mutate(bld_units =
        round(1e6 * pop / length(ct_hh_inc) / hh_size, rnd)) %>%
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

  # Stock aggregated: Number of units (Million) - "arch" level
  stock_aggr <- bld_cases_eneff %>%
    # Aggregate at "arch" level
    select(-c(eneff, bld_age)) %>%
    distinct() %>%
    # Years filtered here
    left_join(bld_units) %>%
    left_join(shr_mat) %>%
    # Long format
    left_join(shr_arch) %>%
    mutate(n_units_aggr = bld_units * shr_mat * shr_arch) %>%
    select(-c(bld_units, shr_mat, shr_arch))

  try(if (nrow(stock_aggr) !=
    nrow(distinct(stock_aggr %>% select(-c(n_units_aggr)))))
    stop("Error in aggregated stock dataset! 
      Multiple records for same combinations in stock_aggr"))

  # Stock aggregated for baseyear
  stock_aggr_base <- geo_data %>%
    select_at(geo_levels) %>%
    left_join(stock_arch_base) %>%
    # Select all variables, except the ones specified
    group_by_at(setdiff(names(stock_arch_base),
      c("bld_age", "yr_con", "stock_arch_base"))) %>%
    summarise(n_units_aggr = sum(stock_arch_base)) %>%
    ungroup()

  # stock aggregated - combined
  stock_aggr <- bind_rows(stock_aggr, stock_aggr_base) %>%
    arrange_at(c(geo_level,
      "urt", "inc_cl", "region_gea", "clim", "mat", "arch", "year"))

  # Add stock variation aggregated
  stock_aggr <- stock_aggr %>%
    group_by_at(c(geo_level, "urt", "inc_cl", "clim", "mat", "arch")) %>%
    mutate(var_aggr = n_units_aggr - lag(n_units_aggr)) %>%
    ungroup()


  try(if(nrow(stock_aggr) != nrow(distinct(stock_aggr %>%
    select(-c(n_units_aggr, var_aggr)))))
    stop("Error in aggregated stock dataset!
      Multiple records for same combinations in stock_aggr"))

  return(stock_aggr)
}
