### INITIALIZE BUILDING STOCK - FUTURE SCENARIOS ###

# add switch to select levels for calculation?
library(dplyr)

#' @param sector: sector to run, "resid" or "comm"
#' @param mod_arch: model architecture, "stock" or "new"
#' @param yrs: years to run, e.g. c(2015,2020,2025,2030,2035,2040,2045,2050)
#' @param geo_data: data with geographical information
#' @param geo_levels: levels to keep track of, e.g. c("region_bld","region_gea")
#' @param geo_level: level for analysis, e.g. "region_bld", "region_gea"
#' @param bld_cases_eneff: building cases for energy efficiency,
#' @param bld_cases_fuel: building cases for fuel,
#' @param pop: population data
#' @param hh_size: household size data, used for residential
#' @param floor_cap: floor space data, used for commercial
#' @param ct_hh_inc: household income data
#' @param ct_eneff: energy efficiency data
#' @param ct_fuel_comb: fuel combination data
#' @param stock_arch_base: base building stock data
#' @param shr_mat: share of materials data
#' @param shr_arch: share of architecture data
#' @param shr_fuel_heat_base: share of fuel for heating data
#' @param shr_distr_heat: share of distribution of heating data
#' @param report_var: report variable, e.g. "energy"
#' @return: data frame with results
#' @export fun_stock_init_fut
fun_stock_init_fut <- function(sector,
                               mod_arch,
                               yrs,
                               geo_data,
                               geo_levels,
                               geo_level,
                               bld_cases_eneff,
                               bld_cases_fuel,
                               pop,
                               hh_size,
                               floor_cap,
                               ct_hh_inc,
                               ct_eneff,
                               ct_fuel_comb,
                               stock_arch_base,
                               shr_mat,
                               shr_arch,
                               shr_fuel_heat_base,
                               shr_distr_heat,
                               report_var) {
  # rounding (number of decimals)
  rnd <- 5

  # Total number of building units: (Residential: number of households (units)
  if (sector == "resid") {
    bld_units <- pop %>%
      filter(year %in% yrs[-1]) %>%
      left_join(hh_size) %>%
      mutate(bld_units = round(1e6 * pop / length(ct_hh_inc) / hh_size, rnd)) %>%
      select(-c(pop, hh_size))
    try(if (nrow(bld_units) != nrow(distinct(bld_units))) stop("Error in aggregated households calculations! duplicated records in hh"))
  }

  # Total number of building units: (Commercial: floor space (m2) / 100 (m2 per unit)
  if (sector == "comm") {
    bld_units <- pop %>%
      filter(year %in% yrs[-1]) %>%
      left_join(floor_cap) %>%
      mutate(bld_units = round(1e6 * pop * floor_cap, rnd)) %>% # convert from million units to units
      # mutate(bld_units = round(pop*floor_cap,rnd)) %>% # million units
      select(-c(pop, floor_cap)) # %>%
    # filter(year %in% yrs) # years already filtered

    try(if (nrow(bld_units) != nrow(distinct(bld_units))) stop("Error in aggregated households calculations! duplicated records in hh"))
  }

  if (mod_arch == "new") {
    # stock aggregated: Number of units (Million) - "mat" level
    stock_aggr <- bld_cases_eneff %>%
      select(-c(eneff, bld_age, arch)) %>% # aggregate at "mat" level
      distinct() %>%
      left_join(bld_units) %>% # years filtered here
      left_join(shr_mat) %>%
        # left_join(shr_arch) %>% # long format
      mutate(n_units_aggr = round(bld_units * shr_mat, rnd)) %>% # to mat level
        # mutate(stock_arch_base = bld_units * shr_mat * shr_arch) %>% # to arch level
      select(-c(bld_units, shr_mat)) # %>%
      # arrange_at(c(geo_level, "urt", "inc_cl", "region_gea", "clim", "mat", "year"))
      # select(-c(bld_units, shr_mat, shr_arch))

    # stock aggregated (mat level) - baseyear
    stock_aggr_base <- geo_data %>%
      select_at(geo_levels) %>%
      left_join(stock_arch_base) %>%
      group_by_at(setdiff(names(stock_arch_base), c("bld_age", "arch", "yr_con", "stock_arch_base"))) %>% # Select all variables, except the ones specified
      summarise(n_units_aggr = sum(stock_arch_base)) %>%
      ungroup()

    # stock aggregated - combined
    stock_aggr <- bind_rows(stock_aggr, stock_aggr_base) %>%
      arrange_at(c(geo_level, "urt", "inc_cl", "region_gea", "clim", "mat", "year"))

    # Add stock variation aggregated
    stock_aggr <- stock_aggr %>%
      # group_by(year) %>%
      group_by_at(c(geo_level, "urt", "inc_cl", "clim", "mat")) %>% # , "arch"
      # mutate(n_units_eneff_p = lag(n_units_eneff))
      mutate(var_aggr = n_units_aggr - lag(n_units_aggr)) %>%
      ungroup()
  } else if (mod_arch == "stock") {
    # stock aggregated: Number of units (Million) - "arch" level
    stock_aggr <- bld_cases_eneff %>%
      select(-c(eneff, bld_age)) %>% # aggregate at "arch" level
      distinct() %>%
      left_join(bld_units) %>% # years filtered here
      left_join(shr_mat) %>%
      left_join(shr_arch) %>% # long format
      # mutate(n_units_aggr = round(bld_units * shr_mat,rnd)) %>% # to mat level
      mutate(n_units_aggr = bld_units * shr_mat * shr_arch) %>% # to arch level
      # select(-c(bld_units, shr_mat)) #%>%
      # arrange_at(c(geo_level, "urt", "inc_cl", "region_gea", "clim", "mat", "year"))
      select(-c(bld_units, shr_mat, shr_arch))

    try(if (nrow(stock_aggr) != nrow(distinct(stock_aggr %>% select(-c(n_units_aggr))))) stop("Error in aggregated stock dataset! multiple records for same combinations in stock_aggr"))

    # stock aggregated (mat level) - baseyear
    stock_aggr_base <- geo_data %>%
      select_at(geo_levels) %>%
      left_join(stock_arch_base) %>%
      group_by_at(setdiff(names(stock_arch_base), c("bld_age", "yr_con", "stock_arch_base"))) %>% # Select all variables, except the ones specified
      summarise(n_units_aggr = sum(stock_arch_base)) %>%
      ungroup()

    # stock aggregated - combined
    stock_aggr <- bind_rows(stock_aggr, stock_aggr_base) %>%
      arrange_at(c(geo_level, "urt", "inc_cl", "region_gea", "clim", "mat", "arch", "year"))

    # add stock variation aggregated
    stock_aggr <- stock_aggr %>%
      # group_by(year) %>%
      group_by_at(c(geo_level, "urt", "inc_cl", "clim", "mat", "arch")) %>% #
      # mutate(n_units_eneff_p = lag(n_units_eneff))
      mutate(var_aggr = n_units_aggr - lag(n_units_aggr)) %>%
      ungroup()
  }

  try(if(nrow(stock_aggr) != nrow(distinct(stock_aggr %>% select(-c(n_units_aggr, var_aggr))))) stop("Error in aggregated stock dataset! multiple records for same combinations in stock_aggr"))

  # # TEST: N. units 2050 - CPA
  # sum(stock_aggr %>% filter(region_bld == "R32CHN" & year == 2050) %>% select(n_units_aggr) %>% pull)/1e6 # Million units


  # initialize DF stock by vintage (baseyear) - detailed fuel level
  bld_det_age_i <- geo_data %>%
    select_at(geo_levels) %>%
    left_join(stock_arch_base) %>% # baseyear results
    left_join(ct_eneff) %>%
    left_join(ct_fuel_comb) %>%
    left_join(shr_fuel_heat_base) %>%
    left_join(shr_distr_heat) %>%
    # left_join(shr_acc_cool) %>%
    mutate(n_units_eneff = stock_arch_base) %>% # assumption: one eneff per period of construction
    mutate(n_units_fuel = ifelse(fuel_heat == "district_heat",
      round(n_units_eneff * shr_distr_heat, rnd), # district heating
      round(n_units_eneff * (1 - shr_distr_heat) * shr_fuel_heat_base, rnd)
    )) %>% # other fuels (decentralized)
    mutate(n_units_fuel = round(n_units_fuel, rnd)) %>%
    mutate_cond(mat == "sub", n_units_fuel = n_units_eneff) %>% # sub-standard buildings - one fuel type only
    select(-c(stock_arch_base, shr_fuel_heat_base, shr_distr_heat, n_units_eneff, mod_decision))

  # ## other option: start from bld cases
  # bld_fuel_age <- bld_cases_fuel %>%
  #   mutate(year = yrs[1]) %>%
  # initialize bld_arch: stock data - arch level - NOT NEEDED!
  # bld_det <- as.data.frame(NULL)
  # Report energy and material results
  # en_stock <- as.data.frame(NULL) # COMMENT IF en_stock is updated for base year
  # mat_stock <- as.data.frame(NULL)

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
      group_by_at(setdiff(names(bld_det_age_i), c("fuel_heat", "fuel_cool", "n_units_fuel"))) %>%
      summarise(n_units_eneff = sum(n_units_fuel)) %>%
      ungroup()

    report <- append(report, list(bld_eneff_age = bld_eneff_age))
  }

  output <- list(
    stock_aggr = stock_aggr,
    bld_det_age_i = bld_det_age_i,
    report = report
  )
}
