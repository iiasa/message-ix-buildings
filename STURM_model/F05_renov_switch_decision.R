# Description: Renovation switch decision
# Complete model with fuel choice

library(dplyr)

  # rounding (number of decimals)
  rnd <- 5


fun_lcc <- function(capex, opex, rate, duration) {
  lcc <- capex + opex * ((1 - (1 + rate)^(-duration)) / rate)
  return(lcc)
}

fun_discount_factor <- function(bld_cases_fuel, discount, duration) {

  # if discount rate is a float then convert to a data.frame
  if (is.numeric(discount)) {
    discount <- data.frame(discount_rate = discount)
  }

  if (is.numeric(duration)) {
    duration <- data.frame(lifetime_ren = duration,
      region_bld = unique(bld_cases_fuel$region_bld))
  }

  bld_cases_fuel <- distinct(select(bld_cases_fuel,
    intersect(c(names(discount), names(duration)),
    names(bld_cases_fuel))))

  temp <- bld_cases_fuel %>%
    left_join(duration) %>%
    left_join(discount) %>%
    mutate(discount_factor =
      (1 - (1 + discount_rate)^-lifetime_ren) / discount_rate)
  temp <- select(temp, setdiff(names(temp), c("discount_rate", "lifetime_ren")))
  return(temp)
}



fun_utility_ren_shell <- function(yrs,
                          i,
                          bld_cases_fuel,
                          ct_bld_age,
                          ct_ren_eneff,
                          hh_size,
                          floor_cap,
                          cost_invest_ren_shell,
                          en_hh_tot,
                          lifetime_ren,
                          discount_ren,
                          sub_ren_shell = NULL,
                          full = FALSE) {

  en_hh_tot <- select(en_hh_tot, -c("budget_share",
    "heating_intensity", "en_hh_std"))
  # Operational energy costs before/after renovation
  en_hh_tot_ren_fin <- en_hh_tot %>%
    rename(eneff_f = eneff)

  en_hh_tot_ren_init <- en_hh_tot %>%
    rename(cost_op_init = cost_op, en_hh_init = en_hh)

  # Prepare investment cost data
  if ("year" %in% names(cost_invest_ren_shell)) {
    cost_invest_ren_shell <- cost_invest_ren_shell %>% filter(year == yrs[i])
  }
  cost_invest_ren_shell <- cost_invest_ren_shell %>%
    mutate(cost_invest_ren_shell = as.numeric(cost_invest_ren_shell))
    
  # Apply subsidy
  if (!is.null(sub_ren_shell)) {
    if ("year" %in% names(sub_ren_shell)) {
      sub_ren_shell <- sub_ren_shell %>%
        filter(year == yrs[i])
    }
  } else {
    sub_ren_shell <- cost_invest_ren_shell %>%
      mutate(sub_ren_shell = 0) %>%
      select(-c("cost_invest_ren_shell"))
  }

  discount_factor <- fun_discount_factor(bld_cases_fuel,
    discount_ren, lifetime_ren)

  bld_age_exst <- ct_bld_age %>%
    filter(year_i < yrs[i]) %>%
    select(bld_age_id) %>%
    pull(bld_age_id)

  utility_ren_hh <- bld_cases_fuel %>%
      filter(bld_age %in% bld_age_exst) %>%
      left_join(ct_ren_eneff %>%
          rename(eneff = eneff_i),
          relationship = "many-to-many") %>%
      filter(!is.na(eneff_f)) %>%
      filter(eneff_f %in% c("adv", "std")) %>%
      # Attach year (in the loop)
      mutate(year = yrs[i]) %>%
      left_join(hh_size) %>%
      left_join(floor_cap) %>%
      mutate(floor_size = floor_cap * hh_size) %>%
      left_join(discount_factor) %>%
      left_join(cost_invest_ren_shell) %>%
      left_join(sub_ren_shell) %>%
      mutate(sub_ren_shell = ifelse(is.na(sub_ren_shell), 0, sub_ren_shell)) %>%
      mutate(cost_invest_ren_shell =
        cost_invest_ren_shell * (1 - sub_ren_shell)) %>%
      # Calculate total investment costs
      mutate(cost_invest_hh =
        cost_invest_ren_shell * floor_size / 1e3) %>%
      # Operation costs after renovation
      left_join(en_hh_tot_ren_fin) %>%
      # Operation costs before renovation
      left_join(en_hh_tot_ren_init) %>%
      # Filter out hh with no operational cost
      filter(cost_op_init > 0) %>%
      # Add operative costs (total)
      mutate(en_saving = en_hh_init - en_hh) %>%
      mutate(cost_op_saving = (cost_op_init - cost_op) / 1e3) %>%
      # Calculate utility
      mutate(utility_ren =
      - cost_invest_hh + cost_op_saving * discount_factor) %>%
      # Rename eneff column
      rename(eneff_i = eneff) %>%
      select(-c(
          "hh_size", "floor_cap", "en_hh",
          "cost_invest_ren_shell", "cost_op", "cost_op_init",
          "discount_factor", "sub_ren_shell"))

  if (!full) {
    utility_ren_hh <- select(utility_ren_hh, -c("floor_size",
      "cost_invest_hh", "en_hh_init", "en_saving", "cost_op_saving"))
  }


  return(utility_ren_hh)
}


#' @title Renovation switch decision - STURM
#' @description Calculate renovation switch decision
#' @param yrs Years to be calculated
#' @param i Time step
#' @param bld_cases_fuel Building cases and fuel
#' @param ct_bld_age Building age
#' @param ct_hh_tenr Household tenure
#' @param ct_fuel_comb Fuel combination
#' @param ct_ren_eneff Renovation energy efficiency
#' @param ct_ren_fuel_heat Renovation fuel for heating
#' @param hh_size Household size
#' @param floor_cap Floor capacity
#' @param hh_tenure Household tenure
#' @param cost_invest_ren_shell Investment costs
#'  for renovation of the building shell
#' @param cost_invest_ren_heat Investment costs
#'  for renovation of the heating system
#' @param ct_fuel_excl_ren Fuel excluded for renovation
#' @param ct_fuel_excl_reg Fuel excluded for specific regions
#' @param discount_ren Discount rate for renovation
#' @param lifetime_ren Lifetime of renovation
#' @param en_hh_tot Energy demand
fun_ms_ren_shell_endogenous <- function(yrs,
                          i,
                          bld_stock,
                          ct_bld_age,
                          ct_ren_eneff,
                          hh_size,
                          floor_cap,
                          cost_invest_ren_shell,
                          sub_ren_shell,
                          en_hh_tot,
                          lifetime_ren,
                          discount_ren,
                          parameters = NULL) {
  print(paste0("Running renovation decisions - year ", yrs[i]))

  ## Define timestep
  stp <- yrs[i] - yrs[i - 1]

  utility_ren_hh <- fun_utility_ren_shell(yrs,
                        i,
                        bld_stock,
                        ct_bld_age,
                        ct_ren_eneff,
                        hh_size,
                        floor_cap,
                        cost_invest_ren_shell,
                        en_hh_tot,
                        lifetime_ren,
                        discount_ren,
                        sub_ren_shell = sub_ren_shell)

  if ((!is.null(parameters))) {
    utility_ren_hh <- utility_ren_hh %>%
      left_join(parameters) %>%
      mutate(utility_ren = utility_ren * scaling_factor + constant) %>%
      mutate(barrier_mfh = ifelse(arch == "mfh",
        barrier_mfh, 0)) %>%
      mutate(utility_ren = ifelse(arch == "mfh",
        utility_ren - barrier_mfh, utility_ren)) %>%
      mutate(barrier_rent = ifelse(tenr == "rent",
        barrier_rent, 0)) %>%
      mutate(utility_ren = ifelse(tenr == "rent",
        utility_ren - barrier_rent, utility_ren)) %>%
      select(-c("scaling_factor", "constant", "barrier_rent", "barrier_mfh"))
  }

  # All possible combinations covered (including no renovation)
  ms_i <- utility_ren_hh %>%
    group_by_at(setdiff(names(utility_ren_hh),
                c("eneff_f", "utility_ren"))) %>%
    mutate(utility_exp_sum = sum(exp(utility_ren)) + 1) %>%
    mutate(ms = (1 / stp) * exp(utility_ren) / utility_exp_sum) %>%
    select(-c("utility_ren", "utility_exp_sum"))

  # Renovation rate
  rate_ren_i <- ms_i %>%
    filter(eneff_i != eneff_f) %>%
    group_by_at(setdiff(names(ms_i), c("eneff_f", "ms"))) %>%
    summarise(rate_ren = sum(ms)) %>%
    ungroup() %>%
    rename(eneff = eneff_i) %>%
    filter(!is.na(rate_ren))

  # Update market shares - keep renovations only
  ms_ren_i <- ms_i %>%
    filter(eneff_i != eneff_f) %>%
    group_by_at(setdiff(names(ms_i), c("eneff_f", "ms"))) %>%
    mutate(ms_tot = sum(ms)) %>%
    ungroup() %>%
    mutate(ms_ren = ifelse(ms_tot > 0, round(ms / ms_tot, rnd), 0)) %>%
    filter(ms_ren > 0) %>%
    select(-c(ms, ms_tot)) %>%
    rename(fuel_heat_i = fuel_heat) %>%
    mutate(fuel_heat_f = fuel_heat_i) %>%
    filter(!is.na(ms_ren))

  output <- list(
    ms_ren_i = ms_ren_i,
    rate_ren_i = rate_ren_i
  )

  return(output)
}


#' @title Renovation rate - shell
#' @description Renovation rate - shell
#' @param yrs vector of years
#' @param i index of the year
#' @param bld_cases_fuel data frame with building cases
#' @param ct_bld_age data frame with construction periods
#' @param rate_shell_ren_exo data frame with exogenous renovation rates
#' @param ms_shell_ren_exo data frame with exogenous market shares
#' @return data frame with renovation rates
fun_ms_ren_shell_exogenous <- function(yrs,
                                        i,
                                        bld_stock,
                                        ct_bld_age,
                                        rate_shell_ren_exo,
                                        ms_shell_ren_exo
                                        ) {
  print(paste0("Running renovation target - year ", yrs[i]))

  # Filter based on the construction period
  p_past <- ct_bld_age %>%
    filter(year_f < yrs[i]) %>%
    pull(bld_age_id)

  rate_ren_i <- bld_stock %>%
    filter(bld_age %in% p_past) %>%
    # Filter based on eneff
    filter(eneff == "avg") %>%
    # Add years columns
    mutate(year = yrs[i]) %>%
    # Join market share column
    left_join(rate_shell_ren_exo) %>%
    # Rename eneff column
    rename(rate_ren = rate_shell_ren_exo) %>%
    filter(!is.na(rate_ren)) %>%
    filter(rate_ren > 0)

  ms_ren_i <- bld_stock %>%
    filter(bld_age %in% p_past) %>%
    # Add years columns
    mutate(year = yrs[i]) %>%
    # Rename eneff column
    rename(eneff_i = eneff) %>%
    rename(fuel_heat_i = fuel_heat) %>%
    # Join market share column
    left_join(ms_shell_ren_exo,
      relationship = "many-to-many") %>%
    rename(ms_ren = ms_shell_ren_exo) %>%
    # Only shell renovation and no switch fuel
    mutate(fuel_heat_f = fuel_heat_i) %>%
    rename(eneff_f = eneff) %>%
    filter(!is.na(ms_ren)) %>%
    filter(ms_ren > 0)

  print(paste0("Completed renovation target - year ", yrs[i]))

  output <- list(
                ms_ren_i = ms_ren_i,
                rate_ren_i = rate_ren_i
                )
  return(output)
}


fun_utility_heat <- function(yrs,
                        i,
                        bld_stock,
                        en_hh_tot,
                        ct_switch_heat,
                        ct_fuel_excl_reg,
                        ct_heat,
                        ct_heat_new,
                        cost_invest_heat,
                        lifetime_heat,
                        discount_heat,
                        sub_heat = NULL,
                        inertia = NULL,
                        full = FALSE) {

  en_hh_tot <- select(en_hh_tot, -c("budget_share", "heating_intensity", "en_hh_std"))

  # Operational energy costs before/after renovation
  en_hh_tot_switch_fin <- en_hh_tot %>%
    rename(fuel_heat_f = fuel_heat)

  # Prepare investment cost data
  if ("year" %in% names(cost_invest_heat)) {
    cost_invest_heat <- cost_invest_heat %>%
      filter(year == yrs[i])
  }
  cost_invest_heat <- cost_invest_heat %>%
    mutate(cost_invest_heat = as.numeric(cost_invest_heat))

  # Apply subsidy
  if (!is.null(sub_heat)) {
    if ("year" %in% names(sub_heat)) {
      sub_heat <- sub_heat %>%
        filter(year == yrs[i])
    }
  } else {
    sub_heat <- cost_invest_heat %>%
      mutate(sub_heat = 0) %>%
      select(-c("cost_invest_heat"))
  }

  # Discount factor
  discount_factor <- fun_discount_factor(bld_stock,
    discount_heat, lifetime_heat)
  
  # Calculate utility
  utility_heat_hh <- bld_stock %>%
    left_join(ct_switch_heat %>%
        rename(fuel_heat = fuel_heat_i),
        relationship = "many-to-many") %>%
    left_join(ct_heat_new, relationship =
      "many-to-many") %>%
    mutate(fuel_heat_f = ifelse(
        !is.na(fuel_heat_new), fuel_heat_new, fuel_heat_f)) %>%
    select(-c("fuel_heat_new", "ct_heat_new")) %>%
    filter(!is.na(fuel_heat_f)) %>%
    left_join(ct_fuel_excl_reg) %>%
    filter(is.na(ct_fuel_excl_reg)) %>%
    left_join(ct_heat %>% rename(fuel_heat_f = fuel_heat)) %>%
    mutate(ct_heat = ifelse(
        is.na(ct_heat) | ct_heat == 1, 1, 0)) %>%
    left_join(cost_invest_heat) %>%
    # Operation costs after renovation
    left_join(en_hh_tot_switch_fin %>% select(-c("fuel", "fuel_cool"))) %>%
    left_join(discount_factor) %>%
    left_join(sub_heat) %>%
    mutate(sub_heat = ifelse(is.na(sub_heat), 0, sub_heat)) %>%
    mutate(cost_invest_heat = cost_invest_heat * (1 - sub_heat)) %>%
    # Calculate utility
    mutate(utility_heat =
      (- cost_invest_heat - cost_op * discount_factor) / 1e3) %>%
    filter((ct_switch_heat == 1) | (bld_age == "p5")) %>%
    filter(ct_heat == 1) %>%
    select(-c("cost_op", "en_hh", "sub_heat",
      "ct_switch_heat", "ct_fuel_excl_reg", "ct_heat", "discount_factor"))

  if (!full) {
    utility_heat_hh <- select(utility_heat_hh, -c("cost_invest_heat"))
  }

  if (!is.null(inertia)) {
    utility_heat_hh <- utility_heat_hh %>%
      left_join(inertia) %>%
      mutate(inertia = inertia *
                ifelse(fuel_heat == fuel_heat_f, 1, 0)) %>%
      mutate(inertia =
          ifelse(bld_age == "p5", 0, inertia)) %>%
      mutate(utility_heat = utility_heat + inertia) %>%
      select(-c("inertia"))
  }
  return(utility_heat_hh)
}


fun_ms_switch_heat_endogenous <- function(yrs,
                          i,
                          bld_stock,
                          ct_bld_age,
                          ct_switch_heat,
                          ct_fuel_excl_reg,
                          cost_invest_heat,
                          sub_heat,
                          en_hh_tot,
                          ct_heat,
                          ct_heat_new,
                          discount_heat,
                          lifetime_heat = 20,
                          inertia = NULL,
                          parameters = NULL,
                          ban_fuel = NULL) {
  print(paste0("Running renovation decisions - year ", yrs[i]))

  
  
  utility_heat_hh <- fun_utility_heat(yrs,
                        i,
                        bld_stock,
                        en_hh_tot,
                        ct_switch_heat,
                        ct_fuel_excl_reg,
                        ct_heat,
                        ct_heat_new,
                        cost_invest_heat,
                        lifetime_heat,
                        discount_heat,
                        inertia = inertia,
                        sub_heat = sub_heat)

  if ((!is.null(ban_fuel))) {
    utility_heat_hh <- utility_heat_hh %>%
      left_join(ban_fuel) %>%
      mutate(ban_fuel = ifelse(is.na(ban_fuel), 0, ban_fuel)) %>%
      filter(ban_fuel != 1) %>%
      select(-c("ban_fuel"))
  }

  if ((!is.null(parameters))) {
    utility_heat_hh <- utility_heat_hh %>%
      left_join(parameters) %>%
      mutate(utility_heat = utility_heat * scaling_factor + constant) %>%
      select(-c("scaling_factor", "constant"))
  }

  # All possible combinations covered (including no renovation)
  ms_i <- utility_heat_hh %>%
    group_by_at(setdiff(names(utility_heat_hh),
                c("fuel_heat_f", "utility_heat"))) %>%
    mutate(utility_exp_sum = sum(exp(utility_heat))) %>%
    mutate(ms = exp(utility_heat) / utility_exp_sum) %>%
    ungroup() %>%
    select(-c("utility_heat", "utility_exp_sum"))

  return(ms_i)
}


#' @title Calculate market shares for fuel switch and renovation
#' @description Calculate market shares for fuel switch and renovation
#' @param yrs vector of years
#' @param i index of the year
#' @param bld_cases_fuel data frame of building cases
#' @param ct_bld_age data frame of building age cohorts
#' @param ms_switch_fuel_exo data frame of exogenous fuel switch market shares
#' @return data frame
fun_ms_fuel_sw_exogenous <- function(yrs,
                                      i,
                                      bld_stock,
                                      ct_bld_age,
                                      ms_switch_fuel_exo) {
  print(paste0("Running fuel switch target - year ", yrs[i]))

  ms_sw_i <- bld_stock %>%
   # Add years columns
    mutate(year = yrs[i]) %>%
    left_join(ms_switch_fuel_exo %>%
      rename(ms = ms_switch_fuel_exo)
      ) %>%
    filter(!is.na(ms)) %>%
    filter(ms > 0)
    
  print(paste0("Completed fuel switch target - year ", yrs[i]))

  return(ms_sw_i)
}
