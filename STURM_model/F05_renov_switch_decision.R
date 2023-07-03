# Description: Renovation switch decision
# Complete model with fuel choice

library(dplyr)

  # rounding (number of decimals)
  rnd <- 5

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
#' @param cost_intang_ren_shell Intangible costs
#' for renovation of the building shell
#' @param cost_intang_ren_heat Intangible costs
#' for renovation of the heating system
#' @param ct_fuel_excl_ren Fuel excluded for renovation
#' @param ct_fuel_excl_reg Fuel excluded for specific regions
#' @param discount_ren Discount rate for renovation
#' @param heterog_ren Heterogeneity parameters of renovation
#' @param lifetime_ren Lifetime of renovation
#' @param rate_ren_low Low renovation rate
#' @param rate_ren_high High renovation rate
#' @param en_hh_tot Energy demand
fun_ms_ren_sw <- function(yrs,
                          i,
                          bld_cases_fuel,
                          ct_bld_age,
                          ct_hh_tenr,
                          ct_fuel_comb,
                          ct_ren_eneff,
                          ct_ren_fuel_heat,
                          hh_size,
                          floor_cap,
                          hh_tenure,
                          cost_invest_ren_shell,
                          cost_invest_ren_heat,
                          cost_intang_ren_shell,
                          cost_intang_ren_heat,
                          ct_fuel_excl_ren,
                          ct_fuel_excl_reg,
                          discount_ren,
                          heterog_ren,
                          lifetime_ren,
                          rate_ren_low,
                          rate_ren_high,
                          en_hh_tot) {
  print(paste0("Running renovation decisions - year ", yrs[i]))

  ## Define timestep
  stp <- yrs[i] - yrs[i - 1]

  # Building cases fuels + tenure
  bld_cases_fuel_tenr <- merge(bld_cases_fuel,
    as.data.frame(ct_hh_tenr)) %>%
    rename(tenr = ct_hh_tenr)

  # Fuel types in renovation decisions
  ct_fuel_heat_mod <- sort(unique(
      ct_fuel_comb$fuel_heat[which(ct_fuel_comb$mod_decision == 1)]))

  # Operational energy costs before/after renovation
  # Final energy costs to be used in renovation decisions
  # (fuels and eneff are after renovation)
  en_hh_tot_ren_fin <- en_hh_tot %>%
    rename(eneff_f = eneff) %>%
    rename(fuel_heat_f = fuel_heat) %>%
    # should be removed, otherwise no match with eneff
    # (e.g. an older building can switch to a newer eneff standard)
    select(-bld_age)

  # Initial energy costs to be used in renovation decisions
  # (fuels and eneff are before renovation)
  # This is used to filter out hh with zero operation costs before renovation
  en_hh_tot_ren_init <- en_hh_tot %>%
    rename(cost_op_m2_init = cost_op_m2) %>%
    # should be removed, otherwise no match with eneff
    # (e.g. an older building can switch to a newer eneff standard)
    select(-bld_age)

  # Prepare investment cost data
  cost_invest_ren_shell_i <- cost_invest_ren_shell %>% filter(year == yrs[i])
  cost_invest_ren_heat_i <- cost_invest_ren_heat %>% filter(year == yrs[i])

  # Prepare intangible cost data
  cost_intang_ren_shell_i <- cost_intang_ren_shell %>% filter(year == yrs[i])
  cost_intang_ren_heat_i <- cost_intang_ren_heat

  # Test inputs - age categories
  print("Test inputs - Building vintage")
  print(paste0("DF ct_bld_age: ", ct_bld_age))
  print(paste0("Class ct_bld_age: ", class(ct_bld_age)))
  print(paste0("Year: ", yrs[i]))

  bld_age_exst <- ct_bld_age %>%
    filter(year_i < yrs[i]) %>%
    select(bld_age_id) %>%
    pull(bld_age_id)

  # Prepare dataframe for LCC calculations at household level - Renovations
  lcc_ren_hh <- bld_cases_fuel_tenr %>%
    # Renovation only for existing buildings
    filter(bld_age %in% bld_age_exst) %>%
    # Add eneff categories for renovations
    left_join(ct_ren_eneff %>% rename(eneff = eneff_i)) %>%
    # Add heat_fuel categories for renovations
    left_join(ct_ren_fuel_heat %>% rename(fuel_heat = fuel_heat_i)) %>%
    # Constraint: fuels not allowed for renovation
    left_join(ct_fuel_excl_ren) %>%
    left_join(ct_fuel_excl_reg %>%
      # Constraint (before renovation): fuels not used in specific regions
      rename(ct_fuel_excl_i_reg = ct_fuel_excl_reg)) %>%
    left_join(ct_fuel_excl_reg %>%
      rename(fuel_heat_f = fuel_heat) %>%
      # Constraint (after renovation): fuels not used in specific regions
      rename(ct_fuel_excl_f_reg = ct_fuel_excl_reg)) %>%
    # Exclude cases out of modelling decisions
    #  (e.g. district heating, substandard buildings)
    filter(mod_decision == 1) %>%
    # Exclude non-permitted fuels (e.g. coal for passive houses)
    filter(
      is.na(ct_fuel_excl_ren),
      is.na(ct_fuel_excl_i_reg),
      is.na(ct_fuel_excl_f_reg)
    ) %>%
    # Filter out new construction, already renovated buildings
    # (no "eneff_f" value available for those)
    filter(!is.na(eneff_f)) %>%
    filter((eneff == eneff_f & fuel_heat == fuel_heat_f) |
      # Transitions between eneffs for existing buildings (without renovation)
      #  require the renovation switch "swt_ren" to be ON
      (eneff == eneff_f & ct_ren_fuel_heat == 1) |
      # Transitions between eneffs for renovations require the renovation
      #  switch "ct_ren_fuel_heat" to be ON (1).
      # Note:  swt_exst and  swt_ren replaced by one column only:
      #  ct_ren_fuel_heat
      (eneff != eneff_f & ct_ren_fuel_heat == 1)) %>%
    # Attach year (in the loop)
    mutate(year = yrs[i]) %>%
    # Add HH size
    left_join(hh_size) %>%
    # Add floor per capita
    left_join(floor_cap) %>%
    # Add lifetime ren construction (for investment: based on loan duration)
    left_join(lifetime_ren) %>%
    # Add investment costs for shell renovation
    left_join(cost_invest_ren_shell_i) %>%
    # Add investment costs for heating renovation
    left_join(cost_invest_ren_heat_i) %>% #
    # No renovation
    mutate_cond(eneff == eneff_f & fuel_heat == fuel_heat_f,
      cost_invest_ren_heat = 0) %>%
    # Calculate total investment costs
    mutate(cost_invest_hh = cost_invest_ren_heat + 
      (cost_invest_ren_shell * floor_cap * hh_size)) %>%
    # Operation costs after renovation
    left_join(en_hh_tot_ren_fin) %>%
    # Operation costs before renovation
    left_join(en_hh_tot_ren_init) %>%
    # Filter out hh with no operational cost
    # REMOVING ALL RECORDS WITH NO HEATING!!!
    filter(cost_op_m2_init > 0) %>%
    # Add operative costs (total)
    mutate(cost_op_hh = cost_op_m2 * floor_cap * hh_size) %>%
    # Add intangible costs for shell renovation
    left_join(cost_intang_ren_shell_i) %>%
    # Add intangible costs for heating renovation
    left_join(cost_intang_ren_heat_i %>% 
    rename(fuel_heat = fuel_heat_i)) %>%
    # Calculate total investment costs
    mutate(cost_intang_hh = cost_intang_ren_heat +
      (cost_intang_ren_shell * floor_cap * hh_size)) %>%
    # Add discount rates
    left_join(discount_ren) %>%
    # Add discount rates
    left_join(heterog_ren) %>%
    # Calculate LCC to new construction
    mutate(lcc_ren = fun_lcc(cost_invest_hh, cost_op_hh,
      cost_intang_hh, discount_ren, lifetime_ren)) %>%
    # Calculate operation lcc (for reporting)
    mutate(cost_op_hh_lcc = lcc_ren - cost_intang_hh - cost_invest_hh) %>%
    # Expon. nu
    mutate(lcc_ren_exp = fun_lcc_exp(lcc_ren, heterog_ren)) %>%
    # Rename eneff column
    rename(eneff_i = eneff) %>%
    # REMOVED IN v0.7
    rename(fuel_heat_i = fuel_heat) %>%
    # FILTERING BASED ON INTANGIBLE COSTS - NECESSARY?
    filter(cost_intang_ren_shell != 99999) %>%
    # FILTERING BASED ON INTANGIBLE COSTS - NECESSARY?
    filter(cost_intang_ren_heat != 99999) %>%
    select(-c(ct_ren_fuel_heat, ct_fuel_excl_ren,
      ct_fuel_excl_i_reg, ct_fuel_excl_f_reg))

  try(if (nrow(lcc_ren_hh) != nrow(distinct(lcc_ren_hh)))
    stop("Error in renovation calculation! Duplicated records in lcc_ren_hh"))

  ### MARKET SHARE - RENOVATIONS + FUEL SWITCHES
  # All possible combinations covered (including no renovation)
  # Totals market shares by eneff_i + fuel_heat_i = 1
  ms_i <- lcc_ren_hh %>%
    select(-c(
      "hh_size", "floor_cap", "cost_invest_ren_shell", "cost_invest_ren_heat",
      "cost_invest_hh", "cost_op_m2", "cost_op_m2_init",
      "cost_op_hh", "cost_intang_ren_heat", "cost_intang_ren_shell",
      "cost_intang_hh", "discount_ren",
      "lifetime_ren", "heterog_ren", "lcc_ren", "cost_op_hh_lcc"
    ))
  
  ms_i <- ms_i %>%
    # Select all variables, except "eneff_f" and "lcc_ren_hh_exp" for grouping)
    group_by_at(setdiff(names(ms_i),
      c("eneff_f", "fuel_heat_f", "lcc_ren_exp"))) %>%
    mutate(lcc_ren_exp_sum = sum(lcc_ren_exp)) %>%
    # ADD CHECK HERE
    # "eneff_f" categories in "lcc_ren_hh" should include all possible renovation categories!
    ungroup() %>% 
    mutate(ms_unwgt = lcc_ren_exp / lcc_ren_exp_sum) %>%
    # Market share - not weighted on share of hh tenure
    # Join data on tenure shares
    left_join(hh_tenure) %>%
    # Weight market share on tenures
    mutate(ms_wgt = ms_unwgt * hh_tenure) %>%
    # Select all variables, except "eneff" and "n_dem" for grouping)
    group_by_at(setdiff(names(ms_i),
      c("lcc_ren_exp", "lcc_ren_exp_sum", "tenr",
        "ms_wgt", "ms_unwgt", "hh_tenure"))) %>%
    summarise(ms = round(sum(ms_wgt), rnd)) %>%
    ungroup() %>%
    # Remove periods of construction
    select(-bld_age)

  # Renovation rate
  # REGIONS WITH NO HEATING ARE EXCLUDED BEFORE!
  rate_ren_i <- ms_i %>%
    filter(eneff_i == eneff_f) %>%
    mutate(ren_share_calc = 1 - ms) %>%
    mutate(rate_ren_calc = (1 - ms) / stp) %>%
    left_join(rate_ren_low) %>%
    left_join(rate_ren_high) %>%
    mutate(rate_ren = ifelse(rate_ren_calc < rate_ren_low, rate_ren_low,
      ifelse(rate_ren_calc > rate_ren_high, rate_ren_high, rate_ren_calc)
      )) %>%
    rename(eneff = eneff_i, fuel_heat = fuel_heat_i) %>%
    select(-c(
      eneff_f, fuel_heat_f, ms,
      # rate_ren,
      rate_ren_low, rate_ren_high,
      rate_ren_calc, ren_share_calc
    ))

  # Update market shares - keep renovations only
  ms_ren_i <- ms_i %>%
    filter(eneff_i != eneff_f) %>%
    group_by_at(setdiff(names(ms_i), c("eneff_f", "fuel_heat_f", "ms"))) %>%
    mutate(ms_tot = sum(ms)) %>%
    ungroup() %>%
    mutate(ms_ren = ifelse(ms_tot > 0, round(ms / ms_tot, rnd), 0)) %>%
    filter(ms_ren > 0) %>%
    select(-c(ms, ms_tot))

  # Update market shares - keep fuel switches only
  ms_sw_i <- ms_i %>%
    filter(eneff_i == eneff_f & fuel_heat_i != fuel_heat_f) %>%
    group_by_at(setdiff(names(ms_i), c("eneff_f", "fuel_heat_f", "ms"))) %>%
    mutate(ms_tot = sum(ms)) %>%
    ungroup() %>%
    mutate(ms = ifelse(ms_tot > 0, round(ms / ms_tot, rnd), 0)) %>%
    filter(ms > 0) %>%
    select(-c(ms_tot, eneff_f)) %>%
    rename(eneff = eneff_i) %>%
    rename(fuel_heat = fuel_heat_i)


  output <- list(
    ms_ren_i = ms_ren_i,
    ms_sw_i = ms_sw_i,
    rate_ren_i = rate_ren_i
  )
  return(output)
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
fun_ms_ren_sw_endogenous <- function(yrs,
                          i,
                          bld_cases_fuel,
                          ct_bld_age,
                          ct_hh_tenr,
                          ct_fuel_comb,
                          ct_ren_eneff,
                          ct_ren_fuel_heat,
                          hh_size,
                          floor_cap,
                          hh_tenure,
                          cost_invest_ren_shell,
                          cost_invest_ren_heat,
                          ct_fuel_excl_ren,
                          ct_fuel_excl_reg,
                          discount_ren,
                          lifetime_ren,
                          en_hh_tot) {
  print(paste0("Running renovation decisions - year ", yrs[i]))

  ## Define timestep
  stp <- yrs[i] - yrs[i - 1]

  # Building cases fuels + tenure
  bld_cases_fuel_tenr <- merge(bld_cases_fuel,
    as.data.frame(ct_hh_tenr)) %>%
    rename(tenr = ct_hh_tenr)

  # Operational energy costs before/after renovation
  # Final energy costs to be used in renovation decisions
  # (fuels and eneff are after renovation)
  en_hh_tot_ren_fin <- en_hh_tot %>%
    rename(eneff_f = eneff) %>%
    rename(fuel_heat_f = fuel_heat) %>%
    # should be removed, otherwise no match with eneff
    # (e.g. an older building can switch to a newer eneff standard)

  # Initial energy costs to be used in renovation decisions
  # (fuels and eneff are before renovation)
  # This is used to filter out hh with zero operation costs before renovation
  en_hh_tot_ren_init <- en_hh_tot %>%
    rename(cost_op_m2_init = cost_op_m2) %>%
    # should be removed, otherwise no match with eneff
    # (e.g. an older building can switch to a newer eneff standard)

  # Prepare investment cost data
  cost_invest_ren_shell_i <- cost_invest_ren_shell %>% filter(year == yrs[i])
  cost_invest_ren_heat_i <- cost_invest_ren_heat %>% filter(year == yrs[i])


  bld_age_exst <- ct_bld_age %>%
    filter(year_i < yrs[i]) %>%
    select(bld_age_id) %>%
    pull(bld_age_id)

  # Prepare dataframe for LCC calculations at household level - Renovations
  lcc_ren_hh <- bld_cases_fuel_tenr %>%
    # Renovation only for existing buildings
    filter(bld_age %in% bld_age_exst) %>%
    # Add eneff categories for renovations
    left_join(ct_ren_eneff %>% rename(eneff = eneff_i)) %>%
    # Add heat_fuel categories for renovations
    left_join(ct_ren_fuel_heat %>% rename(fuel_heat = fuel_heat_i)) %>%
    # Constraint: fuels not allowed for renovation
    left_join(ct_fuel_excl_ren) %>%
    left_join(ct_fuel_excl_reg %>%
      # Constraint (before renovation): fuels not used in specific regions
      rename(ct_fuel_excl_i_reg = ct_fuel_excl_reg)) %>%
    left_join(ct_fuel_excl_reg %>%
      rename(fuel_heat_f = fuel_heat) %>%
      # Constraint (after renovation): fuels not used in specific regions
      rename(ct_fuel_excl_f_reg = ct_fuel_excl_reg)) %>%
    # Exclude cases out of modelling decisions
    #  (e.g. district heating, substandard buildings)
    filter(mod_decision == 1) %>%
    # Exclude non-permitted fuels (e.g. coal for passive houses)
    filter(
      is.na(ct_fuel_excl_ren),
      is.na(ct_fuel_excl_i_reg),
      is.na(ct_fuel_excl_f_reg)
    ) %>%
    # Filter out new construction, already renovated buildings
    # (no "eneff_f" value available for those)
    filter(!is.na(eneff_f)) %>%
    filter((eneff == eneff_f & fuel_heat == fuel_heat_f) |
      # Transitions between eneffs for existing buildings (without renovation)
      #  require the renovation switch "swt_ren" to be ON
      (eneff == eneff_f & ct_ren_fuel_heat == 1) |
      # Transitions between eneffs for renovations require the renovation
      #  switch "ct_ren_fuel_heat" to be ON (1).
      # Note:  swt_exst and  swt_ren replaced by one column only:
      #  ct_ren_fuel_heat
      (eneff != eneff_f & ct_ren_fuel_heat == 1)) %>%
    # Attach year (in the loop)
    mutate(year = yrs[i]) %>%
    # Add HH size
    left_join(hh_size) %>%
    # Add floor per capita
    left_join(floor_cap) %>%
    # Add lifetime ren construction (for investment: based on loan duration)
    left_join(lifetime_ren) %>%
    # Add investment costs for shell renovation
    left_join(cost_invest_ren_shell_i) %>%
    # Add investment costs for heating renovation
    left_join(cost_invest_ren_heat_i) %>% #
    # No renovation
    mutate_cond(eneff == eneff_f & fuel_heat == fuel_heat_f,
      cost_invest_ren_heat = 0) %>%
    # Calculate total investment costs
    mutate(cost_invest_hh = cost_invest_ren_heat + 
      (cost_invest_ren_shell * floor_cap * hh_size)) %>%
    # Operation costs after renovation
    left_join(en_hh_tot_ren_fin) %>%
    # Operation costs before renovation
    left_join(en_hh_tot_ren_init) %>%
    # Filter out hh with no operational cost
    # REMOVING ALL RECORDS WITH NO HEATING!!!
    filter(cost_op_m2_init > 0) %>%
    # Add operative costs (total)
    mutate(cost_op_hh = cost_op_m2 * floor_cap * hh_size) %>%
    # Add intangible costs for shell renovation
    left_join(cost_intang_ren_shell_i) %>%
    # Add intangible costs for heating renovation
    left_join(cost_intang_ren_heat_i %>% 
    rename(fuel_heat = fuel_heat_i)) %>%
    # Calculate total investment costs
    mutate(cost_intang_hh = cost_intang_ren_heat +
      (cost_intang_ren_shell * floor_cap * hh_size)) %>%
    # Add discount rates
    left_join(discount_ren) %>%
    # Add discount rates
    left_join(heterog_ren) %>%
    # Calculate LCC to new construction
    mutate(lcc_ren = fun_lcc(cost_invest_hh, cost_op_hh,
      cost_intang_hh, discount_ren, lifetime_ren)) %>%
    # Calculate operation lcc (for reporting)
    mutate(cost_op_hh_lcc = lcc_ren - cost_intang_hh - cost_invest_hh) %>%
    # Expon. nu
    mutate(lcc_ren_exp = fun_lcc_exp(lcc_ren, heterog_ren)) %>%
    # Rename eneff column
    rename(eneff_i = eneff) %>%
    # REMOVED IN v0.7
    rename(fuel_heat_i = fuel_heat) %>%
    # FILTERING BASED ON INTANGIBLE COSTS - NECESSARY?
    filter(cost_intang_ren_shell != 99999) %>%
    # FILTERING BASED ON INTANGIBLE COSTS - NECESSARY?
    filter(cost_intang_ren_heat != 99999) %>%
    select(-c(ct_ren_fuel_heat, ct_fuel_excl_ren,
      ct_fuel_excl_i_reg, ct_fuel_excl_f_reg))

  try(if (nrow(lcc_ren_hh) != nrow(distinct(lcc_ren_hh)))
    stop("Error in renovation calculation! Duplicated records in lcc_ren_hh"))

  ### MARKET SHARE - RENOVATIONS + FUEL SWITCHES
  # All possible combinations covered (including no renovation)
  # Totals market shares by eneff_i + fuel_heat_i = 1
  ms_i <- lcc_ren_hh %>%
    select(-c(
      "hh_size", "floor_cap", "cost_invest_ren_shell", "cost_invest_ren_heat",
      "cost_invest_hh", "cost_op_m2", "cost_op_m2_init",
      "cost_op_hh", "cost_intang_ren_heat", "cost_intang_ren_shell",
      "cost_intang_hh", "discount_ren",
      "lifetime_ren", "heterog_ren", "lcc_ren", "cost_op_hh_lcc"
    ))
  
  ms_i <- ms_i %>%
    # Select all variables, except "eneff_f" and "lcc_ren_hh_exp" for grouping)
    group_by_at(setdiff(names(ms_i),
      c("eneff_f", "fuel_heat_f", "lcc_ren_exp"))) %>%
    mutate(lcc_ren_exp_sum = sum(lcc_ren_exp)) %>%
    # ADD CHECK HERE
    # "eneff_f" categories in "lcc_ren_hh" should include all possible renovation categories!
    ungroup() %>% 
    mutate(ms_unwgt = lcc_ren_exp / lcc_ren_exp_sum) %>%
    # Market share - not weighted on share of hh tenure
    # Join data on tenure shares
    left_join(hh_tenure) %>%
    # Weight market share on tenures
    mutate(ms_wgt = ms_unwgt * hh_tenure) %>%
    # Select all variables, except "eneff" and "n_dem" for grouping)
    group_by_at(setdiff(names(ms_i),
      c("lcc_ren_exp", "lcc_ren_exp_sum", "tenr",
        "ms_wgt", "ms_unwgt", "hh_tenure"))) %>%
    summarise(ms = round(sum(ms_wgt), rnd)) %>%
    ungroup() %>%
    # Remove periods of construction
    select(-bld_age)

  # Renovation rate
  # REGIONS WITH NO HEATING ARE EXCLUDED BEFORE!
  rate_ren_i <- ms_i %>%
    filter(eneff_i == eneff_f) %>%
    mutate(ren_share_calc = 1 - ms) %>%
    mutate(rate_ren_calc = (1 - ms) / stp) %>%
    left_join(rate_ren_low) %>%
    left_join(rate_ren_high) %>%
    mutate(rate_ren = ifelse(rate_ren_calc < rate_ren_low, rate_ren_low,
      ifelse(rate_ren_calc > rate_ren_high, rate_ren_high, rate_ren_calc)
      )) %>%
    rename(eneff = eneff_i, fuel_heat = fuel_heat_i) %>%
    select(-c(
      eneff_f, fuel_heat_f, ms,
      # rate_ren,
      rate_ren_low, rate_ren_high,
      rate_ren_calc, ren_share_calc
    ))

  # Update market shares - keep renovations only
  ms_ren_i <- ms_i %>%
    filter(eneff_i != eneff_f) %>%
    group_by_at(setdiff(names(ms_i), c("eneff_f", "fuel_heat_f", "ms"))) %>%
    mutate(ms_tot = sum(ms)) %>%
    ungroup() %>%
    mutate(ms_ren = ifelse(ms_tot > 0, round(ms / ms_tot, rnd), 0)) %>%
    filter(ms_ren > 0) %>%
    select(-c(ms, ms_tot))

  # Update market shares - keep fuel switches only
  ms_sw_i <- ms_i %>%
    filter(eneff_i == eneff_f & fuel_heat_i != fuel_heat_f) %>%
    group_by_at(setdiff(names(ms_i), c("eneff_f", "fuel_heat_f", "ms"))) %>%
    mutate(ms_tot = sum(ms)) %>%
    ungroup() %>%
    mutate(ms = ifelse(ms_tot > 0, round(ms / ms_tot, rnd), 0)) %>%
    filter(ms > 0) %>%
    select(-c(ms_tot, eneff_f)) %>%
    rename(eneff = eneff_i) %>%
    rename(fuel_heat = fuel_heat_i)


  output <- list(
    ms_ren_i = ms_ren_i,
    ms_sw_i = ms_sw_i,
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
                                    bld_cases_fuel,
                                    ct_bld_age,
                                    rate_shell_ren_exo,
                                    ms_shell_ren_exo
                                    ) {
  print(paste0("Running renovation target - year ", yrs[i]))

  # Filter based on the construction period
  p_past <- ct_bld_age %>%
    filter(year_f < yrs[i]) %>%
    pull(bld_age_id)

  rate_ren_i <- bld_cases_fuel %>%
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

  ms_ren_i <- bld_cases_fuel %>%
    filter(bld_age %in% p_past) %>%
    # Add years columns
    mutate(year = yrs[i]) %>%
    # Rename eneff column
    rename(eneff_i = eneff) %>%
    rename(fuel_heat_i = fuel_heat) %>%
    # Join market share column
    left_join(ms_shell_ren_exo) %>%
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
                           bld_cases_fuel,
                           ct_bld_age,
                           ms_switch_fuel_exo) {
  print(paste0("Running fuel switch target - year ", yrs[i]))

  # Filter building age cohorts - past periods of construction
  p_past <- ct_bld_age %>%
    filter(year_f < yrs[i]) %>%
    pull(bld_age_id)

  ms_sw_i <- bld_cases_fuel %>%
   # Add years columns
    mutate(year = yrs[i]) %>%
    filter(bld_age %in% p_past) %>%
    left_join(ms_switch_fuel_exo %>%
      rename(ms = ms_switch_fuel_exo) %>%
      rename(fuel_heat_f = fuel_heat)
      ) %>%
    filter(!is.na(ms)) %>%
    filter(ms > 0)
    
  print(paste0("Completed fuel switch target - year ", yrs[i]))

  return(ms_sw_i)
}
