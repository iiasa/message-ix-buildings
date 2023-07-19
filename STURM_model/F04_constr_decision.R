library(dplyr)
library(tidyverse)

#' @title Function to generate market shares for new construction
#' @description Function to generate market shares for new construction
#' @param yrs vector of years
#' @param i index of the year
#' @param bld_cases_fuel dataframe with building cases
#' @param ct_bld_age dataframe with building age cohorts
#' @param ms_shell_new_exo dataframe with exogenous market shares for shell
#' @param ms_switch_fuel_exo dataframe with exogenous market shares
#'  for fuel switch
#' @return dataframe with market shares for new construction
fun_ms_new_exogenous <- function(yrs,
                              i,
                              stock_aggr,
                              ct_bld_age,
                              ct_hh_inc,
                              ct_fuel,
                              share_hh_tenr,
                              ms_shell_new_exo,
                              ms_switch_fuel_exo) {
  print(paste0("Running construction target - year ", yrs[i]))
  

  # Filter building age cohorts - current period of construction
  p_i <- ct_bld_age %>%
    filter(year_i <= yrs[i] & year_f >= yrs[i]) %>%
    select(bld_age_id, mat) %>%
    rename(bld_age = bld_age_id)


  # Test sum of ms_shell_new_exo equal to 1
  test <- ms_shell_new_exo %>% group_by_at(setdiff(names(ms_shell_new_exo),
            c("ms_shell_new_exo", "eneff"))) %>%
            summarise(total = sum(ms_shell_new_exo)) %>%
            ungroup()
  # All ms value of test should be equal to 1
  if (any(round(test$total, 2) != 1)) {
    print("Test failed. Market shares ms_shell_new_exo.")
  } else {
    print("Test passed. Market shares ms_shell_new_exo.")
  }
  
  # ms_switch_fuel_exo <- ms_switch_fuel_exo %>%
    # left_join(ct_fuel %>% select(fuel_heat, fuel) %>% distinct())

  # Test sum of ms_shell_new_exo equal to 1
  test <- ms_switch_fuel_exo %>% group_by_at("region_bld") %>%
            summarise(total = sum(ms_switch_fuel_exo)) %>%
            ungroup()
  # All ms value of test should be equal to 1
  if (any(round(test$total, 2) != 1)) {
    print("Test failed. Market shares ms_switch_fuel_exo.")
  } else {
    print("Test passed. Market shares ms_switch_fuel_exo.")
  }

  ms_new_i <- stock_aggr %>%
    select(-c(n_units_aggr, var_aggr)) %>%
    filter(year == yrs[i]) %>%
    left_join(p_i) %>%
    # Join market share column
    left_join(ms_shell_new_exo) %>%
    left_join(ms_switch_fuel_exo) %>%
    mutate(ms = ms_shell_new_exo * ms_switch_fuel_exo) %>%
    select(-c(ms_shell_new_exo, ms_switch_fuel_exo)) %>%
    filter(!is.na(ms), ms > 0) %>%
    mutate(fuel_cool = "electricity")

  # Add income level
  # Subdivising equally between three classes of income (q1, q2, q3)
  # Assumption of no correlation with other dimension

  shr_inc <- data.frame(
    mat = "perm",
    inc_cl = c("q1", "q2", "q3"),
    share = 1 / 3)

  ms_new_i <- ms_new_i %>%
    left_join(shr_inc) %>%
    mutate(ms = ms * share) %>%
    select(-share)

  # Add hh_tenure
  ms_new_i <- ms_new_i %>%
    left_join(share_hh_tenr) %>%
    mutate(ms = ms * hh_tenure) %>%
    select(-hh_tenure)


  # Test sum of ms_shell_new_exo equal to 1
  test <- ms_new_i %>% group_by_at(setdiff(names(ms_new_i),
            c("ms", "fuel_heat", "eneff", "inc_cl", "tenr"))) %>%
            summarise(total = sum(ms)) %>%
            ungroup()
  # All ms value of test should be equal to 1
  if (any(round(test$total, 2) != 1)) {
    print("Test failed. Market shares ms_new_i.")
  } else {
    print("Test passed. Market shares ms_new_i.")
  }

  print(paste0("Completed construction target - year ", yrs[i]))
  return(ms_new_i)
}