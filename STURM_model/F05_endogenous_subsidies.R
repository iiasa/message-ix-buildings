library(dplyr)


fun_subsidies_renovation <- function(i,
  yrs,
  stp,
  objective_renovation,
  budget_constraint_insulation,
  carbon_revenue,
  subsidies_renovation_type,
  subsidies_renovation_target,
  d,
  cat,
  param,
  bld_det_i,
  en_hh_tot,
  parameters_renovation,
  emission_factors,
  anticipate_renovation,
  region = NULL,
  objectives_endogenous = NULL
  ) {

  budget_renovation_function <- function(x, objective, objective_renovation,
                                  subsidies_renovation_target,
                                  target = c("std", "adv")) {
      sub <- data.frame(subsidies_renovation = x, eneff_f = target,
        year = yrs[i]) %>%
        left_join(subsidies_renovation_target,
          relationship = "many-to-many") %>%
        mutate(subsidies_renovation =
          subsidies_renovation * subsidies_renovation_target) %>%
        select(-c("subsidies_renovation_target"))

      temp <- fun_ms_ren_shell_endogenous(yrs,
            i,
            bld_det_i,
            cat$ct_bld_age,
            cat$ct_ren_eneff,
            d$hh_size,
            d$floor_cap,
            d$cost_invest_ren_shell,
            sub,
            en_hh_tot,
            d$lifetime_ren,
            d$discount_rate_renovation,
            param$social_discount_rate,
            d$income,
            param$credit_constraint,
            subsidies_renovation_type = subsidies_renovation_type,
            parameters = parameters_renovation,
            emission_factors = emission_factors,
            anticipate_renovation = anticipate_renovation)

      anticipate <- temp$anticipate

      temp <- fun_stock_renovation_dyn(
        bld_det_i,
        temp$rate_ren_i,
        temp$ms_ren_i,
        stp,
        anticipate = anticipate,
        verbose = FALSE)
      
      ren_det_i <- temp$ren_det_i %>%
        mutate(subsidies = n_units_fuel * sub_ren_hh)

      if (FALSE) {
        sum <- sum(filter(bld_det_i, bld_age != "p5",
          eneff %in% c("avg"))$n_units_fuel_exst)
      } else {
        sum <- sum(bld_det_i$n_units_fuel_exst)
      }


      if (objective_renovation == "budget") {
        rslt <- sum(ren_det_i$subsidies) * 1e3 / 1e9
      } else if (grepl("rate", objective_renovation)) {
        rslt <- sum(ren_det_i$n_units_fuel) / sum / step_year
      } else if (grepl("renovation", objective_renovation)) {
        rslt <- sum(ren_det_i$n_units_fuel) / step_year
      } else {
        print("NOT IMPLEMENTED: objective_renovation")
      }

      return(rslt - objective)
  }

  if (objective_renovation == "budget") {
    objective <- NULL
    if (!is.null(budget_constraint_insulation)) {
      if (budget_constraint_insulation == "carbon_revenue") {
        objective <- carbon_revenue
      } else if (budget_constraint_insulation == "exogenous") {
        objective <- d$budget_constraint_insulation
      } else {
        print("Cannot read budget_constraint_insulation")
      }
    }
  } else if (grepl("renovation", objective_renovation)) {
      if (!is.null(region)) {
        objective <- filter(objectives_endogenous, region_bld == region)
      } else {
        objective <- objectives_endogenous
      }
    objective <- filter(objective,
      year == yrs[i]) %>% pull(objectives_renovation)
    
    target <- c("std", "adv")
  } else {
    print("NOT IMPLEMENTED: objective_renovation")
  }

  if (grepl("deep", objective_renovation)) {
    target <- c("adv")
  }

  if (subsidies_renovation_type == "ad_valorem") {
    interval <- c(0, 0.99)
  } else if (subsidies_renovation_type == "per_CO2") {
    interval <- c(0, 2000)
  } else if (subsidies_renovation_type == "per_kWh") {
    interval <- c(0.01, 1)
  } else {
    print("NOT IMPLEMENTED: subsidies_renovation_type")
  }

  if ("year" %in% colnames(subsidies_renovation_target)) {
    subsidies_renovation_target <-
      subsidies_renovation_target %>% filter(year == yrs[i])
  } else {
    subsidies_renovation_target <-
      subsidies_renovation_target %>% mutate(year = yrs[i])
  }

  if (budget_renovation_function(interval[2], objective,
    objective_renovation, subsidies_renovation_target, target) <= 0) {
    val <- interval[2]
  } else if (budget_renovation_function(interval[1], objective,
    objective_renovation, subsidies_renovation_target, target) >= 0) {
    val <- interval[1]
  } else {
    root <- uniroot(budget_renovation_function,
      interval = interval, objective = objective,
      objective_renovation = objective_renovation,
      target = target,
      subsidies_renovation_target = subsidies_renovation_target)
    val <- root$root
  }

  if (subsidies_renovation_type == "ad_valorem") {
    print(paste0("Subsidies renovation shell: ", round(val * 100, 3), "%"))
  } else if (subsidies_renovation_type == "per_kWh") {
    print(paste0("Subsidies renovation shell: ", round(val, 3), "EUR/kWh"))
  } else if (subsidies_renovation_type == "per_CO2") {
    print(paste0("Subsidies renovation shell: ", round(val, 0), "EUR/tCO2"))
  } else {
    print("No subsidies type for renovation")
  }

  sub <- data.frame(subsidies_renovation = val,
    eneff_f = target,
    year = yrs[i]) %>%
    left_join(subsidies_renovation_target, relationship = "many-to-many") %>%
    mutate(subsidies_renovation =
      subsidies_renovation * subsidies_renovation_target) %>%
    select(-c("subsidies_renovation_target"))
      
  temp <- colnames(sub)[!colnames(sub) %in% c("subsidies_renovation", "year")]
  sub$resolution <- apply(sub[, all_of(temp)], 1, function(row) paste(row, collapse = "-"))
  sub_report <- sub %>%
    select(-all_of(temp)) %>%
    rename(value = subsidies_renovation) %>%
    mutate(variable = "insulation",
      type = subsidies_renovation_type)
  sub <- sub %>% select(-resolution)
    
  return(list(sub = sub, sub_report = sub_report))

}

fun_subsidies_heater <- function(i,
  yrs,
  stp,
  sub_heater_type,
  budget,
  bld_det_i,
  ren_det_i,
  d,
  cat,
  param,
  en_hh_tot,
  cost_invest_heat,
  parameters_heater,
  heater_vintage,
  emission_factors,
  premature_replacement,
  sub_report
  ) {

  print("Determining level of subsidies for heat-pumps")

  budget_switch_function <- function(x, budget) {

    sub <- data.frame(sub_heat = x,
                      fuel_heat_f = "heat_pump",
                      fuel_heat = c("oil", "gas", "coal"),
                      year = yrs[i])

    temp <- fun_ms_switch_heat_endogenous(yrs,
                      i,
                      bld_det_i,
                      cat$ct_bld_age,
                      d$ct_switch_heat,
                      d$ct_fuel_excl_reg,
                      cost_invest_heat,
                      sub,
                      en_hh_tot,
                      d$ct_heat,
                      d$ct_heat_new,
                      d$discount_rate_heat,
                      param$social_discount_rate,
                      lifetime_heat = 20,
                      inertia = d$inertia,
                      parameters = parameters_heater,
                      ban_fuel = d$ban_fuel,
                      ban_fuel_renovation = d$ban_fuel_renovation,
                      sub_heater_type = sub_heater_type,
                      emission_factors = emission_factors,
                      premature_replacement = premature_replacement)
    ms_sw_i <- temp$ms_i
    premature <- temp$premature

    temp <- fun_stock_switch_fuel_dyn(bld_det_i,
                              heater_vintage,
                              ms_sw_i,
                              cat$ct_fuel,
                              stp,
                              yrs[i],
                              ren_det_i,
                              mandatory_switch = FALSE,
                              premature = premature,
                              verbose = FALSE)
    bld_det_i_sw <- temp$bld_det_i_sw %>%
      mutate(subsidies = n_units_fuel * sub_heat_hh) %>%
      mutate(cost = n_units_fuel * cost_invest_heat)
    
    number_hp <- sum(filter(bld_det_i_sw,
      fuel_heat == "heat_pump")$n_units_fuel, na.rm = TRUE) / 1e3
    rslt <- sum(bld_det_i_sw$subsidies, na.rm = TRUE) / 1e9

    return(rslt - budget)
  }

  if (sub_heater_type == "ad_valorem") {
    interval <- c(0, 0.95)
  } else if (sub_heater_type == "per_CO2") {
    interval <- c(0, 2000)
  } else if (sub_heater_type == "per_kWh") {
    interval <- c(0.01, 1)
  } else {
    print("NOT IMPLEMENTED: subsidies_renovation_type")
  }

  # budget_switch_function(0, budget=0)

  root <- uniroot(budget_switch_function,
    interval = interval, budget = budget, tol = 0.01)

  if (sub_heater_type == "ad_valorem") {
    print(paste0("Subsidies heater: ",
      round(root$root * 100, 0), "%"))

  } else if (sub_heater_type == "per_CO2") {
    print(paste0("Subsidies heater: ",
      round(root$root, 0), "EUR/tCO2"))
  } else {
    print("No subsidies type for renovation")
  }

  sub <- data.frame(sub_heat = root$root,
                    fuel_heat_f = "heat_pump",
                    fuel_heat = c("oil", "gas", "coal"),
                    year = yrs[i])
  temp <- sub %>%
    rename(resolution = fuel_heat_f,
      value = sub_heat) %>%
    mutate(variable = "heater",
      type = sub_heater_type)
  sub_report <- bind_rows(sub_report, temp)

  return(list(sub = sub, sub_report = sub_report))
}