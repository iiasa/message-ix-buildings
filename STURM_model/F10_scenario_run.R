
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)

#' @param run: name of the scenario to run, e.g. "NAV_Dem-NPi-ref"
#' @param scenario_name: name of the scenario to run, e.g. "NAV_Dem-NPi-ref"
#' @param sector: sector to run, available: "com", "resid"
#' @param path_in: path to input data, e.g. "STURM_data/"
#' @param path_rcode: path to R code, e.g. "STURM_model/"
#' @param path_out: path to output data, e.g. "STURM_output/"
#' @param prices: prices to use, e.g. "input_prices_R12.csv"
#' @param file_inputs: file with input data, e.g. "input_list_resid.csv"
#' @param file_scenarios: file with scenarios, e.g. "scenarios_TEST.csv"
#' @param geo_level_report: level for reporting, e.g. "R12"
#' @param yrs: years to run, if NULL run all years, e.g. seq(2015, 2050, 5)
#' @param report_type: available reports,
#' available: "MESSAGE","STURM","IRP","NGFS","NAVIGATE"
#' @param report_var: available report variables,
#' available: "energy","material","vintage","dle"
#' @param region: region to run, if NULL run all regions
#' @return output data
run_scenario <- function(run,
                         scenario_name,
                         sector,
                         path_in,
                         path_rcode,
                         path_out,
                         file_inputs,
                         file_scenarios,
                         geo_level_report,
                         yrs,
                         report_type,
                         report_var,
                         region = NULL,
                         energy_efficiency = "endogenous",
                         en_method = "TABULA") {
  print(paste("Start scenario run: ", run))
  # Track time
  start_time <- Sys.time()


  # Loading functions
  print("Load functions")
  source(file.path(path_rcode, "B00_functions.R"))
  source(file.path(path_rcode, "F01_inputs.R"))
  source(file.path(path_rcode, "F03_energy_demand.R"))
  source(file.path(path_rcode, "F04_constr_decision.R"))
  source(file.path(path_rcode, "F05_renov_switch_decision.R"))
  source(file.path(path_rcode, "F05_endogenous_subsidies.R"))
  source(file.path(path_rcode, "F02_init_stock_dyn_fut.R"))
  source(file.path(path_rcode, "F06_stock_dyn_complete_rev.R"))
  source(file.path(path_rcode, "F07_formatting_output.R"))
  source(file.path(path_rcode, "F08_calibration.R"))

  print("Functions loaded.")
  # --------------------------------------------------------------
  print("Load data")

  path_out_detail <- NULL
  if (run == "EU") {
    path_out_detail <- path_out
  }

  # Loading categories
  path_in_csv <- paste0(path_in, "./input_csv/")
  cat <- read_categories(path_in_csv, sector, region)

  # Loading input data
  temp <- fun_inputs_csv(path_in, file_inputs,
    file_scenarios, sector, run, param)
  d <- temp$d
  param <- temp$param

  if (!is.null(path_out_detail)){
    # Calculate linear evolution of d$income between start and end year
    temp <- d$income %>%
      filter(year %in% c(yrs[[1]], yrs[[length(yrs)]])) %>%
      group_by(region_bld, inc_cl, year) %>%
      summarize(income = mean(income)) %>%
      ungroup() %>%
      pivot_wider(names_from = year, values_from = income) %>%
      mutate(evolution_rate =
        (`2050` / `2015`)^(1 / (yrs[[length(yrs)]] - yrs[[1]])) - 1) %>%
      select(-c(`2015`, `2050`))

    write.csv(temp, file.path(path_out_detail, "income_evolution.csv"),
      row.names = FALSE)

  }

  # Multiple cost by cost_factor if cost_invest_heat do not have cost_factor
  if (!"region_bld" %in% names(d$cost_invest_heat)) {
    d$cost_invest_heat <- d$cost_invest_heat %>%
      cross_join(d$cost_factor) %>%
      mutate(cost_invest_heat = cost_invest_heat * cost_factor) %>%
      select(-cost_factor)
  }

  # Ensure that renovation cost for "adv" renovation are higher than "std"
  temp <- d$cost_invest_ren_shell
  std_data <- temp %>% filter(eneff_f == "std")
  adv_data <- temp %>% filter(eneff_f == "adv")
  joined_data <- left_join(std_data, adv_data, by = "region_bld",
    suffix = c("_std", "_adv"))
  adjusted_data <- joined_data %>%
    rowwise() %>%
    mutate(cost_invest_ren_shell_adv =
      max(cost_invest_ren_shell_std, cost_invest_ren_shell_adv))
  final_data <- adjusted_data %>%
    select(region_bld, eneff_f_std = cost_invest_ren_shell_std,
      eneff_f_adv = cost_invest_ren_shell_adv) %>%
    pivot_longer(cols = -region_bld, names_to = "eneff_f",
      values_to = "cost_invest_ren_shell")
  final_data <- final_data %>%
    mutate(eneff_f = recode(eneff_f, "eneff_f_std" = "std",
      "eneff_f_adv" = "adv"))
  d$cost_invest_ren_shell <- final_data

  # Creating inertia data
  d$inertia <- d$cost_factor %>%
    mutate(cost_factor =
      cost_factor / cost_factor[region_bld == "C-WEU-FRA"]) %>%
    mutate(cost_factor = param$inertia_wtp * cost_factor) %>%
    rename(inertia = cost_factor)

  # Only selecting regions in geo_data that are in shr_fuel_heat_base
  cat$geo_data <- cat$geo_data %>%
    filter(region_bld %in% unique(d$shr_fuel_heat_base$region_bld))
  cat$regions <- unique(pull(cat$geo_data["region_bld"]))

  # Parsing share fuels data
  temp <- parse_share_fuels(d, cat)
  d <- temp$d
  cat <- temp$cat

  # Reading discount rate
  if (energy_efficiency == "endogenous") {
    if (!"tenr" %in% names(d$discount_rate)) {
      d$discount_rate_renovation <- d$discount_rate_renovation %>%
        rename(discount_rate = discount_rate_renovation)

      d$discount_rate_renovation <- crossing(d$discount_rate_renovation, cat$ct_hh_tenr) %>%
        rename(tenr = "cat$ct_hh_tenr") %>%
        # assuming that landlords have the same discount rate as high income
        mutate(discount_rate =
          ifelse(tenr == "rent",
          filter(d$discount_rate_renovation, inc_cl == "q3",
          region_bld == region_bld)$discount_rate,  discount_rate))

      d$discount_rate_heat <- d$discount_rate_heat %>%
        rename(discount_rate = discount_rate_heat)

      d$discount_rate_heat <- crossing(d$discount_rate_heat, cat$ct_hh_tenr) %>%
        rename(tenr = "cat$ct_hh_tenr") %>%
        # assuming that landlords have the same discount rate as high income
        mutate(discount_rate =
          ifelse(tenr == "rent",
          filter(d$discount_rate_heat, inc_cl == "q3",
          region_bld == region_bld)$discount_rate,  discount_rate))
    }
  }

  # Loading energy prices
  print("Load energy prices")
  price_en <- read_energy_prices(d$energy_prices_ini,
    d$energy_prices_projections,
    cat$geo_data, yrs[[1]], yrs[[length(yrs)]], path_out = path_out_detail)

  # Loading emission factors
  print("Load emission factors")
  emission_factors <-
    read_emission_factors(d$emission_factors, d$emission_ini,
      cat$geo_data, yrs[[1]])


  # Calculating carbon price for electricity and district heating
  if (!is.null(d$carbon_market)) {
    d$carbon_market  <- d$carbon_market  %>%
      rowwise() %>%
      mutate(fuel = list(c("electricity", "district_heat"))) %>%
      unnest(fuel)

    carbon_market <- d$carbon_market %>%
      left_join(emission_factors, relationship =
        "many-to-many") %>%
      mutate(carbon_market = 3.6 * carbon_market * emission_factors / 1e6) %>%
      select(-c(emission_factors, region_gea))

    price_en <- price_en %>%
      left_join(carbon_market) %>%
      mutate(carbon_market = ifelse(is.na(carbon_market), 0, carbon_market)) %>%
      mutate(price_en = price_en + carbon_market) %>%
      select(-carbon_market)
  }

  price_en_wt <- price_en %>%
    rename(price_en_wt = price_en)

  # Loading carbon tax for oil, gas and coal
  if (!is.null(d$carbon_tax)) {
    d$carbon_tax  <- d$carbon_tax  %>%
      rowwise() %>%
      mutate(fuel = list(c("oil", "gas", "coal"))) %>%
      unnest(fuel)

    carbon_tax <- d$carbon_tax %>%
      left_join(emission_factors, relationship =
        "many-to-many") %>%
      mutate(carbon_tax = 3.6 * carbon_tax * emission_factors / 1e6) %>%
      select(-c(emission_factors, region_gea))
    price_en <- price_en %>%
      left_join(carbon_tax) %>%
      mutate(carbon_tax = ifelse(is.na(carbon_tax), 0, carbon_tax)) %>%
      mutate(price_en = price_en + carbon_tax) %>%
      select(-carbon_tax)
  }

  # Realization rate of renovation
  d$en_sav_ren <- d$en_sav_ren %>%
    mutate(en_sav_ren = en_sav_ren * param$realization_rate_renovation)

  # Success objective renovation
  if (!is.null(d$objectives_renovation)) {
    d$objectives_renovation  <- d$objectives_renovation %>%
      mutate(objectives_renovation = objectives_renovation * param$success_objective_renovation)
  }

  # Adding year column to start subsidies after calibration
  if (!"year" %in% names(d$subsidies_renovation)) {
    d$subsidies_renovation <- crossing(d$subsidies_renovation, yrs) %>%
      rename(year = "yrs") %>%
      mutate(subsidies_renovation = ifelse(year <= yrs[[2]], 0, subsidies_renovation))
  }
  if (!"year" %in% names(d$sub_heat)) {
    d$sub_heat <- crossing(d$sub_heat, yrs) %>%
      rename(year = "yrs") %>%
      mutate(sub_heat = ifelse(year <= yrs[[2]], 0, sub_heat))
  }
  print("Data loaded.")
  # --------------------------------------------------------------

  # Create matrix of all dimensions
  bld_cases_fuel <- expand.grid(
    region_bld = cat$regions,
    urt = cat$urts,
    inc_cl = cat$ct_hh_inc,
    stringsAsFactors = FALSE
    ) %>%
    left_join(cat$geo_data %>%
      select_at(c("region_bld", "region_gea"))) %>%
    left_join(cat$clim_zones,
      by = c("region_bld", "urt"),
      relationship = "many-to-many") %>%
    left_join(cat$ct_bld,
      relationship = "many-to-many") %>%
    left_join(cat$ct_eneff, by = "mat",
      relationship = "many-to-many") %>%
    inner_join(cat$ct_fuel, by = c("mat" = "mat"),
      relationship = "many-to-many") %>%
    merge(as.data.frame(cat$ct_hh_tenr)) %>%
    rename("tenr" = "cat$ct_hh_tenr")

  # Residential sector
  if (sector == "resid") {
    print("Initializing housing stock")
    # Initialize housing stock aggregated
    stock_aggr <- fun_stock_aggr(
      sector,
      yrs,
      cat$geo_data,
      bld_cases_fuel,
      d$pop,
      d$hh_size,
      d$floor_cap,
      cat$ct_eneff,
      cat$ct_fuel,
      d$shr_mat,
      d$shr_arch
    )

    # Initialize housing stock detailed
    temp <- fun_stock_det_ini(sector,
                           stock_aggr,
                           d$stock_arch_base,
                           cat$geo_data,
                           cat$ct_eneff,
                           cat$ct_fuel,
                           d$ct_heat,
                           d$shr_fuel_heat_base,
                           d$hh_tenure,
                           report_var)
    bld_det_ini <- temp$bld_det_i
    report <- temp$report
    print(paste("Initial building stock based on detailed data:",
      round(sum(bld_det_ini$n_units_fuel) / 1e6, 0), "million units."))
    rm(temp)

    print("Calculating energy consumption at dwelling level")
    # Calculate energy need at dwelling level
    if (en_method == "TABULA") {

      en_sav_ren <- d$en_sav_ren %>%
          filter(year == yrs[[1]]) %>%
          select(-year)

      en_int_heat <- fun_space_heating_calculation(
        bld_cases_fuel,
        d$u_wall,
        d$u_roof,
        d$u_floor,
        d$u_windows,
        d$area_wall,
        d$area_roof,
        d$area_floor,
        d$area_windows,
        d$hdd,
        en_sav_ren
      )
      d$en_int_heat <- en_int_heat
    }

    # Calculation of energy demand demand for space heating and cooling
    temp <- fun_en_sim(
      sector,
      yrs,
      1,
      bld_cases_fuel,
      d$en_int_heat,
      d$en_int_cool,
      d$days_cool,
      d$eff_cool,
      d$eff_heat,
      d$hours_heat,
      d$shr_floor_heat,
      d$hours_cool,
      d$shr_floor_cool,
      d$hours_fans,
      d$power_fans,
      d$shr_acc_cool,
      d$hh_size,
      d$floor_cap,
      price_en,
      price_en_wt,
      d$income,
      d$pe_conversion_factor,
      bill_rebates = NULL,
      en_method = en_method,
      path_out = path_out,
      alpha = NULL,
      short_term_price_elasticity = param$short_term_price_elasticity
    )
    en_m2_scen_heat <- temp$en_m2_scen_heat
    en_m2_scen_cool <- temp$en_m2_scen_cool
    en_hh_tot <- temp$en_hh_tot
    rm(temp)

    # Calibration of energy consumption at base year
    print("Calibrating energy consumption function at the country level")
    alpha <- fun_calibration_consumption(bld_det_ini,
                                          en_hh_tot,
                                          d$hh_size,
                                          d$floor_cap,
                                          d$en_consumption,
                                          path_out)

    print("Recalculating energy consumption with new calibration")
    temp <- fun_en_sim(
      sector,
      yrs,
      1,
      bld_cases_fuel,
      d$en_int_heat,
      d$en_int_cool,
      d$days_cool,
      d$eff_cool,
      d$eff_heat,
      d$hours_heat,
      d$shr_floor_heat,
      d$hours_cool,
      d$shr_floor_cool,
      d$hours_fans,
      d$power_fans,
      d$shr_acc_cool,
      d$hh_size,
      d$floor_cap,
      price_en,
      price_en_wt,
      d$income,
      d$pe_conversion_factor,
      bill_rebates = NULL,
      en_method = en_method,
      path_out = path_out,
      alpha = alpha,
      short_term_price_elasticity = param$short_term_price_elasticity,
      factor_energy_behavior = param$factor_energy_behavior
    )
    en_m2_scen_heat <- temp$en_m2_scen_heat
    en_m2_scen_cool <- temp$en_m2_scen_cool
    en_hh_tot <- temp$en_hh_tot
    rm(temp)
    
    # Calculation of energy demand demand for hot water heating
    en_hh_hw_scen <- fun_hw_resid(
      yrs, 1,
      bld_cases_fuel,
      d$hh_size,
      d$eff_hotwater,
      d$en_int_hotwater,
      d$en_int_heat
    )

    # Calibration energy poverty
    print("Calibrating energy poverty function at the country level")
    threshold_poverty <- fun_calibration_poverty(bld_det_ini,
                                                en_hh_tot,
                                                d$energy_poverty,
                                                path_out)

    # Formatting output
    report <- fun_format_output(1,
                    yrs,
                    1,
                    sector,
                    run,
                    bld_det_ini,
                    bld_cases_fuel,
                    cat$ct_fuel,
                    d$shr_need_heat,
                    d$floor_cap,
                    d$hh_size,
                    d$pop,
                    report_var,
                    report,
                    en_m2_scen_heat,
                    en_m2_scen_cool,
                    en_hh_tot,
                    en_hh_hw_scen,
                    en_m2_hw_scen,
                    en_m2_others,
                    d$mat_int,
                    emission_factors,
                    d$emission_factors_embodied,
                    d$income,
                    d$pe_conversion_factor,
                    threshold_poverty = threshold_poverty)
    
    # Initializing calibration parameters for energy-efficiency decisions
    parameters_renovation <- NULL
    parameters_heater <- NULL
    # Initializing carbon revenu in case of recycling
    carbon_revenue <- NULL
    renovation_ambition <- NULL
    bill_rebates <- NULL

    # Initializing sub_report
    sub_report <- data.frame(value = numeric(),
      resolution = character(),
      variable = character(),
      type = character(),
      stringsAsFactors = FALSE)

    # Data store vintage of heating system by countries
    heater_vintage <- bld_det_ini %>%
      group_by(region_bld, fuel_heat) %>%
      summarize(n = sum(n_units_fuel)) %>%
      ungroup() %>%
      left_join(d$lifetime_heater) %>%
      select(-c("bld_age_min"))

    stp <- yrs[2] - yrs[1]
    heater_vintage <- heater_vintage %>%
      crossing(yrs[2:length(yrs)]) %>%
      rename("vintage" = "yrs[2:length(yrs)]") %>%
      mutate(n_vintage = ifelse(vintage <= yrs[1] + lifetime_heater,
        n * stp / lifetime_heater, 0)) %>%
      filter(n_vintage > 0) %>%
      select(c("region_bld", "fuel_heat", "vintage",
        "n_vintage", "lifetime_heater"))

    if (FALSE) {
      # Testing consistency of vintage stock
      test <- bld_det_ini %>%
        group_by_at(c("region_bld", "fuel_heat")) %>%
        summarize(n_units_fuel = sum(n_units_fuel)) %>%
        ungroup() %>%
        left_join(heater_vintage %>%
          group_by_at(c("region_bld", "fuel_heat")) %>%
          summarize(n_vintage = sum(n_vintage)) %>%
          ungroup())
    }

    # report_input <- 
    # d$energy_prices_ini, d$rate_shell_ren_exo, d$hdd
    
    print("End of initialization")
    # --------------------------------------------------------------
    # Loop over timesteps
    techno_heat_number <- data.frame()
    bld_det_i <- bld_det_ini
    for (i in 2:length(yrs)) {
      print(paste("0. Starting scenario run for year", yrs[i]))
      stp <- yrs[i] - yrs[i - 1]

      # Calculating carbon tax revenues for a period of stp years in Billion euro
      if (!is.null(d$carbon_tax)) {
        carbon_revenue <- d$carbon_tax %>%
          filter(year == yrs[i]) %>%
          mutate(year = year - stp) %>%
          left_join(report$agg_result %>%
            filter(variable == "heat_tCO2") %>%
            rename(fuel = resolution)) %>%
          filter(region_bld %in% unique(cat$geo_data$region_bld)) %>%
          mutate(revenue = carbon_tax * value * stp / 1e9) %>%
          filter(!is.na(revenue)) %>%
          group_by_at(c("region_bld")) %>%
          summarize(revenue = sum(revenue)) %>%
          ungroup()

        print(paste("Carbon revenue starting from year", yrs[i] - stp + 1, "until", yrs[i],"is",
          round(sum(carbon_revenue$revenue), 0), "Billion euro"))
      }

      # Initial quantity to quantify learning
      temp <- bld_det_i %>%
        group_by_at(c("fuel_heat", "year")) %>%
        summarize(n_previous = sum(n_units_fuel)) %>%
        ungroup()
      techno_heat_number <- bind_rows(techno_heat_number, temp)

      techno_heat_previous <- techno_heat_number %>%
        filter(year == yrs[i - 1] - stp) %>%
        select(-year)

      # If techno_heat_previous empty do not calculate learning
      if (nrow(techno_heat_previous) != 0 & yrs[i] >= 2030) {
        # Learning rate and reduction of investment costs
        learning_heat <- bld_det_i %>%
          group_by_at("fuel_heat") %>%
          summarize(n = sum(n_units_fuel)) %>%
          ungroup() %>%
          left_join(techno_heat_previous, by = "fuel_heat") %>%
          rename(fuel_heat_f = fuel_heat) %>%
          left_join(d$learning_rate_heat, by = "fuel_heat_f") %>%
          mutate(alpha = log(1 - learning_rate_heat) / log(2)) %>%
          mutate(learning_heat = (n / n_previous)^(alpha)) %>%
          mutate(learning_heat = ifelse(learning_heat > 1, 1, learning_heat)) %>%
          select(c("fuel_heat_f", "learning_heat"))

        # Add floor cost for heat-pumps. Cannot be lower than gas boilers.
        cost_invest_heat <- d$cost_invest_heat %>%
          left_join(learning_heat, by = "fuel_heat_f") %>%
          mutate(learning_heat = ifelse(is.na(learning_heat), 1, learning_heat)) %>%
          mutate(cost_invest_heat = cost_invest_heat * learning_heat) %>%
          select(-learning_heat)
        if (param$heat_pump_floor_cost) {
          cost_invest_heat <- cost_invest_heat %>%
            group_by(region_bld) %>%
            # Get cost_invest_heat for 'gas'
            mutate(cost_gas = ifelse(fuel_heat_f == "gas",
              cost_invest_heat, NA_real_)) %>%
            # Replace NA values with the cost_invest_heat of 'gas' for the region
            mutate(cost_floor = max(cost_gas, na.rm = TRUE)) %>%
            # Adjust cost_invest_heat for 'heat_pump'
            mutate(cost_invest_heat = ifelse(fuel_heat_f == "heat_pump",
                ifelse(cost_invest_heat < cost_floor,
                  cost_floor, cost_invest_heat), cost_invest_heat)) %>%
            select(-c("cost_gas", "cost_floor")) %>%
            ungroup()
        }
      } else {
        cost_invest_heat <- d$cost_invest_heat
      }

      print("Calculating energy demand")
      if (!is.null(param$sh_recycling_rebates)) {
        # Budget rebates is the carbon revenue for 5 years
        if (is.numeric(carbon_revenue)) {
            budget_rebates <- data.frame(
              year = yrs[i - 1],
              budget_rebates = param$sh_recycling_rebates *
                carbon_revenue * 1e9 / stp
          )
        } else {
          budget_rebates <- carbon_revenue %>%
            mutate(budget_rebates = revenue *
              param$sh_recycling_rebates * 1e9 / stp) %>%
            select(-revenue)

        }
      } else {
        budget_rebates <- data.frame(
          region_bld = unique(bld_det_i$region_bld),
          budget_rebates = 0
        )
      }

      bill_rebates <- bld_det_i %>%
        left_join(budget_rebates) %>%
        mutate(recycling_rebates = param$recycling_rebates) %>%
        group_by_at(names(budget_rebates)[names(budget_rebates) != "budget_rebates"]) %>%
        mutate(bill_rebates =
          ifelse(recycling_rebates == "lump_sum",
            budget_rebates / sum(n_units_fuel),
          ifelse((recycling_rebates == "low_income") & (inc_cl == "q1"),
            budget_rebates / sum(ifelse(inc_cl == "q1", n_units_fuel, 0)), 0))) %>%
        ungroup() %>%
        select(c("region_bld", "inc_cl", "bill_rebates")) %>%
        distinct()
      
      # Calculating energy demand intensities for hot water
      temp <- fun_en_sim(
        sector,
        yrs,
        i,
        bld_cases_fuel,
        d$en_int_heat,
        d$en_int_cool,
        d$days_cool,
        d$eff_cool,
        d$eff_heat,
        d$hours_heat,
        d$shr_floor_heat,
        d$hours_cool,
        d$shr_floor_cool,
        d$hours_fans,
        d$power_fans,
        d$shr_acc_cool,
        d$hh_size,
        d$floor_cap,
        price_en,
        price_en_wt,
        d$income,
        d$pe_conversion_factor,
        bill_rebates = bill_rebates,
        en_method = en_method,
        alpha = alpha,
        short_term_price_elasticity = param$short_term_price_elasticity,
        factor_energy_behavior = param$factor_energy_behavior
      )
      en_m2_scen_heat <- temp$en_m2_scen_heat
      en_m2_scen_cool <- temp$en_m2_scen_cool
      en_hh_tot <- temp$en_hh_tot
      rm(temp)

      # Calculating energy demand intensities for hot water
      en_hh_hw_scen <- fun_hw_resid(
        yrs, i,
        bld_cases_fuel,
        d$hh_size,
        d$eff_hotwater,
        d$en_int_hotwater,
        d$en_int_heat
      )

      # Calculating market share for new construction options
      print("Calculate market share for new construction")
      ms_new_i <- fun_ms_new_exogenous(
                    yrs,
                    i,
                    stock_aggr,
                    cat$ct_bld_age,
                    cat$ct_hh_inc,
                    cat$ct_fuel,
                    d$hh_tenure,
                    d$ms_shell_new_exo
      )
      try(if (nrow(ms_new_i) == 0)
        stop("Error in new construction calculation! Empty dataframe ms_new_i"))

      # Calculating stock turnover
      print(paste("1. Running stock turnover year:", yrs[i]))
      print("1.1 Accounting for demolition and empty buildings")
      temp <- fun_stock_turnover_dyn(i, yrs, bld_cases_fuel, cat$ct_bld_age,
                                    stock_aggr, bld_det_i, d$prob_dem,
                                    path_out = path_out,
                                    rate_dem_target = param$rate_dem_target)

      bld_aggr_i <- temp$bld_aggr_i
      bld_det_i <- temp$bld_det_i
      demolition_heater <- temp$demolition_heater
      report_turnover <- temp$report

      # Updating vintage to consider demolition
      heater_vintage <- heater_vintage %>%
        left_join(demolition_heater, by = c("region_bld", "fuel_heat")) %>%
        group_by(region_bld, fuel_heat) %>%
        mutate(n_dem = n_dem * n_vintage / sum(n_vintage)) %>%
        ungroup() %>%
        mutate(n_vintage = n_vintage - n_dem) %>%
        select(-n_dem)

      if (TRUE) {
        # Testing consistency of vintage stock
        test <- bld_det_i %>%
          group_by_at(c("region_bld", "fuel_heat")) %>%
          summarize(n_units_fuel_exst = sum(n_units_fuel_exst)) %>%
          ungroup() %>%
          left_join(heater_vintage %>%
            group_by_at(c("region_bld", "fuel_heat")) %>%
            summarize(n_vintage = sum(n_vintage)) %>%
            ungroup())
      }

      # Calculating stock for new construction options
      print("1.2 Calculating construction of new buildings")
      new_det_i <- fun_stock_construction_dyn(bld_aggr_i,
                                              ms_new_i,
                                              cat$ct_fuel)

      # Endogenous energy efficiency decisions
      print("2. Shell renovation decisions")
      # Calibrating renovation decision function
      if (energy_efficiency == "endogenous" && i == 2) {
        print("2.0 Calibration of renovation rate")
        if ("parameters_renovation" %in% names(d)) {
          parameters_renovation <- d$parameters_renovation %>%
            rename(constant = parameters_renovation)
        } else {
          parameters_renovation <- fun_calibration_ren_shell(yrs,
                          i,
                          bld_det_ini,
                          cat$ct_bld_age,
                          cat$ct_ren_eneff,
                          d$hh_size,
                          d$floor_cap,
                          d$cost_invest_ren_shell,
                          d$lifetime_ren,
                          en_hh_tot,
                          d$rate_shell_ren_exo,
                          d$ms_shell_ren_exo,
                          d$barriers_mfh,
                          d$barriers_rent,
                          stp,
                          path_out,
                          d$discount_rate_renovation,
                          param$social_discount_rate,
                          d$income,
                          elasticity_renovation = param$elasticity_renovation,
                          credit_constraint = param$credit_constraint
                        )
        }
      }
      if (param$remove_barriers_renovation) {
        print("Removing barriers for renovation")
        # Assigning constant value of "std" to "adv" parameters
        parameters_renovation <- parameters_renovation %>%
          mutate(barrier_rent = 0, barrier_mfh = 0)
        param$credit_constraint <- FALSE
        # put minimum value by region
        d$discount_rate_renovation <- d$discount_rate_renovation %>%
          group_by(region_bld) %>%
          mutate(discount_rate = min(discount_rate)) %>%
          ungroup()
      }
      # Calculating renovation decisions
      if (energy_efficiency == "endogenous") {
        print("2.1 Calculation of renovation rate")
        if ((isTRUE(param$renovation_intensity == "deep_renovation") | isTRUE(grepl("deep", param$objective_renovation))) && i == 3) {
          print("Removing barriers for deep renovation")
          # Assigning constant value of "std" to "adv" parameters
          parameters_std <- filter(parameters_renovation,
            eneff_f == "std")
          parameters_adv <- mutate(parameters_std, "eneff_f" = "adv")
          parameters_std <- mutate(parameters_std, "constant" = NA)
          parameters_renovation <- bind_rows(parameters_std,
            parameters_adv)
        } else {
          # Do nothing
          print("No changes in renovation barriers")
        }

        # Calculating subsidies to meet objectives
        sub <- NULL
        if (!is.null(param$objective_renovation)) {
          print("Endogenous subsidies renovation")
          objectives_endogenous <- NULL
          if (!is.null(param$repartition_renovation) && i >= 3) {
            print("Determining repartition of renovation objectives by region")
            if (param$repartition_renovation == "potential_based") {
                # Calculating renovation potential
                renovation_potential <- bld_det_i %>%
                  mutate(year = yrs[i]) %>%
                  filter(bld_age %in% c("p1", "p2", "p3")) %>%
                  filter(eneff == "avg") %>%
                  left_join(en_hh_tot) %>%
                  left_join(d$objectives_renovation) %>%
                  # Only buildings with energy consumption > 80 kWh/m2
                  filter(en_pe_hh_std > 80) %>%
                  # Sort by energy consumption higher consumption first
                  arrange(desc(en_hh)) %>%
                  # Cumulated number of dwellings
                  mutate(n_units_fuel_exst_cum = cumsum(n_units_fuel_exst)) %>%
                  # Filter to the number of renovation objectives_renovation
                  filter(n_units_fuel_exst_cum <= objectives_renovation) %>%
                  group_by_at(c("region_bld", "year")) %>%
                  summarize(renovation_potential = sum(n_units_fuel_exst)) %>%
                  ungroup()


                if ("region_bld" %in% names(d$objectives_renovation)) {
                  print("Aggregating renovation objectives before using new repartition")
                  d$objectives_renovation <- d$objectives_renovation %>%
                    group_by_at("year") %>%
                    summarize(objectives_renovation = sum(objectives_renovation)) %>%
                    ungroup()
                }

                objectives_endogenous <- d$objectives_renovation %>%
                  left_join(renovation_potential) %>%
                  filter(year == yrs[i]) %>%
                  mutate(objectives_renovation = objectives_renovation * renovation_potential
                    / sum(renovation_potential)) %>%
                  select(-renovation_potential)

            } else {
              print("Repartion is not implemented")
            }
          }
          if (is.null(objectives_endogenous)) {
            objectives_endogenous <- d$objectives_renovation
          }

          if (yrs[[i]] %in% unique(objectives_endogenous$year)) {

            if ("region_bld" %in% names(objectives_endogenous)) {
              print("Objectives for renovation are set per country")
              for (region in unique(objectives_endogenous$region_bld)) {
              #for (region in c("C-WEU-PRT")) {
                print(region)
                bld_det_i_region <- filter(bld_det_i, region_bld == region)

                temp <- fun_subsidies_renovation(i,
                            yrs,
                            stp,
                            param$objective_renovation,
                            param$budget_constraint_insulation,
                            carbon_revenue,
                            param$subsidies_renovation_type,
                            d$subsidies_renovation_target,
                            d,
                            cat,
                            param,
                            bld_det_i_region,
                            en_hh_tot,
                            parameters_renovation,
                            emission_factors,
                            param$anticipate_renovation,
                            region = region,
                            objectives_endogenous = objectives_endogenous)

                sub_region <- temp$sub %>%
                  mutate(region_bld = region)
                sub <- bind_rows(sub, sub_region)
                sub_report_region <- temp$sub_report %>%
                  mutate(region_bld = region)
                sub_report <- bind_rows(sub_report, sub_report_region)
              }
            } else {
              print("Objectives for renovation are set for all EU and based on subsidies")
              temp <- fun_subsidies_renovation(i,
                            yrs,
                            stp,
                            param$objective_renovation,
                            param$budget_constraint_insulation,
                            carbon_revenue,
                            param$subsidies_renovation_type,
                            d$subsidies_renovation_target,
                            d,
                            cat,
                            param,
                            bld_det_i,
                            en_hh_tot,
                            parameters_renovation,
                            emission_factors,
                            param$anticipate_renovation,
                            region = NULL,
                            objectives_endogenous = objectives_endogenous)
              sub <- temp$sub
              sub_report <- bind_rows(sub_report, temp$sub_report)
            }
          } else {
            print(paste("No objectives for renovation in year", yrs[i]))
          }
        } else {
          print("Using exogenous subsidies renovation")
          sub <- filter(d$subsidies_renovation, year == yrs[i])
        }

        temp <- fun_ms_ren_shell_endogenous(yrs,
                          i,
                          bld_cases_fuel,
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
                          subsidies_renovation_type = param$subsidies_renovation_type,
                          parameters = parameters_renovation,
                          emission_factors = emission_factors,
                          anticipate_renovation = param$anticipate_renovation)
    
      } else {
        temp <- fun_ms_ren_shell_exogenous(
                      yrs,
                      i,
                      bld_cases_fuel,
                      cat$ct_bld_age,
                      d$rate_shell_ren_exo,
                      d$ms_shell_ren_exo)
      }
      ms_ren_i <- temp$ms_ren_i
      rate_ren_i <- temp$rate_ren_i
      anticipate <- temp$anticipate
      rm(temp)

      # Integration of renovation in the existing stock
      print("2.2 Integration of renovation in the existing stock")
      temp <- fun_stock_renovation_dyn(bld_det_i,
                                      rate_ren_i,
                                      ms_ren_i,
                                      stp,
                                      anticipate)
      bld_det_i <- temp$bld_det_i
      ren_det_i <- temp$ren_det_i
      rm(temp)
      bld_det_i <- bind_rows(bld_det_i, new_det_i)

      # Test consistency of stock turnover
      if (round(sum(bld_det_i$n_units_fuel) / 1e6, 1) !=
        round(sum(filter(stock_aggr, year == yrs[i])$n_units_aggr) / 1e6, 1)) {
        stop("Error in stock turnover calculation! 
          Sum of new and existing buildings is not equal to the total stock.")
      } else {
        print("Test stock passed. Stock update.")
      }
      
      # Calculating replacement of heating system decisions
      print("3. Replacement heating system decisions")
      # Calibrating switch heating system function
      if (energy_efficiency == "endogenous" && i == 2) {
        print("3.0 Calibration of market shares installation boilers")
        if ("parameters_heater" %in% names(d)) {
          parameters_heater <- d$parameters_heater %>%
            rename(constant = parameters_heater)
          temp <- bld_cases_fuel %>%
            select(c("region_bld", "fuel_heat")) %>%
            distinct() %>%
            rename(fuel_heat_f = fuel_heat) %>%
            left_join(parameters_heater) %>%
            mutate(ct_fuel_excl_reg = ifelse(is.na(constant),
              1, 0)) %>%
            filter(ct_fuel_excl_reg == 1) %>%
            select(c("region_bld", "fuel_heat_f", "ct_fuel_excl_reg"))
          d$ct_fuel_excl_reg <- bind_rows(d$ct_fuel_excl_reg, temp) %>%
            distinct()
          
        } else {
          temp <- fun_calibration_switch_heat(yrs,
                    i,
                    bld_det_i,
                    cat$ct_bld_age,
                    d$ct_switch_heat,
                    d$ct_fuel_excl_reg,
                    d$cost_invest_heat,
                    en_hh_tot,
                    d$ms_switch_fuel_exo,
                    d$ct_heat,
                    d$ct_heat_new,
                    path_out,
                    d$discount_rate_heat,
                    heater_vintage,
                    inertia = d$inertia,
                    emission_factors = emission_factors)
          parameters_heater <- temp$parameters_heater
          d$ct_fuel_excl_reg <- temp$ct_fuel_excl_reg
        }
      }

      if (param$remove_barriers_heater) {
        print("Removing barriers for renovation")
        # Assigning constant value of "std" to "adv" parameters
        d$inertia <- d$inertia %>%
          mutate(inertia = 0)
        d$discount_rate_heat <- d$discount_rate_heat %>%
          group_by(region_bld) %>%
          mutate(discount_rate = min(discount_rate)) %>%
          ungroup()
      }


      # Calculating switch heating system decisions
      if (energy_efficiency == "endogenous") {
        print("3.1 Calculate market share for fuel switches")
        # Calculating subsidies to meet objectives
        budget <- NULL
        if (!is.null(param$budget_constraint_heater)) {
          if (param$budget_constraint_heater == "carbon_revenue") {
            budget <- sum(carbon_revenue$revenue)
            if (!is.null(budget)) {
              budget <- budget * param$sh_recycling_subsidies
            }
          } else if (budget_constraint_heater == "exogenous") {
            budget <- d$budget_constraint_heater
          } else {
            print("Cannot read budget_constraint_heater")
          }
        }

        sub <- NULL
        if (!is.null(budget)) {
          if (budget > 0) {
            temp <- fun_subsidies_heater(i,
                                        yrs,
                                        stp,
                                        param$sub_heater_type,
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
                                        param$premature_replacement,
                                        sub_report)
            sub <- temp$sub
            sub_report <- temp$sub_report
          }
        } else {
          sub <- filter(d$sub_heat, year == yrs[i])
        }

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
                          sub_heater_type = param$sub_heater_type,
                          emission_factors = emission_factors,
                          premature_replacement = param$premature_replacement)
        ms_sw_i <- temp$ms_i
        premature <- temp$premature

      } else {
        ms_sw_i <- fun_ms_fuel_sw_exogenous(yrs,
                          i,
                          bld_det_i,
                          cat$ct_bld_age,
                          d$ms_switch_fuel_exo)
      }
      # Integration of switch fuel in the stock
      print("3.2 Integration of switch fuel in the stock")
      temp <- fun_stock_switch_fuel_dyn(
        bld_det_i,
        heater_vintage,
        ms_sw_i,
        cat$ct_fuel,
        stp,
        yrs[i],
        ren_det_i,
        mandatory_switch = param$mandatory_switch,
        premature = premature)
      bld_det_i <- temp$bld_det_i
      bld_det_i_sw <- temp$bld_det_i_sw
      premature <- temp$premature
      rm(temp)

      # Remove from the vintage tracker heater that were replaced
      heater_vintage <- filter(heater_vintage, vintage > yrs[i])

      # Remove from the vintage tracker heater that were prematurely replaced
      if (!is.null(premature)) {
        temp <- premature %>%
          group_by(region_bld, fuel_heat) %>%
          summarize(n_dem = sum(n_premature)) %>%
          ungroup()

        heater_vintage <- heater_vintage %>%
          left_join(temp) %>%
          group_by(region_bld, fuel_heat) %>%
          mutate(n_dem = n_dem * n_vintage / sum(n_vintage)) %>%
          ungroup() %>%
          mutate(n_dem = ifelse(is.na(n_dem), 0, n_dem)) %>%
          mutate(n_vintage = n_vintage - n_dem) %>%
          select(-n_dem)
      }

      # Updating vintage to consider new installation
      temp <- bld_det_i_sw %>%
        group_by(region_bld, fuel_heat) %>%
        summarize(n_vintage = sum(n_units_fuel)) %>%
        ungroup() %>%
        left_join(d$lifetime_heater) %>%
        mutate(vintage = yrs[i] + lifetime_heater) %>%
        select(c("region_bld", "fuel_heat", "vintage",
          "n_vintage", "lifetime_heater"))
          
      heater_vintage <- bind_rows(heater_vintage, temp) %>%
        arrange(region_bld, fuel_heat, vintage)

      if (FALSE) {
        test <- bld_det_i %>%
          group_by_at(c("region_bld", "fuel_heat")) %>%
          summarize(n_units_fuel = sum(n_units_fuel)) %>%
          ungroup() %>%
          left_join(heater_vintage %>%
            group_by_at(c("region_bld", "fuel_heat")) %>%
            summarize(n_vintage = sum(n_vintage)) %>%
            ungroup())
      }

      # Test consistency of stock turnover
      if (round(sum(bld_det_i$n_units_fuel) / 1e6, 0) !=
        round(sum(filter(stock_aggr, year == yrs[i])$n_units_aggr) / 1e6, 0)) {
        stop("Error in stock turnover calculation! 
          Sum of new and existing buildings is not equal to the total stock.")
      } else {
        print("Test stock passed. Stock turnover is consistent.")
      }


      # Creating report from detailed stock
      report <- fun_format_output(i,
                              yrs,
                              stp,
                              sector,
                              run,
                              bld_det_i,
                              bld_cases_fuel,
                              cat$ct_fuel,
                              d$shr_need_heat,
                              d$floor_cap,
                              d$hh_size,
                              d$pop,
                              report_var,
                              report,
                              en_m2_scen_heat,
                              en_m2_scen_cool,
                              en_hh_tot,
                              en_hh_hw_scen,
                              en_m2_hw_scen,
                              en_m2_others,
                              d$mat_int,
                              emission_factors,
                              d$emission_factors_embodied,
                              d$income,
                              d$pe_conversion_factor,
                              threshold_poverty = threshold_poverty,
                              new_det_i = new_det_i,
                              ren_det_i = ren_det_i,
                              bld_det_i_sw = bld_det_i_sw,
                              report_turnover = report_turnover,
                              alpha = alpha,
                              short_term_price_elasticity = param$short_term_price_elasticity,
                              utility_money = distinct(select(parameters_renovation, c(region_bld, scaling_factor))))


      if (is.null(renovation_ambition)) {
        renovation_ambition <- report$agg_result %>%
          filter(region_bld == "EU", year == 2020, resolution == "all") %>%
          filter(variable %in%
            c("n_renovation", "stock_building")) %>%
          pivot_wider(id_cols = c(region_bld, year, resolution),
            names_from = variable,
            values_from = value) %>%
          mutate(renovation_rate = n_renovation / stock_building / stp) %>%
          pull(renovation_rate)
      }
    }
  }

  write.csv(report$agg_result,
      paste0(path_out, "report_agg_", scenario_name, ".csv"))

  write.csv(sub_report,
    paste0(path_out, "report_subsidies_", scenario_name, ".csv"))

  print("Scenario run completed!")
  duration <- difftime(Sys.time(), start_time, units = "secs")
  print(paste("Time to run scenario:", round(duration / 60, 0), "minutes."))

}
