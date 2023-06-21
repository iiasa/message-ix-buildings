### WRAPPER FUNCTION

library(tidyverse)
library(readxl)
library(readr)
library(dplyr)


# require(devtools) # source code from github
# library(RCurl) # read file from github


#' @param run: name of the scenario to run, e.g. "NAV_Dem-NPi-ref"
#' @param scenario_name: name of the scenario to run, e.g. "NAV_Dem-NPi-ref"
#' @param sector: sector to run, available: "com", "resid"
#' @param path_in: path to input data, e.g. "STURM_data/"
#' @param path_rcode: path to R code, e.g. "STURM_model/"
#' @param path_out: path to output data, e.g. "STURM_output/"
#' @param prices: prices to use, e.g. "input_prices_R12.csv"
#' @param file_inputs: file with input data, e.g. "input_list_resid.csv"
#' @param file_scenarios: file with scenarios, e.g. "scenarios_TEST.csv"
#' @param geo_level: level for analysis, e.g. "region_bld"
#' @param geo_level_aggr: level for aggregation, e.g. "region_gea"
#' @param geo_levels: levels to keep track of, e.g. c("region_bld", "region_gea")
#' @param geo_level_report: level for reporting, e.g. "R12"
#' @param yrs: years to run, if NULL run all years, e.g. seq(2015, 2050, 5)
#' @param input_mode: input mode, available: "csv", "xlsx"
#' @param mod_arch: model architecture, available: "stock", "new"
#' @param report_type: available reports, available: "MESSAGE","STURM","IRP","NGFS","NAVIGATE"
#' @param report_var: available report variables, available: "energy","material","vintage","dle"
#' @param region: region to run, if NULL run all regions
#' @import tidyverse
#' @import readxl
#' @import RCurl
run_scenario <- function(run,
                         scenario_name,
                         sector,
                         path_in,
                         path_rcode,
                         path_out,
                         prices,
                         file_inputs,
                         file_scenarios,
                         geo_level,
                         geo_level_aggr,
                         geo_levels,
                         geo_level_report,
                         yrs,
                         input_mode,
                         mod_arch,
                         report_type,
                         report_var,
                         region = NULL) {
  print(paste("Start scenario run: ", run, "_", sector))
  # Track time
  start_time <- Sys.time()

  # Unit conversion
  u_EJ_GWa <- 31.71

  # SOURCE MODEL FUNCTIONS
  print("Load functions")
  source(file.path(path_rcode, "B00_functions.R"))
  source(file.path(path_rcode, "F01_inputs.R"))
  source(file.path(path_rcode, "F03_energy_demand.R"))
  source(file.path(path_rcode, "F04_constr_decision.R"))
  # source(file.path(path_rcode, "F05_renov_decision.R"))
  source(file.path(path_rcode, "F05_renov_switch_decision.R"))
  source(file.path(path_rcode, "F02_init_stock_dyn_fut.R"))
  source(file.path(path_rcode, "F06_stock_dyn_complete_rev.R"))
  
  if ("STURM" %in% report_type) {
    source(file.path(path_rcode, "R00_report_basic.R"))
  }
  if ("MESSAGE" %in% report_type) {
    source(file.path(path_rcode, "R01_report_MESSAGE.R"))
  }
  if ("IRP" %in% report_type) {
    source(file.path(path_rcode, "R02_report_IRP.R"))
  }
  if ("NGFS" %in% report_type) {
    source(file.path(path_rcode, "R03_report_NGFS.R"))
  }
  source(file.path(path_rcode, "R05_report_NAVIGATE.R"))
  print("Functions loaded!")

  # LOAD INPUT DATA

  if (input_mode == "csv") {
    print("Load data - csv")

    # Source - input data
    d <- fun_inputs_csv(path_in, file_inputs, file_scenarios, sector, run)

    print("Data loaded!")
  }

  #### FROM HERE: MOVE AND RE-ORGANIZE TO SEPARATE SCRIPT FOR MODEL BUILDING -- CREATE MATRIX OF ALL DIMENSIONS ####


  # PATH DATA INPUT FILES
  path_in_csv <- paste0(path_in, "./input_csv/")

  # Regions
  geo_data <- read_csv(paste0(path_in_csv, "/input_basic_geo/regions.csv"),
                       show_col_types = FALSE) # First columns

  # filter if region is specified, region can be a vector
  if (!is.null(region)) {
    geo_data <- geo_data %>%
      filter(region_gea %in% region)
  }
  # Lucas: Why do we need paste?
  regions <- unlist(geo_data[, paste(geo_level)]) # Regions in the analysis
  
  # Lucas: regions_aggr is not used in the code.
  # regions_aggr <- sort(unique(unlist(geo_data[, paste(geo_level_aggr)]))) # Regions in the analysis - aggregated level

  # Climatic zones
  clim_zones <- read_csv(paste0(path_in_csv, "/input_basic_geo/climatic_zones.csv"))

  # Lucas: Should be in a file not in the code.
  # Household categories
  urts <- c("rur", "urb") # # Urban-Rural / Total. options: "rur", "urb", "tot"
  ct_hh_inc <- c("q1", "q2", "q3") # Income classes
  ct_hh_tenr <- c("own", "rent") # c("ownns")

  # Building categories
  # Vintage cohorts
  ct_bld_age <- read_csv(paste0(path_in_csv, "input_", sector, "/categories/ct_bld_age.csv")) 
  ct_bld <- read_csv(paste0(path_in_csv, "input_", sector, "/categories/ct_arch.csv"))
  # Energy efficiency categories
  ct_eneff <- read_csv(paste0(path_in_csv, "input_", sector, "/categories/ct_eneff.csv")) 

  # Fuel type
  ct_fuel_comb <- read_csv(paste0(path_in_csv, "input_", sector, "/categories/ct_fuel.csv"))
  ct_fuel_dhw <- read_csv(paste0(path_in_csv, "input_", sector, "/categories/ct_fuel_res.csv")) # fuels domestic hot water - Add solar thermal options

  # Renovation categories
  ct_ren_eneff <- read_csv(paste0(path_in_csv, "input_", sector, "/categories/ct_ren_eneff2.csv")) # "fuel" settings. Energy efficiency transitions for renovations
  
  # The following is loaded as input data
  # ct_ren_fuel_heat <- read_csv(paste0(path_in_csv,"input_",sector,"/decision/ct_ren_fuel_heat.csv")) # Energy efficiency transitions for renovations

  # BUILDING CASES

  bld_cases_fuel <- expand.grid(
    geo_level = regions,
    urt = urts,
    inc_cl = ct_hh_inc,
    stringsAsFactors = FALSE
  ) %>%
    rename_at("geo_level", ~ paste0(geo_level)) %>%
    left_join(geo_data %>% select_at(geo_levels)) %>%
    left_join(clim_zones, by = c(paste(geo_level), "urt")) %>%
    left_join(ct_bld) %>%
    left_join(ct_eneff, by = "mat") %>%
    inner_join(ct_fuel_comb, by = c("mat" = "mat")) %>%
    arrange(
      !!as.symbol(geo_level),
      urt, clim, inc_cl, arch, mat, eneff,
      fuel_heat, fuel_cool
    ) 

  # Lucas: Why do we need this more aggregated level?
  ### BUILDING CASES at more aggregate level can be generated from bld_cases_fuel
  # bld_cases_arch <- bld_cases_fuel %>% select(-c(eneff, bld_age, fuel_heat, fuel_cool, mod_decision)) %>% distinct()
  bld_cases_eneff <- bld_cases_fuel %>%
    select(-c(fuel_heat, fuel_cool, mod_decision)) %>%
    distinct()

  #### TO HERE: MOVE AND RE-ORGANIZE TO SEPARATE SCRIPT FOR MODEL BUILDING -- CREATE MATRIX OF ALL DIMENSIONS ####

  ## Energy prices (from MESSAGE)
  if (is.null(prices) == FALSE) {
    price_en <- prices %>%
    mutate(price_en = lvl / all_of(u_EJ_GWa)) %>%
    mutate(region = substr(node, 5, 7)) %>%
    # mutate(region_gea = ifelse(region_gea == "LAM", "LAC", region_gea)) %>%
    mutate(fuel = commodity) %>%
    mutate(fuel = ifelse(commodity == "biomass", "biomass_solid", fuel)) %>%
    mutate(fuel = ifelse(commodity == "electr", "electricity", fuel)) %>%
    mutate(fuel = ifelse(commodity == "lightoil", "oil", fuel)) %>%
    filter(commodity != "d_heat") %>%
    filter(year %in% yrs) %>%
    select(region, year, fuel, price_en) %>%
    rename_at("region", ~ paste(substr(prices$node[1], 1, 3))) # rename region column based on R11/R12
    price_en <- geo_data %>% # use the most granular level for prices
    left_join(price_en) %>% # Join R11/R12 data
    select_at(c(paste(geo_level), "year", "fuel", "price_en"))
  }

  ### RESIDENTIAL SECTOR

  if (sector == "resid") {
    # Initialize housing stock (fun)
    print(paste("Initialize scenario run", sector))

    # Lucas: Why do we need geo_data, geo_levels, geo_level here?
    lst_stock_init <- fun_stock_init_fut(
      sector,
      mod_arch,
      yrs,
      geo_data,
      geo_levels,
      geo_level,
      bld_cases_eneff,
      bld_cases_fuel,
      d$pop,
      d$hh_size, # used for residential
      d$floor_cap, # used for commercial
      ct_hh_inc,
      ct_eneff,
      ct_fuel_comb,
      d$stock_arch_base,
      d$shr_mat,
      d$shr_arch,
      d$shr_fuel_heat_base,
      d$shr_distr_heat,
      report_var
    )

    # Extract dataframes from list
    stock_aggr <- lst_stock_init$stock_aggr
    bld_det_age_i <- lst_stock_init$bld_det_age_i

    # Lucas: Can we remove this?
    report <- lst_stock_init$report
    # if ("vintage" %in% report){bld_eneff_age = lst_stock_init$bld_eneff_age} # Not needed - already within the DF report
    # if ("energy" %in% report){en_stock = lst_stock_init$en_stock}
    # if ("material" %in% report){mat_stock = lst_stock_init$mat_stock}

    rm(lst_stock_init)

    print(paste("Initialize scenario run", sector, "- completed!"))

    # Loop over timesteps
    print(paste("Start scenario run", sector))

    for (i in 2:length(yrs)) {
      print(paste("Start scenario run", sector, "for year", yrs[i]))

      # Lucas: try to run this function only once before the loop
      # Energy demand intensities - heating/cooling
      lst_en_i <- fun_en_sim(
        sector,
        yrs,
        i,
        bld_cases_fuel,
        d$en_int_heat,
        d$en_int_cool,
        d$days_cool,
        d$eff_cool,
        d$eff_heat,
        d$en_sav_ren,
        d$hours_heat,
        d$shr_floor_heat,
        d$hours_cool,
        d$shr_floor_cool,
        d$hours_fans,
        d$power_fans,
        d$shr_acc_cool,
        d$hh_size,
        d$floor_cap,
        price_en
      )

      # Extract dataframes from list
      en_m2_scen_heat <- lst_en_i$en_m2_scen_heat
      en_m2_scen_cool <- lst_en_i$en_m2_scen_cool
      en_hh_tot <- lst_en_i$en_hh_tot
      rm(lst_en_i)

      # Energy demand intensities - hot water
      en_hh_hw_scen <- fun_hw_resid(
        yrs, i,
        bld_cases_fuel,
        d$hh_size,
        # ct_fuel_dhw,
        d$eff_hotwater,
        d$en_int_hotwater,
        d$en_int_heat
      )

      # Market share - new construction options
      ms_new_i <- fun_ms_new(
        yrs,
        i,
        bld_cases_fuel,
        ct_bld_age,
        d$hh_size,
        d$floor_cap,
        d$cost_invest_new_shell,
        d$cost_invest_new_heat,
        d$cost_intang_new_shell,
        d$cost_intang_new_heat,
        d$ct_fuel_excl_new,
        d$ct_fuel_excl_reg,
        d$discount_new,
        d$heterog_new,
        d$lifetime_new,
        en_hh_tot
      )

      try(if (nrow(ms_new_i) == 0) stop("Error in new construction calculation! Empty dataframe ms_new_i"))

      # Market share - renovation + fuel switches options for the existing building stock
      lst_ms_ren_sw_i <- fun_ms_ren_sw(
        yrs,
        i,
        bld_cases_fuel,
        ct_bld_age,
        ct_hh_tenr,
        ct_fuel_comb,
        ct_ren_eneff,
        d$ct_ren_fuel_heat,
        d$hh_size,
        d$floor_cap,
        d$hh_tenure,
        d$cost_invest_ren_shell,
        d$cost_invest_ren_heat,
        d$cost_intang_ren_shell,
        d$cost_intang_ren_heat,
        d$ct_fuel_excl_ren,
        d$ct_fuel_excl_reg,
        d$discount_ren,
        d$heterog_ren,
        d$lifetime_ren,
        d$rate_ren_low,
        d$rate_ren_high, # ren_rate,
        # en_hh_tot_ren_init,
        # en_hh_tot_ren_fin
        en_hh_tot
      )

      ms_ren_i <- lst_ms_ren_sw_i$ms_ren_i
      rate_ren_i <- lst_ms_ren_sw_i$rate_ren_i
      ms_sw_i <- lst_ms_ren_sw_i$ms_sw_i
      rm(lst_ms_ren_sw_i)

      try(if (nrow(ms_ren_i) == 0) stop("Error in renovation calculation! Empty dataframe ms_ren_i"))
      try(if (nrow(ms_sw_i) == 0) stop("Error in renovation calculation! Empty dataframe ms_sw_i"))

      # Stock turnover
      lst_stock_i <- fun_stock_dyn(
        sector,
        mod_arch,
        yrs,
        i,
        run, # ssp_r, # removed ssp dimension
        geo_level,
        geo_level_aggr,
        geo_levels,
        bld_cases_fuel,
        bld_cases_eneff,
        ct_bld_age,
        ct_fuel_comb,
        d$hh_size,
        d$floor_cap,
        stock_aggr,
        bld_det_age_i,
        d$prob_dem,
        d$rate_switch_fuel_heat,
        ms_new_i,
        ms_ren_i,
        rate_ren_i,
        ms_sw_i,
        d$shr_distr_heat,
        d$shr_need_heat,
        en_m2_scen_heat,
        en_m2_scen_cool,
        en_hh_hw_scen,
        en_m2_hw_scen,
        en_m2_others, # used only in commercial
        d$mat_int,
        report_var,
        report
      )

      # Extract dataframes from list
      report <- lst_stock_i$report
      # en_stock = lst_stock_i$en_stock
      # mat_stock = lst_stock_i$mat_stock
      # stock_eneff.df = lst_stock_i$stock_eneff.df
      stock_aggr <- lst_stock_i$stock_aggr
      bld_det_age_i <- lst_stock_i$bld_det_age_i
      # bld_det = lst_stock_i$bld_det
      # bld_eneff_age = lst_stock_i$bld_eneff_age
      # ms_new = lst_stock_i$ms_new
      # ms_ren = lst_stock_i$ms_ren
      rm(lst_stock_i)
    }
  }

  ### COMMERCIAL SECTOR
  if (sector == "comm") {
    # Initialize housing stock (fun)
    print(paste("Initialize scenario run", sector))

    lst_stock_init <- fun_stock_init_fut(
      sector, mod_arch,
      yrs,
      geo_data, geo_levels, geo_level,
      bld_cases_eneff, bld_cases_fuel,
      pop_fut,
      hh_size, # used for residential
      floor_cap, # used for commercial
      ct_hh_inc,
      ct_eneff, ct_fuel_comb,
      stock_arch_base,
      shr_mat, shr_arch, shr_fuel_heat_base, shr_distr_heat,
      # eff_cool_scen, eff_heat_scen,eff_hotwater_scen,
      # ren_en_sav_scen,
      # heat_hours_scen,cool_data_scen, heat_floor, shr_acc_cool,
      # en_m2_sim_r, price_en
      report_var
    )

    # Extract dataframes from list
    stock_aggr <- lst_stock_init$stock_aggr
    bld_det_age_i <- lst_stock_init$bld_det_age_i
    report <- lst_stock_init$report
    # bld_det = lst_stock_init$bld_det
    # bld_eneff_age = lst_stock_init$bld_eneff_age
    # en_stock = lst_stock_init$en_stock
    # mat_stock = lst_stock_init$mat_stock
    rm(lst_stock_init)

    # Loop over timesteps
    print(paste("Start scenario run", sector))

    for (i in 2:length(yrs)) {
      # Energy demand intensities
      lst_en_i <- fun_en_sim(sector,
        yrs, i,
        bld_cases_fuel,
        en_m2_sim_r,
        eff_cool_scen, eff_heat_scen,
        en_sav_ren,
        heat_hours_scen, heat_floor,
        cool_data_scen,
        shr_acc_cool,
        hh_size = NULL, # not used for commercial
        floor_cap,
        price_en = NULL # not used for commercial
      )

      # Extract dataframes from list
      en_m2_scen_heat <- lst_en_i$en_m2_scen_heat
      en_m2_scen_cool <- lst_en_i$en_m2_scen_cool
      ## Additional output not needed for commercial
      # en_hh_tot = lst_en_i$en_hh_tot #
      # en_hh_tot_ren_init = lst_en_i$en_hh_tot_ren_init
      # en_hh_tot_ren_fin = lst_en_i$en_hh_tot_ren_fin

      # Energy demand intensities - hot water
      en_m2_hw_scen <- fun_hw_comm(
        yrs, i,
        bld_cases_fuel,
        eff_hotwater_scen,
        en_m2_dhw
      )

      # Market share - new construction options
      ms_new_i <- fun_ms_new_target(
        yrs, i,
        bld_cases_eneff, bld_cases_fuel,
        ct_bld_age,
        shr_eneff_new, shr_fuel_new
      )

      # Market share - renovation options
      ms_ren_i <- fun_ms_ren_target(
        yrs, i,
        bld_cases_fuel, ct_bld_age,
        shr_eneff_ren, shr_fuel_ren
      )

      # Market share - fuel switches
      ms_sw_i <- fun_ms_fuel_sw(
        yrs, i,
        bld_cases_fuel, ct_bld_age,
        # ms_fuel_sw_target
        shr_fuel_sw
      )

      # Stock turnover
      lst_stock_i <- fun_stock_dyn(sector,
        mod_arch, # mod_arch = "new", mod_arch = "stock"
        yrs, i,
        run, ssp_r,
        geo_level, geo_level_aggr, geo_levels,
        bld_cases_fuel, bld_cases_eneff,
        ct_bld_age, ct_fuel_comb,
        hh_size = NULL, # Not used for commercial
        floor_cap,
        stock_aggr, bld_det_age_i, # bld_det,
        # bld_eneff_age, # keep track of age
        bld_dyn_par,
        rate_ren_low, rate_ren_high, # ren_rate,
        rate_switch_fuel_heat,
        # ms_new, ms_ren,
        ms_new_i, ms_ren_i, rate_ren_i,
        ms_sw_i,
        # shr_acc_cool,
        shr_distr_heat, shr_need_heat,
        en_m2_scen_heat, en_m2_scen_cool,
        en_hh_hw_scen, en_m2_hw_scen, en_m2_others,
        # en_stock,
        mat_int,
        # mat_stock,
        report_var,
        report
      )

      # Extract dataframes from list
      report <- lst_stock_i$report
      # en_stock = lst_stock_i$en_stock
      # mat_stock = lst_stock_i$mat_stock
      # stock_eneff.df = lst_stock_i$stock_eneff.df
      stock_aggr <- lst_stock_i$stock_aggr
      bld_det_age_i <- lst_stock_i$bld_det_age_i
      # bld_det = lst_stock_i$bld_det
      # bld_eneff_age = lst_stock_i$bld_eneff_age
      # ms_new = lst_stock_i$ms_new
      # ms_ren = lst_stock_i$ms_ren
      rm(lst_stock_i)
    }
  }

  ### REPORTING ###

  ## MESSAGE report -  Aggregate results for reporting
  if ("MESSAGE" %in% report_type) {
    output <- fun_report_MESSAGE(sector, report_var, report, geo_data, geo_level, geo_level_report)
  }

  ## STURM basic report (results written as csv)
  if ("STURM" %in% report_type) {
    fun_report_basic(report, report_var, geo_data, geo_level, geo_level_report, sector, scenario_name, path_out)
  }

  ## Report results - IRP template (results written as csv)
  if ("IRP" %in% report_type) {
    fun_report_IRP(report, report_var, geo_data, geo_level, geo_level_report, sector, scenario_name, yrs, path_out)
  }

  ## Report results - NGFS template (results written as csv)
  if ("NGFS" %in% report_type) {
    fun_report_NGFS(report, report_var, geo_data, geo_level, geo_level_report, sector, scenario_name, yrs, path_out)
  }

  ## Report results - NGFS template (results written as csv)
  if ("NAVIGATE" %in% report_type) {
    fun_report_NAVIGATE(report, report_var, geo_data, geo_level, geo_level_report, sector, scenario_name, yrs, path_out, path_in, ct_bld, ct_ren_eneff, ren_en_sav_scen)
  }

  # Tracking time
  end_time <- Sys.time()
  print(paste("Time to run script 04_run_future: ", end_time - start_time))
  print(paste0("Start: ", start_time))
  print(paste0("End: ", end_time))
  rm(start_time, end_time)

  print("Senario run completed!")

  # return(en_stock)
  return(output)
}
