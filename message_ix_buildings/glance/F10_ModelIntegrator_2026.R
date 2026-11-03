
#### function 10: integrating functions F01-05 for complete scenario runs ####

fun_scenario_run_app <- function(yr_init,
                                 yrs,
                                 # scenario_combinations,
                                 report_region,
                                 rnd){
  
  # Start measuring time
  start_time <- Sys.time()
    
  #### Import csv files ####
  # regions <- read_csv("input_csv/regions.csv") # regions
  # stock_app_base <- read_csv("input_csv/EU_2020_stocks_detailed_2025-03-22.csv") # stocks of appliances in the initial year
  # 
  # n_hh <- read_csv("input_csv/n_hh_new_ssp2_2024-09-12.csv") # household numbers
  # access_app <- read_csv("input_csv/penetration_GAINS_1990_2050_urt_scenario_2025-03-21.csv") # access to appliances, dependent on scenarios
  # ownership_app <- read_csv("./input_csv/ownership_2020_calibrated_WM_DW_EU28_2025-03-15.csv") # ownership per household that has access
  # lifetime_app <- read_csv("input_csv/weibull_app_baseline_R61_2025-03-22.csv") # uniform function parameters
  # reuse_rate_app <- read_csv("input_csv/reuse_rate_app_R61_baseline_2025-03-22.csv") # reuse rate of appliances
  # 
  # energy_efficiency_class_app <- read_csv("input_csv/energy_class_distribution_1990_2050_normalised_baseline_2025-03-22.csv") # reuse rate of appliances
  # energy_consumption_per_label <- read_csv("input_csv/energy_consumption_per_label_EU_baseline_2025-03-22.csv") # energy intensity
  # material_intensity_app <- read_csv("input_csv/material_intensity_app_EU_baseline_2025-03-22.csv") # material intensity
  # eol_rate_mat <- read_csv("input_csv/eol_rate_mat_GAINS_wide_baseline_2025-03-22.csv") # eol treatment rates of materials
  # emission_factor_electricity <- read_csv("input_csv/complete_emission_factors_ENGAGE_R61_NPi_2025-03-22.csv") # emission factors of different energy types
  # emission_factor_materials_primary <- read_csv("input_csv/CO2_intensity_materials_primary_baselinie_2025-03-22.csv") # emission factors of primary materials production
  # emission_factor_materials_secondary <- read_csv("input_csv/CO2_intensity_materials_secondary_baseline_2025-03-22.csv") # emission factors of secondary materials production
  
  # Load the input list
  # input_list <- read_csv("input_csv/input_list_appliances_20250517.csv")
  input_list <- read_excel("input_csv/input_list_appliances_2026_JIE.xlsx", sheet = "input_list_appliances_2026")
  
  # Fallback: replace missing values in the selected scenario with baseline values
  input_list <- input_list %>%
    mutate(
      selected_file = ifelse(is.na(.data[[scenario_selection]]) | .data[[scenario_selection]] == "", 
                             .data[["baseline"]],
                             .data[[scenario_selection]])
    )
  
  # Create named vector: parameter_name -> selected_file
  input_files <- setNames(input_list$selected_file, input_list$name_parameter)
  
  # Load all CSV files for the selected scenario into a named list
  data_list <- lapply(input_files, function(file) read_csv(file.path("input_csv", paste0(file, ".csv"))))
  
  # assign each data frame to a variable in the global environment
  list2env(data_list, envir = .GlobalEnv)
  
  # ## exclude other appliance types than wash_mach ##
  # access_app <- access_app %>%
  #   filter(type == 'WASH_MACH')
  # 
  # ownership_app <- ownership_app %>%
  #   filter(type == 'WASH_MACH')
  # 
  # lifetime_app <- lifetime_app %>%
  #   filter(type == 'WASH_MACH')
  # 
  # reuse_rate_app <- reuse_rate_app %>%
  #   filter(type == 'WASH_MACH')
  #  
  # stock_app_base <- stock_app_base %>%
  #   filter(type == 'WASH_MACH')  
  # 
  # material_intensity_app <- material_intensity_app %>%
  #   filter(type == 'WASH_MACH')    
  
  
  #### Source functions ####
  source("./F01_future_app_stock_flows_2026.R")
  # source("./F02_energy_intensity.R")
  source("./F03_energy_demand_2026.R")
  source("./F04_material_flows.R")
  source("./F05_emissions.R")
  
  #### Call the functions and run the calculations ####
  # 1. number of needed appliances, new productions, and retirement
  
  # 1.0 calculate the stocks, new productions, and retirement in the base year 2020
  rnd <- 5 # rounding precision
  yr_init <- 2000 # initial year
  time_step <- 5
  yrs <- seq(2000, 2020, by = time_step) # a sequence of years considered
  future_years <- seq(2005, 2020, by = time_step)
  
  stock_flow_app <- fun_dynamic_stock_flow_app(
                                               stock_app_base,
                                               future_years,
                                               lifetime_app,
                                               n_hh,
                                               access_app,
                                               ownership_app,
                                               reuse_rate_app,
                                               energy_efficiency_class_app,
                                               yrs)
  
  
  # process the data
  stock_app_future_years <- stock_flow_app$stock_app_future_years  # Extract the 'stock_app_future_years' df from the list
  
  # 1.2 add / bind the stocks, new productions, and retirement in the base year to the future years
  stock_app_all_years <- stock_app_base %>%
    bind_rows(stock_app_future_years)
  
  # 1.3 aggregate stock/flow results to higher levels - only for reporting, so not passed to next steps
  # stocks
  stock_app_R61 <- stock_app_all_years %>%
    rename(R61 = region) %>%
    # group_by(R61, urt, year, type, capacity, energy_label, generation, yrs_prod) %>%
    group_by(R61, urt, year, type, energy_label, generation, yrs_prod) %>%
    summarise(n_app = sum(n_app)) %>%
    mutate(unit = "unit")
  
  stock_app_EU31 <- stock_app_R61 %>%
    left_join(regions) %>%
    filter(grepl("C-EEU|C-WEU", R61)) %>%
    # group_by(R61, urt, year, type, capacity, energy_label, generation, yrs_prod) %>%
    group_by(R61, urt, year, type, energy_label, generation, yrs_prod) %>%
    summarise(n_app = sum(n_app)) %>%
    rename(EU31 = R61) %>%
    mutate(unit = "unit")
  
  stock_app_R12 <- stock_app_R61 %>%
    left_join(regions) %>%
    # group_by(R61, urt, year, type, capacity, energy_label, generation, yrs_prod) %>%
    group_by(R61, urt, year, type, energy_label, generation, yrs_prod) %>%
    summarise(n_app = sum(n_app)) %>%
    mutate(unit = "unit")
  
  stock_app_global <- stock_app_R12 %>%
    # group_by(R61, urt, year, type, capacity, energy_label, generation, yrs_prod) %>%
    group_by(R61, urt, year, type, energy_label, generation, yrs_prod) %>%
    summarise(n_app = sum(n_app)) %>%
    mutate(unit = "unit")
  
  # only keep the stocks in 2020
  stock_app_2020_R61 <- stock_app_R61 %>%
    filter(year == 2020)
  
  stock_app_2020_EU31 <- stock_app_EU31 %>%
    filter(year == 2020)
  
  stock_app_2020_R12 <- stock_app_R12 %>%
    filter(year == 2020)
  
  stock_app_2020_global <- stock_app_global %>%
    filter(year == 2020)
  
  # stock_app_all[[scenario_name]] <- get(paste0("stock_app_2020_", report_region))
  stock_app_all <- get(paste0("stock_app_2020_", report_region))
  write.csv(stock_app_all, file = paste0("output_csv/stock_appliances_2020_", report_region, "_", Sys.Date(), ".csv"), row.names = FALSE)
  
  ## clean format of stock_app_base used for the future stock turnovers in 2020-2050
  stock_app_base <- stock_app_all %>%
    rename(region = report_region) %>%
    select(-unit)
  
  print(paste0("✅ ✅ ✅ Base year stocks generated and saved for check, now run future scenarios"))
  
  
  # 1.1 calculate the stocks, new productions, and retirement in future years (not including the base year)
  rnd <- 5 # rounding precision
  yr_init <- 2020 # initial year
  time_step <- 5
  yrs <- seq(2020, 2050, by = time_step) # a sequence of years considered
  future_years <- seq(2025, 2050, by = time_step)
  # material_types <- c("steel", "copper", "aluminium", "cobalt", "neodymium","tantalum", "lithium", "glass", "plastics", "pla_abs", "pla_pp", "pla_pp", "pla_oth")
  
  stock_flow_app <- fun_dynamic_stock_flow_app(# current_scenario,
                                               stock_app_base,
                                               future_years,
                                               lifetime_app,
                                               n_hh,
                                               access_app,
                                               ownership_app,
                                               reuse_rate_app,
                                               energy_efficiency_class_app,
                                               yrs)
  
  stock_app_future_years <- stock_flow_app$stock_app_future_years  # Extract the 'stock_app_future_years' df from the list
  inflow_app_future_years <- stock_flow_app$inflow_app_future_years # Extract the 'inflow_app_future_years' df from the list
  outflow_app_future_years <- stock_flow_app$outflow_app_future_years # Access the 'outflow_app_future_years' df from the list
  
  # 1.2 add / bind the stocks, new productions, and retirement in the base year to the future years
  # stocks
  stock_app_all_years <- stock_app_base %>%
    bind_rows(stock_app_future_years)
  
  # inflow
  inflow_app_all_years <- stock_app_base %>%
    filter(generation == 2 | yrs_prod == yr_init) %>%
    rename(n_inflow = n_app) %>%
    bind_rows(inflow_app_future_years)
  
  # the inflows are 5-year totals, now to annual inflows
  inflow_app_all_years_annual <- inflow_app_all_years %>%
    mutate(n_inflow = n_inflow / 5)
  
  # lifetime_app_baseline <- lifetime_app %>%
  #   filter(scenario == "baseline") %>%
  #   select(-scenario)
  
  # outflow
  retd_app_base <- stock_app_base %>% # stocks retired in the previous time step
    rename(n_app_previous = n_app) %>%
    left_join(lifetime_app) %>%
    mutate(p_retirement = pweibull(year-yrs_prod, shape = shape, scale = scale)) %>%
    mutate(n_retired_base = p_retirement * n_app_previous) %>% # retire first before reuse happens
    select(-c(lifetime, shape, scale, p_retirement)) %>%
    left_join(reuse_rate_app) %>%
    mutate(n_reuse_base = n_retired_base * reuse_rate)
  
  outflow_app_base <- retd_app_base %>%
    mutate(across(c(n_retired_base, n_reuse_base), ~replace_na(., 0))) %>% # replace na as 0
    mutate(n_outflow_base = n_retired_base - n_reuse_base) %>%
    # a percentage of retired devices typically transitions into “hibernating” or “storage” stocks
    mutate(n_outflow_store = n_retired_base * 0.1)
  
  outflow_app_all_years <- outflow_app_base %>%
    rename(n_outflow_nominal = n_retired_base) %>% # total outflow including the those recollected
    rename(n_outflow_actual = n_outflow_base) %>% # actual outflow excluding the those recollected
    select(-c(n_app_previous, reuse_rate, n_reuse_base)) %>%
    bind_rows(outflow_app_future_years)
  
  # the outflows are 5-year totals, now to annual outflows
  outflow_app_all_years_annual <- outflow_app_all_years %>%
    mutate(n_outflow_nominal = n_outflow_nominal / 5) %>%
    mutate(n_outflow_actual = n_outflow_actual / 5) %>%
    mutate(n_outflow_store = n_outflow_store / 5)
  
  
  # lifetime_app_baseline <- lifetime_app
  # 
  # outflow_app_all_years <- stock_app_base %>%
  #   left_join(lifetime_app_baseline) %>%
  #   mutate(p_retirement = case_when(
  #     generation == 1 ~ 1 / lifetime,
  #     generation == 2 ~ 1)) %>%
  #   mutate(n_outflow_nominal = ifelse(n_app > 0,
  #                              round(p_retirement * n_app, rnd), 0)) %>%
  #   # mutate(reuse_rate = case_when(
  #   #   generation == 1 ~ 0.1, # 10% of all first-hand being reused
  #   #   generation == 2 ~ 0)) %>% # second-hand not being reused
  #   left_join(reuse_rate_app, by = c("region", "year", "type", "generation")) %>%
  #   mutate(n_reuse_potential = n_outflow_nominal * reuse_rate) %>%
  #   mutate(n_outflow_actual = n_outflow_nominal - n_reuse_potential) %>%
  #   select(-c(n_app, lifetime, p_retirement, reuse_rate, n_reuse_potential)) %>%
  #   bind_rows(outflow_app_future_years)
  
  # 1.3 aggregate stock/flow results to higher levels - only for reporting, so not passed to next steps
  # stocks
  stock_app_R61 <- stock_app_all_years %>%
    rename(R61 = region) %>%
    group_by(R61, urt, year, generation, yrs_prod, type) %>%
    summarise(n_app = sum(n_app)) %>%
    mutate(unit = "unit")
  
  stock_app_EU31 <- stock_app_R61 %>%
    left_join(regions) %>%
    filter(grepl("C-EEU|C-WEU", R61)) %>%
    group_by(R61, urt, year, generation, yrs_prod, type) %>%
    summarise(n_app = sum(n_app)) %>%
    rename(EU31 = R61) %>%
    mutate(unit = "unit")
  
  stock_app_R12 <- stock_app_R61 %>%
    left_join(regions) %>%
    group_by(R12, urt, year, generation, yrs_prod, type) %>%
    summarise(n_app = sum(n_app)) %>%
    mutate(unit = "unit")
  
  stock_app_global <- stock_app_R12 %>%
    group_by(year, generation, yrs_prod, type) %>%
    summarise(n_app = sum(n_app)) %>%
    mutate(unit = "unit")
  
  stock_app_aggr_R61 <- stock_app_R61 %>%
    group_by(R61, urt, year, type) %>%
    summarise(n_app = sum(n_app)) %>%
    mutate(unit = "unit")
  
  stock_app_aggr_EU31 <- stock_app_EU31 %>%
    group_by(EU31, urt, year, type) %>%
    summarise(n_app = sum(n_app)) %>%
    mutate(unit = "unit")
  
  stock_app_aggr_R12 <- stock_app_R12 %>%
    group_by(R12, urt, year, type) %>%
    summarise(n_app = sum(n_app)) %>%
    mutate(unit = "unit")
  
  stock_app_aggr_global <- stock_app_global %>%
    group_by(year, type) %>%
    summarise(n_app = sum(n_app)) %>%
    mutate(unit = "unit")
  
  # inflows
  inflow_app_R61 <- inflow_app_all_years_annual %>%
    rename(R61 = region) %>%
    group_by(R61, urt, year, type) %>%
    summarise(n_inflow = sum(n_inflow)) %>%
    mutate(unit = "unit")
  
  inflow_app_EU31 <- inflow_app_R61 %>%
    left_join(regions) %>%
    filter(grepl("C-EEU|C-WEU", R61)) %>%
    group_by(R61, urt, year, type) %>%
    summarise(n_inflow = sum(n_inflow)) %>%
    rename(EU31 = R61) %>%
    mutate(unit = "unit")
  
  inflow_app_R12 <- inflow_app_R61 %>%
    left_join(regions) %>%
    group_by(R12, urt, year, type) %>%
    summarise(n_inflow = sum(n_inflow)) %>%
    mutate(unit = "unit")
  
  inflow_app_global <- inflow_app_R12 %>%
    group_by(year, type) %>%
    summarise(n_inflow = sum(n_inflow)) %>%
    mutate(unit = "unit")
  
  # outflows
  outflow_app_R61 <- outflow_app_all_years_annual %>%
    rename(R61 = region) %>%
    group_by(R61, urt, year, generation, yrs_prod, type) %>%
    summarise(n_outflow_nominal = sum(n_outflow_nominal),
              n_outflow_actual = sum(n_outflow_actual),
              n_outflow_store = sum(n_outflow_store)) %>%
    mutate(unit = "unit")
  
  outflow_app_EU31 <- outflow_app_R61 %>%
    left_join(regions) %>%
    filter(grepl("C-EEU|C-WEU", R61)) %>%
    group_by(R61, urt, year, generation, yrs_prod, type) %>%
    summarise(n_outflow_nominal = sum(n_outflow_nominal),
              n_outflow_actual = sum(n_outflow_actual),
              n_outflow_store = sum(n_outflow_store)) %>%
    rename(EU31 = R61) %>%
    mutate(unit = "unit")
  
  outflow_app_R12 <- outflow_app_R61 %>%
    left_join(regions) %>%
    group_by(R12, urt, year, generation, yrs_prod, type) %>%
    summarise(n_outflow_nominal = sum(n_outflow_nominal),
              n_outflow_actual = sum(n_outflow_actual),
              n_outflow_store = sum(n_outflow_store)) %>%
    mutate(unit = "unit")
  
  outflow_app_global <- outflow_app_R12 %>%
    group_by(year, generation, yrs_prod, type) %>%
    summarise(n_outflow_nominal = sum(n_outflow_nominal),
              n_outflow_actual = sum(n_outflow_actual),
              n_outflow_store = sum(n_outflow_store)) %>%
    mutate(unit = "unit")
  
  outflow_app_aggr_R61 <- outflow_app_R61 %>%
    group_by(R61, urt, year, type) %>%
    summarise(n_outflow_nominal = sum(n_outflow_nominal),
              n_outflow_actual = sum(n_outflow_actual),
              n_outflow_store = sum(n_outflow_store)) %>%
    mutate(unit = "unit")
  
  outflow_app_aggr_EU31 <- outflow_app_EU31 %>%
    group_by(EU31, urt, year, type) %>%
    summarise(n_outflow_nominal = sum(n_outflow_nominal),
              n_outflow_actual = sum(n_outflow_actual),
              n_outflow_store = sum(n_outflow_store)) %>%
    mutate(unit = "unit")
  
  outflow_app_aggr_R12 <- outflow_app_R12 %>%
    group_by(R12, urt, year, type) %>%
    summarise(n_outflow_nominal = sum(n_outflow_nominal),
              n_outflow_actual = sum(n_outflow_actual),
              n_outflow_store = sum(n_outflow_store)) %>%
    mutate(unit = "unit")
  
  outflow_app_aggr_global <- outflow_app_global %>%
    group_by(year, type) %>%
    summarise(n_outflow_nominal = sum(n_outflow_nominal),
              n_outflow_actual = sum(n_outflow_actual),
              n_outflow_store = sum(n_outflow_store)) %>%
    mutate(unit = "unit")
  
  # stocks and flows combined
  stock_flow_aggr_EU31 <- stock_app_aggr_EU31 %>%
    left_join(inflow_app_EU31) %>%
    left_join(outflow_app_aggr_EU31) %>%
    select(EU31, urt, year, type, n_app, n_inflow, n_outflow_nominal, n_outflow_actual, n_outflow_store, unit)
  
  stock_flow_aggr_R12 <- stock_app_aggr_R12 %>%
    left_join(inflow_app_R12) %>%
    left_join(outflow_app_aggr_R12) %>%
    select(R12, urt, year, type, n_app, n_inflow, n_outflow_nominal, n_outflow_actual, n_outflow_store, unit)
  
  stock_flow_aggr_global <- stock_flow_aggr_R12 %>%
    group_by(year, type) %>%
    summarise(n_app = sum(n_app),
              n_inflow = sum(n_inflow),
              n_outflow_nominal = sum(n_outflow_nominal),
              n_outflow_actual = sum(n_outflow_actual),
              n_outflow_store = sum(n_outflow_store))
  
  # for now, no further processing of energy intensity is needed
  
  # 2. energy use by appliances
  # 2.1 energy intensity modelling 
  # energy_intensity_app <- fun_energy_intensity_app(energy_intensity_app,
  #                                                  R12_gdp_cap,
  #                                                  regions,
  #                                                  SSP)
  
  # 2.2 energy use
  energy_use_app <- fun_energy_app(stock_app_all_years,
                                   # energy_intensity_app,
                                   energy_consumption_per_label,
                                   yrs,
                                   regions)
  
  energy_use_app_detailed <- energy_use_app$energy_use_app # get the energy use per appliances for R12
  energy_use_app_aggr_R12 <- energy_use_app$energy_use_app_aggr_R12 # get the energy use per appliances for R12
  energy_use_app_aggr_EU31 <- energy_use_app$energy_use_app_aggr_EU31 # get the energy use per appliances for R12
  energy_use_app_aggr_R61 <- energy_use_app$energy_use_app_aggr_R61 # get the energy use per appliances for R61
  
  
  # 3. material use and material scraps of appliances
  material_app <- fun_material_app(stock_app_all_years,
                                   inflow_app_all_years,
                                   outflow_app_all_years,
                                   material_intensity_app,
                                   eol_rate_mat,
                                   regions)
  
  material_app_aggr_R61 <- material_app$material_app_aggr_R61 # get the material stocks / flows per appliances for R61
  material_app_aggr_EU31 <- material_app$material_app_aggr_EU31 # get the material stocks / flows per appliances for EU31
  material_app_aggr_R12 <- material_app$material_app_aggr_R12 # get the material stocks / flows per appliances for R12
  material_app_aggr_global <- material_app$material_app_aggr_global # get the material stocks / flows per appliances for global
  
  
  # 4. calculate the CO2 emissions of appliance operation and production
  co2_emission_app <- fun_emission_app(energy_use_app_aggr_R61,
                                       material_app_aggr_R61,
                                       emission_factor_energy,
                                       emission_factor_materials_primary,
                                       emission_factor_materials_secondary,
                                       emission_factor_materials_landfill,
                                       emission_factor_materials_incineration,
                                       emission_factor_materials_burning_open,
                                       # climate_policy,
                                       regions,
                                       yrs)
  
  co2_emission_app_R61 <- co2_emission_app$co2_emission_app_R61
  co2_emission_app_EU31 <- co2_emission_app$co2_emission_app_EU31
  co2_emission_app_R12 <- co2_emission_app$co2_emission_app_R12
  co2_emission_app_global <- co2_emission_app$co2_emission_app_global
  
  # Store the results for the current scenario in the respective lists, for the defined "report_region"
  # stock_app_all[[scenario_name]] <- get(paste0("stock_flow_aggr_", report_region))
  # energy_app_all[[scenario_name]] <- get(paste0("energy_use_app_aggr_", report_region))
  # material_app_all[[scenario_name]] <- get(paste0("material_app_aggr_", report_region))
  # emission_app_all[[scenario_name]] <- get(paste0("co2_emission_app_", report_region))
  
  # stock_app_all[[scenario_selection]] <- get(paste0("stock_flow_aggr_", report_region))
  # energy_app_all[[scenario_selection]] <- get(paste0("energy_use_app_aggr_", report_region))
  # material_app_all[[scenario_selection]] <- get(paste0("material_app_aggr_", report_region))
  # emission_app_all[[scenario_selection]] <- get(paste0("co2_emission_app_", report_region))
  
  stock_app_all <- get(paste0("stock_flow_aggr_", report_region))
  energy_app_detailed <- get(paste0("energy_use_app_detailed"))
  energy_app_all <- get(paste0("energy_use_app_aggr_", report_region))
  material_app_all <- get(paste0("material_app_aggr_", report_region))
  emission_app_all <- get(paste0("co2_emission_app_", report_region))
  
  # }
  
  material_app_all_long <- material_app_all %>%
    pivot_longer(cols = c("material_stock",
                          "material_outflow",
                          "disposal_managed",
                          "disposal_unmanaged",
                          "incineration_energy",
                          "incineration",
                          "burning_open",
                          "composting",   
                          "unknown", 
                          "downcycling", 
                          "material_inflow",
                          "primary",
                          "reuse",
                          "recycling"),
                 names_to = "flow_type",
                 values_to = "value")
  
  
  #### write results to xlsx file ####
  # write_xlsx(stock_app_all, path = paste0("output_xlsx/stock_appliances_", report_region, "_", scenario_selection, "_", Sys.Date(),".xlsx"))
  # write_xlsx(material_app_all, path = paste0("output_xlsx/material_appliances_", report_region, "_", scenario_selection, "_", Sys.Date(),".xlsx"))
  # write_xlsx(emission_app_all, path = paste0("output_xlsx/emission_appliances_", report_region, "_", scenario_selection, "_", Sys.Date(),".xlsx"))
  # 
  write.csv(stock_app_all, file = paste0("output_csv/stock_appliances_", report_region, "_", scenario_selection, "_", Sys.Date(), ".csv"), row.names = FALSE)
  write.csv(energy_app_detailed, file = paste0("output_csv/energy_appliances_detailed_", scenario_selection, "_", Sys.Date(), ".csv"), row.names = FALSE)
  write.csv(energy_app_all, file = paste0("output_csv/energy_appliances_", report_region, "_", scenario_selection, "_", Sys.Date(), ".csv"), row.names = FALSE)
  write.csv(material_app_all_long, file = paste0("output_csv/material_appliances_long_", report_region, "_", scenario_selection, "_", Sys.Date(),".csv"), row.names = FALSE)
  write.csv(emission_app_all, file = paste0("output_csv/emission_appliances_", report_region, "_", scenario_selection, "_", Sys.Date(),".csv"), row.names = FALSE)
  
  
  # End measuring time
  end_time <- Sys.time()
  
  # Calculate the time difference
  time_taken <- end_time - start_time
  
  
  return(list(
    stock_app_all = stock_app_all,
    energy_app_all = energy_app_all,
    energy_app_detailed = energy_app_detailed,
    material_app_all_long = material_app_all_long,
    emission_app_all = emission_app_all,
    message = paste("Output files saved to 'output_xlsx/', taking ", round(time_taken, 2), "mins")
  ))
  
  
}


