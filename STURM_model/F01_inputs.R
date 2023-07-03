# Description: Import input data from csv files
# Input: path_in, file_inputs, file_scenarios, sector, run
# Output: List of input dataframes

library(dplyr)
library(readr)
library(tidyr)


#' @title Import input data
#' @description Import input data from csv files
#' @param path_in Path to input data files
#' @param file_inputs Name of the file containing the list of input data files
#' @param file_scenarios Name of the file containing the list of scenarios
#' @param sector Name of the sector
#' @param run Name of the scenario to run
#' @return List of input dataframes
fun_inputs_csv <- function(path_in, file_inputs, file_scenarios, sector, run
                           ) {
  # PATH DATA INPUT FILES
  path_in_csv <- paste0(path_in, "./input_csv/")

  # IMPORT LIST OF INPUT DATA FILES
  input <- read_csv(paste0(path_in, file_inputs), show_col_types = FALSE)

  # IMPORT LIST OF SCENARIOS
  scenarios <- read_csv(paste0(path_in, file_scenarios), show_col_types = FALSE)

  # Create vector of scenario-dependent parameters
  scen_pars <- names(scenarios)[!names(scenarios) %in%
    c("scenario_id", "scenario_name")]

  # Scenario setup for scenario-dependent parameters
  scen_setup <- scenarios %>%
    filter(scenario_name == run) %>%
    select(-c(scenario_id, scenario_name)) %>%
    pivot_longer(
      cols = all_of(scen_pars),
      names_to = "name_parameter",
      values_to = "scenario"
    )

  # Input data: build vector of input file names for the current scenarios
  input <- input %>%
    left_join(scen_setup) %>%
    # Exclude lines with "skip" indication
    filter(category != "skip") %>%
    mutate(path_file = ifelse(category == "basic",
      paste0(path_in_csv, "input_basic/", name_file),
      paste0(path_in_csv, "input_", sector, "/", category, "/", name_file)
    )) %>%
    mutate(path_file = ifelse(!is.na(scenario),
      paste0(path_file, "_", scenario),
      path_file)) %>%
    mutate(path_file = paste0(path_file, ".csv"))


  ### TEMPORARY ### EXCLUDE SPECIAL DATA FILES
  input <- input %>%
    filter(!name_parameter %in%
      c("bld_dyn_par", "cool_data_scen", "en_m2_sim_r")) %>%
    filter(category != "categories")

  ### DATA LOADING AND PROCESSING

  # Extract data paths, file names and variable names
  input_paths <- input %>%
    select(path_file) %>%
    pull()
     # Extract paths to input files
  input_files <- input %>%
    select(name_file) %>%
    pull()
     # Extract file names
  input_names <- input %>%
    select(name_parameter) %>%
    pull() # Extract variable names

  # Load input data csv files into a list of dataframes
  d <- lapply(input_paths, function(x){
    read_csv(x, show_col_types = FALSE)})
  
  # Rename the dataframes within the list based on the variable names
  d <- setNames(d, input_names)

  # Rename the "value" column within each dataframe with variable-specific name
  d <- Map(fun_rename, d, input_names)

  # Remove provisional inputs
  rm(input, scen_setup)

  return(d)
}


#' @title Create list of dimensions
#' @description Create list of dimensions
#' @param path_in_csv: path to input data
#' @param sector: sector to run, available: "com", "resid"
#' @return list of dimensions
read_categories <- function(path_in_csv,
                            sector,
                            region = NULL) {
  input_list <- list(
    geo_data = "/input_basic_geo/regions.csv",
    clim_zones = "/input_basic_geo/climatic_zones.csv",
    ct_bld_age = paste0("input_", sector, "/categories/ct_bld_age.csv"),
    ct_bld = paste0("input_", sector, "/categories/ct_arch.csv"),
    ct_eneff = paste0("input_", sector, "/categories/ct_eneff.csv"),
    ct_fuel_comb = paste0("input_", sector, "/categories/ct_fuel.csv"),
    ct_fuel_dhw = paste0("input_", sector, "/categories/ct_fuel_res.csv"),
    ct_ren_eneff = paste0("input_", sector, "/categories/ct_ren_eneff2.csv")
  )
  
  # Read all categories, and return a list
  categories <- lapply(input_list, function(x) {
    read_csv(paste0(path_in_csv, x), show_col_types = FALSE)
  })

  # Select region
  if (!is.null(region)) {
    categories$geo_data <- categories$geo_data %>%
    filter(region_gea %in% region)
  }

  # Add others categories
  categories <- c(categories,
                  list(urts = c("rur", "urb"),
                        ct_hh_inc = c("q1", "q2", "q3"),
                        ct_hh_tenr = c("own", "rent")))


  return(categories)
}

#' @title Add dimension to stock data
#' @description Add dimension to stock data 
fun_parse_stock <- function(stock, cat, population) {

  if (!"clim" %in% names(stock)) {
    # Adding climatic zones and urban/rural dimension based on share of population
    # No correlation between housing type and these dimensions.

    shr_clim <- population %>%
                  filter(year == 2015) %>%
                  filter(region_bld %in%
                    unique(stock$region_bld)) %>%
                  group_by(region_bld) %>%
                  mutate(share = pop / sum(pop)) %>%
                  ungroup() %>%
                  select(-pop)

    temp <- sum(stock$stock_arch_base)
    # No correlation between clim
    stock <- stock %>%
              left_join(shr_clim, relationship = "many-to-many") %>%
              mutate(stock_arch_base = stock_arch_base * share) %>%
              select(-share)

    # Test consistency.
    if (round(temp, 0) !=
      round(sum(stock$stock_arch_base), 0)) {
      print("Error:
        Inconsistent population shares.")
    }
  }

  if (!"inc_cl" %in% names(stock)) {
    # Subdivising equally between three classes of income (q1, q2, q3)
    # Assumption of no correlation with other dimension

    shr_inc <- data.frame(
        mat = "perm",
        inc_cl = c("q1", "q2", "q3"),
        share = 1 / 3)

    temp <- sum(stock$stock_arch_base)
    stock <- stock %>%
      left_join(shr_inc) %>%
      mutate(stock_arch_base = stock_arch_base * share) %>%
      select(-share)

    # Test consistency.
    if (round(temp, 0) !=
      round(sum(stock$stock_arch_base), 0)) {
      print("Error:
        Inconsistent income shares.")
    }

  }

  if (!"region_gea" %in% names(stock)) {

    stock <- stock %>%
      left_join(cat$geo_data %>%
        select(c(region_bld, region_gea)))

  }

  # Add tenure status as a dimension
  return(stock)


}


#' @title Read energy prices
#' @description Read energy prices
#' @param path_prices: path to energy prices
#' @param geo_data: list of dimensions
#' @param geo_level: level of geographical aggregation
#' @return list of energy prices
read_energy_prices <- function(path_prices, geo_data, geo_level) {
  prices <- read.csv(path_prices)

  # Parse energy prices from MESSAGE
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
    # Rename region column based on R11/R12
    rename_at("region", ~ paste(substr(prices$node[1], 1, 3)))
    # Use the most granular level for prices
    price_en <- geo_data %>%
    # Join R11/R12 data with prices
    left_join(price_en) %>% 
    select_at(c(paste(geo_level), "year", "fuel", "price_en"))
  } else {
    price_en <- NULL
  }
}
