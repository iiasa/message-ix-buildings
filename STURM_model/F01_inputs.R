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
  input <- read_csv(paste0(path_in, file_inputs), show_col_types = FALSE) %>%
    filter(!is.na(name_file))

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

  print(filter(input, name_parameter == "sub_ren_shell")$path_file)


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
    ct_ren_eneff = paste0("input_", sector, "/categories/ct_ren_eneff.csv"),
    ct_fuel = paste0("input_", sector, "/categories/ct_fuel.csv")
    # ct_fuel_dhw = paste0("input_", sector, "/categories/ct_fuel_res.csv")
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

#' @description Parse energy prices from MESSAGE
read_message_prices <- function(prices, geo_data) {

  price_en <- rename(prices, price_en = energy_prices_message)

  if ("node" %in% names(prices)) {
    price_en <- price_en %>%
      mutate(price_en = price_en / u_EJ_GWa) %>%
      mutate(region = substr(node, 5, 7)) %>%
      mutate(fuel = commodity) %>%
      mutate(fuel = ifelse(commodity == "biomass", "biomass_solid", fuel)) %>%
      mutate(fuel = ifelse(commodity == "electr", "electricity", fuel)) %>%
      mutate(fuel = ifelse(commodity == "lightoil", "oil", fuel)) %>%
      filter(commodity != "d_heat") %>%
      filter(year %in% yrs) %>%
      select(region, year, fuel, price_en) %>%
      # Rename region column based on R11/R12
      rename_at("region", ~ paste(substr(prices$node[1], 1, 3)))
  }
  price_en <- geo_data %>%
    # Join R11/R12 data with prices
    left_join(price_en, relationship = "many-to-many") %>%
    select_at(c("region_bld", "year", "fuel", "price_en"))
  
  return(price_en)
}

#' @title Read energy prices
#' @description Read energy prices
#' @param path_prices: path to energy prices
#' @param geo_data: list of dimensions
#' @param geo_level: level of geographical aggregation
#' @return list of energy prices
read_energy_prices <- function(price_base_year,
                               price_en,
                               geo_data,
                               path_out) {
  
  if (!is.null(price_en)) {
    price_en <- read_message_prices(price_en, geo_data)

    evolution_rate <- price_en %>%
      group_by(region_bld, fuel) %>%
      arrange(year) %>%
      mutate(evolution_rate = price_en / first(price_en)) %>%
      ungroup() %>%
      filter(!is.na(evolution_rate)) %>%
      select(-c(price_en))

  } else {
    evolution_rate <- price_base_year %>%
      mutate(evolution_rate = 1) %>%
      select(-c(energy_prices))
  }

  price_base_year <- rename(price_base_year, value = energy_prices_ini)

  price_en <- price_base_year %>%
    left_join(evolution_rate, by = c("region_bld", "fuel")) %>%
    mutate(value_new = value * evolution_rate) %>%
    select(region_bld, fuel, year = year.y, value = value_new) %>%
    filter(!is.na(value)) %>%
    bind_rows(price_base_year)

  # Complete data if missing
  price_expanded <- price_en %>%
    expand(region_bld, fuel, year = unique(price_en$year)) %>%
    left_join(price_en, by = c("region_bld", "fuel", "year")) %>%
    group_by(region_bld, fuel) %>%
    fill(value, .direction = "down") %>%
    ungroup() %>%
    rename(price_en = value) %>%
    distinct()

  write.csv(price_expanded, paste0(path_out, "energy_prices.csv"),
    row.names = FALSE)

  return(price_expanded)
}

read_emission_factors <- function(emission_factors,
                                  emission_ini,
                                  geo_data,
                                  base_year) {

  if (!is.null(emission_ini)) {
    emission_ini <- geo_data %>%
      left_join(filter(emission_factors, year == base_year),
        relationship = "many-to-many") %>%
      left_join(emission_ini) %>%
      mutate(emission_factors =
        ifelse(!is.na(emission_ini), emission_ini, emission_factors)) %>%
      select(-c(emission_ini))

    evolution_rate <- emission_factors %>%
      filter(year >= base_year) %>%
      group_by(region_gea, fuel) %>%
      arrange(year) %>%
      mutate(evolution_rate = emission_factors / first(emission_factors)) %>%
      ungroup() %>%
      filter(!is.na(evolution_rate)) %>%
      select(-c(emission_factors))
    
    emission_factors <- emission_ini %>%
      left_join(evolution_rate, by = c("region_gea", "fuel"),
        relationship = "many-to-many") %>%
      mutate(emission_factors = emission_factors * evolution_rate) %>%
      select(region_gea, region_bld, fuel, year = year.y, emission_factors) %>%
      filter(!is.na(emission_factors)) %>%
      select(c(region_gea, region_bld, fuel, year, emission_factors))

  } else {
    emission_factors <- geo_data %>%
      left_join(filter(emission_factors))
  }
  return(emission_factors)
}