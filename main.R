# Script to be run in Rstudio

library(rstudioapi)
library(tidyverse, quietly = TRUE)
library(readxl)
library(dplyr)
library(parallel)

suppressPackageStartupMessages(library(tidyverse))

options(dplyr.width = Inf)

# rounding (number of decimals)#
rnd <- 5
u_EJ_GWa <- 31.71


# Paths
path_r <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/") # Path to folder containing the current R file
rcode_path <- paste0(path_r, "/STURM_model/")
data_path <- paste0(path_r, "/STURM_data/")
# Lucas: Should we add date to output folder?
rout_path <- paste0(path_r, "/STURM_output/")

# Source STURM code
source(paste0(rcode_path,"/F10_scenario_runs_MESSAGE_2100.R"))

# configuration file STURM
base_year <- 2015
end_year <- 2050
step_year <- 5
runs <- c("EU_implementation",
          "EU_implementation_nomfhq1rent",
          "EU_implementation_double",
          "EU_implementation_triple")

region <- c("WEU", "EEU")
sector <- "resid"
file_inputs <- "input_list_resid_EU.csv"
path_prices_message <- paste0(data_path,
                              "input_csv/input_price/input_prices_R12.csv")
path_prices <- paste0(data_path,
                      "input_csv/input_price/input_prices_EU.csv")

file_scenarios <- "scenarios_EU.csv"

mod_arch <- "stock"
# energy_efficiency <- "endogenous"
energy_efficiency <- "exogenous"
report <- list(var = c("energy"),
               type = c("STURM"),
               geo_level = "region_bld")

yrs <- seq(base_year, end_year, step_year)

for (run in runs) {
  sturm_scenarios <- run_scenario(
    run = run,
    scenario_name = run,
    sector = sector,
    path_in = data_path,
    path_rcode = rcode_path,
    path_out = rout_path,
    path_prices = path_prices,
    path_prices_message = path_prices_message,
    file_inputs = file_inputs,
    file_scenarios = file_scenarios,
    geo_level_report = report$geo_level,
    yrs = yrs,
    report_var = report$var,
    report_type = report$type,
    region = region,
    energy_efficiency = energy_efficiency
  )
}
