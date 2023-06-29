# Script to be run in Rstudio

library(rstudioapi)
library(tidyverse)
library(readxl)

source("./STURM_model/F10_scenario_runs_MESSAGE_2100.R")

# Paths
rcode_path <- paste(getwd(), "/STURM_model/", sep = "")
data_path <- paste(getwd(), "/STURM_data/", sep = "")
# Lucas: Should we add date to output folder?
rout_path <- paste(getwd(), "/STURM_output/", sep = "")

# configuration file STURM
base_year <- 2015
end_year <- 2025
step_year <- 5
run <- "EU_implementation"
region <- c("WEU", "EEU")
sector <- "resid"
file_inputs <- "input_list_resid_EU.csv"
path_prices <- paste0(data_path,
    "input_csv/input_price/input_prices_R12.csv")
file_scenarios <- "scenarios_TEST.csv"

mod_arch <- "stock"
energy_efficiency <- "exogenous"
report <- list(var = c("energy"),
               type = c("STURM"),
               geo_level = "region_bld")

yrs <- seq(base_year, end_year, step_year)


# call STURM
#' @param run: name of the run
#' @param scenario_name: name of the scenario
#' @param sector: sector to be analysed, default is "resid"
#' @param path_in: path to input data, default is current working directory
#' @param path_rcode: path to R code, default is current working directory
#' @param path_out: path to output data, default is current working directory
#' @param path_prices: name of the price file, default is "input_prices_R12.csv"
#' @param file_inputs: name of the input file, default is "input_list_resid"
#' @param file_scenarios: name of the scenario file, default is "scenarios_TEST"
#' @param geo_level: level for analysis, default is "region_bld"
#' @param geo_level_aggr: level for aggregation, default is "region_gea"
#' @param geo_levels: levels to keep track of,
#' default is c("region_bld","region_gea")
#' @param geo_level_report: level for reporting, default is "R12"
#' @param yrs: years to be analysed, default is seq(2015,2050,5)
#' @param input_mode: input mode, default is "csv"
#' @param mod_arch: model architecture, default is "stock"
#' @param report_type: type of report, default is "STURM"
#' available is c("MESSAGE","STURM","IRP","NGFS","NAVIGATE")
#' @param report_var: variables to be reported,
#' available is c("energy","material","vintage","dle")
#' @return: results of the STURM model
#' @export
sturm_scenarios <- run_scenario(
    run = run,
    scenario_name = run,
    sector = sector,
    path_in = data_path,
    path_rcode = rcode_path,
    path_out = rout_path,
    path_prices = path_prices,
    file_inputs = file_inputs,
    file_scenarios = file_scenarios,
    geo_level = "region_bld",
    geo_level_aggr = "region_gea",
    geo_levels = c("region_bld", "region_gea"),
    geo_level_report = report$geo_level,
    yrs = yrs,
    input_mode = "csv",
    report_var = report$var,
    report_type = report$type,
    region = region,
    energy_efficiency = energy_efficiency
)