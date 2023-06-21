# Script to be run in Rstudio

library(rstudioapi)
library(tidyverse)
library(readxl)

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("./STURM_model/F10_scenario_runs_MESSAGE_2100.R")

# Paths
rcode_path <- paste(getwd(), "/STURM_model/", sep = "")
data_path <- paste(getwd(), "/STURM_data/", sep = "")
# Lucas: Should we add date to output folder?
rout_path <- paste(getwd(), "/STURM_output/", sep = "")

# configuration file STURM
base_year <- 2015
end_year <- 2050
step_year <- 5
region <- c("WEU", "EEU")
sector <- "resid"
prices <- read.csv(paste0(data_path, "input_prices_R12.csv"))
report_var <- c("energy")
report_type <- "STURM"
mod_arch <- "stock"

yrs <- seq(base_year, end_year, step_year)


# call STURM
#' @param run: name of the run, default is "NAV_Dem-NPi-ref"
#' @param scenario_name: name of the scenario, default is "NAV_Dem-NPi-ref"
#' @param sector: sector to be analysed, default is "resid"
#' @param path_in: path to input data, default is current working directory
#' @param path_rcode: path to R code, default is current working directory
#' @param path_out: path to output data, default is current working directory
#' @param prices: prices for energy carriers, default is "input_prices_R12"
#' @param file_inputs: name of the input file, default is "input_list_resid"
#' @param file_scenarios: name of the scenario file, default is "scenarios_TEST"
#' @param geo_level: level for analysis, default is "region_bld"
#' @param geo_level_aggr: level for aggregation, default is "region_gea"
#' @param geo_levels: levels to keep track of, default is c("region_bld","region_gea")
#' @param geo_level_report: level for reporting, default is "R12"
#' @param yrs: years to be analysed, default is seq(2015,2050,5)
#' @param input_mode: input mode, default is "csv"
#' @param mod_arch: model architecture, default is "stock"
#' @param report_type: type of report, available is c("MESSAGE","STURM","IRP","NGFS","NAVIGATE")
#' @param report_var: variables to be reported, available is c("energy","material","vintage","dle")
#' @return: results of the STURM model
#' @export
sturm_scenarios <- run_scenario(
    run = "NAV_Dem-NPi-ref",
    scenario_name = "NAV_Dem-NPi-ref",
    sector = sector,
    path_in = data_path,
    path_rcode = rcode_path,
    path_out = rout_path,
    prices = prices,
    file_inputs = "input_list_resid.csv",
    file_scenarios = "scenarios_TEST.csv",
    geo_level = "region_bld", # Level for analysis
    geo_level_aggr = "region_gea", # Level for aggregation
    geo_levels = c("region_bld", "region_gea"), # Levels to keep track of
    geo_level_report = "R12",
    yrs = yrs,
    input_mode = "csv",
    mod_arch = mod_arch,
    report_var = report_var,
    report_type = report_type,
    region = region
)

# # write results to csv file
# write.csv(sturm_scenarios, paste("./temp/",sect,"_sturm.csv",sep=""),r ow.names=F)
