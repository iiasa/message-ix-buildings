# Script to be run in Rstudio

library(rstudioapi)
library(tidyverse)
library(readxl)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("./STURM_model/F10_scenario_runs_MESSAGE_2100.R")

# Paths
rcode_path <- paste(getwd(), "/STURM_model/", sep = "")
data_path <- paste(getwd(), "/STURM_data/", sep = "")
rout_path <- paste(getwd(), "/STURM_output/", sep = "")


prices <- read.csv(paste0(data_path, "input_prices_R12.csv"))
region <- c("WEU", "EEU")

# call STURM
sturm_scenarios <- run_scenario(
    run = "NAV_Dem-NPi-ref",
    scenario_name = "NAV_Dem-NPi-ref",
    sector = "resid",
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
    yrs = seq(2015, 2050, 5),
    input_mode = "csv",
    mod_arch = "stock",
    report_type = c("MESSAGE", "STURM", "NAVIGATE"), # Available reports: c("MESSAGE","STURM","IRP","NGFS","NAVIGATE")
    report_var = c("energy", "material") # Available report variables: c("energy","material","vintage","dle")
)


#
# # write results to csv file
# write.csv(sturm_scenarios,paste("./temp/",sect,"_sturm.csv",sep=""),row.names=F)





##############################################

## Run out of the function - For debugging

# Run the commands below and then the content of function "run_scenario" in the script "F10_scenario_runs_MESSAGE_2100.R"

rcode_path <- paste(getwd(), "/STURM_model/", sep = "")
data_path <- paste(getwd(), "/STURM_data/", sep = "")
rout_path <- paste(getwd(), "/STURM_output/", sep = "")

file_input <- "input_list_resid.csv"
file_scenario <- "scenarios_TEST.csv"

prices <- read.csv(paste0(data_path, "input_prices_R12.csv"))

scen <- "NAV_Dem-NPi-ref"
# scen <- "NAV_Dem-NPi-all"
# scen <- "SDP_EI-NPi"
# scen <- "NAV_Dem-NPi-tec"
# clim_scen <- "BL"
sect <- "resid"
# sect <- "comm"

run <- scen
scenario_name <- scen
prices <- prices
path_in <- data_path
path_rcode <- rcode_path
path_out <- rout_path
sector <- sect
geo_level <- "region_bld" # Level for analysis
geo_level_aggr <- "region_gea" # Level for aggregation
geo_levels <- c("region_bld", "region_gea") # Levels to keep track of
geo_level_report <- "R12"
yrs <- seq(2015, 2050, 5)
# yrs <- c(seq(2015,2060,5),seq(2070,2100,10))

# # Input data type:
# Values allowed: "RData", "csv"
input_mode <- "csv"
# input_mode <- "rdata"

# Running setting: # Share of buildings archetypes:
# mod_arch = "new",  # provided for new buildings (on the margin)
mod_arch <- "stock" # provided for the entire stock - Default

# Report types
report_type <- c("MESSAGE", "STURM", "NAVIGATE") # Available reports: c("MESSAGE","STURM","IRP","NGFS","NAVIGATE")

# Reporting variables
report_var <- c("energy", "material") # Available report variables: c("energy","material","vintage","dle")
