# Script to be run in Rstudio

library(rstudioapi)
library(tidyverse, quietly = TRUE)
library(readxl)
library(dplyr)
library(parallel)

suppressPackageStartupMessages(library(tidyverse))

options(dplyr.width = Inf)
options(dplyr.summarise.inform = FALSE)

# rounding (number of decimals)#
start_script_time <- Sys.time()
rnd <- 5
u_EJ_GWa <- 31.71

source("./STURM_model/F10_scenario_run.R")

# Paths
rcode_path <- paste(getwd(), "/STURM_model/", sep = "")
data_path <- paste(getwd(), "/STURM_data/", sep = "")
# Lucas: Should we add date to output folder?
rout_path <- paste(getwd(), "/STURM_output/", sep = "")

# configuration file STURM
base_year <- 2015
end_year <- 2040
step_year <- 5
runs <- c("EU", "EU_nomfhq1rent", "EU_double", "EU_triple")
runs <- c("EU", "EU_strong", "EU_medium")
    
region <- c("WEU", "EEU")
sector <- "resid"
file_inputs <- "input_list_resid_EU.csv"
path_prices_message <- paste0(data_path,
    "input_csv/input_price/input_prices_R12.csv")
path_prices <- paste0(data_path,
    "input_csv/input_price/input_prices_EU.csv")

file_scenarios <- "scenarios_EU.csv"

energy_efficiency <- "endogenous"
# energy_efficiency <- "exogenous"
report <- list(var = c("energy"),
               type = c("STURM"),
               geo_level = "region_bld")

yrs <- seq(base_year, end_year, step_year)

for (run in runs) {
    run <- paste(run, energy_efficiency, sep = "_")

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

end_script_time <- Sys.time()
print(paste("Time to run script:",
    round(end_script_time - start_script_time, 0), "seconds."))
}