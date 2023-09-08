
library(rstudioapi)
library(tidyverse, quietly = TRUE)
library(readxl)
library(dplyr)
library(parallel)

suppressPackageStartupMessages(library(tidyverse))

options(dplyr.width = Inf)
options(dplyr.summarise.inform = FALSE)

# Set working directory based on the position of the current R file
path_rcode <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/") # R file directory
setwd(path_rcode)

# rounding (number of decimals)
start_script_time <- Sys.time()
rnd <- 5
u_EJ_GWa <- 31.71
inertia_wtp <- 4.3
min_shr_fuel <- 0.01

rate_dem_target <- NULL 

source("./STURM_model/F10_scenario_run.R")

# paths
path_rcode <- paste0(getwd(), "/STURM_model/")
path_in <- paste0(getwd(), "/STURM_data/")
path_out <- paste0(getwd(), "/STURM_output/results/DG_GROW/")
if (!dir.exists(path_out)) {
    dir.create(path_out)
}

# configuration file STURM
base_year <- 2015
end_year <- 2050
step_year <- 5

# configuration file STURM
region <- c("WEU", "EEU")
sector <- "resid"
file_inputs <- "input_list_resid_DG_GROW_EU.csv"
file_scenarios <- "scenarios_DG_GROW_EU.csv"
en_method <- "TABULA" # "VDD", "TABULA"

runs <- "EU"
energy_efficiency <- "endogenous"


report <- list(var = c("energy","material"),
               type = c("STURM"),
               geo_level = "region_bld")

yrs <- seq(base_year, end_year, step_year)

for (run in runs) {

    sturm_scenarios <- run_scenario(
        run = run,
        scenario_name = run,
        sector = sector,
        path_in = path_in,
        path_rcode = path_rcode,
        path_out = path_out,
        file_inputs = file_inputs,
        file_scenarios = file_scenarios,
        geo_level_report = report$geo_level,
        yrs = yrs,
        report_var = report$var,
        report_type = report$type,
        region = region,
        energy_efficiency = energy_efficiency,
        en_method = en_method
    )

end_script_time <- Sys.time()
print(paste("Time to run script:",
    round(end_script_time - start_script_time, 0), "seconds."))
}
