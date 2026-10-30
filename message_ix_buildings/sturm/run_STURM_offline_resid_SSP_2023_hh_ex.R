# Script to be run in Rstudio (or with Rscript from this folder)
# Residential runs with the household-type overlay - exogenous construction and renovation decisions (fixed market shares)
# Example inputs: input_list_resid_SSP_2023_hh_example.csv (synthetic household types: single, couple, family, elderly)

library(tidyverse)
library(readxl)

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

source("./model/F10_scenario_runs_MESSAGE_2100.R")

#Paths
rcode_path <- paste(getwd(),"/model/",sep="")
data_path <- paste(getwd(),"/data/",sep="")
input_path <- paste(getwd(),"/data/input_csv_SSP_2023_resid/",sep="")
rout_path <- paste(getwd(),"/output/hh_exo/",sep="") # own folder: report names would otherwise overwrite the base-run reports
dir.create(rout_path, showWarnings = FALSE, recursive = TRUE)

prices <- read_csv(paste0(data_path,"input_prices_R12.csv"))


scenarios = c("SSP2") # household-type example data are defined for the reference scenario

for(s in scenarios){

  # call STURM
  sturm_scenarios <- run_scenario(run = s,
                                  sector = "resid",
                                  path_in=data_path,
                                  path_inputs=input_path,
                                  path_rcode=rcode_path,
                                  path_out=rout_path,
                                  prices=prices,
                                  file_inputs = "input_list_resid_SSP_2023_hh_example.csv",
                                  geo_level = "region_bld", # Level for analysis
                                  geo_level_aggr = "region_gea", # Level for aggregated data
                                  geo_levels = c("region_bld", "region_gea"), # Levels to keep track of
                                  geo_level_report="R12", # Level for reporting
                                  region_select = NULL, #list("region_bld", c("R32IND")),
                                  yrs = c(seq(2020,2060,5),seq(2070,2100,10)),
                                  input_mode = "csv",
                                  mod_arch = "stock",
                                  mod_new = "exogenous", # fixed market shares (shr_eneff_new, shr_fuel_heat_new)
                                  mod_ren = "exogenous", # fixed market shares (shr_eneff_ren, shr_fuel_heat_ren, shr_fuel_heat_sw, rate_ren)
                                  report_type = c("MESSAGE","STURM"), # household-type results are written with the STURM report
                                  report_var = c("energy","material"),
                                  mod_hh_type = "overlay" # household-type overlay: "none" or "overlay"
                                  )

  write_csv(
    sturm_scenarios %>%
      filter(!commodity %in% c("resid_heat_v_no_heat", "resid_hotwater_v_no_heat")),
    paste0(rout_path, "report_MESSAGE_resid_hh_ex_", s, ".csv")
  )

}
