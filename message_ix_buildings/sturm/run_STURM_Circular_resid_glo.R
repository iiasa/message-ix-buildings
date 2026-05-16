# Script to be run in Rstudio or batch (e.g. Rscript from the sturm folder)

library(tidyverse)
library(readxl)

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  ca <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", ca, value = TRUE)
  if (length(file_arg)) {
    script_path <- sub("^--file=", "", file_arg[1])
    setwd(dirname(normalizePath(script_path)))
  } else {
    stop("Cannot set working directory: run from RStudio, or `Rscript run_STURM_Circular_resid_glo.R` from the sturm folder.")
  }
}

### RUNS CIRCEULAR PROJECT - Carbon storage potential - Residential

# Paths — CSV inputs live under sturm/data/ (input lists + input_comm / input_resid)
rcode_path <- paste0(getwd(), "/model/")
data_path <- paste0(getwd(), "/data/")
input_path <- paste0(data_path, "input_resid/")
rout_path <- paste0(getwd(), "/output/")

# Source model function
source(paste0(rcode_path, "F10_scenario_runs_MESSAGE_2100.R"))

prices <- read_csv(paste0(data_path, "input_prices_R12.csv"))

dir_message_linking <- file.path(getwd(), "message_linking")
dir.create(dir_message_linking, recursive = TRUE, showWarnings = FALSE)
source(file.path(dir_message_linking, "load_scenario_config.R"))
scenarios <- load_scenarios()

for(s in scenarios){
  
 #  s="SSP2"
  
  #prices <- read_csv(paste0(data_path,"prices_",tolower(substr(s,1,4)),"_r12.csv"))
  
  # call STURM
  sturm_scenarios <- run_scenario(run = s,
                                  sector = "resid",
                                  path_in=data_path,
                                  path_inputs=input_path,
                                  path_rcode=rcode_path,
                                  path_out=rout_path,
                                  prices=prices,
                                  file_inputs = "input_list_resid_2026_05_11_CE.csv",
                                  #file_data_model = "data_model_resid_SSP_2023.csv",
                                  #file_scenarios = "scenarios_SSP_2023.csv",
                                  geo_level = "region_bld", # Level for analysis
                                  geo_level_aggr = "region_gea", # Level for aggregated data
                                  geo_levels = c("region_bld", "region_gea"), # Levels to keep track of
                                  geo_level_report="R12", # Level for reporting
                                  region_select = NULL, #list("region_bld", c("R32IND")), 
                                  #yrs = c(seq(2020,2050,5)), # seq(2020,2030,5),
                                  yrs = c(seq(2020,2060,5),seq(2070,2100,10)), # seq(2020,2030,5),
                                  input_mode = "csv",
                                  mod_arch = "stock",
                                  mod_new = "endogenous", #"external"
                                  mod_ren = "endogenous", #"external"
                                  report_type = c("STURM","MESSAGE"), # ,"STURM" # Available reports: c("MESSAGE","STURM","IRP","NGFS","NAVIGATE")
                                  report_var = c("energy","material") # Available report variables: c("energy","material","vintage","dle")
                                  )
  
  write_csv(sturm_scenarios %>% filter(!commodity %in% c("resid_heat_v_no_heat","resid_hotwater_v_no_heat")),
            paste0(rout_path,"report_MESSAGE_resid_",s,".csv"))

  write.csv(sturm_scenarios, file.path(dir_message_linking, paste0("resid_sturm_", s, ".csv")), row.names = FALSE)

}





##############################################

## Run out of the function - For debugging

# Run the commands below and then the content of function "run_scenario" in the script "F10_scenario_runs_MESSAGE_2100.R"

rcode_path <- paste0(getwd(), "/model/")
data_path <- paste0(getwd(), "/data/")
input_path <- paste0(data_path, "input_resid/")
rout_path <- paste0(getwd(), "/output/")

file_inputs <- "input_list_resid_2026_05_11_CE.csv"
#file_scenario <- "scenarios_SSP_2023.csv"
#file_data_model = "data_model_resid_SSP_2023.csv"


#prices<-read_csv(paste0(getwd(),"/data/","input_prices_R12.csv"))

scen <- "SSP2"
#scen <- "NAV_Dem-NPi-all"
#scen <- "SDP_EI-NPi"
#scen <- "NAV_Dem-NPi-tec"
#clim_scen <- "BL"
sect <- "resid"
#sect <- "comm"

run = scen
prices=prices
path_in=data_path
path_inputs=input_path
path_rcode=rcode_path
path_out=rout_path
sector=sect
geo_level = "region_bld" # Level for analysis
geo_level_aggr = "region_gea" # Level for aggregation
geo_levels <- c("region_bld", "region_gea") # Levels to keep track of
geo_level_report="R12"
yrs = seq(2020,2030,5)
# yrs <- c(seq(2020,2060,5),seq(2070,2100,10))

# # Input data type: 
# Values allowed: "RData", "csv"
input_mode <- "csv"
#input_mode <- "rdata"

# Running setting: # Share of buildings archetypes:
# mod_arch = "new",  # provided for new buildings (on the margin)
mod_arch <- "stock" # provided for the entire stock - Default

# Report types
report_type = c("MESSAGE","STURM","NAVIGATE") # Available reports: c("MESSAGE","STURM","IRP","NGFS","NAVIGATE")

# Reporting variables
report_var=c("energy","material") # Available report variables: c("energy","material","vintage","dle")





