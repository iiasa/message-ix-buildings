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

# Paths — CSV inputs from sturm/data/ (default) or <local-data>/buildings/sturm/ (private)
#   default:  Rscript run_STURM_bmt_resid.R
#   private:  Rscript run_STURM_bmt_resid.R --data=private  (configure via setup_sturm_local_data.R)
rcode_path <- paste0(getwd(), "/model/")
rout_path <- paste0(getwd(), "/output/")

dir_message_linking <- file.path(getwd(), "message_linking")
dir.create(dir_message_linking, recursive = TRUE, showWarnings = FALSE)
source(file.path(dir_message_linking, "resolve_sturm_data_dir.R"))
source(file.path(dir_message_linking, "load_scenario_config.R"))
data_path <- paste0(resolve_sturm_data_dir(), "/")
input_path <- paste0(data_path, "input_resid/")

# Source model function
source(paste0(rcode_path, "F10_scenario_runs_MESSAGE_2100.R"))

prices <- read_csv(paste0(data_path, "input_prices_R12.csv"))
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
                                  file_inputs = "input_list_resid_2026_05_18_CE.csv",
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
