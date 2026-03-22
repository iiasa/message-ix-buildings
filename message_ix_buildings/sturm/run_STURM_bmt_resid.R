# Script for RStudio or batch (e.g. Rscript from Python — rstudioapi is not available there)

library(tidyverse)
library(readxl)

# Working directory: RStudio vs Rscript (Python/mix-models runs `Rscript run_STURM_bmt_resid.R` with cwd=sturm)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  ca <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", ca, value = TRUE)
  if (length(file_arg)) {
    script_path <- sub("^--file=", "", file_arg[1])
    setwd(dirname(normalizePath(script_path)))
  } else {
    stop("Cannot set working directory: run from RStudio, or `Rscript run_STURM_bmt_resid.R` from the sturm folder.")
  }
}

source("./model/F10_scenario_runs_MESSAGE_2100.R")

#Paths
rcode_path <- paste(getwd(),"/model/",sep="")
data_path <- paste(getwd(),"/data/",sep="")
input_path <- paste(getwd(),"/data/input_csv_SSP_2023_resid/",sep="")
rout_path <- paste(getwd(),"/output/",sep="")

prices <- read_csv(paste0(data_path,"input_prices_R12.csv"))

report_type <- c("MESSAGE")
scenarios <- c("SSP2")
sector <- "resid"

for(s in scenarios){
  
  #s="SSP2"
  
  # call STURM
  sturm_scenarios <- run_scenario(run = s,
                                  sector = sector,
                                  path_in=data_path,
                                  path_inputs=input_path,
                                  path_rcode=rcode_path,
                                  path_out=rout_path,
                                  prices=prices,
                                  file_inputs = "input_list_resid_SSP_2023.csv",
                                  #file_data_model = "data_model_resid_SSP_2023.csv",
                                  #file_scenarios = "scenarios_SSP_2023.csv",
                                  geo_level = "region_bld", # Level for analysis
                                  geo_level_aggr = "region_gea", # Level for aggregated data
                                  geo_levels = c("region_bld", "region_gea"), # Levels to keep track of
                                  geo_level_report="R12", # Level for reporting
                                  region_select = NULL, #list("region_bld", c("R32IND")), 
                                  yrs = c(seq(2020,2060,5),seq(2070,2100,10)), # seq(2020,2030,5),
                                  input_mode = "csv",
                                  mod_arch = "stock",
                                  mod_new = "endogenous", #"external"
                                  mod_ren = "endogenous", #"external"
                                  report_type = c("MESSAGE"), # ,"STURM","NAVIGATE" # Available reports: c("MESSAGE","STURM","IRP","NGFS","NAVIGATE")
                                  report_var = c("energy","material") # Available report variables: c("energy","material","vintage","dle")
                                  )
  
  # Only generate the MESSAGE report if "MESSAGE" is included in report_type
  if ("MESSAGE" %in% report_type) {
    write_csv(
      sturm_scenarios %>% 
        filter(!commodity %in% c("resid_heat_v_no_heat", "resid_hotwater_v_no_heat")), 
      paste0(rout_path, "report_MESSAGE_resid_", s, ".csv")
    )
  }
  
}

# write results to csv file
temp_dir <- file.path(getwd(), "temp")
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)

write.csv(
  sturm_scenarios,
  file.path(temp_dir, paste0(sector, "_sturm.csv")),
  row.names = FALSE
)
