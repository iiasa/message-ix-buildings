# Script to be run in Rstudio

library(rstudioapi)
library(tidyverse)
library(readxl)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("./model/F10_scenario_runs_MESSAGE_2100.R")

#Paths
rcode_path <- paste(getwd(),"/model/",sep="")
data_path <- paste(getwd(),"/data/",sep="")
input_path <- paste(getwd(),"/data/input_csv_SSP_2023_comm/",sep="")
rout_path <- paste(getwd(),"/output/",sep="")

#prices <- read_csv(paste0(data_path,"input_prices_R12.csv"))


scenarios = c("SSP2","SSP1","SSP3","SSP2_LED","SSP1_LED")

for(s in scenarios){
  
  #s="SSP2"
  
  # call STURM
  sturm_scenarios <- run_scenario(run = s,
                                  sector = "comm",
                                  path_in=data_path,
                                  path_inputs=input_path,
                                  path_rcode=rcode_path,
                                  path_out=rout_path,
                                  prices=NULL,
                                  file_inputs = "input_list_comm_SSP_2023.csv",
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
                                  mod_new = "exogenous", 
                                  mod_ren = "exogenous", 
                                  report_type = c("STURM"), # ,"STURM","NAVIGATE" # Available reports: c("MESSAGE","STURM","IRP","NGFS","NAVIGATE")
                                  report_var = c("energy","material") # Available report variables: c("energy","material","vintage","dle")
                                  )
  
  # Only generate the MESSAGE report if "MESSAGE" is included in report_type
  if ("MESSAGE" %in% report_type) {
    write_csv(
      sturm_scenarios %>% 
        filter(!commodity %in% c("comm_heat_v_no_heat", "comm_hotwater_v_no_heat")), 
      paste0(rout_path, "report_MESSAGE_comm_", s, ".csv")
    )
  }

}




# 
# # write results to csv file
# write.csv(sturm_scenarios,paste("./temp/",sect,"_sturm.csv",sep=""),row.names=F)





##############################################

## Run out of the function - For debugging

# Run the commands below and then the content of function "run_scenario" in the script "F10_scenario_runs_MESSAGE_2100.R"

rcode_path <- paste(getwd(),"/STURM_model/",sep="")
data_path <- paste(getwd(),"/STURM_data/",sep="")
input_path <- paste(getwd(),"/STURM_data/input_csv_SSP_2023_comm/",sep="")
rout_path <- paste(getwd(),"/STURM_output/",sep="")

file_inputs <- "input_list_comm_SSP_2023.csv"
#file_scenario <- "scenarios_SSP_2023.csv"
#file_data_model = "data_model_resid_SSP_2023.csv"


#prices<-read_csv(paste0(getwd(),"/STURM_data/","input_prices_R12.csv"))
prices=NULL

scen <- "SSP2"

#sect <- "resid"
sect <- "comm"

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

region_select = NULL

mod_new = "exogenous"
mod_ren = "exogenous"


