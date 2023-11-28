# Script to be run in Rstudio

library(rstudioapi)
library(tidyverse)
library(readxl)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("./STURM_model/F10_scenario_runs_MESSAGE_2100.R")

#Paths
rcode_path <- paste(getwd(),"/STURM_model/",sep="")
data_path <- paste(getwd(),"/STURM_data/",sep="")
input_path <- paste(getwd(),"/STURM_data/input_csv_SSP_2023/",sep="")
rout_path <- paste(getwd(),"/STURM_output/",sep="")

prices <- read_csv(paste0(data_path,"input_prices_R12.csv"))


scenarios = c("SSP1","SSP2","SSP3","SSP4","SSP5","SSP2_LED","SSP1_LED")

for(s in scenarios[1:7]){
  
  s="SSP2"
  
  # call STURM
  sturm_scenarios <- run_scenario(run = s,
                                  sector = "resid",
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
                                  yrs = seq(2020,2030,5),
                                  input_mode = "csv",
                                  mod_arch = "stock",
                                  mod_new = "endogenous", #"external"
                                  mod_ren = "endogenous", #"external"
                                  report_type = c("MESSAGE","STURM","NAVIGATE"), # Available reports: c("MESSAGE","STURM","IRP","NGFS","NAVIGATE")
                                  report_var = c("energy","material") # Available report variables: c("energy","material","vintage","dle")
                                  )

}




# 
# # write results to csv file
# write.csv(sturm_scenarios,paste("./temp/",sect,"_sturm.csv",sep=""),row.names=F)





##############################################

## Run out of the function - For debugging

# Run the commands below and then the content of function "run_scenario" in the script "F10_scenario_runs_MESSAGE_2100.R"

rcode_path <- paste(getwd(),"/STURM_model/",sep="")
data_path <- paste(getwd(),"/STURM_data/",sep="")
input_path <- paste(getwd(),"/STURM_data/input_csv_SSP_2023/",sep="")
rout_path <- paste(getwd(),"/STURM_output/",sep="")

file_inputs <- "input_list_resid_SSP_2023.csv"
#file_scenario <- "scenarios_SSP_2023.csv"
#file_data_model = "data_model_resid_SSP_2023.csv"


#prices<-read_csv(paste0(getwd(),"/STURM_data/","input_prices_R12.csv"))

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





