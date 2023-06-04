
## RUN SCRIPTS ##

#library(lazyeval)
library(tidyverse)
library(readxl) 


### RUN SCENARIO - USING FUNCTION - MESSAGE

#path_rcode <- paste0("H:/MyDocuments/MESSAGE-buildings/_model_runs/_model_v1.0/")
path_rcode <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/")
path_in <- paste0(dirname(path_rcode),"/STURM_data/")
path_out <- paste0(dirname(path_rcode),"/STURM_output/")

setwd(path_rcode)


# Scenario run
run <- "SSP2"

# Geo level - for reporting
geo_level_report <- "R12"

# Source scenario runs file
source(paste0(path_rcode,"F10_scenario_runs_MESSAGE_2100.R"))


### RUN RESIDENTIAL SECTOR

# Additional inputs for offline runs (out of function)

# Baseline run
run="SSP2"
scenario_name = "SSP2_BL"

# EFC run: low demand
run <- "EFC_LOWDEM"
scenario_name <- "EFC_LOWDEM"

geo_level_report="R12"
sector="resid"
#report_type = c("MESSAGE","STURM","IRP")
report_type = c("MESSAGE","STURM","NGFS")
report_var=c("energy","material")

# Load prices - from MESSAGE
prices <- read_csv(paste0(path_in,"prices_R12.csv"))
# prices <- read_csv(paste0(path_in,"input_prices_R12.csv")) #old file

# Run scenario
dr <- run_scenario(run="SSP2",scenario_name = "SSP2_BL",prices, 
                   path_in, path_rcode, path_out,
                   geo_level_report="R12", sector="resid",
                   report_type = c("MESSAGE","STURM","NGFS"), 
                   #report_var=c("energy","material","vintage","dle"))
                   report_var=c("energy","material"))


# #Export results
# write_csv(dr, paste0(dirname(path_rcode),"/STURM_output/output_",run,"_",geo_level_report,"_resid.csv") )

# # Simple area plot
# pr <- ggplot(dr %>% 
# filter(commodity %in% c("resid_cool_electr",
#                         "resid_heat_biomass","resid_heat_coal","resid_heat_d_heat", 
#                         "resid_heat_electr", "resid_heat_gas", "resid_heat_lightoil",
#                         "resid_hotwater_biomass", "resid_hotwater_coal",  "resid_hotwater_d_heat",
#                         "resid_hotwater_electr", "resid_hotwater_gas",  "resid_hotwater_lightoil")) + 
#  geom_area(aes(x=year,y=value,fill=commodity))
# pr + facet_wrap(~node)


### RUN COMMERCIAL SECTOR

# Additional inputs for offline runs (out of function)
run="SSP2"
scenario_name = "SSP2_BL"

# EFC run: low demand
run <- "EFC_LOWDEM"
scenario_name <- "EFC_LOWDEM"


geo_level_report="R12"
sector="comm"
#report_type = c("MESSAGE","STURM","IRP")
report_type = c("MESSAGE","STURM","NGFS")
report_var=c("energy","material")
prices <- NULL


# Run scenario
dc <- run_scenario(run="SSP2",scenario_name = "SSP2_BL", prices=NULL, 
                   path_in, path_rcode, path_out,
                   geo_level_report="R12", sector="comm",
                   report_type = c("MESSAGE","STURM","NGFS"), 
                   report_var=c("energy","material"))
#dc <- run_scenario(run,prices=NULL, path_in, path_rcode, geo_level_report, sector)

# #Export results
# write_csv(dc, paste0(dirname(path_rcode),"/STURM_output/output_",run,"_",geo_level_report,"_comm.csv") )

# # Simple area plot
# pc <- ggplot(dc %>% 
#               filter(commodity %in% c("comm_cool_electr",
#                                       "comm_heat_biomass","comm_heat_coal","comm_heat_d_heat", 
#                                       "comm_heat_electr", "comm_heat_gas", "comm_heat_lightoil",
#                                       "comm_hotwater_biomass", "comm_hotwater_coal",  "comm_hotwater_d_heat",
#                                       "comm_hotwater_electr", "comm_hotwater_gas",  "comm_hotwater_lightoil", 
#                                       "comm_others_electr"
#                                       ))) + 
#   geom_area(aes(x=year,y=value,fill=commodity))
# pc + facet_wrap(~node)

