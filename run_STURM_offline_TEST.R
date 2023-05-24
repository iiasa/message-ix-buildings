# Script to be run in Rstudio

library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("./STURM_model/F10_scenario_runs_MESSAGE_2100.R")

rcode_path <- paste(getwd(),"/STURM_model/",sep="")
data_path <- paste(getwd(),"/STURM_data/",sep="")
rout_path <- paste(getwd(),"/STURM_output/",sep="")

prices<-read.csv(paste0(data_path,"input_prices_R12.csv"))
scen <- "NAV_Dem-NPi-ref"
scen <- "NAV_Dem-NPi-all"
#scen <- "SDP_EI-NPi"
#scen <- "NAV_Dem-NPi-tec"
#clim_scen <- "BL"
sect <- "resid"
#sect <- "comm"

# Additional settings moved from "run_scenario" function - to run model out of the function
run=scen
#scenario_name=paste(ssp_scen,clim_scen,sep="_"),
scenario_name=scen
prices=prices
path_in=data_path
path_rcode=rcode_path
path_out=rout_path
geo_level_report="R12"
sector=sect
#report_type = c("MESSAGE","NGFS","STURM"), # Available reports: c("MESSAGE","STURM","IRP","NGFS")
report_type = c("MESSAGE","STURM","NAVIGATE") # Available reports: c("MESSAGE","STURM","IRP","NGFS","NAVIGATE")
report_var=c("energy","material") # Available report variables: c("energy","material","vintage","dle")


# # call STURM
# sturm_scenarios <- run_scenario(run=scen, 
#                                 #scenario_name=paste(ssp_scen,clim_scen,sep="_"),
#                                 scenario_name=ssp_scen,
#                                 prices=prices,
#                                 path_in=data_path,
#                                 path_rcode=rcode_path,
#                                 path_out=rout_path,
#                                 geo_level_report="R12",
#                                 sector=sect,
#                                 #report_type = c("MESSAGE","NGFS","STURM"), # Available reports: c("MESSAGE","STURM","IRP","NGFS")
#                                 report_type = c("MESSAGE","STURM","NAVIGATE"), # Available reports: c("MESSAGE","STURM","IRP","NGFS","NAVIGATE")
#                                 report_var=c("energy","material") # Available report variables: c("energy","material","vintage","dle")
#                                 )
# 
# # write results to csv file
# write.csv(sturm_scenarios,paste("./temp/",sect,"_sturm.csv",sep=""),row.names=F)



