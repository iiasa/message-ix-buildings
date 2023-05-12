
## AGGREGATE AND REPORT RESULTS

## geo_level_report should be one of the column in the DF "geo_data"

fun_report_MESSAGE <- function(sector, report_var, report, geo_data, geo_level, geo_level_report){ # added "..." for en_m2_others (commercial sector)

print(paste0("Aggregate and report results - MESSAGE runs"))

# End-uses  
if(sector == "resid"){end_uses <- c("heat","cool","hotwater","other_uses")}
if(sector == "comm"){end_uses <- c("heat","cool","hotwater","other_uses")}

u_EJ_GWa <- 31.71

#rn=5 # decimals for rounding

## Initiate dataframe - building stock results
output <- data.frame()

## Energy results
if ("energy" %in% report_var){
  if (paste(geo_level_report) %in% names(report$en_stock)) {
    en_stock_aggr <- report$en_stock} else {
    en_stock_aggr <- report$en_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
  
  # others_TJ -- use instead: other_uses_TJ
  
  # Aggregate results -  Energy - for MESSAGE
  en_stock_aggr <- en_stock_aggr %>%
    select(-c(stock_M, floor_Mm2, cool_ac_TJ, cool_fans_TJ)) %>%
    #mutate(en_tot_TJ = en_heat_TJ + en_cool_TJ)
    pivot_longer(cols = c(paste0(end_uses,"_TJ")), names_to = "end_use", values_to = "en_TJ") %>%
    mutate(end_use = substr(end_use,1,nchar(end_use)-3)) %>%
    mutate(fuel = ifelse(end_use %in% c("cool","other_uses"), "electricity", fuel_heat)) %>%
    group_by_at(paste(c(geo_level_report, "year","end_use","fuel"))) %>%
    #group_by(region_gea,year,fuel) %>%
    summarise(en_tot_EJ = sum(en_TJ)/1e6) %>%
    ungroup()
  
  en_stock_aggr[,"reg_rep"] <- en_stock_aggr[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")
  
  en_stock_aggr <- en_stock_aggr %>%
    #mutate(region_gea = ifelse(region_gea == "LAC", "LAM", region_gea)) %>%
    mutate(node = paste(geo_level_report,reg_rep,sep="_")) %>% # use this if R11/R12 change in nodes definition
    #mutate(node = paste("R11",reg_rep,sep="_")) %>% # Tentative "R11" hardcoded
    mutate(commodity = fuel) %>%
    mutate(commodity = ifelse(commodity == "biomass_solid", "biomass",  commodity)) %>%
    mutate(commodity = ifelse(commodity == "electricity","electr",  commodity)) %>%
    mutate(commodity = ifelse(commodity == "oil", "lightoil",  commodity)) %>%
    mutate(commodity = ifelse(commodity == "district_heat", "d_heat",  commodity)) %>%
    mutate(commodity = paste(sector,end_use,commodity, sep="_")) %>%
    mutate(level = "final") %>%
    mutate(time = "year") %>%
    mutate(value = en_tot_EJ*u_EJ_GWa) %>%
    mutate(unit = "GWa") %>%
    select(node,commodity,level,year,time,value,unit)
  
  output <- rbind(output, en_stock_aggr)
}

# ## LIMIT BIOMASS FOR FSU (PLACEHOLDER)
# if (sector == "resid") {
#   en_stock_aggr <- en_stock_aggr %>%
#     #mutate(value = ifelse(node == "R12_FSU" & commodity == "resid_heat_biomass" & value > 5, 5, value)) %>%
#     mutate(value = ifelse(node == "R12_FSU" & commodity == "resid_hotwater_biomass", 0, value))
# } 
# 
# if (sector == "comm") {
#   en_stock_aggr <- en_stock_aggr %>%
#     mutate(value = ifelse(node == "R12_FSU" & commodity == "comm_heat_biomass", 0, value)) %>%
#     mutate(value = ifelse(node == "R12_FSU" & commodity == "comm_hotwater_biomass", 0, value))
# } 


# # Aggregate results -  Energy - for MESSAGE - TOTAL ENERGY BY FUEL
# en_stock_aggr <- en_stock_aggr %>%
#   select(-c(stock_M, floor_Mm2, cool_ac_TJ, cool_fans_TJ)) %>%
#   #mutate(en_tot_TJ = en_heat_TJ + en_cool_TJ)
#   pivot_longer(cols = c("heat_TJ","cool_TJ","hot_water_TJ"), names_to = "end_use", values_to = "en_TJ") %>%
#   mutate(end_use = substr(end_use,1,nchar(end_use)-3)) %>%
#   mutate(fuel = ifelse(end_use == "cool", "electricity", fuel_heat)) %>%
#   group_by_at(paste(c(geo_level_report, "year","fuel"))) %>%
#   #group_by(region_gea,year,fuel) %>%
#   summarise(en_tot_EJ = sum(en_TJ)/1e6) %>%
#   ungroup() %>%
#   #mutate(region_gea = ifelse(region_gea == "LAC", "LAM", region_gea)) %>%
#   mutate(node = paste0(geo_level_report,"_",reg_rep)) %>%
#   #mutate(node = paste0("R11_",region_gea)) %>%  
#   mutate(commodity = fuel) %>%
#   mutate(commodity = ifelse(commodity == "biomass_solid", "biomass",  commodity)) %>%
#   mutate(commodity = ifelse(commodity == "electricity","electr",  commodity)) %>%
#   mutate(commodity = ifelse(commodity == "oil", "lightoil",  commodity)) %>%
#   mutate(commodity = ifelse(commodity == "district_heat", "d_heat",  commodity)) %>%
#   mutate(commodity = paste0("resid_",commodity)) %>%
#   mutate(level = "final") %>%
#   mutate(time = "year") %>%
#   mutate(value = en_tot_EJ*u_EJ_GWa) %>%
#   mutate(unit = "GWa") %>%
#   select(node,commodity,level,year,time,value,unit)

# # Aggregate - region level - total energy heat/cool
# en_stock_aggr <- en_stock %>%
#   group_by(year) %>% #region_gea,fuel_heat,
#   summarize(
#   cool_EJ = sum(cool_TJ)/1e6,
#   heat_EJ = sum(heat_TJ)/1e6)



# Aggregate results - Material - for MESSAGE
# NOTE: only permanent buildings considered (no data for slums)
if ("material" %in% report_var){
  if (paste(geo_level_report) %in% names(report$mat_stock)) {
    mat_stock_aggr <- report$mat_stock %>% filter(mat == "perm")} else {
    mat_stock_aggr <- report$mat_stock %>% filter(mat == "perm")%>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
  
  
  mat_stock_aggr <- mat_stock_aggr %>%
    group_by_at(paste(c(geo_level_report, "year","material"))) %>%
    summarise(mat_demand = sum(mat_demand_Mt),
              mat_scrap = sum(mat_scrap_Mt),
              floor_construction = sum(floor_new_Mm2),
              floor_demolition = sum(floor_dem_Mm2)) %>%
    ungroup() %>%
    mutate(mat_int_demand = ifelse(floor_construction == 0, 0, mat_demand/floor_construction)) %>% # Mat int: t/m2 (Mt/Mm2)
    mutate(mat_int_scrap = ifelse(floor_demolition == 0, 0, mat_scrap/floor_demolition)) %>% # Mat int: t/m2 (Mt/Mm2)
    pivot_longer(cols = c("mat_demand", "mat_scrap", 
                          "floor_construction", "floor_demolition", 
                          "mat_int_demand", "mat_int_scrap"), 
                 names_to = "commodity", values_to = "value")
    
  mat_stock_aggr[,"reg_rep"] <- mat_stock_aggr[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")
  
  mat_stock_aggr <- mat_stock_aggr %>%
    mutate(node = paste0(geo_level_report,"_",reg_rep)) %>%  # use this if R11/R12 change in nodes definition
    #mutate(node = paste("R11",reg_rep,sep="_")) %>% # Tentative "R11" hardcoded
    mutate(unit = ifelse(commodity %in% c("mat_demand", "mat_scrap"), "Mt/y",
                         ifelse(commodity %in% c("floor_construction", "floor_demolition"), "Mm2/y",
                                ifelse(commodity %in% c("mat_int_demand", "mat_int_scrap"), "Mt/Mm2",as.numeric(NA))))) %>%
    mutate(commodity = ifelse(commodity %in% c("floor_construction", "floor_demolition"),
                              paste(sector,commodity, sep="_"),
                              paste(sector,commodity,material, sep="_"))) %>%    
    mutate(level = "demand") %>%
    mutate(time = "year") %>%
    select(node,commodity,level,year,time,value,unit)
  
  mat_stock_aggr <- mat_stock_aggr %>% distinct()
  
  output <- rbind(output, mat_stock_aggr)
}

  # mat_stock_aggr <- mat_stock_aggr %>%
  #   pivot_longer(cols = c("mat_demand_Mt","mat_scrap_Mt"), names_to = "flow", values_to = "value") %>%
  #   mutate(flow = substr(flow,1,nchar(flow)-3)) %>%
  #   group_by_at(paste(c(geo_level_report, "year","flow","material"))) %>%
  # #group_by(region_gea,year,fuel) %>%
  # summarise(value = sum(value)) %>%
  #   ungroup() 
  # 
  # mat_stock_aggr[,"reg_rep"] <- mat_stock_aggr[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")
  # 
  # mat_stock_aggr <- mat_stock_aggr %>%
  #   #mutate(region_gea = ifelse(region_gea == "LAC", "LAM", region_gea)) %>%
  #   mutate(node = paste0(geo_level_report,"_",reg_rep)) %>%
  #   #mutate(node = paste0("R11_",region_gea)) %>% 
  #   mutate(commodity = paste("resid",flow,material, sep="_")) %>%    
  #   mutate(level = "final") %>%
  #   mutate(time = "year") %>%
  #   mutate(unit = "Mt/y") %>%
  #   select(node,commodity,level,year,time,value,unit)


  
return(output)
  
}