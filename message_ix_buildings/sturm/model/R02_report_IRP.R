
## AGGREGATE AND REPORT RESULTS

## geo_level_report should be one of the column in the DF "geo_data"

fun_report_IRP <- function(report, report_var, geo_data, geo_level, geo_level_report, sector, scenario_name, yrs, path_out){

  print(paste0("Aggregate and report results - IRP Report"))
  
  # # End-uses  
  # if(sector == "resid"){end_uses <- c("heat","cool","hotwater")}
  # if(sector == "comm"){end_uses <- c("heat","cool","hotwater","others")}
  
  #rn=5 # decimals for rounding
  
  # INITIATE RESULTS DATAFRAME
  exp_irp <- data.frame()
  
  # sector name
  sector_label <- "Buildings"
  #sector_label <- "Residential and Commercial"
  
  ### ENERGY RESULTS ###
  ## Note: floorspace (stock) included here and not under material results
  
  if ("energy" %in% report_var){
    
    ### Aggregate results -  Floorspace total
    if (paste(geo_level_report) %in% names(report$en_stock)) {
      stock_tot_irp <- report$en_stock} else {
        stock_tot_irp <- report$en_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
    
    stock_tot_irp <- stock_tot_irp %>%
      select(-c(stock_M, heat_TJ, cool_TJ, cool_ac_TJ, cool_fans_TJ)) %>%
      group_by_at(paste(c(geo_level_report,"scenario", "year","arch"))) %>%
      summarise(floor_m2 = round(sum(floor_Mm2)*1e6,0)) %>%
      mutate(top = "Product Stock", sector = sector_label) %>%
      ungroup()
  

    if(sector == "resid"){
      stock_tot_irp <- stock_tot_irp %>% 
        mutate(subsector = "Residential", 
               product = ifelse(arch == "sfh", "Single-Family Houses", 
                                ifelse(arch == "mfh","Multi-Family Houses",
                                       ifelse(arch == "inf","Informal Houses", NA)))) %>%
        mutate(Variable = paste(top, sector, subsector,product, sep = "|"))
    }
    if(sector == "comm"){
      stock_tot_irp <- stock_tot_irp %>% 
        mutate(subsector = "Commercial") %>%
        mutate(Variable = paste(top,sector,subsector,
                                #product,
                                sep = "|"))}
    
    stock_tot_irp[,"Region"] <- stock_tot_irp[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")
    
    stock_tot_irp <- stock_tot_irp %>% 
      mutate(Model ="MESSAGEix-buildings", Scenario = scenario_name, Unit = "m^2") %>%
      pivot_wider(names_from = "year", values_from = "floor_m2") %>%
      select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs[-1])))
    
    exp_irp <- bind_rows(exp_irp,stock_tot_irp)
    
    
    ## Aggregate energy results
    if (paste(geo_level_report) %in% names(report$en_stock)) {
      en_irp <- report$en_stock} else {
      en_irp <- report$en_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
    
    en_irp <- en_irp %>%
      select(-c(stock_M,floor_Mm2,
                #heat_TJ, cool_TJ,
                cool_ac_TJ, cool_fans_TJ)) %>%
      group_by_at(paste(c(geo_level_report,"scenario", "year"))) %>%
      summarise(heat_EJ = round(sum(heat_TJ)/1e6,5), # convert to EJ/yr
                cool_EJ = round(sum(cool_TJ)/1e6,5),
                hotwater_EJ = round(sum(hotwater_TJ)/1e6,5),
                other_uses_EJ = round(sum(other_uses_TJ)/1e6,5)
      ) %>%
      ungroup() %>%
      pivot_longer(cols = c("heat_EJ","cool_EJ","hotwater_EJ","other_uses_EJ"),
                   names_to = "commodity", values_to = "value") %>%
      mutate(top = "Final Energy", sector = sector_label) %>%
      mutate(product = "generic") %>%
      mutate_cond(commodity == "heat_EJ", product = paste0("Heating|Space")) %>%
      mutate_cond(commodity == "hotwater_EJ", product = paste0("Heating|Water")) %>%
      mutate_cond(commodity == "cool_EJ", product = paste0("Cooling")) %>%
      mutate_cond(commodity == "other_uses_EJ", product = paste0("Other")) %>%
      ungroup()
    
    if(sector == "resid"){
      en_irp <- en_irp %>% 
        mutate(subsector = "Residential") %>%
        mutate(Variable = paste(top,sector,subsector,product, sep = "|"))
    }
    if(sector == "comm"){en_irp <- en_irp %>% 
      mutate(subsector = "Commercial") %>%
      mutate(Variable = paste(top,sector,subsector,sep = "|"))}
    
    en_irp[,"Region"] <- en_irp[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")
    
    en_irp <- en_irp %>% 
      mutate(Model ="MESSAGEix-buildings", Scenario = scenario_name, Unit = "EJ/yr") %>%
      pivot_wider(names_from = "year", values_from = "value") %>%
      select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs[-1])))
    
    exp_irp <- bind_rows(exp_irp,en_irp)
    
  }
  
  
  
  ### MATERIAL RESULTS ###
 
  # Aggregate results - Constructions / Demolitions
  if ("material" %in% report_var){
  
    if (paste(geo_level_report) %in% names(report$mat_stock)) {
      stock_var_irp <- report$mat_stock} else {
        stock_var_irp <- report$mat_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
    
    
    stock_var_irp <- stock_var_irp %>%
      group_by_at(paste(c(geo_level_report, "year","arch"))) %>%
      summarise(
                floor_construction = round(sum(floor_new_Mm2)*1e6,0),
                floor_demolition = round(sum(floor_dem_Mm2)*1e6,0) 
                ) %>%
      ungroup() %>%
      pivot_longer(cols = c(
                            "floor_construction", "floor_demolition"), 
                   names_to = "commodity", values_to = "value") %>%
      mutate(top = ifelse(commodity == "floor_demolition", "Stock Retirement", 
                          ifelse(commodity == "floor_construction", "Stock Addition", NA))
             , sector = sector_label
             ) %>%
      ungroup()
    
    if(sector == "resid"){
      stock_var_irp <- stock_var_irp %>% 
        mutate(subsector = "Residential", 
               product = ifelse(arch == "sfh", "Single-Family Houses", 
                                ifelse(arch == "mfh","Multi-Family Houses",
                                       ifelse(arch == "inf","Informal Houses", NA)))) %>%
        mutate(Variable = paste(top,
                                sector,
                                subsector,product, sep = "|"))
    }
    if(sector == "comm"){stock_var_irp <- stock_var_irp %>% 
      mutate(subsector = "Commercial") %>%
      mutate(Variable = paste(top,
                              sector,
                              subsector,sep = "|"))}
    
    stock_var_irp[,"Region"] <- stock_var_irp[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")
    
    stock_var_irp <- stock_var_irp %>% 
      mutate(Model ="MESSAGEix-buildings", Scenario = scenario_name, Unit = "m^2/yr") %>%
      pivot_wider(names_from = "year", values_from = "value") %>%
      select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs[-1])))
    
    
    
      # Aggregate results - Materials
    
    if (paste(geo_level_report) %in% names(report$mat_stock)) {mat_irp <- report$mat_stock} else {
        mat_irp <- report$mat_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
    
    if(sector == "resid"){mat_irp <- mat_irp %>% mutate(subsector = "Residential") %>% filter(arch != "inf")} # Exclude informal buildings (no material data)
    if(sector == "comm"){mat_irp <- mat_irp %>% mutate(subsector = "Commercial")}
    
    mat_irp <- mat_irp %>%
      #select(-c(stock_M, heat_TJ, cool_TJ, cool_ac_TJ, cool_fans_TJ)) %>%
      group_by_at(paste(c(geo_level_report, "year","material"))) %>%
      summarise(mat_stock = round(sum(mat_stock_Mt),3), # convert to Mt/yr
                mat_demand = round(sum(mat_demand_Mt),3), # convert to Mt/yr
                mat_scrap = round(sum(mat_scrap_Mt),3) # convert to Mt/yr
                ) %>%
      # summarise(mat_stock = round(sum(mat_stock_Mt)*1e3,3), # convert to kt/yr
      #           mat_demand = round(sum(mat_demand_Mt)*1e3,3), # convert to kt/yr
      #           mat_scrap = round(sum(mat_scrap_Mt)*1e3,3) # convert to kt/yr
      # ) %>%
      ungroup() %>%
      pivot_longer(cols = c("mat_stock", "mat_demand", "mat_scrap"), 
                   names_to = "commodity", values_to = "value") %>%
      mutate(top = ifelse(commodity == "mat_demand", "Material Demand", 
                          ifelse(commodity == "mat_scrap", "Total Scrap", 
                                ifelse(commodity == "mat_stock", "Material Stock", NA))),
             sector = sector_label) %>%
      mutate(material_name = "generic") %>%
      mutate_cond(material == "aluminum", material_name = paste0("Non-Ferrous Metals|Aluminium")) %>%
      mutate_cond(material == "cement", material_name = paste0("Cement")) %>%
      mutate_cond(material == "concrete", material_name = paste0("Concrete")) %>%
      mutate_cond(material == "copper", material_name = paste0("Non-Ferrous Metals|Copper")) %>%
      mutate_cond(material == "glass", material_name = paste0("Glass")) %>%
      mutate_cond(material == "steel", material_name = paste0("Steel")) %>%
      mutate_cond(material == "wood", material_name = paste0("Construction Wood")) %>%
      ungroup()
      
    if(sector == "resid"){mat_irp <- mat_irp %>% mutate(subsector = "Residential")}
    if(sector == "comm"){mat_irp <- mat_irp %>% mutate(subsector = "Commercial")}
    
    mat_irp[,"Region"] <- mat_irp[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")
    
    mat_irp <- mat_irp %>% 
      mutate(Variable = paste(top,sector,subsector,material_name, sep = "|")) %>%
      mutate(Model ="MESSAGEix-buildings", Scenario = scenario_name, Unit = "Mt/yr") %>%
      # mutate(Model ="MESSAGEix-buildings", Scenario = scenario_name, Unit = "kt/yr") %>%
      pivot_wider(names_from = "year", values_from = "value") %>%
      select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs[-1])))
    
    #mat_irp <- mat_irp %>% distinct()

    # Bind energy and material results
    exp_irp <- rbind(exp_irp, stock_var_irp, mat_irp)
  
  }
  
  
  #Export results
  write_csv(exp_irp, paste0(path_out,"report_IRP_",scenario_name,"_", sector, "_",geo_level_report, ".csv") )
  
  
}