
## AGGREGATE AND REPORT RESULTS

## geo_level_report should be one of the column in the DF "geo_data"

fun_report_NGFS <- function(report, report_var, geo_data, geo_level, geo_level_report, sector, scenario_name, yrs, path_out){

  print(paste0("Aggregate and report results - IRP Report"))
  
  # # End-uses  
  # if(sector == "resid"){end_uses <- c("heat","cool","hotwater")}
  # if(sector == "comm"){end_uses <- c("heat","cool","hotwater","others")}
  
  #rn=5 # decimals for rounding
  
  # INITIATE RESULTS DATAFRAME
  exp_ngfs <- data.frame()
  
  # sector name
  #lab_sector <- "Buildings"
  lab_sector <- "Residential and Commercial"
  
  lab_arch <- c("Single-family","Multi-family","Slum")
  
  lab_top_floor <- "Energy Service"
  lab_top_energy <- "Final Energy"
  
  lab_var_floor <- "Floor space"
  
  rnd <- 5 # Rounding
   
  ### ENERGY RESULTS ###
  ## Note: floorspace (stock) included here and not under material results
  
  #if ("energy" %in% report_var){
    
    ### Aggregate results -  Floorspace total
    if (paste(geo_level_report) %in% names(report$en_stock)) {
      stock_tot_ngfs <- report$en_stock} else {
        stock_tot_ngfs <- report$en_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
    
    stock_tot_ngfs <- stock_tot_ngfs %>%
      select(-c(stock_M, heat_TJ, cool_TJ, cool_ac_TJ, cool_fans_TJ)) %>%
      group_by_at(paste(c(geo_level_report,"scenario", "year","arch"))) %>%
      summarise(floor_m2 = round(sum(floor_Mm2)/1e3,rnd)) %>% #bn m2
      mutate(top = lab_top_floor, sector = lab_sector) %>%
      ungroup()
  

    if(sector == "resid"){
      stock_tot_ngfs <- stock_tot_ngfs %>% 
        mutate(subsector = "Residential", 
               product = ifelse(arch == "sfh", lab_arch[1], 
                                ifelse(arch == "mfh", lab_arch[2],
                                       ifelse(arch == "inf",lab_arch[3], NA))),
               var = lab_var_floor) %>%
        mutate(Variable = paste(top, sector, subsector,product,var, sep = "|"))
    }
    if(sector == "comm"){
      stock_tot_ngfs <- stock_tot_ngfs %>% 
        mutate(subsector = "Commercial") %>%
        mutate(Variable = paste(top,sector,subsector,
                                #product,
                                sep = "|"))}
    
    stock_tot_ngfs[,"Region"] <- stock_tot_ngfs[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")
    
    stock_tot_ngfs <- stock_tot_ngfs %>% 
      mutate(Model ="MESSAGEix-buildings", Scenario = scenario_name, Unit = "bn m2/yr") %>%
      pivot_wider(names_from = "year", values_from = "floor_m2") %>%
      select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs[-1])))
    
    exp_ngfs <- bind_rows(exp_ngfs,stock_tot_ngfs)
    
    
    ## Aggregate energy results
    if (paste(geo_level_report) %in% names(report$en_stock)) {
      en_ngfs <- report$en_stock} else {
      en_ngfs <- report$en_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
    
    # Results by end-use
    en_ngfs_enduse <- en_ngfs %>%
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
      mutate(top = "Final Energy", sector = lab_sector) %>%
      mutate(product = "generic") %>%
      mutate_cond(commodity == "heat_EJ", product = paste0("Heating|Space")) %>%
      mutate_cond(commodity == "hotwater_EJ", product = paste0("Heating|Water")) %>%
      mutate_cond(commodity == "cool_EJ", product = paste0("Cooling")) %>%
      mutate_cond(commodity == "other_uses_EJ", product = paste0("Other")) %>%
      select(-commodity)
    
    # Results by energy carrier
    en_ngfs_carrier <- bind_rows(
      # Heating
      en_ngfs %>%
      select(-c(stock_M,floor_Mm2,
                cool_TJ,other_uses_TJ,
                cool_ac_TJ, cool_fans_TJ)) %>%
      pivot_longer(cols = c("heat_TJ","hotwater_TJ"),
                   names_to = "commodity", values_to = "value") %>%
      group_by_at(paste(c(geo_level_report,"scenario", "year","fuel_heat"))) %>%
      summarise(value = round(sum(value)/1e6,5)) %>% # convert to EJ/yr
      ungroup() %>%
      filter(fuel_heat != "v_no_heat") %>%
      rename(carrier = fuel_heat),
      # Cooling and other uses (electricity only)
      en_ngfs %>%
        select(-c(stock_M,floor_Mm2,
                  heat_TJ,hotwater_TJ,
                  cool_ac_TJ, cool_fans_TJ)) %>%
        pivot_longer(cols = c("cool_TJ","other_uses_TJ"),
                     names_to = "commodity", values_to = "value") %>%
        group_by_at(paste(c(geo_level_report,"scenario", "year","fuel_cool"))) %>%
        summarise(value = round(sum(value)/1e6,5)) %>% # convert to EJ/yr
        ungroup() %>%
        rename(carrier = fuel_cool)
      ) %>%
      mutate(top = "Final Energy", sector = lab_sector) %>%
      mutate(product = "generic") %>%
      mutate_cond(carrier == "biomass_solid", product = paste0("Solids|Biomass")) %>%
      mutate_cond(carrier == "coal", product = paste0("Solids|Coal")) %>%
      mutate_cond(carrier == "district_heat", product = paste0("Heat")) %>%
      mutate_cond(carrier == "electricity", product = paste0("Electricity")) %>%
      mutate_cond(carrier == "gas", product = paste0("Gases")) %>%
      mutate_cond(carrier == "oil", product = paste0("Liquids")) %>%
      select(-carrier) %>%
      group_by_at(paste(c(geo_level_report,"scenario", "year","top","sector","product"))) %>% # Aggregate results from different end-uses
      summarise(value = sum(value)) %>% # convert to EJ/yr
      ungroup() 
    
    if(sector == "resid"){
      en_ngfs <- bind_rows(en_ngfs_enduse, en_ngfs_carrier) %>% 
        mutate(subsector = "Residential") %>%
        mutate(Variable = paste(top,sector,subsector,product, sep = "|"))
    }
    if(sector == "comm"){en_ngfs <- bind_rows(en_ngfs_enduse, en_ngfs_carrier) %>% 
      mutate(subsector = "Commercial") %>%
      mutate(Variable = paste(top,sector,subsector,product,sep = "|"))}
    
    en_ngfs[,"Region"] <- en_ngfs[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")
    
    en_ngfs <- en_ngfs %>% 
      mutate(Model ="MESSAGEix-buildings", Scenario = scenario_name, Unit = "EJ/yr") %>%
      pivot_wider(names_from = "year", values_from = "value") %>%
      select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs[-1]))) %>%
      arrange(Region,Variable)
    
    exp_ngfs <- bind_rows(exp_ngfs,en_ngfs)
    
  #}
  
  
  
  # ### MATERIAL RESULTS ### --- MATERIALS NOT IN NGFS
  # 
  # # Aggregate results - Constructions / Demolitions
  # if ("material" %in% report_var){
  # 
  #   if (paste(geo_level_report) %in% names(report$mat_stock)) {
  #     stock_var_ngfs <- report$mat_stock} else {
  #       stock_var_ngfs <- report$mat_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
  #   
  #   
  #   stock_var_ngfs <- stock_var_ngfs %>%
  #     group_by_at(paste(c(geo_level_report, "year","arch"))) %>%
  #     summarise(
  #               floor_construction = round(sum(floor_new_Mm2)*1e6,0),
  #               floor_demolition = round(sum(floor_dem_Mm2)*1e6,0) 
  #               ) %>%
  #     ungroup() %>%
  #     pivot_longer(cols = c(
  #                           "floor_construction", "floor_demolition"), 
  #                  names_to = "commodity", values_to = "value") %>%
  #     mutate(top = ifelse(commodity == "floor_demolition", "Stock Retirement", 
  #                         ifelse(commodity == "floor_construction", "Stock Addition", NA))
  #            , sector = lab_sector
  #            ) %>%
  #     ungroup()
  #   
  #   if(sector == "resid"){
  #     stock_var_ngfs <- stock_var_ngfs %>% 
  #       mutate(subsector = "Residential", 
  #              product = ifelse(arch == "sfh", "Single-Family Houses", 
  #                               ifelse(arch == "mfh","Multi-Family Houses",
  #                                      ifelse(arch == "inf","Informal Houses", NA)))) %>%
  #       mutate(Variable = paste(top,
  #                               sector,
  #                               subsector,product, sep = "|"))
  #   }
  #   if(sector == "comm"){stock_var_ngfs <- stock_var_ngfs %>% 
  #     mutate(subsector = "Commercial") %>%
  #     mutate(Variable = paste(top,
  #                             sector,
  #                             subsector,sep = "|"))}
  #   
  #   stock_var_ngfs[,"Region"] <- stock_var_ngfs[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")
  #   
  #   stock_var_ngfs <- stock_var_ngfs %>% 
  #     mutate(Model ="MESSAGEix-buildings", Scenario = scenario_name, Unit = "m^2/yr") %>%
  #     pivot_wider(names_from = "year", values_from = "value") %>%
  #     select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs[-1])))
  #   
  #   
  #   
  #     # Aggregate results - Materials
  #   
  #   if (paste(geo_level_report) %in% names(report$mat_stock)) {mat_ngfs <- report$mat_stock} else {
  #       mat_ngfs <- report$mat_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
  #   
  #   if(sector == "resid"){mat_ngfs <- mat_ngfs %>% mutate(subsector = "Residential") %>% filter(arch != "inf")} # Exclude informal buildings (no material data)
  #   if(sector == "comm"){mat_ngfs <- mat_ngfs %>% mutate(subsector = "Commercial")}
  #   
  #   mat_ngfs <- mat_ngfs %>%
  #     #select(-c(stock_M, heat_TJ, cool_TJ, cool_ac_TJ, cool_fans_TJ)) %>%
  #     group_by_at(paste(c(geo_level_report, "year","material"))) %>%
  #     summarise(mat_stock = round(sum(mat_stock_Mt),3), # convert to Mt/yr
  #               mat_demand = round(sum(mat_demand_Mt),3), # convert to Mt/yr
  #               mat_scrap = round(sum(mat_scrap_Mt),3) # convert to Mt/yr
  #               ) %>%
  #     # summarise(mat_stock = round(sum(mat_stock_Mt)*1e3,3), # convert to kt/yr
  #     #           mat_demand = round(sum(mat_demand_Mt)*1e3,3), # convert to kt/yr
  #     #           mat_scrap = round(sum(mat_scrap_Mt)*1e3,3) # convert to kt/yr
  #     # ) %>%
  #     ungroup() %>%
  #     pivot_longer(cols = c("mat_stock", "mat_demand", "mat_scrap"), 
  #                  names_to = "commodity", values_to = "value") %>%
  #     mutate(top = ifelse(commodity == "mat_demand", "Material Demand", 
  #                         ifelse(commodity == "mat_scrap", "Total Scrap", 
  #                               ifelse(commodity == "mat_stock", "Material Stock", NA))),
  #            sector = lab_sector) %>%
  #     mutate(material_name = "generic") %>%
  #     mutate_cond(material == "aluminum", material_name = paste0("Non-Ferrous Metals|Aluminium")) %>%
  #     mutate_cond(material == "cement", material_name = paste0("Cement")) %>%
  #     mutate_cond(material == "concrete", material_name = paste0("Concrete")) %>%
  #     mutate_cond(material == "copper", material_name = paste0("Non-Ferrous Metals|Copper")) %>%
  #     mutate_cond(material == "glass", material_name = paste0("Glass")) %>%
  #     mutate_cond(material == "steel", material_name = paste0("Steel")) %>%
  #     mutate_cond(material == "wood", material_name = paste0("Construction Wood")) %>%
  #     ungroup()
  #     
  #   if(sector == "resid"){mat_ngfs <- mat_ngfs %>% mutate(subsector = "Residential")}
  #   if(sector == "comm"){mat_ngfs <- mat_ngfs %>% mutate(subsector = "Commercial")}
  #   
  #   mat_ngfs[,"Region"] <- mat_ngfs[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")
  #   
  #   mat_ngfs <- mat_ngfs %>% 
  #     mutate(Variable = paste(top,sector,subsector,material_name, sep = "|")) %>%
  #     mutate(Model ="MESSAGEix-buildings", Scenario = scenario_name, Unit = "Mt/yr") %>%
  #     # mutate(Model ="MESSAGEix-buildings", Scenario = scenario_name, Unit = "kt/yr") %>%
  #     pivot_wider(names_from = "year", values_from = "value") %>%
  #     select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs[-1])))
  #   
  #   #mat_ngfs <- mat_ngfs %>% distinct()
  # 
  #   # Bind energy and material results
  #   exp_ngfs <- rbind(exp_ngfs, stock_var_ngfs, mat_ngfs)
  # 
  # }
  
  
  #Export results
  write_csv(exp_ngfs, paste0(path_out,"report_NGFS_",scenario_name,"_", sector, "_",geo_level_report, ".csv") )
  
  
}