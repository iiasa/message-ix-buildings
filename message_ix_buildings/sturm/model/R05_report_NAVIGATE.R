
## AGGREGATE AND REPORT RESULTS -- by sector: residential R or commercial C. aggregated reporting (R+C) needs to come later.

## geo_level_report should be one of the column in the DF "geo_data"

fun_report_NAVIGATE <- function(report, report_var, geo_data, geo_level, geo_level_report, sector, scenario_name, yrs, path_out){ # , path_in, ct_bld , ct_ren_eneff , ren_en_sav_scen ## currently not used - for U-values reporting

  print(paste0("Aggregate and report results - NAVIGATE Report"))
  
  # # End-uses  
  # if(sector == "resid"){end_uses <- c("heat","cool","hotwater")}
  # if(sector == "comm"){end_uses <- c("heat","cool","hotwater","others")}
  
  #rn=5 # decimals for rounding
  
  # INITIATE RESULTS DATAFRAME
  exp_rep <- data.frame()
  
  # Model name
  model_name <- "MESSAGEix-Buildings"
  
  # sector name # Sector level not labelled for results specific to Residential / Commercial
  #lab_sector <- "Buildings"
  #lab_sector <- "Residential and Commercial"
  
  if(sector == "resid"){lab_sector <- c("Residential")} else if(sector == "comm"){lab_sector <- c("Commercial")}
  
  lab_arch <- c("Single-family","Multi-family","Slum")
  
  lab_top_floor <- "Energy Service"
  lab_top_energy <- "Final Energy"
  
  lab_var_floor <- "Floor Space"
  
  rnd <- 5 # Rounding
   
  ### ENERGY RESULTS ###
  ## Note: floorspace (stock) included here and not under material results
  
  #if ("energy" %in% report_var){
    
  ### Aggregate results -  Floorspace total
  if (paste(geo_level_report) %in% names(report$en_stock)) {
    stock_rep <- report$en_stock} else {
      stock_rep <- report$en_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
    
  # report total results
  stock_rep_tot <- stock_rep %>%
    select(-c(stock_M, heat_TJ, cool_TJ, cool_ac_TJ, cool_fans_TJ)) %>%
    group_by_at(paste(c(geo_level_report,"scenario", "year"))) %>%
    summarise(floor_m2 = round(sum(floor_Mm2)/1e3,rnd)) %>% #bn m2
    mutate(top = lab_top_floor) %>%
    #mutate(sector = lab_sector) %>%
    ungroup() %>%
    mutate(subsector = lab_sector,
           var = lab_var_floor) %>%
    mutate(Variable = paste(top, #sector, 
                            subsector,var, sep = "|")) %>%
    rename_at(paste(geo_level_report),~paste("Region")) %>%
    mutate(Model = model_name, Scenario = scenario_name, Unit = "bn m2") %>%
    pivot_wider(names_from = "year", values_from = "floor_m2") %>%
    select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs)))
  
  exp_rep <- bind_rows(exp_rep,stock_rep_tot)

  
  # Report results by archetype (residential only)
  if(sector == "resid"){
    stock_rep_arch <- stock_rep %>%
      select(-c(stock_M, heat_TJ, cool_TJ, cool_ac_TJ, cool_fans_TJ)) %>%
      group_by_at(paste(c(geo_level_report,"scenario", "year","arch"))) %>%
      summarise(floor_m2 = round(sum(floor_Mm2)/1e3,rnd)) %>% #bn m2
      mutate(top = lab_top_floor) %>%
      #mutate(sector = lab_sector) %>%
      ungroup() %>%
      mutate(subsector = lab_sector, 
             product = ifelse(arch == "sfh", lab_arch[1], 
                              ifelse(arch == "mfh", lab_arch[2],
                                     ifelse(arch == "inf",lab_arch[3], NA))),
             var = lab_var_floor) %>%
      mutate(Variable = paste(top, #sector, 
                              subsector,product,var, sep = "|")) %>%
      rename_at(paste(geo_level_report),~paste("Region")) %>%
      mutate(Model = model_name, Scenario = scenario_name, Unit = "bn m2") %>%
      pivot_wider(names_from = "year", values_from = "floor_m2") %>%
      select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs)))
    
    exp_rep <- bind_rows(exp_rep,stock_rep_arch)
  }
  
  
  # Report renovation rate
  if(sector == "resid"){
  
    # first calculate total stock - renovated stock
    stock_tot <- stock_rep %>%
      group_by_at(paste(c(geo_level_report,"scenario", "year"))) %>%
      summarise(stock_tot = sum(stock_M)) %>%
      ungroup
    
    stock_ren_tot <- stock_rep %>%
      filter(eneff %in% c("sr11_std", "sr21_std", "sr31_std", "sr12_low", "sr22_low", "sr32_low")) %>%
      group_by_at(paste(c(geo_level_report,"scenario", "year"))) %>%
      summarise(stock_ren_tot = sum(stock_M)) %>%
      ungroup
    
    # Yearly renovation rate
    ren_rate_rep <- stock_tot %>%
      left_join(stock_ren_tot) %>%
      mutate(stock_ren_tot = ifelse(is.na(stock_ren_tot),0,stock_ren_tot)) %>%
      group_by_at(paste(c(geo_level_report,"scenario"))) %>%
      mutate(stock_ren = ifelse(year==yrs[2], stock_ren_tot, stock_ren_tot - lag(stock_ren_tot))) %>%
      mutate(stock_tot_lag = lag(stock_tot)) %>%
      ungroup %>%
      #mutate(rate_ren = round(stock_ren / stock_tot_lag / (year - lag(year)),rnd)) %>% # renovation rate based on total stock in the PREVIOUS time step
      mutate(rate_ren = ifelse(year==yrs[2], round(stock_ren / stock_tot / (yrs[2]-yrs[1]),rnd), 
                               round(stock_ren / stock_tot / (year - lag(year)),rnd))) %>% # renovation rate based on total stock in the CURRENT time step
      mutate(rate_ren = ifelse(rate_ren <0, 0, rate_ren)) %>%
      mutate(rate_ren = 100 * rate_ren) %>% # Report as %
      select_at(paste(c( "scenario", geo_level_report, "year", "rate_ren"))) %>%
      pivot_wider(names_from = "year", values_from = "rate_ren") %>%
      rename(Scenario = scenario) %>%
      rename_at(paste(geo_level_report),~paste("Region")) %>%
      mutate(Model = model_name, 
             Variable = paste0("Energy Service|", lab_sector, "|Renovation rate"),
             Unit = "%/yr") %>%
      relocate(Model, Scenario, Region, Variable, Unit)
    
    rm(stock_tot,stock_ren_tot)
    
    exp_rep <- bind_rows(exp_rep,ren_rate_rep) 
  }
  
  
  # ### REPORT U-VALUES -- TO BE REVISED!!! inputs to function to be completed
  # if(sector == "resid"){
  # 
  #   # import U-values
  #   
  #   u_val <- read_csv(paste0(path_in, "input_U_values.csv"))
  #   u_val <- u_val %>%
  #     mutate(eneff = substr(arch, 5,12)) %>%
  #     mutate(arch = substr(arch, 1,3)) %>%
  #     mutate(region_gea = substr(name, 1,3)) %>%
  #     select(region_gea, arch, eneff, u_val)
  #   
  #   if (sector =="comm"){
  #     u_val <- u_val %>% 
  #       filter(arch == "mfh") %>%
  #       mutate(arch = "comm")
  #   }
  #   
  #   # # U-values renovation -- already loaded: "ren_en_sav_scen"
  #   # ren_energy_savings <- read_csv(paste0(path_in,"ren_energy_savings.csv")) %>% 
  #   #   rename(eneff_f = eneff) %>%
  #   #   mutate(eneff_i = paste0("s",substr(eneff_f,3,3)), .before=eneff_f) %>%
  #   #   fun_toLong("ren_energy_savings") %>%
  #   #   filter(ssp == "SSP2") %>% # EDIT THIS! THIS IS SCENARIO SPECIFIC!
  #   #   select(-c(scenario,ssp))
  #   
  #   u_val_ren <- ct_bld %>%
  #     left_join(ct_ren_eneff) %>%
  #     filter(eneff_f != eneff_i) %>% # filter only renovated buildings
  #     left_join(ren_en_sav_scen %>% rename(eneff_f = eneff)) %>%
  #     left_join(u_val %>% rename(eneff_i = eneff, u_val_i = u_val)) %>%
  #     mutate(u_val = u_val_i * (1-ren_scl)) %>%
  #     rename(eneff = eneff_f) %>%
  #     select(-c(eneff_i, u_val_i, ren_scl)) %>%
  #     rename(u_val_ren = u_val)
  #   
  #   u_val_ren <- bind_rows( # add values for the base year
  #     u_val_ren %>% filter(year == 2020) %>% mutate(year = 2015),
  #     u_val_ren
  #   ) 
  #   
  #   # U-values slums: take values from eneff s1
  #   u_val_slum <- u_val %>%
  #     filter(eneff == "s1" & arch == "sfh") %>%
  #     select(-c(arch,eneff)) %>%
  #     rename(u_val_slum = u_val)
  #   
  #   # U-values: join all datasets
  #   u_val <- stock_rep %>%
  #     select(-c(stock_M, heat_TJ, cool_TJ, cool_ac_TJ, cool_fans_TJ, hotwater_TJ, other_uses_TJ)) %>%
  #     left_join(u_val) %>%
  #     left_join(u_val_ren) %>%
  #     left_join(u_val_slum) %>%
  #     mutate(u_val = ifelse(is.na(u_val), u_val_ren, u_val)) %>%
  #     mutate(u_val = ifelse(arch == "inf", u_val_slum, u_val)) %>%
  #     select(-c(u_val_slum, u_val_ren, region_gea))
  #   
  #   rm(u_val_slum, u_val_ren)
  #   
  #   # Average U-values: weighted on floorspace
  #   u_val_avg <- u_val %>%
  #     group_by(scenario, R12, year) %>%
  #     summarise(u_val = weighted.mean(u_val, floor_Mm2)) %>%
  #     ungroup %>%
  #     pivot_wider(names_from = "year", values_from = "u_val") %>%
  #     rename(Scenario = scenario, Region = R12) %>%
  #     mutate(Model = model_name, 
  #            Variable = paste0("Energy Service|", lab_sector, "|U-Value"),
  #            Unit = "W/m2/K") %>%
  #     relocate(Model, Scenario, Region, Variable, Unit)
  #   
  #   # # Average U-values: weighted on floorspace - New construction -- NOT REPORTED
  #   # # NOTE: this does not include new slum buildings!
  #   # u_val_new <- u_val %>% 
  #   #   filter(eneff %in% c("s51_std", "s52_low")) %>%
  #   #   group_by(scenario, R12, year) %>%
  #   #   summarise(u_val = weighted.mean(u_val, floor_Mm2)) %>%
  #   #   ungroup %>%
  #   #   pivot_wider(names_from = "year", values_from = "u_val") %>%
  #   #   rename(Scenario = scenario, Region = R12) %>%
  #   #   mutate(Model = model_name, 
  #   #          Variable = paste0("Energy Service|", lab_sector, "|New Construction|U-Value"),
  #   #          Unit = "W/m2/K") %>%
  #   #   relocate(Model, Scenario, Region, Variable, Unit)
  #   # 
  #   # # Average U-values: weighted on floorspace - Renovations -- NOT REPORTED
  #   # # NOTE: this does not include new slum buildings!
  #   # u_val_ren <- u_val %>% 
  #   #   filter(eneff %in% c("sr11_std", "sr21_std", "sr31_std", "sr12_low", "sr22_low", "sr32_low")) %>%
  #   #   group_by(scenario, R12, year) %>%
  #   #   summarise(u_val = weighted.mean(u_val, floor_Mm2)) %>%
  #   #   ungroup %>%
  #   #   pivot_wider(names_from = "year", values_from = "u_val") %>%
  #   #   rename(Scenario = scenario, Region = R12) %>%
  #   #   mutate(Model = model_name, 
  #   #          Variable = paste0("Energy Service|", lab_sector, "|Renovation|U-Value"),
  #   #          Unit = "W/m2/K") %>%
  #   #   relocate(Model, Scenario, Region, Variable, Unit)
  #   
  #   exp_rep <- bind_rows(exp_rep,u_val_avg) 
  #   
  # }  
  
  
  
  ## Aggregate energy results
  if (paste(geo_level_report) %in% names(report$en_stock)) {
    en_rep <- report$en_stock} else {
    en_rep <- report$en_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
  
  # Results by end-use
  en_rep_enduse <- en_rep %>%
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
    mutate(top = "Final Energy"
           #, sector = lab_sector
           ) %>%
    mutate(product = "generic") %>%
    mutate_cond(commodity == "heat_EJ", product = paste0("Heating|Space")) %>%
    mutate_cond(commodity == "hotwater_EJ", product = paste0("Heating|Water")) %>%
    mutate_cond(commodity == "cool_EJ", product = paste0("Cooling")) %>%
    mutate_cond(commodity == "other_uses_EJ", product = paste0("Other")) %>%
    select(-commodity)
  
  # Results by energy carrier
  en_rep_carrier <- bind_rows(
    
    # Heating
    en_rep %>%
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
    en_rep %>%
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
    
    mutate(top = "Final Energy"
           #, sector = lab_sector
           ) %>%
    mutate(product = "generic") %>%
    mutate_cond(carrier == "biomass_solid", product = paste0("Solids|Biomass")) %>%
    mutate_cond(carrier == "coal", product = paste0("Solids|Fossil")) %>% # EDITED from: "Solids|Coal"
    mutate_cond(carrier == "district_heat", product = paste0("Heat")) %>%
    mutate_cond(carrier == "electricity", product = paste0("Electricity")) %>%
    mutate_cond(carrier == "gas", product = paste0("Gases")) %>%
    mutate_cond(carrier == "oil", product = paste0("Liquids")) %>%
    select(-carrier) %>%
    group_by_at(paste(c(geo_level_report,"scenario", "year","top",
                        #"sector",
                        "product"))) %>% # Aggregate results from different end-uses
    summarise(value = sum(value)) %>% # convert to EJ/yr
    ungroup()
  
#  # Results total (heating and cooling)
#  en_rep_tot <- en_rep %>%
#    mutate(en_tot_TJ = heat_TJ + cool_TJ) %>%
#    group_by_at(paste(c(geo_level_report,"scenario", "year"))) %>%
#    summarise(en_tot_EJ = round(sum(en_tot_TJ)/1e6,5)) %>% # convert to EJ/yr
#    ungroup() %>%
#    pivot_longer(cols = c("en_tot_EJ"),
#                 names_to = "commodity", values_to = "value") %>%
#    mutate(top = "Final Energy"
#           #, sector = lab_sector
#           ) %>%
#    mutate(product = "total") %>%
#    select(-c(commodity))
  
  # Bind all final energy results
  en_rep <- bind_rows(en_rep_enduse, en_rep_carrier) %>% #,en_rep_tot # report only by end-use and energy carrier
    mutate(subsector = lab_sector) %>%
    mutate(Variable = paste(top,#sector,
                            subsector,product, sep = "|")) %>%
    rename_at(paste(geo_level_report),~paste("Region")) %>%
    mutate(Model = model_name, Scenario = scenario_name, Unit = "EJ/yr") %>%
    pivot_wider(names_from = "year", values_from = "value") %>%
    select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs))) %>%
    arrange(Region,Variable) %>%
    mutate(Variable = gsub(pattern="\\|total",replacement="", x=Variable)) #replace string. Notice "|" is a special character and is escaped by "\\|"
  
  exp_rep <- bind_rows(exp_rep,en_rep)
    
  #}
  


  
  # ### STOCK RESULTS - NOT INCLUDED ###
  # # Aggregate results - Constructions / Demolitions
  # 
  #   if (paste(geo_level_report) %in% names(report$mat_stock)) {
  #     stock_var_rep <- report$mat_stock} else {
  #       stock_var_rep <- report$mat_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}
  # 
  # 
  #   stock_var_rep <- stock_var_rep %>%
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
  #     stock_var_rep <- stock_var_rep %>%
  #       mutate(subsector = lab_sector,
  #              product = ifelse(arch == "sfh", "Single-Family Houses",
  #                               ifelse(arch == "mfh","Multi-Family Houses",
  #                                      ifelse(arch == "inf","Informal Houses", NA)))) %>%
  #       mutate(Variable = paste(top,
  #                               #sector,
  #                               subsector,product, sep = "|"))
  #   }
  #   if(sector == "comm"){stock_var_rep <- stock_var_rep %>%
  #     mutate(subsector = lab_sector) %>%
  #     mutate(Variable = paste(top,
  #                             #sector,
  #                             subsector,sep = "|"))}
  # 
  #   stock_var_rep[,"Region"] <- stock_var_rep[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")
  # 
  #   stock_var_rep <- stock_var_rep %>%
  #     mutate(Model = model_name, Scenario = scenario_name, Unit = "m^2/yr") %>%
  #     pivot_wider(names_from = "year", values_from = "value") %>%
  #     select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs[-1])))
  # 
  #   # Bind energy and stock results
  #   exp_rep <- rbind(exp_rep, stock_var_rep)


  
  ### MATERIAL RESULTS ###
  
      # Aggregate results - Materials

  if ("material" %in% report_var){
  
    if (paste(geo_level_report) %in% names(report$mat_stock)) {mat_rep <- report$mat_stock} else {
        mat_rep <- report$mat_stock %>% left_join(geo_data %>% select_at(paste(c(geo_level, geo_level_report))))}

    if(sector == "resid"){mat_rep <- mat_rep %>% mutate(subsector = lab_sector) %>% filter(arch != "inf")} # Exclude informal buildings (no material data)
    if(sector == "comm"){mat_rep <- mat_rep %>% mutate(subsector = lab_sector)}

    mats <- c("Non-Ferrous Metals|Aluminium","Non-Ferrous Metals|Copper","Non-metallic minerals|Cement", "Iron and Steel")
    
    mat_rep <- mat_rep %>%
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
      mutate(top = ifelse(commodity == "mat_demand", "Materials Demand",
                          ifelse(commodity == "mat_scrap", "Total Scrap",
                                ifelse(commodity == "mat_stock", "Material Stock", NA))),
             sector = lab_sector) %>%
      
      mutate(material_name = "generic") %>%
      # mutate_cond(material == "aluminum", material_name = paste0("Non-Ferrous Metals")) %>%
      mutate_cond(material == "aluminum", material_name = paste0("Non-Ferrous Metals|Aluminium")) %>%
      mutate_cond(material == "cement", material_name = paste0("Non-metallic minerals|Cement")) %>%
      mutate_cond(material == "concrete", material_name = paste0("Concrete")) %>%
      # mutate_cond(material == "copper", material_name = paste0("Non-Ferrous Metals")) %>%
      mutate_cond(material == "copper", material_name = paste0("Non-Ferrous Metals|Copper")) %>%
      mutate_cond(material == "glass", material_name = paste0("Glass")) %>%
      mutate_cond(material == "steel", material_name = paste0("Iron and Steel")) %>%
      mutate_cond(material == "wood", material_name = paste0("Construction Wood")) %>%
      ungroup() %>%
      # Filter results
      filter(material_name %in% mats) %>%
      filter(commodity %in% c("mat_demand", "mat_scrap")) %>%
      filter(!(commodity == "mat_scrap" & material_name == "Non-metallic minerals|Cement"))

    # if(sector == "resid"){mat_rep <- mat_rep %>% mutate(subsector = "Residential")}
    # if(sector == "comm"){mat_rep <- mat_rep %>% mutate(subsector = "Commercial")}

    mat_rep[,"Region"] <- mat_rep[,paste(geo_level_report)] # Duplicate column: regions for reporting (used to define "nodes")

    mat_rep <- mat_rep %>%
      mutate(subsector = lab_sector) %>%
      mutate(Variable = paste(top,#sector, subsector,
        material_name, subsector,
        sep = "|")) %>%
      mutate(Model = model_name, Scenario = scenario_name) %>% # , Unit = "Mt/yr"
      mutate(Unit = ifelse(commodity == "mat_stock","Mt","Mt/yr")) %>%
      # mutate(Model = model_name, Scenario = scenario_name, Unit = "kt/yr") %>%
      pivot_wider(names_from = "year", values_from = "value") %>%
      select_at(paste(c("Model", "Scenario", "Region", "Variable", "Unit", yrs[-1]))) %>%
      mutate(`2020` = NA, .after="Unit") # PLACEHOLDER

    #mat_rep <- mat_rep %>% distinct()
    
    # Bind energy and material results
    exp_rep <- rbind(exp_rep, mat_rep)

  }
  
  # Change region name: add suffix
  exp_rep <- exp_rep %>% mutate(Region = paste0("R12_",Region))
  
  # Export results
  basename <- paste("report_NAVIGATE", scenario_name, sector, geo_level_report, sep="_")
  write_csv(exp_rep, file.path(path_out, paste0(basename, ".csv")))
}
