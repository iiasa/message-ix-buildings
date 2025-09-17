
### INITIALIZE BUILDING STOCK - FUTURE SCENARIOS ###

# add switch to select levels for calculation?

fun_stock_init_fut <- function(sector, run,
                               mod_arch, # mod_arch = "new", mod_arch = "stock"
                               yrs,
                               geo_data, geo_levels, geo_level,
                               bld_cases_eneff, bld_cases_fuel,
                               pop,
                               hh_size, # used for residential
                               floor_cap, # used for commercial
                               ct_inc_cl,
                               ct_eneff, ct_fuel_comb,
                               stock_arch_base,
                               shr_mat, shr_arch, 
                               shr_fuel_heat_base,
                               shr_distr_heat,
                               en_int_heat,en_int_cool, # Energy demand calculation base year - should be updated for commercial!
                               days_cool,
                               eff_cool,
                               eff_heat,
                               en_sav_ren,
                               hours_heat,
                               shr_floor_heat,
                               hours_cool,
                               shr_floor_cool,
                               hours_fans,
                               power_fans,
                               shr_acc_cool,
                               eff_hotwater,
                               en_int_hotwater,
                               en_int_others, # only for commercial
                               shr_need_heat,
                               price_en,
                               report_var
                               ){

#   eff_cool_scen, eff_heat_scen,
#   ren_en_sav_scen,
#   heat_hours_scen,heat_floor,
#   cool_data_scen,
#   shr_acc_cool,
#   hh_size,floor,
#   price_en)
# yrs, 1,
#                               bld_cases_fuel,hh_size,
#                               #ct_fuel_dhw,
#                               eff_hotwater_scen, en_cap_dhw,
#                               en_m2_sim_r)
# yrs, 1,
#                                bld_cases_fuel,
#                                eff_hotwater_scen,
#                                en_m2_dhw)

  
  # rounding (number of decimals)
  rnd <- 5
  
  # number of income classes
  n_inc_cl <- length(unique(ct_inc_cl$inc_cl))
  
  # Total number of building units: (Residential: number of households (units) - Commercial: total floorspace (m2))
  if (sector == "resid") {
    bld_units <- geo_data %>%
      select_at(geo_levels) %>%
      left_join(pop) %>%
      filter(year %in% yrs) %>% 
      left_join(hh_size) %>% 
      mutate(bld_units = round(1e6*pop/n_inc_cl/hh_size,rnd)) %>% # convert from million units to units
      select(-c(pop,hh_size)) # %>%
    #filter(year %in% yrs) # years already filtered
    
    
    
    try(if(nrow(bld_units) != nrow(distinct(bld_units))) stop("Error in aggregated households calculations! duplicated records in hh"))
  }

  if (sector == "comm") {
    bld_units <- geo_data %>%
      select_at(geo_levels) %>%
      left_join(pop) %>% 
      filter(year %in% yrs) %>% 
      left_join(floor_cap) %>% 
      mutate(bld_units = round(1e6*pop*floor_cap,rnd)) %>% # convert from million units to units
      # mutate(bld_units = round(pop*floor_cap,rnd)) %>% # million units
      select(-c(pop,floor_cap)) # %>%
    #filter(year %in% yrs) # years already filtered
    
    try(if(nrow(bld_units) != nrow(distinct(bld_units))) stop("Error in aggregated households calculations! duplicated records in hh"))
  }
  
  
  if(mod_arch == "new"){
    
    # stock aggregated: Number of units (Million) - "mat" level
    stock_aggr <- bld_cases_eneff %>% 
      select(-c(eneff,bld_age,arch)) %>% # aggregate at "mat" level
      distinct() %>%
      left_join(bld_units) %>% # years filtered here
      left_join(shr_mat) %>% 
      #left_join(shr_arch) %>% # long format
      mutate(n_units_aggr = round(bld_units * shr_mat,rnd)) %>% # to mat level
      #mutate(stock_arch_base = bld_units * shr_mat * shr_arch) %>% # to arch level
      select(-c(bld_units, shr_mat)) #%>%
      #arrange_at(c(geo_level, "urt", "inc_cl", "region_gea", "clim", "mat", "year"))
      #select(-c(bld_units, shr_mat, shr_arch))
    
    # Stock aggregated (mat level) - baseyear 
    stock_aggr_base <- geo_data %>%
      select_at(geo_levels) %>%
      left_join(stock_arch_base) %>% 
      group_by_at(setdiff(names(stock_arch_base), c("bld_age", "arch", "yr_con", "stock_arch_base"))) %>% # Select all variables, except the ones specified
      summarise(n_units_aggr = sum(stock_arch_base)) %>%
      ungroup()
    
    # stock aggregated - combined
    stock_aggr = bind_rows(stock_aggr, stock_aggr_base) %>%
      arrange_at(c(geo_level, "urt", "inc_cl", "region_gea", "clim", "mat", "year"))
      
    # Add stock variation aggregated
    stock_aggr <- stock_aggr %>%
      # group_by(year) %>%
      group_by_at(c(geo_level,"urt","inc_cl", "clim", "mat")) %>% # , "arch"
      #mutate(n_units_eneff_p = lag(n_units_eneff))
      mutate(var_aggr = n_units_aggr - lag(n_units_aggr)) %>%
      mutate(var_aggr = ifelse(year == yrs[2] & is.na(var_aggr),n_units_aggr,var_aggr)) %>% # Correct NA values for second timestep: variation = all buildings in the stock if non-existing in base year
      ungroup()
  
  } else if (mod_arch == "stock") {
    
    # stock aggregated: Number of units (Million) - "arch" level
    stock_aggr <- bld_cases_eneff %>% 
      select(-c(eneff,bld_age)) %>% # aggregate at "arch" level
      distinct() %>%
      left_join(bld_units) %>% # years filtered here
      left_join(shr_mat) %>% 
      left_join(shr_arch) %>% # long format
      #mutate(n_units_aggr = round(bld_units * shr_mat,rnd)) %>% # to mat level
      mutate(n_units_aggr = bld_units * shr_mat * shr_arch) %>% # to arch level
      #select(-c(bld_units, shr_mat)) #%>%
    #arrange_at(c(geo_level, "urt", "inc_cl", "region_gea", "clim", "mat", "year"))
    select(-c(bld_units, shr_mat, shr_arch))
    
    try(if(nrow(stock_aggr) != nrow(distinct(stock_aggr %>% select(-c(n_units_aggr))))) stop("Error in aggregated stock dataset! multiple records for same combinations in stock_aggr"))
    
    # Add stock variation aggregated
    stock_aggr <- stock_aggr %>%
      # group_by(year) %>%
      group_by_at(c(geo_level,"urt","inc_cl", "clim", "mat", "arch")) %>% # 
      #mutate(n_units_eneff_p = lag(n_units_eneff))
      mutate(var_aggr = n_units_aggr - lag(n_units_aggr)) %>%
      mutate(var_aggr = ifelse(year == yrs[2] & is.na(var_aggr),n_units_aggr,var_aggr)) %>% # Correct NA values for second timestep: variation = all buildings in the stock if non-existing in base year
      ungroup()
  }

  try(if(nrow(stock_aggr) != nrow(distinct(stock_aggr %>% select(-c(n_units_aggr,var_aggr))))) stop("Error in aggregated stock dataset! multiple records for same combinations in stock_aggr"))
  
  # # calculate share of eneff in the base year based on the base stock data
  # shr_eneff <- bld_cases_eneff %>%
  #   mutate(year = yrs[1]) %>%
  #   left_join(stock_arch_base) %>%
  #   group_by_at(setdiff(names(stock_arch_base),c("yr_con","stock_arch_base"))) %>%
  #   summarise(shr_eneff = sum(stock_arch_base)) %>%
  #   ungroup() %>%
  #   mutate(shr_eneff = ifelse(is.na(shr_eneff), 0, shr_eneff)) 
  
  # process base stock data
  stock_arch_base <- bld_cases_eneff %>%
    mutate(year = yrs[1]) %>%
    left_join(stock_arch_base) %>%
    mutate(stock_arch_base = ifelse(is.na(stock_arch_base), 0, stock_arch_base)) 
  
  # initialize DF stock by vintage (baseyear) - detailed fuel level
  bld_det_age_i <- stock_aggr %>%
      select(-var_aggr) %>%
      filter(year == yrs[1]) %>%  # baseyear results
    left_join(ct_eneff) %>% 
    left_join(ct_fuel_comb) %>% 
    left_join(stock_arch_base) %>%
    left_join(shr_fuel_heat_base) %>%
    left_join(shr_distr_heat) %>%
    #left_join(shr_acc_cool) %>%
    mutate(n_units_eneff = n_units_aggr * stock_arch_base) %>% 
    mutate(n_units_fuel = ifelse(fuel_heat == "district_heat", 
                                 round(n_units_eneff * shr_distr_heat,rnd), # district heating 
                                 round(n_units_eneff * (1 - shr_distr_heat) * shr_fuel_heat_base,rnd))) %>% # other fuels (decentralized)
    mutate(n_units_fuel = round(n_units_fuel, rnd)) %>%
    mutate_cond(mat == "sub", n_units_fuel = n_units_eneff) %>% # sub-standard buildings - one fuel type only
    select(-c(stock_arch_base,shr_fuel_heat_base, shr_distr_heat, n_units_eneff, mod_decision)) %>%
    filter(!is.na(yr_con))

    # ## other option: start from bld cases
    # bld_fuel_age <- bld_cases_fuel %>% 
    #   mutate(year = yrs[1]) %>%
  
  # initialize bld_arch: stock data - arch level - NOT NEEDED!
  #bld_det <- as.data.frame(NULL)
  
  # Report energy and material results 

  # en_stock <- as.data.frame(NULL) # COMMENT IF en_stock is updated for base year
  # mat_stock <- as.data.frame(NULL)
  report <- list()
  if ("energy" %in% report_var){report = append(report, list(en_stock = as.data.frame(NULL)))}
  if ("material" %in% report_var){report = append(report, list(mat_stock = as.data.frame(NULL)))}
  
  if ("vintage" %in% report_var){
    # Report stock by eneff - vintage
    bld_eneff_age <- bld_det_age_i %>%
      group_by_at(setdiff(names(bld_det_age_i), c(#"arch",
        "fuel_heat","fuel_cool", "n_units_fuel"))) %>%
      summarise(n_units_eneff = sum(n_units_fuel)) %>%
      ungroup()
    
    report = append(report, list(bld_eneff_age = bld_eneff_age))
  }
  
  # Initialize en_stock for the base year
  
  # Energy demand intensities

  lst_en_i <- fun_en_sim(sector,
                         yrs, 1,
                         bld_cases_fuel,
                         en_int_heat, en_int_cool,
                         days_cool,
                         eff_cool, eff_heat,
                         en_sav_ren,
                         hours_heat, shr_floor_heat,
                         hours_cool, shr_floor_cool,
                         hours_fans, power_fans,
                         shr_acc_cool,
                         hh_size,floor_cap,
                         price_en)
  
  # Extract dataframes from list
  en_m2_scen_heat = lst_en_i$en_m2_scen_heat
  en_m2_scen_cool = lst_en_i$en_m2_scen_cool
  
  rm(lst_en_i)
  
  # Energy demand intensities - hot water
  
  if(sector == "resid") {
    en_hh_hw_scen <- fun_hw_resid(yrs, 1,
                                  bld_cases_fuel,
                                  hh_size,
                                  #ct_fuel_dhw,
                                  eff_hotwater, en_int_hotwater,
                                  en_int_heat)
  }
  
  if(sector == "comm") {
    en_m2_hw_scen <- fun_hw_comm(yrs, 1,
                                             bld_cases_fuel, 
                                             eff_hotwater,
                                             en_int_hotwater
    )
  }
  
  
  # Aggregate at fuel level for keeping track of the stock
  bld_det_i <- bld_det_age_i %>%
    group_by_at(setdiff(names(bld_det_age_i), c("yr_con","n_units_fuel","n_dem","n_empty"))) %>% # Select all variables, except the ones indicated, for grouping
    summarise(n_units_fuel = sum(n_units_fuel)
              # , n_dem=sum(n_dem)
    )%>%
    ungroup()
  
  # Initialize energy results - stock level
  en_stock <- bld_cases_fuel %>%
    mutate(scenario = run) %>%
    mutate(year= yrs[1]) %>%
    left_join(bld_det_i %>% select(-bld_age)) %>% # issue matching periods of construction when definition is different!
    #left_join(stock_fuel_i.df) %>%
    # Add "v_no_heat" category
    select(-c(mod_decision)) %>%
    pivot_wider(names_from = "fuel_heat", values_from = "n_units_fuel") %>%
    mutate(v_no_heat = 0) %>%
    pivot_longer(cols = c(paste(sort(unique(pull(select(ct_fuel_comb,fuel_heat))))),"v_no_heat"),
                 names_to = "fuel_heat",
                 values_to = "n_units_fuel" ) %>%
    filter(!is.na(n_units_fuel)) %>%
    group_by_at(paste(c(geo_level, "urt","inc_cl","arch","year","clim","eneff"))) %>%
    mutate(n_units_eneff = sum(n_units_fuel)) %>% # Calculate n_units_eneff to account for buildings with no heating
    ungroup() #%>%
  # select(-n_units_eneff)
  
  ## Stock results - Energy
  
  if(sector == "resid"){
    
    en_stock <- en_stock %>%
      #bld_cases_fuel %>% # To avoid NAs with non-available combination of eneff - fuel (slums)
      #left_join(en_stock_i) %>%
      left_join(shr_need_heat) %>%
      mutate(n_units_fuel = ifelse(fuel_heat == "v_no_heat",# Rescale number of units based on fuel # Heating access not considered here!!
                                   n_units_eneff* (1-shr_need_heat),
                                   n_units_fuel*shr_need_heat)) %>%
      left_join(hh_size) %>%
      left_join(floor_cap) %>%
      #left_join(shr_acc_cool) %>%
      left_join(en_m2_scen_heat) %>%
      left_join(en_m2_scen_cool) %>%
      left_join(en_hh_hw_scen) %>%
      mutate(floor_Mm2 = n_units_fuel / 1e6 * hh_size * floor_cap) %>% # convert n. units to millions
      mutate(floor_heat_Mm2 = floor_Mm2) %>%
      #mutate(floor_heat_Mm2 = ifelse(acc_heat == 1, floor_Mm2, 0)) %>%
      mutate(floor_cool_Mm2 = ifelse(shr_acc_cool == 1, floor_Mm2 * shr_acc_cool, 0)) %>%
      mutate(heat_TJ = ifelse(fuel_heat == "v_no_heat", 0, en_dem_heat * n_units_fuel / 1e6 * hh_size * floor_cap * 3.6)) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(cool_TJ = en_dem_cool * shr_acc_cool * n_units_fuel / 1e6 * hh_size * floor_cap * 3.6) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(cool_ac_TJ = en_dem_c_ac * shr_acc_cool * n_units_fuel / 1e6 * hh_size * floor_cap * 3.6) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(cool_fans_TJ = en_dem_c_fans * shr_acc_cool * n_units_fuel / 1e6 * hh_size * floor_cap * 3.6) %>% # Note:shr_acc_cool=1 for all cases (access calculated before) #converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(hotwater_TJ = ifelse(fuel_heat == "v_no_heat", 0, en_dem_dhw * n_units_fuel / 1e3)) %>% # converted from GJ/hh/yr to TJ
      mutate(other_uses_TJ = 0) %>% # other uses not covered for residential
      mutate(stock_M = n_units_fuel / 1e6) %>%
      filter(stock_M > 0 & !is.na(stock_M)) %>%
      select_at(c(geo_levels, paste(c("urt", "clim", "inc_cl", "arch", "mat", "eneff", "fuel_heat", "fuel_cool",
                                      "scenario", "year", "stock_M", "floor_Mm2",
                                      #"floor_tot_heat_Mm2", "floor_tot_cool_Mm2",
                                      "heat_TJ", "cool_TJ", "cool_ac_TJ", "cool_fans_TJ", "hotwater_TJ", "other_uses_TJ"))))
  }
  
  if(sector == "comm"){
    
    en_stock <- en_stock %>% # To avoid NAs with non-available combination of eneff - fuel (slums)
      # left_join(en_stock_i) %>%
      left_join(shr_need_heat) %>%
      mutate(n_units_fuel = ifelse(fuel_heat == "v_no_heat",# Rescale number of units based on fuel # Heating access not considered here!!
                                   n_units_eneff* (1-shr_need_heat),
                                   n_units_fuel*shr_need_heat)) %>%
      #left_join(hh_size) %>%
      #left_join(floor) %>%
      #left_join(shr_acc_cool) %>%
      left_join(en_m2_scen_heat) %>%
      left_join(en_m2_scen_cool) %>%
      left_join(en_m2_hw_scen) %>%
      left_join(en_int_others) %>%
      mutate(floor_Mm2 = n_units_fuel / 1e6) %>%
      mutate(floor_heat_Mm2 = floor_Mm2) %>%
      #mutate(floor_heat_Mm2 = ifelse(acc_heat == 1, floor_Mm2, 0)) %>%
      mutate(floor_cool_Mm2 = ifelse(shr_acc_cool == 1, floor_Mm2 * shr_acc_cool, 0)) %>%
      mutate(heat_TJ = ifelse(fuel_heat == "v_no_heat", 0, en_dem_heat * n_units_fuel / 1e6 * 3.6)) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(cool_TJ = en_dem_cool * shr_acc_cool * n_units_fuel / 1e6 * 3.6) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(cool_ac_TJ = en_dem_c_ac * shr_acc_cool * n_units_fuel / 1e6 * 3.6) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(cool_fans_TJ = en_dem_c_fans * shr_acc_cool * n_units_fuel / 1e6 * 3.6) %>% # Note:shr_acc_cool=1 for all cases (access calculated before) #converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      #mutate(hotwater_TJ = ifelse(fuel_heat == "v_no_heat", 0, en_dem_dhw * n_units_fuel / 1e6 * 3.6)) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(hotwater_TJ = ifelse(fuel_heat == "v_no_heat", 0, en_dem_dhw * n_units_fuel / 1e6 * 3.6)) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(other_uses_TJ = en_int_others * n_units_fuel / 1e6 * 3.6) %>% # converted from kWh to MJ (3.6). Houssing units are in million, so results are in TJ.
      mutate(stock_M = n_units_fuel / 1e6) %>%
      filter(stock_M > 0 & !is.na(stock_M)) %>%
      select_at(c(geo_levels, paste(c("urt", "clim", "inc_cl", "arch", "mat", "eneff", "fuel_heat", "fuel_cool",
                                      "scenario", "year", "stock_M", "floor_Mm2",
                                      #"floor_tot_heat_Mm2", "floor_tot_cool_Mm2",
                                      "heat_TJ", "cool_TJ", "cool_ac_TJ", "cool_fans_TJ", "hotwater_TJ", "other_uses_TJ"))))
  }
  
  
  # Add results for the base year for reporting
  if ("energy" %in% report_var){report$en_stock <-  bind_rows(report$en_stock,en_stock)}
  # if ("material" %in% report_var){report$mat_stock <- bind_rows(report$mat_stock,mat_stock)}
  
  #OUTPUT
  
  output = list(stock_aggr = stock_aggr,
                bld_det_age_i = bld_det_age_i,
                # bld_det = bld_det,
                # bld_eneff_age = bld_eneff_age,
                # en_stock = en_stock,
                # mat_stock = mat_stock
                report = report
                )

  
  }

# bld_aggr_age <- stock_aggr%>% 
#   filter(year == yrs[1]) %>% 
#   add_column(yr_con = yrs[1], .after = "year") %>%
#   # left_join(ct_bld_age %>% 
#   #             filter(year_i <= yrs[1] & year_f >= yrs[1]) %>% 
#   #             select(bld_age_id, mat) %>%
#   #             rename(bld_age = bld_age_id)) %>%
#   rowwise() %>% 
#   mutate(bld_age = ct_bld_age$bld_age_id[which(ct_bld_age$mat == mat & ct_bld_age$year_i <= yr_con & ct_bld_age$year_f >= yr_con)]) %>%
#   ungroup() %>%
#   relocate(n_units_aggr, .after = last_col()) %>%
#   mutate(n_dem = 0)
