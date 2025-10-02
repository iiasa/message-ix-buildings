
## FUTURE STOCK DYNAMICS
## this calculation is to be put within a loop on "i" (time steps)


## DATA INPUT:

# ms_new_i #Calculated by other modules
# ms_ren_i #Calculated by other modules

fun_stock_dyn <- function(sector,
                          mod_arch, # mod_arch = "new", mod_arch = "stock"
                          yrs,i,
                          run, #ssp_r, # removed ssp dimension
                          geo_level, geo_level_aggr,geo_levels,
                          bld_cases_fuel, bld_cases_eneff, 
                          ct_bld_age, ct_fuel_comb,
                          hh_size, # Not used for commercial
                          floor_cap,
                          stock_aggr, bld_det_age_i, #bld_det, 
                          #bld_eneff_age, # keep track of age
                          prob_dem,
                          #rate_ren_low, rate_ren_high, #ren_rate,
                          rate_switch_fuel_heat,
                          #ms_new, ms_ren,
                          ms_new_i, ms_ren_i, rate_ren_i,
                          ms_sw_i,
                          #shr_acc_cool, # included in en_m2_scen_cool
                          shr_distr_heat, shr_need_heat,
                          en_m2_scen_heat, en_m2_scen_cool,
                          en_hh_hw_scen, en_m2_hw_scen, en_int_others,
                          #en_stock,
                          mat_int,
                          shr_mat_eol,
                          #mat_stock,
                          report_var,
                          report
                          ){

  # rounding (number of decimals)
  rnd <- 5
  
  print(paste0("Running stock turnover - scenario ", run, " - year ", yrs[i]))

# # Testing version: remove year from ms_new, ms_ren
# ms_new_i <- ms_new_i %>% select(-year)
# ms_ren_i <- ms_ren_i %>% select(-year)
  
# for (i in 2:length(yrs))
# {  

  # print(paste("Running - scenario ", run, " - year", yrs[i]))
  
  ## Define timestep
stp <- yrs[i]-yrs[i-1]


# vintage category for the current year ( to remove categories belonging to different periods)
ct_bld_age_i <- ct_bld_age %>% filter(yrs[i] >= year_i & yrs[i] <= year_f) %>% 
  select(mat, bld_age_id) %>% 
  rename(bld_age = bld_age_id)

## STOCK DYNAMICS

# b <- bld_dyn_par %>% select(-c(p_dem_hist, p_ren_hist, ren_tau, ren_sd, l_new, l_ren))

# Calculate demolitions - detailed - by fuel, yr_con - i-th year
dem_det_age_i <- bld_det_age_i %>% filter(year == yrs[i-1]) %>%  # Stock from previous year
  select(-year) %>%
  #mutate(age = yrs[i]-yr_con) %>% # Calculate age of buildings
  # left_join(b) %>% # Attach probability of demolition
  left_join(prob_dem %>% pivot_wider(names_from="parameter",values_from = "prob_dem")) %>% 
  #left_join(bld_dyn_par %>% select(-c(p_dem_hist, p_ren_hist, ren_tau, ren_sd, l_new, l_ren))) %>% # Attach probability of demolition
  mutate(pdem = pweibull(yrs[i]-yr_con, shape = shape, scale = scale) - pweibull(yrs[i-1]-yr_con, shape = shape, scale = scale)) %>% #CDF: difference between two consecutive time steps
  mutate(n_dem = ifelse(n_units_fuel>0,round(pdem * n_units_fuel,rnd),0)) %>%
  rename(n_units_fuel_p = n_units_fuel) %>% # N. units are from the previous time-step
  select(-c(shape, scale, pdem))

# rm(b)

if(mod_arch == "new"){

  
  
  # Calculate demolition, new constructions - aggregated ("mat" level) - for i-th year
  bld_aggr_i <- stock_aggr %>% filter(year == yrs[i]) %>% # stock variation current year 
    left_join(dem_det_age_i %>% # aggregate demolitions by arch
                group_by_at(setdiff(names(dem_det_age_i), 
                                    c("yr_con", "arch","eneff","fuel_heat","fuel_cool"
                                      ,"bld_age"
                                      # ,"mod_decision"
                                      ,"n_units_fuel_p","n_dem"))) %>% # Select all variables, except the ones indicated, for grouping
                summarise(n_dem=sum(n_dem))%>%
                ungroup()) %>%
    mutate(n_dem = ifelse(var_aggr ==  n_units_aggr & is.na(n_dem), 0, n_dem)) %>%
    mutate(n_new = ifelse(var_aggr + n_dem >= 0, var_aggr + n_dem, 0),
           n_empty = ifelse(var_aggr + n_dem >= 0, 0, -var_aggr -n_dem)) %>%
    mutate(n_dem = ifelse(abs(var_aggr) < 1e-9 & abs(n_units_aggr)  < 1e-9 , 0, n_dem),
           n_new = ifelse(abs(var_aggr)  < 1e-9 & abs(n_units_aggr) < 1e-9 , 0, n_new),
           n_empty = ifelse(abs(var_aggr)  < 1e-9 & abs(n_units_aggr)  < 1e-9 , 0, n_empty))
  
  # Distribute abandoned buildings across vintage cohorts
  dem_det_age_i <- dem_det_age_i %>% # First calculate share of buildings by vintage in the previous timestep (to allocate empty buildings)
    group_by_at(setdiff(names(dem_det_age_i), c("yr_con","arch","eneff","fuel_heat","fuel_cool"
                                                #,"mod_decision"
                                                , "n_units_fuel_p","n_dem"))) %>% # Select all variables, except "eneff" and "n_dem" for grouping)
    mutate(n_units_fuel_upd_tot = sum(n_units_fuel_p - n_dem))%>%
    ungroup() %>%
    mutate(shr_aggr_age = (n_units_fuel_p - n_dem)/n_units_fuel_upd_tot)%>%
    left_join(bld_aggr_i %>% select(-c(year, n_units_aggr, var_aggr, n_dem, n_new))) %>%
    mutate(n_empty = ifelse(shr_aggr_age>0 & n_units_fuel_p>0, round(n_empty * shr_aggr_age,rnd), 0)) %>% # update number of empty buildings by vintage cohort
    select(-c(n_units_fuel_upd_tot, shr_aggr_age))
  
  # Update stock results by age - disaggregated - current timestep

  ## New buildings
  # Disaggregate results from totals based on market shares
  new_det_age_i <- bld_aggr_i %>%
    filter(mat != "sub") %>%
    select(-c(n_units_aggr,var_aggr,n_dem,n_empty)) %>% 
    add_column(yr_con = yrs[i], .after = "year") %>%
    # rowwise() %>% # add period of construction
    # mutate(bld_age = ct_bld_age$bld_age_id[which(ct_bld_age$mat == mat & ct_bld_age$year_i <= yr_con & ct_bld_age$year_f >= yr_con)]) %>%
    # ungroup() %>%
    left_join(ct_bld_age_i) %>%
    left_join(shr_arch) %>% # join shares of arch in new construction
    left_join(shr_distr_heat) %>%
    left_join(ms_new_i) %>% # join shares of eneff and fuel_heat in new construction
    #mutate(mod_decision = 1) %>%
    mutate(n_new = round(n_new * shr_arch * c(1-shr_distr_heat) * ms, rnd)) %>%
    #mutate(n_dem = 0) %>% 
    #mutate(n_empty = 0) %>%
    #mutate(n_ren = 0) %>% 
    #relocate(n_new, .before=n_dem) %>%
    filter(n_new>0) %>%
    relocate(n_units_fuel = n_new, .after = last_col()) %>%
    select(-c(shr_arch,shr_distr_heat, ms, mod_decision)) #,
 
  # New buildings - district heating
  new_det_distr_age_i <- 
    bld_aggr_i %>%
    filter(mat != "sub") %>%
    select(-c(n_units_aggr,var_aggr,n_dem,n_empty)) %>% 
    add_column(yr_con = yrs[i], .after = "year") %>%
    left_join(ct_bld_age_i) %>%
    left_join(shr_arch) %>% # join shares of arch in new construction
    left_join(shr_distr_heat) %>%
    mutate(eneff = "s51_std", # CHANGE THIS!!!!
             fuel_heat = "district_heat", fuel_cool = "electricity"
           #, mod_decision = 0
           ) %>%
    mutate(n_new = round(n_new * shr_arch * shr_distr_heat,rnd)) %>%
    #mutate(n_dem = 0) %>% 
    #mutate(n_empty = 0) %>%
    #mutate(n_ren = 0) %>% 
    #relocate(n_new, .before=n_dem) %>%
    filter(n_new>0) %>%
    relocate(n_units_fuel = n_new, .after = last_col()) %>%
    select(-c(shr_arch,shr_distr_heat)) #,
  
  
  
  } else if (mod_arch == "stock") { # Check negative n_empty values!!!
  
  # Calculate demolition, new constructions - aggregated ("arch" level) - for i-th year
  bld_aggr_i <- stock_aggr %>% filter(year == yrs[i]) %>% # stock variation current year 
    left_join(dem_det_age_i %>% # aggregate demolitions by arch
                group_by_at(setdiff(names(dem_det_age_i), 
                                    c("yr_con","eneff","fuel_heat","fuel_cool" #,"arch" 
                                      ,"bld_age"
                                      # ,"mod_decision"
                                      ,"n_units_fuel_p","n_dem"))) %>% # Select all variables, except the ones indicated, for grouping
                summarise(n_dem=sum(n_dem))%>%
                ungroup()) %>%
    mutate(n_dem = ifelse(var_aggr ==  n_units_aggr & is.na(n_dem), 0, n_dem)) %>%
    mutate(n_new = ifelse(var_aggr + n_dem >= 0, var_aggr + n_dem, 0),
           n_empty = ifelse(var_aggr + n_dem >= 0, 0, -var_aggr -n_dem)) %>%
    mutate(n_dem = ifelse(abs(var_aggr) < 1e-9 & abs(n_units_aggr) < 1e-9 , 0, n_dem),
           n_new = ifelse(abs(var_aggr) < 1e-9 & abs(n_units_aggr) < 1e-9 , 0, n_new),
           n_empty = ifelse(abs(var_aggr) < 1e-9 & abs(n_units_aggr) < 1e-9 , 0, n_empty))
    
  # Distribute abandoned buildings across vintage cohorts + arch ## PROBLEM: empty units exceeding n.units(previous stes) - n.units demolished
  dem_det_age_i <- dem_det_age_i %>% # First calculate share of buildings by vintage in the previous timestep (to allocate empty buildings)
    group_by_at(setdiff(names(dem_det_age_i), c("yr_con", # aggregate at same level as bld_aggr
                                                #"arch",
                                                "bld_age", # added 22/11/2021
                                                "eneff","fuel_heat","fuel_cool"  
                                                #,"mod_decision"
                                                , "n_units_fuel_p","n_dem"))) %>% # Select all variables, except "eneff" and "n_dem" for grouping)
    mutate(n_units_fuel_upd_tot = sum(n_units_fuel_p - n_dem))%>% # surviving units
    ungroup() %>%
    mutate(shr_aggr_age = (n_units_fuel_p - n_dem)/n_units_fuel_upd_tot)%>%
    left_join(bld_aggr_i %>% select(-c(year, n_units_aggr, var_aggr, n_dem, n_new))) %>%
    mutate(n_empty = ifelse(shr_aggr_age>0 & n_units_fuel_p>0, round(n_empty * shr_aggr_age,rnd), 0)) %>% # update number of empty buildings by vintage cohort
    mutate(n_empty = ifelse(n_units_fuel_p-n_dem-n_empty<0,0,n_empty)) %>% # correct exceeding empty buildings
    select(-c(n_units_fuel_upd_tot, shr_aggr_age))
  
  ## TEST: N. empty buildings
  print(paste("Empty buildings (det): ", round(sum(dem_det_age_i$n_empty),0)))
  print(paste("Empty buildings (aggr): ", round(sum(bld_aggr_i$n_empty),0)))
  
  # Update stock results by age - disaggregated - current timestep
  
  ## New buildings
  # Disaggregate results from totals based on market shares
  new_det_age_i <- bld_aggr_i %>%
    filter(mat != "sub") %>%
    select(-c(n_units_aggr,var_aggr,n_dem,n_empty)) %>% 
    add_column(yr_con = yrs[i], .after = "year") %>%
    # rowwise() %>% # add period of construction
    # mutate(bld_age = ct_bld_age$bld_age_id[which(ct_bld_age$mat == mat & ct_bld_age$year_i <= yr_con & ct_bld_age$year_f >= yr_con)]) %>%
    # ungroup() %>%
    left_join(ct_bld_age_i) %>%
    #left_join(shr_arch) %>% # join shares of arch in new construction
    left_join(shr_distr_heat) %>%
    left_join(ms_new_i) %>% # join shares of eneff and fuel_heat in new construction
    #mutate(mod_decision = 1) %>%
    mutate(n_new = round(n_new *  c(1-shr_distr_heat) * ms, rnd)) %>%
    #mutate(n_dem = 0) %>% 
    #mutate(n_empty = 0) %>%
    #mutate(n_ren = 0) %>% 
    #relocate(n_new, .before=n_dem) %>%
    filter(n_new>0) %>%
    relocate(n_units_fuel = n_new, .after = last_col()) %>%
    select(-c(shr_distr_heat, ms)) #, mod_decision
  
  # New buildings - district heating
  new_det_distr_age_i <- 
    bld_aggr_i %>%
    filter(mat != "sub") %>%
    select(-c(n_units_aggr,var_aggr,n_dem,n_empty)) %>% 
    add_column(yr_con = yrs[i], .after = "year") %>%
    left_join(ct_bld_age_i) %>%
    #left_join(shr_arch) %>% # join shares of arch in new construction
    left_join(shr_distr_heat) %>%
    mutate(eneff = "s51_std", # CHANGE THIS!!!!
           fuel_heat = "district_heat", fuel_cool = "electricity"
           #, mod_decision = 0
    ) %>%
    mutate(n_new = round(n_new * shr_distr_heat,rnd)) %>%
    #mutate(n_dem = 0) %>% 
    #mutate(n_empty = 0) %>%
    #mutate(n_ren = 0) %>% 
    #relocate(n_new, .before=n_dem) %>%
    filter(n_new>0) %>%
    relocate(n_units_fuel = n_new, .after = last_col()) %>%
    select(-c(shr_distr_heat)) #,
  
}
  
  
if ("sub" %in% unique(bld_cases_eneff$mat)){
  # Slum buildings
  new_det_slum_age_i <- 
    bld_aggr_i %>%
    filter(mat == "sub") %>%
    select(-c(n_units_aggr,var_aggr,n_dem,n_empty)) %>% 
    add_column(yr_con = yrs[i], .after = "year") %>%
    left_join(ct_bld_age_i) %>%
    mutate(arch = "inf", eneff = "ns", # CHANGE THIS!!!!
           fuel_heat = "electric", fuel_cool = "electricity"
           #, mod_decision = 0
           ) %>%
    #mutate(n_new = n_new) %>%
    #mutate(n_dem = 0) %>% 
    #mutate(n_empty = 0) %>%
    #mutate(n_ren = 0) %>% 
    #relocate(n_new, .before=n_dem) %>%
    filter(n_new>0) %>%
    mutate(n_units_fuel = round(n_new,rnd), .after = last_col()) %>%
    select(-n_new)} else {
      
      new_det_slum_age_i=data.frame(NULL)
      }
  
  # Test for new construction - total
  print(paste("New buildings (aggr): ", 
              round(c(sum(new_det_age_i$n_units_fuel) + 
                  sum(new_det_distr_age_i$n_units_fuel) + 
                  sum(new_det_slum_age_i$n_units_fuel)),0))) # Sum disaggregated buildings
  print(paste("New buildings (det): ",round(sum(bld_aggr_i$n_new),0))) # Sum aggregated buildings

  # # Total existing buildings (only for testing)
  # exst_tot_det_i <- dem_det_age_i %>% 
  #   add_column(year = yrs[i], .before = "yr_con") %>%
  #   mutate(n_units_fuel = n_units_fuel_p - n_dem - n_empty) %>%
  #   select(-c(n_units_fuel_p,n_dem,n_empty))
  
  # ## Fuel switching: yes/no (REMOVED)
  # bld_fuel_switch <- bld_cases_fuel %>%
  #   mutate(year=yrs[i]) %>%
  #   #left_join(rate_switch_fuel_heat) %>%
  #   left_join(ms_sw_i) %>%
  #   mutate(sw_fuel = ifelse(!is.na(ms) & ms>0,1,0))
  # bld_fuel_switch <- bld_fuel_switch  %>%
  #   group_by_at(setdiff(names(bld_fuel_switch), 
  #                       c("fuel_heat_f","ms","sw_fuel"))) %>% # Select all variables, except the ones indicated, for grouping
  #   summarise(sw_fuel = mean(sw_fuel))%>%
  #   ungroup() %>%
  #   select(-c(mod_decision,year))
    
    ## Existing buildings - renovated
    ren_det_age_i <- dem_det_age_i %>% 
      add_column(year = yrs[i], .before = "yr_con") %>%
      mutate(n_units_fuel_exst = n_units_fuel_p - n_dem - n_empty)  %>% # update results existing buildings (before renovation)
      ## Account for renovations
      left_join(rate_ren_i) %>%
      mutate(rate_ren = ifelse(is.na(rate_ren),0,rate_ren)) %>% # NAs for rate_ren -> no renovations! (e.g. no space heating demand) 
      left_join(ms_ren_i %>% rename(eneff = eneff_i, fuel_heat = fuel_heat_i)) %>%
      mutate(mod_decision = ifelse(is.na(ms_ren),0,mod_decision), # CHANGE THIS!!! no renovation if there is no MS data available (district heat)
             eneff_f = ifelse(is.na(ms_ren),eneff,eneff_f), # take original eneff if is.na(ms_ren)
             fuel_heat_f = ifelse(is.na(ms_ren),fuel_heat,fuel_heat_f)) %>%
      mutate(ms_ren = ifelse(is.na(ms_ren),0,ms_ren)) %>% # take original fuel heat if is.na(ms_ren)
      mutate(n_units_fuel = round(n_units_fuel_exst * rate_ren * stp * ms_ren, rnd)) %>% # calculate n.units - after renovation
        select(-c(n_units_fuel_p, n_dem, n_empty, n_units_fuel_exst, rate_ren,
                  mod_decision,
                  #eneff, fuel_heat, 
                  ms_ren)) #%>%
      #   filter(n_units_fuel != 0) %>%
    #   # Calculate renovation rate - mat level
    #   #mutate(ren = 1) %>% # flag renovations: 0=no; 1=yes
    #   select(-c(n_units_fuel_p, n_dem, n_empty, n_units_fuel_exst, rate_ren_upd, 
    #             mod_decision, 
    #             eneff, fuel_heat, ms_ren)) %>%
    #   rename(eneff = eneff_f, fuel_heat = fuel_heat_f)
    # #mutate(ren = ifelse(eneff_f == eneff, 0, 1)) # %>% # flag renovations: 0=no; 1=yes
    
    
    ## Existing buildings - non-renovated - initialize
    exst_det_age_i <- dem_det_age_i %>%
      left_join(ren_det_age_i %>%
      #select(-c(eneff_f, fuel_heat_f)) %>%
      group_by_at(setdiff(names(ren_det_age_i), 
                          c("eneff_f","fuel_heat_f","n_units_fuel"))) %>% # Select all variables, except the ones indicated, for grouping
      summarise(n_ren = sum(n_units_fuel))%>%
      ungroup()) %>%
      mutate(n_units_fuel_exst = n_units_fuel_p - n_dem - n_empty - n_ren) %>%
      select(-c(n_units_fuel_p, n_dem, n_empty, n_ren))
  
    # Format DF renovated buildings
    ren_det_age_i <- ren_det_age_i %>% 
      filter(n_units_fuel != 0) %>%
      select(-c(eneff, fuel_heat)) %>%
      rename(eneff = eneff_f, fuel_heat = fuel_heat_f)
    
    
    # Test for existing buildings
    print(paste("Existing units (det1): ",
                round(c(sum(exst_det_age_i$n_units_fuel_exst) +
                          sum(ren_det_age_i$n_units_fuel)),0))) # Sum disaggregated buildings
    print(paste("Existing units (det2): ", round(sum(dem_det_age_i$n_units_fuel_p - dem_det_age_i$n_dem - dem_det_age_i$n_empty),0)))
    

    
    ## Existing buildings - non-renovated - fuel switch
    exst_sw_det_age_i <- exst_det_age_i %>%
      #left_join(bld_fuel_switch) %>%
      left_join(rate_switch_fuel_heat) %>%
      left_join(ms_sw_i) %>%
      mutate(rate_switch_fuel_heat = ifelse(year-yr_con > bld_age_min, rate_switch_fuel_heat, 0)) %>% # fuel switches only over the minimum age of buildings
      #mutate(n_units_fuel = round(n_units_fuel_exst * sw_fuel * (rate_switch_fuel_heat * stp) * ms, rnd)) %>% # calculate n.units - after renovation
      mutate(n_units_fuel = round(n_units_fuel_exst * (rate_switch_fuel_heat * stp) * ms, rnd)) %>% # calculate n.units - after renovation
      filter(n_units_fuel > 0) %>% #No fuel switches with NAs
      # Calculate renovation rate - mat level
      #mutate(ren = 0) %>% # flag renovations: 0=no; 1=yes
      select(-c(n_units_fuel_exst, #sw_fuel, 
                bld_age_min, rate_switch_fuel_heat, mod_decision,  ms))

    # # temporary file of fuel switches - aggregated to calculate existing buildings non switching fuels
    # # Use to avoid "group_by_at" within "left_join" below
    # exst_sw_det_age_i_tmp <- exst_sw_det_age_i %>%
    #   group_by_at(setdiff(names(exst_sw_det_age_i), 
    #                       c("fuel_heat_f","n_units_fuel"))) %>% # Select all variables, except the ones indicated, for grouping
    #   summarise(n_sw = sum(n_units_fuel))%>%
    #   ungroup()
    
    # Update Existing buildings - non-renovated - without fuel switch
    exst_det_age_i <- exst_det_age_i %>%
#      left_join(exst_sw_det_age_i_tmp) %>%
      left_join(exst_sw_det_age_i %>%
                  group_by_at(setdiff(names(exst_sw_det_age_i),
                                      c("fuel_heat_f","n_units_fuel"))) %>% # Select all variables, except the ones indicated, for grouping
                  summarise(n_sw = sum(n_units_fuel))%>%
                  ungroup()) %>%
      mutate(n_sw = ifelse(is.na(n_sw),0,n_sw)) %>%
      mutate(n_units_fuel = n_units_fuel_exst - n_sw) %>%
      select(-c(n_units_fuel_exst, n_sw))
    
    #rm(exst_sw_det_age_i_tmp)
    
    # Format DF non-renovated buildings - fuel switches
    exst_sw_det_age_i <- exst_sw_det_age_i %>%
      select(-fuel_heat) %>%
      rename(fuel_heat = fuel_heat_f)

    
    ### OLD CODE - Existing buildings
    # ## Existing buildings - renovated + non-renovated
    # exst_tot_det_age_i <- dem_det_age_i %>% 
    #   add_column(year = yrs[i], .before = "yr_con") %>%
    #   mutate(n_units_fuel_exst = n_units_fuel_p - n_dem - n_empty)  %>% # update results existing buildings (before renovation)
    #   ## Account for renovations
    #   left_join(rate_ren_i) %>%
    #   mutate(rate_ren_upd = ifelse(is.na(rate_ren_upd),0,rate_ren_upd)) %>% # NAs for rate_ren -> no renovations! (e.g. no space heating demand) 
    #   left_join(ms_ren_i %>% rename(eneff = eneff_i, fuel_heat = fuel_heat_i)) %>%
    #   mutate(mod_decision = ifelse(is.na(ms_ren),0,mod_decision), # CHANGE THIS!!! no renovation if there is no MS data available (district heat)
    #          eneff_f = ifelse(is.na(ms_ren),eneff,eneff_f), # take original eneff if is.na(ms_ren)
    #          fuel_heat_f = ifelse(is.na(ms_ren),fuel_heat,fuel_heat_f)) %>%
    #   mutate(ms_ren = ifelse(is.na(ms_ren),0,ms_ren)) %>% # take original fuel heat if is.na(ms_ren)
    #   mutate(n_units_fuel_ren = round(n_units_fuel_exst * rate_ren_upd * stp * ms_ren, rnd)) %>% # renovated buildings
    #   mutate(n_units_fuel_non = n_units_fuel_exst - n_units_fuel_ren) # non-renovated buildings
    # 
    # ## Existing buildings - renovated
    # ren_det_age_i <- exst_tot_det_age_i %>%
    #   rename(n_units_fuel = n_units_fuel_ren) %>%
    #   filter(n_units_fuel != 0) %>%
    #   # Calculate renovation rate - mat level
    #   #mutate(ren = 1) %>% # flag renovations: 0=no; 1=yes
    #   select(-c(n_units_fuel_p, n_dem, n_empty, n_units_fuel_exst, n_units_fuel_non, rate_ren_upd, 
    #             mod_decision, 
    #             eneff, fuel_heat, ms_ren)) %>%
    #   rename(eneff = eneff_f, fuel_heat = fuel_heat_f)
    # 
    # ## Existing buildings - non-renovated
    # exst_det_age_i <- exst_tot_det_age_i %>%
    #   rename(n_units_fuel = n_units_fuel_non) %>%
    #   filter(n_units_fuel != 0) %>%
    #   distinct()%>%
    #   # Calculate renovation rate - mat level
    #   #mutate(ren = 1) %>% # flag renovations: 0=no; 1=yes
    #   select(-c(n_units_fuel_p, n_dem, n_empty, n_units_fuel_exst, n_units_fuel_ren, rate_ren_upd, 
    #             mod_decision, 
    #             eneff_f, fuel_heat_f, ms_ren)) 
    
  # 
  # Test for existing buildings
  print(paste("Existing units (det1): ",
              round(c(sum(exst_det_age_i$n_units_fuel) +
                      sum(exst_sw_det_age_i$n_units_fuel) +
                      sum(ren_det_age_i$n_units_fuel)),0))) # Sum disaggregated buildings
  print(paste("Existing units (det2): ", round(sum(dem_det_age_i$n_units_fuel_p - dem_det_age_i$n_dem - dem_det_age_i$n_empty),0)))
  print(paste("Existing units (aggr): ", round(sum(bld_aggr_i$n_units_aggr - bld_aggr_i$n_new),0))) # Sum aggregated buildings

  # sum(exst_tot_det_i$n_units_fuel)
  # sum(exst_det_OLD_age_i$n_units_fuel) + sum(ren_det_age_i$n_units_fuel)
  # sum(exst_det_OLD_age_i$n_units_fuel)
  # sum(exst_sw_det_age_i$n_units_fuel) + sum(exst_det_age_i$n_units_fuel)
  
  ### TEST: TOTAL NUMBER OF HOUSING UNITS
  print(paste("Total units (det): ",
              round(c(sum(new_det_age_i$n_units_fuel) +
                        sum(new_det_distr_age_i$n_units_fuel) +
                        sum(new_det_slum_age_i$n_units_fuel) +
                        sum(exst_det_age_i$n_units_fuel) +
                        sum(exst_sw_det_age_i$n_units_fuel) +
                        sum(ren_det_age_i$n_units_fuel)),0))) # Sum disaggregated buildings
  print(paste("Total units (aggr): ", round(sum(stock_aggr %>% filter(year == yrs[i]) %>% select(n_units_aggr) %>% pull),0)))
  
  
  # Bind all datasets - fuel level + vintage
  bld_det_age_i <- bind_rows(exst_det_age_i, exst_sw_det_age_i, ren_det_age_i, new_det_age_i, new_det_distr_age_i, new_det_slum_age_i) %>%
  # bld_det_age_i <- bind_rows(exst_det_age_i, new_det_age_i, new_det_distr_age_i, new_det_slum_age_i) %>%
    # Remove negative values (due to approximation)
    mutate(n_units_fuel = ifelse(n_units_fuel <0, 0, n_units_fuel)) %>%
    # Aggregate to remove doubled categories
    group_by_at(setdiff(names(bld_det_age_i), c("n_units_fuel"))) %>% # Select all variables, except the ones indicated, for grouping
    summarise(n_units_fuel=sum(n_units_fuel)) %>%
    ungroup()
  
  # Aggregate at fuel level for keeping track of the stock
  bld_det_i <- bld_det_age_i %>%
    group_by_at(setdiff(names(bld_det_age_i), c("yr_con","n_units_fuel","n_dem","n_empty"))) %>% # Select all variables, except the ones indicated, for grouping
    summarise(n_units_fuel = sum(n_units_fuel)
              # , n_dem=sum(n_dem)
              )%>%
    ungroup()

  # Keep track of the stock - Not needed! (Taken care of by en_stock, mat_stock, bld_eneff_age)
  # bld_det <- bind_rows(bld_det, bld_det_i)
  # bld_det_age <- bind_rows(bld_det_age, bld_det_age_i) # Not reported - too big!
  
  # # Stock by eneff and vintage (for reporting)
  # bld_eneff_age <- bind_rows(bld_eneff_age,
  #                            bld_det_age_i %>%
  #                              group_by_at(setdiff(names(bld_det_age_i), c(#"arch", 
  #                                "fuel_heat","fuel_cool", "n_units_fuel"))) %>%
  #                              summarise(n_units_eneff = sum(n_units_fuel)) %>%
  #                              ungroup())

#} # end loop over years



## Report Energy Demand

en_stock_i <- bld_cases_fuel %>%
  mutate(scenario = run) %>%
  # mutate(ssp = ssp_r) %>%
  mutate(year= yrs[i]) %>%
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
  #select(-n_units_eneff)

## Stock results - Energy

if(sector == "resid"){
  
  en_stock_i <- en_stock_i %>%
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
                                    "scenario", #"ssp", 
                                    "year", "stock_M", "floor_Mm2",  
                                    #"floor_tot_heat_Mm2", "floor_tot_cool_Mm2",
                                    "heat_TJ", "cool_TJ", "cool_ac_TJ", "cool_fans_TJ", "hotwater_TJ", "other_uses_TJ"))))
}
  
if(sector == "comm"){
  
  en_stock_i <- en_stock_i %>% # To avoid NAs with non-available combination of eneff - fuel (slums)
    #left_join(en_stock_i) %>%
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
                                    "scenario", #"ssp", 
                                    "year", "stock_M", "floor_Mm2",  
                                    #"floor_tot_heat_Mm2", "floor_tot_cool_Mm2",
                                    "heat_TJ", "cool_TJ", "cool_ac_TJ", "cool_fans_TJ", "hotwater_TJ", "other_uses_TJ"))))
}

## Stock results - Material
#mats <- sort(unique(mat_int$material))

# Aggregate results at eneff level
# demolitions
dem_eneff_i <- dem_det_age_i %>%
  group_by_at(setdiff(names(dem_det_age_i), c(#"arch", 
    "bld_age",
    #"yr_con", # Keep year construction to associate time-dependent material intensities
    "fuel_heat","fuel_cool", "n_units_fuel_p","n_dem", "n_empty"))) %>%
  summarise(n_dem = sum(n_dem)
            #, n_empty = sum(n_empty) # account for abandoned buildings
  ) %>%
  ungroup()

# new constructions
new_eneff_i <- bind_rows(new_det_age_i, new_det_distr_age_i, new_det_slum_age_i) %>%
  group_by_at(setdiff(names(new_det_age_i), c(#"arch",
    "bld_age",
    "mod_decision",
    #"yr_con", # Keep year construction to associate time-dependent material intensities
    "fuel_heat","fuel_cool", "n_units_fuel"))) %>%
  summarise(n_new = sum(n_units_fuel)) %>%
  ungroup()

# stock
bld_eneff_i <- bld_det_age_i %>%
  group_by_at(setdiff(names(bld_det_age_i), c(#"arch", 
    "bld_age",
    #"yr_con", # Keep year construction to associate time-dependent material intensities
    "fuel_heat","fuel_cool", "n_units_fuel"))) %>%
  summarise(n_units = sum(n_units_fuel)) %>%
  ungroup()

# # Generate dataset with material names (to get all combinations of materials in the mat_stock DF)
# mats <- mat_int %>% select(any_of(geo_levels), material) %>% distinct # Regions + materials

if("material" %in% report_var){
  if(sector == "resid"){
      
    # Calculate material stock
    mat_stock_i <- bld_cases_eneff %>%
      mutate(scenario = run) %>%
      # mutate(ssp = ssp_r) %>%
      mutate(year= yrs[i]) %>%
      left_join(hh_size) %>%
      left_join(floor_cap) %>%
      left_join(bld_eneff_i) %>%
      left_join(dem_eneff_i) %>%
      left_join(new_eneff_i) %>%
      mutate(n_units = ifelse(is.na(n_units),0,n_units),
             n_new = ifelse(is.na(n_new),0,n_new),
             n_dem = ifelse(is.na(n_dem),0,n_dem)) %>%
      filter(n_units + n_new + n_dem != 0) %>%
    filter(mat != "sub") %>% # Exclude slums (no material intensity data)
    left_join(mat_int) %>%
    #filter(arch != "inf") %>% # Materials not calculated for slums
    mutate(floor_tot_Mm2 = n_units * hh_size * floor_cap / 1e6) %>% # Mm2
    mutate(floor_new_Mm2 = n_new * hh_size * floor_cap / stp / 1e6) %>% # Mm2/yr
    mutate(floor_dem_Mm2 = n_dem * hh_size * floor_cap / stp / 1e6) %>% # Mm2/yr
    mutate(mat_stock_Mt = n_units * hh_size * floor_cap * mat_int / 1e3 / 1e6) %>% #Mt/y
    mutate(mat_demand_Mt = n_new * hh_size * floor_cap * mat_int / stp / 1e3 / 1e6) %>% #Mt/y
    mutate(mat_scrap_Mt = n_dem * hh_size * floor_cap * mat_int / stp / 1e3 / 1e6) %>% #Mt/y
      
    # select_at(c(geo_levels, paste(c("urt", "clim", "inc_cl", "arch", "mat", "eneff", "material", ##### CHECK yr_con
    #                                 "scenario", "ssp", "year",
    #                                 "floor_tot_Mm2",
    #                                 "floor_new_Mm2", "floor_dem_Mm2",
    #                                 "mat_int",
    #                                 "mat_stock_Mt",
    #                                 "mat_demand_Mt", "mat_scrap_Mt")))) %>%
    group_by_at(c(geo_levels, 
                  paste(c("urt", "clim", "inc_cl", "arch", "mat", "eneff", "material", "scenario", #"ssp", 
                          "year")))) %>% #Drop yr_con dimension
    
    summarise(floor_tot_Mm2 = sum(floor_tot_Mm2),
              floor_new_Mm2 = sum(floor_new_Mm2), 
              floor_dem_Mm2 = sum(floor_dem_Mm2),
              #mat_int = weighted.mean(mat_int, floor_tot_Mm2),
              mat_stock_Mt = sum(mat_stock_Mt),
              mat_demand_Mt = sum(mat_demand_Mt),
              mat_scrap_Mt = sum(mat_scrap_Mt)) %>%  
      
    ## add material end-of-life treatments
    left_join(
      shr_mat_eol %>%
        pivot_wider(
          names_from = eol_treat,
          values_from = shr_mat_eol
        ),
      by = c("region_gea", "material", "year")
    ) %>%
    # material reuse - reuse rate is treated as a potential
    mutate(mat_reuse_Mt = pmin(mat_demand_Mt, mat_scrap_Mt * reuse)) %>%
    # material recycling - recycling rate is treated as a potential
    mutate(mat_recyc_Mt = pmin((mat_demand_Mt - mat_reuse_Mt), mat_scrap_Mt * recycling)) %>%
    # material downcycling
    mutate(mat_downcyc_Mt = mat_scrap_Mt * downcycling) %>%
    # other material treatments
    mutate(mat_other_treat_Mt = mat_scrap_Mt - mat_reuse_Mt - mat_recyc_Mt - mat_downcyc_Mt) %>%
    # material demand split into primary, secondary production from recycling or reuse
    mutate(mat_primary_Mt = mat_demand_Mt - mat_reuse_Mt - mat_recyc_Mt) %>%
    # select relevant cols
    select(-c(reuse, recycling, downcycling, others)) %>%
    
      
    ungroup %>%
    mutate(mat_int = 1e3*mat_stock_Mt/floor_tot_Mm2) # Recalculate average material intensity
  
} else {

  mat_stock_i <- bld_cases_eneff %>%
    mutate(scenario = run) %>%
    # mutate(ssp = ssp_r) %>%
    mutate(year= yrs[i]) %>%
    #left_join(floor_cap) %>%
    left_join(bld_eneff_i) %>%
    left_join(dem_eneff_i) %>%
    left_join(new_eneff_i) %>%
    mutate(n_units = ifelse(is.na(n_units),0,n_units),
           n_new = ifelse(is.na(n_new),0,n_new),
           n_dem = ifelse(is.na(n_dem),0,n_dem)) %>%
    filter(n_units + n_new + n_dem != 0) %>%
    left_join(mat_int) %>%
    #filter(arch != "inf") %>% # Materials not calculated for slums
    mutate(floor_tot_Mm2 = n_units / 1e6) %>% # Mm2
    mutate(floor_new_Mm2 = n_new / stp / 1e6) %>% # Mm2/yr
    mutate(floor_dem_Mm2 = n_dem / stp / 1e6) %>% # Mm2/yr
    mutate(mat_stock_Mt = n_units * mat_int / 1e3 / 1e6) %>% #Mt
    mutate(mat_demand_Mt = n_new * mat_int / stp / 1e3 / 1e6) %>% #Mt/y
    mutate(mat_scrap_Mt = n_dem * mat_int / stp / 1e3 / 1e6) %>% #Mt/y
    
    # select_at(c(geo_levels, paste(c("urt", "clim", "inc_cl", "arch", "mat", "eneff", "material",
    #                                 "scenario", "ssp", "year",
    #                                 "floor_tot_Mm2",
    #                                 "floor_new_Mm2", "floor_dem_Mm2",
    #                                 "mat_int",
    #                                 "mat_stock_Mt",
    #                                 "mat_demand_Mt", "mat_scrap_Mt"))))
    group_by_at(c(geo_levels, 
                  paste(c("urt", "clim", "inc_cl", "arch", "mat", "eneff", "material", "scenario", #"ssp", 
                          "year")))) %>% #Drop yr_con dimension
      summarise(floor_tot_Mm2 = sum(floor_tot_Mm2),
                floor_new_Mm2 = sum(floor_new_Mm2), 
                floor_dem_Mm2 = sum(floor_dem_Mm2),
                #mat_int = weighted.mean(mat_int, floor_tot_Mm2),
                mat_stock_Mt = sum(mat_stock_Mt),
                mat_demand_Mt = sum(mat_demand_Mt),
                mat_scrap_Mt = sum(mat_scrap_Mt)) %>%  
    
      ## add material end-of-life treatments
      left_join(
        shr_mat_eol %>%
          pivot_wider(
            names_from = eol_treat,
            values_from = shr_mat_eol
          ),
        by = c("region_gea", "material", "year")
      ) %>%
      # material reuse - reuse rate is treated as a potential
      mutate(mat_reuse_Mt = pmin(mat_demand_Mt, mat_scrap_Mt * reuse)) %>%
      # material recycling - recycling rate is treated as a potential
      mutate(mat_recyc_Mt = pmin((mat_demand_Mt - mat_reuse_Mt), mat_scrap_Mt * recycling)) %>%
      # material downcycling
      mutate(mat_downcyc_Mt = mat_scrap_Mt * downcycling) %>%
      # other material treatments
      mutate(mat_other_treat_Mt = mat_scrap_Mt - mat_reuse_Mt - mat_recyc_Mt - mat_downcyc_Mt) %>%
      # material demand split into primary, secondary production from recycling or reuse
      mutate(mat_primary_Mt = mat_demand_Mt - mat_reuse_Mt - mat_recyc_Mt) %>%
      # select relevant cols
      select(-c(reuse, recycling, downcycling, others)) %>%
      
      ungroup %>%
      mutate(mat_int = 1e3*mat_stock_Mt/floor_tot_Mm2) # Recalculate average material intensity
}

## Stock results - Material - Add Cement

cement_content <- 0.15 ## Cement content in concrete

mat_stock_cem_i <- mat_stock_i %>%
  filter(material == "concrete") %>%
  mutate(material = "cement") %>%
  mutate(mat_stock_Mt = mat_stock_Mt * cement_content,
         mat_demand_Mt = mat_demand_Mt * cement_content,
         mat_scrap_Mt = mat_scrap_Mt * cement_content,
         mat_reuse_Mt = mat_reuse_Mt * cement_content,
         mat_recyc_Mt = mat_recyc_Mt * cement_content,
         mat_downcyc_Mt = mat_downcyc_Mt * cement_content,
         mat_other_treat_Mt = mat_other_treat_Mt * cement_content,
         mat_primary_Mt = mat_primary_Mt * cement_content)

mat_stock_i <- rbind(mat_stock_i, mat_stock_cem_i)
}

# Report stock by eneff - vintage
if ("vintage" %in% report_var){
  
  # Stock by eneff and vintage (for reporting)
  report$bld_eneff_age <- bind_rows(report$bld_eneff_age,
                                    bld_det_age_i %>%
                                      group_by_at(setdiff(names(bld_det_age_i), c(#"arch", 
                                        "fuel_heat","fuel_cool", "n_units_fuel"))) %>%
                                      summarise(n_units_eneff = sum(n_units_fuel)) %>%
                                      ungroup())
}

# Update energy demand/material results (for reporting)
if ("energy" %in% report_var){report$en_stock <-  bind_rows(report$en_stock,en_stock_i)}
if ("material" %in% report_var){report$mat_stock <- bind_rows(report$mat_stock,mat_stock_i)}

output = list(#en_stock = en_stock,
  #mat_stock = mat_stock,
  report = report,
  #stock_eneff.df = stock_eneff.df, 
  stock_aggr = stock_aggr, 
  bld_det_age_i = bld_det_age_i#, 
  #bld_det = bld_det, 
  #bld_eneff_age = bld_eneff_age
  #ms_new_i, ms_ren_i,
  #ms_new = ms_new, 
  #ms_ren = ms_ren
)

}

