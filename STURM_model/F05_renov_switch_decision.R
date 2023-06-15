

## Complete model with fuel choice


# ## Prepare renovation cost data ## Already provided as long format in script 01b
# cost_invest_ren_heat_unit <- cost_invest_ren_heat_unit %>% 
#   gather(key="fuel_heat_f", value="cost_invest_ren_heat", ct_fuel_heat_mod) %>% # Convert to long format (this is not on time steps! dedicated function cannot be used)
#   filter(!is.na(cost_invest_ren_heat)) # Exclude NA values (costs not available = option not feasible)

# # final classes for energy renovations ### MOVED TO o1b_input_data_scenarios_v0.7.R
# ct_eneff_f_ren <- names(cost_invest_ren_shell)[which(names(cost_invest_ren_shell) %in% ct_eneff$eneff)]
  
# # moved to input_scenarios from v0.7
# cost_invest_ren_shell <- cost_invest_ren_shell %>% 
#   gather(key="eneff_f", value="cost_invest_ren_shell", all_of(ct_eneff_f_ren)) %>% # Convert to long format (this is not on time steps! dedicated function cannot be used)
#   filter(!is.na(cost_invest_ren_shell)) # Exclude NA values (costs not available = option not feasible)


## BEGIN FUNCTION
fun_ms_ren_sw <- function(yrs,i,
                       bld_cases_fuel, ct_bld_age, ct_hh_tenr, ct_fuel_comb,
                       ct_ren_eneff, ct_ren_fuel_heat,
                       hh_size, floor_cap,
                       hh_tenure,
                       cost_invest_ren_shell,cost_invest_ren_heat,
                       cost_intang_ren_shell,cost_intang_ren_heat,
                       ct_fuel_excl_ren,ct_fuel_excl_reg,
                       discount_ren, heterog_ren, lifetime_ren,
                       rate_ren_low, rate_ren_high, #rate_ren, 
                       # en_hh_tot_ren_init,
                       # en_hh_tot_ren_fin
                       en_hh_tot
                       ){

print(paste0("Running renovation decisions - year ", yrs[i]))

# rounding (number of decimals)
rnd <- 5

## Define timestep
stp <- yrs[i]-yrs[i-1]
  
# Building cases fuels + tenure
bld_cases_fuel_tenr <- merge(bld_cases_fuel, as.data.frame(ct_hh_tenr)) %>% rename(tenr = ct_hh_tenr)
  
# Removed in v1.0 - now included in lcc_ren_hh processing
# # Edit fuel excluded on a regional level for renovation (from script 02a_data_processing)
# ct_fuel_excl_ren_i_reg <- ct_fuel_excl_reg %>% # initial fuel (before renovation)
#   rename(fuel_heat_i = fuel_heat) %>%
#   rename(fuel_excluded_i_reg = fuel_excluded_reg)
# ct_fuel_excl_ren_f_reg <- ct_fuel_excl_reg %>% # final fuel (after renovation)
#   rename(fuel_heat_f = fuel_heat) %>%
#   rename(fuel_excluded_f_reg = fuel_excluded_reg)
  
# Fuel types in renovation decisions
ct_fuel_heat_mod <-  sort(unique(ct_fuel_comb$fuel_heat[which(ct_fuel_comb$mod_decision == 1)]))

# Operational energy costs before/after renovation
# final energy costs to be used in renovation decisions (fuels and eneff are after renovation)
en_hh_tot_ren_fin <- en_hh_tot %>%
  rename(eneff_f = eneff) %>%
  rename(fuel_heat_f = fuel_heat) %>%
  select(-bld_age) # should be removed, otherwise no match with eneff (e.g. an older building can switch to a newer eneff standard)

# initial energy costs to be used in renovation decisions (fuels and eneff are before renovation)
# this is used to filter out hh with zero operation costs before renovation
en_hh_tot_ren_init <- en_hh_tot %>%
  #rename(eneff_i = eneff) %>% # in lcc_ren_hh initial names are without the "_i" suffix
  #rename(fuel_heat_i = fuel_heat) %>%
  rename(cost_op_m2_init = cost_op_m2) %>%
  select(-bld_age) # should be removed, otherwise no match with eneff (e.g. an older building can switch to a newer eneff standard)

## Prepare investment cost data
cost_invest_ren_shell_i <- cost_invest_ren_shell %>% filter(year == yrs[i])
cost_invest_ren_heat_i <- cost_invest_ren_heat %>% filter(year == yrs[i])
  
## Prepare intangible cost data
cost_intang_ren_shell_i <- cost_intang_ren_shell %>% filter(year == yrs[i])

cost_intang_ren_heat_i <- cost_intang_ren_heat # No need to transform: laready in long format in new version
# cost_intang_ren_heat_i <- cost_intang_ren_heat %>%
#   gather(key="fuel_heat_f", value="cost_intang_ren_heat", all_of(ct_fuel_heat_mod)) %>% # Convert to long format (this is not on time steps! dedicated function cannot be used)
#   filter(!is.na(cost_intang_ren_heat)) # Exclude NA values (costs not available = option not feasible)

# cost_intang_ren_shell <- cost_intang_ren_shell %>% 
#   gather(key="eneff_f", value="cost_intang_ren_shell", all_of(ct_eneff_f_ren)) %>% # Convert to long format (this is not on time steps! dedicated function cannot be used)
#   filter(!is.na(cost_intang_ren_shell)) # Exclude NA values (costs not available = option not feasible)



# ## Prepare data on hh shares by tenure for the year "i" ### NOT NECESSARY WITH LONG FORMAT
# hh_tenure_i <- hh_tenure %>%
#   select(-one_of(paste0("y",yrs[-i]))) %>%  # Results for the year "i"
#   rename(shr_tenr = paste0("y",yrs[i])) # select data for current year

# Test inputs - age categories
print("Test inputs - Building vintage")
print(paste0("DF ct_bld_age: ", ct_bld_age))
print(paste0("Class ct_bld_age: ", class(ct_bld_age)))
print(paste0("Year: ", yrs[i]))

bld_age_exst <- ct_bld_age %>% filter(year_i < yrs[i]) %>% select(bld_age_id) %>% pull(bld_age_id)

## Prepare dataframe for LCC calculations at household level - Renovations
lcc_ren_hh <- bld_cases_fuel_tenr %>% 
  filter(bld_age %in% bld_age_exst) %>% ## RENOVATION POSSIBLE FOR EXISTING BUILDINGS ONLY 
  left_join(ct_ren_eneff %>% rename(eneff = eneff_i)) %>% # add eneff categories for renovations
  left_join(ct_ren_fuel_heat %>% rename(fuel_heat = fuel_heat_i)) %>% # add heat_fuel categories for renovations
  left_join(ct_fuel_excl_ren) %>% ## Constraint: fuels not allowed for renovation
  left_join(ct_fuel_excl_reg %>% 
              rename(ct_fuel_excl_i_reg = ct_fuel_excl_reg)) %>% ## Constraint (before renovation): fuels not used in specific regions
  left_join(ct_fuel_excl_reg %>% 
              rename(fuel_heat_f = fuel_heat) %>%
              rename(ct_fuel_excl_f_reg = ct_fuel_excl_reg)) %>% ## Constraint (after renovation): fuels not used in specific regions
  filter(mod_decision == 1) %>% # exclude cases out of modelling decisions (e.g. district heating, substandard buildings)
  filter(is.na(ct_fuel_excl_ren), 
         is.na(ct_fuel_excl_i_reg), 
         is.na(ct_fuel_excl_f_reg)) %>% #  exclude non-permitted fuels (e.g. coal for passive houses)
  filter(!is.na(eneff_f)) %>% # filter out new construction, already renovated buildings, etc. (no "eneff_f" value availablefor those)
  # filter((eneff == eneff_f & fuel_heat == fuel_heat_f) | 
  #          (eneff == eneff_f & swt_exst == 1) | # transitions between eneffs for existing buildings (without renovation) require the renovation switch "swt_ren" to be ON
  #          (eneff != eneff_f & swt_ren == 1)) %>% # transitions between eneffs for renovations require the renovation switch "swt_ren" to be ON
  filter((eneff == eneff_f & fuel_heat == fuel_heat_f) | 
           (eneff == eneff_f & ct_ren_fuel_heat == 1) | # transitions between eneffs for existing buildings (without renovation) require the renovation switch "swt_ren" to be ON
           (eneff != eneff_f & ct_ren_fuel_heat == 1)) %>% # transitions between eneffs for renovations require the renovation switch "ct_ren_fuel_heat" to be ON (1). Note:  swt_exst and  swt_ren replaced by one column only: ct_ren_fuel_heat 
    mutate(year = yrs[i]) %>% # Attach year (in the loop)
  left_join(hh_size) %>% # Add HH size
  left_join(floor_cap) %>% ## Add floor per capita
  left_join(lifetime_ren) %>% ## Add lifetime ren construction (for investment: based on loan duration)
  #left_join(bld_dyn_par) %>% ## Add all building synamics parameter 
  left_join(cost_invest_ren_shell_i) %>% ## Add investment costs 
  #filter(!is.na(cost_invest_ren_shell)) %>% # Remove NAs
  left_join(cost_invest_ren_heat_i) %>% ## Add investment costs
  #filter(!is.na(cost_invest_ren_heat)) %>% # Remove NAs
  mutate_cond(eneff == eneff_f & fuel_heat == fuel_heat_f, cost_invest_ren_heat = 0) %>% #no renovation
  mutate(cost_invest_hh = cost_invest_ren_heat + (cost_invest_ren_shell * floor_cap * hh_size)) %>% # Calculate total investment costs
  left_join(en_hh_tot_ren_fin) %>% # operation costs after renovation
  left_join(en_hh_tot_ren_init) %>% # operation costs before renovation 
  filter(cost_op_m2_init >0) %>% # Filter out hh with no operational cost # REMOVING ALL RECORDS WITH NO HEATING!!!
  mutate(cost_op_hh = cost_op_m2 * floor_cap * hh_size) %>%  ## Add operative costs (total)
  #mutate(cost_intang_hh = 0) %>% ## Add intangible costs
  left_join(cost_intang_ren_shell_i) %>% ## Add intangible costs
  #filter(!is.na(cost_intang_ren_shell)) %>% # Remove NAs
  left_join(cost_intang_ren_heat_i %>% rename(fuel_heat = fuel_heat_i)) %>% ## Add intangible costs
  #filter(!is.na(cost_intang_ren_heat)) %>% # Remove NAs
  mutate(cost_intang_hh = cost_intang_ren_heat + (cost_intang_ren_shell * floor_cap * hh_size)) %>% # Calculate total investment costs
  left_join(discount_ren) %>% ## Add discount rates
  left_join(heterog_ren) %>% ## Add discount rates
  mutate(lcc_ren = fun_lcc(cost_invest_hh, cost_op_hh, cost_intang_hh, discount_ren, lifetime_ren)) %>% ## Apply LCC function to new construction
  mutate(cost_op_hh_lcc = lcc_ren - cost_intang_hh - cost_invest_hh) %>% # Calculate operation lcc (for reporting)
  mutate(lcc_ren_exp = fun_lcc_exp(lcc_ren, heterog_ren)) %>% ## Expon. nu
  #select(-eneff) %>%
  rename(eneff_i = eneff) %>% # Rename eneff column 
  rename(fuel_heat_i = fuel_heat) %>% ## REMOVED IN v0.7
  filter(cost_intang_ren_shell != 99999) %>% # FILTERING BASED ON INTANGIBLE COSTS - NECESSARY? 
  filter(cost_intang_ren_heat != 99999) %>% # FILTERING BASED ON INTANGIBLE COSTS - NECESSARY?
  #select(-mod_decision) %>% # keep mod_decision so it can be used in the stock dynamics model
  select(-c(ct_ren_fuel_heat,ct_fuel_excl_ren,ct_fuel_excl_i_reg,ct_fuel_excl_f_reg))

  try(if(nrow(lcc_ren_hh)!=nrow(distinct(lcc_ren_hh))) stop("Error in renovation calculation! Duplicated records in lcc_ren_hh"))

### MARKET SHARE - RENOVATIONS + FUEL SWITCHES
# All possible combinations covered (including no renovation)
# Totals market shares by eneff_i + fuel_heat_i = 1
ms_i <- lcc_ren_hh %>%
  select(-c(#"acc_cool",
    "hh_size", "floor_cap", "cost_invest_ren_shell", "cost_invest_ren_heat", "cost_invest_hh", "cost_op_m2", "cost_op_m2_init",
    "cost_op_hh", "cost_intang_ren_heat", "cost_intang_ren_shell", "cost_intang_hh", "discount_ren", 
    "lifetime_ren", "heterog_ren", "lcc_ren", "cost_op_hh_lcc"))
ms_i <- ms_i %>%
  group_by_at(setdiff(names(ms_i), c("eneff_f", "fuel_heat_f", "lcc_ren_exp"))) %>% # Select all variables, except "eneff_f" and "lcc_ren_hh_exp" for grouping)
  mutate(lcc_ren_exp_sum=sum(lcc_ren_exp)) %>%
  ungroup() %>% ### ADD CHECK HERE ### "eneff_f" categories in "lcc_ren_hh" should include all possible renovation categories!
  mutate(ms_unwgt = lcc_ren_exp/lcc_ren_exp_sum) %>% #market share - not weighted on share of hh tenure
  left_join(hh_tenure) %>% #Join data on tenure shares
  mutate(ms_wgt = ms_unwgt * hh_tenure) %>% # Weight market share on tenures
  group_by_at(setdiff(names(ms_i), c("lcc_ren_exp", "lcc_ren_exp_sum", "tenr", "ms_wgt","ms_unwgt", "hh_tenure"))) %>% # Select all variables, except "eneff" and "n_dem" for grouping)
  summarise(ms=round(sum(ms_wgt),rnd)) %>%
  ungroup() %>%
  select(-bld_age) # Remove periods of construction

# Renovation rate ## REGIONS WITH NO HEATING ARE EXCLUDED BEFORE!
rate_ren_i <- ms_i %>% 
  filter(eneff_i == eneff_f) %>% 
  mutate(ren_share_calc = 1-ms) %>%
  mutate(rate_ren_calc = (1-ms)/stp) %>%
  left_join(rate_ren_low) %>%
  left_join(rate_ren_high) %>%
  mutate(rate_ren = ifelse(rate_ren_calc < rate_ren_low, rate_ren_low, 
                               ifelse(rate_ren_calc > rate_ren_high, rate_ren_high, rate_ren_calc))) %>%
  rename(eneff = eneff_i, fuel_heat = fuel_heat_i) %>%
  select(-c(eneff_f, fuel_heat_f, ms, 
            #rate_ren, 
            rate_ren_low, rate_ren_high,
            rate_ren_calc, ren_share_calc)) 

# Update market shares - keep renovations only
ms_ren_i <- ms_i %>%
  filter(eneff_i != eneff_f) %>% 
  group_by_at(setdiff(names(ms_i), c("eneff_f", "fuel_heat_f", "ms"))) %>%
  mutate(ms_tot= sum(ms)) %>%
  ungroup %>%
  mutate(ms_ren = ifelse(ms_tot>0,round(ms/ms_tot, rnd),0)) %>%
  filter(ms_ren>0) %>%
  select(-c(ms, ms_tot))

# Update market shares - keep fuel switches only
ms_sw_i <- ms_i %>%
  filter(eneff_i == eneff_f & fuel_heat_i != fuel_heat_f) %>% 
  group_by_at(setdiff(names(ms_i), c("eneff_f", "fuel_heat_f", "ms"))) %>%
  mutate(ms_tot= sum(ms)) %>%
  ungroup %>%
  mutate(ms = ifelse(ms_tot>0,round(ms/ms_tot, rnd),0)) %>%
  filter(ms>0) %>%
  select(-c(ms_tot, eneff_f)) %>%
  rename(eneff = eneff_i) %>%
  rename(fuel_heat = fuel_heat_i)


#output = ms_ren_i
output = list(ms_ren_i = ms_ren_i,
              ms_sw_i = ms_sw_i,
              rate_ren_i = rate_ren_i)

} # END FUNCTION

# SIMPLE FUNCTION - Fixed market shares 

fun_ms_ren_target <- function(yrs,i,
                              bld_cases_fuel, ct_bld_age,
                              shr_eneff_ren,shr_fuel_heat_ren
                              ){
  
  print(paste0("Running renovation target - year ", yrs[i]))
  
  ms_ren_i <- bld_cases_fuel %>%
    mutate(year = yrs[i]) %>% # Add years columns
    rename(eneff_i = eneff) %>% # Rename eneff column
    rename(fuel_heat_i = fuel_heat) %>%
    left_join(shr_eneff_ren) %>% # Join market share column
    left_join(shr_fuel_heat_ren) %>%
    mutate(ms_ren = ms_eneff*ms_fuel) %>%
    filter(!is.na(ms_ren)) %>%
    filter(ms_ren > 0) %>%
    filter(mod_decision == 1) %>%
    mutate(ct_fuel_excl_ren = 0) %>% # Placeholder for fuels to be excl
    select(-c(bld_age,ms_eneff,ms_fuel,
              ct_fuel_excl_ren
              ))
  
  print(paste0("Completed renovation target - year ", yrs[i]))
  
  output = ms_ren_i
  
}


fun_ms_ren_target2 <- function(yrs,i,
                              bld_cases_fuel, ct_bld_age,
                              ms_ren_target
){
  
  print(paste0("Running renovation target - year ", yrs[i]))
  
  ms_ren_i <- bld_cases_fuel %>%
    mutate(year = yrs[i]) %>% # Add years columns
    rename(eneff_i = eneff) %>% # Rename eneff column
    rename(fuel_heat_i = fuel_heat) %>%
    left_join(ms_ren_target) %>% # Join market share column
    #mutate(ms= ms_eneff*ms_fuel) %>%
    filter(!is.na(ms)) %>%
    filter(ms > 0) %>%
    filter(mod_decision == 1) %>%
    #mutate(fuel_excluded = 0) %>% # Placeholder for fuels to be excluded
    select(-c(bld_age
    ))
  
  print(paste0("Completed renovation target - year ", yrs[i]))
  
  output = ms_ren_i
  
}

## FUNCTION -FUEL SWITCHES (WITHOUT RENOVATION)
fun_ms_fuel_sw <- function(yrs,i,
                           bld_cases_fuel, ct_bld_age,
                           shr_fuel_heat_sw
){
  print(paste0("Running fuel switch target - year ", yrs[i]))
  
  # Filter building age cohorts - past periods of construction
  p_past <- ct_bld_age %>% filter(year_f < yrs[i]) %>% pull(bld_age_id)
  
  ms_sw_i <- bld_cases_fuel %>%
    mutate(year = yrs[i]) %>% # Add years columns
    filter(bld_age %in% p_past) %>%
    #rename(fuel_heat_i = fuel_heat) %>%
    left_join(shr_fuel_heat_sw) %>%
    filter(!is.na(ms)) %>%
    filter(ms > 0) %>%
    filter(mod_decision == 1) %>%
    #mutate(fuel_excluded = 0) %>% # Placeholder for fuels to be excluded
    select(-c(bld_age,
              #, fuel_excluded
    ))
  
  print(paste0("Completed fuel switch target - year ", yrs[i]))
  
  output = ms_sw_i
  
}

# ## Export LCC results for selected years
# yrs_ren_rep <- c(2020,2030,2050)
# 
# if (yrs[i] %in% yrs_ren_rep)
# {write.csv(en_stock_fuel_base.df, paste0(path_out,"lcc_ren_",run,"_",yrs[i],".csv"), row.names = F)}


# # Remove unused files
# rm(lcc_ren_hh)
# rm(lcc_ren_hh_sum)
# rm(lcc_ren_hh_exp)
# rm(ms_ren_tenr_i)
