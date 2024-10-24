
## CONSTRUCTION DECISIONS
## this calculation is to be put within a loop on "i" (time steps)
## Different fuels currently not included

## check construction period: extract valid ones for the current timestep 

# # mod_stock_level == "fuel"
# # ms = 1 -> level "country" "scenario" "urt" "inc_cl""arch"  "clim"  "acc_cool" (choice between eneffs and fuels)


## Complete model with fuel choice

fun_ms_new <- function(yrs,i,
                       bld_cases_fuel, ct_bld_age,
                       hh_size, floor_cap,
                       cost_invest_new_shell, cost_invest_new_heat,
                       cost_intang_new_shell, cost_intang_new_heat,
                       ct_fuel_excl_new, ct_fuel_excl_reg,
                       discount_new, heterog_new, lifetime_new,
                       en_hh_tot){

print(paste0("Running construction decisions - year ", yrs[i]))

ct_bld_age_id_i <- ct_bld_age %>% 
  filter(yrs[i] >= year_i & yrs[i] <= year_f) 

ct_bld_age_id_i <- ct_bld_age_id_i[["bld_age_id"]]

  
## Prepare investment cost data
cost_invest_new_shell_i <- cost_invest_new_shell %>% filter(year == yrs[i])
cost_invest_new_heat_i <- cost_invest_new_heat %>% filter(year == yrs[i])


## Prepare intangible cost data
cost_intang_new_shell_i <- cost_intang_new_shell %>% filter(year == yrs[i])
cost_intang_new_heat_i <- cost_intang_new_heat %>% filter(year == yrs[i])


## Prepare dataframe for LCC calculations at household level - New constructions
lcc_new_hh <- bld_cases_fuel %>%
  mutate(year = yrs[i]) %>% # Attach year (in the loop)
  left_join(hh_size) %>% # Add HH size # %>% filter(year == yrs[i]) # Filter "i" year 
  left_join(floor_cap) %>% 
  #filter(year == yrs[i]) %>% # Filter "i" year 
  left_join(ct_fuel_excl_new) %>% ## Constraint: fuels not allowed for new construction
  left_join(ct_fuel_excl_reg) %>% ## Constraint: fuels not used in specific regions
  mutate_cond(is.na(ct_fuel_excl_new), ct_fuel_excl_new = 0) %>% # 0=fuel permitted; 1=fuel excluded
  mutate_cond(is.na(ct_fuel_excl_reg), ct_fuel_excl_reg = 0) %>% # 0=fuel permitted; 1=fuel excluded
  left_join(cost_invest_new_shell_i) %>% ## Add investment costs - shell
  left_join(cost_invest_new_heat_i) %>% ## Add investment costs - heating fuel shift
  #mutate(cost_inv = cost_shell + cost_heat) %>% 
  mutate(cost_invest_hh = cost_invest_new_heat + (cost_invest_new_shell * floor_cap * hh_size)) %>% # Heat sys cost per unit - Shell costs per m2
  left_join(en_hh_tot) %>% ## Add operative costs (total) # %>% filter(year == yrs[i]) # Filter "i" year 
  mutate(cost_op_hh = cost_op_m2 * floor_cap * hh_size) %>%
  #mutate(cost_intang_hh = 0) %>% ## Add intangible costs
  left_join(cost_intang_new_shell_i) %>% ## Add intangible costs - shell
  left_join(cost_intang_new_heat_i) %>% ## Add intangible costs - heating fuel shift
  mutate(cost_intang_hh = cost_intang_new_heat + (cost_intang_new_shell * floor_cap * hh_size)) %>% # total intangible costs by hh
  left_join(discount_new) %>% ## Add discount rates #%>% filter(year == yrs[i]) # Filter "i" year 
  left_join(lifetime_new) %>% ## Add lifetime new construction (for investment: based on loan duration)
  left_join(heterog_new) %>% # Add heterogeneity parameter
  #mutate(lifetime_new = lifetime_new) %>% ## Add lifetime new construction (for investment: based on loan duration) ## old formulation
  mutate(lcc_new = fun_lcc(cost_invest_hh, cost_op_hh, cost_intang_hh, discount_new, lifetime_new)) %>% ## Apply LCC function to new construction
  mutate(lcc_new_exp = fun_lcc_exp(lcc_new, heterog_new))

print(paste0("generate lcc_new_hh "))
# Filter rows based on constraints and calc settings
lcc_new_hh <- lcc_new_hh %>%
  filter(bld_age %in% ct_bld_age_id_i) %>% # only current construction period allowed
  filter(mod_decision == 1) %>% # cases out of modelling decisions (e.g. district heating, substandard buildings)
  filter(ct_fuel_excl_new == 0 & ct_fuel_excl_reg == 0) %>% # excluded non-permitted fuels (e.g. coal for passive houses)
  filter(cost_intang_new_shell != 99999) %>% # FILTERING BASED ON INTANGIBLE COSTS
  filter(cost_intang_new_heat != 99999) %>% # FILTERING BASED ON INTANGIBLE COSTS
  filter(!is.na(lcc_new))

print(paste0("generate lcc_new_hh_exp "))
# Dataframe with data on exponents  
lcc_new_hh_exp <- lcc_new_hh %>% select(-c(hh_size, floor_cap, #cost_inv, 
                                           cost_invest_hh, 
                                           #fuel_heat, fuel_cool, ## here they should stay!!! they represent different choices 
                                           #en_m2, en_hh,      
                                           # price_en,
                                           mod_decision, ct_fuel_excl_new,ct_fuel_excl_reg,
                                           cost_invest_new_shell, cost_invest_new_heat,
                                           cost_op_m2, cost_op_hh, 
                                           cost_intang_new_shell, cost_intang_new_heat,
                                           cost_intang_hh,
                                           discount_new, lifetime_new, lcc_new, heterog_new))

print(paste0("generate lcc_new_hh_sum "))
# calculate sum of exponents
lcc_new_hh_sum <- lcc_new_hh_exp %>% 
  #mutate(eneff_fuel = paste(c(eneff,fuel_heat),sep="_")) %>%
  group_by_at(setdiff(names(lcc_new_hh_exp), c("eneff", "fuel_heat", "lcc_new_exp"))) %>% # Select all variables, except "eneff_f" and "lcc_ren_hh_exp" for grouping)
  #group_by_at(setdiff(names(lcc_new_hh_exp), c("eneff", "fuel_heat", "fuel_cool", "lcc_new_exp"))) %>% ## NOT WORKING!!! # Select all variables, except "eneff_f" and "lcc_ren_hh_exp" for grouping)
  summarise(lcc_new_exp_sum=sum(lcc_new_exp))

print(paste0("update lcc_new_hh_exp "))
# join sums 
lcc_new_hh_exp <- lcc_new_hh_exp %>% left_join(lcc_new_hh_sum) %>%
  mutate(ms = lcc_new_exp/lcc_new_exp_sum) #market share - not weighted on share of hh tenure

print(paste0("generate ms_new_i"))
# Market shares for the year "i"
ms_new_i <- lcc_new_hh_exp %>% 
  select(-c(lcc_new_exp, lcc_new_exp_sum)) %>%
  mutate_cond(mat == "sub", ms = 1) %>% # assign ms = 1 to substandard buildings
  mutate_cond(bld_age %nin% ct_bld_age_id_i, ms = 0) %>% # assign ms = 0 to standards not valid for the current period
  mutate(ms = round(ms,5)) %>% # round results to save memory
  #filter(!is.na(ms)) %>% # filter out NA values
  select(-bld_age) # Remove periods of construction

# # Market share by eneff (used to assign eneff shares to district heating - new construction) ### MOVED TO stock dynamics script
# # Market shares for the year "i" 
# ms_new_eneff_i <- ms_new_i %>%
#   group_by_at(setdiff(names(ms_new_i), c("fuel_heat", "ms"))) %>% # Select all variables, except "eneff_f" and "lcc_ren_hh_exp" for grouping))
#   summarise(ms_eneff = sum(ms))
#
# output = list(ms_new_i,ms_new_eneff_i)
print(paste0("Completed construction decisions - year ", yrs[i]))
output = ms_new_i

  
} # END FUNCTION

# SIMPLE FUNCTION - Fixed market shares

fun_ms_new_target <- function(yrs,i,
                              bld_cases_eneff, bld_cases_fuel, 
                              ct_bld_age,
                              shr_eneff_new, shr_fuel_heat_new){
  
  print(paste0("Running construction target - year ", yrs[i]))
  
  # Filter building age cohorts - current period of construction
  p_i <- ct_bld_age %>% filter(year_i <= yrs[i] & year_f >= yrs[i]) %>% pull(bld_age_id)
  
  ## Prepare dataframe for LCC calculations at household level - New constructions
  ms_new_i <- bld_cases_fuel %>% 
    mutate(year = yrs[i]) %>% 
    left_join(shr_eneff_new) %>% # Join market share column
    left_join(shr_fuel_heat_new) %>%
    filter(bld_age %in% p_i) %>%
    mutate(ms= shr_eneff_new*shr_fuel_heat_new) %>%
    mutate_cond(is.na(ms) & eneff == "ns", ms = 1) %>%
    filter(!is.na(ms)) %>% # temporary fix (renovated buildings are taken as they belong to building period p5)
    select(-c(bld_age,shr_eneff_new,shr_fuel_heat_new))

  
  # ## Prepare dataframe at eneff level (used to assign district heating in script M04) ### MOVED TO stock dynamics script
  # ms_new_eneff_i <- bld_cases_eneff %>% 
  #   mutate(year = yrs[i]) %>% 
  #   left_join(shr_eneff_new) %>% # Join market share column
  #   filter(bld_age %in% p_i) %>%
  #   mutate_cond(is.na(ms_eneff) & eneff == "ns", ms = 1) %>%
  #   select(-c(bld_age))
  
  print(paste0("Completed construction target - year ", yrs[i]))
  output = ms_new_i
}

  # # remove unused files
  # rm(ct_bld_age_id_i)
  # rm(lcc_new_hh,lcc_new_hh_exp,lcc_new_hh_sum)

# write out results for testing
#write.csv(ms_new_i, paste0(path_out,"ms_new_test.csv"))



