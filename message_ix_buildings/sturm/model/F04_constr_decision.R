
## CONSTRUCTION DECISIONS
## Functions called within the loop on time steps "i" (script F10)


## Endogenous construction decisions
# Multinomial logit on life-cycle costs (LCC): choice of eneff standard and heating fuel for new buildings
# of the current construction period. Returns market shares "ms" by building case, summing to 1 over (eneff, fuel_heat).
fun_ms_new <- function(yrs,i,
                       bld_cases_fuel, ct_bld_age,
                       hh_size, floor_cap,
                       cost_invest_new_shell, cost_invest_new_heat,
                       cost_intang_new_shell, cost_intang_new_heat,
                       ct_fuel_excl_new, ct_fuel_excl_reg,
                       discount_new, heterog_new, lifetime_new,
                       en_hh_tot){

print(paste0("Running construction decisions - year ", yrs[i]))

# Construction period(s) of the current year
ct_bld_age_id_i <- ct_bld_age %>%
  filter(yrs[i] >= year_i & yrs[i] <= year_f) %>%
  pull(bld_age_id)

# Investment and intangible costs for the current year
cost_invest_new_shell_i <- cost_invest_new_shell %>% filter(year == yrs[i])
cost_invest_new_heat_i <- cost_invest_new_heat %>% filter(year == yrs[i])
cost_intang_new_shell_i <- cost_intang_new_shell %>% filter(year == yrs[i])
cost_intang_new_heat_i <- cost_intang_new_heat %>% filter(year == yrs[i])

## LCC of each option, by building case
lcc_new_hh <- bld_cases_fuel %>%
  mutate(year = yrs[i]) %>%
  ljoin(hh_size) %>%
  ljoin(floor_cap) %>%
  ljoin(ct_fuel_excl_new) %>% # fuels not allowed in new construction
  ljoin(ct_fuel_excl_reg) %>% # fuels not used in the region
  mutate_cond(is.na(ct_fuel_excl_new), ct_fuel_excl_new = 0) %>% # 0 = fuel permitted; 1 = fuel excluded
  mutate_cond(is.na(ct_fuel_excl_reg), ct_fuel_excl_reg = 0) %>%
  ljoin(cost_invest_new_shell_i) %>%
  ljoin(cost_invest_new_heat_i) %>%
  mutate(cost_invest_hh = cost_invest_new_heat + (cost_invest_new_shell * floor_cap * hh_size)) %>% # heating system per unit + shell per m2
  ljoin(en_hh_tot) %>%
  mutate(cost_op_hh = cost_op_m2 * floor_cap * hh_size) %>%
  ljoin(cost_intang_new_shell_i) %>%
  ljoin(cost_intang_new_heat_i) %>%
  mutate(cost_intang_hh = cost_intang_new_heat + (cost_intang_new_shell * floor_cap * hh_size)) %>%
  ljoin(discount_new) %>%
  ljoin(lifetime_new) %>% # lifetime for investment (loan duration)
  ljoin(heterog_new) %>%
  mutate(lcc_new = fun_lcc(cost_invest_hh, cost_op_hh, cost_intang_hh, discount_new, lifetime_new)) %>%
  mutate(lcc_new_exp = fun_lcc_exp(lcc_new, heterog_new)) # logit term

# Keep available options only
lcc_new_hh <- lcc_new_hh %>%
  filter(bld_age %in% ct_bld_age_id_i) %>% # current construction period only
  filter(mod_decision == 1) %>% # exclude cases out of modelling decisions (e.g. district heating, substandard buildings)
  filter(ct_fuel_excl_new == 0 & ct_fuel_excl_reg == 0) %>% # exclude non-permitted fuels (e.g. coal for passive houses)
  filter(cost_intang_new_shell != 99999) %>% # 99999 = option not available
  filter(cost_intang_new_heat != 99999) %>%
  filter(!is.na(lcc_new))

## Market shares
# Cost columns are dropped so that the remaining columns identify the decision (case, option)
lcc_new_hh_exp <- lcc_new_hh %>% select(-c(hh_size, floor_cap,
                                           cost_invest_hh,
                                           mod_decision, ct_fuel_excl_new,ct_fuel_excl_reg,
                                           cost_invest_new_shell, cost_invest_new_heat,
                                           cost_op_m2, cost_op_hh,
                                           cost_intang_new_shell, cost_intang_new_heat,
                                           cost_intang_hh,
                                           discount_new, lifetime_new, lcc_new, heterog_new))

# Sum of logit terms over the options (eneff, fuel_heat)
lcc_new_hh_sum <- lcc_new_hh_exp %>%
  group_by_at(setdiff(names(lcc_new_hh_exp), c("eneff", "fuel_heat", "lcc_new_exp"))) %>%
  summarise(lcc_new_exp_sum=sum(lcc_new_exp))

ms_new_i <- lcc_new_hh_exp %>%
  ljoin(lcc_new_hh_sum) %>%
  mutate(ms = lcc_new_exp/lcc_new_exp_sum) %>%
  select(-c(lcc_new_exp, lcc_new_exp_sum)) %>%
  mutate_cond(mat == "sub", ms = 1) %>% # substandard buildings: one option only
  mutate(ms = round(ms,5)) %>%
  select(-bld_age)

print(paste0("Completed construction decisions - year ", yrs[i]))
output = ms_new_i

} # END FUNCTION


## Exogenous construction decisions: fixed market shares of eneff and heating fuel
fun_ms_new_target <- function(yrs,i,
                              bld_cases_eneff, bld_cases_fuel,
                              ct_bld_age,
                              shr_eneff_new, shr_fuel_heat_new){

  print(paste0("Running construction target - year ", yrs[i]))

  # Construction period(s) of the current year
  p_i <- ct_bld_age %>% filter(year_i <= yrs[i] & year_f >= yrs[i]) %>% pull(bld_age_id)

  ms_new_i <- bld_cases_fuel %>%
    mutate(year = yrs[i]) %>%
    ljoin(shr_eneff_new) %>%
    ljoin(shr_fuel_heat_new) %>%
    filter(bld_age %in% p_i) %>%
    mutate(ms= shr_eneff_new*shr_fuel_heat_new) %>%
    mutate_cond(is.na(ms) & eneff == "ns", ms = 1) %>% # non-standard buildings
    filter(!is.na(ms)) %>%
    select(-c(bld_age,shr_eneff_new,shr_fuel_heat_new))

  print(paste0("Completed construction target - year ", yrs[i]))
  output = ms_new_i
}
