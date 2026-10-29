
## RENOVATION AND FUEL SWITCH DECISIONS
## Functions called within the loop on time steps "i" (script F10)


## Weight market shares over a household dimension
# Market shares are calculated separately for each household category (e.g. tenure: discount rates differ by tenure)
# and then aggregated with the share of households in each category.
# ms: data frame with market shares in column "ms_unwgt", by option and by the columns in w_dims
# weights: data frame with the household shares in column w_col, by w_dims (summing to 1 over w_dims)
# Returns the weighted market shares in column "ms", without the w_dims columns
fun_ms_weighted <- function(ms, weights, w_col, w_dims, rnd) {
  keys <- setdiff(names(ms), c(w_dims, "ms_unwgt")) # grouping columns: everything except household dimensions
  ms %>%
    ljoin(weights) %>%
    mutate(ms_wgt = ms_unwgt * .data[[w_col]]) %>%
    group_by_at(keys) %>%
    summarise(ms = round(sum(ms_wgt), rnd)) %>%
    ungroup()
}


## Endogenous renovation and fuel switch decisions
# Multinomial logit on life-cycle costs (LCC), by tenure, weighted with the share of households by tenure.
# The options for each building case (eneff_i, fuel_heat_i) are all final states (eneff_f, fuel_heat_f),
# including no renovation and fuel switch without renovation, so the shares sum to 1 by initial state.
# Returns a list of:
#   ms_ren_i:   market shares among renovation options (eneff_f != eneff_i), column "ms_ren"
#   ms_sw_i:    market shares among fuel switches without renovation, column "ms"
#   rate_ren_i: renovation rate (share of buildings renovated per year), bounded by rate_ren_low and rate_ren_high
fun_ms_ren_sw <- function(yrs,i,
                       bld_cases_fuel, ct_bld_age, ct_tenr, ct_fuel_comb,
                       ct_ren_eneff, ct_ren_fuel_heat,
                       hh_size, floor_cap,
                       hh_tenure,
                       cost_invest_ren_shell,cost_invest_ren_heat,
                       cost_intang_ren_shell,cost_intang_ren_heat,
                       ct_fuel_excl_ren,ct_fuel_excl_reg,
                       discount_ren, heterog_ren, lifetime_ren,
                       rate_ren_low, rate_ren_high,
                       en_hh_tot
                       ){

print(paste0("Running renovation decisions - year ", yrs[i]))

# rounding (number of decimals)
rnd <- 5

# Length of the time step (years)
stp <- yrs[i]-yrs[i-1]

# Building cases by tenure (discount rates differ by tenure)
bld_cases_fuel_tenr <- merge(bld_cases_fuel, data.frame(tenr = sort(unique(ct_tenr$tenr))))

# Operational energy costs after renovation (by final eneff and fuel)
# bld_age is removed: a building can renovate to the eneff standard of a newer construction period
en_hh_tot_ren_fin <- en_hh_tot %>%
  rename(eneff_f = eneff) %>%
  rename(fuel_heat_f = fuel_heat) %>%
  select(-bld_age)

# Operational energy costs before renovation (used to exclude cases without heating)
en_hh_tot_ren_init <- en_hh_tot %>%
  rename(cost_op_m2_init = cost_op_m2) %>%
  select(-bld_age)

# Investment and intangible costs for the current year (intangible heating costs have no year dimension)
cost_invest_ren_shell_i <- cost_invest_ren_shell %>% filter(year == yrs[i])
cost_invest_ren_heat_i <- cost_invest_ren_heat %>% filter(year == yrs[i])
cost_intang_ren_shell_i <- cost_intang_ren_shell %>% filter(year == yrs[i])
cost_intang_ren_heat_i <- cost_intang_ren_heat

# Periods of construction of existing buildings (only existing buildings can be renovated)
bld_age_exst <- ct_bld_age %>% filter(year_i < yrs[i]) %>% pull(bld_age_id)

## LCC of each option, by building case and tenure
lcc_ren_hh <- bld_cases_fuel_tenr %>%
  filter(bld_age %in% bld_age_exst) %>%
  ljoin(ct_ren_eneff %>% rename(eneff = eneff_i)) %>% # final eneff options
  ljoin(ct_ren_fuel_heat %>% rename(fuel_heat = fuel_heat_i)) %>% # final heating fuel options
  ljoin(ct_fuel_excl_ren) %>% # fuels not allowed in renovation
  ljoin(ct_fuel_excl_reg %>%
          rename(ct_fuel_excl_i_reg = ct_fuel_excl_reg)) %>% # fuels not used in the region (before renovation)
  ljoin(ct_fuel_excl_reg %>%
          rename(fuel_heat_f = fuel_heat) %>%
          rename(ct_fuel_excl_f_reg = ct_fuel_excl_reg)) %>% # fuels not used in the region (after renovation)
  filter(mod_decision == 1) %>% # exclude cases out of modelling decisions (e.g. district heating, substandard buildings)
  filter(is.na(ct_fuel_excl_ren),
         is.na(ct_fuel_excl_i_reg),
         is.na(ct_fuel_excl_f_reg)) %>% # exclude non-permitted fuels (e.g. coal for passive houses)
  filter(!is.na(eneff_f)) %>% # exclude cases without renovation options (e.g. already renovated buildings)
  filter((eneff == eneff_f & fuel_heat == fuel_heat_f) | # no renovation
           (eneff == eneff_f & ct_ren_fuel_heat == 1) | # fuel switch without renovation: requires ct_ren_fuel_heat = 1
           (eneff != eneff_f & ct_ren_fuel_heat == 1)) %>% # renovation: requires ct_ren_fuel_heat = 1
  mutate(year = yrs[i]) %>%
  ljoin(hh_size) %>%
  ljoin(floor_cap) %>%
  ljoin(lifetime_ren) %>% # lifetime for investment (loan duration)
  ljoin(cost_invest_ren_shell_i) %>%
  ljoin(cost_invest_ren_heat_i) %>%
  mutate_cond(eneff == eneff_f & fuel_heat == fuel_heat_f, cost_invest_ren_heat = 0) %>% # no renovation: no investment
  mutate(cost_invest_hh = cost_invest_ren_heat + (cost_invest_ren_shell * floor_cap * hh_size)) %>% # investment per household
  ljoin(en_hh_tot_ren_fin) %>% # operational costs after renovation
  ljoin(en_hh_tot_ren_init) %>% # operational costs before renovation
  filter(cost_op_m2_init >0) %>% # exclude cases without operational costs (no heating)
  mutate(cost_op_hh = cost_op_m2 * floor_cap * hh_size) %>% # operational costs per household
  ljoin(cost_intang_ren_shell_i) %>%
  ljoin(cost_intang_ren_heat_i %>% rename(fuel_heat = fuel_heat_i)) %>%
  mutate(cost_intang_hh = cost_intang_ren_heat + (cost_intang_ren_shell * floor_cap * hh_size)) %>% # intangible costs per household
  ljoin(discount_ren) %>%
  ljoin(heterog_ren) %>%
  mutate(lcc_ren = fun_lcc(cost_invest_hh, cost_op_hh, cost_intang_hh, discount_ren, lifetime_ren)) %>%
  mutate(cost_op_hh_lcc = lcc_ren - cost_intang_hh - cost_invest_hh) %>% # operational part of the LCC (for reporting)
  mutate(lcc_ren_exp = fun_lcc_exp(lcc_ren, heterog_ren)) %>% # logit term
  rename(eneff_i = eneff) %>%
  rename(fuel_heat_i = fuel_heat) %>%
  filter(cost_intang_ren_shell != 99999) %>% # 99999 = option not available
  filter(cost_intang_ren_heat != 99999) %>%
  select(-c(ct_ren_fuel_heat,ct_fuel_excl_ren,ct_fuel_excl_i_reg,ct_fuel_excl_f_reg)) # mod_decision is kept for the stock dynamics

try(if(nrow(lcc_ren_hh)!=nrow(distinct(lcc_ren_hh))) stop("Error in renovation calculation! Duplicated records in lcc_ren_hh"))

## Market shares of all options (renovations + fuel switches + no change), weighted by tenure
# Cost columns are dropped so that the remaining columns identify the decision (case, tenure, option)
cols_cost <- c("hh_size", "floor_cap", "cost_invest_ren_shell", "cost_invest_ren_heat", "cost_invest_hh", "cost_op_m2", "cost_op_m2_init",
               "cost_op_hh", "cost_intang_ren_heat", "cost_intang_ren_shell", "cost_intang_hh", "discount_ren",
               "lifetime_ren", "heterog_ren", "lcc_ren", "cost_op_hh_lcc")
ms_i <- lcc_ren_hh %>% select(-all_of(cols_cost))

ms_i <- ms_i %>%
  group_by_at(setdiff(names(ms_i), c("eneff_f", "fuel_heat_f", "lcc_ren_exp"))) %>% # by initial state and tenure
  mutate(lcc_ren_exp_sum=sum(lcc_ren_exp)) %>%
  ungroup() %>%
  mutate(ms_unwgt = lcc_ren_exp/lcc_ren_exp_sum) %>% # market share by tenure
  select(-c(lcc_ren_exp, lcc_ren_exp_sum)) %>%
  fun_ms_weighted(weights = hh_tenure, w_col = "hh_tenure", w_dims = "tenr", rnd = rnd) %>%
  select(-bld_age)

## Renovation rate: share of buildings not keeping their eneff, per year, within bounds
rate_ren_i <- ms_i %>%
  filter(eneff_i == eneff_f) %>%
  mutate(rate_ren_calc = (1-ms)/stp) %>%
  ljoin(rate_ren_low) %>%
  ljoin(rate_ren_high) %>%
  mutate(rate_ren = ifelse(rate_ren_calc < rate_ren_low, rate_ren_low,
                           ifelse(rate_ren_calc > rate_ren_high, rate_ren_high, rate_ren_calc))) %>%
  rename(eneff = eneff_i, fuel_heat = fuel_heat_i) %>%
  select(-c(eneff_f, fuel_heat_f, ms,
            rate_ren_low, rate_ren_high,
            rate_ren_calc))

## Market shares among renovation options only (normalised to 1)
ms_ren_i <- ms_i %>%
  filter(eneff_i != eneff_f) %>%
  group_by_at(setdiff(names(ms_i), c("eneff_f", "fuel_heat_f", "ms"))) %>%
  mutate(ms_tot= sum(ms)) %>%
  ungroup() %>%
  mutate(ms_ren = ifelse(ms_tot>0,round(ms/ms_tot, rnd),0)) %>%
  filter(ms_ren>0) %>%
  select(-c(ms, ms_tot))

## Market shares among fuel switches without renovation only (normalised to 1)
ms_sw_i <- ms_i %>%
  filter(eneff_i == eneff_f & fuel_heat_i != fuel_heat_f) %>%
  group_by_at(setdiff(names(ms_i), c("eneff_f", "fuel_heat_f", "ms"))) %>%
  mutate(ms_tot= sum(ms)) %>%
  ungroup() %>%
  mutate(ms = ifelse(ms_tot>0,round(ms/ms_tot, rnd),0)) %>%
  filter(ms>0) %>%
  select(-c(ms_tot, eneff_f)) %>%
  rename(eneff = eneff_i) %>%
  rename(fuel_heat = fuel_heat_i)

output = list(ms_ren_i = ms_ren_i,
              ms_sw_i = ms_sw_i,
              rate_ren_i = rate_ren_i)

} # END FUNCTION


## Exogenous renovation decisions: fixed market shares of final eneff and heating fuel
fun_ms_ren_target <- function(yrs,i,
                              bld_cases_fuel, ct_bld_age,
                              shr_eneff_ren,shr_fuel_heat_ren
                              ){

  print(paste0("Running renovation target - year ", yrs[i]))

  ms_ren_i <- bld_cases_fuel %>%
    mutate(year = yrs[i]) %>%
    rename(eneff_i = eneff) %>%
    rename(fuel_heat_i = fuel_heat) %>%
    ljoin(shr_eneff_ren) %>%
    ljoin(shr_fuel_heat_ren) %>%
    mutate(ms_ren = shr_eneff_ren*shr_fuel_heat_ren) %>%
    filter(!is.na(ms_ren)) %>%
    filter(ms_ren > 0) %>%
    filter(mod_decision == 1) %>%
    mutate(ct_fuel_excl_ren = 0) %>%
    select(-c(bld_age,shr_eneff_ren,shr_fuel_heat_ren,
              ct_fuel_excl_ren
              ))

  print(paste0("Completed renovation target - year ", yrs[i]))

  output = ms_ren_i

}


## Exogenous fuel switches without renovation: fixed market shares, existing buildings of past construction periods
fun_ms_fuel_sw <- function(yrs,i,
                           bld_cases_fuel, ct_bld_age,
                           shr_fuel_heat_sw
){
  print(paste0("Running fuel switch target - year ", yrs[i]))

  # Periods of construction already completed
  p_past <- ct_bld_age %>% filter(year_f < yrs[i]) %>% pull(bld_age_id)

  ms_sw_i <- bld_cases_fuel %>%
    mutate(year = yrs[i]) %>%
    filter(bld_age %in% p_past) %>%
    ljoin(shr_fuel_heat_sw) %>%
    filter(!is.na(shr_fuel_heat_sw)) %>%
    filter(shr_fuel_heat_sw > 0) %>%
    filter(mod_decision == 1) %>%
    rename(ms=shr_fuel_heat_sw) %>%
    select(-bld_age)

  print(paste0("Completed fuel switch target - year ", yrs[i]))

  output = ms_sw_i

}
