## ENERGY DEMAND
## this calculation is to be put within a loop on "i" (time steps)

u1 <- 3.6 / 1000 # kWh to GJ (to calculate operational costs)


#' @title Energy demand - STURM
#' @description Calculate energy demand for heating, cooling, hot water, fans and other uses
#' @param sector Sector to be calculated
#' @param yrs Years to be calculated
#' @param i Time step
#' @param bld_cases_fuel Building cases and fuel
#' @param en_int_heat Energy intensity for heating
#' @param en_int_cool Energy intensity for cooling
#' @param days_cool Days of cooling
#' @param eff_cool Cooling efficiency
#' @param eff_heat Heating efficiency
#' @param en_sav_ren Energy savings due to renovation
#' @param hours_heat Hours of heating
#' @param shr_floor_heat Share of floor area for heating
#' @param hours_cool Hours of cooling
#' @param shr_floor_cool Share of floor area for cooling
#' @param hours_fans Hours of fans
#' @param power_fans Power of fans
#' @param shr_acc_cool Share of cooling for air conditioning
#' @param hh_size Household size
#' @param floor_cap Floor capacity
#' @param price_en Energy price
#' @param shr_acc_heat Share of heating for air conditioning
#' @return Energy demand
fun_en_sim <- function(sector,
                       yrs,
                       i,
                       bld_cases_fuel,
                       en_int_heat,
                       en_int_cool,
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
                       hh_size,
                       floor_cap,
                       price_en,
                       shr_acc_heat = 1) {
  print(paste0("Running energy demand year ", yrs[i]))


  en_m2_scen_det <- bld_cases_fuel %>%
    mutate(year = yrs[i]) %>%
    # left_join(ct_eneff) %>% # to have "mat" for joining ct_access
    left_join(eff_cool) %>%
    left_join(eff_heat) %>%
    left_join(en_sav_ren) %>%
    mutate_cond(is.na(en_sav_ren), en_sav_ren = 0) %>%
    # mutate(en_sav_ren=0) %>% ## NO RENOVATION SCALING HERE - VALUES ARE NOT TO BE SCALED
    left_join(hours_heat) %>%
    mutate(f_h = hours_heat / 24) %>%
    left_join(shr_floor_cool) %>%
    left_join(hours_cool) %>%
    left_join(hours_fans) %>%
    left_join(power_fans) %>%
    mutate(f_c = hours_cool / 24) %>%
    mutate(f_f = hours_fans / 24) %>% # fans operation
    left_join(shr_floor_heat) %>%
    left_join(shr_acc_cool) %>%
    # left_join(cool_data_base) %>%
    left_join(en_int_heat) %>%
    left_join(en_int_cool) %>%
    left_join(days_cool) %>%
    # left_join(en_m2_sim) %>%
    mutate(en_dem_heat = en_int_heat * (1 - en_sav_ren) * shr_acc_heat / eff_heat * shr_floor_heat * f_h) %>% # Edit heating demand (final) [kWh/m2/y] # Removed acc_heat
    mutate(en_dem_c_ac = en_int_cool * (1 - en_sav_ren) * shr_acc_cool / eff_cool * shr_floor_cool * f_c) %>% # Edit cooling demand - AC (final) [kWh/m2/y] (fans not included for now)
    mutate(en_dem_c_fans = days_cool * shr_floor_cool * f_f * 24 * power_fans / (25 * 1e3)) %>% # Cooling demand - FANS (final) [kWh/m2/y]
    mutate_cond(is.na(en_dem_heat), en_dem_heat = 0) %>%
    mutate_cond(is.na(en_dem_c_ac), en_dem_c_ac = 0) %>% # Remove NA values
    mutate_cond(is.na(en_dem_c_fans), en_dem_c_fans = 0) %>% # Remove NA values
    mutate(en_dem_cool = en_dem_c_ac + en_dem_c_fans) %>% # Total energy demand for cooling
    # mutate_cond(eneff %in% c("s6_low", "s9_rlow") & en_dem_heat/eff_heat >20, en_dem_heat = 20/eff_heat) %>% # ADJUST VALUES FOR  PASSIVE (on useful energy - 20 kWh/m2 with 5kwh/m2 allowance for climate)
    # mutate_cond(eneff %in% c("s6_low", "s9_rlow") & en_dem_cool/eff_cool >20, en_dem_cool = 20/eff_cool) %>% # ADJUST VALUES FOR  PASSIVE (on useful energy - 20 kWh/m2 with 5kwh/m2 allowance for climate)
    # # mutate_cond(eneff %in% c("s6_low", "s9_rlow") & en_dem_heat>15, en_dem_heat = 15) %>% # ADJUST VALUES FOR  PASSIVE
    # # mutate_cond(eneff %in% c("s6_low", "s9_rlow") & en_dem_cool>15, en_dem_cool = 15) %>% # ADJUST VALUES FOR  PASSIVE
    select(-year) %>%
    select(-c(en_int_heat, en_int_cool, days_cool))

  en_m2_scen_S <- en_m2_scen_det %>%
    select(-c(
      bld_age, eff_cool, eff_heat, mod_decision, f_h, f_c, shr_floor_heat, shr_floor_cool, hours_heat, hours_cool,
      # eneff_fuel,
      en_sav_ren
    )) %>%
    select(-c(hours_fans, power_fans, f_f)) # %>% NAs removed before

  ## Energy demand SPACE HEATING ONLY - by fuel: for investment decisions
  en_m2_scen_heat <- bld_cases_fuel %>%
    # mutate(fuel = fuel_heat) %>%
    left_join(en_m2_scen_S) %>%
    select(-c(
      en_dem_cool,
      en_dem_c_ac,
      en_dem_c_fans,
      bld_age,
      shr_acc_cool,
      fuel_cool,
      # eneff_fuel,
      mod_decision
    ))

  en_m2_scen_cool <- bld_cases_fuel %>%
    left_join(en_m2_scen_S) %>%
    select(-c(
      fuel_heat,
      en_dem_heat, bld_age,
      # eneff_fuel,
      mod_decision
    )) %>%
    distinct()


  if (sector == "resid") { # Calculate household energy demand - for cost calculations - residential only

    # Add HH size - HEATING ONLY
    en_hh <- en_m2_scen_heat %>%
      rename(en_m2 = en_dem_heat) %>%
      mutate(fuel = fuel_heat) %>%
      left_join(hh_size) %>%
      filter(year == yrs[i]) # Filter "i" year
    # filter(year >= yr_base & year <= yr_end) #Limit to the period yr_bas to yr_end

    # en_hh <- en_hh %>% left_join(ct_eneff) #Add mat categories
    # en_hh <- en_hh %>% left_join(ct_bld) #Add arch categories
    en_hh <- en_hh %>% left_join(floor_cap) %>% # Add floor surface
      mutate(en_hh = en_m2 * floor_cap * hh_size) %>% # Calculate total energy demand per household
      left_join(price_en) %>% # Associate energy prices to en_perm
      mutate(cost_op = u1 * en_hh * price_en) # calculate the total costs for operational energy

    ## Sum total energy costs for all fuels
    en_hh_tot <- en_hh %>% # Initialize
      select(-c(en_m2, hh_size, floor_cap, en_hh, price_en))
    en_hh_tot <- en_hh_tot %>%
      group_by_at(setdiff(names(en_hh_tot), c("fuel", "cost_op"))) %>% # Select all variables, except "fuel" and  for grouping)
      summarise(cost_op_m2 = sum(cost_op)) %>%
      ungroup()

    # Reorder rows
    en_hh_tot <- bld_cases_fuel %>% left_join(en_hh_tot) # %>% select(-acc_cool)

  } else { # commercial (no cost calculations)
    en_hh_tot <- NULL
    # en_hh_tot_ren_init <- NULL
    # en_hh_tot_ren_fin <- NULL
  }

  output <- list(
    en_m2_scen_heat = en_m2_scen_heat,
    en_m2_scen_cool = en_m2_scen_cool,
    en_hh_tot = en_hh_tot
  )
} 


#' @title Function to calculate energy demand for domestic hot water
#' @param yrs vector of years to be calculated
#' @param i index of the year to be calculated
#' @param bld_cases_fuel data frame with building cases and fuel
#' @param hh_size data frame with household size
#' @param eff_hotwater data frame with efficiency of hot water systems
#' @param en_int_hotwater data frame with internal gains from hot water
#' @param en_int_heat data frame with internal gains from heating
#' @return list with data frames for energy demand for space heating and cooling
fun_hw_resid <- function(yrs, i,
                         bld_cases_fuel,
                         hh_size,
                         # ct_fuel_dhw,
                         eff_hotwater, en_int_hotwater,
                         en_int_heat) {
  print(paste0("Running energy demand year ", yrs[i]))

  acc_hw <- 1 # Access to DHW

  en_hh_hw_scen <- bld_cases_fuel %>%
    mutate(year = yrs[i]) %>%
    # left_join(ct_eneff) %>% # to have "mat" for joining ct_access ## needed for DHW?
    left_join(hh_size) %>%
    # left_join(ct_fuel_dhw) %>%
    left_join(eff_hotwater) %>%
    left_join(en_int_hotwater) %>%
    left_join(en_int_heat) %>% # join data energy demand - filter where heating is needed
    mutate(en_dem_dhw = hh_size * en_int_hotwater * acc_hw / eff_hotwater) %>% # DHW energy demand (final) [GJ/hh/y]
    mutate_cond(en_int_heat == 0, en_dem_dhw = 0) %>% # no hot water demand where there is no heating
    select(-year) %>%
    select(-c(hh_size, eff_hotwater, en_int_hotwater, en_int_heat, mod_decision, bld_age))

  output <- en_hh_hw_scen
}

## FUNCTION HOT WATER - COMMERCIAL

fun_hw_comm <- function(yrs, i,
                        bld_cases_fuel,
                        eff_hotwater,
                        en_m2_dhw) {
  print(paste0("Running energy demand - Hot water - year ", yrs[i]))

  en_m2_hw_scen <- bld_cases_fuel %>%
    mutate(year = yrs[i]) %>%
    # left_join(ct_eneff) %>% # to have "mat" for joining ct_access ## needed for DHW?
    left_join(eff_hotwater) %>%
    left_join(en_m2_dhw) %>%
    mutate(en_dem_dhw = en_dhw_useful_kwh_m2 / eff_hotwater) %>% # DHW energy demand (final) [kWh/m2/y]
    select(-year) %>%
    select(-c(eff_hotwater, en_dhw_useful_kwh_m2, mod_decision, bld_age))

  output <- en_m2_hw_scen
}
