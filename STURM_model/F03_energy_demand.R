## ENERGY DEMAND
## this calculation is to be put within a loop on "i" (time steps)

library(dplyr)


u1 <- 3.6 / 1000 # kWh to GJ (to calculate operational costs)

f_nu <- 0.9
q_gains <- 0

b_floor <- 0.5 # adjusment factor for floor
u_th_br <- 0.1 # W/m2.K, thermal bridge

# ventilation parameters
n_air_infiltr <- 0.2 # 1/h, air infiltration
n_air_use <- 0.4 # 1/h, air infiltration
h_room <- 2.5 # m, height of room
c_air <- 0.34 # Wh/(m3.K)

# solar gains parameters
f_sh <- 0.6 # shading factor
f_f <-  0.3 # frame are fraction of the windows
f_w <- 0.9 # reduction factor for non- perpendicular to the glas
g_gl <- 0.6 # total solar energy transmittance

# internal heat gains parameters
phi_int <- 3 # W/m2, internal heat gains

gain_utilitization_factor <- 0.95

# inertia parameters
a_h0 <- 0.8 # constant parameter
tau_h0 <- 30
c_m <- 45 # Wh/(m2.K), internal heat capacity



#' @title Function to calculate energy demand for space heating
#' @description Calculate energy demand for space heating
#' @param bld_cases_fuel Building cases and fuel
#' @param u_wall U-value of walls, in W/m2K
#' @param u_roof U-value of roof, in W/m2K
#' @param u_floor U-value of floor, in W/m2K
#' @param u_windows U-value of windows, in W/m2K
#' @param area_wall Area of walls, in % of floor area
#' @param area_roof Area of roof, in % of floor area
#' @param area_floor Area of floor, in % of floor area
#' @param area_windows Area of windows, in % of floor area
#' @param hdd Heating degree days, in degree days per year
#' @param d_hs Heating days, in days per year
#' @param i_sol Solar irradiation during heating season, in kWh/m2.a
fun_space_heating_calculation <- function(bld_cases_fuel,
                                      u_wall,
                                      u_roof,
                                      u_floor,
                                      u_windows,
                                      area_wall,
                                      area_roof,
                                      area_floor,
                                      area_windows,
                                      hdd,
                                      en_sav_ren,
                                      d_hs = 200,
                                      i_sol = 200,
                                      simplified = TRUE) {

  attributes <- c("clim", "region_bld", "arch", "bld_age", "eneff")
  if (!simplified) {
    h_ve <- c_air * (n_air_infiltr + n_air_use) * h_room
  } else {
    h_ve <- 0
    u_th_br <- 0
    i_sol <- 0
    d_hs <- 0
  }

  h_tr <- bld_cases_fuel %>%
    select(all_of(attributes)) %>%
    distinct() %>%
    left_join(u_wall) %>%
    left_join(u_roof) %>%
    left_join(u_floor) %>%
    left_join(u_windows) %>%
    left_join(area_wall) %>%
    left_join(area_roof) %>%
    left_join(area_floor) %>%
    left_join(area_windows) %>%
    mutate(h_tr =
      (u_wall * area_wall + u_roof * area_roof +
        b_floor * u_floor * area_floor + u_windows * area_windows)) %>%
    mutate(h_tr = h_tr +
      u_th_br * (area_wall + area_roof + area_floor + area_windows)) %>%
    group_by(region_bld) %>%
    mutate(min_h_tr = min(h_tr)) %>%
    ungroup() %>%
    left_join(en_sav_ren) %>%
    mutate_cond(is.na(en_sav_ren), en_sav_ren = 0) %>%
    mutate(en_sav_ren = ifelse(bld_age == "p5", 0, en_sav_ren)) %>%
    mutate(h_tr = h_tr * (1 - en_sav_ren)) %>%
    mutate(h_tr = ifelse(h_tr < min_h_tr, min_h_tr, h_tr)) %>%
    mutate(h_tr = ifelse((bld_age == "p5") & (eneff == "adv"),
      min(h_tr), h_tr)) %>%
    mutate(u_building = h_tr /
      (area_wall + area_roof + area_floor + area_windows)) %>%
    mutate(u_building = ifelse(u_building < 0.1, 0.1, u_building))

  q_gains <- area_windows %>%
    mutate(q_sol = f_sh * (1 - f_f) * f_w * g_gl * i_sol) %>%
    mutate(q_int = 24 / 1000 * phi_int * d_hs) %>%
    mutate(q_gains = q_int + q_sol)

  en_int_heat <- h_tr %>%
    left_join(hdd) %>%
    left_join(q_gains) %>%
    mutate(q_losses = 24 / 1000 * f_nu * hdd * (h_tr + h_ve)) %>%
    mutate(en_int_heat = q_losses - gain_utilitization_factor * q_gains) %>%
    select(c(all_of(attributes), "en_int_heat", "u_building"))

  return(en_int_heat)
  }



#' @title Energy demand - STURM
#' @description Calculate energy demand for heating, cooling, hot water,
#'  fans and other uses
#' @param sector Sector to be calculated
#' @param yrs Years to be calculated
#' @param i Time step
#' @param bld_cases_fuel Building cases and fuel
#' @param en_int_heat Energy intensity for heating
#' @param en_int_cool Energy intensity for cooling
#' @param days_cool Days of cooling
#' @param eff_cool Cooling efficiency
#' @param eff_heat Heating efficiency
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
                       price_en_wt,
                       income,
                       pe_conversion_factor,
                       bill_rebates = NULL,
                       shr_acc_heat = 1,
                       nzeb = FALSE,
                       en_method = "TABULA",
                       path_out = NULL,
                       alpha = NULL,
                       short_term_price_elasticity = -0.2,
                       factor_energy_behavior = 1) {
  if (en_method == "TABULA") {
    # If using TABULA data, do not calibrated parameters used with CHILLED
    hours_heat <- mutate(hours_heat,
      hours_heat = ifelse(hours_heat > 0, 24, 0))
    shr_floor_heat <- mutate(shr_floor_heat,
      shr_floor_heat = ifelse(shr_floor_heat > 0, 1, 0))
  }



  energy_det <- bld_cases_fuel %>%
    mutate(year = yrs[i]) %>%
    left_join(eff_cool) %>%
    left_join(eff_heat) %>%
    left_join(hours_heat) %>%
    mutate(f_h = hours_heat / 24) %>%
    left_join(shr_floor_cool) %>%
    left_join(hours_cool) %>%
    left_join(hours_fans) %>%
    left_join(power_fans) %>%
    mutate(f_c = hours_cool / 24) %>%
    mutate(f_f = hours_fans / 24) %>%
    left_join(shr_floor_heat) %>%
    left_join(shr_acc_cool) %>%
    left_join(en_int_heat) %>%
    left_join(en_int_cool) %>%
    left_join(days_cool) %>%
    # Edit heating demand (final) [kWh/m2/y]
    mutate(en_dem_heat = en_int_heat * shr_acc_heat /
      eff_heat * shr_floor_heat * f_h) %>%
    # Edit cooling demand - AC (final) [kWh/m2/y] (fans not included for now)
    mutate(en_dem_c_ac = en_int_cool * shr_acc_cool /
      eff_cool * shr_floor_cool * f_c) %>%
    # Cooling demand - FANS (final) [kWh/m2/y]
    mutate(en_dem_c_fans =
      days_cool * shr_floor_cool * f_f * 24 * power_fans / (25 * 1e3)) %>%
    mutate_cond(is.na(en_dem_heat), en_dem_heat = 0) %>%
    mutate_cond(is.na(en_dem_c_ac), en_dem_c_ac = 0) %>%
    mutate_cond(is.na(en_dem_c_fans), en_dem_c_fans = 0) %>%
    mutate(en_dem_cool = en_dem_c_ac + en_dem_c_fans) %>%
    select(-c(en_int_heat, en_int_cool, days_cool))

  # Formatting energy demand
  energy_det_subset <- energy_det %>%
    select(-c(
      eff_cool, eff_heat, f_h, f_c,
      shr_floor_heat, shr_floor_cool, hours_heat, hours_cool,
      hours_fans, power_fans, f_f))

  # Formatting energy demand for space heating
  en_m2_scen_heat <- bld_cases_fuel %>%
    left_join(energy_det_subset) %>%
    select(-c("en_dem_cool", "en_dem_c_ac", "en_dem_c_fans",
      "shr_acc_cool", "fuel_cool"))

  # Formatting energy demand for space cooling
  en_m2_scen_cool <- bld_cases_fuel %>%
    left_join(energy_det_subset) %>%
    select(-c("fuel_heat", "en_dem_heat")) %>%
    distinct()

  en_hh <- NULL
  if (sector == "resid") {

    if (is.null(bill_rebates)) {
      bill_rebates <- data.frame(
        region_bld = unique(bld_cases_fuel$region_bld),
        bill_rebates = 0
      )
    }

    if (is.null(alpha)) {
      # put one value for alpha for all countries
      alpha <- bld_cases_fuel %>%
        select("region_bld") %>%
        distinct() %>%
        mutate(coeff_alpha = 1)
  }

    # Calculating household energy cost (residential only)
    en_hh <- en_m2_scen_heat %>%
      rename(en_m2_std = en_dem_heat) %>%
      left_join(pe_conversion_factor) %>%
      mutate(pe_conversion_factor = ifelse(is.na(pe_conversion_factor),
        1, pe_conversion_factor)) %>%
      mutate(en_pe_hh_std = en_m2_std * pe_conversion_factor) %>%
      left_join(hh_size) %>%
      left_join(floor_cap) %>%
      mutate(m2 = hh_size * floor_cap) %>%
      mutate(en_hh_std = en_m2_std * m2) %>%
      left_join(price_en) %>%
      mutate(cost_op_std = en_hh_std * price_en) %>%
      left_join(bill_rebates) %>%
      mutate(bill_rebates = ifelse(is.na(bill_rebates), 0, bill_rebates)) %>%
      mutate(bill_rebates = ifelse(bill_rebates > 0.9 * cost_op_std,
        0.9 * cost_op_std, bill_rebates)) %>%
      mutate(cost_op_std = cost_op_std - bill_rebates) %>%
      left_join(income) %>%
      mutate(budget_share = cost_op_std / income) %>%
      # mutate(heating_intensity = alpha * (budget_share)**p_elasticity) %>%
      left_join(alpha) %>%
      mutate(heating_intensity = coeff_alpha * cost_op_std**short_term_price_elasticity) %>%
      mutate(heating_intensity = factor_energy_behavior * heating_intensity) %>%
      # Calculating heating intensity (household heating behavior)
      mutate(en_hh = en_hh_std * heating_intensity) %>%
      mutate(en_m2 = en_hh / m2) %>%
      mutate(cost_op = en_hh * price_en - bill_rebates) %>%
      left_join(price_en_wt) %>%
      mutate(cost_op_wt = en_hh * price_en_wt) %>%
      select(-c(hh_size, floor_cap, price_en, fuel, income,
        cost_op_std, bill_rebates, pe_conversion_factor, m2, coeff_alpha,
        price_en_wt)) %>%
      left_join(bld_cases_fuel)
  }
  # print(head(filter(en_hh, region_bld == "C-WEU-FRA")))
  # print(filter(price_en, region_bld == "C-WEU-FRA"))

  # Calculating household energy cost (residential only)
  if (!is.null(path_out)) {
    print('Extracting energy demand and cost at household level')
    temp <- en_hh %>%
      select("region_bld", "clim", "urt",
        "arch", "bld_age", "eneff", "fuel_heat",
        "tenr", "inc_cl", "en_m2_std", "en_m2", "en_hh_std", "en_hh",
        "budget_share", "heating_intensity", "cost_op")
    # write.csv(temp, paste0(path_out, "/detail_energy.csv"))
  }

  en_hh <- en_hh %>%
    select(-c(en_m2_std, en_m2))

  output <- list(
    en_m2_scen_heat = en_m2_scen_heat,
    en_m2_scen_cool = en_m2_scen_cool,
    en_hh_tot = en_hh
  )
  return(output)
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
                         eff_hotwater,
                         en_int_hotwater,
                         en_int_heat) {
  acc_hw <- 1 # Access to DHW

  en_hh_hw_scen <- bld_cases_fuel %>%
    mutate(year = yrs[i]) %>%
    left_join(hh_size) %>%
    # left_join(ct_fuel_dhw) %>%
    left_join(eff_hotwater) %>%
    left_join(en_int_hotwater) %>%
    # join data energy demand - filter where heating is needed
    left_join(en_int_heat) %>%
    # DHW energy demand (final) [GJ/hh/y]
    mutate(en_dem_dhw =
      hh_size * en_int_hotwater * acc_hw / eff_hotwater) %>%
    # no hot water demand where there is no heating
    mutate_cond(en_int_heat == 0, en_dem_dhw = 0) %>%
    select(-year) %>%
    select(-c(hh_size, eff_hotwater, en_int_hotwater, en_int_heat))

  output <- en_hh_hw_scen
}

#' @title Function to calculate energy demand for domestic hot water
#' for commercial buildings
#' @param yrs vector of years to be calculated
#' @param i index of the year to be calculated
#' @param bld_cases_fuel data frame with building cases and fuel
#' @param eff_hotwater data frame with efficiency of hot water systems
#' @param en_m2_dhw data frame with energy demand for domestic hot water
#' @return data frame with energy demand for domestic hot water
fun_hw_comm <- function(yrs,
                        i,
                        bld_cases_fuel,
                        eff_hotwater,
                        en_m2_dhw) {
  print(paste0("Running energy demand - Hot water - year ", yrs[i]))

  en_m2_hw_scen <- bld_cases_fuel %>%
    mutate(year = yrs[i]) %>%
    # left_join(ct_eneff) %>% # to have "mat" for joining ct_access ## needed for DHW?
    left_join(eff_hotwater) %>%
    left_join(en_m2_dhw) %>%
    # DHW energy demand (final) [kWh/m2/y]
    mutate(en_dem_dhw = en_dhw_useful_kwh_m2 / eff_hotwater) %>%
    select(-year) %>%
    select(-c(eff_hotwater, en_dhw_useful_kwh_m2))

  output <- en_m2_hw_scen
}
