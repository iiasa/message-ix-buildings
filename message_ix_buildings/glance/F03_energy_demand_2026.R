

library(tidyverse)
library(readxl)


#### function 3: calculating future energy demand of appliances ####

fun_energy_app <- function(stock_app_all_years,
                           # energy_intensity_app,
                           energy_consumption_per_label,
                           yrs,
                           regions){
  
  # Edit the dataframe of energy intensities
  # energy_intensity_app_filtered <- energy_intensity_app
  # energy_intensity_app_filtered <- energy_intensity_app %>%
    # filter(scenario == SSP) %>% # for now, SSPs not considered
    # select(-scenario)

  
  #### Calculate the energy use of appliances ####
  # energy use = appliances number * energy intensity
  # energy_use_app <- stock_app_all_years %>%
  #   
  #   group_by(region, urt, type, capacity, energy_label, year) %>% # this needs to be skipped for the future version
  #   summarise(n_app = sum(n_app)) %>% # this needs to be skipped for the future version
  #   
  #   filter(year %in% yrs) %>%
  #   left_join(energy_intensity_app_filtered[, c('region', 'type', 'capacity', 'energy_label', 'year', 'energy_intensity')] %>%
  #             mutate(year = as.numeric(year)),
  #             by = c("region", "type", "capacity", "energy_label", "year")) %>%
  #   
  #   mutate(energy_use =
  #            round(n_app * energy_intensity, rnd)) %>%
  #   mutate(unit = "GJ")
  
  # For AC, final energy intensities are determined by:
  # 1) useful energy consumption; and
  # 2) energy efficiency of AC stocks (energy labels + cohort-based EER).
  # The 1st can be pre-processed (e.g., based on CHILLED useful intensities + STURM floor areas).
  # The second can only be calculated during the modelling process, after detailed stocks are obtained.
  
  # check whether AIR_COND exists in the data
  has_ac <- "AIR_COND" %in% stock_app_all_years$type
  
  if (!has_ac) {
    
    # ---------------------------------------------------
    # Case 1: AIR_COND is NOT present
    # → simple energy = stock × intensity
    # ---------------------------------------------------
    
    energy_use_app <- stock_app_all_years %>%
      
      # aggregate stocks (to be skipped in future versions)
      group_by(region, urt, type, energy_label, year, yrs_prod) %>%
      summarise(n_app = sum(n_app), .groups = "drop") %>%
      
      filter(year %in% yrs) %>%
      
      # join energy intensity
      left_join(energy_consumption_per_label) %>%
      
      # calculate final energy
      mutate(
        energy_use = round(n_app * energy_intensity_per_year, rnd),
        unit = "GJ"
      )
    
  } else {
    
    # ---------------------------------------------------
    # Case 2: AIR_COND is present
    # → split into non-AC appliances and AC
    # ---------------------------------------------------
    
    ## --------------------------------
    ## Other appliances (non-AC)
    ## --------------------------------
    energy_use_app_others <- stock_app_all_years %>%
      
      filter(type != "AIR_COND") %>%
      
      # aggregate stocks (to be skipped in future versions)
      group_by(region, urt, type, energy_label, year, yrs_prod) %>%
      summarise(n_app = sum(n_app), .groups = "drop") %>%
      
      filter(year %in% yrs) %>%
      
      # join energy intensity
      left_join(energy_consumption_per_label) %>%
      
      # calculate final energy
      mutate(
        energy_use = round(n_app * energy_intensity_per_year, rnd),
        unit = "GJ"
      )
    
    ## --------------------------------
    ## Air conditioning (AC)
    ## --------------------------------
    energy_use_AC <- stock_app_all_years %>%
      
      filter(type == "AIR_COND") %>%
      
      # aggregate stocks (to be skipped in future versions)
      group_by(region, urt, type, energy_label, year, yrs_prod) %>%
      summarise(n_app = sum(n_app), .groups = "drop") %>%
      
      filter(year %in% yrs) %>%
      
      ## join EER (efficiency by label / cohort)
      left_join(eer_AC) %>%
      
      ## join useful cooling energy
      ## NOTE: this table is NOT urt-specific
      left_join(energy_demand_useful_AC) %>%
      
      ## big assumption:
      ## distribute regional useful cooling energy across
      ## energy labels and production cohorts based on n_app
      ## (allocation is done at region + year + fuel level
      ##  to avoid double counting across urt)
      group_by(region, year, fuel) %>%
      mutate(
        cool_use_Kwh_alloc = {
          tot <- first(cool_use_Kwh)      # repeated regional total
          den <- sum(n_app, na.rm = TRUE) # total AC stock
          if (is.na(tot) || den <= 0) NA_real_ else tot * n_app / den
        }
      ) %>%
      ungroup() %>%
      
      ## calculate final energy
      ## kWh → GJ : divide by SEER, multiply by 3.6, divide by 1000
      mutate(
        energy_use = (cool_use_Kwh_alloc / SEER) * 3.6 / 1000,
        unit = "GJ"
      ) %>%
      
      select(
        region, urt, type, energy_label, year, yrs_prod,
        n_app, fuel, energy_use, unit
      )
    
    ## --------------------------------
    ## Combine AC and other appliances
    ## --------------------------------
    energy_use_app <- bind_rows(
      energy_use_AC,
      energy_use_app_others
    )
  }
  
  
    
  
  
  #### aggregating ####
  energy_use_app_aggr <- energy_use_app %>%
    group_by(region, year, type, fuel) %>%
    summarise(energy_use = sum(energy_use)) %>%
    mutate(unit = "GJ")
  
  energy_use_app_aggr_R61 <- energy_use_app_aggr %>%
    rename(R61 = region)
  
  energy_use_app_aggr_EU31 <- energy_use_app_aggr_R61 %>%
    left_join(regions) %>%
    filter(grepl("C-EEU|C-WEU", R61)) %>%
    group_by(R61, year, type, fuel) %>%
    summarise(energy_use = sum(energy_use)) %>%
    rename(EU31 = R61) %>%
    mutate(unit = "GJ")
  
  ## test
  energy_use_app_aggr_EU31_test <- energy_use_app_aggr_EU31 %>%
    filter(year == 2020, type == "WASH_MACH") %>%
    group_by(unit) %>%
    summarise(energy_use = sum(energy_use))
  
  print(paste("Total energy use for washing machines in 2020 is", energy_use_app_aggr_EU31_test$energy_use, "GJ"))
  
  
  energy_use_app_aggr_R12 <- energy_use_app_aggr_R61 %>%
    left_join(regions) %>%
    group_by(R12, year, type, fuel) %>%
    summarise(energy_use = sum(energy_use)) %>%
    mutate(unit = "GJ")
  
  energy_use_app_aggr_global <- energy_use_app_aggr_R12 %>%
    group_by(year, type, fuel) %>%
    summarise(energy_use = sum(energy_use)) %>%
    mutate(unit = "GJ")
  
  output <- list(
    energy_use_app = energy_use_app,
    energy_use_app_aggr_R61 = energy_use_app_aggr_R61,
    energy_use_app_aggr_EU31 = energy_use_app_aggr_EU31,
    energy_use_app_aggr_R12 = energy_use_app_aggr_R12,
    energy_use_app_aggr_global = energy_use_app_aggr_global
  )
  
  
  return(output)
  
}

