

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
  
  energy_use_app <- stock_app_all_years %>%
    
    group_by(region, urt, type, energy_label, year, yrs_prod) %>% # this needs to be skipped for the future version
    summarise(n_app = sum(n_app)) %>% # this needs to be skipped for the future version
    
    filter(year %in% yrs) %>%
    left_join(energy_consumption_per_label) %>%
  
    # left_join(energy_intensity_app_filtered[, c('region', 'type', 'capacity', 'energy_label', 'year', 'energy_intensity')] %>%
    #             mutate(year = as.numeric(year)),
    #           by = c("region", "type", "capacity", "energy_label", "year")) %>%
    
    mutate(energy_use =
             round(n_app * energy_intensity_per_year, rnd)) %>%
    mutate(unit = "GJ")
  
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

