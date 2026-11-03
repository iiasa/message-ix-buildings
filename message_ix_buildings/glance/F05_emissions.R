

library(tidyverse)
library(readxl)


#### function 5: calculating emissions of energy use and material production of appliances ####
fun_emission_app <- function(energy_use_app_aggr_R61,
                             material_app_aggr_R61,
                             emission_factor_energy,
                             emission_factor_materials_primary,
                             emission_factor_materials_secondary,
                             emission_factor_materials_landfill,
                             emission_factor_materials_incineration,
                             emission_factor_materials_burning_open,
                             # climate_policy,
                             regions,
                             yrs){
  
  
  # Edit the dataframe of CO2 emission factors of electricity (kg CO2/GJ)
  emission_factor_energy_filtered <- emission_factor_energy %>%
    filter(year %in% yrs) %>%
    # filter(clim_policy == climate_policy) %>% # select emission factors in this climate policy scenario
    rename(R61 = region)
    # select(-c(clim_policy, fuel))
    # select(-c(fuel))
  
  # # Edit the dataframe of CO2 emission factors of materials (kg CO2/kg)
  emission_factor_materials_primary_filtered <- emission_factor_materials_primary %>%
    filter(year %in% yrs) %>%
    rename(R61 = region)

  emission_factor_materials_secondary_filtered <- emission_factor_materials_secondary %>%
    filter(year %in% yrs) %>%
    rename(R61 = region)
  
  # # Edit the dataframe of CO2 emission factors of materials (kg CO2/kg)
  # emission_factor_materials_primary_filtered <- emission_factor_materials_primary %>%
  #   filter(year %in% yrs) %>%
  #   rename(clim_policy = climate_policy) %>%  # select emission factors in this climate policy scenario
  #   mutate(clim_policy = ifelse(clim_policy == 'baseline', 'NPi', clim_policy)) %>%
  #   mutate(clim_policy = ifelse(clim_policy == 'rcp26', '1p5C', clim_policy)) %>%
  #   filter(clim_policy == climate_policy) %>%
  #   # filter(material_type %in% material_types) %>%
  #   rename(R61 = region) %>%
  #   select(-clim_policy)
  # 
  # emission_factor_materials_secondary_filtered <- emission_factor_materials_secondary %>%
  #   filter(year %in% yrs) %>%
  #   rename(clim_policy = climate_policy) %>%
  #   mutate(clim_policy = ifelse(clim_policy == 'baseline', 'NPi', clim_policy)) %>%
  #   mutate(clim_policy = ifelse(clim_policy == 'rcp26', '1p5C', clim_policy)) %>%
  #   filter(clim_policy == climate_policy) %>%  # select emission factors in this climate policy scenario
  #   # filter(material_type %in% material_types) %>%
  #   rename(R61 = region) %>%
  #   select(-clim_policy)

  # calculate CO2 emissions of energy use (metric tons CO2 = GJ  * kg CO₂ per GJ / 1000)
  co2_emission_energy_R61 <- energy_use_app_aggr_R61 %>%
    select(-unit) %>%
    left_join(emission_factor_energy_filtered, by = c('R61', 'year', 'fuel')) %>%
    # mutate(co2_emissions = energy_use * emission_factor * 277.78 / 1000000) %>%
    mutate(co2_emissions = energy_use * emission_factor / 1000) %>% # kg to tonne co2
    
    select(-c(fuel, energy_use, emission_factor)) %>%
    mutate(emission_source = 'operation_electricity') %>%
    mutate(unit = 'tonne CO2')

  # calculate CO2 emissions of materials production (tonne CO2 = tonne * kg CO2/kg)
  co2_emission_materials_R61 <- material_app_aggr_R61 %>%
    ungroup() %>%
    select(-c(material_stock, material_outflow, unit)) %>%
    left_join(emission_factor_materials_primary_filtered, by = c('R61', 'material_type', 'year')) %>%
    left_join(emission_factor_materials_secondary_filtered, by = c('R61', 'material_type', 'year')) %>%
    rename(emission_factor_primary = emission_factor.x,
           emission_factor_secondary = emission_factor.y) %>%
    mutate(
      co2_emissions_primary = coalesce(primary * emission_factor_primary, 0),
      co2_emissions_reuse = coalesce(reuse * 0, 0),
      co2_emissions_recycling = coalesce(recycling * emission_factor_secondary, 0),
      co2_emissions = co2_emissions_primary + co2_emissions_reuse + co2_emissions_recycling
    ) %>%
    group_by(R61, year, type, material_type) %>%
    summarise(co2_emissions = sum(co2_emissions),.groups = "drop") %>%
    # select(R61, year, type, material_type, co2_emissions) %>%
    # filter(material_type %in% c("steel", "copper", "aluminium", "glass", "plastics")) %>% # for now, only include 5 materials
    rename(emission_source = 'material_type') %>%
    mutate(emission_source = str_c('production_', emission_source)) %>%
    mutate(unit = 'metric tons CO2')
  
  # calculate CO2 emissions of materials end-of-life treatment (tonne CO2 = tonne * kg CO2/kg)
  co2_emission_eol_R61 <- material_app_aggr_R61 %>%
    ungroup() %>%
    select(-c(material_stock, material_outflow, unit)) %>%
    # recycling - already considered in productions
    # downcycling - no more in the system, assigned to the receiving product system
    # landfill
    # emission_factor_materials_landfill <- ghg_intensity_materials_landfill_baseline
    left_join(emission_factor_materials_landfill %>%
              rename(R61 = region, emission_factor_landfill = emission_factor) %>%
              select(-eol_material),
              by = c("R61", "material_type", "year")) %>%
    mutate(co2_emissions_landfill = disposal_managed * emission_factor_landfill) %>%
    # incineration
    # emission_factor_materials_incineration <- ghg_intensity_materials_incineration_baseline
    left_join(emission_factor_materials_incineration %>%
                rename(R61 = region, emission_factor_incineration = emission_factor) %>%
                select(-eol_material),
              by = c("R61", "material_type", "year")) %>%
    mutate(co2_emissions_incineration = (incineration_energy + incineration) * emission_factor_incineration) %>%
    # open burning
    # emission_factor_materials_burning_open <- ghg_intensity_materials_open_burning_baseline
    left_join(emission_factor_materials_burning_open %>%
                rename(R61 = region, emission_factor_burning_open = emission_factor) %>%
                select(-eol_material),
              by = c("R61", "material_type", "year")) %>%
    mutate(co2_emissions_burning_open = burning_open * emission_factor_burning_open) %>%
    mutate(co2_emissions = co2_emissions_landfill + co2_emissions_incineration + co2_emissions_burning_open) %>%
    group_by(R61, year, type, material_type) %>%
    summarise(co2_emissions = sum(co2_emissions),.groups = "drop") %>%
    # select(R61, year, type, material_type, co2_emissions) %>%
    rename(emission_source = 'material_type') %>%
    mutate(emission_source = str_c('eol_', emission_source)) %>%
    mutate(unit = 'metric tons CO2') %>%
    ## for now, aggregate the eol emissions for all materials
    group_by(R61, year, type) %>%
    summarise(co2_emissions = sum(co2_emissions, na.rm = TRUE),.groups = "drop") %>%
    mutate(emission_source = "eol",unit = "metric tons CO2") %>%
    select(R61, year, type, emission_source, co2_emissions, unit)


  # bind CO2 emissions from energy use, materials production, and EOL
  co2_emission_app_R61 <- bind_rows(co2_emission_energy_R61, co2_emission_materials_R61, co2_emission_eol_R61) %>%
    select(R61, year, type, emission_source, co2_emissions) %>%
    arrange(R61, year, type) %>%
    mutate(unit = 'metric tons CO2')

  co2_emission_app_EU31 <- co2_emission_app_R61 %>%
    left_join(regions, by = "R61") %>%
    filter(grepl("C-EEU|C-WEU", R61)) %>%
    group_by(R61, year, type, emission_source) %>%
    summarise(co2_emissions = sum(co2_emissions)) %>%
    rename(EU31 = R61) %>%
    mutate(unit = 'metric tons CO2')
  
  co2_emission_app_R12 <- co2_emission_app_R61 %>%
    left_join(regions, by = "R61") %>%
    group_by(R12, year, type, emission_source) %>%
    summarise(co2_emissions = sum(co2_emissions)) %>%
    mutate(unit = 'metric tons CO2')

  co2_emission_app_global <- co2_emission_app_R12 %>%
    group_by(year, type, emission_source) %>%
    summarise(co2_emissions = sum(co2_emissions)) %>%
    mutate(unit = 'metric tons CO2')

  output <- list(
    co2_emission_app_R61 = co2_emission_app_R61,
    co2_emission_app_R12 = co2_emission_app_R12,
    co2_emission_app_EU31 = co2_emission_app_EU31,
    co2_emission_app_global = co2_emission_app_global
  )

  return(output)
  
}

