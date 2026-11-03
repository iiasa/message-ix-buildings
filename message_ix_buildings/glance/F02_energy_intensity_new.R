

library(tidyverse)
library(readxl)
library(dplyr)




#### Function: energy intensity modelling driven by GDP and efficiency ####

fun_energy_intensity_app <- function(energy_intensity_initial,
                                     R12_gdp_cap,
                                     regions,
                                     SSP,
                                     efficiency_consideration,
                                     efficiency_speed){

    # set growth_const to determine how rapidly energy intensity responds to GDP changes
    growth_const <- 0.0002
  
    # # Set time_const based on efficiency_speed
    # time_const <- ifelse(efficiency_speed == "fast", 20, 40)
  
    # revise the input data
    energy_intensity_initial_filtered <- energy_intensity_initial %>%
      filter(scenario == SSP)
    
    R12_gdp_cap_filtered <- R12_gdp_cap %>%
      filter(scenario == SSP)
    
    # future energy intensity calculation
    future_years <- seq(2025, 2100, by = 5)
    energy_intensity_future <- energy_intensity_initial_filtered %>%
      left_join(R12_gdp_cap_filtered) %>%
      select(-"unit")
    
    for (i in future_years) {
    
    yr_curr <- i
    ei_efficiency_column_name <- ifelse(efficiency_consideration == "yes", "ei_efficiency_target", "ei_efficiency_none")
    time_const_column_name <- ifelse(efficiency_speed == "fast", "time_const_FAST", "time_const_SLOW")
    
    energy_intensity_future <- energy_intensity_future %>%
      mutate(!!paste0("ei_", i) := ei_saturation / (1 + (ei_saturation - ei_initial) / ei_initial * exp(-growth_const * max(0, !!sym(paste0("gdp_cap_", i)) - gdp_cap_2020))) *
               (!!sym(ei_efficiency_column_name) / ei_saturation) / (1 + (!!sym(ei_efficiency_column_name) - ei_saturation) / ei_saturation * exp(-max(0, yr_curr - yr_init) / !!sym(time_const_column_name))))
    } 
      
    energy_intensity_app <- energy_intensity_future %>%
      select(c(1:7, 27:42)) %>%
      rename("2020" = ei_initial,
             "2025" = ei_2025,
             "2030" = ei_2030,
             "2035" = ei_2035,
             "2040" = ei_2040,
             "2045" = ei_2045,
             "2050" = ei_2050,
             "2055" = ei_2055,
             "2060" = ei_2060,
             "2065" = ei_2065,
             "2070" = ei_2070,
             "2075" = ei_2075,
             "2080" = ei_2080,
             "2085" = ei_2085,
             "2090" = ei_2090,
             "2095" = ei_2095,
             "2100" = ei_2100)
    
    # pivot from wide to long format
    energy_intensity_app_long <- energy_intensity_app %>%
      select(c(1:4, 8:23)) %>%
      pivot_longer(cols=c("2020", "2025", "2030", "2035", "2040", "2045", "2050", "2055", "2060", "2065", "2070", "2075", "2080", "2085", "2090", "2095", "2100"),
                   names_to='year',
                   values_to='energy_intensity') %>%
      mutate(unit = 'GJ/unit')
    
    # from R12 to R61 regions
    energy_intensity_app_R61 <- regions %>%
      select(R61, R12) %>%
      left_join(energy_intensity_app_long,
                by = c('R12' = 'region')) %>%
      select(-R12) %>%
      rename(region = R61)
    
    # add dimension of urban / rural
    location <- c("urb", "rur")
    expanded_df <- energy_intensity_app_R61[rep(seq_len(nrow(energy_intensity_app_R61)), each = length(location)), ]
    expanded_df$urt <- rep(location, times = nrow(energy_intensity_app_R61))
    energy_intensity_app_R61 <- expanded_df %>%
      select(scenario, region, urt, type, year, energy_intensity, unit)
  
    output <- energy_intensity_app_R61

    return(output)


}

  