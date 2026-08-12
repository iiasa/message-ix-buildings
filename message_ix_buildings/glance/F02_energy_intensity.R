

library(tidyverse)
library(readxl)
library(dplyr)




#### Function: energy intensity modelling driven by GDP and efficiency ####

fun_energy_intensity_app <- function(energy_intensity_app,
                                     R12_gdp_cap,
                                     regions,
                                     SSP){

    
    # from R12 to R61 regions
    energy_intensity_app_R61 <- regions %>%
      select(R61, R12) %>%
      left_join(energy_intensity_app,
                by = c('R12' = 'region')) %>%
      select(-R12) %>%
      rename(region = R61)
    
    # # add dimension of urban / rural
    # location <- c("urb", "rur")
    # expanded_df <- energy_intensity_app_R61[rep(seq_len(nrow(energy_intensity_app_R61)), each = length(location)), ]
    # expanded_df$urt <- rep(location, times = nrow(energy_intensity_app_R61))
    # energy_intensity_app_R61 <- expanded_df %>%
    #   select(scenario, region, urt, type, year, energy_intensity, unit)
  
    output <- energy_intensity_app_R61

    return(output)


}

  