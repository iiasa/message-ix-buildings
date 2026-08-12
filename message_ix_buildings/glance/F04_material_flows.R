

library(tidyverse)
library(readxl)


#### function 3: calculating future material stocks, inflows, and outflows per time step ####
# note that the current results for flows are for each time step (5 years) instead of for each year!

fun_material_app <- function(stock_app_all_years,
                             inflow_app_all_years,
                             outflow_app_all_years,
                             material_intensity_app,
                             eol_rate_mat,
                             regions){
  
  # Edit the input dataframes
  # material_intensity_app <- material_intensity_app %>%
  #   filter(scenario == material_intensity_consideration) %>%
  #   # filter(year %in% yrs) %>%
  #   select(-c(scenario))
  
  # material_intensity_app <- material_intensity_app %>%
  #   rename(region = R61)
  
  # eol_rate_mat <- eol_rate_mat %>%
  #   rename(region = R61, material_type = material_types)
  
  #### Calculate the material stocks, inflows, and outflows of appliances ####
  ## material stocks = appliances number * material intensity (unit * g/unit /1000000)
  material_stock_app <- stock_app_all_years %>%
    left_join(material_intensity_app) %>%
    mutate(material_stock =
             round(n_app * material_intensity / 1000000, rnd)) %>%
    group_by(region, urt, year, generation, yrs_prod, type, material_type) %>%
    summarise(material_stock = sum(material_stock)) %>%
    mutate(unit = "tonne")
  
  material_inflow_app <- inflow_app_all_years %>% # note that this is the total materials needed over 5 years
    filter(generation == 1) %>% # no materials needed for appliances reuse
    left_join(material_intensity_app) %>%
    mutate(material_inflow =
             round(n_inflow * material_intensity / 1000000, rnd)) %>%
    select(region, urt, year, generation, yrs_prod, type, material_type, material_inflow) %>%
    mutate(unit = "tonne")
  
  material_outflow_app <- outflow_app_all_years %>% # note that this is the total material outflows over 5 years
    left_join(material_intensity_app) %>%
    mutate(material_outflow =
           round((n_outflow_actual- n_outflow_store) * material_intensity / 1000000, rnd)) %>% # n_outflow_actual instead of n_outflow_nominal
    group_by(region, urt, year, generation, yrs_prod, type, material_type) %>%
    summarise(material_outflow = sum(material_outflow)) %>%
    mutate(unit = "tonne")
  
  # # combine stocks, inflows, outflows into one table
  # material_app <- material_stock_app %>%
  #   left_join(material_inflow_app) %>%
  #   left_join(material_outflow_app) %>%
  #   select(region, urt, year, generation, yrs_prod, type, material_type, material_stock, material_inflow, material_outflow, unit)
  
  # aggregating
  material_stock_app_aggr <- material_stock_app %>%
    group_by(region, urt, year, type, material_type) %>%
    summarise(material_stock = sum(material_stock)) %>%
    mutate(unit = "tonne")
  
  material_inflow_app_aggr <- material_inflow_app %>%
    group_by(region, urt, year, type, material_type) %>%
    summarise(material_inflow = sum(material_inflow)) %>%
    mutate(unit = "tonne")
  
  material_outflow_app_aggr <- material_outflow_app %>%
    group_by(region, urt, year, type, material_type) %>%
    summarise(material_outflow = sum(material_outflow)) %>%
    mutate(unit = "tonne")
  
  # ## incorporate material reuse, recycling, downcycling, landfill, and incineration
  # # calculate the potentials of reuse, recycling, and downcycling
  # material_outflow_EOL <- material_outflow_app_aggr %>%
  #   mutate(reuse_rate_mat = 0.8 * 0.2,
  #          recycle_rate_mat = 0.8 * 0.3,
  #          downcycle_rate_mat = 0.8 * 0.5,
  #          landfill_rate_mat = 0.2 * 0.4,
  #          incinerate_rate_mat = 0.2 * 0.6) %>% # with no other options, they add up to 100%
  #   mutate(mat_reuse_potential = material_outflow * reuse_rate_mat,
  #          mat_recycle_potential = material_outflow * recycle_rate_mat,
  #          mat_downcycle_potential = material_outflow * downcycle_rate_mat,
  #          mat_landfill = material_outflow * landfill_rate_mat,
  #          mat_incinerate = material_outflow * incinerate_rate_mat) %>%
  #   select(region, year, type, material_type, material_outflow, mat_reuse_potential, mat_recycle_potential, mat_downcycle_potential,mat_landfill, mat_incinerate, unit)
  # 
  # # calculate the actual reuse, recycling, and primary productions
  # material_inflow_source <- material_inflow_app_aggr %>% # actual recovery depends on both outflow and inflow, e.g., reuse can not overtake inflow
  #   left_join(material_outflow_EOL) %>%
  #   select(region, year, type, material_type, material_inflow, mat_reuse_potential, mat_recycle_potential) %>%
  #   mutate(reuse = ifelse(material_inflow > mat_reuse_potential, mat_reuse_potential, material_inflow)) %>%
  #   mutate(recycling = ifelse((material_inflow - reuse) > mat_recycle_potential, mat_recycle_potential, (material_inflow - reuse))) %>%
  #   select(-c(mat_reuse_potential, mat_recycle_potential)) %>%
  #   mutate(primary = material_inflow - reuse - recycling) %>%
  #   select(region, year, type, material_type, material_inflow, primary, reuse, recycling)
  # 
  # # combine stocks, inflows, outflows into one table
  # material_app_aggr_5years_total <- material_stock_app_aggr %>% # flows are 5 years total
  #   left_join(material_outflow_EOL) %>%
  #   select(-c(mat_reuse_potential, mat_recycle_potential)) %>%
  #   left_join(material_inflow_source) %>%
  #   select(region, year, type, material_type, material_stock, material_outflow, mat_landfill, mat_incinerate, mat_downcycle_potential, material_inflow, primary, reuse, recycling, unit)
  # 
  # material_app_aggr <- material_app_aggr_5years_total %>% # flows are annual averages
  #   mutate(material_outflow = material_outflow / 5,
  #          landfill = mat_landfill / 5,
  #          incineration = mat_incinerate / 5,
  #          downcycling = mat_downcycle_potential / 5,
  #          material_inflow = material_inflow / 5,
  #          primary = primary / 5,
  #          reuse = reuse / 5,
  #          recycling = recycling / 5)
  # 
  
  
  
  # 
  ## below is new ##


  ## incorporate material reuse, recycling, downcycling, landfill, and incineration
  # calculate the potentials of reuse, recycling, and downcycling

  material_outflow_EOL <- material_outflow_app_aggr %>%
    left_join(eol_rate_mat, by = c('region', 'year', 'type', 'material_type')) %>%
    mutate(mat_reuse_potential = material_outflow * reuse,
           mat_recycle_potential = material_outflow * recycling,
           mat_downcycle_potential = material_outflow * downcycling,
           mat_disposal_managed = material_outflow * managed_solid_waste_disposal_site,
           mat_disposal_unmanaged = material_outflow * unmanaged_solid_waste_disposal_site,
           mat_incineration_energy = material_outflow * incineration_energy,
           mat_incineration = material_outflow * incineration,
           mat_burning_open = material_outflow * open_burning,
           mat_composting = material_outflow * composting,
           mat_unknown = material_outflow * unknown) %>%
    select(region, urt, year, type, material_type, material_outflow,
           mat_reuse_potential, mat_recycle_potential, mat_downcycle_potential,
           mat_disposal_managed, mat_disposal_unmanaged,
           mat_incineration_energy, mat_incineration,
           mat_burning_open, mat_composting, mat_unknown,
           unit)

  # calculate the actual reuse, recycling, and primary productions
  material_inflow_source <- material_inflow_app_aggr %>% # actual recovery depends on both outflow and inflow, e.g., reuse can not overtake inflow
    left_join(material_outflow_EOL) %>%
    select(region, urt, year, type, material_type, material_inflow, mat_reuse_potential, mat_recycle_potential) %>%
    mutate(reuse = ifelse(material_inflow > mat_reuse_potential, mat_reuse_potential, material_inflow)) %>%
    mutate(recycling = ifelse((material_inflow - reuse) > mat_recycle_potential, mat_recycle_potential, (material_inflow - reuse))) %>%
    select(-c(mat_reuse_potential, mat_recycle_potential)) %>%
    mutate(primary = material_inflow - reuse - recycling) %>%
    select(region, urt, year, type, material_type, material_inflow, primary, reuse, recycling)

  # combine stocks, inflows, outflows into one table
  material_app_aggr_5years_total <- material_stock_app_aggr %>% # flows are 5 years total
    left_join(material_outflow_EOL) %>%
    select(-c(mat_reuse_potential, mat_recycle_potential)) %>%
    left_join(material_inflow_source) %>%
    select(region, urt, year, type, material_type, material_stock,
           material_outflow, mat_disposal_managed, mat_disposal_unmanaged,
           mat_incineration_energy, mat_incineration, mat_burning_open,
           mat_composting, mat_unknown,
           mat_downcycle_potential,
           material_inflow, primary, reuse, recycling, unit)

  material_app_aggr <- material_app_aggr_5years_total %>% # flows are annual averages
    mutate(material_outflow = material_outflow / 5,
           disposal_managed = mat_disposal_managed / 5,
           disposal_unmanaged = mat_disposal_unmanaged / 5,
           incineration_energy = mat_incineration_energy / 5,
           incineration = mat_incineration / 5,
           burning_open = mat_burning_open / 5,
           composting = mat_composting / 5,
           unknown = mat_unknown / 5,
           downcycling = mat_downcycle_potential / 5,

           # landfill = mat_landfill / 5,
           # incineration = mat_incinerate / 5,
           # downcycling = mat_downcycle_potential / 5,

           material_inflow = material_inflow / 5,
           primary = primary / 5,
           reuse = reuse / 5,
           recycling = recycling / 5)
  
  #### aggregating by region ####
  
  material_app_aggr_R61 <- material_app_aggr %>%
    rename(R61 = region)
  
  material_app_aggr_EU31 <- material_app_aggr_R61 %>%
    left_join(regions) %>%
    filter(grepl("C-EEU|C-WEU", R61)) %>%
    group_by(R61, urt, year, type, material_type) %>%
    summarise(material_stock = sum(material_stock),
              material_outflow = sum(material_outflow),
              
              disposal_managed = sum(disposal_managed),
              disposal_unmanaged = sum(disposal_unmanaged),
              incineration_energy = sum(incineration_energy),
              incineration = sum(incineration),
              burning_open = sum(burning_open),
              composting = sum(composting),
              unknown = sum(unknown),
              downcycling = sum(downcycling),
              
              material_inflow = sum(material_inflow),
              primary = sum(primary),
              reuse = sum(reuse),
              recycling = sum(recycling)) %>%
    rename(EU31 = R61) %>%
    mutate(unit = "tonne")
  
  material_app_aggr_R12 <- material_app_aggr_R61 %>%
    left_join(regions) %>%
    group_by(R12, urt, year, type, material_type) %>%
    summarise(material_stock = sum(material_stock),
              material_outflow = sum(material_outflow),
              
              disposal_managed = sum(disposal_managed),
              disposal_unmanaged = sum(disposal_unmanaged),
              incineration_energy = sum(incineration_energy),
              incineration = sum(incineration),
              burning_open = sum(burning_open),
              composting = sum(composting),
              unknown = sum(unknown),
              downcycling = sum(downcycling),
              
              material_inflow = sum(material_inflow),
              primary = sum(primary),
              reuse = sum(reuse),
              recycling = sum(recycling)) %>%
    mutate(unit = "tonne")
  
  material_app_aggr_global <- material_app_aggr_R12 %>%
    group_by(year, type, material_type) %>%
    summarise(material_stock = sum(material_stock),
              material_outflow = sum(material_outflow),
              
              disposal_managed = sum(disposal_managed),
              disposal_unmanaged = sum(disposal_unmanaged),
              incineration_energy = sum(incineration_energy),
              incineration = sum(incineration),
              burning_open = sum(burning_open),
              composting = sum(composting),
              unknown = sum(unknown),
              downcycling = sum(downcycling),
              
              material_inflow = sum(material_inflow),
              primary = sum(primary),
              reuse = sum(reuse),
              recycling = sum(recycling)) %>%
    mutate(unit = "tonne")
  
  # bind those tables into a list
  output <- list(
    material_app_aggr_R61 = material_app_aggr_R61,
    material_app_aggr_EU31 = material_app_aggr_EU31,
    material_app_aggr_R12 = material_app_aggr_R12,
    material_app_aggr_global = material_app_aggr_global
  )
  
  return(output)
  
  
  ## above is new ##
  
    
  # #### aggregating by region ####
  # 
  # material_app_aggr_R61 <- material_app_aggr %>%
  #   rename(R61 = region)
  # 
  # material_app_aggr_EU31 <- material_app_aggr_R61 %>%
  #   left_join(regions) %>%
  #   filter(grepl("C-EEU|C-WEU", R61)) %>%
  #   group_by(R61, year, type, material_type) %>%
  #   summarise(material_stock = sum(material_stock),
  #             material_outflow = sum(material_outflow),
  #             landfill = sum(landfill),
  #             incineration = sum(incineration),
  #             downcycling = sum(downcycling),
  #             material_inflow = sum(material_inflow),
  #             primary = sum(primary),
  #             reuse = sum(reuse),
  #             recycling = sum(recycling)) %>%
  #   rename(EU31 = R61) %>%
  #   mutate(unit = "tonne")
  # 
  # material_app_aggr_R12 <- material_app_aggr_R61 %>%
  #   left_join(regions) %>%
  #   group_by(R12, year, type, material_type) %>%
  #   summarise(material_stock = sum(material_stock),
  #             material_outflow = sum(material_outflow),
  #             landfill = sum(landfill),
  #             incineration = sum(incineration),
  #             downcycling = sum(downcycling),
  #             material_inflow = sum(material_inflow),
  #             primary = sum(primary),
  #             reuse = sum(reuse),
  #             recycling = sum(recycling)) %>%
  #   mutate(unit = "tonne")
  # 
  # material_app_aggr_global <- material_app_aggr_R12 %>%
  #   group_by(year, type, material_type) %>%
  #   summarise(material_stock = sum(material_stock),
  #             material_outflow = sum(material_outflow),
  #             landfill = sum(landfill),
  #             incineration = sum(incineration),
  #             downcycling = sum(downcycling),
  #             material_inflow = sum(material_inflow),
  #             primary = sum(primary),
  #             reuse = sum(reuse),
  #             recycling = sum(recycling)) %>%
  #   mutate(unit = "tonne")
  # 
  # # bind those tables into a list
  # output <- list(
  #   material_app_aggr_R61 = material_app_aggr_R61,
  #   material_app_aggr_EU31 = material_app_aggr_EU31,
  #   material_app_aggr_R12 = material_app_aggr_R12,
  #   material_app_aggr_global = material_app_aggr_global
  # )
  # 
  # return(output)
  
}


