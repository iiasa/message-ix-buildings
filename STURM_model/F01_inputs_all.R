
### F01 SCRIPT - FUNCTION TO LOAD AND PROCESS DATA INPUTS

fun_inputs_csv <- function(path_in, path_inputs, file_inputs, sector, run){ #file_scenarios, 

  # # PATH DATA INPUT FILES
  # path_in_csv = path_in #paste0(path_in,"./input_csv/")
  
  # IMPORT LIST OF INPUT DATA FILES
  input = read_csv(paste0(path_in, file_inputs))
  
  # EDIT INPUT DATA - FILL NAs WITH DATA FROM REFERENCE SCENARIO
  input = input[,c("name_parameter",paste(names(input[2])),paste(run))]
  names(input) = c("name_parameter", "reference", "name_file")
  input = input %>% 
    mutate(name_file = ifelse(is.na(name_file),reference,name_file)) %>%
    select(name_parameter,name_file)


  ### DATA LOADING AND PROCESSING
  
  # Extract data paths, file names and variable names
  #input_paths = input %>% select(path_file) %>% pull # Extract paths to input files
  input_files = paste0(path_inputs, input %>% select(name_file) %>% pull, ".csv") # Extract file names
  input_names = input %>% select(name_parameter) %>% pull # Extract variable names
  
  # LOAD INPUT DATA
  d = lapply(input_files, read_csv) # Load input data csv files into a list of dataframes
  d = setNames(d,input_names) # rename the dataframes within the list based on the variable names

  # PROCESS DATA - Rename the "value" column within each dataframe with variable-specific name
  d = Map(fun_rename, d, input_names) 
  
  #remove provisional inputs
  rm(input,input_files,input_names)
  
  # Output
  output = d
  
}
  

# Generate data structures - building on loaded input data

fun_build_data_model <- function(d, sector, geo_level, geo_level_aggr,geo_levels,region_select){
  
  # Regions
  regions <- unlist(d$geo_data[,paste(geo_level)]) # Regions in the analysis
  regions_aggr <- sort(unique(unlist(d$geo_data[,paste(geo_level_aggr)]))) # Regions in the analysis - aggregated level
  
  # #Household categories - not used
  # urts <- sort(unique(d$clim_zones$urt)) # Urban - rural
  # ct_hh_inc <-  sort(unique(d$ct_inc_cl$inc_cl)) # Income classes
  # ct_hh_tenr <- sort(unique(d$ct_tenr$tenr)) # Tenure
  
  # BUILDING CASES
  d$bld_cases_fuel <- expand.grid(geo_level = regions,
                                urt = sort(unique(d$clim_zones$urt)), 
                                inc_cl = sort(unique(d$ct_inc_cl$inc_cl)),
                                #arch = ct_bld_arch,
                                stringsAsFactors = FALSE) %>%
    rename_at("geo_level", ~paste0(geo_level)) %>%
    left_join(d$geo_data %>% select_at(geo_levels)) %>%
    left_join(d$clim_zones, by=c(paste(geo_level), "urt")) %>%
    left_join(d$ct_inc_cl) %>%
    left_join(d$ct_bld) %>%
    left_join(d$ct_eneff, by="mat") %>%
    #left_join(ct_access, by="mat") %>% # REMOVED in this version
    inner_join(d$ct_fuel_comb, by=c("mat" = "mat")) %>%
    # select(!!as.symbol(geo_level), scenario, urt, clim, inc_cl, arch, mat, eneff) %>% # Re-order the columns
    arrange(!!as.symbol(geo_level), #scenario,
            urt, clim, inc_cl, arch, mat, eneff,
            #acc_heat, acc_cool,
            fuel_heat, fuel_cool) # Sort values ## used eneff (ordered categories)
  
  # Region selection
  if(!is.null(region_select)){d$bld_cases_fuel <- d$bld_cases_fuel[which(d$bld_cases_fuel[,paste(region_select[[1]])] %in% region_select[[2]]),]}
  
  ### BUILDING CASES at more aggregate level can be generated from bld_cases_fuel
  # bld_cases_arch <- bld_cases_fuel %>% select(-c(eneff, bld_age, fuel_heat, fuel_cool, mod_decision)) %>% distinct()
  d$bld_cases_eneff <- d$bld_cases_fuel %>% select(-c(fuel_heat, fuel_cool, mod_decision)) %>% distinct()
  
  output = d
}