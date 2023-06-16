
### TEMPORARY - GENERIC FUNCTIONS TO PROCESS DATA - MOVE TO DEDICATED SCRIPT

# # Function to convert dataframes to long format
# fun_toLong <- function(DF){ ## var_name excluded from inputs
#   if("data.frame"  %in% class(DF) == FALSE) stop(paste(deparse(substitute(DF)),'is not a data.frame'))
#   #if(class(var_name)!="character" | length(var_name)!= 1) stop(paste(deparse(substitute(var_name)),'is not a character unit vector. Please, provide a name in quotes.'))
#   if(length(grep("\\d{4}", names(DF)))>0){ # If column names contain years, convert to long format over years, otherwise return the same dataframe
#     DF_L <- pivot_longer(data=DF, values_to = "value", names_to = "year", cols=matches("\\d{4}")) %>%
#       mutate(year=as.integer(year))
#   } else {DF_L = DF}
#   
#   output = DF_L
# }

# # Function to rename data inputs - Moved to script B00_functions
# fun_rename <- function(DF,name){ ## DF = dataframe; name = name to relabel the "value" column
#   if("data.frame"  %in% class(DF) == FALSE) stop(paste(deparse(substitute(DF)),'is not a data.frame'))
#   if(length(grep("value", names(DF)))==0) stop(paste(deparse(substitute(DF)),'no columns named: value'))
#   
#   # DF_R <- DF %>% rename_with(~paste(name), .cols=value) # Works with only one "value" column per dataframe
#   DF_R <- DF %>% rename_with(~gsub("value", name, names(DF)), .cols = everything()) # works also with multiple "value1", "value2", etc. columns
#   
#   output = DF_R
# }

# # Check if "value" is among the column names
# for(i in 1:length(d)){if(length(grep("value", names(d[[i]])))==0) {print(names(d)[i])}}


# ### TEMPORARY ### MOVE PATH DEFINITION TO SCRIPT 00 (run_)
# 
# # PATH DATA INPUT FILES
# path_in_csv <- paste0(data_path,"./input_csv/")
# 
# # IMPORT LIST OF INPUT DATA FILES
# input <- read_csv(paste0(path_in_csv, "input_list_",sector,".csv" ))
# 
# # IMPORT LIST OF SCENARIOS
# scenarios <- read_csv(paste0(path_in_csv, "scenarios_TEST.csv" ))



### F01 SCRIPT FROM HERE


fun_inputs_csv <- function(path_in, file_inputs, file_scenarios, sector, run){

  # PATH DATA INPUT FILES
  path_in_csv = paste0(path_in,"./input_csv/")
  
  # IMPORT LIST OF INPUT DATA FILES
  input = read_csv(paste0(path_in, file_inputs))
  
  # IMPORT LIST OF SCENARIOS
  scenarios = read_csv(paste0(path_in, file_scenarios))

  # Create vector of scenario-dependent parameters
  scen_pars = names(scenarios)[!names(scenarios) %in% c("scenario_id", "scenario_name")]
  
  # Scenario setup for scenario-dependent parameters
  scen_setup = scenarios %>% 
    filter(scenario_name == run) %>%
    select(-c(scenario_id, scenario_name)) %>%
    pivot_longer(cols = all_of(scen_pars), names_to = "name_parameter", values_to = "scenario")
  
  
  # Input data: build vector of input file names for the current scenarios
  input = input %>%
    left_join(scen_setup) %>%
    filter(category !="skip") %>% # Exclude lines with "skip" indication
    mutate(path_file = ifelse(category == "basic",
                              paste0(path_in_csv,"input_basic/",name_file),
                              paste0(path_in_csv,"input_",sector, "/",category,"/",name_file)
                              )) %>%
    mutate(path_file = ifelse(!is.na(scenario),paste0(path_file,"_",scenario),path_file)) %>%
    mutate(path_file = paste0(path_file,".csv"))
  
  
  ### TEMPORARY ### EXCLUDE SPECIAL DATA FILES
  input = input %>%
    filter(!name_parameter %in% c("bld_dyn_par","cool_data_scen","en_m2_sim_r")) %>%
    filter(category != "categories")
  
  ### 
  
  ### DATA LOADING AND PROCESSING
  
  # Extract data paths, file names and variable names
  input_paths <- input %>% select(path_file) %>% pull # Extract paths to input files
  input_files <- input %>% select(name_file) %>% pull # Extract file names
  input_names <- input %>% select(name_parameter) %>% pull # Extract variable names
  
  # LOAD INPUT DATA
  d <- lapply(input_paths, read_csv) # Load input data csv files into a list of dataframes
  d <- setNames(d,input_names) # rename the dataframes within the list based on the variable names
  
  # # PROCESS DATA (1) - Convert timeseries to long format
  # d <- lapply(d, fun_toLong)
  
  # PROCESS DATA (2) - Rename the "value" column within each dataframe with variable-specific name
  d <- Map(fun_rename, d, input_names) 
  # d <- mapply(fun_rename, d, input_names, SIMPLIFY = FALSE) # Alternative coding with mapply (same result)
  
  # ### TEMPORARY ### Extract dataframes to global environment
  # list2env(d, .GlobalEnv)
  
  
  # Temporary - Load special data separately (to be changed!)
  
  ## bld_dyn_par
  # Parameters: # Xiaoyang: separate csv files with multiple variables
  # dem_k, dem_lambda -> F06 # Alessio
  # l_new -> F04 # Xiaoyang
  # l_ren -> F05 # Xiaoyang
  
  ## energy_sim_ref
  # separate heat/cool/days of cooling?
  
  ## cool operation / cool_operation_hours:
  # separate different parameters
  
  ## heat operation: already long format
  
  
  #remove provisional inputs
  rm(input,scen_setup)
  
  # Output
  output = d
  
}
  

# , geo_level, geo_level_aggr
  
  # ### MODEL BUILDING ### MOVE TO SEPARATE SCRIPT?
  # 
  # # Regions
  # geo_data <- read_csv(paste0(path_in_csv,"/input_basic_geo/regions.csv"))  # First columns
  # regions <- unlist(geo_data[,paste(geo_level)]) # Regions in the analysis
  # regions_aggr <- sort(unique(unlist(geo_data[,paste(geo_level_aggr)]))) # Regions in the analysis - aggregated level
  # 
  # # Climatic zones
  # clim_zones <- read_csv(paste0(path_in_csv,"/input_basic_geo/climatic_zones.csv"))
  # 
  # #Household categories
  # urts <- c("rur","urb") # # Urban-Rural / Total. options: "rur", "urb", "tot"
  # ct_hh_inc <- c("q1","q2","q3")  # Income classes
  # ct_hh_tenr <- c("own", "rent") #c("ownns")
  # 
  # #Building categories
  # ct_bld_age <- read_csv(paste0(path_in_csv,"input_",sector,"/categories/ct_bld_age.csv")) # Vintage cohorts
  # ct_bld <- read_csv(paste0(path_in_csv,"input_",sector,"/categories/ct_arch.csv"))
  # ct_eneff <- read_csv(paste0(path_in_csv,"input_",sector,"/categories/ct_eneff.csv")) # Energy efficiency categories
  # 
  # # Fuel type
  # ct_fuel_comb <- read_csv(paste0(path_in_csv,"input_",sector,"/categories/ct_fuel.csv")) 
  # ct_fuel_dhw <- read_csv(paste0(path_in_csv,"input_",sector,"/categories/ct_fuel_res.csv"))# fuels domestic hot water - Add solar thermal options
  # 
  # ## RENOVATION CATEGORIES
  # ct_ren_eneff <- read_csv(paste0(path_in_csv,"input_",sector,"/categories/ct_ren_eneff2.csv")) # "fuel" settings. Energy efficiency transitions for renovations
  # # The following is loaded as input data 
  # #ct_ren_fuel_heat <- read_csv(paste0(path_in_csv,"input_",sector,"/decision/ct_ren_fuel_heat.csv")) # Energy efficiency transitions for renovations
  # 
  # # BUILDING CASES
  # 
  # bld_cases_fuel <- expand.grid(geo_level = regions,
  #                               urt = urts, inc_cl = ct_hh_inc, 
  #                               #arch = ct_bld_arch, 
  #                               stringsAsFactors = FALSE) %>%
  #   rename_at("geo_level", ~paste0(geo_level)) %>% 
  #   left_join(geo_data %>% select_at(geo_levels)) %>%
  #   left_join(clim_zones, by=c(paste(geo_level), "urt")) %>%
  #   left_join(ct_bld) %>%
  #   left_join(ct_eneff, by="mat") %>%
  #   #left_join(ct_access, by="mat") %>% # REMOVED in this version
  #   inner_join(ct_fuel_comb, by=c("mat" = "mat")) %>%
  #   # select(!!as.symbol(geo_level), scenario, urt, clim, inc_cl, arch, mat, eneff) %>% # Re-order the columns
  #   arrange(!!as.symbol(geo_level), #scenario, 
  #           urt, clim, inc_cl, arch, mat, eneff, 
  #           #acc_heat, acc_cool, 
  #           fuel_heat, fuel_cool) # Sort values ## used eneff (ordered categories)
  # 
  # ### BUILDING CASES at more aggregate level can be generated from bld_cases_fuel
  # # bld_cases_arch <- bld_cases_fuel %>% select(-c(eneff, bld_age, fuel_heat, fuel_cool, mod_decision)) %>% distinct()
  # bld_cases_eneff <- bld_cases_fuel %>% select(-c(fuel_heat, fuel_cool, mod_decision)) %>% distinct()
  # 
