
### TEMPORARY - GENERIC FUNCTIONS TO PROCESS DATA - MOVE TO DEDICATED SCRIPT

# Function to convert dataframes to long format
fun_toLong <- function(DF){ ## var_name excluded from inputs
  if("data.frame"  %in% class(DF) == FALSE) stop(paste(deparse(substitute(DF)),'is not a data.frame'))
  #if(class(var_name)!="character" | length(var_name)!= 1) stop(paste(deparse(substitute(var_name)),'is not a character unit vector. Please, provide a name in quotes.'))
  if(length(grep("\\d{4}", names(DF)))>0){ # If column names contain years, convert to long format over years, otherwise return the same dataframe
    DF_L <- pivot_longer(data=DF, values_to = "value", names_to = "year", cols=matches("\\d{4}")) %>%
      mutate(year=as.integer(year))
  } else {DF_L = DF}
  
  output = DF_L
}

# Function to rename data inputs
fun_rename <- function(DF,name){ ## DF = dataframe; name = name to relabel the "value" column
  if("data.frame"  %in% class(DF) == FALSE) stop(paste(deparse(substitute(DF)),'is not a data.frame'))
  if(length(grep("value", names(DF)))==0) stop(paste(deparse(substitute(DF)),'no columns named: value'))
  
  # DF_R <- DF %>% rename_with(~paste(name), .cols=value) # Works with only one "value" column per dataframe
  DF_R <- DF %>% rename_with(~gsub("value", name, names(DF)), .cols = everything()) # works also with multiple "value1", "value2", etc. columns
  
  output = DF_R
}

# # Check if "value" is among the column names
# for(i in 1:length(d)){if(length(grep("value", names(d[[i]])))==0) {print(names(d)[i])}}


### TEMPORARY ### MOVE PATH DEFINITION TO SCRIPT 00

# PATH DATA INPUT FILES
path_in_csv <- "./STURM_data/input_csv/"

# IMPORT LIST OF INPUT DATA FILES
input <- read_csv(paste0(path_in_csv, "input_list_",sector,".csv" ))

# IMPORT LIST OF SCENARIOS
scenarios <- read_csv(paste0(path_in_csv, "scenarios_TEST.csv" ))




### F01 SCRIPT FROM HERE

# Create vector of scenario-dependent parameters
scen_pars <- names(scenarios)[!names(scenarios) %in% c("scenario_id", "scenario_name")]

# Scenario setup for scenario-dependent parameters
scen_setup <- scenarios %>% 
  filter(scenario_name == run) %>%
  select(-c(scenario_id, scenario_name)) %>%
  pivot_longer(cols = all_of(scen_pars), names_to = "name_input", values_to = "scenario")


# Input data: build vector of input file names for the current scenarios
input <- input %>%
  left_join(scen_setup) %>%
  filter(category !="skip") %>% # Exclude lines with "skip" indication
  mutate(path_file = ifelse(category == "basic",
                            paste0(path_in_csv,"input_basic/",name_file),
                            paste0(path_in_csv,"input_",sector, "/",category,"/",name_file)
                            )) %>%
  mutate(path_file = ifelse(!is.na(scenario),paste0(path_file,"_",scenario),path_file)) %>%
  mutate(path_file = paste0(path_file,".csv"))


### TEMPORARY ### EXCLUDE SPECIAL DATA FILES
input <- input %>%
  filter(!name_input %in% c("bld_dyn_par","cool_data_scen","en_m2_sim_r")) %>%
  filter(category != "categories")

### 

### DATA LOADING AND PROCESSING

# Extract data paths, file names and variable names
input_paths <- input %>% select(path_file) %>% pull # Extract paths to input files
input_files <- input %>% select(name_file) %>% pull # Extract file names
input_names <- input %>% select(name_input) %>% pull # Extract variable names

# LOAD INPUT DATA
d <- lapply(input_paths, read_csv)
d <- setNames(d,input_names)


# PROCESS DATA (1) - Convert timeseries to long format
d <- lapply(d, fun_toLong)

# PROCESS DATA (2) - Rename "value" column with variable-specific name
d <- Map(fun_rename, d, input_names) 
# d <- mapply(fun_rename, d, input_names, SIMPLIFY = FALSE) # Alternative coding with mapply (same result)

### TEMPORARY ### Extract dataframes to global environment
list2env(d, .GlobalEnv)


# Temporary - Load special data separately (to be changed!)

## bld_dyn_par
# Parameters: # Xiaoyang: separate csv files with multiple variables
# dem_k, dem_lambda -> F06 # Alessio
# l_new -> F04 # Xiaoyang
# l_ren -> F05 # Xiaoyang

## energy_sim_ref
# separate heat/cool/days of cooling?

## cool operation:
# separate different parameters

## heat operation: already long format


#remove provisional inputs
rm(input,scen_setup)

## TODOs: 
# - processing: from wide to long format
# - work on parameters name: consistency between file names and parameter names
# - check scenario/ssp columns
# - check how to provide inputs: list2env or keep inside lists?

