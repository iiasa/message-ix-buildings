

# Function to convert dataframes to long format
fun_toLong <- function(DF){ ## var_name excluded from inputs
  if("data.frame"  %in% class(DF) == FALSE) stop(paste(deparse(substitute(DF)),'is not a data.frame'))
  #if(class(var_name)!="character" | length(var_name)!= 1) stop(paste(deparse(substitute(var_name)),'is not a character unit vector. Please, provide a name in quotes.'))
  DF_L <- pivot_longer(data=DF, values_to = "value", names_to = "year", cols=matches("\\d{4}")) %>%
    mutate(Year=as.integer(Year))
  output = DF_L
}


# PATH DATA INPUT FILES
path_in_csv <- "./STURM_data/input_csv/"

# IMPORT LIST OF INPUT DATA FILES
input <- read_csv(paste0(path_in_csv, "input_list.csv" ))

# IMPORT LIST OF SCENARIOS
scenarios <- read_csv(paste0(path_in_csv, "scenarios_TEST.csv" ))

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
  filter(category !="skip") %>%
  mutate(path_file = ifelse(category == "basic",
                            paste0(path_in_csv,"input_basic/",name_file),
                            paste0(path_in_csv,"input_",sector, "/",category,"/",name_file)
                            )) %>%
  mutate(path_file = ifelse(!is.na(scenario),paste0(path_file,"_",scenario),path_file)) %>%
  mutate(path_file = paste0(path_file,".csv"))

# Extract paths to input files
input_paths <- input[1:3,] %>% select(path_file) %>% pull
input_names <- input[1:3,] %>% select(name_file) %>% pull


# LOAD INPUT DATA
d <- lapply(input_paths, read_csv)
d <- setNames(d,input_names)

# # Extract dataframes to global environment
# list2env(d, .GlobalEnv)

#remove provisional inputs
rm(input,scen_setup)

## TODOs: 
# - add full list of input data.
# - processing: from wide to long format
# - work on parameters name: consistency between file names and parameter names
# - check scenario/ssp columns
# - check how to provide inputs: list2env or keep inside lists?

