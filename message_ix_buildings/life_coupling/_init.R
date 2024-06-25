
library(tidyverse)
library(rstudioapi)
library(cobs)

#Script to initialize coupling runs - LIFE / MESSAGEix-Buildings models

### Step 1: basic settings

# Set working directory to current directory
path_rcode <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/")
setwd(path_rcode)

# Paths
path_in_LIFE <- "./input/life/"
path_in_IAM <- "./input/iam/"
path_out <- "./output/"

# Timeseries
yrs <- seq(2020,2050,5) # Model years

### Step 2: Source coupling functions
source(paste0(path_rcode,"coupling_code.R"))

### Step 3: Load and process input data

# Data for calibration
cf_ls_mod_I_sens <- read_csv(paste0(path_in_LIFE,"life_sens_mod_beh_I.csv"))

# Define LIFE parameter names
names_life_base <- c("f_beh_prop", "ratio_beh_I_prop", "ratio_beh_A_prop", "beh_I_base") # Static data
names_life_ts <- c("h_cog", "f_prop", "h_prop", "f_cog_I", "f_cog_A") # Timeseries data

# Load LIFE data
d_life_base <- lapply(paste0(path_in_LIFE,names_life_base,".csv"), read_csv) # Load input data csv files into a list of dataframes
d_life_ts <- lapply(paste0(path_in_LIFE,names_life_ts,".csv"), read_csv) # Load input data csv files into a list of dataframes

# Names in list
names(d_life_base) <- names_life_base
names(d_life_ts) <- names_life_ts

# Extract data from list of dataframes to dataframes in the global environment
list2env(d_life_base,envir=.GlobalEnv)

# Step 4: Run calibration process
crv_mod_I <- fun_life_calib(cf_ls_mod_I_sens)

# Step 5: Run lifestyles coupling loop for all timesteps
for (i in 1:length(yrs)){
  print(yrs[i])
  
  # Filter year "i" in LIFE dataframes
  d_life_i <- map(d_life_ts, ~.x %>%
                    filter(year==yrs[i])
                    )

  list2env(d_life_i,envir=.GlobalEnv)
  
  # Load IAM data
  d_beh <- read_csv(paste0(path_in_IAM,"d_beh_",yrs[i],".csv"))
  
  # Run LIFE coupling functions
  output_life <- fun_life_coupling(yrs, i, 
                                d_beh, # Different file imported at every timestep
                                f_beh_prop, # Fixed.by hh_group
                                h_cog, # timeseries - by hh_group
                                f_prop, # timeseries - by hh_group
                                h_prop, # timeseries - by hh_group
                                f_cog_I, # timeseries - by hh_group
                                f_cog_A, # timeseries - by hh_group
                                ratio_beh_I_prop,# Fixed.by hh_group
                                ratio_beh_A_prop, # Fixed.by hh_group
                                beh_I_base # Fixed.by hh_group
  )
  
  write_csv(output_life, paste0(path_out, "output_life_",yrs[i],".csv"))
}

