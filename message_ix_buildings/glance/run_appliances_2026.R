
library(rstudioapi)
library(tidyverse, quietly = TRUE)
library(readxl)
library(dplyr)
library(parallel)
library(writexl)

#### Set working directory based on the position of the current R file ####
path_rcode <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/") # R file directory
setwd(path_rcode)

#### configuration for the model runs and scenarios ####
rnd <- 5 # rounding precision
# yr_init <- 2020 # initial year
time_step <- 5
# yrs <- seq(2020, 2050, by = time_step) # a sequence of years considered
# future_years <- seq(2025, 2050, by = time_step)
# material_types <- c("steel", "copper", "aluminium", "cobalt", "neodymium","tantalum", "lithium", "glass", "plastics", "pla_abs", "pla_pp", "pla_pp", "pla_oth")

## scenario selection
scenario_selection = 'baseline'
## scenario selection ('policy', 'narrow', 'slow', 'close', 'circular', 'efficiency', 'supply', 'all')


# Define the reporting method / regions
report_region  <- c("EU31") # current region options include "R61", "EU31", "R12" and "global"

# Source the model integrator function
source("./F10_ModelIntegrator_2026.R")

# Call the function for scenario runs
result <- fun_scenario_run_app(yr_init,
                               yrs,
                               # scenario_combinations,
                               report_region,
                               rnd)

print(result$message)



