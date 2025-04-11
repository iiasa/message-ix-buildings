library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(argparse)


# Create a parser object
parser <- ArgumentParser(description = "Script to set number of cores")

# Add argument for number of cores
parser$add_argument("-f", "--files", default = NULL,
                    help = "Display or not the figures")

parser$add_argument("-d", "--dir", default = NULL,
                    help = "Path to the directory containing the files")

# Parse the arguments
args <- parser$parse_args()
# args <- list(file = "max_scenario.csv", dir = "2024-06-16_184750")

scenarios <- read.csv(args$f, header = TRUE)
# get last element of args$f only the name of the file 

dir <- paste("STURM_output/results", args$dir, sep = "/")
data <- data.frame()

for (scenario in unique(scenarios$scenario_name)) {

  region <- scenarios %>%
    filter(scenario_name == scenario) %>%
    select(region_bld) %>%
    unique()

  print(paste("Scenario:", scenario))
  file <- paste0("report_agg_", scenario, ".csv")
  file <- paste(dir, file, sep = "/")

  if (!file.exists(file)) {
    print("Check file name or directory!")
  }

  # Read output files
  temp <- read.csv(file) %>%
    select(c("region_bld", "year", "variable", "resolution", "value")) %>%
    filter(region_bld %in% region$region_bld) 
    #%>% mutate(scenario = scenario)

  data <- rbind(data, temp)
}

eu <- data %>%
  group_by(year, variable, resolution) %>%
  summarise(value = sum(value)) %>%
  mutate(region_bld = "EU")

data <- rbind(data, eu)

save_dir <- paste("STURM_output/figures", paste(args$dir, "optimal_scenarios", sep = "_"), sep = "/")
if (!dir.exists(save_dir)) {
  dir.create(save_dir)
}

write.csv(data, paste(save_dir, paste0("report_agg_", basename(args$f)), sep = "/"), row.names = FALSE)