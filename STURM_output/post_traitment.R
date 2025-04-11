library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(argparse)
library(sf)

# Loding figures setttings and functions
source("STURM_output/C00_plots.R")

# Create a parser object
parser <- ArgumentParser(description = "Script to set number of cores")

# Add argument for number of cores
parser$add_argument("-p", "--path", type = "character", default = NULL,
                    help = "Name of the path to the results directory")

parser$add_argument("-c", "--counterfactual", type = "character", default = NULL,
                    help = "Name of the counterfactual scenario")

parser$add_argument("-n", "--names_scenarios", type = "character", default = NULL,
                    help = "Name of the scenario file")

parser$add_argument("-f", "--figures", default = TRUE,
                    help = "Display or not the figures")

# Parse the arguments
args <- parser$parse_args()
#args <- list(path = "2025-02-10_100652", names_scenarios = "STURM_data/scenarios_sensitivity.csv", figures = TRUE, counterfactual="EU")

# Rename the scenarios
scenarios <- c("EU" = "Counterfactual",
    "EU_carbon_tax_low" = "Carbon tax EUETS2",
    "EU_carbon_tax" = "Carbon tax EUETS",
    "EU_carbon_tax_social" = "Carbon tax social",
    "EU_carbon_tax_rebates" = "Carbon tax EUETS rebates",
    "EU_hp_subsidies" = "Heat pump subsidies",
    # "EU_hp_high_elasticity" = "Heat pump high elasticity",
    "EU_hp_learning" = "Heat pump learning",
    "EU_barriers_heater" = "Barriers heater",
    "EU_reno" = "Renovation wave",
    "EU_deep_reno" = "Deep renovation wave",
    "EU_barriers_renovation" = "Barriers renovation",
    "EU_realization_rate" = "Quality renovation"
    )

# Parameters
ref <- "Counterfactual" # reference scenario
base_year <- 2015
run <- "standalone"
stp <- 5

path_results <- "STURM_output/results"

# costs-benefits analysis parameters
lifetime_renovation <- 30 #30 # years
lifetime_heater <- 20 # years
social_discount <- 0.02 # % per year


path <- "STURM_data/input_csv/input_resid/macro/social_cost_carbon_20.csv"
social_cost_carbon <- read.csv(path) %>%
  rename(social_cost_carbon = value)

names_scenarios <- NULL
# if scenarios is str then we take all scenarios
if (!is.null(args$path)) {
  path_results <- paste(path_results, args$path, sep = "/")
  print(paste("Loading resuls from:", path_results))

  # only capture the name of the file
  scenarios <- list.files(path_results, pattern = "report_agg_.*.csv")
  scenarios <- sub("report_agg_(.*).csv", "\\1", scenarios)
  print(scenarios)
  scenarios <- setNames(scenarios, scenarios)

  run <- args$path

  if (!is.null(args$c)) {
    ref <- args$c
    print(paste("Trying to set:", ref, "as the counterfactual scenario"))
    if (!ref %in% names(scenarios)) {
      print("Check the name of the counterfactual scenario!")
      print("Automatically set to the first scenario!")
      ref <- names(scenarios)[1]
    } else {
      print(paste("Counterfactual scenario:", ref))
      }
  } else {
    print("Automatically set to the first scenario!")
    # first scenario is the reference
    ref <- names(scenarios)[1]
  }

  if (!is.null(args$names_scenarios)) {
    # columns are characters and not factors
    print(paste("Reading:", args$names_scenarios))
    names_scenarios <- read.csv(args$names_scenarios, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  }
}

save_dir <- paste("STURM_output", "figures", run, sep = "/")
if (!dir.exists(save_dir)) {
  dir.create(save_dir)
}

data <- data.frame(NULL)

for (scenario in names(scenarios)) {
  print(paste("Scenario:", scenario))
  file <- paste0("report_agg_", scenario, ".csv")
  file <- paste(path_results, file, sep = "/")

  if (!file.exists(file)) {
    print("Check file name or directory!")
  }

  # Read output files
  temp <- read.csv(file) %>%
    mutate(scenario = scenario)
  
  data <- bind_rows(data, temp)
}
# Reconstuction flows
flows <- c("n_renovation", "cost_renovation_EUR", "sub_renovation_EUR",
  "n_replacement", "cost_heater_EUR", "sub_heater_EUR",
  "to_pay_renovation", "to_pay_heater")

data <- data %>%
  mutate(scenario = scenarios[scenario])

stp <- 5

data <- data %>%
 mutate(year = ifelse(variable %in% flows, year - stp, year),
        value = ifelse(variable %in% flows, value
         / stp, value))
data <- distinct(data)
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# Output table

print("Make output table")

end_year <- 2050
df <- output_table(data, save_dir = NULL, end_year = end_year, base_year = 2015)

if (!is.null(names_scenarios)) {
  df <- df %>%
    rename(scenario_name = Scenario) %>%
    left_join(names_scenarios, by = "scenario_name") %>%
    select(colnames(names_scenarios), everything())
}

save_file <- paste(save_dir,  paste0(run, "_", end_year, "_summary_countries.csv"), sep = "/")
write.csv(df, save_file, row.names = FALSE)

save_file <- paste(save_dir, paste0(run, "_", end_year, "_summary_eu.csv"), sep = "/")
final_df_eu <- df %>%
  filter(`Member states` == "EU") %>%
  select(-c("Member states"))
write.csv(final_df_eu, save_file, row.names = FALSE)

# ----------------------------------------------------------------------
# Cost-benefits analysis

make_cost_benefits(data, ref, save_dir, nb_years = 30, figures = args$figures, make_summary = TRUE)

# ----------------------------------------------------------------------

