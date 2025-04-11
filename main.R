library(tidyverse, quietly = TRUE)
library(readxl)
library(dplyr)
# Load the parallel package
library(parallel)
library(argparse)


suppressPackageStartupMessages(library(tidyverse))

options(dplyr.width = Inf)
options(dplyr.summarise.inform = FALSE)
options(dplyr.show_progress = FALSE)

if (getRversion() != "4.3.0") {
    print("This script is only tested with R version 4.3.0")
}


start_script_time <- Sys.time()

# Create a parser object
parser <- ArgumentParser(description = "Script to set number of cores")

# Add argument for number of cores
parser$add_argument("-c", "--cores", type = "integer", default = NULL,
                    help = "Number of cores to use")

parser$add_argument("-s", "--scenarios_file", type = "character", default = NULL,
                    help = "Name of scenario file")

parser$add_argument("-a", "--all_scenarios", default = FALSE,
                    help = "Run all scenarios in the file")

# Parse the arguments
args <- parser$parse_args()
#args <- list("scenarios_file" = "scenarios_sensitivity.csv", "all_scenarios" = FALSE)

# Set num_cores based on the argument or use the default value
parallel <- TRUE
num_cores <- detectCores() - 2
if (!is.null(args$cores)) {
    num_cores <- args$cores
    parallel <- TRUE
    print(paste("Number of cores to be used:", num_cores))
}

#file_scenarios <- "all_scenarios.csv"
file_scenarios <- "scenarios_EU.csv"
if (!is.null(args$scenarios_file)) {
  file_scenarios <- args$scenarios_file
}
print(paste("Scenarios file:", file_scenarios))

# Parameters
rnd <- 5 # rounding (number of decimals)
u_EJ_GWa <- 31.71
min_shr_fuel <- 0.01

param <- list(subsidies_renovation_type = "ad_valorem",
            objective_renovation = NULL,
            objective_heat_pump = NULL,
            budget_constraint_insulation = NULL,
            premature_replacement = 3,
            anticipate_renovation = 5,
            sub_heater_type = "per_CO2",
            budget_constraint_heater = NULL,
            recycling_rebates = "lump_sum",
            rate_dem_target = NULL,
            mandatory_switch = FALSE,
            inertia_wtp = 4.3,
            social_discount_rate = 0.03,
            heat_pump_floor_cost = TRUE,
            short_term_price_elasticity = -0.2,
            credit_constraint = 0.2,
            duration_remboursment = 10,
            nzeb = FALSE,
            realization_rate_renovation = 1,
            renovation_intensity = NULL,
            success_objective_renovation = 1,#
            repartition_renovation = NULL,
            elasticity_renovation = -1,
            elasticity_heat_pump = -1,
            remove_barriers_renovation = FALSE,
            remove_barriers_heater = FALSE,
            factor_energy_behavior = 1,
            tol = 1e-2)

source("./STURM_model/F10_scenario_run.R")

# paths
path_rcode <- paste(getwd(), "/STURM_model/", sep = "")
path_in <- paste(getwd(), "/STURM_data/", sep = "")
path_out <- paste(getwd(), "/STURM_output/results/", sep = "")
if (!dir.exists(path_out)) {
    dir.create(path_out)
}

# configuration file STURM
base_year <- 2015
end_year <- 2050
step_year <- 5


# configuration file STURM
region <- c("WEU", "EEU")
sector <- "resid"
file_inputs <- "input_list_resid_EU.csv"
en_method <- "TABULA"
energy_efficiency <- "endogenous"
run <- "policies"

runs <- c("EU") #
if (args$all_scenarios) {
    runs <- "all"
}

# check if run is a vector or a string
if (is.character(runs) && length(runs) == 1) {
    if (runs == "all") {
        runs <- read.csv2(paste0(path_in, file_scenarios), sep = ',')$scenario_name
    }
}

# Create name_dir
name_dir <- paste0(Sys.Date(), "_", format(Sys.time(), "%H%M%S"), "/")
path_out <- paste0(path_out, name_dir)
if (!dir.exists(path_out)) {
    dir.create(path_out)
}


report <- list(var = c("energy"),
               type = c("STURM"),
               geo_level = "region_bld")

yrs <- seq(base_year, end_year, step_year)

if (!parallel) {

    for (run in runs) {

        if (energy_efficiency == "exogenous") {
            run <- paste(run, energy_efficiency, sep = "_")
        }

        run_scenario(
            run = run,
            scenario_name = run,
            sector = sector,
            path_in = path_in,
            path_rcode = path_rcode,
            path_out = path_out,
            file_inputs = file_inputs,
            file_scenarios = file_scenarios,
            geo_level_report = report$geo_level,
            yrs = yrs,
            report_var = report$var,
            report_type = report$type,
            region = region,
            energy_efficiency = energy_efficiency,
            en_method = en_method
        )

    print("Scenario run completed!")
    duration <- difftime(Sys.time(), start_time, units = "secs")
    print(paste("Time to run scenario:", round(duration / 60, 0), "minutes."))
    }
} else {

    print(paste("Running in parallel with", num_cores, "cores"))

    # Function to log messages
    log_message <- function(message, log_file) {
        cat(message, file = log_file, append = TRUE, sep = "\n")
    }

    # Path to the log file
    log_file <- paste0(path_out, "run_scenario_log.txt")

    run_scenario_wrapper <- function(run) {
        tryCatch({
            result <- run_scenario(
            run = run,
            scenario_name = run,
            sector = sector,
            path_in = path_in,
            path_rcode = path_rcode,
            path_out = path_out,
            file_inputs = file_inputs,
            file_scenarios = file_scenarios,
            geo_level_report = report$geo_level,
            yrs = yrs,
            report_var = report$var,
            report_type = report$type,
            region = region,
            energy_efficiency = energy_efficiency,
            en_method = en_method
            )
            log_message(sprintf("Completed run '%s'", run), log_file)
            return(result)
        }, error = function(e) {
            error_message <- sprintf("Error in run '%s': %s", run, e$message)
            log_message(error_message, log_file)
            return(NULL)  # Return NULL in case of error
        })
    }
    # Run in parallel
    mclapply(runs, run_scenario_wrapper, mc.cores = num_cores)

}

