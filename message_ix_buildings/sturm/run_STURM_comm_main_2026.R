# ============================================================
# Run STURM commercial scenarios
#
# Geographic scope:
#   "global" = all regions
#   "eu27"   = EU-27
#   "custom" = selected region_bld codes
#
# Vacancy module:
#   Currently available only for EU-27 residential runs.
#   Commercial vacancy runs are therefore blocked.
# ============================================================

library(rstudioapi)
library(tidyverse)


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

script_path <- tryCatch(
  rstudioapi::getSourceEditorContext()$path,
  error = function(e) ""
)

if (nzchar(script_path)) {
  setwd(
    dirname(script_path)
  )
}

source(
  "./model/F10_scenario_runs_MESSAGE_2100.R"
)


# ------------------------------------------------------------
# 1. User settings
# ------------------------------------------------------------

# Scenarios
scenarios <- c(
  "R"
  # "N_r", "N_tp",
  # "S_r", "S_tp",
  # "C_r", "C_tp",
  # "A_r", "A_tp"
)


# Geographic scope:
#   "global", "eu27", or "custom"
region_run_mode <- "global"


# Used only when region_run_mode = "custom"
custom_region_bld <- c(
  "C-WEU-AUT"
)


# For custom runs only:
#   "global" = global commercial assumptions
#   "eu27"   = EU CircEUlar commercial assumptions
custom_input_scope <- "eu27"


# Vacancy mode:
#   "none" or "vacant"
#
# Any commercial request for "vacant" stops intentionally.
vacant_mode_selected <- "none"


# Reports
report_type_selected <- c(
  "STURM",
  "MESSAGE"
)


report_var_selected <- c(
  "energy",
  "material"
)


# Years
years_to_run <- c(
  2020,
  2025,
  2030
)


# Full time horizon:
#
# years_to_run <- c(
#   seq(2020, 2060, 5),
#   seq(2070, 2100, 10)
# )


# ------------------------------------------------------------
# 2. Validate user settings
# ------------------------------------------------------------

if (
  !region_run_mode %in% c(
    "global",
    "eu27",
    "custom"
  )
) {
  stop(
    paste(
      "`region_run_mode` must be",
      "'global', 'eu27', or 'custom'."
    )
  )
}


if (
  !vacant_mode_selected %in% c(
    "none",
    "vacant"
  )
) {
  stop(
    paste(
      "`vacant_mode_selected` must be",
      "'none' or 'vacant'."
    )
  )
}


if (
  region_run_mode == "custom" &&
  !custom_input_scope %in% c(
    "global",
    "eu27"
  )
) {
  stop(
    paste(
      "`custom_input_scope` must be",
      "'global' or 'eu27'."
    )
  )
}


if (
  region_run_mode == "custom" &&
  length(custom_region_bld) == 0
) {
  stop(
    paste(
      "`custom_region_bld` is empty.",
      "Add at least one region_bld code."
    )
  )
}


# Commercial vacancy safeguard
if (
  vacant_mode_selected == "vacant"
) {
  stop(
    paste(
      "Vacancy mode is currently available only",
      "for EU-27 residential runs."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 3. Paths
# ------------------------------------------------------------

rcode_path <- paste0(
  getwd(),
  "/model/"
)

data_path <- paste0(
  getwd(),
  "/data/"
)

input_path <- paste0(
  getwd(),
  "/data/input_csv_SSP_2023_comm/"
)

rout_path <- paste0(
  getwd(),
  "/output/"
)


dir.create(
  rout_path,
  recursive = TRUE,
  showWarnings = FALSE
)


# Commercial runs currently use no price table
prices <- NULL


# ------------------------------------------------------------
# 4. Define geographic scope
# ------------------------------------------------------------

eu27_region_bld <- c(
  "C-WEU-AUT",
  "C-WEU-BEL",
  "C-EEU-BGR",
  "C-WEU-CYP",
  "C-EEU-CZE",
  "C-WEU-DEU",
  "C-WEU-DNK",
  "C-EEU-EST",
  "C-WEU-ESP",
  "C-WEU-FIN",
  "C-WEU-FRA",
  "C-WEU-GRC",
  "C-EEU-HRV",
  "C-EEU-HUN",
  "C-WEU-IRL",
  "C-WEU-ITA",
  "C-EEU-LTU",
  "C-WEU-LUX",
  "C-EEU-LVA",
  "C-WEU-MLT",
  "C-WEU-NLD",
  "C-EEU-POL",
  "C-WEU-PRT",
  "C-EEU-ROU",
  "C-WEU-SWE",
  "C-EEU-SVN",
  "C-EEU-SVK"
)


if (
  region_run_mode == "global"
) {
  
  region_selection <- NULL
  
  region_label <- "global"
  
  input_scope <- "global"
  
} else if (
  region_run_mode == "eu27"
) {
  
  region_selection <- list(
    "region_bld",
    eu27_region_bld
  )
  
  region_label <- "EU27"
  
  input_scope <- "eu27"
  
} else {
  
  region_selection <- list(
    "region_bld",
    custom_region_bld
  )
  
  region_label <- paste0(
    "custom_",
    paste(
      sub(
        "^.*-",
        "",
        custom_region_bld
      ),
      collapse = "-"
    )
  )
  
  input_scope <- custom_input_scope
}


# ------------------------------------------------------------
# 5. Select commercial input list
# ------------------------------------------------------------

input_list_file <- if (
  input_scope == "eu27"
) {
  "input_list_comm_CircEUlar_2026.csv"
} else {
  "input_list_comm_2026_05_18_CE.csv"
}


cat("\n")
cat("========================================\n")
cat("STURM COMMERCIAL RUN SETTINGS\n")
cat("========================================\n")

cat(
  "Scenarios:       ",
  paste(
    scenarios,
    collapse = ", "
  ),
  "\n"
)

cat(
  "Region scope:    ",
  region_label,
  "\n"
)

cat(
  "Input scope:     ",
  input_scope,
  "\n"
)

cat(
  "Vacancy mode:    ",
  vacant_mode_selected,
  "\n"
)

cat(
  "Input list:      ",
  input_list_file,
  "\n"
)

cat(
  "Output folder:   ",
  rout_path,
  "\n"
)


# ------------------------------------------------------------
# 6. Run scenarios
# ------------------------------------------------------------

for (
  s in scenarios
) {
  
  cat("\n")
  cat("========================================\n")
  cat("STARTING COMMERCIAL SCENARIO:", s, "\n")
  cat("========================================\n")
  
  
  # Second safeguard in case only this section is executed
  if (
    vacant_mode_selected == "vacant"
  ) {
    stop(
      paste(
        "Vacancy mode is currently available only",
        "for EU-27 residential runs."
      ),
      call. = FALSE
    )
  }
  
  
  tryCatch({
    
    sturm_result <- run_scenario(
      
      # Scenario and sector
      run = s,
      sector = "comm",
      
      # Paths
      path_in = data_path,
      path_inputs = input_path,
      path_rcode = rcode_path,
      path_out = rout_path,
      
      # Inputs
      prices = prices,
      file_inputs = input_list_file,
      
      # Geography
      geo_level = "region_bld",
      geo_level_aggr = "region_gea",
      
      geo_levels = c(
        "region_bld",
        "region_gea"
      ),
      
      geo_level_report = "region_bld",
      
      region_select = region_selection,
      
      # Years and input format
      yrs = years_to_run,
      input_mode = "csv",
      
      # Commercial model settings
      mod_arch = "stock",
      mod_new = "exogenous",
      mod_vacant = vacant_mode_selected,
      
      # Reporting
      report_type = report_type_selected,
      report_var = report_var_selected
    )
    
    
    # --------------------------------------------------------
    # Rename STURM outputs to include geographic scope
    # --------------------------------------------------------
    
    if (
      "STURM" %in% report_type_selected
    ) {
      
      for (
        output_type in report_var_selected
      ) {
        
        old_file <- paste0(
          rout_path,
          "report_STURM_",
          s,
          "_comm_region_bld_",
          output_type,
          ".csv"
        )
        
        
        new_file <- paste0(
          rout_path,
          "report_STURM_",
          s,
          "_comm_region_bld_",
          output_type,
          "_",
          region_label,
          ".csv"
        )
        
        
        if (
          file.exists(old_file)
        ) {
          
          if (
            file.exists(new_file)
          ) {
            file.remove(
              new_file
            )
          }
          
          
          file.rename(
            old_file,
            new_file
          )
          
          
          cat("\nSTURM report written to:\n")
          cat(new_file, "\n")
        }
      }
    }
    
    
    # --------------------------------------------------------
    # Save MESSAGE report
    # --------------------------------------------------------
    
    if (
      "MESSAGE" %in% report_type_selected
    ) {
      
      message_output <- sturm_result
      
      
      if (
        is.data.frame(message_output) &&
        "commodity" %in% names(message_output)
      ) {
        
        message_output <- message_output %>%
          filter(
            !commodity %in% c(
              "comm_heat_v_no_heat",
              "comm_hotwater_v_no_heat"
            )
          )
      }
      
      
      message_file <- paste0(
        rout_path,
        "report_MESSAGE_comm_",
        region_label,
        "_",
        s,
        ".csv"
      )
      
      
      write_csv(
        message_output,
        message_file
      )
      
      
      cat("\nMESSAGE report written to:\n")
      cat(message_file, "\n")
    }
    
    
    cat(
      "\nFINISHED COMMERCIAL SCENARIO:",
      s,
      "\n"
    )
    
  }, error = function(e) {
    
    cat(
      "\nERROR IN COMMERCIAL SCENARIO:",
      s,
      "\n"
    )
    
    cat(
      conditionMessage(e),
      "\n"
    )
  })
}


# ------------------------------------------------------------
# 7. List output files
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat("OUTPUT FILES\n")
cat("========================================\n")


print(
  list.files(
    rout_path,
    pattern = "\\.csv$",
    full.names = FALSE
  )
)

