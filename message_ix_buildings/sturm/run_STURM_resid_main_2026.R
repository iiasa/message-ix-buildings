# ============================================================
# Run STURM residential scenarios
#
# Geographic scope:
#   "global" = all available countries
#   "eu27"   = EU-27 countries
#   "custom" = selected region_bld codes
#
# Vacancy:
#   Available only for EU-27 residential runs
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
#   "global" = global residential assumptions
#   "eu27"   = EU CircEUlar residential assumptions
custom_input_scope <- "eu27"


# Vacancy mode:
#   "none" or "vacant"
#
# "vacant" is allowed only when region_run_mode = "eu27"
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


# Short diagnostic run
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
# 2. Validation and vacancy safeguard
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
    ),
    call. = FALSE
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
    ),
    call. = FALSE
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
    ),
    call. = FALSE
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
    ),
    call. = FALSE
  )
}


# Vacancy is currently available only for EU-27 residential runs.
if (
  vacant_mode_selected == "vacant" &&
  region_run_mode != "eu27"
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
  "/data/input_csv_SSP_2023_resid/"
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


# ------------------------------------------------------------
# 4. Read residential energy prices
# ------------------------------------------------------------

price_file <- paste0(
  data_path,
  "input_prices_R12.csv"
)

if (!file.exists(price_file)) {
  stop(
    paste(
      "Price file not found:",
      price_file
    ),
    call. = FALSE
  )
}


prices <- read_csv(
  price_file,
  show_col_types = FALSE
)


# ------------------------------------------------------------
# 5. Define geographic scope
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
# 6. Select residential input list
# ------------------------------------------------------------

input_list_file <- if (
  input_scope == "eu27"
) {
  "input_list_resid_CircEUlar_2026.csv"
} else {
  "input_list_resid_2026_05_18_CE.csv"
}


input_list_path <- paste0(
  data_path,
  input_list_file
)

if (!file.exists(input_list_path)) {
  stop(
    paste(
      "Input-list file not found:",
      input_list_path
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 7. Validate vacancy inputs for EU-27 vacancy runs
# ------------------------------------------------------------

if (
  vacant_mode_selected == "vacant"
) {
  
  input_list_check <- read_csv(
    input_list_path,
    show_col_types = FALSE
  )
  
  
  vacancy_parameters <- c(
    "stock_vacant_base",
    "shr_vacant_base_arch",
    "shr_vacant_base_period",
    "rate_vacant_occ"
  )
  
  
  missing_vacancy_parameters <- setdiff(
    vacancy_parameters,
    input_list_check$name_parameter
  )
  
  
  if (
    length(missing_vacancy_parameters) > 0
  ) {
    stop(
      paste(
        "The following vacancy parameters are missing:",
        paste(
          missing_vacancy_parameters,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
}


# ------------------------------------------------------------
# 8. Output labels
# ------------------------------------------------------------

vacancy_suffix <- if (
  vacant_mode_selected == "vacant"
) {
  "_v"
} else {
  ""
}


output_label <- paste0(
  region_label,
  vacancy_suffix
)


cat("\n")
cat("========================================\n")
cat("STURM RESIDENTIAL RUN SETTINGS\n")
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
# 9. Run scenarios
# ------------------------------------------------------------

for (
  s in scenarios
) {
  
  cat("\n")
  cat("========================================\n")
  cat("STARTING RESIDENTIAL SCENARIO:", s, "\n")
  cat("========================================\n")
  
  cat(
    "Geographic scope:",
    region_label,
    "\n"
  )
  
  cat(
    "Input-list scope:",
    input_scope,
    "\n"
  )
  
  cat(
    "Vacant-building mode:",
    vacant_mode_selected,
    "\n"
  )
  
  
  # Second safeguard in case execution begins at this section.
  # This is deliberately outside tryCatch().
  if (
    vacant_mode_selected == "vacant" &&
    region_run_mode != "eu27"
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
      sector = "resid",
      
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
      
      # Residential model settings
      mod_arch = "stock",
      mod_new = "endogenous",
      mod_ren = "endogenous",
      mod_vacant = vacant_mode_selected,
      
      # Reporting
      report_type = report_type_selected,
      report_var = report_var_selected
    )
    
    
    # --------------------------------------------------------
    # Rename STURM energy and material outputs
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
          "_resid_region_bld_",
          output_type,
          ".csv"
        )
        
        
        new_file <- paste0(
          rout_path,
          "report_STURM_",
          s,
          "_resid_region_bld_",
          output_type,
          "_",
          output_label,
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
          
          
          renamed_successfully <- file.rename(
            old_file,
            new_file
          )
          
          
          if (
            renamed_successfully
          ) {
            cat("\nSTURM report written to:\n")
            cat(new_file, "\n")
          } else {
            warning(
              paste(
                "Could not rename:",
                old_file
              )
            )
          }
        }
      }
      
      
      # ------------------------------------------------------
      # Rename vacancy output when vacancy is enabled
      # ------------------------------------------------------
      
      if (
        vacant_mode_selected == "vacant"
      ) {
        
        vacancy_old_file <- paste0(
          rout_path,
          "report_STURM_",
          s,
          "_resid_region_bld_vacant.csv"
        )
        
        
        vacancy_new_file <- paste0(
          rout_path,
          "report_STURM_",
          s,
          "_resid_region_bld_vacant_",
          output_label,
          ".csv"
        )
        
        
        if (
          file.exists(vacancy_old_file)
        ) {
          
          if (
            file.exists(vacancy_new_file)
          ) {
            file.remove(
              vacancy_new_file
            )
          }
          
          
          renamed_successfully <- file.rename(
            vacancy_old_file,
            vacancy_new_file
          )
          
          
          if (
            renamed_successfully
          ) {
            cat("\nSTURM vacancy report written to:\n")
            cat(vacancy_new_file, "\n")
          } else {
            warning(
              paste(
                "Could not rename:",
                vacancy_old_file
              )
            )
          }
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
              "resid_heat_v_no_heat",
              "resid_hotwater_v_no_heat"
            )
          )
      }
      
      
      message_file <- paste0(
        rout_path,
        "report_MESSAGE_resid_",
        output_label,
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
      "\nFINISHED RESIDENTIAL SCENARIO:",
      s,
      "\n"
    )
    
  }, error = function(e) {
    
    cat(
      "\nERROR IN RESIDENTIAL SCENARIO:",
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
# 10. List output files
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
