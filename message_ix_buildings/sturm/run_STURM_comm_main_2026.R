# ============================================================
# Run STURM commercial scenarios
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
  setwd(dirname(script_path))
}

source("./model/F10_scenario_runs_MESSAGE_2100.R")


# ------------------------------------------------------------
# 1. User settings
# ------------------------------------------------------------

# Circular-economy scenarios:
# R, N_r, N_tp, S_r, S_tp, C_r, C_tp,
# A_r, A_tp, E_r, E_tp
#
# SSP scenarios:
# SSP1, SSP2, SSP3, SSP4, SSP5, SSP1_LED, SSP2_LED
#
# Select only the scenarios to run below.

scenarios <- c(
  "SSP1",
  "E_tp"
)

region_run_mode <- "global"   # global, eu27, custom

custom_region_bld <- c(
  "C-WEU-AUT"
)

custom_input_scope <- "eu27"  # global or eu27

vacant_mode_selected <- "none"  # commercial runs must use none

report_type_selected <- c(
  "STURM",
  "MESSAGE"
)

report_var_selected <- c(
  "energy",
  "material"
)

years_to_run <- c(
  2020,
  2025,
  2030
)

# Full horizon:
# years_to_run <- c(
#   seq(2020, 2060, 5),
#   seq(2070, 2100, 10)
# )


# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

root_path <- getwd()

rcode_path <- paste0(
  file.path(root_path, "model"),
  "/"
)

data_path <- paste0(
  file.path(root_path, "data"),
  "/"
)

input_path <- paste0(
  file.path(
    root_path,
    "data",
    "input_csv_SSP_2023_comm"
  ),
  "/"
)

rout_path <- paste0(
  file.path(root_path, "output"),
  "/"
)

dir.create(
  rout_path,
  recursive = TRUE,
  showWarnings = FALSE
)

prices <- NULL


# ------------------------------------------------------------
# 3. Geographic scope
# ------------------------------------------------------------

eu27_region_bld <- c(
  "C-WEU-AUT", "C-WEU-BEL", "C-EEU-BGR", "C-WEU-CYP",
  "C-EEU-CZE", "C-WEU-DEU", "C-WEU-DNK", "C-EEU-EST",
  "C-WEU-ESP", "C-WEU-FIN", "C-WEU-FRA", "C-WEU-GRC",
  "C-EEU-HRV", "C-EEU-HUN", "C-WEU-IRL", "C-WEU-ITA",
  "C-EEU-LTU", "C-WEU-LUX", "C-EEU-LVA", "C-WEU-MLT",
  "C-WEU-NLD", "C-EEU-POL", "C-WEU-PRT", "C-EEU-ROU",
  "C-WEU-SWE", "C-EEU-SVN", "C-EEU-SVK"
)

if (
  region_run_mode == "custom" &&
  !custom_input_scope %in% c(
    "global",
    "eu27"
  )
) {
  stop(
    "`custom_input_scope` must be 'global' or 'eu27'.",
    call. = FALSE
  )
}

region_settings <- switch(
  region_run_mode,
  
  global = list(
    selection = NULL,
    label = "global",
    input_scope = "global"
  ),
  
  eu27 = list(
    selection = list(
      "region_bld",
      eu27_region_bld
    ),
    label = "EU27",
    input_scope = "eu27"
  ),
  
  custom = list(
    selection = list(
      "region_bld",
      custom_region_bld
    ),
    label = paste0(
      "custom_",
      paste(
        sub(
          "^.*-",
          "",
          custom_region_bld
        ),
        collapse = "-"
      )
    ),
    input_scope = custom_input_scope
  ),
  
  stop(
    "`region_run_mode` must be 'global', 'eu27', or 'custom'.",
    call. = FALSE
  )
)

region_selection <- region_settings$selection
region_label <- region_settings$label
input_scope <- region_settings$input_scope

if (vacant_mode_selected != "none") {
  stop(
    paste(
      "Commercial vacancy runs are not currently supported.",
      "Use `vacant_mode_selected <- \"none\"`."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 4. Inputs
# ------------------------------------------------------------

input_list_file <- if (
  input_scope == "eu27"
) {
  "input_list_comm_CircEUlar_2026.csv"
} else {
  "input_list_comm_2026_SSP_CE.csv"
}

input_list_path <- file.path(
  data_path,
  input_list_file
)

if (!file.exists(input_list_path)) {
  stop(
    paste(
      "Input list not found:",
      input_list_path
    ),
    call. = FALSE
  )
}

input_list_check <- read_csv(
  input_list_path,
  show_col_types = FALSE
)

missing_scenarios <- setdiff(
  scenarios,
  names(input_list_check)
)

if (length(missing_scenarios) > 0) {
  stop(
    paste(
      "Missing scenario columns:",
      paste(
        missing_scenarios,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 5. Helpers
# ------------------------------------------------------------

output_label <- region_label

rename_output <- function(
    old_file,
    new_file
) {
  
  if (!file.exists(old_file)) {
    return(invisible(FALSE))
  }
  
  if (file.exists(new_file)) {
    file.remove(new_file)
  }
  
  success <- file.rename(
    old_file,
    new_file
  )
  
  if (!success) {
    warning(
      paste(
        "Could not rename:",
        old_file
      )
    )
  }
  
  invisible(success)
}


# ------------------------------------------------------------
# 6. Run scenarios
# ------------------------------------------------------------

cat(
  "\nRunning: ",
  paste(
    scenarios,
    collapse = ", "
  ),
  "\nScope: ",
  region_label,
  "\nInput list: ",
  input_list_file,
  "\n",
  sep = ""
)

for (s in scenarios) {
  
  cat(
    "\nStarting ",
    s,
    "...\n",
    sep = ""
  )
  
  tryCatch({
    
    sturm_result <- run_scenario(
      run = s,
      sector = "comm",
      
      path_in = data_path,
      path_inputs = input_path,
      path_rcode = rcode_path,
      path_out = rout_path,
      
      prices = prices,
      file_inputs = input_list_file,
      input_mode = "csv",
      
      geo_level = "region_bld",
      geo_level_aggr = "region_gea",
      
      geo_levels = c(
        "region_bld",
        "region_gea"
      ),
      
      geo_level_report = "region_bld",
      region_select = region_selection,
      
      yrs = years_to_run,
      
      mod_arch = "stock",
      mod_new = "exogenous",
      mod_vacant = "none",
      
      report_type = report_type_selected,
      report_var = report_var_selected
    )
    
    if ("STURM" %in% report_type_selected) {
      
      walk(
        report_var_selected,
        function(output_type) {
          
          rename_output(
            file.path(
              rout_path,
              paste0(
                "report_STURM_",
                s,
                "_comm_region_bld_",
                output_type,
                ".csv"
              )
            ),
            file.path(
              rout_path,
              paste0(
                "report_STURM_",
                s,
                "_comm_region_bld_",
                output_type,
                "_",
                output_label,
                ".csv"
              )
            )
          )
        }
      )
    }
    
    if ("MESSAGE" %in% report_type_selected) {
      
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
      
      write_csv(
        message_output,
        file.path(
          rout_path,
          paste0(
            "report_MESSAGE_comm_",
            output_label,
            "_",
            s,
            ".csv"
          )
        )
      )
    }
    
    cat(
      "Finished ",
      s,
      ".\n",
      sep = ""
    )
    
  }, error = function(e) {
    
    cat(
      "Failed ",
      s,
      ": ",
      conditionMessage(e),
      "\n",
      sep = ""
    )
  })
}


# ------------------------------------------------------------
# 7. List matching outputs
# ------------------------------------------------------------

output_pattern <- paste0(
  "(",
  paste(
    scenarios,
    collapse = "|"
  ),
  ").*",
  output_label,
  "|",
  output_label,
  ".*(",
  paste(
    scenarios,
    collapse = "|"
  ),
  ")"
)

cat("\nCreated outputs:\n")

print(
  list.files(
    rout_path,
    pattern = output_pattern,
    full.names = FALSE
  )
)
