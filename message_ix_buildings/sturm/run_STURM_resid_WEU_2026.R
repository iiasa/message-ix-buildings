# ============================================================
# Run STURM residential scenarios
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

# Run SSP2 for MESSAGEix WEU, including Türkiye
scenarios <- "SSP2"

custom_region_bld <- c(
  "C-WEU-AUT", "C-WEU-BEL", "C-WEU-CHE", "C-WEU-CYP",
  "C-WEU-DEU", "C-WEU-DNK", "C-WEU-ESP", "C-WEU-FIN",
  "C-WEU-FRA", "C-WEU-GBR", "C-WEU-GRC", "C-WEU-IRL",
  "C-WEU-ISL", "C-WEU-ITA", "C-WEU-LUX", "C-WEU-MLT",
  "C-WEU-NLD", "C-WEU-NOR", "C-WEU-PRT", "C-WEU-SWE",
  "R32TUR"
)

vacant_mode_selected <- "none"  # none or vacant

report_type_selected <- c(
  "STURM",
  "MESSAGE"
)

report_var_selected <- c(
  "energy",
  "material"
)

#Full horizon:
years_to_run <- c(
  seq(2020, 2060, 5),
  seq(2070, 2100, 10)
)


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
    "input_csv_SSP_2023_resid"
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


# ------------------------------------------------------------
# 3. Geographic scope: MESSAGEix WEU
# ------------------------------------------------------------

if (
  length(custom_region_bld) == 0L ||
  anyNA(custom_region_bld) ||
  any(!nzchar(custom_region_bld))
) {
  stop(
    "`custom_region_bld` must contain valid WEU region codes.",
    call. = FALSE
  )
}

region_selection <- list(
  "region_bld",
  custom_region_bld
)

region_label <- "WEU"


if (!identical(vacant_mode_selected, "none")) {
  stop(
    "This WEU runner requires `vacant_mode_selected = 'none'`.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 4. Inputs
# ------------------------------------------------------------

input_list_file <- "input_list_resid_2026_SSP_CE.csv"

input_list_path <- file.path(data_path, input_list_file)
price_file <- file.path(data_path, "input_prices_R12.csv")

if (!file.exists(input_list_path)) {
  stop(
    paste("Input list not found:", input_list_path),
    call. = FALSE
  )
}

if (!file.exists(price_file)) {
  stop(
    paste("Price file not found:", price_file),
    call. = FALSE
  )
}

input_list_check <- read_csv(
  input_list_path,
  show_col_types = FALSE
)

prices <- read_csv(
  price_file,
  show_col_types = FALSE
)

missing_scenarios <- setdiff(scenarios, names(input_list_check))

if (length(missing_scenarios) > 0) {
  stop(
    paste(
      "Missing scenario columns:",
      paste(missing_scenarios, collapse = ", ")
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 5. Helpers
# ------------------------------------------------------------

output_label <- region_label

rename_output <- function(old_file, new_file) {
  
  if (!file.exists(old_file)) {
    return(invisible(FALSE))
  }
  
  if (file.exists(new_file)) {
    if (!file.remove(new_file)) {
      warning(paste("Could not replace:", new_file))
      return(invisible(FALSE))
    }
  }
  
  success <- file.rename(old_file, new_file)
  
  if (!success) {
    warning(paste("Could not rename:", old_file))
  }
  
  invisible(success)
}

# ------------------------------------------------------------
# 6. Run SSP2 for WEU
# ------------------------------------------------------------

cat(
  "\nRunning: ", paste(scenarios, collapse = ", "),
  "\nScope: ", region_label,
  "\nInput list: ", input_list_file,
  "\n",
  sep = ""
)

for (s in scenarios) {
  
  cat("\nStarting ", s, "...\n", sep = "")
  
  sturm_result <- run_scenario(
    run = s,
    sector = "resid",
    
    path_in = data_path,
    path_inputs = input_path,
    path_rcode = rcode_path,
    path_out = rout_path,
    
    prices = prices,
    file_inputs = input_list_file,
    input_mode = "csv",
    
    geo_level = "region_bld",
    geo_level_aggr = "region_gea",
    geo_levels = c("region_bld", "region_gea"),
    geo_level_report = "region_bld",
    
    region_select = region_selection,
    yrs = years_to_run,
    
    mod_arch = "stock",
    mod_new = "endogenous",
    mod_ren = "endogenous",
    mod_vacant = "none",
    
    report_type = report_type_selected,
    report_var = report_var_selected
  )
  
  if ("STURM" %in% report_type_selected) {
    
    walk(report_var_selected, function(output_type) {
      
      rename_output(
        file.path(
          rout_path,
          paste0(
            "report_STURM_", s,
            "_resid_region_bld_", output_type, ".csv"
          )
        ),
        file.path(
          rout_path,
          paste0(
            "report_STURM_", s,
            "_resid_region_bld_", output_type,
            "_", output_label, ".csv"
          )
        )
      )
    })
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
            "resid_heat_v_no_heat",
            "resid_hotwater_v_no_heat"
          )
        )
    }
    
    write_csv(
      message_output,
      file.path(
        rout_path,
        paste0(
          "report_MESSAGE_resid_",
          output_label, "_", s, ".csv"
        )
      )
    )
  }
  
  cat("Finished ", s, ".\n", sep = "")
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
