# ============================================================
# Run STURM commercial scenarios (global only) for BMT linking
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


# MESSAGE linking outputs (comm_sturm_<scenario>.csv)
dir_message_linking <- file.path(
  getwd(),
  "message_linking"
)

dir.create(
  dir_message_linking,
  recursive = TRUE,
  showWarnings = FALSE
)


# Shared scenario list loader (scenario_config.yaml)
source(
  file.path(
    dir_message_linking,
    "load_scenario_config.R"
  )
)


# ------------------------------------------------------------
# 1. User settings
# ------------------------------------------------------------

# Scenarios (from scenario_config.yaml)
scenarios <- load_scenarios()

# Energy price file (read from data_path); NULL = no price table.
price_file <- "input_prices_R12.csv"

# Reports:
report_type_selected <- c(
  "STURM",
  "MESSAGE"
)

report_var_selected <- c(
  "energy",
  "material"
)

# Geo resolution for all reports (MESSAGE and STURM):
#   "region_bld", "region_gea", "R11", or "R12"
report_geo_level <- c(
  "region_bld",
  "R12"
)

# Years
years_to_run <- c(
  seq(2020, 2060, 5),
  seq(2070, 2100, 10)
)

# Commercial model settings
mod_arch <- "stock"
mod_new <- "exogenous"
mod_ren <- "exogenous"
mod_vacant <- "none"


# ------------------------------------------------------------
# 2. Fixed run configuration (global scope)
# ------------------------------------------------------------

region_selection <- NULL
region_label <- "global"

input_list_file <- "input_list_comm_2026_07_01.csv"


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


# Energy prices (NULL price_file -> no price table)
if (is.null(price_file)) {
  prices <- NULL
} else {
  price_path <- paste0(
    data_path,
    price_file
  )
  if (!file.exists(price_path)) {
    stop(
      paste(
        "Price file not found:",
        price_path
      ),
      call. = FALSE
    )
  }
  prices <- read_csv(
    price_path,
    show_col_types = FALSE
  )
}


# ------------------------------------------------------------
# 4. Print run settings
# ------------------------------------------------------------

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
# 5. Run scenarios
# ------------------------------------------------------------

for (
  s in scenarios
) {
  
  cat("\n")
  cat("========================================\n")
  cat("STARTING COMMERCIAL SCENARIO:", s, "\n")
  cat("========================================\n")
  
  
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
      
      geo_level_report = report_geo_level,
      
      region_select = region_selection,
      
      # Years and input format
      yrs = years_to_run,
      input_mode = "csv",
      
      # Commercial model settings
      mod_arch = mod_arch,
      mod_new = mod_new,
      mod_ren = mod_ren,
      mod_vacant = mod_vacant,
      
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
    
    
    # --------------------------------------------------------
    # Save full result for MESSAGE linking
    # --------------------------------------------------------
    
    linking_file <- file.path(
      dir_message_linking,
      paste0("comm_sturm_", s, ".csv")
    )
    
    write.csv(
      sturm_result,
      linking_file,
      row.names = FALSE
    )
    
    
    cat("\nMESSAGE linking file written to:\n")
    cat(linking_file, "\n")
    
    
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
# 6. List output files
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
