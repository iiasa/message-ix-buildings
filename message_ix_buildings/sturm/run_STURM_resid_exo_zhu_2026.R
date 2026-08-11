# ============================================================
# Run the zhu renovation/fuel-switching STURM residential scenarios
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

# Available scenarios:
# IND_SSP1, IND_SSP2, IND_SSP2_zhu, IND_SSP3

scenarios <- c(
  "IND_SSP2_zhu"
  # "IND_SSP2"
  # "IND_SSP1",
  # "IND_SSP3"
)

# Use "all", ISO3 codes, or full region_bld codes.
countries_to_run <- "R32USA"

# Examples:
# countries_to_run <- c("AUT", "DEU", "FRA")
# countries_to_run <- c("C-WEU-AUT", "C-EEU-POL")

geo_level_report_selected <- "region_bld"

report_type_selected <- c(
  "STURM"
  # "MESSAGE"
)

report_var_selected <- c(
  "energy"
  # "material"
)

years_to_run <- seq(2020, 2050, 5)

# Full horizon:
# years_to_run <- seq(2020, 2050, 5)


# ------------------------------------------------------------
# 2. Paths and inputs
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
    "input_csv_SSP_2023_resid_exo_zhu"
  ),
  "/"
)

input_list_file <- "input_list_resid_2026_exo_zhu.csv"

input_list_path <- file.path(
  data_path,
  input_list_file
)

price_file <- file.path(
  data_path,
  "input_prices_R12.csv"
)

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

input_list <- read_csv(
  input_list_path,
  show_col_types = FALSE
)

prices <- read_csv(
  price_file,
  show_col_types = FALSE
)

missing_scenarios <- setdiff(
  scenarios,
  names(input_list)
)

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
# 3. Resolve population file and geographic selection
# ------------------------------------------------------------

scenario_order <- c(
  "IND_SSP2",
  "IND_SSP2_zhu",
  "IND_SSP1",
  "IND_SSP3"
)

resolve_input_filename <- function(
    parameter,
    scenario
) {
  
  scenario_position <- match(
    scenario,
    scenario_order
  )
  
  candidate_scenarios <- rev(
    scenario_order[
      seq_len(scenario_position)
    ]
  )
  
  values <- input_list %>%
    filter(
      name_parameter == parameter
    ) %>%
    select(
      any_of(candidate_scenarios)
    ) %>%
    unlist(
      use.names = FALSE
    ) %>%
    as.character() %>%
    str_trim()
  
  values <- values[
    !is.na(values) &
      values != ""
  ]
  
  if (length(values) == 0) {
    stop(
      paste(
        "Could not resolve input for",
        parameter,
        "in scenario",
        scenario
      ),
      call. = FALSE
    )
  }
  
  values[1]
}


add_csv_extension <- function(filename) {
  
  if (
    str_detect(
      filename,
      regex("\\.csv$", ignore_case = TRUE)
    )
  ) {
    filename
  } else {
    paste0(filename, ".csv")
  }
}


pop_filename <- resolve_input_filename(
  parameter = "pop",
  scenario = scenarios[1]
) %>%
  add_csv_extension()

pop_file <- file.path(
  input_path,
  pop_filename
)

if (!file.exists(pop_file)) {
  stop(
    paste("Population file not found:", pop_file),
    call. = FALSE
  )
}

available_region_bld <- read_csv(
  pop_file,
  show_col_types = FALSE
) %>%
  distinct(region_bld) %>%
  filter(
    !is.na(region_bld),
    region_bld != ""
  ) %>%
  pull(region_bld) %>%
  sort()


global_run <- (
  length(countries_to_run) == 1 &&
    tolower(str_trim(countries_to_run)) == "all"
)

if (global_run) {
  
  region_selection <- NULL
  region_label <- "global"
  
} else {
  
  requested_regions <- toupper(
    str_trim(
      as.character(countries_to_run)
    )
  )
  
  selected_region_bld <- map_chr(
    requested_regions,
    function(requested_region) {
      
      if (requested_region %in% available_region_bld) {
        return(requested_region)
      }
      
      matches <- available_region_bld[
        str_detect(
          available_region_bld,
          paste0("-", requested_region, "$")
        )
      ]
      
      if (length(matches) != 1) {
        return(NA_character_)
      }
      
      matches
    }
  )
  
  unmatched_regions <- requested_regions[
    is.na(selected_region_bld)
  ]
  
  if (length(unmatched_regions) > 0) {
    stop(
      paste(
        "Unknown countries or regions:",
        paste(unmatched_regions, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  selected_region_bld <- unique(
    selected_region_bld
  )
  
  region_selection <- list(
    "region_bld",
    selected_region_bld
  )
  
  region_label <- paste(
    sub("^.*-", "", selected_region_bld),
    collapse = "-"
  )
}


# ------------------------------------------------------------
# 4. Output helpers
# ------------------------------------------------------------

output_root <- file.path(
  root_path,
  "output",
  paste0(
    "resid_exo_zhu_",
    region_label
  )
)

dir.create(
  output_root,
  recursive = TRUE,
  showWarnings = FALSE
)


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
      paste("Could not rename:", old_file)
    )
  }
  
  invisible(success)
}


# ------------------------------------------------------------
# 5. Run scenarios
# ------------------------------------------------------------

cat(
  "\nRunning: ",
  paste(scenarios, collapse = ", "),
  "\nRegions: ",
  region_label,
  "\nInput list: ",
  input_list_file,
  "\n",
  sep = ""
)


for (scenario_name in scenarios) {
  
  scenario_output_path <- file.path(
    output_root,
    scenario_name
  )
  
  dir.create(
    scenario_output_path,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  scenario_output_path <- paste0(
    scenario_output_path,
    "/"
  )
  
  cat(
    "\nStarting ",
    scenario_name,
    "...\n",
    sep = ""
  )
  
  tryCatch({
    
    result <- run_scenario(
      run = scenario_name,
      sector = "resid",
      
      path_in = data_path,
      path_inputs = input_path,
      path_rcode = rcode_path,
      path_out = scenario_output_path,
      
      prices = prices,
      file_inputs = input_list_file,
      input_mode = "csv",
      
      geo_level = "region_bld",
      geo_level_aggr = "region_gea",
      
      geo_levels = c(
        "region_bld",
        "region_gea"
      ),
      
      geo_level_report = geo_level_report_selected,
      region_select = region_selection,
      
      yrs = years_to_run,
      
      mod_arch = "stock",
      mod_new = "exogenous",
      mod_ren = "zhu",
      mod_vacant = "none",
      
      report_type = report_type_selected,
      report_var = report_var_selected
    )
    
    
    # Rename STURM reports
    
    if ("STURM" %in% report_type_selected) {
      
      walk(
        report_var_selected,
        function(report_kind) {
          
          rename_output(
            old_file = file.path(
              scenario_output_path,
              paste0(
                "report_STURM_",
                scenario_name,
                "_resid_",
                geo_level_report_selected,
                "_",
                report_kind,
                ".csv"
              )
            ),
            
            new_file = file.path(
              scenario_output_path,
              paste0(
                "report_STURM_",
                scenario_name,
                "_resid_",
                geo_level_report_selected,
                "_exo_zhu_",
                report_kind,
                ".csv"
              )
            )
          )
        }
      )
    }
    
    
    # Save MESSAGE report
    
    if (
      "MESSAGE" %in% report_type_selected &&
      is.data.frame(result)
    ) {
      
      message_output <- result
      
      if ("commodity" %in% names(message_output)) {
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
          scenario_output_path,
          paste0(
            "report_MESSAGE_",
            scenario_name,
            "_resid_",
            geo_level_report_selected,
            "_exo_zhu.csv"
          )
        )
      )
    }
    
    cat(
      "Finished ",
      scenario_name,
      ".\n",
      sep = ""
    )
    
  }, error = function(e) {
    
    cat(
      "Failed ",
      scenario_name,
      ": ",
      conditionMessage(e),
      "\n",
      sep = ""
    )
  })
}


# ------------------------------------------------------------
# 6. List outputs
# ------------------------------------------------------------

cat("\nCreated outputs:\n")

for (scenario_name in scenarios) {
  
  scenario_output_path <- file.path(
    output_root,
    scenario_name
  )
  
  cat(
    "\n",
    scenario_name,
    ":\n",
    sep = ""
  )
  
  print(
    list.files(
      scenario_output_path,
      pattern = "\\.csv$",
      full.names = FALSE
    )
  )
}
