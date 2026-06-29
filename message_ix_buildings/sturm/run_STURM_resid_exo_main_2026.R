# ============================================================
# STURM RESIDENTIAL RUNNER
# SIMPLIFIED / EXOGENOUS MODEL
#
# Exogenous scenarios:
#   IND_SSP2
#   IND_SSP1
#   IND_SSP3
#
# Model configuration:
#   mod_new = "exogenous"
#   mod_ren = "exogenous"
#
# Reports:
#   STURM
#   MESSAGE
#
# Geographic selection:
#   countries_to_run <- "all"
#
# or:
#   countries_to_run <- c("AUT", "DEU", "FRA")
#
# Full region_bld codes are also accepted:
#   countries_to_run <- c("C-WEU-AUT", "C-EEU-POL")
#
# Input list:
#   data/input_list_resid_2026_exogenous.csv
#
# Parameter files:
#   data/input_csv_SSP_2023_resid_ex/
#
# Outputs:
#   output/resid_exogenous_<region label>/<scenario>/
#
# All final output filenames include "_exogenous".
# Shared model and reporting files are not modified.
# ============================================================

library(rstudioapi)
library(tidyverse)


# ------------------------------------------------------------
# 0. Set working directory and load STURM functions
# ------------------------------------------------------------

script_path <- tryCatch(
  rstudioapi::getSourceEditorContext()$path,
  error = function(e) ""
)

if (nzchar(script_path)) {
  setwd(dirname(script_path))
}

source(
  "./model/F10_scenario_runs_MESSAGE_2100.R"
)


# ------------------------------------------------------------
# 1. User settings
# ------------------------------------------------------------

# Exogenous residential scenarios
scenarios <- c(
  "IND_SSP2",
  "IND_SSP1",
  "IND_SSP3"
)


# Single-scenario test:
#
# scenarios <- c(
#   "IND_SSP3"
# )


# Model years
# years_to_run <- seq(
#   2020,
#   2050,
#   5
# )


# Short diagnostic run:

years_to_run <- c(
  2020,
  2025,
  2030
)


# Reports
report_type_selected <- c(
  "STURM",
  "MESSAGE"
)

report_var_selected <- c(
  "energy",
  "material"
)


# ------------------------------------------------------------
# Geographic selection
#
# Global:
#   countries_to_run <- "all"
#
# Subset using ISO3 codes:
#   countries_to_run <- c("AUT", "DEU", "FRA")
#
# Full region_bld codes also work:
#   countries_to_run <- c("C-WEU-AUT", "C-EEU-POL")
# ------------------------------------------------------------

countries_to_run <- "AUT"#"all"


# Output reporting geography:
#
# "region_bld" = retain country-level results
# "R12"        = aggregate selected countries to MESSAGE regions

geo_level_report_selected <- "region_bld"


# Label added to exported filenames
output_label <- "exogenous"


# ------------------------------------------------------------
# 2. Define core paths
#
# STURM concatenates some paths directly with filenames.
# Therefore, paths passed to run_scenario() must end with "/".
# ------------------------------------------------------------

rcode_path <- paste0(
  normalizePath(
    file.path(
      getwd(),
      "model"
    ),
    winslash = "/",
    mustWork = TRUE
  ),
  "/"
)


data_path <- paste0(
  normalizePath(
    file.path(
      getwd(),
      "data"
    ),
    winslash = "/",
    mustWork = TRUE
  ),
  "/"
)


input_path <- paste0(
  normalizePath(
    file.path(
      getwd(),
      "data",
      "input_csv_SSP_2023_resid_ex"
    ),
    winslash = "/",
    mustWork = TRUE
  ),
  "/"
)


input_list_file <- "input_list_resid_2026_exogenous.csv"

input_list_path <- paste0(
  data_path,
  input_list_file
)


# ------------------------------------------------------------
# 3. Validate core paths and input list
# ------------------------------------------------------------

required_directories <- c(
  rcode_path,
  data_path,
  input_path
)

missing_directories <- required_directories[
  !dir.exists(required_directories)
]

if (length(missing_directories) > 0) {
  stop(
    paste0(
      "The following required directories were not found:\n",
      paste(
        missing_directories,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


if (!file.exists(input_list_path)) {
  stop(
    paste0(
      "Exogenous residential input list not found:\n",
      input_list_path
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 4. Read and validate input list
# ------------------------------------------------------------

input_list_check <- read_csv(
  input_list_path,
  show_col_types = FALSE
)


if (!"name_parameter" %in% names(input_list_check)) {
  stop(
    "The input list does not contain `name_parameter`.",
    call. = FALSE
  )
}


missing_scenario_columns <- setdiff(
  scenarios,
  names(input_list_check)
)

if (length(missing_scenario_columns) > 0) {
  stop(
    paste0(
      "The following scenario columns are missing:\n",
      paste(
        missing_scenario_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


required_exogenous_parameters <- c(
  "pop",
  "pop_urt",
  "pop_clim",
  "shr_eneff_new",
  "shr_fuel_heat_new",
  "shr_eneff_ren",
  "shr_fuel_heat_ren",
  "shr_fuel_heat_sw",
  "rate_ren"
)


missing_exogenous_parameters <- setdiff(
  required_exogenous_parameters,
  input_list_check$name_parameter
)

if (length(missing_exogenous_parameters) > 0) {
  stop(
    paste0(
      "The following required parameters are missing:\n",
      paste(
        missing_exogenous_parameters,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 5. Resolve blank scenario cells for validation
#
# Some rows assign a filename only in IND_SSP2 and leave later
# scenario cells blank. For validation, filenames are carried
# from left to right across scenario columns.
# ------------------------------------------------------------

scenario_order <- c(
  "IND_SSP2",
  "IND_SSP1",
  "IND_SSP3"
)

scenario_order <- scenario_order[
  scenario_order %in% names(input_list_check)
]


input_list_resolved <- input_list_check %>%
  mutate(
    across(
      all_of(scenario_order),
      ~ na_if(
        str_trim(as.character(.x)),
        ""
      )
    )
  )


for (i in seq_along(scenario_order)) {
  
  if (i == 1) {
    next
  }
  
  current_scenario <- scenario_order[i]
  
  previous_scenarios <- rev(
    scenario_order[
      seq_len(i - 1)
    ]
  )
  
  
  input_list_resolved[[current_scenario]] <- pmap_chr(
    input_list_resolved[
      c(
        current_scenario,
        previous_scenarios
      )
    ],
    function(...) {
      
      candidate_values <- c(...)
      
      candidate_values <- candidate_values[
        !is.na(candidate_values) &
          str_trim(candidate_values) != ""
      ]
      
      if (length(candidate_values) == 0) {
        return(NA_character_)
      }
      
      candidate_values[1]
    }
  )
}


# ------------------------------------------------------------
# 6. Validate referenced input files
# ------------------------------------------------------------

listed_input_files <- input_list_resolved %>%
  select(
    all_of(scenarios)
  ) %>%
  unlist(
    use.names = FALSE
  ) %>%
  as.character() %>%
  na.omit() %>%
  str_trim() %>%
  unique()


listed_input_files <- listed_input_files[
  listed_input_files != ""
]


listed_input_files_csv <- ifelse(
  str_detect(
    listed_input_files,
    regex(
      "\\.csv$",
      ignore_case = TRUE
    )
  ),
  listed_input_files,
  paste0(
    listed_input_files,
    ".csv"
  )
)


missing_input_files <- listed_input_files_csv[
  !file.exists(
    paste0(
      input_path,
      listed_input_files_csv
    )
  )
]


if (length(missing_input_files) > 0) {
  stop(
    paste0(
      "The following files were not found in:\n",
      input_path,
      "\n\n",
      paste(
        missing_input_files,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 7. Validate population input structures
# ------------------------------------------------------------

validate_population_file <- function(
    parameter,
    filename
) {
  
  filename <- if (
    str_detect(
      filename,
      regex(
        "\\.csv$",
        ignore_case = TRUE
      )
    )
  ) {
    filename
  } else {
    paste0(
      filename,
      ".csv"
    )
  }
  
  
  file_path_current <- paste0(
    input_path,
    filename
  )
  
  
  dat <- read_csv(
    file_path_current,
    show_col_types = FALSE
  )
  
  
  expected_columns <- switch(
    parameter,
    
    pop = c(
      "region_bld",
      "year",
      "pop"
    ),
    
    pop_urt = c(
      "region_bld",
      "year",
      "urt",
      "pop_urt"
    ),
    
    pop_clim = c(
      "region_bld",
      "urt",
      "clim",
      "value"
    )
  )
  
  
  missing_columns <- setdiff(
    expected_columns,
    names(dat)
  )
  
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Invalid structure for `",
        parameter,
        "` in:\n",
        file_path_current,
        "\n\nMissing columns:\n",
        paste(
          missing_columns,
          collapse = ", "
        ),
        "\n\nColumns found:\n",
        paste(
          names(dat),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  
  invisible(TRUE)
}


for (scenario_name in scenarios) {
  
  population_rows <- input_list_resolved %>%
    filter(
      name_parameter %in% c(
        "pop",
        "pop_urt",
        "pop_clim"
      )
    ) %>%
    select(
      name_parameter,
      filename = all_of(scenario_name)
    )
  
  
  walk2(
    population_rows$name_parameter,
    population_rows$filename,
    validate_population_file
  )
}


# ------------------------------------------------------------
# 8. Identify available region_bld codes
#
# The population file for the first selected scenario is used
# to determine the available model regions.
# ------------------------------------------------------------

first_scenario <- scenarios[1]


pop_filename <- input_list_resolved %>%
  filter(
    name_parameter == "pop"
  ) %>%
  pull(
    all_of(first_scenario)
  )


if (
  length(pop_filename) == 0 ||
  is.na(pop_filename) ||
  str_trim(pop_filename) == ""
) {
  stop(
    paste0(
      "Could not resolve the population file for scenario ",
      first_scenario,
      "."
    ),
    call. = FALSE
  )
}


if (!str_detect(
  pop_filename,
  regex(
    "\\.csv$",
    ignore_case = TRUE
  )
)) {
  pop_filename <- paste0(
    pop_filename,
    ".csv"
  )
}


pop_region_file <- paste0(
  input_path,
  pop_filename
)


if (!file.exists(pop_region_file)) {
  stop(
    paste0(
      "Population file used to identify regions was not found:\n",
      pop_region_file
    ),
    call. = FALSE
  )
}


available_region_bld <- read_csv(
  pop_region_file,
  show_col_types = FALSE
) %>%
  distinct(
    region_bld
  ) %>%
  filter(
    !is.na(region_bld),
    region_bld != ""
  ) %>%
  pull(
    region_bld
  ) %>%
  sort()


# ------------------------------------------------------------
# 9. Resolve requested countries or regions
# ------------------------------------------------------------

countries_to_run <- as.character(
  countries_to_run
)


global_run <- (
  length(countries_to_run) == 1 &&
    tolower(
      str_trim(countries_to_run)
    ) == "all"
)


if (global_run) {
  
  selected_region_bld <- available_region_bld
  
  region_selection <- NULL
  
  region_label <- "global"
  
} else {
  
  requested_regions <- toupper(
    str_trim(
      countries_to_run
    )
  )
  
  
  selected_region_bld <- map_chr(
    requested_regions,
    function(requested_region) {
      
      # Accept a complete region_bld code
      if (requested_region %in% available_region_bld) {
        return(requested_region)
      }
      
      
      # Otherwise interpret the entry as an ISO3 code
      matching_regions <- available_region_bld[
        str_detect(
          available_region_bld,
          paste0(
            "-",
            requested_region,
            "$"
          )
        )
      ]
      
      
      if (length(matching_regions) == 0) {
        return(NA_character_)
      }
      
      
      if (length(matching_regions) > 1) {
        stop(
          paste0(
            "More than one region_bld code matched `",
            requested_region,
            "`:\n",
            paste(
              matching_regions,
              collapse = "\n"
            )
          ),
          call. = FALSE
        )
      }
      
      
      matching_regions
    }
  )
  
  
  unmatched_regions <- requested_regions[
    is.na(selected_region_bld)
  ]
  
  
  if (length(unmatched_regions) > 0) {
    stop(
      paste0(
        "The following requested countries or regions ",
        "were not found:\n",
        paste(
          unmatched_regions,
          collapse = "\n"
        ),
        "\n\nExamples of available region_bld codes:\n",
        paste(
          head(
            available_region_bld,
            30
          ),
          collapse = "\n"
        )
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
    sub(
      "^.*-",
      "",
      selected_region_bld
    ),
    collapse = "-"
  )
}


# ------------------------------------------------------------
# 10. Define output root after resolving geographic selection
# ------------------------------------------------------------

output_root_directory <- file.path(
  getwd(),
  "output",
  paste0(
    "resid_exogenous_",
    region_label
  )
)


dir.create(
  output_root_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


output_root_path <- paste0(
  normalizePath(
    output_root_directory,
    winslash = "/",
    mustWork = TRUE
  ),
  "/"
)


# ------------------------------------------------------------
# 11. Read energy prices
# ------------------------------------------------------------

price_file <- paste0(
  data_path,
  "input_prices_R12.csv"
)


if (!file.exists(price_file)) {
  stop(
    paste0(
      "Energy-price file not found:\n",
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
# 12. Helper: rename one output safely
# ------------------------------------------------------------

rename_output_file <- function(
    old_file,
    new_file
) {
  
  if (!file.exists(old_file)) {
    return(FALSE)
  }
  
  
  if (file.exists(new_file)) {
    file.remove(new_file)
  }
  
  
  rename_success <- file.rename(
    old_file,
    new_file
  )
  
  
  if (!rename_success) {
    warning(
      paste0(
        "Could not rename:\n",
        old_file,
        "\nTo:\n",
        new_file
      )
    )
  }
  
  
  rename_success
}


# ------------------------------------------------------------
# 13. Helper: rename internally exported STURM files
#
# This runs immediately after each scenario finishes and only
# within that scenario's dedicated output directory.
# ------------------------------------------------------------

rename_scenario_outputs <- function(
    scenario_name,
    scenario_output_path,
    output_label,
    geo_level_report
) {
  
  rename_log <- tibble(
    report = character(),
    old_file = character(),
    new_file = character(),
    renamed = logical()
  )
  
  
  sturm_report_types <- c(
    "energy",
    "material",
    "vintage",
    "vacant"
  )
  
  
  for (report_kind in sturm_report_types) {
    
    old_filename <- paste0(
      "report_STURM_",
      scenario_name,
      "_resid_",
      geo_level_report,
      "_",
      report_kind,
      ".csv"
    )
    
    
    new_filename <- paste0(
      "report_STURM_",
      scenario_name,
      "_resid_",
      geo_level_report,
      "_",
      output_label,
      "_",
      report_kind,
      ".csv"
    )
    
    
    old_file <- paste0(
      scenario_output_path,
      old_filename
    )
    
    new_file <- paste0(
      scenario_output_path,
      new_filename
    )
    
    
    if (file.exists(old_file)) {
      
      renamed <- rename_output_file(
        old_file,
        new_file
      )
      
      
      rename_log <- bind_rows(
        rename_log,
        tibble(
          report = paste0(
            "STURM_",
            report_kind
          ),
          old_file = old_filename,
          new_file = new_filename,
          renamed = renamed
        )
      )
    }
  }
  
  
  rename_log
}


# ------------------------------------------------------------
# 14. Print run configuration
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat("STURM EXOGENOUS RESIDENTIAL RUN\n")
cat("========================================\n")


cat(
  "Scenarios:        ",
  paste(
    scenarios,
    collapse = ", "
  ),
  "\n",
  sep = ""
)


cat(
  "Years:            ",
  paste(
    years_to_run,
    collapse = ", "
  ),
  "\n",
  sep = ""
)


cat(
  "Reports:          ",
  paste(
    report_type_selected,
    collapse = ", "
  ),
  "\n",
  sep = ""
)


cat(
  "Region label:     ",
  region_label,
  "\n",
  sep = ""
)


cat(
  "Report geography: ",
  geo_level_report_selected,
  "\n",
  sep = ""
)


cat(
  "Selected regions: ",
  if (
    global_run
  ) {
    paste0(
      "all ",
      length(selected_region_bld),
      " available region_bld codes"
    )
  } else {
    paste(
      selected_region_bld,
      collapse = ", "
    )
  },
  "\n",
  sep = ""
)


cat(
  "Input list:       ",
  input_list_path,
  "\n",
  sep = ""
)


cat(
  "Parameter folder: ",
  input_path,
  "\n",
  sep = ""
)


cat(
  "Output root:      ",
  output_root_path,
  "\n",
  sep = ""
)


cat("New-build mode:   exogenous\n")
cat("Renovation mode:  exogenous\n")
cat("Vacancy mode:     none\n")
cat("Output label:     exogenous\n")


# ------------------------------------------------------------
# 15. Initialise run containers
# ------------------------------------------------------------

run_results <- vector(
  mode = "list",
  length = length(scenarios)
)

names(run_results) <- scenarios


scenario_output_paths <- vector(
  mode = "character",
  length = length(scenarios)
)

names(scenario_output_paths) <- scenarios


run_errors <- vector(
  mode = "character",
  length = length(scenarios)
)

names(run_errors) <- scenarios


# ------------------------------------------------------------
# 16. Run scenarios
# ------------------------------------------------------------

for (scenario_name in scenarios) {
  
  cat("\n")
  cat("========================================\n")
  cat(
    "STARTING SCENARIO: ",
    scenario_name,
    "\n",
    sep = ""
  )
  cat("========================================\n")
  
  
  scenario_output_directory <- file.path(
    output_root_directory,
    scenario_name
  )
  
  
  dir.create(
    scenario_output_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  scenario_output_path <- paste0(
    normalizePath(
      scenario_output_directory,
      winslash = "/",
      mustWork = TRUE
    ),
    "/"
  )
  
  
  scenario_output_paths[[scenario_name]] <-
    scenario_output_path
  
  
  cat(
    "Scenario output folder:\n",
    scenario_output_path,
    "\n",
    sep = ""
  )
  
  
  run_results[[scenario_name]] <- tryCatch({
    
    result <- run_scenario(
      
      # Scenario and sector
      run = scenario_name,
      sector = "resid",
      
      # Paths
      path_in = data_path,
      path_inputs = input_path,
      path_rcode = rcode_path,
      path_out = scenario_output_path,
      
      # Inputs
      prices = prices,
      file_inputs = input_list_file,
      input_mode = "csv",
      
      # Geography
      geo_level = "region_bld",
      geo_level_aggr = "region_gea",
      
      geo_levels = c(
        "region_bld",
        "region_gea"
      ),
      
      geo_level_report = geo_level_report_selected,
      
      region_select = region_selection,
      
      # Years
      yrs = years_to_run,
      
      # Exogenous residential configuration
      mod_arch = "stock",
      mod_new = "exogenous",
      mod_ren = "exogenous",
      mod_vacant = "none",
      
      # Reporting
      report_type = report_type_selected,
      report_var = report_var_selected
    )
    
    
    # Rename internally generated STURM reports immediately.
    rename_log <- rename_scenario_outputs(
      scenario_name = scenario_name,
      scenario_output_path = scenario_output_path,
      output_label = output_label,
      geo_level_report = geo_level_report_selected
    )
    
    
    cat(
      "\nFINISHED SCENARIO: ",
      scenario_name,
      "\n",
      sep = ""
    )
    
    
    if (nrow(rename_log) > 0) {
      
      cat("\nRenamed STURM files:\n")
      
      print(
        rename_log,
        n = Inf,
        width = Inf
      )
    }
    
    
    result
    
  }, error = function(e) {
    
    error_message <- conditionMessage(e)
    
    run_errors[[scenario_name]] <<-
      error_message
    
    
    cat(
      "\nERROR IN SCENARIO: ",
      scenario_name,
      "\n\n",
      sep = ""
    )
    
    cat(
      error_message,
      "\n"
    )
    
    
    NULL
  })
}


# ------------------------------------------------------------
# 17. Save returned MESSAGE outputs
#
# These are written directly with "exogenous" in the filename.
# ------------------------------------------------------------

if ("MESSAGE" %in% report_type_selected) {
  
  for (scenario_name in scenarios) {
    
    message_output <- run_results[[scenario_name]]
    
    
    if (!is.data.frame(message_output)) {
      next
    }
    
    
    if ("commodity" %in% names(message_output)) {
      
      message_output <- message_output %>%
        filter(
          !commodity %in% c(
            "resid_heat_v_no_heat",
            "resid_hotwater_v_no_heat"
          )
        )
    }
    
    
    scenario_output_path <-
      scenario_output_paths[[scenario_name]]
    
    
    message_file <- paste0(
      scenario_output_path,
      "report_MESSAGE_",
      scenario_name,
      "_resid_",
      geo_level_report_selected,
      "_",
      output_label,
      ".csv"
    )
    
    
    write_csv(
      message_output,
      message_file
    )
    
    
    cat(
      "\nMESSAGE output written directly to:\n",
      message_file,
      "\n",
      sep = ""
    )
  }
}


# ------------------------------------------------------------
# 18. Report run status
# ------------------------------------------------------------

completed_scenarios <- scenarios[
  map_lgl(
    run_results,
    ~ !is.null(.x)
  )
]


failed_scenarios <- setdiff(
  scenarios,
  completed_scenarios
)


cat("\n")
cat("========================================\n")
cat("RUN STATUS\n")
cat("========================================\n")


if (length(completed_scenarios) > 0) {
  
  cat(
    "\nCompleted scenarios:\n",
    paste(
      completed_scenarios,
      collapse = "\n"
    ),
    "\n",
    sep = ""
  )
}


if (length(failed_scenarios) == 0) {
  
  cat(
    "\nAll exogenous residential scenarios completed successfully.\n"
  )
  
} else {
  
  cat(
    "\nFailed scenarios:\n",
    paste(
      failed_scenarios,
      collapse = "\n"
    ),
    "\n",
    sep = ""
  )
  
  
  for (scenario_name in failed_scenarios) {
    
    cat(
      "\n",
      scenario_name,
      " error:\n",
      run_errors[[scenario_name]],
      "\n",
      sep = ""
    )
  }
}


# ------------------------------------------------------------
# 19. List final output files
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat("FINAL OUTPUT FILES\n")
cat("========================================\n")


for (scenario_name in scenarios) {
  
  scenario_output_path <-
    scenario_output_paths[[scenario_name]]
  
  
  cat(
    "\n",
    scenario_name,
    ":\n",
    sep = ""
  )
  
  
  scenario_files <- list.files(
    scenario_output_path,
    recursive = TRUE,
    full.names = FALSE
  )
  
  
  if (length(scenario_files) == 0) {
    
    cat(
      "No output files found.\n"
    )
    
  } else {
    
    print(
      scenario_files
    )
  }
}
