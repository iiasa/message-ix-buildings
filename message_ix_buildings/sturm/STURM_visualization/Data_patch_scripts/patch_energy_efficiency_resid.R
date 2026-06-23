# ============================================================
# Patch efficiency / policy input files to align 2020 and 2025
# with Reference / SSP2 baseline
#
# This script ONLY creates patched CSV input files.
# It does NOT read or modify the input-list CSV.
#
# Rule:
#   2020 and 2025 = baseline/reference file values
#   2030 onward   = original target file values
# ============================================================

library(tidyverse)
library(readr)

# ----------------------------
# 1. Path
# ----------------------------

input_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/data/input_csv_SSP_2023_resid"

baseline_years <- c(2020, 2025)

# ----------------------------
# 2. Patch plan
# ----------------------------
# target_file   = file currently used by Efficiency / Climate policy
# baseline_file = file used by Reference / circular economy scenarios
#
# IMPORTANT:
# Check the last three baseline files against your input list.
# If the Reference/circular input-list uses different filenames,
# edit baseline_file manually before running.

patch_plan <- tibble::tribble(
  ~target_file,                         ~baseline_file,
  "eff_heat_ssp1",                      "eff_heat_ssp2",
  "eff_cool_ssp1",                      "eff_cool_ssp2",
  "heat_operation_hours_ssp1",          "heat_operation_hours_ssp2",
  "ren_energy_savings_ssp1",            "ren_energy_savings_ssp2",
  "rate_switch_fuel_heat_ssp2_LED",     "rate_switch_fuel_heat_ssp2",
  "ren_rate_en_min_REN_H",              "ren_rate_en_min",
  "ren_rate_en_max_ssp2_REN_H",         "ren_rate_en_max_ssp2"
)

# ----------------------------
# 3. Helper functions
# ----------------------------

clean_name <- function(x) {
  x %>%
    as.character() %>%
    str_remove("\\.csv$") %>%
    str_trim()
}

add_csv <- function(x) {
  paste0(clean_name(x), ".csv")
}

make_patched_name <- function(x) {
  paste0(clean_name(x), "_baselineAligned_2020_2025")
}

read_input_file <- function(file_base) {
  file_path <- file.path(input_dir, add_csv(file_base))
  
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  read_csv(file_path, show_col_types = FALSE)
}

patch_file <- function(target_file, baseline_file) {
  
  target_file <- clean_name(target_file)
  baseline_file <- clean_name(baseline_file)
  
  cat("\n============================================================\n")
  cat("Target file:   ", target_file, "\n")
  cat("Baseline file: ", baseline_file, "\n")
  
  target_raw <- read_input_file(target_file)
  baseline_raw <- read_input_file(baseline_file)
  
  if (!all(c("year", "value") %in% names(target_raw))) {
    stop("Target file must have columns 'year' and 'value': ", target_file)
  }
  
  if (!all(c("year", "value") %in% names(baseline_raw))) {
    stop("Baseline file must have columns 'year' and 'value': ", baseline_file)
  }
  
  join_cols <- intersect(
    setdiff(names(target_raw), "value"),
    setdiff(names(baseline_raw), "value")
  )
  
  if (!"year" %in% join_cols) {
    stop("No common 'year' column found for: ", target_file)
  }
  
  cat("Join columns:\n")
  print(join_cols)
  
  baseline_values <- baseline_raw %>%
    filter(year %in% baseline_years) %>%
    select(all_of(join_cols), value_baseline = value)
  
  patched <- target_raw %>%
    left_join(baseline_values, by = join_cols) %>%
    mutate(
      value_original = value,
      value = if_else(
        year %in% baseline_years & !is.na(value_baseline),
        value_baseline,
        value
      )
    )
  
  diagnostic <- patched %>%
    filter(year %in% baseline_years) %>%
    summarise(
      target_file = target_file,
      baseline_file = baseline_file,
      n_rows = n(),
      n_changed = sum(abs(value - value_original) > 1e-12, na.rm = TRUE),
      max_abs_change = max(abs(value - value_original), na.rm = TRUE),
      mean_abs_change = mean(abs(value - value_original), na.rm = TRUE),
      .groups = "drop"
    )
  
  print(diagnostic, width = Inf)
  
  patched_out <- patched %>%
    select(all_of(names(target_raw)))
  
  output_file <- file.path(
    input_dir,
    add_csv(make_patched_name(target_file))
  )
  
  write_csv(patched_out, output_file)
  
  cat("Saved patched file:\n")
  cat(output_file, "\n")
  
  diagnostic
}

# ----------------------------
# 4. Run patching
# ----------------------------

cat("\nPatch plan:\n")
print(patch_plan, n = Inf)

diagnostics <- patch_plan %>%
  pmap_dfr(~ patch_file(
    target_file = ..1,
    baseline_file = ..2
  ))

cat("\n============================================================\n")
cat("All patch diagnostics:\n")
print(diagnostics, n = Inf, width = Inf)

cat("\nDone. Patched files created in:\n")
cat(input_dir, "\n")

