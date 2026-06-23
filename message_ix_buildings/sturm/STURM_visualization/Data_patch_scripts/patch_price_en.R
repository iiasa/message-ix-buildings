# ============================================================
# Patch price_en input file so 2020 and 2025 match NPi baseline
#
# Source scenario file:
#   price_en_message_engage_NPi2020_600_2024_07.csv
#
# Baseline file:
#   price_en_message_engage_NPi_2024_07.csv
#
# Output:
#   price_en_message_engage_NPi2020_600_2024_07_baselineAligned_2020_2025.csv
# ============================================================

library(tidyverse)
library(readr)

# ----------------------------
# 1. File paths
# ----------------------------
input_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/data/input_csv_SSP_2023_resid"

file_scenario <- file.path(
  input_dir,
  "price_en_message_engage_NPi2020_600_2024_07.csv"
)

file_baseline <- file.path(
  input_dir,
  "price_en_message_engage_NPi_2024_07.csv"
)

file_out <- file.path(
  input_dir,
  "price_en_message_engage_NPi2020_600_2024_07_baselineAligned_2020_2025.csv"
)

cat("\nChecking files:\n")
cat("Scenario file:", file.exists(file_scenario), file_scenario, "\n")
cat("Baseline file:", file.exists(file_baseline), file_baseline, "\n")

if (!file.exists(file_scenario)) stop("Scenario file not found.")
if (!file.exists(file_baseline)) stop("Baseline file not found.")

# ----------------------------
# 2. Read files
# ----------------------------
price_scenario <- read_csv(file_scenario, show_col_types = FALSE)
price_baseline <- read_csv(file_baseline, show_col_types = FALSE)

cat("\nScenario columns:\n")
print(names(price_scenario))

cat("\nBaseline columns:\n")
print(names(price_baseline))

# ----------------------------
# 3. Basic checks
# ----------------------------
required_cols <- c("region_gea", "fuel", "year", "value")

missing_scenario <- setdiff(required_cols, names(price_scenario))
missing_baseline <- setdiff(required_cols, names(price_baseline))

if (length(missing_scenario) > 0) {
  stop(paste("Scenario file missing columns:", paste(missing_scenario, collapse = ", ")))
}

if (length(missing_baseline) > 0) {
  stop(paste("Baseline file missing columns:", paste(missing_baseline, collapse = ", ")))
}

# ----------------------------
# 4. Compare before patching
# ----------------------------
baseline_years <- c(2020, 2025)

price_compare_before <- price_scenario %>%
  filter(year %in% baseline_years) %>%
  rename(value_scenario_before = value) %>%
  left_join(
    price_baseline %>%
      filter(year %in% baseline_years) %>%
      rename(value_baseline = value),
    by = c("region_gea", "fuel", "year")
  ) %>%
  mutate(
    diff_abs_before = value_scenario_before - value_baseline,
    diff_pct_before = if_else(
      !is.na(value_baseline) & value_baseline != 0,
      diff_abs_before / value_baseline * 100,
      NA_real_
    )
  )

cat("\nDifferences before patching, summary by year and fuel:\n")
price_compare_before %>%
  group_by(year, fuel) %>%
  summarise(
    mean_scenario_before = mean(value_scenario_before, na.rm = TRUE),
    mean_baseline = mean(value_baseline, na.rm = TRUE),
    mean_diff_abs_before = mean(diff_abs_before, na.rm = TRUE),
    mean_diff_pct_before = mean(diff_pct_before, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, fuel) %>%
  print(n = Inf, width = Inf)

# ----------------------------
# 5. Patch 2020 and 2025 values
# ----------------------------
baseline_patch <- price_baseline %>%
  filter(year %in% baseline_years) %>%
  select(region_gea, fuel, year, value_baseline = value)

price_scenario_patched <- price_scenario %>%
  left_join(
    baseline_patch,
    by = c("region_gea", "fuel", "year")
  ) %>%
  mutate(
    value_original = value,
    value = if_else(
      year %in% baseline_years & !is.na(value_baseline),
      value_baseline,
      value
    )
  )

# ----------------------------
# 6. Check after patching
# ----------------------------
price_compare_after <- price_scenario_patched %>%
  filter(year %in% baseline_years) %>%
  select(region_gea, fuel, year, value_patched = value, value_original, value_baseline) %>%
  mutate(
    diff_abs_after = value_patched - value_baseline,
    diff_pct_after = if_else(
      !is.na(value_baseline) & value_baseline != 0,
      diff_abs_after / value_baseline * 100,
      NA_real_
    )
  )

cat("\nDifferences after patching, summary by year and fuel:\n")
price_compare_after %>%
  group_by(year, fuel) %>%
  summarise(
    mean_patched = mean(value_patched, na.rm = TRUE),
    mean_baseline = mean(value_baseline, na.rm = TRUE),
    mean_diff_abs_after = mean(diff_abs_after, na.rm = TRUE),
    mean_diff_pct_after = mean(diff_pct_after, na.rm = TRUE),
    max_abs_diff_after = max(abs(diff_abs_after), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, fuel) %>%
  print(n = Inf, width = Inf)

# ----------------------------
# 7. Save patched file
# ----------------------------
price_scenario_patched_to_save <- price_scenario_patched %>%
  select(all_of(names(price_scenario)))

write_csv(
  price_scenario_patched_to_save,
  file_out
)

cat("\nSaved patched file to:\n")
cat(file_out, "\n")

# ----------------------------
# 8. Save diagnostic comparison
# ----------------------------
diagnostic_out <- file.path(
  input_dir,
  "diagnostic_price_en_NPi2020_600_patch_2020_2025.csv"
)

price_compare_after %>%
  write_csv(diagnostic_out)

cat("\nSaved diagnostic comparison to:\n")
cat(diagnostic_out, "\n")

cat("\nDone.\n")