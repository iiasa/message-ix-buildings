# ============================================================
# Patch operational emission-factor file
#
# Goal:
#   Make NPi and 1p5C operational emission factors consistent
#   in 2020 and 2025.
#
# Logic:
#   - Keep original NPi values.
#   - Set 1p5C 2020 = NPi 2020.
#   - Set 1p5C 2025 = NPi 2025.
#   - Keep original 1p5C values from 2030 onward.
#
# Input:
#   emission_factors_ENGAGE.csv
#
# Output:
#   emission_factors_ENGAGE_baselineAligned_2020_2025.csv
# ============================================================

# ----------------------------
# 0. Packages
# ----------------------------
library(tidyverse)
library(readr)

# ----------------------------
# 1. File paths
# ----------------------------
input_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/reporting_emissions/emission_intensity_operational"

output_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization/emissions_diagnostics"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

file_in <- file.path(
  input_dir,
  "emission_factors_ENGAGE.csv"
)

file_out <- file.path(
  input_dir,
  "emission_factors_ENGAGE_baselineAligned_2020_2025.csv"
)

diagnostic_out <- file.path(
  output_dir,
  "diagnostic_emission_factors_ENGAGE_baselineAligned_2020_2025.csv"
)

cat("\nInput file:\n", file_in, "\n")
cat("\nOutput file:\n", file_out, "\n")

if (!file.exists(file_in)) {
  stop("Input file not found. Check file path.")
}

# ----------------------------
# 2. Read file
# ----------------------------
op_raw <- read_csv(file_in, show_col_types = FALSE)

cat("\nColumns in original file:\n")
print(names(op_raw))

cat("\nPreview:\n")
print(head(op_raw, 10), width = Inf)

# ----------------------------
# 3. Basic checks
# ----------------------------
required_base_cols <- c("clim_policy", "region_gea", "fuel")

missing_base_cols <- setdiff(required_base_cols, names(op_raw))

if (length(missing_base_cols) > 0) {
  stop(
    paste0(
      "Missing required columns:\n",
      paste(missing_base_cols, collapse = "\n")
    )
  )
}

year_cols <- names(op_raw)[str_detect(names(op_raw), "^\\d{4}$")]

if (length(year_cols) == 0) {
  stop("No year columns detected. Expected columns such as 2020, 2025, 2030.")
}

cat("\nDetected year columns:\n")
print(year_cols)

# ----------------------------
# 4. Convert to long format
# ----------------------------
op_long <- op_raw %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "emission_factor"
  ) %>%
  mutate(
    clim_policy = as.character(clim_policy),
    region_gea = as.character(region_gea),
    fuel = as.character(fuel),
    year = as.numeric(year),
    emission_factor = as.numeric(emission_factor)
  )

cat("\nClimate-policy pathways:\n")
print(sort(unique(op_long$clim_policy)))

cat("\nFuels:\n")
print(sort(unique(op_long$fuel)))

cat("\nYears by climate-policy pathway before patching:\n")
op_long %>%
  group_by(clim_policy) %>%
  summarise(
    years = paste(sort(unique(year)), collapse = ", "),
    .groups = "drop"
  ) %>%
  print(width = Inf)

# ----------------------------
# 5. Diagnostic before patching
# ----------------------------
baseline_years <- c(2020, 2025)

op_compare_before <- op_long %>%
  filter(
    clim_policy %in% c("NPi", "1p5C"),
    year %in% baseline_years
  ) %>%
  select(region_gea, fuel, year, clim_policy, emission_factor) %>%
  pivot_wider(
    names_from = clim_policy,
    values_from = emission_factor
  ) %>%
  mutate(
    diff_1p5C_vs_NPi = `1p5C` - NPi,
    pct_diff_1p5C_vs_NPi = if_else(
      !is.na(NPi) & NPi != 0,
      diff_1p5C_vs_NPi / NPi * 100,
      NA_real_
    )
  )

cat("\nBefore patching: 1p5C vs NPi differences in 2020/2025:\n")
op_compare_before %>%
  group_by(year, fuel) %>%
  summarise(
    mean_NPi = mean(NPi, na.rm = TRUE),
    mean_1p5C = mean(`1p5C`, na.rm = TRUE),
    mean_diff = mean(diff_1p5C_vs_NPi, na.rm = TRUE),
    mean_pct_diff = mean(pct_diff_1p5C_vs_NPi, na.rm = TRUE),
    max_abs_diff = max(abs(diff_1p5C_vs_NPi), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, fuel) %>%
  print(n = Inf, width = Inf)

# ----------------------------
# 6. Create baseline patch from NPi 2020 and 2025
# ----------------------------
baseline_patch <- op_long %>%
  filter(
    clim_policy == "NPi",
    year %in% baseline_years
  ) %>%
  select(
    region_gea,
    fuel,
    year,
    emission_factor_baseline = emission_factor
  )

# ----------------------------
# 7. Patch 1p5C values for 2020 and 2025
# ----------------------------
op_long_patched <- op_long %>%
  left_join(
    baseline_patch,
    by = c("region_gea", "fuel", "year")
  ) %>%
  mutate(
    emission_factor_original = emission_factor,
    emission_factor = if_else(
      clim_policy == "1p5C" &
        year %in% baseline_years &
        !is.na(emission_factor_baseline),
      emission_factor_baseline,
      emission_factor
    )
  )

# ----------------------------
# 8. Diagnostic after patching
# ----------------------------
op_compare_after <- op_long_patched %>%
  filter(
    clim_policy %in% c("NPi", "1p5C"),
    year %in% baseline_years
  ) %>%
  select(region_gea, fuel, year, clim_policy, emission_factor) %>%
  pivot_wider(
    names_from = clim_policy,
    values_from = emission_factor
  ) %>%
  mutate(
    diff_1p5C_vs_NPi = `1p5C` - NPi,
    pct_diff_1p5C_vs_NPi = if_else(
      !is.na(NPi) & NPi != 0,
      diff_1p5C_vs_NPi / NPi * 100,
      NA_real_
    )
  )

cat("\nAfter patching: 1p5C vs NPi difference summary:\n")
op_compare_after %>%
  group_by(year) %>%
  summarise(
    max_abs_diff = max(abs(diff_1p5C_vs_NPi), na.rm = TRUE),
    mean_abs_diff = mean(abs(diff_1p5C_vs_NPi), na.rm = TRUE),
    n_nonzero_diff = sum(abs(diff_1p5C_vs_NPi) > 1e-12, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(width = Inf)

cat("\nAfter patching: summary by year and fuel:\n")
op_compare_after %>%
  group_by(year, fuel) %>%
  summarise(
    mean_NPi = mean(NPi, na.rm = TRUE),
    mean_1p5C = mean(`1p5C`, na.rm = TRUE),
    mean_diff = mean(diff_1p5C_vs_NPi, na.rm = TRUE),
    max_abs_diff = max(abs(diff_1p5C_vs_NPi), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, fuel) %>%
  print(n = Inf, width = Inf)

# ----------------------------
# 9. Convert back to original wide format
# ----------------------------
id_cols <- setdiff(names(op_raw), year_cols)

op_patched_wide <- op_long_patched %>%
  select(
    all_of(id_cols),
    year,
    emission_factor
  ) %>%
  mutate(
    year = as.character(as.integer(year))
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = emission_factor
  )

# Restore original column order
op_patched_wide <- op_patched_wide %>%
  select(all_of(names(op_raw)))

# ----------------------------
# 10. Save patched file and diagnostics
# ----------------------------
write_csv(
  op_patched_wide,
  file_out
)

write_csv(
  op_compare_after,
  diagnostic_out
)

cat("\nSaved patched operational emission-factor file to:\n")
cat(file_out, "\n")

cat("\nSaved diagnostic comparison to:\n")
cat(diagnostic_out, "\n")

# ----------------------------
# 11. Quick plot after patching
# ----------------------------
plot_data <- op_long_patched %>%
  filter(
    clim_policy %in% c("NPi", "1p5C")
  ) %>%
  group_by(clim_policy, fuel, year) %>%
  summarise(
    mean_emission_factor = mean(emission_factor, na.rm = TRUE),
    .groups = "drop"
  )

p_check <- ggplot(
  plot_data,
  aes(
    x = year,
    y = mean_emission_factor,
    colour = clim_policy,
    linetype = clim_policy,
    group = clim_policy
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ fuel, scales = "free_y", ncol = 3) +
  scale_colour_manual(
    values = c("NPi" = "#4D4D4D", "1p5C" = "#B2182B"),
    name = "Climate-policy pathway"
  ) +
  scale_linetype_manual(
    values = c("NPi" = "solid", "1p5C" = "dashed"),
    name = "Climate-policy pathway"
  ) +
  scale_x_continuous(
    breaks = c(2020, 2025, 2030, 2050, 2100)
  ) +
  labs(
    title = "Baseline-aligned operational emission factors",
    x = NULL,
    y = expression("Emission factor (kgCO"[2]*"e/GJ)")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_check)

ggsave(
  filename = file.path(
    output_dir,
    "diagnostic_operational_emission_factors_baselineAligned_2020_2025.png"
  ),
  plot = p_check,
  width = 9,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

cat("\nDone.\n")