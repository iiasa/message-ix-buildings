# ============================================================
# Patch embodied emission-factor file
#
# Goal:
#   Make NPi and 1p5C consistent in 2020 and 2025.
#
# Logic:
#   - Keep original NPi values.
#   - Create NPi 2025 by linear interpolation between NPi 2020 and NPi 2030.
#   - Set 1p5C 2020 equal to NPi 2020.
#   - Set 1p5C 2025 equal to interpolated NPi 2025.
#   - Keep original 1p5C values from 2030 onward.
#
# Input:
#   ghg_image_r12_2025-05-06_edit.csv
#
# Output:
#   ghg_image_r12_2025-05-06_edit_baselineAligned_2020_2025.csv
# ============================================================

# ----------------------------
# 0. Packages
# ----------------------------
library(tidyverse)
library(readr)

# ----------------------------
# 1. File paths
# ----------------------------
input_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/reporting_emissions/emission_intensity_ecoinvent"

file_in <- file.path(
  input_dir,
  "ghg_image_r12_2025-05-06_edit.csv"
)

file_out <- file.path(
  input_dir,
  "ghg_image_r12_2025-05-06_edit_baselineAligned_2020_2025.csv"
)

diagnostic_out <- file.path(
  input_dir,
  "diagnostic_ghg_image_r12_baselineAligned_2020_2025.csv"
)

cat("\nInput file:\n", file_in, "\n")
cat("\nOutput file:\n", file_out, "\n")

if (!file.exists(file_in)) {
  stop("Input file not found. Check file path.")
}

# ----------------------------
# 2. Read file
# ----------------------------
ghg_raw <- read_csv(file_in, show_col_types = FALSE)

cat("\nColumns:\n")
print(names(ghg_raw))

cat("\nPreview:\n")
print(head(ghg_raw, 10), width = Inf)

# ----------------------------
# 3. Basic checks
# ----------------------------
required_cols <- c(
  "scenario_supply",
  "region_gea",
  "material",
  "production",
  "year",
  "emission_factor",
  "unit"
)

missing_cols <- setdiff(required_cols, names(ghg_raw))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Missing required columns:\n",
      paste(missing_cols, collapse = "\n")
    )
  )
}

ghg <- ghg_raw %>%
  mutate(
    scenario_supply = as.character(scenario_supply),
    region_gea = as.character(region_gea),
    material = as.character(material),
    production = as.character(production),
    year = as.numeric(year),
    emission_factor = as.numeric(emission_factor),
    unit = as.character(unit)
  )

cat("\nScenario supplies:\n")
print(sort(unique(ghg$scenario_supply)))

cat("\nYears by scenario supply before patching:\n")
ghg %>%
  group_by(scenario_supply) %>%
  summarise(
    years = paste(sort(unique(year)), collapse = ", "),
    .groups = "drop"
  ) %>%
  print(width = Inf)

# ----------------------------
# 4. Create NPi 2025 by interpolation
# ----------------------------
# Interpolate within each region/material/production/unit group:
# NPi_2025 = NPi_2020 + 0.5 * (NPi_2030 - NPi_2020)

npi_2020_2030 <- ghg %>%
  filter(
    scenario_supply == "NPi",
    year %in% c(2020, 2030)
  ) %>%
  select(
    region_gea,
    material,
    production,
    unit,
    year,
    emission_factor
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = emission_factor,
    names_prefix = "ef_"
  )

# Check missing 2020/2030 values
missing_interp <- npi_2020_2030 %>%
  filter(is.na(ef_2020) | is.na(ef_2030))

if (nrow(missing_interp) > 0) {
  cat("\nWARNING: Some NPi groups are missing 2020 or 2030 values.\n")
  print(missing_interp, n = Inf, width = Inf)
}

npi_2025 <- npi_2020_2030 %>%
  filter(!is.na(ef_2020), !is.na(ef_2030)) %>%
  transmute(
    scenario_supply = "NPi",
    region_gea,
    material,
    production,
    year = 2025,
    emission_factor = ef_2020 + 0.5 * (ef_2030 - ef_2020),
    unit
  )

cat("\nCreated NPi 2025 rows:\n")
print(npi_2025 %>% count(year), n = Inf)

# ----------------------------
# 5. Create 1p5C 2020 and 2025 baseline-aligned rows
# ----------------------------
onep5c_2020 <- ghg %>%
  filter(
    scenario_supply == "NPi",
    year == 2020
  ) %>%
  mutate(
    scenario_supply = "1p5C"
  )

onep5c_2025 <- npi_2025 %>%
  mutate(
    scenario_supply = "1p5C"
  )

baseline_rows <- bind_rows(
  npi_2025,
  onep5c_2020,
  onep5c_2025
)

cat("\nBaseline-aligned rows to add/replace:\n")
baseline_rows %>%
  count(scenario_supply, year) %>%
  arrange(scenario_supply, year) %>%
  print(n = Inf)

# ----------------------------
# 6. Remove old rows for patched years, then add patched rows
# ----------------------------
# This makes the script safe even if 2025 or 1p5C 2020 rows already exist.

ghg_patched <- ghg %>%
  filter(
    !(
      scenario_supply == "NPi" &
        year == 2025
    ),
    !(
      scenario_supply == "1p5C" &
        year %in% c(2020, 2025)
    )
  ) %>%
  bind_rows(baseline_rows) %>%
  arrange(
    scenario_supply,
    region_gea,
    material,
    production,
    year
  )

# Restore original column order
ghg_patched <- ghg_patched %>%
  select(all_of(names(ghg_raw)))

cat("\nYears by scenario supply after patching:\n")
ghg_patched %>%
  group_by(scenario_supply) %>%
  summarise(
    years = paste(sort(unique(year)), collapse = ", "),
    .groups = "drop"
  ) %>%
  print(width = Inf)

# ----------------------------
# 7. Diagnostic check: NPi vs 1p5C in 2020 and 2025
# ----------------------------
diagnostic <- ghg_patched %>%
  filter(
    scenario_supply %in% c("NPi", "1p5C"),
    year %in% c(2020, 2025)
  ) %>%
  select(
    region_gea,
    material,
    production,
    unit,
    year,
    scenario_supply,
    emission_factor
  ) %>%
  pivot_wider(
    names_from = scenario_supply,
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

cat("\nDiagnostic: difference between 1p5C and NPi in 2020/2025:\n")
diagnostic %>%
  summarise(
    n_rows = n(),
    max_abs_diff = max(abs(diff_1p5C_vs_NPi), na.rm = TRUE),
    mean_abs_diff = mean(abs(diff_1p5C_vs_NPi), na.rm = TRUE),
    n_nonzero_diff = sum(abs(diff_1p5C_vs_NPi) > 1e-12, na.rm = TRUE)
  ) %>%
  print(width = Inf)

cat("\nDiagnostic by year:\n")
diagnostic %>%
  group_by(year) %>%
  summarise(
    n_rows = n(),
    max_abs_diff = max(abs(diff_1p5C_vs_NPi), na.rm = TRUE),
    mean_abs_diff = mean(abs(diff_1p5C_vs_NPi), na.rm = TRUE),
    n_nonzero_diff = sum(abs(diff_1p5C_vs_NPi) > 1e-12, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(width = Inf)

# ----------------------------
# 8. Save patched file and diagnostic
# ----------------------------
write_csv(
  ghg_patched,
  file_out
)

write_csv(
  diagnostic,
  diagnostic_out
)

cat("\nSaved patched file to:\n")
cat(file_out, "\n")

cat("\nSaved diagnostic to:\n")
cat(diagnostic_out, "\n")

# ----------------------------
# 9. Optional quick plot for WEU/EEU
# ----------------------------
plot_data <- ghg_patched %>%
  filter(
    region_gea %in% c("WEU", "EEU"),
    scenario_supply %in% c("NPi", "1p5C"),
    production == "primary"
  )

p_check <- ggplot(
  plot_data,
  aes(
    x = year,
    y = emission_factor,
    colour = scenario_supply,
    linetype = scenario_supply,
    group = scenario_supply
  )
) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 1.8) +
  facet_grid(region_gea ~ material, scales = "free_y") +
  scale_colour_manual(
    values = c("NPi" = "#4D4D4D", "1p5C" = "#B2182B"),
    name = "Scenario supply"
  ) +
  scale_linetype_manual(
    values = c("NPi" = "solid", "1p5C" = "dashed"),
    name = "Scenario supply"
  ) +
  scale_x_continuous(
    breaks = c(2020, 2025, 2030, 2050, 2100)
  ) +
  labs(
    title = "Baseline-aligned embodied emission factors for WEU and EEU",
    x = NULL,
    y = expression("Emission factor (kgCO"[2]*"e/kg)")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_check)

diagnostic %>%
  group_by(year) %>%
  summarise(
    max_abs_diff = max(abs(diff_1p5C_vs_NPi), na.rm = TRUE),
    mean_abs_diff = mean(abs(diff_1p5C_vs_NPi), na.rm = TRUE),
    n_nonzero_diff = sum(abs(diff_1p5C_vs_NPi) > 1e-12, na.rm = TRUE),
    .groups = "drop"
  )
