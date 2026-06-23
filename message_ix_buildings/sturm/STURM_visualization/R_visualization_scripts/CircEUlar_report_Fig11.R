# ============================================================
# Fig11_v3
# EU-27 residential and commercial greenhouse gas emissions
# Embodied and operational emissions under realistic scenarios
#
# Input:
#   emissions_outputs/resid_total_emissions_summary.csv
#   emissions_outputs/comm_total_emissions_summary.csv
#
# Expected columns:
#   sector, scenario, scenario_name, year,
#   embodied_MtCO2e, operational_MtCO2e, total_MtCO2e
#
# Notes:
#   - Residential scenarios:
#       Reference, Narrow R, Slow R, Close R,
#       Combined R, Efficiency R, Climate policy R
#   - Commercial scenarios:
#       Reference, Narrow R, Slow R, Close R,
#       Combined R, Efficiency R
#   - Climate policy is shown for residential only because it is not
#     represented for commercial buildings in the current modelling.
#   - Plot starts in 2025 because 2020 embodied emissions may reflect
#     a base-year accounting artefact.
# ============================================================

# ----------------------------
# 0. Packages
# ----------------------------
library(tidyverse)
library(readr)
library(scales)
library(grid)

# ----------------------------
# 1. File paths
# ----------------------------
input_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization/emissions_outputs/"
output_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

emissions_files <- c(
  resid = file.path(input_dir, "resid_total_emissions_summary.csv"),
  comm  = file.path(input_dir, "comm_total_emissions_summary.csv")
)

cat("\nChecking emissions files:\n")
print(emissions_files)

existing_files <- emissions_files[file.exists(emissions_files)]

if (length(existing_files) == 0) {
  stop(
    paste0(
      "No emissions summary files found in:\n",
      input_dir,
      "\nExpected files:\n",
      paste(emissions_files, collapse = "\n")
    )
  )
}

# ----------------------------
# 2. Read data
# ----------------------------
ghg_raw <- map_dfr(
  names(existing_files),
  function(sector_code) {
    
    f <- existing_files[[sector_code]]
    
    cat("\nLoaded emissions file:\n")
    cat(f, "\n")
    
    read_csv(f, show_col_types = FALSE) %>%
      mutate(
        source_sector_code = sector_code,
        source_file = basename(f)
      )
  }
)

cat("\nColumn names:\n")
print(names(ghg_raw))

cat("\nScenarios available:\n")
print(sort(unique(ghg_raw$scenario_name)))

cat("\nSector values available:\n")
print(sort(unique(ghg_raw$sector)))

cat("\nYears available:\n")
print(sort(unique(ghg_raw$year)))

# ----------------------------
# 3. Basic checks
# ----------------------------
required_cols <- c(
  "sector",
  "scenario",
  "scenario_name",
  "year",
  "embodied_MtCO2e",
  "operational_MtCO2e",
  "total_MtCO2e"
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

# ----------------------------
# 4. Scenario and sector settings
# ----------------------------
scenario_order <- c(
  "Reference",
  "Narrow R",
  "Slow R",
  "Close R",
  "Combined R",
  "Efficiency R",
  "Climate policy R"
)

plot_order <- c(
  "Reference",
  "Narrow",
  "Slow",
  "Close",
  "Combined",
  "Efficiency",
  "Climate policy"
)

sector_order <- c(
  "Residential",
  "Commercial"
)

# ----------------------------
# 5. Clean and keep realistic scenarios only
# ----------------------------
ghg_clean <- ghg_raw %>%
  mutate(
    sector_clean = case_when(
      str_to_lower(sector) %in% c("resid", "residential") ~ "Residential",
      str_to_lower(sector) %in% c("comm", "commercial") ~ "Commercial",
      source_sector_code == "resid" ~ "Residential",
      source_sector_code == "comm" ~ "Commercial",
      TRUE ~ as.character(sector)
    ),
    scenario_name_clean = case_when(
      scenario_name %in% c("Reference", "R") ~ "Reference",
      scenario_name %in% c("Narrow R", "N_r") ~ "Narrow R",
      scenario_name %in% c("Slow R", "S_r") ~ "Slow R",
      scenario_name %in% c("Close R", "C_r") ~ "Close R",
      scenario_name %in% c("Combined R", "A_r") ~ "Combined R",
      scenario_name %in% c("Efficiency R", "E_r", "Energy efficiency R") ~ "Efficiency R",
      scenario_name %in% c("Climate policy R", "CP_r", "Climate Policy R") ~ "Climate policy R",
      TRUE ~ as.character(scenario_name)
    ),
    scenario_plot = case_when(
      scenario_name_clean == "Reference" ~ "Reference",
      scenario_name_clean == "Narrow R" ~ "Narrow",
      scenario_name_clean == "Slow R" ~ "Slow",
      scenario_name_clean == "Close R" ~ "Close",
      scenario_name_clean == "Combined R" ~ "Combined",
      scenario_name_clean == "Efficiency R" ~ "Efficiency",
      scenario_name_clean == "Climate policy R" ~ "Climate policy",
      TRUE ~ scenario_name_clean
    )
  ) %>%
  filter(year >= 2025) %>%
  filter(scenario_name_clean %in% scenario_order) %>%
  filter(sector_clean %in% sector_order) %>%
  # Climate policy only for residential in current modelling
  filter(!(sector_clean == "Commercial" & scenario_name_clean == "Climate policy R")) %>%
  mutate(
    sector_clean = factor(sector_clean, levels = sector_order),
    scenario_name_clean = factor(scenario_name_clean, levels = scenario_order),
    scenario_plot = factor(scenario_plot, levels = plot_order)
  )

cat("\nGHG summary in selected years:\n")
ghg_clean %>%
  filter(year %in% c(2025, 2050, 2100)) %>%
  select(
    sector_clean,
    scenario_name_clean,
    scenario_plot,
    year,
    embodied_MtCO2e,
    operational_MtCO2e,
    total_MtCO2e,
    source_file
  ) %>%
  arrange(sector_clean, year, scenario_name_clean) %>%
  print(n = Inf, width = Inf)

# ----------------------------
# 6. Check missing / zero values
# ----------------------------
cat("\nNon-missing emissions values by sector and type:\n")

ghg_clean %>%
  group_by(sector_clean) %>%
  summarise(
    n_rows = n(),
    n_embodied_non_na = sum(!is.na(embodied_MtCO2e)),
    n_operational_non_na = sum(!is.na(operational_MtCO2e)),
    sum_embodied_MtCO2e = sum(embodied_MtCO2e, na.rm = TRUE),
    sum_operational_MtCO2e = sum(operational_MtCO2e, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(width = Inf)

# ----------------------------
# 7. Convert to long format
# ----------------------------
ghg_long <- ghg_clean %>%
  select(
    sector_clean,
    year,
    scenario,
    scenario_name_clean,
    scenario_plot,
    embodied_MtCO2e,
    operational_MtCO2e
  ) %>%
  pivot_longer(
    cols = c(embodied_MtCO2e, operational_MtCO2e),
    names_to = "emission_type",
    values_to = "emissions_MtCO2e"
  ) %>%
  mutate(
    emission_type = case_when(
      emission_type == "embodied_MtCO2e" ~ "Embodied",
      emission_type == "operational_MtCO2e" ~ "Operational",
      TRUE ~ emission_type
    ),
    emission_type = factor(
      emission_type,
      levels = c("Embodied", "Operational")
    ),
    emissions_GtCO2e = emissions_MtCO2e / 1000
  ) %>%
  filter(!is.na(emissions_GtCO2e))

cat("\nLong-format plotted data check:\n")

ghg_long %>%
  group_by(sector_clean, emission_type, scenario_plot) %>%
  summarise(
    n = n(),
    min_value = min(emissions_GtCO2e, na.rm = TRUE),
    max_value = max(emissions_GtCO2e, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(sector_clean, emission_type, scenario_plot) %>%
  print(n = Inf, width = Inf)

# ----------------------------
# 8. Plot settings
# ----------------------------
plot_colours <- c(
  "Reference"      = "#4D4D4D",
  "Narrow"         = "#D55E00",
  "Slow"           = "#0072B2",
  "Close"          = "#009E73",
  "Combined"       = "#CC79A7",
  "Efficiency"     = "#E69F00",
  "Climate policy" = "#B2182B"
)

theme_fig <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 10)
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    legend.key.width = unit(1.1, "cm"),
    legend.box = "vertical",
    legend.box.margin = margin(t = 6, r = 6, b = 6, l = 6),
    
    strip.text = element_text(
      face = "bold",
      size = 13
    ),
    strip.text.y.right = element_text(
      face = "bold",
      size = 13,
      angle = 270
    ),
    strip.background = element_blank(),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88"),
    panel.grid.major.y = element_line(colour = "grey85"),
    
    panel.spacing.x = unit(1.2, "lines"),
    panel.spacing.y = unit(0.9, "lines"),
    
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 18, 24, 10)
  )

# ============================================================
# 9. FIGURE 11
# ============================================================
fig11_v3 <- ggplot(
  ghg_long,
  aes(
    x = year,
    y = emissions_GtCO2e,
    colour = scenario_plot,
    group = scenario_plot
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.0) +
  facet_grid(
    rows = vars(sector_clean),
    cols = vars(emission_type),
    scales = "free_y"
  ) +
  scale_colour_manual(
    values = plot_colours,
    name = "Scenario",
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = c(2025, 2050, 2100),
    limits = c(2025, 2100),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(
    title = "EU-27 residential and commercial greenhouse gas emissions",
    subtitle = "Embodied and operational emissions under realistic scenarios",
    x = NULL,
    y = expression("GHG emissions (GtCO"[2]*"e/yr)")
  ) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_fig

print(fig11_v3)

# ----------------------------
# 10. Save outputs
# ----------------------------
file_stub <- "Fig11_v4_EU27_residential_commercial_GHG_emissions_realistic"

ggsave(
  filename = file.path(output_dir, paste0(file_stub, ".png")),
  plot = fig11_v3,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(output_dir, paste0(file_stub, ".tiff")),
  plot = fig11_v3,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(output_dir, paste0(file_stub, ".pdf")),
  plot = fig11_v3,
  width = 11,
  height = 8,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

# ----------------------------
# 11. Save plotted data
# ----------------------------
write_csv(
  ghg_clean,
  file.path(output_dir, paste0(file_stub, "_summary_data.csv"))
)

write_csv(
  ghg_long,
  file.path(output_dir, paste0(file_stub, "_long_data.csv"))
)

# ----------------------------
# 12. Optional reductions relative to Reference
# ----------------------------
ref_ghg <- ghg_clean %>%
  filter(scenario_name_clean == "Reference") %>%
  select(
    sector_clean,
    year,
    ref_embodied_MtCO2e = embodied_MtCO2e,
    ref_operational_MtCO2e = operational_MtCO2e,
    ref_total_MtCO2e = total_MtCO2e
  )

ghg_reductions <- ghg_clean %>%
  left_join(
    ref_ghg,
    by = c("sector_clean", "year")
  ) %>%
  mutate(
    embodied_reduction_pct =
      (ref_embodied_MtCO2e - embodied_MtCO2e) / ref_embodied_MtCO2e * 100,
    operational_reduction_pct =
      (ref_operational_MtCO2e - operational_MtCO2e) / ref_operational_MtCO2e * 100,
    total_reduction_pct =
      (ref_total_MtCO2e - total_MtCO2e) / ref_total_MtCO2e * 100
  )

cat("\nGHG reductions relative to Reference in selected years:\n")

ghg_reductions %>%
  filter(year %in% c(2030, 2050, 2100)) %>%
  select(
    sector_clean,
    year,
    scenario_name_clean,
    embodied_reduction_pct,
    operational_reduction_pct,
    total_reduction_pct
  ) %>%
  arrange(sector_clean, year, scenario_name_clean) %>%
  print(n = Inf, width = Inf)

write_csv(
  ghg_reductions,
  file.path(output_dir, paste0(file_stub, "_reductions_relative_to_reference.csv"))
)

cat("\nSaved Fig11_v3 and plotted data to:\n", output_dir, "\n")

######################
### EXTRACT DATA
######################
# ============================================================
# Extract exact Fig. 11 numbers for Results text
# Embodied, operational, and total GHG emissions
# Residential + commercial sectors
# ============================================================

cat("\n\n============================================================\n")
cat("Exact Fig. 11 numbers for Results text\n")
cat("Embodied, operational, and total GHG emissions\n")
cat("============================================================\n")

# ----------------------------
# Helper formatting
# ----------------------------
fmt_num <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  paste0(format(round(x, digits), nsmall = digits, trim = TRUE), "%")
}

text_years <- c(2025, 2030, 2050, 2100)

# ============================================================
# 1. Selected-year emissions by sector and scenario
# ============================================================

fig11_selected <- ghg_clean %>%
  filter(year %in% text_years) %>%
  select(
    sector_clean,
    year,
    scenario_name_clean,
    scenario_plot,
    embodied_MtCO2e,
    operational_MtCO2e,
    total_MtCO2e
  ) %>%
  mutate(
    embodied_GtCO2e = embodied_MtCO2e / 1000,
    operational_GtCO2e = operational_MtCO2e / 1000,
    total_GtCO2e = total_MtCO2e / 1000
  ) %>%
  arrange(sector_clean, year, scenario_name_clean)

cat("\nSelected-year emissions:\n")
cat("Units: GtCO2e/yr\n")

fig11_selected %>%
  select(
    sector_clean,
    year,
    scenario_name_clean,
    embodied_GtCO2e,
    operational_GtCO2e,
    total_GtCO2e
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# Wide format for easier reading
fig11_selected_wide <- fig11_selected %>%
  select(
    sector_clean,
    year,
    scenario_name_clean,
    embodied_GtCO2e,
    operational_GtCO2e,
    total_GtCO2e
  ) %>%
  pivot_wider(
    names_from = scenario_name_clean,
    values_from = c(embodied_GtCO2e, operational_GtCO2e, total_GtCO2e)
  ) %>%
  arrange(sector_clean, year)

cat("\nSelected-year emissions, wide format:\n")

fig11_selected_wide %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 2. Reference trajectory over time
# ============================================================

fig11_reference_change <- ghg_clean %>%
  filter(
    scenario_name_clean == "Reference",
    year %in% c(2025, 2050, 2100)
  ) %>%
  mutate(
    embodied_GtCO2e = embodied_MtCO2e / 1000,
    operational_GtCO2e = operational_MtCO2e / 1000,
    total_GtCO2e = total_MtCO2e / 1000
  ) %>%
  select(
    sector_clean,
    year,
    embodied_GtCO2e,
    operational_GtCO2e,
    total_GtCO2e
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = c(embodied_GtCO2e, operational_GtCO2e, total_GtCO2e),
    names_prefix = "y"
  ) %>%
  mutate(
    embodied_change_2025_2100_Gt = embodied_GtCO2e_y2100 - embodied_GtCO2e_y2025,
    operational_change_2025_2100_Gt = operational_GtCO2e_y2100 - operational_GtCO2e_y2025,
    total_change_2025_2100_Gt = total_GtCO2e_y2100 - total_GtCO2e_y2025,
    
    embodied_pct_change_2025_2100 =
      100 * (embodied_GtCO2e_y2100 / embodied_GtCO2e_y2025 - 1),
    operational_pct_change_2025_2100 =
      100 * (operational_GtCO2e_y2100 / operational_GtCO2e_y2025 - 1),
    total_pct_change_2025_2100 =
      100 * (total_GtCO2e_y2100 / total_GtCO2e_y2025 - 1)
  ) %>%
  arrange(sector_clean)

cat("\nReference emissions change over time:\n")

fig11_reference_change %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

cat("\nText-ready Reference trajectory sentences:\n")

fig11_reference_change %>%
  rowwise() %>%
  mutate(
    sentence = paste0(
      sector_clean,
      ": Reference embodied emissions change from ",
      fmt_num(embodied_GtCO2e_y2025, 2), " GtCO2e/yr in 2025 to ",
      fmt_num(embodied_GtCO2e_y2050, 2), " in 2050 and ",
      fmt_num(embodied_GtCO2e_y2100, 2), " in 2100. ",
      "Operational emissions change from ",
      fmt_num(operational_GtCO2e_y2025, 2), " GtCO2e/yr in 2025 to ",
      fmt_num(operational_GtCO2e_y2050, 2), " in 2050 and ",
      fmt_num(operational_GtCO2e_y2100, 2), " in 2100."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ============================================================
# 3. Reductions relative to Reference
# ============================================================

fig11_vs_reference <- ghg_reductions %>%
  filter(year %in% text_years) %>%
  mutate(
    embodied_GtCO2e = embodied_MtCO2e / 1000,
    operational_GtCO2e = operational_MtCO2e / 1000,
    total_GtCO2e = total_MtCO2e / 1000,
    
    ref_embodied_GtCO2e = ref_embodied_MtCO2e / 1000,
    ref_operational_GtCO2e = ref_operational_MtCO2e / 1000,
    ref_total_GtCO2e = ref_total_MtCO2e / 1000,
    
    embodied_reduction_Gt =
      ref_embodied_GtCO2e - embodied_GtCO2e,
    operational_reduction_Gt =
      ref_operational_GtCO2e - operational_GtCO2e,
    total_reduction_Gt =
      ref_total_GtCO2e - total_GtCO2e
  ) %>%
  select(
    sector_clean,
    year,
    scenario_name_clean,
    embodied_GtCO2e,
    operational_GtCO2e,
    total_GtCO2e,
    embodied_reduction_Gt,
    operational_reduction_Gt,
    total_reduction_Gt,
    embodied_reduction_pct,
    operational_reduction_pct,
    total_reduction_pct
  ) %>%
  arrange(sector_clean, year, scenario_name_clean)

cat("\nGHG reductions relative to Reference:\n")
cat("Positive values mean lower emissions than Reference.\n")

fig11_vs_reference %>%
  filter(year %in% c(2050, 2100)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 4. Text-ready reduction sentences for 2050 and 2100
# ============================================================

cat("\nText-ready 2050 and 2100 GHG reduction sentences:\n")

fig11_vs_reference %>%
  filter(
    year %in% c(2050, 2100),
    scenario_name_clean != "Reference"
  ) %>%
  rowwise() %>%
  mutate(
    sentence = paste0(
      sector_clean, ", ", scenario_name_clean, ", ", year, ": ",
      "embodied = ", fmt_num(embodied_GtCO2e, 2), " GtCO2e/yr (",
      fmt_pct(embodied_reduction_pct, 1), " vs Reference); ",
      "operational = ", fmt_num(operational_GtCO2e, 2), " GtCO2e/yr (",
      fmt_pct(operational_reduction_pct, 1), " vs Reference); ",
      "total = ", fmt_num(total_GtCO2e, 2), " GtCO2e/yr (",
      fmt_pct(total_reduction_pct, 1), " vs Reference)."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ============================================================
# 5. Ranking scenarios by total, embodied, and operational reductions
# ============================================================

fig11_ranked <- fig11_vs_reference %>%
  filter(year %in% c(2050, 2100)) %>%
  group_by(sector_clean, year) %>%
  mutate(
    rank_embodied = rank(-embodied_reduction_pct, ties.method = "first"),
    rank_operational = rank(-operational_reduction_pct, ties.method = "first"),
    rank_total = rank(-total_reduction_pct, ties.method = "first")
  ) %>%
  ungroup() %>%
  arrange(sector_clean, year, rank_total)

cat("\nScenario ranking by total GHG reduction:\n")

fig11_ranked %>%
  select(
    sector_clean,
    year,
    rank_total,
    scenario_name_clean,
    embodied_reduction_pct,
    operational_reduction_pct,
    total_reduction_pct
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 6. Embodied vs operational shares of total emissions
# ============================================================

fig11_emission_shares <- ghg_clean %>%
  filter(year %in% text_years) %>%
  mutate(
    embodied_share_pct = 100 * embodied_MtCO2e / total_MtCO2e,
    operational_share_pct = 100 * operational_MtCO2e / total_MtCO2e,
    embodied_GtCO2e = embodied_MtCO2e / 1000,
    operational_GtCO2e = operational_MtCO2e / 1000,
    total_GtCO2e = total_MtCO2e / 1000
  ) %>%
  select(
    sector_clean,
    year,
    scenario_name_clean,
    embodied_GtCO2e,
    operational_GtCO2e,
    total_GtCO2e,
    embodied_share_pct,
    operational_share_pct
  ) %>%
  arrange(sector_clean, year, scenario_name_clean)

cat("\nEmbodied and operational shares of total emissions:\n")

fig11_emission_shares %>%
  filter(year %in% c(2025, 2050, 2100)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 7. Close scenario check
# Useful because Close may affect embodied emissions more than gross material demand
# ============================================================

fig11_close_check <- fig11_vs_reference %>%
  filter(scenario_name_clean == "Close R") %>%
  select(
    sector_clean,
    year,
    embodied_GtCO2e,
    operational_GtCO2e,
    total_GtCO2e,
    embodied_reduction_Gt,
    operational_reduction_Gt,
    total_reduction_Gt,
    embodied_reduction_pct,
    operational_reduction_pct,
    total_reduction_pct
  ) %>%
  arrange(sector_clean, year)

cat("\nClose R relative to Reference:\n")

fig11_close_check %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 8. Slow trade-off check
# Embodied may decrease while operational may increase
# ============================================================

fig11_slow_check <- fig11_vs_reference %>%
  filter(scenario_name_clean == "Slow R") %>%
  select(
    sector_clean,
    year,
    embodied_GtCO2e,
    operational_GtCO2e,
    total_GtCO2e,
    embodied_reduction_pct,
    operational_reduction_pct,
    total_reduction_pct
  ) %>%
  arrange(sector_clean, year)

cat("\nSlow R embodied-operational trade-off relative to Reference:\n")

fig11_slow_check %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 9. Efficiency and Climate policy comparison
# Climate policy only exists for residential in current modelling
# ============================================================

fig11_eff_cp <- fig11_vs_reference %>%
  filter(
    scenario_name_clean %in% c("Efficiency R", "Climate policy R"),
    year %in% c(2050, 2100)
  ) %>%
  select(
    sector_clean,
    year,
    scenario_name_clean,
    embodied_GtCO2e,
    operational_GtCO2e,
    total_GtCO2e,
    embodied_reduction_pct,
    operational_reduction_pct,
    total_reduction_pct
  ) %>%
  arrange(sector_clean, year, scenario_name_clean)

cat("\nEfficiency and Climate policy comparison:\n")

fig11_eff_cp %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# Residential-only direct comparison between Efficiency and Climate policy
fig11_cp_minus_eff <- fig11_vs_reference %>%
  filter(
    sector_clean == "Residential",
    scenario_name_clean %in% c("Efficiency R", "Climate policy R"),
    year %in% c(2050, 2100)
  ) %>%
  select(
    year,
    scenario_name_clean,
    embodied_GtCO2e,
    operational_GtCO2e,
    total_GtCO2e
  ) %>%
  pivot_wider(
    names_from = scenario_name_clean,
    values_from = c(embodied_GtCO2e, operational_GtCO2e, total_GtCO2e)
  ) %>%
  mutate(
    cp_minus_eff_embodied_Gt =
      `embodied_GtCO2e_Climate policy R` - `embodied_GtCO2e_Efficiency R`,
    cp_minus_eff_operational_Gt =
      `operational_GtCO2e_Climate policy R` - `operational_GtCO2e_Efficiency R`,
    cp_minus_eff_total_Gt =
      `total_GtCO2e_Climate policy R` - `total_GtCO2e_Efficiency R`
  )

cat("\nResidential Climate policy minus Efficiency:\n")
cat("Negative values mean Climate policy has lower emissions than Efficiency.\n")

fig11_cp_minus_eff %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 10. Best circular economy scenario vs Efficiency / Climate policy
# ============================================================

fig11_best_ce <- fig11_vs_reference %>%
  filter(scenario_name_clean %in% c("Narrow R", "Slow R", "Close R", "Combined R")) %>%
  group_by(sector_clean, year) %>%
  slice_max(order_by = total_reduction_pct, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    sector_clean,
    year,
    best_ce_scenario = scenario_name_clean,
    best_ce_total_GtCO2e = total_GtCO2e,
    best_ce_total_reduction_pct = total_reduction_pct
  )

fig11_eff_vs_best_ce <- fig11_vs_reference %>%
  filter(scenario_name_clean %in% c("Efficiency R", "Climate policy R")) %>%
  left_join(
    fig11_best_ce,
    by = c("sector_clean", "year")
  ) %>%
  mutate(
    scenario_minus_best_ce_Gt =
      total_GtCO2e - best_ce_total_GtCO2e,
    extra_total_reduction_pct_points =
      total_reduction_pct - best_ce_total_reduction_pct
  ) %>%
  select(
    sector_clean,
    year,
    scenario_name_clean,
    total_GtCO2e,
    total_reduction_pct,
    best_ce_scenario,
    best_ce_total_GtCO2e,
    best_ce_total_reduction_pct,
    scenario_minus_best_ce_Gt,
    extra_total_reduction_pct_points
  ) %>%
  arrange(sector_clean, year, scenario_name_clean)

cat("\nEfficiency / Climate policy compared with best circular economy scenario:\n")
cat("Negative Gt values mean lower total emissions than best CE scenario.\n")

fig11_eff_vs_best_ce %>%
  filter(year %in% c(2050, 2100)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 11. Text-ready key sentences
# ============================================================

cat("\nText-ready embodied-emissions reduction sentences for 2050:\n")

fig11_vs_reference %>%
  filter(
    year == 2050,
    scenario_name_clean %in% c("Narrow R", "Slow R", "Close R", "Combined R")
  ) %>%
  rowwise() %>%
  mutate(
    sentence = paste0(
      sector_clean, ", ", scenario_name_clean, ": embodied emissions are ",
      fmt_num(embodied_GtCO2e, 2), " GtCO2e/yr in 2050, ",
      fmt_pct(embodied_reduction_pct, 1),
      " relative to Reference."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

cat("\nText-ready operational-emissions reduction sentences for 2050:\n")

fig11_vs_reference %>%
  filter(
    year == 2050,
    scenario_name_clean %in% c("Narrow R", "Slow R", "Combined R", "Efficiency R", "Climate policy R")
  ) %>%
  rowwise() %>%
  mutate(
    sentence = paste0(
      sector_clean, ", ", scenario_name_clean, ": operational emissions are ",
      fmt_num(operational_GtCO2e, 2), " GtCO2e/yr in 2050, ",
      fmt_pct(operational_reduction_pct, 1),
      " relative to Reference."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ============================================================
# 12. Save text-supporting tables
# ============================================================

write_csv(
  fig11_selected,
  file.path(output_dir, paste0(file_stub, "_text_selected_values.csv"))
)

write_csv(
  fig11_selected_wide,
  file.path(output_dir, paste0(file_stub, "_text_selected_values_wide.csv"))
)

write_csv(
  fig11_reference_change,
  file.path(output_dir, paste0(file_stub, "_text_reference_change.csv"))
)

write_csv(
  fig11_vs_reference,
  file.path(output_dir, paste0(file_stub, "_text_vs_reference.csv"))
)

write_csv(
  fig11_ranked,
  file.path(output_dir, paste0(file_stub, "_text_ranked_reductions.csv"))
)

write_csv(
  fig11_emission_shares,
  file.path(output_dir, paste0(file_stub, "_text_emission_shares.csv"))
)

write_csv(
  fig11_close_check,
  file.path(output_dir, paste0(file_stub, "_text_close_check.csv"))
)

write_csv(
  fig11_slow_check,
  file.path(output_dir, paste0(file_stub, "_text_slow_check.csv"))
)

write_csv(
  fig11_eff_cp,
  file.path(output_dir, paste0(file_stub, "_text_efficiency_climate_policy.csv"))
)

if (exists("fig11_cp_minus_eff")) {
  write_csv(
    fig11_cp_minus_eff,
    file.path(output_dir, paste0(file_stub, "_text_climate_policy_minus_efficiency.csv"))
  )
}

write_csv(
  fig11_eff_vs_best_ce,
  file.path(output_dir, paste0(file_stub, "_text_efficiency_vs_best_ce.csv"))
)

cat("\nSaved Fig. 11 text-supporting tables to:\n")
cat(output_dir, "\n")
