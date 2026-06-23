# ============================================================
# Fig. 9
# EU-27 residential and commercial space heating and cooling demand
# under realistic circular economy and energy-efficiency scenarios
#
# Scenarios included:
#   R, N_r, S_r, C_r, A_r, E_r
#
# Grouped trajectories:
#   Reference / Close R
#
# Energy plotted:
#   heat_TJ + cool_TJ only
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
input_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/output"

output_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# 2. Settings
# ----------------------------
sectors <- c("resid", "comm")

scenarios <- c(
  "R",
  "N_r",
  "S_r",
  "C_r",
  "A_r",
  "E_r"
)

sector_labels <- c(
  "resid" = "Residential",
  "comm"  = "Commercial"
)

scenario_labels <- c(
  "R"   = "Reference",
  "N_r" = "Narrow R",
  "S_r" = "Slow R",
  "C_r" = "Close R",
  "A_r" = "Combined R",
  "E_r" = "Energy efficiency R"
)

scenario_order <- c(
  "Reference",
  "Narrow R",
  "Slow R",
  "Close R",
  "Combined R",
  "Energy efficiency R"
)

plot_order <- c(
  "Reference / Close R",
  "Narrow R",
  "Slow R",
  "Combined R",
  "Energy efficiency R"
)

plot_colours <- c(
  "Reference / Close R" = "#4D4D4D",
  "Narrow R"            = "#D55E00",
  "Slow R"              = "#0072B2",
  "Combined R"          = "#CC79A7",
  "Energy efficiency R" = "#E69F00"
)

plot_linetypes <- c(
  "Reference / Close R" = "solid",
  "Narrow R"            = "solid",
  "Slow R"              = "solid",
  "Combined R"          = "solid",
  "Energy efficiency R" = "longdash"
)

# ----------------------------
# 3. Helper: find energy report
# ----------------------------
find_energy_file <- function(sc, sector_code, input_dir) {
  
  pattern <- paste0(
    "^report_STURM_",
    sc,
    "_",
    sector_code,
    "_.*_energy\\.csv$"
  )
  
  files <- list.files(
    input_dir,
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    warning(
      "No energy file found for scenario = ",
      sc,
      ", sector = ",
      sector_code,
      "\nPattern used: ",
      pattern
    )
    return(NA_character_)
  }
  
  # Prefer region_bld if available; otherwise use first match, e.g. R12
  region_bld_file <- files[str_detect(files, "_region_bld_energy\\.csv$")]
  
  if (length(region_bld_file) > 0) {
    return(region_bld_file[1])
  }
  
  files[1]
}

# ----------------------------
# 4. Read energy reports
# ----------------------------
read_energy_report <- function(sc, sector_code) {
  
  f <- find_energy_file(
    sc = sc,
    sector_code = sector_code,
    input_dir = input_dir
  )
  
  if (is.na(f) || !file.exists(f)) {
    return(NULL)
  }
  
  cat("Loaded:", basename(f), "\n")
  
  df <- read_csv(f, show_col_types = FALSE)
  
  required_energy_cols <- c("year", "heat_TJ", "cool_TJ")
  missing_energy_cols <- setdiff(required_energy_cols, names(df))
  
  if (length(missing_energy_cols) > 0) {
    stop(
      "Missing required energy columns in ",
      basename(f),
      ":\n",
      paste(missing_energy_cols, collapse = "\n")
    )
  }
  
  df %>%
    mutate(
      scenario = sc,
      scenario_label = scenario_labels[sc],
      sector = sector_code,
      sector_label = sector_labels[sector_code],
      source_file = basename(f)
    )
}

df_raw <- crossing(
  scenario = scenarios,
  sector = sectors
) %>%
  pmap_dfr(function(scenario, sector) {
    read_energy_report(sc = scenario, sector_code = sector)
  })

if (nrow(df_raw) == 0) {
  stop("No energy files were loaded. Check input_dir and scenario names.")
}

df_raw <- df_raw %>%
  mutate(
    scenario = factor(scenario, levels = scenarios),
    scenario_label = factor(scenario_label, levels = scenario_order),
    sector_label = factor(sector_label, levels = c("Residential", "Commercial"))
  )

cat("\nLoaded rows by sector and scenario:\n")
print(df_raw %>% count(sector_label, scenario_label), n = Inf)

cat("\nAvailable years:\n")
print(sort(unique(df_raw$year)))

cat("\nLoaded files:\n")
print(df_raw %>% distinct(sector_label, scenario, source_file), n = Inf)

# ----------------------------
# 5. Calculate heating + cooling demand
# ----------------------------
df_energy <- df_raw %>%
  distinct() %>%
  mutate(
    heat_TJ = replace_na(heat_TJ, 0),
    cool_TJ = replace_na(cool_TJ, 0),
    heat_cool_TJ = heat_TJ + cool_TJ
  )

cat("\nRows before deduplication:", nrow(df_raw), "\n")
cat("Rows after deduplication: ", nrow(df_energy), "\n")

# ----------------------------
# 6. Aggregate annual heating and cooling demand
# ----------------------------
df_heatcool <- df_energy %>%
  group_by(sector, sector_label, year, scenario, scenario_label) %>%
  summarise(
    heat_TJ = sum(heat_TJ, na.rm = TRUE),
    cool_TJ = sum(cool_TJ, na.rm = TRUE),
    heat_cool_TJ = sum(heat_cool_TJ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    heat_EJ = heat_TJ / 1e6,
    cool_EJ = cool_TJ / 1e6,
    heat_cool_EJ = heat_cool_TJ / 1e6
  )

cat("\nHeating + cooling demand in selected years:\n")
df_heatcool %>%
  filter(year %in% c(2020, 2025, 2050, 2100)) %>%
  select(
    sector_label,
    year,
    scenario_label,
    heat_EJ,
    cool_EJ,
    heat_cool_EJ
  ) %>%
  arrange(sector_label, year, scenario_label) %>%
  print(n = Inf, width = Inf)

# ----------------------------
# 7. Check Reference vs Close overlap
# ----------------------------
if (all(c("R", "C_r") %in% unique(as.character(df_heatcool$scenario)))) {
  
  check_ref_close <- df_heatcool %>%
    filter(scenario %in% c("R", "C_r")) %>%
    select(
      sector_label,
      scenario,
      year,
      heat_EJ,
      cool_EJ,
      heat_cool_EJ
    ) %>%
    pivot_wider(
      names_from = scenario,
      values_from = c(heat_EJ, cool_EJ, heat_cool_EJ)
    ) %>%
    mutate(
      diff_heat = heat_EJ_C_r - heat_EJ_R,
      diff_cool = cool_EJ_C_r - cool_EJ_R,
      diff_heatcool = heat_cool_EJ_C_r - heat_cool_EJ_R
    )
  
  cat("\nMaximum absolute differences: Reference vs Close R\n")
  check_ref_close %>%
    group_by(sector_label) %>%
    summarise(
      max_abs_diff_heat = max(abs(diff_heat), na.rm = TRUE),
      max_abs_diff_cool = max(abs(diff_cool), na.rm = TRUE),
      max_abs_diff_heatcool = max(abs(diff_heatcool), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    print(width = Inf)
}

# ----------------------------
# 8. Group scenarios for plotting
# ----------------------------
df_fig9 <- df_heatcool %>%
  mutate(
    scenario_label_fig9 = case_when(
      scenario %in% c("R", "C_r") ~ "Reference / Close R",
      scenario == "N_r" ~ "Narrow R",
      scenario == "S_r" ~ "Slow R",
      scenario == "A_r" ~ "Combined R",
      scenario == "E_r" ~ "Energy efficiency R",
      TRUE ~ as.character(scenario_label)
    ),
    scenario_label_fig9 = factor(
      scenario_label_fig9,
      levels = plot_order
    )
  ) %>%
  filter(!is.na(scenario_label_fig9)) %>%
  group_by(sector, sector_label, year, scenario_label_fig9) %>%
  summarise(
    heat_EJ = mean(heat_EJ, na.rm = TRUE),
    cool_EJ = mean(cool_EJ, na.rm = TRUE),
    heat_cool_EJ = mean(heat_cool_EJ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sector_label = factor(
      sector_label,
      levels = c("Residential", "Commercial")
    )
  )

cat("\nGrouped Fig. 9 trajectories in selected years:\n")
df_fig9 %>%
  filter(year %in% c(2020, 2025, 2050, 2100)) %>%
  arrange(sector_label, year, scenario_label_fig9) %>%
  print(n = Inf, width = Inf)

# ----------------------------
# 9. Theme
# ----------------------------
theme_fig <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      margin = margin(b = 4)
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
    legend.key.width = unit(1.5, "cm"),
    legend.box = "vertical",
    legend.spacing.y = unit(0.15, "cm"),
    
    strip.text.y.right = element_text(
      face = "bold",
      size = 13,
      angle = 270
    ),
    strip.background = element_blank(),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85"),
    panel.grid.major.y = element_line(colour = "grey85"),
    
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 12, 10, 10)
  )

# ============================================================
# 10. FIGURE 9
# ============================================================
fig9 <- ggplot(
  df_fig9,
  aes(
    x = year,
    y = heat_cool_EJ,
    colour = scenario_label_fig9,
    linetype = scenario_label_fig9,
    group = scenario_label_fig9
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.1) +
  facet_grid(
    rows = vars(sector_label),
    scales = "free_y"
  ) +
  scale_colour_manual(values = plot_colours, name = "Scenario") +
  scale_linetype_manual(values = plot_linetypes, name = "Scenario") +
  scale_x_continuous(
    breaks = c(2020, 2030, 2040, 2050, 2060, 2080, 2100)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.06)),
    labels = label_number(accuracy = 0.1)
  ) +
  labs(
    title = "EU-27 residential and commercial space heating and cooling demand",
    subtitle = "Realistic circular economy and energy-efficiency scenarios",
    x = NULL,
    y = "Heating and cooling demand (EJ/yr)"
  ) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_fig

print(fig9)

# ----------------------------
# 11. Save outputs
# ----------------------------
file_stub <- "Fig9_EU27_v4_residential_commercial_heating_cooling_demand_no_CP"

ggsave(
  filename = file.path(output_dir, paste0(file_stub, ".png")),
  plot = fig9,
  width = 10,
  height = 7.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(output_dir, paste0(file_stub, ".tiff")),
  plot = fig9,
  width = 10,
  height = 7.5,
  units = "in",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(output_dir, paste0(file_stub, ".pdf")),
  plot = fig9,
  width = 10,
  height = 7.5,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

write_csv(
  df_heatcool,
  file.path(output_dir, paste0(file_stub, "_all_scenarios_data.csv"))
)

write_csv(
  df_fig9,
  file.path(output_dir, paste0(file_stub, "_grouped_data.csv"))
)

# ----------------------------
# 12. Reductions relative to Reference
# ----------------------------
ref_energy <- df_heatcool %>%
  filter(scenario == "R") %>%
  select(
    sector,
    sector_label,
    year,
    ref_heat_cool_EJ = heat_cool_EJ
  )

heatcool_reductions <- df_heatcool %>%
  left_join(
    ref_energy,
    by = c("sector", "sector_label", "year")
  ) %>%
  mutate(
    reduction_EJ = ref_heat_cool_EJ - heat_cool_EJ,
    reduction_pct = reduction_EJ / ref_heat_cool_EJ * 100
  )

cat("\nHeating + cooling reduction relative to Reference:\n")
heatcool_reductions %>%
  filter(year %in% c(2030, 2050, 2100)) %>%
  select(
    sector_label,
    year,
    scenario_label,
    heat_cool_EJ,
    reduction_EJ,
    reduction_pct
  ) %>%
  arrange(sector_label, year, scenario_label) %>%
  print(n = Inf, width = Inf)

write_csv(
  heatcool_reductions,
  file.path(output_dir, paste0(file_stub, "_reductions_relative_to_reference.csv"))
)

cat("\nSaved Fig. 9 and data to:\n", output_dir, "\n")



#######################
### EXTRACT DATA
#######################

# ============================================================
# Extract exact Fig. 9 numbers for Results text
# Space heating and cooling demand
# Residential + commercial sectors
# ============================================================

cat("\n\n============================================================\n")
cat("Exact Fig. 9 numbers for Results text\n")
cat("Space heating and cooling demand\n")
cat("============================================================\n")

# Helper formatting
fmt_num <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  paste0(format(round(x, digits), nsmall = digits, trim = TRUE), "%")
}

text_years <- c(2020, 2025, 2030, 2050, 2100)

# ── 1) Selected-year values from plotted trajectories ───────

fig9_selected_values <- df_fig9 %>%
  filter(year %in% text_years) %>%
  select(
    sector_label,
    year,
    scenario_label_fig9,
    heat_EJ,
    cool_EJ,
    heat_cool_EJ
  ) %>%
  arrange(sector_label, year, scenario_label_fig9)

cat("\nSelected-year heating and cooling demand:\n")
cat("Units: EJ/yr\n")

fig9_selected_values %>%
  mutate(
    heat_EJ = round(heat_EJ, 3),
    cool_EJ = round(cool_EJ, 3),
    heat_cool_EJ = round(heat_cool_EJ, 3)
  ) %>%
  print(n = Inf, width = Inf)

fig9_selected_values_wide <- fig9_selected_values %>%
  select(sector_label, year, scenario_label_fig9, heat_cool_EJ) %>%
  pivot_wider(
    names_from = scenario_label_fig9,
    values_from = heat_cool_EJ
  ) %>%
  arrange(sector_label, year)

cat("\nSelected-year total heating + cooling demand, wide format:\n")

fig9_selected_values_wide %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ── 2) Reference / Close change over time ───────────────────

fig9_reference_change <- df_fig9 %>%
  filter(
    scenario_label_fig9 == "Reference / Close R",
    year %in% c(2020, 2050, 2100)
  ) %>%
  select(sector_label, year, heat_cool_EJ) %>%
  pivot_wider(
    names_from = year,
    values_from = heat_cool_EJ,
    names_prefix = "y"
  ) %>%
  mutate(
    change_2020_2050_EJ = y2050 - y2020,
    change_2050_2100_EJ = y2100 - y2050,
    change_2020_2100_EJ = y2100 - y2020,
    pct_change_2020_2050 = 100 * (y2050 / y2020 - 1),
    pct_change_2050_2100 = 100 * (y2100 / y2050 - 1),
    pct_change_2020_2100 = 100 * (y2100 / y2020 - 1)
  )

cat("\nReference / Close trajectory change over time:\n")

fig9_reference_change %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

cat("\nText-ready Reference / Close sentences:\n")

fig9_reference_change %>%
  rowwise() %>%
  mutate(
    sentence = paste0(
      sector_label,
      ": Reference / Close heating and cooling demand declines from ",
      fmt_num(y2020, 2), " EJ/yr in 2020 to ",
      fmt_num(y2050, 2), " EJ/yr in 2050 and ",
      fmt_num(y2100, 2), " EJ/yr in 2100, a ",
      fmt_pct(abs(pct_change_2020_2100), 1),
      " reduction over 2020-2100."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ── 3) Reductions relative to Reference / Close ─────────────

fig9_ref <- df_fig9 %>%
  filter(scenario_label_fig9 == "Reference / Close R") %>%
  select(
    sector_label,
    year,
    ref_heat_cool_EJ = heat_cool_EJ
  )

fig9_vs_reference <- df_fig9 %>%
  filter(scenario_label_fig9 != "Reference / Close R") %>%
  left_join(
    fig9_ref,
    by = c("sector_label", "year")
  ) %>%
  mutate(
    reduction_EJ = ref_heat_cool_EJ - heat_cool_EJ,
    reduction_pct = 100 * reduction_EJ / ref_heat_cool_EJ
  ) %>%
  arrange(sector_label, scenario_label_fig9, year)

cat("\nScenario reductions relative to Reference / Close:\n")
cat("Positive values mean lower heating + cooling demand than Reference / Close.\n")

fig9_vs_reference %>%
  filter(year %in% c(2030, 2050, 2100)) %>%
  select(
    sector_label,
    scenario_label_fig9,
    year,
    ref_heat_cool_EJ,
    heat_cool_EJ,
    reduction_EJ,
    reduction_pct
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ── 4) Text-ready 2050 and 2100 scenario comparisons ────────

cat("\nText-ready 2050 and 2100 scenario sentences:\n")

fig9_vs_reference %>%
  filter(year %in% c(2050, 2100)) %>%
  mutate(
    sentence = paste0(
      sector_label, ", ", scenario_label_fig9, ", ", year, ": ",
      "demand = ", fmt_num(heat_cool_EJ, 2), " EJ/yr",
      " (", fmt_pct(reduction_pct, 1), " relative to Reference / Close; ",
      fmt_num(reduction_EJ, 2), " EJ/yr difference)."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ── 5) Ranking scenarios by reduction in 2050 and 2100 ──────

fig9_ranked <- fig9_vs_reference %>%
  filter(year %in% c(2050, 2100)) %>%
  group_by(sector_label, year) %>%
  arrange(desc(reduction_pct), .by_group = TRUE) %>%
  mutate(rank_reduction = row_number()) %>%
  ungroup()

cat("\nScenario ranking by percentage reduction relative to Reference / Close:\n")

fig9_ranked %>%
  select(
    sector_label,
    year,
    rank_reduction,
    scenario_label_fig9,
    heat_cool_EJ,
    reduction_EJ,
    reduction_pct
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ── 6) Narrow, Slow, Combined comparison ────────────────────

fig9_ce_comparison <- fig9_vs_reference %>%
  filter(
    scenario_label_fig9 %in% c("Narrow R", "Slow R", "Combined R"),
    year %in% c(2050, 2100)
  ) %>%
  select(
    sector_label,
    year,
    scenario_label_fig9,
    heat_cool_EJ,
    reduction_EJ,
    reduction_pct
  ) %>%
  arrange(sector_label, year, scenario_label_fig9)

cat("\nCircular economy scenario comparison:\n")

fig9_ce_comparison %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ── 7) Slow relative to Reference / Close ───────────────────
# Useful for checking whether Slow is above/below Reference.

fig9_slow_check <- df_fig9 %>%
  filter(scenario_label_fig9 %in% c("Reference / Close R", "Slow R")) %>%
  select(sector_label, year, scenario_label_fig9, heat_cool_EJ) %>%
  pivot_wider(
    names_from = scenario_label_fig9,
    values_from = heat_cool_EJ
  ) %>%
  mutate(
    slow_minus_ref_EJ = `Slow R` - `Reference / Close R`,
    slow_minus_ref_pct = 100 * slow_minus_ref_EJ / `Reference / Close R`
  ) %>%
  arrange(sector_label, year)

cat("\nSlow R relative to Reference / Close:\n")
cat("Positive values mean Slow R is higher than Reference / Close.\n")

fig9_slow_check %>%
  filter(year %in% text_years) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ── 8) Energy efficiency relative to best CE scenario ───────
# Useful for saying whether energy efficiency dominates circular strategies.

fig9_best_ce <- fig9_vs_reference %>%
  filter(scenario_label_fig9 %in% c("Narrow R", "Slow R", "Combined R")) %>%
  group_by(sector_label, year) %>%
  slice_max(order_by = reduction_pct, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    sector_label,
    year,
    best_ce_scenario = scenario_label_fig9,
    best_ce_demand_EJ = heat_cool_EJ,
    best_ce_reduction_pct = reduction_pct
  )

fig9_efficiency_gap <- fig9_vs_reference %>%
  filter(scenario_label_fig9 == "Energy efficiency R") %>%
  select(
    sector_label,
    year,
    efficiency_demand_EJ = heat_cool_EJ,
    efficiency_reduction_pct = reduction_pct
  ) %>%
  left_join(
    fig9_best_ce,
    by = c("sector_label", "year")
  ) %>%
  mutate(
    efficiency_minus_best_ce_EJ =
      efficiency_demand_EJ - best_ce_demand_EJ,
    extra_reduction_pct_points =
      efficiency_reduction_pct - best_ce_reduction_pct
  ) %>%
  arrange(sector_label, year)

cat("\nEnergy efficiency compared with best circular economy scenario:\n")
cat("Negative EJ values mean Energy efficiency has lower demand than best CE.\n")

fig9_efficiency_gap %>%
  filter(year %in% c(2030, 2050, 2100)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ── 9) Heat versus cooling components ───────────────────────
# Useful if you want to mention whether reductions are mainly heating.

fig9_heat_cool_components <- df_fig9 %>%
  filter(year %in% c(2020, 2050, 2100)) %>%
  mutate(
    heat_share = 100 * heat_EJ / heat_cool_EJ,
    cool_share = 100 * cool_EJ / heat_cool_EJ
  ) %>%
  select(
    sector_label,
    year,
    scenario_label_fig9,
    heat_EJ,
    cool_EJ,
    heat_cool_EJ,
    heat_share,
    cool_share
  ) %>%
  arrange(sector_label, year, scenario_label_fig9)

cat("\nHeating and cooling components:\n")

fig9_heat_cool_components %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ── 10) Reference vs Close overlap detail ───────────────────

if (exists("check_ref_close")) {
  
  fig9_close_overlap_detail <- check_ref_close %>%
    arrange(sector_label, year)
  
  cat("\nReference vs Close overlap detail:\n")
  
  fig9_close_overlap_detail %>%
    filter(year %in% text_years) %>%
    mutate(across(where(is.numeric), ~ round(.x, 10))) %>%
    print(n = Inf, width = Inf)
  
  fig9_close_overlap_summary <- fig9_close_overlap_detail %>%
    group_by(sector_label) %>%
    summarise(
      max_abs_diff_heat = max(abs(diff_heat), na.rm = TRUE),
      max_abs_diff_cool = max(abs(diff_cool), na.rm = TRUE),
      max_abs_diff_heatcool = max(abs(diff_heatcool), na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nReference vs Close overlap summary:\n")
  
  fig9_close_overlap_summary %>%
    mutate(across(where(is.numeric), ~ round(.x, 10))) %>%
    print(n = Inf, width = Inf)
}

# ── 11) Save text-supporting tables ─────────────────────────

write_csv(
  fig9_selected_values,
  file.path(output_dir, paste0(file_stub, "_selected_values_for_text.csv"))
)

write_csv(
  fig9_selected_values_wide,
  file.path(output_dir, paste0(file_stub, "_selected_values_wide_for_text.csv"))
)

write_csv(
  fig9_reference_change,
  file.path(output_dir, paste0(file_stub, "_reference_change_for_text.csv"))
)

write_csv(
  fig9_vs_reference,
  file.path(output_dir, paste0(file_stub, "_vs_reference_for_text.csv"))
)

write_csv(
  fig9_ranked,
  file.path(output_dir, paste0(file_stub, "_ranked_reductions_for_text.csv"))
)

write_csv(
  fig9_ce_comparison,
  file.path(output_dir, paste0(file_stub, "_ce_comparison_for_text.csv"))
)

write_csv(
  fig9_slow_check,
  file.path(output_dir, paste0(file_stub, "_slow_vs_reference_for_text.csv"))
)

write_csv(
  fig9_efficiency_gap,
  file.path(output_dir, paste0(file_stub, "_efficiency_gap_for_text.csv"))
)

write_csv(
  fig9_heat_cool_components,
  file.path(output_dir, paste0(file_stub, "_heat_cool_components_for_text.csv"))
)

if (exists("fig9_close_overlap_detail")) {
  write_csv(
    fig9_close_overlap_detail,
    file.path(output_dir, paste0(file_stub, "_close_overlap_detail_for_text.csv"))
  )
}

cat("\nSaved Fig. 9 text-supporting tables to:\n")
cat(output_dir, "\n")