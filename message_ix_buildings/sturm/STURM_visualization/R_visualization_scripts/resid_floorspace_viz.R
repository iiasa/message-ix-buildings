# ============================================================
# STURM Results — EU-27 Residential Floor-space Figures
#
# Main manuscript figure:
#   Panel A: EU-27 aggregate residential floor-space trajectories
#            in billion m2
#   Panel B: Country-level 2100 floor-space reduction vs Reference
#            with EU-27 aggregate reduction shown as labelled vertical lines
#
# Supplementary figure:
#   Country-level residential floor-space trajectories
#   in million m2
#
# Important:
#   Uses only:
#     report_STURM_<scenario>_resid_region_bld_energy.csv
#
#   Do NOT use vacant output files here, because those are not
#   total residential floor-space trajectories.
# ============================================================

library(tidyverse)
library(readr)
library(scales)
library(patchwork)

# ── Step 0: User settings ───────────────────────────────────

output_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/output"

plot_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

years_to_run <- c(seq(2020, 2060, 5), seq(2070, 2100, 10))

# Total residential floor-space scenarios available
scenarios <- c(
  "R",
  "N_r", "N_tp",
  "S_r", "S_tp",
  "C_r", "C_tp"
)

make_energy_file_path <- function(scenario) {
  file.path(
    output_dir,
    paste0("report_STURM_", scenario, "_resid_region_bld_energy.csv")
  )
}

# Clean model region labels, e.g. C-WEU-DEU -> DEU
clean_region_label <- function(x) {
  x %>%
    str_replace("^C-", "") %>%
    str_replace("^[A-Z]+-", "")
}

# Optional: ISO3 to country names for Panel B / SI.
# Keep FALSE for compact ISO3 labels.
use_country_names <- FALSE

iso3_to_country <- c(
  "AUT" = "Austria",
  "BEL" = "Belgium",
  "BGR" = "Bulgaria",
  "CYP" = "Cyprus",
  "CZE" = "Czechia",
  "DEU" = "Germany",
  "DNK" = "Denmark",
  "ESP" = "Spain",
  "EST" = "Estonia",
  "FIN" = "Finland",
  "FRA" = "France",
  "GRC" = "Greece",
  "HRV" = "Croatia",
  "HUN" = "Hungary",
  "IRL" = "Ireland",
  "ITA" = "Italy",
  "LTU" = "Lithuania",
  "LUX" = "Luxembourg",
  "LVA" = "Latvia",
  "MLT" = "Malta",
  "NLD" = "Netherlands",
  "POL" = "Poland",
  "PRT" = "Portugal",
  "ROU" = "Romania",
  "SVK" = "Slovakia",
  "SVN" = "Slovenia",
  "SWE" = "Sweden"
)

# ── Step 1: Load scenario files ──────────────────────────────

df_all <- map_dfr(scenarios, function(sc) {
  f <- make_energy_file_path(sc)
  
  if (!file.exists(f)) {
    cat("  NOT FOUND:", f, "\n")
    return(NULL)
  }
  
  cat("  Loaded:", f, "\n")
  
  read_csv(f, show_col_types = FALSE) %>%
    mutate(scenario = sc)
}) %>%
  mutate(scenario = factor(scenario, levels = scenarios))

if (nrow(df_all) == 0) {
  stop("No scenario files were loaded. Check output_dir and scenario names.")
}

# ── Step 2: Identify floor-space column ──────────────────────

cat("\n=== Columns in loaded data ===\n")
print(names(df_all))

candidate_floor_cols <- c(
  "floor_Mm2",
  "floor",
  "floor_m2",
  "floorspace",
  "floorspace_Mm2",
  "value"
)

floor_col <- candidate_floor_cols[candidate_floor_cols %in% names(df_all)][1]

if (is.na(floor_col)) {
  stop(
    paste0(
      "Could not find a floor-space column. Expected one of: ",
      paste(candidate_floor_cols, collapse = ", ")
    )
  )
}

cat("\nUsing floor-space column:", floor_col, "\n")

# ── Step 3: Basic checks ─────────────────────────────────────

cat("\n=== Basic checks ===\n")
cat("Total rows:", nrow(df_all), "\n")

cat("\nScenarios loaded:\n")
print(table(df_all$scenario))

cat("\nYears available:\n")
print(sort(unique(df_all$year)))

cat("\nYear range by scenario:\n")
df_all %>%
  group_by(scenario) %>%
  summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_years  = n_distinct(year),
    .groups  = "drop"
  ) %>%
  print(n = Inf)

# ── Step 4: Deduplicate building segments ────────────────────
# Energy output can repeat floor-space across energy carriers/end uses.
# This avoids double-counting.

segment_cols <- c(
  "year", "scenario",
  "region_bld", "region_gea",
  "urt", "clim", "inc_cl",
  "arch", "mat", "eneff",
  floor_col
)

segment_cols <- segment_cols[segment_cols %in% names(df_all)]

df_floor_segments <- df_all %>%
  filter(year %in% years_to_run) %>%
  select(all_of(segment_cols)) %>%
  distinct() %>%
  rename(floor_Mm2 = all_of(floor_col))

cat("\nRows after deduplication:", nrow(df_floor_segments), "\n")

cat("\nFloor-space summary before conversion, assumed million m2:\n")
print(summary(df_floor_segments$floor_Mm2))

# If floor_Mm2 is million m2, divide by 1000 to get billion m2.
# If your floor column is not million m2, adjust this.
million_to_billion <- 1000

# ── Step 5: EU-27 aggregate and country-level floor-space ────

df_eu_floor <- df_floor_segments %>%
  group_by(year, scenario) %>%
  summarise(
    floor_Mm2        = sum(floor_Mm2, na.rm = TRUE),
    floor_billion_m2 = floor_Mm2 / million_to_billion,
    .groups = "drop"
  )

df_country_floor <- df_floor_segments %>%
  group_by(year, scenario, region_bld) %>%
  summarise(
    floor_Mm2        = sum(floor_Mm2, na.rm = TRUE),
    floor_billion_m2 = floor_Mm2 / million_to_billion,
    .groups = "drop"
  ) %>%
  mutate(
    region_iso3 = clean_region_label(region_bld),
    region_label = ifelse(
      use_country_names & region_iso3 %in% names(iso3_to_country),
      iso3_to_country[region_iso3],
      region_iso3
    )
  )

cat("\n=== EU-27 floor-space check ===\n")
df_eu_floor %>%
  filter(year %in% c(2020, 2025, 2050, 2100)) %>%
  arrange(year, scenario) %>%
  print(n = 100)

# ── Step 6: Define simplified trajectory groups ──────────────

trajectory_levels <- c(
  "Reference / Slow / Close",
  "Narrow realistic",
  "Narrow technical potential"
)

# For total occupied floor-space, Reference, Slow, and Close
# should overlap or nearly overlap. Use R as representative.
representative_scenarios <- c("R", "N_r", "N_tp")

df_eu_rep <- df_eu_floor %>%
  filter(as.character(scenario) %in% representative_scenarios) %>%
  mutate(
    trajectory = recode(
      as.character(scenario),
      "R"    = "Reference / Slow / Close",
      "N_r"  = "Narrow realistic",
      "N_tp" = "Narrow technical potential"
    ),
    trajectory = factor(trajectory, levels = trajectory_levels)
  )

df_country_rep <- df_country_floor %>%
  filter(as.character(scenario) %in% representative_scenarios) %>%
  mutate(
    trajectory = recode(
      as.character(scenario),
      "R"    = "Reference",
      "N_r"  = "Narrow realistic",
      "N_tp" = "Narrow technical potential"
    ),
    trajectory = factor(
      trajectory,
      levels = c("Reference", "Narrow realistic", "Narrow technical potential")
    )
  )

# ── Step 7: Calculate 2100 country-level positive reductions ─

country_2100 <- df_country_rep %>%
  filter(year == 2100) %>%
  select(region_bld, region_iso3, region_label, trajectory, floor_billion_m2) %>%
  pivot_wider(names_from = trajectory, values_from = floor_billion_m2) %>%
  mutate(
    reduction_narrow_r =
      (Reference - `Narrow realistic`) / Reference * 100,
    reduction_narrow_tp =
      (Reference - `Narrow technical potential`) / Reference * 100
  )

country_reduction_long <- country_2100 %>%
  select(
    region_bld,
    region_iso3,
    region_label,
    reduction_narrow_r,
    reduction_narrow_tp
  ) %>%
  pivot_longer(
    cols = starts_with("reduction_"),
    names_to = "scenario",
    values_to = "reduction_pct"
  ) %>%
  mutate(
    scenario = recode(
      scenario,
      "reduction_narrow_r"  = "Narrow realistic",
      "reduction_narrow_tp" = "Narrow technical potential"
    ),
    scenario = factor(
      scenario,
      levels = c("Narrow realistic", "Narrow technical potential")
    )
  )

# Order countries by technical-potential reduction, highest first
country_order <- country_2100 %>%
  arrange(desc(reduction_narrow_tp)) %>%
  pull(region_label)

country_reduction_long <- country_reduction_long %>%
  mutate(region_label = factor(region_label, levels = country_order))

# ── Step 8: Calculate EU-27 aggregate reductions for vertical lines ─

eu_2100 <- df_eu_rep %>%
  filter(year == 2100) %>%
  select(trajectory, floor_billion_m2) %>%
  pivot_wider(names_from = trajectory, values_from = floor_billion_m2) %>%
  mutate(
    reduction_narrow_r =
      (`Reference / Slow / Close` - `Narrow realistic`) /
      `Reference / Slow / Close` * 100,
    reduction_narrow_tp =
      (`Reference / Slow / Close` - `Narrow technical potential`) /
      `Reference / Slow / Close` * 100
  )

eu_reduction_lines <- tibble(
  scenario = factor(
    c("Narrow realistic", "Narrow technical potential"),
    levels = c("Narrow realistic", "Narrow technical potential")
  ),
  eu_reduction_pct = c(
    eu_2100$reduction_narrow_r,
    eu_2100$reduction_narrow_tp
  )
) %>%
  mutate(
    label = paste0("EU-27: ", round(eu_reduction_pct, 1), "%")
  )

cat("\n=== EU-27 aggregate floor-space reduction in 2100 ===\n")
print(eu_reduction_lines)

# ── Step 9: Common aesthetics ────────────────────────────────
# Colour-blind-friendlier palette:
#   Reference = dark grey
#   Realistic = orange
#   Technical potential = blue

cols_main <- c(
  "Reference / Slow / Close" = "#4D4D4D",
  "Narrow realistic" = "#D55E00",
  "Narrow technical potential" = "#0072B2"
)

cols_reduction <- c(
  "Narrow realistic" = "#D55E00",
  "Narrow technical potential" = "#0072B2"
)

cols_supp <- c(
  "Reference" = "#4D4D4D",
  "Narrow realistic" = "#D55E00",
  "Narrow technical potential" = "#0072B2"
)

lts_main <- c(
  "Reference / Slow / Close" = "solid",
  "Narrow realistic" = "dashed",
  "Narrow technical potential" = "dotted"
)

lts_supp <- c(
  "Reference" = "solid",
  "Narrow realistic" = "dashed",
  "Narrow technical potential" = "dotted"
)

shapes_reduction <- c(
  "Narrow realistic" = 16,
  "Narrow technical potential" = 17
)

# ── Step 10: Themes ──────────────────────────────────────────
# Larger fonts for journal readability.

theme_main <- theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 11),
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 11),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

theme_supp <- theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 10),
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 9),
    strip.text       = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

# ── Step 11: Main Figure Panel A — EU-27 aggregate ───────────
# Unit: billion m2

pA <- ggplot(
  df_eu_rep,
  aes(
    x = year,
    y = floor_billion_m2,
    colour = trajectory,
    linetype = trajectory
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.9) +
  scale_colour_manual(values = cols_main) +
  scale_linetype_manual(values = lts_main) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05)),
    labels = label_number(accuracy = 0.1)
  ) +
  scale_x_continuous(
    breaks = c(2020, 2030, 2040, 2050, 2060, 2080, 2100)
  ) +
  labs(
    title = "A. EU-27 residential floor-space trajectories",
    x = "Year",
    y = expression("Residential floor space (billion m"^2*")")
  ) +
  theme_main +
  guides(
    colour   = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  )

# ── Step 12: Main Figure Panel B — Country positive reductions ─
# Unit: %

pB <- ggplot(
  country_reduction_long,
  aes(
    x = reduction_pct,
    y = region_label,
    colour = scenario,
    shape = scenario
  )
) +
  geom_vline(
    data = eu_reduction_lines,
    aes(xintercept = eu_reduction_pct, colour = scenario),
    linetype = "dashed",
    linewidth = 0.7,
    alpha = 0.85,
    show.legend = FALSE
  ) +
  geom_label(
    data = eu_reduction_lines,
    aes(
      x = eu_reduction_pct,
      y = Inf,
      label = label,
      colour = scenario
    ),
    inherit.aes = FALSE,
    vjust = 1.15,
    hjust = -0.05,
    size = 3.5,
    label.size = 0.2,
    fill = "white",
    show.legend = FALSE
  ) +
  geom_point(size = 2.8, alpha = 0.9) +
  scale_colour_manual(values = cols_reduction) +
  scale_shape_manual(values = shapes_reduction) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = pretty_breaks(n = 6),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title = "B. Country-level floor-space reduction in 2100",
    subtitle = "Dashed vertical lines show EU-27 aggregate reductions",
    x = "Reduction relative to Reference in 2100",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_main +
  theme(
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    plot.margin = margin(t = 10, r = 45, b = 5, l = 5)
  ) +
  guides(
    colour = guide_legend(nrow = 1),
    shape  = guide_legend(nrow = 1)
  )

# ── Step 13: Combine main figure ─────────────────────────────

p_main <- pA / pB +
  plot_layout(heights = c(1.0, 1.45)) +
  plot_annotation(
    title = "Residential floor-space trajectories under circular economy scenarios",
    subtitle = "Reference, Slow, and Close show similar occupied floor-space trajectories; Narrow scenarios reduce floor-space demand.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12)
    )
  )

print(p_main)

# ── Step 14: Export high-quality main figure ─────────────────

ggsave(
  filename = file.path(plot_dir, "fig_main_EU27_residential_floorspace_trajectory_and_country_reduction.png"),
  plot     = p_main,
  width    = 9.5,
  height   = 10.5,
  dpi      = 300,
  bg       = "white"
)

ggsave(
  filename = file.path(plot_dir, "fig_main_EU27_residential_floorspace_trajectory_and_country_reduction.tiff"),
  plot     = p_main,
  width    = 9.5,
  height   = 10.5,
  dpi      = 300,
  compression = "lzw",
  bg       = "white"
)


# ── Step 15: Supplementary figure — country trajectories ─────
# Unit: million m2
# EU-27 aggregate excluded because it is already shown in Panel A.
# Layout: 3 columns × 9 rows for EU-27 countries.

df_country_rep_for_supp <- df_country_rep %>%
  mutate(
    trajectory = as.character(trajectory)
  ) %>%
  select(
    year,
    scenario,
    trajectory,
    region_bld,
    region_iso3,
    region_label,
    floor_Mm2,
    floor_billion_m2
  )

region_order_supp <- sort(unique(as.character(df_country_rep$region_label)))

df_supp <- df_country_rep_for_supp %>%
  mutate(
    region_label = factor(region_label, levels = region_order_supp),
    trajectory = factor(
      trajectory,
      levels = c("Reference", "Narrow realistic", "Narrow technical potential")
    )
  )

p_supp <- ggplot(
  df_supp,
  aes(
    x = year,
    y = floor_Mm2,
    colour = trajectory,
    linetype = trajectory
  )
) +
  geom_line(linewidth = 1.0) +
  scale_colour_manual(values = cols_supp) +
  scale_linetype_manual(values = lts_supp) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05)),
    labels = label_number(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = c(2020, 2050, 2100)
  ) +
  facet_wrap(~ region_label, scales = "free_y", ncol = 3) +
  labs(
    title = "Country-level residential floor-space trajectories",
    subtitle = "EU-27 countries; y-axis is free by country",
    x = "Year",
    y = expression("Residential floor space (million m"^2*")")
  ) +
  theme_supp +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  ) +
  guides(
    colour   = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  )

print(p_supp)

# ── Step 16: Export high-quality supplementary figure ────────

ggsave(
  filename = file.path(plot_dir, "fig_supp_EU27_country_residential_floorspace_trajectories_3col_million_m2.png"),
  plot     = p_supp,
  width    = 12,
  height   = 14,
  dpi      = 300,
  bg       = "white"
)

ggsave(
  filename = file.path(plot_dir, "fig_supp_EU27_country_residential_floorspace_trajectories_3col_million_m2.tiff"),
  plot     = p_supp,
  width    = 12,
  height   = 14,
  dpi      = 300,
  compression = "lzw",
  bg       = "white"
)


# ── Step 17: Save summaries and processed data ───────────────

# EU-27 2100 summary for all loaded scenarios
floor_2100 <- df_eu_floor %>%
  filter(year == 2100)

ref_2100 <- floor_2100 %>%
  filter(scenario == "R") %>%
  pull(floor_Mm2)

summary_2100 <- floor_2100 %>%
  mutate(
    pct_change_vs_R = (floor_Mm2 - ref_2100) / ref_2100 * 100,
    pct_reduction_vs_R = (ref_2100 - floor_Mm2) / ref_2100 * 100,
    scenario = factor(scenario, levels = scenarios)
  ) %>%
  arrange(scenario) %>%
  transmute(
    scenario,
    `2100 floor space (million m2)` = round(floor_Mm2, 1),
    `2100 floor space (billion m2)` = round(floor_billion_m2, 3),
    `% change vs R` = round(pct_change_vs_R, 2),
    `% reduction vs R` = round(pct_reduction_vs_R, 2)
  )

cat("\n=== 2100 EU-27 residential floor-space summary ===\n")
print(summary_2100, n = Inf)

write_csv(
  summary_2100,
  file.path(plot_dir, "summary_2100_EU27_resid_floorspace.csv")
)

write_csv(
  country_2100,
  file.path(plot_dir, "summary_2100_country_floorspace_reduction.csv")
)

write_csv(
  country_reduction_long,
  file.path(plot_dir, "data_country_floorspace_reduction_long.csv")
)

write_csv(
  eu_reduction_lines,
  file.path(plot_dir, "summary_2100_EU27_reduction_lines.csv")
)

write_csv(
  df_eu_floor,
  file.path(plot_dir, "data_EU27_resid_floorspace_all_scenarios.csv")
)

write_csv(
  df_country_floor,
  file.path(plot_dir, "data_country_resid_floorspace_all_scenarios.csv")
)

cat("\nDone. Main and supplementary floor-space figures saved to:\n", plot_dir, "\n")