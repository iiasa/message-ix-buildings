# ============================================================
# Fig 2 — Country-level residential floor-space reduction in 2050
# ============================================================

library(tidyverse)
library(readr)
library(scales)

# ── User settings ───────────────────────────────────────────

output_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/output"

plot_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

target_year <- 2050

years_to_run <- c(seq(2020, 2060, 5), seq(2070, 2100, 10))

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

clean_region_label <- function(x) {
  x %>%
    str_replace("^C-", "") %>%
    str_replace("^[A-Z]+-", "")
}

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

# ── Load scenario files ─────────────────────────────────────

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

# ── Identify floor-space column ─────────────────────────────

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

# ── Deduplicate building segments ───────────────────────────
# Energy output can repeat floor space across energy carriers/end uses.

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

million_to_billion <- 1000

# ── Country-level floor space ───────────────────────────────

df_country_floor <- df_floor_segments %>%
  group_by(year, scenario, region_bld) %>%
  summarise(
    floor_Mm2 = sum(floor_Mm2, na.rm = TRUE),
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

df_eu_floor <- df_floor_segments %>%
  group_by(year, scenario) %>%
  summarise(
    floor_Mm2 = sum(floor_Mm2, na.rm = TRUE),
    floor_billion_m2 = floor_Mm2 / million_to_billion,
    .groups = "drop"
  )

# ── Keep representative scenarios ───────────────────────────

representative_scenarios <- c("R", "N_r", "N_tp")

df_country_rep <- df_country_floor %>%
  filter(as.character(scenario) %in% representative_scenarios) %>%
  mutate(
    trajectory = recode(
      as.character(scenario),
      "R"    = "Reference",
      "N_r"  = "Narrow R",
      "N_tp" = "Narrow TP"
    ),
    trajectory = factor(
      trajectory,
      levels = c("Reference", "Narrow R", "Narrow TP")
    )
  )

df_eu_rep <- df_eu_floor %>%
  filter(as.character(scenario) %in% representative_scenarios) %>%
  mutate(
    trajectory = recode(
      as.character(scenario),
      "R"    = "Reference",
      "N_r"  = "Narrow R",
      "N_tp" = "Narrow TP"
    ),
    trajectory = factor(
      trajectory,
      levels = c("Reference", "Narrow R", "Narrow TP")
    )
  )

# ── Calculate country-level reductions in target year ───────

country_target <- df_country_rep %>%
  filter(year == target_year) %>%
  select(region_bld, region_iso3, region_label, trajectory, floor_billion_m2) %>%
  pivot_wider(names_from = trajectory, values_from = floor_billion_m2) %>%
  mutate(
    reduction_narrow_r =
      (Reference - `Narrow R`) / Reference * 100,
    reduction_narrow_tp =
      (Reference - `Narrow TP`) / Reference * 100
  )

country_reduction_long <- country_target %>%
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
      "reduction_narrow_r"  = "Narrow R",
      "reduction_narrow_tp" = "Narrow TP"
    ),
    scenario = factor(
      scenario,
      levels = c("Narrow R", "Narrow TP")
    )
  )

country_order <- country_target %>%
  arrange(desc(reduction_narrow_tp)) %>%
  pull(region_label)

country_reduction_long <- country_reduction_long %>%
  mutate(region_label = factor(region_label, levels = country_order))

# ── EU-27 aggregate reductions for vertical lines ───────────

eu_target <- df_eu_rep %>%
  filter(year == target_year) %>%
  select(trajectory, floor_billion_m2) %>%
  pivot_wider(names_from = trajectory, values_from = floor_billion_m2) %>%
  mutate(
    reduction_narrow_r =
      (Reference - `Narrow R`) / Reference * 100,
    reduction_narrow_tp =
      (Reference - `Narrow TP`) / Reference * 100
  )

eu_reduction_lines <- tibble(
  scenario = factor(
    c("Narrow R", "Narrow TP"),
    levels = c("Narrow R", "Narrow TP")
  ),
  eu_reduction_pct = c(
    eu_target$reduction_narrow_r,
    eu_target$reduction_narrow_tp
  )
) %>%
  mutate(
    label = paste0("EU-27: ", round(eu_reduction_pct, 1), "%"),
    label_x = case_when(
      scenario == "Narrow R"  ~ eu_reduction_pct - 3.5,  # move left of dashed line
      scenario == "Narrow TP" ~ eu_reduction_pct + 1.0   # keep a bit to the right
    ),
    label_hjust = case_when(
      scenario == "Narrow R"  ~ 1,
      scenario == "Narrow TP" ~ 0
    )
  )
cat("\n=== EU-27 aggregate floor-space reduction in ", target_year, " ===\n", sep = "")
print(eu_reduction_lines)

# ── Aesthetics ──────────────────────────────────────────────

cols_reduction <- c(
  "Narrow R" = "#D55E00",
  "Narrow TP" = "#0072B2"
)

shapes_reduction <- c(
  "Narrow R" = 16,
  "Narrow TP" = 17
)

theme_fig <- theme_minimal(base_size = 14) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 12),
    plot.title       = element_text(face = "bold", size = 18),
    plot.subtitle    = element_text(size = 13, margin = margin(b = 10)),
    axis.title       = element_text(size = 13),
    axis.text.x      = element_text(size = 12),
    axis.text.y      = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.margin      = margin(t = 10, r = 50, b = 10, l = 10)
  )

# ── Plot ────────────────────────────────────────────────────

p_fig2 <- ggplot(
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
      x = label_x,
      y = Inf,
      label = label,
      colour = scenario,
      hjust = label_hjust
    ),
    inherit.aes = FALSE,
    vjust = 1.15,
    size = 3.6,
    label.size = 0.2,
    fill = "white",
    show.legend = FALSE
  ) +
  geom_point(size = 3.0, alpha = 0.9) +
  scale_colour_manual(values = cols_reduction) +
  scale_shape_manual(values = shapes_reduction) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = pretty_breaks(n = 6),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title = paste0("Country-level residential floor-space reduction in ", target_year),
    subtitle = "Dashed vertical lines show EU-27 aggregate reductions",
    x = paste0("Reduction relative to Reference in ", target_year),
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_fig +
  guides(
    colour = guide_legend(nrow = 1),
    shape  = guide_legend(nrow = 1)
  )

print(p_fig2)

# ── Save ────────────────────────────────────────────────────

file_stub <- paste0("Fig2_country_residential_floor_space_reduction_", target_year)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".png")),
  plot     = p_fig2,
  width    = 9,
  height   = 8.5,
  dpi      = 300,
  bg       = "white"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".tiff")),
  plot     = p_fig2,
  width    = 9,
  height   = 8.5,
  dpi      = 300,
  compression = "lzw",
  bg       = "white"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".pdf")),
  plot     = p_fig2,
  width    = 9,
  height   = 8.5,
  device   = cairo_pdf,
  bg       = "white"
)

# ── Save processed data ─────────────────────────────────────

write_csv(
  country_reduction_long,
  file.path(plot_dir, paste0(file_stub, "_data_long.csv"))
)

write_csv(
  eu_reduction_lines,
  file.path(plot_dir, paste0(file_stub, "_EU27_reduction_lines.csv"))
)

cat("\nSaved Fig 2 country reduction figure to:\n", plot_dir, "\n")


############################
### EXTRACT DATA FOR REPORT
############################
# ============================================================
# Extract exact Fig. 2 numbers for Results text
# Country-level residential floor-space reduction in 2050
# ============================================================

cat("\n\n============================================================\n")
cat("Exact Fig. 2 numbers for Results text\n")
cat("Country-level residential floor-space reduction in ", target_year, "\n", sep = "")
cat("============================================================\n")

# Helper formatting functions
fmt_pct <- function(x, digits = 1) {
  paste0(format(round(x, digits), nsmall = digits, trim = TRUE), "%")
}

fmt_num <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

# ── 1) Country-level reduction summary by scenario ──────────

fig2_country_summary <- country_reduction_long %>%
  group_by(scenario) %>%
  summarise(
    n_countries = n(),
    min_reduction_pct = min(reduction_pct, na.rm = TRUE),
    q25_reduction_pct = quantile(reduction_pct, 0.25, na.rm = TRUE),
    median_reduction_pct = median(reduction_pct, na.rm = TRUE),
    mean_reduction_pct = mean(reduction_pct, na.rm = TRUE),
    q75_reduction_pct = quantile(reduction_pct, 0.75, na.rm = TRUE),
    max_reduction_pct = max(reduction_pct, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nCountry-level reduction summary by scenario:\n")
fig2_country_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print(n = Inf, width = Inf)

# ── 2) EU-27 aggregate reductions ───────────────────────────

fig2_eu_summary <- eu_reduction_lines %>%
  select(
    scenario,
    eu_reduction_pct
  ) %>%
  mutate(
    eu_reduction_pct = as.numeric(eu_reduction_pct)
  )

cat("\nEU-27 aggregate reductions:\n")
fig2_eu_summary %>%
  mutate(eu_reduction_pct = round(eu_reduction_pct, 2)) %>%
  print(n = Inf, width = Inf)

# ── 3) Countries with smallest and largest reductions ───────

fig2_country_extremes <- country_reduction_long %>%
  group_by(scenario) %>%
  arrange(reduction_pct, .by_group = TRUE) %>%
  summarise(
    lowest_country = as.character(first(region_label)),
    lowest_iso3 = as.character(first(region_iso3)),
    lowest_reduction_pct = first(reduction_pct),
    highest_country = as.character(last(region_label)),
    highest_iso3 = as.character(last(region_iso3)),
    highest_reduction_pct = last(reduction_pct),
    .groups = "drop"
  )

cat("\nCountries with lowest and highest reductions:\n")
fig2_country_extremes %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print(n = Inf, width = Inf)

# ── 4) Top and bottom 5 countries by scenario ───────────────

fig2_top_bottom_countries <- country_reduction_long %>%
  group_by(scenario) %>%
  arrange(reduction_pct, .by_group = TRUE) %>%
  mutate(rank_low_to_high = row_number()) %>%
  filter(
    rank_low_to_high <= 5 |
      rank_low_to_high > max(rank_low_to_high) - 5
  ) %>%
  mutate(
    group = ifelse(rank_low_to_high <= 5, "Lowest 5", "Highest 5")
  ) %>%
  ungroup() %>%
  select(
    scenario,
    group,
    rank_low_to_high,
    region_label,
    region_iso3,
    reduction_pct
  ) %>%
  arrange(scenario, group, rank_low_to_high)

cat("\nTop and bottom 5 countries by scenario:\n")
fig2_top_bottom_countries %>%
  mutate(reduction_pct = round(reduction_pct, 2)) %>%
  print(n = Inf, width = Inf)

# ── 5) Gap between Narrow TP and Narrow R by country ─────────

fig2_gap_by_country <- country_reduction_long %>%
  select(region_label, region_iso3, scenario, reduction_pct) %>%
  pivot_wider(
    names_from = scenario,
    values_from = reduction_pct
  ) %>%
  mutate(
    gap_tp_minus_r_pct_points = `Narrow TP` - `Narrow R`
  ) %>%
  arrange(desc(gap_tp_minus_r_pct_points))

cat("\nGap between Narrow TP and Narrow R by country:\n")
fig2_gap_by_country %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print(n = Inf, width = Inf)

fig2_gap_summary <- fig2_gap_by_country %>%
  summarise(
    min_gap_pct_points = min(gap_tp_minus_r_pct_points, na.rm = TRUE),
    median_gap_pct_points = median(gap_tp_minus_r_pct_points, na.rm = TRUE),
    mean_gap_pct_points = mean(gap_tp_minus_r_pct_points, na.rm = TRUE),
    max_gap_pct_points = max(gap_tp_minus_r_pct_points, na.rm = TRUE)
  )

cat("\nSummary of TP minus R gap, percentage points:\n")
fig2_gap_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print(n = Inf, width = Inf)

# ── 6) Text-ready sentences for Results section ─────────────

cat("\nText-ready Results sentences:\n")

# Pull values for compact text
narrow_r_summary <- fig2_country_summary %>%
  filter(scenario == "Narrow R")

narrow_tp_summary <- fig2_country_summary %>%
  filter(scenario == "Narrow TP")

eu_r <- fig2_eu_summary %>%
  filter(scenario == "Narrow R") %>%
  pull(eu_reduction_pct)

eu_tp <- fig2_eu_summary %>%
  filter(scenario == "Narrow TP") %>%
  pull(eu_reduction_pct)

gap_median <- fig2_gap_summary$median_gap_pct_points
gap_mean <- fig2_gap_summary$mean_gap_pct_points

cat(
  "\nUnder Narrow R, country-level residential floor-space reductions in ",
  target_year,
  " range from ",
  fmt_pct(narrow_r_summary$min_reduction_pct),
  " to ",
  fmt_pct(narrow_r_summary$max_reduction_pct),
  " relative to Reference, with an EU-27 aggregate reduction of ",
  fmt_pct(eu_r),
  ".\n",
  sep = ""
)

cat(
  "\nUnder Narrow TP, country-level residential floor-space reductions in ",
  target_year,
  " range from ",
  fmt_pct(narrow_tp_summary$min_reduction_pct),
  " to ",
  fmt_pct(narrow_tp_summary$max_reduction_pct),
  " relative to Reference, with an EU-27 aggregate reduction of ",
  fmt_pct(eu_tp),
  ".\n",
  sep = ""
)

cat(
  "\nThe median country-level gap between Narrow TP and Narrow R is ",
  fmt_num(gap_median, 1),
  " percentage points, while the mean gap is ",
  fmt_num(gap_mean, 1),
  " percentage points.\n",
  sep = ""
)

# ── 7) Save text-supporting tables ──────────────────────────

write_csv(
  fig2_country_summary,
  file.path(plot_dir, paste0(file_stub, "_country_summary_for_text.csv"))
)

write_csv(
  fig2_eu_summary,
  file.path(plot_dir, paste0(file_stub, "_EU27_summary_for_text.csv"))
)

write_csv(
  fig2_country_extremes,
  file.path(plot_dir, paste0(file_stub, "_country_extremes_for_text.csv"))
)

write_csv(
  fig2_top_bottom_countries,
  file.path(plot_dir, paste0(file_stub, "_top_bottom_countries_for_text.csv"))
)

write_csv(
  fig2_gap_by_country,
  file.path(plot_dir, paste0(file_stub, "_TP_minus_R_gap_by_country.csv"))
)

write_csv(
  fig2_gap_summary,
  file.path(plot_dir, paste0(file_stub, "_TP_minus_R_gap_summary.csv"))
)

cat("\nSaved Fig. 2 text-supporting tables to:\n")
cat(plot_dir, "\n")