# ============================================================
# Fig. 1. EU-27 floor-space trajectories
# Residential + commercial in one faceted figure
#
# Key feature:
#   Works for both residential and commercial outputs.
#   Does NOT hardcode region_bld; it searches for any matching
#   report_STURM_<scenario>_<sector>_*_energy.csv file.
# ============================================================

library(tidyverse)
library(readr)
library(scales)

# ── User settings ───────────────────────────────────────────

output_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/output"

plot_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

years_to_plot <- c(seq(2020, 2060, 5), seq(2070, 2100, 10))

# Sectors to include
sectors <- c("resid", "comm")

# Scenario files available in output folder
scenarios <- c(
  "R",
  "N_r", "N_tp",
  "S_r", "S_tp",
  "C_r", "C_tp"
)

# Representative scenarios to show in the figure
# R represents Reference / Slow / Close because occupied floor-space overlaps
representative_scenarios <- c("R", "N_r", "N_tp")

# ── Labels ──────────────────────────────────────────────────

sector_labels <- c(
  "resid" = "Residential",
  "comm"  = "Commercial"
)

trajectory_levels <- c(
  "Reference / Slow / Close",
  "Narrow R",
  "Narrow TP"
)

cols_main <- c(
  "Reference / Slow / Close" = "#4D4D4D",
  "Narrow R" = "#D55E00",
  "Narrow TP" = "#0072B2"
)

lts_main <- c(
  "Reference / Slow / Close" = "solid",
  "Narrow R" = "longdash",
  "Narrow TP" = "dotted"
)

# ── Helper: find STURM energy file ──────────────────────────

find_energy_file <- function(scenario_code, sector_code, output_dir) {
  
  pattern <- paste0(
    "^report_STURM_",
    scenario_code,
    "_",
    sector_code,
    "_.*_energy\\.csv$"
  )
  
  files <- list.files(
    output_dir,
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop(
      "No energy file found for scenario = ",
      scenario_code,
      ", sector = ",
      sector_code,
      "\nPattern used: ",
      pattern
    )
  }
  
  # Prefer region_bld if available; otherwise use first match, e.g. R12
  region_bld_file <- files[str_detect(files, "_region_bld_energy\\.csv$")]
  
  if (length(region_bld_file) > 0) {
    return(region_bld_file[1])
  }
  
  return(files[1])
}

# ── Helper: read and aggregate floor space ──────────────────

read_floor_data <- function(scenario_code,
                            sector_code,
                            sector_label,
                            scenario_label,
                            output_dir) {
  
  file_path <- find_energy_file(
    scenario_code = scenario_code,
    sector_code = sector_code,
    output_dir = output_dir
  )
  
  cat("Loaded:", basename(file_path), "\n")
  
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Identify floor-space column
  candidate_floor_cols <- c(
    "floor_Mm2",
    "floor",
    "floor_m2",
    "floorspace",
    "floorspace_Mm2",
    "value"
  )
  
  floor_col <- candidate_floor_cols[candidate_floor_cols %in% names(df)][1]
  
  if (is.na(floor_col)) {
    stop(
      "Could not find floor-space column in file: ",
      basename(file_path),
      "\nAvailable columns:\n",
      paste(names(df), collapse = ", ")
    )
  }
  
  cat("  Using floor-space column:", floor_col, "\n")
  
  # Deduplicate repeated energy-carrier/end-use rows
  # Keep all building-stock dimensions that exist in the file.
  dedup_cols <- intersect(
    c(
      "year",
      "region_bld",
      "region_gea",
      "urt",
      "clim",
      "inc_cl",
      "arch",
      "mat",
      "eneff",
      floor_col
    ),
    names(df)
  )
  
  df %>%
    filter(year %in% years_to_plot) %>%
    distinct(across(all_of(dedup_cols))) %>%
    rename(floor_Mm2 = all_of(floor_col)) %>%
    group_by(year) %>%
    summarise(
      floor_billion_m2 = sum(floor_Mm2, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) %>%
    mutate(
      scenario = scenario_code,
      scenario_label = scenario_label,
      sector = sector_code,
      sector_label = sector_label
    )
}

# ── Scenario-sector map ─────────────────────────────────────

scenario_map <- tibble(
  scenario = representative_scenarios,
  scenario_label = c(
    "Reference / Slow / Close",
    "Narrow R",
    "Narrow TP"
  )
)

sector_map <- tibble(
  sector = sectors,
  sector_label = sector_labels[sectors]
)

# ── Load data ───────────────────────────────────────────────

floor_plot_data <- crossing(
  scenario_map,
  sector_map
) %>%
  pmap_dfr(function(scenario, scenario_label, sector, sector_label) {
    read_floor_data(
      scenario_code  = scenario,
      sector_code    = sector,
      sector_label   = sector_label,
      scenario_label = scenario_label,
      output_dir     = output_dir
    )
  })

# ── Factor ordering ─────────────────────────────────────────

floor_plot_data <- floor_plot_data %>%
  mutate(
    scenario_label = factor(
      scenario_label,
      levels = trajectory_levels
    ),
    sector_label = factor(
      sector_label,
      levels = c("Residential", "Commercial")
    )
  )

# ── Sanity check table ──────────────────────────────────────

cat("\nSelected-year floor-space summary:\n")

floor_plot_data %>%
  filter(year %in% c(2025, 2030, 2050, 2100)) %>%
  select(sector_label, scenario_label, year, floor_billion_m2) %>%
  pivot_wider(
    names_from = scenario_label,
    values_from = floor_billion_m2
  ) %>%
  arrange(sector_label, year) %>%
  print(n = Inf, width = Inf)

# ── Theme ───────────────────────────────────────────────────

theme_fig <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 20,
      margin = margin(b = 10)
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.width = unit(1.4, "cm"),
    
    strip.text.y.right = element_text(
      face = "bold",
      size = 13,
      angle = 270
    ),
    strip.background = element_blank(),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88"),
    panel.grid.major.y = element_line(colour = "grey85"),
    
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 12, 10, 10)
  )

# ── Plot ────────────────────────────────────────────────────

p_floor_combined <- ggplot(
  floor_plot_data,
  aes(
    x = year,
    y = floor_billion_m2,
    colour = scenario_label,
    linetype = scenario_label
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.6) +
  facet_grid(
    rows = vars(sector_label),
    scales = "free_y"
  ) +
  scale_colour_manual(values = cols_main) +
  scale_linetype_manual(values = lts_main) +
  scale_x_continuous(
    breaks = c(2020, 2030, 2040, 2050, 2060, 2080, 2100)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.06)),
    labels = label_number(accuracy = 0.1)
  ) +
  labs(
    title = "EU-27 floor-space trajectories",
    x = "Year",
    y = expression("Floor space (billion m"^2*")")
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE),
    linetype = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_fig

print(p_floor_combined)

# ── Save figure and data ────────────────────────────────────

file_stub <- "Fig1_v4_EU27_residential_commercial_floor_space_trajectories"

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".png")),
  plot = p_floor_combined,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".tiff")),
  plot = p_floor_combined,
  width = 10,
  height = 7,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".pdf")),
  plot = p_floor_combined,
  width = 10,
  height = 7,
  device = cairo_pdf,
  bg = "white"
)

write_csv(
  floor_plot_data,
  file.path(plot_dir, paste0(file_stub, "_data.csv"))
)

cat("\nSaved combined floor-space figure to:\n")
cat(plot_dir, "\n")


###################
### DATA EXTRACTION
###################

# ============================================================
# Extract exact floor-space numbers for Results text
# Fig. 1: Residential + commercial floor-space trajectories
# ============================================================

cat("\n\n============================================================\n")
cat("Exact floor-space numbers for Results text\n")
cat("============================================================\n")

# Years commonly used in the Results narrative
text_years <- c(2020, 2050, 2100)

# Helper for nice formatting
fmt_num <- function(x, digits = 1) {
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

fmt_change <- function(x, digits = 1) {
  out <- round(x, digits)
  ifelse(out > 0,
         paste0("+", format(out, nsmall = digits, trim = TRUE)),
         format(out, nsmall = digits, trim = TRUE))
}

# 1) Selected values for all plotted trajectories
fig1_selected_values <- floor_plot_data %>%
  filter(year %in% text_years) %>%
  select(
    sector_label,
    scenario_label,
    year,
    floor_billion_m2
  ) %>%
  arrange(
    sector_label,
    scenario_label,
    year
  )

cat("\nSelected floor-space values, billion m2:\n")
fig1_selected_values %>%
  mutate(floor_billion_m2 = round(floor_billion_m2, 2)) %>%
  print(n = Inf, width = Inf)

# 2) Wide table for easier checking
fig1_selected_values_wide <- fig1_selected_values %>%
  mutate(floor_billion_m2 = round(floor_billion_m2, 2)) %>%
  pivot_wider(
    names_from = year,
    values_from = floor_billion_m2,
    names_prefix = "year_"
  ) %>%
  arrange(sector_label, scenario_label)

cat("\nSelected floor-space values, wide format:\n")
fig1_selected_values_wide %>%
  print(n = Inf, width = Inf)

# 3) Reference trajectory numbers for paragraph text
fig1_reference_text_numbers <- floor_plot_data %>%
  filter(
    scenario_label == "Reference / Slow / Close",
    year %in% text_years
  ) %>%
  select(sector_label, year, floor_billion_m2) %>%
  pivot_wider(
    names_from = year,
    values_from = floor_billion_m2,
    names_prefix = "y"
  ) %>%
  mutate(
    change_2020_2050 = y2050 - y2020,
    change_2050_2100 = y2100 - y2050,
    pct_change_2020_2050 = 100 * (y2050 / y2020 - 1),
    pct_change_2050_2100 = 100 * (y2100 / y2050 - 1)
  )

cat("\nReference trajectory text numbers:\n")
fig1_reference_text_numbers %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print(n = Inf, width = Inf)

# 4) Narrow reductions relative to Reference
fig1_narrow_reductions <- floor_plot_data %>%
  filter(year %in% c(2050, 2100)) %>%
  select(
    sector_label,
    scenario_label,
    year,
    floor_billion_m2
  ) %>%
  pivot_wider(
    names_from = scenario_label,
    values_from = floor_billion_m2
  ) %>%
  mutate(
    Narrow_R_abs_reduction = `Narrow R` - `Reference / Slow / Close`,
    Narrow_TP_abs_reduction = `Narrow TP` - `Reference / Slow / Close`,
    Narrow_R_pct_reduction = 100 * (`Narrow R` / `Reference / Slow / Close` - 1),
    Narrow_TP_pct_reduction = 100 * (`Narrow TP` / `Reference / Slow / Close` - 1)
  ) %>%
  arrange(sector_label, year)

cat("\nNarrow reductions relative to Reference:\n")
fig1_narrow_reductions %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print(n = Inf, width = Inf)

# 5) Console-ready sentences for the Results section
cat("\nText-ready Reference trajectory sentences:\n")

fig1_reference_text_numbers %>%
  rowwise() %>%
  mutate(
    sentence = paste0(
      as.character(sector_label), 
      ": Reference floor space changes from ",
      fmt_num(y2020, 1), " billion m2 in 2020 to ",
      fmt_num(y2050, 1), " billion m2 in 2050, and ",
      fmt_num(y2100, 1), " billion m2 in 2100. ",
      "This corresponds to a ",
      fmt_change(change_2020_2050, 1), " billion m2 change from 2020 to 2050 (",
      fmt_change(pct_change_2020_2050, 1), "%), followed by a ",
      fmt_change(change_2050_2100, 1), " billion m2 change from 2050 to 2100 (",
      fmt_change(pct_change_2050_2100, 1), "%)."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

cat("\nText-ready Narrow comparison sentences:\n")

fig1_narrow_reductions %>%
  rowwise() %>%
  mutate(
    sentence = paste0(
      as.character(sector_label), ", ", year, ": Narrow R = ",
      fmt_num(`Narrow R`, 1), " billion m2 (",
      fmt_change(Narrow_R_pct_reduction, 1), "% vs Reference); Narrow TP = ",
      fmt_num(`Narrow TP`, 1), " billion m2 (",
      fmt_change(Narrow_TP_pct_reduction, 1), "% vs Reference)."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# 6) Save tables for traceability
write_csv(
  fig1_selected_values,
  file.path(plot_dir, "Fig1_v4_selected_floor_space_values_for_text.csv")
)

write_csv(
  fig1_reference_text_numbers,
  file.path(plot_dir, "Fig1_v4_reference_floor_space_numbers_for_text.csv")
)

write_csv(
  fig1_narrow_reductions,
  file.path(plot_dir, "Fig1_v4_narrow_floor_space_reductions_for_text.csv")
)

cat("\nSaved text-supporting Fig. 1 tables to:\n")
cat(plot_dir, "\n")