# ============================================================
# Fig. 5. Annual construction material demand
# EU-27 residential + commercial sectors
# ============================================================

library(tidyverse)
library(readr)
library(scales)
library(grid)

# ── User settings ───────────────────────────────────────────

output_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/output"

plot_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

sectors_to_load <- c("resid", "comm")

years_to_plot <- c(seq(2025, 2060, 5), seq(2070, 2100, 10))

scenarios_to_load <- c(
  "R",
  "N_r", "N_tp",
  "S_r", "S_tp",
  "C_r", "C_tp",
  "A_r", "A_tp"
)

# Plot R as representative for Reference / Close
# Close scenarios are loaded only for the overlap check
scenarios_to_plot <- c(
  "R",
  "N_r", "N_tp",
  "S_r", "S_tp",
  "A_r", "A_tp"
)

# ── Labels ──────────────────────────────────────────────────

scenario_labels <- c(
  "R"    = "Reference / Close",
  "N_r"  = "Narrow R",
  "N_tp" = "Narrow TP",
  "S_r"  = "Slow R",
  "S_tp" = "Slow TP",
  "C_r"  = "Close R",
  "C_tp" = "Close TP",
  "A_r"  = "Combined R",
  "A_tp" = "Combined TP"
)

scenario_order <- c(
  "Reference / Close",
  "Narrow R",
  "Narrow TP",
  "Slow R",
  "Slow TP",
  "Combined R",
  "Combined TP"
)

sector_labels <- c(
  "resid" = "Residential",
  "comm"  = "Commercial"
)

scenario_colours <- c(
  "Reference / Close" = "#4D4D4D",
  "Narrow R"          = "#D55E00",
  "Narrow TP"         = "#D55E00",
  "Slow R"            = "#0072B2",
  "Slow TP"           = "#0072B2",
  "Combined R"        = "#CC79A7",
  "Combined TP"       = "#CC79A7"
)

scenario_linetypes <- c(
  "Reference / Close" = "solid",
  "Narrow R"          = "solid",
  "Narrow TP"         = "longdash",
  "Slow R"            = "solid",
  "Slow TP"           = "longdash",
  "Combined R"        = "solid",
  "Combined TP"       = "longdash"
)

# ── Helper: find material report file ───────────────────────

find_material_file <- function(scenario_code, sector_code, output_dir) {
  
  pattern <- paste0(
    "^report_STURM_",
    scenario_code,
    "_",
    sector_code,
    "_.*_material\\.csv$"
  )
  
  files <- list.files(
    output_dir,
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    warning(
      "No material file found for scenario = ",
      scenario_code,
      ", sector = ",
      sector_code,
      "\nPattern used: ",
      pattern
    )
    return(NA_character_)
  }
  
  # Prefer the expected reporting level by sector
  if (sector_code == "resid") {
    preferred_file <- files[str_detect(files, "_region_bld_material\\.csv$")]
  } else if (sector_code == "comm") {
    preferred_file <- files[str_detect(files, "_R12_material\\.csv$")]
  } else {
    preferred_file <- character(0)
  }
  
  if (length(preferred_file) > 0) {
    return(preferred_file[1])
  }
  
  files[1]
}

# ── Helper: read and aggregate material demand ──────────────

read_material_demand <- function(scenario_code,
                                 sector_code,
                                 output_dir) {
  
  file_path <- find_material_file(
    scenario_code = scenario_code,
    sector_code   = sector_code,
    output_dir    = output_dir
  )
  
  if (is.na(file_path) || !file.exists(file_path)) {
    return(NULL)
  }
  
  cat("Loaded:", basename(file_path), "\n")
  
  df <- read_csv(file_path, show_col_types = FALSE)
  
  if (!"mat_demand_Mt" %in% names(df)) {
    stop(
      "Column 'mat_demand_Mt' not found in ",
      basename(file_path),
      ". Available columns:\n",
      paste(names(df), collapse = ", ")
    )
  }
  
  if (!"material" %in% names(df)) {
    stop(
      "Column 'material' not found in ",
      basename(file_path),
      ". Available columns:\n",
      paste(names(df), collapse = ", ")
    )
  }
  
  df_clean <- df %>%
    filter(year %in% years_to_plot)
  
  # Remove possible total rows if present
  if ("mat" %in% names(df_clean)) {
    df_clean <- df_clean %>%
      filter(!str_to_lower(mat) %in% c("all", "total", "sum"))
  }
  
  # Drop NA material rows, but warn if any exist
  n_material_na <- sum(is.na(df_clean$material))
  
  if (n_material_na > 0) {
    warning(
      basename(file_path),
      " has ",
      n_material_na,
      " rows with material = NA. These rows will be excluded."
    )
  }
  
  df_clean %>%
    filter(!is.na(material)) %>%
    group_by(year) %>%
    summarise(
      material_Gt = sum(mat_demand_Mt, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) %>%
    mutate(
      scenario = scenario_code,
      scenario_label = scenario_labels[scenario_code],
      sector = sector_code,
      sector_label = sector_labels[sector_code],
      source_file = basename(file_path)
    )
}

# ── Load all material-demand data ───────────────────────────

mat_all <- crossing(
  sector = sectors_to_load,
  scenario = scenarios_to_load
) %>%
  mutate(
    data = map2(
      scenario,
      sector,
      ~ read_material_demand(
        scenario_code = .x,
        sector_code = .y,
        output_dir = output_dir
      )
    )
  ) %>%
  select(data) %>%
  unnest(data) %>%
  mutate(
    scenario_label = factor(
      scenario_label,
      levels = c(
        "Reference / Close",
        "Narrow R", "Narrow TP",
        "Slow R", "Slow TP",
        "Close R", "Close TP",
        "Combined R", "Combined TP"
      )
    ),
    sector_label = factor(
      sector_label,
      levels = c("Residential", "Commercial")
    )
  )

# ── Plot data ───────────────────────────────────────────────

plot_data <- mat_all %>%
  filter(scenario %in% scenarios_to_plot) %>%
  mutate(
    scenario_label = factor(
      scenario_label,
      levels = scenario_order
    )
  )

# ── Optional check: Reference vs Close overlap ──────────────

if (all(c("R", "C_r", "C_tp") %in% unique(mat_all$scenario))) {
  
  cat("\nReference vs Close material-demand check by sector:\n")
  
  mat_all %>%
    filter(scenario %in% c("R", "C_r", "C_tp")) %>%
    select(sector_label, scenario, year, material_Gt) %>%
    pivot_wider(names_from = scenario, values_from = material_Gt) %>%
    mutate(
      diff_C_r_vs_R  = C_r - R,
      diff_C_tp_vs_R = C_tp - R
    ) %>%
    group_by(sector_label) %>%
    summarise(
      max_abs_diff_C_r  = max(abs(diff_C_r_vs_R), na.rm = TRUE),
      max_abs_diff_C_tp = max(abs(diff_C_tp_vs_R), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    print(width = Inf)
}

# ── Sanity check table ──────────────────────────────────────

cat("\nSelected-year annual construction material-demand summary:\n")

plot_data %>%
  filter(year %in% c(2025, 2030, 2050, 2100)) %>%
  select(sector_label, scenario_label, year, material_Gt) %>%
  pivot_wider(
    names_from = scenario_label,
    values_from = material_Gt
  ) %>%
  arrange(sector_label, year) %>%
  print(n = Inf, width = Inf)

# ── Check material categories in all loaded files ───────────

cat("\nLoaded scenario/source file check:\n")

mat_all %>%
  distinct(sector_label, scenario, source_file) %>%
  arrange(sector_label, scenario) %>%
  print(n = Inf, width = Inf)

# ── Theme ───────────────────────────────────────────────────

theme_fig <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 20,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 13,
      margin = margin(b = 10)
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    
    strip.text = element_text(
      face = "bold",
      size = 13
    ),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key.width = unit(1.4, "cm"),
    legend.box = "vertical",
    legend.spacing.y = unit(0.15, "cm"),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88"),
    panel.grid.major.y = element_line(colour = "grey85"),
    
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 12, 10, 10)
  )

# ── Plot ────────────────────────────────────────────────────

p_fig5 <- ggplot(
  plot_data,
  aes(
    x = year,
    y = material_Gt,
    colour = scenario_label,
    linetype = scenario_label,
    group = scenario_label
  )
) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.3) +
  facet_wrap(
    ~ sector_label,
    ncol = 1,
    scales = "free_y"
  ) +
  scale_colour_manual(values = scenario_colours) +
  scale_linetype_manual(values = scenario_linetypes) +
  scale_x_continuous(
    breaks = c(2025, 2030, 2040, 2050, 2060, 2080, 2100)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08)),
    labels = label_number(accuracy = 0.01)
  ) +
  labs(
    title = "Annual construction material demand",
    subtitle = "Residential and commercial sectors; Reference and Close are grouped because gross material-demand trajectories overlap",
    x = "Year",
    y = "Material demand (Gt/yr)",
    colour = "Scenario",
    linetype = "Scenario"
  ) +
  guides(
    colour = guide_legend(order = 1, nrow = 2, byrow = TRUE),
    linetype = guide_legend(order = 1, nrow = 2, byrow = TRUE)
  ) +
  theme_fig

print(p_fig5)

# ── Save outputs ────────────────────────────────────────────

file_stub <- "Fig5_v4_annual_residential_commercial_construction_material_demand"

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".png")),
  plot = p_fig5,
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".tiff")),
  plot = p_fig5,
  width = 10,
  height = 8.5,
  units = "in",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".pdf")),
  plot = p_fig5,
  width = 11,
  height = 8.5,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

write_csv(
  plot_data,
  file.path(plot_dir, paste0(file_stub, "_data.csv"))
)

write_csv(
  mat_all,
  file.path(plot_dir, paste0(file_stub, "_all_loaded_scenarios_data.csv"))
)

cat("\nSaved updated Fig. 5 outputs to:\n")
cat(plot_dir, "\n")


#################
# ============================================================
# Extract exact Fig. 5 numbers for Results text
# Annual construction material demand
# Residential + commercial sectors
# ============================================================

cat("\n\n============================================================\n")
cat("Exact Fig. 5 numbers for Results text\n")
cat("Annual construction material demand\n")
cat("============================================================\n")

# Years useful for Results text
text_years <- c(2025, 2030, 2050, 2100)

# Helper formatting
fmt_num <- function(x, digits = 3) {
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  paste0(format(round(x, digits), nsmall = digits, trim = TRUE), "%")
}

# ── 1) Selected annual material-demand values ───────────────

fig5_selected_values <- plot_data %>%
  filter(year %in% text_years) %>%
  select(
    sector_label,
    scenario,
    scenario_label,
    year,
    material_Gt
  ) %>%
  arrange(
    sector_label,
    scenario_label,
    year
  )

cat("\nSelected annual construction material demand:\n")
cat("Units: Gt/yr\n")

fig5_selected_values %>%
  mutate(material_Gt = round(material_Gt, 4)) %>%
  print(n = Inf, width = Inf)

# Wide format for easier checking
fig5_selected_values_wide <- fig5_selected_values %>%
  select(sector_label, scenario_label, year, material_Gt) %>%
  pivot_wider(
    names_from = scenario_label,
    values_from = material_Gt
  ) %>%
  arrange(sector_label, year)

cat("\nSelected annual construction material demand, wide format:\n")

fig5_selected_values_wide %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  print(n = Inf, width = Inf)

# ── 2) Changes relative to Reference / Close ────────────────

fig5_reference <- plot_data %>%
  filter(scenario_label == "Reference / Close") %>%
  select(
    sector_label,
    year,
    ref_material_Gt = material_Gt
  )

fig5_vs_reference <- plot_data %>%
  filter(scenario_label != "Reference / Close") %>%
  left_join(
    fig5_reference,
    by = c("sector_label", "year")
  ) %>%
  mutate(
    abs_reduction_Gt = ref_material_Gt - material_Gt,
    pct_reduction = 100 * abs_reduction_Gt / ref_material_Gt
  ) %>%
  select(
    sector_label,
    scenario,
    scenario_label,
    year,
    ref_material_Gt,
    material_Gt,
    abs_reduction_Gt,
    pct_reduction
  ) %>%
  arrange(sector_label, scenario_label, year)

cat("\nScenario reductions relative to Reference / Close:\n")
cat("Positive values mean lower material demand than Reference / Close.\n")

fig5_vs_reference %>%
  filter(year %in% text_years) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  print(n = Inf, width = Inf)

# ── 3) Text-ready values for 2050 and 2100 ──────────────────

cat("\nText-ready 2050 and 2100 sentences:\n")

fig5_text_sentences <- fig5_vs_reference %>%
  filter(year %in% c(2050, 2100)) %>%
  mutate(
    sentence = paste0(
      sector_label, ", ", scenario_label, ", ", year, ": ",
      "material demand = ", fmt_num(material_Gt, 3), " Gt/yr",
      " (", fmt_pct(pct_reduction, 1), " below Reference / Close; ",
      fmt_num(abs_reduction_Gt, 3), " Gt/yr lower)."
    )
  )

fig5_text_sentences %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ── 4) Reference trajectory change over time ────────────────

fig5_reference_change <- plot_data %>%
  filter(
    scenario_label == "Reference / Close",
    year %in% c(2025, 2050, 2100)
  ) %>%
  select(sector_label, year, material_Gt) %>%
  pivot_wider(
    names_from = year,
    values_from = material_Gt,
    names_prefix = "y"
  ) %>%
  mutate(
    change_2025_2050_Gt = y2050 - y2025,
    change_2050_2100_Gt = y2100 - y2050,
    pct_change_2025_2050 = 100 * (y2050 / y2025 - 1),
    pct_change_2050_2100 = 100 * (y2100 / y2050 - 1)
  )

cat("\nReference / Close trajectory change over time:\n")

fig5_reference_change %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  print(n = Inf, width = Inf)

cat("\nText-ready Reference / Close sentences:\n")

fig5_reference_change %>%
  rowwise() %>%
  mutate(
    sentence = paste0(
      sector_label,
      ": Reference / Close annual material demand changes from ",
      fmt_num(y2025, 3), " Gt/yr in 2025 to ",
      fmt_num(y2050, 3), " Gt/yr in 2050 and ",
      fmt_num(y2100, 3), " Gt/yr in 2100."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ── 5) Average annual reductions by period ──────────────────
# Useful for describing broad patterns without relying only on one year.

fig5_period_summary <- fig5_vs_reference %>%
  mutate(
    period = case_when(
      year >= 2025 & year <= 2050 ~ "2025-2050",
      year > 2050 & year <= 2100  ~ "2050-2100",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(sector_label, scenario_label, period) %>%
  summarise(
    mean_material_Gt = mean(material_Gt, na.rm = TRUE),
    mean_ref_material_Gt = mean(ref_material_Gt, na.rm = TRUE),
    mean_abs_reduction_Gt = mean(abs_reduction_Gt, na.rm = TRUE),
    mean_pct_reduction = mean(pct_reduction, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(sector_label, scenario_label, period)

cat("\nAverage annual material-demand reductions by period:\n")

fig5_period_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  print(n = Inf, width = Inf)

# ── 6) Cumulative material-demand reductions by period ──────
# This sums annual values across reported model years, not time-integrated interpolation.
# Useful for comparing scenario totals across the model time points.

fig5_period_cumulative <- fig5_vs_reference %>%
  mutate(
    period = case_when(
      year >= 2025 & year <= 2050 ~ "2025-2050",
      year > 2050 & year <= 2100  ~ "2050-2100",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(sector_label, scenario_label, period) %>%
  summarise(
    sum_ref_material_Gt = sum(ref_material_Gt, na.rm = TRUE),
    sum_material_Gt = sum(material_Gt, na.rm = TRUE),
    sum_abs_reduction_Gt = sum(abs_reduction_Gt, na.rm = TRUE),
    cumulative_pct_reduction =
      100 * sum_abs_reduction_Gt / sum_ref_material_Gt,
    .groups = "drop"
  ) %>%
  arrange(sector_label, scenario_label, period)

cat("\nCumulative material-demand reductions by period, summed across model years:\n")

fig5_period_cumulative %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  print(n = Inf, width = Inf)

# ── 7) Ranking scenarios by material-demand reduction ───────

fig5_ranked_2050 <- fig5_vs_reference %>%
  filter(year == 2050) %>%
  arrange(sector_label, desc(pct_reduction)) %>%
  group_by(sector_label) %>%
  mutate(rank_2050 = row_number()) %>%
  ungroup()

cat("\nScenario ranking by percentage reduction in 2050:\n")

fig5_ranked_2050 %>%
  select(
    sector_label,
    rank_2050,
    scenario_label,
    material_Gt,
    ref_material_Gt,
    abs_reduction_Gt,
    pct_reduction
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  print(n = Inf, width = Inf)

# ── 8) Reference vs Close overlap check with details ────────

if (all(c("R", "C_r", "C_tp") %in% unique(mat_all$scenario))) {
  
  fig5_close_overlap_detail <- mat_all %>%
    filter(scenario %in% c("R", "C_r", "C_tp")) %>%
    select(sector_label, scenario, year, material_Gt) %>%
    pivot_wider(
      names_from = scenario,
      values_from = material_Gt
    ) %>%
    mutate(
      diff_C_r_vs_R  = C_r - R,
      diff_C_tp_vs_R = C_tp - R
    ) %>%
    arrange(sector_label, year)
  
  cat("\nReference vs Close overlap detail:\n")
  
  fig5_close_overlap_detail %>%
    filter(year %in% text_years) %>%
    mutate(across(where(is.numeric), ~ round(.x, 8))) %>%
    print(n = Inf, width = Inf)
  
  fig5_close_overlap_summary <- fig5_close_overlap_detail %>%
    group_by(sector_label) %>%
    summarise(
      max_abs_diff_C_r_vs_R =
        max(abs(diff_C_r_vs_R), na.rm = TRUE),
      max_abs_diff_C_tp_vs_R =
        max(abs(diff_C_tp_vs_R), na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nReference vs Close overlap summary:\n")
  
  fig5_close_overlap_summary %>%
    mutate(across(where(is.numeric), ~ round(.x, 10))) %>%
    print(n = Inf, width = Inf)
}

# ── 9) Save text-supporting tables ──────────────────────────

write_csv(
  fig5_selected_values,
  file.path(plot_dir, paste0(file_stub, "_selected_values_for_text.csv"))
)

write_csv(
  fig5_selected_values_wide,
  file.path(plot_dir, paste0(file_stub, "_selected_values_wide_for_text.csv"))
)

write_csv(
  fig5_vs_reference,
  file.path(plot_dir, paste0(file_stub, "_vs_reference_for_text.csv"))
)

write_csv(
  fig5_reference_change,
  file.path(plot_dir, paste0(file_stub, "_reference_change_for_text.csv"))
)

write_csv(
  fig5_period_summary,
  file.path(plot_dir, paste0(file_stub, "_period_summary_for_text.csv"))
)

write_csv(
  fig5_period_cumulative,
  file.path(plot_dir, paste0(file_stub, "_period_cumulative_for_text.csv"))
)

write_csv(
  fig5_ranked_2050,
  file.path(plot_dir, paste0(file_stub, "_ranked_2050_for_text.csv"))
)

if (exists("fig5_close_overlap_detail")) {
  write_csv(
    fig5_close_overlap_detail,
    file.path(plot_dir, paste0(file_stub, "_close_overlap_detail_for_text.csv"))
  )
}

cat("\nSaved Fig. 5 text-supporting tables to:\n")
cat(plot_dir, "\n")
