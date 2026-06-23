# ============================================================
# Figure 3. Close strategies alone are insufficient to drive
# substantial material-demand reductions
#
# EU-27 residential sector
#
# Panel A: Annual residential construction material demand
#          Reference, Close R, and Close TP are grouped as
#          "Reference / Close" because their total material-demand
#          trajectories overlap.
#
# Panel B: Cumulative material-demand reduction, 2025–2100
#
# Interpretation note:
#   The model starts in 2020, but material demand is first reported
#   in 2025. Therefore, 2025 is already the first reported scenario
#   timestep. Slow scenarios can show lower material demand in 2025
#   because vacant-dwelling reoccupation and lifetime extension are
#   already active during the 2020–2025 timestep.
#
# Uses:
#   report_STURM_<scenario>_resid_region_bld_material.csv
#
# Scenarios:
#   R, N_r, N_tp, S_r, S_tp, C_r, C_tp, A_r, A_tp
#
# Notes:
#   mat_demand_Mt is material-specific and should be summed across
#   material categories. Unlike floor-space variables, do NOT remove
#   the material column when aggregating material demand.
# ============================================================

# ----------------------------
# 0. Packages
# ----------------------------
library(tidyverse)
library(readr)
library(patchwork)
library(scales)
library(grid)

# ----------------------------
# 1. File paths
# ----------------------------
input_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/output"
output_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# 2. Scenario settings
# ----------------------------
scenarios <- c(
  "R",
  "N_r", "N_tp",
  "S_r", "S_tp",
  "C_r", "C_tp",
  "A_r", "A_tp"
)

scenario_labels <- c(
  "R"    = "Reference",
  "N_r"  = "Narrow R",
  "N_tp" = "Narrow TP",
  "S_r"  = "Slow R",
  "S_tp" = "Slow TP",
  "C_r"  = "Close R",
  "C_tp" = "Close TP",
  "A_r"  = "Combined R",
  "A_tp" = "Combined TP"
)

scenario_short <- c(
  "Reference"   = "Reference",
  "Narrow R"    = "Narrow R",
  "Narrow TP"   = "Narrow TP",
  "Slow R"      = "Slow R",
  "Slow TP"     = "Slow TP",
  "Close R"     = "Close R",
  "Close TP"    = "Close TP",
  "Combined R"  = "Combined R",
  "Combined TP" = "Combined TP"
)

scenario_order <- unname(scenario_labels[scenarios])

# ----------------------------
# 3. Panel A settings
# ----------------------------
# Reference and Close scenarios are grouped only in Panel A.
panelA_order <- c(
  "Reference / Close",
  "Narrow R",
  "Narrow TP",
  "Slow R",
  "Slow TP",
  "Combined R",
  "Combined TP"
)

panelA_colours <- c(
  "Reference / Close" = "#4D4D4D",
  "Narrow R"          = "#D95F02",
  "Narrow TP"         = "#D95F02",
  "Slow R"            = "#0072B2",
  "Slow TP"           = "#0072B2",
  "Combined R"        = "#CC79A7",
  "Combined TP"       = "#CC79A7"
)

panelA_linetypes <- c(
  "Reference / Close" = "solid",
  "Narrow R"          = "solid",
  "Narrow TP"         = "dashed",
  "Slow R"            = "solid",
  "Slow TP"           = "dashed",
  "Combined R"        = "solid",
  "Combined TP"       = "dashed"
)

# ----------------------------
# 4. Panel B settings
# ----------------------------
strategy_colours <- c(
  "Reference"   = "#4D4D4D",
  "Narrow R"    = "#D95F02",
  "Narrow TP"   = "#D95F02",
  "Slow R"      = "#0072B2",
  "Slow TP"     = "#0072B2",
  "Close R"     = "#009E73",
  "Close TP"    = "#009E73",
  "Combined R"  = "#CC79A7",
  "Combined TP" = "#CC79A7"
)

baseline_year <- 2020
first_material_year <- 2025
end_year <- 2100

# ----------------------------
# 5. Read material reports
# ----------------------------
read_material_report <- function(sc) {
  f <- file.path(
    input_dir,
    paste0("report_STURM_", sc, "_resid_region_bld_material.csv")
  )
  
  if (!file.exists(f)) {
    cat("NOT FOUND:", f, "\n")
    return(NULL)
  }
  
  read_csv(f, show_col_types = FALSE) %>%
    mutate(
      scenario = sc,
      scenario_label = scenario_labels[sc]
    )
}

df_raw <- map_dfr(scenarios, read_material_report)

if (nrow(df_raw) == 0) {
  stop("No material files loaded. Check input_dir and scenario names.")
}

df_raw <- df_raw %>%
  mutate(
    scenario = factor(scenario, levels = scenarios),
    scenario_label = factor(scenario_label, levels = scenario_order)
  )

cat("\nLoaded rows by scenario:\n")
print(df_raw %>% count(scenario_label), n = Inf)

cat("\nYears available:\n")
print(sort(unique(df_raw$year)))

cat("\nMaterials available:\n")
print(sort(unique(df_raw$material)))

# ----------------------------
# 6. Deduplicate material-demand records
# ----------------------------
# mat_demand_Mt is material-specific, so keep 'material' in the key.
# This is different from floor-space variables, which are repeated by material.

mat_key_cols <- c(
  "region_bld", "region_gea", "urt", "clim",
  "inc_cl", "arch", "mat", "eneff",
  "material", "scenario", "scenario_label", "year",
  "mat_demand_Mt", "mat_primary_Mt",
  "mat_stock_Mt", "mat_scrap_Mt",
  "mat_reuse_Mt", "mat_recycling_Mt",
  "mat_downcycling_Mt", "mat_other_treat_Mt"
)

mat_key_cols <- intersect(mat_key_cols, names(df_raw))

df_mat <- df_raw %>%
  select(all_of(mat_key_cols)) %>%
  distinct()

cat("\nRows before deduplication:", nrow(df_raw), "\n")
cat("Rows after deduplication: ", nrow(df_mat), "\n")

# ----------------------------
# 7. Aggregate total material demand
# ----------------------------
df_total <- df_mat %>%
  group_by(year, scenario, scenario_label) %>%
  summarise(
    mat_demand_Mt = sum(mat_demand_Mt, na.rm = TRUE),
    mat_primary_Mt = if ("mat_primary_Mt" %in% names(df_mat)) {
      sum(mat_primary_Mt, na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(
    mat_demand_Gt = mat_demand_Mt / 1000,
    mat_primary_Gt = mat_primary_Mt / 1000
  )

cat("\nMaterial demand in selected years:\n")
df_total %>%
  filter(year %in% c(2025, 2050, 2100)) %>%
  select(year, scenario_label, mat_demand_Gt, mat_primary_Gt) %>%
  arrange(year, scenario_label) %>%
  print(n = Inf)

# ----------------------------
# 8. Panel A data: group Reference + Close scenarios
# ----------------------------
df_total_panelA <- df_total %>%
  mutate(
    scenario_label_panelA = case_when(
      scenario %in% c("R", "C_r", "C_tp") ~ "Reference / Close",
      scenario == "N_r"  ~ "Narrow R",
      scenario == "N_tp" ~ "Narrow TP",
      scenario == "S_r"  ~ "Slow R",
      scenario == "S_tp" ~ "Slow TP",
      scenario == "A_r"  ~ "Combined R",
      scenario == "A_tp" ~ "Combined TP",
      TRUE ~ as.character(scenario_label)
    ),
    scenario_label_panelA = factor(
      scenario_label_panelA,
      levels = panelA_order
    )
  ) %>%
  group_by(year, scenario_label_panelA) %>%
  summarise(
    # Mean is used only for the grouped Reference/Close line.
    # Since these trajectories overlap, this produces a single clean line.
    # For other scenarios, this equals the original scenario value.
    mat_demand_Gt = mean(mat_demand_Gt, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nPanel A grouped trajectories:\n")
df_total_panelA %>%
  filter(year %in% c(2025, 2050, 2100)) %>%
  arrange(year, scenario_label_panelA) %>%
  print(n = Inf)

cat("\nDiagnostic: Reference and Close overlap check:\n")
df_total %>%
  filter(scenario %in% c("R", "C_r", "C_tp")) %>%
  group_by(year) %>%
  summarise(
    min_demand_Gt = min(mat_demand_Gt, na.rm = TRUE),
    max_demand_Gt = max(mat_demand_Gt, na.rm = TRUE),
    range_demand_Gt = max_demand_Gt - min_demand_Gt,
    .groups = "drop"
  ) %>%
  print(n = Inf)

# ----------------------------
# 9. Cumulative material-demand reduction
# ----------------------------
# mat_demand_Mt is annual material demand.
#
# Because the model starts in 2020 but material demand is first reported
# in 2025, the 2025 value is interpreted as the first reported scenario
# material-demand timestep. Each model-year value is multiplied by the
# following timestep length to approximate cumulative demand.

df_total_cumulative <- df_total %>%
  arrange(scenario_label, year) %>%
  group_by(scenario_label) %>%
  mutate(
    timestep_years = lead(year) - year,
    timestep_years = if_else(
      is.na(timestep_years),
      lag(timestep_years),
      timestep_years
    ),
    period_mat_demand_Gt = mat_demand_Gt * timestep_years,
    cumulative_mat_demand_Gt = cumsum(period_mat_demand_Gt)
  ) %>%
  ungroup()

ref_cumulative <- df_total_cumulative %>%
  filter(scenario_label == "Reference", year == end_year) %>%
  pull(cumulative_mat_demand_Gt)

if (length(ref_cumulative) == 0 || is.na(ref_cumulative)) {
  stop("Could not calculate cumulative Reference demand. Check Reference scenario and end_year.")
}

df_cumulative_reduction <- df_total_cumulative %>%
  filter(year == end_year, scenario_label != "Reference") %>%
  mutate(
    cumulative_reduction_Gt = ref_cumulative - cumulative_mat_demand_Gt,
    cumulative_reduction_pct = cumulative_reduction_Gt / ref_cumulative * 100,
    scenario_label = factor(scenario_label, levels = scenario_order)
  )

cat("\nCumulative material-demand reduction, 2025–2100, relative to Reference:\n")
df_cumulative_reduction %>%
  select(
    scenario_label,
    cumulative_mat_demand_Gt,
    cumulative_reduction_Gt,
    cumulative_reduction_pct
  ) %>%
  arrange(scenario_label) %>%
  print(n = Inf)

# ----------------------------
# 10. Theme
# ----------------------------
theme_fig <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 10.5),
    legend.text = element_text(size = 9.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85"),
    panel.grid.major.y = element_line(colour = "grey85"),
    plot.margin = margin(5.5, 10, 5.5, 5.5)
  )

# ============================================================
# Panel A — Annual material-demand trajectories
# ============================================================

pA <- ggplot(
  df_total_panelA,
  aes(
    x = year,
    y = mat_demand_Gt,
    colour = scenario_label_panelA,
    linetype = scenario_label_panelA,
    group = scenario_label_panelA
  )
) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = panelA_colours, name = "Scenario") +
  scale_linetype_manual(values = panelA_linetypes, name = "Scenario") +
  scale_x_continuous(
    breaks = c(2025, 2030, 2040, 2050, 2060, 2080, 2100)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "A. Annual residential construction material demand",
    subtitle = "Reference and Close scenarios are grouped because their total material-demand trajectories overlap",
    x = NULL,
    y = "Material demand (Gt/yr)"
  ) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_fig +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.4, "cm")
  )

# ============================================================
# Panel B — Cumulative reduction relative to Reference
# ============================================================

pB <- ggplot(
  df_cumulative_reduction,
  aes(
    x = scenario_label,
    y = cumulative_reduction_pct,
    fill = scenario_label
  )
) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.25) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  geom_text(
    aes(label = paste0(round(cumulative_reduction_pct), "%")),
    vjust = -0.35,
    size = 3.4
  ) +
  scale_fill_manual(values = strategy_colours, guide = "none") +
  scale_x_discrete(labels = scenario_short) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(
      0,
      max(df_cumulative_reduction$cumulative_reduction_pct, na.rm = TRUE) * 1.12
    ),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "B. Cumulative material-demand reduction, 2025–2100",
    subtitle = "Values show avoided cumulative demand relative to Reference over the full scenario period",
    x = NULL,
    y = "Cumulative reduction (%)"
  ) +
  theme_fig +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

# ============================================================
# 11. Combine Figure 3
# ============================================================

fig3 <- pA / pB +
  plot_layout(
    heights = c(1.1, 0.9)
  )

print(fig3)

# ----------------------------
# 12. Save outputs: square image
# ----------------------------
fig3_png <- file.path(output_dir, "fig3_close_insufficient_material_demand_square.png")
fig3_tiff <- file.path(output_dir, "fig3_close_insufficient_material_demand_square.tiff")
fig3_pdf <- file.path(output_dir, "fig3_close_insufficient_material_demand_square.pdf")

ggsave(
  filename = fig3_png,
  plot = fig3,
  width = 9,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = fig3_tiff,
  plot = fig3,
  width = 9,
  height = 9,
  units = "in",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = fig3_pdf,
  plot = fig3,
  width = 9,
  height = 9,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

cat("\nSaved Figure 3:\n")
cat(fig3_png, "\n")
cat(fig3_tiff, "\n")
cat(fig3_pdf, "\n")

# ----------------------------
# 13. Save data used for figure
# ----------------------------
write_csv(
  df_total,
  file.path(output_dir, "fig3_data_total_material_demand_all_scenarios.csv")
)

write_csv(
  df_total_panelA,
  file.path(output_dir, "fig3_data_panelA_grouped_reference_close.csv")
)

write_csv(
  df_cumulative_reduction,
  file.path(output_dir, "fig3_data_panelB_cumulative_reduction.csv")
)

cat("\nDone. Data saved to:\n", output_dir, "\n")