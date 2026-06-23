# ============================================================
# Figure 2. Slow strategies reduce demolition and new construction
# EU-27 residential sector
#
# Panel A: Annual residential floor-space additions and removals
# Panel B: Residential floor-space composition by stock category over time
#
# Uses:
#   report_STURM_R_resid_region_bld_material.csv
#   report_STURM_S_r_resid_region_bld_material.csv
#   report_STURM_S_tp_resid_region_bld_material.csv
#
# Important:
#   floor_tot_Mm2, floor_new_Mm2, and floor_dem_Mm2 are repeated
#   for each material in the material report. Therefore, we deduplicate
#   floor-space records before aggregation.
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

files <- c(
  R    = file.path(input_dir, "report_STURM_R_resid_region_bld_material.csv"),
  S_r  = file.path(input_dir, "report_STURM_S_r_resid_region_bld_material.csv"),
  S_tp = file.path(input_dir, "report_STURM_S_tp_resid_region_bld_material.csv")
)

missing_files <- files[!file.exists(files)]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "The following files were not found:\n",
      paste(missing_files, collapse = "\n")
    )
  )
}

# ----------------------------
# 2. Scenario labels
# ----------------------------
scenario_levels <- c("R", "S_r", "S_tp")

scenario_labels <- c(
  "R"    = "Reference",
  "S_r"  = "Slow realistic",
  "S_tp" = "Slow technical potential"
)

scenario_colours <- c(
  "Reference" = "#4D4D4D",
  "Slow realistic" = "#D95F02",
  "Slow technical potential" = "#0072B2"
)

# ----------------------------
# 3. Read data
# ----------------------------
df_raw <- purrr::imap_dfr(
  files,
  ~ read_csv(.x, show_col_types = FALSE) %>%
    mutate(scenario = .y)
) %>%
  mutate(
    scenario = factor(scenario, levels = scenario_levels),
    scenario_label = factor(
      scenario_labels[as.character(scenario)],
      levels = c("Reference", "Slow realistic", "Slow technical potential")
    )
  )

cat("\nLoaded rows by scenario:\n")
print(df_raw %>% count(scenario_label))

cat("\nUnique materials:\n")
print(sort(unique(df_raw$material)))

# ----------------------------
# 4. Deduplicate floor-space records
# ----------------------------
# The material report repeats floor-space values once for each material.
# We remove material-specific rows and keep one floor-space record per
# building-stock segment.

segment_cols <- c(
  "region_bld", "region_gea", "urt", "clim",
  "inc_cl", "arch", "mat", "eneff",
  "scenario", "scenario_label", "year"
)

df_floor_segments <- df_raw %>%
  select(
    all_of(segment_cols),
    floor_tot_Mm2,
    floor_new_Mm2,
    floor_dem_Mm2
  ) %>%
  distinct()

cat("\nRows before deduplication:", nrow(df_raw), "\n")
cat("Rows after deduplication: ", nrow(df_floor_segments), "\n")
cat("Raw / deduplicated ratio:", nrow(df_raw) / nrow(df_floor_segments), "\n")

# ----------------------------
# 5. Diagnostic: check repeated floor-space values
# ----------------------------
repeat_check <- df_raw %>%
  group_by(
    region_bld, region_gea, urt, clim,
    inc_cl, arch, mat, eneff,
    scenario, scenario_label, year
  ) %>%
  summarise(
    n_material_rows = n(),
    n_materials = n_distinct(material),
    n_floor_tot_values = n_distinct(floor_tot_Mm2),
    n_floor_new_values = n_distinct(floor_new_Mm2),
    n_floor_dem_values = n_distinct(floor_dem_Mm2),
    .groups = "drop"
  )

cat("\nDiagnostic: material repetition check\n")
print(
  repeat_check %>%
    summarise(
      min_material_rows = min(n_material_rows),
      max_material_rows = max(n_material_rows),
      min_materials = min(n_materials),
      max_materials = max(n_materials),
      max_distinct_floor_tot = max(n_floor_tot_values),
      max_distinct_floor_new = max(n_floor_new_values),
      max_distinct_floor_dem = max(n_floor_dem_values)
    )
)

# ============================================================
# PANEL A: Annual additions and removals
# ============================================================

df_flows <- df_floor_segments %>%
  group_by(scenario_label, year) %>%
  summarise(
    floor_new_Mm2 = sum(floor_new_Mm2, na.rm = TRUE),
    floor_dem_Mm2 = sum(floor_dem_Mm2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    floor_new_bm2 = floor_new_Mm2 / 1000,
    floor_dem_bm2 = floor_dem_Mm2 / 1000
  ) %>%
  select(scenario_label, year, floor_new_bm2, floor_dem_bm2) %>%
  pivot_longer(
    cols = c(floor_new_bm2, floor_dem_bm2),
    names_to = "flow",
    values_to = "value_bm2"
  ) %>%
  mutate(
    flow = recode(
      flow,
      "floor_new_bm2" = "New construction",
      "floor_dem_bm2" = "Demolition"
    ),
    plot_value = if_else(flow == "Demolition", -value_bm2, value_bm2),
    flow = factor(flow, levels = c("New construction", "Demolition"))
  )

# ============================================================
# PANEL B: Stock category shares over time
# ============================================================

df_stock_comp <- df_floor_segments %>%
  mutate(
    stock_group = case_when(
      eneff %in% c("s1", "s2", "s3") ~ "Existing floor space",
      str_detect(eneff, "^sr") ~ "Renovated floor space",
      eneff %in% c("s51_std", "s52_low") ~ "New floor space",
      TRUE ~ "Other"
    )
  ) %>%
  filter(stock_group != "Other") %>%
  group_by(year, scenario_label, stock_group) %>%
  summarise(
    floor_tot_Mm2 = sum(floor_tot_Mm2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year, scenario_label) %>%
  mutate(
    total_floor_Mm2 = sum(floor_tot_Mm2, na.rm = TRUE),
    share = floor_tot_Mm2 / total_floor_Mm2 * 100,
    floor_tot_bm2 = floor_tot_Mm2 / 1000
  ) %>%
  ungroup() %>%
  mutate(
    # For geom_area stacking:
    # Last level appears at the bottom, so this puts:
    # Existing = bottom, Renovated = middle, New = top.
    stock_group = factor(
      stock_group,
      levels = c(
        "New floor space",
        "Renovated floor space",
        "Existing floor space"
      )
    )
  )

# Check stock shares sum to 100%
cat("\nPanel B share check. Rows shown only if shares do not sum to 100:\n")
print(
  df_stock_comp %>%
    group_by(year, scenario_label) %>%
    summarise(
      sum_share = sum(share),
      n_groups = n_distinct(stock_group),
      .groups = "drop"
    ) %>%
    filter(abs(sum_share - 100) > 1e-6),
  n = Inf
)

# ----------------------------
# 6. Plot settings
# ----------------------------
theme_base_fig <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10.5, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85"),
    panel.grid.major.y = element_line(colour = "grey85"),
    plot.margin = margin(5.5, 10, 5.5, 5.5)
  )

stock_group_colours <- c(
  "Existing floor space" = "#B3A700",
  "Renovated floor space" = "#1FB5A9",
  "New floor space" = "#F1736A"
)

# ============================================================
# 7. Panel A plot
# ============================================================

pA <- ggplot(
  df_flows,
  aes(
    x = year,
    y = plot_value,
    colour = scenario_label,
    linetype = flow,
    group = interaction(scenario_label, flow)
  )
) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = scenario_colours) +
  scale_linetype_manual(
    values = c(
      "New construction" = "solid",
      "Demolition" = "dotted"
    )
  ) +
  scale_x_continuous(
    breaks = c(2025, 2030, 2040, 2050, 2060, 2080, 2100)
  ) +
  scale_y_continuous(
    breaks = c(-0.10, -0.05, 0, 0.05, 0.10),
    labels = function(x) {
      ifelse(
        x == 0,
        "0",
        number(abs(x), accuracy = 0.01, trim = TRUE)
      )
    },
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  labs(
    title = "A. Annual residential floor-space additions and removals",
    subtitle = "New construction is shown above zero; demolition is shown below zero",
    x = NULL,
    y = expression(paste("Annual floor space (billion ", m^2, "/yr)")),
    colour = "Scenario",
    linetype = "Flow"
  ) +
  guides(
    colour = guide_legend(order = 1, nrow = 1, byrow = TRUE),
    linetype = guide_legend(order = 2, nrow = 1, byrow = TRUE)
  ) +
  theme_base_fig +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing.y = unit(0.15, "cm")
  )

# ============================================================
# 8. Panel B plot
# ============================================================

pB <- ggplot(
  df_stock_comp,
  aes(
    x = year,
    y = share,
    fill = stock_group
  )
) +
  geom_area(alpha = 0.95, colour = "white", linewidth = 0.15) +
  geom_vline(
    xintercept = 2050,
    linetype = "dashed",
    colour = "grey35",
    linewidth = 0.45
  ) +
  facet_wrap(~ scenario_label, nrow = 1) +
  scale_fill_manual(
    values = stock_group_colours,
    breaks = c(
      "Existing floor space",
      "Renovated floor space",
      "New floor space"
    )
  ) +
  scale_x_continuous(
    breaks = c(2025, 2050, 2100)
  ) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 75, 100),
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(ylim = c(0, 100), clip = "off") +
  labs(
    title = "B. Residential floor-space composition by stock category",
    x = NULL,
    y = "Share of residential floor space",
    fill = "Stock category"
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme_base_fig +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.3, "cm"),
    panel.spacing.x = unit(1.0, "lines")
  )

# ============================================================
# 9. Combine Figure 2
# ============================================================

fig2 <- pA / pB +
  plot_layout(
    heights = c(1.05, 0.95)
  )

print(fig2)

# ============================================================
# 10. Save outputs
# ============================================================

fig2_png <- file.path(output_dir, "fig2_slow_annual_turnover_stock_category_area.png")
fig2_tiff <- file.path(output_dir, "fig2_slow_annual_turnover_stock_category_area.tiff")
fig2_pdf <- file.path(output_dir, "fig2_slow_annual_turnover_stock_category_area.pdf")

ggsave(
  filename = fig2_png,
  plot = fig2,
  width = 9.5,
  height = 9.0,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = fig2_tiff,
  plot = fig2,
  width = 9.5,
  height = 9.0,
  units = "in",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = fig2_pdf,
  plot = fig2,
  width = 9.5,
  height = 9.0,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

cat("\nSaved:\n", fig2_png, "\n", fig2_tiff, "\n", fig2_pdf, "\n")

# ============================================================
# 11. Save data used for the figure
# ============================================================

write_csv(
  df_flows,
  file.path(output_dir, "fig2_data_panelA_annual_turnover.csv")
)

write_csv(
  df_stock_comp,
  file.path(output_dir, "fig2_data_panelB_stock_category_composition.csv")
)

# ============================================================
# 12. Optional printed summaries
# ============================================================

cat("\n=== Panel A summary: annual additions and removals ===\n")
df_flows %>%
  mutate(value_bm2 = abs(plot_value)) %>%
  select(scenario_label, year, flow, value_bm2) %>%
  pivot_wider(names_from = flow, values_from = value_bm2) %>%
  print(n = Inf)

cat("\n=== Panel B summary: stock category composition ===\n")
df_stock_comp %>%
  mutate(
    stock_group = factor(
      stock_group,
      levels = c(
        "Existing floor space",
        "Renovated floor space",
        "New floor space"
      )
    )
  ) %>%
  select(year, scenario_label, stock_group, share) %>%
  arrange(year, scenario_label, stock_group) %>%
  print(n = Inf)