# =========================================================
# Figure 4
# Narrow strategies reduce material demand while shifting
# material composition
#
# Panel A:
#   Material composition of annual residential construction
#   material demand in 2050 across scenarios.
#
# Panel B:
#   Material-specific change in annual material demand in 2050
#   under Narrow strategies relative to Reference.
#
#   Wide filled bars     = Narrow TP
#   Narrow hollow bars   = Narrow R
# =========================================================

# -----------------------------
# 0. Packages
# -----------------------------
library(tidyverse)
library(patchwork)
library(scales)
library(grid)

# -----------------------------
# 1. File paths
# -----------------------------
base_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/output"
out_dir  <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

scenario_files <- c(
  R    = file.path(base_dir, "report_STURM_R_resid_region_bld_material.csv"),
  N_r  = file.path(base_dir, "report_STURM_N_r_resid_region_bld_material.csv"),
  N_tp = file.path(base_dir, "report_STURM_N_tp_resid_region_bld_material.csv"),
  S_r  = file.path(base_dir, "report_STURM_S_r_resid_region_bld_material.csv"),
  S_tp = file.path(base_dir, "report_STURM_S_tp_resid_region_bld_material.csv"),
  C_r  = file.path(base_dir, "report_STURM_C_r_resid_region_bld_material.csv"),
  C_tp = file.path(base_dir, "report_STURM_C_tp_resid_region_bld_material.csv"),
  A_r  = file.path(base_dir, "report_STURM_A_r_resid_region_bld_material.csv"),
  A_tp = file.path(base_dir, "report_STURM_A_tp_resid_region_bld_material.csv")
)

# -----------------------------
# 2. Scenario labels and order
# -----------------------------
scenario_labels <- c(
  R    = "Reference",
  N_r  = "Narrow R",
  N_tp = "Narrow TP",
  S_r  = "Slow R",
  S_tp = "Slow TP",
  C_r  = "Close R",
  C_tp = "Close TP",
  A_r  = "Combined R",
  A_tp = "Combined TP"
)

scenario_order <- c(
  "Reference",
  "Narrow R", "Narrow TP",
  "Slow R", "Slow TP",
  "Close R", "Close TP",
  "Combined R", "Combined TP"
)

# -----------------------------
# 3. Material order and colours
# -----------------------------
material_order <- c(
  "concrete", "brick", "cement", "steel",
  "wood", "glass", "aluminum", "copper"
)

material_colours <- c(
  "concrete"  = "#595959",
  "brick"     = "#F0141A",
  "cement"    = "#9E9E9E",
  "steel"     = "#9C5F00",
  "wood"      = "#D4BC6E",
  "glass"     = "#36A65C",
  "aluminum"  = "#E36C09",
  "copper"    = "#F46D43"
)

# Main policy-relevant year
panel_year <- 2050

# -----------------------------
# 4. Read and combine data
# -----------------------------
read_material_file <- function(path, scen_code) {
  if (!file.exists(path)) {
    stop(paste("Missing file:", path))
  }
  
  read_csv(path, show_col_types = FALSE) %>%
    mutate(
      scenario = scen_code,
      scenario_label = scenario_labels[scen_code]
    )
}

df_all <- imap_dfr(scenario_files, read_material_file)

cat("\nLoaded scenarios:\n")
print(unique(df_all$scenario))

cat("\nAvailable material names:\n")
print(sort(unique(df_all$material)))

cat("\nAvailable years:\n")
print(sort(unique(df_all$year)))

if (!panel_year %in% unique(df_all$year)) {
  stop(paste0(
    "panel_year = ", panel_year,
    " is not available in the data. Available years are: ",
    paste(sort(unique(df_all$year)), collapse = ", ")
  ))
}

# -----------------------------
# 5. Deduplicate and aggregate annual material demand
# -----------------------------
# mat_demand_Mt is material-specific, so material must remain in the key.
# This avoids accidental duplication while summing across all EU-27
# spatial/building dimensions.

required_cols <- c(
  "region_bld", "region_gea", "urt", "clim",
  "inc_cl", "arch", "mat", "eneff",
  "material", "scenario", "scenario_label", "year",
  "mat_demand_Mt"
)

missing_cols <- setdiff(required_cols, names(df_all))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Missing required columns:\n",
      paste(missing_cols, collapse = "\n")
    )
  )
}

df_demand <- df_all %>%
  mutate(
    material = tolower(material),
    scenario_label = factor(scenario_label, levels = scenario_order)
  ) %>%
  filter(material %in% material_order) %>%
  select(all_of(required_cols)) %>%
  distinct() %>%
  group_by(scenario, scenario_label, year, material) %>%
  summarise(
    mat_demand_Mt = sum(mat_demand_Mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mat_demand_Gt = mat_demand_Mt / 1000,
    scenario_label = factor(scenario_label, levels = scenario_order),
    material = factor(material, levels = material_order)
  )

cat(paste0("\nMaterial demand in ", panel_year, " by scenario and material:\n"))
df_demand %>%
  filter(year == panel_year) %>%
  arrange(scenario_label, material) %>%
  print(n = Inf)

# =========================================================
# PANEL A
# Material composition in selected year
# =========================================================

df_panelA <- df_demand %>%
  filter(year == panel_year) %>%
  group_by(scenario_label, material) %>%
  summarise(
    mat_demand_Gt = sum(mat_demand_Gt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    scenario_label = factor(scenario_label, levels = scenario_order),
    material = factor(material, levels = material_order)
  )

pA <- ggplot(
  df_panelA,
  aes(
    x = scenario_label,
    y = mat_demand_Gt,
    fill = material
  )
) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.15) +
  scale_fill_manual(values = material_colours, drop = FALSE) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.001),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = paste0("A. Material composition of residential construction demand in ", panel_year),
    subtitle = "Stacked bars show annual material demand by scenario",
    x = NULL,
    y = "Material demand (Gt/yr)",
    fill = "Material"
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key.width = unit(1.1, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# =========================================================
# PANEL B
# Material-specific change under Narrow strategies
#
# Wide filled bars = Narrow TP
# Narrow hollow bars = Narrow R
# =========================================================

# Reference demand by material in selected year
ref_panel <- df_demand %>%
  filter(scenario == "R", year == panel_year) %>%
  group_by(material) %>%
  summarise(
    ref_Gt = sum(mat_demand_Gt, na.rm = TRUE),
    .groups = "drop"
  )

# Narrow R and Narrow TP relative to Reference
df_narrow_change <- df_demand %>%
  filter(
    scenario %in% c("N_r", "N_tp"),
    year == panel_year
  ) %>%
  group_by(scenario, scenario_label, material) %>%
  summarise(
    demand_Gt = sum(mat_demand_Gt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ref_panel, by = "material") %>%
  mutate(
    pct_change = if_else(
      ref_Gt > 0,
      (demand_Gt - ref_Gt) / ref_Gt * 100,
      NA_real_
    ),
    material = factor(material, levels = material_order),
    scenario_label = factor(
      scenario_label,
      levels = c("Narrow R", "Narrow TP")
    )
  )

df_narrow_r <- df_narrow_change %>%
  filter(scenario == "N_r")

df_narrow_tp <- df_narrow_change %>%
  filter(scenario == "N_tp")

cat(paste0("\nNarrow material-specific changes in ", panel_year, " relative to Reference:\n"))
df_narrow_change %>%
  select(scenario_label, material, demand_Gt, ref_Gt, pct_change) %>%
  arrange(material, scenario_label) %>%
  print(n = Inf)

pB <- ggplot() +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
  
  # Narrow TP: wide filled bars
  geom_col(
    data = df_narrow_tp,
    aes(
      x = material,
      y = pct_change,
      fill = material
    ),
    width = 0.82,
    alpha = 0.95,
    colour = NA
  ) +
  
  # Narrow R: narrow hollow bars
  geom_col(
    data = df_narrow_r,
    aes(
      x = material,
      y = pct_change
    ),
    width = 0.34,
    fill = "white",
    colour = "black",
    linewidth = 0.7
  ) +
  
  scale_fill_manual(values = material_colours, drop = FALSE) +
  scale_x_discrete(
    limits = material_order,
    labels = function(x) str_to_title(x)
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "%", accuracy = 1),
    limits = c(-100, 200),
    breaks = seq(-100, 200, by = 50),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  labs(
    title = paste0("B. Material-specific change under Narrow strategies in ", panel_year),
    subtitle = "Wide filled bars = Narrow TP; narrow hollow bars = Narrow R",
    x = NULL,
    y = "Change relative to Reference (%)"
  ) +
  guides(
    fill = "none"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# =========================================================
# 6. Combine figure
# =========================================================

fig4 <- pA / pB +
  plot_layout(heights = c(1.15, 1)) &
  theme(
    plot.margin = margin(8, 12, 8, 12)
  )

print(fig4)

# =========================================================
# 7. Save outputs
# =========================================================

ggsave(
  filename = file.path(out_dir, paste0("fig4_material_composition_and_narrow_change_", panel_year, ".png")),
  plot = fig4,
  width = 11,
  height = 11,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(out_dir, paste0("fig4_material_composition_and_narrow_change_", panel_year, ".tiff")),
  plot = fig4,
  width = 11,
  height = 11,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(out_dir, paste0("fig4_material_composition_and_narrow_change_", panel_year, ".pdf")),
  plot = fig4,
  width = 11,
  height = 11,
  device = cairo_pdf,
  bg = "white"
)

# =========================================================
# 8. Save data
# =========================================================

write_csv(
  df_panelA,
  file.path(out_dir, paste0("fig4_data_panelA_material_composition_", panel_year, ".csv"))
)

write_csv(
  df_narrow_change,
  file.path(out_dir, paste0("fig4_data_panelB_narrow_change_", panel_year, ".csv"))
)

# =========================================================
# 9. Print summary tables
# =========================================================

cat("\nSaved Figure 4 and data to:\n", out_dir, "\n")

cat(paste0("\n=== ", panel_year, " annual material demand by scenario (Gt/yr) ===\n"))
df_panelA %>%
  pivot_wider(names_from = material, values_from = mat_demand_Gt) %>%
  arrange(scenario_label) %>%
  print(n = Inf, width = Inf)

cat(paste0("\n=== ", panel_year, " Narrow change relative to Reference (%) ===\n"))
df_narrow_change %>%
  select(scenario_label, material, demand_Gt, ref_Gt, pct_change) %>%
  arrange(scenario_label, material) %>%
  print(n = Inf, width = Inf)