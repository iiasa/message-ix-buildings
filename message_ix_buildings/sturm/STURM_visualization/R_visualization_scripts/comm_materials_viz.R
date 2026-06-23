# ============================================================
# STURM Results — Commercial Material Demand Panels
#
# Date: 2026-05-26
#
# Panel A: Global total material demand over time
#          (unique trajectories only)
# Panel B: Regional material demand % breakdown in 2100 + World
# Panel C: Narrow vs Reference focus (R, N_r, N_tp only)
#
# Uses:
#   *_comm_region_bld_material.csv
# ============================================================

library(tidyverse)
library(readr)
library(patchwork)

# ── Step 0: User settings ───────────────────────────────────

output_dir <- "C:/Users/nawawi/IIASA/ECE.prog - sturm/Modelling/sturm_runs_GLO/STURM_output"

plot_dir <- "C:/Users/nawawi/IIASA/ECE.prog - sturm/Modelling/sturm_runs_GLO/STURM_visualization"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

scenarios <- c("R", "N_r", "N_tp", "S_r", "S_tp", "C_r", "C_tp",
               "A_r", "A_tp", "E_r", "E_tp", "LED")

material_var  <- "mat_demand_Mt"
baseline_year <- 2025
panel_b_year  <- 2100

make_material_file_path <- function(scenario) {
  file.path(output_dir,
            paste0("report_STURM_", scenario, "_comm_region_bld_material.csv"))
}

# ── Step 1: Load all material files ─────────────────────────

df_mat_all <- map_dfr(scenarios, function(sc) {
  f <- make_material_file_path(sc)
  if (!file.exists(f)) { cat("  NOT FOUND:", f, "\n"); return(NULL) }
  read_csv(f, show_col_types = FALSE) %>% mutate(scenario = sc)
}) %>%
  mutate(scenario = factor(scenario, levels = scenarios))

# ── Step 2: Basic checks ─────────────────────────────────────

cat("=== Basic checks ===\n")
cat("Total rows:", nrow(df_mat_all), "\n")
cat("\nScenarios loaded:\n");  print(table(df_mat_all$scenario))
cat("\nYears:\n");             print(sort(unique(df_mat_all$year)))
cat("\nMaterials:\n");         print(sort(unique(df_mat_all$material)))
cat("\nRegions (region_gea):\n"); print(sort(unique(df_mat_all$region_gea)))

# ── Step 3: Deduplicate ───────────────────────────────────────

mat_key_cols <- c("year", "scenario", "region_bld", "region_gea",
                  "urt", "clim", "inc_cl", "arch", "mat", "eneff",
                  "material", material_var)

df_mat_dedup <- df_mat_all %>%
  select(all_of(mat_key_cols)) %>%
  distinct() %>%
  rename(mat_demand_Mt = all_of(material_var))

cat("\nRows before dedup:", nrow(df_mat_all), "\n")
cat("Rows after dedup:", nrow(df_mat_dedup), "\n")

# ── Step 4: Aggregate ─────────────────────────────────────────

df_global_mat_demand <- df_mat_dedup %>%
  group_by(year, scenario) %>%
  summarise(mat_demand_Mt = sum(mat_demand_Mt, na.rm = TRUE),
            mat_demand_Gt = mat_demand_Mt / 1000,
            .groups = "drop")

df_global_mat_by_material <- df_mat_dedup %>%
  group_by(year, scenario, material) %>%
  summarise(mat_demand_Mt = sum(mat_demand_Mt, na.rm = TRUE),
            mat_demand_Gt = mat_demand_Mt / 1000,
            .groups = "drop")

df_region_mat_by_material <- df_mat_dedup %>%
  group_by(year, scenario, region_gea, material) %>%
  summarise(mat_demand_Mt = sum(mat_demand_Mt, na.rm = TRUE),
            mat_demand_Gt = mat_demand_Mt / 1000,
            .groups = "drop")

df_region_mat_total <- df_region_mat_by_material %>%
  group_by(year, scenario, region_gea) %>%
  summarise(total_mat_demand_Gt = sum(mat_demand_Gt, na.rm = TRUE),
            .groups = "drop")

cat("\n=== Global material demand check ===\n")
df_global_mat_demand %>%
  filter(year %in% c(2025, 2050, 2100)) %>%
  arrange(year, scenario) %>%
  print(n = 40)

# ── Step 5: Add World ────────────────────────────────────────

region_order_plus_world <- c(sort(unique(df_region_mat_by_material$region_gea)), "World")

df_region_mat_by_material_plus_world <- bind_rows(
  df_region_mat_by_material %>% mutate(region_plot = region_gea),
  df_global_mat_by_material %>% mutate(region_plot = "World")
) %>%
  select(year, scenario, region_plot, material, mat_demand_Mt, mat_demand_Gt) %>%
  mutate(region_plot = factor(region_plot, levels = region_order_plus_world))

# ── Step 6: 2025 markers ─────────────────────────────────────

df_panel_b_markers <- bind_rows(
  df_region_mat_total %>%
    filter(year == baseline_year) %>%
    mutate(region_plot = region_gea) %>%
    select(region_plot, scenario, baseline_2025_Gt = total_mat_demand_Gt),
  df_global_mat_demand %>%
    filter(year == baseline_year) %>%
    mutate(region_plot = "World", baseline_2025_Gt = mat_demand_Gt) %>%
    select(region_plot, scenario, baseline_2025_Gt)
) %>%
  mutate(region_plot = factor(region_plot, levels = region_order_plus_world))

# ── Step 7: Material order ───────────────────────────────────

material_order_2100 <- df_global_mat_by_material %>%
  filter(year == panel_b_year) %>%
  group_by(material) %>%
  summarise(total = sum(mat_demand_Gt, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(material)

cat("\n=== Material order (largest to smallest) ===\n")
print(material_order_2100)

# ── Step 8: Detect unique trajectories ───────────────────────

df_all_wide <- df_global_mat_demand %>%
  select(year, scenario, mat_demand_Gt) %>%
  pivot_wider(names_from = scenario, values_from = mat_demand_Gt)

threshold <- 0.001
assigned  <- c()
groups    <- list()

for (sc in scenarios) {
  if (sc %in% assigned) next
  grp <- sc
  for (sc2 in setdiff(scenarios, sc)) {
    if (sc2 %in% assigned) next
    if (all(c(sc, sc2) %in% names(df_all_wide))) {
      if (max(abs(df_all_wide[[sc]] - df_all_wide[[sc2]]), na.rm = TRUE) < threshold) {
        grp <- c(grp, sc2)
      }
    }
  }
  groups[[length(groups) + 1]] <- grp
  assigned <- c(assigned, grp)
}

n_groups <- length(groups)
cat("\n=== UNIQUE TRAJECTORY GROUPS ===\n")
for (i in seq_along(groups))
  cat(sprintf("  Group %d: %s\n", i, paste(sort(groups[[i]]), collapse = " = ")))
cat("Total unique trajectories:", n_groups, "\n")

# ── Step 9: Styling ──────────────────────────────────────────

traj_colors_all    <- c("#2166ac", "#66a61e", "#d95f02", "#984ea3",
                        "#e7298a", "#1b9e77", "#a65628", "#999999",
                        "#762a83", "#01665e")
traj_linetypes_all <- c("solid", "dashed", "dotted", "dotdash",
                        "longdash", "twodash", "dashed", "dotted",
                        "solid", "dashed")

# Commercial does not have brick — remove from material colors if absent
material_colors <- c(
  "aluminum" = "#D95F02",
  "brick"    = "#FF1F1F",
  "cement"   = "#969696",
  "concrete" = "#4D4D4D",
  "copper"   = "#F46D43",
  "glass"    = "#2CA25F",
  "steel"    = "#8C510A",
  "wood"     = "#D8BE74"
)

scenario_labels_short <- setNames(as.character(scenarios), scenarios)

theme_sturm <- theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(size = 12),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 10),
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank()
  )

# ── Step 10: Panel A — Global total material demand ──────────

df_plot_unique <- map_dfr(seq_along(groups), function(i) {
  grp <- groups[[i]]
  df_global_mat_demand %>%
    filter(as.character(scenario) == grp[1]) %>%
    mutate(traj_label = paste(sort(grp), collapse = " = "))
})

grp_labels <- unique(df_plot_unique$traj_label)

pA_global_mat <- df_plot_unique %>%
  ggplot(aes(x = year, y = mat_demand_Gt,
             color = traj_label, linetype = traj_label)) +
  geom_vline(xintercept = 2025, linetype = "dotted",
             color = "grey45", linewidth = 0.5) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_color_manual(values    = setNames(traj_colors_all[1:n_groups], grp_labels)) +
  scale_linetype_manual(values = setNames(traj_linetypes_all[1:n_groups], grp_labels)) +
  scale_x_continuous(limits = c(2020, 2100),
                     breaks = c(2020, 2030, 2040, 2050, 2060, 2080, 2100)) +
  labs(
    title    = sprintf("Global commercial material demand — %d unique trajectories", n_groups),
    subtitle = "Annual commercial building material demand | scenarios with identical projections grouped",
    x = "Year", y = "Material demand (Gt/yr)"
  ) +
  theme_sturm +
  theme(legend.text = element_text(size = 9)) +
  guides(color    = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2))

print(pA_global_mat)

ggsave(file.path(plot_dir, "plot_02A_comm_global_material_demand.png"),
       plot = pA_global_mat, width = 10, height = 6, dpi = 300)

# ── Step 11: Panel B — % breakdown 2100 ──────────────────────

df_pct_2100 <- df_region_mat_by_material_plus_world %>%
  filter(year == panel_b_year) %>%
  group_by(scenario, region_plot) %>%
  mutate(pct = mat_demand_Mt / sum(mat_demand_Mt, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  mutate(material = factor(material, levels = rev(material_order_2100)))

pB_region_mat_2100 <- df_pct_2100 %>%
  ggplot(aes(x = scenario, y = pct, fill = material)) +
  geom_col(width = 0.75, color = NA) +
  facet_wrap(~ region_plot, ncol = 4) +
  scale_fill_manual(values = material_colors, breaks = material_order_2100) +
  scale_x_discrete(labels = scenario_labels_short) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = paste0("Regional commercial material demand — % breakdown by material, ", panel_b_year),
    subtitle = "100% stacked bars show material composition by scenario and region",
    x = NULL, y = "Share of material demand (%)", fill = "Material"
  ) +
  theme_sturm +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

print(pB_region_mat_2100)

ggsave(file.path(plot_dir, "plot_02B_comm_regional_material_pct_2100.png"),
       plot = pB_region_mat_2100, width = 13, height = 8, dpi = 300)

# ── Step 12: Summary table (print only) ──────────────────────

cat("\n=== Material demand summary — 2025 & 2100 ===\n")
df_global_mat_demand %>%
  filter(year %in% c(baseline_year, panel_b_year)) %>%
  mutate(scenario = factor(scenario, levels = scenarios)) %>%
  arrange(year, scenario) %>%
  mutate(`Material demand (Gt/yr)` = round(mat_demand_Gt, 3)) %>%
  select(year, scenario, `Material demand (Gt/yr)`) %>%
  print(n = Inf)

# ── Step 13: Panel C — Narrow vs Reference focus ─────────────

scenarios_focus <- c("R", "N_r", "N_tp")

scenario_labels_focus <- c(
  "R"    = "R (Ref)",
  "N_r"  = "N_r",
  "N_tp" = "N_tp"
)

df_pct_2100_focus <- df_region_mat_by_material_plus_world %>%
  filter(year == panel_b_year) %>%
  mutate(scenario = as.character(scenario)) %>%
  filter(scenario %in% scenarios_focus) %>%
  group_by(scenario, region_plot) %>%
  mutate(pct = mat_demand_Mt / sum(mat_demand_Mt, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  mutate(
    material = factor(material, levels = rev(material_order_2100)),
    scenario = factor(scenario, levels = scenarios_focus)
  )

pC_narrow_focus <- df_pct_2100_focus %>%
  ggplot(aes(x = scenario, y = pct, fill = material)) +
  geom_col(width = 0.6, color = NA) +
  facet_wrap(~ region_plot, ncol = 4) +
  scale_fill_manual(values = material_colors,
                    breaks = material_order_2100,
                    name   = "Material") +
  scale_x_discrete(labels = scenario_labels_focus) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = paste0("Material composition — Reference vs Narrow strategies, ", panel_b_year),
    subtitle = "R = Reference | N_r = Narrow moderate | N_tp = Narrow ambitious",
    x = NULL,
    y = "Share of material demand (%)",
    fill = "Material"
  ) +
  theme_sturm +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9))

print(pC_narrow_focus)

ggsave(file.path(plot_dir, "plot_02C_comm_material_pct_narrow_focus.png"),
       plot = pC_narrow_focus, width = 13, height = 9, dpi = 300)

cat("\nDone. Plots saved to:\n", plot_dir, "\n")
