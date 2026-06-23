# ============================================================
# STURM Results — Commercial Floorspace Trajectories
#
# Date: 2026-05-26
#
# Plots:
#   1A. Global commercial floorspace — all scenarios, colored by group
#   1B. Regional floorspace by region_gea + World — 3 groups
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

floorspace_col <- "floor_Mm2"

make_energy_file_path <- function(scenario) {
  file.path(
    output_dir,
    paste0("report_STURM_", scenario, "_comm_region_bld_energy.csv")
  )
}

# ── Step 1: Load all energy files ───────────────────────────

df_energy_all <- map_dfr(scenarios, function(sc) {
  f <- make_energy_file_path(sc)
  if (!file.exists(f)) {
    cat("  NOT FOUND:", f, "\n")
    return(NULL)
  }
  read_csv(f, show_col_types = FALSE) %>%
    mutate(scenario = sc)
}) %>%
  mutate(scenario = factor(scenario, levels = scenarios))

# ── Step 2: Basic checks ─────────────────────────────────────

cat("=== Basic checks ===\n")
cat("Total rows:", nrow(df_energy_all), "\n")
cat("\nScenarios loaded:\n");  print(table(df_energy_all$scenario))
cat("\nYears available:\n");   print(sort(unique(df_energy_all$year)))
cat("\nYear range by scenario:\n")
df_energy_all %>%
  group_by(scenario) %>%
  summarise(min_year = min(year), max_year = max(year), n_years = n_distinct(year), .groups = "drop") %>%
  print()
cat("\nRegions, region_gea:\n"); print(sort(unique(df_energy_all$region_gea)))

# ── Step 3: Deduplicate building segments ────────────────────

df_floor_segments <- df_energy_all %>%
  select(year, scenario, region_bld, region_gea, urt, clim,
         inc_cl, arch, mat, eneff, all_of(floorspace_col)) %>%
  distinct() %>%
  rename(floor_Mm2 = all_of(floorspace_col))

cat("\nRows after deduplication:", nrow(df_floor_segments), "\n")

# ── Step 4: Global and regional floorspace ───────────────────

df_global_floor <- df_floor_segments %>%
  group_by(year, scenario) %>%
  summarise(
    floor_Mm2        = sum(floor_Mm2, na.rm = TRUE),
    floor_billion_m2 = floor_Mm2 / 1000,
    .groups = "drop"
  )

df_region_floor <- df_floor_segments %>%
  group_by(year, scenario, region_gea) %>%
  summarise(
    floor_Mm2        = sum(floor_Mm2, na.rm = TRUE),
    floor_billion_m2 = floor_Mm2 / 1000,
    .groups = "drop"
  )

cat("\n=== Global floorspace check ===\n")
df_global_floor %>%
  filter(year %in% c(2020, 2025, 2050, 2100)) %>%
  arrange(year, scenario) %>%
  print(n = 60)

# ── Step 5: Add World as extra regional facet ────────────────

region_order_plus_world <- c(sort(unique(df_region_floor$region_gea)), "World")

df_region_floor_plus_world <- bind_rows(
  df_region_floor %>% mutate(region_plot = region_gea),
  df_global_floor %>%
    mutate(region_plot = "World") %>%
    select(year, scenario, region_plot, floor_Mm2, floor_billion_m2)
) %>%
  mutate(region_plot = factor(region_plot, levels = region_order_plus_world))

# ── Step 6: Themes ───────────────────────────────────────────

theme_sturm <- theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 10),
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(size = 12),
    axis.title       = element_text(size = 13),
    axis.text        = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

theme_sturm_facet <- theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(size = 12),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 9),
    strip.text       = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

# ── Step 7: Define 3 scenario groups ─────────────────────────

group_colors <- c("#2166ac", "#66a61e", "#d95f02")

scenario_groups <- list(
  "High (R, Close, Slow)" = c("R", "C_r", "C_tp", "S_r", "S_tp"),
  "Mid (Narrow _r)"       = c("N_r", "A_r", "E_r"),
  "Low (Narrow _tp, LED)" = c("N_tp", "A_tp", "E_tp", "LED")
)

grp_levels <- names(scenario_groups)
grp_colors <- setNames(group_colors, grp_levels)

# ── Step 8: Styling ──────────────────────────────────────────

scenario_lt_named <- c(
  "R"    = "solid",
  "C_r"  = "solid",   "C_tp" = "dashed",
  "S_r"  = "dotted",  "S_tp" = "dotdash",
  "N_r"  = "solid",   "A_r"  = "dashed",  "E_r"  = "dotted",
  "N_tp" = "solid",   "A_tp" = "dashed",  "E_tp" = "dotted",
  "LED"  = "longdash"
)

traj_linetypes_3 <- c("solid", "dashed", "dotted")
grp_lt_named     <- setNames(traj_linetypes_3, grp_levels)

# ── Step 9: Map each scenario to its group ───────────────────

df_scenario_groups <- map_dfr(seq_along(scenario_groups), function(i) {
  tibble(
    scenario    = scenario_groups[[i]],
    group_label = grp_levels[i]
  )
})

df_global_floor_chr <- df_global_floor %>%
  mutate(scenario = as.character(scenario)) %>%
  left_join(df_scenario_groups, by = "scenario") %>%
  mutate(group_label = factor(group_label, levels = grp_levels))

# ── Step 10: Plot 1A — All scenarios, colored by group ───────

p1A <- df_global_floor_chr %>%
  ggplot(aes(x = year, y = floor_billion_m2,
             group    = scenario,
             color    = group_label,
             linetype = scenario)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  scale_color_manual(values = grp_colors, name = "Trajectory group") +
  scale_linetype_manual(values = scenario_lt_named, name = "Scenario") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = c(2020, 2030, 2040, 2050, 2060, 2080, 2100)) +
  labs(
    title    = "Global commercial floorspace — all scenarios",
    subtitle = "12 scenarios grouped into 3 floorspace trajectory groups | Color = group",
    x = "Year",
    y = expression("Total floorspace (billion m"^2*")")
  ) +
  theme_sturm +
  guides(
    color    = guide_legend(ncol = 1, order = 1),
    linetype = "none"
  )

print(p1A)

ggsave(file.path(plot_dir, "plot_01A_comm_global_floorspace.png"),
       plot = p1A, width = 11, height = 7, dpi = 300)

# ── Step 11: Plot 1B — Regional floorspace + World ───────────

representative_scenarios <- c("R", "N_r", "N_tp")

group_label_map <- c(
  "R"    = "High (R, Close, Slow)",
  "N_r"  = "Mid (Narrow _r)",
  "N_tp" = "Low (Narrow _tp, LED)"
)

df_region_3grp <- df_region_floor_plus_world %>%
  filter(as.character(scenario) %in% representative_scenarios) %>%
  mutate(
    scenario    = as.character(scenario),
    group_label = factor(group_label_map[scenario], levels = grp_levels)
  )

p1_region_floor <- df_region_3grp %>%
  ggplot(aes(x = year, y = floor_billion_m2,
             color    = group_label,
             linetype = group_label)) +
  geom_line(linewidth = 0.95) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_color_manual(values = grp_colors) +
  scale_linetype_manual(values = grp_lt_named) +
  facet_wrap(~ region_plot, scales = "free_y", ncol = 4) +
  labs(
    title    = "Regional commercial floorspace trajectories",
    subtitle = "Facets show GEA regions plus World | y-axis free by region | 3 trajectory groups",
    x = "Year",
    y = expression("Floorspace (billion m"^2*")")
  ) +
  theme_sturm_facet +
  theme(legend.title = element_blank(),
        legend.text  = element_text(size = 9)) +
  guides(color    = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1))

print(p1_region_floor)

ggsave(file.path(plot_dir, "plot_01B_comm_regional_floorspace.png"),
       plot = p1_region_floor, width = 13, height = 8, dpi = 300)

# ── Step 12: 2100 summary table (print only) ─────────────────

floor_2100 <- df_global_floor %>% filter(year == 2100)
ref_2100   <- floor_2100 %>% filter(scenario == "R") %>% pull(floor_Mm2)

cat("\n=== 2100 global commercial floorspace summary ===\n")
floor_2100 %>%
  mutate(scenario = as.character(scenario)) %>%
  left_join(df_scenario_groups, by = "scenario") %>%
  mutate(
    pct_change_vs_R                = (floor_Mm2 - ref_2100) / ref_2100 * 100,
    scenario                       = factor(scenario, levels = scenarios),
    `2100 floorspace (billion m²)` = round(floor_billion_m2, 1),
    `% change vs R`                = round(pct_change_vs_R, 2)
  ) %>%
  arrange(scenario) %>%
  select(scenario, group_label, `2100 floorspace (billion m²)`, `% change vs R`) %>%
  print(n = Inf)

cat("\nDone. Plots saved to:\n", plot_dir, "\n")
