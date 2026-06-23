# ============================================================
# Fig6_v3. Cumulative construction material-demand reduction
# Residential + commercial under circular scenarios
#
# Values show avoided cumulative demand relative to sector-specific
# Reference, 2025–2100
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
# 2. Scenario and sector settings
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

scenario_order <- unname(scenario_labels[scenarios])

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

sectors <- c("resid", "comm")

sector_labels <- c(
  "resid" = "Residential",
  "comm"  = "Commercial"
)

sector_order <- c("Residential", "Commercial")

end_year <- 2100

years_to_use <- c(seq(2025, 2060, 5), seq(2070, 2100, 10))

# ----------------------------
# 3. Helper: find material report
# ----------------------------
# Residential is usually region_bld.
# Commercial may be R12.
# This searches flexibly.

find_material_file <- function(sc, sector) {
  
  pattern <- paste0(
    "^report_STURM_",
    sc,
    "_",
    sector,
    "_.*_material\\.csv$"
  )
  
  files <- list.files(
    input_dir,
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    cat("NOT FOUND for scenario =", sc, "sector =", sector, "\n")
    return(NA_character_)
  }
  
  # Prefer region_bld if available
  region_bld_file <- files[str_detect(files, "_region_bld_material\\.csv$")]
  
  if (length(region_bld_file) > 0) {
    return(region_bld_file[1])
  }
  
  # Otherwise prefer R12 if available
  r12_file <- files[str_detect(files, "_R12_material\\.csv$")]
  
  if (length(r12_file) > 0) {
    return(r12_file[1])
  }
  
  files[1]
}

# ----------------------------
# 4. Read material reports
# ----------------------------

read_material_report <- function(sc, sector) {
  
  f <- find_material_file(sc, sector)
  
  if (is.na(f) || !file.exists(f)) {
    return(NULL)
  }
  
  cat("Loaded:", basename(f), "\n")
  
  read_csv(f, show_col_types = FALSE) %>%
    mutate(
      scenario = sc,
      scenario_label = scenario_labels[sc],
      sector = sector,
      sector_label = sector_labels[sector]
    )
}

df_raw <- crossing(
  scenario = scenarios,
  sector = sectors
) %>%
  pmap_dfr(function(scenario, sector) {
    read_material_report(sc = scenario, sector = sector)
  })

if (nrow(df_raw) == 0) {
  stop("No material files loaded. Check input_dir, scenario names, and sector names.")
}

df_raw <- df_raw %>%
  mutate(
    scenario = factor(scenario, levels = scenarios),
    scenario_label = factor(scenario_label, levels = scenario_order),
    sector_label = factor(sector_label, levels = sector_order)
  )

cat("\nLoaded rows by sector and scenario:\n")
print(
  df_raw %>%
    count(sector_label, scenario_label),
  n = Inf
)

cat("\nYears available:\n")
print(sort(unique(df_raw$year)))

cat("\nMaterials available:\n")
print(sort(unique(df_raw$material)))

# ----------------------------
# 5. Deduplicate material-demand records
# ----------------------------
# mat_demand_Mt is material-specific, so keep 'material' in the key.
# Commercial files may have R12 instead of region_bld/clim/inc_cl.
# Use the intersection of available columns.

mat_key_cols <- c(
  "R12",
  "region_bld", "region_gea", "urt", "clim",
  "inc_cl", "arch", "mat", "eneff",
  "material",
  "sector", "sector_label",
  "scenario", "scenario_label",
  "year",
  "mat_demand_Mt", "mat_primary_Mt",
  "mat_stock_Mt", "mat_scrap_Mt",
  "mat_reuse_Mt", "mat_recycling_Mt",
  "mat_downcycling_Mt", "mat_other_treat_Mt"
)

mat_key_cols <- intersect(mat_key_cols, names(df_raw))

df_mat <- df_raw %>%
  filter(year %in% years_to_use) %>%
  select(all_of(mat_key_cols)) %>%
  distinct()

cat("\nRows before deduplication:", nrow(df_raw), "\n")
cat("Rows after deduplication: ", nrow(df_mat), "\n")

# ----------------------------
# 6. Aggregate total annual material demand
# ----------------------------

df_total <- df_mat %>%
  group_by(sector, sector_label, year, scenario, scenario_label) %>%
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
  select(sector_label, year, scenario_label, mat_demand_Gt, mat_primary_Gt) %>%
  arrange(sector_label, year, scenario_label) %>%
  print(n = Inf, width = Inf)

# ----------------------------
# 7. Cumulative material-demand reduction
# ----------------------------
# mat_demand_Gt is annual material demand.
#
# Each model-year value is multiplied by the following timestep length
# to approximate cumulative material demand over the model period.
#
# Reduction is calculated separately for each sector relative to that
# sector's Reference scenario.

df_total_cumulative <- df_total %>%
  arrange(sector_label, scenario_label, year) %>%
  group_by(sector, sector_label, scenario_label) %>%
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
  select(
    sector,
    sector_label,
    ref_cumulative_mat_demand_Gt = cumulative_mat_demand_Gt
  )

if (nrow(ref_cumulative) == 0) {
  stop("Could not calculate cumulative Reference demand. Check Reference scenario and end_year.")
}

df_fig6 <- df_total_cumulative %>%
  filter(year == end_year, scenario_label != "Reference") %>%
  left_join(
    ref_cumulative,
    by = c("sector", "sector_label")
  ) %>%
  mutate(
    cumulative_reduction_Gt =
      ref_cumulative_mat_demand_Gt - cumulative_mat_demand_Gt,
    cumulative_reduction_pct =
      cumulative_reduction_Gt / ref_cumulative_mat_demand_Gt * 100,
    scenario_label = factor(scenario_label, levels = scenario_order),
    sector_label = factor(sector_label, levels = sector_order)
  )

cat("\nCumulative material-demand reduction, 2025–2100, relative to sector-specific Reference:\n")
df_fig6 %>%
  select(
    sector_label,
    scenario_label,
    cumulative_mat_demand_Gt,
    ref_cumulative_mat_demand_Gt,
    cumulative_reduction_Gt,
    cumulative_reduction_pct
  ) %>%
  arrange(sector_label, scenario_label) %>%
  print(n = Inf, width = Inf)

# ----------------------------
# 8. Theme
# ----------------------------

theme_fig <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    
    strip.text = element_text(face = "bold", size = 13),
    strip.background = element_blank(),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 12, 10, 10)
  )

# ----------------------------
# 9. Plot
# ----------------------------

fig6_v3 <- ggplot(
  df_fig6,
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
    size = 4.0
  ) +
  facet_grid(
    rows = vars(sector_label),
    scales = "free_y"
  ) +
  scale_fill_manual(values = strategy_colours, guide = "none") +
  scale_x_discrete(labels = scenario_short) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(
      0,
      max(df_fig6$cumulative_reduction_pct, na.rm = TRUE) * 1.12
    ),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Cumulative construction material-demand reduction",
    subtitle = "Values show avoided cumulative demand relative to sector-specific Reference, 2025–2100",
    x = NULL,
    y = "Cumulative reduction (%)"
  ) +
  theme_fig +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

print(fig6_v3)

# ----------------------------
# 10. Save outputs
# ----------------------------

file_stub <- "Fig6_v4_cumulative_residential_commercial_material_demand_reduction"

ggsave(
  filename = file.path(output_dir, paste0(file_stub, ".png")),
  plot = fig6_v3,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(output_dir, paste0(file_stub, ".tiff")),
  plot = fig6_v3,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(output_dir, paste0(file_stub, ".pdf")),
  plot = fig6_v3,
  width = 10,
  height = 7,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

write_csv(
  df_total,
  file.path(output_dir, paste0(file_stub, "_annual_material_demand.csv"))
)

write_csv(
  df_total_cumulative,
  file.path(output_dir, paste0(file_stub, "_cumulative_material_demand.csv"))
)

write_csv(
  df_fig6,
  file.path(output_dir, paste0(file_stub, "_reduction_summary.csv"))
)

cat("\nSaved Fig6_v3 outputs to:\n")
cat(output_dir, "\n")