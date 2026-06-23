# =========================================================
# Fig8_v3
# Material-specific change under Narrow scenarios in 2050
# Residential + commercial
# =========================================================

# -----------------------------
# 0. Packages
# -----------------------------
library(tidyverse)
library(readr)
library(scales)
library(grid)

# -----------------------------
# 1. File paths
# -----------------------------
base_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/output"
out_dir  <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2. Scenario and sector settings
# -----------------------------
scenarios <- c("R", "N_r", "N_tp")

scenario_labels <- c(
  R    = "Reference",
  N_r  = "Narrow R",
  N_tp = "Narrow TP"
)

scenario_order <- c("Reference", "Narrow R", "Narrow TP")

sectors <- c("resid", "comm")

sector_labels <- c(
  "resid" = "Residential",
  "comm"  = "Commercial"
)

sector_order <- c("Residential", "Commercial")

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

panel_year <- 2050

# -----------------------------
# 4. Helper: find material report file
# -----------------------------
# Residential outputs are usually region_bld.
# Commercial outputs may be R12.
# This searches flexibly.

find_material_file <- function(scenario_code, sector_code, base_dir) {
  
  pattern <- paste0(
    "^report_STURM_",
    scenario_code,
    "_",
    sector_code,
    "_.*_material\\.csv$"
  )
  
  files <- list.files(
    base_dir,
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop(
      "No material file found for scenario = ",
      scenario_code,
      ", sector = ",
      sector_code,
      "\nPattern used: ",
      pattern
    )
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
  
  # Otherwise use first match
  files[1]
}

# -----------------------------
# 5. Read and combine data
# -----------------------------

read_material_file <- function(scenario_code, sector_code) {
  
  file_path <- find_material_file(
    scenario_code = scenario_code,
    sector_code = sector_code,
    base_dir = base_dir
  )
  
  cat("Loaded:", basename(file_path), "\n")
  
  read_csv(file_path, show_col_types = FALSE) %>%
    mutate(
      scenario = scenario_code,
      scenario_label = scenario_labels[scenario_code],
      sector = sector_code,
      sector_label = sector_labels[sector_code]
    )
}

df_all <- crossing(
  scenario = scenarios,
  sector = sectors
) %>%
  pmap_dfr(function(scenario, sector) {
    read_material_file(
      scenario_code = scenario,
      sector_code = sector
    )
  })

cat("\nLoaded scenarios and sectors:\n")
df_all %>%
  count(sector_label, scenario_label) %>%
  print(n = Inf)

cat("\nAvailable years:\n")
print(sort(unique(df_all$year)))

cat("\nAvailable materials:\n")
print(sort(unique(df_all$material)))

if (!panel_year %in% unique(df_all$year)) {
  stop(paste0("panel_year = ", panel_year, " is not available in the data."))
}

# -----------------------------
# 6. Deduplicate and aggregate material demand
# -----------------------------
# mat_demand_Mt is material-specific, so keep 'material' in the key.
# Residential files may include region_bld/clim/inc_cl.
# Commercial files may include R12 instead.
# Use the intersection of available columns.

required_basic_cols <- c(
  "material", "scenario", "scenario_label",
  "sector", "sector_label", "year", "mat_demand_Mt"
)

missing_basic_cols <- setdiff(required_basic_cols, names(df_all))

if (length(missing_basic_cols) > 0) {
  stop(
    paste0(
      "Missing required columns:\n",
      paste(missing_basic_cols, collapse = "\n")
    )
  )
}

mat_key_cols <- c(
  "R12",
  "region_bld", "region_gea", "urt", "clim",
  "inc_cl", "arch", "mat", "eneff",
  "material",
  "scenario", "scenario_label",
  "sector", "sector_label",
  "year",
  "mat_demand_Mt"
)

mat_key_cols <- intersect(mat_key_cols, names(df_all))

df_demand <- df_all %>%
  mutate(
    material = tolower(material),
    scenario_label = factor(scenario_label, levels = scenario_order),
    sector_label = factor(sector_label, levels = sector_order)
  ) %>%
  filter(material %in% material_order) %>%
  select(all_of(mat_key_cols)) %>%
  distinct() %>%
  group_by(
    sector,
    sector_label,
    scenario,
    scenario_label,
    year,
    material
  ) %>%
  summarise(
    mat_demand_Mt = sum(mat_demand_Mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mat_demand_Gt = mat_demand_Mt / 1000,
    scenario_label = factor(scenario_label, levels = scenario_order),
    sector_label = factor(sector_label, levels = sector_order),
    material = factor(material, levels = material_order)
  )

# -----------------------------
# 7. Reference demand by sector and material
# -----------------------------

ref_demand <- df_demand %>%
  filter(scenario == "R", year == panel_year) %>%
  group_by(sector, sector_label, material) %>%
  summarise(
    ref_Gt = sum(mat_demand_Gt, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# 8. Narrow R and Narrow TP relative to Reference
# -----------------------------

df_narrow_change <- df_demand %>%
  filter(
    scenario %in% c("N_r", "N_tp"),
    year == panel_year
  ) %>%
  group_by(
    sector,
    sector_label,
    scenario,
    scenario_label,
    material
  ) %>%
  summarise(
    demand_Gt = sum(mat_demand_Gt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    ref_demand,
    by = c("sector", "sector_label", "material")
  ) %>%
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
    ),
    sector_label = factor(sector_label, levels = sector_order)
  )

df_narrow_r <- df_narrow_change %>%
  filter(scenario == "N_r")

df_narrow_tp <- df_narrow_change %>%
  filter(scenario == "N_tp")

cat(paste0("\nNarrow material-specific changes in ", panel_year, " relative to sector-specific Reference:\n"))
df_narrow_change %>%
  select(
    sector_label,
    scenario_label,
    material,
    demand_Gt,
    ref_Gt,
    pct_change
  ) %>%
  arrange(sector_label, material, scenario_label) %>%
  print(n = Inf, width = Inf)

# -----------------------------
# 9. Optional: check y-axis range
# -----------------------------

cat("\nRange of percentage changes:\n")
df_narrow_change %>%
  group_by(sector_label) %>%
  summarise(
    min_pct = min(pct_change, na.rm = TRUE),
    max_pct = max(pct_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# -----------------------------
# 10. Theme
# -----------------------------

theme_fig <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    
    strip.text.y.right = element_text(
      face = "bold",
      size = 13,
      angle = 270
    ),
    strip.background = element_blank(),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    
    panel.spacing.y = unit(1.2, "lines"),
    
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 12, 10, 10)
  )

# -----------------------------
# 11. Plot
# -----------------------------
# Wide filled bars = Narrow TP
# Narrow hollow bars = Narrow R

fig8_v3 <- ggplot() +
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
  
  facet_grid(
    rows = vars(sector_label),
    scales = "free_y"
  ) +
  scale_fill_manual(values = material_colours, drop = FALSE) +
  scale_x_discrete(
    limits = material_order,
    labels = function(x) str_to_title(x)
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "%", accuracy = 1),
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  labs(
    title = paste0("Material-specific change under Narrow scenarios in ", panel_year),
    subtitle = "Wide filled bars = Narrow TP; narrow hollow bars = Narrow R",
    x = NULL,
    y = "Change relative to sector-specific Reference (%)"
  ) +
  guides(
    fill = "none"
  ) +
  theme_fig

print(fig8_v3)

# -----------------------------
# 12. Save outputs
# -----------------------------

file_stub <- paste0("Fig8_v4_residential_commercial_material_specific_change_narrow_", panel_year)

ggsave(
  filename = file.path(out_dir, paste0(file_stub, ".png")),
  plot = fig8_v3,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(out_dir, paste0(file_stub, ".tiff")),
  plot = fig8_v3,
  width = 10,
  height = 7,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(out_dir, paste0(file_stub, ".pdf")),
  plot = fig8_v3,
  width = 10,
  height = 7,
  device = cairo_pdf,
  bg = "white"
)

write_csv(
  df_narrow_change,
  file.path(out_dir, paste0(file_stub, "_data.csv"))
)

cat("\nSaved Fig8_v3 outputs to:\n")
cat(out_dir, "\n")

################################
#### CHECK
################################
# =========================================================
# Extract exact Fig. 8 numbers for Results text
# Material-specific changes under Narrow scenarios in 2050
# Residential + commercial
# =========================================================

cat("\n\n============================================================\n")
cat("Exact Fig. 8 numbers for Results text\n")
cat("Material-specific changes under Narrow scenarios in ", panel_year, "\n", sep = "")
cat("============================================================\n")

# Helper formatting
fmt_num <- function(x, digits = 3) {
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  paste0(format(round(x, digits), nsmall = digits, trim = TRUE), "%")
}

# ── 1) Core table: material-specific changes ────────────────

fig8_changes <- df_narrow_change %>%
  mutate(
    abs_change_Gt = demand_Gt - ref_Gt,
    abs_reduction_Gt = ref_Gt - demand_Gt,
    pct_reduction = -pct_change
  ) %>%
  select(
    sector_label,
    scenario_label,
    material,
    ref_Gt,
    demand_Gt,
    abs_change_Gt,
    abs_reduction_Gt,
    pct_change,
    pct_reduction
  ) %>%
  arrange(sector_label, scenario_label, material)

cat("\nMaterial-specific changes under Narrow scenarios:\n")
cat("Units: Gt/yr and % relative to sector-specific Reference\n")

fig8_changes %>%
  mutate(
    across(
      c(ref_Gt, demand_Gt, abs_change_Gt, abs_reduction_Gt),
      ~ round(.x, 5)
    ),
    pct_change = round(pct_change, 1),
    pct_reduction = round(pct_reduction, 1)
  ) %>%
  print(n = Inf, width = Inf)

# ── 2) Key materials only ───────────────────────────────────

key_materials <- c("concrete", "brick", "cement", "steel", "wood")

fig8_key_materials <- fig8_changes %>%
  filter(as.character(material) %in% key_materials) %>%
  arrange(sector_label, scenario_label, material)

cat("\nKey material changes for Results text:\n")

fig8_key_materials %>%
  mutate(
    across(
      c(ref_Gt, demand_Gt, abs_change_Gt, abs_reduction_Gt),
      ~ round(.x, 5)
    ),
    pct_change = round(pct_change, 1),
    pct_reduction = round(pct_reduction, 1)
  ) %>%
  print(n = Inf, width = Inf)

# ── 3) Largest absolute reductions by sector/scenario ───────
# Useful for identifying which materials drive total reductions.

fig8_largest_abs_reductions <- fig8_changes %>%
  filter(abs_reduction_Gt > 0) %>%
  group_by(sector_label, scenario_label) %>%
  arrange(desc(abs_reduction_Gt), .by_group = TRUE) %>%
  mutate(rank_abs_reduction = row_number()) %>%
  slice_head(n = 5) %>%
  ungroup()

cat("\nLargest absolute material-demand reductions:\n")

fig8_largest_abs_reductions %>%
  mutate(
    across(
      c(ref_Gt, demand_Gt, abs_change_Gt, abs_reduction_Gt),
      ~ round(.x, 5)
    ),
    pct_change = round(pct_change, 1),
    pct_reduction = round(pct_reduction, 1)
  ) %>%
  print(n = Inf, width = Inf)

# ── 4) Wood-specific changes ────────────────────────────────
# Useful for the wood trade-off paragraph.

fig8_wood <- fig8_changes %>%
  filter(material == "wood") %>%
  arrange(sector_label, scenario_label)

cat("\nWood-specific changes under Narrow scenarios:\n")

fig8_wood %>%
  mutate(
    across(
      c(ref_Gt, demand_Gt, abs_change_Gt, abs_reduction_Gt),
      ~ round(.x, 5)
    ),
    pct_change = round(pct_change, 1),
    pct_reduction = round(pct_reduction, 1)
  ) %>%
  print(n = Inf, width = Inf)

cat("\nText-ready wood sentences:\n")

fig8_wood %>%
  mutate(
    sentence = paste0(
      sector_label, ", ", scenario_label, ": wood demand changes from ",
      fmt_num(ref_Gt, 4), " Gt/yr under Reference to ",
      fmt_num(demand_Gt, 4), " Gt/yr, a ",
      if_else(pct_change >= 0, "increase of ", "decrease of "),
      fmt_pct(abs(pct_change), 1), "."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ── 5) Concrete, brick, cement, steel reductions ────────────

main_declining_materials <- c("concrete", "brick", "cement", "steel")

fig8_main_reductions <- fig8_changes %>%
  filter(as.character(material) %in% main_declining_materials) %>%
  arrange(sector_label, scenario_label, desc(abs_reduction_Gt))

cat("\nConcrete, brick, cement, and steel reductions:\n")

fig8_main_reductions %>%
  mutate(
    across(
      c(ref_Gt, demand_Gt, abs_change_Gt, abs_reduction_Gt),
      ~ round(.x, 5)
    ),
    pct_change = round(pct_change, 1),
    pct_reduction = round(pct_reduction, 1)
  ) %>%
  print(n = Inf, width = Inf)

# ── 6) Aggregate reductions for concrete + brick + cement ───
# Useful for summarising dominant heavy/material categories.

fig8_cbc <- fig8_changes %>%
  filter(as.character(material) %in% c("concrete", "brick", "cement")) %>%
  group_by(sector_label, scenario_label) %>%
  summarise(
    ref_cbc_Gt = sum(ref_Gt, na.rm = TRUE),
    demand_cbc_Gt = sum(demand_Gt, na.rm = TRUE),
    abs_reduction_cbc_Gt = sum(abs_reduction_Gt, na.rm = TRUE),
    pct_reduction_cbc = 100 * abs_reduction_cbc_Gt / ref_cbc_Gt,
    .groups = "drop"
  ) %>%
  arrange(sector_label, scenario_label)

cat("\nAggregate concrete + brick + cement reductions:\n")

fig8_cbc %>%
  mutate(
    across(
      c(ref_cbc_Gt, demand_cbc_Gt, abs_reduction_cbc_Gt),
      ~ round(.x, 5)
    ),
    pct_reduction_cbc = round(pct_reduction_cbc, 1)
  ) %>%
  print(n = Inf, width = Inf)

# ── 7) Compare Narrow R vs Narrow TP by material ─────────────
# Useful for saying where TP assumptions matter most.

fig8_r_vs_tp <- fig8_changes %>%
  select(
    sector_label,
    scenario_label,
    material,
    demand_Gt,
    pct_change
  ) %>%
  pivot_wider(
    names_from = scenario_label,
    values_from = c(demand_Gt, pct_change)
  ) %>%
  mutate(
    TP_minus_R_demand_Gt =
      `demand_Gt_Narrow TP` - `demand_Gt_Narrow R`,
    TP_minus_R_pct_change_pp =
      `pct_change_Narrow TP` - `pct_change_Narrow R`
  ) %>%
  arrange(sector_label, material)

cat("\nNarrow TP minus Narrow R differences by material:\n")

fig8_r_vs_tp %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 5)
    )
  ) %>%
  print(n = Inf, width = Inf)

# ── 8) Text-ready summary for top reductions ────────────────

cat("\nText-ready top-reduction sentences:\n")

fig8_largest_abs_reductions %>%
  group_by(sector_label, scenario_label) %>%
  slice_head(n = 3) %>%
  summarise(
    sentence = paste0(
      sector_label, ", ", scenario_label,
      ": largest absolute reductions are in ",
      paste0(
        as.character(material),
        " (",
        fmt_num(abs_reduction_Gt, 3),
        " Gt/yr; ",
        fmt_pct(pct_reduction, 1),
        " reduction)",
        collapse = ", "
      ),
      "."
    ),
    .groups = "drop"
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ── 9) Save text-supporting tables ──────────────────────────

write_csv(
  fig8_changes,
  file.path(out_dir, paste0(file_stub, "_changes_for_text.csv"))
)

write_csv(
  fig8_key_materials,
  file.path(out_dir, paste0(file_stub, "_key_materials_for_text.csv"))
)

write_csv(
  fig8_largest_abs_reductions,
  file.path(out_dir, paste0(file_stub, "_largest_abs_reductions_for_text.csv"))
)

write_csv(
  fig8_wood,
  file.path(out_dir, paste0(file_stub, "_wood_for_text.csv"))
)

write_csv(
  fig8_main_reductions,
  file.path(out_dir, paste0(file_stub, "_main_reductions_for_text.csv"))
)

write_csv(
  fig8_cbc,
  file.path(out_dir, paste0(file_stub, "_concrete_brick_cement_for_text.csv"))
)

write_csv(
  fig8_r_vs_tp,
  file.path(out_dir, paste0(file_stub, "_narrow_tp_minus_r_for_text.csv"))
)

cat("\nSaved Fig. 8 text-supporting tables to:\n")
cat(out_dir, "\n")