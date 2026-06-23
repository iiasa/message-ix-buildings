# =========================================================
# Fig7_v3
# Residential + commercial construction material composition
# in 2050
#
# Updates:
# 1) Residential and Commercial panels use free y-axis scales
#    but equal panel heights.
# 2) Increased vertical gap between panels.
# 3) Reversed material stacking and legend order.
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

scenario_order <- c(
  "Reference",
  "Narrow R", "Narrow TP",
  "Slow R", "Slow TP",
  "Close R", "Close TP",
  "Combined R", "Combined TP"
)

sectors <- c("resid", "comm")

sector_labels <- c(
  "resid" = "Residential",
  "comm"  = "Commercial"
)

sector_order <- c("Residential", "Commercial")

# -----------------------------
# 3. Material order and colours
# -----------------------------
# Reversed order relative to the previous version.
# This controls both stacking order and legend order.

material_order <- c(
  "copper", "aluminum", "glass", "wood",
  "steel", "cement", "brick", "concrete"
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
  
  # Otherwise use the first match
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
  group_by(sector, sector_label, scenario, scenario_label, year, material) %>%
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
# 7. Prepare Fig7 data
# -----------------------------

df_fig7 <- df_demand %>%
  filter(year == panel_year) %>%
  group_by(sector, sector_label, scenario_label, material) %>%
  summarise(
    mat_demand_Gt = sum(mat_demand_Gt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    scenario_label = factor(scenario_label, levels = scenario_order),
    sector_label = factor(sector_label, levels = sector_order),
    material = factor(material, levels = material_order)
  )

cat(paste0("\n", panel_year, " material demand by sector, scenario, and material:\n"))
df_fig7 %>%
  arrange(sector_label, scenario_label, material) %>%
  print(n = Inf, width = Inf)

cat(paste0("\n", panel_year, " total material demand by sector and scenario:\n"))
df_fig7 %>%
  group_by(sector_label, scenario_label) %>%
  summarise(
    total_mat_demand_Gt = sum(mat_demand_Gt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(sector_label, scenario_label) %>%
  print(n = Inf, width = Inf)

# -----------------------------
# 8. Theme
# -----------------------------

theme_fig <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.text.y = element_text(size = 11),
    
    strip.text.y.right = element_text(
      face = "bold",
      size = 13,
      angle = 270
    ),
    strip.background = element_blank(),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key.width = unit(1.1, "cm"),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    
    # Increased gap between Residential and Commercial panels
    # while keeping equal panel heights.
    panel.spacing.y = unit(1.8, "lines"),
    
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 12, 10, 10)
  )

# -----------------------------
# 9. Plot
# -----------------------------
# Important:
# - scales = "free_y" keeps residential and commercial readable.
# - Do NOT use space = "free_y", because it compresses the residential panel.

fig7_v3 <- ggplot(
  df_fig7,
  aes(
    x = scenario_label,
    y = mat_demand_Gt,
    fill = material
  )
) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.15) +
  facet_grid(
    rows = vars(sector_label),
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = material_colours,
    breaks = material_order,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.001),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = paste0("Construction material composition in ", panel_year),
    subtitle = "Residential and commercial sectors",
    x = NULL,
    y = "Material demand (Gt/yr)",
    fill = "Material"
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_fig

print(fig7_v3)

# -----------------------------
# 10. Save outputs
# -----------------------------

file_stub <- paste0("Fig7_v4_residential_commercial_material_composition_", panel_year)

ggsave(
  filename = file.path(out_dir, paste0(file_stub, ".png")),
  plot = fig7_v3,
  width = 10,
  height = 7.5,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(out_dir, paste0(file_stub, ".tiff")),
  plot = fig7_v3,
  width = 10,
  height = 7.5,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(out_dir, paste0(file_stub, ".pdf")),
  plot = fig7_v3,
  width = 10,
  height = 7.5,
  device = cairo_pdf,
  bg = "white"
)

write_csv(
  df_fig7,
  file.path(out_dir, paste0(file_stub, "_data.csv"))
)

cat("\nSaved Fig7_v3 outputs to:\n")
cat(out_dir, "\n")




######## EXTRACT DATA ##################
# =========================================================
# Extract exact Fig. 7 numbers for Results text
# Construction material composition in 2050
# Residential + commercial sectors
# =========================================================

cat("\n\n============================================================\n")
cat("Exact Fig. 7 numbers for Results text\n")
cat("Construction material composition in ", panel_year, "\n", sep = "")
cat("============================================================\n")

# Helper formatting
fmt_num <- function(x, digits = 3) {
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  paste0(format(round(x, digits), nsmall = digits, trim = TRUE), "%")
}

# ── 1) Total material demand by sector and scenario ─────────

fig7_totals <- df_fig7 %>%
  group_by(sector_label, scenario_label) %>%
  summarise(
    total_mat_demand_Gt = sum(mat_demand_Gt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(sector_label, scenario_label)

cat("\nTotal material demand in 2050 by sector and scenario:\n")
cat("Units: Gt/yr\n")

fig7_totals %>%
  mutate(total_mat_demand_Gt = round(total_mat_demand_Gt, 4)) %>%
  print(n = Inf, width = Inf)

# ── 2) Material shares by sector and scenario ───────────────

fig7_material_shares <- df_fig7 %>%
  group_by(sector_label, scenario_label) %>%
  mutate(
    total_mat_demand_Gt = sum(mat_demand_Gt, na.rm = TRUE),
    material_share = 100 * mat_demand_Gt / total_mat_demand_Gt
  ) %>%
  ungroup() %>%
  arrange(sector_label, scenario_label, desc(material_share))

cat("\nMaterial shares in 2050 by sector and scenario:\n")
cat("Units: Gt/yr and % of scenario total\n")

fig7_material_shares %>%
  mutate(
    mat_demand_Gt = round(mat_demand_Gt, 4),
    total_mat_demand_Gt = round(total_mat_demand_Gt, 4),
    material_share = round(material_share, 1)
  ) %>%
  print(n = Inf, width = Inf)

# ── 3) Reference material composition ───────────────────────
# Useful for saying what dominates the material mix.

fig7_reference_composition <- fig7_material_shares %>%
  filter(scenario_label == "Reference") %>%
  select(
    sector_label,
    material,
    mat_demand_Gt,
    material_share
  ) %>%
  arrange(sector_label, desc(material_share))

cat("\nReference material composition in 2050:\n")

fig7_reference_composition %>%
  mutate(
    mat_demand_Gt = round(mat_demand_Gt, 4),
    material_share = round(material_share, 1)
  ) %>%
  print(n = Inf, width = Inf)

cat("\nText-ready Reference composition sentences:\n")

fig7_reference_composition %>%
  group_by(sector_label) %>%
  slice_max(order_by = material_share, n = 4, with_ties = FALSE) %>%
  summarise(
    sentence = paste0(
      sector_label,
      ": top materials under Reference are ",
      paste0(
        as.character(material),
        " (",
        fmt_num(mat_demand_Gt, 3),
        " Gt/yr; ",
        fmt_pct(material_share, 1),
        ")",
        collapse = ", "
      ),
      "."
    ),
    .groups = "drop"
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ── 4) Key materials across scenarios ───────────────────────
# Focus on dominant / policy-relevant materials.

key_materials <- c(
  "concrete", "brick", "cement", "wood",
  "steel", "glass", "aluminum", "copper"
)

fig7_key_materials <- fig7_material_shares %>%
  filter(as.character(material) %in% key_materials) %>%
  select(
    sector_label,
    scenario_label,
    material,
    mat_demand_Gt,
    material_share
  ) %>%
  arrange(sector_label, material, scenario_label)

cat("\nKey material demand and shares across scenarios:\n")

fig7_key_materials %>%
  mutate(
    mat_demand_Gt = round(mat_demand_Gt, 4),
    material_share = round(material_share, 1)
  ) %>%
  print(n = Inf, width = Inf)

# ── 5) Changes in material demand relative to Reference ─────

fig7_reference_by_material <- fig7_material_shares %>%
  filter(scenario_label == "Reference") %>%
  select(
    sector_label,
    material,
    ref_mat_demand_Gt = mat_demand_Gt,
    ref_material_share = material_share
  )

fig7_vs_reference_by_material <- fig7_material_shares %>%
  filter(scenario_label != "Reference") %>%
  left_join(
    fig7_reference_by_material,
    by = c("sector_label", "material")
  ) %>%
  mutate(
    abs_change_Gt = mat_demand_Gt - ref_mat_demand_Gt,
    abs_reduction_Gt = ref_mat_demand_Gt - mat_demand_Gt,
    pct_reduction = 100 * abs_reduction_Gt / ref_mat_demand_Gt,
    share_change_pp = material_share - ref_material_share
  ) %>%
  arrange(sector_label, scenario_label, material)

cat("\nMaterial-specific changes relative to Reference:\n")
cat("Positive pct_reduction means lower demand than Reference.\n")
cat("share_change_pp is percentage-point change in material share.\n")

fig7_vs_reference_by_material %>%
  mutate(
    mat_demand_Gt = round(mat_demand_Gt, 4),
    ref_mat_demand_Gt = round(ref_mat_demand_Gt, 4),
    abs_reduction_Gt = round(abs_reduction_Gt, 4),
    pct_reduction = round(pct_reduction, 1),
    material_share = round(material_share, 1),
    ref_material_share = round(ref_material_share, 1),
    share_change_pp = round(share_change_pp, 1)
  ) %>%
  print(n = Inf, width = Inf)

# ── 6) Concrete + brick + cement dominance ──────────────────
# Useful for compact text on dominant mineral materials.

mineral_materials <- c("concrete", "brick", "cement")

fig7_mineral_share <- fig7_material_shares %>%
  filter(as.character(material) %in% mineral_materials) %>%
  group_by(sector_label, scenario_label) %>%
  summarise(
    mineral_demand_Gt = sum(mat_demand_Gt, na.rm = TRUE),
    total_mat_demand_Gt = first(total_mat_demand_Gt),
    mineral_share = 100 * mineral_demand_Gt / total_mat_demand_Gt,
    .groups = "drop"
  ) %>%
  arrange(sector_label, scenario_label)

cat("\nConcrete + brick + cement share of total material demand:\n")

fig7_mineral_share %>%
  mutate(
    mineral_demand_Gt = round(mineral_demand_Gt, 4),
    total_mat_demand_Gt = round(total_mat_demand_Gt, 4),
    mineral_share = round(mineral_share, 1)
  ) %>%
  print(n = Inf, width = Inf)

# ── 7) Wood demand and wood share ───────────────────────────
# Useful for checking whether Narrow / Combined increase wood share.

fig7_wood <- fig7_material_shares %>%
  filter(material == "wood") %>%
  select(
    sector_label,
    scenario_label,
    wood_demand_Gt = mat_demand_Gt,
    wood_share = material_share
  ) %>%
  left_join(
    fig7_reference_by_material %>%
      filter(material == "wood") %>%
      select(
        sector_label,
        ref_wood_demand_Gt = ref_mat_demand_Gt,
        ref_wood_share = ref_material_share
      ),
    by = "sector_label"
  ) %>%
  mutate(
    wood_abs_change_Gt = wood_demand_Gt - ref_wood_demand_Gt,
    wood_pct_change = 100 * wood_abs_change_Gt / ref_wood_demand_Gt,
    wood_share_change_pp = wood_share - ref_wood_share
  ) %>%
  arrange(sector_label, scenario_label)

cat("\nWood demand and share in 2050:\n")

fig7_wood %>%
  mutate(
    wood_demand_Gt = round(wood_demand_Gt, 4),
    wood_share = round(wood_share, 1),
    ref_wood_demand_Gt = round(ref_wood_demand_Gt, 4),
    ref_wood_share = round(ref_wood_share, 1),
    wood_abs_change_Gt = round(wood_abs_change_Gt, 4),
    wood_pct_change = round(wood_pct_change, 1),
    wood_share_change_pp = round(wood_share_change_pp, 1)
  ) %>%
  print(n = Inf, width = Inf)

# ── 8) Scenario ranking by total material demand ────────────

fig7_ranked_totals <- fig7_totals %>%
  group_by(sector_label) %>%
  arrange(total_mat_demand_Gt, .by_group = TRUE) %>%
  mutate(rank_lowest_demand = row_number()) %>%
  ungroup()

cat("\nScenario ranking by lowest total material demand in 2050:\n")

fig7_ranked_totals %>%
  mutate(total_mat_demand_Gt = round(total_mat_demand_Gt, 4)) %>%
  print(n = Inf, width = Inf)

# ── 9) Text-ready summary lines for dominant materials ──────

cat("\nText-ready dominant-material summary lines:\n")

fig7_reference_composition %>%
  group_by(sector_label) %>%
  slice_max(order_by = material_share, n = 4, with_ties = FALSE) %>%
  summarise(
    top_materials = paste0(
      as.character(material),
      " (",
      fmt_num(mat_demand_Gt, 3),
      " Gt/yr; ",
      fmt_pct(material_share, 1),
      ")",
      collapse = ", "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    sentence = paste0(
      sector_label,
      ": top materials under Reference are ",
      top_materials,
      "."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ── 10) Save text-supporting tables ─────────────────────────

write_csv(
  fig7_totals,
  file.path(out_dir, paste0(file_stub, "_totals_for_text.csv"))
)

write_csv(
  fig7_material_shares,
  file.path(out_dir, paste0(file_stub, "_material_shares_for_text.csv"))
)

write_csv(
  fig7_reference_composition,
  file.path(out_dir, paste0(file_stub, "_reference_composition_for_text.csv"))
)

write_csv(
  fig7_vs_reference_by_material,
  file.path(out_dir, paste0(file_stub, "_material_changes_vs_reference_for_text.csv"))
)

write_csv(
  fig7_mineral_share,
  file.path(out_dir, paste0(file_stub, "_mineral_share_for_text.csv"))
)

write_csv(
  fig7_wood,
  file.path(out_dir, paste0(file_stub, "_wood_for_text.csv"))
)

write_csv(
  fig7_ranked_totals,
  file.path(out_dir, paste0(file_stub, "_ranked_totals_for_text.csv"))
)

cat("\nSaved Fig. 7 text-supporting tables to:\n")
cat(out_dir, "\n")