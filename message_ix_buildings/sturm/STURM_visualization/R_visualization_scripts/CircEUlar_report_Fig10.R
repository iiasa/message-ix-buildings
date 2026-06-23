# ============================================================
# Fig10_v7
# EU-27 residential and commercial space heating and cooling demand
# by energy carrier under realistic scenarios
#
# Layout:
#   rows    = Residential / Commercial
#   columns = 2025 / 2050 / 2100
#
# Scenarios:
#   R, C_r grouped as Ref / Close
#   N_r shown as Narrow
#   S_r shown as Slow
#   A_r shown as Combined
#   E_r shown as Efficiency
#
# Notes:
#   - 2025 shows only the common baseline.
#   - Reference and Close are grouped because they are identical
#     for heating/cooling demand and carrier mix.
#   - Residential and commercial have separate y-axis ranges.
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
# 2. Scenario settings
# ----------------------------
scenario_table <- tribble(
  ~sector, ~scenario,
  "resid", "R",
  "resid", "N_r",
  "resid", "S_r",
  "resid", "C_r",
  "resid", "A_r",
  "resid", "E_r",
  
  "comm", "R",
  "comm", "N_r",
  "comm", "S_r",
  "comm", "C_r",
  "comm", "A_r",
  "comm", "E_r"
)

sector_labels <- c(
  "resid" = "Residential",
  "comm"  = "Commercial"
)

scenario_labels <- c(
  "R"    = "Reference",
  "N_r"  = "Narrow R",
  "S_r"  = "Slow R",
  "C_r"  = "Close R",
  "A_r"  = "Combined R",
  "E_r"  = "Energy efficiency R"
)

selected_years <- c(2025, 2050, 2100)

# Plot order within each year
# Empty label is used only for the 2025 common baseline.
plot_order <- c(
  "",
  "Ref / Close",
  "Narrow",
  "Slow",
  "Combined",
  "Efficiency"
)

# ----------------------------
# 3. Energy carrier settings
# ----------------------------
carrier_order <- c(
  "Biomass",
  "Coal",
  "District heat",
  "Electricity",
  "Gas",
  "Oil"
)

carrier_colours <- c(
  "Biomass"       = "#253494",
  "Coal"          = "#2C7FB8",
  "District heat" = "#41B6C4",
  "Electricity"   = "#7FCDBB",
  "Gas"           = "#C7E9B4",
  "Oil"           = "#FFFFCC"
)

clean_carrier <- function(x) {
  x <- as.character(x)
  
  case_when(
    is.na(x) ~ "No heating",
    x %in% c("v_no_heat", "no_heat", "none", "NA") ~ "No heating",
    x %in% c("biomass", "biomass_solid", "solid_biomass") ~ "Biomass",
    x %in% c("coal") ~ "Coal",
    x %in% c("district_heat", "district heating", "district_heating") ~ "District heat",
    x %in% c("electricity", "elec") ~ "Electricity",
    x %in% c("gas", "natural_gas") ~ "Gas",
    x %in% c("oil", "liquids") ~ "Oil",
    TRUE ~ str_to_sentence(str_replace_all(x, "_", " "))
  )
}

# ----------------------------
# 4. Helper: find energy report file
# ----------------------------
find_energy_file <- function(sc, sector_code, input_dir) {
  
  pattern <- paste0(
    "^report_STURM_",
    sc,
    "_",
    sector_code,
    "_.*_energy\\.csv$"
  )
  
  files <- list.files(
    input_dir,
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    warning(
      "No energy file found for scenario = ",
      sc,
      ", sector = ",
      sector_code,
      "\nPattern used: ",
      pattern
    )
    return(NA_character_)
  }
  
  # Prefer region_bld if available; otherwise use R12 if available.
  region_bld_file <- files[str_detect(files, "_region_bld_energy\\.csv$")]
  
  if (length(region_bld_file) > 0) {
    return(region_bld_file[1])
  }
  
  r12_file <- files[str_detect(files, "_R12_energy\\.csv$")]
  
  if (length(r12_file) > 0) {
    return(r12_file[1])
  }
  
  files[1]
}

# ----------------------------
# 5. Read energy reports
# ----------------------------
read_energy_report <- function(sc, sector_code) {
  
  f <- find_energy_file(
    sc = sc,
    sector_code = sector_code,
    input_dir = input_dir
  )
  
  if (is.na(f) || !file.exists(f)) {
    return(NULL)
  }
  
  cat("Loaded:", basename(f), "\n")
  
  df <- read_csv(f, show_col_types = FALSE)
  
  required_cols <- c(
    "year",
    "fuel_heat",
    "fuel_cool",
    "heat_TJ",
    "cool_TJ"
  )
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in ",
      basename(f),
      ":\n",
      paste(missing_cols, collapse = "\n"),
      "\nAvailable columns:\n",
      paste(names(df), collapse = ", ")
    )
  }
  
  df %>%
    mutate(
      sector = sector_code,
      sector_label = sector_labels[sector_code],
      scenario = sc,
      scenario_label = scenario_labels[sc],
      source_file = basename(f)
    )
}

df_raw <- scenario_table %>%
  pmap_dfr(function(sector, scenario) {
    read_energy_report(
      sc = scenario,
      sector_code = sector
    )
  })

if (nrow(df_raw) == 0) {
  stop("No energy files were loaded. Check input_dir and scenario names.")
}

df_raw <- df_raw %>%
  mutate(
    sector_label = factor(
      sector_label,
      levels = c("Residential", "Commercial")
    )
  )

cat("\nLoaded rows by sector and scenario:\n")
print(df_raw %>% count(sector_label, scenario_label), n = Inf)

cat("\nAvailable years:\n")
print(sort(unique(df_raw$year)))

cat("\nLoaded files:\n")
print(df_raw %>% distinct(sector_label, scenario, source_file), n = Inf)

cat("\nUnique fuel_heat values:\n")
print(sort(unique(df_raw$fuel_heat)))

cat("\nUnique fuel_cool values:\n")
print(sort(unique(df_raw$fuel_cool)))

# ----------------------------
# 6. Deduplicate
# ----------------------------
df_energy <- df_raw %>%
  select(
    sector,
    sector_label,
    scenario,
    scenario_label,
    year,
    fuel_heat,
    fuel_cool,
    heat_TJ,
    cool_TJ
  ) %>%
  distinct() %>%
  mutate(
    heat_TJ = replace_na(heat_TJ, 0),
    cool_TJ = replace_na(cool_TJ, 0)
  )

cat("\nRows before deduplication:", nrow(df_raw), "\n")
cat("Rows after deduplication: ", nrow(df_energy), "\n")

# ----------------------------
# 7. Create carrier-level heating and cooling demand
# ----------------------------

# Heating demand by heating carrier
df_heat_carrier <- df_energy %>%
  transmute(
    sector,
    sector_label,
    scenario,
    scenario_label,
    year,
    carrier_raw = fuel_heat,
    carrier = clean_carrier(fuel_heat),
    end_use = "Space heating",
    energy_TJ = heat_TJ
  )

# Cooling demand by cooling carrier
df_cool_carrier <- df_energy %>%
  transmute(
    sector,
    sector_label,
    scenario,
    scenario_label,
    year,
    carrier_raw = fuel_cool,
    carrier = clean_carrier(fuel_cool),
    end_use = "Space cooling",
    energy_TJ = cool_TJ
  )

df_carrier_long <- bind_rows(
  df_heat_carrier,
  df_cool_carrier
) %>%
  filter(year %in% selected_years) %>%
  filter(carrier != "No heating") %>%
  mutate(
    carrier = factor(carrier, levels = carrier_order)
  )

cat("\nCarrier mapping check:\n")
df_carrier_long %>%
  distinct(carrier_raw, carrier) %>%
  arrange(carrier_raw) %>%
  print(n = Inf)

# ----------------------------
# 8. Aggregate by sector, scenario, year, and carrier
# ----------------------------
df_carrier <- df_carrier_long %>%
  group_by(
    sector,
    sector_label,
    year,
    scenario,
    scenario_label,
    carrier
  ) %>%
  summarise(
    energy_TJ = sum(energy_TJ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    energy_EJ = energy_TJ / 1e6
  )

# ----------------------------
# 9. Group scenarios for plotting
# ----------------------------
# Reference and Close are grouped because they are identical
# for heating/cooling demand and carrier mix.
#
# Use mean() when grouping R and C_r to avoid double-counting.
# This is safe because the verification check confirmed that
# Reference and Close are identical by carrier.

df_fig10 <- df_carrier %>%
  mutate(
    scenario_group = case_when(
      scenario %in% c("R", "C_r") ~ "Ref / Close",
      scenario == "N_r" ~ "Narrow",
      scenario == "S_r" ~ "Slow",
      scenario == "A_r" ~ "Combined",
      scenario == "E_r" ~ "Efficiency",
      TRUE ~ as.character(scenario_label)
    )
  ) %>%
  group_by(
    sector,
    sector_label,
    year,
    scenario_group,
    carrier
  ) %>%
  summarise(
    energy_EJ = mean(energy_EJ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    x_label = case_when(
      year == 2025 & scenario_group == "Ref / Close" ~ "",
      TRUE ~ scenario_group
    ),
    x_label = factor(x_label, levels = plot_order),
    year = factor(year, levels = selected_years),
    carrier = factor(carrier, levels = carrier_order),
    sector_label = factor(
      sector_label,
      levels = c("Residential", "Commercial")
    )
  ) %>%
  # 2025 should show only the common baseline.
  filter(
    !(year == "2025" & scenario_group != "Ref / Close")
  )

cat("\nGrouped carrier-level energy demand for Fig10:\n")
df_fig10 %>%
  arrange(sector_label, year, x_label, carrier) %>%
  print(n = Inf, width = Inf)

# ----------------------------
# 10. Optional checks
# ----------------------------
cat("\nTotal heating/cooling demand by sector, year, and scenario group:\n")

df_fig10 %>%
  group_by(sector_label, year, x_label) %>%
  summarise(
    total_EJ = sum(energy_EJ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(sector_label, year, x_label) %>%
  print(n = Inf, width = Inf)

cat("\nReference / Close grouped check:\n")

df_carrier %>%
  filter(
    year %in% c(2050, 2100),
    scenario %in% c("R", "C_r")
  ) %>%
  mutate(
    scenario_check = recode(
      scenario,
      "R" = "Reference",
      "C_r" = "Close"
    )
  ) %>%
  group_by(sector_label, year, carrier, scenario_check) %>%
  summarise(
    energy_EJ = sum(energy_EJ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = scenario_check,
    values_from = energy_EJ
  ) %>%
  mutate(
    diff = Close - Reference
  ) %>%
  arrange(sector_label, year, carrier) %>%
  print(n = Inf, width = Inf)

cat("\nEfficiency check:\n")

df_fig10 %>%
  filter(
    year %in% c("2050", "2100"),
    x_label %in% c("Combined", "Efficiency")
  ) %>%
  group_by(sector_label, year, x_label) %>%
  summarise(
    total_EJ = sum(energy_EJ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(sector_label, year, x_label) %>%
  print(n = Inf, width = Inf)

# ----------------------------
# 11. Theme
# ----------------------------
theme_fig <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 10)
    ),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size = 10
    ),
    axis.text.y = element_text(size = 11),
    
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    legend.key.width = unit(1.0, "cm"),
    
    strip.text = element_text(
      face = "bold",
      size = 13
    ),
    strip.text.y.right = element_text(
      face = "bold",
      size = 13,
      angle = 270
    ),
    strip.background = element_blank(),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85"),
    
    panel.spacing.x = unit(1.1, "lines"),
    panel.spacing.y = unit(0.8, "lines"),
    
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 12, 10, 10)
  )

# ----------------------------
# 12. Plot figure
# ----------------------------
fig10_v7 <- ggplot(
  df_fig10,
  aes(
    x = x_label,
    y = energy_EJ,
    fill = carrier
  )
) +
  geom_col(
    width = 0.72,
    colour = "white",
    linewidth = 0.2
  ) +
  facet_grid(
    rows = vars(sector_label),
    cols = vars(year),
    scales = "free",
    space = "free_x"
  ) +
  scale_fill_manual(
    values = carrier_colours,
    drop = FALSE,
    name = "Energy carrier"
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08)),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    title = "EU-27 space heating and cooling demand by energy carrier",
    subtitle = "Residential and commercial sectors under realistic circular economy and energy-efficiency scenarios",
    x = NULL,
    y = "Heating and cooling demand (EJ/yr)"
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_fig

print(fig10_v7)

# ----------------------------
# 13. Save outputs
# ----------------------------
file_stub <- "Fig10_v7_EU27_residential_commercial_heatcool_by_energy_carrier_ref_close_grouped"

ggsave(
  filename = file.path(output_dir, paste0(file_stub, ".png")),
  plot = fig10_v7,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(output_dir, paste0(file_stub, ".tiff")),
  plot = fig10_v7,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(output_dir, paste0(file_stub, ".pdf")),
  plot = fig10_v7,
  width = 12,
  height = 8,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

# ----------------------------
# 14. Save processed data
# ----------------------------
write_csv(
  df_carrier,
  file.path(output_dir, paste0(file_stub, "_all_scenarios_data.csv"))
)

write_csv(
  df_fig10,
  file.path(output_dir, paste0(file_stub, "_grouped_data.csv"))
)

cat("\nSaved Fig10_v7 and data to:\n", output_dir, "\n")



###################
#### EXTRACT DATA
###################
# ============================================================
# Extract exact Fig. 10 numbers for Results text
# Energy carrier composition of space heating and cooling demand
# Residential + commercial sectors
# ============================================================

cat("\n\n============================================================\n")
cat("Exact Fig. 10 numbers for Results text\n")
cat("Heating and cooling demand by energy carrier\n")
cat("============================================================\n")

# ----------------------------
# Helper formatting
# ----------------------------
fmt_num <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  paste0(format(round(x, digits), nsmall = digits, trim = TRUE), "%")
}

# Ensure year is numeric for calculations
df_fig10_text <- df_fig10 %>%
  mutate(
    year_num = as.numeric(as.character(year)),
    scenario_text = as.character(x_label),
    scenario_text = if_else(scenario_text == "", "2025 baseline", scenario_text),
    carrier = as.character(carrier)
  )

# ============================================================
# 1. Total demand by sector, year, and scenario
# ============================================================

fig10_totals <- df_fig10_text %>%
  group_by(sector_label, year_num, scenario_text) %>%
  summarise(
    total_EJ = sum(energy_EJ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(sector_label, year_num, scenario_text)

cat("\nTotal heating and cooling demand by sector, year, and scenario:\n")
fig10_totals %>%
  mutate(total_EJ = round(total_EJ, 3)) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 2. Carrier-level values and shares
# ============================================================

fig10_carrier_shares <- df_fig10_text %>%
  group_by(sector_label, year_num, scenario_text) %>%
  mutate(
    total_EJ = sum(energy_EJ, na.rm = TRUE),
    share_pct = 100 * energy_EJ / total_EJ
  ) %>%
  ungroup() %>%
  select(
    sector_label,
    year_num,
    scenario_text,
    carrier,
    energy_EJ,
    share_pct,
    total_EJ
  ) %>%
  arrange(sector_label, year_num, scenario_text, carrier)

cat("\nCarrier-level demand and shares:\n")
fig10_carrier_shares %>%
  mutate(
    energy_EJ = round(energy_EJ, 3),
    share_pct = round(share_pct, 1),
    total_EJ = round(total_EJ, 3)
  ) %>%
  print(n = Inf, width = Inf)

# Wide format: easier to read for results writing
fig10_carrier_wide <- fig10_carrier_shares %>%
  select(sector_label, year_num, scenario_text, carrier, energy_EJ) %>%
  pivot_wider(
    names_from = carrier,
    values_from = energy_EJ,
    values_fill = 0
  ) %>%
  arrange(sector_label, year_num, scenario_text)

cat("\nCarrier-level demand, wide format:\n")
fig10_carrier_wide %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

fig10_carrier_share_wide <- fig10_carrier_shares %>%
  select(sector_label, year_num, scenario_text, carrier, share_pct) %>%
  pivot_wider(
    names_from = carrier,
    values_from = share_pct,
    values_fill = 0
  ) %>%
  arrange(sector_label, year_num, scenario_text)

cat("\nCarrier shares, wide format:\n")
fig10_carrier_share_wide %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 3. Fossil vs electricity/district heat/biomass groupings
# ============================================================

fig10_carrier_groups <- df_fig10_text %>%
  mutate(
    carrier_group = case_when(
      carrier %in% c("Coal", "Gas", "Oil") ~ "Fossil carriers",
      carrier %in% c("Electricity", "District heat") ~ "Electricity + district heat",
      carrier == "Biomass" ~ "Biomass",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(sector_label, year_num, scenario_text, carrier_group) %>%
  summarise(
    energy_EJ = sum(energy_EJ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(sector_label, year_num, scenario_text) %>%
  mutate(
    total_EJ = sum(energy_EJ, na.rm = TRUE),
    share_pct = 100 * energy_EJ / total_EJ
  ) %>%
  ungroup() %>%
  arrange(sector_label, year_num, scenario_text, carrier_group)

cat("\nGrouped carrier demand and shares:\n")
fig10_carrier_groups %>%
  mutate(
    energy_EJ = round(energy_EJ, 3),
    total_EJ = round(total_EJ, 3),
    share_pct = round(share_pct, 1)
  ) %>%
  print(n = Inf, width = Inf)

fig10_carrier_groups_wide <- fig10_carrier_groups %>%
  select(sector_label, year_num, scenario_text, carrier_group, energy_EJ) %>%
  pivot_wider(
    names_from = carrier_group,
    values_from = energy_EJ,
    values_fill = 0
  ) %>%
  arrange(sector_label, year_num, scenario_text)

cat("\nGrouped carrier demand, wide format:\n")
fig10_carrier_groups_wide %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

fig10_carrier_groups_share_wide <- fig10_carrier_groups %>%
  select(sector_label, year_num, scenario_text, carrier_group, share_pct) %>%
  pivot_wider(
    names_from = carrier_group,
    values_from = share_pct,
    values_fill = 0
  ) %>%
  arrange(sector_label, year_num, scenario_text)

cat("\nGrouped carrier shares, wide format:\n")
fig10_carrier_groups_share_wide %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 4. Baseline 2025 carrier composition
# ============================================================

fig10_2025_baseline <- fig10_carrier_shares %>%
  filter(year_num == 2025, scenario_text == "2025 baseline") %>%
  arrange(sector_label, desc(energy_EJ))

cat("\n2025 baseline carrier composition:\n")
fig10_2025_baseline %>%
  mutate(
    energy_EJ = round(energy_EJ, 3),
    share_pct = round(share_pct, 1),
    total_EJ = round(total_EJ, 3)
  ) %>%
  print(n = Inf, width = Inf)

cat("\nText-ready 2025 baseline carrier composition:\n")
fig10_2025_baseline %>%
  group_by(sector_label) %>%
  summarise(
    total_EJ = first(total_EJ),
    top_carriers = paste0(
      carrier[1:min(3, n())],
      " = ",
      fmt_num(energy_EJ[1:min(3, n())], 2),
      " EJ/yr (",
      fmt_pct(share_pct[1:min(3, n())], 1),
      ")",
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    sentence = paste0(
      sector_label,
      ": total demand is ",
      fmt_num(total_EJ, 2),
      " EJ/yr in 2025; the largest carriers are ",
      top_carriers,
      "."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ============================================================
# 5. Change in carrier groups over time in Reference / Close
# ============================================================

fig10_ref_group_change <- fig10_carrier_groups %>%
  filter(
    scenario_text %in% c("2025 baseline", "Ref / Close"),
    year_num %in% c(2025, 2050, 2100)
  ) %>%
  select(
    sector_label,
    year_num,
    carrier_group,
    energy_EJ,
    share_pct
  ) %>%
  pivot_wider(
    names_from = year_num,
    values_from = c(energy_EJ, share_pct),
    names_prefix = "y"
  ) %>%
  mutate(
    change_EJ_2025_2050 = energy_EJ_y2050 - energy_EJ_y2025,
    change_EJ_2025_2100 = energy_EJ_y2100 - energy_EJ_y2025,
    pct_change_2025_2050 = 100 * (energy_EJ_y2050 / energy_EJ_y2025 - 1),
    pct_change_2025_2100 = 100 * (energy_EJ_y2100 / energy_EJ_y2025 - 1),
    share_change_pp_2025_2050 = share_pct_y2050 - share_pct_y2025,
    share_change_pp_2025_2100 = share_pct_y2100 - share_pct_y2025
  ) %>%
  arrange(sector_label, carrier_group)

cat("\nReference / Close carrier-group change over time:\n")
fig10_ref_group_change %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 6. Change in individual carriers over time in Reference / Close
# ============================================================

fig10_ref_carrier_change <- fig10_carrier_shares %>%
  filter(
    scenario_text %in% c("2025 baseline", "Ref / Close"),
    year_num %in% c(2025, 2050, 2100)
  ) %>%
  select(
    sector_label,
    year_num,
    carrier,
    energy_EJ,
    share_pct
  ) %>%
  pivot_wider(
    names_from = year_num,
    values_from = c(energy_EJ, share_pct),
    names_prefix = "y"
  ) %>%
  mutate(
    change_EJ_2025_2050 = energy_EJ_y2050 - energy_EJ_y2025,
    change_EJ_2025_2100 = energy_EJ_y2100 - energy_EJ_y2025,
    pct_change_2025_2050 = 100 * (energy_EJ_y2050 / energy_EJ_y2025 - 1),
    pct_change_2025_2100 = 100 * (energy_EJ_y2100 / energy_EJ_y2025 - 1),
    share_change_pp_2025_2050 = share_pct_y2050 - share_pct_y2025,
    share_change_pp_2025_2100 = share_pct_y2100 - share_pct_y2025
  ) %>%
  arrange(sector_label, carrier)

cat("\nReference / Close individual carrier change over time:\n")
fig10_ref_carrier_change %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 7. Scenario differences relative to Reference / Close
# ============================================================

fig10_ref_totals <- fig10_totals %>%
  filter(
    (year_num == 2025 & scenario_text == "2025 baseline") |
      (year_num %in% c(2050, 2100) & scenario_text == "Ref / Close")
  ) %>%
  select(
    sector_label,
    year_num,
    ref_total_EJ = total_EJ
  )

fig10_total_vs_ref <- fig10_totals %>%
  filter(year_num %in% c(2050, 2100)) %>%
  filter(scenario_text != "2025 baseline") %>%
  left_join(
    fig10_ref_totals,
    by = c("sector_label", "year_num")
  ) %>%
  mutate(
    reduction_EJ = ref_total_EJ - total_EJ,
    reduction_pct = 100 * reduction_EJ / ref_total_EJ
  ) %>%
  arrange(sector_label, year_num, scenario_text)

cat("\nTotal demand difference relative to Reference / Close:\n")
fig10_total_vs_ref %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

fig10_carrier_ref <- fig10_carrier_shares %>%
  filter(year_num %in% c(2050, 2100), scenario_text == "Ref / Close") %>%
  select(
    sector_label,
    year_num,
    carrier,
    ref_energy_EJ = energy_EJ,
    ref_share_pct = share_pct
  )

fig10_carrier_vs_ref <- fig10_carrier_shares %>%
  filter(year_num %in% c(2050, 2100), scenario_text != "2025 baseline") %>%
  left_join(
    fig10_carrier_ref,
    by = c("sector_label", "year_num", "carrier")
  ) %>%
  mutate(
    change_EJ = energy_EJ - ref_energy_EJ,
    change_pct = 100 * change_EJ / ref_energy_EJ,
    share_change_pp = share_pct - ref_share_pct
  ) %>%
  arrange(sector_label, year_num, scenario_text, carrier)

cat("\nCarrier-level difference relative to Reference / Close:\n")
fig10_carrier_vs_ref %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 8. Efficiency compared with Ref / Close and best CE scenario
# ============================================================

fig10_best_ce <- fig10_total_vs_ref %>%
  filter(scenario_text %in% c("Narrow", "Slow", "Combined")) %>%
  group_by(sector_label, year_num) %>%
  slice_max(order_by = reduction_pct, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    sector_label,
    year_num,
    best_ce_scenario = scenario_text,
    best_ce_total_EJ = total_EJ,
    best_ce_reduction_pct = reduction_pct
  )

fig10_efficiency_gap <- fig10_total_vs_ref %>%
  filter(scenario_text == "Efficiency") %>%
  select(
    sector_label,
    year_num,
    efficiency_total_EJ = total_EJ,
    efficiency_reduction_pct = reduction_pct
  ) %>%
  left_join(
    fig10_best_ce,
    by = c("sector_label", "year_num")
  ) %>%
  mutate(
    efficiency_minus_best_ce_EJ =
      efficiency_total_EJ - best_ce_total_EJ,
    extra_reduction_pct_points =
      efficiency_reduction_pct - best_ce_reduction_pct
  ) %>%
  arrange(sector_label, year_num)

cat("\nEfficiency compared with best circular economy scenario:\n")
cat("Negative EJ values mean Efficiency has lower demand than best CE.\n")
fig10_efficiency_gap %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 9. Heating carrier vs cooling carrier contribution
# Optional but useful if we want to explain electricity shares
# ============================================================

fig10_end_use_carrier <- df_carrier_long %>%
  mutate(
    year_num = as.numeric(as.character(year)),
    carrier = as.character(carrier),
    scenario_group = case_when(
      scenario %in% c("R", "C_r") ~ "Ref / Close",
      scenario == "N_r" ~ "Narrow",
      scenario == "S_r" ~ "Slow",
      scenario == "A_r" ~ "Combined",
      scenario == "E_r" ~ "Efficiency",
      TRUE ~ as.character(scenario_label)
    ),
    scenario_text = case_when(
      year_num == 2025 & scenario_group == "Ref / Close" ~ "2025 baseline",
      TRUE ~ scenario_group
    )
  ) %>%
  filter(
    !(year_num == 2025 & scenario_group != "Ref / Close")
  ) %>%
  group_by(
    sector_label,
    year_num,
    scenario_text,
    end_use,
    carrier
  ) %>%
  summarise(
    energy_EJ = sum(energy_TJ, na.rm = TRUE) / 1e6,
    .groups = "drop"
  ) %>%
  arrange(sector_label, year_num, scenario_text, end_use, carrier)

cat("\nEnd-use and carrier contribution:\n")
fig10_end_use_carrier %>%
  mutate(energy_EJ = round(energy_EJ, 3)) %>%
  print(n = Inf, width = Inf)

# ============================================================
# 10. Text-ready key sentences
# ============================================================

cat("\nText-ready fossil vs electricity/district heat sentences for Reference / Close:\n")

fig10_ref_group_change %>%
  filter(carrier_group %in% c("Fossil carriers", "Electricity + district heat")) %>%
  arrange(sector_label, carrier_group) %>%
  rowwise() %>%
  mutate(
    sentence = paste0(
      sector_label, ", ", carrier_group, ": ",
      fmt_num(energy_EJ_y2025, 2), " EJ/yr in 2025 (",
      fmt_pct(share_pct_y2025, 1), ") to ",
      fmt_num(energy_EJ_y2050, 2), " EJ/yr in 2050 (",
      fmt_pct(share_pct_y2050, 1), ") and ",
      fmt_num(energy_EJ_y2100, 2), " EJ/yr in 2100 (",
      fmt_pct(share_pct_y2100, 1), ")."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

cat("\nText-ready scenario total demand sentences for 2050:\n")

fig10_total_vs_ref %>%
  filter(year_num == 2050) %>%
  rowwise() %>%
  mutate(
    sentence = paste0(
      sector_label, ", ", scenario_text, ": ",
      fmt_num(total_EJ, 2), " EJ/yr in 2050, ",
      fmt_pct(reduction_pct, 1),
      " relative to Ref / Close."
    )
  ) %>%
  pull(sentence) %>%
  walk(~ cat("\n", .x, "\n", sep = ""))

# ============================================================
# 11. Save text-supporting tables
# ============================================================

write_csv(
  fig10_totals,
  file.path(output_dir, paste0(file_stub, "_text_totals.csv"))
)

write_csv(
  fig10_carrier_shares,
  file.path(output_dir, paste0(file_stub, "_text_carrier_shares.csv"))
)

write_csv(
  fig10_carrier_wide,
  file.path(output_dir, paste0(file_stub, "_text_carrier_wide.csv"))
)

write_csv(
  fig10_carrier_share_wide,
  file.path(output_dir, paste0(file_stub, "_text_carrier_share_wide.csv"))
)

write_csv(
  fig10_carrier_groups,
  file.path(output_dir, paste0(file_stub, "_text_carrier_groups.csv"))
)

write_csv(
  fig10_carrier_groups_wide,
  file.path(output_dir, paste0(file_stub, "_text_carrier_groups_wide.csv"))
)

write_csv(
  fig10_carrier_groups_share_wide,
  file.path(output_dir, paste0(file_stub, "_text_carrier_groups_share_wide.csv"))
)

write_csv(
  fig10_2025_baseline,
  file.path(output_dir, paste0(file_stub, "_text_2025_baseline.csv"))
)

write_csv(
  fig10_ref_group_change,
  file.path(output_dir, paste0(file_stub, "_text_ref_group_change.csv"))
)

write_csv(
  fig10_ref_carrier_change,
  file.path(output_dir, paste0(file_stub, "_text_ref_carrier_change.csv"))
)

write_csv(
  fig10_total_vs_ref,
  file.path(output_dir, paste0(file_stub, "_text_total_vs_ref.csv"))
)

write_csv(
  fig10_carrier_vs_ref,
  file.path(output_dir, paste0(file_stub, "_text_carrier_vs_ref.csv"))
)

write_csv(
  fig10_efficiency_gap,
  file.path(output_dir, paste0(file_stub, "_text_efficiency_gap.csv"))
)

write_csv(
  fig10_end_use_carrier,
  file.path(output_dir, paste0(file_stub, "_text_end_use_carrier.csv"))
)

cat("\nSaved Fig. 10 text-supporting tables to:\n")
cat(output_dir, "\n")
