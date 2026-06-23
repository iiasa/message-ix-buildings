# ============================================================
# SI Figure: EU-focused material EOL treatment shares
# Reference vs Close R vs Close TP
#
# Data structure:
#   region_gea, material, year, eol_treat, value
#
# Output:
#   Fig_SI_EU_material_EOL_treatment_shares_by_scenario.png/tiff/pdf
#   Fig_SI_EU_material_EOL_treatment_shares_by_scenario_data.csv
# ============================================================

library(tidyverse)
library(readr)
library(scales)
library(grid)

# ============================================================
# 1. Paths
# ============================================================

sturm_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm"

input_dir <- file.path(sturm_dir, "data", "input_csv_SSP_2023_resid")
output_dir <- file.path(sturm_dir, "visualization")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

files <- tibble(
  scenario = c("Reference", "Close R", "Close TP"),
  file = c(
    file.path(input_dir, "shr_mat_eol_long_SSP2_26_may.csv"),
    file.path(input_dir, "shr_mat_eol_long_CloseR_26_may.csv"),
    file.path(input_dir, "shr_mat_eol_long_CloseTP_26_may.csv")
  )
)

missing_files <- files %>%
  filter(!file.exists(file))

if (nrow(missing_files) > 0) {
  stop(
    "Missing input files:\n",
    paste(missing_files$file, collapse = "\n")
  )
}

# ============================================================
# 2. Read data
# ============================================================

eol_all <- files %>%
  mutate(data = map(file, read_csv, show_col_types = FALSE)) %>%
  select(scenario, data) %>%
  unnest(data) %>%
  mutate(
    region_gea = as.character(region_gea),
    material = as.character(material),
    year = as.integer(year),
    eol_treat = as.character(eol_treat),
    value = as.numeric(value)
  )

cat("\nAvailable regions:\n")
print(sort(unique(eol_all$region_gea)))

cat("\nAvailable materials:\n")
print(sort(unique(eol_all$material)))

cat("\nAvailable EOL treatments:\n")
print(sort(unique(eol_all$eol_treat)))

cat("\nAvailable years:\n")
print(sort(unique(eol_all$year)))

# ============================================================
# 3. EU-focused filtering
# ============================================================
# EU-27 is represented mainly through WEU and EEU in this regional structure.
# We average WEU and EEU for an EU-focused representation of assumptions.

eu_regions <- c("WEU", "EEU")

eol_eu <- eol_all %>%
  filter(region_gea %in% eu_regions)

if (nrow(eol_eu) == 0) {
  stop("No WEU/EEU rows found. Check available region_gea values above.")
}

cat("\nUsing EU-focused regions:\n")
print(sort(unique(eol_eu$region_gea)))

# ============================================================
# 4. Clean labels and aggregate
# ============================================================

material_order <- c(
  "aluminum", "brick", "concrete", "copper", "glass", "steel", "wood"
)

material_labels <- c(
  "aluminum" = "Aluminium",
  "brick" = "Brick",
  "concrete" = "Concrete",
  "copper" = "Copper",
  "glass" = "Glass",
  "steel" = "Steel",
  "wood" = "Wood"
)

eol_order <- c("reuse", "recycling", "downcycling", "others")

eol_labels <- c(
  "reuse" = "Reuse",
  "recycling" = "Recycling",
  "downcycling" = "Downcycling",
  "others" = "Other"
)

eol_plot_data <- eol_eu %>%
  filter(
    material %in% material_order,
    eol_treat %in% eol_order
  ) %>%
  group_by(scenario, material, year, eol_treat) %>%
  summarise(
    share = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = c("Reference", "Close R", "Close TP")
    ),
    material = factor(
      material,
      levels = material_order,
      labels = material_labels[material_order]
    ),
    eol_treat = factor(
      eol_treat,
      levels = eol_order,
      labels = eol_labels[eol_order]
    )
  )

# ============================================================
# 5. Share-sum check
# ============================================================

cat("\nShare-sum check for selected years:\n")

eol_plot_data %>%
  group_by(scenario, material, year) %>%
  summarise(
    sum_share = sum(share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(year %in% c(2020, 2025, 2050, 2060, 2100)) %>%
  arrange(scenario, material, year) %>%
  print(n = Inf)

write_csv(
  eol_plot_data,
  file.path(output_dir, "Fig_SI_EU_material_EOL_treatment_shares_by_scenario_data.csv")
)

# ============================================================
# 6. Plot
# ============================================================

scenario_cols <- c(
  "Reference" = "#4D4D4D",
  "Close R"   = "#0072B2",
  "Close TP"  = "#D55E00"
)

theme_eol <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10, margin = margin(b = 8)),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 8),
    strip.background = element_rect(fill = "grey90", colour = "grey45"),
    panel.border = element_rect(fill = NA, colour = "grey55", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(8, 12, 8, 8)
  )

fig_eol <- ggplot(
  eol_plot_data,
  aes(
    x = year,
    y = share,
    colour = scenario,
    group = scenario
  )
) +
  geom_line(linewidth = 0.9) +
  facet_grid(
    rows = vars(material),
    cols = vars(eol_treat)
  ) +
  scale_colour_manual(
    values = scenario_cols,
    name = "Scenario"
  ) +
  scale_x_continuous(
    breaks = c(2020, 2040, 2060, 2080, 2100)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.8),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    title = "EU-focused end-of-life treatment shares by scenario",
    subtitle = "Residential construction material EOL assumptions; WEU and EEU averaged",
    x = "Year",
    y = "Share of material outflow"
  ) +
  theme_eol

print(fig_eol)

# ============================================================
# 7. Save
# ============================================================

ggsave(
  file.path(output_dir, "Fig_SI_EU_material_EOL_treatment_shares_by_scenario.png"),
  fig_eol,
  width = 11,
  height = 13,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(output_dir, "Fig_SI_EU_material_EOL_treatment_shares_by_scenario.tiff"),
  fig_eol,
  width = 11,
  height = 13,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  file.path(output_dir, "Fig_SI_EU_material_EOL_treatment_shares_by_scenario.pdf"),
  fig_eol,
  width = 11,
  height = 8,
  device = cairo_pdf,
  bg = "white"
)

cat("\nSaved EU-focused EOL SI figure and data to:\n")
cat(output_dir, "\n")