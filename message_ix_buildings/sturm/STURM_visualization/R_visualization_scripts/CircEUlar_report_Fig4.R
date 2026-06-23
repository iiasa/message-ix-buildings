# ============================================================
# Fig. 4. EU-27 floor-space composition by stock category
# Residential + commercial under Slow scenarios
# ============================================================

library(tidyverse)
library(readr)
library(scales)
library(grid)

# ── User settings ───────────────────────────────────────────

output_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/output"

plot_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

years_to_plot <- c(seq(2025, 2060, 5), seq(2070, 2100, 10))

sectors <- c("resid", "comm")

scenario_levels <- c("R", "S_r", "S_tp")

scenario_labels <- c(
  "R"    = "Reference",
  "S_r"  = "Slow R",
  "S_tp" = "Slow TP"
)

sector_labels <- c(
  "resid" = "Residential",
  "comm"  = "Commercial"
)

# ── Helper: find material report file ───────────────────────
# Residential outputs are usually region_bld.
# Commercial outputs may be R12.
# This searches flexibly.

find_material_file <- function(scenario_code, sector_code, output_dir) {
  
  pattern <- paste0(
    "^report_STURM_",
    scenario_code,
    "_",
    sector_code,
    "_.*_material\\.csv$"
  )
  
  files <- list.files(
    output_dir,
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
  
  # Prefer region_bld if available; otherwise use first match, e.g. R12
  region_bld_file <- files[str_detect(files, "_region_bld_material\\.csv$")]
  
  if (length(region_bld_file) > 0) {
    return(region_bld_file[1])
  }
  
  files[1]
}

# ── Helper: classify stock category ─────────────────────────

classify_stock_group <- function(eneff) {
  case_when(
    str_detect(eneff, "^sr") ~ "Renovated floor space",
    eneff %in% c("s51_std", "s52_low", "s101_std", "s102_low") ~ "New floor space",
    str_detect(eneff, "^s[0-9]+") ~ "Existing floor space",
    TRUE ~ "Other"
  )
}

# ── Helper: read and prepare one scenario-sector file ───────

read_stock_comp_data <- function(scenario_code,
                                 sector_code,
                                 output_dir) {
  
  file_path <- find_material_file(
    scenario_code = scenario_code,
    sector_code   = sector_code,
    output_dir    = output_dir
  )
  
  cat("Loaded:", basename(file_path), "\n")
  
  df <- read_csv(file_path, show_col_types = FALSE)
  
  required_cols <- c("year", "floor_tot_Mm2")
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in ",
      basename(file_path),
      ": ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (!"eneff" %in% names(df)) {
    stop(
      "Column 'eneff' is needed to classify existing / renovated / new stock, but was not found in ",
      basename(file_path)
    )
  }
  
  # Material reports repeat floor-space values across material rows.
  # For stock composition, floor space should be counted once per stock segment.
  # Therefore, 'mat' is intentionally NOT used as a deduplication key.
  dedup_cols <- intersect(
    c(
      "year",
      "region_bld",
      "region_gea",
      "urt",
      "clim",
      "inc_cl",
      "arch",
      "eneff",
      "floor_tot_Mm2"
    ),
    names(df)
  )
  
  df %>%
    filter(year %in% years_to_plot) %>%
    distinct(across(all_of(dedup_cols))) %>%
    mutate(
      stock_group = classify_stock_group(eneff),
      scenario = scenario_code,
      scenario_label = scenario_labels[scenario_code],
      sector = sector_code,
      sector_label = sector_labels[sector_code]
    ) %>%
    filter(stock_group != "Other")
}

# ── Load all scenario-sector data ───────────────────────────

df_floor_segments <- crossing(
  scenario = scenario_levels,
  sector = sectors
) %>%
  pmap_dfr(function(scenario, sector) {
    read_stock_comp_data(
      scenario_code = scenario,
      sector_code = sector,
      output_dir = output_dir
    )
  }) %>%
  mutate(
    scenario_label = factor(
      scenario_label,
      levels = c("Reference", "Slow R", "Slow TP")
    ),
    sector_label = factor(
      sector_label,
      levels = c("Residential", "Commercial")
    )
  )

cat("\nRows after deduplication and classification:\n")
print(df_floor_segments %>% count(sector_label, scenario_label, stock_group))

# ── Prepare stock composition shares ────────────────────────

df_stock_comp <- df_floor_segments %>%
  group_by(sector_label, scenario_label, year, stock_group) %>%
  summarise(
    floor_tot_Mm2 = sum(floor_tot_Mm2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(sector_label, scenario_label, year) %>%
  mutate(
    total_floor_Mm2 = sum(floor_tot_Mm2, na.rm = TRUE),
    share = floor_tot_Mm2 / total_floor_Mm2 * 100,
    floor_tot_bm2 = floor_tot_Mm2 / 1000
  ) %>%
  ungroup() %>%
  mutate(
    # For geom_area stacking:
    # Last level appears at the bottom.
    # This places Existing at the bottom, Renovated in the middle, New on top.
    stock_group = factor(
      stock_group,
      levels = c(
        "New floor space",
        "Renovated floor space",
        "Existing floor space"
      )
    )
  )

# ── Share check ─────────────────────────────────────────────

cat("\nShare check. Rows shown only if shares do not sum to 100:\n")
print(
  df_stock_comp %>%
    group_by(sector_label, scenario_label, year) %>%
    summarise(
      sum_share = sum(share),
      n_groups = n_distinct(stock_group),
      .groups = "drop"
    ) %>%
    filter(abs(sum_share - 100) > 1e-6),
  n = Inf
)

cat("\nSelected-year stock composition summary:\n")
df_stock_comp %>%
  filter(year %in% c(2025, 2050, 2100)) %>%
  select(sector_label, scenario_label, year, stock_group, share) %>%
  mutate(stock_group = as.character(stock_group)) %>%
  pivot_wider(
    names_from = stock_group,
    values_from = share
  ) %>%
  arrange(sector_label, scenario_label, year) %>%
  print(n = Inf, width = Inf)

# ── Plot settings ───────────────────────────────────────────

stock_group_colours <- c(
  "Existing floor space" = "#B3A700",
  "Renovated floor space" = "#1FB5A9",
  "New floor space" = "#F1736A"
)

theme_fig <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 20,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 13,
      margin = margin(b = 10)
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.width = unit(1.3, "cm"),
    
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y.right = element_text(
      face = "bold",
      size = 13,
      angle = 270
    ),
    strip.background = element_blank(),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85"),
    panel.grid.major.y = element_line(colour = "grey85"),
    panel.spacing.x = unit(1.0, "lines"),
    panel.spacing.y = unit(1.0, "lines"),
    
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 12, 10, 10)
  )

# ── Plot ────────────────────────────────────────────────────

p_fig4 <- ggplot(
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
  facet_grid(
    rows = vars(sector_label),
    cols = vars(scenario_label)
  ) +
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
    title = "EU-27 floor-space composition by stock category",
    subtitle = "Dashed vertical line marks 2050",
    x = NULL,
    y = "Share of floor space",
    fill = "Stock category"
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_fig

print(p_fig4)

# ── Save outputs ────────────────────────────────────────────

file_stub <- "Fig4_v4_EU27_residential_commercial_stock_category_composition"

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".png")),
  plot = p_fig4,
  width = 11,
  height = 7.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".tiff")),
  plot = p_fig4,
  width = 11,
  height = 7.5,
  units = "in",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".pdf")),
  plot = p_fig4,
  width = 11,
  height = 7.5,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

write_csv(
  df_stock_comp,
  file.path(plot_dir, paste0(file_stub, "_data.csv"))
)

cat("\nSaved Fig. 4 outputs to:\n")
cat(plot_dir, "\n")