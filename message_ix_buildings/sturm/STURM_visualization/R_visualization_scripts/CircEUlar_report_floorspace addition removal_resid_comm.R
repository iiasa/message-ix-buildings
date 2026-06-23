# ============================================================
# Fig. 3. EU-27 floor-space additions and removals
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

scenario_colours <- c(
  "Reference" = "#4D4D4D",
  "Slow R"    = "#D55E00",
  "Slow TP"   = "#0072B2"
)

flow_linetypes <- c(
  "New construction" = "solid",
  "Demolition"       = "dotted"
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

# ── Helper: read and aggregate one scenario-sector file ─────

read_flow_data <- function(scenario_code,
                           sector_code,
                           output_dir) {
  
  file_path <- find_material_file(
    scenario_code = scenario_code,
    sector_code   = sector_code,
    output_dir    = output_dir
  )
  
  cat("Loaded:", basename(file_path), "\n")
  
  df <- read_csv(file_path, show_col_types = FALSE)
  
  required_cols <- c("year", "floor_new_Mm2", "floor_dem_Mm2")
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in ",
      basename(file_path),
      ": ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Material reports repeat floor-space values across materials.
  # Deduplicate by available stock dimensions.
  dedup_cols <- intersect(
    c(
      "year",
      "region_bld",
      "region_gea",
      "urt",
      "clim",
      "inc_cl",
      "arch",
      "mat",
      "eneff",
      "floor_tot_Mm2",
      "floor_new_Mm2",
      "floor_dem_Mm2"
    ),
    names(df)
  )
  
  df %>%
    filter(year %in% years_to_plot) %>%
    distinct(across(all_of(dedup_cols))) %>%
    group_by(year) %>%
    summarise(
      floor_new_bm2 = sum(floor_new_Mm2, na.rm = TRUE) / 1000,
      floor_dem_bm2 = sum(floor_dem_Mm2, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) %>%
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
      scenario = scenario_code,
      scenario_label = scenario_labels[scenario_code],
      sector = sector_code,
      sector_label = sector_labels[sector_code]
    )
}

# ── Load all data ───────────────────────────────────────────

plot_data <- crossing(
  scenario = scenario_levels,
  sector = sectors
) %>%
  pmap_dfr(function(scenario, sector) {
    read_flow_data(
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
    ),
    flow = factor(
      flow,
      levels = c("New construction", "Demolition")
    )
  )

# ── Sanity check table ──────────────────────────────────────

cat("\nSelected-year floor-space additions/removals summary:\n")

plot_data %>%
  filter(year %in% c(2025, 2030, 2050, 2100)) %>%
  select(sector_label, scenario_label, year, flow, value_bm2) %>%
  pivot_wider(
    names_from = flow,
    values_from = value_bm2
  ) %>%
  arrange(sector_label, scenario_label, year) %>%
  print(n = Inf, width = Inf)

# ── Theme ───────────────────────────────────────────────────

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
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key.width = unit(1.4, "cm"),
    legend.box = "vertical",
    legend.spacing.y = unit(0.15, "cm"),
    
    strip.text.y.right = element_text(
      face = "bold",
      size = 13,
      angle = 270
    ),
    strip.background = element_blank(),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88"),
    panel.grid.major.y = element_line(colour = "grey85"),
    
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 12, 10, 10)
  )

# ── Plot ────────────────────────────────────────────────────

p_fig3 <- ggplot(
  plot_data,
  aes(
    x = year,
    y = plot_value,
    colour = scenario_label,
    linetype = flow,
    group = interaction(scenario_label, flow)
  )
) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.45) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.3) +
  facet_grid(
    rows = vars(sector_label),
    scales = "free_y"
  ) +
  scale_colour_manual(values = scenario_colours) +
  scale_linetype_manual(values = flow_linetypes) +
  scale_x_continuous(
    breaks = c(2025, 2030, 2040, 2050, 2060, 2080, 2100)
  ) +
  scale_y_continuous(
    labels = function(x) {
      ifelse(
        x == 0,
        "0",
        number(abs(x), accuracy = 0.01, trim = TRUE)
      )
    },
    expand = expansion(mult = c(0.08, 0.10))
  ) +
  labs(
    title = "EU-27 floor-space additions and removals",
    subtitle = "New construction is shown above zero; demolition is shown below zero",
    x = "Year",
    y = expression("Annual floor space (billion m"^2*"/yr)"),
    colour = "Scenario",
    linetype = "Flow"
  ) +
  guides(
    colour = guide_legend(order = 1, nrow = 1, byrow = TRUE),
    linetype = guide_legend(order = 2, nrow = 1, byrow = TRUE)
  ) +
  theme_fig

print(p_fig3)

# ── Save outputs ────────────────────────────────────────────

file_stub <- "Fig3_EU27_residential_commercial_floor_space_additions_removals"

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".png")),
  plot = p_fig3,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".tiff")),
  plot = p_fig3,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".pdf")),
  plot = p_fig3,
  width = 10,
  height = 7,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

write_csv(
  plot_data,
  file.path(plot_dir, paste0(file_stub, "_data.csv"))
)

cat("\nSaved Fig. 3 outputs to:\n")
cat(plot_dir, "\n")