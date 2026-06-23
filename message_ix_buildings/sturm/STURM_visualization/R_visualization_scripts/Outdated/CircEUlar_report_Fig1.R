# ============================================================
# Fig1_v2 — EU-27 residential floor-space trajectories
# ============================================================

library(tidyverse)
library(readr)
library(scales)

# ── User settings ───────────────────────────────────────────

output_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/output"

plot_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

years_to_run <- c(seq(2020, 2060, 5), seq(2070, 2100, 10))

scenarios <- c(
  "R",
  "N_r", "N_tp",
  "S_r", "S_tp",
  "C_r", "C_tp"
)

make_energy_file_path <- function(scenario) {
  file.path(
    output_dir,
    paste0("report_STURM_", scenario, "_resid_region_bld_energy.csv")
  )
}

# ── Load scenario files ─────────────────────────────────────

df_all <- map_dfr(scenarios, function(sc) {
  f <- make_energy_file_path(sc)
  
  if (!file.exists(f)) {
    cat("  NOT FOUND:", f, "\n")
    return(NULL)
  }
  
  cat("  Loaded:", f, "\n")
  
  read_csv(f, show_col_types = FALSE) %>%
    mutate(scenario = sc)
}) %>%
  mutate(scenario = factor(scenario, levels = scenarios))

if (nrow(df_all) == 0) {
  stop("No scenario files were loaded. Check output_dir and scenario names.")
}

# ── Identify floor-space column ─────────────────────────────

candidate_floor_cols <- c(
  "floor_Mm2",
  "floor",
  "floor_m2",
  "floorspace",
  "floorspace_Mm2",
  "value"
)

floor_col <- candidate_floor_cols[candidate_floor_cols %in% names(df_all)][1]

if (is.na(floor_col)) {
  stop(
    paste0(
      "Could not find a floor-space column. Expected one of: ",
      paste(candidate_floor_cols, collapse = ", ")
    )
  )
}

cat("\nUsing floor-space column:", floor_col, "\n")

# ── Deduplicate building segments ───────────────────────────
# Energy output can repeat floor space across energy carriers/end uses.
# This avoids double-counting.

segment_cols <- c(
  "year", "scenario",
  "region_bld", "region_gea",
  "urt", "clim", "inc_cl",
  "arch", "mat", "eneff",
  floor_col
)

segment_cols <- segment_cols[segment_cols %in% names(df_all)]

df_floor_segments <- df_all %>%
  filter(year %in% years_to_run) %>%
  select(all_of(segment_cols)) %>%
  distinct() %>%
  rename(floor_Mm2 = all_of(floor_col))

# If floor_Mm2 is million m2, divide by 1000 to get billion m2.
million_to_billion <- 1000

# ── EU-27 aggregate floor space ─────────────────────────────

df_eu_floor <- df_floor_segments %>%
  group_by(year, scenario) %>%
  summarise(
    floor_Mm2        = sum(floor_Mm2, na.rm = TRUE),
    floor_billion_m2 = floor_Mm2 / million_to_billion,
    .groups = "drop"
  )

# ── Keep representative trajectories ────────────────────────
# For occupied floor space, Reference, Slow, and Close overlap.
# Use Reference as the representative line.

trajectory_levels <- c(
  "Reference / Slow / Close",
  "Narrow R",
  "Narrow TP"
)

representative_scenarios <- c("R", "N_r", "N_tp")

df_eu_rep <- df_eu_floor %>%
  filter(as.character(scenario) %in% representative_scenarios) %>%
  mutate(
    trajectory = recode(
      as.character(scenario),
      "R"    = "Reference / Slow / Close",
      "N_r"  = "Narrow R",
      "N_tp" = "Narrow TP"
    ),
    trajectory = factor(trajectory, levels = trajectory_levels)
  )

# ── Aesthetics ──────────────────────────────────────────────

cols_main <- c(
  "Reference / Slow / Close" = "#4D4D4D",
  "Narrow R" = "#D55E00",
  "Narrow TP" = "#0072B2"
)

lts_main <- c(
  "Reference / Slow / Close" = "solid",
  "Narrow R" = "dashed",
  "Narrow TP" = "dotted"
)

theme_fig <- theme_minimal(base_size = 14) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 12),
    plot.title       = element_text(face = "bold", size = 18),
    plot.subtitle    = element_text(size = 13, margin = margin(b = 10)),
    axis.title       = element_text(size = 13),
    axis.text        = element_text(size = 12),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

# ── Plot ────────────────────────────────────────────────────

p_fig1_v2a <- ggplot(
  df_eu_rep,
  aes(
    x = year,
    y = floor_billion_m2,
    colour = trajectory,
    linetype = trajectory
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = cols_main) +
  scale_linetype_manual(values = lts_main) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05)),
    labels = label_number(accuracy = 0.1)
  ) +
  scale_x_continuous(
    breaks = c(2020, 2030, 2040, 2050, 2060, 2080, 2100)
  ) +
  labs(
    title = "EU-27 residential floor-space trajectories",
    x = "Year",
    y = expression("Residential floor space (billion m"^2*")")
  ) +
  theme_fig +
  guides(
    colour   = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  )

print(p_fig1_v2a)

# ── Save ────────────────────────────────────────────────────

ggsave(
  filename = file.path(plot_dir, "Fig1_v2_EU27_residential_floor_space_trajectories.png"),
  plot     = p_fig1_v2a,
  width    = 9,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)

ggsave(
  filename = file.path(plot_dir, "Fig1_v2_EU27_residential_floor_space_trajectories.tiff"),
  plot     = p_fig1_v2a,
  width    = 9,
  height   = 6,
  dpi      = 300,
  compression = "lzw",
  bg       = "white"
)

ggsave(
  filename = file.path(plot_dir, "Fig1_v2_EU27_residential_floor_space_trajectories.pdf"),
  plot     = p_fig1_v2a,
  width    = 9,
  height   = 6,
  device   = cairo_pdf,
  bg       = "white"
)

cat("\nSaved Fig1_v2 trajectory figure to:\n", plot_dir, "\n")