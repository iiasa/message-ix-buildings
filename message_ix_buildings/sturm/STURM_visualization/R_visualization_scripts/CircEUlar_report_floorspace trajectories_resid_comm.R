# ============================================================
# Fig — EU-27 floor-space trajectories
# Works for either residential or commercial sector
# ============================================================

library(tidyverse)
library(readr)
library(scales)

# ── User settings ───────────────────────────────────────────

sector <- "resid"   # choose: "resid" or "comm"

output_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/output"

plot_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

years_to_run <- c(seq(2020, 2060, 5), seq(2070, 2100, 10))

scenarios <- c(
  "R",
  "N_r", "N_tp",
  "S_r", "S_tp",
  "C_r", "C_tp",
  "A_r", "A_tp"
)

# ── Sector labels ───────────────────────────────────────────

sector_label <- case_when(
  sector == "resid" ~ "residential",
  sector == "comm"  ~ "commercial",
  TRUE ~ sector
)

sector_title <- case_when(
  sector == "resid" ~ "Residential",
  sector == "comm"  ~ "Commercial",
  TRUE ~ str_to_title(sector)
)

# ── Helper: find STURM energy file ──────────────────────────
# Residential files may be region_bld.
# Commercial files may be R12 depending on geo_level_report.
# This function searches rather than hardcoding the geography level.

find_energy_file <- function(scenario, sector) {
  
  pattern <- paste0(
    "^report_STURM_",
    scenario,
    "_",
    sector,
    "_.*_energy\\.csv$"
  )
  
  files <- list.files(
    output_dir,
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    return(NA_character_)
  }
  
  # Prefer region_bld if available, otherwise use first match
  region_bld_file <- files[str_detect(files, "_region_bld_energy\\.csv$")]
  
  if (length(region_bld_file) > 0) {
    return(region_bld_file[1])
  }
  
  return(files[1])
}

# ── Load scenario files ─────────────────────────────────────

df_all <- map_dfr(scenarios, function(sc) {
  
  f <- find_energy_file(sc, sector)
  
  if (is.na(f) || !file.exists(f)) {
    cat("  NOT FOUND for", sc, "\n")
    return(NULL)
  }
  
  cat("  Loaded:", basename(f), "\n")
  
  read_csv(f, show_col_types = FALSE) %>%
    mutate(scenario = sc)
}) %>%
  mutate(scenario = factor(scenario, levels = scenarios))

if (nrow(df_all) == 0) {
  stop("No scenario files were loaded. Check output_dir, sector, and scenario names.")
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
# For occupied floor space, Reference, Slow, and Close should overlap.
# Use Reference as representative line.

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

# ── Print selected values for sanity check ──────────────────

cat("\nSelected-year floor-space summary:\n")

df_eu_floor %>%
  filter(year %in% c(2025, 2030, 2050, 2100)) %>%
  mutate(scenario = as.character(scenario)) %>%
  pivot_wider(
    names_from = scenario,
    values_from = floor_billion_m2
  ) %>%
  print(n = Inf, width = Inf)

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

p_floor <- ggplot(
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
    title = paste0("EU-27 ", sector_label, " floor-space trajectories"),
    x = "Year",
    y = bquote(.(sector_title) ~ "floor space (billion m"^2*")")
  ) +
  theme_fig +
  guides(
    colour   = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  )

print(p_floor)

# ── Save ────────────────────────────────────────────────────

file_stub <- paste0(
  "Fig_floor_space_EU27_",
  sector,
  "_trajectories"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".png")),
  plot     = p_floor,
  width    = 9,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".tiff")),
  plot     = p_floor,
  width    = 9,
  height   = 6,
  dpi      = 300,
  compression = "lzw",
  bg       = "white"
)

ggsave(
  filename = file.path(plot_dir, paste0(file_stub, ".pdf")),
  plot     = p_floor,
  width    = 9,
  height   = 6,
  device   = cairo_pdf,
  bg       = "white"
)

cat("\nSaved floor-space trajectory figure to:\n", plot_dir, "\n")