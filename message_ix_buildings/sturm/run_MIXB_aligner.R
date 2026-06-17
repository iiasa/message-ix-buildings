# =============================================================================
# run_MIXB_aligner.R — align base year MixB vs IEA
# =============================================================================

library(tidyverse)

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  ca <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", ca, value = TRUE)
  if (length(file_arg)) {
    setwd(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  } else {
    stop("Run from RStudio or: Rscript run_MIXB_aligner.R (from sturm/)")
  }
}

# =============================================================================
# USER SETTINGS
# =============================================================================

dir_message_linking <- file.path(getwd(), "message_linking")
dir.create(dir_message_linking, recursive = TRUE, showWarnings = FALSE)
source(file.path(dir_message_linking, "resolve_sturm_data_dir.R"))
source(file.path(dir_message_linking, "load_scenario_config.R"))
scenarios <- load_scenarios()

# IEA reference: sturm/data/ (default) or <local-data>/buildings/sturm/ (private)
#   default:  Rscript run_MIXB_aligner.R
#   private:  Rscript run_MIXB_aligner.R --data=private
path_iea <- file.path(resolve_sturm_data_dir(), "ref_iea_bt.csv")

# -----------------------------------------------------------------------------
# STEP 0 — Aligner parameters
# -----------------------------------------------------------------------------
# Units (must stay consistent across STEP 2–6):
#   STURM message_linking csv `value` column → GWa
#   IEA ref_iea_bt.csv (2020 col)     → EJ
#   ALIGN_THRESH_EJ                   → EJ  (compare |gap_EJ| to these)
#   Adjustments on MixB rows          → GWa
# Conversion: value_EJ = value_GWa / u_EJ_GWa  (same factor as F10 / R01)

ALIGN_FUELS <- c(
  "gas", 
  "coal", 
  "oil", 
  "electricity")
ALIGN_THRESH_EJ <- list(
  oil = 2, 
  coal = 2, 
  gas = 2, 
  electricity = 5)  # EJ; use |gap_EJ|

gap_exceeds_threshold <- function(gap_EJ, fuel) {
  !is.na(gap_EJ) && abs(gap_EJ) > ALIGN_THRESH_EJ[[fuel]]
}

# Gap vs IEA at IEA_ALIGN_YEAR; correction only within [ALIGN_APPLY_START, ALIGN_APPLY_END].
ALIGN_APPLY_START <- 2020
ALIGN_APPLY_END   <- 2080   # e.g. 2040 or 2060; years outside window keep raw STURM/GLANCE
# Fade on the 2020 gap (1 at start → 0 at end of apply window); gap is scaled before apply:
#   "logistic"     — S-curve (ALIGN_FADE_K, ALIGN_FADE_T_MID)
#   "exponential"  — smooth decay to zero at ALIGN_APPLY_END (ALIGN_FADE_EXP_RATE)
#   "none"         — full correction at every year in the window
ALIGN_FADE <- "exponential"
ALIGN_FADE_K     <- 8    # logistic steepness
ALIGN_FADE_T_MID <- 0.5  # logistic midpoint in t ∈ [0, 1]
ALIGN_FADE_EXP_RATE <- 5 # exponential λ (higher = faster fade toward yr_end)
#   "scale"    — multiply by 1 + fade * (scale_2020 - 1) (default)
#   "additive" — add fade * delta_EJ from 2020 shares
ALIGN_METHOD <- "scale"
# How the 2020 vs IEA gap is split before STEP 6 (STEP 4 threshold is always global):
#   "global"   — one gap per fuel (R12 sum); correction shared across all regions
#   "regional" — if global threshold exceeded, correct each R12 node to its regional IEA 2020
ALIGN_GAP_ALLOCATION <- "regional"
ALIGN_VALUE_EPS <- 1e-9  # GWa; below this at 2020 → additive fallback for that row
u_EJ_GWa <- 31.71

gwa_to_ej <- function(value_GWa) value_GWa / u_EJ_GWa
ej_to_gwa <- function(value_EJ)  value_EJ * u_EJ_GWa

# Fuel labels for MixB vs IEA comparison (fuel_sector_enduse)
ALIGN_COMMODITIES <- list(
  oil = c(
    "lightoil_resid_heat", "lightoil_resid_hotwater", "lightoil_resid_cook",
    "lightoil_comm_heat", "lightoil_comm_hotwater"
  ),
  gas = c(
    "gas_resid_heat", "gas_resid_hotwater",
    "gas_comm_heat", "gas_comm_hotwater",
    "gas_resid_apps"
  ),
  coal = c(
    "coal_resid_heat", "coal_resid_hotwater",
    "coal_comm_heat", "coal_comm_hotwater"
  ),
  electricity = c(
    "electr_resid_cool", "electr_resid_heat", "electr_resid_hotwater", "electr_resid_other_uses",
    "electr_comm_cool", "electr_comm_heat", "electr_comm_hotwater", "electr_comm_other_uses",
    "electr_resid_cook", "electr_resid_apps"
  )
)
# First segment of align names (gas, lightoil, coal, electr) — used by align_commodity_to_mixb
ALIGN_NAME_FUEL_TOKENS <- unique(vapply(
  unlist(ALIGN_COMMODITIES, use.names = FALSE),
  function(x) sub("_(resid|comm)_.*", "", x),
  character(1)
))

# IEA World Energy Balances variables → aligner fuel groups
IEA_VARIABLE_FUEL <- c(
  "Final Energy|Residential and Commercial|Liquids"       = "oil",
  "Final Energy|Residential and Commercial|Solids|Coal"   = "coal",
  "Final Energy|Residential and Commercial|Gases"         = "gas",
  "Final Energy|Residential and Commercial|Electricity"   = "electricity"
)
IEA_ALIGN_YEAR <- 2020L

# Align label {fuel}_{sector}_{enduse}  e.g. gas_resid_heat, electr_resid_cool
# MESSAGE export {sector}_{enduse}_{fuel_token}  e.g. resid_heat_gas, resid_cool_electr
align_commodity_to_mixb <- function(align_commodity) {
  m <- stringr::str_match(align_commodity, "^([^_]+)_(resid|comm)_(.+)$")
  if (is.na(m[1, 1])) {
    return(NA_character_)
  }
  fuel <- m[1, 2]
  if (!fuel %in% ALIGN_NAME_FUEL_TOKENS) {
    return(NA_character_)
  }
  # align: {fuel}_{sector}_{enduse}  →  MESSAGE: {sector}_{enduse}_{fuel}
  paste(m[1, 3], m[1, 4], fuel, sep = "_")
}

build_commodity_name_maps <- function() {
  align <- unlist(ALIGN_COMMODITIES, use.names = FALSE)
  message <- vapply(align, align_commodity_to_mixb, character(1))
  if (any(is.na(message))) {
    stop(
      "ALIGN_COMMODITIES entry has no MESSAGE name: ",
      paste(align[is.na(message)], collapse = ", ")
    )
  }
  list(
    align_to_message = stats::setNames(message, align),
    message_to_align = stats::setNames(align, message)
  )
}

.COMMODITY_MAPS <- build_commodity_name_maps()

mixb_commodity_to_align <- function(commodity) {
  commodity <- sub("^resids_", "resid_", commodity) # YJ: Pin XZ to rename
  if (commodity %in% names(.COMMODITY_MAPS$message_to_align)) {
    return(unname(.COMMODITY_MAPS$message_to_align[commodity]))
  }
  if (commodity %in% names(.COMMODITY_MAPS$align_to_message)) {
    return(commodity)
  }
  NA_character_
}

map_mixb_commodity_to_fuel <- function(commodity) {
  align_name <- mixb_commodity_to_align(commodity)
  if (is.na(align_name)) {
    return(NA_character_)
  }
  for (fuel in ALIGN_FUELS) {
    if (align_name %in% ALIGN_COMMODITIES[[fuel]]) {
      return(fuel)
    }
  }
  NA_character_
}

#' Sum all R12 regions by fuel and year (energy rows only: unit GWa → EJ for IEA compare)
aggregate_mixb_by_fuel <- function(mixb) {
  mixb %>%
    filter(unit == "GWa") %>%  # exclude material / floor rows in MESSAGE export
    mutate(
      fuel = vapply(commodity, map_mixb_commodity_to_fuel, character(1))
    ) %>%
    filter(!is.na(fuel)) %>%
    group_by(year, fuel) %>%
    summarise(
      value_GWa = sum(value, na.rm = TRUE),
      value_EJ  = gwa_to_ej(sum(value, na.rm = TRUE)),
      .groups = "drop"
    )
}

#' Sum by fuel, year, and R12 node (GWa → EJ)
aggregate_mixb_by_fuel_region <- function(mixb) {
  mixb %>%
    filter(unit == "GWa", grepl("^R12_", node)) %>%
    mutate(fuel = vapply(commodity, map_mixb_commodity_to_fuel, character(1))) %>%
    filter(!is.na(fuel)) %>%
    group_by(year, fuel, region = node) %>%
    summarise(value_EJ = gwa_to_ej(sum(value, na.rm = TRUE)), .groups = "drop")
}

#' IEA final energy by fuel, region, and year (World + R12_*)
load_iea_fuel_timeseries <- function(path_iea) {
  raw <- read_csv(path_iea, show_col_types = FALSE)
  yr_cols <- names(raw)[grepl("^\\d{4}$", names(raw))]
  if (length(yr_cols) == 0) {
    stop("IEA file has no year columns: ", path_iea)
  }
  raw %>%
    filter(Variable %in% names(IEA_VARIABLE_FUEL)) %>%
    filter(Region == "World" | grepl("^R12_", Region)) %>%
    mutate(fuel = unname(IEA_VARIABLE_FUEL[Variable])) %>%
    select(region = Region, fuel, all_of(yr_cols)) %>%
    pivot_longer(
      all_of(yr_cols),
      names_to = "year",
      values_to = "value_EJ"
    ) %>%
    mutate(
      year = as.integer(year),
      series = "IEA"
    ) %>%
    filter(!is.na(value_EJ))
}

#' Load IEA reference: Variable, Region, value for IEA_ALIGN_YEAR only
load_iea_reference <- function(path_iea) {
  yr_col <- as.character(IEA_ALIGN_YEAR)
  raw <- read_csv(path_iea, show_col_types = FALSE)
  if (!yr_col %in% names(raw)) {
    stop("IEA file missing column ", yr_col, ": ", path_iea)
  }
  raw %>%
    filter(Variable %in% names(IEA_VARIABLE_FUEL)) %>%
    transmute(
      region = Region,
      fuel = unname(IEA_VARIABLE_FUEL[Variable]),
      variable = Variable,
      year = IEA_ALIGN_YEAR,
      value_EJ = .data[[yr_col]]
    )
}

#' Sum R12 regions by fuel (same global scope as MixB aggregate)
aggregate_iea_by_fuel <- function(iea) {
  iea %>%
    filter(!is.na(fuel), grepl("^R12_", region)) %>%
    group_by(year, fuel) %>%
    summarise(value_EJ = sum(value_EJ, na.rm = TRUE), .groups = "drop")
}


# -----------------------------------------------------------------------------
# STEP 3 — Gaps between MixB and IEA
# -----------------------------------------------------------------------------

compute_fuel_gaps <- function(mixb_agg, iea_agg) {
  gaps <- mixb_agg %>%
    filter(year == IEA_ALIGN_YEAR) %>%
    select(year, fuel, mixb_EJ = value_EJ) %>%
    left_join(
      iea_agg %>% select(year, fuel, iea_EJ = value_EJ),
      by = c("year", "fuel")
    ) %>%
    mutate(
      gap_EJ = mixb_EJ - iea_EJ,
      abs_gap_EJ = abs(gap_EJ)
    )
  missing <- gaps %>% filter(is.na(iea_EJ) | is.na(mixb_EJ))
  if (nrow(missing) > 0) {
    warning(
      "Gap rows with missing mixb or IEA data: ",
      paste(missing$fuel, collapse = ", ")
    )
  }
  gaps
}

log_gap_summary <- function(gaps) {
  thresh_txt <- paste(
    vapply(ALIGN_FUELS, function(f) {
      sprintf("%s %.1f", f, ALIGN_THRESH_EJ[[f]])
    }, character(1)),
    collapse = ", "
  )
  message(
    "Step 3 — mixb vs IEA (", IEA_ALIGN_YEAR,
    ", global R12 sum, EJ; |gap| thresholds: ", thresh_txt, "):"
  )
  for (fuel in ALIGN_FUELS) {
    row <- gaps %>% filter(fuel == !!fuel)
    if (nrow(row) == 0) {
      message("  ", fuel, ": no mapped MixB / IEA rows (check ALIGN_COMMODITIES)")
      next
    }
    if (is.na(row$iea_EJ[1])) {
      message("  ", fuel, ": mixb = ", sprintf("%.3f", row$mixb_EJ[1]),
              " EJ | IEA missing for this fuel")
      next
    }
    if (is.na(row$mixb_EJ[1])) {
      message("  ", fuel, ": IEA = ", sprintf("%.3f", row$iea_EJ[1]),
              " EJ | no mapped MixB commodities")
      next
    }
    thresh <- ALIGN_THRESH_EJ[[fuel]]
    gap <- row$gap_EJ[1]
    abs_gap <- row$abs_gap_EJ[1]
    flag <- if (gap_exceeds_threshold(gap, fuel)) "EXCEEDS threshold" else "within threshold"
    n_comm <- length(ALIGN_COMMODITIES[[fuel]])
    message(sprintf(
      "  %s (%d commodities): mixb = %.3f EJ | IEA = %.3f EJ | gap = %+.3f EJ | |gap| = %.3f EJ (thresh %.1f EJ) → %s",
      fuel, n_comm, row$mixb_EJ[1], row$iea_EJ[1], gap, abs_gap, thresh, flag
    ))
  }
  invisible(gaps)
}

#' Per R12 node and fuel at IEA_ALIGN_YEAR (for regional gap allocation).
compute_fuel_gaps_by_region <- function(mixb, path_iea) {
  mixb_reg <- aggregate_mixb_by_fuel_region(mixb) %>%
    filter(year == IEA_ALIGN_YEAR) %>%
    select(node = region, fuel, mixb_EJ = value_EJ)

  iea_reg <- load_iea_reference(path_iea) %>%
    filter(grepl("^R12_", region)) %>%
    select(node = region, fuel, iea_EJ = value_EJ)

  gaps <- mixb_reg %>%
    left_join(iea_reg, by = c("node", "fuel")) %>%
    mutate(
      year = IEA_ALIGN_YEAR,
      gap_EJ = mixb_EJ - iea_EJ,
      abs_gap_EJ = abs(gap_EJ)
    )

  missing <- gaps %>% filter(is.na(iea_EJ) | is.na(mixb_EJ))
  if (nrow(missing) > 0) {
    warning(
      "Regional gap rows with missing MixB or IEA: ",
      paste(unique(paste0(missing$node, "/", missing$fuel)), collapse = ", ")
    )
  }
  gaps
}

# -----------------------------------------------------------------------------
# STEP 4 — Threshold check
# -----------------------------------------------------------------------------

needs_alignment <- function(gaps) {
  # TRUE if any fuel has |MixB − IEA| above its EJ threshold
  any(vapply(ALIGN_FUELS, function(fuel) {
    row <- gaps %>% filter(fuel == !!fuel)
    nrow(row) > 0 && gap_exceeds_threshold(row$gap_EJ[1], fuel)
  }, logical(1)))
}

fuels_needing_alignment <- function(gaps) {
  ALIGN_FUELS[vapply(ALIGN_FUELS, function(fuel) {
    row <- gaps %>% filter(fuel == !!fuel)
    nrow(row) > 0 && gap_exceeds_threshold(row$gap_EJ[1], fuel)
  }, logical(1))]
}

# -----------------------------------------------------------------------------
# STEP 5 — Allocate gap to fuel × node × commodity
# -----------------------------------------------------------------------------

factors_from_shares_2020 <- function(shares_2020, target_correction_EJ, fuel) {
  total_EJ_2020 <- sum(shares_2020$value_EJ_2020, na.rm = TRUE)
  if (total_EJ_2020 <= 0) {
    return(NULL)
  }
  shares_2020 %>%
    mutate(
      share = value_EJ_2020 / total_EJ_2020,
      delta_EJ = target_correction_EJ * share,
      value_GWa_2020 = ej_to_gwa(value_EJ_2020),
      scale_factor = if_else(
        value_GWa_2020 > ALIGN_VALUE_EPS,
        ej_to_gwa(value_EJ_2020 + delta_EJ) / value_GWa_2020,
        NA_real_
      ),
      use_additive = is.na(scale_factor) | !is.finite(scale_factor),
      fuel = fuel
    ) %>%
    select(node, commodity, fuel, share, delta_EJ, scale_factor, use_additive)
}

#' Global allocation: one 2020 gap per fuel (R12 sum), split over all node × commodity.
#'
#' Share at each (node, commodity) for that fuel:
#'   share = value_EJ_2020(fuel, node, commodity) / sum_{node,commodity} value_EJ_2020(fuel, ·, ·)
allocate_gap_to_tech_region <- function(mixb, gaps, fuel) {
  gap_row <- gaps %>% filter(fuel == !!fuel, year == IEA_ALIGN_YEAR)
  if (nrow(gap_row) == 0 || is.na(gap_row$gap_EJ[1])) {
    return(NULL)
  }

  # Move MixB toward IEA: total correction = IEA − MixB = −gap_EJ (EJ)
  target_correction_EJ <- -gap_row$gap_EJ[1]

  rows <- mixb %>%
    filter(unit == "GWa") %>%
    mutate(
      row_fuel = vapply(commodity, map_mixb_commodity_to_fuel, character(1)),
      value_EJ = gwa_to_ej(value)
    ) %>%
    filter(row_fuel == fuel)

  if (nrow(rows) == 0) {
    warning("Step 5 — no mixb rows for fuel: ", fuel)
    return(NULL)
  }

  shares_2020 <- rows %>%
    filter(year == IEA_ALIGN_YEAR) %>%
    group_by(node, commodity) %>%
    summarise(value_EJ_2020 = sum(value_EJ, na.rm = TRUE), .groups = "drop")

  out <- factors_from_shares_2020(shares_2020, target_correction_EJ, fuel)
  if (is.null(out)) {
    warning("Step 5 — zero mixb total for ", fuel, " in ", IEA_ALIGN_YEAR)
  }
  out
}

#' Regional allocation: one 2020 gap per fuel × R12 node; split only within that node.
#'
#' At each node: target_correction_EJ(node) = IEA(node) − MixB(node) = −gap_EJ(node).
#' Commodity shares use only rows with that node at IEA_ALIGN_YEAR.
allocate_gap_to_tech_region_regional <- function(mixb, gaps_by_region, fuel) {
  rows <- mixb %>%
    filter(unit == "GWa", grepl("^R12_", node)) %>%
    mutate(
      row_fuel = vapply(commodity, map_mixb_commodity_to_fuel, character(1)),
      value_EJ = gwa_to_ej(value)
    ) %>%
    filter(row_fuel == fuel)

  if (nrow(rows) == 0) {
    warning("Step 5 — no mixb rows for fuel: ", fuel)
    return(NULL)
  }

  region_gaps <- gaps_by_region %>%
    filter(fuel == !!fuel, grepl("^R12_", node), !is.na(gap_EJ))

  factor_list <- lapply(region_gaps$node, function(node) {
    gap_row <- region_gaps %>% filter(node == !!node)
    if (nrow(gap_row) == 0) {
      return(NULL)
    }
    target_correction_EJ <- -gap_row$gap_EJ[1]

    shares_2020 <- rows %>%
      filter(node == !!node, year == IEA_ALIGN_YEAR) %>%
      group_by(node, commodity) %>%
      summarise(value_EJ_2020 = sum(value_EJ, na.rm = TRUE), .groups = "drop")

    out <- factors_from_shares_2020(shares_2020, target_correction_EJ, fuel)
    if (is.null(out)) {
      warning(
        "Step 5 — zero mixb at ", node, " for ", fuel, " in ", IEA_ALIGN_YEAR,
        " (skipped)"
      )
    }
    out
  })

  factor_list <- Filter(Negate(is.null), factor_list)
  if (length(factor_list) == 0) {
    return(NULL)
  }
  bind_rows(factor_list)
}


# -----------------------------------------------------------------------------
# STEP 6 — Apply faded 2020 gap within ALIGN_APPLY_START:ALIGN_APPLY_END
# -----------------------------------------------------------------------------

#' Fade weight for the gap: 1 at yr_start, 0 at yr_end, monotonic in between.
alignment_fade_factor <- function(
    year,
    yr_start = ALIGN_APPLY_START,
    yr_end = ALIGN_APPLY_END,
    mode = ALIGN_FADE,
    k = ALIGN_FADE_K,
    t_mid = ALIGN_FADE_T_MID,
    exp_rate = ALIGN_FADE_EXP_RATE) {
  if (year < yr_start || year > yr_end) {
    return(0)
  }
  if (yr_end <= yr_start) {
    return(as.numeric(year == yr_start))
  }
  if (year == yr_start) {
    return(1)
  }
  if (year == yr_end) {
    return(0)
  }
  if (mode == "none") {
    return(1)
  }

  t <- (year - yr_start) / (yr_end - yr_start)
  if (mode == "logistic") {
    raw_lo <- stats::plogis(k * (0 - t_mid))
    raw_hi <- stats::plogis(k * (1 - t_mid))
    s <- (stats::plogis(k * (t - t_mid)) - raw_lo) / (raw_hi - raw_lo)
    return(1 - s)
  }
  if (mode == "exponential") {
    lam <- exp_rate
    den <- 1 - exp(-lam)
    if (den <= 0) {
      return(1 - t)
    }
    return((exp(-lam * t) - exp(-lam)) / den)
  }
  warning("Unknown ALIGN_FADE mode: ", mode, " — using fade = 1")
  1
}

#' Apply alignment inside [yr_start, yr_end] with faded gap; other years unchanged.
apply_alignment_adjustment <- function(
    mixb,
    factors,
    method = ALIGN_METHOD,
    yr_start = ALIGN_APPLY_START,
    yr_end = ALIGN_APPLY_END,
    fade_mode = ALIGN_FADE) {
  if (is.null(factors) || nrow(factors) == 0) {
    return(mixb)
  }

  mixb %>%
    left_join(
      factors %>% select(node, commodity, delta_EJ, scale_factor, use_additive),
      by = c("node", "commodity")
    ) %>%
    mutate(
      delta_EJ = tidyr::replace_na(delta_EJ, 0),
      use_additive = tidyr::replace_na(use_additive, TRUE),
      fade = vapply(
        as.integer(year),
        alignment_fade_factor,
        numeric(1),
        yr_start = yr_start,
        yr_end = yr_end,
        mode = fade_mode
      ),
      in_apply_window = fade > 0 | (year >= yr_start & year <= yr_end),
      value = case_when(
        unit != "GWa" ~ value,
        fade <= 0 ~ value,
        method == "additive" ~ pmax(value + ej_to_gwa(fade * delta_EJ), 0),
        use_additive ~ pmax(value + ej_to_gwa(fade * delta_EJ), 0),
        TRUE ~ pmax(value * (1 + fade * (scale_factor - 1)), 0)
      )
    ) %>%
    select(-delta_EJ, -scale_factor, -use_additive, -fade, -in_apply_window)
}


# -----------------------------------------------------------------------------
# STEP 7 — Write output csv files to feed into MESSAGE linking
# -----------------------------------------------------------------------------

write_aligned_mixb <- function(mixb, path_out) {
  # MESSAGE schema; `value` remains in GWa
  mixb %>%
    select(node, commodity, level, year, time, value, unit) %>%
    write_csv(path_out)
}


# -----------------------------------------------------------------------------
# STEP 7b — Before / after alignment plots (PDF)
# -----------------------------------------------------------------------------

#' Human-readable list of align commodities for plot subtitles (from ALIGN_COMMODITIES).
#' Multiple lines when there are more than `n_per_line` names (ggplot subtitle uses \\n).
align_fuel_commodities_label <- function(fuel, n_per_line = 3L) {
  x <- ALIGN_COMMODITIES[[fuel]]
  if (length(x) <= n_per_line) {
    return(paste(x, collapse = ", "))
  }
  chunks <- split(x, ceiling(seq_along(x) / n_per_line))
  paste(vapply(chunks, paste, character(1), collapse = ", "), collapse = "\n")
}

#' Long time series for one scenario: MixB before, MixB after, IEA (EJ).
prep_alignment_plot_data <- function(mixb_before, mixb_after, path_iea) {
  before <- aggregate_mixb_by_fuel_region(mixb_before) %>%
    mutate(series = "mixb before")
  after <- aggregate_mixb_by_fuel_region(mixb_after) %>%
    mutate(series = "mixb after")
  world <- bind_rows(before, after) %>%
    group_by(year, fuel, series) %>%
    summarise(value_EJ = sum(value_EJ, na.rm = TRUE), .groups = "drop") %>%
    mutate(region = "World")

  regions <- bind_rows(before, after) %>%
    mutate(region_label = sub("^R12_", "", region))

  # Full IEA time series for plots (alignment still uses IEA_ALIGN_YEAR only)
  iea <- if (file.exists(path_iea)) {
    load_iea_fuel_timeseries(path_iea) %>%
      mutate(region_label = if_else(region == "World", "World", sub("^R12_", "", region)))
  } else {
    tibble()
  }

  r12_order <- sort(unique(regions$region_label))
  list(
    world = bind_rows(world, iea %>% filter(region == "World") %>% select(year, fuel, series, value_EJ, region)),
    regions = regions,
    iea_regions = iea %>% filter(region != "World"),
    r12_order = r12_order
  )
}

plot_alignment_fuel_page <- function(plot_data, fuel, scenario) {
  world_df <- plot_data$world %>% filter(fuel == !!fuel)
  reg_df <- plot_data$regions %>%
    filter(fuel == !!fuel, region_label %in% plot_data$r12_order) %>%
    mutate(region_label = factor(region_label, levels = plot_data$r12_order))
  iea_reg <- plot_data$iea_regions %>%
    filter(fuel == !!fuel, region_label %in% plot_data$r12_order) %>%
    mutate(region_label = factor(region_label, levels = plot_data$r12_order))

  commodities_label <- align_fuel_commodities_label(fuel)
  plot_subtitle <- paste0(
    "Scenario ", scenario, " | EJ/yr\nCommodities:\n", commodities_label
  )

  series_cols <- c(
    "mixb before" = "#80B1D3",
    "mixb after"  = "#6A3D9A",
    "IEA"         = "#e82326"
  )

  series_lty <- c("mixb before" = "solid", "mixb after" = "solid", "IEA" = "22")

  p_world <- ggplot(world_df, aes(x = year, y = value_EJ, color = series, linetype = series)) +
    geom_line(linewidth = 0.9) +
    geom_point(
      data = world_df %>% filter(series == "IEA", year == IEA_ALIGN_YEAR),
      size = 2.2,
      shape = 18
    ) +
    scale_color_manual(values = series_cols, breaks = names(series_cols)) +
    scale_linetype_manual(values = series_lty, breaks = names(series_cols)) +
    labs(
      title = paste0(toupper(fuel), " - World (R12 sum)"),
      subtitle = plot_subtitle,
      x = NULL,
      y = "EJ/yr",
      color = NULL,
      linetype = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 7, lineheight = 0.85, hjust = 0)
    )

  p_regions <- ggplot(reg_df, aes(x = year, y = value_EJ, color = series, linetype = series)) +
    geom_line(linewidth = 0.65) +
    geom_line(
      data = iea_reg,
      mapping = aes(x = year, y = value_EJ, group = 1),
      inherit.aes = FALSE,
      color = series_cols[["IEA"]],
      linewidth = 0.65,
      linetype = "22"
    ) +
    geom_point(
      data = iea_reg %>% filter(year == IEA_ALIGN_YEAR),
      mapping = aes(x = year, y = value_EJ),
      inherit.aes = FALSE,
      color = series_cols[["IEA"]],
      shape = 18,
      size = 1.8
    ) +
    facet_wrap(~region_label, ncol = 3, nrow = 4, scales = "free_y") +
    scale_color_manual(values = series_cols[c("mixb before", "mixb after")], breaks = names(series_cols)[1:2]) +
    scale_linetype_manual(values = series_lty[1:2], breaks = names(series_cols)[1:2]) +
    labs(
      title = paste0(toupper(fuel), " - R12 regions"),
      subtitle = plot_subtitle,
      x = "Year",
      y = "EJ/yr",
      color = NULL,
      linetype = NULL
    ) +
    theme_bw(base_size = 9) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 7, lineheight = 0.85, hjust = 0),
      strip.text = element_text(face = "bold")
    )

  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(p_world, p_regions, ncol = 2, widths = c(1.35, 1.65)) +
      patchwork::plot_annotation(
        title = paste0("mixb alignment - ", scenario),
        theme = theme(plot.title = element_text(face = "bold", size = 14))
      )
  } else if (requireNamespace("gridExtra", quietly = TRUE)) {
    gridExtra::arrangeGrob(p_world, p_regions, ncol = 2, widths = c(1.35, 1.65))
  } else {
    warning("Install patchwork or gridExtra; returning world panel only.")
    p_world
  }
}

#' PDF: one page per fuel; left = World, right = 12 R12 regions (3x4 facets).
write_alignment_comparison_pdf <- function(
    mixb_before,
    mixb_after,
    scenario,
    path_pdf,
    path_iea,
    fuels = ALIGN_FUELS) {
  plot_data <- prep_alignment_plot_data(mixb_before, mixb_after, path_iea)
  dir.create(dirname(path_pdf), recursive = TRUE, showWarnings = FALSE)

  grDevices::pdf(path_pdf, width = 11, height = 6.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (fuel in fuels) {
    print(plot_alignment_fuel_page(plot_data, fuel, scenario))
  }
  message("Step 7b — wrote alignment plot: ", path_pdf)
  invisible(path_pdf)
}


# -----------------------------------------------------------------------------
# The full pipeline of the MixB aligner
# -----------------------------------------------------------------------------

align_scenario <- function(s, dir_message_linking, path_iea) {

  path_sturm_comm  <- file.path(dir_message_linking, paste0("comm_sturm_", s, ".csv"))
  path_sturm_resid <- file.path(dir_message_linking, paste0("resid_sturm_", s, ".csv"))
  path_glance <- file.path(dir_message_linking, paste0("resid_comm_glance_", s, ".csv"))

  # STEP 1 — load paired exports (skip if insufficient exports exist)
  if (!file.exists(path_sturm_comm) || !file.exists(path_sturm_resid)) {
    message("skip '", s, "': need both\n  ", path_sturm_comm, "\n  ", path_sturm_resid)
    return(invisible(NULL))
  }

  message("=== align scenario: ", s, " ===")
  mixb <- bind_rows(
    read_csv(path_sturm_comm,  show_col_types = FALSE) %>%
      mutate(sector = "comm", source = "sturm"),
    read_csv(path_sturm_resid, show_col_types = FALSE) %>%
      mutate(sector = "resid", source = "sturm")
  )
  has_glance <- file.exists(path_glance)
  if (has_glance) {
    mixb <- bind_rows(
      mixb,
      read_csv(path_glance, show_col_types = FALSE) %>%
        mutate(sector = "resid", source = "glance")
    )
  } else {
    message("note: no GLANCE file — ", path_glance)
  }

  if (!file.exists(path_iea)) {
    warning("IEA file missing — skip alignment: ", path_iea)
    return(invisible(NULL))
  }
  iea <- load_iea_reference(path_iea)

  # STEP 2 — aggregate MixB by fuel
  mixb_agg <- aggregate_mixb_by_fuel(mixb)
  iea_agg  <- aggregate_iea_by_fuel(iea)

  # STEP 3 — global gaps (regional gaps computed silently when needed for STEP 5)
  gaps <- compute_fuel_gaps(mixb_agg, iea_agg)
  log_gap_summary(gaps)
  gaps_by_region <- NULL
  if (ALIGN_GAP_ALLOCATION == "regional") {
    gaps_by_region <- compute_fuel_gaps_by_region(mixb, path_iea)
  }

  mixb_before <- mixb

  # STEP 4–6 — allocate gap to fuel × node × commodity; apply adjustment
  mixb_aligned <- mixb
  use_regional <- identical(ALIGN_GAP_ALLOCATION, "regional")
  if (!is.character(ALIGN_GAP_ALLOCATION) ||
      !ALIGN_GAP_ALLOCATION %in% c("global", "regional")) {
    stop("ALIGN_GAP_ALLOCATION must be 'global' or 'regional'")
  }
  if (!needs_alignment(gaps)) {
    message("gaps below threshold — no adjustment")
  } else {
    message(
      "apply correction in ", ALIGN_APPLY_START, "–", ALIGN_APPLY_END,
      " (gap vs IEA at ", IEA_ALIGN_YEAR, ", fade = ", ALIGN_FADE,
      ", allocation = ", ALIGN_GAP_ALLOCATION, ")"
    )
    if (IEA_ALIGN_YEAR < ALIGN_APPLY_START || IEA_ALIGN_YEAR > ALIGN_APPLY_END) {
      warning(
        "IEA_ALIGN_YEAR (", IEA_ALIGN_YEAR, ") is outside apply window [",
        ALIGN_APPLY_START, ", ", ALIGN_APPLY_END, "] — 2020 will not match IEA"
      )
    }
    for (fuel in fuels_needing_alignment(gaps)) {
      message("Step 5–6 — aligning fuel: ", fuel, " (", ALIGN_GAP_ALLOCATION, ")")
      factors <- if (use_regional) {
        allocate_gap_to_tech_region_regional(mixb_before, gaps_by_region, fuel)
      } else {
        allocate_gap_to_tech_region(mixb_before, gaps, fuel)
      }
      if (!is.null(factors) && nrow(factors) > 0) {
        mixb_aligned <- apply_alignment_adjustment(mixb_aligned, factors)
      }
    }
  }

  # STEP 7 — split mixb_aligned back to prepare for MESSAGE linking
  write_aligned_mixb(
    filter(mixb_aligned, sector == "comm"),
    file.path(dir_message_linking, paste0("comm_sturm_aligned_", s, ".csv"))
  )
  write_aligned_mixb(
    filter(mixb_aligned, sector == "resid", source == "sturm"),
    file.path(dir_message_linking, paste0("resid_sturm_aligned_", s, ".csv"))
  )
  if (has_glance) {
    write_aligned_mixb(
      filter(mixb_aligned, source == "glance"),
      file.path(dir_message_linking, paste0("resid_comm_glance_aligned_", s, ".csv"))
    )
  }

  # Optional: before / after alignment plots (PDF)
  write_alignment_comparison_pdf(
    mixb_before,
    mixb_aligned,
    scenario = s,
    path_pdf = file.path(dir_message_linking, paste0("align_comparison_", s, ".pdf")),
    path_iea = path_iea
  )

  invisible(mixb_aligned)
}


# =============================================================================
# MAIN — loop scenarios
# =============================================================================

message("MixB aligner cwd: ", getwd())
message("message_linking: ", dir_message_linking, " | IEA: ", path_iea)
message(
  "Align fuels (vs IEA R&C): ", paste(ALIGN_FUELS, collapse = ", "),
  " | allocation: ", ALIGN_GAP_ALLOCATION,
  " | apply ", ALIGN_APPLY_START, "–", ALIGN_APPLY_END
)

for (s in scenarios) {
  align_scenario(s, dir_message_linking, path_iea)
}

message("MixB aligner done.")
