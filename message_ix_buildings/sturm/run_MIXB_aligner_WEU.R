# ============================================================
# SPECIAL WEU EXPERIMENT ALIGNER (not the standard MIXB aligner)
# ============================================================
#
# WARNING: This script is for the WEU single-region experiment
# only. It is NOT the standard MIXB / IEA aligner workflow.
#
# Method:
#   1) Global scale (all GWa, all years):
#        scale = MESSAGE_final_2020 / STURM_resid+comm_2020
#   2) Fossils + biomass (coal/gas/lightoil/biomass): additional
#      align to MESSAGE fuel totals in 2020, with fade → 0 by 2040
#      (same scale-fade logic as the general run_MIXB_aligner.R).
#   Other fuels keep only step (1) for all years.
#
# Outputs:
#   resid_sturm_aligned_<code>.csv
#   comm_sturm_aligned_<code>.csv
# ============================================================

library(rstudioapi)
library(tidyverse)


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

script_path <- tryCatch(
  rstudioapi::getSourceEditorContext()$path,
  error = function(e) ""
)

if (nzchar(script_path)) {
  setwd(dirname(script_path))
} else {
  ca <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", ca, value = TRUE)
  if (length(file_arg)) {
    setwd(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
}

dir_message_linking <- file.path(getwd(), "message_linking")
dir.create(dir_message_linking, recursive = TRUE, showWarnings = FALSE)

source(file.path(dir_message_linking, "load_scenario_config.R"))
scenarios <- load_scenarios()

path_message_final <- file.path(getwd(), "data", "final_energy_rc.csv")

if (!file.exists(path_message_final)) {
  stop("MESSAGE final-energy file not found: ", path_message_final)
}


# ------------------------------------------------------------
# 1. Experiment parameters / warning
# ------------------------------------------------------------

# Fuels with extra MESSAGE-2020 align + fade (incl. biomass)
FOSSIL_FUELS <- c("coal", "gas", "lightoil", "biomass")

# Fossil fade window (1 at start → 0 at end), style of run_MIXB_aligner.R
ALIGN_APPLY_START <- 2020L
ALIGN_APPLY_END <- 2040L
ALIGN_FADE <- "exponential"
ALIGN_FADE_K <- 8
ALIGN_FADE_T_MID <- 0.5
ALIGN_FADE_EXP_RATE <- 5
ALIGN_VALUE_EPS <- 1e-9

warning(
  paste(
    "SPECIAL WEU EXPERIMENT ALIGNER ONLY:",
    "1) global GWa scale = MESSAGE(final, 2020) / STURM(resid+comm, 2020);",
    "2) coal/gas/lightoil/biomass additionally aligned to MESSAGE 2020",
    "with fade to 0 by 2040. Not the standard MIXB/IEA aligner."
  ),
  call. = FALSE
)

cat("\n")
cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
cat(" WARNING: SPECIAL WEU EXPERIMENT ALIGNER\n")
cat(" 1) Global MESSAGE/STURM scale for all GWa / all years\n")
cat(" 2) coal/gas/lightoil/biomass: align to MESSAGE 2020,\n")
cat("    fade out by 2040\n")
cat(" This is NOT the standard MIXB / IEA aligner.\n")
cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
cat("\n")


# ------------------------------------------------------------
# 2. Helpers
# ------------------------------------------------------------

commodity_fuel <- function(commodity) {
  str_extract(commodity, "[^_]+$")
}

is_fossil_commodity <- function(commodity) {
  commodity_fuel(commodity) %in% FOSSIL_FUELS
}

sturm_energy_2020 <- function(df) {
  df %>%
    filter(
      year == 2020L,
      unit == "GWa",
      level == "final",
      !str_detect(commodity, "v_no_heat")
    ) %>%
    summarise(total = sum(value, na.rm = TRUE)) %>%
    pull(total)
}

sturm_fossil_by_fuel_2020 <- function(df) {
  df %>%
    filter(
      year == 2020L,
      unit == "GWa",
      level == "final",
      !str_detect(commodity, "v_no_heat")
    ) %>%
    mutate(fuel = commodity_fuel(commodity)) %>%
    filter(fuel %in% FOSSIL_FUELS) %>%
    group_by(fuel) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

message_fossil_by_fuel_2020 <- function(message_rc) {
  message_rc %>%
    filter(
      year == 2020L,
      level == "final",
      commodity %in% FOSSIL_FUELS
    ) %>%
    group_by(fuel = commodity) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

# Fade weight: 1 at yr_start, 0 at yr_end (from run_MIXB_aligner.R)
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

# Step 1: global scale for all GWa / all years
apply_global_scale <- function(df, scale_factor) {
  df %>%
    mutate(
      value = if_else(
        unit == "GWa",
        value * scale_factor,
        value
      )
    )
}

# Step 2: fossils only — faded scale to MESSAGE 2020 fuel totals
apply_fossil_fade_alignment <- function(df, fossil_scale_by_fuel) {
  fade_lookup <- tibble(
    year = sort(unique(as.integer(df$year)))
  ) %>%
    mutate(
      fade = vapply(
        year,
        alignment_fade_factor,
        numeric(1)
      )
    )

  scale_lookup <- tibble(
    fuel = names(fossil_scale_by_fuel),
    fossil_scale_2020 = as.numeric(fossil_scale_by_fuel)
  )

  df %>%
    mutate(fuel = commodity_fuel(commodity)) %>%
    left_join(fade_lookup, by = "year") %>%
    left_join(scale_lookup, by = "fuel") %>%
    mutate(
      fade = replace_na(fade, 0),
      fossil_scale_2020 = replace_na(fossil_scale_2020, NA_real_),
      value = case_when(
        unit != "GWa" ~ value,
        is.na(fossil_scale_2020) ~ value,
        fade <= 0 ~ value,
        # scale method from run_MIXB_aligner.R:
        # value * (1 + fade * (scale_2020 - 1))
        TRUE ~ pmax(value * (1 + fade * (fossil_scale_2020 - 1)), 0)
      )
    ) %>%
    select(-fuel, -fade, -fossil_scale_2020)
}

round_values <- function(df, digits = 4) {
  df %>%
    mutate(value = round(value, digits))
}


# ------------------------------------------------------------
# 3. Align each scenario from config.yaml
# ------------------------------------------------------------

message_rc <- read_csv(path_message_final, show_col_types = FALSE)

message_total_2020 <- message_rc %>%
  filter(year == 2020L, level == "final") %>%
  summarise(total = sum(value, na.rm = TRUE)) %>%
  pull(total)

message_fossil_2020 <- message_fossil_by_fuel_2020(message_rc)

cat("Scenarios: ", paste(scenarios, collapse = ", "), "\n", sep = "")
cat(
  "MESSAGE final energy 2020 (data/final_energy_rc.csv): ",
  round(message_total_2020, 4),
  " GWa\n",
  sep = ""
)
cat(
  "Fossil fade: ",
  ALIGN_APPLY_START,
  "–",
  ALIGN_APPLY_END,
  " (",
  ALIGN_FADE,
  ")\n",
  sep = ""
)

for (code in scenarios) {

  resid_file <- file.path(
    dir_message_linking,
    paste0("resid_sturm_", code, ".csv")
  )
  comm_file <- file.path(
    dir_message_linking,
    paste0("comm_sturm_", code, ".csv")
  )

  if (!file.exists(resid_file)) {
    stop("Missing resid STURM file: ", resid_file)
  }
  if (!file.exists(comm_file)) {
    stop("Missing comm STURM file: ", comm_file)
  }

  resid <- read_csv(resid_file, show_col_types = FALSE)
  comm <- read_csv(comm_file, show_col_types = FALSE)

  sturm_total_2020 <- sturm_energy_2020(resid) + sturm_energy_2020(comm)

  if (!is.finite(sturm_total_2020) || sturm_total_2020 <= 0) {
    stop("Invalid STURM 2020 GWa total for scenario ", code, ": ", sturm_total_2020)
  }
  if (!is.finite(message_total_2020) || message_total_2020 <= 0) {
    stop("Invalid MESSAGE 2020 GWa total: ", message_total_2020)
  }

  # ---- Step 1: global MESSAGE / STURM scale (all fuels, all years) ----
  scale_factor <- message_total_2020 / sturm_total_2020

  cat("\n--- Scenario ", code, " ---\n", sep = "")
  cat(
    "STURM resid+comm energy 2020: ",
    round(sturm_total_2020, 4),
    " GWa\n",
    sep = ""
  )
  cat(
    "Global scaling factor (MESSAGE / STURM): ",
    round(scale_factor, 4),
    "\n",
    sep = ""
  )

  resid_scaled <- apply_global_scale(resid, scale_factor)
  comm_scaled <- apply_global_scale(comm, scale_factor)

  # ---- Step 2: fossil align to MESSAGE 2020 with fade to 2040 ----
  sturm_fossil_scaled_2020 <- bind_rows(
    sturm_fossil_by_fuel_2020(resid_scaled),
    sturm_fossil_by_fuel_2020(comm_scaled)
  ) %>%
    group_by(fuel) %>%
    summarise(sturm = sum(value, na.rm = TRUE), .groups = "drop")

  fossil_scales <- message_fossil_2020 %>%
    rename(message = value) %>%
    full_join(sturm_fossil_scaled_2020, by = "fuel") %>%
    mutate(
      message = replace_na(message, 0),
      sturm = replace_na(sturm, 0),
      fossil_scale_2020 = if_else(
        sturm > ALIGN_VALUE_EPS,
        message / sturm,
        NA_real_
      )
    )

  cat("\nFossil scales after global scale (MESSAGE / STURM_scaled, 2020):\n")
  print(
    fossil_scales %>%
      mutate(across(where(is.numeric), ~ round(.x, 4)))
  )

  fossil_scale_by_fuel <- fossil_scales %>%
    filter(!is.na(fossil_scale_2020), is.finite(fossil_scale_2020)) %>%
    {
      stats::setNames(.$fossil_scale_2020, .$fuel)
    }

  resid_aligned <- resid_scaled %>%
    apply_fossil_fade_alignment(fossil_scale_by_fuel) %>%
    round_values(4)

  comm_aligned <- comm_scaled %>%
    apply_fossil_fade_alignment(fossil_scale_by_fuel) %>%
    round_values(4)

  resid_out <- file.path(
    dir_message_linking,
    paste0("resid_sturm_aligned_", code, ".csv")
  )
  comm_out <- file.path(
    dir_message_linking,
    paste0("comm_sturm_aligned_", code, ".csv")
  )

  write_csv(resid_aligned, resid_out)
  write_csv(comm_aligned, comm_out)

  aligned_total_2020 <- sturm_energy_2020(resid_aligned) +
    sturm_energy_2020(comm_aligned)

  aligned_fossil_2020 <- bind_rows(
    sturm_fossil_by_fuel_2020(resid_aligned),
    sturm_fossil_by_fuel_2020(comm_aligned)
  ) %>%
    group_by(fuel) %>%
    summarise(aligned = sum(value, na.rm = TRUE), .groups = "drop") %>%
    full_join(
      message_fossil_2020 %>% rename(message = value),
      by = "fuel"
    ) %>%
    mutate(
      across(c(aligned, message), ~ replace_na(.x, 0)),
      across(where(is.numeric), ~ round(.x, 4))
    )

  cat(
    "\nAligned STURM resid+comm energy 2020: ",
    round(aligned_total_2020, 4),
    " GWa\n",
    sep = ""
  )
  cat("Aligned fossils vs MESSAGE (2020 GWa):\n")
  print(aligned_fossil_2020)
  cat("Wrote:\n  ", resid_out, "\n  ", comm_out, "\n", sep = "")
}

cat("\nDone (special WEU experiment aligner).\n")
