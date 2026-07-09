# GLANCE runner placeholder
# ACCESS cook + GLANCE appliances (glance_app.csv), then constant-elasticity demand response.

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  ca <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", ca, value = TRUE)
  if (length(file_arg)) {
    script_path <- sub("^--file=", "", file_arg[1])
    setwd(dirname(normalizePath(script_path)))
  } else {
    stop("Cannot set working directory: run from RStudio, or `Rscript run_GLANCE_placeholder.R` from the sturm folder.")
  }
}

# -----------------------------------------------------------------------------
# Price elasticity settings (constant elasticity of demand w.r.t. fuel price)
# Q_adj = Q_base * (P / P_ref)^epsilon
#
# P_ref — data/input_prices_R12_default.csv (read as prepared; never smoothed or overwritten)
# P     — data/input_prices_R12.csv (scenario; optional centered MA before elasticity)
# -----------------------------------------------------------------------------

ELASTICITY_REF_YEAR <- 2020L  # shown on demand-plot subtitles

# Per fuel (MESSAGE commodity in price files)
ELASTICITY_BY_FUEL <- c(
  biomass = -0.30,
  lightoil = -0.70,  # more sensitive to lightoil price
  gas = -0.70,       # same elasticity as lightoil
  electr = -0.20
)

# Centered moving average on P only (odd window >= 3; 0 = off). P_ref file is unchanged.
# Skipped automatically when P and P_ref have identical lvl for elasticity fuels.
PRICE_SMOOTH_WINDOW <- 5L

# resid_cook_non-comm has no price commodity -> unchanged (no entry above)

GLANCE_WRITE_PLOTS <- TRUE

# Years (aligned with run_STURM_bmt_*); glance_app.csv rows outside this set are dropped.
years_to_run <- c(
  seq(2020, 2060, 5),
  seq(2070, 2100, 10)
)

# -----------------------------------------------------------------------------

dir_message_linking <- file.path(getwd(), "message_linking")
dir.create(dir_message_linking, recursive = TRUE, showWarnings = FALSE)
source(file.path(dir_message_linking, "load_scenario_config.R"))
source(file.path(dir_message_linking, "run_MIX_responder.R"))
if (isTRUE(GLANCE_WRITE_PLOTS)) {
  source(file.path(dir_message_linking, "responder_plots.R"))
}
scenarios <- load_scenarios()

data_dir <- file.path(getwd(), "data")
path_cook <- file.path(data_dir, "access_cook.csv")
path_glance_app <- file.path(data_dir, "glance_app.csv")
path_prices <- file.path(data_dir, "input_prices_R12.csv")
path_prices_ref <- file.path(data_dir, "input_prices_R12_default.csv")

missing <- c(
  if (!file.exists(path_cook)) path_cook,
  if (!file.exists(path_glance_app)) path_glance_app,
  if (!file.exists(path_prices)) path_prices,
  if (!file.exists(path_prices_ref)) path_prices_ref
)
if (length(missing)) {
  stop(
    "Missing GLANCE inputs under sturm/data:\n",
    paste0("  ", missing, collapse = "\n"),
    "\nAdd access_cook.csv, glance_app.csv, input_prices_R12.csv, ",
    "and input_prices_R12_default.csv, then re-run."
  )
}

#' GLANCE export uses resids_*; MESSAGE/STURM use resid_*.
normalize_glance_commodity <- function(commodity) {
  sub("^resids_", "resid_", commodity) # YJ: Pin XZ to rename
}

#' Energy rows (GWa) for one scenario key from glance_app.csv.
load_glance_app_demand <- function(glance_app, scenario, years = years_to_run) {
  if (!"scenario" %in% names(glance_app)) {
    stop("glance_app.csv must have a 'scenario' column")
  }
  out <- glance_app[
    glance_app$scenario == scenario &
      glance_app$unit == "GWa" &
      glance_app$year %in% years,
    ,
    drop = FALSE
  ]
  if (!nrow(out)) {
    warning(
      "GLANCE: no GWa rows for scenario '", scenario,
      "' in glance_app.csv (available: ",
      paste(sort(unique(glance_app$scenario)), collapse = ", "),
      ")"
    )
  }
  out$commodity <- normalize_glance_commodity(out$commodity)
  out$scenario <- NULL
  out[, c("node", "commodity", "level", "year", "time", "value", "unit"), drop = FALSE]
}

demand_cook <- read.csv(path_cook, stringsAsFactors = FALSE)
glance_app_all <- read.csv(path_glance_app, stringsAsFactors = FALSE)

resolve_price_path_p <- function(scenario, path_p) {
  p_candidates <- c(
    file.path(data_dir, paste0("input_prices_R12_", scenario, ".csv")),
    path_p
  )
  p_found <- p_candidates[file.exists(p_candidates)]
  if (!length(p_found)) {
    stop("No P price file found for scenario ", scenario)
  }
  p_found[[1L]]
}

for (s in scenarios) {
  demand_scenario <- rbind(
    demand_cook,
    load_glance_app_demand(glance_app_all, s)
  )

  path_p <- resolve_price_path_p(s, path_prices)
  prices <- read_prices_csv(path_p)
  prices_ref <- read_prices_csv(path_prices_ref)

  price_smooth_window <- PRICE_SMOOTH_WINDOW
  if (PRICE_SMOOTH_WINDOW >= 2L) {
    if (prices_lvl_identical(prices, prices_ref, names(ELASTICITY_BY_FUEL))) {
      price_smooth_window <- 0L
      message(
        "GLANCE: P and P_ref identical on elasticity fuels — skipping price smoothing (",
        s, ")"
      )
    } else {
      prices <- smooth_prices_lvl(prices, window = PRICE_SMOOTH_WINDOW)
    }
  }

  demand_responded <- apply_mix_responder_demand(
    demand = demand_scenario,
    prices = prices,
    prices_ref = prices_ref,
    elasticity = ELASTICITY_BY_FUEL
  )
  demand_responded <- demand_responded[
    order(demand_responded$node, demand_responded$commodity, demand_responded$year),
    ,
    drop = FALSE
  ]

  out_glance <- file.path(dir_message_linking, paste0("resid_comm_glance_", s, ".csv"))
  write.csv(demand_responded, out_glance, row.names = FALSE)
  smooth_note <- if (price_smooth_window >= 2L) {
    paste0(", P smoothed (window ", price_smooth_window, ")")
  } else {
    ""
  }
  message(
    "GLANCE: wrote ", basename(out_glance),
    " (P: ", basename(path_p),
    ", P_ref: ", basename(path_prices_ref), smooth_note, ")"
  )

  if (isTRUE(GLANCE_WRITE_PLOTS)) {
    path_pdf <- file.path(
      dir_message_linking,
      paste0("responder_comparison_", s, ".pdf")
    )
    write_responder_comparison_pdf(
      demand_scenario = demand_scenario,
      demand_responded = demand_responded,
      prices = prices,
      prices_ref = prices_ref,
      scenario = s,
      path_pdf = path_pdf,
      path_prices = path_p,
      path_prices_ref = path_prices_ref,
      fuels = names(ELASTICITY_BY_FUEL),
      elasticity_ref_year = ELASTICITY_REF_YEAR,
      price_smooth_window = price_smooth_window
    )
  }
}

message("GLANCE done (", length(scenarios), " scenario(s)).")
