# MIX price responder — constant-elasticity demand adjustment for usages so far with fixed exogenous demands.
# (including cooking and appliances; non-commercial biomass does not apply price elasticity)
# Q_adj = Q_base * (P / P_ref)^epsilon  (epsilon < 0: higher price -> lower demand)
#
# P_ref — input_prices_R12_default.csv (user-prepared baseline; read only, not smoothed)
# P     — input_prices_R12.csv (scenario prices; may be smoothed in runner before call)

#' Map R12 MESSAGE node to R11 price node (price files use R11_*).
r12_node_to_r11_price_node <- function(node) {
  out <- sub("^R12_", "R11_", node)
  sub("R11_RCPA$", "R11_CPA", out)
}

#' Demand commodities that use a given price fuel.
responder_commodities_for_price_fuel <- function(fuel) {
  switch(
    fuel,
    biomass = "resid_cook_biomass",
    lightoil = "resid_cook_lightoil",
    electr = c("resid_cook_electr", "resid_apps_electr"),
    character(0)
  )
}

#' Map demand commodity to price file commodity.
responder_commodity_to_price_commodity <- function(commodity) {
  switch(
    commodity,
    resid_cook_biomass = "biomass",
    resid_cook_lightoil = "lightoil",
    resid_cook_electr = "electr",
    resid_apps_electr = "electr",
    NULL
  )
}

read_prices_csv <- function(path) {
  p <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(p) > 0L && (names(p)[1L] == "" || grepl("^X", names(p)[1L]))) {
    p <- p[, -1L, drop = FALSE]
  }
  p
}

#' Centered moving average of lvl on scenario P only (irregular years OK).
#' Does not read or write input_prices_R12_default.csv.
#' @param window odd integer >= 2; use 0 or 1 to skip smoothing
smooth_prices_lvl <- function(prices, window = 5L) {
  if (window < 2L || !"lvl" %in% names(prices) || !nrow(prices)) {
    return(prices)
  }
  window <- as.integer(window)
  if (window %% 2L == 0L) {
    window <- window + 1L
  }

  out <- prices
  groups <- split(seq_len(nrow(prices)), interaction(prices$node, prices$commodity, drop = TRUE))

  for (idx in groups) {
    sub <- prices[idx, , drop = FALSE]
    ord <- order(sub$year)
    sub <- sub[ord, , drop = FALSE]
    w <- min(window, nrow(sub))
    if (w < 2L) {
      next
    }
    smoothed <- stats::filter(sub$lvl, rep(1 / w, w), sides = 2)
    na_fill <- is.na(smoothed)
    if (any(na_fill)) {
      smoothed[na_fill] <- sub$lvl[na_fill]
    }
    out[idx[ord], "lvl"] <- as.numeric(smoothed)
  }

  out
}

prep_prices_long <- function(prices, price_fuels) {
  p <- prices[prices$commodity %in% price_fuels, c("node", "commodity", "year", "lvl"), drop = FALSE]
  names(p)[1L] <- "node_price"
  p
}

#' TRUE if P and P_ref share the same lvl on all elasticity fuels (node, commodity, year).
prices_lvl_identical <- function(
    prices,
    prices_ref,
    price_fuels = c("biomass", "lightoil", "electr"),
    tol = 1e-9) {
  p <- prep_prices_long(prices, price_fuels)
  pref <- prep_prices_long(prices_ref, price_fuels)
  if (!nrow(p) && !nrow(pref)) {
    return(TRUE)
  }
  if (!nrow(p) || !nrow(pref)) {
    return(FALSE)
  }
  m <- merge(
    p,
    pref,
    by = c("node_price", "commodity", "year"),
    suffixes = c("_p", "_ref")
  )
  if (nrow(m) != nrow(p) || nrow(m) != nrow(pref)) {
    return(FALSE)
  }
  diffs <- abs(m$lvl_p - m$lvl_ref)
  if (!any(is.finite(diffs))) {
    return(FALSE)
  }
  max(diffs, na.rm = TRUE) <= tol
}

#' @param demand data.frame scenario demand before elasticity (node, commodity, year, value GWa)
#' @param prices data.frame P — input_prices_R12 (node, commodity, year, lvl)
#' @param prices_ref data.frame P_ref — input_prices_R12_default
#' @param elasticity named numeric: biomass, lightoil, electr (negative typical)
apply_mix_responder_demand <- function(demand, prices, prices_ref, elasticity) {
  price_fuels <- intersect(names(elasticity), c("biomass", "lightoil", "electr"))
  if (!length(price_fuels)) {
    stop("elasticity must name at least one of: biomass, lightoil, electr")
  }

  prices_ref <- prep_prices_long(prices_ref, price_fuels)
  names(prices_ref)[names(prices_ref) == "lvl"] <- "lvl_ref"

  prices_cur <- prep_prices_long(prices, price_fuels)

  demand$price_commodity <- vapply(
    demand$commodity,
    function(c) {
      pc <- responder_commodity_to_price_commodity(c)
      if (is.null(pc)) NA_character_ else pc
    },
    character(1L)
  )
  demand$node_price <- r12_node_to_r11_price_node(demand$node)
  demand$epsilon <- elasticity[demand$price_commodity]

  out <- merge(
    demand,
    prices_ref,
    by.x = c("node_price", "price_commodity", "year"),
    by.y = c("node_price", "commodity", "year"),
    all.x = TRUE
  )
  out <- merge(
    out,
    prices_cur,
    by.x = c("node_price", "price_commodity", "year"),
    by.y = c("node_price", "commodity", "year"),
    all.x = TRUE
  )

  ratio <- out$lvl / out$lvl_ref
  ok <- !is.na(out$epsilon) & !is.na(ratio) & out$lvl_ref > 0 & ratio > 0
  mult <- rep(1, nrow(out))
  mult[ok] <- ratio[ok]^out$epsilon[ok]
  out$value <- out$value * mult

  drop_cols <- c("price_commodity", "node_price", "epsilon", "lvl_ref", "lvl")
  out <- out[, setdiff(names(out), drop_cols), drop = FALSE]
  col_order <- c("node", "commodity", "level", "year", "time", "value", "unit")
  out[, intersect(col_order, names(out)), drop = FALSE]
}
