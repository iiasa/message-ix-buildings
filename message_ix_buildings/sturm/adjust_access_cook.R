# Build data/access_cook.csv from access_cook_ssp2.csv.
# 1) lightoil (optional): cap/scale; 2020 fixed, smooth to 2080 caps.
# 2) electr (independent): +20% at 2100; 2020-2040 unchanged; smooth rise from 2040.
# USE_ORIGINAL_DATA = TRUE skips lightoil only; electr still applied when ADJUST_ELECTR = TRUE.

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  ca <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", ca, value = TRUE)
  if (length(file_arg)) {
    script_path <- sub("^--file=", "", file_arg[1])
    setwd(dirname(normalizePath(script_path)))
  } else {
    stop(
      "Cannot set working directory: run from RStudio, or ",
      "`Rscript adjust_access_cook.R` from the sturm folder."
    )
  }
}

REGION_CAP_LO <- 99.5
WORLD_CAP_2080 <- 120
WORLD_CAP_2100 <- 116
ANCHOR_YEAR <- 2020L
PIVOT_YEAR <- 2080L
END_YEAR <- 2100L
USE_ORIGINAL_DATA <- FALSE
ADJUST_ELECTR <- TRUE
ELECTR_END_BOOST <- 0.20
ELECTR_BLEND_START_YEAR <- 2040L

dir_message_linking <- file.path(getwd(), "message_linking")
source(file.path(dir_message_linking, "resolve_sturm_data_dir.R"))
data_dir <- resolve_sturm_data_dir()
path_in <- file.path(data_dir, "access_cook_ssp2.csv")
path_out <- file.path(data_dir, "access_cook.csv")
path_pdf <- file.path(dir_message_linking, "access_cook_before_after.pdf")
COOK_COMMODITIES <- c(
  "resid_cook_biomass",
  "resid_cook_non-comm",
  "resid_cook_electr",
  "resid_cook_lightoil"
)

if (!file.exists(path_in)) {
  stop("Missing input: ", path_in)
}

blend_weight <- function(year, pivot_year = PIVOT_YEAR) {
  if (year <= ANCHOR_YEAR) {
    return(0)
  }
  if (year >= pivot_year) {
    return(1)
  }
  (year - ANCHOR_YEAR) / (pivot_year - ANCHOR_YEAR)
}

electr_blend_weight <- function(
    year,
    start_year = ELECTR_BLEND_START_YEAR,
    end_year = END_YEAR) {
  if (year <= start_year) {
    return(0)
  }
  if (year >= end_year) {
    return(1)
  }
  (year - start_year) / (end_year - start_year)
}

world_cap_target <- function(year, world_sum_2020) {
  if (year <= ANCHOR_YEAR) {
    return(world_sum_2020)
  }
  if (year <= PIVOT_YEAR) {
    return(world_sum_2020 + (year - ANCHOR_YEAR) / (PIVOT_YEAR - ANCHOR_YEAR) * (WORLD_CAP_2080 - world_sum_2020))
  }
  if (year <= END_YEAR) {
    return(WORLD_CAP_2080 + (year - PIVOT_YEAR) / (END_YEAR - PIVOT_YEAR) * (WORLD_CAP_2100 - WORLD_CAP_2080))
  }
  WORLD_CAP_2100
}

#' Per-year lightoil targets: regional cap then global scale; blend from 2020 anchor.
adjust_lightoil_block <- function(block, commodity_lo = "resid_cook_lightoil") {
  lo <- block[block$commodity == commodity_lo, , drop = FALSE]
  if (!nrow(lo)) {
    return(list(block = block, lo_before = lo, lo_after = lo))
  }

  nodes <- sort(unique(lo$node))
  years <- sort(unique(lo$year))
  mat <- matrix(NA_real_, nrow = length(nodes), ncol = length(years),
                dimnames = list(nodes, as.character(years)))
  for (i in seq_len(nrow(lo))) {
    mat[lo$node[i], as.character(lo$year[i])] <- lo$value[i]
  }

  anchor <- mat[, as.character(ANCHOR_YEAR)]
  world_sum_2020 <- sum(anchor, na.rm = TRUE)
  mat_adj <- mat

  for (yr in years) {
    yr_chr <- as.character(yr)
    orig <- mat[, yr_chr]
    w <- blend_weight(yr)
    if (w <= 0) {
      next
    }

    capped <- pmin(orig, REGION_CAP_LO)
    cap_world <- world_cap_target(yr, world_sum_2020)
    s <- sum(capped, na.rm = TRUE)
    tgt <- if (s > cap_world && s > 0) capped * (cap_world / s) else capped
    mat_adj[, yr_chr] <- anchor + w * (tgt - anchor)
  }

  lo_after <- lo
  for (i in seq_len(nrow(lo_after))) {
    lo_after$value[i] <- mat_adj[lo_after$node[i], as.character(lo_after$year[i])]
  }

  block_out <- block
  idx <- block_out$commodity == commodity_lo
  block_out$value[idx] <- lo_after$value

  list(
    block = block_out,
    lo_before = lo,
    lo_after = lo_after,
    mat_before = mat,
    mat_after = mat_adj
  )
}

#' Electr: 2020-2040 unchanged; from 2040 smooth to +end_boost at 2100 (vs input at each year).
adjust_electr_block <- function(
    block,
    commodity_el = "resid_cook_electr",
    end_boost = ELECTR_END_BOOST,
    blend_start_year = ELECTR_BLEND_START_YEAR) {
  el <- block[block$commodity == commodity_el, , drop = FALSE]
  if (!nrow(el)) {
    return(list(block = block, el_before = el, el_after = el))
  }

  nodes <- sort(unique(el$node))
  years <- sort(unique(el$year))
  mat <- matrix(NA_real_, nrow = length(nodes), ncol = length(years),
                dimnames = list(nodes, as.character(years)))
  for (i in seq_len(nrow(el))) {
    mat[el$node[i], as.character(el$year[i])] <- el$value[i]
  }

  start_chr <- as.character(blend_start_year)
  if (!start_chr %in% colnames(mat)) {
    stop("electr blend start year ", blend_start_year, " missing in data")
  }
  anchor <- mat[, start_chr]
  mat_adj <- mat

  for (yr in years) {
    yr_chr <- as.character(yr)
    orig <- mat[, yr_chr]
    w <- electr_blend_weight(yr, start_year = blend_start_year)
    if (w <= 0) {
      next
    }
    mult <- 1 + end_boost * w
    tgt <- orig * mult
    mat_adj[, yr_chr] <- anchor + w * (tgt - anchor)
  }

  el_after <- el
  for (i in seq_len(nrow(el_after))) {
    el_after$value[i] <- mat_adj[el_after$node[i], as.character(el_after$year[i])]
  }

  block_out <- block
  idx <- block_out$commodity == commodity_el
  block_out$value[idx] <- el_after$value

  list(
    block = block_out,
    el_before = el,
    el_after = el_after
  )
}

sum_cook_by_node_year <- function(df, commodities = COOK_COMMODITIES) {
  sub <- df[df$commodity %in% commodities, , drop = FALSE]
  if (!nrow(sub)) {
    return(sub)
  }
  agg <- aggregate(value ~ node + year, data = sub, FUN = sum, na.rm = TRUE)
  agg$commodity <- "resid_cook_total"
  agg$level <- "final"
  agg$time <- "year"
  agg$unit <- "GWa"
  agg[, c("node", "commodity", "level", "year", "time", "value", "unit")]
}

max_value_diff <- function(b, a) {
  if (!nrow(b) && !nrow(a)) {
    return(0)
  }
  m <- merge(
    b[, c("node", "year", "value"), drop = FALSE],
    a[, c("node", "year", "value"), drop = FALSE],
    by = c("node", "year"),
    all = FALSE
  )
  if (!nrow(m)) {
    return(Inf)
  }
  max(abs(m$value.x - m$value.y), na.rm = TRUE)
}

prep_plot_series <- function(df, series) {
  w <- aggregate(value ~ year, data = df, FUN = sum, na.rm = TRUE)
  w$region_label <- "World"
  w$series <- series
  r <- df
  r$region_label <- sub("^R12_", "", r$node)
  r$series <- series
  rbind(
    w[, c("year", "region_label", "value", "series")],
    r[, c("year", "region_label", "value", "series")]
  )
}

plot_access_cook_adjustment <- function(
    before,
    after,
    path_pdf,
    commodities = COOK_COMMODITIES) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 not installed; skipping adjustment plots.")
    return(invisible(NULL))
  }

  dir.create(dirname(path_pdf), recursive = TRUE, showWarnings = FALSE)

  series_cols <- c(before = "#d62728", after = "#1f77b4")
  series_lty <- c(before = "dashed", after = "solid")

  plot_one <- function(b, a, title_label, subtitle_extra = NULL) {
    d <- rbind(prep_plot_series(b, "before"), prep_plot_series(a, "after"))
    d$series <- factor(d$series, levels = c("before", "after"))
    sub_title <- "Red dashed = SSP2 input; blue solid = adjusted"
    if (!is.null(subtitle_extra)) {
      sub_title <- paste(sub_title, subtitle_extra, sep = "\n")
    }

    p_world <- ggplot2::ggplot(
      d[d$region_label == "World", , drop = FALSE],
      ggplot2::aes(x = year, y = value, color = series, linetype = series)
    ) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::scale_color_manual(values = series_cols) +
      ggplot2::scale_linetype_manual(values = series_lty) +
      ggplot2::labs(
        title = paste0(title_label, " — World (R12 sum, GWa)"),
        subtitle = sub_title,
        x = NULL, y = "GWa", color = NULL, linetype = NULL
      ) +
      ggplot2::theme_bw(base_size = 10) +
      ggplot2::theme(
        legend.position = "bottom",
        plot.subtitle = ggplot2::element_text(size = 8, hjust = 0)
      )

    p_reg <- ggplot2::ggplot(
      d[d$region_label != "World", , drop = FALSE],
      ggplot2::aes(x = year, y = value, color = series, linetype = series)
    ) +
      ggplot2::geom_line(linewidth = 0.55) +
      ggplot2::facet_wrap(~region_label, ncol = 4, scales = "free_y") +
      ggplot2::scale_color_manual(values = series_cols) +
      ggplot2::scale_linetype_manual(values = series_lty) +
      ggplot2::labs(
        title = paste0(title_label, " — R12 regions (GWa)"),
        x = "Year", y = "GWa", color = NULL, linetype = NULL
      ) +
      ggplot2::theme_bw(base_size = 8) +
      ggplot2::theme(legend.position = "none", strip.text = ggplot2::element_text(face = "bold"))

    if (requireNamespace("patchwork", quietly = TRUE)) {
      patchwork::wrap_plots(p_world, p_reg, ncol = 2, widths = c(1.2, 1.8)) +
        patchwork::plot_annotation(
          title = "access_cook: SSP2 input (before) vs adjusted (after)",
          theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13))
        )
    } else {
      p_world
    }
  }

  commodity_label <- function(commodity) {
    if (commodity == "resid_cook_total") {
      return("resid_cook total (4 fuels)")
    }
    commodity
  }

  plot_commodities <- c(commodities, "resid_cook_total")
  n_pages <- length(plot_commodities)

  grDevices::pdf(path_pdf, width = 12, height = 6.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  for (commodity in commodities) {
    b <- before[before$commodity == commodity, , drop = FALSE]
    a <- after[after$commodity == commodity, , drop = FALSE]
    print(plot_one(b, a, commodity_label(commodity)))
  }

  b_tot <- sum_cook_by_node_year(before, commodities)
  a_tot <- sum_cook_by_node_year(after, commodities)
  print(plot_one(b_tot, a_tot, commodity_label("resid_cook_total")))

  message("Wrote ", path_pdf, " (", n_pages, " pages)")
  invisible(path_pdf)
}

# --- run ----------------------------------------------------------------------

raw <- read.csv(path_in, stringsAsFactors = FALSE)
before <- raw
after <- before

if (isTRUE(USE_ORIGINAL_DATA)) {
  message("lightoil: using original SSP2 (no adjustment).")
} else {
  adj_lo <- adjust_lightoil_block(after)
  after <- adj_lo$block

  lo_chk <- adj_lo$lo_after
  for (yr in c(ANCHOR_YEAR, PIVOT_YEAR, END_YEAR)) {
    sub <- lo_chk[lo_chk$year == yr, , drop = FALSE]
    message(
      "lightoil ", yr,
      ": sum=", round(sum(sub$value), 2),
      ", max=", round(max(sub$value), 2),
      ", n>=100=", sum(sub$value >= 100)
    )
  }

  anchor_ok <- all(abs(
    adj_lo$lo_before[adj_lo$lo_before$year == ANCHOR_YEAR, "value"] -
      adj_lo$lo_after[adj_lo$lo_after$year == ANCHOR_YEAR, "value"]
  ) < 1e-9)
  if (!anchor_ok) {
    warning("2020 lightoil values changed; expected unchanged.")
  }
}

if (isTRUE(ADJUST_ELECTR)) {
  adj_el <- adjust_electr_block(after)
  after <- adj_el$block

  el_2020 <- adj_el$el_after[adj_el$el_after$year == ANCHOR_YEAR, , drop = FALSE]
  el_2100 <- adj_el$el_after[adj_el$el_after$year == END_YEAR, , drop = FALSE]
  el_2100_in <- adj_el$el_before[adj_el$el_before$year == END_YEAR, , drop = FALSE]
  m <- merge(el_2100, el_2100_in, by = "node", suffixes = c(".adj", ".in"))
  ratio <- m$value.adj / m$value.in
  message(
    "electr ", END_YEAR, ": world sum ",
    round(sum(el_2100$value), 2), " vs input ",
    round(sum(el_2100_in$value), 2),
    " (boost ", round(100 * (sum(el_2100$value) / sum(el_2100_in$value) - 1), 1), "%)"
  )
  el_flat <- adj_el$el_before$year <= ELECTR_BLEND_START_YEAR
  if (!all(abs(
    adj_el$el_before[el_flat, "value"] - adj_el$el_after[el_flat, "value"]
  ) < 1e-9)) {
    warning(
      "electr values changed for year <= ", ELECTR_BLEND_START_YEAR,
      "; expected unchanged through ", ELECTR_BLEND_START_YEAR, "."
    )
  }
}

write.csv(after, path_out, row.names = FALSE)
message("Wrote ", path_out)

if (!identical(before, after)) {
  plot_access_cook_adjustment(before, after, path_pdf)
} else {
  message("No adjustments applied; skipping PDF.")
}
