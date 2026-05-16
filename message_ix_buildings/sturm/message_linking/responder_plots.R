# Before/after MIX responder plots

U_EJ_GWA <- 31.71

gwa_to_ej_responder <- function(value_GWa) {
  value_GWa / U_EJ_GWA
}

#' Sum demand (GWa) by R12 node and year for one commodity; add EJ column.
aggregate_responder_demand_region <- function(demand, commodity) {
  d <- demand[demand$commodity == commodity & grepl("^R12_", demand$node), , drop = FALSE]
  if (!nrow(d)) {
    return(data.frame(
      year = integer(0),
      region = character(0),
      region_label = character(0),
      value_EJ = numeric(0)
    ))
  }
  agg <- aggregate(value ~ node + year, data = d, FUN = sum, na.rm = TRUE)
  agg$region_label <- sub("^R12_", "", agg$node)
  agg$value_EJ <- gwa_to_ej_responder(agg$value)
  agg$region <- agg$node
  agg[, c("year", "region", "region_label", "value_EJ")]
}

prep_responder_demand_plot_data <- function(demand_scenario, demand_responded, commodity) {
  before <- aggregate_responder_demand_region(demand_scenario, commodity)
  after <- aggregate_responder_demand_region(demand_responded, commodity)
  if (!nrow(before) && !nrow(after)) {
    return(list(world = data.frame(), regions = data.frame(), r12_order = character(0)))
  }

  before$series <- "respond before"
  after$series <- "respond after"

  world <- rbind(
    if (nrow(before)) {
      w <- aggregate(value_EJ ~ year, data = before, FUN = sum, na.rm = TRUE)
      w$series <- "respond before"
      w$region <- "World"
      w
    },
    if (nrow(after)) {
      w <- aggregate(value_EJ ~ year, data = after, FUN = sum, na.rm = TRUE)
      w$series <- "respond after"
      w$region <- "World"
      w
    }
  )

  regions <- rbind(before, after)
  r12_order <- sort(unique(regions$region_label))

  list(world = world, regions = regions, r12_order = r12_order)
}

prep_responder_price_series <- function(prices, fuel, series_label) {
  p <- prices[prices$commodity == fuel & grepl("^R11_", prices$node), , drop = FALSE]
  if (!nrow(p)) {
    return(data.frame(
      year = integer(0),
      region_label = character(0),
      price_lvl = numeric(0),
      series = character(0)
    ))
  }
  p$region_label <- sub("^R11_", "", p$node)
  out <- p[, c("year", "region_label", "lvl")]
  names(out)[3] <- "price_lvl"
  out$series <- series_label
  out
}

prep_responder_price_plot_data <- function(prices, prices_ref, fuel) {
  p_cur <- prep_responder_price_series(prices, fuel, "P")
  p_ref <- prep_responder_price_series(prices_ref, fuel, "P_ref")

  if (!nrow(p_cur) && !nrow(p_ref)) {
    return(list(world = data.frame(), regions = data.frame(), r11_order = character(0)))
  }

  regions <- rbind(p_ref, p_cur)
  r11_order <- sort(unique(regions$region_label))

  world_p <- if (nrow(p_cur)) {
    w <- aggregate(price_lvl ~ year, data = p_cur, FUN = mean, na.rm = TRUE)
    w$series <- "P"
    w$region <- "World"
    w
  } else {
    NULL
  }

  world_pref <- if (nrow(p_ref)) {
    w <- aggregate(price_lvl ~ year, data = p_ref, FUN = mean, na.rm = TRUE)
    w$series <- "P_ref"
    w$region <- "World"
    w
  } else {
    NULL
  }

  world <- rbind(world_pref, world_p)

  list(world = world, regions = regions, r11_order = r11_order)
}

plot_responder_demand_page <- function(plot_data, commodity, scenario, elasticity_ref_year) {
  if (!nrow(plot_data$world)) {
    warning("No demand data to plot for ", commodity)
    return(NULL)
  }

  world_df <- plot_data$world
  reg_df <- plot_data$regions
  reg_df$region_label <- factor(reg_df$region_label, levels = plot_data$r12_order)

  series_cols <- c("respond before" = "#80B1D3", "respond after" = "#6A3D9A")
  series_lty <- c("respond before" = "solid", "respond after" = "solid")

  plot_subtitle <- paste0(
    "Scenario ", scenario, " | EJ/yr",
    "\nCommodity: ", commodity,
    "\nELASTICITY_REF_YEAR: ", elasticity_ref_year
  )

  p_world <- ggplot2::ggplot(
    world_df,
    ggplot2::aes(x = year, y = value_EJ, color = series, linetype = series)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_color_manual(values = series_cols, breaks = names(series_cols)) +
    ggplot2::scale_linetype_manual(values = series_lty, breaks = names(series_cols)) +
    ggplot2::labs(
      title = paste0(commodity, " - World (R12 sum)"),
      subtitle = plot_subtitle,
      x = NULL,
      y = "EJ/yr",
      color = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 7, lineheight = 0.85, hjust = 0)
    )

  p_regions <- ggplot2::ggplot(
    reg_df,
    ggplot2::aes(x = year, y = value_EJ, color = series, linetype = series)
  ) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::facet_wrap(~region_label, ncol = 3, nrow = 4, scales = "free_y") +
    ggplot2::scale_color_manual(values = series_cols, breaks = names(series_cols)) +
    ggplot2::scale_linetype_manual(values = series_lty, breaks = names(series_cols)) +
    ggplot2::labs(
      title = paste0(commodity, " - R12 regions"),
      subtitle = plot_subtitle,
      x = "Year",
      y = "EJ/yr",
      color = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 7, lineheight = 0.85, hjust = 0),
      strip.text = ggplot2::element_text(face = "bold")
    )

  responder_wrap_world_regions(p_world, p_regions, paste0("MIX responder demand - ", scenario))
}

plot_responder_price_page <- function(
    plot_data,
    fuel,
    scenario,
    path_prices,
    path_prices_ref,
    price_smooth_window = 0L) {
  if (!nrow(plot_data$world)) {
    warning("No price data to plot for ", fuel)
    return(NULL)
  }

  world_df <- plot_data$world
  reg_df <- plot_data$regions
  reg_df$region_label <- factor(reg_df$region_label, levels = plot_data$r11_order)

  series_cols <- c("P_ref" = "#80B1D3", "P" = "#D95F02")
  series_lty <- c("P_ref" = "22", "P" = "solid")

  smooth_note <- if (price_smooth_window >= 2L) {
    paste0("\nP smoothed (centered MA, window ", price_smooth_window, "); P_ref as in file")
  } else {
    "\nP_ref as in file (not smoothed)"
  }
  plot_subtitle <- paste0(
    "Scenario ", scenario, " | price lvl | commodity: ", fuel, "\n",
    "P_ref: ", basename(path_prices_ref), "\n",
    "P: ", basename(path_prices),
    "\nWorld = R12 unweighted mean (12 regions)",
    smooth_note
  )

  p_world <- ggplot2::ggplot(
    world_df,
    ggplot2::aes(x = year, y = price_lvl, color = series, linetype = series)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_color_manual(values = series_cols, breaks = names(series_cols)) +
    ggplot2::scale_linetype_manual(values = series_lty, breaks = names(series_cols)) +
    ggplot2::labs(
      title = paste0(fuel, " price - World (R12 mean)"),
      subtitle = plot_subtitle,
      x = NULL,
      y = "Price lvl",
      color = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 7, lineheight = 0.85, hjust = 0)
    )

  p_regions <- ggplot2::ggplot(
    reg_df,
    ggplot2::aes(x = year, y = price_lvl, color = series, linetype = series)
  ) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::facet_wrap(~region_label, ncol = 3, nrow = 4, scales = "free_y") +
    ggplot2::scale_color_manual(values = series_cols, breaks = names(series_cols)) +
    ggplot2::scale_linetype_manual(values = series_lty, breaks = names(series_cols)) +
    ggplot2::labs(
      title = paste0(fuel, " price - R12 regions"),
      subtitle = plot_subtitle,
      x = "Year",
      y = "Price lvl",
      color = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 7, lineheight = 0.85, hjust = 0),
      strip.text = ggplot2::element_text(face = "bold")
    )

  responder_wrap_world_regions(p_world, p_regions, paste0("MIX responder price - ", scenario))
}

responder_wrap_world_regions <- function(p_world, p_regions, title) {
  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(p_world, p_regions, ncol = 2, widths = c(1.35, 1.65)) +
      patchwork::plot_annotation(
        title = title,
        theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14))
      )
  } else if (requireNamespace("gridExtra", quietly = TRUE)) {
    gridExtra::arrangeGrob(p_world, p_regions, ncol = 2, widths = c(1.35, 1.65))
  } else {
    warning("Install patchwork or gridExtra; returning world panel only.")
    p_world
  }
}

#' PDF: for each price fuel — price page, then one page per linked demand commodity.
write_responder_comparison_pdf <- function(
    demand_scenario,
    demand_responded,
    prices,
    prices_ref,
    scenario,
    path_pdf,
    path_prices,
    path_prices_ref,
    fuels,
    elasticity_ref_year = 2020L,
    price_smooth_window = 0L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 not installed; skipping responder plots.")
    return(invisible(NULL))
  }

  dir.create(dirname(path_pdf), recursive = TRUE, showWarnings = FALSE)
  grDevices::pdf(path_pdf, width = 11, height = 6.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  for (fuel in fuels) {
    price_data <- prep_responder_price_plot_data(prices, prices_ref, fuel)
    page <- plot_responder_price_page(
      price_data,
      fuel,
      scenario,
      path_prices,
      path_prices_ref,
      price_smooth_window = price_smooth_window
    )
    if (!is.null(page)) {
      print(page)
    }

    commodities <- responder_commodities_for_price_fuel(fuel)
    commodities <- commodities[commodities %in% unique(demand_scenario$commodity)]
    for (commodity in commodities) {
      demand_data <- prep_responder_demand_plot_data(
        demand_scenario,
        demand_responded,
        commodity
      )
      page <- plot_responder_demand_page(
        demand_data,
        commodity,
        scenario,
        elasticity_ref_year
      )
      if (!is.null(page)) {
        print(page)
      }
    }
  }

  message("Responder plots — wrote ", path_pdf)
  invisible(path_pdf)
}
