# MESSAGEix-Buildings - color schemes

library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(sf)
library(rnaturalearth)
library(viridis)  # Load the viridis package

source("STURM_output/C00_plots_settings.R")

plot_settings <- list(
  "size_axis" = 5, # axes
  "size_title" = 24, # axes and legend
  "size_text" = 24,
  "big_size_title" = 40, # axes and legend
  "big_size_text" = 40,
  "small_size_text" = 12,
  "small_size_title" = 18,
  "width" = 16, #width cm
  "height" = 1 * 16, #height cm
  "dpi" = 300, #DPI
  "font_family" = "Arial",
  "colors" = c(colors_efficiency, colors_fuels,
    colors_countries, colors_cost, colors_insulation, colors_primary),
  "rename" = c(rename_eneff, rename_fuels, rename_countries,
    rename_hh, rename_cost, rename_insulation, rename_primary),
  "order" = c(order_fuels, order_eneff, order_countries,
    order_hh, order_cost, order_insulation, order_primary)
)

message_building_theme_presentation <- theme_minimal() +
    theme(text = element_text(size = plot_settings[["big_size_text"]],
                              family = plot_settings[["font_family"]]),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = plot_settings[["big_size_text"]]),
          axis.text.y = element_text(size = plot_settings[["big_size_text"]]),
          axis.title.y = element_blank(),
          axis.title = element_text(hjust = 0,
                                    size = plot_settings[["big_size_title"]]),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = plot_settings[["big_size_text"]]),
          legend.position = "right",
          # Top, Right, Bottom and Left margin
          plot.margin = margin(t = 0,
                      r = 0,
                      b = 0,
                      l = 0),
          axis.line.x =
            element_line(colour = "black", size = 1.5, linetype = "solid"),
          axis.line.y =
            element_line(colour = "black", size = 1.5, linetype = "solid")
        )

message_building_theme <- theme_minimal() +
    theme(text = element_text(size = plot_settings[["size_text"]],
                              family = plot_settings[["font_family"]]),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = plot_settings[["size_text"]]),
          axis.text.y = element_text(size = plot_settings[["size_text"]]),
          axis.title.y = element_blank(),
          axis.title = element_text(hjust = 0,
                                    size = plot_settings[["size_title"]]),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = plot_settings[["size_text"]]),
          legend.position = "right",
          # Top, Right, Bottom and Left margin
          plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.line.x =
          element_line(colour = "black", size = 1.5, linetype = "solid"),
        axis.line.y =
          element_line(colour = "black", size = 1.5, linetype = "solid")
        )

message_building_subplot_theme <- theme_minimal() +
    theme(text = element_text(size = plot_settings[["small_size_text"]],
                              family = plot_settings[["font_family"]]),
          plot.title = element_text(size = plot_settings[["small_size_title"]]),
          axis.title = element_text(hjust = 0,
                          size = plot_settings[["small_size_title"]]),
          axis.title.x = element_blank(),
          strip.text.x = element_text(size = plot_settings[["small_size_title"]]),
          axis.text.x = element_text(size = plot_settings[["small_size_text"]]),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = plot_settings[["small_size_text"]]),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = plot_settings[["small_size_text"]]),
          legend.position = "bottom",
          # Top, Right, Bottom and Left margin
          plot.margin = margin(t = 0,
                      r = 0,
                      b = 0,
                      l = 0)
          )


# Function to create Message-ix-Buidling Figures

# Function to build and save histogram
plot_histogram <- function(data, column, save_path) {
  # Check if the column exists in the data
  if (!column %in% colnames(data)) {
    stop(sprintf("Column '%s' does not exist in the data", column))
  }

  # Create the histogram plot
  p <- data %>%
    ggplot(aes(x = .data[[column]])) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = sprintf("Histogram of %s", column), x = column, y = "Frequency") +
    message_building_theme

  # Save the plot to the specified path
  ggsave(filename = save_path, plot = p,
    width = plot_settings[["width"]],
    height = plot_settings[["height"]],
    dpi = plot_settings[["dpi"]])
}


plot_stacked_bars <- function(data,
                              fill_column,
                              x_column,
                              y_column,
                              save_path,
                              orientation = "vertical"
                              ) {

  p <- data %>%
    mutate(!!fill_column := plot_settings[["rename"]][.data[[fill_column]]]) %>%
    mutate(!!sym(fill_column) := factor(!!sym(fill_column), levels = rev(plot_settings[["order"]]))) %>%
    ggplot(aes(x = .data[[x_column]], y = .data[[y_column]])) +
    geom_bar(stat = "identity", aes(fill = .data[[fill_column]])) +
    scale_fill_manual(values = plot_settings[["colors"]]) +
    scale_y_continuous(labels = percent_format()) +  # Formatting y-axis as percentage
    message_building_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

  if (orientation == "horizontal") {
    p <- p + coord_flip() +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5))
  }


  ggsave(save_path, plot = p, width = plot_settings[["width"]],
          height = plot_settings[["height"]],
          dpi = plot_settings[["dpi"]])
}




plot_stacked_areas <- function(data,
                              x_column,
                              y_column,
                              fill_column,
                              subplot_column,
                              y_label = "",
                              save_path = NULL,
                              ncol = 4,
                              percent = TRUE,
                              vertical = NULL,
                              y_label_suffix = "",
                              presentation = FALSE,
                              legend = TRUE) {

  # Create the stacked area plot

  if (all(unique(data[[subplot_column]]) %in% names(plot_settings[["rename"]]))) {
    data <- data %>%
      mutate(!!subplot_column := plot_settings[["rename"]][.data[[subplot_column]]])
  }

  group <- c(x_column, fill_column, subplot_column)
  p <- data %>%
    mutate(!!fill_column := plot_settings[["rename"]][.data[[fill_column]]]) %>%
    group_by(across(all_of(group))) %>%
    summarise(total = sum(.data[[y_column]])) %>%
    ungroup()

  if (percent) {
    p <- p %>%
      group_by_at(c(x_column, subplot_column)) %>%
      mutate(total = total / sum(total)) %>%
      ungroup()
    scales <- NULL
  } else {
    scales <- "free_y"
  }
  p <- p %>%
    mutate(!!sym(fill_column) := factor(!!sym(fill_column),
      levels = rev(plot_settings[["order"]]))) %>%
    ggplot(aes(x = .data[[x_column]],
               y = total,
               fill = .data[[fill_column]])) +
    geom_area(position = "stack")

  if (!is.null(vertical)) {
    p <- p +
      geom_vline(xintercept = vertical, size = 1.5, color = "red")
  }
  p <- p +
    facet_wrap(subplot_column, ncol = ncol, scales = scales) +
    scale_fill_manual(values = plot_settings[["colors"]]) +
    labs(title = y_label,
         fill = str_replace_all(str_to_title(fill_column), "_", " "))
  
  if (length(unique(data[["region_bld"]])) > 1) {
    p <- p +
      message_building_subplot_theme

    } else {
      p <- p
      if (presentation) {
        p <- p +
          message_building_theme_presentation +
           theme(strip.text = element_blank())
      } else {
        p <- p +
          message_building_theme
      }
    }
  
  p <- p +
    scale_x_continuous(breaks = c(min(data[[x_column]]), 2015, 2030, max(data[[x_column]])),
      labels = c(min(data[[x_column]]), 2015, 2030, max(data[[x_column]])))
  # Custom label function that combines comma and suffix
  custom_label <- function(x) {
    label_comma()(x) %>% paste0(" ", y_label_suffix)
  }

  if (percent) {
    p <- p +
      scale_y_continuous(breaks =  c(0, 0.5, 1),
        labels = function(x) paste0(format(100 * x, digits = 1), "%"))
  } else {
    p <- p +
      scale_y_continuous(labels = custom_label, expand = c(0, 0))
  }

  if (!legend) {
    p <- p + theme(legend.position = "none")
  }
  # Save the plot as PNG if save_path is specified
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = plot_settings[["width"]],
           height = plot_settings[["height"]],
           dpi = plot_settings[["dpi"]])
  }
  # Print the plot
  print(p)
}

plot_clustered_barplot <- function(df,
                                    x_column,
                                    fill_column,
                                    subplot_column = "year",
                                    save_path = NULL,
                                    y_label_suffix = "",
                                    y_label = "",
                                    year_start = 2015,
                                    x_order = NULL,
                                    display_total = TRUE,
                                    angle_x_label = NULL,
                                    legend = TRUE) {

  custom_label <- function(x) {
    label_comma()(x) %>% paste0(" ", y_label_suffix)
  }

  if (!is.null(year_start)) {
    #print(year_start)
    df <- df %>%
      filter(!(year == year_start & scenario != "Baseline"))
  }

  df[[subplot_column]] <- factor(df[[subplot_column]], levels = unique(df[[subplot_column]]))

  df <- df %>%
    mutate(!!fill_column := plot_settings[["rename"]][.data[[fill_column]]]) %>%
    mutate(!!sym(fill_column) := factor(!!sym(fill_column),
      levels = rev(plot_settings[["order"]])))

  if (!is.null(x_order)) {
    df <- df %>%
      mutate(!!sym(x_column) := factor(!!sym(x_column),
        levels = x_order))
  }

  p <- df %>%
    ggplot(aes(fill = .data[[fill_column]],
      y = value, x = factor(.data[[x_column]])),
    color = .data[[fill_column]]) +
    geom_bar(stat = "identity", position = "stack", width=0.6)
  if (display_total) {
    p <- p +
      geom_text(
        aes(label = paste0(round(after_stat(y), 0)),
          group = x_column),
        stat = 'summary', fun = sum, vjust = -1, fontface = "bold", size = 6
      )
  }
  p <- p +
    facet_grid(reformulate(subplot_column), scales = "free_x", space = "free") +
    labs(title = y_label) +
    scale_fill_manual(values =
      plot_settings[["colors"]], na.translate = FALSE) +
    scale_y_continuous(labels = custom_label) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = plot_settings[["size_text"]]),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = plot_settings[["small_size_text"]]),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = plot_settings[["size_text"]]),
          strip.text.x = element_text(size = plot_settings[["size_text"]]),
          plot.title = element_text(size = plot_settings[["size_text"]]),
          panel.background = element_rect(fill = "#efefef", color = "white"))
  
  if (!is.null(angle_x_label)) {
    # put text.x in bold
    p <- p +
      theme(axis.text.x = element_text(angle = angle_x_label, hjust = 1, size = plot_settings[["size_text"]], face = "bold"))
  }
  if (!legend) {
    p <- p + theme(legend.position = "none", legend.title = element_text(size=10))
  }

  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = plot_settings[["width"]],
            height = plot_settings[["height"]],
            dpi = plot_settings[["dpi"]])
  }
  # Print the plot
  print(p)
  
}

plot_multiple_lines <- function(df,
    x_column,
    y_column,
    line_column = NULL,
    type_column = NULL,
    group_column = NULL,
    ncol = 4,
    y_label = "",
    save_path = NULL,
    free_y = FALSE,
    colors_lines = NULL,
    legend = TRUE,
    y_label_suffix = "",
    line_types = NULL,
    presentation = FALSE,
    line_order = NULL,
    legend_title = FALSE) {
  
  if (is.null(type_column)) {
    type_column <- line_column
  }

  linewidth <- 2
  if (presentation) {
    linewidth <- 3
  }
  if (!is.null(group_column)) {
    df <- df %>%
      mutate(!!group_column := plot_settings[["rename"]][.data[[group_column]]]) %>%
      filter(!is.na(.data[[group_column]]))
    linewidth <- 1.5
  }

  if (!is.null(line_column)) {
    if (all(unique(df[[line_column]]) %in% names(plot_settings[["rename"]]))) {
      df <- df %>%
        mutate(!!line_column := plot_settings[["rename"]][.data[[line_column]]])
    }
  }

  if (!is.null(line_order)) {
    #print(line_order)
    df <- df %>%
      mutate(!!sym(line_column) := factor(!!sym(line_column),
        levels = line_order))
  }

  if (!is.null(line_column)) {
    p <- ggplot(df, aes(x = .data[[x_column]], y = .data[[y_column]],
                color = .data[[line_column]], linetype = .data[[type_column]]))
    } else {
    p <- ggplot(df, aes(x = .data[[x_column]], y = .data[[y_column]]))
  }
    p <- p +
        geom_line(linewidth = linewidth) +
        expand_limits(y = 0)
  
  if (!is.null(colors_lines)) {
    p <- p + scale_color_manual(values = colors_lines)
  }
  if (!is.null(line_types)) {
    p <- p + scale_linetype_manual(values = line_types)
  }

  # Create subplot for each instance of group_column
  if (!is.null(group_column)) {
    if (free_y) {
        p <- p + facet_wrap(group_column, ncol = ncol, scales = "free_y")
      } else {
        p <- p + facet_wrap(group_column, ncol = ncol)
    }
    p <- p +
        # message_building_subplot_theme
        message_building_theme
  } else {
    if (presentation) {
      p <- p +
        message_building_theme_presentation
      y_label <- NULL
    } else {
      p <- p +
        message_building_theme
    }

  }
  if (!legend) {
    p <- p + theme(legend.position = "none", legend.title = element_text(size=10))
  }


  # Custom label function that combines comma and suffix
  custom_label <- function(x) {
    label_comma()(x) %>% paste0(" ", y_label_suffix)
  }

  p <- p +
      labs(title = y_label) +
      scale_x_continuous(
        breaks = c(min(df[[x_column]]), 2030, max(df[[x_column]])),
        labels = c(min(df[[x_column]]), 2030, max(df[[x_column]]))
        ) +
      scale_y_continuous(labels = custom_label) # expand = c(0, 0)

  if (legend_title) {
    print(str_replace_all(str_to_title(line_column), "_", " "))
    p <- p + labs(
      color = str_replace_all(str_to_title(line_column), "_", " "),
      linetype = str_replace_all(str_to_title(type_column), "_", " "))
  }

# Save the plot as PNG if save_path is specified
if (!is.null(save_path)) {
  ggsave(save_path, plot = p, width = plot_settings[["width"]],
          height = plot_settings[["height"]],
          dpi = plot_settings[["dpi"]])
}
  # Print the plot
  print(p)
}

scatter_plots <- function(df,
                          x_column,
                          y_column,
                          color_column = "Scenario",
                          colors_scenarios,
                          size_column = NULL,
                          shape_column = NULL,
                          save_path = NULL,
                          hline = NULL,
                          scenarios_shape = NULL,
                          x_label_suffix = "",
                          y_label_suffix = "",
                          legend_suffix = "",
                          y_label = "",
                          x_label = "",
                          discrete = FALSE,
                          legend = FALSE,
                          presentation = FALSE) {
  
  legend_shape <- TRUE
  if (is.null(shape_column)) {
    legend_shape <- FALSE
    df <- df %>%
      mutate("shape" = 16)

    shape_column <- "shape"
  }

  legend_size <- TRUE
  if (is.null(size_column)) {
    legend_size <- FALSE
    df <- df %>%
      mutate("size" = 1)

    size_column <- "size"
  }
  df[[shape_column]] <- as.factor(df[[shape_column]])

  p <- ggplot(df, aes(x = .data[[x_column]], y = .data[[y_column]],
    color = .data[[color_column]], size = .data[[size_column]], shape = .data[[shape_column]]),
    ) +
    geom_point(alpha = 1) +
    # expand_limits(y = 0, x = 0) +
    scale_color_manual(values = colors_scenarios) +
    scale_size(range = c(5, 10),
                breaks = c(
                min(df[[size_column]]),
                (min(df[[size_column]]) + max(df[[size_column]])) / 2,
                max(df[[size_column]])),
              labels = function(x) paste0(round(x, 0), legend_suffix))

  if (!is.null(hline)) {
    p <- p + geom_hline(yintercept = hline, linetype = "dashed", color = "red")
  }

  if (!is.null(scenarios_shape)) {
    p <- p + scale_shape_manual(values = scenarios_shape)
  }

  if (!presentation) {
      p <- p +
        message_building_theme +
        theme(axis.title.y = element_text(size = 30, vjust = 0.5, hjust = 0.5, angle = 90),
              axis.title.x = element_text(size = 30, hjust = 0.5))

      size_axis <- plot_settings[["size_text"]]
  } else {
      p <- p +
        message_building_theme_presentation
      size_axis <- plot_settings[["big_size_text"]]
  }

  custom_label <- function(x, suffix) {
    label_comma()(x) %>% paste0(" ", suffix)
  }

  # y_max <- max(df[[y_column]], na.rm = TRUE) * 1.1
  # y_min <- min(df[[y_column]], na.rm = TRUE) * 1.1

  # if (y_min < 0) {
  #   y_min <- y_min * 1.1
  # } else {
  #   y_min <- y_min * 0.9
  # }

  # if (y_max > 0) {
  #   y_max <- y_max * 1.1
  # } else {
  #   y_max <- y_max * 0.9
  # }
  p <- p +
    labs(y = y_label, x = x_label) +
    scale_y_continuous(labels =
      function(x) custom_label(x, suffix = y_label_suffix))

  if (!discrete) {
    p <- p +
      scale_x_continuous(labels =
      function(x) custom_label(x, suffix = x_label_suffix))
  }

  # , limits = c(y_min, y_max)


  if (!legend) {
    p <- p + theme(legend.position = "none")
  } else {
    guide_list <- guides(color = guide_legend(override.aes = list(size = 5)), shape = "none", size = "none")

    if (legend_size) {
      guide_list$size <- guide_legend(override.aes = list(size = 5))
    }
    
    if (legend_shape) {
      guide_list$shape <- guide_legend(override.aes = list(size = 5))
    }

    p <- p + guides(!!!guide_list)


  }
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = plot_settings[["width"]],
            height = plot_settings[["height"]],
            dpi = plot_settings[["dpi"]])
  }

  print(p)
}

stacked_plots <- function(data,
                          subplot_column = NULL,
                          save_path = NULL,
                          color_list = NULL,
                          y_label_suffix = "",
                          presentation = FALSE,
                          legend = TRUE,
                          horizontal = FALSE,
                          scales = "free_y") {

  if (!is.null(subplot_column)) {
    data <- data %>%
      mutate(!!subplot_column := plot_settings[["rename"]][.data[[subplot_column]]]) %>%
      filter(!is.na(.data[[subplot_column]]))

    size_text <- 3
    size_point <- 1
    nudge_y <- 0
    round <- 1
  } else {
    round <- 0
    if (presentation) {
      size_text <- 12
      size_point <- 5
      nudge_y <- 0.2

    } else {
      size_text <- 8
      size_point <- 5
      nudge_y <- 0.15
    }

    }
  p <- data %>%
    ggplot(aes(x = scenario, y = value, fill = variable)) +
    geom_bar(stat = "identity") +
    geom_point(aes(x = scenario, y = total_value),
      color = "black", size = size_point, , show.legend = FALSE) +
    geom_text(aes(x = scenario,
      label = paste(round(total_value, round)), y = total_value),
      vjust = -0.5, nudge_y = nudge_y, size = size_text)
    if ("total_value_private" %in% names(data)) {
      # make red diamonds for private investments
      p <- p +
        geom_point(aes(x = scenario, y = total_value_private),
          color = "red", size = size_point, shape = 18, show.legend = FALSE) +
        geom_text(aes(x = scenario,
          label = paste(round(total_value_private, round)), y = total_value_private),
          vjust = -0.5, nudge_y = nudge_y, size = size_text, color = "red")
          

        #geom_point(aes(x = scenario, y = total_value_private), color = "red", size = size_point, , show.legend = FALSE)
    }

  if (horizontal) {
    p <- p + coord_flip()
  }

  if (!is.null(color_list)) {
    p <- p + scale_fill_manual(values = color_list)
  }

  if (!is.null(subplot_column)) {
    p <- p +
      facet_wrap(subplot_column, ncol = 4, scales = scales) +
      message_building_subplot_theme +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))

  } else {
    if (presentation) {
      p <- p +
        geom_hline(yintercept = 0, color = "black", size = 1) +
        message_building_theme_presentation +
        theme(axis.line.y = element_blank(),
              axis.line.x = element_blank()
                )
        if (horizontal) {
          p <- p + theme(axis.text.y = element_blank())

        } else {
          p <- p + theme(axis.text.x = element_blank())

        }
    } else {
      p <- p +
        message_building_theme +
        theme(axis.line.y = element_blank())

      if (!horizontal) {
        p <- p + theme(axis.text.x = element_text(angle = 20, hjust = 1))
      }
    }

  }

  if (!legend) {
    p <- p + theme(legend.position = "none")
  }

  # Custom label function that combines comma and suffix
  custom_label <- function(x) {
    label_comma()(x) %>% paste0(" ", y_label_suffix)
  }
  if (horizontal) {
    p <- p +
      scale_y_continuous(labels = custom_label)
  } else {
    p <- p +
      scale_y_continuous(labels = custom_label)
  }

  print(p)
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = plot_settings[["width"]],
           height = plot_settings[["height"]],
           dpi = plot_settings[["dpi"]])
  }
}

cost_curve <- function(data, x_column, y_column, save_path, ncol = 4) {
  # Calculate cumulative values of energy_savings or n_units_fuel
  data <- data %>%
    group_by_at("region_bld") %>%
    arrange(!!sym(y_column)) %>%
    mutate(cumulative_x = cumsum(!!sym(x_column))) %>%
    mutate(cumulative_x = cumulative_x / sum(!!sym(x_column)))
  
  # Create the plot
  p <- ggplot(data, aes(x = cumulative_x, y = .data[[y_column]], group = !!sym("region_bld"))) +
    geom_line(size = 1.5) +
    geom_hline(yintercept = 0, color = "grey", size = 1) +
    labs(title = "Net Present Cost Curve of Shell Renovation",
         x = "Cumulative Number of Units of Fuel",
         y = "Net present cost") +
    message_building_subplot_theme +
    scale_y_continuous(labels =
      function(x) paste0(format(x, big.mark = ",", scientific = FALSE), "EUR")) +
    scale_x_continuous(breaks =  c(0, 0.5, 1),
        labels = function(x) paste0(format(100 * x, digits = 1), "%")) +
    facet_wrap(~ region_bld, ncol = ncol) 



  print(p)
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = plot_settings[["width"]],
           height = plot_settings[["height"]],
           dpi = plot_settings[["dpi"]])
  }
}


output_table <- function(data,
  save_dir,
  end_year,
  base_year = 2015
  ) {

  variables <- c("heat_kWh", "heat_std_kWh", "heat_tCO2",
      "energy_poverty_thresh", "stock_building",
        "heating_intensity", "population")

  low_carbon <- c("biomass_solid", "heat_pump", "electricity", "district_heat")

  # initial data from variables
  ini <- data %>%
    filter(year == base_year) %>%
    filter(scenario == ref) %>%
    mutate(scenario = "Initial") %>%
    filter(resolution == "all") %>%
    filter(variable %in% variables) %>%
    select(-c("resolution", "year")) %>%
    pivot_wider(id_cols = c(region_bld, scenario),
      names_from = variable,
      values_from = value)

  # final data from variables
  table <- data %>%
    filter(year == end_year) %>%
    filter(resolution == "all") %>%
    filter(variable %in% variables) %>%
    select(-c("resolution", "year")) %>%
    pivot_wider(id_cols = c(region_bld, scenario),
      names_from = variable,
      values_from = value)

  table <- bind_rows(ini, table)

  # cumulated ghg
  temp <- data %>%
    filter(year <= end_year) %>%
    filter(resolution == "all") %>%
    filter(variable == "heat_tCO2") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_tCO2_cum = sum(value) * 5) %>%
    ungroup()

  table <- left_join(table, temp, by = c("region_bld", "scenario"))

  # replacement heating system
  temp <- data %>%
    filter(year <= end_year) %>%
    filter(resolution == "all") %>%
    filter(variable == "n_replacement") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(replacement_heater = sum(value) * stp) %>%
    ungroup()
  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(replacement_heater =
      ifelse(is.na(replacement_heater), 0, replacement_heater))

  # revenues from taxes
  temp <- data %>%
    filter(year <= end_year) %>%
    filter(resolution == "all") %>%
    filter(variable == "taxes_revenues_EUR") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(taxes_revenues_EUR = sum(value) * stp) %>%
    ungroup()
  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(taxes_revenues_EUR =
      ifelse(is.na(taxes_revenues_EUR), 0, taxes_revenues_EUR))

  # cost renovation
  temp <- data %>%
    filter(year <= end_year) %>%
    filter(resolution == "all") %>%
    filter(variable == "cost_renovation_EUR") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(cost_renovation_EUR = sum(value) * stp) %>%
    ungroup()
  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(cost_renovation_EUR =
      ifelse(is.na(cost_renovation_EUR), 0, cost_renovation_EUR))

  # subsidies renovation
  temp <- data %>%
    filter(year <= end_year) %>%
    filter(resolution == "all") %>%
    filter(variable == "sub_renovation_EUR") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(sub_renovation_EUR = sum(value) * stp) %>%
    ungroup()
  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(sub_renovation_EUR =
      ifelse(is.na(sub_renovation_EUR), 0, sub_renovation_EUR))

  # cost heater
  temp <- data %>%
    filter(year <= end_year) %>%
    filter(resolution == "all") %>%
    filter(variable == "cost_heater_EUR") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(cost_heater_EUR = sum(value) * stp) %>%
    ungroup()
  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(cost_heater_EUR =
      ifelse(is.na(cost_heater_EUR), 0, cost_heater_EUR))

  # subsidies heater
  temp <- data %>%
    filter(year <= end_year) %>%
    filter(resolution == "all") %>%
    filter(variable == "sub_heater_EUR") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(sub_heater_EUR = sum(value) * stp) %>%
    ungroup()
  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(sub_heater_EUR =
      ifelse(is.na(sub_heater_EUR), 0, sub_heater_EUR))

  # renovated buildings
  temp <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == end_year) %>%
    filter(resolution %in% c("std", "adv")) %>%
    select(-c("resolution", "year")) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(renovated_building = sum(value)) %>%
    ungroup()

  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(renovated_building =
      ifelse(is.na(renovated_building), 0, renovated_building))

  # advanced buildings
  temp <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == end_year) %>%
    filter(resolution == "adv") %>%
    select(-c("resolution", "year")) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(advanced_renovated_building = sum(value)) %>%
    ungroup()

  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(advanced_renovated_building =
      ifelse(is.na(advanced_renovated_building), 0, advanced_renovated_building))

  # stock energy consumption
  temp <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == end_year) %>%
    filter(resolution %in% names(rename_primary)) %>%
    mutate(resolution = rename_primary[resolution]) %>%
    group_by_at(c("region_bld", "scenario", "resolution")) %>%
    summarize(building = sum(value)) %>%
    ungroup() %>%
    mutate(building = ifelse(is.na(building), 0, building)) %>%
    pivot_wider(id_cols = c("region_bld", "scenario"),
      names_from = resolution,
      values_from = building)
  
  ini <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == base_year) %>%
    filter(scenario == ref) %>%
    filter(resolution %in% names(rename_primary)) %>%
    mutate(resolution = rename_primary[resolution]) %>%
    group_by_at(c("region_bld", "scenario", "resolution")) %>%
    summarize(building = sum(value)) %>%
    ungroup() %>%
    mutate(building = ifelse(is.na(building), 0, building)) %>%
    pivot_wider(id_cols = c("region_bld", "scenario"),
      names_from = resolution,
      values_from = building) %>%
    mutate(scenario = "Initial")

  temp <- bind_rows(ini, temp)
  table <- left_join(table, temp, by = c("region_bld", "scenario"))

  # low-carbon buildings
  temp <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == end_year) %>%
    filter(resolution %in% low_carbon) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(low_carbon_building = sum(value)) %>%
    ungroup()

  # initial data
  ini <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == base_year) %>%
    filter(scenario == ref) %>%
    filter(resolution %in% low_carbon) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(low_carbon_building = sum(value)) %>%
    ungroup() %>%
    mutate(scenario = "Initial")
  temp <- bind_rows(ini, temp)

  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(renovated_building =
      ifelse(is.na(renovated_building), 0, renovated_building))

  # heat_pumps
  temp <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == end_year) %>%
    filter(resolution == "heat_pump") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_pump = sum(value)) %>%
    ungroup()

  # initial data
  ini <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == base_year) %>%
    filter(scenario == ref) %>%
    filter(resolution == "heat_pump") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_pump = sum(value)) %>%
    ungroup() %>%
    mutate(scenario = "Initial")
  temp <- bind_rows(ini, temp)

  table <- left_join(table, temp, by = c("region_bld", "scenario"))

  # consumption fossil-fuels
  temp <- data %>%
    filter(variable == "heat_kWh") %>%
    filter(year == end_year) %>%
    filter(resolution %in% c("oil", "coal", "gas")) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_kWh_fossil = sum(value)) %>%
    ungroup()

  # initial data
  ini <- data %>%
    filter(variable == "heat_kWh") %>%
    filter(year == base_year) %>%
    filter(scenario == ref) %>%
    filter(resolution %in% c("oil", "coal", "gas")) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_kWh_fossil = sum(value)) %>%
    ungroup() %>%
    mutate(scenario = "Initial")

  temp <- bind_rows(ini, temp)
  table <- left_join(table, temp, by = c("region_bld", "scenario"))

  # consumption electricity
  temp <- data %>%
    filter(variable == "heat_kWh") %>%
    filter(year == end_year) %>%
    filter(resolution %in% c("electricity", "heat_pump")) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_kWh_electricity = sum(value)) %>%
    ungroup()

  # initial data
  ini <- data %>%
    filter(variable == "heat_kWh") %>%
    filter(year == base_year) %>%
    filter(scenario == ref) %>%
    filter(resolution %in% c("electricity", "heat_pump")) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_kWh_electricity = sum(value)) %>%
    ungroup() %>%
    mutate(scenario = "Initial")

  temp <- bind_rows(ini, temp)
  table <- left_join(table, temp, by = c("region_bld", "scenario"))

  # formatting output
  format_temp <- table %>%
    arrange(region_bld, scenario) %>%
    mutate(
      heat_kWh_fossil = round(heat_kWh_fossil / 1e9, 2),
      heat_kWh_electricity = round(heat_kWh_electricity / 1e9, 2),
      heat_kWh = round(heat_kWh / 1e9, 2),
      heat_std_kWh = round(heat_std_kWh / 1e9, 2),
      heat_tCO2 = round(heat_tCO2 / 1e6, 2),
      heat_tCO2_cum = round(heat_tCO2_cum / 1e9, 2),
      stock_building = round(stock_building / 1e6, 2),
      heating_intensity = round(heating_intensity, 2),
      renovated_building = round(renovated_building / 1e6, 2),
      advanced_renovated_building =
        round(advanced_renovated_building / 1e6, 2),
      replacement_heater = round(replacement_heater / 1e6, 2),
      low_carbon_building = round(low_carbon_building / 1e6, 2),
      heat_pump = round(heat_pump / 1e6, 2),
      energy_poverty_thresh =
        round(energy_poverty_thresh / 1e6, 2),
      cost_renovation_EUR = round(cost_renovation_EUR / 1e9, 2),
      sub_renovation_EUR = round(sub_renovation_EUR / 1e9, 2),
      cost_heater_EUR = round(cost_heater_EUR / 1e9, 2),
      sub_heater_EUR = round(sub_heater_EUR / 1e9, 2),
      taxes_revenues_EUR = round(taxes_revenues_EUR / 1e9, 2),
      population = round(population / 1e6, 2),
      `Advanced performance` = round(`Advanced performance` / 1e6, 2),
      `Basic performance` = round(`Basic performance` / 1e6, 2),
      `Bad performance` = round(`Bad performance` / 1e6, 2),
      `No performance` = round(`No performance` / 1e6, 2),
      `No consumption` = round(`No consumption` / 1e6, 2)
        ) %>%
    mutate(region_bld = plot_settings[["rename"]][region_bld]) %>%
    select(c("region_bld", "scenario",
      "population",
      "heat_kWh", "heat_std_kWh", "heat_tCO2", "heat_tCO2_cum",
      "heat_kWh_fossil", "heat_kWh_electricity",
      "heating_intensity", "stock_building",
      "low_carbon_building", "heat_pump",
      "renovated_building", "advanced_renovated_building",
      "replacement_heater", "energy_poverty_thresh",
      "cost_renovation_EUR", "sub_renovation_EUR",
      "cost_heater_EUR", "sub_heater_EUR", "taxes_revenues_EUR",
      "Advanced performance", "Basic performance", 
      "Bad performance", "No performance", "No consumption")) %>%
    mutate(low_carbon_building_rate = low_carbon_building / stock_building,
      renovated_building_rate = renovated_building / stock_building,
      energy_poverty_rate = energy_poverty_thresh / stock_building,
      heat_kWh_fossil_rate = heat_kWh_fossil / heat_kWh,
      sh_sub_renovation = sub_renovation_EUR / cost_renovation_EUR,
      sh_sub_heater = sub_heater_EUR / cost_heater_EUR,
      total_cost = cost_renovation_EUR + cost_heater_EUR,
      total_subsidies = sub_renovation_EUR + sub_heater_EUR,
      government_expenditures = - taxes_revenues_EUR + total_subsidies,
      consumption_hh = heat_kWh / stock_building,
      consumption_capita = heat_kWh / population,
      emission_capita = heat_tCO2 / population
      ) %>%
    rename(
      "Population (Million)" = population,
      "Energy poverty (Million)" = energy_poverty_thresh,
      "Energy poverty (Percent)" = energy_poverty_rate,
      "Renovated buildings (Million)" = renovated_building,
      "Renovated buildings advanced (Million)" = advanced_renovated_building,
      "Renovated buildings (Percent)" = renovated_building_rate,
      "Heat-pumps (Million)" = heat_pump,
      "Low carbon buildings (Million)" = low_carbon_building,
      "Low carbon buildings (Percent)" = low_carbon_building_rate,
      "Stock building (Million)" = stock_building,
      "Replacement heating system (Million)" = replacement_heater,
      "Heating intensity (Percent)" = heating_intensity,
      "Space heating consumption (TWh)" = heat_kWh,
      "Space heating consumption (MWh/hh)" = consumption_hh,
      "Space heating consumption (MWh/capita)" = consumption_capita,
      "Space heating consumption fossil (TWh)" = heat_kWh_fossil,
      "Share fossil-fuels (Percent)" = heat_kWh_fossil_rate,
      "Space heating consumption electricity (TWh)" = heat_kWh_electricity,
      "Space heating consumption standard (TWh)" = heat_std_kWh,
      "Emission (MtCO2)" = heat_tCO2,
      "Emission cumulated (GtCO2)" = heat_tCO2_cum,
      "Emission (tCO2/capita)" = emission_capita,
      "Member states" = region_bld,
      "Scenario" = scenario,
      "Total cost (Billion EUR)" = total_cost,
      "Total subsidies (Billion EUR)" = total_subsidies,
      "Cost renovation (Billion EUR)" = cost_renovation_EUR,
      "Subsidies renovation (Billion EUR)" = sub_renovation_EUR,
      "Share subsidies renovation (Percent)" = sh_sub_renovation,
      "Cost heater (Billion EUR)" = cost_heater_EUR,
      "Subsidies heater (Billion EUR)" = sub_heater_EUR,
      "Share subsidies heater (Percent)" = sh_sub_heater,
      "Taxes revenues (Billion EUR)" = taxes_revenues_EUR,
      "Government expenditures (Billion EUR)" = government_expenditures
      )

  consumption_ini <- format_temp %>%
    filter(Scenario == "Initial") %>%
    select(c("Member states", "Space heating consumption (TWh)")) %>%
    rename(consumption_ini = "Space heating consumption (TWh)")

  format_temp <- format_temp %>%
    left_join(consumption_ini) %>%
    mutate(`Consumption saving (%)` =
      (consumption_ini - `Space heating consumption (TWh)`) / consumption_ini) %>%
    select(-c("consumption_ini"))

  emission_ini <- format_temp %>%
    filter(Scenario == "Initial") %>%
    select(c("Member states", "Emission (MtCO2)")) %>%
    rename(emission_ini = "Emission (MtCO2)")

  format_temp <- format_temp %>%
    left_join(emission_ini) %>%
    mutate(`Emission saving (%)` =
      (emission_ini - `Emission (MtCO2)`) / emission_ini) %>%
    select(-c("emission_ini"))


  consumption_elec_ini <- format_temp %>%
    filter(Scenario == "Initial") %>%
    select(c("Member states", "Space heating consumption electricity (TWh)")) %>%
    rename(consumption_elec_ini = "Space heating consumption electricity (TWh)")

  format_temp <- format_temp %>%
    left_join(consumption_elec_ini) %>%
    mutate(`Consumption electricity variation (%)` =
      (`Space heating consumption electricity (TWh)` - consumption_elec_ini) / consumption_elec_ini) %>%
    select(-c("consumption_elec_ini"))

  # formatting
  
  select_order <- c(
    "Population (Million)",
    "Stock building (Million)",
    "Space heating consumption (TWh)",
    "Space heating consumption standard (TWh)",
    "Space heating consumption (MWh/hh)",
    "Space heating consumption (MWh/capita)",
    "Consumption saving (%)",
    "Space heating consumption fossil (TWh)",
    "Share fossil-fuels (Percent)",
    "Space heating consumption electricity (TWh)",
    "Replacement heating system (Million)",
    "Heat-pumps (Million)",
    "Low carbon buildings (Million)",
    "Low carbon buildings (Percent)",
    "Renovated buildings (Million)",
    "Renovated buildings advanced (Million)",
    "Renovated buildings (Percent)",
    "Total cost (Billion EUR)",
    "Total subsidies (Billion EUR)",
    "Cost heater (Billion EUR)",
    "Subsidies heater (Billion EUR)",
    "Share subsidies heater (Percent)",
    "Cost renovation (Billion EUR)",
    "Subsidies renovation (Billion EUR)",
    "Share subsidies renovation (Percent)",
    "Taxes revenues (Billion EUR)",
    "Government expenditures (Billion EUR)",
    "Emission (MtCO2)",
    "Emission saving (%)",
    "Emission cumulated (GtCO2)",
    "Emission (tCO2/capita)",
    "Heating intensity (Percent)",
    "Energy poverty (Million)",
    "Energy poverty (Percent)",
    "Advanced performance",
    "Basic performance",
    "Bad performance",
    "No performance",
    "No consumption"
  )

  long_df <- format_temp %>% 
    pivot_longer(
      cols = -c("Member states", "Scenario"),
      names_to = "variable",
      values_to = "value"
    )

  wide_df <- long_df %>%
    pivot_wider(
      names_from = Scenario,
      values_from = value
    )
  wide_df$variable <- factor(wide_df$variable, levels = select_order)
  # Arrange the rows by 'variable' and then 'Member states'
  final_df <- wide_df %>%
    arrange(variable, `Member states`)

  # Select the columns in the order you want
  final_df <- final_df %>%
    select(variable, `Member states`, "Initial", everything())

  if (!is.null(save_dir)) {

    save_file <- paste(save_dir,
      paste0(run, "_", end_year, "_summary_countries.csv"), sep = "/")
    save_file_eu <- paste(save_dir,
      paste0(run, "_", end_year, "_summary_eu.csv"), sep = "/")
    write.csv(final_df, save_file, row.names = FALSE)

    final_df_eu <- final_df %>%
      filter(`Member states` == "EU") %>%
      select(-c("Member states"))

    write.csv(final_df_eu, save_file_eu, row.names = FALSE)
  }
  return(format_temp)
}

make_cost_hh_figures <- function(data, ref, save_path, x_column = "scenario",
  subplot_column = "resolution", angle_x_label = NULL) {

  data_ref <- parse_data %>%
    filter(scenario == ref) %>%
    rename(value_ref = value) %>%
    select(-scenario)

  diff <- parse_data %>%
    filter(variable != "total_cost_hh") %>%
    filter(scenario != ref) %>%
    left_join(data_ref, by = c("region_bld", "variable", "resolution")) %>%
    mutate(diff = (value - value_ref)) %>%
    select(-c(value, value_ref)) %>%
    rename(value = diff)

  plot_clustered_barplot(
    df = diff,
    x_column = x_column,
    fill_column = "variable",
    subplot_column = subplot_column,
    y_label = "Difference energy cost per household compared to current policies",
    y_label_suffix = "EUR",
    year_start = NULL,
    save_path = save_path,
    display_total = TRUE,
    x_order = NULL,
    angle_x_label = angle_x_label)

}


plot_map <- function(data,
                    limits,
                    figure_title = "",
                    save_path = NULL,
                    legend_title = "",
                    subplot_column = NULL,
                    threshold_colormap = NULL,
                    reverse_colormap = FALSE,
                    ncol = 2,
                    key = "iso_a3_eh") {
  
  eu_countries <- ne_countries(continent = "Europe", returnclass = "sf")

  if (key == "iso_a3_eh") {
    data <- data %>%
      mutate(iso_a3_eh = substr(region_bld, nchar(region_bld)-2, nchar(region_bld)))

    merged_data <- merge(eu_countries, data, by = "iso_a3_eh") %>%
      filter(!is.na(value))
  }

  if (key == "name") {
    merged_data <- merge(eu_countries, data, by = "name") %>%
      filter(!is.na(value))
  }


  merged_data <- st_crop(merged_data, xmin = -20, xmax = 45,
                                      ymin = 30, ymax = 73)

  p <- ggplot(merged_data) +
    geom_sf(aes(fill = value), color = "gray20", size = 5) +
    geom_sf_text(aes(label = iso_a3_eh), size = 5, color = "grey50",
      fontface = "bold")
  
  if (!is.null(threshold_colormap)){
    low <- "black"
    high <- "white"
    if (reverse_colormap) {
      low <- "white"
      high <- "black"
    }
    p <- p + scale_fill_gradient2(
      low = low,
      mid = "grey",
      high = high,
      midpoint = threshold_colormap,
      limits = limits,
      oob = scales::squish,
      na.value = "grey50"
    )
  } else {
    p <- p + scale_fill_viridis_c(
      limits = limits,
      oob = scales::squish,
      na.value = "grey50",
      direction = -1
      )
  }

  if (!is.null(subplot_column)) {
    p <- p +
      facet_wrap(subplot_column, ncol = ncol)
  }

  p <- p +
    theme_void() +
    theme(
      plot.title = element_text(size = plot_settings[["big_size_text"]]),
      legend.title = element_text(size = plot_settings[["size_text"]]),
      legend.text = element_text(size = plot_settings[["size_text"]]),
      strip.background = element_blank(),
      strip.text = element_text(size = 15, face = "bold", margin = margin(b = 10)),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +  # Adjust the plot margins
    labs(title = figure_title) +
    guides(fill = guide_legend(title = legend_title))

  if (!is.null(save_path)) {

    if (!is.null(subplot_column)) {

    if (length(unique(data[[subplot_column]])) / ncol <= 1) {
      ggsave(save_path, plot = p, width = plot_settings[["width"]],
            dpi = plot_settings[["dpi"]])
      
    } else {
      ggsave(save_path, plot = p, width = plot_settings[["width"]],
        dpi = plot_settings[["dpi"]], height = plot_settings[["height"]])
    }
    } else {
      ggsave(save_path, plot = p, width = plot_settings[["width"]],
        dpi = plot_settings[["dpi"]], height = plot_settings[["height"]])
    }
  } else {
    print(p)
  }

}

# Define the manual Sobol analysis function
manual_sobol_analysis <- function(df, list_features, y) {
  # Initialize a tibble to store the results
  sobol_df <- tibble(
    Feature = list_features,
    `First Order` = numeric(length(list_features)),
    `Total Order` = numeric(length(list_features))
  )
  
  # Calculate the expectation and variance of the output variable
  expectation <- mean(df[[y]], na.rm = TRUE)
  variance <- var(df[[y]], na.rm = TRUE)

  nrow_scenarios = nrow(df)
  
  for (col in list_features) {
    # First order Sobol index
    conditional_means <- df %>%
      group_by_at(col) %>%
      summarize(mean_y = mean(.data[[y]], na.rm = TRUE)) %>%
      ungroup()
    
    counts <- df %>%
        group_by_at(col) %>%
        summarize(n = n()) %>%
      mutate(prop = n / nrow_scenarios) %>%
      select(prop) %>%
      pull()
    
    sobol_first_order <- sum(counts * (conditional_means$mean_y - expectation) ^ 2) / variance
    sobol_df <- sobol_df %>%
      mutate(`First Order` = if_else(Feature == col, sobol_first_order, `First Order`))

    #print(paste("First Order Sobol Index for", col, ":", sobol_first_order))

    
    # Total order Sobol index
    list_features_minus_i <- setdiff(list_features, col)
    
    conditional_means <- df %>%
      group_by_at(list_features_minus_i) %>%
      summarize(mean_y = mean(.data[[y]], na.rm = TRUE)) %>%
      ungroup()
    
    counts <- df %>%
        group_by_at(list_features_minus_i) %>%
        summarize(n = n()) %>%
        mutate(prop = n / nrow_scenarios) %>%
        select(prop) %>%
        pull()
    
    sobol_total_order <- 1 - sum(counts * (conditional_means$mean_y - expectation) ^ 2) / variance
    sobol_df <- sobol_df %>%
      mutate(`Total Order` = if_else(Feature == col, sobol_total_order, `Total Order`))

    #print(paste("Total Order Sobol Index for", col, ":", sobol_total_order))

  }
  
  return(sobol_df)
}

horizontal_bar_plot <- function(df, columns = NULL, title = NULL, order = NULL, save_path = NULL) {

  # If no specific columns are provided, use all columns in the DataFrame
  if (is.null(columns)) {
    columns <- colnames(df)
  }

  # Order the DataFrame if the order parameter is provided
  if (!is.null(order)) {
    df <- df %>% arrange(!!sym(order))
  }

  # Convert the DataFrame to long format for ggplot2
  df_long <- df %>%
    rownames_to_column(var = "Feature") %>%
    pivot_longer(cols = all_of(columns), names_to = "Metric", values_to = "Value")

  # Create the horizontal bar plot
  p <- ggplot(df_long, aes(x = Feature, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    coord_flip() +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 18),
          plot.title = element_text(size = 22, face = "bold"),
          legend.title = element_blank(),
          legend.text = element_text(size = 18)) +
    labs(title = title) +
    scale_fill_brewer(palette = "Set3")

  # Save the plot if a save path is provided
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = p, width = 14, height = 9.6)
  }

  return(p)
}

sobol_figures <- function(df, list_features, y, rename_feature, save_path) {

  df <- manual_sobol_analysis(df, list_features, y)
  print(df)

  # order ascendng by Total_Order column
  df <- df %>%
      arrange(`Total Order`) %>%
      mutate(Feature = rename_feature[Feature]) %>%
      mutate(Feature = factor(Feature, levels = unique(Feature)))

  # make horizontal bar plot of the first order and total order indices

  p <- df %>%
      pivot_longer(cols = c(`First Order`, `Total Order`),
                  names_to = "Order_Type", 
                  values_to = "Value") %>%
      ggplot(aes(x = Value, y = Feature, fill = Order_Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      message_building_theme +
      theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) 

  ggsave(save_path, plot = p, width = plot_settings[["width"]],
          height = plot_settings[["height"]],
          dpi = plot_settings[["dpi"]])

  # save the data
  t <- df %>%
      select(Feature, `First Order`, `Total Order`) %>%
      rename("First Order" = `First Order`, "Total Order" = `Total Order`)

  write.csv(t, paste0(save_path, ".csv"), row.names = FALSE)

}

calculate_cost_hh <- function(df, discount = 0.05, lifetime_loan = 10,
  lifetime_renovation = 30, lifetime_heater = 20, stp = 5) {

  var <- c("to_pay_renovation", "to_pay_heater", "cost_energy", "stock_building")

  temp <- df %>%
    filter(variable %in% var)

  wide_data <- pivot_wider(
    temp,
    id_cols = c("region_bld", "year", "scenario", "resolution"),
    names_from = variable,
    values_from = value
    )

  wide_data <- wide_data %>%
    mutate(
      to_pay_renovation = ifelse(is.na(to_pay_renovation), 0, to_pay_renovation),
      to_pay_heater = ifelse(is.na(to_pay_heater), 0, to_pay_heater),
      cost_energy = ifelse(is.na(cost_energy), 0, cost_energy),
      to_pay_renovation = to_pay_renovation *
        discount / (1 - (1 + discount)^(-lifetime_loan)),
      to_pay_heater = to_pay_heater *
        discount / (1 - (1 + discount)^(-lifetime_loan))
          )

  wide_data <- wide_data %>%
    rowwise() %>%
    mutate(to_pay_renovation_cumsum =
      sum(wide_data$to_pay_renovation[wide_data$region_bld == region_bld &
          wide_data$scenario == scenario &
          wide_data$resolution == resolution &
          wide_data$year <= year &
          wide_data$year >= (year - lifetime_loan)], na.rm = TRUE)) %>%
    mutate(to_pay_heater_cumsum =
      sum(wide_data$to_pay_heater[wide_data$region_bld == region_bld &
          wide_data$scenario == scenario &
          wide_data$resolution == resolution &
          wide_data$year <= year &
          wide_data$year > (year - lifetime_loan)], na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(stock_building)) %>%
    mutate(total_cost_hh =
      to_pay_renovation_cumsum + to_pay_heater_cumsum +
      cost_energy,
      to_pay_renovation_cumsum = to_pay_renovation_cumsum / stock_building,
      to_pay_heater_cumsum = to_pay_heater_cumsum / stock_building,
      cost_energy = cost_energy / stock_building,
      total_cost_hh = total_cost_hh / stock_building)

  long_data <- wide_data %>%
    select(c("region_bld", "year", "scenario", "resolution",
      "to_pay_renovation_cumsum", "to_pay_heater_cumsum", "cost_energy", "total_cost_hh")) %>%
    pivot_longer(
      cols = c("to_pay_renovation_cumsum", "to_pay_heater_cumsum",
        "cost_energy", "total_cost_hh"),
      names_to = "variable",
      values_to = "value"
    )

  long_data  <- long_data %>%
    group_by(region_bld, scenario, resolution, variable) %>%
    summarize(value = mean(value, na.rm = TRUE)) %>%
    ungroup()

  return(long_data)
}

make_cost_benefits <- function(data, ref, save_dir, nb_years = 30, figures = TRUE, 
  social_discount = 0.03, lifetime_renovation = 30, lifetime_heater = 20, stp = 5,
  make_summary = FALSE) {
  # Each year represents a stp years period
  # Cost heat in 2020 represnts the cost of heat from 2015 to 2020.
  # Need to be corrected to account for energy saving

  path <- "STURM_data/input_csv/input_resid/macro/social_cost_carbon_20.csv"
  social_cost_carbon <- read.csv(path) %>%
    rename(social_cost_carbon = value)

  data_raw <- data

  variables_of_interest <- c("cost_renovation_EUR", "cost_heater_EUR",
        "cost_heat_EUR", "heat_tCO2", "thermal_comfort_EUR")

  data <- filter(data,
      resolution == "all",
      variable %in% variables_of_interest) %>%
      filter(year >= 2015) %>%
      select(-resolution)

  # Create all combinations of region_bld, year, scenario, and variable
  expanded_data <- expand_grid(
    region_bld = unique(data$region_bld),
    year = unique(data$year),
    scenario = unique(data$scenario),
    variable = variables_of_interest
  )

  # Merge with the original data and fill missing values with 0
  data <- expanded_data %>%
    left_join(data, by = c("region_bld", "year", "scenario", "variable")) %>%
    replace_na(list(value = 0))

  wide_data <- pivot_wider(data,
                          id_cols = c("region_bld", "year", "scenario"),
                          names_from = variable,
                          values_from = value)
  
  # Correct to account that cost heat over the period is not the cost of last year but the
  # average cost over the period

  if (FALSE) {
    wide_data <- wide_data %>%
      group_by(region_bld, scenario) %>%
      arrange(year) %>%
      mutate(
        cost_heat_EUR = ifelse(year == 2015, cost_heat_EUR, cost_heat_EUR + lag(cost_heat_EUR, default = NA)) / 2 * stp,
        heat_tCO2 = ifelse(year == 2015, heat_tCO2, heat_tCO2 + lag(heat_tCO2, default = NA)) / 2 * stp,
        thermal_comfort_EUR = ifelse(year == 2015, thermal_comfort_EUR, thermal_comfort_EUR + lag(thermal_comfort_EUR, default = NA)) / 2 * stp
        ) %>%
      ungroup()
  }


  wide_data <- wide_data %>%
    left_join(social_cost_carbon, by = "year") %>%
    mutate(social_cost_carbon = ifelse(is.na(social_cost_carbon), 0, social_cost_carbon)) %>%
    mutate(
      # Flow have been reduced by stp in post-processing. Calculate total cost over stp years.
      cost_renovation_EUR =
        ifelse(is.na(cost_renovation_EUR), 0, cost_renovation_EUR) * stp,
      cost_heater_EUR =
        ifelse(is.na(cost_heater_EUR), 0, cost_heater_EUR) * stp,
      # Running cost are over a 5 year period. 
      annuities_renovation =
        stp * social_discount / (1 - (1 + social_discount)^(-lifetime_renovation)),
      annuities_heater =
        stp * social_discount / (1 - (1 + social_discount)^(-lifetime_heater)),
      cost_renovation_EUR_year = cost_renovation_EUR * annuities_renovation,
      cost_heater_EUR_year = cost_heater_EUR * annuities_heater,
      cost_emission_EUR = heat_tCO2 * social_cost_carbon * stp,
      cost_heat_EUR = cost_heat_EUR * stp,
      thermal_comfort_EUR = - thermal_comfort_EUR * stp
      )

  wide_data <- wide_data %>%
    rowwise() %>%
    mutate(cost_renovation_EUR_cumsum =
      sum(wide_data$cost_renovation_EUR_year[wide_data$region_bld == region_bld &
          wide_data$scenario == scenario &
          wide_data$year <= year &
          wide_data$year >= (year - lifetime_renovation)], na.rm = TRUE)) %>%
    mutate(cost_heater_EUR_cumsum =
      sum(wide_data$cost_heater_EUR_year[wide_data$region_bld == region_bld &
          wide_data$scenario == scenario &
          wide_data$year <= year &
          wide_data$year > (year - lifetime_heater)], na.rm = TRUE)) %>%
    ungroup()

  wide_data <- wide_data %>%
    mutate(running_cost = cost_renovation_EUR_cumsum + cost_heater_EUR_cumsum +
      cost_heat_EUR + cost_emission_EUR + thermal_comfort_EUR) %>%
    mutate(running_cost_private = running_cost - cost_emission_EUR)

  write.csv(filter(wide_data, region_bld == "EU"), paste0(save_dir, "/cba_calculation_eu.csv"))

  long_data <- wide_data %>%
    select("region_bld", "year", "scenario", "cost_renovation_EUR_cumsum",
      "cost_heater_EUR_cumsum", "cost_heater_EUR_cumsum",
      "cost_heat_EUR", "cost_emission_EUR", "thermal_comfort_EUR", "running_cost",
      "running_cost_private") %>%
    pivot_longer(cols = c("cost_renovation_EUR_cumsum",
      "cost_heater_EUR_cumsum", "cost_heater_EUR_cumsum",
      "cost_heat_EUR", "cost_emission_EUR", "thermal_comfort_EUR", "running_cost",
      "running_cost_private"),
                names_to = "variable",
                values_to = "value")
  data_ref <- long_data %>%
    filter(scenario == ref) %>%
    rename(value_ref = value) %>%
    select(-scenario)

  diff <- long_data %>%
    filter(scenario != ref) %>%
    left_join(data_ref,
      by = c("region_bld", "year", "variable")) %>%
    mutate(diff = (value - value_ref)) %>%
    select(-c(value, value_ref)) %>%
    rename(value = diff)

  wide_data_diff <- pivot_wider(diff,
                          id_cols = c("region_bld", "year", "scenario"),
                          names_from = variable,
                          values_from = value) %>%
                    mutate(discount_factor = 1 / ((1 + social_discount)^(year - 2015)))

  # households
  pop <- data_raw %>%
    filter(variable == "stock_building", resolution == "all") %>%
    pivot_wider(id_cols = c("region_bld", "year", "scenario"),
                names_from = variable,
                values_from = value) %>%
    mutate(discount_factor = 1 / ((1 + social_discount)^(year - 2015))) %>%
    filter(year >= 2020) %>%
    group_by(scenario, region_bld) %>%
    summarize(
      stock_building_avg = sum(stock_building * discount_factor) / sum(discount_factor)
    ) %>%
    ungroup()

  # summary
  summary <- wide_data_diff %>%
    group_by(scenario, region_bld) %>%
    summarize(
      cost_renovation_sum = sum(cost_renovation_EUR_cumsum * discount_factor),
      cost_heater_sum = sum(cost_heater_EUR_cumsum * discount_factor),
      cost_heat_sum = sum(cost_heat_EUR * discount_factor),
      cost_emission_sum = sum(cost_emission_EUR * discount_factor),
      thermal_comfort_sum = sum(thermal_comfort_EUR * discount_factor),
      running_cost = sum(running_cost * discount_factor),
      running_cost_private = sum(running_cost_private * discount_factor)
    ) %>%
    ungroup() %>%
    left_join(pop) %>%
    mutate(
      cost_renovation_sum = cost_renovation_sum / (stock_building_avg * nb_years),
      cost_heater_sum = cost_heater_sum/ (stock_building_avg * nb_years),
      cost_heat_sum = cost_heat_sum / (stock_building_avg * nb_years),
      cost_emission_sum = cost_emission_sum / (stock_building_avg * nb_years),
      thermal_comfort_sum = thermal_comfort_sum / (stock_building_avg * nb_years),
      running_cost = running_cost / (stock_building_avg * nb_years),
      running_cost_private = running_cost_private / (stock_building_avg * nb_years)
    ) %>%
    select(-stock_building_avg)
  #-------------------------------------
  file <- paste0(save_dir, "/", run, "_2050_summary_eu.csv")
  if (file.exists(file) & make_summary) {
    print("Generate summary table")
    # read file column as character

    list_variables <- c(
      "Space heating consumption (TWh)",
      "Space heating consumption electricity (TWh)",
      "Emission (MtCO2)",
      "Emission cumulated (GtCO2)",
      "Total cost (Billion EUR)",
      "Government expenditures (Billion EUR)",
      "Energy poverty (Million)",
      "Emission saving (%)",
      "Consumption saving (%)",
      "Consumption electricity variation (%)",
      "Energy poverty (Percent)"
      )

    rename_list <- c(
      cost_renovation_sum = "Delta cost renovation (euro/hh/year)",
      cost_heater_sum = "Delta cost heating system (euro/hh/year)",
      cost_heat_sum = "Delta cost fuel (euro/hh/year)",
      thermal_comfort_sum = "Delta cost thermal comfort (euro/hh/year)",
      running_cost_private = "Delta total cost private (euro/hh/year)",
      cost_emission_sum = "Delta cost emission (euro/hh/year)",
      running_cost = "Delta total cost (euro/hh/year)"
    )


    summary_2050 <- read.csv(file, check.names = FALSE) %>%
      select(all_of(c("scenario_name", list_variables))) %>%
      pivot_longer(cols = list_variables, names_to = "variable", values_to = "value") %>%
      pivot_wider(names_from = scenario_name, values_from = value) %>%
      # select only rows who variables are in list_variables
      # remove column "Initial"
      select(-Initial) %>%
      mutate(across(-c(variable, all_of(ref)), ~ . - .data[[ref]])) %>%
      select(variable, all_of(ref), everything())


    temp <- summary %>%
      filter(region_bld == "EU") %>%
      select(-region_bld) %>%
      gather(variable, value, cost_renovation_sum, cost_heater_sum, cost_heat_sum,
      , thermal_comfort_sum, running_cost_private, cost_emission_sum, running_cost) %>%
      mutate(variable = rename_list[.data[["variable"]]]) %>%
      # put scneario in column
      pivot_wider(names_from = scenario, values_from = value)

    
    results <- bind_rows(summary_2050, temp) %>%
        select(variable, all_of(ref), everything()) %>%
        # if na put nothing
        mutate(across(-variable, ~ ifelse(is.na(.), "", formatC(., format = "f", digits = 2)))) 

    if (!is.null(names_scenarios)) {

      results <- results %>%
        pivot_longer(-variable, names_to = "scenario_name", values_to = "value") %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        left_join(names_scenarios, by = "scenario_name") %>%
        # put names_scenarios columns first
        select(colnames(names_scenarios), everything())
    }

    write.csv(results, paste0(save_dir, "/results.csv"), row.names = FALSE)

    # Calculate difference of each column with the column "Current policies"
    file <- paste0(save_dir, "/", run, "_2050_summary_countries.csv")


    summary_2050 <- read.csv(file, check.names = FALSE) %>%
      select(all_of(c("scenario_name", "Member states", list_variables))) %>%
      filter(scenario_name != "Initial") %>%
      pivot_longer(list_variables, names_to = "variable", values_to = "value")

    # value_ref <- summary_2050 %>%
    #   filter(scenario_name == ref) %>%
    #   rename(value_ref = value) %>%
    #   select(-scenario_name)

    # summary_2050 <- summary_2050 %>%
    #   left_join(value_ref) %>%
    #   mutate(value = value - value_ref) %>%
    #   select(-value_ref)


    temp <- summary %>%
      gather(variable, value, cost_renovation_sum, cost_heater_sum, cost_heat_sum,
      , thermal_comfort_sum, running_cost_private, cost_emission_sum, running_cost) %>%
      mutate(variable = rename_list[.data[["variable"]]]) %>%
      rename(`Member states` = region_bld) %>%
      mutate(`Member states` = rename_countries[`Member states`]) %>%
      rename(scenario_name = scenario)

    results <- bind_rows(summary_2050, temp)

    if (!is.null(names_scenarios)) {

      results <- results %>%
        left_join(names_scenarios, by = "scenario_name") %>%
        # put names_scenarios columns first
        select(colnames(names_scenarios), everything())
    }

    write.csv(results, paste0(save_dir, "/results_countries.csv"), row.names = FALSE)

    


  }
  #-------------------------------------
  rename_list <- c(
    cost_renovation_sum = "Cost renovation",
    cost_heater_sum = "Cost heating system",
    cost_heat_sum = "Cost fuel",
    thermal_comfort_sum = "Cost thermal comfort",
    running_cost_private = "Total cost private",
    cost_emission_sum = "Cost emission",
    running_cost = "Total cost"
  )

  color_list <- c(
    "Cost renovation" = "#ee6a6b",
    "Cost heating system" = "#62c5c0",
    "Cost fuel" = "#00589d",
    "Cost emission" = "#fdbb40",
    "Cost thermal comfort" = "lightblue"
  )

  df <- summary %>%
    gather(variable, value, cost_renovation_sum, cost_heater_sum, cost_heat_sum,
      cost_emission_sum, thermal_comfort_sum, running_cost, running_cost_private) %>%
    mutate(variable = rename_list[.data[["variable"]]]) %>%
    filter(scenario != ref)
    # mutate(scenario = scenarios[.data[["scenario"]]]) %>%

  # pop <- pop %>%
  #   mutate(variable = "Stock building") %>%
  #   rename(value = stock_building_avg)
  # temp <- bind_rows(pop, df)
  df_return <- df
  write.csv(df, paste0(save_dir, "/data_cba_country.csv"), row.names = FALSE)

  total <- df %>%
    filter(variable == "Total cost") %>%
    rename(total_value = value) %>%
    select(-variable)

  total_private <- df %>%
    filter(variable == "Total cost private") %>%
    rename(total_value_private = value) %>%
    select(-variable)

  df <- df %>%
    filter(variable != "Total cost", variable != "Total cost private")

  df <- df %>%
    left_join(total, by = c("scenario", "region_bld")) %>%
    left_join(total_private, by = c("scenario", "region_bld"))

  presentation <- FALSE
  legend <- TRUE

  if (figures) {
    stacked_plots(df, subplot_column = "region_bld",
      save_path = paste0(save_dir, "/cba_countries.png"),
      color_list = color_list, scales = "fixed")

    stacked_plots(filter(df, region_bld == "EU"),
      save_path = paste0(save_dir, "/cba_eu.png"),
      color_list = color_list, y_label_suffix = "€",
      presentation = presentation, legend = legend)

    # save data
    write.csv(filter(df, region_bld == "EU"), paste0(save_dir, "/cba_eu.csv"), row.names = FALSE)

    stacked_plots(filter(df, region_bld == "EU"),
      save_path = paste0(save_dir, "/cba_eu_horizontal.png"),
      color_list = color_list, y_label_suffix = "€",
      presentation = presentation, legend = legend, horizontal = TRUE)
  }
  return(df_return)
}


make_national_policy_mix <- function(df, scenario, save_path) {

    temp <- df %>%
        left_join(scenario) %>%
        mutate(subsidies_hp = case_when(
            sub_heat == "carbon" ~ "HighSubHeatPump",
            sub_heat == "half_carbon" ~ "MidSubHeatPump",
            TRUE ~ ""
        )) %>%
        mutate(subsidies_renovation  = case_when(
            (`_objective_renovation` == "renovation") & (`_success_objective_renovation` == 0.75) ~ "MidSubRenovation",
            (`_objective_renovation` == "renovation") & (`_success_objective_renovation` == 1) ~ "HighSubRenovation",
            (`_objective_renovation` == "deep_renovation") & (`_success_objective_renovation` == 0.75) ~ "MidSubDeepRenovation",
            (`_objective_renovation` == "deep_renovation") & (`_success_objective_renovation` == 1) ~ "HighSubDeepRenovation",
            TRUE ~ ""
        )) %>%
        mutate(`_realization_rate_renovation` = ifelse(`_realization_rate_renovation` == 1, "ImprovedRealization", "")) %>%
        mutate(learning_rate_heat = ifelse(learning_rate_heat == "constant", "", "HeatPumpLearning")) %>%
        mutate(carbon_tax = case_when(
            carbon_tax == "EUETS" ~ "Carbon tax",
            carbon_tax == "social" ~ "Carbon tax social price",
            TRUE ~ ""
        )) %>%
        mutate(`_remove_barriers_renovation` = ifelse(`_remove_barriers_renovation`, "Tackling market-failures renovation", "")) %>%
        mutate(scenario_descriptor = paste(
            carbon_tax, subsidies_hp, learning_rate_heat, subsidies_renovation, `_realization_rate_renovation`,
            `_remove_barriers_renovation`, sep = " ") %>%
            trimws %>%   # Remove leading and trailing whitespace
            gsub(" +", " ", .))

    temp <- temp %>%
        select(c("region_bld", "scenario_descriptor")) %>%
        rename(value = scenario_descriptor)

    # make map with 

    eu_countries <- ne_countries(continent = "Europe", returnclass = "sf")
    temp <- temp %>%
        mutate(iso_a3_eh = substr(region_bld, nchar(region_bld)-2, nchar(region_bld)))

    merged_data <- merge(eu_countries, temp, by = "iso_a3_eh")

    legend_title <- "National policy mix"
    figure_title <- "National policy mix accross EU-27 Member States"
    save_path <- paste(save_dir, save_path, sep = "/")
    merged_data <- st_crop(merged_data, xmin = -20, xmax = 45,
                                        ymin = 30, ymax = 73)

    p <- ggplot(merged_data) +
        geom_sf(aes(fill = value), color = "gray20", size = 5) +
        geom_sf_text(aes(label = iso_a3_eh), size = 5, color = "grey50",
            fontface = "bold") +
        theme_void() +
        theme(
        legend.title = element_text(size = plot_settings[["size_text"]]),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "vertical", legend.box = "vertical",
        strip.background = element_blank(),
        strip.text = element_text(size = 15, face = "bold", margin = margin(b = 10)),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +  # Adjust the plot margins
        guides(fill = guide_legend(title = legend_title))

    ggsave(save_path, plot = p, width = plot_settings[["width"]],
        dpi = plot_settings[["dpi"]], height = plot_settings[["height"]])
}