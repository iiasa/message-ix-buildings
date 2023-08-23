# MESSAGEix-Buildings - color schemes

library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

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
  "colors" = c(colors_efficiency, colors_fuels, colors_countries),
  "rename" = c(rename_eneff, rename_fuels, rename_countries),
  "order" = c(order_fuels, order_eneff, order_countries)
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
          plot.margin = margin(t = 0,
                      r = 0,
                      b = 0,
                      l = 0),
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
                              presentation = FALSE) {

  # Create the stacked area plot
  # Calculate the breaks and labels for the x-axis
  breaks <- seq(ceiling(min(data[[x_column]])),
    floor(max(data[[x_column]])), by = 5)
  labels <- breaks[seq(1, length(breaks), by = 5)]

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
      if (presentation) {
        p <- p +
          message_building_theme_presentation
      } else {
        p <- p +
          message_building_theme
      }
    }
  
  p <- p + 
    scale_x_continuous(breaks = c(min(data[[x_column]]), max(data[[x_column]])),
      labels = c(min(data[[x_column]]), max(data[[x_column]])))
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


  # Save the plot as PNG if save_path is specified
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
    group_column = NULL,
    ncol = 4,
    y_label = "",
    save_path = NULL,
    free_y = FALSE,
    colors_lines = NULL,
    legend = TRUE,
    y_label_suffix = "",
    line_types = NULL) {
  
  linewidth <- 2
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

  if (!is.null(line_column)) {
    p <- ggplot(df, aes(x = .data[[x_column]], y = .data[[y_column]],
                color = .data[[line_column]], linetype = .data[[line_column]]))
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
        message_building_subplot_theme
  } else {
    p <- p +
        message_building_theme

  }
  if (!legend) {
    p <- p + theme(legend.position = "none")
  }

  # Custom label function that combines comma and suffix
  custom_label <- function(x) {
    label_comma()(x) %>% paste0(" ", y_label_suffix)
  }

  p <- p +
      labs(title = y_label) +
      scale_x_continuous(
        breaks = c(min(df[[x_column]]), max(df[[x_column]])),
        labels = c(min(df[[x_column]]), max(df[[x_column]]))
        ) +
      scale_y_continuous(labels = custom_label, expand = c(0, 0))

# Save the plot as PNG if save_path is specified
if (!is.null(save_path)) {
  ggsave(save_path, plot = p, width = plot_settings[["width"]],
          height = plot_settings[["height"]],
          dpi = plot_settings[["dpi"]])
}
  # Print the plot
  print(p)
}

stacked_plots <- function(data, subplot_column = NULL,
                        save_path = NULL, color_list = NULL,
                        y_label_suffix = "") {

  if (!is.null(subplot_column)) {
    data <- data %>%
      mutate(!!subplot_column := plot_settings[["rename"]][.data[[subplot_column]]]) %>%
      filter(!is.na(.data[[subplot_column]]))

    size_text <- 3
    size_point <- 1
    nudge_y <- 0
    round <- 1
  } else {
    size_text <- 8
    size_point <- 3
    nudge_y <- 0.15
    round <- 0
    }
  p <- data %>%
    ggplot(aes(x = scenario, y = value, fill = variable)) +
    geom_bar(stat = "identity") +
    geom_point(aes(x = scenario, y = total_value),
      color = "black", size = size_point, , show.legend = FALSE) +
    geom_text(aes(x = scenario,
      label = paste(round(total_value, round)), y = total_value),
      vjust = -0.5, nudge_y = nudge_y, size = size_text)

  if (!is.null(color_list)) {
    p <- p + scale_fill_manual(values = color_list)
  }

  if (!is.null(subplot_column)) {
    p <- p +
      facet_wrap(subplot_column, ncol = 4, scales = "free_y") +
      message_building_subplot_theme
  } else {
    p <- p +
      message_building_theme +
      theme(axis.line.y = element_blank())
  }

  # Custom label function that combines comma and suffix
  custom_label <- function(x) {
    label_comma()(x) %>% paste0(" ", y_label_suffix)
  }
  p <- p + 
    theme(axis.text.x = element_text(angle = 20, hjust = 1)) + 
    scale_y_continuous(labels = custom_label, expand = c(0, 0))

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