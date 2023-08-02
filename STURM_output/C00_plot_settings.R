# MESSAGEix-Buildings - color schemes

library(dplyr)
library(ggplot2)
library(stringr)



# Energy efficiency cohorts
rename_eneff <- c("adv" = "Renovated advanced",
  "avg" = "Average",
  "std" = "Renovated standard",
  "p1" = "Existing <1945",
  "p2" = "Existing 1946-1990",
  "p3" = "Existing 1991-2015",
  "p5" = "New standard"
)

order_eneff <- c("Existing <1945", "Existing 1946-1990", "Existing 1991-2015",
  "Renovated standard", "Renovated advanced", "New standard")


colors_efficiency <- c("Slum" = "grey30",
                "Existing <1945" = "coral4",
                "Existing 1946-1990" = "coral3",
                "Existing 1991-2015" = "coral2",
                "Average" = "grey30",
                "Renovated standard" = "khaki1",
                "Renovated advanced" = "goldenrod1",
                "New standard" = "palegreen3",
                "New advanced" = "palegreen4")



# Heating fuels
rename_fuels <- c("biomass_solid" = "Biomass",
            "coal" = "Coal",
            "district_heat" = "District heating",
            "electricity" = "Direct electric",
            "heat_pump" = "Heat pump",
            "gas" = "Gas",
            "oil" = "Oil")

order_fuels <- c("Coal", "Oil", "Gas", "District heating", "Direct electric", "Heat pump", "Biomass")

colors_fuels <- c("No Heating" = "grey92",
  "Direct electric" = "slateblue",
  "Heat pump" = "slateblue4",
  "Gas" = "skyblue2",
  "District heating" = "lightgoldenrod3",
  "Oil" = "navajowhite4",
  "Coal" = "grey5",
  "Biomass" = "palegreen4")

plot_settings <- list(
  "size_axis" = 5, # axes
  "size_title" = 24, # axes and legend
  "size_text" = 24,
  "small_size_text" = 12,
  "small_size_title" = 18,
  "width" = 16, #width cm
  "height" = 1 * 16, #height cm
  "dpi" = 300, #DPI
  "font_family" = "Arial",
  "colors" = c(colors_efficiency, colors_fuels),
  "rename" = c(rename_eneff, rename_fuels),
  "order" = c(order_fuels, order_eneff)
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
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = plot_settings[["size_text"]]),
          legend.position = "right",
          # Top, Right, Bottom and Left margin
          plot.margin = margin(t = 0,
                      r = 0,
                      b = 0,
                      l = 0)
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
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = plot_settings[["size_text"]]),
          legend.position = "right",
          # Top, Right, Bottom and Left margin
          plot.margin = margin(t = 0,
                      r = 0,
                      b = 0,
                      l = 0)
          )


# Function to create Message-ix-Buidling Figures

plot_stacked_area <- function(data, x_column, y_column, fill_column,
                              y_label = "", save_path = NULL) {
  # Create the stacked area plot
  # Calculate the breaks and labels for the x-axis
  breaks <- seq(ceiling(min(data[[x_column]])),
    floor(max(data[[x_column]])), by = 5)
  labels <- breaks[seq(1, length(breaks), by = 5)]

  group <- c(x_column, fill_column)
  p <- data %>%
    mutate(!!fill_column := plot_settings[["rename"]][.data[[fill_column]]]) %>%
    group_by(across(all_of(group))) %>%
    summarise(total = sum(.data[[y_column]])) %>%
    ungroup() %>%
    mutate(!!sym(fill_column) := factor(!!sym(fill_column),
      levels = rev(plot_settings[["order"]]))) %>%
    ggplot(aes(x = .data[[x_column]],
               y = total,
               fill = .data[[fill_column]])) +
    geom_area(position = "stack") +
    scale_fill_manual(values = plot_settings[["colors"]]) +
    labs(title = y_label,
         fill = str_replace_all(str_to_title(fill_column), "_", " ")) +
    message_building_theme  +
    scale_x_continuous(breaks = seq(min(data[[x_column]]),
      max(data[[x_column]]), 5),
      labels = seq(min(data[[x_column]]), max(data[[x_column]]), 5)) +
    scale_y_continuous(labels =
      function(x) format(x, big.mark = ",", scientific = FALSE))

  # Save the plot as PNG if save_path is specified
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = plot_settings[["width"]],
           height = plot_settings[["height"]],
           dpi = plot_settings[["dpi"]])
  }
  # Print the plot
  print(p)
}

plot_stacked_areas <- function(data, x_column, y_column, fill_column, subplot_column,
                              y_label = "", save_path = NULL, ncol = 4, percent = TRUE,
                              vertical = NULL) {
  # Create the stacked area plot
  # Calculate the breaks and labels for the x-axis
  breaks <- seq(ceiling(min(data[[x_column]])),
    floor(max(data[[x_column]])), by = 5)
  labels <- breaks[seq(1, length(breaks), by = 5)]

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
    facet_wrap(subplot_column, ncol = ncol) +
    scale_fill_manual(values = plot_settings[["colors"]]) +
    labs(title = y_label,
         fill = str_replace_all(str_to_title(fill_column), "_", " ")) +
    message_building_subplot_theme  +
    scale_x_continuous(breaks = c(min(data[[x_column]]), max(data[[x_column]])),
      labels = c(min(data[[x_column]]), max(data[[x_column]])))
  
  if (percent) {
    p <- p +
      scale_y_continuous(breaks =  c(0, 0.5, 1),
        labels = function(x) paste0(format(100 * x, digits = 1), "%"))
  } else {
    p <- p +
      scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))
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



plot_variable_evolution <- function(df,
    x_column,
    y_column,
    group_column,
    rename_group,
    ncol = 3,
    subplot = FALSE,
    y_label = "",
    save_path = NULL) {
  # Group and summarize data
  group <- c(x_column, group_column)


  summarized_data <- df %>%
    mutate(!!group_column := rename_group[.data[[group_column]]]) %>%
    # Group by year and group_column
    group_by(across(all_of(group))) %>%
    summarise(value = sum(.data[[y_column]])) %>%
    ungroup()

  # Convert year column to numeric
  summarized_data$year <- as.numeric(summarized_data$year)
  if (subplot) {
    # Create subplot for each instance of group_column
    p <- ggplot(summarized_data, aes(x = .data[[x_column]], y = value)) +
                geom_line(size = 1.5) +
                expand_limits(y = 0) +
                facet_wrap(group_column, ncol = ncol) +
                message_building_theme +
                scale_x_continuous(breaks = c(2020, 2040)) +
                labs(title = y_label) +
                scale_y_continuous(labels =
                  function(x) format(x, big.mark = ",", scientific = FALSE))

  } else {
    # Create line plot on the same axis
    p <- ggplot(summarized_data, aes(x = .data[[x_column]],
                                y = value,
                                color = .data[[group_column]])) +
      geom_line(size = 1.5) +
      message_building_theme +
      theme(legend.position = "right") +
      labs(title = y_label) +
      scale_y_continuous(labels =
        function(x) format(x, big.mark = ",", scientific = FALSE)) +
      expand_limits(y = 0)
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

profitability_curve <- function(data, x_column, y_column, save_path, ncol = 4) {
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