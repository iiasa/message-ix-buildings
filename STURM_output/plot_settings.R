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
            "electricity" = "Electricity",
            "gas" = "Gas",
            "oil" = "Oil")

order_fuels <- c("Coal", "Oil", "Gas", "District heating", "Electricity", "Biomass")

colors_fuels <- c("No Heating" = "grey92",
  "Electricity" = "slateblue",
  "Gas" = "skyblue2",
  "District heating" = "lightgoldenrod3",
  "Oil" = "navajowhite4",
  "Coal" = "grey5",
  "Biomass" = "palegreen4")

plot_settings <- list(
  "size_axis" = 5, # axes
  "size_title" = 24, # axes and legend
  "size_text" = 24,
  "width" = 16, #width cm
  "height" = 1.3 * 16, #height cm
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


# Function to create Message-ix-Buidling Figures

plot_stacked_area <- function(data, x_column, y_column, fill_column,
                              y_label = "", save_path = NULL) {
  # Create the stacked area plot
  group <- c(x_column, fill_column)
  p <- data %>%
    mutate(!!fill_column := plot_settings[["rename"]][.data[[fill_column]]]) %>%
    group_by(across(all_of(group))) %>%
    summarise(total = sum(.data[[y_column]])) %>%
    ungroup() %>%
    mutate(!!sym(fill_column) := factor(!!sym(fill_column), levels = rev(plot_settings[["order"]]))) %>%
    ggplot(aes(x = .data[[x_column]],
               y = total,
               fill = .data[[fill_column]])) +
    geom_area(position = "stack") +
    # guides(fill = guide_legend(reverse=TRUE)) +
    scale_fill_manual(values = plot_settings[["colors"]]) +
    labs(title = y_label,
         fill = str_replace_all(str_to_title(fill_column), "_", " ")) +
    message_building_theme  +
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


plot_variable_evolution <- function(data,
    x_column,
    y_column,
    group_column,
    subplot = FALSE,
    y_label = "",
    save_path = NULL) {
  # Group and summarize data
  group <- c(x_column, group_column)

  summarized_data <- data %>%
    # Group by year and group_column
    group_by(across(all_of(group))) %>%
    summarise(value = sum(.data[[y_column]])) %>%
    ungroup()
  # Convert year column to numeric
  summarized_data$year <- as.numeric(summarized_data$year)
  if (subplot) {
    # Create subplot for each instance of group_column
    p <- ggplot(summarized_data, aes(x = .data[[x_column]], y = value)) +
                geom_line() +
                facet_wrap(group_column) +
                message_building_theme
  } else {
    # Create line plot on the same axis
    p <- ggplot(summarized_data, aes(x = .data[[x_column]],
                                y = value,
                                color = .data[[group_column]])) +
      geom_line() +
      message_building_theme
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



alessio_function <- function(data){
  p2f1_eneff <- data %>%
    group_by(scenario, region2, eneff2) %>%
    summarise(floor_Mm2 = sum(floor_Mm2)) %>%
    ungroup() %>%
    ggplot() +
    geom_bar(aes(x=scenario, y= floor_Mm2/1e3, fill=as_factor(desc(eneff2))), stat="identity") +
    scale_fill_manual(values = rev(col_eneff2), breaks = levels(as_factor(desc(e2f1$eneff2))), # Add breaks to remove NA from legend
                      labels = rev(lab_eneff2), name = "Housing cohorts") +
    theme_bw()+
    theme(    legend.position = "bottom",
              strip.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y=element_text(size=sztxt),
              axis.title.x = element_blank(),
              axis.title=element_text(size=sztitle)
    ) +
    scale_y_continuous(limits=c(0, 300),breaks=seq(0,300,100), expand = c(0,0)) +
    scale_x_discrete(breaks = scenarios_axis_lab[1:7]) +
    labs(
      x ="Scenario", y = "Floorspace (bn m2)") +
    facet_wrap(~region2,nrow=1, labeller = region_labeller)
    }