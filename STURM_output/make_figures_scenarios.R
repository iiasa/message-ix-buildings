# Description: This script generates the figures for the scenarios of the STURM model.

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(RColorBrewer)
print(paste("Working directory is:", getwd()))

# Loading figures setttings and functions
source("STURM_output/C00_plots.R")

run <- "2024-06-26_091653_optimal_scenarios"

scenarios <- c(
    "S1" = "No additional policy",
    "S46" = "Heat pump max",
    "S86" = "Renovation",
    "S179" = "improved depth with quality"
)

scenarios <- c(
    "S1" = "Baseline",
    "S143" = "All policies",
    "optimal_scenario" = "Min. cost national",
    "S137" = "Min. cost EU",
    "constraint_scenario" = "Min. cost national, constraint"
)



ref <- "Baseline"
# Generate distinct colors using RColorBrewer
num_scenarios <- length(scenarios)
colors <- brewer.pal(min(12, num_scenarios), "Set1") # Use Set3 palette with a maximum of 12 colors
if (num_scenarios > 12) {
  colors <- colorRampPalette(brewer.pal(12, "Set1"))(num_scenarios) # Generate more colors if needed
}
# Assign colors to scenarios
colors_scenarios <- setNames(colors, scenarios)
 
# Generate distinct line types
line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
num_scenarios <- length(scenarios)
if (num_scenarios > length(line_types)) {
  # If more scenarios than predefined line types, repeat line types with modification
  extended_line_types <- rep(line_types, length.out = num_scenarios)
} else {
  extended_line_types <- line_types[1:num_scenarios]
}

# Assign line types to scenarios
line_styles_scenarios <- setNames(extended_line_types, scenarios)


save_dir <- paste("STURM_output", "figures", sep = "/")
if (!dir.exists(save_dir)) {
  dir.create(save_dir)
}
save_dir <- paste(save_dir, run, sep = "/")
if (!dir.exists(save_dir)) {
  dir.create(save_dir)
}

save_dir <- paste(save_dir, "scenario_analysis", sep = "/")
if (!dir.exists(save_dir)) {
  dir.create(save_dir)
}

## Read data
data <- data.frame(NULL)

for (scenario in names(scenarios)) {
  print(paste("Scenario:", scenario))
  file <- paste0("report_agg_", scenario, ".csv")
  file <- paste("STURM_output/results", run, file, sep = "/")

  if (!file.exists(file)) {
    print("Check file name or directory!")
  }

  # Read output files
  temp <- read.csv(file) %>%
    mutate(scenario = scenario)
  
  data <- bind_rows(data, temp)
}

# Reconstuction flows
flows <- c("n_renovation", "cost_renovation_EUR", "sub_renovation_EUR",
  "n_replacement", "cost_heater_EUR", "sub_heater_EUR",
  "to_pay_renovation", "to_pay_heater", "taxes_revenues_EUR")

data <- data %>%
  mutate(scenario = scenarios[.data[["scenario"]]])

stp <- 5

data <- data %>%
 mutate(#year = ifelse(variable %in% flows, year - stp, year),
        value = ifelse(variable %in% flows, value
         / stp, value))
data <- distinct(data)

#--------------------------------------------------------------
## Processing output
### Table summary


base_year <- 2015

output_table(data, save_dir = save_dir, end_year = 2030, base_year = 2015)
format_temp <- output_table(data, save_dir = save_dir,
  end_year = 2050, base_year = 2015)

format_temp_eu <- format_temp %>%
  filter(Scenario != "Initial") %>%
  filter(`Member states` == "EU") %>%
  mutate(`Energy poverty (Percent)` = `Energy poverty (Percent)` * 100) %>%
  mutate(`Cost total (Billion EUR)` =
    `Cost renovation (Billion EUR)` + `Cost heater (Billion EUR)`) %>%
  mutate(`Cost total (Billion EUR per year)` =
    `Cost total (Billion EUR)` / 35) %>%
  mutate(Scenario = factor(Scenario), levels = unname(Scenario))

legend <- TRUE
presentation <- FALSE
scatter_plots(format_temp_eu,
              "Emission (MtCO2)",
              "Energy poverty (Percent)",
              color_column = "Scenario",
              colors_scenarios = colors_scenarios,
              size_column = "Cost total (Billion EUR per year)",
              legend_suffix = "B€/year",
              save_path = paste(save_dir,
                paste0(run, "_tCO2_poverty_eu.png"), sep = "/"),
              y_label_suffix = "%",
              x_label_suffix = "MtCO2",
              legend = legend,
              presentation = presentation,
              y_label = "Share of population in energy poverty",
              x_label = "Space heating emission (scope 2)"
              )

legend <- TRUE
presentation <- FALSE
scatter_plots(format_temp_eu,
              "Space heating consumption (TWh)",
              "Energy poverty (Percent)",
              color_column = "Scenario",
              colors_scenarios = colors_scenarios,
              size_column = "Cost total (Billion EUR per year)",
              legend_suffix = "B€/year",
              save_path = paste(save_dir,
                paste0(run, "_kWh_poverty_eu.png"), sep = "/"),
              y_label_suffix = "%",
              x_label_suffix = "TWh",
              legend = legend,
              presentation = presentation,
              y_label = "Share of population in energy poverty",
              x_label = "Space heating consumption"
              )

temp <- format_temp %>%
  filter(Scenario == "Current policies") %>%
  filter(`Member states` != "EU") %>%
  select(c("Scenario", "Member states", "Population (Million)",
    "Emission saving (%)", "Energy poverty (Percent)")) %>%
  mutate(`Emission saving (%)` = `Emission saving (%)` * 100,
         `Energy poverty (Percent)` = `Energy poverty (Percent)` * 100)


legend <- TRUE
presentation <- FALSE
scatter_plots(temp,
              "Emission saving (%)",
              "Energy poverty (Percent)",
              color_column = "Member states",
              colors_scenarios = plot_settings[["colors"]],
              size_column = "Population (Million)",
              legend_suffix = "M",
              save_path = paste(save_dir,
                paste0(run, "_tCO2_poverty_ms.png"), sep = "/"),
              y_label_suffix = "%",
              x_label_suffix = "%",
              legend = legend,
              presentation = presentation,
              y_label = "Share of population in energy poverty",
              x_label = "Space heating emission saving (%)"
              )
#--------------------------------------------------------------
### Cost-benefits analysis
source("STURM_output/C00_plots.R")

df <- make_cost_benefits(data, ref, save_dir, nb_years = 30, figures = TRUE, make_summary = FALSE)

var <- "Total cost"
temp <- df %>%
  filter(variable == var)

min_maps <- min(filter(df, variable == var)$value, na.rm = TRUE)
min_maps <- - 200
max_maps <- max(filter(df, variable == var)$value, na.rm = TRUE)
limits <- c(min_maps, max_maps)
figure_title <- ""
legend_title <- "euro/(hh.year)"
title <- "cba"

plot_map(temp,
  limits,
  threshold_colormap = 0,
  reverse_colormap = TRUE,
  figure_title = figure_title,
  legend_title = legend_title,
  subplot_column = "scenario",
  ncol = 4,
  save_path = paste(save_dir,
    paste0("map_", title, ".png"), sep = "/"))

#--------------------------------------------------------------
### Maps by scenarios

var <- "heat_tCO2"
ref <- "population"
years <- c(2050)
limits <- c(0, 0.8)
figure_title <- "Emission for space heating"
legend_title <- "tCO2/(capita.year)"
title <- "tCO2_capita"

df <- data %>%
    filter(variable %in% c(var, ref)) %>%
    filter(resolution == "all") %>%
    pivot_wider(id_cols = c(region_bld, year, resolution, scenario),
      names_from = variable,
      values_from = value) %>%
    filter(!is.na(.data[[var]])) %>%
    filter(!is.na(.data[[ref]])) %>%
    mutate(value = .data[[var]] / .data[[ref]]) %>%
    filter(year %in% years) %>%
    select(-c(all_of(var), all_of(ref), "resolution"))

if (!is.null(scenarios)) {
  df <- filter(df, scenario %in% scenarios) %>%
    mutate(scenario = factor(scenario, levels = unname(scenarios)))
}


plot_map(df,
  limits,
  threshold_colormap = 0.1,
  reverse_colormap = TRUE,
  figure_title = figure_title,
  legend_title = legend_title,
  subplot_column = "scenario",
  ncol = 3,
  save_path = paste(save_dir,
    paste0("map_", title, "_2050.png"), sep = "/"))


#--------------------------------------------------------------
var <- "heat_kWh"
ref <- "floor_m2"
years <- c(2050)
limits <- c(0, 200)
figure_title <- "Energy consumption for space heating"
legend_title <- "kWh/m2.year"
title <- "kwh_m2"

df <- data %>%
    filter(variable %in% c(var, ref)) %>%
    filter(resolution == "all") %>%
    pivot_wider(id_cols = c(region_bld, year, resolution, scenario),
      names_from = variable,
      values_from = value) %>%
    filter(!is.na(.data[[var]])) %>%
    filter(!is.na(.data[[ref]])) %>%
    mutate(value = .data[[var]] / .data[[ref]]) %>%
    filter(year %in% years) %>%
    select(-c(all_of(var), all_of(ref), "resolution"))

if (!is.null(scenarios)) {
  df <- filter(df, scenario %in% scenarios)
}

plot_map(df,
  limits,
  figure_title = figure_title,
  legend_title = legend_title,
  subplot_column = "scenario",
  ncol = 3,
  save_path = paste(save_dir,
    paste0("map_", title, "_2050.png"), sep = "/"))

#--------------------------------------------------------------
### Clustered stack bar plot by countries
#### Fuel types

var <- "stock_building"
years <- c(2050)
region <- c("C-WEU-DEU", "C-WEU-FRA", "C-WEU-SWE", "C-EEU-POL")

df <- data %>%
  filter(resolution %in% names(rename_fuels)) %>%
  filter(variable == var) %>%
  filter(region_bld %in% region) %>%
  filter(year %in% years) %>%
  mutate(value = value / 1e6) %>%
  mutate(region_bld = rename_countries[.data[["region_bld"]]])


plot_clustered_barplot(df,
  "scenario",
  "resolution",
  subplot_column = "region_bld",
  y_label = "Dwelling units by fuel type",
  y_label_suffix = "Million",
  display_total = FALSE,
  x_order = scenarios,
  angle_x_label = 90,
  save_path = paste(save_dir, paste0(run, "_dwelling_fuel_year_scenario_stack.png"), sep = "/"))

#### PE Consumption
var <- "stock_building"
yr <- 2050
initial <- TRUE
region <- c("C-WEU-DEU", "C-WEU-FRA", "C-WEU-SWE", "C-EEU-POL")

region <- NULL
breakdown <- rename_primary
y_label <- "Dwelling units by primary energy"
percent <- TRUE
if (percent) {
  y_label_suffix <- "%"
} else {
  y_label_suffix <- "Million"
}
file <- "_dwelling_pe_year_scenario_stack.png"

df <- data %>%
  filter(resolution %in% names(breakdown)) %>%
  filter(variable == var)

if (initial) {
  temp <- df %>%
    filter(year == 2015) %>%
    filter(scenario == "Current policies") %>%
    mutate(scenario = "Initial") %>%
    mutate(year = yr)
  
  df <- bind_rows(df, temp)
  temp_scenarios <- c("Initial", scenarios)
}

df <- df %>%
  filter(year == yr) %>%
  mutate(value = value / 1e6)

region_grouping <- df %>%
  filter(resolution %in% c(">200", "150-200")) %>%
  group_by_at("region_bld") %>%
  summarise(order_by = sum(value)) %>%
  ungroup()

df <- df %>%
  left_join(region_grouping, by = "region_bld") %>%
  mutate(order_by = ifelse(is.na(order_by), 0, order_by)) %>%
  arrange(desc(order_by)) %>%
  select(-order_by)


if (!is.null(region)) {
  df <- df %>%
    filter(region_bld %in% region)
} else {
  # Select only the top 5 regions
  df <- df %>%
    filter(region_bld != "EU") %>%
    filter(region_bld %in% head(unique(df$region_bld), 5 + 1))
}

if (percent) {
  df <- df %>%
    group_by(scenario, region_bld) %>%
    mutate(value = value / sum(value) * 100) %>%
    ungroup()
}

df <- mutate(df, region_bld = rename_countries_code[.data[["region_bld"]]])

plot_clustered_barplot(df,
  "scenario",
  "resolution",
  subplot_column = "region_bld",
  y_label = y_label,
  y_label_suffix = y_label_suffix,
  display_total = FALSE,
  x_order = temp_scenarios,
  angle_x_label = 90,
  legend = FALSE,
  save_path = paste(save_dir, paste0(run, file), sep = "/"))


### Dwelling units by fuel type

var <- "stock_building"
years <- c(2015, 2030, 2050)

df <- data %>%
  filter(resolution %in% names(rename_fuels)) %>%
  filter(variable == var) %>%
  filter(region_bld == "EU") %>%
  filter(year %in% years) %>%
  mutate(value = value / 1e6)

plot_clustered_barplot(df,
  "scenario",
  "resolution",
  subplot_column = "year",
  y_label = "Dwelling units in the EU by fuel type",
  year_start = 2015,
  y_label_suffix = "Million",
  display_total = FALSE,
  x_order = scenarios,
  angle_x_label = 90,
  save_path = paste(save_dir, paste0(run, "_dwelling_fuel_year_scenario_stack.png"), sep = "/"))

# save data
write.csv(df, paste(save_dir, paste0(run, "_dwelling_fuel_year_scenario_stack.csv"), sep = "/"))

### Dwelling units by energy efficiency
var <- "stock_building"
years <- c(2015, 2030, 2050)

df <- data %>%
  filter(resolution %in% names(rename_eneff)) %>%
  filter(variable == var) %>%
  filter(region_bld == "EU") %>%
  filter(year %in% years) %>%
  mutate(value = value / 1e6)

plot_clustered_barplot(df,
  "scenario",
  "resolution",
  y_label = "Dwelling units in the EU by fuel type",
  y_label_suffix = "Million",
  display_total = FALSE,
  x_order = scenarios,
  angle_x_label = 90,
  save_path = paste(save_dir, paste0(run, "_dwelling_eneff_year_scenario_stack.png"), sep = "/"))

### Dwelling units by insulation_level
var <- "stock_building"
years <- c(2015, 2030, 2050)

df <- data %>%
  filter(resolution %in% names(rename_insulation)) %>%
  filter(variable == var) %>%
  filter(region_bld == "EU") %>%
  filter(year %in% years) %>%
  mutate(value = value / 1e6)

plot_clustered_barplot(df,
  "scenario",
  "resolution",
  y_label = "Dwelling units in the EU by fuel type",
  y_label_suffix = "Million",
  display_total = FALSE,
  x_order = scenarios,
  angle_x_label = 90,
  save_path = paste(save_dir, paste0(run, "_dwelling_insulation_year_scenario_stack.png"), sep = "/"))

### Dwelling units by primary energy
var <- "stock_building"
years <- c(2015, 2030, 2050)

df <- data %>%
  filter(resolution %in% names(rename_primary)) %>%
  filter(variable == var) %>%
  filter(region_bld == "EU") %>%
  filter(year %in% years) %>%
  mutate(value = value / 1e6)

plot_clustered_barplot(df,
  "scenario",
  "resolution",
  y_label = "Dwelling units in the EU by fuel type",
  y_label_suffix = "Million",
  display_total = FALSE,
  x_order = scenarios,
  angle_x_label = 90,
  save_path = paste(save_dir, paste0(run, "_dwelling_pe_year_scenario_stack.png"), sep = "/"))

# save data
write.csv(df, paste(save_dir, paste0(run, "_dwelling_pe_year_scenario_stack.csv"), sep = "/"))


### Space heating consumption by fuel type and scenario
var <- "heat_kWh"
years <- c(2015, 2030, 2050)

df <- data %>%
  filter(resolution %in% names(rename_fuels)) %>%
  filter(variable == var) %>%
  filter(region_bld == "EU") %>%
  filter(year %in% years) %>%
  mutate(value = value / 1e9)

plot_clustered_barplot(df,
  "scenario",
  "resolution",
  y_label = "Space heating consumption in the EU by fuel type",
  y_label_suffix = "TWh",
  display_total = TRUE,
  x_order = scenarios,
  angle_x_label = 90,
  save_path = paste(save_dir, paste0(run, "_kWh_year_scenario_stack.png"), sep = "/"))

# save data
write.csv(df, paste(save_dir, paste0(run, "_kWh_year_scenario_stack.csv"), sep = "/"))

### Space heating emission by fuel type and scenario
var <- "heat_tCO2"
df <- data %>%
  filter(resolution %in% names(rename_fuels)) %>%
  filter(variable == var) %>%
  filter(region_bld == "EU") %>%
  filter(year %in% years) %>%
  mutate(value = value / 1e6)

plot_clustered_barplot(df,
  "scenario",
  "resolution",
  y_label = "Space heating emission",
  y_label_suffix = "MtCO2",
  x_order = scenarios,
  angle_x_label = 90,
  save_path = paste(save_dir, paste0(run, "_tCO2_year_scenario_stack.png"), sep = "/"))

### Line plots figures
#### Space heating consumption

temp <- data %>%
  filter(variable == "heat_kWh") %>%
  filter(resolution %in% "all") %>%
  select(-c("variable", "resolution")) %>%
  mutate(value = value / 1e9)

plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Residential space heating consumption in EU\nTWh",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_heat_kWh_countries.png"), sep = "/")
    )

legend <- TRUE
presentation <- FALSE
plot_multiple_lines(filter(temp, region_bld == "EU"),
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Residential space heating consumption",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    #line_types = line_styles_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_heat_kWh_eu.png"), sep = "/"),
    legend = legend,
    y_label_suffix = "TWh",
    presentation = presentation,
    line_order = unname(scenarios))

# save data
write.csv(filter(temp, region_bld == "EU"), paste(save_dir, paste0(run, "_heat_kWh_eu.csv"), sep = "/"))


##### Electricity consumption
temp <- data %>%
  filter(variable == "heat_kWh") %>%
  filter(resolution %in% c("heat_pump", "electricity")) %>%
  group_by_at(c("region_bld", "year", "scenario")) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  mutate(value = value / 1e9)

plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Residential space heating electricity consumption in EU\nTWh",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_heat_kWh_electricity_countries.png"), sep = "/"))

legend <- TRUE
presentation <- FALSE
plot_multiple_lines(filter(temp, region_bld == "EU"),
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Residential space heating electricity consumption in EU\nTWh",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    line_types = line_styles_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_heat_kWh_electricity_eu.png"), sep = "/"),
    y_label_suffix = "TWh",
    legend = legend,
    presentation = presentation,
    line_order = unname(scenarios))

# save data
write.csv(filter(temp, region_bld == "EU"), paste(save_dir, paste0(run, "_heat_kWh_electricity_eu.csv"), sep = "/"))

##### Standard consumption
temp <- data %>%
  filter(variable == "heat_std_kWh") %>%
  filter(resolution %in% "all") %>%
  select(-c("variable", "resolution")) %>%
  mutate(value = value / 1e9)

plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Consumtion standard\nTWh",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_heat_std_kWh_countries.png"), sep = "/"))

plot_multiple_lines(filter(temp, region_bld == "EU"),
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Consumtion standard\nTWh",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    line_types = line_styles_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_heat_std_kWh_eu.png"), sep = "/"),
    y_label_suffix = "TWh")

##### Emission
temp <- data %>%
  filter(variable == "heat_tCO2") %>%
  filter(resolution %in% "all") %>%
  select(-c("variable", "resolution")) %>%
  mutate(value = value / 1e6)

plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Residential space heating emission scope 2\nMtCO2",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_heat_tCO2_countries.png"), sep = "/"))

legend <- TRUE
presentation <- FALSE
plot_multiple_lines(filter(temp, region_bld == "EU"),
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Residential space heating emission scope 2",
    free_y = TRUE,
    line_types = line_styles_scenarios,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_heat_tCO2_eu.png"), sep = "/"),
    legend = legend,
    y_label_suffix = "MtCO2",
    presentation = presentation,
    line_order = unname(scenarios))

# save data
write.csv(filter(temp, region_bld == "EU"), paste(save_dir, paste0(run, "_heat_tCO2_eu.csv"), sep = "/"))


#### Number of renovations cumulated
temp <- data %>%
  filter(variable == "n_renovation") %>%
  filter(resolution %in% "all") %>%
  select(-c("variable", "resolution")) %>%
  group_by(scenario, region_bld) %>%
  arrange(year) %>%
  mutate(value = cumsum(value)) %>%
  ungroup() %>%
  mutate(value = value * 5 / 1e3)

plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Cumulated number of renovations\nThousand",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_renovation_cumulated_countries.png"), sep = "/"),
    line_order = unname(scenarios))

legend <- FALSE
presentation <- TRUE
plot_multiple_lines(filter(temp, region_bld == "EU"),
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Cumulated number of renovations\nThousand",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    legend = legend,
    presentation = presentation,
    save_path = paste(save_dir,
      paste0(run, "_renovation_cumulated_eu.png"), sep = "/"),
    line_order = unname(scenarios))

#### Number of renovations by income cumulated
temp <- data %>%
  filter(region_bld == "EU") %>%
  filter(variable == "n_renovation") %>%
  filter(resolution %in% c("q1", "q2", "q3")) %>%
  group_by(scenario, resolution) %>%
  arrange(year) %>%
  mutate(value = cumsum(value)) %>%
  ungroup() %>%
  mutate(value = value * 5 / 1e3) %>%
  select(-c("variable", "region_bld"))


plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "resolution",
    ncol = 3,
    y_label = "Cumulated number of renovations\nThousand",
    free_y = FALSE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_renovation_cumulated_income_eu.png"), sep = "/"))

#### Number of renovation
temp <- data %>%
  filter(variable == "n_renovation") %>%
  filter(resolution %in% "all") %>%
  select(-c("variable", "resolution")) %>%
  mutate(value = value / 1e3)

plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Renovation\nThousands",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_renovation_countries.png"), sep = "/"))

plot_multiple_lines(filter(temp, region_bld == "EU"),
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Renovation\nThousands",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_renovation_eu.png"), sep = "/"))

#### Number of heat-pumps
temp <- data %>%
  filter(variable == "stock_building") %>%
  filter(resolution %in% "heat_pump") %>%
  select(-c("variable", "resolution")) %>%
  mutate(value = value / 1e6)

plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Stock Heat-pumps\nMillion",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_heat_pumps_countries.png"), sep = "/"))

plot_multiple_lines(filter(temp, region_bld == "EU"),
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Stock Heat-pumps\nMillion",
    free_y = TRUE,
    line_types = line_styles_scenarios,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_heat_pumps_eu.png"), sep = "/"),
    line_order = unname(scenarios))

#### Cost of renovation
temp <- data %>%
  filter(variable == "cost_renovation_EUR") %>%
  filter(resolution %in% "all") %>%
  select(-c("variable", "resolution")) %>%
  mutate(value = value / 1e9)

plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Cost Energy Renovation\nBillion EUR per year",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_cost_renovation_countries.png"), sep = "/"))


legend <- FALSE
presentation <- TRUE
plot_multiple_lines(filter(temp, region_bld == "EU"),
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Cost Energy Renovation\nBillion EUR per year",
    free_y = TRUE,
    legend = legend,
    presentation = presentation,
    y_label_suffix = "B€/year",
    colors_lines = colors_scenarios,
    line_types = line_styles_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_cost_renovation_eu.png"), sep = "/"),
    line_order = unname(scenarios))

#### Cost heating system
temp <- data %>%
  filter(variable == "cost_heater_EUR") %>%
  filter(resolution %in% "all") %>%
  select(-c("variable", "resolution")) %>%
  mutate(value = value / 1e9)

plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Cost Heating system\nBillion EUR per year",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_cost_heater_countries.png"), sep = "/"))

plot_multiple_lines(filter(temp, region_bld == "EU"),
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Cost Heating system\nBillion EUR per year",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    line_types = line_styles_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_cost_heater_eu.png"), sep = "/"),
    line_order = unname(scenarios))

#### Cost heating 
temp <- data %>%
  filter(variable == "cost_heat_EUR") %>%
  filter(resolution %in% "all") %>%
  select(-c("variable", "resolution")) %>%
  mutate(value = value / 1e9)

plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Cost Heating\nBillion EUR",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_cost_heat_countries.png"), sep = "/"))

plot_multiple_lines(filter(temp, region_bld == "EU"),
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Cost Heating\nBillion EURn",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_cost_heat_eu.png"), sep = "/"))


### Indicators comparison
#### kWh per square meter
var <- "heat_kWh"
ref <- "floor_m2"
path <- "_kwh_m2_countries.png"
y_label <- "Space heating consumption\nkWh per square meter"

temp <- data %>%
  filter(variable %in% c(var, ref)) %>%
  filter(resolution == "all") %>%
  pivot_wider(id_cols = c(region_bld, year, scenario, resolution),
    names_from = variable,
    values_from = value) %>%
  filter(!is.na(.data[[var]])) %>%
  filter(!is.na(.data[[ref]])) %>%
  mutate(value = .data[[var]] / .data[[ref]]) %>%
  select(-c(var, ref, "resolution"))


plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = y_label,
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, path), sep = "/"))

path <- "_kwh_m2_eu.png"
temp <- filter(temp, region_bld == "EU")
plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = y_label,
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, path), sep = "/"))


#### kWh per dwelling
var <- "heat_kWh"
ref <- "stock_building"

temp <- data %>%
  filter(variable %in% c(var, ref)) %>%
  filter(resolution == "all") %>%
  pivot_wider(id_cols = c(region_bld, year, scenario, resolution),
    names_from = variable,
    values_from = value) %>%
  filter(!is.na(.data[[var]])) %>%
  filter(!is.na(.data[[ref]])) %>%
  mutate(value = .data[[var]] / .data[[ref]]) %>%
  select(-c(var, ref, "resolution"))


plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Space heating consumption\nkWh per dwelling",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_kwh_dw_countries.png"), sep = "/"))

path <- "_kwh_dw_eu.png"
temp <- filter(temp, region_bld == "EU")
plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Space heating consumption\nkWh per dwelling",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_kwh_dw_eu.png"), sep = "/"),
    y_label_suffix = "kWh/dw")

#### kWh std per m2
var <- "heat_std_kWh"
ref <- "floor_m2"

temp <- data %>%
  filter(variable %in% c(var, ref)) %>%
  filter(resolution == "all") %>%
  pivot_wider(id_cols = c(region_bld, year, scenario, resolution),
    names_from = variable,
    values_from = value) %>%
  filter(!is.na(.data[[var]])) %>%
  filter(!is.na(.data[[ref]])) %>%
  mutate(value = .data[[var]] / .data[[ref]]) %>%
  select(-c(var, ref, "resolution"))


plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Space heating consumption\nkWh per m2",
    free_y = FALSE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_kwh_std_m2_countries.png"), sep = "/"))

path <- "_kwh_m2_eu.png"
temp <- filter(temp, region_bld == "EU")
plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Space heating consumption\nkWh per dwelling",
    free_y = FALSE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_kwh_std_m2_eu.png"), sep = "/"),
    y_label_suffix = "kWh/m2")

#### kWh per m2
var <- "heat_kWh"
ref <- "floor_m2"

temp <- data %>%
  filter(variable %in% c(var, ref)) %>%
  filter(resolution == "all") %>%
  pivot_wider(id_cols = c(region_bld, year, scenario, resolution),
    names_from = variable,
    values_from = value) %>%
  filter(!is.na(.data[[var]])) %>%
  filter(!is.na(.data[[ref]])) %>%
  mutate(value = .data[[var]] / .data[[ref]]) %>%
  select(-c(var, ref, "resolution"))


plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Space heating consumption\nkWh per m2",
    free_y = FALSE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_kwh_m2_countries.png"), sep = "/"))

path <- "_kwh_m2_eu.png"
temp <- filter(temp, region_bld == "EU")
plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Space heating consumption\nkWh per dwelling",
    free_y = FALSE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_kwh_m2_eu.png"), sep = "/"),
    y_label_suffix = "kWh/m2")

#### Renovation rate
var <- "n_renovation"
ref <- "stock_building"

temp <- data %>%
  filter(variable %in% c(var, ref)) %>%
  filter(resolution == "all") %>%
  pivot_wider(id_cols = c(region_bld, year, scenario, resolution),
    names_from = variable,
    values_from = value) %>%
  filter(!is.na(.data[[var]])) %>%
  filter(!is.na(.data[[ref]])) %>%
  mutate(value = .data[[var]] / .data[[ref]]) %>%
  select(-c(var, ref, "resolution"))


plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Renovation rate",
    free_y = FALSE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_renovation_rate_countries.png"), sep = "/"))

presentation <- FALSE
legend <- TRUE
path <- "_renovation_rate_eu.png"
temp <- filter(temp, region_bld == "EU")
plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Renovation rate",
    free_y = FALSE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_renovation_rate_eu.png"), sep = "/"),
    presentation = presentation,
    legend = legend,
    y_label_suffix = "%")

#### Energy poverty
var <- "energy_poverty_thresh"
ref <- "stock_building"
temp <- data %>%
  filter(variable %in% c(var, ref)) %>%
  filter(resolution == "all") %>%
  pivot_wider(id_cols = c(region_bld, year, scenario, resolution),
    names_from = variable,
    values_from = value) %>%
  filter(!is.na(.data[[var]])) %>%
  filter(!is.na(.data[[ref]])) %>%
  mutate(value = .data[[var]] / .data[[ref]]) %>%
  select(-c(var, ref, "resolution")) %>%
  mutate(value = value * 100)


plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "region_bld",
    ncol = 4,
    y_label = "Energy poverty rate (%)",
    free_y = FALSE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_energy_poverty_countries.png"), sep = "/"),
    y_label_suffix = "%")

legend <- FALSE
presentation <- FALSE
plot_multiple_lines(filter(temp, region_bld == "EU"),
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = NULL,
    ncol = 4,
    y_label = "Energy poverty rate (%)",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_energy_poverty_eu.png"), sep = "/"),
    y_label_suffix = "%",
    legend = legend,
    presentation = presentation,
    line_types = line_styles_scenarios,
    line_order = unname(scenarios))

  # save data
write.csv(filter(temp, region_bld == "EU"), paste(save_dir, paste0(run, "_energy_poverty_eu.csv"), sep = "/"))

#### Energy poverty by income class
temp <- data %>%
  filter(region_bld == "EU") %>%
  filter(variable == "energy_poverty_thresh") %>%
  filter(resolution %in% c("q1", "q2", "q3")) %>%
  mutate(value = value / 1e6) %>%
  select(-c("variable", "region_bld"))

plot_multiple_lines(temp,
    x_column = "year",
    y_column = "value",
    line_column = "scenario",
    group_column = "resolution",
    ncol = 4,
    y_label = "Energy poverty\nMillion",
    free_y = TRUE,
    colors_lines = colors_scenarios,
    save_path = paste(save_dir,
      paste0(run, "_energy_poverty_income.png"), sep = "/"),
    y_label_suffix = "M")

#--------------------------------------------------------------
### Distributional consequences

source("STURM_output/C00_plots.R")

years <- c(2030, 2050)

rename_income <- c("q1" = "Tertile 1", "q2" = "Tertile 2", "q3" = "Tertile 3")

temp <- data %>%
    filter(resolution %in% c("q1", "q2", "q3"))

temp <- calculate_cost_hh(temp)

if (!is.null(rename_income)) {
  temp <- temp %>%
    mutate(resolution = ifelse(resolution %in% names(rename_income), rename_income[.data[["resolution"]]], resolution))
}

# at EU-level
parse_data <- temp %>%
  filter(region_bld == "EU") %>%
  mutate(value = value)

# Order of scenarios
parse_data <- parse_data %>%
  mutate(scenario = factor(scenario, levels = unname(scenarios)))

save_path <- paste(save_dir, paste0(run, "_cost_average_hh_eu.png"), sep = "/")
make_cost_hh_figures(temp, ref, save_path, x_column = "scenario",
  subplot_column = "resolution", angle_x_label = 20)