# Compare aligned R_mitig, R_mitig_agg and R_mitig_mod: GAS + ELECTRICITY.
# Read-only comparison: does not run simulations or modify aligned inputs.
library(tidyverse)

# USER SETTINGS ---------------------------------------------------------------
sturm_dir <- "C:/Users/nawawi/Desktop/message-ix-buildings-yiyi/message_ix_buildings/sturm"
input_dir <- file.path(sturm_dir, "message_linking")
output_dir <- file.path(input_dir, "scenario_comparison")
scenarios <- c("R_mitig", "R_mitig_agg", "R_mitig_mod")
baseline <- "R_mitig"
trials <- setdiff(scenarios, baseline)
u_EJ_GWa <- 31.71 # Identical to run_MIXB_aligner.R
colours <- c(R_mitig = "#80B1D3", R_mitig_agg = "#6A3D9A", R_mitig_mod = "#D95F02")
line_types <- c(R_mitig = "dashed", R_mitig_agg = "solid", R_mitig_mod = "solid")
components <- c(resid_sturm = "Residential STURM", comm_sturm = "Commercial STURM",
                resid_comm_glance = "GLANCE")
fuel_commodities <- list(
  gas = c("resid_heat_gas", "resid_hotwater_gas", "comm_heat_gas",
          "comm_hotwater_gas", "resid_apps_gas"),
  electricity = c("resid_heat_electr", "resid_hotwater_electr", "resid_cool_electr",
                  "resid_other_uses_electr", "comm_heat_electr", "comm_hotwater_electr",
                  "comm_cool_electr", "comm_other_uses_electr",
                  "resid_cook_electr", "resid_apps_electr")
)
fuel_labels <- c(gas = "Gas", electricity = "Electricity")

# LOAD ALL NINE EXPORTS --------------------------------------------------------
paths <- unlist(lapply(scenarios, function(s) {
  file.path(input_dir, paste0(names(components), "_aligned_", s, ".csv"))
}))
if (any(!file.exists(paths))) {
  stop("Missing aligned exports:\n", paste(paths[!file.exists(paths)], collapse = "\n"))
}
read_demand <- function(s, stem) {
  path <- file.path(input_dir, paste0(stem, "_aligned_", s, ".csv"))
  x <- read_csv(path, show_col_types = FALSE)
  required <- c("node", "commodity", "level", "year", "time", "value", "unit")
  if (!all(required %in% names(x))) stop("Missing columns: ", path)
  x <- x %>% mutate(commodity = sub("^resids_", "resid_", commodity))
  if (anyDuplicated(select(x, all_of(setdiff(required, "value"))))) {
    stop("Duplicate demand keys: ", path)
  }
  x <- x %>% filter(commodity %in% unlist(fuel_commodities, use.names = FALSE))
  if (!nrow(x)) stop("No expected gas/electricity commodities: ", path)
  if (anyNA(x) || any(!is.finite(x$value)) || any(x$value < 0)) {
    stop("Missing, nonfinite or negative demand: ", path)
  }
  if (any(x$unit != "GWa" | x$level != "final" | x$time != "year")) {
    stop("Expected final-level annual energy demand in GWa: ", path)
  }
  x %>% mutate(scenario = s, component = unname(components[stem]),
               fuel = if_else(commodity %in% fuel_commodities$gas, "gas", "electricity"),
               value_EJ = value / u_EJ_GWa)
}
demand <- map_dfr(scenarios, function(s) {
  map_dfr(names(components), function(stem) read_demand(s, stem))
})

# Require identical coverage so omitted demand cannot look like a reduction.
keys <- c("node", "commodity", "level", "year", "time", "unit", "component", "fuel")
base_keys <- demand %>% filter(scenario == baseline) %>% select(all_of(keys))
for (s in trials) {
  trial_keys <- demand %>% filter(scenario == s) %>% select(all_of(keys))
  if (nrow(anti_join(base_keys, trial_keys, by = keys)) ||
      nrow(anti_join(trial_keys, base_keys, by = keys))) {
    stop("Demand coverage differs between ", baseline, " and ", s,
         ". Resolve missing demand keys before comparing.")
  }
}
expected_pairs <- expand_grid(scenario = scenarios, component = unname(components),
                              fuel = names(fuel_commodities))
if (nrow(anti_join(expected_pairs, distinct(demand, scenario, component, fuel),
                   by = c("scenario", "component", "fuel")))) {
  stop("A scenario/component has no gas or electricity rows; check appliance exports.")
}

world <- demand %>% group_by(fuel, scenario, year) %>%
  summarise(value_EJ = sum(value_EJ), .groups = "drop")
regional <- demand %>% group_by(fuel, scenario, node, year) %>%
  summarise(value_EJ = sum(value_EJ), .groups = "drop") %>%
  mutate(region = sub("^R12_", "", node))
by_component <- demand %>% group_by(fuel, scenario, component, year) %>%
  summarise(value_EJ = sum(value_EJ), .groups = "drop") %>%
  mutate(component = factor(component, levels = unname(components)))
by_end_use <- demand %>% group_by(fuel, scenario, commodity, year) %>%
  summarise(value_EJ = sum(value_EJ), .groups = "drop")

# COMMON PLOT STYLE ------------------------------------------------------------
style <- function(p, fuel_name, base_size = 12) {
  p + scale_colour_manual(values = colours, breaks = scenarios, drop = FALSE) +
    scale_linetype_manual(values = line_types, breaks = scenarios, drop = FALSE) +
    scale_x_continuous(breaks = seq(2020, 2100, 20)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.09))) +
    labs(x = "Year", y = paste0(fuel_name, " demand (EJ/year)"),
         colour = NULL, linetype = NULL) +
    theme_bw(base_size = base_size) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0, size = 10))
}
make_plots <- function(fuel_key) {
  label <- unname(fuel_labels[fuel_key])
  w <- world %>% filter(fuel == fuel_key)
  r <- regional %>% filter(fuel == fuel_key)
  c <- by_component %>% filter(fuel == fuel_key)
  values_2060 <- w %>% filter(year == 2060) %>%
    mutate(scenario = factor(scenario, levels = scenarios)) %>% arrange(scenario)
  if (nrow(values_2060) != length(scenarios)) stop("Missing 2060 results for ", fuel_key)
  # Report the three values outside the plotting area to avoid overlapping labels.
  caption <- paste0("2060: ", paste(sprintf("%s = %.2f EJ", values_2060$scenario,
                                          values_2060$value_EJ), collapse = "  |  "))
  p_world <- ggplot(w, aes(year, value_EJ, colour = scenario, linetype = scenario))
  if (fuel_key == "gas") {
    p_world <- p_world +
      annotate("segment", x = 2060, xend = 2060, y = 4, yend = 7,
               linewidth = 8, colour = "#CBD9C4", alpha = 0.8)
    caption <- paste0(caption, "\nGreen marker: 2060 gas target of 4-7 EJ/year.")
  }
  p_world <- p_world + geom_line(linewidth = 1.1) +
    geom_point(data = filter(w, year == 2060), size = 2.8) +
    labs(title = paste0("Aligned building ", tolower(label), " demand - World"),
         subtitle = "Residential + commercial STURM + GLANCE", caption = caption)
  p_world <- style(p_world, label)
  p_regions <- style(
    ggplot(r, aes(year, value_EJ, colour = scenario, linetype = scenario)) +
      geom_line(linewidth = 0.85) + facet_wrap(~region, ncol = 3, scales = "free_y") +
      labs(title = paste0("Aligned ", tolower(label), " demand - R12 regions"),
           subtitle = "Independent y-axis scales; every panel starts at zero"),
    label, base_size = 10)
  p_components <- style(
    ggplot(c, aes(year, value_EJ, colour = scenario, linetype = scenario)) +
      geom_line(linewidth = 1) + facet_wrap(~component, ncol = 3, scales = "free_y") +
      labs(title = paste0("Aligned ", tolower(label), " demand by component"),
           subtitle = "Independent y-axis scales; identical trajectories overlap"), label)
  list(world = p_world, regions = p_regions, components = p_components)
}
plots <- setNames(lapply(names(fuel_commodities), make_plots), names(fuel_commodities))

# WRITE PLOTS AND TABLES --------------------------------------------------------
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_pages <- function(path, pages) {
  grDevices::pdf(path, width = 12, height = 8, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (p in pages) print(p)
}
for (fuel_key in names(plots)) {
  pages <- plots[[fuel_key]]
  for (panel in names(pages)) {
    ggsave(file.path(output_dir, paste0("aligned_", fuel_key, "_", panel,
                                      "_three_scenarios.png")), pages[[panel]],
           width = 12, height = 8, dpi = 300, bg = "white")
  }
  write_pages(file.path(output_dir, paste0("aligned_", fuel_key, "_three_scenarios.pdf")), pages)
  write_csv(filter(world, fuel == fuel_key),
            file.path(output_dir, paste0("aligned_", fuel_key, "_world_EJ.csv")))
  write_csv(filter(regional, fuel == fuel_key),
            file.path(output_dir, paste0("aligned_", fuel_key, "_regions_EJ.csv")))
  write_csv(filter(by_component, fuel == fuel_key),
            file.path(output_dir, paste0("aligned_", fuel_key, "_components_EJ.csv")))
}
write_pages(file.path(output_dir, "aligned_gas_electricity_three_scenarios.pdf"),
            c(plots$gas, plots$electricity))
write_csv(by_end_use, file.path(output_dir, "aligned_gas_electricity_end_use_EJ.csv"))

wide <- world %>% pivot_wider(names_from = scenario, values_from = value_EJ)
comparison <- world %>% filter(scenario != baseline) %>%
  left_join(world %>% filter(scenario == baseline) %>%
              select(fuel, year, baseline_EJ = value_EJ), by = c("fuel", "year")) %>%
  mutate(change_EJ = value_EJ - baseline_EJ,
         change_percent = if_else(baseline_EJ > 0, 100 * change_EJ / baseline_EJ, NA_real_))
write_csv(wide, file.path(output_dir, "aligned_gas_electricity_scenario_totals_EJ.csv"))
write_csv(comparison, file.path(output_dir, "aligned_gas_electricity_changes_vs_R_mitig.csv"))
print(filter(wide, year %in% c(2030, 2050, 2060, 2100)))
print(filter(comparison, year == 2060))

# CHECK EARLY DEMAND AND LATE GAS TRAJECTORIES ----------------------------------
base_rows <- demand %>% filter(scenario == baseline) %>%
  select(all_of(keys), baseline_EJ = value_EJ)
early_check <- demand %>% filter(scenario != baseline, year <= 2030) %>%
  left_join(base_rows, by = keys) %>% mutate(difference_EJ = value_EJ - baseline_EJ)
if (any(abs(early_check$difference_EJ) > 1e-8)) {
  warning("Some individual gas/electricity demand values differ through 2030.")
} else message("Gas and electricity demand values through 2030 match the baseline.")
for (s in trials) {
  late <- world %>% filter(fuel == "gas", scenario == s, year >= 2060) %>% arrange(year)
  if (any(diff(late$value_EJ) > 1e-8)) {
    warning("Total gas rises at a reported point after 2060 in ", s)
  } else message("No total gas rebound at reported points after 2060: ", s)
}
message("Plots and tables saved to: ", output_dir)
