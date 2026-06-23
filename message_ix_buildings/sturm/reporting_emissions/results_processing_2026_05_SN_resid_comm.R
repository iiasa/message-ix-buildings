# ============================================================
# Post-processing STURM results: GHG emissions
# Residential + commercial, or either sector
#
# Outputs:
#   visualization/emissions_outputs/resid_operational_emissions_detailed.csv
#   visualization/emissions_outputs/resid_embodied_emissions_detailed.csv
#   visualization/emissions_outputs/resid_operational_emissions_summary.csv
#   visualization/emissions_outputs/resid_embodied_emissions_summary.csv
#   visualization/emissions_outputs/resid_total_emissions_summary.csv
#
#   visualization/emissions_outputs/comm_operational_emissions_detailed.csv
#   visualization/emissions_outputs/comm_embodied_emissions_detailed.csv
#   visualization/emissions_outputs/comm_operational_emissions_summary.csv
#   visualization/emissions_outputs/comm_embodied_emissions_summary.csv
#   visualization/emissions_outputs/comm_total_emissions_summary.csv
#
# Notes:
#   - Residential usually uses region_bld output files.
#   - Commercial usually uses R12 output files.
#   - Operational emissions are calculated for both sectors.
#   - Embodied emissions are calculated only when material-demand
#     columns are populated.
#   - Commercial material-demand outputs may currently be NA; if so,
#     commercial embodied emissions will also be NA rather than zero.
# ============================================================

library(tidyverse)
library(readr)
library(zoo)

options(scipen = 999)

# ============================================================
# 1. User settings
# ============================================================

# Run either one sector or both:
# sectors_to_process <- c("resid")
# sectors_to_process <- c("comm")
sectors_to_process <- c("resid", "comm")

sturm_dir <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm"

path_in_runs <- file.path(sturm_dir, "output")
path_report  <- file.path(sturm_dir, "visualization")

# Correct location for emission-factor inputs
path_reporting_emissions <- file.path(sturm_dir, "reporting_emissions")

path_in_ems_op  <- file.path(path_reporting_emissions, "emission_intensity_operational")
path_in_ems_emb <- file.path(path_reporting_emissions, "emission_intensity_ecoinvent")

path_out_ems <- file.path(path_report, "emissions_outputs")
dir.create(path_out_ems, recursive = TRUE, showWarnings = FALSE)

cat("\nSTURM output folder:\n", path_in_runs, "\n")
cat("\nReporting emissions folder:\n", path_reporting_emissions, "\n")
cat("\nOperational emission factors folder:\n", path_in_ems_op, "\n")
cat("\nEmbodied emission factors folder:\n", path_in_ems_emb, "\n")
cat("\nEmissions output folder:\n", path_out_ems, "\n")

# ============================================================
# 2. Scenario setup
# ============================================================

scenarios <- data.frame(
  scenario = c(
    "R",
    "N_r", "N_tp",
    "S_r", "S_tp",
    "C_r", "C_tp",
    "A_r", "A_tp",
    "E_r", "E_tp",
    "CP_r", "CP_tp"
  ),
  scenario_name = c(
    "Reference",
    "Narrow R", "Narrow TP",
    "Slow R", "Slow TP",
    "Close R", "Close TP",
    "Combined R", "Combined TP",
    "Efficiency R", "Efficiency TP",
    "Climate policy R", "Climate policy TP"
  ),
  scenario_supply = c(
    "NPi",
    "NPi", "NPi",
    "NPi", "NPi",
    "NPi", "NPi",
    "NPi", "NPi",
    "NPi", "NPi",
    "1p5C", "1p5C"
  )
)

sectors <- data.frame(
  sector = c("resid", "comm"),
  sector_name = c("Residential", "Commercial")
) %>%
  filter(sector %in% sectors_to_process)

yrs <- c(seq(2020, 2060, 5), seq(2070, 2100, 10))

stp_df <- data.frame(
  year = yrs,
  stp = yrs - lag(yrs)
)

materials_secondary <- c("aluminum", "copper", "steel")

# ============================================================
# 3. Region mappings
# ============================================================

candidate_region_files <- c(
  file.path(path_report, "regions_R61.csv"),
  file.path(sturm_dir, "data", "input_csv_SSP_2023_resid", "regions_R61.csv"),
  file.path(sturm_dir, "data", "input_csv_SSP_2023_comm", "regions_R61.csv"),
  file.path(sturm_dir, "input_csv_SSP_2023_resid", "regions_R61.csv"),
  file.path(sturm_dir, "input_csv_SSP_2023_comm", "regions_R61.csv"),
  file.path(sturm_dir, "data", "input_csv", "regions_R61.csv")
)

region_file <- candidate_region_files[file.exists(candidate_region_files)][1]

if (is.na(region_file)) {
  stop(
    "Could not find regions_R61.csv. Checked:\n",
    paste(candidate_region_files, collapse = "\n")
  )
}

cat("\nUsing region mapping file:\n", region_file, "\n")

regions <- read_csv(region_file, show_col_types = FALSE)

cat("\nRegion mapping columns:\n")
print(names(regions))

if (!all(c("region_bld", "region_gea") %in% names(regions))) {
  stop(
    "regions_R61.csv must contain columns 'region_bld' and 'region_gea'. Available columns:\n",
    paste(names(regions), collapse = ", ")
  )
}

regions <- regions %>%
  mutate(
    region_bld = as.character(region_bld),
    region_gea = as.character(region_gea)
  ) %>%
  select(region_bld, region_gea) %>%
  distinct()

# ============================================================
# 4. Operational emission factors
# ============================================================
# Unit: kgCO2/GJ

candidate_op_files <- c(
  file.path(
    path_in_ems_op,
    "emission_factors_ENGAGE_baselineAligned_2020_2025.csv"
  ),
  file.path(
    path_in_ems_op,
    "emission_factors_ENGAGE.csv"
  ),
  file.path(
    path_in_ems_op,
    "emission_factors_ENGAGE_2025-05-06.csv"
  )
)

existing_candidate_op_files <- candidate_op_files[file.exists(candidate_op_files)]

if (length(existing_candidate_op_files) == 0) {
  
  searched_files <- list.files(
    path_in_ems_op,
    pattern = "emission.*factor|ENGAGE|emission_factors",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(searched_files) == 0) {
    stop(
      "No operational emission factor file found in:\n",
      path_in_ems_op,
      "\nChecked candidates:\n",
      paste(candidate_op_files, collapse = "\n")
    )
  }
  
  ems_int_op_file <- searched_files[1]
  
} else {
  
  ems_int_op_file <- existing_candidate_op_files[1]
}

cat("\nUsing operational emission factor file:\n")
cat(ems_int_op_file, "\n")

ems_int_op <- read_csv(
  ems_int_op_file,
  show_col_types = FALSE
)

cat("\nOperational EF columns:\n")
print(names(ems_int_op))

ems_int_op <- ems_int_op %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "ems_int_op"
  ) %>%
  mutate(year = as.integer(year))

if ("clim_policy" %in% names(ems_int_op)) {
  ems_int_op <- ems_int_op %>%
    rename(scenario_supply = clim_policy)
}

if (!"scenario_supply" %in% names(ems_int_op)) {
  stop(
    "Could not find scenario supply column in operational EF file. Expected 'clim_policy' or 'scenario_supply'. Available columns:\n",
    paste(names(ems_int_op), collapse = ", ")
  )
}

required_op_cols <- c("scenario_supply", "region_gea", "year", "fuel", "ems_int_op")
missing_op_cols <- setdiff(required_op_cols, names(ems_int_op))

if (length(missing_op_cols) > 0) {
  stop(
    "Operational EF file is missing required columns:\n",
    paste(missing_op_cols, collapse = "\n"),
    "\nAvailable columns:\n",
    paste(names(ems_int_op), collapse = ", ")
  )
}

ems_int_op <- ems_int_op %>%
  mutate(
    region_gea = as.character(region_gea),
    fuel = as.character(fuel),
    scenario_supply = as.character(scenario_supply)
  ) %>%
  arrange(scenario_supply, region_gea, year, fuel)

cat("\nOperational EF scenario_supply values:\n")
print(sort(unique(ems_int_op$scenario_supply)))

cat("\nOperational EF fuels:\n")
print(sort(unique(ems_int_op$fuel)))

cat("\nOperational EF years:\n")
print(sort(unique(ems_int_op$year)))

if (all(c("NPi", "1p5C") %in% unique(ems_int_op$scenario_supply))) {
  
  cat("\nOperational EF baseline-alignment check:\n")
  
  ems_int_op %>%
    filter(
      scenario_supply %in% c("NPi", "1p5C"),
      year %in% c(2020, 2025)
    ) %>%
    select(region_gea, fuel, year, scenario_supply, ems_int_op) %>%
    pivot_wider(
      names_from = scenario_supply,
      values_from = ems_int_op
    ) %>%
    mutate(diff_1p5C_vs_NPi = `1p5C` - NPi) %>%
    summarise(
      max_abs_diff = max(abs(diff_1p5C_vs_NPi), na.rm = TRUE),
      n_nonzero_diff = sum(abs(diff_1p5C_vs_NPi) > 1e-12, na.rm = TRUE)
    ) %>%
    print()
}

# ============================================================
# 5. Embodied emission factors
# ============================================================
# Unit: kgCO2/kg

candidate_emb_files <- c(
  file.path(
    path_in_ems_emb,
    "ghg_image_r12_2025-05-06_edit_baselineAligned_2020_2025.csv"
  ),
  file.path(
    path_in_ems_emb,
    "ghg_image_r12_2025-05-06_edit.csv"
  ),
  file.path(
    path_in_ems_emb,
    "ghg_image_r12_2025-05-06.csv"
  )
)

existing_candidate_emb_files <- candidate_emb_files[file.exists(candidate_emb_files)]

if (length(existing_candidate_emb_files) == 0) {
  
  searched_emb_files <- list.files(
    path_in_ems_emb,
    pattern = "ghg.*image|ecoinvent|emission",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(searched_emb_files) == 0) {
    stop(
      "No embodied emission factor file found in:\n",
      path_in_ems_emb,
      "\nChecked candidates:\n",
      paste(candidate_emb_files, collapse = "\n")
    )
  }
  
  ems_int_emb_file <- searched_emb_files[1]
  
} else {
  
  ems_int_emb_file <- existing_candidate_emb_files[1]
}

cat("\nUsing embodied emission factor file:\n")
cat(ems_int_emb_file, "\n")

ems_int_emb_raw <- read_csv(
  ems_int_emb_file,
  show_col_types = FALSE
)

cat("\nEmbodied EF columns:\n")
print(names(ems_int_emb_raw))

required_emb_raw_cols <- c(
  "scenario_supply",
  "region_gea",
  "material",
  "production",
  "year",
  "emission_factor"
)

missing_emb_raw_cols <- setdiff(required_emb_raw_cols, names(ems_int_emb_raw))

if (length(missing_emb_raw_cols) > 0) {
  stop(
    "Embodied EF file is missing required columns:\n",
    paste(missing_emb_raw_cols, collapse = "\n"),
    "\nAvailable columns:\n",
    paste(names(ems_int_emb_raw), collapse = ", ")
  )
}

ems_int_emb_raw <- ems_int_emb_raw %>%
  mutate(
    scenario_supply = as.character(scenario_supply),
    region_gea = as.character(region_gea),
    material = as.character(material),
    production = as.character(production),
    year = as.integer(year)
  )

cat("\nEmbodied EF scenario_supply values:\n")
print(sort(unique(ems_int_emb_raw$scenario_supply)))

cat("\nEmbodied EF materials:\n")
print(sort(unique(ems_int_emb_raw$material)))

cat("\nEmbodied EF years:\n")
print(sort(unique(ems_int_emb_raw$year)))

if (all(c("NPi", "1p5C") %in% unique(ems_int_emb_raw$scenario_supply))) {
  
  cat("\nEmbodied EF baseline-alignment check:\n")
  
  ems_int_emb_raw %>%
    filter(
      scenario_supply %in% c("NPi", "1p5C"),
      year %in% c(2020, 2025)
    ) %>%
    select(region_gea, material, production, year, scenario_supply, emission_factor) %>%
    pivot_wider(
      names_from = scenario_supply,
      values_from = emission_factor
    ) %>%
    mutate(diff_1p5C_vs_NPi = `1p5C` - NPi) %>%
    summarise(
      max_abs_diff = max(abs(diff_1p5C_vs_NPi), na.rm = TRUE),
      n_nonzero_diff = sum(abs(diff_1p5C_vs_NPi) > 1e-12, na.rm = TRUE)
    ) %>%
    print()
}

# Secondary production assumptions
candidate_secondary_ref_files <- c(
  file.path(path_report, "secondary production", "share_secondary_production_pauliuk_2021_test_2100.csv"),
  file.path(path_report, "secondary_production", "share_secondary_production_pauliuk_2021_test_2100.csv"),
  file.path(path_reporting_emissions, "secondary production", "share_secondary_production_pauliuk_2021_test_2100.csv"),
  file.path(path_reporting_emissions, "secondary_production", "share_secondary_production_pauliuk_2021_test_2100.csv")
)

candidate_secondary_high_files <- c(
  file.path(path_report, "secondary production", "share_secondary_production_pauliuk_2021_test_2100_HIGH.csv"),
  file.path(path_report, "secondary_production", "share_secondary_production_pauliuk_2021_test_2100_HIGH.csv"),
  file.path(path_reporting_emissions, "secondary production", "share_secondary_production_pauliuk_2021_test_2100_HIGH.csv"),
  file.path(path_reporting_emissions, "secondary_production", "share_secondary_production_pauliuk_2021_test_2100_HIGH.csv")
)

shr_secondary_ref_file <- candidate_secondary_ref_files[file.exists(candidate_secondary_ref_files)][1]
shr_secondary_high_file <- candidate_secondary_high_files[file.exists(candidate_secondary_high_files)][1]

if (is.na(shr_secondary_ref_file)) {
  stop(
    "Secondary production Reference file not found. Checked:\n",
    paste(candidate_secondary_ref_files, collapse = "\n")
  )
}

if (is.na(shr_secondary_high_file)) {
  stop(
    "Secondary production HIGH file not found. Checked:\n",
    paste(candidate_secondary_high_files, collapse = "\n")
  )
}

cat("\nUsing secondary production Reference file:\n")
cat(shr_secondary_ref_file, "\n")

cat("\nUsing secondary production HIGH file:\n")
cat(shr_secondary_high_file, "\n")

shr_secondary_ref <- read_csv(
  shr_secondary_ref_file,
  show_col_types = FALSE
) %>%
  pivot_longer(
    cols = any_of(materials_secondary),
    names_to = "material",
    values_to = "shr_sec"
  ) %>%
  mutate(scenario_supply = "NPi")

shr_secondary_high <- read_csv(
  shr_secondary_high_file,
  show_col_types = FALSE
) %>%
  pivot_longer(
    cols = any_of(materials_secondary),
    names_to = "material",
    values_to = "shr_sec"
  ) %>%
  mutate(scenario_supply = "1p5C")

shr_secondary <- bind_rows(
  shr_secondary_ref,
  shr_secondary_high
)

ems_int_emb <- ems_int_emb_raw %>%
  mutate(
    emission_factor = ifelse(
      year %in% c(2040, 2080),
      NA_real_,
      emission_factor
    )
  ) %>%
  bind_rows(
    ems_int_emb_raw %>%
      filter(year == 2030) %>%
      mutate(year = 2035, emission_factor = NA_real_)
  ) %>%
  bind_rows(
    ems_int_emb_raw %>%
      filter(year == 2030) %>%
      mutate(year = 2045, emission_factor = NA_real_)
  ) %>%
  bind_rows(
    ems_int_emb_raw %>%
      filter(year == 2030) %>%
      mutate(year = 2055, emission_factor = NA_real_)
  ) %>%
  select(-any_of("unit")) %>%
  arrange(scenario_supply, region_gea, material, production, year) %>%
  group_by(scenario_supply, region_gea, material, production) %>%
  mutate(
    emission_factor = na.approx(emission_factor, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  rename(ems_int_emb = emission_factor)

ems_int_emb <- crossing(
  scenarios,
  stp_df %>% select(year)
) %>%
  left_join(
    ems_int_emb,
    by = c("scenario_supply", "year")
  ) %>%
  pivot_wider(
    names_from = "production",
    values_from = "ems_int_emb"
  ) %>%
  left_join(
    shr_secondary,
    by = c("scenario_supply", "year", "material")
  ) %>%
  rename(shr_secondary = shr_sec) %>%
  mutate(
    shr_secondary = replace_na(shr_secondary, 0),
    ems_int_emb = ifelse(
      shr_secondary > 0,
      primary * (1 - shr_secondary) + secondary * shr_secondary,
      primary
    )
  ) %>%
  select(
    scenario, scenario_name, scenario_supply,
    region_gea, year, material, ems_int_emb
  ) %>%
  arrange(scenario, scenario_name, scenario_supply, region_gea, material, year)

# ============================================================
# 6. Helper functions
# ============================================================

find_sturm_file <- function(scenario_code, sector_code, output_type) {
  
  pattern <- paste0(
    "^report_STURM_",
    scenario_code,
    "_",
    sector_code,
    "_.*_",
    output_type,
    "\\.csv$"
  )
  
  files <- list.files(
    path_in_runs,
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    warning(
      "No ",
      output_type,
      " file found for scenario = ",
      scenario_code,
      ", sector = ",
      sector_code
    )
    return(NA_character_)
  }
  
  region_bld_file <- files[str_detect(files, "_region_bld_")]
  
  if (length(region_bld_file) > 0) {
    return(region_bld_file[1])
  }
  
  files[1]
}

standardise_region_columns <- function(df, sector_code) {
  
  # Case 1: already has both columns
  if (all(c("region_bld", "region_gea") %in% names(df))) {
    
    df <- df %>%
      mutate(
        region_bld = as.character(region_bld),
        region_gea = as.character(region_gea)
      )
    
    # Case 2: residential-style file with region_bld only
  } else if ("region_bld" %in% names(df)) {
    
    df <- df %>%
      mutate(region_bld = as.character(region_bld)) %>%
      left_join(
        regions %>%
          mutate(
            region_bld = as.character(region_bld),
            region_gea = as.character(region_gea)
          ) %>%
          select(region_bld, region_gea) %>%
          distinct(),
        by = "region_bld"
      )
    
    # Case 3: commercial-style file with R12
  } else if ("R12" %in% names(df)) {
    
    df <- df %>%
      rename(region_gea = R12) %>%
      mutate(
        region_gea = as.character(region_gea),
        region_bld = region_gea
      )
    
    # Case 4: already has region_gea only
  } else if ("region_gea" %in% names(df)) {
    
    df <- df %>%
      mutate(
        region_gea = as.character(region_gea),
        region_bld = region_gea
      )
    
  } else {
    
    stop(
      "Could not identify region column. Expected region_bld, R12, or region_gea. Available columns:\n",
      paste(names(df), collapse = ", ")
    )
  }
  
  # Safety clean-up in case joins created suffixes
  if (!"region_gea" %in% names(df)) {
    
    possible_gea_cols <- names(df)[names(df) %in% c("region_gea.x", "region_gea.y")]
    
    if (length(possible_gea_cols) > 0) {
      df <- df %>%
        mutate(region_gea = coalesce(!!!syms(possible_gea_cols)))
    }
  }
  
  if (!"region_gea" %in% names(df)) {
    stop(
      "region_gea could not be created. Available columns after region standardisation:\n",
      paste(names(df), collapse = ", ")
    )
  }
  
  df
}

read_energy_file <- function(scenario_code, sector_code, sector_name) {
  
  file_path <- find_sturm_file(
    scenario_code = scenario_code,
    sector_code = sector_code,
    output_type = "energy"
  )
  
  if (is.na(file_path) || !file.exists(file_path)) {
    return(NULL)
  }
  
  cat("Loaded energy:", basename(file_path), "\n")
  
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    standardise_region_columns(sector_code) %>%
    mutate(
      sector = sector_name,
      scenario = scenario_code
    ) %>%
    left_join(
      scenarios,
      by = "scenario"
    )
  
  required_cols <- c(
    "region_bld", "region_gea",
    "sector", "scenario", "scenario_name", "scenario_supply",
    "fuel_heat", "year", "heat_TJ", "cool_TJ"
  )
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required energy columns in ",
      basename(file_path),
      ":\n",
      paste(missing_cols, collapse = "\n")
    )
  }
  
  if (!"hotwater_TJ" %in% names(df)) {
    df <- df %>%
      mutate(hotwater_TJ = 0)
  }
  
  df
}

read_material_file <- function(scenario_code, sector_code, sector_name) {
  
  file_path <- find_sturm_file(
    scenario_code = scenario_code,
    sector_code = sector_code,
    output_type = "material"
  )
  
  if (is.na(file_path) || !file.exists(file_path)) {
    return(NULL)
  }
  
  cat("Loaded material:", basename(file_path), "\n")
  
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    standardise_region_columns(sector_code) %>%
    mutate(
      sector = sector_name,
      scenario = scenario_code
    ) %>%
    left_join(
      scenarios,
      by = "scenario"
    )
  
  required_cols <- c(
    "region_bld", "region_gea",
    "sector", "scenario", "scenario_name", "scenario_supply",
    "material", "year", "mat_demand_Mt"
  )
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required material columns in ",
      basename(file_path),
      ":\n",
      paste(missing_cols, collapse = "\n"),
      "\nAvailable columns:\n",
      paste(names(df), collapse = ", ")
    )
  }
  
  df
}

# ============================================================
# 7. Load energy and material outputs
# ============================================================

run_grid <- crossing(
  scenarios %>% select(scenario),
  sectors %>% select(sector, sector_name)
)

energy_all <- pmap_dfr(
  run_grid,
  function(scenario, sector, sector_name) {
    read_energy_file(
      scenario_code = scenario,
      sector_code = sector,
      sector_name = sector_name
    )
  }
)

material_all <- pmap_dfr(
  run_grid,
  function(scenario, sector, sector_name) {
    read_material_file(
      scenario_code = scenario,
      sector_code = sector,
      sector_name = sector_name
    )
  }
)

if (nrow(energy_all) == 0) {
  stop("No energy outputs were loaded.")
}

if (nrow(material_all) == 0) {
  warning("No material outputs were loaded. Embodied emissions will not be calculated.")
}

cat("\nLoaded energy rows by sector and scenario:\n")
energy_all %>%
  count(sector, scenario_name) %>%
  print(n = Inf)

cat("\nLoaded material rows by sector and scenario:\n")
material_all %>%
  count(sector, scenario_name) %>%
  print(n = Inf)

# ============================================================
# 8. Process operational energy by fuel
# ============================================================

energy_tot <- energy_all %>%
  mutate(
    heat_TJ = replace_na(heat_TJ, 0),
    cool_TJ = replace_na(cool_TJ, 0),
    hotwater_TJ = replace_na(hotwater_TJ, 0)
  ) %>%
  group_by(
    region_bld, region_gea,
    sector, scenario, scenario_name, scenario_supply,
    fuel_heat, year
  ) %>%
  summarise(
    heat_EJ = sum(heat_TJ, na.rm = TRUE) / 1e6,
    cool_EJ = sum(cool_TJ, na.rm = TRUE) / 1e6,
    hotwater_EJ = sum(hotwater_TJ, na.rm = TRUE) / 1e6,
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c("heat_EJ", "cool_EJ", "hotwater_EJ"),
    names_to = "enduse",
    values_to = "en_EJ"
  ) %>%
  mutate(
    fuel = ifelse(enduse == "cool_EJ", "electricity", fuel_heat)
  ) %>%
  group_by(
    region_bld, region_gea,
    sector, scenario, scenario_name, scenario_supply,
    fuel, year
  ) %>%
  summarise(
    en_EJ = sum(en_EJ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(fuel != "v_no_heat") %>%
  arrange(region_bld, sector, scenario, fuel, year)

# ============================================================
# 9. Calculate operational emissions
# ============================================================

ems_op <- energy_tot %>%
  left_join(
    ems_int_op,
    by = c("scenario_supply", "region_gea", "year", "fuel")
  ) %>%
  mutate(
    type = ifelse(fuel %in% c("electricity", "district_heat"), "indirect", "direct"),
    # en_EJ * kgCO2/GJ = MtCO2e
    ems_op = en_EJ * ems_int_op
  ) %>%
  select(
    region_bld, region_gea, sector,
    scenario, scenario_name, scenario_supply,
    fuel, type, year, ems_op
  )


cat("\nOperational emissions missing EF check:\n")

ems_op %>%
  filter(is.na(ems_op)) %>%
  count(sector, scenario_name, fuel, year) %>%
  print(n = Inf)

# ============================================================
# 10. Process material demand
# ============================================================

if (nrow(material_all) > 0) {
  
  mat_tot <- material_all %>%
    filter(material != "cement") %>%
    group_by(
      region_bld, region_gea,
      sector, scenario, scenario_name, scenario_supply,
      material, year
    ) %>%
    summarise(
      mat_demand_Mt = sum(mat_demand_Mt, na.rm = FALSE),
      .groups = "drop"
    ) %>%
    arrange(region_bld, sector, scenario, material, year)
  
} else {
  
  mat_tot <- tibble(
    region_bld = character(),
    region_gea = character(),
    sector = character(),
    scenario = character(),
    scenario_name = character(),
    scenario_supply = character(),
    material = character(),
    year = integer(),
    mat_demand_Mt = numeric()
  )
}

cat("\nMaterial-demand availability check:\n")

mat_tot %>%
  group_by(sector) %>%
  summarise(
    n_rows = n(),
    n_non_na_mat_demand = sum(!is.na(mat_demand_Mt)),
    sum_mat_demand_Mt = sum(mat_demand_Mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(width = Inf)

missing_material_sectors <- mat_tot %>%
  group_by(sector) %>%
  summarise(
    n_non_na_mat_demand = sum(!is.na(mat_demand_Mt)),
    .groups = "drop"
  ) %>%
  filter(n_non_na_mat_demand == 0)

if (nrow(missing_material_sectors) > 0) {
  warning(
    "Material demand is missing for: ",
    paste(missing_material_sectors$sector, collapse = ", "),
    ". Embodied emissions for these sectors will be NA."
  )
}

# ============================================================
# 11. Calculate embodied emissions
# ============================================================

ems_emb <- mat_tot %>%
  left_join(
    ems_int_emb,
    by = c(
      "scenario", "scenario_name", "scenario_supply",
      "material", "year", "region_gea"
    )
  ) %>%
  mutate(
    type = "embodied",
    # Mt material * kgCO2/kg = MtCO2e
    ems_emb = mat_demand_Mt * ems_int_emb
  ) %>%
  select(
    region_bld, region_gea, sector,
    scenario, scenario_name, scenario_supply,
    material, type, year, ems_emb
  )

cat("\nEmbodied emissions missing EF / material check:\n")

ems_emb %>%
  filter(is.na(ems_emb)) %>%
  count(sector, scenario_name, material, year) %>%
  print(n = 50)

# ============================================================
# 12. Export outputs by sector
# ============================================================

for (sec in unique(sectors$sector_name)) {
  
  sec_code <- sectors %>%
    filter(sector_name == sec) %>%
    pull(sector)
  
  cat("\nExporting emissions outputs for:", sec, "(", sec_code, ")\n")
  
  ems_op_sec <- ems_op %>%
    filter(sector == sec)
  
  ems_emb_sec <- ems_emb %>%
    filter(sector == sec)
  
  write_csv(
    ems_op_sec,
    file.path(path_out_ems, paste0(sec_code, "_operational_emissions_detailed.csv"))
  )
  
  write_csv(
    ems_emb_sec,
    file.path(path_out_ems, paste0(sec_code, "_embodied_emissions_detailed.csv"))
  )
  
  operational_summary <- ems_op_sec %>%
    group_by(sector, scenario, scenario_name, year) %>%
    summarise(
      operational_MtCO2e = sum(ems_op, na.rm = TRUE),
      .groups = "drop"
    )
  
  embodied_summary <- ems_emb_sec %>%
    group_by(sector, scenario, scenario_name, year) %>%
    summarise(
      n_non_na_embodied = sum(!is.na(ems_emb)),
      embodied_MtCO2e = ifelse(
        n_non_na_embodied > 0,
        sum(ems_emb, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    select(-n_non_na_embodied)
  
  ghg_summary <- operational_summary %>%
    full_join(
      embodied_summary,
      by = c("sector", "scenario", "scenario_name", "year")
    ) %>%
    mutate(
      operational_MtCO2e = replace_na(operational_MtCO2e, 0),
      total_MtCO2e = ifelse(
        is.na(embodied_MtCO2e),
        NA_real_,
        embodied_MtCO2e + operational_MtCO2e
      )
    ) %>%
    select(
      sector, scenario, scenario_name, year,
      embodied_MtCO2e,
      operational_MtCO2e,
      total_MtCO2e
    ) %>%
    arrange(scenario, year)
  
  write_csv(
    embodied_summary,
    file.path(path_out_ems, paste0(sec_code, "_embodied_emissions_summary.csv"))
  )
  
  write_csv(
    operational_summary,
    file.path(path_out_ems, paste0(sec_code, "_operational_emissions_summary.csv"))
  )
  
  write_csv(
    ghg_summary,
    file.path(path_out_ems, paste0(sec_code, "_total_emissions_summary.csv"))
  )
  
  cat("\nSummary check for ", sec, ":\n", sep = "")
  
  ghg_summary %>%
    filter(year %in% c(2025, 2050, 2100)) %>%
    arrange(scenario_name, year) %>%
    print(n = Inf, width = Inf)
}

cat("\nSaved all emissions outputs to:\n")
cat(path_out_ems, "\n")
