# ============================================================
# Packages
# ============================================================

library(tidyverse)
library(readxl)
library(patchwork)
library(zoo)          # interpolation of missing values
library(RColorBrewer)
library(ggnewscale)

options(scipen = 999)

# ============================================================
# Working directory
# ============================================================
# Set the working directory to the folder containing this script
# when the script is opened in RStudio.

if (
  requireNamespace("rstudioapi", quietly = TRUE) &&
  rstudioapi::isAvailable()
) {
  
  script_path <- rstudioapi::getSourceEditorContext()$path
  
  if (nzchar(script_path)) {
    setwd(dirname(script_path))
  }
}

cat("\nCurrent working directory:\n")
cat(getwd(), "\n")

# ============================================================
# Repository paths
# ============================================================

sturm_dir <- paste0(
  "C:/Users/nawawi/Documents/GitHub/",
  "message-ix-buildings/message_ix_buildings/sturm"
)

# Latest STURM model outputs
path_in_runs <- file.path(
  sturm_dir,
  "output"
)

# STURM data directory
path_input <- file.path(
  sturm_dir,
  "data"
)

# CircEUlar residential and commercial input-list files
file_input_resid <- file.path(
  path_input,
  "input_list_resid_CircEUlar_2026.csv"
)

file_input_comm <- file.path(
  path_input,
  "input_list_comm_CircEUlar_2026.csv"
)

# Reporting output folder
path_report <- file.path(
  sturm_dir,
  "reporting_additional",
  "report_2026_06"
)

dir.create(
  path_report,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# Path checks
# ============================================================

required_directories <- c(
  sturm_dir,
  path_in_runs,
  path_input,
  path_report
)

missing_directories <- required_directories[
  !dir.exists(required_directories)
]

if (length(missing_directories) > 0) {
  stop(
    "The following required directories do not exist:\n",
    paste(missing_directories, collapse = "\n")
  )
}

required_files <- c(
  file_input_resid,
  file_input_comm
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "The following required CircEUlar input-list files do not exist:\n",
    paste(missing_files, collapse = "\n")
  )
}

# ============================================================
# Read CircEUlar input lists
# ============================================================

input_list_resid <- read_csv(
  file_input_resid,
  show_col_types = FALSE
)

input_list_comm <- read_csv(
  file_input_comm,
  show_col_types = FALSE
)

cat("\nResidential input-list columns:\n")
print(names(input_list_resid))

cat("\nCommercial input-list columns:\n")
print(names(input_list_comm))

# ============================================================
# Configured paths
# ============================================================

cat("\nConfigured paths:\n")
cat("STURM directory:          ", sturm_dir, "\n")
cat("Model outputs:            ", path_in_runs, "\n")
cat("Input-data root:          ", path_input, "\n")
cat("Residential input list:   ", file_input_resid, "\n")
cat("Commercial input list:    ", file_input_comm, "\n")
cat("Reporting output:         ", path_report, "\n")

### SETUP

# Scenarios setup
scenarios <- data.frame(scenario = c("R", "N_r", "S_r", "C_r", "A_r"),
                        scenario_name = c("Reference", "Narrow", "Slow", "Close","Circular"))

# scenarios <- data.frame(scenario = c("SSP2", "NEW_LT", "WOOD", "FLRD", "EFC_LOWDEM", "CP_SSP2_ENEFF","CP_EFC_LOWDEM_ENEFF"),
#                         scenario_name = c("NPi-Reference",  "NPi-Lifetime", "NPi-Wood", "NPi-Floor", "NPi-Circular", "1.5C-Reference", "1.5C-Circular" ),
#                         scenario_supply = c(rep("NPi",5),rep("1p5C",2)))


# Sectors
sectors <- data.frame(sector = c("resid", "comm"),
                      sector_name = c("Residential", "Commercial"))

fuels <- data.frame(fuel = c("biomass_solid", "coal", "district_heat", "electricity", "gas", "oil"),
                    fuel_name = c("Biomass", "Coal", "District Heat", "Electricity", "Gas", "Oil"))

# Load regions
# Load regional mapping
region_file <- file.path(
  sturm_dir,
  "reporting_additional",
  "regions_R61.csv"
)

if (!file.exists(region_file)) {
  stop(
    "Regional mapping file not found:\n",
    region_file
  )
}

regions <- read_csv(
  region_file,
  show_col_types = FALSE
)

cat("\nLoaded regional mapping from:\n")
cat(region_file, "\n")

cat("\nRegion mapping columns:\n")
print(names(regions))

# # 6 regions
# R6 <- c("EU27","USA","other GN","China","India","other GS") #"Subs.Africa",

# Regional definitions
reg_EU <- c("C-EEU-BGR","C-EEU-CZE","C-EEU-EST","C-EEU-HUN","C-EEU-HVR","C-EEU-LTU","C-EEU-LVA",
            "C-EEU-POL","C-EEU-ROU","C-EEU-SVK","C-EEU-SVN",
            "C-WEU-AUT","C-WEU-BEL",#"C-WEU-CHE",
            "C-WEU-CYP","C-WEU-DEU","C-WEU-DNK","C-WEU-ESP","C-WEU-FIN","C-WEU-FRA",#"C-WEU-GBR",
            "C-WEU-GRC","C-WEU-IRL",#"C-WEU-ISL",
            "C-WEU-ITA","C-WEU-LUX","C-WEU-MLT","C-WEU-NLD",#"C-WEU-NOR",
            "C-WEU-PRT","C-WEU-SWE")

reg_EU_plus <- c("C-EEU-BGR","C-EEU-CZE","C-EEU-EST","C-EEU-HUN","C-EEU-HVR","C-EEU-LTU","C-EEU-LVA",
            "C-EEU-POL","C-EEU-ROU","C-EEU-SVK","C-EEU-SVN",
            "C-WEU-AUT","C-WEU-BEL","C-WEU-CHE",
            "C-WEU-CYP","C-WEU-DEU","C-WEU-DNK","C-WEU-ESP","C-WEU-FIN","C-WEU-FRA","C-WEU-GBR",
            "C-WEU-GRC","C-WEU-IRL","C-WEU-ISL",
            "C-WEU-ITA","C-WEU-LUX","C-WEU-MLT","C-WEU-NLD","C-WEU-NOR",
            "C-WEU-PRT","C-WEU-SWE")

# Years
yrs <- c(seq(2020,2060,5),seq(2070,2100,10))
#yrs <- c(seq(2020,2060,5))

# Time steps

stp_df = data.frame(year = yrs,
                 stp = yrs - lag(yrs))

# Materials

mats_df <- data.frame(material = c("brick",
                                   #"brick_cdr",
                                   "concrete", 
                                   #"concrete_cdr",
                                   #"mortar", 
                                   #"plaster",
                                   "wood",
                                   "glass",
                                   "steel",
                                   "aluminum", 
                                   "copper"
),
material_name = c("Brick", # Brick
                  #"Brick - bio-fibers", # Brick - CDR
                  "Concrete", # Brick
                  #"Concrete - biochar/aggr.", # Brick - CDR
                  #"Mortar", # Mortar
                  #"Plaster", # Plaster
                  "Wood", # Wood
                  "Glass",
                  "Steel",
                  "Aluminum",
                  "Copper"
))


# # POPULATION DATA
# 
# pop <- read_csv("./../model_runs/STURM_data/input_resid/pop_clim_rev_SSP2.csv")
# pop_gea <- pop %>% 
#   left_join(regions) %>%
#   group_by(region_gea,year) %>%
#   summarise(pop =sum(value)) %>%
#   ungroup

# ============================================================
# ADDITIONAL REPORTING FOR CIRCULARITY SCENARIOS
# Country-level EU-27 floor-space and material results
#
# Required objects defined earlier in the script:
#   path_in_runs
#   path_report
#
# Outputs:
#   floor_space_EU27_scenarios_<date>.csv
#   building_materials_EU27_scenarios_<date>.csv
#   additional_reporting_EU27_long_<date>.csv
#   reporting_file_inventory_<date>.csv
#
# Variables:
#   - Total floor space
#   - Newly added floor space
#   - Demolished floor space
#   - Material stock
#   - Material inflows
#   - Material outflows
#
# Important:
#   Country-level reporting requires region_bld output files.
#   R12 commercial files are not silently treated as country results.
# ============================================================

library(tidyverse)
library(readr)

options(scipen = 999)

# ============================================================
# 1. Settings
# ============================================================

# Include Reference plus all CircEUlar scenarios
scenarios <- tribble(
  ~scenario, ~scenario_name,
  "R",       "Reference",
  "N_r",     "Narrow R",
  "N_tp",    "Narrow TP",
  "S_r",     "Slow R",
  "S_tp",    "Slow TP",
  "C_r",     "Close R",
  "C_tp",    "Close TP",
  "A_r",     "Combined R",
  "A_tp",    "Combined TP"
)

sectors <- tribble(
  ~sector, ~sector_name,
  "resid", "Residential",
  "comm",  "Commercial"
)

# EU-27 ISO3 country codes
eu27_codes <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK",
  "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL",
  "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL",
  "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
)

# Materials excluded from the final sharing file
excluded_materials <- c(
  "concrete_cdr",
  "brick_cdr",
  "mortar",
  "plaster",
  "cement"
)

# Use country-level files only.
# Setting this to TRUE would load R12 files, but those cannot be
# interpreted as country-level commercial results.
allow_R12_fallback <- FALSE

dir.create(
  path_report,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 2. Helper functions
# ============================================================

find_material_file <- function(
    scenario_code,
    sector_code,
    allow_R12 = FALSE
) {
  
  region_bld_file <- file.path(
    path_in_runs,
    paste0(
      "report_STURM_",
      scenario_code,
      "_",
      sector_code,
      "_region_bld_material.csv"
    )
  )
  
  r12_file <- file.path(
    path_in_runs,
    paste0(
      "report_STURM_",
      scenario_code,
      "_",
      sector_code,
      "_R12_material.csv"
    )
  )
  
  if (file.exists(region_bld_file)) {
    return(
      tibble(
        file = region_bld_file,
        aggregation_level = "country"
      )
    )
  }
  
  if (allow_R12 && file.exists(r12_file)) {
    return(
      tibble(
        file = r12_file,
        aggregation_level = "R12"
      )
    )
  }
  
  tibble(
    file = NA_character_,
    aggregation_level = NA_character_
  )
}


extract_country_code <- function(region_bld) {
  
  region_bld <- as.character(region_bld)
  
  # Standard STURM labels often look like R32AUT.
  # Extract the final three letters.
  country <- str_extract(region_bld, "[A-Z]{3}$")
  
  country
}


read_material_reporting_file <- function(
    scenario_code,
    scenario_name,
    sector_code,
    sector_name,
    allow_R12 = FALSE
) {
  
  file_info <- find_material_file(
    scenario_code = scenario_code,
    sector_code = sector_code,
    allow_R12 = allow_R12
  )
  
  if (is.na(file_info$file)) {
    
    warning(
      "No country-level material reporting file found for ",
      scenario_code,
      " / ",
      sector_code,
      ". Expected:\n",
      file.path(
        path_in_runs,
        paste0(
          "report_STURM_",
          scenario_code,
          "_",
          sector_code,
          "_region_bld_material.csv"
        )
      )
    )
    
    return(NULL)
  }
  
  cat(
    "\nLoading material file:\n",
    file_info$file,
    "\n"
  )
  
  dat <- read_csv(
    file_info$file,
    show_col_types = FALSE
  )
  
  # Standardise the regional field
  if ("region_bld" %in% names(dat)) {
    
    dat <- dat %>%
      mutate(
        region_bld = as.character(region_bld),
        country = extract_country_code(region_bld),
        aggregation_level = "country"
      )
    
  } else if ("R12" %in% names(dat)) {
    
    dat <- dat %>%
      rename(region_bld = R12) %>%
      mutate(
        region_bld = as.character(region_bld),
        country = NA_character_,
        aggregation_level = "R12"
      )
    
  } else if ("region_gea" %in% names(dat)) {
    
    dat <- dat %>%
      mutate(
        region_bld = as.character(region_gea),
        country = NA_character_,
        aggregation_level = "R12"
      )
    
  } else {
    
    warning(
      "No recognised regional column found in:\n",
      file_info$file,
      "\nAvailable columns:\n",
      paste(names(dat), collapse = ", ")
    )
    
    return(NULL)
  }
  
  dat <- dat %>%
    mutate(
      scenario = scenario_code,
      scenario_name = scenario_name,
      sector_code = sector_code,
      sector = sector_name
    )
  
  # Confirm that the required fields exist
  required_cols <- c(
    "region_bld",
    "scenario",
    "scenario_name",
    "sector",
    "material",
    "year",
    "floor_tot_Mm2",
    "floor_new_Mm2",
    "floor_dem_Mm2",
    "mat_stock_Mt",
    "mat_demand_Mt",
    "mat_scrap_Mt"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(dat)
  )
  
  if (length(missing_cols) > 0) {
    
    stop(
      "The following columns are missing from:\n",
      file_info$file,
      "\n\nMissing columns:\n",
      paste(missing_cols, collapse = "\n"),
      "\n\nAvailable columns:\n",
      paste(names(dat), collapse = ", ")
    )
  }
  
  dat
}

# ============================================================
# 3. Build file inventory
# ============================================================

reporting_grid <- crossing(
  scenarios,
  sectors
)

file_inventory <- reporting_grid %>%
  mutate(
    file_info = pmap(
      list(
        scenario,
        sector
      ),
      ~ find_material_file(
        scenario_code = ..1,
        sector_code = ..2,
        allow_R12 = allow_R12_fallback
      )
    )
  ) %>%
  unnest(file_info) %>%
  mutate(
    exists = !is.na(file)
  )

cat("\nReporting-file inventory:\n")

file_inventory %>%
  select(
    scenario,
    scenario_name,
    sector,
    sector_name,
    aggregation_level,
    exists,
    file
  ) %>%
  print(
    n = Inf,
    width = Inf
  )

write_csv(
  file_inventory,
  file.path(
    path_report,
    paste0(
      "reporting_file_inventory_",
      Sys.Date(),
      ".csv"
    )
  )
)

# ============================================================
# 4. Load all available material reporting files
# ============================================================

mat_det <- pmap_dfr(
  reporting_grid,
  function(
    scenario,
    scenario_name,
    sector,
    sector_name
  ) {
    
    read_material_reporting_file(
      scenario_code = scenario,
      scenario_name = scenario_name,
      sector_code = sector,
      sector_name = sector_name,
      allow_R12 = allow_R12_fallback
    )
  }
)

if (nrow(mat_det) == 0) {
  stop(
    "No material reporting files were loaded."
  )
}

cat("\nLoaded rows by scenario and sector:\n")

mat_det %>%
  count(
    scenario,
    scenario_name,
    sector
  ) %>%
  print(
    n = Inf
  )

# ============================================================
# 5. Keep EU-27 country-level data
# ============================================================

mat_det_eu27 <- mat_det %>%
  filter(
    aggregation_level == "country",
    country %in% eu27_codes
  )

cat("\nEU-27 countries found by sector:\n")

mat_det_eu27 %>%
  distinct(
    sector,
    country
  ) %>%
  count(
    sector,
    name = "number_of_countries"
  ) %>%
  print()

cat("\nEU-27 country codes found:\n")

mat_det_eu27 %>%
  distinct(country) %>%
  arrange(country) %>%
  print(
    n = Inf
  )

missing_eu27 <- setdiff(
  eu27_codes,
  unique(mat_det_eu27$country)
)

if (length(missing_eu27) > 0) {
  warning(
    "The following EU-27 countries were not found in the loaded ",
    "country-level reporting files:\n",
    paste(missing_eu27, collapse = ", ")
  )
}

# ============================================================
# 6. Floor-space reporting
# ============================================================
# Floor-space variables occur once for every material.
# To prevent multiplying floor space by the number of materials,
# retain one common material ("concrete") and then sum across all
# remaining building dimensions.

floor_sector <- mat_det_eu27 %>%
  filter(material == "concrete") %>%
  group_by(
    country,
    sector,
    sector_code,
    scenario,
    scenario_name,
    year
  ) %>%
  summarise(
    floor_total_Mm2 = sum(
      floor_tot_Mm2,
      na.rm = TRUE
    ),
    floor_added_Mm2 = sum(
      floor_new_Mm2,
      na.rm = TRUE
    ),
    floor_demolished_Mm2 = sum(
      floor_dem_Mm2,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    year = as.integer(year)
  )

# Check that one row remains for every reporting combination
floor_duplicates <- floor_sector %>%
  count(
    country,
    sector,
    scenario,
    year
  ) %>%
  filter(n > 1)

if (nrow(floor_duplicates) > 0) {
  stop(
    "Duplicate floor-space rows remain after aggregation."
  )
}

# Confirm that the floor-flow variables contain non-zero values
cat("\nFloor-space variable checks after aggregation:\n")

floor_sector %>%
  summarise(
    floor_total_min = min(floor_total_Mm2, na.rm = TRUE),
    floor_total_max = max(floor_total_Mm2, na.rm = TRUE),
    floor_total_sum = sum(floor_total_Mm2, na.rm = TRUE),
    
    floor_added_min = min(floor_added_Mm2, na.rm = TRUE),
    floor_added_max = max(floor_added_Mm2, na.rm = TRUE),
    floor_added_sum = sum(floor_added_Mm2, na.rm = TRUE),
    floor_added_nonzero = sum(
      floor_added_Mm2 != 0,
      na.rm = TRUE
    ),
    
    floor_demolished_min = min(
      floor_demolished_Mm2,
      na.rm = TRUE
    ),
    floor_demolished_max = max(
      floor_demolished_Mm2,
      na.rm = TRUE
    ),
    floor_demolished_sum = sum(
      floor_demolished_Mm2,
      na.rm = TRUE
    ),
    floor_demolished_nonzero = sum(
      floor_demolished_Mm2 != 0,
      na.rm = TRUE
    )
  ) %>%
  print(width = Inf)

# ============================================================
# 7. Material reporting
# ============================================================

materials_sector <- mat_det_eu27 %>%
  filter(
    !material %in% excluded_materials
  ) %>%
  group_by(
    country,
    sector,
    sector_code,
    scenario,
    scenario_name,
    material,
    year
  ) %>%
  summarise(
    material_stock_Mt = sum(
      mat_stock_Mt,
      na.rm = FALSE
    ),
    material_inflow_Mt = sum(
      mat_demand_Mt,
      na.rm = FALSE
    ),
    material_outflow_Mt = sum(
      mat_scrap_Mt,
      na.rm = FALSE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    year = as.integer(year)
  )

# ============================================================
# 8. Create residential + commercial totals
# ============================================================
# A combined total is created only when both Residential and
# Commercial are available for the same country/scenario/year.
# This prevents residential-only values from being labelled as
# whole-building-sector totals.

floor_total <- floor_sector %>%
  group_by(
    country,
    scenario,
    scenario_name,
    year
  ) %>%
  summarise(
    number_of_sectors = n_distinct(sector),
    floor_total_Mm2 = sum(
      floor_total_Mm2,
      na.rm = FALSE
    ),
    floor_added_Mm2 = sum(
      floor_added_Mm2,
      na.rm = FALSE
    ),
    floor_demolished_Mm2 = sum(
      floor_demolished_Mm2,
      na.rm = FALSE
    ),
    .groups = "drop"
  ) %>%
  filter(
    number_of_sectors == 2
  ) %>%
  mutate(
    sector = "Buildings total",
    sector_code = "total"
  ) %>%
  select(
    country,
    sector,
    sector_code,
    scenario,
    scenario_name,
    year,
    floor_total_Mm2,
    floor_added_Mm2,
    floor_demolished_Mm2
  )

materials_total <- materials_sector %>%
  group_by(
    country,
    scenario,
    scenario_name,
    material,
    year
  ) %>%
  summarise(
    number_of_sectors = n_distinct(sector),
    material_stock_Mt = sum(
      material_stock_Mt,
      na.rm = FALSE
    ),
    material_inflow_Mt = sum(
      material_inflow_Mt,
      na.rm = FALSE
    ),
    material_outflow_Mt = sum(
      material_outflow_Mt,
      na.rm = FALSE
    ),
    .groups = "drop"
  ) %>%
  filter(
    number_of_sectors == 2
  ) %>%
  mutate(
    sector = "Buildings total",
    sector_code = "total"
  ) %>%
  select(
    country,
    sector,
    sector_code,
    scenario,
    scenario_name,
    material,
    year,
    material_stock_Mt,
    material_inflow_Mt,
    material_outflow_Mt
  )

floor_export <- bind_rows(
  floor_sector,
  floor_total
) %>%
  mutate(
    sector = factor(
      sector,
      levels = c(
        "Residential",
        "Commercial",
        "Buildings total"
      )
    ),
    scenario_name = factor(
      scenario_name,
      levels = scenarios$scenario_name
    )
  ) %>%
  arrange(
    country,
    sector,
    scenario_name,
    year
  ) %>%
  mutate(
    sector = as.character(sector),
    scenario_name = as.character(scenario_name)
  )

materials_export <- bind_rows(
  materials_sector,
  materials_total
) %>%
  mutate(
    sector = factor(
      sector,
      levels = c(
        "Residential",
        "Commercial",
        "Buildings total"
      )
    ),
    scenario_name = factor(
      scenario_name,
      levels = scenarios$scenario_name
    )
  ) %>%
  arrange(
    country,
    sector,
    scenario_name,
    material,
    year
  ) %>%
  mutate(
    sector = as.character(sector),
    scenario_name = as.character(scenario_name)
  )

# ============================================================
# 9. Create one long-format sharing file
# ============================================================

floor_long <- floor_export %>%
  pivot_longer(
    cols = c(
      floor_total_Mm2,
      floor_added_Mm2,
      floor_demolished_Mm2
    ),
    names_to = "variable_code",
    values_to = "value"
  ) %>%
  mutate(
    variable = recode(
      variable_code,
      floor_total_Mm2 =
        "Floor space|Total",
      floor_added_Mm2 =
        "Floor space|Newly added",
      floor_demolished_Mm2 =
        "Floor space|Demolished"
    ),
    unit = case_when(
      variable_code == "floor_total_Mm2" ~ "million m2",
      variable_code == "floor_added_Mm2" ~ "million m2/yr",
      variable_code == "floor_demolished_Mm2" ~ "million m2/yr",
      TRUE ~ NA_character_
    ),
    material = NA_character_
  ) %>%
  select(
    country,
    sector,
    scenario,
    scenario_name,
    year,
    variable,
    material,
    unit,
    value
  )

materials_long <- materials_export %>%
  pivot_longer(
    cols = c(
      material_stock_Mt,
      material_inflow_Mt,
      material_outflow_Mt
    ),
    names_to = "variable_code",
    values_to = "value"
  ) %>%
  mutate(
    variable = recode(
      variable_code,
      material_stock_Mt =
        "Material|Stock",
      material_inflow_Mt =
        "Material|Inflow",
      material_outflow_Mt =
        "Material|Outflow"
    ),
    unit = case_when(
      variable_code == "material_stock_Mt" ~ "Mt",
      variable_code == "material_inflow_Mt" ~ "Mt/yr",
      variable_code == "material_outflow_Mt" ~ "Mt/yr",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    country,
    sector,
    scenario,
    scenario_name,
    year,
    variable,
    material,
    unit,
    value
  )

additional_reporting_long <- bind_rows(
  floor_long,
  materials_long
) %>%
  arrange(
    country,
    sector,
    scenario_name,
    variable,
    material,
    year
  )

# ============================================================
# 10. Validation checks
# ============================================================

cat("\nFloor-space summary by sector:\n")

floor_export %>%
  group_by(sector) %>%
  summarise(
    rows = n(),
    countries = n_distinct(country),
    scenarios = n_distinct(scenario),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(
    width = Inf
  )

cat("\nMaterial summary by sector:\n")

materials_export %>%
  group_by(sector) %>%
  summarise(
    rows = n(),
    countries = n_distinct(country),
    scenarios = n_distinct(scenario),
    materials = n_distinct(material),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(
    width = Inf
  )

cat("\nMissing-value check:\n")

additional_reporting_long %>%
  group_by(
    sector,
    variable
  ) %>%
  summarise(
    rows = n(),
    missing_values = sum(is.na(value)),
    zero_values = sum(value == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(
    width = Inf
  )

# Check negative values
negative_values <- additional_reporting_long %>%
  filter(
    !is.na(value),
    value < 0
  )

if (nrow(negative_values) > 0) {
  
  warning(
    nrow(negative_values),
    " negative values were found. Inspect `negative_values`."
  )
}

# Check exact duplicate keys in long output
duplicate_keys <- additional_reporting_long %>%
  count(
    country,
    sector,
    scenario,
    year,
    variable,
    material
  ) %>%
  filter(n > 1)

if (nrow(duplicate_keys) > 0) {
  
  warning(
    nrow(duplicate_keys),
    " duplicated reporting keys were found. ",
    "Inspect `duplicate_keys`."
  )
}

# ============================================================
# 11. Export results
# ============================================================

date_stamp <- Sys.Date()

floor_file <- file.path(
  path_report,
  paste0(
    "floor_space_EU27_scenarios_",
    date_stamp,
    ".csv"
  )
)

materials_file <- file.path(
  path_report,
  paste0(
    "building_materials_EU27_scenarios_",
    date_stamp,
    ".csv"
  )
)

long_file <- file.path(
  path_report,
  paste0(
    "additional_reporting_EU27_long_",
    date_stamp,
    ".csv"
  )
)

write_csv(
  floor_export,
  floor_file,
  na = ""
)

write_csv(
  materials_export,
  materials_file,
  na = ""
)

write_csv(
  additional_reporting_long,
  long_file,
  na = ""
)

cat("\nSaved reporting files:\n")
cat("Floor-space results:\n", floor_file, "\n\n")
cat("Material results:\n", materials_file, "\n\n")
cat("Combined long-format results:\n", long_file, "\n")

# ============================================================
# 12. Important commercial-data note
# ============================================================

commercial_country_files <- file_inventory %>%
  filter(
    sector == "comm",
    aggregation_level == "country",
    exists
  )

if (nrow(commercial_country_files) == 0) {
  
  warning(
    paste0(
      "\nNo commercial region_bld material files were found.\n",
      "The exported country-level files therefore contain ",
      "residential data only, and no Buildings total rows were created.\n",
      "Commercial R12 files cannot be disaggregated reliably to countries.\n"
    )
  )
}



# ######### INSPECTION
# # ============================================================
# # CHECK 1: Coverage and expected row counts
# # ============================================================
# 
# expected_floor_rows <- 27 * 9 * 12
# expected_material_rows <- 27 * 9 * 12 * 7
# 
# floor_coverage_check <- floor_export %>%
#   group_by(sector) %>%
#   summarise(
#     rows = n(),
#     expected_rows = expected_floor_rows,
#     row_count_ok = rows == expected_rows,
#     countries = n_distinct(country),
#     country_count_ok = countries == 27,
#     scenarios = n_distinct(scenario),
#     scenario_count_ok = scenarios == 9,
#     years = n_distinct(year),
#     first_year = min(year, na.rm = TRUE),
#     last_year = max(year, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# material_coverage_check <- materials_export %>%
#   group_by(sector) %>%
#   summarise(
#     rows = n(),
#     expected_rows = expected_material_rows,
#     row_count_ok = rows == expected_rows,
#     countries = n_distinct(country),
#     country_count_ok = countries == 27,
#     scenarios = n_distinct(scenario),
#     scenario_count_ok = scenarios == 9,
#     materials = n_distinct(material),
#     material_count_ok = materials == 7,
#     years = n_distinct(year),
#     first_year = min(year, na.rm = TRUE),
#     last_year = max(year, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# cat("\nFloor-space coverage check:\n")
# print(floor_coverage_check, width = Inf)
# 
# cat("\nMaterial coverage check:\n")
# print(material_coverage_check, width = Inf)
# 
# # ============================================================
# # CHECK 2: Reporting years
# # ============================================================
# 
# reporting_years <- sort(unique(floor_export$year))
# 
# cat("\nReporting years:\n")
# print(reporting_years)
# 
# cat("\nNumber of reporting years:\n")
# print(length(reporting_years))
# 
# # ============================================================
# # CHECK 3: Missing, zero, and negative values
# # ============================================================
# 
# value_check <- additional_reporting_long %>%
#   group_by(
#     sector,
#     variable
#   ) %>%
#   summarise(
#     rows = n(),
#     missing_values = sum(is.na(value)),
#     zero_values = sum(value == 0, na.rm = TRUE),
#     negative_values = sum(value < 0, na.rm = TRUE),
#     minimum_value = min(value, na.rm = TRUE),
#     maximum_value = max(value, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# print(value_check, n = Inf, width = Inf)
# 
# negative_values <- additional_reporting_long %>%
#   filter(
#     !is.na(value),
#     value < 0
#   )
# 
# if (nrow(negative_values) == 0) {
#   cat("\nNegative-value check passed.\n")
# } else {
#   warning(
#     nrow(negative_values),
#     " negative values found. Inspect `negative_values`."
#   )
# }
# 
# 
# # ============================================================
# # CHECK 4: Duplicate reporting keys
# # ============================================================
# 
# duplicate_keys <- additional_reporting_long %>%
#   count(
#     country,
#     sector,
#     scenario,
#     year,
#     variable,
#     material
#   ) %>%
#   filter(n > 1)
# 
# if (nrow(duplicate_keys) == 0) {
#   cat("\nDuplicate-key check passed.\n")
# } else {
#   warning(
#     nrow(duplicate_keys),
#     " duplicated reporting keys found."
#   )
#   
#   print(duplicate_keys, n = Inf)
# }
# 
# # ============================================================
# # CHECK 5: Floor-space total arithmetic
# # ============================================================
# 
# floor_total_check <- floor_export %>%
#   mutate(
#     sector_key = case_when(
#       sector == "Residential" ~ "residential",
#       sector == "Commercial" ~ "commercial",
#       sector == "Buildings total" ~ "buildings_total",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   select(
#     country,
#     scenario,
#     year,
#     sector_key,
#     floor_total_Mm2,
#     floor_added_Mm2,
#     floor_demolished_Mm2
#   ) %>%
#   pivot_wider(
#     names_from = sector_key,
#     values_from = c(
#       floor_total_Mm2,
#       floor_added_Mm2,
#       floor_demolished_Mm2
#     )
#   ) %>%
#   mutate(
#     diff_total =
#       floor_total_Mm2_buildings_total -
#       floor_total_Mm2_residential -
#       floor_total_Mm2_commercial,
#     
#     diff_added =
#       floor_added_Mm2_buildings_total -
#       floor_added_Mm2_residential -
#       floor_added_Mm2_commercial,
#     
#     diff_demolished =
#       floor_demolished_Mm2_buildings_total -
#       floor_demolished_Mm2_residential -
#       floor_demolished_Mm2_commercial
#   )
# 
# floor_total_check_summary <- floor_total_check %>%
#   summarise(
#     max_abs_diff_total = max(abs(diff_total), na.rm = TRUE),
#     max_abs_diff_added = max(abs(diff_added), na.rm = TRUE),
#     max_abs_diff_demolished = max(
#       abs(diff_demolished),
#       na.rm = TRUE
#     )
#   )
# 
# print(floor_total_check_summary, width = Inf)
# 
# tolerance <- 1e-10
# 
# if (
#   all(
#     unlist(floor_total_check_summary) <= tolerance
#   )
# ) {
#   cat("\nFloor-space Buildings-total arithmetic check passed.\n")
# } else {
#   warning(
#     "Floor-space Buildings total does not exactly equal ",
#     "Residential + Commercial."
#   )
# }
# 
# 
# # ============================================================
# # CHECK 6: Material total arithmetic
# # ============================================================
# 
# material_total_check <- materials_export %>%
#   mutate(
#     sector_key = case_when(
#       sector == "Residential" ~ "residential",
#       sector == "Commercial" ~ "commercial",
#       sector == "Buildings total" ~ "buildings_total",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   select(
#     country,
#     scenario,
#     material,
#     year,
#     sector_key,
#     material_stock_Mt,
#     material_inflow_Mt,
#     material_outflow_Mt
#   ) %>%
#   pivot_wider(
#     names_from = sector_key,
#     values_from = c(
#       material_stock_Mt,
#       material_inflow_Mt,
#       material_outflow_Mt
#     )
#   ) %>%
#   mutate(
#     diff_stock =
#       material_stock_Mt_buildings_total -
#       material_stock_Mt_residential -
#       material_stock_Mt_commercial,
#     
#     diff_inflow =
#       material_inflow_Mt_buildings_total -
#       material_inflow_Mt_residential -
#       material_inflow_Mt_commercial,
#     
#     diff_outflow =
#       material_outflow_Mt_buildings_total -
#       material_outflow_Mt_residential -
#       material_outflow_Mt_commercial
#   )
# 
# material_total_check_summary <- material_total_check %>%
#   summarise(
#     max_abs_diff_stock = max(abs(diff_stock), na.rm = TRUE),
#     max_abs_diff_inflow = max(abs(diff_inflow), na.rm = TRUE),
#     max_abs_diff_outflow = max(abs(diff_outflow), na.rm = TRUE)
#   )
# 
# print(material_total_check_summary, width = Inf)
# 
# if (
#   all(
#     unlist(material_total_check_summary) <= tolerance
#   )
# ) {
#   cat("\nMaterial Buildings-total arithmetic check passed.\n")
# } else {
#   warning(
#     "Material Buildings total does not exactly equal ",
#     "Residential + Commercial."
#   )
# }
# 
# # ============================================================
# # CHECK 7: Zero floor-space additions
# # ============================================================
# 
# zero_floor_additions <- floor_export %>%
#   filter(
#     floor_added_Mm2 == 0
#   ) %>%
#   arrange(
#     sector,
#     scenario_name,
#     country,
#     year
#   )
# 
# zero_floor_additions_summary <- zero_floor_additions %>%
#   count(
#     sector,
#     scenario_name,
#     sort = TRUE
#   )
# 
# cat("\nZero floor-space additions by sector and scenario:\n")
# print(zero_floor_additions_summary, n = Inf)
# 
# cat("\nCountries with zero floor-space additions:\n")
# 
# zero_floor_additions %>%
#   count(
#     sector,
#     country,
#     sort = TRUE
#   ) %>%
#   print(n = Inf)
# 
# 
# # ============================================================
# # CHECK 8: Zero additions and stock change
# # ============================================================
# 
# floor_stock_change_check <- floor_export %>%
#   filter(
#     sector %in% c(
#       "Residential",
#       "Commercial"
#     )
#   ) %>%
#   arrange(
#     country,
#     sector,
#     scenario,
#     year
#   ) %>%
#   group_by(
#     country,
#     sector,
#     scenario
#   ) %>%
#   mutate(
#     previous_floor_Mm2 = lag(floor_total_Mm2),
#     floor_change_Mm2 =
#       floor_total_Mm2 - previous_floor_Mm2,
#     net_reported_flow_Mm2 =
#       floor_added_Mm2 - floor_demolished_Mm2
#   ) %>%
#   ungroup()
# 
# zero_addition_stock_check <- floor_stock_change_check %>%
#   filter(
#     floor_added_Mm2 == 0,
#     !is.na(floor_change_Mm2)
#   ) %>%
#   summarise(
#     rows = n(),
#     declining_stock_rows = sum(
#       floor_change_Mm2 < 0,
#       na.rm = TRUE
#     ),
#     stable_stock_rows = sum(
#       abs(floor_change_Mm2) < 1e-10,
#       na.rm = TRUE
#     ),
#     increasing_stock_rows = sum(
#       floor_change_Mm2 > 0,
#       na.rm = TRUE
#     )
#   )
# 
# print(zero_addition_stock_check, width = Inf)
# 
# 
# # ============================================================
# # CHECK 9: Material names
# # ============================================================
# 
# material_names <- materials_export %>%
#   distinct(material) %>%
#   arrange(material)
# 
# cat("\nMaterials included in export:\n")
# print(material_names, n = Inf)
# 
# 
# # ============================================================
# # CHECK 10: Zero material inflows
# # ============================================================
# 
# zero_material_inflows <- materials_export %>%
#   filter(
#     material_inflow_Mt == 0
#   )
# 
# cat("\nZero material inflows by sector and material:\n")
# 
# zero_material_inflows %>%
#   count(
#     sector,
#     material,
#     sort = TRUE
#   ) %>%
#   print(n = Inf)
# 
# cat("\nZero material inflows by scenario and material:\n")
# 
# zero_material_inflows %>%
#   count(
#     sector,
#     scenario_name,
#     material,
#     sort = TRUE
#   ) %>%
#   print(n = Inf)
# 
# 
# # ============================================================
# # CHECK 11: Reference versus Close floor-space equality
# # ============================================================
# 
# reference_close_check <- floor_export %>%
#   filter(
#     sector %in% c(
#       "Residential",
#       "Commercial"
#     ),
#     scenario %in% c(
#       "R",
#       "C_r",
#       "C_tp"
#     )
#   ) %>%
#   select(
#     country,
#     sector,
#     year,
#     scenario,
#     floor_total_Mm2,
#     floor_added_Mm2,
#     floor_demolished_Mm2
#   ) %>%
#   pivot_wider(
#     names_from = scenario,
#     values_from = c(
#       floor_total_Mm2,
#       floor_added_Mm2,
#       floor_demolished_Mm2
#     )
#   ) %>%
#   mutate(
#     diff_total_Cr =
#       floor_total_Mm2_C_r - floor_total_Mm2_R,
#     diff_total_Ctp =
#       floor_total_Mm2_C_tp - floor_total_Mm2_R,
#     
#     diff_added_Cr =
#       floor_added_Mm2_C_r - floor_added_Mm2_R,
#     diff_added_Ctp =
#       floor_added_Mm2_C_tp - floor_added_Mm2_R,
#     
#     diff_demolished_Cr =
#       floor_demolished_Mm2_C_r -
#       floor_demolished_Mm2_R,
#     
#     diff_demolished_Ctp =
#       floor_demolished_Mm2_C_tp -
#       floor_demolished_Mm2_R
#   )
# 
# reference_close_check %>%
#   summarise(
#     max_diff_total_Cr = max(
#       abs(diff_total_Cr),
#       na.rm = TRUE
#     ),
#     max_diff_total_Ctp = max(
#       abs(diff_total_Ctp),
#       na.rm = TRUE
#     ),
#     max_diff_added_Cr = max(
#       abs(diff_added_Cr),
#       na.rm = TRUE
#     ),
#     max_diff_added_Ctp = max(
#       abs(diff_added_Ctp),
#       na.rm = TRUE
#     ),
#     max_diff_demolished_Cr = max(
#       abs(diff_demolished_Cr),
#       na.rm = TRUE
#     ),
#     max_diff_demolished_Ctp = max(
#       abs(diff_demolished_Ctp),
#       na.rm = TRUE
#     )
#   ) %>%
#   print(width = Inf)

