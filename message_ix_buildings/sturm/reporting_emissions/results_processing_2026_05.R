
library("tidyverse")
library("readxl")
library("writexl")
library("patchwork")
library("zoo") # approx NAs
library("RColorBrewer")
library("ggnewscale")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Postprocessing STURM results - emission calculation

options(scipen=999)

# Version model runs
#v_runs <- "2026_03" ### TO UPDATE


# Path model data
##path_in_runs <- paste0("./../model_runs_2026_03/STURM_output/output_",v_runs,"/") ### TO UPDATE
path_in_runs <- paste0("C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/output/") ### TO UPDATE

path_in_ems_op <- "./emission_intensity_operational/"
path_in_ems_emb <- ".//emission_intensity_ecoinvent/"
#path_in_ems_emb <- "./emission_intensity_materials_2024/"


# Path reporting
path_report <- "./"
path_plots <- paste0(path_report,"/plots/")
path_print <- paste0(path_report,"/print_results/")
path_tables <- paste0(path_report,"/tables/")



### SETUP    

# Full set of scenarios ### TO UPDATE
# scenarios <- data.frame(scenario = c("NPi-Reference", "NPi-Sufficiency", "NPi-Circular", "15C-Reference", "15C-Sufficiency", "15C-Circular"),
#                         scenario_name = c("NPi-Reference", "NPi-Sufficiency", "NPi-Circular", "15C-Reference", "15C-Sufficiency", "15C-Circular"),
#                         scenario_supply = c(rep("NPi",3),rep("1p5C",3)))

# Full set of scenarios ### TO UPDATE
# Full set of scenarios ### TO UPDATE
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

# Sectors
# sectors <- data.frame(sector = c("resid", "comm"),
#                       sector_name = c("Residential", "Commercial"))

sectors <- data.frame(
  sector = c("resid"),
  sector_name = c("Residential")
)

fuels <- data.frame(fuel = c("biomass_solid", "coal", "district_heat", "electricity", "gas", "oil"),
                    fuel_name = c("Biomass", "Coal", "District Heat", "Electricity", "Gas", "Oil"))

# Load regions
regions <- read_csv("regions_R61.csv")
regions_r6 <- read_csv("regions_R61_R6.csv") %>% 
  mutate(R6 = ifelse(R6 == "Central and South America", "South America", R6)) # mapping region_bld (R61) and region_gea (R11)


# #Reporting 6 regions 
R6 <- sort(unique(regions_r6$R6)) # new definition
#R6 <- c("EU27","USA","other GN","China","India","other GS") #"Subs.Africa", # old definition


# Regional definitions
reg_EU <- c("C-EEU-BGR","C-EEU-CZE","C-EEU-EST","C-EEU-HUN","C-EEU-HVR","C-EEU-LTU","C-EEU-LVA",
            "C-EEU-POL","C-EEU-ROU","C-EEU-SVK","C-EEU-SVN",
            "C-WEU-AUT","C-WEU-BEL",#"C-WEU-CHE",
            "C-WEU-CYP","C-WEU-DEU","C-WEU-DNK","C-WEU-ESP","C-WEU-FIN","C-WEU-FRA",#"C-WEU-GBR",
            "C-WEU-GRC","C-WEU-IRL",#"C-WEU-ISL",
            "C-WEU-ITA","C-WEU-LUX","C-WEU-MLT","C-WEU-NLD",#"C-WEU-NOR",
            "C-WEU-PRT","C-WEU-SWE")

# Years
yrs <- c(seq(2020,2060,5),seq(2070,2100,10))

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
                  #"Concrete - biochar", # Brick - CDR
                  #"Mortar", # Mortar
                  #"Plaster", # Plaster
                  "Wood", # Wood
                  "Glass",
                  "Steel",
                  "Aluminum",
                  "Copper"
))


# Load & process data
# # CO2 Emission factors - operational
# # Unit: kgCO2/GJ
# ems_int_op <- read_csv(paste0(path_in_ems_op, "emission_factors_ENGAGE.csv")) 
# ems_int_op <- ems_int_op %>%
#   pivot_longer(cols=matches("\\d"), names_to = "year", values_to = "ems_int_op") %>%
#   mutate(year = as.integer(year)) %>%
#   #rename(region = region_gea) %>%
#   rename(scenario_supply = clim_policy) %>%
#   arrange(scenario_supply, region_gea, year, fuel) 

###SN
# CO2 Emission factors - operational
# Unit: kgCO2/GJ
# Uses baseline-aligned file:
#   2020: 1p5C = NPi
#   2025: 1p5C = NPi
#   2030 onward: original 1p5C pathway

ems_int_op <- read_csv(
  paste0(
    path_in_ems_op,
    "emission_factors_ENGAGE_baselineAligned_2020_2025.csv"
  )
)

ems_int_op <- ems_int_op %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "ems_int_op"
  ) %>%
  mutate(year = as.integer(year)) %>%
  rename(scenario_supply = clim_policy) %>%
  arrange(scenario_supply, region_gea, year, fuel)

# Optional sanity check: 1p5C and NPi should match in 2020 and 2025
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

# Load: Share of secondary production on total production 
# for weighting primary/secondary embodied emission factors
# Only available for China (same values applied to all regions)
materials<-c("aluminum","copper","steel")

shr_secondary_ref <- read_csv("./secondary production/share_secondary_production_pauliuk_2021_test_2100.csv") %>% 
  pivot_longer(cols = materials, names_to = "material", values_to = "shr_sec")

shr_secondary_ref <- crossing(scenarios %>% filter(grepl("Reference",scenario)), shr_secondary_ref)

shr_secondary_high <- read_csv("./secondary production/share_secondary_production_pauliuk_2021_test_2100_HIGH.csv") %>% 
  pivot_longer(cols = materials, names_to = "material", values_to = "shr_sec")

shr_secondary_high <- crossing(scenarios %>% filter(grepl("15C",scenario)), shr_secondary_high)

shr_secondary <- bind_rows(shr_secondary_ref, shr_secondary_high)
  
rm(shr_secondary_ref, shr_secondary_high)

# CO2 Emission factors - embodied
# Unit: kgCO2/kg
#ems_int_emb <- read_csv(paste0(path_in_ems_emb, "ghg_image_r12_2025-05-06_edit.csv"))


# ems_int_emb <- ems_int_emb %>%
#   mutate(emission_factor = ifelse(year %in% c(2040,2080), NA, emission_factor)) %>%
#   bind_rows(ems_int_emb %>% filter(year==2020) %>% mutate(scenario_supply = "1p5C")) %>%
#   bind_rows(ems_int_emb %>% filter(year==2030) %>% mutate(year = 2025) %>% mutate(emission_factor = NA)) %>%
#   bind_rows(ems_int_emb %>% filter(year==2030) %>% mutate(year = 2035) %>% mutate(emission_factor = NA)) %>%
#   bind_rows(ems_int_emb %>% filter(year==2030) %>% mutate(year = 2045) %>% mutate(emission_factor = NA)) %>%
#   bind_rows(ems_int_emb %>% filter(year==2030) %>% mutate(year = 2055) %>% mutate(emission_factor = NA)) %>%
#   select(-unit) %>%
#   arrange(scenario_supply, region_gea, material, production, year) %>%
#   group_by(scenario_supply, region_gea, material, production) %>%
#   mutate(emission_factor = na.approx(emission_factor, na.rm = F)) %>%
#   ungroup %>%
#   rename(ems_int_emb = emission_factor)


# CO2 Emission factors - embodied
# Unit: kgCO2/kg
# Uses baseline-aligned file:
#   2020: 1p5C = NPi
#   2025: 1p5C = NPi interpolated baseline
#   2030 onward: original 1p5C pathway


###SN
ems_int_emb_raw <- read_csv(
  paste0(
    path_in_ems_emb,
    "ghg_image_r12_2025-05-06_edit_baselineAligned_2020_2025.csv"
  )
)

# Check that baseline alignment worked
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

ems_int_emb <- ems_int_emb_raw %>%
  # Keep Alessio's original smoothing/interpolation logic for 2040 and 2080
  mutate(
    emission_factor = ifelse(
      year %in% c(2040, 2080),
      NA_real_,
      emission_factor
    )
  ) %>%
  # Add intermediate years needed by STURM reporting.
  # Do NOT add 2020 or 2025 here because the patched file already contains them.
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
  select(-unit) %>%
  arrange(scenario_supply, region_gea, material, production, year) %>%
  group_by(scenario_supply, region_gea, material, production) %>%
  mutate(
    emission_factor = na.approx(emission_factor, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  rename(ems_int_emb = emission_factor)
##SN


ems_int_emb <- crossing(scenarios,stp_df %>% select(year)) %>%
  left_join(ems_int_emb) %>%
  pivot_wider(names_from = "production", values_from = "ems_int_emb") %>%
  left_join(shr_secondary) %>%
  rename(shr_secondary = shr_sec) %>%
  mutate(shr_secondary = ifelse(is.na(shr_secondary), 0, shr_secondary)) %>%
  mutate(ems_int_emb = ifelse(shr_secondary>0, primary*(1-shr_secondary) + secondary*shr_secondary, primary)) %>%
  select(scenario, scenario_name, scenario_supply, region_gea, year, material, ems_int_emb) %>%
  arrange(scenario, scenario_name, scenario_supply, region_gea, material, year)

# # CO2 Emission factors - embodied -- OLD DATA
# # Unit: kgCO2/kg
# ems_int_emb <- read_csv(paste0(path_in_ems_emb, "ghg_intensity_production_processed.csv"))
# 
# # Weighted average primary - secondary production
# ems_int_emb <- scenarios %>% 
#   left_join(ems_int_emb, relationship = "many-to-many") %>%
#   pivot_longer(cols=matches("\\d"), names_to = "year", values_to = "ems_int_emb") %>%
#   mutate(year = as.integer(year)) %>%
#   pivot_wider(names_from = "production", values_from = "ems_int_emb") %>%
#   left_join(shr_secondary) %>%
#   rename(shr_secondary = shr_sec) %>%
#   mutate(shr_secondary = ifelse(is.na(shr_secondary), 0, shr_secondary)) %>%
#   mutate(ems_int_emb = ifelse(shr_secondary>0, primary*(1-shr_secondary) + secondary*shr_secondary, primary)) %>%
#   select(scenario, scenario_name, scenario_supply, region_gea, year, material, ems_int_emb) %>%
#   arrange(scenario, scenario_name, scenario_supply, region_gea, material, year)


##### UPDATE ALL FROM HERE #####




# Energy demand results - detailed results (by eneff and fuel)

## 1) Load results - NPi scenarios

en_det <- data.frame()

for (s in 1:nrow(scenarios)) for(i in 1:nrow(sectors)){
  en_tmp <- read_csv(paste0(path_in_runs, "report_STURM_",scenarios$scenario[s],"_", sectors$sector[i],"_region_bld_energy.csv"))
  # aggregate results - by region and fuel
  en_tmp <- en_tmp %>%
    #rename(region = R12) %>%
    mutate(scenario = scenarios$scenario[s]) %>%
    mutate(scenario_name = scenarios$scenario_name[s]) %>%
    mutate(sector = sectors$sector_name[i], .after="region_bld")
  en_det <- en_det %>% bind_rows(en_tmp)
  rm(en_tmp)
}


# Energy demand results - aggregated by fuel 
en <- en_det %>%
  #filter(urt == "urb") %>% # URBAN ONLY
  group_by(region_bld,sector, fuel_heat, scenario,scenario_name, year) %>%
  summarise(stock_M = sum(stock_M),
            floor_bnm2 = sum(floor_Mm2)/1e3,
            heat_EJ = sum(heat_TJ)/1e6,
            cool_EJ = sum(cool_TJ)/1e6,
            hotwater_EJ = sum(hotwater_TJ)/1e6
  ) %>%
  ungroup 

# # Re-aggregate cooling - under electricity
# en_cool <- en %>% 
#   select(sector, scenario,scenario_name, year, cool_EJ) %>%
#   group_by(sector,scenario,scenario_name,year) %>%
#   summarise(cool_EJ = sum(cool_EJ)) %>%
#   ungroup %>%
#   mutate(fuel = "electricity",.after=sector)
# 
# # Add to energy dataset
# en <- en %>% select(-cool_EJ) %>% left_join(en_cool) %>% mutate(cool_EJ = ifelse(is.na(cool_EJ),0,cool_EJ)) 
# 
# # Replace NAs
# en <- en %>% mutate(across(.cols=c("heat_EJ","hotwater_EJ","cool_EJ"), ~replace_na(.,  0))) # WITHOUT APPLIANCES


# Calculate energy totals by fuel
en_tot <- en %>%
  select(-c("stock_M","floor_bnm2")) %>%
  pivot_longer(cols = c("heat_EJ", "cool_EJ", "hotwater_EJ"), names_to = "enduse",values_to = "en_EJ") %>%
  mutate(fuel = ifelse(enduse == "cool_EJ", "electricity", fuel_heat)) %>%
  group_by(region_bld, sector, scenario,scenario_name, fuel, year) %>%
  summarise(en_EJ = sum(en_EJ)) %>%
  ungroup %>%
  filter(fuel != "v_no_heat") %>%
  left_join(scenarios) %>% 
  relocate(scenario_name, scenario_supply, .after=scenario) %>%
  #mutate(scenario = factor(scenario, levels = scenarios$scenario)) %>% # Use factors to order data
  mutate(scenario_name = factor(scenario_name, levels = scenarios$scenario_name)) %>% # Use factors to order data
  mutate(sector = factor(sector, levels = sectors$sector_name)) %>% # Use factors to order data
  arrange(region_bld,sector,scenario)


# Material demand results - detailed
mat_det <- data.frame()

for (s in 1:nrow(scenarios)) for(i in 1:nrow(sectors)){
  mat_tmp <- read_csv(paste0(path_in_runs, "report_STURM_",scenarios$scenario[s],"_", sectors$sector[i],"_region_bld_material.csv"))
  # aggregate results - by region and fuel
  mat_tmp <- mat_tmp %>%
    #rename(region = R12) %>%
    mutate(scenario = scenarios$scenario[s]) %>%
    mutate(scenario_name = scenarios$scenario_name[s]) %>%
    mutate(sector = sectors$sector_name[i], .after="region_bld")
  mat_det <- mat_det %>% bind_rows(mat_tmp)
  rm(mat_tmp)
}

    

# Regional material results - total by material
# Apply updated material intensities and aggregate data - New buildings  
mat_tot_new <- mat_det %>%
  #filter(urt == "urb") %>% # URBAN ONLY
  filter(material != "cement") %>%
  select(region_bld, sector, scenario,scenario_name, material, year, mat_stock_Mt, mat_demand_Mt, mat_scrap_Mt) %>%
  group_by(region_bld, sector, scenario, scenario_name,  material, year) %>%
  summarise(mat_stock_Mt = sum(mat_stock_Mt),
            mat_demand_Mt = sum(mat_demand_Mt),
            mat_scrap_Mt = sum(mat_scrap_Mt),
  ) %>%
  ungroup %>%
  left_join(scenarios) %>% 
  relocate(scenario_name, scenario_supply, .after=scenario) %>%
  mutate(scenario_name = factor(scenario_name, levels = scenarios$scenario_name)) %>% # Use factors to order data
  mutate(sector = factor(sector, levels = sectors$sector_name)) %>% # Use factors to order data
  arrange(region_bld, sector, scenario, material, year)


### Calculate CO2 Emissions ###

# Calculate CO2 emissions - operational
# Calculate emissions
ems_op <- scenarios %>%
  left_join(en_tot) %>%
  left_join(regions %>% select(region_bld,region_gea)) %>%
  left_join(ems_int_op) %>%
  mutate(type = ifelse(fuel %in% c("electricity","district_heat"), "indirect","direct")) %>%
  mutate(ems_op = en_EJ * ems_int_op) %>%
  select(region_bld, sector, scenario, scenario_name, scenario_supply,fuel, type, year, ems_op) 
  
# Calculate CO2 emissions - embodied
# Calculate emissions
ems_emb <- scenarios %>%
  left_join(mat_tot_new, by = c("scenario", "scenario_name", "scenario_supply")) %>%
  left_join(regions %>% select(region_bld, region_gea), by = "region_bld") %>%
  left_join(
    ems_int_emb,
    by = c("scenario", "scenario_name", "scenario_supply", "material", "year", "region_gea")
  ) %>%
  # filter(scenario_recycling == scenario_recycling_sel) %>%
  mutate(
    type = "embodied",
    ems_emb = mat_demand_Mt * ems_int_emb
  ) %>%
  select(
    region_bld, sector,
    scenario, scenario_name, scenario_supply,
    material, type, year,
    ems_emb
  )


##SN
# ============================================================
# Export emissions outputs
# ============================================================

path_out_ems <- "C:/Users/nawawi/Documents/GitHub/message-ix-buildings/message_ix_buildings/sturm/visualization/emissions_outputs/"

dir.create(path_out_ems, recursive = TRUE, showWarnings = FALSE)

# Detailed emissions outputs
write_csv(
  ems_op,
  file.path(path_out_ems, "resid_operational_emissions_detailed.csv")
)

write_csv(
  ems_emb,
  file.path(path_out_ems, "resid_embodied_emissions_detailed.csv")
)

# Summary emissions outputs
embodied_summary <- ems_emb %>%
  group_by(sector, scenario, scenario_name, year) %>%
  summarise(
    embodied_MtCO2e = sum(ems_emb, na.rm = TRUE),
    .groups = "drop"
  )

operational_summary <- ems_op %>%
  group_by(sector, scenario, scenario_name, year) %>%
  summarise(
    operational_MtCO2e = sum(ems_op, na.rm = TRUE),
    .groups = "drop"
  )

ghg_summary <- embodied_summary %>%
  full_join(
    operational_summary,
    by = c("sector", "scenario", "scenario_name", "year")
  ) %>%
  mutate(
    embodied_MtCO2e = replace_na(embodied_MtCO2e, 0),
    operational_MtCO2e = replace_na(operational_MtCO2e, 0),
    total_MtCO2e = embodied_MtCO2e + operational_MtCO2e
  )

write_csv(
  embodied_summary,
  file.path(path_out_ems, "resid_embodied_emissions_summary.csv")
)

write_csv(
  operational_summary,
  file.path(path_out_ems, "resid_operational_emissions_summary.csv")
)

write_csv(
  ghg_summary,
  file.path(path_out_ems, "resid_total_emissions_summary.csv")
)

cat("\nSaved emissions outputs to:\n", path_out_ems, "\n")


### CHECK
ems_emb %>%
  count(scenario_name)

ems_op %>%
  count(scenario_name)

