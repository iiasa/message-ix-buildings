import datetime

import numpy as np

from message_ix_buildings.chilled.aggregate import (
    aggregate_ISO_tables_to_regions,
    create_prereg_data,
)
from message_ix_buildings.chilled.regression import apply_regression
from message_ix_buildings.chilled.user_settings import DICT_USER_SETTINGS
from message_ix_buildings.chilled.vdd_functions import (
    aggregate_urban_rural_files,
    calculate_cumulative_carbon_emissions,
    create_archetype_template_map,
    create_building_archetype_maps,
    create_climate_variables_maps,
    create_dummy_folders,
    load_all_scenarios_data,
    load_parametric_analysis_data,
    make_vdd_total_maps,
    process_construction_shares,
    process_country_maps,
    process_final_maps,
    process_floor_area_maps,
    process_iso_tables_updated,
    read_in_gaul_file,
)

start = datetime.datetime.now()

# SPECIFY USER
user = "MEAS"  # options: ALE, ED, MEAS
print(f"USER: {user}")

# RUN SETTINGS
paranalysis_mode = 1  # 1 = run entire parametric analysis; 0 = run only ref case
runsdd = 0  # 1= run simple (standard) degree days calculation; 0 = don't run

# Netcdf settings
netcdf4_format = "NETCDF4_CLASSIC"
comp = dict(zlib=True, complevel=5)  # Compression between 0 and 9 (highest)

# TESTING MODE
testing_mode = (
    0  # 1= selects only two years for testing; 0= select all years (full calculation)
)

# VERSION SETTINGS
vstr = "v19_ALPS2023"  # version input data
vstrcntry = "v4"  # version string for country data and floor surface
gcm = "IPSL-CM6A-LR"  # options: GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, and UKESM1-0-LL

# SCENARIO SETTINGS
rcps = ["ssp126", "ssp370", "ssp585", "baseline"]  # list all possible scenarios
rcp = "baseline"  # options: "ssp126", "ssp370", "ssp585", "baseline"
print(f"SCENARIO: {rcp}")

if rcp == "baseline":
    rcpdata = "ssp126"
else:
    rcpdata = rcp


# CLIMATIC DATA INPUTS
if rcp == "baseline":
    yeardic = {
        "2015": ("2015", "2020"),
        "2020": ("2015", "2020"),
        "2030": ("2015", "2020"),
        "2040": ("2015", "2020"),
        "2050": ("2015", "2020"),
        "2060": ("2015", "2020"),
        "2070": ("2015", "2020"),
        "2080": ("2015", "2020"),
        "2090": ("2015", "2020"),
        "2100": ("2015", "2020"),
    }
else:
    yeardic = {
        "2015": ("2015", "2020"),
        "2020": ("2015", "2025"),
        "2030": ("2015", "2045"),
        "2040": ("2025", "2055"),
        "2050": ("2035", "2065"),
        "2060": ("2045", "2075"),
        "2070": ("2055", "2085"),
        "2080": ("2065", "2095"),
        "2090": ("2080", "2100"),
        "2100": ("2095", "2100"),
    }

# climatic inputs for sensitivity runs
# clims = ["hist"]  # options: "hist", "1p5"
clims = list(yeardic.keys())

# POPULATION SETTINGS
popfix = True  # If True, fix to SSP2, else.... (see script 4/5)

# BUILDING SCENARIO SETTINGS

# CONMSTRUCTION MATERIAL SHARES
constr_setting = 0

# FLOOR_SETTING CHOICE
# Choose per_cap for different values by MESSAGE region or std_cap for fixed values
floor_setting = "std_cap"  # v16; options: "std_cap", "per_cap"

# BUILDING ARCHETYPE SETTINGS
# Choose "regional" for different values by MESSAGE region or "fixed" for fixed values
# (same for all regions)
arch_setting = "regional"  # options: ["fixed", "regional"]

# URBAN/RURAL DISAGGREGATIONS
urts = ["urban", "rural"]  # options (mult): "urban", "rural"

# ARCHETYPES
archs = ["new", "exist"]

# PARAMETERS FOR STEP 2 SCRIPT
verbose = True

## SWITCH COOLING/HEATING CALCULATIONS
## 1=calculate; 0=skip
heat = 0
cool = 1

# CHOICE: SOLAR GAIN CALCULATION
solar_gains = "TOT"  # from windows and roof
# solar_gains = 'VERT' #from windows only
# solar_gains = 'HOR' #from windows only

# SELECT VAR
var = "tas"

if var == "tas":
    davar = "tas"
elif var == "twb":
    davar = "twb"

overwrite = 0

# Climate input variable format
climate_filestr_hist = f"tas_day_{gcm}_{rcpdata}_r1i1p1_EWEMBI_landonly_"  # ISIMIP2

if gcm == "UKESM1-0-LL":
    climate_filestr_future = f"{gcm}_r1i1p1f2_w5e5_{rcpdata}_{var}_global_daily_"
else:
    climate_filestr_future = f"{gcm}_r1i1p1f1_w5e5_{rcpdata}_{var}_global_daily_"

endstr = ".nc"


# BULDING PARAMETERS
# Fixed values
bal_temps = [18.3, 21.1, 26]  # [21.1] #  For simple cooling degree days
arb_fan = 2
t_sp_h = np.int8(20)  # Indoor setpoint temperature for heating
P_f = 55  # power of fan (W)
area_fan = 25  # Numer of m2 per fan
gridshape2 = (360, 720)


y2_attrs_dic = {
    "title": "map_area_env",
    "authors": "Alessio Mastrucci & Edward Byers",
    "date": str(datetime.datetime.now()),
    "institution": "IIASA Energy Program",
    "contact": "mastrucc@iiasa.ac.at; byers@iiasa.ac.at; ",
    "arch_setting": arch_setting,
}

# Final maps (y4) parameters
nd_thresh = 5


def set_up_run():
    # Load scenarios data
    s_runs = load_all_scenarios_data(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"], input_version_name=vstr
    )

    # Load paramtric analysis data
    par_var = load_parametric_analysis_data(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_version_name=vstr,
        input_paranalysis_mode=paranalysis_mode,
    )

    # Create dummy folders
    create_dummy_folders(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_version_name=vstr,
        input_gcm=gcm,
        input_rcp_scenario=rcp,
    )

    return s_runs, par_var


s_runs, par_var = set_up_run()

# NOTE: the following is run per GCM and RCP


def create_archetype_files():
    create_archetype_template_map(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_version_name=vstr,
        input_gcm=gcm,
        input_rcp_scenario=rcp,
        input_message_region_file=DICT_USER_SETTINGS[user]["message_region_map_file"],
        input_archs_specified=archs,
        input_arch_setting=arch_setting,
        input_urts=urts,
        input_comp=comp,
    )

    create_building_archetype_maps(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_version_name=vstr,
        input_gcm=gcm,
        input_rcp_scenario=rcp,
        input_arch_setting=arch_setting,
        input_archs=archs,
        input_urts=urts,
        input_comp=comp,
    )


create_archetype_files()


def grab_gcm_rcp_data():
    create_climate_variables_maps(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_version_name=vstr,
        input_rcp_scenario=rcp,
        input_rcpdata=rcpdata,
        input_gcm=gcm,
        input_start=start,
        input_clims=clims,
        input_testing_mode=testing_mode,
        input_yeardic=yeardic,
        input_isimip_bias_adj_path=DICT_USER_SETTINGS[user]["isimip_bias_adj_path"],
        input_isimip_ewemib_path=DICT_USER_SETTINGS[user]["isimip_ewembi_path"],
        input_climate_filestr_hist=climate_filestr_hist,
        input_climate_filestr_fut=climate_filestr_future,
        input_endstring=endstr,
        input_chunk_size=DICT_USER_SETTINGS[user]["chunk_size"],
        input_davar=davar,
        input_archs=archs,
        input_arch_setting=arch_setting,
        input_par_var=par_var,
        input_cool=cool,
        input_heat=heat,
        input_runsdd=runsdd,
        input_bal_temps=bal_temps,
        input_comp=comp,
        input_verbose=verbose,
        input_urts=urts,
        input_solar_gains=solar_gains,
        input_area_fan=area_fan,
    )


def aggregate_gcm_rcp_data():
    print("Running aggregate_urban_rural_files()...")
    aggregate_urban_rural_files(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_version_name=vstr,
        input_gcm=gcm,
        input_rcp_scenario=rcp,
        input_clims=clims,
        input_archs=archs,
        input_cool=cool,
        input_heat=heat,
        input_par_var=par_var,
        input_urts=urts,
    )

    print("Running make_vdd_total_maps()...")
    make_vdd_total_maps(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_version_name=vstr,
        input_gcm=gcm,
        input_rcp_scenario=rcp,
        input_s_runs=s_runs,
        input_clims=clims,
        input_archs=archs,
        input_urts=urts,
        input_cool=cool,
        input_heat=heat,
    )

    print("Creating country_ras, ISO_attrs, reg_ras...")
    country_ras, ISO_attrs, reg_ras = read_in_gaul_file(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_message_region_file=DICT_USER_SETTINGS[user]["message_region_map_file"],
    )

    print("Running process_construction_shares()...")
    process_construction_shares(
        input_constr_setting=constr_setting,
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_urts=urts,
        input_country_ras=country_ras,
        input_iso_attrs=ISO_attrs,
        input_version_name=vstr,
        input_gcm=gcm,
        input_rcp_scenario=rcp,
    )

    print("Running process_construction_shares()...")
    process_floor_area_maps(
        input_floor_setting=floor_setting,
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_version_name=vstr,
        input_gcm=gcm,
        input_rcp_scenario=rcp,
        input_urts=urts,
        input_reg_ras=reg_ras,
        input_s_runs=s_runs,
        input_vstrcntry=vstrcntry,
    )

    print("Running process_country_maps()...")
    process_country_maps(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_version_name=vstr,
        input_gcm=gcm,
        input_rcp_scenario=rcp,
        input_s_runs=s_runs,
        input_vstrcntry=vstrcntry,
        input_iso_attrs=ISO_attrs,
        input_country_ras=country_ras,
    )

    print("Running process_final_maps()...")
    process_final_maps(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_version_name=vstr,
        input_gcm=gcm,
        input_rcp_scenario=rcp,
        input_cool=cool,
        input_heat=heat,
        input_par_var=par_var,
        input_paranalysis_mode=paranalysis_mode,
        input_s_runs=s_runs,
        input_clims=clims,
        input_archs=archs,
        input_popfix=popfix,
        input_floor_setting=floor_setting,
        input_nd_thresh=nd_thresh,
        input_urts=urts,
        input_comp=comp,
    )

    print("Running process_iso_tables_updated()...")
    process_iso_tables_updated(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_version_name=vstr,
        input_gcm=gcm,
        input_rcp_scenario=rcp,
        input_vstrcntry=vstrcntry,
        input_cool=cool,
        input_heat=heat,
        input_clims=clims,
        input_s_runs=s_runs,
        input_archs=archs,
        input_urts=urts,
        input_par_var=par_var,
    )


aggregate_gcm_rcp_data()

# NOTE: from here on, run after running all GCMs and RCPs


def process_all_gcm_data():
    calculate_cumulative_carbon_emissions(
        input_dle_path=DICT_USER_SETTINGS[user]["dle_path"],
        input_version_name=vstr,
        input_snapshot_file=DICT_USER_SETTINGS[user]["ar6_snapshot_file"],
        input_yeardic=yeardic,
    )

    aggregate_ISO_tables_to_regions(
        dle_path=DICT_USER_SETTINGS[user]["dle_path"], version_name=vstr
    )

    create_prereg_data(dle_path=DICT_USER_SETTINGS[user]["dle_path"], version_name=vstr)


process_all_gcm_data()

# NOTE: from here on, applying regression

apply_regression(dle_path=DICT_USER_SETTINGS[user]["dle_path"], version_name=vstr)
