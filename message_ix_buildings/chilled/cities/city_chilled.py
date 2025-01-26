import os
from itertools import product

import numpy as np
import pandas as pd
import xarray as xr

from message_ix_buildings.chilled.functions.buildings_funcs_cities import (
    P_f,
    Q_c_tmax,
    Q_h,
    calc_E_c_ac,
    calc_E_c_fan,
    calc_E_h,
    calc_gn_sol,
    calc_gn_sol_h,
    calc_gn_sol_tot,
    calc_H_tr,
    calc_H_v_cl,
    calc_H_v_op,
    calc_Nd,
    calc_Nf,
    calc_SCDD_m,
    calc_SHDD_m,
    calc_t_bal_c,
    calc_t_bal_h,
    calc_t_max_c,
    calc_vdd_h,
    calc_vdd_tmax_c,
)
from message_ix_buildings.chilled.functions.extract_cities import (
    rasters_to_df_cities,
    select_nearest_points,
)
from message_ix_buildings.chilled.functions.variable_dicts import VARS_ARCHETYPES
from message_ix_buildings.chilled.util.base import (
    get_archs,
    get_paths,
    load_all_scenarios_data,
    load_parametric_analysis_data,
)
from message_ix_buildings.chilled.util.common import get_logger, get_project_root
from message_ix_buildings.chilled.util.config import Config  # type: ignore

log = get_logger(__name__)

# list of GCMs and RCPs
list_gcm = ["MRI-ESM2-0"]
list_rcp = ["baseline", "ssp126", "ssp370"]

# specify config
config = Config(vstr="ALPS2024_cities", user="MEAS", gcm="MRI-ESM2-0", rcp="ssp585")

# set paths
project_path = get_paths(config, "project_path")
dle_path = get_paths(config, "dle_path")
input_path = dle_path
isimip_bias_adj_path = get_paths(config, "isimip_bias_adj_path")
isimip_ewembi_path = get_paths(config, "isimip_ewembi_path")
out_path = os.path.join(project_path, "out", "version", config.vstr)
archetype_path = os.path.join(out_path, "rasters")
floorarea_path = os.path.join(out_path, "floorarea_country")
save_path = os.path.join(out_path, "VDD_ene_calcs")
output_path_vdd = os.path.join(
    save_path,
    config.gcm,
    config.rcp,
)

# settings
sel_var = config.var
rcpdata = "ssp126" if config.rcp == "baseline" else config.rcp
if config.rcp == "baseline":
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

# green space file location, relative to the root directory
root_path = get_project_root()
green_path = os.path.join(root_path, "data", "green-space", "ALPS2024")

# create paths
if not os.path.exists(output_path_vdd):
    os.makedirs(output_path_vdd)

# read in archs and par_var
vers_archs = get_archs(config)
par_var = load_parametric_analysis_data(config)


# read in all files in green_path
city_lcz = pd.read_csv(os.path.join(green_path, "outer.csv")).drop(
    columns=["Unnamed: 0"]
)
lcz_coef = pd.read_csv(os.path.join(green_path, "outer_2.csv")).drop(
    columns=["Unnamed: 0"]
)
scen_gvi = pd.read_csv(os.path.join(green_path, "outer_3.csv")).drop(
    columns=["Unnamed: 0"]
)
hist_gvi = pd.read_csv(os.path.join(green_path, "outer_4.csv")).drop(
    columns=["Unnamed: 0"]
)


# Example: List of city coordinates (lat, lon)
city_df = city_lcz[["UC_NM_MN", "CTR_MN_ISO", "x", "y"]].drop_duplicates()

# use rasters_to_df_cities to extract data from rasters
t_city = rasters_to_df_cities(
    isimip_bias_adj_path,
    config.var,
    config.gcm,
    config.rcp,
    city_df,
    "UC_NM_MN",
    "y",
    "x",
)

# calculate t_out_ave column
t_city["t_out_ave"] = t_city[config.var] - 273.16

# add year and month columns
t_city["year"] = t_city["time"].dt.year
t_city["month"] = t_city["time"].dt.month


# read in i_sol_v and i_sol_h
with xr.open_dataarray(
    os.path.join(dle_path, "EWEMBI_vert_irrad_1980-2009_avg.nc")
) as ds:
    i_sol_v = ds.load()

with xr.open_dataarray(
    os.path.join(dle_path, "EWEMBI_horiz_irrad_1980-2009_avg.nc")
) as ds:
    i_sol_h = ds.load()

# select nearest points to city coordinates
i_sol_v_points = (
    select_nearest_points(i_sol_v, city_df, "UC_NM_MN", "y", "x")
    .to_dataframe()
    .reset_index()
)
i_sol_h_points = (
    select_nearest_points(i_sol_h, city_df, "UC_NM_MN", "y", "x")
    .to_dataframe()
    .reset_index()
)

# settings

# clim = 2030
# arch = "new"
# urt = "urban"
# p_it = par_var.itertuples()
# parset = next(p_it)


def read_netcdf_files(input_args):
    varname, arch, urt = input_args
    var = xr.open_dataset(
        os.path.join(archetype_path, "arch_" + arch + "_" + str(varname) + ".nc")
    )[urt]

    return var


def map_city_climate_variables(t_city, args):
    clim, arch, parset, urt = args
    log.info(str(clim) + " + " + arch + " + " + parset.name_run + " + " + urt)

    years_clim = yeardic[str(clim)]
    if config.testing_mode == 1:
        years_clim = (
            years_clim[0],
            str(int(years_clim[0]) + 1),
        )
    nyrs_clim = int(years_clim[1]) - int(years_clim[0]) + 1

    # filter t_city by years_clim
    t_city_filtered = t_city[
        (t_city["year"] >= int(years_clim[0])) & (t_city["year"] <= int(years_clim[1]))
    ]

    # drop duplicates
    t_city_filtered = t_city_filtered.drop_duplicates()

    # create t_city_month dataframe, with t_out_ave grouped by locations,
    # lat, lon, city, city_lat, city_lon, gcm, rcp, and month
    t_city_month = (
        t_city_filtered.groupby(
            [
                "gcm",
                "rcp",
                "locations",
                "lat",
                "lon",
                "city",
                "city_lat",
                "city_lon",
                "year",
                "month",
            ]
        )["t_out_ave"]
        .mean()
        .reset_index()
    )

    # dfa = pd.DataFrame(columns=["H_v_cl", "H_v_op", "H_tr"], index=par_var.index)

    suff = str(clim) + "_" + arch  # suffix
    # suff1 = arch  # only arch (for imports arch data)

    log.info("Starting: " + suff + "_" + str(parset.name_run))
    if config.cool == 1:
        cop = parset.cop
        t_sp_c = np.int8(parset.t_sp_c)  # Indoor setpoint temperature for cooling -> 26
        t_sp_c_max = np.int8(
            parset.t_sp_c_max
        )  # Indoor max temperature when fans are on (°C) -> 28

        f_c = parset.f_c
        f_f = parset.f_f

    if config.heat == 1:
        t_sp_h = np.int8(
            parset.t_sp_h
        )  # Indoor max temperature when fans are on (°C) -> 20
        eff = parset.eff  # Efficiency heating system

        f_h = parset.f_h

    if config.runsdd == 1:
        bal_temp = config.bal_temps[0]
        log.info("Calculating: SCDD_m")
        sdd_c = calc_SCDD_m(t_city_filtered, "t_out_ave", "city", bal_temp, nyrs_clim)
        log.info("... Completed calculating SCDD_m")

        log.info("Calculating: SHDD_m")
        sdd_h = calc_SHDD_m(t_city_filtered, "t_out_ave", "city", bal_temp, nyrs_clim)
        log.info("... Completed calculating SHDD_m")

    list_args = product(VARS_ARCHETYPES, [arch], [urt])
    list_netcdf = list(map(read_netcdf_files, list_args))
    dict_netcdf = dict(zip(VARS_ARCHETYPES, list_netcdf))

    # for all dataarrays in list_netcdf, select nearest points to city coordinates
    for key, value in dict_netcdf.items():
        dict_netcdf[key] = select_nearest_points(
            value, city_df, "UC_NM_MN", "y", "x"
        ).to_dataframe()

    gn_sol = calc_gn_sol(
        i_sol_v_points,
        "irrad",
        dict_netcdf["gl_perc"],
        dict_netcdf["gl_g"],
        dict_netcdf["gl_sh"],
    )

    if config.solar_gains == "VERT":
        gn_sol = calc_gn_sol(
            i_sol_v_points,
            "irrad",
            dict_netcdf["gl_perc"],
            dict_netcdf["gl_g"],
            dict_netcdf["gl_sh"],
        )

    elif config.solar_gains == "TOT":
        gn_sol = calc_gn_sol_tot(
            i_sol_v_points,
            "irrad",
            i_sol_h_points,
            "irrad",
            dict_netcdf["gl_perc"],
            dict_netcdf["gl_g"],
            dict_netcdf["gl_sh"],
            dict_netcdf["roof_area"],
            dict_netcdf["roof_abs"],
            dict_netcdf["u_roof"],
        )

    elif config.solar_gains == "HOR":
        gn_sol = calc_gn_sol_h(
            i_sol_h_points,
            "irrad",
            dict_netcdf["roof_area"],
            dict_netcdf["roof_abs"],
            dict_netcdf["u_roof"],
        )

    # ==============================================================================
    # Heat transfer functions
    # ==============================================================================
    H_v_cl = calc_H_v_cl(dict_netcdf["vol"], dict_netcdf["ach_cl"])
    H_v_op = calc_H_v_op(dict_netcdf["vol"], dict_netcdf["ach_op"])
    H_tr = calc_H_tr(dict_netcdf["u_val"], dict_netcdf["area_env"])

    if config.cool == 1:
        t_bal_c = calc_t_bal_c(t_sp_c, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_cl)
        t_max_c = calc_t_max_c(t_sp_c_max, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_op)
        Nd = calc_Nd(t_city_filtered, "t_out_ave", t_max_c, nyrs_clim)
        Nf = calc_Nf(t_city_filtered, "t_out_ave", t_bal_c, nyrs_clim)
        vdd_tmax_c = calc_vdd_tmax_c(t_city_month, "t_out_ave", t_max_c)
        qctmax = Q_c_tmax(H_tr, H_v_cl, vdd_tmax_c, t_max_c, t_bal_c, Nd, f_c)
        E_c_ac = calc_E_c_ac(qctmax, cop)
        E_c_fan = calc_E_c_fan(f_f, P_f, Nf, config.area_fan)

    if config.heat == 1:
        t_bal_h = calc_t_bal_h(t_sp_h, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_cl)
        vdd_h = calc_vdd_h(t_city_month, "t_out_ave", t_bal_h)
        qh = Q_h(H_tr, H_v_cl, f_h, vdd_h)
        E_h = calc_E_h(qh, eff)

    # Save each calculated variable as an individual CSV file
    output_data = {
        "H_v_cl": H_v_cl,
        "H_v_op": H_v_op,
        "H_tr": H_tr,
    }

    if config.cool == 1:
        output_data.update(
            {
                "t_bal_c": t_bal_c,
                "t_max_c": t_max_c,
                "Nd": Nd,
                "Nf": Nf,
                "vdd_tmax_c": vdd_tmax_c,
                "qctmax": qctmax,
                "E_c_ac": E_c_ac,
                "E_c_fan": E_c_fan,
            }
        )

    if config.heat == 1:
        output_data.update(
            {
                "t_bal_h": t_bal_h,
                "vdd_h": vdd_h,
                "qh": qh,
                "E_h": E_h,
            }
        )

    # Save each variable as an individual CSV file
    for var_name, var_value in output_data.items():
        fname = f"{suff}_{parset.Index}_{var_name}_{urt}.csv"
        print(fname)
        filestr = os.path.join(output_path_vdd, fname)
        var_value.to_csv(filestr, index=False)


# apply process_climate_data function to all combinations of scenarios and runs
s_runs = load_all_scenarios_data(config).clim
inputs = product(s_runs, vers_archs, par_var.itertuples(), ["urban"])
list(map(lambda args: map_city_climate_variables(t_city, args), inputs))

# NOTE: next step: floor area calculations

s_run = s_runs.itertuples()
s_run = next(s_run)

if config.floor_setting == "std_cap":
    floorarea = pd.read_csv(
        os.path.join(input_path, "floor_std_cap.csv")
    )  # STD conditioned floor area (same for all regions, based on DLE)
elif config.floor_setting == "per_cap":
    suff = str(s_run.scen) + "_" + str(s_run.year) + "_" + str(s_run.clim)  # suffix
    suff2 = str(s_run.scen) + "_" + str(s_run.year)  # suffix: only scen and year

    floorarea = pd.read_excel(
        os.path.join(input_path, "floor_per_cap_" + config.vstrcntry + ".xlsx"),
        sheet_name=suff2,
    )

# use the UC_NM_MN and CTR_MN_ISO columns in city_lcz to map ISO3 to MESSAGEix R11 regions
# read in the countries in message_ix_models.data.node.R11
