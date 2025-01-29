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


def process_climate_data(config: Config, climate_zones: bool = True):
    # set paths
    project_path = get_paths(config, "project_path")
    dle_path = get_paths(config, "dle_path")
    input_path = dle_path
    isimip_bias_adj_path = get_paths(config, "isimip_bias_adj_path")
    isimip_ewembi_path = get_paths(config, "isimip_ewembi_path")
    out_path = os.path.join(project_path, "out", "version", config.vstr)
    archetype_path = os.path.join(out_path, "rasters")
    floorarea_path = os.path.join(out_path, "floorarea_country")
    vdd_path = os.path.join(out_path, "VDD_ene_calcs")
    if climate_zones:
        output_path_vdd = os.path.join(
            vdd_path,
            "climate_zones",
            config.gcm,
            config.rcp,
        )
    else:
        output_path_vdd = os.path.join(
            "cities",
            vdd_path,
            config.gcm,
            config.rcp,
        )
    # create output_path_vdd if it does not exist
    if not os.path.exists(output_path_vdd):
        os.makedirs(output_path_vdd)

    # settings
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
    tas_city = rasters_to_df_cities(
        isimip_bias_adj_path,
        config.var,
        config.gcm,
        rcpdata,
        city_df,
        "UC_NM_MN",
        "y",
        "x",
    )

    # tas_city as csv
    # tas_city.to_csv(os.path.join("/Users/meas/Desktop", "tas_city.csv"), index=False)

    # read in tas_city
    # tas_city = pd.read_csv(os.path.join("/Users/meas/Desktop", "tas_city.csv"))

    # change time column to datetime
    tas_city["time"] = pd.to_datetime(tas_city["time"])

    # add year and month columns
    tas_city["year"] = tas_city["time"].dt.year
    tas_city["month"] = tas_city["time"].dt.month

    if climate_zones:
        tas_city = pd.merge(
            tas_city,
            hist_gvi,
            how="outer",
            left_on=["city", "month"],
            right_on=["UC_NM_MN", "month"],
        )

        # Calculate adjusted tas column
        tas_city["tas_adj"] = tas_city["tas"] * (1 + tas_city["delta"] / 100)

        # calculate t_out_ave column using adjusted tas
        tas_city["t_out_ave"] = tas_city["tas_adj"] - 273.16

        # drop rows where tas_adj is NaN
        tas_city = tas_city.dropna(subset=["tas_adj", "lcz"]).reset_index(drop=True)

    else:
        tas_city["t_out_ave"] = tas_city["tas"] - 273.16

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

    def map_city_climate_variables(t_city, climate_zones, args):
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
            (t_city["year"] >= int(years_clim[0]))
            & (t_city["year"] <= int(years_clim[1]))
        ]

        # drop duplicates
        t_city_filtered = t_city_filtered.drop_duplicates()

        # create t_city_month dataframe, with t_out_ave grouped by locations,
        # lat, lon, city, city_lat, city_lon, gcm, rcp, and month
        groupby_cols = [
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
        if climate_zones:
            groupby_cols += ["lcz"]

        t_city_month = (
            t_city_filtered.groupby(groupby_cols)["t_out_ave"].mean().reset_index()
        )

        # dfa = pd.DataFrame(columns=["H_v_cl", "H_v_op", "H_tr"], index=par_var.index)

        suff = str(clim) + "_" + arch  # suffix
        # suff1 = arch  # only arch (for imports arch data)

        log.info("Starting: " + suff + "_" + str(parset.name_run))
        if config.cool == 1:
            cop = parset.cop
            t_sp_c = np.int8(
                parset.t_sp_c
            )  # Indoor setpoint temperature for cooling -> 26
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
            sdd_c = calc_SCDD_m(
                t_city_filtered, "t_out_ave", "city", bal_temp, nyrs_clim
            )
            log.info("... Completed calculating SCDD_m")

            log.info("Calculating: SHDD_m")
            sdd_h = calc_SHDD_m(
                t_city_filtered, "t_out_ave", "city", bal_temp, nyrs_clim
            )
            log.info("... Completed calculating SHDD_m")

        list_args = product(VARS_ARCHETYPES, [arch], [urt])
        list_netcdf = list(map(read_netcdf_files, list_args))
        dict_netcdf = dict(zip(VARS_ARCHETYPES, list_netcdf))

        # for all dataarrays in list_netcdf, select nearest points to city coordinates
        for key, value in dict_netcdf.items():
            dict_netcdf[key] = select_nearest_points(
                value, city_df, "UC_NM_MN", "y", "x"
            ).to_dataframe()

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
            t_max_c = calc_t_max_c(
                t_sp_c_max, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_op
            )
            Nd = calc_Nd(
                t_city_filtered, "t_out_ave", t_max_c, nyrs_clim, climate_zones
            )
            Nf = calc_Nf(
                t_city_filtered, "t_out_ave", t_bal_c, nyrs_clim, climate_zones
            )
            vdd_tmax_c = calc_vdd_tmax_c(
                t_city_month, "t_out_ave", t_max_c, nyrs_clim, climate_zones
            )

            qctmax = Q_c_tmax(
                H_tr, H_v_cl, vdd_tmax_c, t_max_c, t_bal_c, Nd, f_c, climate_zones
            )
            E_c_ac = calc_E_c_ac(qctmax, cop)
            E_c_fan = calc_E_c_fan(f_f, P_f, Nf, config.area_fan)

        if config.heat == 1:
            t_bal_h = calc_t_bal_h(t_sp_h, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_cl)
            vdd_h = calc_vdd_h(t_city_month, "t_out_ave", t_bal_h)
            qh = Q_h(H_tr, H_v_cl, f_h, vdd_h)
            E_h = calc_E_h(qh, eff)

        # Function to add metadata columns to a dataframe
        def add_metadata(df, clim, arch, parset, urt):
            df["clim"] = clim
            df["arch"] = arch
            df["name_run"] = parset.name_run
            df["urt"] = urt
            df["gcm"] = config.gcm
            df["rcp"] = config.rcp
            return df

        # Add metadata to each dataframe
        H_v_cl = add_metadata(H_v_cl, clim, arch, parset, urt)
        H_v_op = add_metadata(H_v_op, clim, arch, parset, urt)
        H_tr = add_metadata(H_tr, clim, arch, parset, urt)

        if config.cool == 1:
            t_bal_c = add_metadata(t_bal_c, clim, arch, parset, urt)
            t_max_c = add_metadata(t_max_c, clim, arch, parset, urt)
            Nd = add_metadata(Nd, clim, arch, parset, urt)
            Nf = add_metadata(Nf, clim, arch, parset, urt)
            vdd_tmax_c = add_metadata(vdd_tmax_c, clim, arch, parset, urt)
            qctmax = add_metadata(qctmax, clim, arch, parset, urt)
            E_c_ac = add_metadata(E_c_ac, clim, arch, parset, urt)
            E_c_fan = add_metadata(E_c_fan, clim, arch, parset, urt)

        if config.heat == 1:
            t_bal_h = add_metadata(t_bal_h, clim, arch, parset, urt)
            vdd_h = add_metadata(vdd_h, clim, arch, parset, urt)
            qh = add_metadata(qh, clim, arch, parset, urt)
            E_h = add_metadata(E_h, clim, arch, parset, urt)

        # Collect all dataframes into a dictionary
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

        return output_data

    # apply process_climate_data function to all combinations of scenarios and runs
    s_runs = load_all_scenarios_data(config).clim
    # par_var_sel = par_var.iloc[1].to_frame().T
    inputs = product(s_runs, vers_archs, par_var.itertuples(), ["urban"])
    output = list(
        map(lambda args: map_city_climate_variables(tas_delta, True, args), inputs)
    )

    # concat all dataframes for each variable in output into a single dataframe
    output_data = {}
    for key in output[0].keys():
        output_data[key] = pd.concat([df[key] for df in output])

    # save each dataframe in output_data to a csv file
    for key, value in output_data.items():
        log.info(f"Saving {key} to csv in {output_path_vdd}")
        value.to_csv(os.path.join(output_path_vdd, key + ".csv"))

    # # read in city level parameters data
    # df_cities_par = pd.read_csv(
    #     os.path.join(root_path, "data", "alps", "cities_population_buildings.csv")
    # )

    # df_cities_par_exist = (
    #     df_cities_par.query("vintage == 'exist'")
    #     .rename(columns={"shr_floor": "shr_floor_exist"})
    #     .filter(
    #         items=[
    #             "city",
    #             "citycode",
    #             "country",
    #             "iso-alpha3_code",
    #             "global_south",
    #             "region_gea",
    #             "sector",
    #             "year",
    #             "popscen",
    #             "shr_floor_exist",
    #         ]
    #     )
    # )
    # df_cities_par_new = (
    #     df_cities_par.query("vintage == 'new'")
    #     .rename(columns={"shr_floor": "shr_floor_new"})
    #     .filter(
    #         items=[
    #             "city",
    #             "citycode",
    #             "country",
    #             "iso-alpha3_code",
    #             "global_south",
    #             "region_gea",
    #             "sector",
    #             "year",
    #             "popscen",
    #             "shr_floor_new",
    #         ]
    #     )
    # )

    # df_cities_vintage = pd.merge(
    #     df_cities_par_exist,
    #     df_cities_par_new,
    #     on=[
    #         "city",
    #         "citycode",
    #         "country",
    #         "iso-alpha3_code",
    #         "global_south",
    #         "region_gea",
    #         "sector",
    #         "year",
    #         "popscen",
    #     ],
    #     how="inner",
    # )

    # df_cities_par_wide = pd.merge(
    #     df_cities_par.drop(columns=["vintage", "shr_floor"]),
    #     df_cities_vintage,
    #     on=[
    #         "city",
    #         "citycode",
    #         "country",
    #         "iso-alpha3_code",
    #         "global_south",
    #         "region_gea",
    #         "sector",
    #         "year",
    #         "popscen",
    #     ],
    #     how="inner",
    # )

    # # Convert df_cities_par to wide format on vintage, with shr_floor as values
    # df_cities_par_wide = df_cities_par.pivot_table(
    #     index=[
    #         col for col in df_cities_par.columns if col not in ["vintage", "shr_floor"]
    #     ],
    #     columns="vintage",
    #     values="shr_floor",
    #     aggfunc="first",
    # ).reset_index()

    # # get E_c_ac and E_c_fan data, merge together and merge with df_cities_par
    # df_e_c_ac = output_data["E_c_ac"]
    # df_e_c_fan = output_data["E_c_fan"]

    # # pivot the E_c_ac column on arch
    # df_e_c_ac_wide = df_e_c_ac.pivot_table(
    #     index=[col for col in df_e_c_ac.columns if col not in ["E_c_ac", "arch"]],
    #     columns="arch",
    #     values="E_c_ac",
    #     aggfunc="first",
    # ).reset_index()

    # # rename columns
    # df_e_c_ac_wide.columns = [
    #     f"E_c_ac_{col}" if col in ["exist", "new"] else col
    #     for col in df_e_c_ac_wide.columns
    # ]

    # df_e_c_fan_wide = df_e_c_fan.pivot_table(
    #     index=[col for col in df_e_c_fan.columns if col not in ["E_c_fan", "arch"]],
    #     columns="arch",
    #     values="E_c_fan",
    #     aggfunc="first",
    # ).reset_index()

    # # rename columns
    # df_e_c_fan_wide.columns = [
    #     f"E_c_fan_{col}" if col in ["exist", "new"] else col
    #     for col in df_e_c_fan_wide.columns
    # ]

    # merge_cols = [
    #     "gcm",
    #     "rcp",
    #     "name_run",
    #     "urt",
    #     "city",
    #     "city_lat",
    #     "city_lon",
    #     "lat",
    #     "lon",
    #     "clim",
    #     "month",
    # ]
    # if climate_zones:
    #     merge_cols += ["lcz"]

    # df_e_inten = pd.merge(
    #     df_e_c_ac_wide,
    #     df_e_c_fan_wide,
    #     on=merge_cols,
    #     how="inner",
    # )

    # df_e_inten = pd.merge(
    #     df_e_inten,
    #     df_cities_par_wide,
    #     left_on=["city", "clim"],
    #     right_on=["city", "year"],
    #     how="inner",
    # )

    # def calculate_energy(df, intensity_col, energy_col):
    #     out = df.copy()
    #     out[energy_col] = (
    #         out["population"]
    #         * out["floor_cap"]
    #         * out["ac_penetr_u"]
    #         * (
    #             out["shr_floor_exist"] * out[f"{intensity_col}_exist"]
    #             + out["shr_floor_new"] * out[f"{intensity_col}_new"]
    #         )
    #         * out["f_c_scl"]
    #         * out["fl_cnd_c"]
    #         / out["eff_ac"]
    #     )
    #     return out

    # # Calculate energy needed for AC
    # df_energy = calculate_energy(df_e_inten, "E_c_ac", "energy_cooling_ac")

    # # Save to CSV

    # return df_energy


def calculate_energy(config: Config, climate_zones: bool = True):
    # set paths
    project_path = get_paths(config, "project_path")
    dle_path = get_paths(config, "dle_path")
    input_path = dle_path
    isimip_bias_adj_path = get_paths(config, "isimip_bias_adj_path")
    isimip_ewembi_path = get_paths(config, "isimip_ewembi_path")
    out_path = os.path.join(project_path, "out", "version", config.vstr)
    archetype_path = os.path.join(out_path, "rasters")
    floorarea_path = os.path.join(out_path, "floorarea_country")
    vdd_path = os.path.join(out_path, "VDD_ene_calcs")

    # search for "E_c_ac.csv" in vdd_path (search recursively)
    for root, dirs, files in os.walk(vdd_path):
        for file in files:
            if file == "E_c_ac.csv":
                df_energy = pd.read_csv(os.path.join(root, file))
