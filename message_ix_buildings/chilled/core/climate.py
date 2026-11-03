import datetime
import os
from itertools import product

import cartopy  # type: ignore
import cartopy.crs as ccrs  # type: ignore
import cartopy.feature as cfeature  # type: ignore
import matplotlib.pyplot as plt  # type: ignore
import numpy as np
import pandas as pd
import xarray as xr
from dask.diagnostics import ProgressBar

from message_ix_buildings.chilled.functions.buildings_funcs_grid import (
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
from message_ix_buildings.chilled.functions.variable_dicts import (
    VARDICT_COOL,
    VARDICT_HEAT,
    VARS_ARCHETYPES,
    VARUNDICT_COOL,
    VARUNDICT_HEAT,
)
from message_ix_buildings.chilled.preprocess.message_raster import (
    create_message_raster,  # type: ignore
)
from message_ix_buildings.chilled.util.base import (
    get_archs,
    get_paths,
    load_all_scenarios_data,
    load_parametric_analysis_data,
)
from message_ix_buildings.chilled.util.common import get_logger
from message_ix_buildings.chilled.util.config import Config  # type: ignore

log = get_logger(__name__)


def create_climate_variables_maps(config: "Config", start_time: datetime.datetime):
    project_path = get_paths(config, "project_path")
    dle_path = get_paths(config, "dle_path")
    isimip_bias_adj_path = get_paths(config, "isimip_bias_adj_path")
    isimip_ewembi_path = get_paths(config, "isimip_ewembi_path")

    out_path = os.path.join(project_path, "out", "version", config.vstr)
    archetype_path = os.path.join(out_path, "rasters")
    save_path = os.path.join(out_path, "VDD_ene_calcs")

    output_path_vdd = os.path.join(
        save_path,
        config.gcm,
        config.rcp,
    )

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

    if not os.path.exists(output_path_vdd):
        os.makedirs(output_path_vdd)

    vers_archs = get_archs(config)
    par_var = load_parametric_analysis_data(config)

    def map_calculated_variables(args):
        clim, arch, parset, urt = args

        log.info(str(clim) + " + " + arch + " + " + parset.name_run + " + " + urt)

        years_clim = yeardic[str(clim)]
        # << this selects the correct years.
        # But when testing you’ll want to use just say 3 years data,
        # so set years manually, e.g.
        # years_clim = yeardic6p0[str(s_run.clim)]

        # this will be the shortcut line to make the testing faster (2 years data)
        if config.testing_mode == 1:
            years_clim = (
                years_clim[0],
                str(int(years_clim[0]) + 1),
            )

        nyrs_clim = int(years_clim[1]) - int(years_clim[0]) + 1

        # Climate input variable format
        climate_filestr_hist = (
            f"tas_day_{config.gcm}_{rcpdata}_r1i1p1_EWEMBI_landonly_"  # ISIMIP2
        )

        if config.gcm == "UKESM1-0-LL":
            climate_filestr_future = (
                f"{config.gcm}_r1i1p1f2_w5e5_{rcpdata}_{config.var}_global_daily_"
            )
        else:
            climate_filestr_future = (
                f"{config.gcm}_r1i1p1f1_w5e5_{rcpdata}_{config.var}_global_daily_"
            )

        endstr = ".nc"

        if str(clim) == "hist":
            isi_folder = isimip_ewembi_path
            filestr = climate_filestr_hist
        else:
            isi_folder = isimip_bias_adj_path
            filestr = climate_filestr_future

        filepath = os.path.join(
            isi_folder, rcpdata, config.gcm, f"{filestr.lower()}*{endstr}"
        )

        log.info("Reading: " + filepath)
        if config.rcp == "rcp26":
            dst = xr.open_mfdataset(
                filepath,
                chunks={"lon": get_paths(config, "chunk_size")},
                concat_dim="time",
                use_cftime=True,
            )  # Setting for RCP2.6
        else:
            dst = xr.open_mfdataset(
                filepath,
                chunks={"lon": get_paths(config, "chunk_size")},
            )  # , concat_dim='time' )  # Setting for RCP6.0

        dst_crop = dst.sel(time=slice(years_clim[0], years_clim[1]))
        t_out_ave = dst_crop[config.davar].astype("float32") - 273.16
        t_out_ave = t_out_ave.transpose("lat", "lon", "time")
        t_oa_gbm = t_out_ave.groupby("time.month")

        i_sol_v = xr.open_dataarray(
            os.path.join(dle_path, "EWEMBI_vert_irrad_1980-2009_avg.nc")
        )  # Values  in daily Wh/m2

        # Horizontal irradiation
        i_sol_h = xr.open_dataarray(
            os.path.join(dle_path, "EWEMBI_horiz_irrad_1980-2009_avg.nc")
        )  # Values in daily Wh/m2

        if config.arch_setting == "regional":
            xr.open_dataset(
                os.path.join(
                    archetype_path,
                    "arch_map_" + config.arch_setting + "_" + arch + ".nc",
                )
            )

        dfa = pd.DataFrame(columns=["H_v_cl", "H_v_op", "H_tr"], index=par_var.index)

        suff = str(clim) + "_" + arch  # suffix
        # suff =  clim+'_'+arch+'_'+str(parset.name_run)  #suffix
        suff1 = arch  # only arch (for imports arch data)

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

        #            # Archetypes should be in parset to execute this  (OLD CODE)
        #            if arch_setting == 'fixed':
        #                au = parset.arch_u
        #                ar = parset.arch_r
        #                al = [ar, au]

        #    log.info(sddf)

        if config.runsdd == 1:
            # =============================================================
            # Simple CDD calculation
            # =============================================================

            for bal_temp in config.bal_temps:
                with ProgressBar():
                    log.info("Stage 3 - Simple HDDCDD - cooling")
                    log.info("Balance temp " + str(bal_temp) + "C")
                    sdd_c = calc_SCDD_m(t_out_ave, bal_temp)
                    sdd_c = sdd_c.chunk(chunks={"lon": get_paths(config, "chunk_size")})
                    log.info("chunked")
                    sdd_c.attrs = {
                        "name": "sdd_c",
                        "description": "Simple cooling degree days",
                        "units": "degC",
                        "short name": "CDD.",
                        #                                   'urt': urt,
                        "name_run": parset.name_run,
                        "id_run": str(parset.Index),
                        "bal_temp": str(bal_temp),
                    }
                    log.info(sdd_c)
                    sdd_c = sdd_c.to_dataset(name="sdd_c")
                    encoding = {"sdd_c": config.comp}
                    # fname = suff+'_'+str(parset.Index)+'_sdd_c_'+str(bal_temp)+'.nc'
                    fname = suff + "_sdd_c_" + str(bal_temp) + ".nc"
                    filestr = os.path.join(output_path_vdd, fname)
                    sdd_c.to_netcdf(filestr, encoding=encoding)
                    log.info("...Saved " + filestr)
                    if config.verbose:
                        log.info(datetime.datetime.now() - start_time)
                    sdd_c = xr.open_dataarray(filestr)

            # ==============================================================
            # Simple HDD calculation
            # ==============================================================

            for bal_temp in config.bal_temps:
                with ProgressBar():
                    log.info("Stage 3 - Simple HDDCDD - heating")
                    log.info("Balance temp " + str(bal_temp) + "C")
                    sdd_h = calc_SHDD_m(t_out_ave, bal_temp)
                    sdd_h = sdd_h.chunk(chunks={"lon": get_paths(config, "chunk_size")})
                    log.info("chunked")
                    sdd_h.attrs = {
                        "name": "sdd_h",
                        "description": "Simple heating degree days",
                        "units": "degC",
                        "short name": "HDD.",
                        #                                   'urt': urt,
                        "name_run": parset.name_run,
                        "id_run": str(parset.Index),
                        "bal_temp": str(bal_temp),
                    }
                    sdd_h = sdd_h.to_dataset(name="sdd_h")
                    encoding = {"sdd_h": config.comp}
                    # fname = suff+'_'+str(parset.Index)+'_sdd_h_'+str(bal_temp)+'.nc'
                    fname = suff + "_sdd_h_" + str(bal_temp) + ".nc"
                    filestr = os.path.join(output_path_vdd, fname)
                    sdd_h.to_netcdf(filestr, encoding=encoding)
                    log.info("...Saved " + filestr)
                    if config.verbose:
                        log.info(datetime.datetime.now() - start_time)
                    sdd_h = xr.open_dataarray(filestr)

        def read_netcdf_files(input_args):
            varname, arch, urt = input_args
            var = xr.open_dataset(
                os.path.join(
                    archetype_path, "arch_" + arch + "_" + str(varname) + ".nc"
                )
            )[urt]

            return var

        list_args = product(VARS_ARCHETYPES, [arch], [urt])
        list_netcdf = list(map(read_netcdf_files, list_args))
        dict_netcdf = dict(zip(VARS_ARCHETYPES, list_netcdf))

        if config.solar_gains == "VERT":
            # Solar gains - From windows only
            with ProgressBar():
                log.info("Stage 3 - calc gn_sol")
                gn_sol = calc_gn_sol(
                    i_sol_v,
                    dict_netcdf["gl_perc"],
                    dict_netcdf["gl_g"],
                    dict_netcdf["gl_sh"],
                )
                gn_sol = gn_sol.chunk(chunks={"lon": get_paths(config, "chunk_size")})
                log.info("chunked")
                gn_sol.attrs = {
                    "name": "gn_sol",
                    "description": "Solar gains - Windows",
                    "units": "W/m2",
                    "short name": "Solar gains - Windows",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }
                gn_sol = gn_sol.to_dataset(name="gn_sol")
                encoding = {"gn_sol": config.comp}
                fname = suff + "_" + str(parset.Index) + "_gn_sol_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                gn_sol.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
                gn_sol = xr.open_dataarray(filestr).load()

        elif config.solar_gains == "TOT":
            # Solar gains - Total
            with ProgressBar():
                log.info("Stage 3 - calc gn_sol")
                gn_sol = calc_gn_sol_tot(
                    i_sol_v,
                    dict_netcdf["gl_perc"],
                    dict_netcdf["gl_g"],
                    dict_netcdf["gl_sh"],
                    i_sol_h,
                    dict_netcdf["roof_area"],
                    dict_netcdf["roof_abs"],
                    dict_netcdf["u_roof"],
                )
                gn_sol = gn_sol.chunk(chunks={"lon": get_paths(config, "chunk_size")})
                log.info("chunked")
                gn_sol.attrs = {
                    "name": "gn_sol",
                    "description": "Solar gains",
                    "units": "W/m2",
                    "short name": "Solar gains",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }
                gn_sol = gn_sol.to_dataset(name="gn_sol")
                encoding = {"gn_sol": config.comp}
                fname = suff + "_" + str(parset.Index) + "_gn_sol_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                gn_sol.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
                gn_sol = xr.open_dataarray(filestr).load()

        elif config.solar_gains == "HOR":
            # Solar gains - Total
            with ProgressBar():
                log.info("Stage 3 - calc gn_sol")
                gn_sol = calc_gn_sol_h(
                    i_sol_h,
                    dict_netcdf["roof_area"],
                    dict_netcdf["roof_abs"],
                    dict_netcdf["u_roof"],
                )
                gn_sol = gn_sol.chunk(chunks={"lon": get_paths(config, "chunk_size")})
                log.info("chunked")
                gn_sol.attrs = {
                    "name": "gn_sol",
                    "description": "Solar gains",
                    "units": "W/m2",
                    "short name": "Solar gains",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }
                gn_sol = gn_sol.to_dataset(name="gn_sol")
                encoding = {"gn_sol": config.comp}
                fname = suff + "_" + str(parset.Index) + "_gn_sol_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                gn_sol.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
                gn_sol = xr.open_dataarray(filestr).load()

        # ==============================================================================
        # Heat transfer functions
        # ==============================================================================
        with ProgressBar():
            H_v_cl = calc_H_v_cl(dict_netcdf["vol"], dict_netcdf["ach_cl"])
            if config.verbose:
                log.info("Stage 3 - calc_H_v_cl")
                # log.info(datetime.datetime.now() - start_time)
        # H_v_cl = xr.open_dataarray(output_folder2+'H_v_cl_'+urt+'.nc').load()

        with ProgressBar():
            H_v_op = calc_H_v_op(dict_netcdf["vol"], dict_netcdf["ach_op"])
            if config.verbose:
                log.info("Stage 3 - calc_H_v_op")
                # log.info(datetime.datetime.now() - start_time)
        # H_v_op = xr.open_dataarray(output_folder2+'H_v_op_'+urt+'.nc').load()

        with ProgressBar():
            H_tr = calc_H_tr(dict_netcdf["u_val"], dict_netcdf["area_env"])
            if config.verbose:
                log.info("Stage 3 - calc_H_tr")
                # log.info(datetime.datetime.now() - start_time)
        #    H_tr = xr.open_dataarray(output_folder2+'H_tr_'+urt+'.nc').load()
        dfa.loc[parset.Index, :] = [H_v_cl, H_v_op, H_tr]

        if config.cool == 1:
            # ==============================================================================
            # Variable CDD functions
            # ==============================================================================
            with ProgressBar():
                log.info("t_bal_c")
                t_bal_c = calc_t_bal_c(
                    t_sp_c, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_cl
                ).astype("float32")  # , x_diff0
                t_bal_c = t_bal_c.chunk(chunks={"lon": get_paths(config, "chunk_size")})
                log.info("chunked")
                t_bal_c.attrs = {
                    "name": "t_bal_c",
                    "description": "Balance (base) temperature",
                    "units": "degC",
                    "short name": "Balance temp.",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }
                t_bal_c = t_bal_c.to_dataset(
                    name="t_bal_c"
                )  # comment out because already a Dataset
                encoding = {"t_bal_c": config.comp}
                fname = suff + "_" + str(parset.Index) + "_t_bal_c_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                t_bal_c.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
                t_bal_c = xr.open_dataarray(filestr)

            # =============================================================================
            # t_max_c
            # =============================================================================
            with ProgressBar():
                log.info("Calc_t_max_c")
                t_max_c = calc_t_max_c(
                    t_sp_c_max, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_op
                )  # , x_diff0)
                t_max_c.attrs = {
                    "name": "t_max_c",
                    "description": "This returns the max temperature",
                    "units": "degC",
                    "short name": "Max temp.",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }
                t_max_c = t_max_c.to_dataset(name="t_max_c")
                encoding = {"t_max_c": config.comp}
                fname = suff + "_" + str(parset.Index) + "_t_max_c_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                t_max_c.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
                t_max_c = xr.open_dataarray(filestr).load()
            # =============================================================================
            # Nd - only this one uses daily
            # =============================================================================
            # Days per month over t_max_c
            with ProgressBar():
                log.info("Calc Nd")
                Nd = calc_Nd(t_out_ave, t_max_c, nyrs_clim)
                Nd.attrs = {
                    "name": "Nd",
                    "description": "This returns the days per month over t_max_c",
                    "units": "days/month",
                    "short name": "Days > t_max_c",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }
                Nd = Nd.to_dataset(name="Nd")
                encoding = {"Nd": config.comp}
                fname = suff + "_" + str(parset.Index) + "_Nd_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                Nd.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
                Nd = xr.open_dataarray(filestr)

            # =============================================================================
            # Nf - only this one uses daily
            # =============================================================================
            # Days per month over t_max_c
            with ProgressBar():
                log.info("Calc Nf")
                Nf = calc_Nf(t_out_ave, t_bal_c, nyrs_clim)
                Nf.attrs = {
                    "name": "Nf",
                    "description": "This returns the days per month above t_bal_c",
                    "units": "days/month",
                    "short name": "Days > t_bal_c",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }
                Nf = Nf.to_dataset(name="Nf")
                encoding = {"Nf": config.comp}
                fname = suff + "_" + str(parset.Index) + "_Nf_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                Nf.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
                Nf = xr.open_dataarray(filestr)

            # =============================================================================
            # vdd_tmax_c
            # =============================================================================
            # Days per month over t_max_c
            with ProgressBar():
                log.info("Calc_vdd_tmax_c")
                vdd_tmax_c = calc_vdd_tmax_c(t_oa_gbm, t_max_c)
                vdd_tmax_c = vdd_tmax_c.chunk(
                    chunks={"lon": get_paths(config, "chunk_size")}
                )
                vdd_tmax_c = (
                    vdd_tmax_c.groupby("time.month").sum("time") / nyrs_clim
                )  # <<< divide by years

                # old = xr.open_dataarray("D:\\mengm\\IIASA\\DLE - Data\\output_data_v19_ALPS2023\\GFDL-ESM4\\ssp585\\2_VDD_ene_calcs\\archive\\2015_new_0_vdd_tmax_c_urban.nc")
                vdd_tmax_c.attrs = {
                    "name": "vdd_tmax_c",
                    "description": "This returns the sum of variable cooling degree days per month based on Tmax",
                    "units": "degC",
                    "short name": "Var. cooling DD",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }
                vdd_tmax_c = vdd_tmax_c.to_dataset(name="vdd_tmax_c")
                encoding = {"vdd_tmax_c": config.comp}
                fname = suff + "_" + str(parset.Index) + "_vdd_tmax_c_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                vdd_tmax_c.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
                vdd_tmax_c = xr.open_dataarray(filestr)

            # =============================================================================
            # qctmax
            # =============================================================================
            # t_bal_c = xr.open_dataarray(
            #     os.path.join(
            #         output_path_vdd,
            #         suff + "_" + str(parset.Index) + "_t_bal_c_" + urt + ".nc",
            #     )
            # )
            with ProgressBar():
                log.info("Calc_qctmax")
                qctmax = Q_c_tmax(H_tr, H_v_cl, vdd_tmax_c, t_max_c, t_bal_c, Nd, f_c)
                qctmax.attrs = {
                    "name": "qctmax",
                    "description": "This returns the monthly cooling energy (MJ) based on variable degree days",
                    "units": "MJ/month",
                    "short name": "Sensible load",
                    "AC_hours": str(f_c),
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }

                qctmax = qctmax.to_dataset(name="qctmax")
                encoding = {"qctmax": config.comp}
                fname = suff + "_" + str(parset.Index) + "_qctmax_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                qctmax.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
            # t_bal_c.close()
            qctmax = xr.open_dataarray(filestr)

            # =============================================================================
            # E_c_ac electricity
            # =============================================================================
            # qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
            with ProgressBar():
                log.info("E_c AC")
                E_c_ac = calc_E_c_ac(qctmax, cop)
                E_c_ac.attrs = {
                    "name": "E_c_ac",
                    "description": "monthly electricity requirement for air conditioning - sensible (MJ)",
                    "units": "MJ/month",
                    "short name": "AC energy sens",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }

                E_c_ac = E_c_ac.to_dataset(name="E_c_ac")
                encoding = {"E_c_ac": config.comp}
                fname = suff + "_" + str(parset.Index) + "_E_c_ac_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                E_c_ac.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
            #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
            with ProgressBar():
                log.info("E_f fans")
                E_c_fan = calc_E_c_fan(
                    f_f, P_f, Nf, config.area_fan
                )  # Where Nf is same as Nd
                E_c_fan.attrs = {
                    "name": "E_c_fan",
                    "description": "monthly electricity requirement for fans (MJ)",
                    "units": "MJ/month",
                    "short name": "Fan energy",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }
                E_c_fan = E_c_fan.to_dataset(name="E_c_fan")
                encoding = {"E_c_fan": config.comp}
                fname = suff + "_" + str(parset.Index) + "_E_c_fan_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                E_c_fan.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
        # =============================================================================
        # dfa.to_csv(output_folder2+'constant_vars_out.csv')
        # log.info('Finished!: '+str(parset))
        # log.info(datetime.datetime.now()-start)
        # =============================================================================

        # ==============================================================================
        # HEATING CALCULATIONS
        # ==============================================================================

        if config.heat == 1:
            # ==============================================================================
            # Variable HDD functions
            # ==============================================================================
            with ProgressBar():
                log.info("calc_t_bal_h")
                t_bal_h = calc_t_bal_h(
                    t_sp_h, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_cl
                ).astype("float32")  # , x_diff0
                t_bal_h = t_bal_h.chunk(chunks={"lon": get_paths(config, "chunk_size")})
                log.info("chunked")
                t_bal_h.attrs = {
                    "name": "t_bal_h",
                    "description": "Balance (base) temperature",
                    "units": "degC",
                    "short name": "Balance temp.",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }
                t_bal_h = t_bal_h.to_dataset(name="t_bal_h")
                encoding = {"t_bal_h": config.comp}
                fname = suff + "_" + str(parset.Index) + "_t_bal_h_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                t_bal_h.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
                t_bal_h = xr.open_dataarray(filestr)

            # =============================================================================
            # vdd_h
            # =============================================================================

            with ProgressBar():
                log.info("calc_vdd_h")
                vdd_h = calc_vdd_h(t_oa_gbm, t_bal_h)
                vdd_h = vdd_h.chunk(chunks={"lon": get_paths(config, "chunk_size")})
                vdd_h = (
                    vdd_h.groupby("time.month").sum("time") / nyrs_clim
                )  # <<< divide by years
                vdd_h.attrs = {
                    "name": "vdd_h",
                    "description": "This returns the sum of variable heating degree days per month",
                    "units": "degC",
                    "short name": "Var. heating DD",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }
                vdd_h = vdd_h.to_dataset(name="vdd_h")
                encoding = {"vdd_h": config.comp}
                fname = suff + "_" + str(parset.Index) + "_vdd_h_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                vdd_h.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
                vdd_h = xr.open_dataarray(filestr)

            # =============================================================================
            # qh
            # =============================================================================
            # t_bal_h = xr.open_dataarray(
            #     os.path.join(
            #         output_path_vdd,
            #         suff + "_" + str(parset.Index) + "_t_bal_h_" + urt + ".nc",
            #     )
            # )
            with ProgressBar():
                log.info("Calc_qh")
                qh = Q_h(H_tr, H_v_cl, f_h, vdd_h)
                qh.attrs = {
                    "name": "qh",
                    "description": "This returns the monthly heating energy (MJ) based on variable degree days",
                    "units": "MJ/month",
                    "short name": "Sensible load",
                    "heating_hours": str(f_h),
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }

                qh = qh.to_dataset(name="qh")
                encoding = {"qh": config.comp}
                fname = suff + "_" + str(parset.Index) + "_qh_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                qh.to_netcdf(filestr, encoding=encoding)
                log.info("...Saved " + filestr)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
                qh = xr.open_dataarray(filestr)

            # =============================================================================
            # E_h final energy
            # =============================================================================
            # qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
            with ProgressBar():
                log.info("E_h")
                E_h = calc_E_h(qh, eff)
                E_h.attrs = {
                    "name": "E_h",
                    "description": "monthly final energy for heating (MJ)",
                    "units": "MJ/month",
                    "short name": "heating energy",
                    "urt": urt,
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }

                E_h = E_h.to_dataset(name="E_h")
                encoding = {"E_h": config.comp}
                fname = suff + "_" + str(parset.Index) + "_E_h_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                E_h.to_netcdf(filestr, encoding=encoding)
                if config.verbose:
                    log.info(datetime.datetime.now() - start_time)
                #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
            dfa.to_csv(
                os.path.join(output_path_vdd, suff + "_" + "constant_vars_out.csv")
            )
            # log.info('Finished!: '+suff+'+str(parset))
            # log.info('Finished!')
            log.info(datetime.datetime.now() - start_time)

    s_runs = load_all_scenarios_data(config).clim
    inputs = product(s_runs, vers_archs, par_var.itertuples(), config.urts)
    list(map(map_calculated_variables, inputs))

    # mypool = Pool(4)
    # return list(mypool.map(map_calculated_variables, inputs))


def aggregate_urban_rural_files(config: "Config"):
    project_path = get_paths(config, "project_path")
    out_path = os.path.join(project_path, "out", "version", config.vstr)
    save_path = os.path.join(out_path, "VDD_ene_calcs")

    output_path_vdd = os.path.join(
        save_path,
        config.gcm,
        config.rcp,
    )

    vers_archs = get_archs(config)
    par_var = load_parametric_analysis_data(config)
    runs_clims = load_all_scenarios_data(config).clim

    if not os.path.exists(output_path_vdd):
        os.makedirs(output_path_vdd)

    for clim in runs_clims:
        for arch in vers_archs:
            suff = str(clim) + "_" + arch  # suffix

            log.info("Aggregating results for " + suff)
            # Aggregate archetypes into same files.
            varlist = []

            if config.cool == 1:
                varlist.extend(
                    [
                        "t_bal_c",
                        "vdd_tmax_c",
                        "qctmax",
                        "Nd",
                        "Nf",
                        "E_c_ac",
                        "E_c_fan",
                        "t_max_c",
                    ]
                )

            if config.heat == 1:
                varlist.extend(["t_bal_h", "vdd_h", "qh", "E_h"])

            comp = {"zlib": True}

            for parset in par_var.itertuples():
                for var in varlist:
                    log.info("." + var)
                    mds = xr.open_mfdataset(
                        os.path.join(
                            output_path_vdd,
                            suff + "_" + str(parset.Index) + "_" + var + "_*.nc",
                        ),
                        concat_dim="urt",
                        combine="nested",
                    )  # +urt[0]
                    # mds['arch'] = al #arcs#[:4]
                    mds["urt"] = config.urts  # arcs#[:4]

                    mds.attrs = {
                        "title": mds[var].attrs["name"],
                        "authors": "Edward Byers & Alessio Mastrucci",
                        "date": str(datetime.datetime.now()),
                        "institution": "IIASA Energy Program",
                        "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
                        # 'archetypes': al,
                        "urb_rur": config.urts,
                        "name_run": parset.name_run,
                        "id_run": str(parset.Index),
                    }
                    encoding = {var: comp for var in mds.data_vars}
                    filestr = os.path.join(
                        output_path_vdd,
                        suff + "_" + str(parset.Index) + "_" + var + "ALL.nc",
                    )
                    mds.to_netcdf(filestr, encoding=encoding)
                    log.info("...Saved " + filestr)


def make_vdd_total_maps(config: "Config"):
    project_path = get_paths(config, "project_path")
    out_path = os.path.join(project_path, "out", "version", config.vstr)
    save_path = os.path.join(out_path, "VDD_ene_calcs")

    output_path_vdd = os.path.join(
        save_path,
        config.gcm,
        config.rcp,
    )

    vers_archs = get_archs(config)
    par_var = load_parametric_analysis_data(config)
    s_runs = load_all_scenarios_data(config)
    run_clims = s_runs.clim

    # TODO: (meas) the original code does not query for clims,
    # but without it the code will crash if not all years have been run
    # clims_int = list(map(int, run_clims))
    # log.info(clims_int)

    for s_run in s_runs.itertuples():
        for arch in vers_archs:
            for urt in config.urts:
                suff = str(s_run.clim) + "_" + arch  # suffix

                log.info("Running " + suff)

                def make_map(
                    data,
                    ax=None,
                    add_features=True,
                    cbar=True,
                    grid=False,
                    logNorm=False,
                    crs=ccrs.PlateCarree(),
                    plotargs={},
                ):
                    if ax is None:
                        fig, ax = plt.subplots(subplot_kw=dict(projection=crs))
                    elif not isinstance(ax, cartopy.mpl.geoaxes.GeoAxesSubplot):
                        msg = "Must provide a cartopy axes object, not: {}"
                        raise ValueError(msg.format(type(ax)))

                    #    ax = plt.axes(projection=ccrs.Robinson())
                    if add_features:
                        #        ax.add_feature(cartopy.feature.LAND, zorder=0)

                        ax.add_feature(
                            cartopy.feature.OCEAN, facecolor="#bde6ed", zorder=0
                        )
                        ax.add_feature(
                            cartopy.feature.LAND, facecolor=[0.8, 0.8, 0.8]
                        )  # , zorder=0)

                        ax.coastlines(linewidth=0.3, edgecolor="k")  # '#778899'
                        ax.add_feature(
                            cfeature.BORDERS, linewidth=0.3, edgecolor="#778899"
                        )

                    #        ax.add_feature(cartopy.feature.COASTLINE)
                    #        ax.add_feature(cartopy.feature.BORDERS)

                    if logNorm:
                        from matplotlib.colors import LogNorm  # type: ignore

                        plotargs["norm"] = LogNorm(
                            vmin=plotargs["vmin"], vmax=plotargs["vmax"]
                        )

                    if data is not None:
                        p = data.plot(
                            transform=ccrs.PlateCarree(),
                            ax=ax,
                            add_colorbar=cbar,
                            **plotargs,
                        )

                    if grid:
                        import matplotlib.ticker as mticker  # type: ignore
                        from cartopy.mpl.gridliner import (
                            LATITUDE_FORMATTER,
                            LONGITUDE_FORMATTER,
                        )

                        gl = ax.gridlines(
                            crs=crs,
                            draw_labels=True,
                            linewidth=1,
                            color="gray",
                            alpha=0.5,
                            linestyle="--",
                        )
                        gl.xlabels_top = False
                        gl.ylabels_left = False
                        gl.xlines = False
                        gl.xlocator = mticker.FixedLocator([-180, -90, 0, 90, 180])
                        gl.ylocator = mticker.FixedLocator(
                            [-90, -60, -30, 0, 30, 60, 90]
                        )
                        gl.xformatter = LONGITUDE_FORMATTER
                        gl.yformatter = LATITUDE_FORMATTER
                        gl.xlabel_style = {"size": 15, "color": "gray"}
                        gl.xlabel_style = {"color": "black"}  # , 'weight': 'bold'}

                    return p

                if config.cool == 1:
                    vdd_c_in = xr.open_dataarray(
                        os.path.join(
                            output_path_vdd, suff + "_0_vdd_tmax_c_" + urt + ".nc"
                        )
                    ).load()
                    vdd_c_year = vdd_c_in.sum(dim="month")
                    E_c_ac_in = xr.open_dataarray(
                        os.path.join(output_path_vdd, suff + "_0_E_c_ac_" + urt + ".nc")
                    ).load()
                    E_c_ac_year = (
                        E_c_ac_in.sum(dim="month") * 0.2777778
                    )  # Yearly total in kWh/m2

                if config.heat == 1:
                    vdd_h_in = xr.open_dataarray(
                        os.path.join(output_path_vdd, suff + "_0_vdd_h_" + urt + ".nc")
                    ).load()
                    vdd_h_year = vdd_h_in.sum(dim="month")
                    E_h_in = xr.open_dataarray(
                        os.path.join(output_path_vdd, suff + "_0_E_h_" + urt + ".nc")
                    ).load()
                    E_h_year = (
                        E_h_in.sum(dim="month") * 0.2777778
                    )  # Yearly total in kWh/m2

                cbloc = [
                    0.86,
                    0.185,
                    0.03,
                    0.61,
                ]  # [0.3, 0.07, 0.4, cbh] #color bar location
                cbor = "vertical"

                proj = ccrs.PlateCarree()
                rows = 1
                fw = "bold"
                tfs = 9
                cmap = "magma_r"

                # cb8 = plt.colorbar(im8, cax=cbaxes8, orientation=cbor, extend='max')
                # cb8.ax.tick_params(labelsize=tfs-2)
                # cbaxes8.text(-3,-0.12, u'people/km\u00b2', fontsize=10)

                poplog = False

                # Cooling Degree Days
                if config.cool == 1:
                    vmin = 0
                    vmax = 4000
                    plotargs = {"cmap": cmap, "vmin": vmin, "vmax": vmax}

                    dvar = eval("vdd_c_year")  # / larea
                    # dvar = dvar[urt]
                    fig = plt.figure(figsize=(6, 4))
                    ax8 = fig.add_subplot(rows, 1, 1, projection=proj)
                    im8 = make_map(
                        dvar.where(dvar > 0),
                        ax=ax8,
                        cbar=False,
                        grid=False,
                        logNorm=poplog,
                        plotargs=plotargs,
                    )
                    plt.title("Cooling Degree Days", fontsize=tfs, fontweight=fw)
                    cbaxes8 = fig.add_axes(cbloc)
                    cb8 = plt.colorbar(im8, cax=cbaxes8, orientation=cbor)
                    cb8.ax.tick_params(labelsize=tfs - 2)

                    # cbaxes8.text(-3,-0.12, u'VDD/year', fontsize=10)
                    plt.tight_layout()
                    fig.subplots_adjust(
                        left=0.05, right=0.85, top=0.95, bottom=0.03
                    )  # subplot margins
                    plt.savefig(
                        os.path.join(
                            output_path_vdd,
                            suff + "_VDD_c_" + cmap + str(poplog) + ".png",
                        ),
                        dpi=300,
                        bbox_inches="tight",
                    )

                # Heating Degree Days
                if config.heat == 1:
                    vmin = 0
                    vmax = 10000
                    plotargs = {"cmap": cmap, "vmin": vmin, "vmax": vmax}

                    dvar = eval("vdd_h_year")  # / larea
                    # dvar = dvar[urt]
                    fig = plt.figure(figsize=(6, 4))
                    ax8 = fig.add_subplot(rows, 1, 1, projection=proj)
                    im8 = make_map(
                        dvar.where(dvar > 0),
                        ax=ax8,
                        cbar=False,
                        grid=False,
                        logNorm=poplog,
                        plotargs=plotargs,
                    )
                    plt.title("Heating Degree Days", fontsize=tfs, fontweight=fw)
                    cbaxes8 = fig.add_axes(cbloc)
                    cb8 = plt.colorbar(im8, cax=cbaxes8, orientation=cbor)
                    cb8.ax.tick_params(labelsize=tfs - 2)

                    # cbaxes8.text(-3,-0.12, u'VDD/year', fontsize=10)
                    plt.tight_layout()
                    fig.subplots_adjust(
                        left=0.05, right=0.85, top=0.95, bottom=0.03
                    )  # subplot margins
                    plt.savefig(
                        os.path.join(
                            output_path_vdd,
                            suff + "_VDD_h_" + cmap + str(poplog) + ".png",
                        ),
                        dpi=300,
                        bbox_inches="tight",
                    )

                # Energy demand - Heating [kWh/m2]
                if config.heat == 1:
                    vmin = 0
                    vmax = 300
                    plotargs = {"cmap": cmap, "vmin": vmin, "vmax": vmax}

                    dvar = eval("E_h_year")  # / larea
                    # dvar = dvar[urt]
                    fig = plt.figure(figsize=(6, 4))
                    ax8 = fig.add_subplot(rows, 1, 1, projection=proj)
                    im8 = make_map(
                        dvar.where(dvar > 0),
                        ax=ax8,
                        cbar=False,
                        grid=False,
                        logNorm=poplog,
                        plotargs=plotargs,
                    )
                    plt.title("Energy demand - Heating", fontsize=tfs, fontweight=fw)
                    cbaxes8 = fig.add_axes(cbloc)
                    cb8 = plt.colorbar(im8, cax=cbaxes8, orientation=cbor)
                    cb8.ax.tick_params(labelsize=tfs - 2)

                    # cbaxes8.text(-3,-0.12, u'VDD/year', fontsize=10)
                    plt.tight_layout()
                    fig.subplots_adjust(
                        left=0.05, right=0.85, top=0.95, bottom=0.03
                    )  # subplot margins
                    plt.savefig(
                        os.path.join(
                            output_path_vdd,
                            suff + "_E_h_" + cmap + str(poplog) + ".png",
                        ),
                        dpi=300,
                        bbox_inches="tight",
                    )

                # Energy demand - Cooling - AC only [kWh/m2]
                if config.cool == 1:
                    vmin = 0
                    vmax = 100
                    plotargs = {"cmap": cmap, "vmin": vmin, "vmax": vmax}

                    dvar = eval("vdd_c_year")  # / larea
                    # dvar = dvar[urt]
                    fig = plt.figure(figsize=(6, 4))
                    ax8 = fig.add_subplot(rows, 1, 1, projection=proj)
                    im8 = make_map(
                        dvar.where(dvar > 0),
                        ax=ax8,
                        cbar=False,
                        grid=False,
                        logNorm=poplog,
                        plotargs=plotargs,
                    )
                    plt.title(
                        "Energy demand - Cooling (AC only)", fontsize=tfs, fontweight=fw
                    )
                    cbaxes8 = fig.add_axes(cbloc)
                    cb8 = plt.colorbar(im8, cax=cbaxes8, orientation=cbor)
                    cb8.ax.tick_params(labelsize=tfs - 2)

                    # cbaxes8.text(-3,-0.12, u'VDD/year', fontsize=10)
                    plt.tight_layout()
                    fig.subplots_adjust(
                        left=0.05, right=0.85, top=0.95, bottom=0.03
                    )  # subplot margins
                    plt.savefig(
                        os.path.join(
                            output_path_vdd,
                            suff + "_E_c_ac_" + cmap + str(poplog) + ".png",
                        ),
                        dpi=300,
                        bbox_inches="tight",
                    )


def process_construction_shares(config: "Config"):
    project_path = get_paths(config, "project_path")
    dle_path = get_paths(config, "dle_path")
    out_path = os.path.join(project_path, "out", "version", config.vstr)
    floorarea_path = os.path.join(out_path, "floorarea_country")

    output_path = os.path.join(
        floorarea_path,
        config.gcm,
        config.rcp,
    )

    if not os.path.exists(output_path):
        os.makedirs(output_path)

    vers_archs = get_archs(config)
    par_var = load_parametric_analysis_data(config)

    # get raster file and message map
    country_ras, reg_ras, map_reg, iso_attrs = create_message_raster(config)

    # If constr_setting == 1, then process construction shares. Otherwise, skip
    if config.constr_setting == 1:
        input_path = dle_path

        dsc = xr.Dataset()
        for urt in config.urts:
            # Read in excel files
            conshare = pd.read_csv(
                os.path.join(input_path, "constr_share_" + urt + ".csv")
            )
            tl = list(conshare.columns)
            arcs = [s for s in tl if s[0] == urt[0]]

            # build dummy dataset
            block = np.full((360, 720, 20), np.nan)
            lats = country_ras.lat
            lons = country_ras.lon
            coords = {"lat": lats, "lon": lons, "arch": arcs}
            ds = xr.DataArray(block, coords=coords, dims=["lat", "lon", "arch"])

            for row in iso_attrs.itertuples():
                conshare.loc[conshare.ISO == row.ISO, "regnum"] = row.Index

            # missing  AND, Andorra DMA, Dominica GRD,Grenada LIE,
            # liechenstein ATG, Antigua & barbuda KNA, Saint kitts SYC seychelles
            for arch in arcs:  # For each arch
                ai = arcs.index(arch)
                ta = ds.sel(arch=arch).values

                for row in iso_attrs.itertuples():  # For each country
                    ats = conshare.loc[conshare.ISO == row.ISO, :]

                    try:
                        ta[country_ras == int(row.Index)] = ats.loc[
                            ats.ISO == row.ISO, arch
                        ].values[0]
                    except IndexError:
                        log.info("map:" + row.ISO)
                ds.values[:, :, ai] = ta

            ds.attrs = {
                "title": "Construction shares by archetype",
                "urb_rur": urt,
                "authors": "Edward Byers & Alessio Mastrucci",
                "date": str(datetime.datetime.now()),
                "institution": "IIASA Energy Program",
                "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
            }
            ds = ds.to_dataset(name=urt)
            comp = {"zlib": True}
            encoding = {var: comp for var in ds.data_vars}
            ds.to_netcdf(
                os.path.join(output_path, "constr_share_" + urt + ".nc"),
                encoding=encoding,
            )
            dsc[urt] = ds[urt]

        dsc.attrs = {
            "title": "Construction share by archetype",
            "authors": "Edward Byers & Alessio Mastrucci",
            "date": str(datetime.datetime.now()),
            "institution": "IIASA Energy Program",
            "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
        }

        encoding = {var: comp for var in dsc.data_vars}
        dsc.to_netcdf(
            os.path.join(output_path, "constr_share_urbanrural.nc"), encoding=encoding
        )
        log.info("Completed construction share maps")
    else:
        log.info("Skipping construction share maps because constr_setting != 1")


def process_floor_area_maps(config: "Config"):
    project_path = get_paths(config, "project_path")
    dle_path = get_paths(config, "dle_path")
    input_path = dle_path
    out_path = os.path.join(project_path, "out", "version", config.vstr)
    save_path = os.path.join(out_path, "floorarea_country")

    output_path = os.path.join(
        save_path,
        config.gcm,
        config.rcp,
    )

    if not os.path.exists(output_path):
        os.makedirs(output_path)

    vers_archs = get_archs(config)
    par_var = load_parametric_analysis_data(config)
    country_ras, reg_ras, map_reg, iso_attrs = create_message_raster(config)
    s_runs = load_all_scenarios_data(config)

    if config.floor_setting == "std_cap":
        floorarea = pd.read_csv(
            os.path.join(input_path, "floor_std_cap.csv")
        )  # STD conditioned floor area (same for all regions, based on DLE)

        floormap = xr.Dataset(
            {
                "urban": reg_ras.MESSAGE11.copy(deep=True),
                "rural": reg_ras.MESSAGE11.copy(deep=True),
            }
        )

        for row in floorarea.itertuples():
            floormap["urban"].values[floormap.urban == row.RegNum] = float(row.urban)
            floormap["rural"].values[floormap.rural == row.RegNum] = float(row.rural)
            # floormap['urban'].values[floormap.urban==row.RegNum] = getattr(row,'urban'+suff)
            # floormap['rural'].values[floormap.rural==row.RegNum] = getattr(row,'rural'+suff)

        floormap = floormap.astype(float)
        for urt in config.urts:
            floormap[urt].values[floormap[urt] == -1] = np.nan

        plt.figure()
        floormap.urban.plot()
        plt.figure()
        floormap.rural.plot()

        # % Write out to netcdf
        floormap.attrs = {
            "title": "Floor area by region",
            "authors": "Edward Byers & Alessio Mastrucci",
            "date": str(datetime.datetime.now()),
            "institution": "IIASA Energy Program",
            "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
            "floor_setting": config.floor_setting,
        }
        comp = {"zlib": True}
        encoding = {var: comp for var in floormap.data_vars}
        floormap.to_netcdf(
            os.path.join(output_path, "floor_area_map_std_cap.nc"), encoding=encoding
        )
        log.info("Completed floor area maps")

    elif config.floor_setting == "per_cap":
        for s_run in s_runs.itertuples():
            suff = (
                str(s_run.scen) + "_" + str(s_run.year) + "_" + str(s_run.clim)
            )  # suffix
            suff2 = (
                str(s_run.scen) + "_" + str(s_run.year)
            )  # suffix: only scen and year

            floorarea = pd.read_excel(
                os.path.join(input_path, "floor_per_cap_" + config.vstrcntry + ".xlsx"),
                sheet_name=suff2,
            )

            floormap = xr.Dataset(
                {
                    "urban": reg_ras.MESSAGE11.copy(deep=True),
                    "rural": reg_ras.MESSAGE11.copy(deep=True),
                }
            )

            for row in floorarea.itertuples():
                floormap["urban"].values[floormap.urban == row.RegNum] = float(
                    row.urban
                )
                floormap["rural"].values[floormap.rural == row.RegNum] = float(
                    row.rural
                )
                # floormap['urban'].values[floormap.urban==row.RegNum] = getattr(row,'urban'+suff)
                # floormap['rural'].values[floormap.rural==row.RegNum] = getattr(row,'rural'+suff)

            floormap = floormap.astype(float)
            for urt in config.urts:
                floormap[urt].values[floormap[urt] == -1] = np.nan

            plt.figure()
            floormap.urban.plot()
            plt.figure()
            floormap.rural.plot()

            # % Write out to netcdf

            floormap.attrs = {
                "title": "Floor area by region",
                "authors": "Edward Byers & Alessio Mastrucci",
                "date": str(datetime.datetime.now()),
                "institution": "IIASA Energy Program",
                "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
                "floor_setting": config.floor_setting,
            }
            comp = {"zlib": True}
            encoding = {var: comp for var in floormap.data_vars}
            floormap.to_netcdf(
                os.path.join(
                    output_path,
                    "floor_area_map_" + config.floor_setting + "_" + suff + ".nc",
                ),
                encoding=encoding,
            )

            log.info("Completed floor area maps")

    else:
        log.info("Skipping floor area maps because floor_setting != std_cap or per_cap")


def process_country_maps(config: "Config"):
    project_path = get_paths(config, "project_path")
    dle_path = get_paths(config, "dle_path")

    input_path = dle_path
    out_path = os.path.join(project_path, "out", "version", config.vstr)
    save_path = os.path.join(out_path, "floorarea_country")

    output_path = os.path.join(
        save_path,
        config.gcm,
        config.rcp,
    )

    if not os.path.exists(output_path):
        os.makedirs(output_path)

    vers_archs = get_archs(config)
    par_var = load_parametric_analysis_data(config)
    s_runs = load_all_scenarios_data(config)
    country_ras, reg_ras, map_reg, iso_attrs = create_message_raster(config)

    for s_run in s_runs.itertuples():
        suff = str(s_run.scen) + "_" + str(s_run.year) + "_" + str(s_run.clim)  # suffix
        str(s_run.scen) + "_" + str(s_run.year)  # suffix: only scen and year

        # Read in country_data for AC penetration, electricity access, slumpop
        # country_data = pd.read_excel(input_folder+'country_data_'+vstrcntry+'.xlsx', sheet_name = 'suff2')
        country_data = pd.read_excel(
            os.path.join(input_path, "country_data_" + config.vstrcntry + ".xlsx"),
            sheet_name="ssp2_2010",
        )  # hack to use same sheet every time

        # Column names
        # cols = ['AC_penetr','access_elec','slum_pop']
        cols = [
            "AC_penetr",
            "AC_penetr_U",
            "AC_penetr_R",
            "access_elec",
            "slum_pop",
            "GLOBAL_SOUTH",
        ]
        # cols = ['AC_penetr'+suff,'AC_penetr_U'+suff,'AC_penetr_R'+suff,'access_elec'+suff,'slum_pop'+suff,'GLOBAL_SOUTH']

        # Match the GAUL country numbers to the ISOs in country_data table (just in case)
        iso_attrs["GAULi"] = iso_attrs.index.astype(float)
        country_data = country_data.merge(iso_attrs, on="ISO", how="outer")

        # create dataset four country data
        cd_map = xr.Dataset({cols[0]: country_ras.copy(deep=True).astype("float")})
        for col in cols[1:]:
            cd_map[col] = country_ras.copy(deep=True).astype("float")

        # Populate the dataset with country data
        for col in cols:
            for idx in iso_attrs.GAULi:
                cd_map[col].values[cd_map[col] == idx] = country_data.loc[
                    country_data.GAULi == idx, col
                ]

            plt.figure()
            cd_map[col].plot()
            plt.close()

        # Write out to netcdf
        cd_map.attrs = {
            "title": "Maps of country data",
            "authors": "Edward Byers & Alessio Mastrucci",
            "date": str(datetime.datetime.now()),
            "institution": "IIASA Energy Program",
            "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
            "variables": cols,
        }

        comp = {"zlib": True, "complevel": 5}
        encoding = {var: comp for var in cd_map.data_vars}
        cd_map.to_netcdf(
            os.path.join(output_path, "country_data_maps_" + suff + ".nc"),
            encoding=encoding,
        )

    log.info("Finished country data maps")


def process_final_maps(config: "Config"):
    project_path = get_paths(config, "project_path")
    dle_path = get_paths(config, "dle_path")

    input_path = dle_path
    out_path = os.path.join(project_path, "out", "version", config.vstr)
    vdd_path = os.path.join(
        out_path,
        "VDD_ene_calcs",
        config.gcm,
        config.rcp,
    )
    floorarea_path = os.path.join(
        out_path,
        "floorarea_country",
        config.gcm,
        config.rcp,
    )
    finalmaps_path = os.path.join(out_path, "final_maps", config.gcm, config.rcp)

    if not os.path.exists(finalmaps_path):
        os.makedirs(finalmaps_path)

    vers_archs = get_archs(config)
    par_var = load_parametric_analysis_data(config)
    s_runs = load_all_scenarios_data(config)
    # country_ras, reg_ras, map_reg, iso_attrs = create_message_raster(config)

    # country_maps_path = os.path.join(
    #     input_dle_path,
    #     f"output_data_{input_version_name}",
    #     input_gcm,
    #     input_rcp_scenario,
    #     "3_floorarea_country_data",
    # )

    # ene_calcs_path = os.path.join(
    #     input_dle_path,
    #     f"output_data_{input_version_name}",
    #     input_gcm,
    #     input_rcp_scenario,
    #     "2_VDD_ene_calcs",
    # )

    # final_maps_path = os.path.join(
    #     input_dle_path,
    #     f"output_data_{input_version_name}",
    #     input_gcm,
    #     input_rcp_scenario,
    #     "4_final_maps",
    # )

    vardic = {}
    varundic = {}

    # Update dictionaries if config.cool == 1
    if config.cool == 1:
        vardic.update(VARDICT_COOL)
        varundic.update(VARUNDICT_COOL)
    # Update dictionaries if config.heat == 1
    if config.heat == 1:
        vardic.update(VARDICT_HEAT)
        varundic.update(VARUNDICT_HEAT)

    [key for key in varundic.keys()]

    if (
        config.paranalysis_mode == 0
    ):  # If running in ref mode, keep only the ref parameter set
        par_var = par_var.loc[par_var.name_run == "ref", :]

    # TODO: (meas) the original code does not query for clims,
    # but without it the code will crash if not all years have been run
    # clims_int = list(map(int, config.clims))
    # log.info("Years of data available: " + str(clims_int))
    # s_runs = s_runs.query("clim in @clims_int")

    for s_run in s_runs.itertuples():
        for arch in vers_archs:
            suff = (
                str(s_run.scen)
                + "_"
                + str(s_run.year)
                + "_"
                + str(s_run.clim)
                + "_"
                + arch
            )  # suffix
            suff1 = str(s_run.clim) + "_" + arch  # suffix
            if config.popfix is True:
                suff2 = "ssp2_" + str(s_run.year)
            else:
                suff2 = (
                    str(s_run.scen) + "_" + str(s_run.year)
                )  # suffix: only scen and year
            suff3 = (
                str(s_run.scen) + "_" + str(s_run.year) + "_" + str(s_run.clim)
            )  # suffix: scen, year and arch

            log.info("Starting " + suff)

            # Load country data
            country_data = xr.open_dataset(
                os.path.join(floorarea_path, "country_data_maps_" + suff3 + ".nc")
            )

            # Load population data
            popdata = xr.Dataset()
            popdata["urban"] = xr.open_dataarray(
                os.path.join(
                    input_path, "population", "population", suff2 + "_urban_hd.nc4"
                )
            )
            popdata["rural"] = xr.open_dataarray(
                os.path.join(
                    input_path, "population", "population", suff2 + "_rural_hd.nc4"
                )
            )
            popdata["total"] = xr.open_dataarray(
                os.path.join(
                    input_path, "population", "population", suff2 + "_total_hd.nc4"
                )
            )

            # Floor area data
            if config.floor_setting == "std_cap":
                floorarea = xr.open_dataset(
                    os.path.join(
                        floorarea_path,
                        "floor_area_map_" + config.floor_setting + ".nc",
                    )
                )
            elif config.floor_setting == "per_cap":
                floorarea = xr.open_dataset(
                    os.path.join(
                        floorarea_path,
                        "floor_area_map_" + config.floor_setting + "_" + suff3 + ".nc",
                    )
                )

            # Construction shares
            # constr = xr.open_dataset(input_folder+'map_constr_floorarea\\constr_share_urbanrural.nc')

            # =============================================================================
            # First apply population weighting to energy demand:
            # =============================================================================

            for parset in par_var.itertuples():
                if config.cool == 1:
                    Nd1 = xr.open_dataarray(
                        os.path.join(
                            vdd_path,
                            suff1 + "_" + str(parset.Index) + "_NdALL.nc",
                        )
                    ).load()  # For calculation
                    # Nd = Nd.sum(dim=['arch','month'])/2    #<<< divide by 2 because 2 archetypes
                    Nd1 = (
                        Nd1.sum(dim=["urt", "month"]) / 2
                    )  # <<< divide by 2 (rural + urban)

                    Ndr = xr.open_dataarray(
                        os.path.join(
                            vdd_path,
                            suff1 + "_" + str(parset.Index) + "_NdALL.nc",
                        )
                    ).load()  # For reporting
                    # Nd = Nd.sum(dim=['arch','month'])/2    #<<< divide by 2 because 2 archetypes
                    # Nd = Nd.sum(dim=['month'])    #<<< divide by 2 (rural + urban)

                    Nfr = xr.open_dataarray(
                        os.path.join(
                            vdd_path,
                            suff1 + "_" + str(parset.Index) + "_NfALL.nc",
                        )
                    ).load()
                    # Nf = Nf.sum(dim=['arch','month'])/2    #<<< divide by 2 because 2 archetypes
                    # Nf = Nf.sum(dim=['month'])  #<<< divide by 2 (rural + urban)

                    E_c_ac = (
                        xr.open_dataarray(
                            os.path.join(
                                vdd_path,
                                suff1 + "_" + str(parset.Index) + "_E_c_ac" + "ALL.nc",
                            )
                        ).where(Nd1 >= config.nd_thresh)
                    ).load()
                    E_c_fan = (
                        xr.open_dataarray(
                            os.path.join(
                                vdd_path,
                                suff1 + "_" + str(parset.Index) + "_E_c_fan" + "ALL.nc",
                            )
                        ).where(Nd1 >= config.nd_thresh)
                    ).load()
                    E_c = E_c_ac + E_c_fan
                    vdd_c_in = xr.open_dataarray(
                        os.path.join(
                            vdd_path,
                            suff1 + "_" + str(parset.Index) + "_vdd_tmax_cALL.nc",
                        )
                    ).load()

                if config.heat == 1:
                    E_h = (
                        xr.open_dataarray(
                            os.path.join(
                                vdd_path,
                                suff1 + "_" + str(parset.Index) + "_E_h" + "ALL.nc",
                            )
                        )
                    ).load()
                    vdd_h_in = xr.open_dataarray(
                        os.path.join(
                            vdd_path,
                            suff1 + "_" + str(parset.Index) + "_vdd_hALL.nc",
                        )
                    ).load()
                #    vdd_in = vdd_in.sum(dim='arch')

                #    if (runsdd==1) and (parset.Index==0):
                #        for bal_temp in bal_temps:
                #            sdd_c_in = xr.open_dataarray(input_folder2+str(parset.Index)+'_s_cddALL.nc').load()
                #            sdd_h_in = xr.open_dataarray(input_folder2+str(parset.Index)+'_s_hddALL.nc').load()
                #        log.info(xkss)

                if config.cool == 1:
                    # Cooling outputs
                    E_c_perpix = (
                        xr.Dataset()
                    )  # Pixel / person per gridsquare energy demand
                    E_c_ac_popwei = (
                        xr.Dataset()
                    )  # Total Energy, population weighted, per pixel
                    E_c_fan_popwei = xr.Dataset()
                    E_c_popwei = xr.Dataset()
                    E_c_ac_wAccess = xr.Dataset()
                    E_c_ac_gap = xr.Dataset()
                    E_c_fan_wAccess = xr.Dataset()
                    E_c_fan_gap = xr.Dataset()
                    E_c_wAccess = xr.Dataset()
                    E_c_gap = xr.Dataset()
                    Nf = xr.Dataset()
                    Nd = xr.Dataset()
                    vdd_c_popwei = xr.Dataset()  # Degree Days multiplied by population
                    #    sdd_c = xr.Dataset()

                    P_c_ac_potential = xr.Dataset()
                    P_c_ac_gap = xr.Dataset()
                    P_c_fan_gap = xr.Dataset()
                    P_c_fanNoAC = xr.Dataset()

                if config.heat == 1:
                    # Heating outputs
                    E_h_perpix = (
                        xr.Dataset()
                    )  # Pixel / person per gridsquare energy demand
                    E_h_popwei = xr.Dataset()  # population weighted (FULL demand)

                    P_h_potential = xr.Dataset()

                    vdd_h_popwei = xr.Dataset()  # Degree Days multiplied by population
                #    sdd_h = xr.Dataset()

                # % Produce spatial results
                for urt in config.urts:
                    if config.cool == 1:
                        if urt == "urban":
                            # at = au
                            acp = "AC_penetr_U"
                        elif urt == "rural":
                            # at = ar
                            acp = "AC_penetr_R"

                        #        log.info(sddddd)
                        E_c_perpix[urt] = (
                            floorarea[urt] * E_c["urt" == urt]
                        )  # energy  per person per pixel
                        E_c_ac_popwei[urt] = (
                            popdata[urt] * floorarea[urt] * E_c_ac["urt" == urt]
                        )  # Total Energy, population weighted, per pixel
                        E_c_fan_popwei[urt] = (
                            popdata[urt] * floorarea[urt] * E_c_fan["urt" == urt]
                        )  # Total Energy, population weighted, per pixel
                        E_c_popwei[urt] = (
                            popdata[urt] * E_c_perpix[urt]
                        )  # Total Energy, population weighted, per pixel
                        # E_c_pwpercap = E_c_popwei[urt] / popdata[urt]

                        E_c_ac_wAccess[urt] = (
                            E_c_ac_popwei[urt] * country_data[acp]
                        )  # AC energy incl AC penetre
                        E_c_ac_gap[urt] = E_c_ac_popwei[urt] * (
                            1 - country_data[acp]
                        )  # AC energy gap

                        E_c_fan_wAccess[urt] = (
                            E_c_fan_popwei[urt] * country_data["access_elec"]
                        )  # Fan energy incl elec access
                        E_c_fan_gap[urt] = E_c_fan_popwei[urt] * (
                            1 - country_data["access_elec"]
                        )  # Fan energy gap

                        E_c_wAccess[urt] = (
                            E_c_ac_wAccess[urt] + E_c_fan_wAccess[urt]
                        )  # << Current energy usage
                        E_c_gap[urt] = (
                            E_c_ac_gap[urt] + E_c_fan_gap[urt]
                        )  # Total energy gap

                        Nd[urt] = Ndr[
                            "urt" == urt
                        ]  # .sum(dim='month') # summed later over months
                        Nf[urt] = Nfr["urt" == urt]

                        vdd_c_popwei[urt] = (
                            popdata[urt] * vdd_c_in["urt" == urt]
                        )  # Degree Days multiplied by population
                        #        if (runsdd==1) and (parset.Index==0):
                        #            sdd_c[urt] =  sdd_c_in # popdata[urt] *

                        # Population that needs access to AC (AC market)
                        P_c_ac_potential[urt] = popdata[urt].where(
                            E_c_ac["urt" == urt].sum(dim="month") > 0
                        )
                        # Population (unsatisfied) that needs access to AC
                        P_c_ac_gap[urt] = popdata[urt].where(
                            E_c_ac["urt" == urt].sum(dim="month") > 0
                        ) * (1 - country_data[acp])
                        P_c_fan_gap[urt] = popdata[urt].where(
                            E_c_fan["urt" == urt].sum(dim="month") > 0
                        ) * (1 - country_data["access_elec"])
                        P_c_fanNoAC[urt] = P_c_ac_gap[urt] - P_c_fan_gap[urt]

                    if config.heat == 1:
                        # Heating results
                        E_h_perpix[urt] = (
                            floorarea[urt] * E_h["urt" == urt]
                        )  # energy  per person per pixel
                        E_h_popwei[urt] = (
                            popdata[urt] * floorarea[urt] * E_h["urt" == urt]
                        )  # Total Energy, population weighted, per pixel

                        P_h_potential[urt] = popdata[urt].where(
                            E_h["urt" == urt].sum(dim="month") > 0
                        )  # Population that needs access to heating

                        vdd_h_popwei[urt] = (
                            popdata[urt] * vdd_h_in["urt" == urt]
                        )  # Degree Days multiplied by population
                #        if (runsdd==1) and (parset.Index==0):
                #            sdd_h[urt] =  sdd_h_in # popdata[urt] *

                for var in vardic.keys():
                    #        if (runsdd==1) and (var=='sdd_c' or var =='sdd_h') and (parset.Index>0):
                    #            log.info('do nothing')
                    #        else:
                    ds = eval(var)
                    # ds['arch'] = al
                    ds["urt"] = config.urts
                    ds.attrs = {
                        "title": var,
                        "description": vardic[var],
                        "units": varundic[var],
                        "authors": "Edward Byers & Alessio Mastrucci",
                        "date": str(datetime.datetime.now()),
                        "institution": "IIASA Energy Program",
                        "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
                        "name_run": parset.name_run,
                        "id_run": str(parset.Index),
                    }

                    #            if (runsdd==1) and (var=='sdd_c' or var=='sdd_h'):
                    #                ds.attrs['bal_temps'] = str(bal_temps)
                    #            else:
                    # ds.attrs['archetypes']  = al

                    encoding = {vari: config.comp for vari in ds.data_vars}
                    fname = suff + "_" + str(parset.Index) + "_" + var + ".nc"
                    filestr = os.path.join(finalmaps_path, fname)
                    ds.to_netcdf(filestr, encoding=encoding)

                    if varundic[var][-5:] == "month":
                        dsy = ds.sum(dim="month")
                        for urt in config.urts:
                            #                    if (runsdd==1) and (var == 'sdd_c' or var=='sdd_h'):
                            #                        dsy[urt] = (['bal_temp', 'lat','lon',], dsy[urt].values) #.sum(dim='arch')
                            #                    else:
                            dsy[urt] = (
                                ["lat", "lon"],
                                dsy[urt].values,
                            )  # .sum(dim='arch')

                        dsy.attrs = ds.attrs
                        dsy.attrs["units"] = varundic[var][:-5] + "year"
                        encoding = {vari: config.comp for vari in dsy.data_vars}
                        fname = suff + "_" + str(parset.Index) + "_" + var + "_year.nc"
                        filestr = os.path.join(finalmaps_path, fname)
                        dsy.to_netcdf(filestr, encoding=encoding)


def process_iso_tables(config: "Config"):
    start = datetime.datetime.now()

    project_path = get_paths(config, "project_path")
    dle_path = get_paths(config, "dle_path")

    input_path = dle_path
    out_path = os.path.join(project_path, "out", "version", config.vstr)
    vdd_path = os.path.join(out_path, "VDD_ene_calcs", config.gcm, config.rcp)
    floorarea_path = os.path.join(out_path, "floorarea_country", config.gcm, config.rcp)
    finalmaps_path = os.path.join(out_path, "final_maps", config.gcm, config.rcp)
    iso_path = os.path.join(out_path, "iso_tables", config.gcm, config.rcp)

    if not os.path.exists(iso_path):
        os.makedirs(iso_path)

    vers_archs = get_archs(config)
    par_var = load_parametric_analysis_data(config)
    s_runs = load_all_scenarios_data(config)
    # ras, map_reg, iso_attrs = create_message_raster(config)

    updated_urts = config.urts + ["total"]

    # final_maps_path = os.path.join(
    #     input_dle_path,
    #     f"output_data_{input_version_name}",
    #     input_gcm,
    #     input_rcp_scenario,
    #     "4_final_maps",
    # )

    # iso_tables_path = os.path.join(
    #     input_dle_path,
    #     f"output_data_{input_version_name}",
    #     input_gcm,
    #     input_rcp_scenario,
    #     "5_ISO_tables",
    # )

    vardic = {}
    varundic = {}

    # Update dictionaries if config.cool == 1
    if config.cool == 1:
        vardic.update(VARDICT_COOL)
        varundic.update(VARUNDICT_COOL)
    # Update dictionaries if config.heat == 1
    if config.heat == 1:
        vardic.update(VARDICT_HEAT)
        varundic.update(VARUNDICT_HEAT)

    varlist_cool = [key for key in VARDICT_COOL.keys()]
    varlist_heat = [key for key in VARDICT_HEAT.keys()]

    # TODO: (meas) the original code does not query for clims,
    # but without it the code will crash if not all years have been run
    # clims_int = list(map(int, config.clims))
    # log.info("Years of data available: " + str(clims_int))
    # s_runs = s_runs.query("clim in @clims_int")

    # Read raster data
    raster = xr.open_dataarray(os.path.join(input_path, "gaul_lvl0_hybrid_05_3.nc"))

    # Read country data
    dfd = pd.read_csv(
        os.path.join(input_path, "GAUL_lvl0_raster0.5.csv"), index_col="ID"
    ).assign(ISONUM=lambda x: x.index)

    # Import MESSAGE regions and North/South classification
    msgNS = pd.read_excel(
        os.path.join(input_path, "country_data_" + config.vstrcntry + ".xlsx"),
        sheet_name="ssp2_2010",
    )

    # Add 'GLOBAL_SOUTH' and 'REGION_GEA' to dfd
    dfd = dfd.merge(
        msgNS.reindex(columns=["ISO", "GLOBAL_SOUTH", "REGION_GEA"]),
        left_on="ISO3",
        right_on="ISO",
    ).set_index("ISONUM")

    # Load population data
    log.info("Opening population data....")
    l_popdata = []
    for s_run in s_runs.itertuples():
        suff = str(s_run.scen) + "_" + str(s_run.year)
        log.info(suff)

        popdata = xr.Dataset()
        for urt in updated_urts:
            popdata[urt] = xr.open_dataarray(
                os.path.join(
                    input_path,
                    "population",
                    "population",
                    suff + "_" + urt + "_hd.nc4",
                )
            )

        agg_popdata = (
            popdata.groupby(raster)
            .sum()
            .to_dataframe()
            .reset_index()
            .melt(id_vars="gaul_lvl0", var_name="urt", value_name="popsum")
            .assign(population_scenario=s_run.scen, year=s_run.year)
        )

        l_popdata.append(agg_popdata)

    pop_agg = (
        pd.concat(l_popdata)
        .reset_index(drop=True)
        .assign(year=lambda x: x.year.astype(int))
    )

    # map s_runs, archs, updated_urts, and par_var to a tuple
    # then map the tuple to the process_data function
    # then convert the map object to a list
    inputs_cool = product(
        s_runs.itertuples(),
        vers_archs,
        updated_urts,
        par_var.itertuples(),
        varlist_cool,
    )
    inputs_heat = product(
        s_runs.itertuples(),
        vers_archs,
        updated_urts,
        par_var.itertuples(),
        varlist_heat,
    )

    def aggregate_ncfile(args: tuple) -> xr.Dataset:
        s_run, arch, urt, parset, varname = args
        str_varname = str(varname)

        log.info(
            f"Opening {varname} data for: {s_run.scen}_{s_run.year}_{s_run.clim}_{arch}_{urt}_{parset.Index}"
        )
        suff = (
            str(s_run.scen) + "_" + str(s_run.year) + "_" + str(s_run.clim) + "_" + arch
        )

        # if urt is "urban" or "rural" then load the data from the file
        # otherwise if urt is "total" then add the data from the "urban" and "rural" files
        if (urt == "urban") or (urt == "rural"):
            varname = xr.open_dataset(
                os.path.join(
                    finalmaps_path,
                    suff + "_" + str(parset.Index) + "_" + varname + ".nc",
                )
            )[urt]

        elif urt == "total":
            varname = (
                xr.open_dataset(
                    os.path.join(
                        finalmaps_path,
                        suff + "_" + str(parset.Index) + "_" + varname + ".nc",
                    )
                )["urban"]
                + xr.open_dataset(
                    os.path.join(
                        finalmaps_path,
                        suff + "_" + str(parset.Index) + "_" + varname + ".nc",
                    )
                )["rural"]
            )

        # If varname is Nd or Nf, then take the mean. Otherwise, take the sum
        if str_varname in ["Nd", "Nf"]:
            # Group varname by raster index and take the mean
            log.info("...Aggregating data by raster")
            agg_ras_month = (
                varname.groupby(raster).mean().to_dataframe(name="value").reset_index()
            )

            # Group by gaul_lvl0 and take the mean
            log.info(".....Aggregating data by gaul_lvl0")
            agg_gaul_lvl0 = (
                agg_ras_month.groupby("gaul_lvl0")["value"]
                .agg(lambda x: x.mean() / 2)
                .reset_index()
            )
        else:
            # Group varname by raster index and sum
            log.info("...Aggregating data by raster")
            agg_ras_month = (
                varname.groupby(raster).sum().to_dataframe(name="value").reset_index()
            )

            # Group by gaul_lvl0 and sum
            log.info(".....Aggregating data by gaul_lvl0")
            agg_gaul_lvl0 = (
                agg_ras_month.groupby("gaul_lvl0")["value"].sum().reset_index()
            )

        # Add columns for:
        # - s_run.scen
        # - s_run.year
        # - s_run.clim
        # - arch
        # - urt
        # - varname
        df = agg_gaul_lvl0.assign(
            gcm=str(config.gcm),
            scenario=str(config.rcp),
            scen=str(s_run.scen),
            year=str(s_run.year),
            clim=str(s_run.clim),
            arch=str(arch),
            urt=str(urt),
            par_var=str(parset.Index),
            name_run=str(parset.name_run),
            varname=str_varname,
        )

        return df

    if config.cool == 1:
        list_cool = list(map(aggregate_ncfile, inputs_cool))
        df_cool = (
            pd.concat(list_cool)
            .reset_index(drop=True)
            .merge(dfd, left_on="gaul_lvl0", right_on="ISONUM")
        )
    if config.heat == 1:
        list_heat = list(map(aggregate_ncfile, inputs_heat))
        df_heat = (
            pd.concat(list_heat)
            .reset_index(drop=True)
            .merge(dfd, left_on="gaul_lvl0", right_on="ISONUM")
        )

    # if only cool or only heat is selected, df_agg = df_cool or df_heat
    # if both cool and heat are selected, df_agg = df_cool + df_heat (concatenated)
    if (config.cool == 1) and (config.heat == 0):
        df_agg = df_cool
    elif (config.cool == 0) and (config.heat == 1):
        df_agg = df_heat
    elif (config.cool == 1) and (config.heat == 1):
        df_agg = pd.concat([df_cool, df_heat], ignore_index=True)

    log.info("Completed aggregating raster data! Now processing and saving...")

    # Merge df_agg with pop_agg
    df_agg = df_agg.assign(year=lambda x: x.year.astype(int)).merge(
        pop_agg,
        left_on=["gaul_lvl0", "urt", "year"],
        right_on=["gaul_lvl0", "urt", "year"],
    )

    # Convert from long to wide on varname
    df_agg_wide = df_agg.pivot_table(
        index=[
            "gaul_lvl0",
            "ADM0_CODE",
            "ADM0_NAME",
            "CONTINENT",
            "FAO_CODE",
            "ISO3",
            "UN_CODE",
            "UN_REGION",
            "ISO",
            "GLOBAL_SOUTH",
            "REGION_GEA",
            "gcm",
            "scenario",
            "scen",
            "population_scenario",
            "year",
            "clim",
            "arch",
            "urt",
            "par_var",
            "name_run",
            "popsum",
        ],
        columns="varname",
        values="value",
    ).reset_index()

    # Calculate population-averaged degree days
    if config.cool == 1:
        df_agg_wide = df_agg_wide.assign(vdd_c_avg=lambda x: x.vdd_c_popwei / x.popsum)
    if config.heat == 1:
        df_agg_wide = df_agg_wide.assign(vdd_h_avg=lambda x: x.vdd_h_popwei / x.popsum)

    # Drop and rename columns
    df_agg_wide = df_agg_wide.drop(
        columns=["ADM0_CODE", "CONTINENT", "FAO_CODE", "ISO3", "UN_CODE", "UN_REGION"]
    ).rename(columns={"gaul_lvl0": "id", "ADM0_NAME": "NAME"})

    # Save to excel and csv
    df_agg_wide.to_excel(
        os.path.join(
            iso_path,
            "ISO_agg_data_" + config.vstr + ".xlsx",
        )
    )
    df_agg_wide.to_csv(
        os.path.join(
            iso_path,
            "ISO_agg_data_" + config.vstr + ".csv",
        ),
        index=False,
    )

    end = datetime.datetime.now()
    log.info(
        "Done! Total time to aggregate variables and process ISO tables: "
        + str(end - start)
    )


def create_climate_outputs(config: "Config", start_time: datetime.datetime):
    project_path = get_paths(config, "project_path")
    dle_path = get_paths(config, "dle_path")
    isimip_bias_adj_path = get_paths(config, "isimip_bias_adj_path")
    isimip_ewemib_path = get_paths(config, "isimip_ewemib_path")
    out_path = os.path.join(project_path, "out", "version", config.vstr)
    archetype_path = os.path.join(out_path, "rasters")
    save_path = os.path.join(out_path, "VDD_ene_calcs")

    output_path_vdd = os.path.join(
        save_path,
        config.gcm,
        config.rcp,
    )

    if not os.path.exists(output_path_vdd):
        os.makedirs(output_path_vdd)

    vers_archs = get_archs(config)
    par_var = load_parametric_analysis_data(config)

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

    for clim in config.clims:
        log.info(f"Starting {clim} ######################")

        ## =============================================================================
        #  Mean air temperature
        ## =============================================================================

        # output_folder_scen = output_folder+scen+'\\'
        years_clim = yeardic[clim]
        # << this selects the correct years.
        # But when testing you’ll want to use just say 3 years data,
        # so set years manually, e.g.
        # years_clim = yeardic6p0[str(s_run.clim)]

        # this will be the shortcut line to make the testing faster (2 years data)
        if config.testing_mode == 1:
            years_clim = (
                years_clim[0],
                str(int(years_clim[0]) + 1),
            )

        nyrs_clim = int(years_clim[1]) - int(years_clim[0]) + 1

        if str(clim) == "hist":
            isi_folder = isimip_ewemib_path
            filestr = config.climate_filestr_hist
        else:
            isi_folder = isimip_bias_adj_path
            filestr = config.climate_filestr_future

        filepath = os.path.join(
            isi_folder, rcpdata, config.gcm, f"{filestr.lower()}*{config.endstr}"
        )
        if config.rcp == "rcp26":
            dst = xr.open_mfdataset(
                filepath,
                chunks={"lon": get_paths(config, "chunk_size")},
                concat_dim="time",
                use_cftime=True,
            )  # Setting for RCP2.6
        else:
            dst = xr.open_mfdataset(
                filepath,
                chunks={"lon": get_paths(config, "chunk_size")},
            )  # , concat_dim='time' )  # Setting for RCP6.0

        dst_crop = dst.sel(time=slice(years_clim[0], years_clim[1]))
        t_out_ave = dst_crop[config.davar].astype("float32") - 273.16
        t_out_ave = t_out_ave.transpose("lat", "lon", "time")

        t_oa_gbm = t_out_ave.groupby("time.month")

        # =============================================================================
        # %% Solar Irradiation - Building parameters
        # =============================================================================
        # Fixed values
        # arb_fan = 2
        # t_sp_h = np.int8(20) # Indoor setpoint temperature for heating
        #
        # P_f = 55 # power of fan (W)
        # area_fan = 25 # Numer of m2 per fan
        #
        # gains = np.int8(50) # Heat gains
        # gridshape2 = (360,720)
        #
        # 3D - time variable
        # i_sol = xr.open_dataarray('P:\\watxene\\Wat-Data\\ISI-MIP2b\\multi-GCM_input\\historical\\HadGEM2-ES\\rsds_day_HadGEM2-ES_historical_r1i1p1_EWEMBI_19710101-19801231.nc4')
        # Vertical irradiation
        # i_sol_v = xr.open_dataarray(input_folder+'CERES_vert_irrad_2001-13_avg.nc') #Values  in daily Wh/m2
        i_sol_v = xr.open_dataarray(
            os.path.join(dle_path, "EWEMBI_vert_irrad_1980-2009_avg.nc")
        )  # Values  in daily Wh/m2

        # Horizontal irradiation
        i_sol_h = xr.open_dataarray(
            os.path.join(dle_path, "EWEMBI_horiz_irrad_1980-2009_avg.nc")
        )  # Values in daily Wh/m2

        # i_sol = i_sol.sel(time=slice(years_clim[0],years_clim[1]))

        # =============================================================================
        #  Start Degree Days calculations
        # =============================================================================

        for arch in vers_archs:
            # Read in archetype parameters
            if config.arch_setting == "regional":
                xr.open_dataset(
                    os.path.join(
                        archetype_path,
                        "arch_map_" + config.arch_setting + "_" + arch + ".nc",
                    )
                )
            #        elif floor_setting == 'per_cap':
            #            floorarea = xr.open_dataset(input_folder3+'floor_area_map_'+floor_setting+'_'+suff+'.nc')

            dfa = pd.DataFrame(
                columns=["H_v_cl", "H_v_op", "H_tr"], index=par_var.index
            )
            #        suff = arch+'_'+clim
            #
            #        suff2 = str(s_run.scen)+'_'+str(s_run.year) #suffix: only scen and year
            #        #suff3 = str(s_run.scen)+'_'+str(s_run.year)+'_'+arch #suffix: scen, year and arch

            for parset in par_var.itertuples():
                # parset = par_var.iloc[0] # Placeholder select first row

                suff = clim + "_" + arch  # suffix
                # suff =  clim+'_'+arch+'_'+str(parset.name_run)  #suffix
                suff1 = arch  # only arch (for imports arch data)

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

                #            # Archetypes should be in parset to execute this  (OLD CODE)
                #            if arch_setting == 'fixed':
                #                au = parset.arch_u
                #                ar = parset.arch_r
                #                al = [ar, au]

                #    log.info(sddf)

                if config.runsdd == 1:
                    # =============================================================
                    # Simple CDD calculation
                    # =============================================================

                    for bal_temp in config.bal_temps:
                        with ProgressBar():
                            log.info("Stage 3 - Simple HDDCDD - cooling")
                            log.info("Balance temp " + str(bal_temp) + "C")
                            sdd_c = calc_SCDD_m(t_out_ave, bal_temp)
                            sdd_c = sdd_c.chunk(
                                chunks={"lon": get_paths(config, "chunk_size")}
                            )
                            log.info("chunked")
                            sdd_c.attrs = {
                                "name": "sdd_c",
                                "description": "Simple cooling degree days",
                                "units": "degC",
                                "short name": "CDD.",
                                #                                   'urt': urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                                "bal_temp": str(bal_temp),
                            }
                            sdd_c = sdd_c.to_dataset(name="sdd_c")
                            encoding = {"sdd_c": config.comp}
                            # fname = suff+'_'+str(parset.Index)+'_sdd_c_'+str(bal_temp)+'.nc'
                            fname = suff + "_sdd_c_" + str(bal_temp) + ".nc"
                            filestr = os.path.join(output_path_vdd, fname)
                            sdd_c.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                            sdd_c = xr.open_dataarray(filestr)

                    # ==============================================================
                    # Simple HDD calculation
                    # ==============================================================

                    for bal_temp in config.bal_temps:
                        with ProgressBar():
                            log.info("Stage 3 - Simple HDDCDD - heating")
                            log.info("Balance temp " + str(bal_temp) + "C")
                            sdd_h = calc_SHDD_m(t_out_ave, bal_temp)
                            sdd_h = sdd_h.chunk(
                                chunks={"lon": get_paths(config, "chunk_size")}
                            )
                            log.info("chunked")
                            sdd_h.attrs = {
                                "name": "sdd_h",
                                "description": "Simple heating degree days",
                                "units": "degC",
                                "short name": "HDD.",
                                #                                   'urt': urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                                "bal_temp": str(bal_temp),
                            }
                            sdd_h = sdd_h.to_dataset(name="sdd_h")
                            encoding = {"sdd_h": config.comp}
                            # fname = suff+'_'+str(parset.Index)+'_sdd_h_'+str(bal_temp)+'.nc'
                            fname = suff + "_sdd_h_" + str(bal_temp) + ".nc"
                            filestr = os.path.join(output_path_vdd, fname)
                            sdd_h.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                            sdd_h = xr.open_dataarray(filestr)

                for urt in config.urts:
                    if config.cool == 1:
                        # AC penetration values
                        if urt == "urban":
                            pass
                        elif urt == "rural":
                            pass

                    area_env = xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_area_env.nc")
                    )[urt]  # Area of the building envelope
                    gl_perc = xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_gl_perc.nc")
                    )[urt]  # 0.1
                    vol = xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_vol.nc")
                    )[urt]  # 3
                    xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_fl_cnd.nc")
                    )[urt]  # 1
                    u_val = xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_u_val.nc")
                    )[urt]  # 2.4 # Average U-value of building envelope # float
                    ach_cl = xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_ach_cl.nc")
                    )[urt]  # 0.5
                    ach_op = xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_ach_op.nc")
                    )[urt]  # 1.5 # TAB - ACH (1/h) - open windows
                    gn_int = xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_gn_int.nc")
                    )[urt]  # 5 # FIX Internal gains (W/m2)
                    gl_g = xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_gl_g.nc")
                    )[urt]  # 0.85 # TAB g-value for glazing
                    gl_sh = xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_gl_sh.nc")
                    )[urt]  # 0.9 # TAB shading coeff for openings
                    roof_area = xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_roof_area.nc")
                    )[urt]
                    roof_abs = xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_roof_abs.nc")
                    )[urt]
                    u_roof = xr.open_dataset(
                        os.path.join(archetype_path, "arch_" + suff1 + "_u_roof.nc")
                    )[urt]

                    # ==============================================================================
                    # Heat gains functions
                    # ==============================================================================

                    if config.solar_gains == "VERT":
                        # Solar gains - From windows only
                        with ProgressBar():
                            log.info("Stage 3 - calc gn_sol")
                            gn_sol = calc_gn_sol(i_sol_v, gl_perc, gl_g, gl_sh)
                            gn_sol = gn_sol.chunk(
                                chunks={"lon": get_paths(config, "chunk_size")}
                            )
                            log.info("chunked")
                            gn_sol.attrs = {
                                "name": "gn_sol",
                                "description": "Solar gains - Windows",
                                "units": "W/m2",
                                "short name": "Solar gains - Windows",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }
                            gn_sol = gn_sol.to_dataset(name="gn_sol")
                            encoding = {"gn_sol": config.comp}
                            fname = (
                                suff
                                + "_"
                                + str(parset.Index)
                                + "_gn_sol_"
                                + urt
                                + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            gn_sol.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                        gn_sol = xr.open_dataarray(filestr).load()

                    elif config.solar_gains == "TOT":
                        # Solar gains - Total
                        with ProgressBar():
                            log.info("Stage 3 - calc gn_sol")
                            gn_sol = calc_gn_sol_tot(
                                i_sol_v,
                                gl_perc,
                                gl_g,
                                gl_sh,
                                i_sol_h,
                                roof_area,
                                roof_abs,
                                u_roof,
                            )
                            gn_sol = gn_sol.chunk(
                                chunks={"lon": get_paths(config, "chunk_size")}
                            )
                            log.info("chunked")
                            gn_sol.attrs = {
                                "name": "gn_sol",
                                "description": "Solar gains",
                                "units": "W/m2",
                                "short name": "Solar gains",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }
                            gn_sol = gn_sol.to_dataset(name="gn_sol")
                            encoding = {"gn_sol": config.comp}
                            fname = (
                                suff
                                + "_"
                                + str(parset.Index)
                                + "_gn_sol_"
                                + urt
                                + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            gn_sol.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                        gn_sol = xr.open_dataarray(filestr).load()

                    elif config.solar_gains == "HOR":
                        # Solar gains - Total
                        with ProgressBar():
                            log.info("Stage 3 - calc gn_sol")
                            gn_sol = calc_gn_sol_h(i_sol_h, roof_area, roof_abs, u_roof)
                            gn_sol = gn_sol.chunk(
                                chunks={"lon": get_paths(config, "chunk_size")}
                            )
                            log.info("chunked")
                            gn_sol.attrs = {
                                "name": "gn_sol",
                                "description": "Solar gains",
                                "units": "W/m2",
                                "short name": "Solar gains",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }
                            gn_sol = gn_sol.to_dataset(name="gn_sol")
                            encoding = {"gn_sol": config.comp}
                            fname = (
                                suff
                                + "_"
                                + str(parset.Index)
                                + "_gn_sol_"
                                + urt
                                + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            gn_sol.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                            gn_sol = xr.open_dataarray(filestr).load()
                    #
                    #        #Solar gains - From roof only
                    #        with ProgressBar():
                    #            log.info('Stage 3 - calc gn_sol_h')
                    #            gn_sol_h = calc_gn_sol_h(i_sol_h, roof_area, roof_abs, u_roof)
                    #            gn_sol_h = gn_sol_h.chunk(chunks={'lon':chunksize2})
                    #    #        t_out_ave = t_out_ave.chunk(chunks={'lon':chunksize2})
                    #            log.info('chunked')
                    #    #        vdd_c = calc_vdd_c(t_out_ave, t_bal_c, arb_fan=2) # Not needed any more
                    #            log.info('out')
                    #            gn_sol_h.attrs = {'name':'gn_sol_h',
                    #                           'description':'Solar gains - Roof',
                    #                           'units':'W/m2',
                    #                           'short name': 'Solar gains - Roof',
                    #                           'urt': urt,
                    #                           'name_run': parset.name_run,
                    #                           'id_run': str(parset.Index)}
                    #            gn_sol_h = gn_sol_h.to_dataset(name='gn_sol_h')
                    #            encoding = {'gn_sol_h': comp}
                    #            fname = suff+'_'+str(parset.Index)+'_gn_sol_h_'+urt+'.nc'
                    #            filestr = output_folder2+fname
                    #            gn_sol_h.to_netcdf(filestr, encoding=encoding)
                    #            log.info(datetime.datetime.now()-start)
                    #        gn_sol_h = xr.open_dataarray(filestr).load()
                    #
                    #                #Solar gains - From windows only
                    #        with ProgressBar():
                    #            log.info('Stage 3 - calc gn_sol_v')
                    #            gn_sol_v = calc_gn_sol_v(i_sol_v, gl_perc, gl_g, gl_sh)
                    #            gn_sol_v = gn_sol_v.chunk(chunks={'lon':chunksize2})
                    #    #        t_out_ave = t_out_ave.chunk(chunks={'lon':chunksize2})
                    #            log.info('chunked')
                    #    #        vdd_c = calc_vdd_c(t_out_ave, t_bal_c, arb_fan=2) # Not needed any more
                    #            log.info('out')
                    #            gn_sol_v.attrs = {'name':'gn_sol_v',
                    #                           'description':'Solar gains - Windows',
                    #                           'units':'W/m2',
                    #                           'short name': 'Solar gains - Windows',
                    #                           'urt': urt,
                    #                           'name_run': parset.name_run,
                    #                           'id_run': str(parset.Index)}
                    #            gn_sol_v = gn_sol_v.to_dataset(name='gn_sol_v')
                    #            encoding = {'gn_sol_v': comp}
                    #            fname = suff+'_'+str(parset.Index)+'_gn_sol_v_'+urt+'.nc'
                    #            filestr = output_folder2+fname
                    #            gn_sol_v.to_netcdf(filestr, encoding=encoding)
                    #            log.info(datetime.datetime.now()-start)
                    #        gn_sol_v = xr.open_dataarray(filestr).load()

                    # ==============================================================================
                    # Heat transfer functions
                    # ==============================================================================

                    with ProgressBar():
                        H_v_cl = calc_H_v_cl(vol, ach_cl)
                        if config.verbose:
                            log.info("Stage 3 - calc_H_v_cl")
                            log.info(datetime.datetime.now() - start_time)
                    # H_v_cl = xr.open_dataarray(output_folder2+'H_v_cl_'+urt+'.nc').load()

                    with ProgressBar():
                        H_v_op = calc_H_v_op(vol, ach_op)
                        if config.verbose:
                            log.info("Stage 3 - calc_H_v_op")
                            log.info(datetime.datetime.now() - start_time)
                    # H_v_op = xr.open_dataarray(output_folder2+'H_v_op_'+urt+'.nc').load()

                    with ProgressBar():
                        H_tr = calc_H_tr(u_val, area_env)
                        if config.verbose:
                            log.info("Stage 3 - calc_H_tr")
                            log.info(datetime.datetime.now() - start_time)
                    #    H_tr = xr.open_dataarray(output_folder2+'H_tr_'+urt+'.nc').load()
                    dfa.loc[parset.Index, :] = [H_v_cl, H_v_op, H_tr]

                    # ==============================================================================
                    # COOLING CALCULATIONS
                    # ==============================================================================

                    if config.cool == 1:
                        # ==============================================================================
                        # Variable CDD functions
                        # ==============================================================================
                        with ProgressBar():
                            log.info("t_bal_c")
                            t_bal_c = calc_t_bal_c(
                                t_sp_c, gn_int, gn_sol, H_tr, H_v_cl
                            ).astype("float32")  # , x_diff0
                            t_bal_c = t_bal_c.chunk(
                                chunks={"lon": get_paths(config, "chunk_size")}
                            )
                            log.info("chunked")
                            t_bal_c.attrs = {
                                "name": "t_bal_c",
                                "description": "Balance (base) temperature",
                                "units": "degC",
                                "short name": "Balance temp.",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }
                            t_bal_c = t_bal_c.to_dataset(name="t_bal_c")
                            encoding = {"t_bal_c": config.comp}
                            fname = (
                                suff
                                + "_"
                                + str(parset.Index)
                                + "_t_bal_c_"
                                + urt
                                + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            t_bal_c.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                            t_bal_c = xr.open_dataarray(filestr)

                        # =============================================================================
                        # t_max_c
                        # =============================================================================
                        with ProgressBar():
                            log.info("Calc_t_max_c")
                            t_max_c = calc_t_max_c(
                                t_sp_c_max, gn_int, gn_sol, H_tr, H_v_op
                            )  # , x_diff0)
                            t_max_c.attrs = {
                                "name": "t_max_c",
                                "description": "This returns the max temperature",
                                "units": "degC",
                                "short name": "Max temp.",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }

                            t_max_c = t_max_c.to_dataset(name="t_max_c")
                            encoding = {"t_max_c": config.comp}
                            fname = (
                                suff
                                + "_"
                                + str(parset.Index)
                                + "_t_max_c_"
                                + urt
                                + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            t_max_c.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                        t_max_c = xr.open_dataarray(filestr).load()
                        # =============================================================================
                        # Nd - only this one uses daily
                        # =============================================================================
                        # Days per month over t_max_c
                        with ProgressBar():
                            log.info("Calc Nd")
                            Nd = calc_Nd(t_out_ave, t_max_c, nyrs_clim)
                            Nd.attrs = {
                                "name": "Nd",
                                "description": "This returns the days per month over t_max_c",
                                "units": "days/month",
                                "short name": "Days > t_max_c",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }
                            Nd = Nd.to_dataset(name="Nd")
                            encoding = {"Nd": config.comp}
                            fname = (
                                suff + "_" + str(parset.Index) + "_Nd_" + urt + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            Nd.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                            Nd = xr.open_dataarray(filestr)

                        # =============================================================================
                        # Nf - only this one uses daily
                        # =============================================================================
                        # Days per month over t_max_c
                        with ProgressBar():
                            log.info("Calc Nf")
                            Nf = calc_Nf(t_out_ave, t_bal_c, nyrs_clim)
                            Nf.attrs = {
                                "name": "Nf",
                                "description": "This returns the days per month above t_bal_c",
                                "units": "days/month",
                                "short name": "Days > t_bal_c",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }
                            Nf = Nf.to_dataset(name="Nf")
                            encoding = {"Nf": config.comp}
                            fname = (
                                suff + "_" + str(parset.Index) + "_Nf_" + urt + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            Nf.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                            Nf = xr.open_dataarray(filestr)

                        # =============================================================================
                        # vdd_tmax_c
                        # =============================================================================
                        # Days per month over t_max_c
                        with ProgressBar():
                            log.info("Calc_vdd_tmax_c")
                            vdd_tmax_c = calc_vdd_tmax_c(t_oa_gbm, t_max_c)
                            vdd_tmax_c = vdd_tmax_c.chunk(
                                chunks={"lon": get_paths(config, "chunk_size")}
                            )
                            vdd_tmax_c = (
                                vdd_tmax_c.groupby("time.month").sum("time") / nyrs_clim
                            )  # <<< divide by years
                            vdd_tmax_c.attrs = {
                                "name": "vdd_tmax_c",
                                "description": "This returns the sum of variable cooling degree days per month based on Tmax",
                                "units": "degC",
                                "short name": "Var. cooling DD",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }
                            vdd_tmax_c = vdd_tmax_c.to_dataset(name="vdd_tmax_c")
                            encoding = {"vdd_tmax_c": config.comp}
                            fname = (
                                suff
                                + "_"
                                + str(parset.Index)
                                + "_vdd_tmax_c_"
                                + urt
                                + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            vdd_tmax_c.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                            vdd_tmax_c = xr.open_dataarray(filestr)

                        # =============================================================================
                        # qctmax
                        # =============================================================================
                        t_bal_c = xr.open_dataarray(
                            os.path.join(
                                output_path_vdd,
                                suff
                                + "_"
                                + str(parset.Index)
                                + "_t_bal_c_"
                                + urt
                                + ".nc",
                            )
                        )
                        with ProgressBar():
                            log.info("Calc_qctmax")
                            qctmax = Q_c_tmax(
                                H_tr, H_v_cl, vdd_tmax_c, t_max_c, t_bal_c, Nd, f_c
                            )
                            qctmax.attrs = {
                                "name": "qctmax",
                                "description": "This returns the monthly cooling energy (MJ) based on variable degree days",
                                "units": "MJ/month",
                                "short name": "Sensible load",
                                "AC_hours": str(f_c),
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }

                            qctmax = qctmax.to_dataset(name="qctmax")
                            encoding = {"qctmax": config.comp}
                            fname = (
                                suff
                                + "_"
                                + str(parset.Index)
                                + "_qctmax_"
                                + urt
                                + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            qctmax.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                        t_bal_c.close()
                        qctmax = xr.open_dataarray(filestr)

                        # =============================================================================
                        # E_c_ac electricity
                        # =============================================================================
                        # qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
                        with ProgressBar():
                            log.info("E_c AC")
                            E_c_ac = calc_E_c_ac(qctmax, cop)
                            E_c_ac.attrs = {
                                "name": "E_c_ac",
                                "description": "monthly electricity requirement for air conditioning - sensible (MJ)",
                                "units": "MJ/month",
                                "short name": "AC energy sens",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }

                            E_c_ac = E_c_ac.to_dataset(name="E_c_ac")
                            encoding = {"E_c_ac": config.comp}
                            fname = (
                                suff
                                + "_"
                                + str(parset.Index)
                                + "_E_c_ac_"
                                + urt
                                + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            E_c_ac.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                        #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
                        with ProgressBar():
                            log.info("E_f fans")
                            E_c_fan = calc_E_c_fan(
                                f_f, P_f, Nf, config.area_fan
                            )  # Where Nf is same as Nd
                            E_c_fan.attrs = {
                                "name": "E_c_fan",
                                "description": "monthly electricity requirement for fans (MJ)",
                                "units": "MJ/month",
                                "short name": "Fan energy",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }
                            E_c_fan = E_c_fan.to_dataset(name="E_c_fan")
                            encoding = {"E_c_fan": config.comp}
                            fname = (
                                suff
                                + "_"
                                + str(parset.Index)
                                + "_E_c_fan_"
                                + urt
                                + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            E_c_fan.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                    # =============================================================================
                    # dfa.to_csv(output_folder2+'constant_vars_out.csv')
                    # log.info('Finished!: '+str(parset))
                    # log.info(datetime.datetime.now()-start)
                    # =============================================================================

                    # ==============================================================================
                    # HEATING CALCULATIONS
                    # ==============================================================================

                    if config.heat == 1:
                        # ==============================================================================
                        # Variable HDD functions
                        # ==============================================================================
                        with ProgressBar():
                            log.info("calc_t_bal_h")
                            t_bal_h = calc_t_bal_h(
                                t_sp_h, gn_int, gn_sol, H_tr, H_v_cl
                            ).astype("float32")  # , x_diff0
                            t_bal_h = t_bal_h.chunk(
                                chunks={"lon": get_paths(config, "chunk_size")}
                            )
                            log.info("chunked")
                            t_bal_h.attrs = {
                                "name": "t_bal_h",
                                "description": "Balance (base) temperature",
                                "units": "degC",
                                "short name": "Balance temp.",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }
                            t_bal_h = t_bal_h.to_dataset(name="t_bal_h")
                            encoding = {"t_bal_h": config.comp}
                            fname = (
                                suff
                                + "_"
                                + str(parset.Index)
                                + "_t_bal_h_"
                                + urt
                                + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            t_bal_h.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                            t_bal_h = xr.open_dataarray(filestr)

                        # =============================================================================
                        # vdd_h
                        # =============================================================================

                        with ProgressBar():
                            log.info("calc_vdd_h")
                            vdd_h = calc_vdd_h(t_oa_gbm, t_bal_h)
                            vdd_h = vdd_h.chunk(
                                chunks={"lon": get_paths(config, "chunk_size")}
                            )
                            vdd_h = (
                                vdd_h.groupby("time.month").sum("time") / nyrs_clim
                            )  # <<< divide by years
                            vdd_h.attrs = {
                                "name": "vdd_h",
                                "description": "This returns the sum of variable heating degree days per month",
                                "units": "degC",
                                "short name": "Var. heating DD",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }
                            vdd_h = vdd_h.to_dataset(name="vdd_h")
                            encoding = {"vdd_h": config.comp}
                            fname = (
                                suff + "_" + str(parset.Index) + "_vdd_h_" + urt + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            vdd_h.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                        vdd_h = xr.open_dataarray(filestr)

                        # =============================================================================
                        # qh
                        # =============================================================================
                        t_bal_h = xr.open_dataarray(
                            os.path.join(
                                output_path_vdd,
                                suff
                                + "_"
                                + str(parset.Index)
                                + "_t_bal_h_"
                                + urt
                                + ".nc",
                            )
                        )
                        with ProgressBar():
                            log.info("Calc_qh")
                            qh = Q_h(H_tr, H_v_cl, f_h, vdd_h)
                            qh.attrs = {
                                "name": "qh",
                                "description": "This returns the monthly heating energy (MJ) based on variable degree days",
                                "units": "MJ/month",
                                "short name": "Sensible load",
                                "heating_hours": str(f_h),
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }

                            qh = qh.to_dataset(name="qh")
                            encoding = {"qh": config.comp}
                            fname = (
                                suff + "_" + str(parset.Index) + "_qh_" + urt + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            qh.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                        qh = xr.open_dataarray(filestr)

                        # =============================================================================
                        # E_h final energy
                        # =============================================================================
                        # qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
                        with ProgressBar():
                            log.info("E_h")
                            E_h = calc_E_h(qh, eff)
                            E_h.attrs = {
                                "name": "E_h",
                                "description": "monthly final energy for heating (MJ)",
                                "units": "MJ/month",
                                "short name": "heating energy",
                                "urt": urt,
                                "name_run": parset.name_run,
                                "id_run": str(parset.Index),
                            }

                            E_h = E_h.to_dataset(name="E_h")
                            encoding = {"E_h": config.comp}
                            fname = (
                                suff + "_" + str(parset.Index) + "_E_h_" + urt + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            E_h.to_netcdf(filestr, encoding=encoding)
                            log.info("Saved: " + filestr)
                            if config.verbose:
                                log.info(datetime.datetime.now() - start_time)
                            #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
                        dfa.to_csv(
                            os.path.join(
                                output_path_vdd, suff + "_" + "constant_vars_out.csv"
                            )
                        )
                        # log.info('Finished!: '+suff+'+str(parset))
                        # log.info('Finished!')
                        log.info(datetime.datetime.now() - start_time)
