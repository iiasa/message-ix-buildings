import datetime
import os
from itertools import product

import numpy as np
import pandas as pd
import xarray as xr
from dask.diagnostics import ProgressBar

from message_ix_buildings.chilled.util.config import Config
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
        dle_path,
        f"output_data_{version_name}",
        gcm,
        rcp_scenario,
        "2_VDD_ene_calcs",
    )

    output_path_reg = os.path.join(
        dle_path,
        f"output_data_{version_name}",
        gcm,
        rcp_scenario,
        "3_floorarea_country_data",
    )

    msgregions = pd.read_excel(
        message_region_file,
        sheet_name="regional definition",
    )

    # Country raster
    country_ras = xr.open_dataarray(
        os.path.join(dle_path, "input_data", "gaul_lvl0_hybrid_05_3.nc")
    )

    ISO_attrs = pd.DataFrame([country_ras.attrs]).T.rename(columns={0: "ISO"})

    # Region raster
    reg_ras = xr.Dataset({"MESSAGE11": country_ras.copy(deep=True)})
    reg_ras["ISO"] = country_ras.copy(deep=True)

    for row in ISO_attrs.itertuples():
        code = row.ISO  # get country ISO code
        regval = msgregions.loc[msgregions.iso_code == code, "RegNum"]
        if regval.values.size != 0:
            reg_ras.MESSAGE11.values[country_ras.values == float(row.Index)] = regval
        else:
            print(row.Index)
            print()

    # % manual post process for pixels with the fill value
    # Latin america
    tarr = reg_ras.MESSAGE11.sel(lat=slice(25.25, -55.25), lon=slice(-180, -21))
    tarr.values[tarr.values < -10] = 5
    reg_ras.MESSAGE11.sel(lat=slice(25.25, -55.25), lon=slice(-180, -21)).values = tarr

    # Svalbard etc
    tarr = reg_ras.MESSAGE11.sel(lat=slice(84, 55), lon=slice(-30, 45))
    tarr.values[tarr.values < -10] = 11
    reg_ras.MESSAGE11.sel(lat=slice(84, 55), lon=slice(-30, 45)).values = tarr

    # SSA etc
    tarr = reg_ras.MESSAGE11.sel(lat=slice(24, -55), lon=slice(-30, 75))
    tarr.values[tarr.values < -10] = 1
    reg_ras.MESSAGE11.sel(lat=slice(24, -55), lon=slice(-30, 75)).values = tarr

    # other pacific Asia
    tarr = reg_ras.MESSAGE11.sel(lat=slice(25, -55), lon=slice(90, 180))
    tarr.values[tarr.values < -10] = 9
    reg_ras.MESSAGE11.sel(lat=slice(25, -55), lon=slice(90, 180)).values = tarr

    # british Indian Ocean territory to South ASIA
    tarr = reg_ras.MESSAGE11.sel(lat=slice(0, -10), lon=slice(65, 75))
    tarr.values[tarr.values < -10] = 10
    reg_ras.MESSAGE11.sel(lat=slice(0, -10), lon=slice(65, 75)).values = tarr

    # St Miquelon  island in Canada?
    tarr = reg_ras.MESSAGE11.sel(lat=slice(48, 44), lon=slice(-60, -50))
    tarr.values[tarr.values < -10] = 10
    reg_ras.MESSAGE11.sel(lat=slice(48, 44), lon=slice(-60, -50)).values = tarr

    # Midway atoll
    tarr = reg_ras.MESSAGE11.sel(lat=slice(80, 20), lon=slice(-180, -150))
    tarr.values[tarr.values < -10] = 10
    reg_ras.MESSAGE11.sel(lat=slice(80, 20), lon=slice(-180, -150)).values = tarr

    reg_ras.MESSAGE11.plot()

    # reg_ras.MESSAGE11.where(reg_ras.MESSAGE11 < -1).plot() # Check if worked..

    map_reg = reg_ras.MESSAGE11.copy(deep=True)

    # Write out to netcdf
    map_reg.attrs = {
        "title": "Map MESSAGE R11 regions",
        "authors": "Edward Byers & Alessio Mastrucci",
        "date": str(datetime.datetime.now()),
        "institution": "IIASA Energy Program",
        "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
    }

    map_reg.to_netcdf(os.path.join(output_path_reg, "map_reg_MESSAGE_R11.nc"))

    print(
        "Saved MESSAGE and raster map data to "
        + os.path.join(output_path_reg, "map_reg_MESSAGE_R11.nc")
    )

    for arch in archs_specified:
        suff = arch  # suffix

        if arch_setting == "fixed":
            file = os.path.join(
                dle_path, "input_data", f"arch_input_{version_name}.xlsx"
            )

            if os.path.exists(file):
                arch_inputs = pd.read_excel(file, sheet_name=suff, index_col="id")
            else:
                print(
                    "Archetypes input file "
                    + file
                    + " does not exist! Please create file for input."
                )

        elif arch_setting == "regional":
            file = os.path.join(
                dle_path,
                "input_data",
                f"arch_input_{version_name}_reg.xlsx",
            )

            reg_file = os.path.join(
                dle_path,
                "input_data",
                f"arch_regions_{version_name}.xlsx",
            )

            if os.path.exists(file):
                arch_inputs = pd.read_excel(file, sheet_name="arch")
            else:
                print(
                    "Archetypes input file "
                    + file
                    + " does not exist! Please create file for input."
                )

            if os.path.exists(reg_file):
                arch_reg = pd.read_excel(reg_file, sheet_name=arch)
            else:
                print(
                    "Regional archetypes input file "
                    + reg_file
                    + " does not exist! Please create file for input."
                )

            # Create map of archetypes based on MESSAGE regions raster
            arch_map = xr.Dataset(
                {
                    "urban": reg_ras.MESSAGE11.copy(deep=True),
                    "rural": reg_ras.MESSAGE11.copy(deep=True),
                }
            )

            for row in arch_reg.itertuples():
                arch_map["urban"].values[map_reg == row.RegNum] = float(row.urban)
                arch_map["rural"].values[map_reg == row.RegNum] = float(row.rural)

            arch_map = arch_map.astype(float)
            # NOT WORKING with integers!!
            for urt in urts:
                arch_map[urt].values[arch_map[urt] < 0] = np.nan

            # % Write out to netcdf

            arch_map.attrs = {
                "title": "Archetype IDs by region",
                "authors": "Edward Byers & Alessio Mastrucci",
                "date": str(datetime.datetime.now()),
                "institution": "IIASA Energy Program",
                "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
                "arch_setting": arch_setting,
            }

            encoding = {var: comp for var in arch_map.data_vars}
            arch_map.to_netcdf(
                os.path.join(
                    output_path_arch,
                    "arch_map_" + arch_setting + "_" + suff + ".nc",
                ),
                encoding=encoding,
            )

            print(
                "Completed archetype map at "
                + os.path.join(
                    output_path_arch,
                    "arch_map_" + arch_setting + "_" + suff + ".nc",
                )
            )


def create_building_archetype_maps(
    dle_path,
    version_name,
    gcm,
    rcp_scenario,
    arch_setting,
    archs,
    urts,
    comp,
):
    output_path_arch = os.path.join(
        dle_path,
        f"output_data_{version_name}",
        gcm,
        rcp_scenario,
        "2_VDD_ene_calcs",
    )

    def read_arch_inputs_df(version_name, arch_setting, arch):
        if arch_setting == "fixed":
            arch_file = os.path.join(
                dle_path, "input_data", f"arch_{version_name}.xlsx"
            )

            if os.path.exists(arch_file):
                arch_inputs_df = pd.read_excel(
                    arch_file, sheet_name=arch, index_col="id"
                )
                return arch_inputs_df
            else:
                print(
                    "Archetypes input file "
                    + arch_file
                    + " does not exist! Please create file for input."
                )
        elif arch_setting == "regional":
            arch_file = os.path.join(
                dle_path,
                "input_data",
                f"arch_input_{version_name}_reg.xlsx",
            )

            if os.path.exists(arch_file):
                arch_inputs_df = pd.read_excel(arch_file, sheet_name="arch")
                return arch_inputs_df
            else:
                print(
                    "Archetypes input file "
                    + arch_file
                    + " does not exist! Please create file for input."
                )

    def map_archetype_variables(args):
        varname, arch_setting, arch, list_urts = args
        print(
            "Creating archetype map for: "
            + varname
            + " + "
            + arch_setting
            + " + "
            + arch
        )
        arch_inputs_df = read_arch_inputs_df(version_name, arch_setting, arch)

        # print("...Reading archetype file")
        map = xr.open_dataset(
            os.path.join(
                output_path_arch, "arch_map_" + arch_setting + "_" + arch + ".nc"
            )
        )

        # print(".....Writing to netCDF")
        for urt in list_urts:
            for index, row in arch_inputs_df.iterrows():
                map[urt].values[map[urt] == row["id"]] = float(row[varname])
                map[urt].values[map[urt] == -1] = np.nan

        map.attrs = {
            "title": "map_" + varname,
            "authors": "Edward Byers & Alessio Mastrucci",
            "date": str(datetime.datetime.now()),
            "institution": "IIASA Energy Program",
            "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
            "arch_setting": arch_setting,
        }

        encoding = {var: comp for var in map.data_vars}
        map.to_netcdf(
            os.path.join(output_path_arch, "arch_" + arch + "_" + varname + ".nc"),
            encoding=encoding,
        )

        print(
            ".......Completed writing to file: "
            + os.path.join(output_path_arch, "arch_" + arch + "_" + varname + ".nc")
        )

    arch_inputs_list = product(VARS_ARCHETYPES, [arch_setting], archs, [urts])

    list(map(map_archetype_variables, arch_inputs_list))

    print("Completed building input maps!")


def map_calculated_variables(args):
    clim, arch, parset, urt = args

    parset_index = parset[0]
    parset_name_run = parset[1]
    parset_cop = parset[2]
    parset_eff = parset[3]
    parset_t_sp_c = parset[4]
    parset_t_sp_c_max = parset[5]
    parset_t_sp_h = parset[6]
    parset_f_c = parset[7]
    parset_f_f = parset[8]
    parset_f_h = parset[9]

    print(clim + " + " + arch + " + " + parset_name_run + " + " + urt)

    # path = os.path.join(Config.dle_path, "input_data")

    output_path_vdd = os.path.join(
        Config.dle_path,
        f"output_data_{Config.vstr}",
        Config.gcm,
        Config.rcp,
        "2_VDD_ene_calcs",
    )

    years_clim = Config.yeardic[clim]
    # << this selects the correct years.
    # But when testing you’ll want to use just say 3 years data,
    # so set years manually, e.g.
    # years_clim = yeardic6p0[str(s_run.clim)]

    start = datetime.datetime.now()

    # this will be the shortcut line to make the testing faster (2 years data)
    if Config.testing_mode == 1:
        years_clim = (
            years_clim[0],
            str(int(years_clim[0]) + 1),
        )

    nyrs_clim = int(years_clim[1]) - int(years_clim[0]) + 1

    if str(clim) == "hist":
        isi_folder = Config.isimip_ewemib_path
        filestr = Config.climate_filestr_hist
    else:
        isi_folder = Config.isimip_bias_adj_path
        filestr = Config.climate_filestr_future

    filepath = os.path.join(
        isi_folder, Config.rcpdata, Config.gcm, f"{filestr.lower()}*{Config.endstr}"
    )
    if Config.rcp == "rcp26":
        dst = xr.open_mfdataset(
            filepath,
            chunks={"lon": Config.chunk_size},
            concat_dim="time",
            use_cftime=True,
        )  # Setting for RCP2.6
    else:
        dst = xr.open_mfdataset(
            filepath,
            chunks={"lon": Config.chunk_size},
        )  # , concat_dim='time' )  # Setting for RCP6.0

    dst_crop = dst.sel(time=slice(years_clim[0], years_clim[1]))
    t_out_ave = dst_crop[Config.davar].astype("float32") - 273.16
    t_out_ave = t_out_ave.transpose("lat", "lon", "time")
    t_oa_gbm = t_out_ave.groupby("time.month")

    i_sol_v = xr.open_dataarray(
        os.path.join(Config.input_path, "EWEMBI_vert_irrad_1980-2009_avg.nc")
    )  # Values  in daily Wh/m2

    # Horizontal irradiation
    i_sol_h = xr.open_dataarray(
        os.path.join(Config.input_path, "EWEMBI_horiz_irrad_1980-2009_avg.nc")
    )  # Values in daily Wh/m2

    if Config.arch_setting == "regional":
        xr.open_dataset(
            os.path.join(
                output_path_vdd,
                "arch_map_" + Config.arch_setting + "_" + arch + ".nc",
            )
        )

    dfa = pd.DataFrame(columns=["H_v_cl", "H_v_op", "H_tr"], index=Config.par_var.index)

    suff = clim + "_" + arch  # suffix
    # suff =  clim+'_'+arch+'_'+str(parset_name_run)  #suffix
    suff1 = arch  # only arch (for imports arch data)

    print("Starting: " + suff + "_" + str(parset_name_run))
    if Config.cool == 1:
        cop = parset_cop
        t_sp_c = np.int8(parset_t_sp_c)  # Indoor setpoint temperature for cooling -> 26
        t_sp_c_max = np.int8(
            parset_t_sp_c_max
        )  # Indoor max temperature when fans are on (°C) -> 28

        f_c = parset_f_c
        f_f = parset_f_f

    if Config.heat == 1:
        t_sp_h = np.int8(
            parset_t_sp_h
        )  # Indoor max temperature when fans are on (°C) -> 20
        eff = parset_eff  # Efficiency heating system

        f_h = parset_f_h

    #            # Archetypes should be in parset to execute this  (OLD CODE)
    #            if arch_setting == 'fixed':
    #                au = parset_arch_u
    #                ar = parset_arch_r
    #                al = [ar, au]

    #    print(sddf)

    if Config.runsdd == 1:
        # =============================================================
        # Simple CDD calculation
        # =============================================================

        for bal_temp in Config.bal_temps:
            with ProgressBar():
                print("Stage 3 - Simple HDDCDD - cooling")
                print("Balance temp " + str(bal_temp) + "C")
                sdd_c = calc_SCDD_m(t_out_ave, bal_temp)
                sdd_c = sdd_c.chunk(chunks={"lon": Config.chunk_size})
                print("chunked")
                sdd_c.attrs = {
                    "name": "sdd_c",
                    "description": "Simple cooling degree days",
                    "units": "degC",
                    "short name": "CDD.",
                    #                                   'urt': urt,
                    "name_run": parset_name_run,
                    "id_run": str(parset_index),
                    "bal_temp": str(bal_temp),
                }
                print(sdd_c)
                sdd_c = sdd_c.to_dataset(name="sdd_c")
                encoding = {"sdd_c": Config.comp}
                # fname = suff+'_'+str(parset_index)+'_sdd_c_'+str(bal_temp)+'.nc'
                fname = suff + "_sdd_c_" + str(bal_temp) + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                sdd_c.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if Config.verbose:
                    print(datetime.datetime.now() - start)
                sdd_c = xr.open_dataarray(filestr)

        # ==============================================================
        # Simple HDD calculation
        # ==============================================================

        for bal_temp in Config.bal_temps:
            with ProgressBar():
                print("Stage 3 - Simple HDDCDD - heating")
                print("Balance temp " + str(bal_temp) + "C")
                sdd_h = calc_SHDD_m(t_out_ave, bal_temp)
                sdd_h = sdd_h.chunk(chunks={"lon": Config.chunk_size})
                print("chunked")
                sdd_h.attrs = {
                    "name": "sdd_h",
                    "description": "Simple heating degree days",
                    "units": "degC",
                    "short name": "HDD.",
                    #                                   'urt': urt,
                    "name_run": parset_name_run,
                    "id_run": str(parset_index),
                    "bal_temp": str(bal_temp),
                }
                sdd_h = sdd_h.to_dataset(name="sdd_h")
                encoding = {"sdd_h": Config.comp}
                # fname = suff+'_'+str(parset_index)+'_sdd_h_'+str(bal_temp)+'.nc'
                fname = suff + "_sdd_h_" + str(bal_temp) + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                sdd_h.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if Config.verbose:
                    print(datetime.datetime.now() - start)
                sdd_h = xr.open_dataarray(filestr)

    def read_netcdf_files(in_args):
        varname, arch, urt = in_args
        var = xr.open_dataset(
            os.path.join(output_path_vdd, "arch_" + arch + "_" + str(varname) + ".nc")
        )[urt]

        return var

    list_args = product(VARS_ARCHETYPES, [arch], [urt])
    list_netcdf = list(map(read_netcdf_files, list_args))
    dict_netcdf = dict(zip(VARS_ARCHETYPES, list_netcdf))

    if Config.solar_gains == "VERT":
        # Solar gains - From windows only
        with ProgressBar():
            print("Stage 3 - calc gn_sol")
            gn_sol = calc_gn_sol(
                i_sol_v,
                dict_netcdf["gl_perc"],
                dict_netcdf["gl_g"],
                dict_netcdf["gl_sh"],
            )
            gn_sol = gn_sol.chunk(chunks={"lon": Config.chunk_size})
            print("chunked")
            gn_sol.attrs = {
                "name": "gn_sol",
                "description": "Solar gains - Windows",
                "units": "W/m2",
                "short name": "Solar gains - Windows",
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            gn_sol = gn_sol.to_dataset(name="gn_sol")
            encoding = {"gn_sol": Config.comp}
            fname = suff + "_" + str(parset_index) + "_gn_sol_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            gn_sol.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
            gn_sol = xr.open_dataarray(filestr).load()

    elif Config.solar_gains == "TOT":
        # Solar gains - Total
        with ProgressBar():
            print("Stage 3 - calc gn_sol")
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
            gn_sol = gn_sol.chunk(chunks={"lon": Config.chunk_size})
            print("chunked")
            gn_sol.attrs = {
                "name": "gn_sol",
                "description": "Solar gains",
                "units": "W/m2",
                "short name": "Solar gains",
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            gn_sol = gn_sol.to_dataset(name="gn_sol")
            encoding = {"gn_sol": Config.comp}
            fname = suff + "_" + str(parset_index) + "_gn_sol_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            gn_sol.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
            gn_sol = xr.open_dataarray(filestr).load()

    elif Config.solar_gains == "HOR":
        # Solar gains - Total
        with ProgressBar():
            print("Stage 3 - calc gn_sol")
            gn_sol = calc_gn_sol_h(
                i_sol_h,
                dict_netcdf["roof_area"],
                dict_netcdf["roof_abs"],
                dict_netcdf["u_roof"],
            )
            gn_sol = gn_sol.chunk(chunks={"lon": Config.chunk_size})
            print("chunked")
            gn_sol.attrs = {
                "name": "gn_sol",
                "description": "Solar gains",
                "units": "W/m2",
                "short name": "Solar gains",
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            gn_sol = gn_sol.to_dataset(name="gn_sol")
            encoding = {"gn_sol": Config.comp}
            fname = suff + "_" + str(parset_index) + "_gn_sol_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            gn_sol.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
            gn_sol = xr.open_dataarray(filestr).load()

    # ==============================================================================
    # Heat transfer functions
    # ==============================================================================
    with ProgressBar():
        H_v_cl = calc_H_v_cl(dict_netcdf["vol"], dict_netcdf["ach_cl"])
        if Config.verbose:
            print("Stage 3 - calc_H_v_cl")
            # print(datetime.datetime.now() - start)
    # H_v_cl = xr.open_dataarray(output_folder2+'H_v_cl_'+urt+'.nc').load()

    with ProgressBar():
        H_v_op = calc_H_v_op(dict_netcdf["vol"], dict_netcdf["ach_op"])
        if Config.verbose:
            print("Stage 3 - calc_H_v_op")
            # print(datetime.datetime.now() - start)
    # H_v_op = xr.open_dataarray(output_folder2+'H_v_op_'+urt+'.nc').load()

    with ProgressBar():
        H_tr = calc_H_tr(dict_netcdf["u_val"], dict_netcdf["area_env"])
        if Config.verbose:
            print("Stage 3 - calc_H_tr")
            # print(datetime.datetime.now() - start)
    #    H_tr = xr.open_dataarray(output_folder2+'H_tr_'+urt+'.nc').load()
    dfa.loc[parset_index, :] = [H_v_cl, H_v_op, H_tr]

    if Config.cool == 1:
        # ==============================================================================
        # Variable CDD functions
        # ==============================================================================
        with ProgressBar():
            print("t_bal_c")
            t_bal_c = calc_t_bal_c(
                t_sp_c, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_cl
            ).astype("float32")  # , x_diff0
            t_bal_c = t_bal_c.chunk(chunks={"lon": Config.chunk_size})
            print("chunked")
            t_bal_c.attrs = {
                "name": "t_bal_c",
                "description": "Balance (base) temperature",
                "units": "degC",
                "short name": "Balance temp.",
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            t_bal_c = t_bal_c.to_dataset(
                name="t_bal_c"
            )  # comment out because already a Dataset
            encoding = {"t_bal_c": Config.comp}
            fname = suff + "_" + str(parset_index) + "_t_bal_c_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            t_bal_c.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
            t_bal_c = xr.open_dataarray(filestr)

        # =============================================================================
        # t_max_c
        # =============================================================================
        with ProgressBar():
            print("Calc_t_max_c")
            t_max_c = calc_t_max_c(
                t_sp_c_max, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_op
            )  # , x_diff0)
            t_max_c.attrs = {
                "name": "t_max_c",
                "description": "This returns the max temperature",
                "units": "degC",
                "short name": "Max temp.",
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            t_max_c = t_max_c.to_dataset(name="t_max_c")
            encoding = {"t_max_c": Config.comp}
            fname = suff + "_" + str(parset_index) + "_t_max_c_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            t_max_c.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
            t_max_c = xr.open_dataarray(filestr).load()
        # =============================================================================
        # Nd - only this one uses daily
        # =============================================================================
        # Days per month over t_max_c
        with ProgressBar():
            print("Calc Nd")
            Nd = calc_Nd(t_out_ave, t_max_c, nyrs_clim)
            Nd.attrs = {
                "name": "Nd",
                "description": "This returns the days per month over t_max_c",
                "units": "days/month",
                "short name": "Days > t_max_c",
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            Nd = Nd.to_dataset(name="Nd")
            encoding = {"Nd": Config.comp}
            fname = suff + "_" + str(parset_index) + "_Nd_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            Nd.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
            Nd = xr.open_dataarray(filestr)

        # =============================================================================
        # Nf - only this one uses daily
        # =============================================================================
        # Days per month over t_max_c
        with ProgressBar():
            print("Calc Nf")
            Nf = calc_Nf(t_out_ave, t_bal_c, nyrs_clim)
            Nf.attrs = {
                "name": "Nf",
                "description": "This returns the days per month above t_bal_c",
                "units": "days/month",
                "short name": "Days > t_bal_c",
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            Nf = Nf.to_dataset(name="Nf")
            encoding = {"Nf": Config.comp}
            fname = suff + "_" + str(parset_index) + "_Nf_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            Nf.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
            Nf = xr.open_dataarray(filestr)

        # =============================================================================
        # vdd_tmax_c
        # =============================================================================
        # Days per month over t_max_c
        with ProgressBar():
            print("Calc_vdd_tmax_c")
            vdd_tmax_c = calc_vdd_tmax_c(t_oa_gbm, t_max_c)
            vdd_tmax_c = vdd_tmax_c.chunk(chunks={"lon": Config.chunk_size})
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
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            vdd_tmax_c = vdd_tmax_c.to_dataset(name="vdd_tmax_c")
            encoding = {"vdd_tmax_c": Config.comp}
            fname = suff + "_" + str(parset_index) + "_vdd_tmax_c_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            vdd_tmax_c.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
            vdd_tmax_c = xr.open_dataarray(filestr)

        # =============================================================================
        # qctmax
        # =============================================================================
        # t_bal_c = xr.open_dataarray(
        #     os.path.join(
        #         output_path_vdd,
        #         suff + "_" + str(parset_index) + "_t_bal_c_" + urt + ".nc",
        #     )
        # )
        with ProgressBar():
            print("Calc_qctmax")
            qctmax = Q_c_tmax(H_tr, H_v_cl, vdd_tmax_c, t_max_c, t_bal_c, Nd, f_c)
            qctmax.attrs = {
                "name": "qctmax",
                "description": "This returns the monthly cooling energy (MJ) based on variable degree days",
                "units": "MJ/month",
                "short name": "Sensible load",
                "AC_hours": str(f_c),
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }

            qctmax = qctmax.to_dataset(name="qctmax")
            encoding = {"qctmax": Config.comp}
            fname = suff + "_" + str(parset_index) + "_qctmax_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            qctmax.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
        # t_bal_c.close()
        qctmax = xr.open_dataarray(filestr)

        # =============================================================================
        # E_c_ac electricity
        # =============================================================================
        # qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
        with ProgressBar():
            print("E_c AC")
            E_c_ac = calc_E_c_ac(qctmax, cop)
            E_c_ac.attrs = {
                "name": "E_c_ac",
                "description": "monthly electricity requirement for air conditioning - sensible (MJ)",
                "units": "MJ/month",
                "short name": "AC energy sens",
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }

            E_c_ac = E_c_ac.to_dataset(name="E_c_ac")
            encoding = {"E_c_ac": Config.comp}
            fname = suff + "_" + str(parset_index) + "_E_c_ac_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            E_c_ac.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
        #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
        with ProgressBar():
            print("E_f fans")
            E_c_fan = calc_E_c_fan(
                f_f, P_f, Nf, Config.area_fan
            )  # Where Nf is same as Nd
            E_c_fan.attrs = {
                "name": "E_c_fan",
                "description": "monthly electricity requirement for fans (MJ)",
                "units": "MJ/month",
                "short name": "Fan energy",
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            E_c_fan = E_c_fan.to_dataset(name="E_c_fan")
            encoding = {"E_c_fan": Config.comp}
            fname = suff + "_" + str(parset_index) + "_E_c_fan_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            E_c_fan.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
    # =============================================================================
    # dfa.to_csv(output_folder2+'constant_vars_out.csv')
    # print('Finished!: '+str(parset))
    # print(datetime.datetime.now()-start)
    # =============================================================================

    # ==============================================================================
    # HEATING CALCULATIONS
    # ==============================================================================

    if Config.heat == 1:
        # ==============================================================================
        # Variable HDD functions
        # ==============================================================================
        with ProgressBar():
            print("calc_t_bal_h")
            t_bal_h = calc_t_bal_h(
                t_sp_h, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_cl
            ).astype("float32")  # , x_diff0
            t_bal_h = t_bal_h.chunk(chunks={"lon": Config.chunk_size})
            print("chunked")
            t_bal_h.attrs = {
                "name": "t_bal_h",
                "description": "Balance (base) temperature",
                "units": "degC",
                "short name": "Balance temp.",
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            t_bal_h = t_bal_h.to_dataset(name="t_bal_h")
            encoding = {"t_bal_h": Config.comp}
            fname = suff + "_" + str(parset_index) + "_t_bal_h_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            t_bal_h.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
            t_bal_h = xr.open_dataarray(filestr)

        # =============================================================================
        # vdd_h
        # =============================================================================

        with ProgressBar():
            print("calc_vdd_h")
            vdd_h = calc_vdd_h(t_oa_gbm, t_bal_h)
            vdd_h = vdd_h.chunk(chunks={"lon": Config.chunk_size})
            vdd_h = (
                vdd_h.groupby("time.month").sum("time") / nyrs_clim
            )  # <<< divide by years
            vdd_h.attrs = {
                "name": "vdd_h",
                "description": "This returns the sum of variable heating degree days per month",
                "units": "degC",
                "short name": "Var. heating DD",
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            vdd_h = vdd_h.to_dataset(name="vdd_h")
            encoding = {"vdd_h": Config.comp}
            fname = suff + "_" + str(parset_index) + "_vdd_h_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            vdd_h.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
            vdd_h = xr.open_dataarray(filestr)

        # =============================================================================
        # qh
        # =============================================================================
        # t_bal_h = xr.open_dataarray(
        #     os.path.join(
        #         output_path_vdd,
        #         suff + "_" + str(parset_index) + "_t_bal_h_" + urt + ".nc",
        #     )
        # )
        with ProgressBar():
            print("Calc_qh")
            qh = Q_h(H_tr, H_v_cl, f_h, vdd_h)
            qh.attrs = {
                "name": "qh",
                "description": "This returns the monthly heating energy (MJ) based on variable degree days",
                "units": "MJ/month",
                "short name": "Sensible load",
                "heating_hours": str(f_h),
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }

            qh = qh.to_dataset(name="qh")
            encoding = {"qh": Config.comp}
            fname = suff + "_" + str(parset_index) + "_qh_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            qh.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if Config.verbose:
                print(datetime.datetime.now() - start)
            qh = xr.open_dataarray(filestr)

        # =============================================================================
        # E_h final energy
        # =============================================================================
        # qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
        with ProgressBar():
            print("E_h")
            E_h = calc_E_h(qh, eff)
            E_h.attrs = {
                "name": "E_h",
                "description": "monthly final energy for heating (MJ)",
                "units": "MJ/month",
                "short name": "heating energy",
                "urt": urt,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }

            E_h = E_h.to_dataset(name="E_h")
            encoding = {"E_h": Config.comp}
            fname = suff + "_" + str(parset_index) + "_E_h_" + urt + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            E_h.to_netcdf(filestr, encoding=encoding)
            if Config.verbose:
                print(datetime.datetime.now() - start)
            #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
        dfa.to_csv(os.path.join(output_path_vdd, suff + "_" + "constant_vars_out.csv"))
        # print('Finished!: '+suff+'+str(parset))
        # print('Finished!')
        print(datetime.datetime.now() - start)


def map_calculated_variables_2(
    dle_path,
    input_path,
    vstr,
    gcm,
    rcp,
    rcpdata,
    clims,
    archs,
    par_var,
    urts,
    cool,
    heat,
    solar_gains,
    chunk_size,
    testing_mode,
    verbose,
    comp,
    isimip_ewemib_path,
    isimip_bias_adj_path,
    climate_filestr_future,
    climate_filestr_hist,
    endstr,
    bal_temps,
    davar,
    runsdd,
    area_fan,
    arch_setting,
):
    # clim, arch, parset, urt = args

    parset_index = par_var[0]
    parset_name_run = par_var[1]
    parset_cop = par_var[2]
    parset_eff = par_var[3]
    parset_t_sp_c = par_var[4]
    parset_t_sp_c_max = par_var[5]
    parset_t_sp_h = par_var[6]
    parset_f_c = par_var[7]
    parset_f_f = par_var[8]
    parset_f_h = par_var[9]

    print(clims + " + " + archs + " + " + parset_name_run + " + " + urts)

    # path = os.path.join(dle_path, "input_data")

    output_path_vdd = os.path.join(
        dle_path,
        f"output_data_{vstr}",
        gcm,
        rcp,
        "2_VDD_ene_calcs",
    )

    if rcp == "baseline":
        yeardic = YEARS_BASELINE
    else:
        yeardic = YEARS_OTHERS

    years_clim = yeardic[clims]
    # << this selects the correct years.
    # But when testing you’ll want to use just say 3 years data,
    # so set years manually, e.g.
    # years_clim = yeardic6p0[str(s_run.clim)]

    start = datetime.datetime.now()

    # this will be the shortcut line to make the testing faster (2 years data)
    if testing_mode == 1:
        years_clim = (
            years_clim[0],
            str(int(years_clim[0]) + 1),
        )

    nyrs_clim = int(years_clim[1]) - int(years_clim[0]) + 1

    if str(clims) == "hist":
        isi_folder = isimip_ewemib_path
        filestr = climate_filestr_hist
    else:
        isi_folder = isimip_bias_adj_path
        filestr = climate_filestr_future

    filepath = os.path.join(isi_folder, rcpdata, gcm, f"{filestr.lower()}*{endstr}")
    if rcp == "rcp26":
        dst = xr.open_mfdataset(
            filepath,
            chunks={"lon": chunk_size},
            concat_dim="time",
            use_cftime=True,
        )  # Setting for RCP2.6
    else:
        dst = xr.open_mfdataset(
            filepath,
            chunks={"lon": chunk_size},
        )  # , concat_dim='time' )  # Setting for RCP6.0

    dst_crop = dst.sel(time=slice(years_clim[0], years_clim[1]))
    t_out_ave = dst_crop[davar].astype("float32") - 273.16
    t_out_ave = t_out_ave.transpose("lat", "lon", "time")
    t_oa_gbm = t_out_ave.groupby("time.month")

    i_sol_v = xr.open_dataarray(
        os.path.join(input_path, "EWEMBI_vert_irrad_1980-2009_avg.nc")
    )  # Values  in daily Wh/m2

    # Horizontal irradiation
    i_sol_h = xr.open_dataarray(
        os.path.join(input_path, "EWEMBI_horiz_irrad_1980-2009_avg.nc")
    )  # Values in daily Wh/m2

    if arch_setting == "regional":
        xr.open_dataset(
            os.path.join(
                output_path_vdd,
                "arch_map_" + arch_setting + "_" + archs + ".nc",
            )
        )

    dfa = (
        pd.DataFrame(columns=["H_v_cl", "H_v_op", "H_tr"])
        .assign(ind=parset_index)
        .set_index("ind")
    )

    suff = clims + "_" + archs  # suffix
    # suff =  clim+'_'+arch+'_'+str(parset_name_run)  #suffix
    suff1 = archs  # only arch (for imports arch data)

    print("Starting: " + suff + "_" + str(parset_name_run))
    if cool == 1:
        cop = parset_cop
        t_sp_c = np.int8(parset_t_sp_c)  # Indoor setpoint temperature for cooling -> 26
        t_sp_c_max = np.int8(
            parset_t_sp_c_max
        )  # Indoor max temperature when fans are on (°C) -> 28

        f_c = parset_f_c
        f_f = parset_f_f

    if heat == 1:
        t_sp_h = np.int8(
            parset_t_sp_h
        )  # Indoor max temperature when fans are on (°C) -> 20
        eff = parset_eff  # Efficiency heating system

        f_h = parset_f_h

    #            # Archetypes should be in parset to execute this  (OLD CODE)
    #            if arch_setting == 'fixed':
    #                au = parset_arch_u
    #                ar = parset_arch_r
    #                al = [ar, au]

    #    print(sddf)

    if runsdd == 1:
        # =============================================================
        # Simple CDD calculation
        # =============================================================

        for bal_temp in bal_temps:
            with ProgressBar():
                print("Stage 3 - Simple HDDCDD - cooling")
                print("Balance temp " + str(bal_temp) + "C")
                sdd_c = calc_SCDD_m(t_out_ave, bal_temp)
                sdd_c = sdd_c.chunk(chunks={"lon": chunk_size})
                print("chunked")
                sdd_c.attrs = {
                    "name": "sdd_c",
                    "description": "Simple cooling degree days",
                    "units": "degC",
                    "short name": "CDD.",
                    #                                   'urt': urt,
                    "name_run": parset_name_run,
                    "id_run": str(parset_index),
                    "bal_temp": str(bal_temp),
                }
                print(sdd_c)
                sdd_c = sdd_c.to_dataset(name="sdd_c")
                encoding = {"sdd_c": comp}
                # fname = suff+'_'+str(parset_index)+'_sdd_c_'+str(bal_temp)+'.nc'
                fname = suff + "_sdd_c_" + str(bal_temp) + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                sdd_c.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if verbose:
                    print(datetime.datetime.now() - start)
                sdd_c = xr.open_dataarray(filestr)

        # ==============================================================
        # Simple HDD calculation
        # ==============================================================

        for bal_temp in bal_temps:
            with ProgressBar():
                print("Stage 3 - Simple HDDCDD - heating")
                print("Balance temp " + str(bal_temp) + "C")
                sdd_h = calc_SHDD_m(t_out_ave, bal_temp)
                sdd_h = sdd_h.chunk(chunks={"lon": chunk_size})
                print("chunked")
                sdd_h.attrs = {
                    "name": "sdd_h",
                    "description": "Simple heating degree days",
                    "units": "degC",
                    "short name": "HDD.",
                    #                                   'urt': urt,
                    "name_run": parset_name_run,
                    "id_run": str(parset_index),
                    "bal_temp": str(bal_temp),
                }
                sdd_h = sdd_h.to_dataset(name="sdd_h")
                encoding = {"sdd_h": comp}
                # fname = suff+'_'+str(parset_index)+'_sdd_h_'+str(bal_temp)+'.nc'
                fname = suff + "_sdd_h_" + str(bal_temp) + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                sdd_h.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if verbose:
                    print(datetime.datetime.now() - start)
                sdd_h = xr.open_dataarray(filestr)

    def read_netcdf_files(in_args):
        varname, arch, urt = in_args
        var = xr.open_dataset(
            os.path.join(output_path_vdd, "arch_" + arch + "_" + str(varname) + ".nc")
        )[urt]

        return var

    list_args = product(VARS_ARCHETYPES, [archs], [urts])
    list_netcdf = list(map(read_netcdf_files, list_args))
    dict_netcdf = dict(zip(VARS_ARCHETYPES, list_netcdf))

    if solar_gains == "VERT":
        # Solar gains - From windows only
        with ProgressBar():
            print("Stage 3 - calc gn_sol")
            gn_sol = calc_gn_sol(
                i_sol_v,
                dict_netcdf["gl_perc"],
                dict_netcdf["gl_g"],
                dict_netcdf["gl_sh"],
            )
            gn_sol = gn_sol.chunk(chunks={"lon": chunk_size})
            print("chunked")
            gn_sol.attrs = {
                "name": "gn_sol",
                "description": "Solar gains - Windows",
                "units": "W/m2",
                "short name": "Solar gains - Windows",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            gn_sol = gn_sol.to_dataset(name="gn_sol")
            encoding = {"gn_sol": comp}
            fname = suff + "_" + str(parset_index) + "_gn_sol_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            gn_sol.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
            gn_sol = xr.open_dataarray(filestr).load()

    elif solar_gains == "TOT":
        # Solar gains - Total
        with ProgressBar():
            print("Stage 3 - calc gn_sol")
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
            gn_sol = gn_sol.chunk(chunks={"lon": chunk_size})
            print("chunked")
            gn_sol.attrs = {
                "name": "gn_sol",
                "description": "Solar gains",
                "units": "W/m2",
                "short name": "Solar gains",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            gn_sol = gn_sol.to_dataset(name="gn_sol")
            encoding = {"gn_sol": comp}
            fname = suff + "_" + str(parset_index) + "_gn_sol_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            gn_sol.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
            gn_sol = xr.open_dataarray(filestr).load()

    elif solar_gains == "HOR":
        # Solar gains - Total
        with ProgressBar():
            print("Stage 3 - calc gn_sol")
            gn_sol = calc_gn_sol_h(
                i_sol_h,
                dict_netcdf["roof_area"],
                dict_netcdf["roof_abs"],
                dict_netcdf["u_roof"],
            )
            gn_sol = gn_sol.chunk(chunks={"lon": chunk_size})
            print("chunked")
            gn_sol.attrs = {
                "name": "gn_sol",
                "description": "Solar gains",
                "units": "W/m2",
                "short name": "Solar gains",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            gn_sol = gn_sol.to_dataset(name="gn_sol")
            encoding = {"gn_sol": comp}
            fname = suff + "_" + str(parset_index) + "_gn_sol_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            gn_sol.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
            gn_sol = xr.open_dataarray(filestr).load()

    # ==============================================================================
    # Heat transfer functions
    # ==============================================================================
    with ProgressBar():
        H_v_cl = calc_H_v_cl(dict_netcdf["vol"], dict_netcdf["ach_cl"])
        if verbose:
            print("Stage 3 - calc_H_v_cl")
            # print(datetime.datetime.now() - start)
    # H_v_cl = xr.open_dataarray(output_folder2+'H_v_cl_'+urt+'.nc').load()

    with ProgressBar():
        H_v_op = calc_H_v_op(dict_netcdf["vol"], dict_netcdf["ach_op"])
        if verbose:
            print("Stage 3 - calc_H_v_op")
            # print(datetime.datetime.now() - start)
    # H_v_op = xr.open_dataarray(output_folder2+'H_v_op_'+urt+'.nc').load()

    with ProgressBar():
        H_tr = calc_H_tr(dict_netcdf["u_val"], dict_netcdf["area_env"])
        if verbose:
            print("Stage 3 - calc_H_tr")
            # print(datetime.datetime.now() - start)
    #    H_tr = xr.open_dataarray(output_folder2+'H_tr_'+urt+'.nc').load()
    dfa.loc[parset_index, :] = [H_v_cl, H_v_op, H_tr]

    if cool == 1:
        # ==============================================================================
        # Variable CDD functions
        # ==============================================================================
        with ProgressBar():
            print("t_bal_c")
            t_bal_c = calc_t_bal_c(
                t_sp_c, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_cl
            ).astype("float32")  # , x_diff0
            t_bal_c = t_bal_c.chunk(chunks={"lon": chunk_size})
            print("chunked")
            t_bal_c.attrs = {
                "name": "t_bal_c",
                "description": "Balance (base) temperature",
                "units": "degC",
                "short name": "Balance temp.",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            t_bal_c = t_bal_c.to_dataset(
                name="t_bal_c"
            )  # comment out because already a Dataset
            encoding = {"t_bal_c": comp}
            fname = suff + "_" + str(parset_index) + "_t_bal_c_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            t_bal_c.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
            t_bal_c = xr.open_dataarray(filestr)

        # =============================================================================
        # t_max_c
        # =============================================================================
        with ProgressBar():
            print("Calc_t_max_c")
            t_max_c = calc_t_max_c(
                t_sp_c_max, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_op
            )  # , x_diff0)
            t_max_c.attrs = {
                "name": "t_max_c",
                "description": "This returns the max temperature",
                "units": "degC",
                "short name": "Max temp.",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            t_max_c = t_max_c.to_dataset(name="t_max_c")
            encoding = {"t_max_c": comp}
            fname = suff + "_" + str(parset_index) + "_t_max_c_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            t_max_c.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
            t_max_c = xr.open_dataarray(filestr).load()
        # =============================================================================
        # Nd - only this one uses daily
        # =============================================================================
        # Days per month over t_max_c
        with ProgressBar():
            print("Calc Nd")
            Nd = calc_Nd(t_out_ave, t_max_c, nyrs_clim)
            Nd.attrs = {
                "name": "Nd",
                "description": "This returns the days per month over t_max_c",
                "units": "days/month",
                "short name": "Days > t_max_c",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            Nd = Nd.to_dataset(name="Nd")
            encoding = {"Nd": comp}
            fname = suff + "_" + str(parset_index) + "_Nd_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            Nd.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
            Nd = xr.open_dataarray(filestr)

        # =============================================================================
        # Nf - only this one uses daily
        # =============================================================================
        # Days per month over t_max_c
        with ProgressBar():
            print("Calc Nf")
            Nf = calc_Nf(t_out_ave, t_bal_c, nyrs_clim)
            Nf.attrs = {
                "name": "Nf",
                "description": "This returns the days per month above t_bal_c",
                "units": "days/month",
                "short name": "Days > t_bal_c",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            Nf = Nf.to_dataset(name="Nf")
            encoding = {"Nf": comp}
            fname = suff + "_" + str(parset_index) + "_Nf_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            Nf.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
            Nf = xr.open_dataarray(filestr)

        # =============================================================================
        # vdd_tmax_c
        # =============================================================================
        # Days per month over t_max_c
        with ProgressBar():
            print("Calc_vdd_tmax_c")
            vdd_tmax_c = calc_vdd_tmax_c(t_oa_gbm, t_max_c)
            vdd_tmax_c = vdd_tmax_c.chunk(chunks={"lon": chunk_size})
            vdd_tmax_c = (
                vdd_tmax_c.groupby("time.month").sum("time") / nyrs_clim
            )  # <<< divide by years

            # old = xr.open_dataarray("D:\\mengm\\IIASA\\DLE - Data\\output_data_v19_ALPS2023\\GFDL-ESM4\\ssp585\\2_VDD_ene_calcs\\archive\\2015_new_0_vdd_tmax_c_urban.nc")
            vdd_tmax_c.attrs = {
                "name": "vdd_tmax_c",
                "description": "This returns the sum of variable cooling degree days per month based on Tmax",
                "units": "degC",
                "short name": "Var. cooling DD",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            vdd_tmax_c = vdd_tmax_c.to_dataset(name="vdd_tmax_c")
            encoding = {"vdd_tmax_c": comp}
            fname = suff + "_" + str(parset_index) + "_vdd_tmax_c_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            vdd_tmax_c.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
            vdd_tmax_c = xr.open_dataarray(filestr)

        # =============================================================================
        # qctmax
        # =============================================================================
        # t_bal_c = xr.open_dataarray(
        #     os.path.join(
        #         output_path_vdd,
        #         suff + "_" + str(parset_index) + "_t_bal_c_" + urt + ".nc",
        #     )
        # )
        with ProgressBar():
            print("Calc_qctmax")
            qctmax = Q_c_tmax(H_tr, H_v_cl, vdd_tmax_c, t_max_c, t_bal_c, Nd, f_c)
            qctmax.attrs = {
                "name": "qctmax",
                "description": "This returns the monthly cooling energy (MJ) based on variable degree days",
                "units": "MJ/month",
                "short name": "Sensible load",
                "AC_hours": str(f_c),
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }

            qctmax = qctmax.to_dataset(name="qctmax")
            encoding = {"qctmax": comp}
            fname = suff + "_" + str(parset_index) + "_qctmax_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            qctmax.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
        # t_bal_c.close()
        qctmax = xr.open_dataarray(filestr)

        # =============================================================================
        # E_c_ac electricity
        # =============================================================================
        # qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
        with ProgressBar():
            print("E_c AC")
            E_c_ac = calc_E_c_ac(qctmax, cop)
            E_c_ac.attrs = {
                "name": "E_c_ac",
                "description": "monthly electricity requirement for air conditioning - sensible (MJ)",
                "units": "MJ/month",
                "short name": "AC energy sens",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }

            E_c_ac = E_c_ac.to_dataset(name="E_c_ac")
            encoding = {"E_c_ac": comp}
            fname = suff + "_" + str(parset_index) + "_E_c_ac_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            E_c_ac.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
        #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
        with ProgressBar():
            print("E_f fans")
            E_c_fan = calc_E_c_fan(f_f, P_f, Nf, area_fan)  # Where Nf is same as Nd
            E_c_fan.attrs = {
                "name": "E_c_fan",
                "description": "monthly electricity requirement for fans (MJ)",
                "units": "MJ/month",
                "short name": "Fan energy",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            E_c_fan = E_c_fan.to_dataset(name="E_c_fan")
            encoding = {"E_c_fan": comp}
            fname = suff + "_" + str(parset_index) + "_E_c_fan_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            E_c_fan.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
    # =============================================================================
    # dfa.to_csv(output_folder2+'constant_vars_out.csv')
    # print('Finished!: '+str(parset))
    # print(datetime.datetime.now()-start)
    # =============================================================================

    # ==============================================================================
    # HEATING CALCULATIONS
    # ==============================================================================

    if heat == 1:
        # ==============================================================================
        # Variable HDD functions
        # ==============================================================================
        with ProgressBar():
            print("calc_t_bal_h")
            t_bal_h = calc_t_bal_h(
                t_sp_h, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_cl
            ).astype("float32")  # , x_diff0
            t_bal_h = t_bal_h.chunk(chunks={"lon": chunk_size})
            print("chunked")
            t_bal_h.attrs = {
                "name": "t_bal_h",
                "description": "Balance (base) temperature",
                "units": "degC",
                "short name": "Balance temp.",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            t_bal_h = t_bal_h.to_dataset(name="t_bal_h")
            encoding = {"t_bal_h": comp}
            fname = suff + "_" + str(parset_index) + "_t_bal_h_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            t_bal_h.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
            t_bal_h = xr.open_dataarray(filestr)

        # =============================================================================
        # vdd_h
        # =============================================================================

        with ProgressBar():
            print("calc_vdd_h")
            vdd_h = calc_vdd_h(t_oa_gbm, t_bal_h)
            vdd_h = vdd_h.chunk(chunks={"lon": chunk_size})
            vdd_h = (
                vdd_h.groupby("time.month").sum("time") / nyrs_clim
            )  # <<< divide by years
            vdd_h.attrs = {
                "name": "vdd_h",
                "description": "This returns the sum of variable heating degree days per month",
                "units": "degC",
                "short name": "Var. heating DD",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }
            vdd_h = vdd_h.to_dataset(name="vdd_h")
            encoding = {"vdd_h": comp}
            fname = suff + "_" + str(parset_index) + "_vdd_h_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            vdd_h.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
            vdd_h = xr.open_dataarray(filestr)

        # =============================================================================
        # qh
        # =============================================================================
        # t_bal_h = xr.open_dataarray(
        #     os.path.join(
        #         output_path_vdd,
        #         suff + "_" + str(parset_index) + "_t_bal_h_" + urt + ".nc",
        #     )
        # )
        with ProgressBar():
            print("Calc_qh")
            qh = Q_h(H_tr, H_v_cl, f_h, vdd_h)
            qh.attrs = {
                "name": "qh",
                "description": "This returns the monthly heating energy (MJ) based on variable degree days",
                "units": "MJ/month",
                "short name": "Sensible load",
                "heating_hours": str(f_h),
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }

            qh = qh.to_dataset(name="qh")
            encoding = {"qh": comp}
            fname = suff + "_" + str(parset_index) + "_qh_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            qh.to_netcdf(filestr, encoding=encoding)
            print("...Saved " + filestr)
            if verbose:
                print(datetime.datetime.now() - start)
            qh = xr.open_dataarray(filestr)

        # =============================================================================
        # E_h final energy
        # =============================================================================
        # qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
        with ProgressBar():
            print("E_h")
            E_h = calc_E_h(qh, eff)
            E_h.attrs = {
                "name": "E_h",
                "description": "monthly final energy for heating (MJ)",
                "units": "MJ/month",
                "short name": "heating energy",
                "urt": urts,
                "name_run": parset_name_run,
                "id_run": str(parset_index),
            }

            E_h = E_h.to_dataset(name="E_h")
            encoding = {"E_h": comp}
            fname = suff + "_" + str(parset_index) + "_E_h_" + urts + ".nc"
            filestr = os.path.join(output_path_vdd, fname)
            E_h.to_netcdf(filestr, encoding=encoding)
            if verbose:
                print(datetime.datetime.now() - start)
            #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
        dfa.to_csv(os.path.join(output_path_vdd, suff + "_" + "constant_vars_out.csv"))
        # print('Finished!: '+suff+'+str(parset))
        # print('Finished!')
        print(datetime.datetime.now() - start)


def run_preprocessing(cfg):
    # Run create_archetype_template_map
    create_archetype_template_map(
        dle_path=cfg.dle_path,
        version_name=cfg.vstr,
        gcm=cfg.gcm,
        rcp_scenario=cfg.rcp,
        message_region_file=cfg.message_region_file,
        archs_specified=cfg.archs,
        arch_setting=cfg.arch_setting,
        urts=cfg.urts,
        comp=cfg.comp,
    )

    # Run create_building_archetype_maps
    create_building_archetype_maps(
        dle_path=cfg.dle_path,
        version_name=cfg.vstr,
        gcm=cfg.gcm,
        rcp_scenario=cfg.rcp,
        arch_setting=cfg.arch_setting,
        archs=cfg.archs,
        urts=cfg.urts,
        comp=cfg.comp,
    )


# # task executed in a worker process
# def task(identifier):
#     # generate a value
#     value = random()
#     # report a message
#     print(f"Task {identifier} executing with {value}", flush=True)
#     # block for a moment
#     sleep(value)
#     # return the generated value
#     return value
