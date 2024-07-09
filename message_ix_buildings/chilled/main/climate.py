import datetime
import os

import numpy as np
import pandas as pd  # type: ignore
import xarray as xr
from dask.diagnostics import ProgressBar
from functions.buildings_funcs_grid import (
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
from utils.config import Config  # type: ignore
from utils.util import load_parametric_analysis_data  # type: ignore


def create_climate_outputs(config: "Config", input_start: datetime.datetime):
    out_path = os.path.join(config.dle_path, "out", "version")
    save_path = os.path.join(out_path, config.vstr, "VDD_ene_calcs")

    # input_path = os.path.join(input_dle_path, "input_data")

    output_path_vdd = os.path.join(
        save_path,
        config.gcm,
        config.rcp,
        "2_VDD_ene_calcs",
    )

    if not os.path.exists(output_path_vdd):
        os.makedirs(output_path_vdd)

    par_var = load_parametric_analysis_data(config)

    for clim in config.clims:
        print(f"Starting {clim} ######################")

        ## =============================================================================
        #  Mean air temperature
        ## =============================================================================

        # output_folder_scen = output_folder+scen+'\\'
        years_clim = config.yeardic[clim]
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
            isi_folder = config.isimip_ewemib_path
            filestr = config.climate_filestr_hist
        else:
            isi_folder = config.isimip_bias_adj_path
            filestr = config.input_climate_filestr_fut

        filepath = os.path.join(
            isi_folder, config.rcpdata, config.gcm, f"{filestr.lower()}*{config.endstr}"
        )
        if config.rcp == "rcp26":
            dst = xr.open_mfdataset(
                filepath,
                chunks={"lon": config.chunk_size},
                concat_dim="time",
                use_cftime=True,
            )  # Setting for RCP2.6
        else:
            dst = xr.open_mfdataset(
                filepath,
                chunks={"lon": config.chunk_size},
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
            os.path.join(config.dle_path, "EWEMBI_vert_irrad_1980-2009_avg.nc")
        )  # Values  in daily Wh/m2

        # Horizontal irradiation
        i_sol_h = xr.open_dataarray(
            os.path.join(config.dle_path, "EWEMBI_horiz_irrad_1980-2009_avg.nc")
        )  # Values in daily Wh/m2

        # i_sol = i_sol.sel(time=slice(years_clim[0],years_clim[1]))

        # =============================================================================
        #  Start Degree Days calculations
        # =============================================================================

        for arch in config.archs:
            # Read in archetype parameters
            if config.arch_setting == "regional":
                xr.open_dataset(
                    os.path.join(
                        output_path_vdd,
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

                print("Starting: " + suff + "_" + str(parset.name_run))
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

                #    print(sddf)

                if config.runsdd == 1:
                    # =============================================================
                    # Simple CDD calculation
                    # =============================================================

                    for bal_temp in config.bal_temps:
                        with ProgressBar():
                            print("Stage 3 - Simple HDDCDD - cooling")
                            print("Balance temp " + str(bal_temp) + "C")
                            sdd_c = calc_SCDD_m(t_out_ave, bal_temp)
                            sdd_c = sdd_c.chunk(chunks={"lon": config.chunk_size})
                            print("chunked")
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
                            sdd_c = xr.open_dataarray(filestr)

                    # ==============================================================
                    # Simple HDD calculation
                    # ==============================================================

                    for bal_temp in config.bal_temps:
                        with ProgressBar():
                            print("Stage 3 - Simple HDDCDD - heating")
                            print("Balance temp " + str(bal_temp) + "C")
                            sdd_h = calc_SHDD_m(t_out_ave, bal_temp)
                            sdd_h = sdd_h.chunk(chunks={"lon": config.chunk_size})
                            print("chunked")
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
                            sdd_h = xr.open_dataarray(filestr)

                for urt in config.urts:
                    if config.cool == 1:
                        # AC penetration values
                        if urt == "urban":
                            pass
                        elif urt == "rural":
                            pass

                    area_env = xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_area_env.nc")
                    )[urt]  # Area of the building envelope
                    gl_perc = xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_gl_perc.nc")
                    )[urt]  # 0.1
                    vol = xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_vol.nc")
                    )[urt]  # 3
                    xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_fl_cnd.nc")
                    )[urt]  # 1
                    u_val = xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_u_val.nc")
                    )[urt]  # 2.4 # Average U-value of building envelope # float
                    ach_cl = xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_ach_cl.nc")
                    )[urt]  # 0.5
                    ach_op = xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_ach_op.nc")
                    )[urt]  # 1.5 # TAB - ACH (1/h) - open windows
                    gn_int = xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_gn_int.nc")
                    )[urt]  # 5 # FIX Internal gains (W/m2)
                    gl_g = xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_gl_g.nc")
                    )[urt]  # 0.85 # TAB g-value for glazing
                    gl_sh = xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_gl_sh.nc")
                    )[urt]  # 0.9 # TAB shading coeff for openings
                    roof_area = xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_roof_area.nc")
                    )[urt]
                    roof_abs = xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_roof_abs.nc")
                    )[urt]
                    u_roof = xr.open_dataset(
                        os.path.join(output_path_vdd, "arch_" + suff1 + "_u_roof.nc")
                    )[urt]

                    # ==============================================================================
                    # Heat gains functions
                    # ==============================================================================

                    if config.solar_gains == "VERT":
                        # Solar gains - From windows only
                        with ProgressBar():
                            print("Stage 3 - calc gn_sol")
                            gn_sol = calc_gn_sol(i_sol_v, gl_perc, gl_g, gl_sh)
                            gn_sol = gn_sol.chunk(chunks={"lon": config.chunk_size})
                            print("chunked")
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
                        gn_sol = xr.open_dataarray(filestr).load()

                    elif config.solar_gains == "TOT":
                        # Solar gains - Total
                        with ProgressBar():
                            print("Stage 3 - calc gn_sol")
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
                            gn_sol = gn_sol.chunk(chunks={"lon": config.chunk_size})
                            print("chunked")
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
                        gn_sol = xr.open_dataarray(filestr).load()

                    elif config.solar_gains == "HOR":
                        # Solar gains - Total
                        with ProgressBar():
                            print("Stage 3 - calc gn_sol")
                            gn_sol = calc_gn_sol_h(i_sol_h, roof_area, roof_abs, u_roof)
                            gn_sol = gn_sol.chunk(chunks={"lon": config.chunk_size})
                            print("chunked")
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
                            gn_sol = xr.open_dataarray(filestr).load()
                    #
                    #        #Solar gains - From roof only
                    #        with ProgressBar():
                    #            print('Stage 3 - calc gn_sol_h')
                    #            gn_sol_h = calc_gn_sol_h(i_sol_h, roof_area, roof_abs, u_roof)
                    #            gn_sol_h = gn_sol_h.chunk(chunks={'lon':chunksize2})
                    #    #        t_out_ave = t_out_ave.chunk(chunks={'lon':chunksize2})
                    #            print('chunked')
                    #    #        vdd_c = calc_vdd_c(t_out_ave, t_bal_c, arb_fan=2) # Not needed any more
                    #            print('out')
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
                    #            print(datetime.datetime.now()-start)
                    #        gn_sol_h = xr.open_dataarray(filestr).load()
                    #
                    #                #Solar gains - From windows only
                    #        with ProgressBar():
                    #            print('Stage 3 - calc gn_sol_v')
                    #            gn_sol_v = calc_gn_sol_v(i_sol_v, gl_perc, gl_g, gl_sh)
                    #            gn_sol_v = gn_sol_v.chunk(chunks={'lon':chunksize2})
                    #    #        t_out_ave = t_out_ave.chunk(chunks={'lon':chunksize2})
                    #            print('chunked')
                    #    #        vdd_c = calc_vdd_c(t_out_ave, t_bal_c, arb_fan=2) # Not needed any more
                    #            print('out')
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
                    #            print(datetime.datetime.now()-start)
                    #        gn_sol_v = xr.open_dataarray(filestr).load()

                    # ==============================================================================
                    # Heat transfer functions
                    # ==============================================================================

                    with ProgressBar():
                        H_v_cl = calc_H_v_cl(vol, ach_cl)
                        if config.verbose:
                            print("Stage 3 - calc_H_v_cl")
                            print(datetime.datetime.now() - input_start)
                    # H_v_cl = xr.open_dataarray(output_folder2+'H_v_cl_'+urt+'.nc').load()

                    with ProgressBar():
                        H_v_op = calc_H_v_op(vol, ach_op)
                        if config.verbose:
                            print("Stage 3 - calc_H_v_op")
                            print(datetime.datetime.now() - input_start)
                    # H_v_op = xr.open_dataarray(output_folder2+'H_v_op_'+urt+'.nc').load()

                    with ProgressBar():
                        H_tr = calc_H_tr(u_val, area_env)
                        if config.verbose:
                            print("Stage 3 - calc_H_tr")
                            print(datetime.datetime.now() - input_start)
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
                            print("t_bal_c")
                            t_bal_c = calc_t_bal_c(
                                t_sp_c, gn_int, gn_sol, H_tr, H_v_cl
                            ).astype("float32")  # , x_diff0
                            t_bal_c = t_bal_c.chunk(chunks={"lon": config.chunk_size})
                            print("chunked")
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
                            t_bal_c = xr.open_dataarray(filestr)

                        # =============================================================================
                        # t_max_c
                        # =============================================================================
                        with ProgressBar():
                            print("Calc_t_max_c")
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
                            Nf = xr.open_dataarray(filestr)

                        # =============================================================================
                        # vdd_tmax_c
                        # =============================================================================
                        # Days per month over t_max_c
                        with ProgressBar():
                            print("Calc_vdd_tmax_c")
                            vdd_tmax_c = calc_vdd_tmax_c(t_oa_gbm, t_max_c)
                            vdd_tmax_c = vdd_tmax_c.chunk(
                                chunks={"lon": config.chunk_size}
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
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
                            print("Calc_qctmax")
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
                        t_bal_c.close()
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
                        #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
                        with ProgressBar():
                            print("E_f fans")
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
                    # =============================================================================
                    # dfa.to_csv(output_folder2+'constant_vars_out.csv')
                    # print('Finished!: '+str(parset))
                    # print(datetime.datetime.now()-start)
                    # =============================================================================

                    # ==============================================================================
                    # HEATING CALCULATIONS
                    # ==============================================================================

                    if config.heat == 1:
                        # ==============================================================================
                        # Variable HDD functions
                        # ==============================================================================
                        with ProgressBar():
                            print("calc_t_bal_h")
                            t_bal_h = calc_t_bal_h(
                                t_sp_h, gn_int, gn_sol, H_tr, H_v_cl
                            ).astype("float32")  # , x_diff0
                            t_bal_h = t_bal_h.chunk(chunks={"lon": config.chunk_size})
                            print("chunked")
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
                            t_bal_h = xr.open_dataarray(filestr)

                        # =============================================================================
                        # vdd_h
                        # =============================================================================

                        with ProgressBar():
                            print("calc_vdd_h")
                            vdd_h = calc_vdd_h(t_oa_gbm, t_bal_h)
                            vdd_h = vdd_h.chunk(chunks={"lon": config.chunk_size})
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
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
                            print("Calc_qh")
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
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
                            if config.verbose:
                                print(datetime.datetime.now() - input_start)
                            #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
                        dfa.to_csv(
                            os.path.join(
                                output_path_vdd, suff + "_" + "constant_vars_out.csv"
                            )
                        )
                        # print('Finished!: '+suff+'+str(parset))
                        # print('Finished!')
                        print(datetime.datetime.now() - input_start)
