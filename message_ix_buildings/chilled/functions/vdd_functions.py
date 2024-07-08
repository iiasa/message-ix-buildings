import datetime
import os
from itertools import product

import cartopy  # type: ignore
import cartopy.crs as ccrs  # type: ignore
import cartopy.feature as cfeature  # type: ignore
import matplotlib.pyplot as plt  # type: ignore
import numpy as np
import pandas as pd  # type: ignore
import pyam  # type: ignore
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


def create_dummy_folders(
    input_dle_path, input_version_name, input_gcm, input_rcp_scenario
):
    """Create dummy folders for outputs"""
    output_path = os.path.join(input_dle_path, f"output_data_{input_version_name}")

    if not os.path.exists(os.path.join(output_path, "output")):
        os.makedirs(os.path.join(output_path, "output"))

    if not os.path.exists(
        os.path.join(output_path, input_gcm, input_rcp_scenario, "2_VDD_ene_calcs")
    ):
        os.makedirs(
            os.path.join(output_path, input_gcm, input_rcp_scenario, "2_VDD_ene_calcs")
        )

    if not os.path.exists(
        os.path.join(
            output_path, input_gcm, input_rcp_scenario, "3_floorarea_country_data"
        )
    ):
        os.makedirs(
            os.path.join(
                output_path, input_gcm, input_rcp_scenario, "3_floorarea_country_data"
            )
        )

    if not os.path.exists(
        os.path.join(output_path, input_gcm, input_rcp_scenario, "4_final_maps")
    ):
        os.makedirs(
            os.path.join(output_path, input_gcm, input_rcp_scenario, "4_final_maps")
        )

    if not os.path.exists(
        os.path.join(output_path, input_gcm, input_rcp_scenario, "5_ISO_tables")
    ):
        os.makedirs(
            os.path.join(output_path, input_gcm, input_rcp_scenario, "5_ISO_tables")
        )

    if not os.path.exists(
        os.path.join(output_path, input_gcm, input_rcp_scenario, "6_graphs")
    ):
        os.makedirs(
            os.path.join(output_path, input_gcm, input_rcp_scenario, "6_graphs")
        )

    if not os.path.exists(
        os.path.join(output_path, input_gcm, input_rcp_scenario, "6_maps")
    ):
        os.makedirs(os.path.join(output_path, input_gcm, input_rcp_scenario, "6_maps"))

    if not os.path.exists(
        os.path.join(output_path, input_gcm, input_rcp_scenario, "7_emulator")
    ):
        os.makedirs(
            os.path.join(output_path, input_gcm, input_rcp_scenario, "7_emulator")
        )

    if not os.path.exists(os.path.join(output_path, input_gcm, "output_emulator")):
        os.makedirs(os.path.join(output_path, input_gcm, "output_emulator"))


def load_all_scenarios_data(input_dle_path, input_version_name):
    input_file = os.path.join(
        input_dle_path, "input_data", f"runs_{input_version_name}.csv"
    )

    if os.path.exists(input_file):
        df = pd.read_csv(input_file, index_col="id")
        return df
    else:
        print(
            "Scenarios file "
            + input_file
            + " does not exist! Please create file for input."
        )


def load_parametric_analysis_data(
    input_dle_path, input_version_name, input_paranalysis_mode
):
    input_file = os.path.join(
        input_dle_path, "input_data", f"par_var_{input_version_name}_complete.csv"
    )

    if os.path.exists(input_file):
        df = pd.read_csv(input_file, index_col="id_run")

        if input_paranalysis_mode == 0:
            df = df.loc[df.name_run == "ref", :]

        return df
    else:
        print(
            "Parametric analysis data file "
            + input_file
            + " does not exist! Please create file for input."
        )


def create_archetype_template_map(
    input_dle_path,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
    input_message_region_file,
    input_archs_specified,
    input_arch_setting,
    input_urts,
    input_comp,
):
    output_path_arch = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "2_VDD_ene_calcs",
    )

    output_path_reg = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "3_floorarea_country_data",
    )

    msgregions = pd.read_excel(
        input_message_region_file,
        sheet_name="regional definition",
    )

    # Country raster
    country_ras = xr.open_dataarray(
        os.path.join(input_dle_path, "input_data", "gaul_lvl0_hybrid_05_3.nc")
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

    for arch in input_archs_specified:
        suff = arch  # suffix

        if input_arch_setting == "fixed":
            input_file = os.path.join(
                input_dle_path, "input_data", f"arch_input_{input_version_name}.xlsx"
            )

            if os.path.exists(input_file):
                arch_inputs = pd.read_excel(input_file, sheet_name=suff, index_col="id")
            else:
                print(
                    "Archetypes input file "
                    + input_file
                    + " does not exist! Please create file for input."
                )

        elif input_arch_setting == "regional":
            input_file = os.path.join(
                input_dle_path,
                "input_data",
                f"arch_input_{input_version_name}_reg.xlsx",
            )

            reg_file = os.path.join(
                input_dle_path,
                "input_data",
                f"arch_regions_{input_version_name}.xlsx",
            )

            if os.path.exists(input_file):
                arch_inputs = pd.read_excel(input_file, sheet_name="arch")
            else:
                print(
                    "Archetypes input file "
                    + input_file
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
            for urt in input_urts:
                arch_map[urt].values[arch_map[urt] < 0] = np.nan

            # % Write out to netcdf

            arch_map.attrs = {
                "title": "Archetype IDs by region",
                "authors": "Edward Byers & Alessio Mastrucci",
                "date": str(datetime.datetime.now()),
                "institution": "IIASA Energy Program",
                "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
                "arch_setting": input_arch_setting,
            }

            encoding = {var: input_comp for var in arch_map.data_vars}
            arch_map.to_netcdf(
                os.path.join(
                    output_path_arch,
                    "arch_map_" + input_arch_setting + "_" + suff + ".nc",
                ),
                encoding=encoding,
            )

            print(
                "Completed archetype map at "
                + os.path.join(
                    output_path_arch,
                    "arch_map_" + input_arch_setting + "_" + suff + ".nc",
                )
            )


def create_building_archetype_maps(
    input_dle_path,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
    input_arch_setting,
    input_archs,
    input_urts,
    input_comp,
):
    output_path_arch = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "2_VDD_ene_calcs",
    )

    def read_arch_inputs_df(input_version_name, arch_setting, arch):
        if arch_setting == "fixed":
            input_arch_file = os.path.join(
                input_dle_path, "input_data", f"arch_input_{input_version_name}.xlsx"
            )

            if os.path.exists(input_arch_file):
                arch_inputs_df = pd.read_excel(
                    input_arch_file, sheet_name=arch, index_col="id"
                )
                return arch_inputs_df
            else:
                print(
                    "Archetypes input file "
                    + input_arch_file
                    + " does not exist! Please create file for input."
                )
        elif arch_setting == "regional":
            input_arch_file = os.path.join(
                input_dle_path,
                "input_data",
                f"arch_input_{input_version_name}_reg.xlsx",
            )

            if os.path.exists(input_arch_file):
                arch_inputs_df = pd.read_excel(input_arch_file, sheet_name="arch")
                return arch_inputs_df
            else:
                print(
                    "Archetypes input file "
                    + input_arch_file
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
        arch_inputs_df = read_arch_inputs_df(input_version_name, arch_setting, arch)

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

        encoding = {var: input_comp for var in map.data_vars}
        map.to_netcdf(
            os.path.join(output_path_arch, "arch_" + arch + "_" + varname + ".nc"),
            encoding=encoding,
        )

        print(
            ".......Completed writing to file: "
            + os.path.join(output_path_arch, "arch_" + arch + "_" + varname + ".nc")
        )

    arch_inputs_list = product(
        VARS_ARCHETYPES, [input_arch_setting], input_archs, [input_urts]
    )

    list(map(map_archetype_variables, arch_inputs_list))

    print("Completed building input maps!")


def create_climate_outputs(
    input_dle_path,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
    input_rcpdata,
    input_start,
    input_clims,
    input_testing_mode,
    input_yeardic,
    input_isimip_bias_adj_path,
    input_isimip_ewemib_path,
    input_climate_filestr_hist,
    input_climate_filestr_fut,
    input_endstring,
    input_chunk_size,
    input_davar,
    input_archs,
    input_arch_setting,
    input_par_var,
    input_cool,
    input_heat,
    input_runsdd,
    input_bal_temps,
    input_comp,
    input_verbose,
    input_urts,
    input_solar_gains,
    input_area_fan,
):
    input_path = os.path.join(input_dle_path, "input_data")

    output_path_vdd = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "2_VDD_ene_calcs",
    )

    for clim in input_clims:
        print(f"Starting {clim} ######################")

        ## =============================================================================
        #  Mean air temperature
        ## =============================================================================

        # output_folder_scen = output_folder+scen+'\\'
        years_clim = input_yeardic[clim]
        # << this selects the correct years.
        # But when testing you’ll want to use just say 3 years data,
        # so set years manually, e.g.
        # years_clim = yeardic6p0[str(s_run.clim)]

        # this will be the shortcut line to make the testing faster (2 years data)
        if input_testing_mode == 1:
            years_clim = (
                years_clim[0],
                str(int(years_clim[0]) + 1),
            )

        nyrs_clim = int(years_clim[1]) - int(years_clim[0]) + 1

        if str(clim) == "hist":
            isi_folder = input_isimip_ewemib_path
            filestr = input_climate_filestr_hist
        else:
            isi_folder = input_isimip_bias_adj_path
            filestr = input_climate_filestr_fut

        filepath = os.path.join(
            isi_folder, input_rcpdata, input_gcm, f"{filestr.lower()}*{input_endstring}"
        )
        if input_rcp_scenario == "rcp26":
            dst = xr.open_mfdataset(
                filepath,
                chunks={"lon": input_chunk_size},
                concat_dim="time",
                use_cftime=True,
            )  # Setting for RCP2.6
        else:
            dst = xr.open_mfdataset(
                filepath,
                chunks={"lon": input_chunk_size},
            )  # , concat_dim='time' )  # Setting for RCP6.0

        dst_crop = dst.sel(time=slice(years_clim[0], years_clim[1]))
        t_out_ave = dst_crop[input_davar].astype("float32") - 273.16
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
            os.path.join(input_path, "EWEMBI_vert_irrad_1980-2009_avg.nc")
        )  # Values  in daily Wh/m2

        # Horizontal irradiation
        i_sol_h = xr.open_dataarray(
            os.path.join(input_path, "EWEMBI_horiz_irrad_1980-2009_avg.nc")
        )  # Values in daily Wh/m2

        # i_sol = i_sol.sel(time=slice(years_clim[0],years_clim[1]))

        # =============================================================================
        #  Start Degree Days calculations
        # =============================================================================

        for arch in input_archs:
            # Read in archetype parameters
            if input_arch_setting == "regional":
                xr.open_dataset(
                    os.path.join(
                        output_path_vdd,
                        "arch_map_" + input_arch_setting + "_" + arch + ".nc",
                    )
                )
            #        elif floor_setting == 'per_cap':
            #            floorarea = xr.open_dataset(input_folder3+'floor_area_map_'+floor_setting+'_'+suff+'.nc')

            dfa = pd.DataFrame(
                columns=["H_v_cl", "H_v_op", "H_tr"], index=input_par_var.index
            )
            #        suff = arch+'_'+clim
            #
            #        suff2 = str(s_run.scen)+'_'+str(s_run.year) #suffix: only scen and year
            #        #suff3 = str(s_run.scen)+'_'+str(s_run.year)+'_'+arch #suffix: scen, year and arch

            for parset in input_par_var.itertuples():
                # parset = par_var.iloc[0] # Placeholder select first row

                suff = clim + "_" + arch  # suffix
                # suff =  clim+'_'+arch+'_'+str(parset.name_run)  #suffix
                suff1 = arch  # only arch (for imports arch data)

                print("Starting: " + suff + "_" + str(parset.name_run))
                if input_cool == 1:
                    cop = parset.cop
                    t_sp_c = np.int8(
                        parset.t_sp_c
                    )  # Indoor setpoint temperature for cooling -> 26
                    t_sp_c_max = np.int8(
                        parset.t_sp_c_max
                    )  # Indoor max temperature when fans are on (°C) -> 28

                    f_c = parset.f_c
                    f_f = parset.f_f

                if input_heat == 1:
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

                if input_runsdd == 1:
                    # =============================================================
                    # Simple CDD calculation
                    # =============================================================

                    for bal_temp in input_bal_temps:
                        with ProgressBar():
                            print("Stage 3 - Simple HDDCDD - cooling")
                            print("Balance temp " + str(bal_temp) + "C")
                            sdd_c = calc_SCDD_m(t_out_ave, bal_temp)
                            sdd_c = sdd_c.chunk(chunks={"lon": input_chunk_size})
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
                            encoding = {"sdd_c": input_comp}
                            # fname = suff+'_'+str(parset.Index)+'_sdd_c_'+str(bal_temp)+'.nc'
                            fname = suff + "_sdd_c_" + str(bal_temp) + ".nc"
                            filestr = os.path.join(output_path_vdd, fname)
                            sdd_c.to_netcdf(filestr, encoding=encoding)
                            if input_verbose:
                                print(datetime.datetime.now() - input_start)
                            sdd_c = xr.open_dataarray(filestr)

                    # ==============================================================
                    # Simple HDD calculation
                    # ==============================================================

                    for bal_temp in input_bal_temps:
                        with ProgressBar():
                            print("Stage 3 - Simple HDDCDD - heating")
                            print("Balance temp " + str(bal_temp) + "C")
                            sdd_h = calc_SHDD_m(t_out_ave, bal_temp)
                            sdd_h = sdd_h.chunk(chunks={"lon": input_chunk_size})
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
                            encoding = {"sdd_h": input_comp}
                            # fname = suff+'_'+str(parset.Index)+'_sdd_h_'+str(bal_temp)+'.nc'
                            fname = suff + "_sdd_h_" + str(bal_temp) + ".nc"
                            filestr = os.path.join(output_path_vdd, fname)
                            sdd_h.to_netcdf(filestr, encoding=encoding)
                            if input_verbose:
                                print(datetime.datetime.now() - input_start)
                            sdd_h = xr.open_dataarray(filestr)

                for urt in input_urts:
                    if input_cool == 1:
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

                    if input_solar_gains == "VERT":
                        # Solar gains - From windows only
                        with ProgressBar():
                            print("Stage 3 - calc gn_sol")
                            gn_sol = calc_gn_sol(i_sol_v, gl_perc, gl_g, gl_sh)
                            gn_sol = gn_sol.chunk(chunks={"lon": input_chunk_size})
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
                            encoding = {"gn_sol": input_comp}
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
                            if input_verbose:
                                print(datetime.datetime.now() - input_start)
                        gn_sol = xr.open_dataarray(filestr).load()

                    elif input_solar_gains == "TOT":
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
                            gn_sol = gn_sol.chunk(chunks={"lon": input_chunk_size})
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
                            encoding = {"gn_sol": input_comp}
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
                            if input_verbose:
                                print(datetime.datetime.now() - input_start)
                        gn_sol = xr.open_dataarray(filestr).load()

                    elif input_solar_gains == "HOR":
                        # Solar gains - Total
                        with ProgressBar():
                            print("Stage 3 - calc gn_sol")
                            gn_sol = calc_gn_sol_h(i_sol_h, roof_area, roof_abs, u_roof)
                            gn_sol = gn_sol.chunk(chunks={"lon": input_chunk_size})
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
                            encoding = {"gn_sol": input_comp}
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
                            if input_verbose:
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
                        if input_verbose:
                            print("Stage 3 - calc_H_v_cl")
                            print(datetime.datetime.now() - input_start)
                    # H_v_cl = xr.open_dataarray(output_folder2+'H_v_cl_'+urt+'.nc').load()

                    with ProgressBar():
                        H_v_op = calc_H_v_op(vol, ach_op)
                        if input_verbose:
                            print("Stage 3 - calc_H_v_op")
                            print(datetime.datetime.now() - input_start)
                    # H_v_op = xr.open_dataarray(output_folder2+'H_v_op_'+urt+'.nc').load()

                    with ProgressBar():
                        H_tr = calc_H_tr(u_val, area_env)
                        if input_verbose:
                            print("Stage 3 - calc_H_tr")
                            print(datetime.datetime.now() - input_start)
                    #    H_tr = xr.open_dataarray(output_folder2+'H_tr_'+urt+'.nc').load()
                    dfa.loc[parset.Index, :] = [H_v_cl, H_v_op, H_tr]

                    # ==============================================================================
                    # COOLING CALCULATIONS
                    # ==============================================================================

                    if input_cool == 1:
                        # ==============================================================================
                        # Variable CDD functions
                        # ==============================================================================
                        with ProgressBar():
                            print("t_bal_c")
                            t_bal_c = calc_t_bal_c(
                                t_sp_c, gn_int, gn_sol, H_tr, H_v_cl
                            ).astype("float32")  # , x_diff0
                            t_bal_c = t_bal_c.chunk(chunks={"lon": input_chunk_size})
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
                            encoding = {"t_bal_c": input_comp}
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
                            if input_verbose:
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
                            encoding = {"t_max_c": input_comp}
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
                            if input_verbose:
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
                            encoding = {"Nd": input_comp}
                            fname = (
                                suff + "_" + str(parset.Index) + "_Nd_" + urt + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            Nd.to_netcdf(filestr, encoding=encoding)
                            if input_verbose:
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
                            encoding = {"Nf": input_comp}
                            fname = (
                                suff + "_" + str(parset.Index) + "_Nf_" + urt + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            Nf.to_netcdf(filestr, encoding=encoding)
                            if input_verbose:
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
                                chunks={"lon": input_chunk_size}
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
                            encoding = {"vdd_tmax_c": input_comp}
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
                            if input_verbose:
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
                            encoding = {"qctmax": input_comp}
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
                            if input_verbose:
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
                            encoding = {"E_c_ac": input_comp}
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
                            if input_verbose:
                                print(datetime.datetime.now() - input_start)
                        #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
                        with ProgressBar():
                            print("E_f fans")
                            E_c_fan = calc_E_c_fan(
                                f_f, P_f, Nf, input_area_fan
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
                            encoding = {"E_c_fan": input_comp}
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
                            if input_verbose:
                                print(datetime.datetime.now() - input_start)
                    # =============================================================================
                    # dfa.to_csv(output_folder2+'constant_vars_out.csv')
                    # print('Finished!: '+str(parset))
                    # print(datetime.datetime.now()-start)
                    # =============================================================================

                    # ==============================================================================
                    # HEATING CALCULATIONS
                    # ==============================================================================

                    if input_heat == 1:
                        # ==============================================================================
                        # Variable HDD functions
                        # ==============================================================================
                        with ProgressBar():
                            print("calc_t_bal_h")
                            t_bal_h = calc_t_bal_h(
                                t_sp_h, gn_int, gn_sol, H_tr, H_v_cl
                            ).astype("float32")  # , x_diff0
                            t_bal_h = t_bal_h.chunk(chunks={"lon": input_chunk_size})
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
                            encoding = {"t_bal_h": input_comp}
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
                            if input_verbose:
                                print(datetime.datetime.now() - input_start)
                            t_bal_h = xr.open_dataarray(filestr)

                        # =============================================================================
                        # vdd_h
                        # =============================================================================

                        with ProgressBar():
                            print("calc_vdd_h")
                            vdd_h = calc_vdd_h(t_oa_gbm, t_bal_h)
                            vdd_h = vdd_h.chunk(chunks={"lon": input_chunk_size})
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
                            encoding = {"vdd_h": input_comp}
                            fname = (
                                suff + "_" + str(parset.Index) + "_vdd_h_" + urt + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            vdd_h.to_netcdf(filestr, encoding=encoding)
                            if input_verbose:
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
                            encoding = {"qh": input_comp}
                            fname = (
                                suff + "_" + str(parset.Index) + "_qh_" + urt + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            qh.to_netcdf(filestr, encoding=encoding)
                            if input_verbose:
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
                            encoding = {"E_h": input_comp}
                            fname = (
                                suff + "_" + str(parset.Index) + "_E_h_" + urt + ".nc"
                            )
                            filestr = os.path.join(output_path_vdd, fname)
                            E_h.to_netcdf(filestr, encoding=encoding)
                            if input_verbose:
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


def create_climate_variables_maps(
    input_dle_path,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
    input_rcpdata,
    input_start,
    input_clims,
    input_testing_mode,
    input_yeardic,
    input_isimip_bias_adj_path,
    input_isimip_ewemib_path,
    input_climate_filestr_hist,
    input_climate_filestr_fut,
    input_endstring,
    input_chunk_size,
    input_davar,
    input_archs,
    input_arch_setting,
    input_par_var,
    input_cool,
    input_heat,
    input_runsdd,
    input_bal_temps,
    input_comp,
    input_verbose,
    input_urts,
    input_solar_gains,
    input_area_fan,
):
    def map_calculated_variables(args):
        clim, arch, parset, urt = args
        print(clim + " + " + arch + " + " + parset.name_run + " + " + urt)

        input_path = os.path.join(input_dle_path, "input_data")

        output_path_vdd = os.path.join(
            input_dle_path,
            f"output_data_{input_version_name}",
            input_gcm,
            input_rcp_scenario,
            "2_VDD_ene_calcs",
        )

        years_clim = input_yeardic[clim]
        # << this selects the correct years.
        # But when testing you’ll want to use just say 3 years data,
        # so set years manually, e.g.
        # years_clim = yeardic6p0[str(s_run.clim)]

        # this will be the shortcut line to make the testing faster (2 years data)
        if input_testing_mode == 1:
            years_clim = (
                years_clim[0],
                str(int(years_clim[0]) + 1),
            )

        nyrs_clim = int(years_clim[1]) - int(years_clim[0]) + 1

        if str(clim) == "hist":
            isi_folder = input_isimip_ewemib_path
            filestr = input_climate_filestr_hist
        else:
            isi_folder = input_isimip_bias_adj_path
            filestr = input_climate_filestr_fut

        filepath = os.path.join(
            isi_folder, input_rcpdata, input_gcm, f"{filestr.lower()}*{input_endstring}"
        )
        if input_rcp_scenario == "rcp26":
            dst = xr.open_mfdataset(
                filepath,
                chunks={"lon": input_chunk_size},
                concat_dim="time",
                use_cftime=True,
            )  # Setting for RCP2.6
        else:
            dst = xr.open_mfdataset(
                filepath,
                chunks={"lon": input_chunk_size},
            )  # , concat_dim='time' )  # Setting for RCP6.0

        dst_crop = dst.sel(time=slice(years_clim[0], years_clim[1]))
        t_out_ave = dst_crop[input_davar].astype("float32") - 273.16
        t_out_ave = t_out_ave.transpose("lat", "lon", "time")
        t_oa_gbm = t_out_ave.groupby("time.month")

        i_sol_v = xr.open_dataarray(
            os.path.join(input_path, "EWEMBI_vert_irrad_1980-2009_avg.nc")
        )  # Values  in daily Wh/m2

        # Horizontal irradiation
        i_sol_h = xr.open_dataarray(
            os.path.join(input_path, "EWEMBI_horiz_irrad_1980-2009_avg.nc")
        )  # Values in daily Wh/m2

        if input_arch_setting == "regional":
            xr.open_dataset(
                os.path.join(
                    output_path_vdd,
                    "arch_map_" + input_arch_setting + "_" + arch + ".nc",
                )
            )

        dfa = pd.DataFrame(
            columns=["H_v_cl", "H_v_op", "H_tr"], index=input_par_var.index
        )

        suff = clim + "_" + arch  # suffix
        # suff =  clim+'_'+arch+'_'+str(parset.name_run)  #suffix
        suff1 = arch  # only arch (for imports arch data)

        print("Starting: " + suff + "_" + str(parset.name_run))
        if input_cool == 1:
            cop = parset.cop
            t_sp_c = np.int8(
                parset.t_sp_c
            )  # Indoor setpoint temperature for cooling -> 26
            t_sp_c_max = np.int8(
                parset.t_sp_c_max
            )  # Indoor max temperature when fans are on (°C) -> 28

            f_c = parset.f_c
            f_f = parset.f_f

        if input_heat == 1:
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

        if input_runsdd == 1:
            # =============================================================
            # Simple CDD calculation
            # =============================================================

            for bal_temp in input_bal_temps:
                with ProgressBar():
                    print("Stage 3 - Simple HDDCDD - cooling")
                    print("Balance temp " + str(bal_temp) + "C")
                    sdd_c = calc_SCDD_m(t_out_ave, bal_temp)
                    sdd_c = sdd_c.chunk(chunks={"lon": input_chunk_size})
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
                    print(sdd_c)
                    sdd_c = sdd_c.to_dataset(name="sdd_c")
                    encoding = {"sdd_c": input_comp}
                    # fname = suff+'_'+str(parset.Index)+'_sdd_c_'+str(bal_temp)+'.nc'
                    fname = suff + "_sdd_c_" + str(bal_temp) + ".nc"
                    filestr = os.path.join(output_path_vdd, fname)
                    sdd_c.to_netcdf(filestr, encoding=encoding)
                    print("...Saved " + filestr)
                    if input_verbose:
                        print(datetime.datetime.now() - input_start)
                    sdd_c = xr.open_dataarray(filestr)

            # ==============================================================
            # Simple HDD calculation
            # ==============================================================

            for bal_temp in input_bal_temps:
                with ProgressBar():
                    print("Stage 3 - Simple HDDCDD - heating")
                    print("Balance temp " + str(bal_temp) + "C")
                    sdd_h = calc_SHDD_m(t_out_ave, bal_temp)
                    sdd_h = sdd_h.chunk(chunks={"lon": input_chunk_size})
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
                    encoding = {"sdd_h": input_comp}
                    # fname = suff+'_'+str(parset.Index)+'_sdd_h_'+str(bal_temp)+'.nc'
                    fname = suff + "_sdd_h_" + str(bal_temp) + ".nc"
                    filestr = os.path.join(output_path_vdd, fname)
                    sdd_h.to_netcdf(filestr, encoding=encoding)
                    print("...Saved " + filestr)
                    if input_verbose:
                        print(datetime.datetime.now() - input_start)
                    sdd_h = xr.open_dataarray(filestr)

        def read_netcdf_files(input_args):
            varname, arch, urt = input_args
            var = xr.open_dataset(
                os.path.join(
                    output_path_vdd, "arch_" + arch + "_" + str(varname) + ".nc"
                )
            )[urt]

            return var

        list_args = product(VARS_ARCHETYPES, [arch], [urt])
        list_netcdf = list(map(read_netcdf_files, list_args))
        dict_netcdf = dict(zip(VARS_ARCHETYPES, list_netcdf))

        if input_solar_gains == "VERT":
            # Solar gains - From windows only
            with ProgressBar():
                print("Stage 3 - calc gn_sol")
                gn_sol = calc_gn_sol(
                    i_sol_v,
                    dict_netcdf["gl_perc"],
                    dict_netcdf["gl_g"],
                    dict_netcdf["gl_sh"],
                )
                gn_sol = gn_sol.chunk(chunks={"lon": input_chunk_size})
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
                encoding = {"gn_sol": input_comp}
                fname = suff + "_" + str(parset.Index) + "_gn_sol_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                gn_sol.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
                    print(datetime.datetime.now() - input_start)
                gn_sol = xr.open_dataarray(filestr).load()

        elif input_solar_gains == "TOT":
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
                gn_sol = gn_sol.chunk(chunks={"lon": input_chunk_size})
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
                encoding = {"gn_sol": input_comp}
                fname = suff + "_" + str(parset.Index) + "_gn_sol_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                gn_sol.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
                    print(datetime.datetime.now() - input_start)
                gn_sol = xr.open_dataarray(filestr).load()

        elif input_solar_gains == "HOR":
            # Solar gains - Total
            with ProgressBar():
                print("Stage 3 - calc gn_sol")
                gn_sol = calc_gn_sol_h(
                    i_sol_h,
                    dict_netcdf["roof_area"],
                    dict_netcdf["roof_abs"],
                    dict_netcdf["u_roof"],
                )
                gn_sol = gn_sol.chunk(chunks={"lon": input_chunk_size})
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
                encoding = {"gn_sol": input_comp}
                fname = suff + "_" + str(parset.Index) + "_gn_sol_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                gn_sol.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
                    print(datetime.datetime.now() - input_start)
                gn_sol = xr.open_dataarray(filestr).load()

        # ==============================================================================
        # Heat transfer functions
        # ==============================================================================
        with ProgressBar():
            H_v_cl = calc_H_v_cl(dict_netcdf["vol"], dict_netcdf["ach_cl"])
            if input_verbose:
                print("Stage 3 - calc_H_v_cl")
                # print(datetime.datetime.now() - input_start)
        # H_v_cl = xr.open_dataarray(output_folder2+'H_v_cl_'+urt+'.nc').load()

        with ProgressBar():
            H_v_op = calc_H_v_op(dict_netcdf["vol"], dict_netcdf["ach_op"])
            if input_verbose:
                print("Stage 3 - calc_H_v_op")
                # print(datetime.datetime.now() - input_start)
        # H_v_op = xr.open_dataarray(output_folder2+'H_v_op_'+urt+'.nc').load()

        with ProgressBar():
            H_tr = calc_H_tr(dict_netcdf["u_val"], dict_netcdf["area_env"])
            if input_verbose:
                print("Stage 3 - calc_H_tr")
                # print(datetime.datetime.now() - input_start)
        #    H_tr = xr.open_dataarray(output_folder2+'H_tr_'+urt+'.nc').load()
        dfa.loc[parset.Index, :] = [H_v_cl, H_v_op, H_tr]

        if input_cool == 1:
            # ==============================================================================
            # Variable CDD functions
            # ==============================================================================
            with ProgressBar():
                print("t_bal_c")
                t_bal_c = calc_t_bal_c(
                    t_sp_c, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_cl
                ).astype("float32")  # , x_diff0
                t_bal_c = t_bal_c.chunk(chunks={"lon": input_chunk_size})
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
                t_bal_c = t_bal_c.to_dataset(
                    name="t_bal_c"
                )  # comment out because already a Dataset
                encoding = {"t_bal_c": input_comp}
                fname = suff + "_" + str(parset.Index) + "_t_bal_c_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                t_bal_c.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
                    print(datetime.datetime.now() - input_start)
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
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }
                t_max_c = t_max_c.to_dataset(name="t_max_c")
                encoding = {"t_max_c": input_comp}
                fname = suff + "_" + str(parset.Index) + "_t_max_c_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                t_max_c.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
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
                encoding = {"Nd": input_comp}
                fname = suff + "_" + str(parset.Index) + "_Nd_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                Nd.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
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
                encoding = {"Nf": input_comp}
                fname = suff + "_" + str(parset.Index) + "_Nf_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                Nf.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
                    print(datetime.datetime.now() - input_start)
                Nf = xr.open_dataarray(filestr)

            # =============================================================================
            # vdd_tmax_c
            # =============================================================================
            # Days per month over t_max_c
            with ProgressBar():
                print("Calc_vdd_tmax_c")
                vdd_tmax_c = calc_vdd_tmax_c(t_oa_gbm, t_max_c)
                vdd_tmax_c = vdd_tmax_c.chunk(chunks={"lon": input_chunk_size})
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
                encoding = {"vdd_tmax_c": input_comp}
                fname = suff + "_" + str(parset.Index) + "_vdd_tmax_c_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                vdd_tmax_c.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
                    print(datetime.datetime.now() - input_start)
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
                print("Calc_qctmax")
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
                encoding = {"qctmax": input_comp}
                fname = suff + "_" + str(parset.Index) + "_qctmax_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                qctmax.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
                    print(datetime.datetime.now() - input_start)
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
                    "name_run": parset.name_run,
                    "id_run": str(parset.Index),
                }

                E_c_ac = E_c_ac.to_dataset(name="E_c_ac")
                encoding = {"E_c_ac": input_comp}
                fname = suff + "_" + str(parset.Index) + "_E_c_ac_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                E_c_ac.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
                    print(datetime.datetime.now() - input_start)
            #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
            with ProgressBar():
                print("E_f fans")
                E_c_fan = calc_E_c_fan(
                    f_f, P_f, Nf, input_area_fan
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
                encoding = {"E_c_fan": input_comp}
                fname = suff + "_" + str(parset.Index) + "_E_c_fan_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                E_c_fan.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
                    print(datetime.datetime.now() - input_start)
        # =============================================================================
        # dfa.to_csv(output_folder2+'constant_vars_out.csv')
        # print('Finished!: '+str(parset))
        # print(datetime.datetime.now()-start)
        # =============================================================================

        # ==============================================================================
        # HEATING CALCULATIONS
        # ==============================================================================

        if input_heat == 1:
            # ==============================================================================
            # Variable HDD functions
            # ==============================================================================
            with ProgressBar():
                print("calc_t_bal_h")
                t_bal_h = calc_t_bal_h(
                    t_sp_h, dict_netcdf["gn_int"], gn_sol, H_tr, H_v_cl
                ).astype("float32")  # , x_diff0
                t_bal_h = t_bal_h.chunk(chunks={"lon": input_chunk_size})
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
                encoding = {"t_bal_h": input_comp}
                fname = suff + "_" + str(parset.Index) + "_t_bal_h_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                t_bal_h.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
                    print(datetime.datetime.now() - input_start)
                t_bal_h = xr.open_dataarray(filestr)

            # =============================================================================
            # vdd_h
            # =============================================================================

            with ProgressBar():
                print("calc_vdd_h")
                vdd_h = calc_vdd_h(t_oa_gbm, t_bal_h)
                vdd_h = vdd_h.chunk(chunks={"lon": input_chunk_size})
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
                encoding = {"vdd_h": input_comp}
                fname = suff + "_" + str(parset.Index) + "_vdd_h_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                vdd_h.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
                    print(datetime.datetime.now() - input_start)
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
                encoding = {"qh": input_comp}
                fname = suff + "_" + str(parset.Index) + "_qh_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                qh.to_netcdf(filestr, encoding=encoding)
                print("...Saved " + filestr)
                if input_verbose:
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
                encoding = {"E_h": input_comp}
                fname = suff + "_" + str(parset.Index) + "_E_h_" + urt + ".nc"
                filestr = os.path.join(output_path_vdd, fname)
                E_h.to_netcdf(filestr, encoding=encoding)
                if input_verbose:
                    print(datetime.datetime.now() - input_start)
                #    qlat_month = xr.open_dataarray(output_folder2+'qlat_month_'+urt+'.nc')
            dfa.to_csv(
                os.path.join(output_path_vdd, suff + "_" + "constant_vars_out.csv")
            )
            # print('Finished!: '+suff+'+str(parset))
            # print('Finished!')
            print(datetime.datetime.now() - input_start)

    inputs = product(input_clims, input_archs, input_par_var.itertuples(), input_urts)
    list(map(map_calculated_variables, inputs))

    # mypool = Pool(4)
    # return list(mypool.map(map_calculated_variables, inputs))


def aggregate_urban_rural_files(
    input_dle_path,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
    input_clims,
    input_archs,
    input_cool,
    input_heat,
    input_par_var,
    input_urts,
):
    output_path_vdd = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "2_VDD_ene_calcs",
    )

    for clim in input_clims:
        for arch in input_archs:
            suff = clim + "_" + arch  # suffix

            print("Aggregating results for " + suff)
            # Aggregate archetypes into same files.
            varlist = []

            if input_cool == 1:
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

            if input_heat == 1:
                varlist.extend(["t_bal_h", "vdd_h", "qh", "E_h"])

            comp = {"zlib": True}

            for parset in input_par_var.itertuples():
                for var in varlist:
                    print(var)
                    mds = xr.open_mfdataset(
                        os.path.join(
                            output_path_vdd,
                            suff + "_" + str(parset.Index) + "_" + var + "_*.nc",
                        ),
                        concat_dim="urt",
                        combine="nested",
                    )  # +urt[0]
                    # mds['arch'] = al #arcs#[:4]
                    mds["urt"] = input_urts  # arcs#[:4]

                    mds.attrs = {
                        "title": mds[var].attrs["name"],
                        "authors": "Edward Byers & Alessio Mastrucci",
                        "date": str(datetime.datetime.now()),
                        "institution": "IIASA Energy Program",
                        "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
                        # 'archetypes': al,
                        "urb_rur": input_urts,
                        "name_run": parset.name_run,
                        "id_run": str(parset.Index),
                    }
                    encoding = {var: comp for var in mds.data_vars}
                    filestr = os.path.join(
                        output_path_vdd,
                        suff + "_" + str(parset.Index) + "_" + var + "ALL.nc",
                    )
                    mds.to_netcdf(filestr, encoding=encoding)


def make_vdd_total_maps(
    input_dle_path,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
    input_s_runs,
    input_clims,
    input_archs,
    input_urts,
    input_cool,
    input_heat,
):
    output_path_vdd = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "2_VDD_ene_calcs",
    )

    # TODO: (meas) the original code does not query for clims,
    # but without it the code will crash if not all years have been run
    clims_int = list(map(int, input_clims))
    print(clims_int)
    s_runs = input_s_runs.query("clim in @clims_int")

    for s_run in s_runs.itertuples():
        for arch in input_archs:
            for urt in input_urts:
                suff = str(s_run.clim) + "_" + arch  # suffix

                print("Running " + suff)

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
                        from matplotlib.colors import LogNorm

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
                        import matplotlib.ticker as mticker
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

                if input_cool == 1:
                    vdd_c_in = xr.open_dataarray(
                        os.path.join(
                            output_path_vdd, suff + "_0_vdd_tmax_c_" + urt + ".nc"
                        )
                    ).load()
                    vdd_c_year = vdd_c_in.sum(dim="month")
                    E_c_ac_in = xr.open_dataarray(
                        os.path.join(output_path_vdd, suff + "_0_E_c_ac_" + urt + ".nc")
                    ).load()
                    (E_c_ac_in.sum(dim="month") * 0.2777778)  # Yearly total in kWh/m2

                if input_heat == 1:
                    vdd_h_in = xr.open_dataarray(
                        os.path.join(output_path_vdd, suff + "_0_vdd_h_" + urt + ".nc")
                    ).load()
                    vdd_h_year = vdd_h_in.sum(dim="month")
                    E_h_in = xr.open_dataarray(
                        os.path.join(output_path_vdd, suff + "_0_E_h_" + urt + ".nc")
                    ).load()
                    (E_h_in.sum(dim="month") * 0.2777778)  # Yearly total in kWh/m2

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
                if input_cool == 1:
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
                if input_heat == 1:
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
                if input_heat == 1:
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
                if input_cool == 1:
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


def read_in_gaul_file(input_dle_path, input_message_region_file):
    input_path = os.path.join(input_dle_path, "input_data")
    gaul_file = os.path.join(input_path, "gaul_lvl0_hybrid_05_3.nc")

    country_ras = xr.open_dataarray(gaul_file)

    country_ras.values = country_ras.values.astype(float)
    country_ras.values[country_ras == -1] = np.nan
    ISO_attrs = pd.DataFrame([country_ras.attrs]).T
    ISO_attrs.columns = ["ISO"]

    # Region raster

    msgregions = pd.read_excel(
        input_message_region_file,
        sheet_name="regional definition",
    )

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

    return country_ras, ISO_attrs, reg_ras


def process_construction_shares(
    input_constr_setting,
    input_dle_path,
    input_urts,
    input_country_ras,
    input_iso_attrs,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
):
    # If constr_setting == 1, then process construction shares. Otherwise, skip
    if input_constr_setting == 1:
        input_path = os.path.join(input_dle_path, "input_data")
        output_path = os.path.join(
            input_dle_path,
            f"output_data_{input_version_name}",
            input_gcm,
            input_rcp_scenario,
            "3_floorarea_country_data",
        )

        dsc = xr.Dataset()
        for urt in input_urts:
            # Read in excel files
            conshare = pd.read_csv(
                os.path.join(input_path, "constr_share_" + urt + ".csv")
            )
            tl = list(conshare.columns)
            arcs = [s for s in tl if s[0] == urt[0]]

            # build dummy dataset
            block = np.full((360, 720, 20), np.nan)
            lats = input_country_ras.lat
            lons = input_country_ras.lon
            coords = {"lat": lats, "lon": lons, "arch": arcs}
            ds = xr.DataArray(block, coords=coords, dims=["lat", "lon", "arch"])

            for row in input_iso_attrs.itertuples():
                conshare.loc[conshare.ISO == row.ISO, "regnum"] = row.Index

            # missing  AND, Andorra DMA, Dominica GRD,Grenada LIE,
            # liechenstein ATG, Antigua & barbuda KNA, Saint kitts SYC seychelles
            for arch in arcs:  # For each arch
                ai = arcs.index(arch)
                ta = ds.sel(arch=arch).values

                for row in input_iso_attrs.itertuples():  # For each country
                    ats = conshare.loc[conshare.ISO == row.ISO, :]

                    try:
                        ta[input_country_ras == int(row.Index)] = ats.loc[
                            ats.ISO == row.ISO, arch
                        ].values[0]
                    except IndexError:
                        print("map:" + row.ISO)
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
        print("Completed construction share maps")
    else:
        print("Skipping construction share maps because constr_setting != 1")


# Function to create floor area maps
def process_floor_area_maps(
    input_floor_setting,
    input_dle_path,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
    input_urts,
    input_reg_ras,
    input_s_runs,
    input_vstrcntry,
):
    input_path = os.path.join(input_dle_path, "input_data")
    output_path = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "3_floorarea_country_data",
    )

    if input_floor_setting == "std_cap":
        floorarea = pd.read_csv(
            os.path.join(input_path, "floor_std_cap.csv")
        )  # STD conditioned floor area (same for all regions, based on DLE)

        floormap = xr.Dataset(
            {
                "urban": input_reg_ras.MESSAGE11.copy(deep=True),
                "rural": input_reg_ras.MESSAGE11.copy(deep=True),
            }
        )

        for row in floorarea.itertuples():
            floormap["urban"].values[floormap.urban == row.RegNum] = float(row.urban)
            floormap["rural"].values[floormap.rural == row.RegNum] = float(row.rural)
            # floormap['urban'].values[floormap.urban==row.RegNum] = getattr(row,'urban'+suff)
            # floormap['rural'].values[floormap.rural==row.RegNum] = getattr(row,'rural'+suff)

        floormap = floormap.astype(float)
        for urt in input_urts:
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
            "floor_setting": input_floor_setting,
        }
        comp = {"zlib": True}
        encoding = {var: comp for var in floormap.data_vars}
        floormap.to_netcdf(
            os.path.join(output_path, "floor_area_map_std_cap.nc"), encoding=encoding
        )
        print("Completed floor area maps")

    elif input_floor_setting == "per_cap":
        for s_run in input_s_runs.itertuples():
            suff = (
                str(s_run.scen) + "_" + str(s_run.year) + "_" + str(s_run.clim)
            )  # suffix
            suff2 = (
                str(s_run.scen) + "_" + str(s_run.year)
            )  # suffix: only scen and year

            floorarea = pd.read_excel(
                os.path.join(input_path, "floor_per_cap_" + input_vstrcntry + ".xlsx"),
                sheet_name=suff2,
            )

            floormap = xr.Dataset(
                {
                    "urban": input_reg_ras.MESSAGE11.copy(deep=True),
                    "rural": input_reg_ras.MESSAGE11.copy(deep=True),
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
            for urt in input_urts:
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
                "floor_setting": input_floor_setting,
            }
            comp = {"zlib": True}
            encoding = {var: comp for var in floormap.data_vars}
            floormap.to_netcdf(
                os.path.join(
                    output_path,
                    "floor_area_map_" + input_floor_setting + "_" + suff + ".nc",
                ),
                encoding=encoding,
            )

            print("Completed floor area maps")

    else:
        print("Skipping floor area maps because floor_setting != std_cap or per_cap")


# Function to create country maps
def process_country_maps(
    input_dle_path,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
    input_s_runs,
    input_vstrcntry,
    input_iso_attrs,
    input_country_ras,
):
    """Energy demand by floor areas, construction shares, and population

    Adapted from `y4_final_maps.py`

    Parameters
    ----------
    input_dle_path : str
        Path to the DLE directory
    input_version_name : str
        Name of the DLE version
    input_rcp_scenario : str
        RCP scenario
    input_s_runs : pandas.DataFrame
        Scenario runs
    input_vstrcntry : str
        Version of the country data
    input_iso_attrs : pandas.DataFrame
        ISO attributes
    input_country_ras : xarray.DataArray
        Country raster

    Returns
    -------
    None
        Saves multiple netcdf files to the output directory

    """
    input_path = os.path.join(input_dle_path, "input_data")
    output_path = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "3_floorarea_country_data",
    )

    for s_run in input_s_runs.itertuples():
        suff = str(s_run.scen) + "_" + str(s_run.year) + "_" + str(s_run.clim)  # suffix
        str(s_run.scen) + "_" + str(s_run.year)  # suffix: only scen and year

        # Read in country_data for AC penetration, electricity access, slumpop
        # country_data = pd.read_excel(input_folder+'country_data_'+vstrcntry+'.xlsx', sheet_name = 'suff2')
        country_data = pd.read_excel(
            os.path.join(input_path, "country_data_" + input_vstrcntry + ".xlsx"),
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
        input_iso_attrs["GAULi"] = input_iso_attrs.index.astype(float)
        country_data = country_data.merge(input_iso_attrs, on="ISO", how="outer")

        # create dataset four country data
        cd_map = xr.Dataset(
            {cols[0]: input_country_ras.copy(deep=True).astype("float")}
        )
        for col in cols[1:]:
            cd_map[col] = input_country_ras.copy(deep=True).astype("float")

        # Populate the dataset with country data
        for col in cols:
            for idx in input_iso_attrs.GAULi:
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

    print("Finished country data maps")


# Function to create final maps
def process_final_maps(
    input_dle_path,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
    input_cool,
    input_heat,
    input_par_var,
    input_paranalysis_mode,
    input_s_runs,
    input_clims,
    input_archs,
    input_popfix,
    input_floor_setting,
    input_nd_thresh,
    input_urts,
    input_comp,
):
    input_path = os.path.join(input_dle_path, "input_data")

    country_maps_path = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "3_floorarea_country_data",
    )

    ene_calcs_path = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "2_VDD_ene_calcs",
    )

    final_maps_path = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "4_final_maps",
    )

    vardic = {}
    varundic = {}

    # Update dictionaries if input_cool == 1
    if input_cool == 1:
        vardic.update(VARDICT_COOL)
        varundic.update(VARUNDICT_COOL)
    # Update dictionaries if input_heat == 1
    if input_heat == 1:
        vardic.update(VARDICT_HEAT)
        varundic.update(VARUNDICT_HEAT)

    [key for key in varundic.keys()]

    # Read in file
    # par_var = pd.read_csv(
    #     os.path.join(input_path, "par_var_" + input_version_name + ".csv"),
    #     index_col="id_run",
    # )  # .iloc[:3,:] #,
    par_var = input_par_var.copy()

    if (
        input_paranalysis_mode == 0
    ):  # If running in ref mode, keep only the ref parameter set
        par_var = par_var.loc[par_var.name_run == "ref", :]

    # TODO: (meas) the original code does not query for clims,
    # but without it the code will crash if not all years have been run
    clims_int = list(map(int, input_clims))
    print("Years of data available: " + str(clims_int))
    s_runs = input_s_runs.query("clim in @clims_int")

    for s_run in s_runs.itertuples():
        for arch in input_archs:
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
            if input_popfix is True:
                suff2 = "ssp2_" + str(s_run.year)
            else:
                suff2 = (
                    str(s_run.scen) + "_" + str(s_run.year)
                )  # suffix: only scen and year
            suff3 = (
                str(s_run.scen) + "_" + str(s_run.year) + "_" + str(s_run.clim)
            )  # suffix: scen, year and arch

            print("Starting " + suff)

            # Load country data
            country_data = xr.open_dataset(
                os.path.join(country_maps_path, "country_data_maps_" + suff3 + ".nc")
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
            if input_floor_setting == "std_cap":
                floorarea = xr.open_dataset(
                    os.path.join(
                        country_maps_path,
                        "floor_area_map_" + input_floor_setting + ".nc",
                    )
                )
            elif input_floor_setting == "per_cap":
                floorarea = xr.open_dataset(
                    os.path.join(
                        country_maps_path,
                        "floor_area_map_" + input_floor_setting + "_" + suff3 + ".nc",
                    )
                )

            # Construction shares
            # constr = xr.open_dataset(input_folder+'map_constr_floorarea\\constr_share_urbanrural.nc')

            # =============================================================================
            # First apply population weighting to energy demand:
            # =============================================================================

            for parset in par_var.itertuples():
                if input_cool == 1:
                    Nd1 = xr.open_dataarray(
                        os.path.join(
                            ene_calcs_path,
                            suff1 + "_" + str(parset.Index) + "_NdALL.nc",
                        )
                    ).load()  # For calculation
                    # Nd = Nd.sum(dim=['arch','month'])/2    #<<< divide by 2 because 2 archetypes
                    Nd1 = (
                        Nd1.sum(dim=["urt", "month"]) / 2
                    )  # <<< divide by 2 (rural + urban)

                    Ndr = xr.open_dataarray(
                        os.path.join(
                            ene_calcs_path,
                            suff1 + "_" + str(parset.Index) + "_NdALL.nc",
                        )
                    ).load()  # For reporting
                    # Nd = Nd.sum(dim=['arch','month'])/2    #<<< divide by 2 because 2 archetypes
                    # Nd = Nd.sum(dim=['month'])    #<<< divide by 2 (rural + urban)

                    Nfr = xr.open_dataarray(
                        os.path.join(
                            ene_calcs_path,
                            suff1 + "_" + str(parset.Index) + "_NfALL.nc",
                        )
                    ).load()
                    # Nf = Nf.sum(dim=['arch','month'])/2    #<<< divide by 2 because 2 archetypes
                    # Nf = Nf.sum(dim=['month'])  #<<< divide by 2 (rural + urban)

                    E_c_ac = (
                        xr.open_dataarray(
                            os.path.join(
                                ene_calcs_path,
                                suff1 + "_" + str(parset.Index) + "_E_c_ac" + "ALL.nc",
                            )
                        ).where(Nd1 >= input_nd_thresh)
                    ).load()
                    E_c_fan = (
                        xr.open_dataarray(
                            os.path.join(
                                ene_calcs_path,
                                suff1 + "_" + str(parset.Index) + "_E_c_fan" + "ALL.nc",
                            )
                        ).where(Nd1 >= input_nd_thresh)
                    ).load()
                    E_c = E_c_ac + E_c_fan
                    vdd_c_in = xr.open_dataarray(
                        os.path.join(
                            ene_calcs_path,
                            suff1 + "_" + str(parset.Index) + "_vdd_tmax_cALL.nc",
                        )
                    ).load()

                if input_heat == 1:
                    E_h = (
                        xr.open_dataarray(
                            os.path.join(
                                ene_calcs_path,
                                suff1 + "_" + str(parset.Index) + "_E_h" + "ALL.nc",
                            )
                        ).where(Nd1 >= 0)
                    ).load()  # ******************************
                    vdd_h_in = xr.open_dataarray(
                        os.path.join(
                            ene_calcs_path,
                            suff1 + "_" + str(parset.Index) + "_vdd_hALL.nc",
                        )
                    ).load()
                #    vdd_in = vdd_in.sum(dim='arch')

                #    if (runsdd==1) and (parset.Index==0):
                #        for bal_temp in bal_temps:
                #            sdd_c_in = xr.open_dataarray(input_folder2+str(parset.Index)+'_s_cddALL.nc').load()
                #            sdd_h_in = xr.open_dataarray(input_folder2+str(parset.Index)+'_s_hddALL.nc').load()
                #        print(xkss)

                if input_cool == 1:
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

                if input_heat == 1:
                    # Heating outputs
                    E_h_perpix = (
                        xr.Dataset()
                    )  # Pixel / person per gridsquare energy demand
                    E_h_popwei = xr.Dataset()  # population weighted (FULL demand)

                    P_h_potential = xr.Dataset()

                    vdd_h_popwei = xr.Dataset()  # Degree Days multiplied by population
                #    sdd_h = xr.Dataset()

                # % Produce spatial results
                for urt in input_urts:
                    if input_cool == 1:
                        if urt == "urban":
                            # at = au
                            acp = "AC_penetr_U"
                        elif urt == "rural":
                            # at = ar
                            acp = "AC_penetr_R"

                        #        print(sddddd)
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

                    if input_heat == 1:
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
                    #            print('do nothing')
                    #        else:
                    ds = eval(var)
                    # ds['arch'] = al
                    ds["urt"] = input_urts
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

                    encoding = {vari: input_comp for vari in ds.data_vars}
                    fname = suff + "_" + str(parset.Index) + "_" + var + ".nc"
                    filestr = os.path.join(final_maps_path, fname)
                    ds.to_netcdf(filestr, encoding=encoding)

                    if varundic[var][-5:] == "month":
                        dsy = ds.sum(dim="month")
                        for urt in input_urts:
                            #                    if (runsdd==1) and (var == 'sdd_c' or var=='sdd_h'):
                            #                        dsy[urt] = (['bal_temp', 'lat','lon',], dsy[urt].values) #.sum(dim='arch')
                            #                    else:
                            dsy[urt] = (
                                ["lat", "lon"],
                                dsy[urt].values,
                            )  # .sum(dim='arch')

                        dsy.attrs = ds.attrs
                        dsy.attrs["units"] = varundic[var][:-5] + "year"
                        encoding = {vari: input_comp for vari in dsy.data_vars}
                        fname = suff + "_" + str(parset.Index) + "_" + var + "_year.nc"
                        filestr = os.path.join(final_maps_path, fname)
                        dsy.to_netcdf(filestr, encoding=encoding)


# Updated function to create ISO tables
def process_iso_tables_updated(
    input_dle_path,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
    input_vstrcntry,
    input_cool,
    input_heat,
    input_clims,
    input_s_runs,
    input_archs,
    input_urts,
    input_par_var,
):
    start = datetime.datetime.now()

    updated_urts = input_urts + ["total"]
    input_path = os.path.join(input_dle_path, "input_data")

    final_maps_path = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "4_final_maps",
    )

    iso_tables_path = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "5_ISO_tables",
    )

    vardic = {}
    varundic = {}

    # Update dictionaries if input_cool == 1
    if input_cool == 1:
        vardic.update(VARDICT_COOL)
        varundic.update(VARUNDICT_COOL)
    # Update dictionaries if input_heat == 1
    if input_heat == 1:
        vardic.update(VARDICT_HEAT)
        varundic.update(VARUNDICT_HEAT)

    varlist_cool = [key for key in VARDICT_COOL.keys()]
    varlist_heat = [key for key in VARDICT_HEAT.keys()]

    # TODO: (meas) the original code does not query for clims,
    # but without it the code will crash if not all years have been run
    clims_int = list(map(int, input_clims))
    print("Years of data available: " + str(clims_int))
    s_runs = input_s_runs.query("clim in @clims_int")

    # Read raster data
    raster = xr.open_dataarray(os.path.join(input_path, "gaul_lvl0_hybrid_05_3.nc"))

    # Read country data
    dfd = pd.read_csv(
        os.path.join(input_path, "gaul_lvl0_raster0.5.csv"), index_col="ID"
    ).assign(ISONUM=lambda x: x.index)

    # Import MESSAGE regions and North/South classification
    msgNS = pd.read_excel(
        os.path.join(input_path, "country_data_" + input_vstrcntry + ".xlsx"),
        sheet_name="ssp2_2010",
    )

    # Add 'GLOBAL_SOUTH' and 'REGION_GEA' to dfd
    dfd = dfd.merge(
        msgNS.reindex(columns=["ISO", "GLOBAL_SOUTH", "REGION_GEA"]),
        left_on="ISO3",
        right_on="ISO",
    ).set_index("ISONUM")

    # Load population data
    print("Opening population data....")
    l_popdata = []
    for s_run in s_runs.itertuples():
        suff = str(s_run.scen) + "_" + str(s_run.year)
        print(suff)

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
        input_archs,
        updated_urts,
        input_par_var.itertuples(),
        varlist_cool,
    )
    inputs_heat = product(
        s_runs.itertuples(),
        input_archs,
        updated_urts,
        input_par_var.itertuples(),
        varlist_heat,
    )

    def aggregate_ncfile(args: tuple) -> xr.Dataset:
        s_run, arch, urt, parset, varname = args
        str_varname = str(varname)

        print(
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
                    final_maps_path,
                    suff + "_" + str(parset.Index) + "_" + varname + ".nc",
                )
            )[urt]

        elif urt == "total":
            varname = (
                xr.open_dataset(
                    os.path.join(
                        final_maps_path,
                        suff + "_" + str(parset.Index) + "_" + varname + ".nc",
                    )
                )["urban"]
                + xr.open_dataset(
                    os.path.join(
                        final_maps_path,
                        suff + "_" + str(parset.Index) + "_" + varname + ".nc",
                    )
                )["rural"]
            )

        # If varname is Nd or Nf, then take the mean. Otherwise, take the sum
        if str_varname in ["Nd", "Nf"]:
            # Group varname by raster index and take the mean
            print("...Aggregating data by raster")
            agg_ras_month = (
                varname.groupby(raster).mean().to_dataframe(name="value").reset_index()
            )

            # Group by gaul_lvl0 and take the mean
            print(".....Aggregating data by gaul_lvl0")
            agg_gaul_lvl0 = (
                agg_ras_month.groupby("gaul_lvl0")["value"]
                .agg(lambda x: x.mean() / 2)
                .reset_index()
            )
        else:
            # Group varname by raster index and sum
            print("...Aggregating data by raster")
            agg_ras_month = (
                varname.groupby(raster).sum().to_dataframe(name="value").reset_index()
            )

            # Group by gaul_lvl0 and sum
            print(".....Aggregating data by gaul_lvl0")
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
            gcm=str(input_gcm),
            scenario=str(input_rcp_scenario),
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

    if input_cool == 1:
        list_cool = list(map(aggregate_ncfile, inputs_cool))
        df_agg = (
            pd.concat(list_cool)
            .reset_index(drop=True)
            .merge(dfd, left_on="gaul_lvl0", right_on="ISONUM")
        )
    if input_heat == 1:
        list_heat = list(map(aggregate_ncfile, inputs_heat))
        df_heat = (
            pd.concat(list_heat)
            .reset_index(drop=True)
            .merge(dfd, left_on="gaul_lvl0", right_on="ISONUM")
        )
        # Add df_heat to df_agg
        df_agg = df_agg.append(df_heat, ignore_index=True)

    print("Completed aggregating raster data! Now processing and saving...")

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
    if input_cool == 1:
        df_agg_wide = df_agg_wide.assign(vdd_c_avg=lambda x: x.vdd_c_popwei / x.popsum)
    if input_heat == 1:
        df_agg_wide = df_agg_wide.assign(vdd_h_avg=lambda x: x.vdd_h_popwei / x.popsum)

    # Drop and rename columns
    df_agg_wide = df_agg_wide.drop(
        columns=["ADM0_CODE", "CONTINENT", "FAO_CODE", "ISO3", "UN_CODE", "UN_REGION"]
    ).rename(columns={"gaul_lvl0": "id", "ADM0_NAME": "NAME"})

    # Save to excel and csv
    df_agg_wide.to_excel(
        os.path.join(
            iso_tables_path,
            "ISO_agg_data_" + input_version_name + ".xlsx",
        )
    )
    df_agg_wide.to_csv(
        os.path.join(
            iso_tables_path,
            "ISO_agg_data_" + input_version_name + ".csv",
        ),
        index=False,
    )

    end = datetime.datetime.now()
    print(
        "Done! Total time to aggregate variables and process ISO tables: "
        + str(end - start)
    )


# (Previous) Function to create ISO tables
def process_iso_tables(
    input_dle_path,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
    input_vstrcntry,
    input_cool,
    input_heat,
    input_clims,
    input_s_runs,
    input_archs,
    input_urts,
    input_par_var,
):
    """Create ISO tables

    Adapted from `y5_ISO_tables.py`

    """

    updated_urts = input_urts + ["total"]
    input_path = os.path.join(input_dle_path, "input_data")

    final_maps_path = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "4_final_maps",
    )

    iso_tables_path = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "5_ISO_tables",
    )

    vardic = {}
    varundic = {}

    # Update dictionaries if input_cool == 1
    if input_cool == 1:
        vardic.update(VARDICT_COOL)
        varundic.update(VARUNDICT_COOL)
    # Update dictionaries if input_heat == 1
    if input_heat == 1:
        vardic.update(VARDICT_HEAT)
        varundic.update(VARUNDICT_HEAT)

    varlist = [key for key in varundic.keys()]

    dty = float

    # TODO: (meas) the original code does not query for clims,
    # but without it the code will crash if not all years have been run
    list(map(int, input_clims))
    s_runs = input_s_runs.query("clim in @clims_int")

    for s_run in s_runs.itertuples():
        for arch in input_archs:
            suff = (
                str(s_run.scen)
                + "_"
                + str(s_run.year)
                + "_"
                + str(s_run.clim)
                + "_"
                + arch
            )  # suffix
            suff2 = (
                str(s_run.scen) + "_" + str(s_run.year)
            )  # suffix: only scen and year
            (
                str(s_run.scen) + "_" + str(s_run.year) + "_" + str(s_run.clim)
            )  # suffix: scen, year and arch

            print("Starting " + suff)

            # DATA Imports
            raster = xr.open_dataarray(
                os.path.join(input_path, "gaul_lvl0_hybrid_05_3.nc")
            )
            # Open ISO Raster
            dfd = pd.read_csv(
                os.path.join(input_path, "gaul_lvl0_raster0.5.csv"), index_col="ID"
            )  # Read country data
            dfd["ISONUM"] = dfd.index

            ISO_attrs = pd.DataFrame([raster.attrs]).T
            ISO_attrs.columns = ["ISO"]

            # Import MESSAGE regions and North/South classification
            # msgNS = pd.read_csv(input_folder+'country_data_v2.csv')
            # msgNS = pd.read_csv(input_folder+'country_data_v2.csv')
            msgNS = pd.read_excel(
                os.path.join(input_path, "country_data_" + input_vstrcntry + ".xlsx"),
                sheet_name="ssp2_2010",
            )

            # Add 'GLOBAL_SOUTH', 'REGION_GEA' to dfd
            dfd = dfd.merge(
                msgNS[["ISO", "GLOBAL_SOUTH", "REGION_GEA"]],
                left_on="ISO3",
                right_on="ISO",
            )  # , left_index=True)
            dfd.set_index("ISONUM", inplace=True)

            popdata = xr.Dataset()

            for urt in updated_urts:
                popdata[urt] = xr.open_dataarray(
                    os.path.join(
                        input_path,
                        "population",
                        "population",
                        suff2 + "_" + urt + "_hd.nc4",
                    )
                )

            popdata.load()

            # PRODUCE COUNTRY RESULTS

            # PREPARE DATAFRAME "dfl" FOR COUNTRY RESULTS
            # dfl = pd.DataFrame(columns =['ID','ISO','NAME','GLOBAL_SOUTH','REGION_GEA','pop','par_var','name_run','archetype','E_perpix', 'E_popwei','E_popwei_wACcess', 'E_popwei_gap', 'P_ac_potential', 'P_acgap'] )
            # add_cols = ['id','ISO','NAME','GLOBAL_SOUTH','REGION_GEA', 'scen', 'year', 'pop','par_var','name_run','archetype','popsum',]
            add_cols = [
                "id",
                "ISO",
                "NAME",
                "GLOBAL_SOUTH",
                "REGION_GEA",
                "pop",
                "par_var",
                "name_run",
                "popsum",
            ]  # removed 'archetype'

            dfl = pd.DataFrame(columns=add_cols + varlist)

            ct = 0
            start = datetime.datetime.now()
            for parset in input_par_var.itertuples():
                # Import spatial results as dataarrays
                for urt in updated_urts:
                    if (urt == "urban") or (urt == "rural"):
                        if input_cool == 1:
                            E_c_perpix = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff + "_" + str(parset.Index) + "_E_c_perpix.nc",
                                )
                            )[urt]
                            E_c_ac_popwei = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff
                                    + "_"
                                    + str(parset.Index)
                                    + "_E_c_ac_popwei.nc",
                                )
                            )[urt]
                            E_c_fan_popwei = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff
                                    + "_"
                                    + str(parset.Index)
                                    + "_E_c_fan_popwei.nc",
                                )
                            )[urt]
                            E_c_popwei = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff + "_" + str(parset.Index) + "_E_c_popwei.nc",
                                )
                            )[urt]
                            E_c_ac_wAccess = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff
                                    + "_"
                                    + str(parset.Index)
                                    + "_E_c_ac_wAccess.nc",
                                )
                            )[urt]
                            E_c_ac_gap = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff + "_" + str(parset.Index) + "_E_c_ac_gap.nc",
                                )
                            )[urt]
                            E_c_fan_wAccess = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff
                                    + "_"
                                    + str(parset.Index)
                                    + "_E_c_fan_wAccess.nc",
                                )
                            )[urt]
                            E_c_fan_gap = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff + "_" + str(parset.Index) + "_E_c_fan_gap.nc",
                                )
                            )[urt]
                            E_c_wAccess = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff + "_" + str(parset.Index) + "_E_c_wAccess.nc",
                                )
                            )[urt]
                            E_c_gap = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff + "_" + str(parset.Index) + "_E_c_gap.nc",
                                )
                            )[urt]
                            Nf = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff + "_" + str(parset.Index) + "_Nf_year.nc",
                                )
                            )[urt]
                            Nd = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff + "_" + str(parset.Index) + "_Nd_year.nc",
                                )
                            )[urt]
                            vdd_c_popwei = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff
                                    + "_"
                                    + str(parset.Index)
                                    + "_vdd_c_popwei_year.nc",
                                )
                            )[urt]
                            P_c_ac_potential = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff
                                    + "_"
                                    + str(parset.Index)
                                    + "_P_c_ac_potential.nc",
                                )
                            )[urt]
                            P_c_ac_gap = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff + "_" + str(parset.Index) + "_P_c_ac_gap.nc",
                                )
                            )[urt]
                            P_c_fan_gap = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff + "_" + str(parset.Index) + "_P_c_fan_gap.nc",
                                )
                            )[urt]
                            P_c_fanNoAC = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff + "_" + str(parset.Index) + "_P_c_fanNoAC.nc",
                                )
                            )[urt]

                        if input_heat == 1:
                            E_h_perpix = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff
                                    + "_"
                                    + str(parset.Index)
                                    + "_E_h_perpix_year.nc",
                                )
                            )[urt]
                            E_h_popwei = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff
                                    + "_"
                                    + str(parset.Index)
                                    + "_E_h_popwei_year.nc",
                                )
                            )[urt]
                            P_h_potential = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff
                                    + "_"
                                    + str(parset.Index)
                                    + "_P_h_potential.nc",
                                )
                            )[urt]
                            vdd_h_popwei = xr.open_dataset(
                                os.path.join(
                                    final_maps_path,
                                    suff
                                    + "_"
                                    + str(parset.Index)
                                    + "_vdd_h_popwei_year.nc",
                                )
                            )[urt]
                    else:
                        if input_cool == 1:
                            E_c_perpix = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_perpix_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_perpix_year.nc",
                                    )
                                )["rural"]
                            )
                            E_c_ac_popwei = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_ac_popwei_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_ac_popwei_year.nc",
                                    )
                                )["rural"]
                            )
                            E_c_fan_popwei = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_fan_popwei_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_fan_popwei_year.nc",
                                    )
                                )["rural"]
                            )
                            E_c_popwei = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_popwei_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_popwei_year.nc",
                                    )
                                )["rural"]
                            )
                            E_c_ac_wAccess = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_ac_wAccess_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_ac_wAccess_year.nc",
                                    )
                                )["rural"]
                            )
                            E_c_ac_gap = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_ac_gap_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_ac_gap_year.nc",
                                    )
                                )["rural"]
                            )
                            E_c_fan_wAccess = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_fan_wAccess_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_fan_wAccess_year.nc",
                                    )
                                )["rural"]
                            )
                            E_c_fan_gap = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_fan_gap_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_fan_gap_year.nc",
                                    )
                                )["rural"]
                            )
                            E_c_wAccess = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_wAccess_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_wAccess_year.nc",
                                    )
                                )["rural"]
                            )
                            E_c_gap = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_gap_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_c_gap_year.nc",
                                    )
                                )["rural"]
                            )
                            Nf = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff + "_" + str(parset.Index) + "_Nf_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff + "_" + str(parset.Index) + "_Nf_year.nc",
                                    )
                                )["rural"]
                            )
                            Nd = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff + "_" + str(parset.Index) + "_Nd_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff + "_" + str(parset.Index) + "_Nd_year.nc",
                                    )
                                )["rural"]
                            )
                            vdd_c_popwei = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_vdd_c_popwei_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_vdd_c_popwei_year.nc",
                                    )
                                )["rural"]
                            )
                            P_c_ac_potential = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_P_c_ac_potential.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_P_c_ac_potential.nc",
                                    )
                                )["rural"]
                            )
                            P_c_ac_gap = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_P_c_ac_gap.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_P_c_ac_gap.nc",
                                    )
                                )["rural"]
                            )
                            P_c_fan_gap = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_P_c_fan_gap.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_P_c_fan_gap.nc",
                                    )
                                )["rural"]
                            )
                            P_c_fanNoAC = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_P_c_fanNoAC.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_P_c_fanNoAC.nc",
                                    )
                                )["rural"]
                            )

                        if input_heat == 1:
                            E_h_perpix = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_h_perpix_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_h_perpix_year.nc",
                                    )
                                )["rural"]
                            )
                            E_h_popwei = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_h_popwei_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_E_h_popwei_year.nc",
                                    )
                                )["rural"]
                            )
                            P_h_potential = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_P_h_potential.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_P_h_potential.nc",
                                    )
                                )["rural"]
                            )
                            vdd_h_popwei = (
                                xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_vdd_h_popwei_year.nc",
                                    )
                                )["urban"]
                                + xr.open_dataset(
                                    os.path.join(
                                        final_maps_path,
                                        suff
                                        + "_"
                                        + str(parset.Index)
                                        + "_vdd_h_popwei_year.nc",
                                    )
                                )["rural"]
                            )

                    # AGGREGATE RESULTS AT COUNTRY LEVEL
                    for isorow in dfd.itertuples():
                        idx = int(isorow.Index)
                        iso = isorow.ISO3
                        name = isorow.ADM0_NAME
                        NS = isorow.GLOBAL_SOUTH
                        msg = isorow.REGION_GEA

                        #                        dty = float
                        popsum = (
                            popdata[urt]
                            .where(raster == idx)
                            .sum()
                            .values.astype("int32")
                        )
                        # popsum = popdata[scen+urt+year].where(raster==idx).sum().values.astype('int32')
                        if input_cool == 1:
                            e1 = (
                                E_c_perpix.where(raster == idx).sum().values.astype(dty)
                            )
                            e2c = (
                                E_c_ac_popwei.where(raster == idx)
                                .sum()
                                .values.astype(dty)
                            )
                            e2f = (
                                E_c_fan_popwei.where(raster == idx)
                                .sum()
                                .values.astype(dty)
                            )
                            e2 = (
                                E_c_popwei.where(raster == idx).sum().values.astype(dty)
                            )
                            #                e2cp = (e2c / popdata['ssp2'+urt+'2010'].where(raster==idx).sum()).values.astype(dty)
                            #                e2fp = (e2f / popdata['ssp2'+urt+'2010'].where(raster==idx).sum()).values.astype(dty)
                            e3c = (
                                E_c_ac_wAccess.where(raster == idx)
                                .sum()
                                .values.astype(dty)
                            )
                            e3f = (
                                E_c_fan_wAccess.where(raster == idx)
                                .sum()
                                .values.astype(dty)
                            )
                            e3 = (
                                E_c_wAccess.where(raster == idx)
                                .sum()
                                .values.astype(dty)
                            )
                            e4c = (
                                E_c_ac_gap.where(raster == idx).sum().values.astype(dty)
                            )
                            e4f = (
                                E_c_fan_gap.where(raster == idx)
                                .sum()
                                .values.astype(dty)
                            )
                            e4 = E_c_gap.where(raster == idx).sum().values.astype(dty)

                            nd1 = Nd.where(raster == idx).mean().values.astype(dty) / 2
                            nf1 = Nf.where(raster == idx).mean().values.astype(dty) / 2

                            vdc_tot = (
                                vdd_c_popwei.where(raster == idx)
                                .sum()
                                .values.astype(dty)
                            )
                            #                if parset.Index==0:
                            #                    sdc = sdd_c.where(raster==idx).sum().values.astype(dty)
                            #                else:
                            #                    sdc = np.nan
                            p1 = (
                                P_c_ac_potential.where(raster == idx)
                                .sum()
                                .values.astype(dty)
                            )
                            p2 = (
                                P_c_ac_gap.where(raster == idx).sum().values.astype(dty)
                            )
                            p3 = (
                                P_c_fan_gap.where(raster == idx)
                                .sum()
                                .values.astype(dty)
                            )
                            p4 = (
                                P_c_fanNoAC.where(raster == idx)
                                .sum()
                                .values.astype(dty)
                            )

                        if input_heat == 1:
                            eh1 = (
                                E_h_perpix.where(raster == idx).sum().values.astype(dty)
                            )
                            eh2 = (
                                E_h_popwei.where(raster == idx).sum().values.astype(dty)
                            )

                            ph1 = (
                                P_h_potential.where(raster == idx)
                                .sum()
                                .values.astype(dty)
                            )

                            vdh_tot = (
                                vdd_h_popwei.where(raster == idx)
                                .sum()
                                .values.astype(dty)
                            )
                        #                 if parset.Index==0:
                        #                     sdh = sdd_h.where(raster==idx).sum().values.astype(dty)
                        #                else:
                        #                     sdh = np.nan

                        lsel = [
                            idx,
                            iso,
                            name,
                            NS,
                            msg,
                            urt,
                            parset.Index,
                            parset.name_run,  # at,
                            popsum,
                        ]

                        if input_cool == 1:
                            lsel.extend(
                                [
                                    e1,
                                    e2c,
                                    e2f,
                                    e2,
                                    e3c,
                                    e3f,
                                    e3,
                                    e4c,
                                    e4f,
                                    e4,
                                    nd1,
                                    nf1,
                                    vdc_tot,  # sdc,
                                    p1,
                                    p2,
                                    p3,
                                    p4,
                                ]
                            )
                        if input_heat == 1:
                            lsel.extend([eh1, eh2, ph1, vdh_tot])  # , sdh])

                        dfl.loc[ct, :] = lsel

                        ct = ct + 1

                print(datetime.datetime.now() - start)

            # Calculate population-averaged degree days
            if input_cool == 1:
                dfl.insert(len(dfl.columns), "vdd_c_avg", 0)
                dfl["vdd_c_avg"] = dfl["vdd_c_popwei"] / dfl["popsum"]
            if input_heat == 1:
                dfl.insert(len(dfl.columns), "vdd_h_avg", 0)
                dfl["vdd_h_avg"] = dfl["vdd_h_popwei"] / dfl["popsum"]

            # =============================================================================
            #     Write out
            # =============================================================================
            # Convert columns to floats
            for col in dfl.columns[len(add_cols) - 1 :]:  # UPDATE ONLY NUMERIC COLS
                dfl[col] = dfl[col].astype(float)  # CONVERT TO FLOAT

            dfl.to_excel(
                os.path.join(
                    iso_tables_path,
                    "ISO_agg_data_" + input_version_name + "_" + suff + ".xlsx",
                )
            )
            dfl.to_csv(
                os.path.join(
                    iso_tables_path,
                    "ISO_agg_data_" + input_version_name + "_" + suff + ".csv",
                ),
                index=False,
            )
            print(datetime.datetime.now() - start)
            print("Completed " + suff)


# Function to calculate cumulative carbon emissions
def calculate_cumulative_carbon_emissions(
    input_dle_path, input_version_name, input_snapshot_file, input_yeardic
):
    """Calculate cumulative carbon emissions

    Adapted from `y7_process_results_to_CumCarb_table.py`

    """
    print("Calculating cumulative carbon emissions...")

    version_output_path = os.path.join(
        input_dle_path, f"output_data_{input_version_name}", "output"
    )

    varins = [
        "Emissions|CO2",
        "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile",
        "AR6 climate diagnostics|Infilled|Emissions|CO2",
    ]

    msdic = {
        "IMAGE 3.0.1": "SSP1-26",
        "AIM/CGE 2.0": "SSP3-Baseline",
        "REMIND-MAgPIE 1.5": "SSP5-Baseline",
    }

    dfar6 = pyam.IamDataFrame(input_snapshot_file)
    a = dfar6.filter(scenario="SSP*")
    ndf = a.filter(model="xxx")
    for m, s in msdic.items():
        ndf.append(a.filter(model=m, scenario=s, variable=varins), inplace=True)

    ndf.interpolate(range(2000, 2101, 1), inplace=True)

    # ENGAGE scenarios
    scenarios = ["EN_NPi2020_300f", "EN_NPi2020_1400f", "EN_NPi2100"]
    engage = dfar6.filter(model="MESSAGE*", scenario=scenarios, variable=varins)

    # Calculate Cumulative CO2 emissions
    unit = "Gt CO2"

    ts_engage = (
        engage.filter(variable="Emissions|CO2", year=range(2015, 2101))
        .convert_unit(
            "Mt CO2/yr",
            "Gt CO2/yr",
        )
        .timeseries()
    )

    ts_native = (
        ndf.filter(variable="Emissions|CO2", year=range(2015, 2101))
        .convert_unit(
            "Mt CO2/yr",
            "Gt CO2/yr",
        )
        .timeseries()
    )
    ts_infilled = (
        ndf.filter(
            variable="AR6 climate diagnostics|Infilled|Emissions|CO2",
            year=range(2015, 2101),
        )
        .convert_unit(
            "Mt CO2/yr",
            "Gt CO2/yr",
        )
        .timeseries()
    )

    engage_cumCO2 = pd.DataFrame()
    for year in input_yeardic.keys():
        year = int(year)

        df = (
            ts_engage.apply(
                pyam.cumulative, raw=False, axis=1, first_year=2015, last_year=year
            )
            .to_frame()
            .reset_index()
        )
        df["year"] = year
        df["variable"] = f"Cumulative CO2 infilled 2015-{year}, Gt CO2"
        df.rename(columns={0: "value"}, inplace=True)
        engage_cumCO2 = pd.concat([engage_cumCO2, df])
        engage_cumCO2["unit"] = unit

    engage.append(engage_cumCO2, inplace=True)

    df_cumCO2 = pd.DataFrame()
    for year in input_yeardic.keys():
        year = int(year)

        df = (
            ts_native.apply(
                pyam.cumulative, raw=False, axis=1, first_year=2015, last_year=year
            )
            .to_frame()
            .reset_index()
        )
        df["year"] = year
        df["variable"] = f"Cumulative CO2 native 2015-{year}, Gt CO2"
        df.rename(columns={0: "value"}, inplace=True)
        df_cumCO2 = pd.concat([df_cumCO2, df])

        df = (
            ts_infilled.apply(
                pyam.cumulative, raw=False, axis=1, first_year=2015, last_year=year
            )
            .to_frame()
            .reset_index()
        )
        df["year"] = year
        df["variable"] = f"Cumulative CO2 infilled 2015-{year}, Gt CO2"
        df.rename(columns={0: "value"}, inplace=True)

        df_cumCO2 = pd.concat([df_cumCO2, df])

    df_cumCO2["unit"] = unit

    ndf.append(df_cumCO2, inplace=True)

    print("Writing out cumulative carbon emissions to " + version_output_path + "...")

    engage.to_excel(
        os.path.join(version_output_path, "engage_emissions_tempAR6_cumCO2.xlsx")
    )
    engage_cumCO2.to_csv(
        os.path.join(version_output_path, "engage_cumCO2.csv"), index=False
    )

    ndf.to_excel(
        os.path.join(version_output_path, "isimip3b_emissions_tempAR6_cumCO2.xlsx")
    )
    df_cumCO2.to_csv(
        os.path.join(version_output_path, "isimip3b_cumCO2.csv"), index=False
    )


# Function to read in ISO tables and aggregate to region
def aggregate_ISO_tables_to_regions(
    input_dle_path,
    input_version_name,
    input_gcm,
    input_rcp_scenario,
    input_rcps,
    input_archs,
    input_yeardic,
):
    iso_tables_path = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "5_ISO_tables",
    )

    emulator_sub_path = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        input_rcp_scenario,
        "7_emulator",
    )

    emulator_output_path = os.path.join(
        input_dle_path,
        f"output_data_{input_version_name}",
        input_gcm,
        "output_emulator",
    )

    scenario_name_dic = {
        "SSP1-26": "ssp126",
        "SSP3-Baseline": "ssp370",
        "SSP5-Baseline": "ssp585",
    }
    name_run = "tm23ts23"
    model = "MESS-CHILL-URM"

    for rcp in input_rcps:
        # fo5 = iso_tables_path.replace("ssp126", rcp)
        # rcp = 'ssp126'
        for arch in input_archs:
            df = pd.DataFrame()

            for year in input_yeardic.keys():
                suff = f"ssp2_{year}_{year}_{arch}"

                data = pd.read_csv(
                    os.path.join(
                        iso_tables_path,
                        "ISO_agg_data_"
                        + input_version_name
                        + "_"
                        + input_gcm
                        + "_"
                        + suff
                        + ".csv",
                    )
                )

                data.rename(columns={"REGION_GEA": "region"}, inplace=True)
                data = data.loc[data["pop"] == "total"]

                # print(data.head())

                gbcols = [
                    "pop",
                    "name_run",
                    "region",
                ]
                gb = data.groupby(gbcols).sum()[
                    ["popsum", "E_c_perpix", "E_c_ac_popwei"]
                ]

                # print(gb.head())

                # Convert  to Av EI/m2
                gb["EI_ac_m2"] = gb["E_c_ac_popwei"] / (
                    gb["popsum"] * 10
                )  # 10 because 10m2 per person assumed
                gb.reset_index(inplace=True)
                gb["year"] = year

                if len(df) == 0:
                    write_out_pop = True
                else:
                    write_out_pop = True

                df = pd.concat([df, gb])

                # print(df.head())

                df["scenario"] = rcp

                # print(df)

                # Write out the regional sum of population, first time
                if write_out_pop is True:
                    out_pop = df.copy(deep=True)[["region", "popsum", "year"]]
                    out_pop[["model", "scenario", "variable", "unit"]] = [
                        model,
                        "SSP2",
                        "Population",
                        "People",
                    ]
                    out_pop.rename(columns={"popsum": "value"}, inplace=True)
                    out_pop.drop_duplicates(inplace=True)
                    out_pop.to_csv(
                        os.path.join(emulator_output_path, "population_by_region.csv"),
                        index=False,
                    )

            # Filter and split files by name_run

            for name_run in df.name_run.unique():
                dff = df.loc[df.name_run == name_run]
                dff = dff[["scenario", "region", "year", "EI_ac_m2"]]

                fo = emulator_sub_path.replace(input_rcp_scenario, rcp)
                fstr = os.path.join(
                    fo,
                    "region_agg_EI_ac_m2_" + rcp + "_" + arch + "_" + name_run + ".csv",
                )
                print(fstr)
                dff.to_csv(fstr, index=False)

    # TODO: split this part onwards into a separate function
    # %% Join new Region tables to cumulative carbon data

    df_cumCO2 = pd.read_csv(os.path.join(emulator_output_path, "isimip3b_cumCO2.csv"))
    df_cumCO2.scenario.replace(scenario_name_dic, inplace=True)
    df_cumCO2 = df_cumCO2.loc[df_cumCO2.variable.str.contains("infilled")]

    # Add baseline dummy data, constant for 2015

    runs = ["noCC", "CC"]

    dfei = pd.DataFrame()
    EI_cumCO2 = pd.DataFrame()

    name_run = "tm23ts23"

    for arch in input_archs[::-1]:
        for rcp in input_rcps:
            fo = emulator_sub_path.replace(input_rcp_scenario, rcp)
            fstr = os.path.join(
                fo, "region_agg_EI_ac_m2_" + rcp + "_" + arch + "_" + name_run + ".csv"
            )
            dft = pd.read_csv(fstr)
            dft["stock"] = arch
            dfei = pd.concat([dfei, dft])

    dfm1 = pd.merge(
        left=dfei,
        right=df_cumCO2[["scenario", "value", "year"]],
        left_on=["scenario", "year"],
        right_on=["scenario", "year"],
        how="left",
    )

    EI_cumCO2 = pd.concat([EI_cumCO2, dfm1])

    EI_cumCO2.rename(columns={"value": "cumCO2"}, inplace=True)
    EI_cumCO2.to_csv(
        os.path.join(emulator_output_path, "regional_EI_cumCO2_pre-regress.csv"),
        index=False,
    )

    # %%
    model = "MESS-CHILL-URM"
    unit = "MJ / m2 / yr"

    def return_EI_from_emissions(
        regr,
        data,
        arch=arch,
        time="year",
        cumCO2="value",
        model="MESS-CHILL-URM",
        unit="MJ / m2 / yr",
    ):
        """""
        Input:
            regr:  regression coefficents per region,
            data: input timeseries of cumulative carbon emissions per region

        Returns:
            Energy Intensity through time (MJ)

        """ ""
        df_all_years = pd.DataFrame()
        for year in data[time].unique():
            # Multiply slope by CumCO2
            df = pd.DataFrame(
                np.outer(data.value, regr.slope), index=data.year, columns=regr.index
            )
            # Add the intercept value
            df = df.add(regr.intercept)
            df = df.reset_index().melt(id_vars="year")
            dfn = pd.DataFrame(columns=pyam.IAMC_IDX + ["year", "value"])
            dfn = pd.concat([dfn, df])

            variable = f"Elec_Intensity|AC|{arch}"

            dfn[["model", "scenario", "variable", "unit"]] = [
                model,
                data.scenario.unique()[0],
                variable,
                unit,
            ]

            # dfn['arch'] = arch
            df_all_years = pd.concat([df_all_years, dfn])

            return df_all_years

    def grouped_linReg(x, y):
        """linear regression using numpy starting from two one dimensional numpy arrays"""
        A = np.vstack([x, np.ones(len(x))]).T
        slope, intercept = np.linalg.lstsq(A, y, rcond=None)[0]
        return pd.Series({"slope": slope, "intercept": intercept})

    def scenarios_through_emulator(
        scenario_data,
        EI_cumCO2_training_data,
        plot_regressions=True,
        palette="Spectral",
    ):
        """
        Main emulator wrapper
            Input:
                scenario_data: global cumulative emissions scenarios, by year, as pyam.IamDF
                EI_cumCO2_training data: Regional daat combining regional Energy Intensities with global cumCO2 data.
            Output: Energy Intensity by year
        """
        # Select only the relevant scenarios
        EI_ensemble = pd.DataFrame()

        scenarios = scenario_data.scenario.unique()
        # For each scenario
        for scenario in scenarios:
            scen_cumCO2 = scenario_data.loc[scenario_data.scenario == scenario]

            # By archetype - e.g. new or exist
            for arch in input_archs:
                # Only use SSP data
                regr_input_data = EI_cumCO2_training_data.loc[
                    (EI_cumCO2_training_data.scenario.str.startswith("ssp"))
                    & (EI_cumCO2_training_data.stock == arch)
                ][
                    [
                        "region",
                        "cumCO2",
                        "EI_ac_m2",
                    ]
                ]
                # regr = regr.pivot_table(values='EI_ac_m2', index='cumCO2', columns='region')

                # Get the REGRESSION parameters - drop the year
                regr = regr_input_data.groupby("region").apply(
                    lambda x: grouped_linReg(x.cumCO2, x.EI_ac_m2)
                )

                if plot_regressions:
                    plot_regression_params(
                        regr, EI_cumCO2_training_data, arch, palette=palette
                    )

                # Iterate through the years and provide EU by region
                EI_pathway = return_EI_from_emissions(
                    regr,
                    scen_cumCO2,
                    arch=arch,
                )

                EI_ensemble = pd.concat([EI_ensemble, EI_pathway])

        return EI_ensemble, regr

    def plot_regression_params(
        regr, EI_cumCO2_training_data, arch, cco2_max=8001, palette="Spectral"
    ):
        cco2 = np.arange(0, cco2_max, 50)
        mx = np.outer(cco2, regr.slope)
        c = np.tile(regr.intercept.to_numpy(), (len(cco2), 1))
        y = mx + c
        y = pd.DataFrame(y, columns=regr.index, index=cco2)

        fig = plt.figure()
        fig.add_subplot(111)

        # y.plot(cmap=sns.color_palette(palette, as_cmap=True),
        #     legend={'bbox_to_anchor':(1.02, 1.0)}, ax=ax)

        # sns.scatterplot(data=EI_cumCO2_training_data.loc[EI_cumCO2_training_data.stock==arch], x='cumCO2', y='EI_ac_m2', hue='region', ax=ax,
        #                 palette=palette, legend=False)

        # ax.set_title(f'Regional Energy Intensity to cumulative CO2: {arch}')
        # ax.set_ylabel('MJ/m2/yr')
        # ax.set_xlabel(' Cumulative global GtCO2 from 2015')
        # plt.tight_layout()
        # fstr = f'{output_folder_emulator}Regr_regional_{arch}.png'
        # ax.set_ylim([0,1200])

        # plt.ylabel('MJ/m2/yr')
        # plt.xlabel(' Cumulative global GtCO2 from 2015')
        # plt.tight_layout()
        # fstr = f'{output_folder_emulator}Regr_regional_{arch}.png'
        # plt.ylim([0,1200])
        plt.savefig(fstr, bbox_inches="tight")
        # plt.close()

    # Load engage emissions
    engage_cumCO2 = pyam.IamDataFrame(
        os.path.join(emulator_output_path, "engage_emissions_tempAR6_cumCO2.xlsx")
    )
    engage_cumCO2 = engage_cumCO2.filter(
        variable="Cumulative CO2 infilled*"
    ).as_pandas()
    engage_cumCO2["variable"] = "Cumulative Emissions|CO2"

    # % Add regional populations
    scenarios = ["EN_NPi2020_300f", "EN_NPi2020_1400f", "EN_NPi2100"]
    popdata = pyam.IamDataFrame(
        os.path.join(emulator_output_path, "population_by_region.csv")
    )
    for scenario in scenarios:
        popnew = popdata.rename({"scenario": {"SSP2": scenario}})
        popdata.append(popnew.filter(scenario=scenario), inplace=True)
    popdata.filter(scenario="SSP2", keep=False, inplace=True)

    EI_cumCO2 = pd.read_csv(
        os.path.join(emulator_output_path, "regional_EI_cumCO2_pre-regress.csv")
    )

    # Put ensemble
    runs = ["noCC", "CC"]
    palette = "Paired"
    # palette = sns.color_palette(palette_str, as_cmap=True)

    for runcc in runs:
        if runcc == "CC":
            EI_ensemble, regr = scenarios_through_emulator(
                engage_cumCO2, EI_cumCO2, palette=palette, plot_regressions=False
            )
            EI_ensemble = pyam.IamDataFrame(EI_ensemble)

        elif runcc == "noCC":
            EI_ensemble = EI_cumCO2.drop("cumCO2", axis=1)
            EI_ensemble[["model", "unit"]] = [model, unit]
            EI_ensemble = EI_ensemble.loc[EI_ensemble.scenario == "baseline"]
            EI_ensemble["variable"] = np.where(
                EI_ensemble.stock == "new",
                "Elec_Intensity|AC|new",
                "Elec_Intensity|AC|exist",
            )
            EI_ensemble.rename(columns={"EI_ac_m2": "value"}, inplace=True)
            EI_ensemble.drop("stock", axis=1, inplace=True)
            EI_ensemble = pyam.IamDataFrame(EI_ensemble)

            for scenario in scenarios:
                EI_ensemblenew = EI_ensemble.rename(
                    {"scenario": {"baseline": scenario}}
                )
                EI_ensemble.append(
                    EI_ensemblenew.filter(scenario=scenario), inplace=True
                )
            EI_ensemble.filter(scenario="baseline", keep=False, inplace=True)

        EI_ensemble.append(popdata, inplace=True)

        varis = list(EI_ensemble.variable)
        varis.remove("Population")
        for vari in varis:
            EI_ensemble.aggregate_region(
                variable=vari,
                region="World",
                method="sum",
                weight="Population",
                append=True,
            )

        # interpolate years
        EI_ensemble.interpolate(range(2010, 2061, 5), inplace=True)
        EI_ensemble.rename({"region": {"LAC": "LAM"}}, inplace=True)
        # Add new/exist
        EI_ensemble_long = EI_ensemble.as_pandas()
        EI_ensemble_long["stock"] = EI_ensemble_long["variable"].str.split("|").str[-1]

        EI_ensemble_long.to_csv(
            os.path.join(
                emulator_output_path, "REGIONAL_EI_PATHWAYS_" + runcc + "_long.csv"
            ),
            index=False,
        )
        EI_ensemble.to_csv(
            os.path.join(
                emulator_output_path, "REGIONAL_EI_PATHWAYS_" + runcc + "_IAMC.csv"
            )
        )

        for vari in varis:
            EI_ensemble.filter(variable=vari).plot(
                x="year",
                y="value",
                color="region",
                fill_between=True,
                linestyle=":",
                final_ranges=True,
                cmap=palette,
                legend={"bbox_to_anchor": (1.02, 1.0)},
            )
            plt.ylim([0, 1100])
            plt.tight_layout()
            x = vari.split("|")[2]
            fstr = os.path.join(emulator_output_path, x + "_" + runcc + ".png")
            plt.savefig(fstr)
            plt.close()
