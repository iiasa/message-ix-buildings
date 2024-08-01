import os
from itertools import product

import pandas as pd
import xarray as xr

from message_ix_buildings.chilled.functions.variable_dicts import (
    VARDICT_COOL,
    VARDICT_HEAT,
    VARUNDICT_COOL,
    VARUNDICT_HEAT,
)

vstr = cfg.vstr
vstrcntry = cfg.vstrcntry
gcm = cfg.gcm
rcp_scenario = cfg.rcp
archs = cfg.archs
cool = cfg.cool
heat = cfg.heat
clims = cfg.clims
s_runs = cfg.s_runs
archs = cfg.archs
urts = cfg.urts
par_var = cfg.par_var


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
    )  # noqa

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
        suff = "ssp2_" + str(s_run.year)
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

    # Merge df_agg with pop_agg
    df_agg2 = df_agg.assign(year=lambda x: x.year.astype(int)).merge(
        pop_agg,
        left_on=["gaul_lvl0", "urt", "year"],
        right_on=["gaul_lvl0", "urt", "year"],
    )

    return pop_agg, df_agg, df_agg2


pop_agg, df_agg, df_agg2 = process_iso_tables_updated(
    dle_path,
    vstr,
    gcm,
    rcp_scenario,
    vstrcntry,
    cool,
    heat,
    clims,
    s_runs,
    archs,
    urts,
    par_var,
)
