"""
Create buildings archetypes maps using specified inputs
"""

import datetime
import os
from itertools import product

import numpy as np
import pandas as pd  # type: ignore
import xarray as xr
from functions.variable_dicts import VARS_ARCHETYPES  # type: ignore
from preprocess.message_raster import create_message_raster  # type: ignore
from utils.config import Config  # type: ignore
from utils.util import get_project_root  # type: ignore


def read_arch_inputs_df(config: "Config", suff: str):
    root_path = get_project_root()
    version_path = os.path.join(root_path, "data", "chilled", "version", config.vstr)

    if config.arch_setting == "fixed":
        input_file = os.path.join(version_path, "arch_input.xlsx")

        if os.path.exists(input_file):
            arch_inputs = pd.read_excel(input_file, sheet_name=suff, index_col="id")

            return arch_inputs
        else:
            print(
                "Archetypes input file "
                + input_file
                + " does not exist! Please create file for input."
            )
    elif config.arch_setting == "regional":
        input_file = os.path.join(
            version_path,
            "arch_input_reg.xlsx",
        )

        if os.path.exists(input_file):
            arch_inputs = pd.read_excel(input_file, sheet_name="arch")

            return arch_inputs
        else:
            print(
                "Archetypes input file "
                + input_file
                + " does not exist! Please create file for input."
            )


def read_arch_reg_df(config: "Config", arch: str):
    root_path = get_project_root()
    version_path = os.path.join(root_path, "data", "chilled", "version", config.vstr)

    if config.arch_setting == "regional":
        reg_file = os.path.join(
            version_path,
            "arch_regions.xlsx",
        )

        if os.path.exists(reg_file):
            arch_reg = pd.read_excel(reg_file, sheet_name=arch)
            return arch_reg
        else:
            print(
                "Regional archetypes input file "
                + reg_file
                + " does not exist! Please create file for input."
            )

    else:
        print("Archetypes are not regional. No regional file to read.")


def create_archetypes(config: "Config"):
    out_path = os.path.join(config.dle_path, "out", "version")
    archetype_path = os.path.join(out_path, config.vstr, "rasters")

    # if archetypes folder does not exist, create it
    if not os.path.exists(archetype_path):
        os.makedirs(archetype_path)

    # get raster file and message map
    ras, map_reg = create_message_raster(config)

    # save MESSAGE regions map
    msg_file = "map_reg_MESSAGE_" + config.node + ".nc"
    map_reg.to_netcdf(os.path.join(archetype_path, msg_file))
    print(
        "- Saved MESSAGE and raster map data to "
        + os.path.join(
            archetype_path,
            msg_file,
        )
    )

    for arch in config.archs:
        arch_reg = read_arch_reg_df(config, arch)

        # Create map of archetypes based on MESSAGE regions raster
        arch_map = xr.Dataset(
            {
                "urban": ras.MESSAGE11.copy(deep=True),
                "rural": ras.MESSAGE11.copy(deep=True),
            }
        )

        for row in arch_reg.itertuples():
            arch_map["urban"].values[map_reg == row.RegNum] = float(row.urban)
            arch_map["rural"].values[map_reg == row.RegNum] = float(row.rural)

        arch_map = arch_map.astype(float)

        # TODO: NOT WORKING with integers!!
        for urt in config.urts:
            arch_map[urt].values[arch_map[urt] < 0] = np.nan

        # % Write out to netcdf

        arch_map.attrs = {
            "title": "Archetype IDs by region",
            "authors": "Edward Byers & Alessio Mastrucci",
            "date": str(datetime.datetime.now()),
            "institution": "IIASA Energy Program",
            "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
            "arch_setting": config.arch_setting,
        }

        encoding = {var: config.comp for var in arch_map.data_vars}

        arch_file = "arch_map_" + config.arch_setting + "_" + arch + ".nc"
        arch_map.to_netcdf(
            os.path.join(
                archetype_path,
                arch_file,
            ),
            encoding=encoding,
        )

        print(
            "-- Saved archetype map to "
            + os.path.join(
                archetype_path,
                arch_file,
            )
        )


def create_archetype_variables(config: "Config"):
    out_path = os.path.join(config.dle_path, "out", "version")
    archetype_path = os.path.join(out_path, config.vstr, "rasters")

    def map_archetype_variables(args):
        arch_setting, arch, varname = args

        print(
            "Creating archetype map for: " + arch_setting + " " + arch + " " + varname
        )

        # read in input files
        arch_inputs = read_arch_inputs_df(config, arch)

        # read in relevant archetype raster
        map = xr.open_dataset(
            os.path.join(
                archetype_path, "arch_map_" + arch_setting + "_" + arch + ".nc"
            )
        )

        # print(".....Writing to netCDF")
        for urt in config.urts:
            for index, row in arch_inputs.iterrows():
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

        encoding = {var: config.comp for var in map.data_vars}

        # save archetype variables maps to netcdf
        filename = "arch_" + arch + "_" + varname + ".nc"

        map.to_netcdf(
            os.path.join(archetype_path, filename),
            encoding=encoding,
        )

        print(
            ".......Completed writing to file: "
            + os.path.join(archetype_path, filename)
        )

    # create archetype variables maps

    func_inputs = product(config.arch_settings, config.archs, VARS_ARCHETYPES)

    list(map(map_archetype_variables, func_inputs))
