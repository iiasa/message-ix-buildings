"""
Create buildings archetypes maps using specified inputs
"""

import datetime
import os

import numpy as np
import pandas as pd
import xarray as xr
from utils.config import Config  # type: ignore
from utils.util import get_project_root  # type: ignore


def create_archetypes(config: "Config"):
    root_path = get_project_root()
    raster_path = os.path.join(config.dle_path, "out", "raster")
    version_path = os.path.join(root_path, "data", "chilled", "versions", config.vstr)
    archetype_path = os.path.join(raster_path, "archetypes", config.vstr)

    # if archetypes folder does not exist, create it
    if not os.path.exists(archetype_path):
        os.makedirs(archetype_path)

    # read in message raster
    reg_ras = xr.open_dataarray(
        os.path.join(raster_path, "map_reg_MESSAGE_" + config.node + ".nc")
    )

    for arch in config.archs:
        suff = arch  # suffix

        if config.arch_setting == "fixed":
            input_file = os.path.join(version_path, "arch_input.xlsx")

            if os.path.exists(input_file):
                arch_inputs = pd.read_excel(input_file, sheet_name=suff, index_col="id")
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

            reg_file = os.path.join(
                version_path,
                "arch_regions.xlsx",
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
                arch_map["urban"].values[reg_ras == row.RegNum] = float(row.urban)
                arch_map["rural"].values[reg_ras == row.RegNum] = float(row.rural)

            arch_map = arch_map.astype(float)
            # NOT WORKING with integers!!
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

            filename = "arch_map_" + config.arch_setting + "_" + suff + ".nc"
            arch_map.to_netcdf(
                os.path.join(
                    archetype_path,
                    filename,
                ),
                encoding=encoding,
            )

            print(
                "Completed archetype map at "
                + os.path.join(
                    archetype_path,
                    filename,
                )
            )
