"""
Create buildings archetypes maps using specified inputs
"""

import datetime
import os
from itertools import product

import numpy as np
import xarray as xr

from message_ix_buildings.chilled.functions.variable_dicts import (
    VARS_ARCHETYPES,  # type: ignore
)
from message_ix_buildings.chilled.preprocess.message_raster import (
    create_message_raster,  # type: ignore
)
from message_ix_buildings.chilled.util.base import (
    get_archs,
    get_paths,
    read_arch_inputs_df,
    read_arch_reg_df,
)
from message_ix_buildings.chilled.util.common import get_logger
from message_ix_buildings.chilled.util.config import Config  # type: ignore

log = get_logger(__name__)


def create_archetypes(config: "Config"):
    project_path = get_paths(config, "project_path")

    out_path = os.path.join(project_path, "out", "version")
    archetype_path = os.path.join(out_path, config.vstr, "rasters")

    # if archetypes folder does not exist, create it
    if not os.path.exists(archetype_path):
        os.makedirs(archetype_path)

    # get raster file and message map
    country_ras, reg_ras, map_reg, iso_attrs = create_message_raster(config)

    # get archs
    vers_archs = get_archs(config)

    # save MESSAGE regions map
    msg_file = "map_reg_MESSAGE_" + config.node + ".nc"
    map_reg.to_netcdf(os.path.join(archetype_path, msg_file))

    log.info(
        "- Saved MESSAGE and raster map data to "
        + os.path.join(
            archetype_path,
            msg_file,
        )
    )

    for arch in vers_archs:
        arch_reg = read_arch_reg_df(config).query("arch == @arch")

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

        log.info(
            "-- Saved archetype map to "
            + os.path.join(
                archetype_path,
                arch_file,
            )
        )


def create_archetype_variables(config: "Config"):
    project_path = get_paths(config, "project_path")

    out_path = os.path.join(project_path, "out", "version")
    archetype_path = os.path.join(out_path, config.vstr, "rasters")

    # get archs
    vers_archs = get_archs(config)

    def map_archetype_variables(args):
        arch_setting, arch, varname = args

        log.info(
            "Creating archetype map for: " + arch_setting + " " + arch + " " + varname
        )

        # read in input files
        arch_inputs = read_arch_inputs_df(config).query("arch == @arch")

        # read in relevant archetype raster
        map = xr.open_dataset(
            os.path.join(
                archetype_path, "arch_map_" + arch_setting + "_" + arch + ".nc"
            )
        )

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

        log.info(
            ".......Completed writing to file: "
            + os.path.join(archetype_path, filename)
        )

    # create archetype variables maps
    func_inputs = product([config.arch_setting], vers_archs, VARS_ARCHETYPES)

    list(map(map_archetype_variables, func_inputs))
