import datetime
import os
import sys
from argparse import ArgumentParser

import pandas as pd
from dask import delayed
from rich.progress import track  # type: ignore

from message_ix_buildings.chilled.core.climate import (
    aggregate_urban_rural_files,
    create_climate_variables_maps,
    make_vdd_total_maps,
    process_construction_shares,
    process_country_maps,
    process_final_maps,
    process_floor_area_maps,
    process_iso_tables,
)
from message_ix_buildings.chilled.util.common import get_logger, get_project_root
from message_ix_buildings.chilled.util.config import Config  # type: ignore

log = get_logger(__name__)


def parse_arguments(arguments):
    """

    :return:
    """
    parser = ArgumentParser(add_help=True)

    parser.add_argument(
        "-version",
        "--version",
        default="ALPS2024_cities",
        help="Version of inputs to run. Default: ALPS2023.",
    )
    parser.add_argument(
        "-gcm",
        "--gcm",
        default="MRI-ESM2-0",
        help="GCM to run. Options: GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, \
            UKESM1-0-LL. Default: GFDL-ESM4.",
    )
    parser.add_argument(
        "-rcp",
        "--rcp",
        default="baseline",
        help="RCP to run. Options: ssp126, ssp370, ssp585, baseline. \
            Default: baseline.",
    )

    # Parse arguments
    parsed_arguments = parser.parse_known_args(args=arguments)[0]

    return parsed_arguments


def print_arguments(parsed_arguments):
    """
    :param parsed_arguments:

    """

    # Print arguments
    log.info(
        "\n"
        + "---------- Parsed arguments ------------"
        + "\n"
        + "Selected version: "
        + parsed_arguments.version
        + "\n"
        + "Selected GCM: "
        + parsed_arguments.gcm
        + "\n"
        + "Selected RCP scenario: "
        + parsed_arguments.rcp
    )


# create climate outputs
def create_config(parsed_arguments):
    cfg = Config(
        vstr=parsed_arguments.version,
        gcm=parsed_arguments.gcm,
        rcp=parsed_arguments.rcp,
    )

    return cfg


def main(args=None):
    if args is None:
        args = sys.argv[1:]

    parsed_args = parse_arguments(arguments=args)

    # Run the core functions
    start = datetime.datetime.now()
    print_arguments(parsed_arguments=parsed_args)
    cfg = create_config(parsed_arguments=parsed_args)

    # green space file location, relative to the root directory
    root_path = get_project_root()
    green_path = os.path.join(root_path, "data", "green-space", "ALPS2024")

    # read in all files in green_path
    city_lcz = pd.read_csv(os.path.join(green_path, "outer.csv")).drop(
        columns=["Unnamed: 0"]
    )

    # Example: List of city coordinates (lat, lon)
    city_df = city_lcz[["UC_NM_MN", "CTR_MN_ISO", "x", "y"]].drop_duplicates()

    # Wrap functions in dask.delayed
    tasks = [
        delayed(create_climate_variables_maps)(
            config=cfg,
            start_time=start,
            extract_cities=True,
            city_df=city_df,
            name_col="UC_NM_MN",
            lat_col="y",
            lon_col="x",
        ),
        delayed(aggregate_urban_rural_files)(cfg),
        delayed(make_vdd_total_maps)(cfg),
        delayed(process_construction_shares)(cfg),
        delayed(process_floor_area_maps)(cfg),
        delayed(process_country_maps)(cfg),
        delayed(process_final_maps)(cfg),
        delayed(process_iso_tables)(cfg),
    ]

    # Compute tasks in sequential order
    for task in track(tasks, description="Running core functions..."):
        task.compute()


if __name__ == "__main__":
    main()
