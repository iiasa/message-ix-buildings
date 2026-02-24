import datetime
import sys
from argparse import ArgumentParser

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
from message_ix_buildings.chilled.util.common import get_logger
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
        default="ALPS2023",
        help="Version of inputs to run. Default: ALPS2023.",
    )
    parser.add_argument(
        "-gcm",
        "--gcm",
        default="GFDL-ESM4",
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
        cool=0,
        heat=1,
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

    tasks = [
        delayed(create_climate_variables_maps)(cfg, start),
        delayed(aggregate_urban_rural_files)(cfg),
        delayed(make_vdd_total_maps)(cfg),
        delayed(process_construction_shares)(cfg),
        delayed(process_floor_area_maps)(cfg),
        delayed(process_country_maps)(cfg),
        delayed(process_final_maps)(cfg),
        delayed(process_iso_tables)(cfg),
    ]

    # Ensure tasks run in order
    result = None
    for task in tasks:
        result = task.compute()

    for _ in track(tasks, description="Running core functions..."):
        pass


if __name__ == "__main__":
    main()
