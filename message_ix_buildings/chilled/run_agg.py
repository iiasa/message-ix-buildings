import sys
from argparse import ArgumentParser

from message_ix_buildings.chilled.core.climate import (
    aggregate_urban_rural_files,
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
        help="GCM to run. Options: GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL. Default: GFDL-ESM4.",
    )
    parser.add_argument(
        "-rcp",
        "--rcp",
        default="baseline",
        help="RCP to run. Options: ssp126, ssp370, ssp585, baseline. Default: baseline.",
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

    # Run the main function
    print_arguments(parsed_arguments=parsed_args)
    cfg = create_config(parsed_arguments=parsed_args)

    log.info("RUNNING aggregate_urban_rural_files()........")
    aggregate_urban_rural_files(cfg)

    log.info("RUNNING make_vdd_total_maps()........")
    make_vdd_total_maps(cfg)

    log.info("RUNNING process_construction_shares()........")
    process_construction_shares(cfg)

    log.info("RUNNING process_floor_area_maps()........")
    process_floor_area_maps(cfg)

    log.info("RUNNING process_country_maps()........")
    process_country_maps(cfg)

    log.info("RUNNING process_final_maps()........")
    process_final_maps(cfg)

    log.info("RUNNING process_iso_tables()........")
    process_iso_tables(cfg)


if __name__ == "__main__":
    main()
