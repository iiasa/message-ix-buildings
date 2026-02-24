import sys
from argparse import ArgumentParser

from message_ix_buildings.chilled.core.aggregate import (
    aggregate_ISO_tables_to_regions,
    calculate_cumulative_carbon_emissions,
    create_prereg_data,
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
    )


def create_config(parsed_arguments):
    cfg = Config(
        vstr=parsed_arguments.version,
        cool=0,
        heat=1,
    )

    return cfg


def main(args=None):
    if args is None:
        args = sys.argv[1:]

    parsed_args = parse_arguments(arguments=args)

    # Run the main function
    print_arguments(parsed_arguments=parsed_args)
    cfg = create_config(parsed_arguments=parsed_args)

    log.info("RUNNING calculate_cumulative_carbon_emissions()........")
    calculate_cumulative_carbon_emissions(cfg)

    log.info("RUNNING aggregate_ISO_tables_to_regions()........")
    aggregate_ISO_tables_to_regions(cfg)

    log.info("RUNNING create_prereg_data()........")
    create_prereg_data(cfg)


if __name__ == "__main__":
    main()
