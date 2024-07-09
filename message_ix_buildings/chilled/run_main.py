import datetime
import sys
from argparse import ArgumentParser

from main.climate import create_climate_variables_maps  # type: ignore
from utils.config import Config  # type: ignore


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
    print("---------- Parsed arguments ------------")
    print("Selected version: " + parsed_arguments.version)
    print("Selected GCM: " + parsed_arguments.gcm)
    print("Selected RCP scenario: " + parsed_arguments.rcp)


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

    # Run the main function
    start = datetime.datetime.now()
    print_arguments(parsed_arguments=parsed_args)
    cfg = create_config(parsed_arguments=parsed_args)
    create_climate_variables_maps(cfg, start)


if __name__ == "__main__":
    main()
