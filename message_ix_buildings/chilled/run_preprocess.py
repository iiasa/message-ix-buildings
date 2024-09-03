import logging
import sys
from argparse import ArgumentParser

from message_ix_buildings.chilled.preprocess.archetypes import (
    create_archetype_variables,
    create_archetypes,
)
from message_ix_buildings.chilled.util.config import Config

log = logging.getLogger(__name__)
log.setLevel(logging.INFO)

# configure the handler and formatter as needed
handler = logging.FileHandler(f"{__name__}.log", mode="w")
formatter = logging.Formatter("%(name)s %(asctime)s %(levelname)s %(message)s")

# add formatter to the handler
handler.setFormatter(formatter)

# add handler to the logger
log.addHandler(handler)


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


# create climate outputs
def create_config(parsed_arguments):
    cfg = Config(
        vstr=parsed_arguments.version,
    )

    return cfg


def main(args=None):
    if args is None:
        args = sys.argv[1:]

    parsed_args = parse_arguments(arguments=args)

    # Run the main function
    print_arguments(parsed_arguments=parsed_args)
    cfg = create_config(parsed_arguments=parsed_args)

    create_archetypes(cfg)
    log.info("Archetypes created successfully.")

    create_archetype_variables(cfg)
    log.info("Archetype variables created successfully.")


if __name__ == "__main__":
    main()
