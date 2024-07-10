import sys
from argparse import ArgumentParser

from preprocess.archetypes import create_archetype_variables, create_archetypes
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
    print("Archetypes created successfully.")
    create_archetype_variables(cfg)
    print("Archetype variables created successfully.")


if __name__ == "__main__":
    main()


# check to see if "map_reg_MESSAGE_{config.node}.nc" exists
# if not, create the file
# if not os.path.exists(
#     os.path.join(cfg.dle_path, "out", "raster", "map_reg_MESSAGE_" + cfg.node + ".nc")
# ):
#     create_message_raster(cfg)
# else:
#     print(f"map_reg_MESSAGE_{cfg.node}.nc already exists. Using existing file.")

# run create_archetypes


# create maps of archetype variables
