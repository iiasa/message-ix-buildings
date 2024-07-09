import os
from pathlib import Path

import pandas as pd  # type: ignore
from config import Config  # type: ignore


def get_project_root() -> Path:
    return Path(__file__).parent.parent.parent


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
