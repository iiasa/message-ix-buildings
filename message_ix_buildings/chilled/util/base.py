import os

import pandas as pd

from message_ix_buildings.chilled.functions.user_settings import DICT_USER_SETTINGS
from message_ix_buildings.chilled.util.common import get_project_root  # type: ignore
from message_ix_buildings.chilled.util.config import Config


def get_paths(config: "Config", selection):
    # if selection is chunk_size, then return chunk_size as is
    # else, return string version of the path
    if selection == "chunk_size":
        return DICT_USER_SETTINGS[config.user][selection]
    else:
        return str(DICT_USER_SETTINGS[config.user][selection])


def get_archs(config: "Config"):
    root_path = get_project_root()
    version_path = os.path.join(root_path, "data", "chilled", "version", config.vstr)

    if config.arch_setting == "fixed":
        input_file = os.path.join(version_path, "arch_input.csv")
    elif config.arch_setting == "regional":
        input_file = os.path.join(version_path, "arch_input_reg.csv")

    if os.path.exists(input_file):
        archs = pd.read_csv(input_file).arch.unique().tolist()

        return archs
    else:
        raise FileNotFoundError(
            "Archetypes input file "
            + input_file
            + " does not exist! Please create file for input."
        )


def read_arch_inputs_df(config: "Config"):
    root_path = get_project_root()
    version_path = os.path.join(root_path, "data", "chilled", "version", config.vstr)

    if config.arch_setting == "fixed":
        input_file = os.path.join(version_path, "arch_input.csv")
    elif config.arch_setting == "regional":
        input_file = os.path.join(version_path, "arch_input_reg.csv")

    if os.path.exists(input_file):
        arch_inputs = pd.read_csv(input_file)

        return arch_inputs

    else:
        raise FileNotFoundError(
            "Archetypes input file "
            + input_file
            + " does not exist! Please create file for input."
        )


def read_arch_reg_df(config: "Config"):
    root_path = get_project_root()
    version_path = os.path.join(root_path, "data", "chilled", "version", config.vstr)

    if config.arch_setting == "regional":
        reg_file = os.path.join(
            version_path,
            "arch_regions.csv",
        )

        if os.path.exists(reg_file):
            arch_reg = pd.read_csv(reg_file)
            return arch_reg
        else:
            raise FileNotFoundError(
                "Regional archetypes input file "
                + reg_file
                + " does not exist! Please create file for input."
            )

    else:
        raise TypeError("Archetypes are not regional. No regional file to read.")


def load_all_scenarios_data(config: "Config"):
    root_path = get_project_root()
    version_path = os.path.join(root_path, "data", "chilled", "version", config.vstr)

    input_file = os.path.join(version_path, "runs.csv")

    if os.path.exists(input_file):
        df = pd.read_csv(input_file, index_col="id")
        return df
    else:
        raise FileNotFoundError(
            "Scenarios file "
            + input_file
            + " does not exist! Please create file for input."
        )


def load_parametric_analysis_data(config: "Config"):
    root_path = get_project_root()
    version_path = os.path.join(root_path, "data", "chilled", "version", config.vstr)

    input_file = os.path.join(version_path, "par_var.csv")

    if os.path.exists(input_file):
        df = pd.read_csv(input_file, index_col="id_run")

        if config.paranalysis_mode == 0:
            df = df.loc[df.name_run == "ref", :]

        return df
    else:
        raise FileNotFoundError(
            "Parametric analysis data file "
            + input_file
            + " does not exist! Please create file for input."
        )
