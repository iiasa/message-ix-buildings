import logging
import os
from pathlib import Path

import pandas as pd
from util.config import Config  # type: ignore


def get_project_root() -> Path:
    return Path(__file__).parent.parent.parent


def get_logger(__name__):
    log = logging.getLogger(__name__)
    log.setLevel(logging.INFO)

    # configure the handler and formatter as needed
    handler = logging.FileHandler(f"{__name__}.log", mode="w")
    formatter = logging.Formatter("%(name)s %(asctime)s %(levelname)s %(message)s")

    # add formatter to the handler
    handler.setFormatter(formatter)

    # add handler to the logger
    log.addHandler(handler)

    return log


def get_archs(config: "Config"):
    root_path = get_project_root()
    version_path = os.path.join(root_path, "data", "chilled", "version", config.vstr)

    if config.arch_setting == "fixed":
        input_file = os.path.join(version_path, "arch_input.xlsx")

        if os.path.exists(input_file):
            input_file = os.path.join(version_path, "arch_input.xlsx")
            archs = pd.ExcelFile(input_file).sheet_names

            return archs
        else:
            raise TypeError(
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
            archs = pd.read_excel(input_file, sheet_name="arch").arch.unique()  # type: ignore

            return archs
        else:
            raise TypeError(
                "Archetypes input file "
                + input_file
                + " does not exist! Please create file for input"
            )


def read_arch_inputs_df(config: "Config", suff: str):
    root_path = get_project_root()
    version_path = os.path.join(root_path, "data", "chilled", "version", config.vstr)

    if config.arch_setting == "fixed":
        input_file = os.path.join(version_path, "arch_input.xlsx")

        if os.path.exists(input_file):
            arch_inputs = pd.read_excel(input_file, sheet_name=suff, index_col="id")

            return arch_inputs
        else:
            raise TypeError(
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
            raise TypeError(
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
            raise TypeError(
                "Regional archetypes input file "
                + reg_file
                + " does not exist! Please create file for input."
            )

    else:
        log.warning("Archetypes are not regional. No regional file to read.")


def load_all_scenarios_data(config: "Config"):
    root_path = get_project_root()
    version_path = os.path.join(root_path, "data", "chilled", "version", config.vstr)

    input_file = os.path.join(version_path, "runs.csv")

    if os.path.exists(input_file):
        df = pd.read_csv(input_file, index_col="id")
        return df
    else:
        raise TypeError(
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
        raise TypeError(
            "Parametric analysis data file "
            + input_file
            + " does not exist! Please create file for input."
        )
