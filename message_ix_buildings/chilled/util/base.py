import logging
import os
from pathlib import Path

import pandas as pd
import xarray as xr

from message_ix_buildings.chilled.util.config import Config  # type: ignore


def get_project_root() -> Path:
    return Path(__file__).parent.parent.parent


def get_logger(name: str):
    log = logging.getLogger(name)
    log.setLevel(logging.INFO)

    # configure the handler and formatter as needed
    handler = logging.FileHandler(f"{name}.log", mode="w")
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
            archs = pd.ExcelFile(input_file).sheet_names

            return archs
        else:
            raise FileNotFoundError(
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
            archs = pd.read_excel(input_file, sheet_name="arch").arch.unique().tolist()

            return archs
        else:
            raise FileNotFoundError(
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
            raise FileNotFoundError(
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
            raise FileNotFoundError(
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


def set_climate_data_paths(config: "Config"):
    # Climate input variable format
    climate_filestr_hist = (
        f"tas_day_{config.gcm}_{config.rcpdata}_r1i1p1_EWEMBI_landonly_"  # ISIMIP2
    )

    if config.gcm == "UKESM1-0-LL":
        climate_filestr_future = (
            f"{config.gcm}_r1i1p1f2_w5e5_{config.rcpdata}_{config.var}_global_daily_"
        )
    else:
        climate_filestr_future = (
            f"{config.gcm}_r1i1p1f1_w5e5_{config.rcpdata}_{config.var}_global_daily_"
        )

    endstr = ".nc"

    clim = "hist"

    if str(clim) == "hist":
        isi_folder = config.isimip_ewemib_path
        filestr = climate_filestr_hist
    else:
        isi_folder = config.isimip_bias_adj_path
        filestr = climate_filestr_future

    filepath = os.path.join(
        isi_folder, config.rcpdata, config.gcm, f"{filestr.lower()}*{endstr}"
    )

    print(filepath)

    assert False

    if config.rcp == "rcp26":
        dst = xr.open_mfdataset(
            filepath,
            chunks={"lon": config.chunk_size},
            concat_dim="time",
            use_cftime=True,
        )  # Setting for RCP2.6
    else:
        dst = xr.open_mfdataset(
            filepath,
            chunks={"lon": config.chunk_size},
        )  # , concat_dim='time' )  # Setting for RCP6.0

    return dst
