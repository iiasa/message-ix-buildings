import os

import pandas as pd

from message_ix_buildings.chilled.util.base import get_paths
from message_ix_buildings.chilled.util.common import get_logger
from message_ix_buildings.chilled.util.config import Config

log = get_logger(__name__)
cfg = Config(user="MEAS")
dle_path = get_paths(cfg, "dle_path")


def get_sturm_data(input_path, input_version_name):
    path_dls = os.path.join(input_path, "output_data_" + input_version_name)
    file_dls = "reg_en_cool_det_v19_ALPS2023_DLS.csv"
    path_proj = os.path.join(
        os.path.normpath(input_path + os.sep + os.pardir),
        "Analysis",
        "ALPS2022_data_processing",
        "output",
    )
    file_proj = "building_stock_SSP2_v19_ALPS_ssps.csv"

    # read in data
    df_proj = (
        pd.read_csv(os.path.join(path_proj, file_proj))
        .rename(columns={"R11": "region"})
        .assign(access_scenario="projected")
    )

    df_dls = (
        pd.read_csv(os.path.join(path_dls, file_dls))
        .reindex(
            [
                "sector",
                "region",
                "year",
                "vintage",
                "floor_Mm2",
                "ac_share",
                "f_c_scl",
                "fl_cnd_c",
                "eff_ac",
            ],
            axis=1,
        )
        .drop_duplicates()
        .assign(access_scenario="DLS")
    )

    df_sturm = pd.concat([df_proj, df_dls], ignore_index=True).reindex(
        [
            "access_scenario",
            "sector",
            "region",
            "year",
            "vintage",
            "floor_Mm2",
            "ac_share",
            "f_c_scl",
            "fl_cnd_c",
            "eff_ac",
        ],
        axis=1,
    )

    return df_sturm


def get_chilled_data(input_path, input_version_name):
    version_output_path = os.path.join(
        input_path, f"output_data_{input_version_name}", "output"
    )

    # Import cooling energy intensities from CHILLED - WITH climate change
    eicc = (
        pd.read_csv(
            os.path.join(version_output_path, "REGIONAL_EI_PATHWAYS_cc_long.csv")
        )
        .query("urt == 'total'")
        .rename(columns={"value": "ei_cool_CC", "arch": "vintage"})
        .query("variable != 'Population'")
        .reindex(
            [
                "model",
                "scenario",
                "region",
                "name_run",
                "q",
                "variable",
                "unit",
                "year",
                "vintage",
                "ei_cool_CC",
            ],
            axis=1,
        )
    )

    # Import cooling energy intensities from CHILLED - WITHOUT climate change
    eincc = (
        pd.read_csv(
            os.path.join(version_output_path, "REGIONAL_EI_PATHWAYS_nocc_long.csv")
        )
        .query("urt == 'total'")
        .rename(columns={"value": "ei_cool_noCC", "arch": "vintage"})
        .query("variable != 'Population'")
        .reindex(
            [
                "model",
                "scenario",
                "region",
                "name_run",
                "variable",
                "unit",
                "year",
                "vintage",
                "ei_cool_noCC",
            ],
            axis=1,
        )
    )

    # Join datasets
    ei = pd.merge(
        eicc,
        eincc,
        how="left",
        on=[
            "model",
            "scenario",
            "region",
            "name_run",
            "variable",
            "unit",
            "year",
            "vintage",
        ],
    ).replace({"region": {"LAC": "LAM"}})

    return ei


def postprocess_electricity_demand(input_path, input_version_name):
    version_output_path = os.path.join(
        input_path, f"output_data_{input_version_name}", "output"
    )

    df_sturm = get_sturm_data(input_path, input_version_name)
    df_chilled = get_chilled_data(input_path, input_version_name)

    # Calculate total energy demand for cooling
    e = pd.merge(df_sturm, df_chilled, how="left", on=["region", "year", "vintage"])

    # Interpolate energy intensities for missing values
    e = (
        e.sort_values(by=["access_scenario", "sector", "region", "vintage", "year"])
        .assign(
            e_cool_CC=lambda x: (
                x.floor_Mm2
                * x.ac_share
                * x.ei_cool_CC
                * x.f_c_scl
                * x.fl_cnd_c
                / x.eff_ac
                / 1e6
            ),
            e_cool_noCC=lambda x: (
                x.floor_Mm2
                * x.ac_share
                * x.ei_cool_noCC
                * x.f_c_scl
                * x.fl_cnd_c
                / x.eff_ac
                / 1e6
            ),
        )
        .query("e_cool_CC.notnull()")
    )

    # Calculate total across all sectors
    e_tot = (
        e.groupby(["access_scenario", "scenario", "name_run", "q", "region", "year"])
        .agg(e_cool_CC=("e_cool_CC", "sum"), e_cool_noCC=("e_cool_noCC", "sum"))
        .reset_index()
        .assign(e_cool_delta=lambda x: x.e_cool_CC - x.e_cool_noCC)
    )

    # Sector totals by region - aggregate housing types
    e_sect = (
        e.groupby(
            ["access_scenario", "sector", "scenario", "name_run", "q", "region", "year"]
        )
        .agg(e_cool_CC=("e_cool_CC", "sum"), e_cool_noCC=("e_cool_noCC", "sum"))
        .reset_index()
        .assign(e_cool_delta=lambda x: x.e_cool_CC - x.e_cool_noCC)
    )

    # Save files

    df_chilled.to_csv(
        os.path.join(version_output_path, "chilled_postprocessed.csv"),
        index=False,
    )

    log.info("Saved: " + os.path.join(version_output_path, "chilled_postprocessed.csv"))

    df_sturm.to_csv(
        os.path.join(version_output_path, "sturm_building_stock_inputs.csv"),
        index=False,
    )

    log.info(
        "Saved: " + os.path.join(version_output_path, "sturm_building_stock_inputs.csv")
    )

    e.to_csv(
        os.path.join(
            version_output_path,
            "reg_en_cool_det_" + input_version_name + "_2024_3settemps.csv",
        ),
        index=False,
    )

    log.info(
        "Saved: "
        + os.path.join(
            version_output_path,
            "reg_en_cool_det_" + input_version_name + "_2024_3settemps.csv",
        )
    )

    e_tot.to_csv(
        os.path.join(
            version_output_path,
            "reg_en_cool_tot_" + input_version_name + "_2024_3settemps.csv",
        ),
        index=False,
    )

    log.info(
        "Saved :"
        + os.path.join(
            version_output_path,
            "reg_en_cool_tot_" + input_version_name + "_2024_3settemps.csv",
        )
    )

    e_sect.to_csv(
        os.path.join(
            version_output_path,
            "reg_en_cool_sect_" + input_version_name + "_2024_3settemps.csv",
        ),
        index=False,
    )

    log.info(
        "Saved: "
        + os.path.join(
            version_output_path,
            "reg_en_cool_sect_" + input_version_name + "_2024_3settemps.csv",
        )
    )


postprocess_electricity_demand(dle_path, cfg.vstr2 + "_" + cfg.vstr)
