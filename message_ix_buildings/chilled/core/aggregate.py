import glob
import os

import pandas as pd
import pyam  # type: ignore
from sklearn.utils.fixes import parse_version, sp_version  # type: ignore

from message_ix_buildings.chilled.functions.variable_dicts import SCENARIO_NAMES
from message_ix_buildings.chilled.util.base import get_paths
from message_ix_buildings.chilled.util.common import get_logger
from message_ix_buildings.chilled.util.config import Config  # type: ignore

# This is line is to avoid incompatibility if older SciPy version.
# You should use `solver="highs"` with recent version of SciPy.
solver = "highs" if sp_version >= parse_version("1.6.0") else "interior-point"

# load logger
log = get_logger(__name__)


# Function to calculate cumulative carbon emissions
def calculate_cumulative_carbon_emissions(config: "Config"):
    """Calculate cumulative carbon emissions

    Adapted from `y7_process_results_to_CumCarb_table.py`

    """
    snapshot_file = get_paths(config, "ar6_snapshot_file")
    project_path = get_paths(config, "project_path")
    out_path = os.path.join(project_path, "out", "version", config.vstr, "output")

    if config.rcp == "baseline":
        yeardic = {
            "2015": ("2015", "2020"),
            "2020": ("2015", "2020"),
            "2030": ("2015", "2020"),
            "2040": ("2015", "2020"),
            "2050": ("2015", "2020"),
            "2060": ("2015", "2020"),
            "2070": ("2015", "2020"),
            "2080": ("2015", "2020"),
            "2090": ("2015", "2020"),
            "2100": ("2015", "2020"),
        }
    else:
        yeardic = {
            "2015": ("2015", "2020"),
            "2020": ("2015", "2025"),
            "2030": ("2015", "2045"),
            "2040": ("2025", "2055"),
            "2050": ("2035", "2065"),
            "2060": ("2045", "2075"),
            "2070": ("2055", "2085"),
            "2080": ("2065", "2095"),
            "2090": ("2080", "2100"),
            "2100": ("2095", "2100"),
        }

    # if out_path does not exist, create it
    if not os.path.exists(out_path):
        os.makedirs(out_path)

    log.info("Calculating cumulative carbon emissions...")

    varins = [
        "Emissions|CO2",
        "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile",
        "AR6 climate diagnostics|Infilled|Emissions|CO2",
    ]

    msdic = {
        "IMAGE 3.0.1": "SSP1-26",
        "AIM/CGE 2.0": "SSP3-Baseline",
        "REMIND-MAgPIE 1.5": "SSP5-Baseline",
    }

    log.info("Reading in AR6 snapshot file...")
    dfar6 = pyam.IamDataFrame(snapshot_file)
    log.info("...AR6 snapshot file read.")

    log.info("Filtering AR6 data...")
    a = dfar6.filter(scenario="SSP*")
    ndf = a.filter(model="xxx")
    for m, s in msdic.items():
        ndf.append(a.filter(model=m, scenario=s, variable=varins), inplace=True)
    log.info("...AR6 data filtered.")

    log.info("Interpolating AR6 data...")
    ndf.interpolate(range(2000, 2101, 1), inplace=True)
    log.info("...AR6 data interpolated.")

    # ENGAGE scenarios
    scenarios = ["EN_NPi2020_300f", "EN_NPi2020_1400f", "EN_NPi2100"]

    log.info("Selected scenarios: " + str(scenarios))

    log.info("Filtering for ENGAGE data...")
    engage = dfar6.filter(model="MESSAGE*", scenario=scenarios, variable=varins)
    log.info("...ENGAGE data filtered.")

    # Calculate Cumulative CO2 emissions
    unit = "Gt CO2"

    ts_engage = (
        engage.filter(variable="Emissions|CO2", year=range(2015, 2101))
        .convert_unit(
            "Mt CO2/yr",
            "Gt CO2/yr",
        )
        .timeseries()
    )

    ts_native = (
        ndf.filter(variable="Emissions|CO2", year=range(2015, 2101))
        .convert_unit(
            "Mt CO2/yr",
            "Gt CO2/yr",
        )
        .timeseries()
    )
    ts_infilled = (
        ndf.filter(
            variable="AR6 climate diagnostics|Infilled|Emissions|CO2",
            year=range(2015, 2101),
        )
        .convert_unit(
            "Mt CO2/yr",
            "Gt CO2/yr",
        )
        .timeseries()
    )

    engage_cumCO2 = pd.DataFrame()
    for year in yeardic.keys():
        int_year = int(year)

        df = (
            ts_engage.apply(
                pyam.timeseries.cumulative,
                raw=False,
                axis=1,
                first_year=2015,
                last_year=int_year,
            )
            .to_frame()
            .reset_index()
        )
        df["year"] = int_year
        df["variable"] = f"Cumulative CO2 infilled 2015-{int_year}, Gt CO2"
        df.rename(columns={0: "value"}, inplace=True)
        engage_cumCO2 = pd.concat([engage_cumCO2, df])
        engage_cumCO2["unit"] = unit

    engage.append(engage_cumCO2, inplace=True)

    df_cumCO2 = pd.DataFrame()
    for year in yeardic.keys():
        int_year = int(year)

        df = (
            ts_native.apply(
                pyam.timeseries.cumulative,
                raw=False,
                axis=1,
                first_year=2015,
                last_year=int_year,
            )
            .to_frame()
            .reset_index()
        )
        df["year"] = int_year
        df["variable"] = f"Cumulative CO2 native 2015-{int_year}, Gt CO2"
        df.rename(columns={0: "value"}, inplace=True)
        df_cumCO2 = pd.concat([df_cumCO2, df])

        df = (
            ts_infilled.apply(
                pyam.timeseries.cumulative,
                raw=False,
                axis=1,
                first_year=2015,
                last_year=int_year,
            )
            .to_frame()
            .reset_index()
        )
        df["year"] = int_year
        df["variable"] = f"Cumulative CO2 infilled 2015-{year}, Gt CO2"
        df.rename(columns={0: "value"}, inplace=True)

        df_cumCO2 = pd.concat([df_cumCO2, df])

    df_cumCO2["unit"] = unit

    ndf.append(df_cumCO2, inplace=True)

    log.info("Writing out cumulative carbon emissions to " + out_path + "...")

    engage.to_excel(os.path.join(out_path, "engage_emissions_tempAR6_cumCO2.xlsx"))
    log.info(
        "...Saved: " + os.path.join(out_path, "engage_emissions_tempAR6_cumCO2.xlsx")
    )

    engage_cumCO2.to_csv(os.path.join(out_path, "engage_cumCO2.csv"), index=False)
    log.info("...Saved: " + os.path.join(out_path, "engage_cumCO2.csv"))

    ndf.to_excel(os.path.join(out_path, "isimip3b_emissions_tempAR6_cumCO2.xlsx"))
    log.info(
        "...Saved: " + os.path.join(out_path, "isimip3b_emissions_tempAR6_cumCO2.xlsx")
    )

    df_cumCO2.to_csv(os.path.join(out_path, "isimip3b_cumCO2.csv"), index=False)
    log.info("...Saved: " + os.path.join(out_path, "isimip3b_cumCO2.csv"))


def aggregate_ISO_tables_to_regions(config: "Config"):
    project_path = get_paths(config, "project_path")
    version_path = os.path.join(project_path, "out", "version", config.vstr)
    out_path = os.path.join(version_path, "output")

    # if out_path does not exist, create it
    if not os.path.exists(out_path):
        os.makedirs(out_path)

    model = "MESS-CHILL-URM"

    files = glob.glob(
        os.path.join(version_path, "**/*/" + "ISO_agg_data_*" + ".csv"),
        recursive=True,
    )

    log.info("Reading ISO table files....")
    data = pd.concat((pd.read_csv(f) for f in files), ignore_index=True).rename(
        columns={"REGION_GEA": "region"}
    )
    log.info("...ISO table files read.")

    # get population data from UKESM1-0-LL gcm and get unique combinations of population
    pop_uk = (
        data.query("gcm == 'UKESM1-0-LL'")
        .reindex(
            [
                "id",
                "NAME",
                "ISO",
                "region",
                "clim",
                "arch",
                "urt",
                "par_var",
                "name_run",
                "year",
                "popsum",
            ],
            axis=1,
        )
        .drop_duplicates()
    )

    if config.cool == 1:
        log.info("COOLING: Calculating EI_ac_m2 assuming 10m2 per person...")
        # drop population from data and get unique combinations of data
        # NOTE: there are some countries in LAC ssp126 that have multiple E_c_ac_popwei values
        # and one of them is 0. so I've decided to choose the maximum value for E_c_ac_popwei
        data_sub = (
            data[
                [
                    "id",
                    "NAME",
                    "ISO",
                    "GLOBAL_SOUTH",
                    "region",
                    "gcm",
                    "scenario",
                    "scen",
                    "year",
                    "clim",
                    "arch",
                    "urt",
                    "par_var",
                    "name_run",
                    "E_c_ac_popwei",
                ]
            ]
            .drop_duplicates()
            .groupby(
                [
                    "id",
                    "NAME",
                    "ISO",
                    "GLOBAL_SOUTH",
                    "region",
                    "gcm",
                    "scenario",
                    "scen",
                    "year",
                    "clim",
                    "arch",
                    "urt",
                    "par_var",
                    "name_run",
                ]
            )[["E_c_ac_popwei"]]
            .max()
            .reset_index()
        )

        # merge with UKESM1-0-LL population data so that all gcms use the same population data
        data_new_pop = pd.merge(
            data_sub,
            pop_uk,
            on=[
                "id",
                "NAME",
                "ISO",
                "region",
                "clim",
                "arch",
                "urt",
                "par_var",
                "name_run",
                "year",
            ],
            how="left",
        )

        # aggregate popsum and E_c_ac_popwei to the region level
        gb_region = (
            data_new_pop.groupby(
                [
                    "gcm",
                    "scenario",
                    "scen",
                    "year",
                    "clim",
                    "arch",
                    "urt",
                    "par_var",
                    "name_run",
                    "region",
                ]
            )[["popsum", "E_c_ac_popwei"]]
            .sum()
            .reset_index()
        )

        # aggregate popsum and E_c_ac_popwei to the global level and assign a new region == "World"
        gb_world = (
            data_new_pop.groupby(
                [
                    "gcm",
                    "scenario",
                    "scen",
                    "year",
                    "clim",
                    "arch",
                    "urt",
                    "par_var",
                    "name_run",
                ]
            )[["popsum", "E_c_ac_popwei"]]
            .sum()
            .reset_index()
            .assign(region="World")
        )

        # concatenate region and world data
        gb = pd.concat([gb_region, gb_world])

        # calculate EI_ac_m2 assuming 10m2 per person
        log.info("Calculating EI_ac_m2 assuming 10m2 per person...")
        gb = gb.assign(
            EI_ac_m2=lambda x: x.E_c_ac_popwei / (x.popsum * 10)
        )  # 10 because 10m2 per person assumed
        log.info("...EI_ac_m2 calculated.")

        # gb = (
        #     data_new_pop.groupby(
        #         [
        #             "gcm",
        #             "scenario",
        #             "scen",
        #             "year",
        #             "clim",
        #             "arch",
        #             "urt",
        #             "par_var",
        #             "name_run",
        #             "region",
        #         ]
        #     )[["popsum", "E_c_ac_popwei"]]
        #     .sum()
        #     .reset_index()
        #     .assign(
        #         EI_ac_m2=lambda x: x.E_c_ac_popwei / (x.popsum * 10)
        #     )  # 10 because 10m2 per person assumed
        # )

        gb.to_csv(os.path.join(out_path, "region_agg_EI_ac_m2.csv"), index=False)

        log.info("Saved: " + os.path.join(out_path, "region_agg_EI_ac_m2.csv"))

    if config.heat == 1:
        log.info("HEATING: Calculating EI_h_m2 assuming 10m2 per person...")
        # drop population from data and get unique combinations of data
        data_sub = (
            data[
                [
                    "id",
                    "NAME",
                    "ISO",
                    "GLOBAL_SOUTH",
                    "region",
                    "gcm",
                    "scenario",
                    "scen",
                    "year",
                    "clim",
                    "arch",
                    "urt",
                    "par_var",
                    "name_run",
                    "E_h_popwei",
                ]
            ]
            .drop_duplicates()
            .groupby(
                [
                    "id",
                    "NAME",
                    "ISO",
                    "GLOBAL_SOUTH",
                    "region",
                    "gcm",
                    "scenario",
                    "scen",
                    "year",
                    "clim",
                    "arch",
                    "urt",
                    "par_var",
                    "name_run",
                ]
            )[["E_h_popwei"]]
            .max()
            .reset_index()
        )

        # merge with UKESM1-0-LL population data so that all gcms use the same population data
        data_new_pop = pd.merge(
            data_sub,
            pop_uk,
            on=[
                "id",
                "NAME",
                "ISO",
                "region",
                "clim",
                "arch",
                "urt",
                "par_var",
                "name_run",
                "year",
            ],
            how="left",
        )

        # aggregate popsum and E_h_popwei to the region level
        gb_region = (
            data_new_pop.groupby(
                [
                    "gcm",
                    "scenario",
                    "scen",
                    "year",
                    "clim",
                    "arch",
                    "urt",
                    "par_var",
                    "name_run",
                    "region",
                ]
            )[["popsum", "E_h_popwei"]]
            .sum()
            .reset_index()
        )

        # aggregate popsum and E_h_popwei to the global level and assign a new region == "World"
        gb_world = (
            data_new_pop.groupby(
                [
                    "gcm",
                    "scenario",
                    "scen",
                    "year",
                    "clim",
                    "arch",
                    "urt",
                    "par_var",
                    "name_run",
                ]
            )[["popsum", "E_h_popwei"]]
            .sum()
            .reset_index()
            .assign(region="World")
        )

        # concatenate region and world data
        gb = pd.concat([gb_region, gb_world])

        # calculate EI_h_m2 assuming 10m2 per person
        log.info("Calculating EI_h_m2 assuming 10m2 per person...")
        gb = gb.assign(
            EI_h_m2=lambda x: x.E_h_popwei / (x.popsum * 10)
        )  # 10 because 10m2 per person assumed
        log.info("...EI_h_m2 calculated.")

        gb.to_csv(os.path.join(out_path, "region_agg_EI_h_m2.csv"), index=False)

        log.info("Saved: " + os.path.join(out_path, "region_agg_EI_h_m2.csv"))

    log.info("Calculating population by region...")
    pop_only = (
        gb.reindex(
            [
                "scenario",
                "scen",
                "year",
                "clim",
                "arch",
                "urt",
                "par_var",
                "name_run",
                "region",
                "popsum",
            ],
            axis=1,
        )
        .assign(model=model, scenario="SSP2", variable="Population", unit="People")
        .rename(columns={"popsum": "value"})
        .drop_duplicates()
        # .reindex(
        #     ["model", "scenario", "region", "year", "variable", "unit", "value"], axis=1
        # )
    )

    pop_only.to_csv(os.path.join(out_path, "population_by_region.csv"), index=False)

    log.info("Saved: " + os.path.join(out_path, "population_by_region.csv"))


def create_prereg_data(config: "Config"):
    project_path = get_paths(config, "project_path")
    version_path = os.path.join(project_path, "out", "version", config.vstr)
    out_path = os.path.join(version_path, "output")

    # if out_path does not exist, create it
    if not os.path.exists(out_path):
        os.makedirs(out_path)

    log.info("Read in isimip3b_cumCO2.csv...")
    df_cumCO2 = pd.read_csv(os.path.join(out_path, "isimip3b_cumCO2.csv"))
    log.info("...isimip3b_cumCO2.csv read.")

    df_cumCO2.scenario.replace(SCENARIO_NAMES, inplace=True)
    df_cumCO2 = df_cumCO2.loc[df_cumCO2.variable.str.contains("infilled")]

    if config.cool == 1:
        log.info("COOLING: Create pre-regress data for cooling")
        log.info("Read in region_agg_EI_ac_m2.csv...")
        df_ei = pd.read_csv(os.path.join(out_path, "region_agg_EI_ac_m2.csv"))
        log.info("...region_agg_EI_ac_m2.csv read.")

        log.info("Combine EI_ac_m2 and cumCO2 data...")
        ei_cumCO2 = pd.merge(
            df_ei,
            df_cumCO2.reindex(["scenario", "year", "value"], axis=1),
            on=["scenario", "year"],
            how="left",
        ).rename(columns={"value": "cumCO2"})
        log.info("...EI_ac_m2 and cumCO2 data combined.")

        log.info("Saving pre-regress data...")

        ei_cumCO2.to_csv(
            os.path.join(out_path, "region_EI_cool_cumCO2_pre-regress.csv"),
            index=False,
        )
        log.info(
            "Saved: " + os.path.join(out_path, "region_EI_cool_cumCO2_pre-regress.csv")
        )

    if config.heat == 1:
        log.info("HEATING: Create pre-regress data for heating")
        log.info("Read in region_agg_EI_h_m2.csv...")
        df_ei = pd.read_csv(os.path.join(out_path, "region_agg_EI_h_m2.csv"))
        log.info("...region_agg_EI_h_m2.csv read.")

        log.info("Combine EI_h_m2 and cumCO2 data...")
        ei_cumCO2 = pd.merge(
            df_ei,
            df_cumCO2.reindex(["scenario", "year", "value"], axis=1),
            on=["scenario", "year"],
            how="left",
        ).rename(columns={"value": "cumCO2"})
        log.info("...EI_h_m2 and cumCO2 data combined.")

        log.info("Saving pre-regress data...")

        ei_cumCO2.to_csv(
            os.path.join(out_path, "region_EI_heat_cumCO2_pre-regress.csv"),
            index=False,
        )
        log.info(
            "Saved: " + os.path.join(out_path, "region_EI_heat_cumCO2_pre-regress.csv")
        )
