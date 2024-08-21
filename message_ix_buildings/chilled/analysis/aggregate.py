import glob
import os

import pandas as pd
from sklearn.utils.fixes import parse_version, sp_version  # type: ignore

from message_ix_buildings.chilled.variable_dicts import SCENARIO_NAMES

# This is line is to avoid incompatibility if older SciPy version.
# You should use `solver="highs"` with recent version of SciPy.
solver = "highs" if sp_version >= parse_version("1.6.0") else "interior-point"

# from message_ix_buildings.chilled.config import Config

# cfg = Config()

# dle_path = cfg.dle_path
# version_name = cfg.vstr

# model = "MESS-CHILL-URM"

# main_path = os.path.join(dle_path, f"output_data_{version_name}")

# version_output_path = os.path.join(dle_path, f"output_data_{version_name}", "output")

# files = glob.glob(
#     os.path.join(main_path, "**/*/" + "ISO_agg_data_" + version_name + ".csv"),
#     recursive=True,
# )

# print("Reading files....")
# data = pd.concat((pd.read_csv(f) for f in files), ignore_index=True).rename(
#     columns={"REGION_GEA": "region"}
# )


# data_sub.query(
#     "gcm == 'IPSL-CM6A-LR' and scenario == 'ssp126' and year == 2015 and region == 'LAC' and urt == 'total' and arch == 'exist' and par_var == 0"
# ).sort_values(by=["ISO"], ascending=False)

# data_sub.query("E_c_ac_popwei == 0")

# data_sub.query(
#     "gcm == 'GFDL-ESM4' and year == 2015 and region == 'LAC' and urt == 'total' and arch == 'exist' and par_var == 0"
# ).sort_values(by=["NAME"], ascending=False)


# gb.query(
#     "scenario == 'ssp126' and year == 2015 and arch == 'exist' and urt == 'total' and par_var == 0 and region == 'LAC'"
# )
# data_new_pop.query(
#     "scenario == 'ssp126' and year == 2015 and arch == 'exist' and urt == 'total' and par_var == 0 and region == 'LAC'"
# ).sort_values(by=["popsum"], ascending=False)

# # count how many countries in LAC under each gcm
# data_new_pop.query(
#     "scenario == 'ssp126' and year == 2015 and arch == 'exist' and urt == 'total' and par_var == 0 and region == 'LAC'"
# ).groupby(["gcm"])["ISO"].count()


# # count how many countries in LAC under each gcm
# data_new_pop.groupby(["gcm"])["ISO"].count()


# # list unique countries in GFDL-ESM4 gcm
# iso_gfdl = data_new_pop.query(
#     "gcm == 'GFDL-ESM4' and scenario == 'ssp126' and year == 2015 and arch == 'exist' and urt == 'total' and par_var == 0 and region == 'LAC'"
# ).ISO.unique()

# # list unique countries in UKESM1-0-LL gcm
# iso_uk = data_new_pop.query(
#     "gcm == 'UKESM1-0-LL' and scenario == 'ssp126' and year == 2015 and arch == 'exist' and urt == 'total' and par_var == 0 and region == 'LAC'"
# ).ISO.unique()

# list countries that are in GFSL-ESM4 but not UKESM1-0-LL
# iso_gfdl_not_uk = [x for x in iso_gfdl if x not in iso_uk]
# iso_gfdl_not_uk

# gb.query(
#     "scenario == 'ssp126' and year == 2050 and arch == 'exist' and urt == 'urban' and par_var == 0 and region == 'NAM'"
# )
# gb.query(
#     "scenario == 'ssp126' and year == 2050 and arch == 'exist' and urt == 'total' and par_var == 0 and region == 'NAM'"
# )


def aggregate_ISO_tables_to_regions(dle_path, version_name):
    model = "MESS-CHILL-URM"

    main_path = os.path.join(dle_path, f"output_data_{version_name}")

    version_output_path = os.path.join(
        dle_path, f"output_data_{version_name}", "output"
    )

    files = glob.glob(
        os.path.join(main_path, "**/*/" + "ISO_agg_data_" + version_name + ".csv"),
        recursive=True,
    )

    print("Reading files....")
    data = pd.concat((pd.read_csv(f) for f in files), ignore_index=True).rename(
        columns={"REGION_GEA": "region"}
    )

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

    # drop population from data and get unique combinations of data
    # NOTE: there are some countries in LAC ssp126 that have multiple E_c_ac_popwei values
    # and one of them is 0. so I've decided to choose the maximum value for E_c_ac_popwei
    data_sub = (
        data.drop(
            columns=[
                "E_c_ac_gap",
                "E_c_ac_wAccess",
                "E_c_fan_gap",
                "E_c_fan_popwei",
                "E_c_fan_wAccess",
                "E_c_gap",
                "E_c_perpix",
                "E_c_popwei",
                "E_c_wAccess",
                "Nd",
                "Nf",
                "P_c_ac_gap",
                "P_c_ac_potential",
                "P_c_fanNoAC",
                "P_c_fan_gap",
                "vdd_c_popwei",
                "vdd_c_avg",
                "population_scenario",
                "popsum",
            ]
        )
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
    gb = gb.assign(
        EI_ac_m2=lambda x: x.E_c_ac_popwei / (x.popsum * 10)
    )  # 10 because 10m2 per person assumed

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

    gb.to_csv(os.path.join(version_output_path, "region_agg_EI_ac_m2.csv"), index=False)

    print("Saved: " + os.path.join(version_output_path, "region_agg_EI_ac_m2.csv"))

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

    pop_only.to_csv(
        os.path.join(version_output_path, "population_by_region.csv"), index=False
    )

    print("Saved: " + os.path.join(version_output_path, "population_by_region.csv"))


def create_prereg_data(dle_path, version_name):
    version_output_path = os.path.join(
        dle_path, f"output_data_{version_name}", "output"
    )

    df_cumCO2 = pd.read_csv(os.path.join(version_output_path, "isimip3b_cumCO2.csv"))
    df_cumCO2.scenario.replace(SCENARIO_NAMES, inplace=True)
    df_cumCO2 = df_cumCO2.loc[df_cumCO2.variable.str.contains("infilled")]

    df_ei = pd.read_csv(os.path.join(version_output_path, "region_agg_EI_ac_m2.csv"))

    ei_cumCO2 = pd.merge(
        df_ei,
        df_cumCO2.reindex(["scenario", "year", "value"], axis=1),
        on=["scenario", "year"],
        how="left",
    ).rename(columns={"value": "cumCO2"})

    ei_cumCO2.to_csv(
        os.path.join(version_output_path, "region_EI_cumCO2_pre-regress.csv"),
        index=False,
    )

    print(
        "Saved: "
        + os.path.join(version_output_path, "region_EI_cumCO2_pre-regress.csv")
    )
