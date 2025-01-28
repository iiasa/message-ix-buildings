import os

import pandas as pd

from message_ix_buildings.chilled.util.base import get_paths
from message_ix_buildings.chilled.util.common import get_project_root
from message_ix_buildings.chilled.util.config import Config  # type: ignore
from message_ix_buildings.project.urged.util.data import (
    prettify_column_names,
    read_and_concat_excel_sheets,
)

# Specify config and data paths
config = Config(user="MEAS")
dle_path = get_paths(config, "dle_path")
population_path = os.path.join(dle_path, "population", "cities")
buildings_path = "/Users/meas/Library/CloudStorage/OneDrive-SharedLibraries-IIASA/DLE - Analysis/Climate impacts and space conditioning/Analysis/ALPS2022_data_processing/output"

# Specify input files
shr_file = "building_stock_SSP2_v19_ALPS_ssps.csv"
ac_file = "country_data_v4.xlsx"
pop_file = "SSP_Population_Extrapolation.csv"

# Specify output file
root_path = get_project_root()
save_path = os.path.join(root_path, "data", "alps", "cities_population_buildings.csv")

# Apply function to read and concatenate Excel sheets for AC data across SSPs
df_ac = read_and_concat_excel_sheets(os.path.join(dle_path, ac_file))
print(df_ac)

# Read in shr_file
df_shr = pd.read_csv(os.path.join(buildings_path, shr_file)).rename(
    columns={"R11": "region_gea"}
)

# Calculate shr_floor column
df_shr["shr_floor"] = df_shr.groupby(["sector", "region_gea", "year"])[
    "floor_Mm2"
].transform(lambda x: x / x.sum())

# Read in country-level population data
# Drop rows where city is "NA"
# Convert "citycode" column to integer
df_pop = pd.read_csv(os.path.join(population_path, pop_file))
df_pop = (
    prettify_column_names(df_pop)[
        [
            "city",
            "citycode",
            "country",
            "iso-alpha3_code",
            "year",
            "popscen",
            "population",
        ]
    ]
    .dropna(subset=["city"])
    .assign(
        citycode=lambda x: x["citycode"].astype(int),
        year=lambda x: x["year"].astype(int),
    )
)

# Merge df_pop and df_ac:
# - iso-alpha3_code = iso
# - year = year
# - popscen = ssp
df_all = pd.merge(
    df_pop,
    df_ac,
    left_on=["iso-alpha3_code", "year", "popscen"],
    right_on=["iso", "year", "ssp"],
    how="inner",
)

# Merge df_all and df_shr:
# - region_gea = region_gea
# - year = year

df_all = pd.merge(
    df_all,
    df_shr,
    left_on=["region_gea", "year"],
    right_on=["region_gea", "year"],
    how="inner",
)

# Reorganize columns in df_all
df_all = df_all[
    [
        "city",
        "citycode",
        "country",
        "iso-alpha3_code",
        "global_south",
        "region_gea",
        "sector",
        "vintage",
        "year",
        "popscen",
        "population",
        "cdd_(popwtd)",
        "gdppercap",
        "access_elec",
        "slum_pop",
        "ac_penetr",
        "ac_penetr_r",
        "ac_penetr_u",
        "floor_Mm2",
        "shr_floor",
        "ac_share",
        "f_c_scl",
        "fl_cnd_c",
        "eff_ac",
    ]
]

# Save to CSV
df_all.to_csv(save_path, index=False)
