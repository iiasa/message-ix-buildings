import os

import pandas as pd
import xarray as xr

from message_ix_buildings.chilled.functions.extract_cities import select_nearest_points
from message_ix_buildings.chilled.util.common import get_project_root

# Load example temperature data
temp_file = "/Users/meas/Library/CloudStorage/OneDrive-IIASA/Documents/chilled/ISIMIP3b/gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020.nc"

# green space file location, relative to the root directory
root_path = get_project_root()
green_path = os.path.join(root_path, "data", "green-space", "ALPS2024")

# Read the temperature data
temp = xr.open_dataarray(temp_file)

# Example: List of city coordinates (lat, lon)
city_df = pd.read_csv(os.path.join(green_path, "outer.csv"))[
    ["UC_NM_MN", "CTR_MN_ISO", "x", "y"]
].drop_duplicates()

# Example usage
selected_vector = select_nearest_points(temp, city_df, "UC_NM_MN", "y", "x")
df = selected_vector.to_dataframe().reset_index()
print(df)
