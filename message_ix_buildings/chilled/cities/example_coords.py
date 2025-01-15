import numpy as np
import pandas as pd
import xarray as xr

# Load example temperature data
temp_file = "/Volumes/mengm.pdrv/watxene/ISIMIP/ISIMIP3b/InputData/climate_updated/bias-adjusted/ssp126/GFDL-ESM4/gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020.nc"

# Read the temperature data
temp = xr.open_dataarray(temp_file)

# Example: List of city coordinates (lat, lon)
city_coords = [
    {"city": "London", "lat": 51.5074, "lon": -0.1278},
    {"city": "New York", "lat": 40.7128, "lon": -74.0060},
    {"city": "Tokyo", "lat": 35.6895, "lon": 139.6917},
]


def select_nearest_points(
    ras: xr.DataArray, city_df: pd.DataFrame, name_col: str, lat_col: str, lon_col: str
):
    # Step 1: Stack lat and lon of the ras into a single dimension 'locations'
    stacked_ras = ras.stack(locations=("lat", "lon"))

    # Step 2: Find the nearest indices manually by calculating the distances
    selected_points = []
    for _, row in city_df.iterrows():
        city, lat, lon = row[name_col], row[lat_col], row[lon_col]
        print("city: " + str(lat) + ", " + str(lon))

        # Find the nearest index by calculating the distance between each point
        # and the given coordinates
        dist = np.sqrt((stacked_ras.lat - lat) ** 2 + (stacked_ras.lon - lon) ** 2)
        nearest_idx = dist.argmin()  # index of the nearest point

        # Select the nearest point based on the index
        nearest_point = stacked_ras.isel(locations=nearest_idx)

        # Add original city, lat, and lon from city_df
        nearest_point["city"] = city
        nearest_point["city_lat"] = lat
        nearest_point["city_lon"] = lon
        # Append the selected point to the list
        selected_points.append(nearest_point)

    # Step 3: Concatenate the selected points into a single 1D xarray
    selected_vector = xr.concat(selected_points, dim="locations")

    return selected_vector


# Example usage
city_df = pd.DataFrame(city_coords)
selected_vector = select_nearest_points(temp, city_df, "city", "lat", "lon")
df = selected_vector.to_dataframe().reset_index()
print(df)
