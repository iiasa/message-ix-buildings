import numpy as np
import pandas as pd
import xarray as xr

# Load example temperature data
temp_file = "/Volumes/mengm.pdrv/watxene/ISIMIP/ISIMIP3b/InputData/climate_updated/bias-adjusted/ssp126/GFDL-ESM4/gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020.nc"

# Read the temperature data
temp = xr.open_dataarray(temp_file)

# Example: List of city coordinates (lat, lon)
city_coords = [
    (51.5074, -0.1278),  # London
    (40.7128, -74.0060),  # New York
    (35.6895, 139.6917),  # Tokyo
]

# Convert to a pandas DataFrame for clarity
city_df = pd.DataFrame(city_coords, columns=["lat", "lon"])

# Step 1: Stack lat and lon into a single dimension 'locations'
stacked_temp = temp.stack(locations=("lat", "lon"))

# Step 2: Find the nearest indices manually by calculating the distances
selected_points = []
for _, row in city_df.iterrows():
    lat, lon = row["lat"], row["lon"]
    print("city: " + str(lat) + ", " + str(lon))

    # Find the nearest index by calculating the distance between each point
    # and the given coordinates
    dist = np.sqrt((stacked_temp.lat - lat) ** 2 + (stacked_temp.lon - lon) ** 2)
    nearest_idx = dist.argmin()  # index of the nearest point

    # Select the nearest point based on the index
    nearest_point = stacked_temp.isel(locations=nearest_idx)

    # Add lat, lon as coordinates as city_lat, city_lon
    nearest_point["city_location"] = "(" + str(lat) + ", " + str(lon) + ")"
    nearest_point["city_lat"] = lat
    nearest_point["city_lon"] = lon

    # Append the selected point to the list
    selected_points.append(nearest_point)

# Step 3: Concatenate the selected points into a single 1D xarray
selected_vector = xr.concat(selected_points, dim="locations")

# Display results as a 1D array or convert to pandas DataFrame if preferred
df = selected_vector.to_dataframe().reset_index()
print(df)
