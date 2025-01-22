import os

import dask
import numpy as np
import pandas as pd
import xarray as xr
from dask import delayed

from message_ix_buildings.chilled.util.common import get_logger, get_project_root

log = get_logger(__name__)


def select_nearest_points(
    ras: xr.DataArray | xr.Dataset,
    city_df: pd.DataFrame,
    name_col: str,
    lat_col: str,
    lon_col: str,
) -> xr.DataArray | xr.Dataset:
    """Find nearest points in raster data for each city provided

    This function does a more manual process of finding the nearest points in the
    raster data for each city provided in the city_df DataFrame.
    First, the lat, lon dimensions of the raster data are stacked into a single
    dimension 'locations'.

    The method is:
    1. **Stacking**: We combine the `lat` and `lon` dimensions into a new `locations`
    dimension, which reduces the dimensionality.
    2. **Manual distance calculation**:
        - We calculate the Euclidean distance (or any other metric) between each
        city's coordinates and the grid's latitude and longitude.
        - Use `np.sqrt((stacked_temp.lat - lat)**2 + (stacked_temp.lon - lon)**2)`
        to compute the distance.
        - Find the index of the smallest distance using `dist.argmin()`.
    3. **Using `.isel()`**: After finding the nearest index, we use
    `.isel(locations=nearest_idx)` to select the nearest value from the stacked
    data.
    4. **Concatenation**: Finally, we concatenate the selected points (cities) into
    a single 1D array using `xr.concat()`.

    Using the `method=nearest` with `xarray.sel()` would not
    work, as when you use `.stack()` on `lat` and `lon`, it creates a **multi-indexed**
    dimension, and `method='nearest'` does not support multi-indexing. But without
    creating the stacked dimension, the `.sel(method='nearest')` function would instead
    find the nearest latitude AND the nearest longitude, which is not what we want.

    Parameters
    ----------
    ras : xr.DataArray
        Raster data containing lat and lon dimensions
    city_df : pd.DataFrame
        DataFrame containing city names and coordinates
    name_col : str
        Column name for city names
    lat_col : str
        Column name for latitude of cities
    lon_col : str
        Column name for longitude of cities


    Returns
    -------
    selected_vector : xr.DataArray
        1D xarray containing the selected points (cities) from the raster data, with the
        city names and coordinates added as additional variables
    """
    # Step 1: Stack lat and lon of the ras into a single dimension 'locations'
    log.info("Stacking lat and lon of the ras into a single dimension 'locations'")
    stacked_ras = ras.stack(locations=("lat", "lon"))

    # Step 2: Find the nearest indices manually by calculating the distances
    log.info(
        "Finding the nearest indices manually by calculating the distances \
            for each city"
    )
    selected_points = []
    for _, row in city_df.iterrows():
        city, lat, lon = row[name_col], row[lat_col], row[lon_col]

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
    log.info("Concatenating the selected points into a single 1D xarray")
    selected_vector = xr.concat(selected_points, dim="locations")  # type: ignore

    return selected_vector


@delayed
def process_raster_data(data_path, var, gcm, rcp):
    # selections
    var = var
    gcm = gcm
    rcp = rcp

    log.info(f"Searching for files in {data_path} that match the string: ")
    log.info(f"{gcm}_r1i1p1f1_w5e5_{rcp}_{var}_global_daily_")
    l_files = []
    for root, dirs, files in os.walk(data_path):
        for file in files:
            # change gcm to lower case
            gcm_l = gcm.lower()
            if f"{gcm_l}_r1i1p1f1_w5e5_{rcp}_{var}_global_daily_" in file:
                l_files.append(os.path.join(root, file))

    # sort l_files
    l_files.sort()

    # green space file location, relative to the root directory
    root_path = get_project_root()
    green_path = os.path.join(root_path, "data", "green-space", "ALPS2024")

    # Example: List of city coordinates (lat, lon)
    city_df = pd.read_csv(os.path.join(green_path, "outer.csv"))[
        ["UC_NM_MN", "CTR_MN_ISO", "x", "y"]
    ].drop_duplicates()

    # create function to read in raster file, apply select_nearest_points,
    # and convert to pandas.DataFrame
    @delayed
    def extract_raster_file(file):
        log.info(f"Extracting raster data from file: {file}")
        ras = xr.open_dataarray(file)

        log.info("...Selecting nearest points from raster data")
        selected_vector = select_nearest_points(ras, city_df, "UC_NM_MN", "y", "x")

        log.info("...Converting selected points to pandas DataFrame")
        df = selected_vector.to_dataframe().reset_index()

        return df

    # NOTE: this uses dask to parallelize the computation
    log.info("Define applying function to all files in l_files")
    delayed_extract = [extract_raster_file(file) for file in l_files]

    # Compute the results in parallel
    log.info("Compute the results in parallel using dask")
    extract = dask.compute(*delayed_extract)

    # Concatenate the results
    df_extract = pd.concat(extract)

    # add gcm, rcp, and var columns
    df_extract["gcm"] = gcm
    df_extract["rcp"] = rcp
    df_extract["var"] = var

    return df_extract
