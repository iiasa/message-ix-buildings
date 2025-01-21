import numpy as np
import pandas as pd
import xarray as xr

from message_ix_buildings.chilled.util.common import get_logger

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
