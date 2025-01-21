import os

import dask
import pandas as pd
import xarray as xr
from dask import delayed

from message_ix_buildings.chilled.functions.extract_cities import select_nearest_points
from message_ix_buildings.chilled.util.common import get_logger, get_project_root

log = get_logger(__name__)

# list of GCMs and RCPs
sel_var = "tas"
list_gcm = ["GFDL-ESM4", "IPSL-CM6A-LR"]
list_rcp = ["ssp126", "ssp370", "ssp585"]

# path to the data
var_path = "/Volumes/mengm.pdrv/watxene/ISIMIP/ISIMIP3b/InputData/climate_updated/bias-adjusted"


@delayed
def process_raster_data(data_path, var, gcm, rcp):
    # selections
    var = var
    gcm = gcm
    rcp = rcp

    # Load example temperature data
    # temp_file = "/Users/meas/Library/CloudStorage/OneDrive-IIASA/Documents/chilled/ISIMIP3b/gfdl-esm4_r1i1p1f1_w5e5_ssp126_tas_global_daily_2015_2020.nc"

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


# apply process_raster_data to all combinations of gcm and rcp
delayed_results = [
    process_raster_data(var_path, sel_var, gcm, rcp)
    for gcm in list_gcm
    for rcp in list_rcp
]

# Compute the results in parallel
log.info("Compute the results in parallel using dask")
results = dask.compute(*delayed_results)


# # Example usage:
# # df_results = process_raster_data("tas", "GFDL-ESM4", "ssp126").compute()

# # NOTE: if not using dask, remove the @delayed decorator from extract_raster_file
# # and run this instead:
# df_con = pd.concat([extract_raster_file(file) for file in l_files])

# # NOTE: could also read in all files at once using xr.open_mfdataset
# # and apply select_nearest_points to the resulting xr.Dataset
# # but the problem is that converting the resulting xr.Dataset to a pandas.DataFrame
# # is increeeeeddibllyyyyy slow
# ras_all = xr.open_mfdataset(l_files, combine="by_coords")
# sel_points_all = select_nearest_points(ras_all, city_df, "UC_NM_MN", "y", "x")


# def extract_raster_dataset(l_files):
#     ras_all = xr.open_mfdataset(l_files, combine="by_coords")
#     sel_points_all = select_nearest_points(ras_all, city_df, "UC_NM_MN", "y", "x")
#     sel_points_all = sel_points_all.compute()
#     df = sel_points_all.to_dataframe().reset_index()

#     return df
