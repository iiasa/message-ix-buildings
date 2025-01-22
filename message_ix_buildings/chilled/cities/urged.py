import dask
import pandas as pd

from message_ix_buildings.chilled.functions.extract_cities import process_raster_data
from message_ix_buildings.chilled.util.base import get_paths
from message_ix_buildings.chilled.util.common import get_logger
from message_ix_buildings.chilled.util.config import Config  # type: ignore

log = get_logger(__name__)

# list of GCMs and RCPs
sel_var = "tas"
list_gcm = ["GFDL-ESM4", "IPSL-CM6A-LR"]
list_rcp = ["ssp126", "ssp370"]

# specify config
cfg = Config(user="MEAS")
isimip_bias_adj_path = get_paths(cfg, "isimip_bias_adj_path")

# apply process_raster_data to all combinations of gcm and rcp
delayed_results = [
    process_raster_data(isimip_bias_adj_path, sel_var, gcm, rcp)
    for gcm in list_gcm
    for rcp in list_rcp
]

# Compute the results in parallel
log.info("Compute the results in parallel using dask")
results = dask.compute(*delayed_results)

# Concatenate results
log.info("Concatenate the results into single dataframe")
df_results = pd.concat(results).reset_index(drop=True)


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
