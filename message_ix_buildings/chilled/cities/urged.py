import os

import dask
import pandas as pd
import xarray as xr

from message_ix_buildings.chilled.functions.buildings_funcs_grid import calc_SCDD_m
from message_ix_buildings.chilled.functions.extract_cities import (
    combine_rasters,
    rasters_to_df_cities,
    select_nearest_points,
)
from message_ix_buildings.chilled.util.base import get_paths
from message_ix_buildings.chilled.util.common import get_logger, get_project_root
from message_ix_buildings.chilled.util.config import Config  # type: ignore

log = get_logger(__name__)

# list of GCMs and RCPs
list_gcm = ["MRI-ESM2-0"]
list_rcp = ["baseline", "ssp126", "ssp370"]

# specify config
cfg = Config(user="MEAS")
sel_var = cfg.var
isimip_bias_adj_path = get_paths(cfg, "isimip_bias_adj_path")
dle_path = get_paths(cfg, "dle_path")

# green space file location, relative to the root directory
root_path = get_project_root()
green_path = os.path.join(root_path, "data", "green-space", "ALPS2024")

# read in all files in green_path
city_lcz = pd.read_csv(os.path.join(green_path, "outer.csv")).drop(
    columns=["Unnamed: 0"]
)
city_coef = pd.read_csv(os.path.join(green_path, "outer_2.csv")).drop(
    columns=["Unnamed: 0"]
)
city_gvi = pd.read_csv(os.path.join(green_path, "outer_3.csv")).drop(
    columns=["Unnamed: 0"]
)
hist_gvi = pd.read_csv(os.path.join(green_path, "outer_4.csv")).drop(
    columns=["Unnamed: 0"]
)

# Example: List of city coordinates (lat, lon)
city_df = city_lcz[["UC_NM_MN", "CTR_MN_ISO", "x", "y"]].drop_duplicates()

# Run combine_rasters for just one GCM and RCP
ras_scen = combine_rasters(isimip_bias_adj_path, sel_var, "MRI-ESM2-0", "ssp126")

# wrap combine_rasters in delayed, and apply to all combinations of gcm and rcp
delayed_combine_rasters = [
    dask.delayed(combine_rasters)(isimip_bias_adj_path, sel_var, gcm, rcp)
    for gcm in list_gcm
    for rcp in list_rcp
]

# Compute the results in parallel
log.info("Compute the results in parallel using dask")
ras_all = dask.compute(*delayed_combine_rasters)

# Apply select_nearest_points to the each xr.Dataset in ras_all
delayed_points_all = [
    dask.delayed(select_nearest_points)(ras, city_df, "UC_NM_MN", "y", "x")
    for ras in ras_all
]

# Compute the results in parallel
points_all = dask.compute(*delayed_points_all)

points_scen = select_nearest_points(ras_scen, city_df, "UC_NM_MN", "y", "x")

# calculate average temperature
t_out_ave = points_scen[cfg.davar].astype("float32") - 273.16
t_out_ave = t_out_ave.transpose("locations", "time")
t_oa_gbm = t_out_ave.groupby("time.month")

# try calculation
bal_temp = cfg.bal_temps[1]
sdd_c = calc_SCDD_m(t_out_ave, bal_temp)


i_sol_v = xr.open_dataarray(
    os.path.join(dle_path, "input_data", "EWEMBI_vert_irrad_1980-2009_avg.nc")
)  # Values  in daily Wh/m2

# Horizontal irradiation
i_sol_h = xr.open_dataarray(
    os.path.join(dle_path, "input_data", "EWEMBI_horiz_irrad_1980-2009_avg.nc")
)  # Values in daily Wh/m2


# calculate average temperature
t_out_ave = ras_scen[cfg.davar].astype("float32") - 273.16
t_out_ave = t_out_ave.transpose("lat", "lon", "time")
t_oa_gbm = t_out_ave.groupby("time.month")

# try calculation

bal_temp = cfg.bal_temps[1]
sdd_c = calc_SCDD_m(t_out_ave, bal_temp)


# NOTE: following is for parallelizing the process_raster_data function


# NOTE: following is doing everything in dataframes instead

# apply process_raster_data to all combinations of gcm and rcp
delayed_results = [
    rasters_to_df_cities(
        isimip_bias_adj_path, sel_var, gcm, rcp, city_df, "UC_NM_MN", "y", "x"
    )
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
