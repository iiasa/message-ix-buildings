import datetime
import os

import dask
import pandas as pd
import xarray as xr

from message_ix_buildings.chilled.core.climate import create_climate_variables_maps_city
from message_ix_buildings.chilled.functions.extract_cities import (
    combine_rasters,
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
cfg = Config(vstr="ALPS2024_cities", user="MEAS")
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

# Example: List of city coordinates (lat, lon)
city_df = city_lcz[["UC_NM_MN", "CTR_MN_ISO", "x", "y"]].drop_duplicates()

# apply create_climate_variables_maps to cfg
start = datetime.datetime.now()
delayed_vars = dask.delayed(create_climate_variables_maps_city)(
    config=cfg,
    start_time=start,
    extract_cities=True,
    city_df=city_df,
    name_col="UC_NM_MN",
    lat_col="y",
    lon_col="x",
)

delayed_vars.compute()


# Run combine_rasters for just one GCM and RCP
ras_scen = combine_rasters(isimip_bias_adj_path, sel_var, "MRI-ESM2-0", "ssp126")

# find the nearest points to the city coordinates
points_scen = select_nearest_points(ras_scen, city_df, "UC_NM_MN", "y", "x")

# remove the locations coordinates
points_scen = points_scen.drop_vars("locations")


# remove the 'city' data_vars
points_scen = points_scen.drop_vars("city")

# Ensure points_scen is an xarray DataArray or Dataset
if not isinstance(points_scen, (xr.DataArray, xr.Dataset)):
    points_scen = xr.DataArray(points_scen)

# Convert data variables to a compatible format
for var in points_scen.data_vars:
    points_scen[var] = points_scen[var].astype("float32")

# Ensure points_scen is an xarray DataArray or Dataset
if not isinstance(points_scen, (xr.DataArray, xr.Dataset)):
    points_scen = xr.DataArray(points_scen)


# # calculate average temperature
# t_out_ave = points_scen[cfg.davar].astype("float32") - 273.16
# t_out_ave = t_out_ave.transpose("locations", "time")
# t_oa_gbm = t_out_ave.groupby("time.month")

i_sol_v = xr.open_dataarray(
    os.path.join(dle_path, "EWEMBI_vert_irrad_1980-2009_avg.nc")
)  # Values  in daily Wh/m2
i_sol_v_points = select_nearest_points(i_sol_v, city_df, "UC_NM_MN", "y", "x")

# Horizontal irradiation
i_sol_h = xr.open_dataarray(
    os.path.join(dle_path, "EWEMBI_horiz_irrad_1980-2009_avg.nc")
)  # Values in daily Wh/m2
i_sol_h_points = select_nearest_points(i_sol_h, city_df, "UC_NM_MN", "y", "x")

save_path = "/Users/meas/iiasagit/message-ix-buildings/out/version/ALPS2024_cities"

# try saving points_scen, i_sol_v_points, i_sol_h_points to save_path as netCDF files
points_scen.to_netcdf(os.path.join(save_path, "points_scen.nc"))
i_sol_v_points.to_netcdf(os.path.join(save_path, "i_sol_v_points.nc"))
i_sol_h_points.to_netcdf(os.path.join(save_path, "i_sol_h_points.nc"))

# # load run data
# vers_archs = get_archs(cfg)
# par_var = load_parametric_analysis_data(cfg)


# dfa = pd.DataFrame(columns=["H_v_cl", "H_v_op", "H_tr"], index=par_var.index)

# # set parset as first row of par_var
# parset = par_var.iloc[0]
# clim = cfg.clims[0]

# suff = str(clim) + "_" + arch  # suffix
# # suff =  clim+'_'+arch+'_'+str(parset.name_run)  #suffix
# suff1 = arch  # only arch (for imports arch data)


# log.info("Starting: " + suff + "_" + str(parset.name_run))
# if config.cool == 1:
#     cop = parset.cop
#     t_sp_c = np.int8(parset.t_sp_c)  # Indoor setpoint temperature for cooling -> 26
#     t_sp_c_max = np.int8(
#         parset.t_sp_c_max
#     )  # Indoor max temperature when fans are on (°C) -> 28

#     f_c = parset.f_c
#     f_f = parset.f_f

# if config.heat == 1:
#     t_sp_h = np.int8(
#         parset.t_sp_h
#     )  # Indoor max temperature when fans are on (°C) -> 20
#     eff = parset.eff  # Efficiency heating system

#     f_h = parset.f_h
