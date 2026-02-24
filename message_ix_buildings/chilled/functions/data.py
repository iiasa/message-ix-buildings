import os

import xarray as xr

from message_ix_buildings.chilled.util.base import (
    get_archs,
    get_paths,
    load_parametric_analysis_data,
)
from message_ix_buildings.chilled.util.common import get_logger
from message_ix_buildings.chilled.util.config import Config  # type: ignore

log = get_logger(__name__)


def average_temperature(config: "Config"):
    project_path = get_paths(config, "project_path")
    dle_path = get_paths(config, "dle_path")
    isimip_bias_adj_path = get_paths(config, "isimip_bias_adj_path")
    isimip_ewembi_path = get_paths(config, "isimip_ewembi_path")

    out_path = os.path.join(project_path, "out", "version", config.vstr)
    archetype_path = os.path.join(out_path, "rasters")
    save_path = os.path.join(out_path, "VDD_ene_calcs")

    output_path_vdd = os.path.join(
        save_path,
        config.gcm,
        config.rcp,
    )

    if not os.path.exists(output_path_vdd):
        os.makedirs(output_path_vdd)

    vers_archs = get_archs(config)
    par_var = load_parametric_analysis_data(config)

    nyrs_clim = int(years_clim[1]) - int(years_clim[0]) + 1

    # Climate input variable format
    climate_filestr_hist = (
        f"tas_day_{config.gcm}_{config.rcpdata}_r1i1p1_EWEMBI_landonly_"  # ISIMIP2
    )

    if config.gcm == "UKESM1-0-LL":
        climate_filestr_future = (
            f"{config.gcm}_r1i1p1f2_w5e5_{config.rcpdata}_{config.var}_global_daily_"
        )
    else:
        climate_filestr_future = (
            f"{config.gcm}_r1i1p1f1_w5e5_{config.rcpdata}_{config.var}_global_daily_"
        )

    endstr = ".nc"

    if str(clim) == "hist":
        isi_folder = isimip_ewembi_path
        filestr = climate_filestr_hist
    else:
        isi_folder = isimip_bias_adj_path
        filestr = climate_filestr_future

    filepath = os.path.join(
        isi_folder, config.rcpdata, config.gcm, f"{filestr.lower()}*{endstr}"
    )

    log.info("Reading: " + filepath)
    if config.rcp == "rcp26":
        dst = xr.open_mfdataset(
            filepath,
            chunks={"lon": get_paths(config, "chunk_size")},
            concat_dim="time",
            use_cftime=True,
        )  # Setting for RCP2.6
    else:
        dst = xr.open_mfdataset(
            filepath,
            chunks={"lon": get_paths(config, "chunk_size")},
        )  # , concat_dim='time' )  # Setting for RCP6.0

    dst_crop = dst.sel(time=slice(years_clim[0], years_clim[1]))
    t_out_ave = dst_crop[config.davar].astype("float32") - 273.16
    t_out_ave = t_out_ave.transpose("lat", "lon", "time")
    t_oa_gbm = t_out_ave.groupby("time.month")
