"""
Create raster of MESSAGE regions for CHILLED with specified nodes
"""

import datetime
import os

import numpy as np
import pandas as pd
import xarray as xr

from message_ix_buildings.chilled.util.base import get_paths
from message_ix_buildings.chilled.util.config import Config


def create_message_raster(config: "Config"):
    """
    Process global raster into MESSAGE regions

    Parameters
    ----------
    config : .Config
        The function responds to, or passes on to other functions, the fields:
        :attr:`~.Config.dle_path` and
        :attr:`~.Config.node`,

    Returns
    -------
    country_ras : xarray.DataArray
        Raster of countries
    reg_ras : xarray.Dataset
        Raster of regions
    map_reg : xarray.DataArray
        Map of regions
    iso_attrs : pd.DataFrame
        ISO attributes in a dataframe

    """
    input_path = get_paths(config, "dle_path")
    message_region_file = get_paths(config, "message_region_map_file")

    if config.node == "R11":
        msgregions = pd.read_excel(
            message_region_file,
            sheet_name="regional definition",
        )

        # Country raster
        country_ras = xr.open_dataarray(
            os.path.join(input_path, "gaul_lvl0_hybrid_05_3.nc")
        )

        country_ras.values = country_ras.values.astype(float)
        country_ras.values[country_ras == -1] = np.nan

        iso_attrs = pd.DataFrame([country_ras.attrs]).T.rename(columns={0: "ISO"})

        # Region raster
        reg_ras = xr.Dataset({"MESSAGE11": country_ras.copy(deep=True)})
        reg_ras["ISO"] = country_ras.copy(deep=True)

        for row in iso_attrs.itertuples():
            code = row.ISO  # get country ISO code
            regval = msgregions.loc[msgregions.iso_code == code, "RegNum"]
            if len(regval.values) != 0:
                reg_ras.MESSAGE11.values[
                    country_ras.values == float(str(row.Index))
                ] = regval
            else:
                print(row.Index)
                print()

        # % manual post process for pixels with the fill value
        # Latin america
        tarr = reg_ras.MESSAGE11.sel(lat=slice(25.25, -55.25), lon=slice(-180, -21))
        tarr.values[tarr.values < -10] = 5
        reg_ras.MESSAGE11.sel(
            lat=slice(25.25, -55.25), lon=slice(-180, -21)
        ).values = tarr

        # Svalbard etc
        tarr = reg_ras.MESSAGE11.sel(lat=slice(84, 55), lon=slice(-30, 45))
        tarr.values[tarr.values < -10] = 11
        reg_ras.MESSAGE11.sel(lat=slice(84, 55), lon=slice(-30, 45)).values = tarr

        # SSA etc
        tarr = reg_ras.MESSAGE11.sel(lat=slice(24, -55), lon=slice(-30, 75))
        tarr.values[tarr.values < -10] = 1
        reg_ras.MESSAGE11.sel(lat=slice(24, -55), lon=slice(-30, 75)).values = tarr

        # other pacific Asia
        tarr = reg_ras.MESSAGE11.sel(lat=slice(25, -55), lon=slice(90, 180))
        tarr.values[tarr.values < -10] = 9
        reg_ras.MESSAGE11.sel(lat=slice(25, -55), lon=slice(90, 180)).values = tarr

        # british Indian Ocean territory to South ASIA
        tarr = reg_ras.MESSAGE11.sel(lat=slice(0, -10), lon=slice(65, 75))
        tarr.values[tarr.values < -10] = 10
        reg_ras.MESSAGE11.sel(lat=slice(0, -10), lon=slice(65, 75)).values = tarr

        # St Miquelon  island in Canada?
        tarr = reg_ras.MESSAGE11.sel(lat=slice(48, 44), lon=slice(-60, -50))
        tarr.values[tarr.values < -10] = 10
        reg_ras.MESSAGE11.sel(lat=slice(48, 44), lon=slice(-60, -50)).values = tarr

        # Midway atoll
        tarr = reg_ras.MESSAGE11.sel(lat=slice(80, 20), lon=slice(-180, -150))
        tarr.values[tarr.values < -10] = 10
        reg_ras.MESSAGE11.sel(lat=slice(80, 20), lon=slice(-180, -150)).values = tarr

        reg_ras.MESSAGE11.plot()

        # reg_ras.MESSAGE11.where(reg_ras.MESSAGE11 < -1).plot() # Check if worked..

        map_reg = reg_ras.MESSAGE11.copy(deep=True)

        # Write out to netcdf
        map_reg.attrs = {
            "title": "Map MESSAGE R11 regions",
            "authors": "Edward Byers & Alessio Mastrucci",
            "date": str(datetime.datetime.now()),
            "institution": "IIASA Energy Program",
            "contact": "byers@iiasa.ac.at; mastrucc@iiasa.ac.at",
        }

        return country_ras, reg_ras, map_reg, iso_attrs

    else:
        raise TypeError("Only R11 is supported at the moment.")
