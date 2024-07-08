"""
Create raster of MESSAGE regions for CHILLED with specified nodes
"""

import datetime
import os

import pandas as pd  # type: ignore
import xarray as xr  # type: ignore
from utils.config import Config  # type: ignore


def create_archetype_template_map(config: "Config"):
    input_path = os.path.join(config.dle_path, "data")
    output_path = os.path.join(config.dle_path, "out", "raster")

    if config.node == "R11":
        msgregions = pd.read_excel(
            config.message_region_file,
            sheet_name="regional definition",
        )

        # Country raster
        country_ras = xr.open_dataarray(
            os.path.join(input_path, "gaul_lvl0_hybrid_05_3.nc")
        )

        ISO_attrs = pd.DataFrame([country_ras.attrs]).T.rename(columns={0: "ISO"})

        # Region raster
        reg_ras = xr.Dataset({"MESSAGE11": country_ras.copy(deep=True)})
        reg_ras["ISO"] = country_ras.copy(deep=True)

        for row in ISO_attrs.itertuples():
            code = row.ISO  # get country ISO code
            regval = msgregions.loc[msgregions.iso_code == code, "RegNum"]
            if regval.values.size != 0:
                reg_ras.MESSAGE11.values[country_ras.values == float(row.Index)] = (
                    regval
                )
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

        filename = "map_reg_MESSAGE_" + config.node + ".nc"

        map_reg.to_netcdf(os.path.join(output_path, filename))

        print(
            "Saved MESSAGE and raster map data to "
            + os.path.join(output_path, filename)
        )

    else:
        print("Only R11 is supported at the moment.")
