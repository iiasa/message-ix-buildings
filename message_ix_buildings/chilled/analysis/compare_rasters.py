import xarray as xr


def compare_rasters(raster1_path, raster2_path):
    # Load the raster files as xarray datasets
    raster1 = xr.open_dataset(raster1_path)
    raster2 = xr.open_dataset(raster2_path)

    # Compare the values of the two rasters
    comparison = raster1 == raster2

    # Check if all values are the same
    if comparison.all():
        print("The rasters are identical.")
    else:
        print("The rasters are different.")


compare_rasters(raster1_path, raster2_path)

# Example usage
raster1_path = "/projects/chilled/out/version/ALPS2024_heat/VDD_ene_calcs/GFDL-ESM4/ssp126/2060_exist_1_vdd_hALL.nc"
raster2_path = "/projects/chilled/out/version/ALPS2024_heat/VDD_ene_calcs/GFDL-ESM4/ssp370/2060_exist_1_vdd_hALL.nc"

raster1 = xr.open_dataset(raster1_path)
raster2 = xr.open_dataset(raster2_path)

# Compare the values of the two rasters
comparison = raster1 == raster2

# Check if all values are the same
if comparison.all():
    print("The rasters are identical.")
else:
    print("The rasters are different.")
