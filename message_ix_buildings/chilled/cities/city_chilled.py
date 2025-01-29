import dask
from dask import delayed

from message_ix_buildings.chilled.cities.util.climate import process_climate_data
from message_ix_buildings.chilled.util.common import get_logger
from message_ix_buildings.chilled.util.config import Config  # type: ignore

log = get_logger(__name__)

# list of GCMs and RCPs
list_gcm = ["MRI-ESM2-0"]
list_rcp = ["baseline", "ssp126", "ssp370"]

# specify config
config = Config(vstr="ALPS2024_cities", user="MEAS", gcm="MRI-ESM2-0", rcp="ssp585")
climate_zones = False

tasks = []
for gcm in list_gcm:
    for rcp in list_rcp:
        config = Config(vstr="ALPS2024_cities", user="MEAS_UNICC", gcm=gcm, rcp=rcp)
        task = delayed(process_climate_data)(config, climate_zones)
        tasks.append(task)

dask.compute(*tasks)
