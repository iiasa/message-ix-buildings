import datetime

from main.climate import create_climate_outputs  # type: ignore
from utils.config import Config  # type: ignore

start = datetime.datetime.now()

cfg = Config()

# create climate outputs
create_climate_outputs(cfg, start)

end = datetime.datetime.now()
