import os

from preprocess.message_raster import create_archetype_template_map  # type: ignore
from utils.config import Config  # type: ignore
from utils.config import Config  # type: ignore

cfg = Config()

# check to see if "map_reg_MESSAGE_{config.node}.nc" exists
# if not, create the file
if not os.path.exists(
    os.path.join(cfg.dle_path, "out", "raster", "map_reg_MESSAGE_{cfg.node}.nc")
):
    create_archetype_template_map(cfg)
else:
    print(f"map_reg_MESSAGE_{cfg.node}.nc already exists")
