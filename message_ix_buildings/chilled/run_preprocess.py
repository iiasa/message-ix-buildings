from preprocess.archetypes import (  # type: ignore
    create_archetype_variables,
    create_archetypes,
)
                                   create_archetypes)
from utils.config import Config  # type: ignore

cfg = Config()

# check to see if "map_reg_MESSAGE_{config.node}.nc" exists
# if not, create the file
# if not os.path.exists(
#     os.path.join(cfg.dle_path, "out", "raster", "map_reg_MESSAGE_" + cfg.node + ".nc")
# ):
#     create_message_raster(cfg)
# else:
#     print(f"map_reg_MESSAGE_{cfg.node}.nc already exists. Using existing file.")

# run create_archetypes
create_archetypes(cfg)
print("Archetypes created successfully.")

# create maps of archetype variables
create_archetype_variables(cfg)
print("Archetype variables created successfully.")
