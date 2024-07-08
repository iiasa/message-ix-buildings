from message_ix_buildings.chilled.config import Config
from message_ix_buildings.chilled.preprocess.message_raster import (
    create_archetype_template_map,
)

cfg = Config()

create_archetype_template_map(cfg)
