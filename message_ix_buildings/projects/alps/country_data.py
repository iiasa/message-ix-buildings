import os

from message_ix_buildings.chilled.util.base import get_paths
from message_ix_buildings.chilled.util.common import get_project_root
from message_ix_buildings.chilled.util.config import Config  # type: ignore
from message_ix_buildings.projects.alps.util.data import read_and_concat_excel_sheets

# Specify config and data paths
config = Config(user="MEAS")
dle_path = get_paths(config, "dle_path")
buildings_path = (
    "/Users/meas/Library/CloudStorage/OneDrive-SharedLibraries-IIASA/DLE - Analysis/\
    Climate impacts and space conditioning/Analysis/ALPS2022_data_processing/output"
)

# Specify input files
shr_file = "building_stock_SSP2_v19_ALPS_ssps.csv"
ac_file = "country_data_v4.xlsx"

# Specify output file
root_path = get_project_root()
save_path = os.path.join(root_path, "data", "country", "country_ac_penetration.csv")


# Apply function to read and concatenate Excel sheets
df_ac = read_and_concat_excel_sheets(os.path.join(dle_path, ac_file))
print(df_ac)

# Save to CSV
combined_df.to_csv(save_path, index=False)
