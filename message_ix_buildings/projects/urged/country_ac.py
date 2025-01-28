import os

import pandas as pd

from message_ix_buildings.chilled.util.base import get_paths
from message_ix_buildings.chilled.util.common import get_project_root
from message_ix_buildings.chilled.util.config import Config  # type: ignore


def prettify_column_names(df):
    # Convert column names to lowercase and replace spaces with underscores
    df.columns = df.columns.str.lower().str.replace(" ", "_")
    return df


def read_and_concat_excel_sheets(file_path):
    # Read the Excel file
    xls = pd.ExcelFile(file_path)

    # Initialize an empty list to store DataFrames
    df_list = []

    # Iterate over each sheet name
    for sheet_name in xls.sheet_names:
        # Skip the sheet named "read_me"
        if sheet_name == "read_me":
            continue

        # Read the sheet into a DataFrame
        df = pd.read_excel(xls, sheet_name=sheet_name)

        # Drop the last column titled "Unnamed: 5"
        if "Unnamed: 5" in df.columns:
            df = df.drop(columns=["Unnamed: 5"])

        # Extract ssp and year from the sheet name
        ssp, year = sheet_name.split("_")

        # Add ssp and year columns to the DataFrame
        df["ssp"] = ssp
        df["year"] = year

        # Prettify column names
        df = prettify_column_names(df)

        # Append the DataFrame to the list
        df_list.append(df)

    # Concatenate all DataFrames in the list
    concatenated_df = pd.concat(df_list, ignore_index=True)

    return concatenated_df


# Specify config and data path
config = Config(user="MEAS")
dle_path = get_paths(config, "dle_path")
data_file = "country_data_v4.xlsx"
root_path = get_project_root()
save_path = os.path.join(root_path, "data", "country", "country_ac_penetration.csv")

# Apply function to read and concatenate Excel sheets
combined_df = read_and_concat_excel_sheets(os.path.join(dle_path, data_file))
print(combined_df)

# Save to CSV
combined_df.to_csv(save_path, index=False)
