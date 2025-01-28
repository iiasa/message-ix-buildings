import pandas as pd


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
