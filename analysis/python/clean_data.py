from load_data import get_data
import pandas as pd
import numpy as np

def steel_filter_columns(df):
    df_filtered = df.drop(columns=[
                                "Plant name (other language)",
                            ], axis=1)
    return(df_filtered)

def bf_filter_columns(df):
    df_filtered = df.drop(columns=[
                                "GEM Wiki Page",
                                "Unit Age",
                                "Idled Date",
                                "Furnace Manufacturer",
                                "Furnace Model",
                                "Current Size (m3)",
                                "Relining Type",
                                "Furnace Relining Duration (Days)",
                                "Furnace Relining Number",
                                "Relining Cost",
                                "Relining Cost Unit",
                                "Relining Capacity Expansion",
                                "Relining Capacity Expansion Unit",
                                "Furnace Relining Start Date",
                                "Most Recent Relining",
                                "CCS/CCUS",
                                "Relining Status",#could be kept and used in the analysis
                            ], axis=1)
    return(df_filtered)

def indicate_steel_prod(df):
    methods = ['BOF', 'EAF', 'OHF']
    other_column = 'Other/unspecified steel capacity (ttpa)'

    for method in methods:
        column_name = f'Nominal {method} steel capacity (ttpa)'
        if column_name in df.columns:
            # Create a new column with True if value > 0, otherwise False
            df[method] = df[column_name].notna()

    if other_column in df.columns:
        df['Other steel'] = df[other_column].notna()
    return(df)

def indicate_iron_prod(df):
    methods = ['BF', 'DRI']
    other_column = 'Other/unspecified iron capacity (ttpa)'

    for method in methods:
        column_name = f'Nominal {method} capacity (ttpa)'
        if column_name in df.columns:
            # Create a new column with True if value > 0, otherwise False
            df[method] = df[column_name].notna()

    if other_column in df.columns:
        df['Other iron'] = df[other_column].notna()
    return(df)

def remove_cancelled_plants(df):
    #remove cancelled plants
    not_cancelled = df[~df["Capacity operating status"].isin(["cancelled"])]
    return not_cancelled

def remove_retired_plants(df):
    not_retired = df[~df["Capacity operating status"].isin(["retired", "mothballed", "mothballed pre-retirement"])]
    return not_retired

def remove_constructing_plants(df):
    not_constructing = df[~df["Capacity operating status"].isin(["announced","construction"])]
    return not_constructing

def remove_operating_plants(df):
    not_operating = df[~df["Capacity operating status"].isin(["operating", "operating pre-retirement"])]
    return not_operating

def remove_operating_and_constructing_plants(df):
    #keeps only mothballed or retired plants
    not_operating_or_constructing = df[~df["Capacity operating status"].isin(["operating", "operating pre-retirement", "announced","construction"])]
    return not_operating_or_constructing

def fuse_bf_dfs(df_bf, df_relinings):
    #first, select most recent relining (some plants have multiple relinings entered)
    #make unknowns NaN so they do not interfere with the max function
    df_relinings[df_relinings["Furnace Relining Stop Date"] == "unknown"] = np.nan

    #filter
    idx = df_relinings.groupby('GEM Unit ID')['Furnace Relining Stop Date'].transform("max") == df_relinings['Furnace Relining Stop Date']
    df_relinings_filtered = df_relinings[idx]
    
    #merge
    fused_bf = pd.merge(df_bf, df_relinings_filtered, how='left', on='GEM Unit ID')
    return fused_bf

def fuse_steel_dfs(df_plant, df_general_plant_data):
    #select the few columns needed from the general plant info sheet
    df_general_plant_data_filtered = df_general_plant_data[["Plant ID", "Announced date", "Construction date", "Pre-retirement announcement date", "Retired date"]]
    #rename also for continuity with previous data version
    df_general_plant_data_filtered_newnames = df_general_plant_data_filtered.rename(columns={"Pre-retirement announcement date":"Pre-retirement Announcement Date", "Retired date":"Retired Date"})
    #fuse
    df_plant_filtered = pd.merge(df_plant, df_general_plant_data_filtered_newnames, how='left', on='Plant ID')
    return df_plant_filtered

def remove_days_and_months(df, col):
    #first, harmonize data type
    df[col] = df[col].astype(str)

    #remove time
    df[col] = df[col].str.replace(' 00:00:00', '')

    #separate the cols that have a format "YYYY-MM" or "YYYY-MM-DD"
    df["year"] = df[col].str.split("-", expand=True)[0]
    df.loc[df[col].str.contains("-") == True, col] = df["year"]
    df.drop(["year"], axis = 1, inplace = True)

    #rename unknown to np.nan
    #df[col][df[col] == "unknown"] = np.nan
    df.loc[df[col] == "unknown", col] = np.nan

    #return as type float
    df[col] = df[col].astype(float)

    return df


def simplify_dates(df):
    #use to remove month, keep only year
    #find columns which contain "Date"
    date_columns = df.columns[df.columns.str.contains("Date")|df.columns.str.contains("date")]

    for col in date_columns:
        #clean up the columns (remove time, day and month, and convert to float)
        df = remove_days_and_months(df, col)
    
    return df

def convert_cols_to_float(df, cols):
    for col in cols:
        df[col] = df[col].astype(float)
    return df

def clean_numerical_cols(df):
    #replace all columns that have string ">0" with a numerical value
    with pd.option_context("future.no_silent_downcasting", True):
        new_df = df.replace(">0", 0).infer_objects(copy = False)
    return new_df


def separate_eaf_bof_duplicates (bof_df, eaf_df):
    duplicate_rows = pd.merge(bof_df, eaf_df, how='inner')

    #bof_df_duplicaterows = bof_df[bof_df["Plant ID"].isin(duplicate_rows["Plant ID"])]
    # bof_df_duplicaterows["Nominal crude steel capacity (ttpa)"] = bof_df_duplicaterows["Nominal crude steel capacity (ttpa)"] - bof_df_duplicaterows["Nominal EAF steel capacity (ttpa)"]
    #bof_df_duplicaterows["Nominal EAF steel capacity (ttpa)"] = 0
    duplicate_condition = bof_df["Plant ID"].isin(duplicate_rows["Plant ID"])
    bof_df.loc[duplicate_condition,"Nominal crude steel capacity (ttpa)"] = bof_df.loc[duplicate_condition,"Nominal crude steel capacity (ttpa)"] - bof_df.loc[duplicate_condition,"Nominal EAF steel capacity (ttpa)"]
    bof_df.loc[duplicate_condition,"Nominal EAF steel capacity (ttpa)"] = 0
    bof_df.loc[bof_df[duplicate_condition].index, :] = bof_df[duplicate_condition]

    #eaf_df_duplicaterows = eaf_df[eaf_df["Plant ID"].isin(duplicate_rows["Plant ID"])]
    #eaf_df_duplicaterows["Nominal crude steel capacity (ttpa)"] = eaf_df_duplicaterows["Nominal crude steel capacity (ttpa)"] - eaf_df_duplicaterows["Nominal BOF steel capacity (ttpa)"]
    #eaf_df_duplicaterows["Nominal BOF steel capacity (ttpa)"] = 0
    duplicate_condition = eaf_df["Plant ID"].isin(duplicate_rows["Plant ID"])
    eaf_df.loc[duplicate_condition, "Nominal crude steel capacity (ttpa)"] = eaf_df.loc[duplicate_condition,"Nominal crude steel capacity (ttpa)"] - eaf_df.loc[duplicate_condition,"Nominal BOF steel capacity (ttpa)"]
    eaf_df.loc[duplicate_condition,"Nominal BOF steel capacity (ttpa)"] = 0
    eaf_df.loc[eaf_df[duplicate_condition].index, :] = eaf_df[duplicate_condition]
    
    return bof_df, eaf_df

def steel_clean_data():
    #imports the two xlsx sheets as dataframes.
    #first df has steel plants capacity data, second df has general data and dates for steel plants.
    df_plant, df_general_plant_data = get_data("steel")

    #fuse the two sheets
    fused_df = fuse_steel_dfs(df_plant, df_general_plant_data)

    #clean out df plant
    df_plant_clean = clean_numerical_cols(fused_df)
    df_plant_clean = simplify_dates(df_plant_clean)

    #filter out columns that are not needed
    df_plant_filtered = steel_filter_columns(df_plant_clean)
    df_plant_filtered = indicate_steel_prod(df_plant_filtered)
    df_plant_filtered = indicate_iron_prod(df_plant_filtered)

    #rename Status column to old name used in previous data version
    df_plant_filtered.rename(columns={"Status":"Capacity operating status"}, inplace=True)

    #remove all cancelled plants before proceeding with the filtering
    df_plant_filtered = remove_cancelled_plants(df_plant_filtered)
    #filtering out: save rows that are retired or mothballed separately
    df_plant_filtered_old = remove_operating_and_constructing_plants(df_plant_filtered)
    #filtering out: remove rows that are retired, cancelled, or mothballed for the next df slices
    df_plant_filtered = remove_retired_plants(df_plant_filtered)
    df_plant_filtered_current = remove_constructing_plants(df_plant_filtered)
    df_plant_filtered_new = remove_operating_plants(df_plant_filtered)
    
    #filter further: group by plant ID, and if any row in the group has BOF = True, then the whole group is saved in the new df
    # the .any() is required here, because some rows for a given plant have no BOF capacity, but have information on the iron capacity
    # which is important for later
    df_plant_filtered_current_bof = df_plant_filtered_current.groupby("Plant ID").filter(lambda x: (x["BOF"] == True).any())
    df_plant_filtered_new_bof = df_plant_filtered_new.groupby("Plant ID").filter(lambda x: (x["BOF"] == True).any())
    df_plant_filtered_old_bof = df_plant_filtered_old[df_plant_filtered_old["BOF"] == True]

    
    return df_plant_filtered_current_bof, df_plant_filtered_new_bof, df_plant_filtered_old_bof

def bf_clean_data():
    #imports the two xlsx sheets as dataframes.
    #first df has capacity data, second df has production data.
    df_bf, df_relinings = get_data("bf")
    
    #some rows in the relining df are duplicated, remove them
    df_relinings = df_relinings.drop_duplicates()

    #fuse relining data with bf data
    fused_df = fuse_bf_dfs(df_bf, df_relinings)
    #remove columns we dont need
    filtered_bf_df = bf_filter_columns(fused_df)

    #remove date detail, convert numerical cols, deal with "unknowns", "<0" etc
    filtered_bf_df = simplify_dates(filtered_bf_df)
    filtered_bf_df = clean_numerical_cols(filtered_bf_df)
    filtered_bf_df = convert_cols_to_float(filtered_bf_df, ["Current Capacity (ttpa)"])

    return filtered_bf_df
    