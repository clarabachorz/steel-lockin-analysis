import pandas as pd
import os

# obtain directory in which to read files
grandparent_dir = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
# Path to the inputdata folder one level above
data_dir = os.path.join(grandparent_dir, "inputdata", "python_source")

DATA_CORRECTION_PATH = os.path.join(data_dir, "Missing_information.xlsx")

def load_data(filepath, sheet):
    return (pd.read_excel(filepath, sheet_name=sheet) )

def get_data_correction():
    FILE_PATH = DATA_CORRECTION_PATH
    df = load_data(FILE_PATH, "Missing_information")
    df = df.set_index("Plant ID").drop(columns = ["BOF capacity", "Notes"])
    return df

