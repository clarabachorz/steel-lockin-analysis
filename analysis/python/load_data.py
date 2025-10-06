import pandas as pd
import os

# obtain directory in which to read files
grandparent_dir = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
# Path to the inputdata folder one level above
data_dir = os.path.join(grandparent_dir, "inputdata", "python_source")


STEEL_FILE_PATH = os.path.join(data_dir, "Plant-level-data-Global-Iron-and-Steel-Tracker-March-2025-V1.xlsx")
BF_FILE_PATH = os.path.join(data_dir, "Iron-unit-data-Global-Iron-and-Steel-Tracker-March-2025-V1.xlsx")

def load_data(filepath, sheet):
    return (pd.read_excel(filepath, sheet_name=sheet) )

#pd.set_option('display.max_columns', None)
def get_data(str):
    if str == "steel":
        FILE_PATH = STEEL_FILE_PATH
        df1 = load_data(FILE_PATH, "Plant capacities and status")
        df2 = load_data(FILE_PATH, "Plant data")
    elif str == "bf":
        FILE_PATH = BF_FILE_PATH
        df1 = load_data(FILE_PATH, "Blast furnaces")
        df2 = load_data(FILE_PATH, "Blast furnace relinings")
    else:
        raise Exception(f"Invalid input: str was {str}, must be 'steel' or 'bf'")
    return df1, df2