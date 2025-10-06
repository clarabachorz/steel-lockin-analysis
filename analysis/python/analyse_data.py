from clean_data import steel_clean_data, bf_clean_data
from load_data_correction import get_data_correction
import pandas as pd
from itertools import combinations
import numpy as np

def add_data_correction(df):
    """Get data correction for the steel plants, as some construction dates are not recorded in the initial dataset

    Args:
        df (pd.DataFrame): dataframe to be updated
    """

    #get the data correction for the steel plants
    df_correction = get_data_correction()
    df_correction[df_correction == "unknown"] = np.nan

    #columns with date to float type
    date_columns = df_correction.columns[df_correction.columns.str.contains("Date")|df_correction.columns.str.contains("date")]

    for col in date_columns:
        #clean up the columns (convert to float)
        df_correction[col] = df_correction[col].astype(float)

    #update values
    df_updated = df.copy().set_index("Plant ID")
    df_updated.update(df_correction)
    df_updated = df_updated.reset_index()

    return df_updated

def clean_rows_withno_bf_capacity(df):
    #clean the rows that have no BF capacity (on the steel side), as we cannot match them to the BF data
    
    #make sure these are rows that have non-zero BOF capacity - rows with no BF or BOF capacity can be dropped
    na_rows = df[(df["Nominal BF capacity (ttpa)"].isna()) & (df["Nominal BOF steel capacity (ttpa)"].notna())]

    #filter any rows with no BF capacity out.
    df = df.dropna(subset = ["Nominal BF capacity (ttpa)"])

    #if na_rows is not empty, clean them to later return them 
    # (unless all rows are na, in which case we raise Value error)
    if not na_rows.empty:
        na_rows = clean_na_rows(na_rows)

    #if all rows are na, raise Value error
    if df.empty:
        raise ValueError("No rows with non-zero BF capacity.")
    return df, na_rows

def clean_na_rows(na_df):
    # this df contains all BF capacity = NA rows. Ie. they only contain non-zero BOF steel capacity
    # we remove duplicates in the steel row identifier column, 
    # and remove the BF information columns (since no BF can be matched to these rows)
    # set to NA the correct columns
    na_df = na_df.drop_duplicates(subset = "Steel row identifier")
    new_na_df = na_df.copy()
    new_na_df[["BF Unit Name", "Unit Status", "BF Capacity (ttpa)", "BF Start Date", "BF Retirement Date"]] = np.nan

    return new_na_df


def add_custom_name_to_unnamed_bf_units(group):
    #add a custom name to the BF unit, only when the value is "unknown":
    #this is to avoid matching issues when the name is unknown
    # the group has already been made: for all names that are "unknown" replace by "custom1", "custom2" etc
    
    mask = group == "unknown"
    group[mask] = (
        "custom"+ (mask.cumsum()[mask].astype(str))
        )
    return group

def test_bf_capacities_selfconsistent(group):
    # takes a group with steel plants entries, and checks if the cumulative BF capacity is consistent with the total expected BF capacity
    # ie. ensure there is no double-counting
    # an equivalent test here would be to ensure that each BF unit Name is found, and found once in the df.
    # we also add a rounding to 2 decimal places, to make up for floating point arithmetic errors

    if round(group["BF Capacity (ttpa)"].sum(), 2) != round(group["Total BF Capacity (ttpa)"].unique()[0],2):
        raise ValueError("BF capacity not matched correctly")

def test_if_bf_duplicates(group):
    if group.duplicated(subset = "Steel row identifier").any() or group.duplicated(subset = "BF Unit Name").any():
        modified_group = drop_bf_duplicates(group)
        return modified_group
    else:
        return group

def calculate_newcols_bfdfs(df):
    """Adds 3 new columns to the BF dataframes: # of BF Units, Total BF Capacity (ttpa) and Unit Name.

    Args:
        pandas.DataFrame: complex df containing information on BF units (start dates, retirement dates, capacity etc.)

    Returns:
        pandas.DataFrame: identical df with 3 new columns added
    """

    df["# of BF Units"] = df.groupby("GEM Plant ID").transform("size")
    df['Total BF Capacity (ttpa)'] = df.groupby("GEM Plant ID")["Current Capacity (ttpa)"].transform("sum")
    df["Unit Name"] = df.groupby("GEM Plant ID")["Unit Name"].transform(add_custom_name_to_unnamed_bf_units)

    return df

def find_valid_combinations(combinations, df):
    """Finds the first valid combination of indices from multiple sets.

    More specifically, the indices must not be repeated and each index must
    correspond to a unique value in "BF Unit Name" (avoid duplicating BFs)

    Args:
        combinations (list containing two list of tuples): 2 list of tuples containing the indices of the BF units to match
        df (pd.DataFrame): full df

    Raises:
        ValueError: If combinations contains more than 2 list (= 2 lists of combinations). 

    Returns:
        list of tuples: First combination of indices (taken from combinations) that fulfills the conditions
    """
    # converts the combination sets available from tupple to sets, and check for pairs that are compatible
    # ie. where there are no indices overlap (corresponding to the same BF accounted for twice)
    # we stop at the first pair found, since we cannot make a better guess (this function is used as last resort) 
    #for now, works with two sets of many combinations. Could improve this
    valid_combinations = []

    if len(combinations) != 2:
        print(len(combinations))
        raise ValueError("Function only works with two sets of combinations currently. The case present is not covered.")

    combinations1_sets = [(comb, set(comb)) for comb in combinations[0]]
    combinations2_sets = [(comb, set(comb)) for comb in combinations[1]]

    for comb1, set1 in combinations1_sets:
        for comb2, set2 in combinations2_sets:
            if set1.isdisjoint(set2):
                bf_unit_names = [df.loc[idx, "BF Unit Name"] for idx in comb1 + comb2]

                #check if bf names are all unique
                if len(bf_unit_names) == len(set(bf_unit_names)):
                    valid_combination = [item for sublist in ((comb1, comb2)) for item in sublist]
                    return valid_combination

def find_complex_match_operating_status(group):
    #try to match bf and steel based on operating status. Return the appropriate row indices if this is possible
    # if not possible, return Value error
    
    #first, match rows based on capacity operating status and unit status
    capacity_bool = group["Capacity operating status"] == group["Unit Status"]
    new_group = group[capacity_bool]
    
    #then, check if this matching allows the bf capacities to match up on both sides. 
    # (we are comparing capacities within a steel row, so we use the Nominal BF capacity column as a reference
    #print(new_group[['Plant ID', "Steel row identifier", 'Nominal BF capacity (ttpa)', 'BF Capacity (ttpa)', 'Total BF Capacity (ttpa)', 'BF Unit Name',]])
    if round(new_group["BF Capacity (ttpa)"].sum()) != new_group["Nominal BF capacity (ttpa)"].unique()[0]:
        bfcapsum = (round(new_group["BF Capacity (ttpa)"].sum()))
        nominal_bfcap = new_group["Nominal BF capacity (ttpa)"].unique()[0]
        raise ValueError(f"BF capacity not matched correctly: {bfcapsum} != {nominal_bfcap}")
    else:
        return new_group.index

def find_complex_match_start_date(group):
    #try to match bf and steel based on start date. Return the appropriate row indices if this is possible
    # if not possible, return Value error
    
    #first, match rows based on capacity start date and unit start date
    start_bool = group["BF Start Date"].astype(float) == group["Start date"].astype(float)
    new_group = group[start_bool]

    #then, check if this matching allows the bf capacities to match up on both sides.
    if round(new_group["BF Capacity (ttpa)"].sum()) != new_group["Nominal BF capacity (ttpa)"].unique()[0]:
        bfcapsum = (round(new_group["BF Capacity (ttpa)"].sum()))
        nominal_bfcap = new_group["Nominal BF capacity (ttpa)"].unique()[0]
        raise ValueError(f"BF capacity not matched correctly: {bfcapsum} != {nominal_bfcap}")
    else:
        return new_group.index

def check_steel_total_caps(ref_df, modified_df):
    # this function verifies that the total steel capacity is consistent between the reference and modified dataframes
    ref_bof_cap = ref_df["Nominal BOF steel capacity (ttpa)"].sum()
    modified_bof_cap = modified_df["Adjusted BOF capacity"].sum()

    #If there is any, the capacity difference could be explained by the rows with NA BF capacity, but non-zero BOF capacity
    suspicious_rows = modified_df[modified_df["Adjusted BOF capacity"].isna()].copy()
    suspicious_bof_cap = suspicious_rows["Nominal BOF steel capacity (ttpa)"].sum()
    
    if ref_bof_cap != modified_bof_cap:
        raise ValueError("Total BOF steel capacity is not consistent between reference and modified dataframes")


def calculate_bf_prod_fraction(group):
    """calculates the fraction of BF capacity attributable to each steel line.

    Main usage is to catch any divisions by zero, when there is no available information
    on BF capacity.

    Args:
        group (pd.DataFrame): Small dataframe containing the steel plant entries

    Returns:
        pd.Series: Fraction of BF capacity attributable to each steel line
    """
    fraction = group["BF Capacity (ttpa)"] / group["Total BF Capacity (ttpa)"]

    if fraction.isna().any():
        fraction = 1

    return fraction

def adjust_steel_capacity_proportionally(group):
    # assume 0.93 tons of pig iron (BF iron) is needed to produce 1 ton of steel 
    # (source: https://worldsteel.org/wp-content/uploads/Fact-sheet-raw-materials-2023.pdf - 1.4 billion tons of crude require 1.3 billion tons of "BF" and some scrap).
    # We do a simple analysis here: take the total amount of steel produced, and divide it proportionally to the BF capacity
    # a better way would be to first check if BF iron and BOF steel capacity are consistent (within the factor of 0.93), and then flag up any inconsistencies
    
    #calculate proportion of BF capacity attributable to each steel line
    # group["BF production fraction"] = group["BF Capacity (ttpa)"] / group["Total BF Capacity (ttpa)"]
    group["BF production fraction"] = calculate_bf_prod_fraction(group)

    #obtain total capacities. We will then re-distribute them proportionally, according to BF capacity
    group_unique_steel_rows = group.drop_duplicates("Steel row identifier")

    total_crude_cap = group_unique_steel_rows["Nominal crude steel capacity (ttpa)"].sum()
    total_bof_cap = group_unique_steel_rows["Nominal BOF steel capacity (ttpa)"].sum()
    total_eaf_cap = group_unique_steel_rows["Nominal EAF steel capacity (ttpa)"].sum()
    total_bf_cap = group_unique_steel_rows["Nominal BF capacity (ttpa)"].sum()
    total_ohf_cap = group_unique_steel_rows["Nominal OHF steel capacity (ttpa)"].sum()
    total_othersteel_cap = group_unique_steel_rows["Other/unspecified steel capacity (ttpa)"].sum()  

    
    if total_bof_cap + total_eaf_cap + total_ohf_cap + total_othersteel_cap != total_crude_cap:
        raise ValueError("BOF steel capacity and crude steel capacity are not consistent")
    
    #new group columns, with the adjusted capacities.
    group["Adjusted BF capacity"] = group["BF production fraction"] * total_bf_cap
    group["Adjusted EAF capacity"] = group["BF production fraction"] * total_eaf_cap
    group["Adjusted BOF capacity"] = group["BF production fraction"] * total_bof_cap
    group["Adjusted OHF capacity"] = group["BF production fraction"] * total_ohf_cap
    group["Adjusted other steel capacity"] = group["BF production fraction"] * total_othersteel_cap

    group["Adjusted crude steel capacity"] = group["Adjusted BOF capacity"] + group["Adjusted EAF capacity"] + group["Adjusted OHF capacity"] + group["Adjusted other steel capacity"]

    # catch any inconsistencies in the adjusted capacities by checking the total crude capacity obtained.
    if round(group["Adjusted crude steel capacity"].sum()) != total_crude_cap:
        raise ValueError("Adjusted crude steel capacity is not consistent with total crude steel capacity")

    return group

def adjust_steel_capacity_NArows(group):
    #this function adjusts the steel capacities for the rows that have no BF capacity, but have non-zero BOF steel capacity
    # this mean simply copying over the information from the steel df ("Nominal ... capacity") to the adjusted capacities column
    group.loc[group["Nominal BF capacity (ttpa)"].isna(), "Adjusted BOF capacity"] = group.loc[group["Nominal BF capacity (ttpa)"].isna(), "Nominal BOF steel capacity (ttpa)"]
    group.loc[group["Nominal BF capacity (ttpa)"].isna(), "Adjusted EAF capacity"] = group.loc[group["Nominal BF capacity (ttpa)"].isna(), "Nominal EAF steel capacity (ttpa)"]
    group.loc[group["Nominal BF capacity (ttpa)"].isna(), "Adjusted OHF capacity"] = group.loc[group["Nominal BF capacity (ttpa)"].isna(), "Nominal OHF steel capacity (ttpa)"]
    group.loc[group["Nominal BF capacity (ttpa)"].isna(), "Adjusted other steel capacity"] = group.loc[group["Nominal BF capacity (ttpa)"].isna(), "Other/unspecified steel capacity (ttpa)"]

    return group

def harmonize_oldplant_df(df):
    df["BF Capacity (ttpa)"] = df["Nominal BF capacity (ttpa)"]
    df['Total BF Capacity (ttpa)'] = df.groupby("Plant ID")["Nominal BF capacity (ttpa)"].transform("sum")
    df["Steel row identifier"] = df.groupby("Plant ID").cumcount()

    df["BF Retirement Date"] = np.nan
    df["BF Start Date"] = np.nan
    df["Custom calculated retirement"] = False

    # a few rows have no BF capacity, but have a non-zero BOF capacity.
    # could check why this happens. For now, set to 0
    df.loc[df["BF Capacity (ttpa)"].isna(),"BF Capacity (ttpa)"] = 0

    df = df.groupby("Plant ID")[df.columns.tolist()].apply(adjust_steel_capacity_proportionally, include_groups = False).reset_index(drop = True)

    return df

def harmonize_retirement_dates(df):
    #the point of this function is to isolate a final retirement date for each steel capacity row
    # this can either be the retirement date of the associated BF unit, or the steel unit retirement date.

    # We pick, in order of preference, the steel unit retirement date, then the BF retirement date, then the pre-retirement announcement date.
    # some rows do not have any information - about 200 rows making up 15% of the total BOF capacity.
    
    ### DEBUGGING ###
    # #rows that do not have a retirement date for the steel unit
    # non_retired_rows = df[df["Retired Date"].isna()]
    # #rows that do not have a retirement date for the steel unit or for the BF unit
    # non_retired_rows_withnobf = non_retired_rows[non_retired_rows["BF Retirement Date"].isna()]
    # no_preretirement = non_retired_rows_withnobf[non_retired_rows_withnobf["Pre-retirement Announcement Date"].isna()]
    # #used for debugging - but really looks like there is nothing to do to find a retirement date for these rows
    # pd.set_option('display.max_rows', None)
    # print(no_preretirement[['Plant ID', 'Steel row identifier', 'BF Unit Name', "Adjusted BOF capacity", "Nominal BOF steel capacity (ttpa)", "Adjusted BF capacity", "BF Capacity (ttpa)", 'BF Retirement Date']])
    # pd.reset_option('display.max_rows')
    
    #the possible cases.
    conditions = [
        df["Retired Date"].notna() & df["BF Retirement Date"].isna(),
        df["Retired Date"].notna() & df["BF Retirement Date"].notna(),
        df["Retired Date"].isna() & df["BF Retirement Date"].notna(),
        df["Retired Date"].isna() & df["BF Retirement Date"].isna() & df["Pre-retirement Announcement Date"].notna(),
        df["Retired Date"].isna() & df["BF Retirement Date"].isna() & df["Pre-retirement Announcement Date"].isna(),
    ]

    #what action to carry out for the effective retirement date selected:
    # the +4 on the pre-retirement date is chosen as a best-guess.
    actions_retirementdate = [
        df["Retired Date"],
        df["Retired Date"],
        df["BF Retirement Date"],
        df["Pre-retirement Announcement Date"] +4,
        np.nan,
    ]

    # FInalise the boolean indicating if the retirement date has been custom calculated
    # False: the retirement date is available in the data
    # True: the retirement date has been calculated based on the BF unit retirement date
    # this information is already available for the BF rows (df["Custom calculated retirement"])
    # but not for the BOF. We add this information here.
    actions_boolretirementdate = [
        False,
        False,
        df["Custom calculated retirement"],
        df["Custom calculated retirement"],
        df["Custom calculated retirement"],
    ]
    
    #apply the conditions and actions to the dataframe
    df["Effective Retirement Date"] = np.select(conditions, actions_retirementdate)
    df["Custom calculated retirement"] = np.select(conditions, actions_boolretirementdate)


    return df

def harmonize_start_dates(df):
    #possible cases - 
    # if the plant is operating, the BF start date is a good indicator. If not, we take the construction date.
    # if an operating plant has no bf start date, we can take the construction date.
    # but if an announced plant has no construction date, and it has an announcement date from 2019 or later, we take the announcement date and add 4 years (like for pre-retirement date above)
    # announced plants with announcement dates dating from before 2019 typically have substantial difficulties.
    # this is fairly optimistic: assume all recently announced plants will be built, but ignore announcements that have been significantly delayed (construction has not started in last 5 years) 
    conditions = [
        df["Start date"].notna(),
        df["Start date"].isna() & df["BF Start Date"].notna() & df["Capacity operating status"].isin(["operating", "operating pre-retirement", "retired"]),
        df["Start date"].isna() & df["BF Start Date"].isna() & df["Construction date"].notna() & df["Capacity operating status"].isin(["operating", "operating pre-retirement", "retired"]),
        df["Start date"].isna() & df["Construction date"].notna() & df["Capacity operating status"].isin(["construction", "announced"]),
        df["Start date"].isna() & df["Construction date"].isna() & (df["Announced date"] > 2018) & df["Capacity operating status"].isin(["construction", "announced"]),
        df["Start date"].isna() & df["Construction date"].isna() & (df["Announced date"] <= 2018) & df["Capacity operating status"].isin(["construction", "announced"]),
        df["Start date"].isna() & df["Construction date"].isna() & df["Announced date"].isna() & df["Capacity operating status"].isin(["construction", "announced"]),
        df["Start date"].isna() & df["Construction date"].isna() & df["BF Start Date"].isna(),
    ]

    #actions to carry out for the start date
    actions = [
        df["Start date"],
        df["BF Start Date"],
        df["Construction date"]+2,
        df["Construction date"]+2,
        df["Announced date"] + 4,
        np.nan,
        np.nan,
        np.nan,
    ]

    #apply the conditions and actions to the dataframe
    df["Effective Start Date"] = np.select(conditions, actions)

    #remove rows with no effective start date -
    # 1) for the new announcements df, these are usually announced plans with no further information due
    # to financial difficulties etc. These are ~ 8 rows.
    # 2) for the operating and retired plants, these are plants that have very limited information (with often also no
    # associated BOF capacity). These are ~12 rows.
    # these rows additionally account for very little BOF capacity, as they are often NA rows.
    df = df.dropna(subset = ["Effective Start Date"])

    return df


def drop_bf_duplicates(group):
    new_group = group.groupby("Steel row identifier")
    list_of_bf_names = []
    list_of_new_rows = []
    for name, sub_group in new_group:
        #remove any BF names that have already been used up (held in list_of_bf_names)
        sub_group = sub_group[~sub_group["BF Unit Name"].isin(list_of_bf_names)]
        
        #then, drop duplicates to select only one row
        new_sub_group = sub_group.drop_duplicates("BF Capacity (ttpa)")

        #save the bf unit name that has been selected to not pick another duplicate again
        list_of_bf_names.append(new_sub_group["BF Unit Name"].values[0])

        #save the row
        list_of_new_rows.append(new_sub_group)
    
    #concatenate new group without any of the duplicates
    new_group = pd.concat(list_of_new_rows)
    
    return new_group
    
def find_complex_bf_capacity_matches(df):
    results = []
    plant_id = df["Plant ID"].unique()[0]

    #list in case of multiple matches more complex solve required
    multiple_matches = []

    # Group by Nominal BF capacity, as we want to try every possible combination of BFs
    for row_id, group in df.groupby(["Steel row identifier"]): #"Nominal BF capacity (ttpa)" 
        # Convert BF capacities to list for combinations
        nominal_capacity = group["Nominal BF capacity (ttpa)"].unique()[0]
        bf_capacities = group["BF Capacity (ttpa)"].values
        bf_indices = group.index  # Store the indices to retrieve matching rows
        matches = []
        
        
        # Find any combinations that sum up to the nominal capacity
        for r in range(1, len(bf_capacities) + 1):
            for combo_indices in combinations(bf_indices, r):
                #rounding is necessary here because of floating point arithmetic
                combo_sum = round(sum(df.loc[idx, "BF Capacity (ttpa)"] for idx in combo_indices))
                if combo_sum == nominal_capacity:
                    matches.append(combo_indices)

        # Classify the result based on the number of matches found
        if len(matches) == 1:
            # Unique match found, keep rows in the unique match
            results.extend(matches[0])
        elif len(matches) > 1:
            # Multiple fitting combinations of BFs found.
            # Attempt to match the BFs using operating status matching instead
            # If this fails, try to match based on starting date.
            # if this fails, as last resort, use a fitting match at random:
            try:
                results.extend(find_complex_match_operating_status(group))
            except ValueError:
                try:
                    #Attempt to match rows based on starting date of the unit and of the BFs.
                    results.extend(find_complex_match_start_date(group))
                except ValueError:
                    multiple_matches.append(matches)
        else:
            # No match found, raise error
            raise ValueError(f"No matches found for the row of nominal capacity {nominal_capacity} ttpa for plant number {plant_id}. Check the BF assignment.")
    #check if multiple matches have been found (list not empty)
    if multiple_matches:
        #check which matches are valid together, such that no BF is used twice. Pick the first valid combination found.
        # this raises an error for now, as we ideally want to match the rows in a better manner.
        valid_combination = find_valid_combinations(multiple_matches, df)
        results.extend(valid_combination)
        print("Multiple matches found. The first one has been chosen at random for plant {p} - check that this makes sense.".format(p=plant_id))

    # Return the filtered DataFrame with the correct rows
    filtered_df = df.loc[results].reset_index(drop=True)
    return filtered_df


def match_bf_capacity_to_steel_capacity(group):
    try:
        group, na_rows = clean_rows_withno_bf_capacity(group)
    except ValueError as e:
        #this is the case where the group has no rows with any BF capacity. 
        # We can't match these to BFs, so we return the group as is : otherwise the function will crash
        return group

    bool_indicator = group["Nominal BF capacity (ttpa)"] == group["BF Capacity (ttpa)"]
    partial_group = group[bool_indicator]

    #check that partial group has no duplicates (using BF names column and the steel row identifier columns)
    # as for this exact matching, there should be a one-on-one correspondance between steel rows and bfs
    partial_group = test_if_bf_duplicates(partial_group)
    
    #filter out the BFs that have been matched in "partial_group" by applying 2 conditions (to ensure the matched BFs are filtered out)
    #as well as the steel line that has already been matched (identified using the steel row identifier)
    bf_cond1 = ~group["BF Unit Name"].isin(partial_group["BF Unit Name"])
    bof_cond1 = ~group["Steel row identifier"].isin(partial_group["Steel row identifier"])
    
    group = group[bf_cond1 & bof_cond1]

    #Logic checks: is all the BF capacity accounted for ? (No double counting on the BF side ?)
    try:
        test_bf_capacities_selfconsistent(pd.concat([group,partial_group], axis=0))
    except ValueError as e:
        try:
            group = find_complex_bf_capacity_matches(group)
            try:
                test_bf_capacities_selfconsistent(pd.concat([group,partial_group], axis=0))
            except ValueError as f:
                print(f)
                raise
        except ValueError as f:
            print(e)
            print(f)
            raise
    
    group = pd.concat([group,partial_group], axis=0)

    #final step: adjust steel capacities accordingly, to avoid double counting. Eg. if two BFs are used for a single line, 
    # divide the steel capacity and bof steel capacity of each line by 2
    group = adjust_steel_capacity_proportionally(group)
    group = pd.concat([group,na_rows], axis = 0)

    return group


def match_bf_and_steel_dfs(df):
    #the purpose of this function is to avoid double counting of capacity, by matching the capacity of individual blast furnaces 
    # to the correct steel plant entry.

    #use two identifier columns for this: steel row identifier and bf unit name, to ensure no double counting on either side.
    # (if there are 6 rows, and 3 bf units, then we should be left with 2 rows after analysis)

    new_df = df.groupby(by = "Plant ID")[df.columns.tolist()].apply(match_bf_capacity_to_steel_capacity, include_groups = False).reset_index(drop = True)
    
    return new_df

def complex_merge_bf_and_steel_dfs(df_bof, df_bf):
    #matching bf and bof rows is done using a simple merge
    df_bof["Steel row identifier"] = df_bof.groupby("Plant ID").cumcount()
    new_df = df_bof.merge(df_bf, how = "left", left_on = "Plant ID", right_on = "GEM Plant ID")
    
    #some df cleaning up after the merge
    new_df = new_df.drop(columns = ["GEM Plant ID"])
    new_df = new_df.rename(columns = {"Unit Name":"BF Unit Name", "Current Capacity (ttpa)":"BF Capacity (ttpa)", "Retired Date_y":"BF Retirement Date", "Retired Date_x":"Retired Date", "Start Date":"BF Start Date"})

    #carry out the matching between bf and steel rows, to check that the right BF(s) is/are associated to the correct steel entry
    new_df = match_bf_and_steel_dfs(new_df)

    #final step: harmonize rows that have N/A BF capacity => that have no adjusted steel capacities so far
    new_df = adjust_steel_capacity_NArows(new_df)

    return new_df

def analyse_steel_dfs(relining_cycle_0=20, relining_cycle_1=15, relining_cycle_2=10):
    #get the bof plants data
    df_plant_bof, df_newplant_bof, df_oldplant_bof = steel_clean_data()

    #get the bf specific data, which has been analyzed to include the expected retirement / capital reinvestment date
    active_bfs, new_bfs = analyse_bf_df(relining_cycle_0 = relining_cycle_0, relining_cycle_1 = relining_cycle_1, relining_cycle_2 = relining_cycle_2)

    # add a few column for both bf dataframes : total # of BF units, total BF capacity, and a custom name for each BF unit within a plant
    active_bfs = calculate_newcols_bfdfs(active_bfs)
    new_bfs = calculate_newcols_bfdfs(new_bfs)

    #keep only few important column: plant id (for matching), unit name, unit iron-making capacity and expected retirement date columns
    active_bfs = active_bfs[["GEM Plant ID","Unit Name", "Unit Status", "# of BF Units","Total BF Capacity (ttpa)","Current Capacity (ttpa)","Start Date","Retired Date", "BF Retirement Date (1 relining)","BF Retirement Date (2 relinings)", "Custom calculated retirement"]]
    new_bfs = new_bfs[["GEM Plant ID","Unit Name", "Unit Status", "# of BF Units","Total BF Capacity (ttpa)", "Current Capacity (ttpa)","Simplified Start Date","Retired Date","BF Retirement Date (1 relining)","BF Retirement Date (2 relinings)", "Custom calculated retirement"]]
    #For new bfs only: we use the column Simplified start date. Rename this to Start Date for consistency
    new_bfs = new_bfs.rename(columns = {"Simplified Start Date":"Start Date"})

    #merge the two dataframes in the function below.
    joined_bof_df = complex_merge_bf_and_steel_dfs(df_plant_bof, active_bfs)
    joined_newplant_bof = complex_merge_bf_and_steel_dfs(df_newplant_bof, new_bfs)
    
    # for the old plants df, we do not match BFs (as relinings don't matter). We still need to ensure compatibility with the dfs above, adding the relevant columns
    joined_oldplant_bof = harmonize_oldplant_df(df_oldplant_bof)

    #check that the total steel capacity is consistent between the reference and modified dataframes
    check_steel_total_caps(df_plant_bof, joined_bof_df)
    check_steel_total_caps(df_newplant_bof, joined_newplant_bof)
    check_steel_total_caps(df_oldplant_bof, joined_oldplant_bof)

    #calculate new column, "Effective retirement date", to identify when certain BOF steel capacities are likely to go offline
    # for each of the dfs (old, current, future)
    joined_bof_df = harmonize_retirement_dates(joined_bof_df)
    joined_newplant_bof = harmonize_retirement_dates(joined_newplant_bof)
    joined_oldplant_bof = harmonize_retirement_dates(joined_oldplant_bof)

    # manually update rows that have no start dates in original data, but have information on GSPT website
    joined_newplant_bof = add_data_correction(joined_newplant_bof)

    #for these dfs, we also need to harmonize the start dates and create an "Effective Start Date" column
    joined_bof_df = harmonize_start_dates(joined_bof_df)
    joined_newplant_bof = harmonize_start_dates(joined_newplant_bof)
    joined_oldplant_bof = harmonize_start_dates(joined_oldplant_bof)
    

    # We additionally add a projected retirement date for the plants that have an effective start date, but no retirement date
    # using the retrofit cycle parameter
    joined_newplant_bof.loc[joined_newplant_bof["Effective Retirement Date"].isna(), "Effective Retirement Date"] = joined_newplant_bof["Effective Start Date"] + relining_cycle_0
    
    # add the retirement date if 1 relining is carried out. For all new plants, we assume a lifetime of 2 relinings below. For old plants, we only have
    # a non-null date if there was a BF associated with the BOF, for which we could find the "BF Retirement Date (1 relining)"
    joined_newplant_bof.loc[joined_newplant_bof["BF Retirement Date (1 relining)"].isna(), "Effective Retirement Date (1 relining)"] = joined_newplant_bof["Effective Start Date"] + relining_cycle_0 + relining_cycle_1
    joined_newplant_bof.loc[~joined_newplant_bof["BF Retirement Date (1 relining)"].isna(), "Effective Retirement Date (1 relining)"] = joined_newplant_bof["BF Retirement Date (1 relining)"]
    #same for part of the operating BOF df where we calculate this extended retirement date (plants that have not yet undergone a BF relining)
    joined_bof_df.loc[~joined_bof_df["BF Retirement Date (1 relining)"].isna(), "Effective Retirement Date (1 relining)"] = joined_bof_df["BF Retirement Date (1 relining)"]

    #same logic with 2 relinings
    joined_newplant_bof.loc[joined_newplant_bof["BF Retirement Date (2 relinings)"].isna(), "Effective Retirement Date (2 relinings)"] = joined_newplant_bof["Effective Start Date"] + relining_cycle_0 + relining_cycle_1 + relining_cycle_2
    joined_newplant_bof.loc[~joined_newplant_bof["BF Retirement Date (2 relinings)"].isna(), "Effective Retirement Date (2 relinings)"] = joined_newplant_bof["BF Retirement Date (2 relinings)"]
    joined_bof_df.loc[~joined_bof_df["BF Retirement Date (2 relinings)"].isna(), "Effective Retirement Date (2 relinings)"] = joined_bof_df["BF Retirement Date (2 relinings)"]

    
   
    return joined_bof_df, joined_newplant_bof, joined_oldplant_bof

def analyse_bf_df(relining_cycle_0=20, relining_cycle_1=15, relining_cycle_2=10):
    """
    This function analyses the blast furnace data, and calculates the expected retirement date for active plants.
    Args:
    relining_cycle_x: the number of years between relinings x and x+1.

    It returns:
    active_bfs: a dataframe of active blast furnaces, with the last CAPEX cycle and retirement date calculated
    new_bfs: a dataframe of blast furnaces that are under construction or announced, with a simplified start date calculated
    """
    bf_df = bf_clean_data()

    #filter out old bfs, that are operating or operating pre-retirement
    # print(bf_df[(bf_df["Start Date"] < 2000) & (bf_df["Unit Status"].str.contains("operating"))])

    #bfs that are operating or operating pre-retirement
    active_bfs = bf_df.loc[bf_df["Unit Status"].str.contains("operating"),:].copy()

    # introduce last CAPEX cycle: this is either the end of the last relining, 
    # or the start date of the plant if no relinings have taken place 
    active_bfs["Last CAPEX cycle"] = active_bfs["Furnace Relining Stop Date"]
    active_bfs.loc[active_bfs["Last CAPEX cycle"].isna(),"Last CAPEX cycle"] = active_bfs["Start Date"]

    #260 /1000 active bfs have no start date or last furnace relining date. 
    # This corresponds to ~230 000 ttpa of capacity (out of 1 460 454 ttpa total, so roughly 15%)
    # we keep these as NAs for now
    # print(active_bfs.loc[active_bfs["Last CAPEX cycle"].isna(),"Current Capacity (ttpa)" ].sum())#

    # add a flag highlighting for which BFs a custom retirement date will be calculated (rows with no retired date at this point)
    active_bfs.loc[active_bfs["Retired Date"].isna(), "Custom calculated retirement"] = True
    active_bfs.loc[~active_bfs["Retired Date"].isna(), "Custom calculated retirement"] = False

    # Now, need to calculate when retirement would happen, for plants that are still operating (not operating pre-retirement)
    ### ASSUME RELINING CYCLE IS x YEARS ###
    active_bfs.loc[active_bfs["Unit Status"] != "operating pre-retirement", "Retired Date"] = active_bfs["Last CAPEX cycle"] + relining_cycle_0
    # if the plants are not just starting up, the retirement could happen earlier (2nd relining campaign typically shorter than the first)
    active_bfs.loc[(active_bfs["Unit Status"] != "operating pre-retirement")&(active_bfs["Start Date"] != active_bfs["Last CAPEX cycle"]), "Retired Date"] = active_bfs["Last CAPEX cycle"] + relining_cycle_1
    
    #add a column for the retirement date if 1 relining is carried out. For existing plants, this is only added if no relining have been carried out
    active_bfs.loc[(active_bfs["Unit Status"] != "operating pre-retirement")&(active_bfs["Start Date"]== active_bfs["Last CAPEX cycle"]), "BF Retirement Date (1 relining)"] = active_bfs["Last CAPEX cycle"] + relining_cycle_0 + relining_cycle_1
    
    # add another column if 2 relinings are carried out, This assumption includes more plants, notably those that have only undergone one relining scenario.
    active_bfs.loc[(active_bfs["Unit Status"] != "operating pre-retirement")&(active_bfs["Start Date"]== active_bfs["Last CAPEX cycle"]), "BF Retirement Date (2 relinings)"] = active_bfs["Last CAPEX cycle"] + relining_cycle_0 + relining_cycle_1 + relining_cycle_2

    #BFs that have had only 1 relining (found using start date >= 2025 - relining_cycle_0 - relining_cycle_1) 
    active_bfs.loc[(active_bfs["Unit Status"] != "operating pre-retirement")&(active_bfs["Start Date"]!= active_bfs["Last CAPEX cycle"])&(active_bfs["Start Date"]>= 2025 - relining_cycle_0 - relining_cycle_1), "BF Retirement Date (2 relinings)"] = active_bfs["Last CAPEX cycle"] + relining_cycle_1 + relining_cycle_2
    
    #lots of BFs have no start date. We can't calculate a 2 relining retirement date for these.
    #print(active_bfs.loc[(active_bfs["Unit Status"] != "operating pre-retirement")&(active_bfs["Start Date"]!= active_bfs["Last CAPEX cycle"])&(active_bfs["Start Date"].isna()), ["GEM Plant ID", "Unit Name", "Start Date","Last CAPEX cycle", "BF Retirement Date (2 relinings)"]])
    
    
    # 20 877.0 ttpa of capacity is urgently due for reinvestment (Retired Date < 2022)
    # following up on before, 232 054 ttpa has no start date / furnace relining date, so we can't calculate retirement date

    # print(active_bfs.loc[active_bfs["Retired Date"] < 2022,"Current Capacity (ttpa)"].sum())
    # print(active_bfs.loc[active_bfs["Retired Date"] < 2022, ["GEM Plant ID", "Unit Name", "Furnace Relining Stop Date","Current Capacity (ttpa)"]].sort_values(by = "Current Capacity (ttpa)", ascending = False))
    
    #filter out bfs that are being constructed, and pick a single date for the start of the plant (the biggest of announced, contruction and start date)
    #there are 9 plants that have no start date, announced date or construction date. We can ignore them
    new_bfs = bf_df.loc[bf_df["Unit Status"].str.contains("construction|announced"),:].copy()
    
    #obtain a simplified start date for the new projects
    new_bfs["Simplified Start Date"] = new_bfs[["Announced Date", "Construction Date", "Start Date"]].max(axis = 1)
    #but excludes projects that have no start date if: they have an announcement date before 2015, or a construction date before 2015: in the database, these are only a few projects,
    # which are encountering big financing issues.
    new_bfs.loc[(new_bfs["Start Date"].isna()) & ((new_bfs["Announced Date"] < 2015) | (new_bfs["Construction Date"] < 2015)), "Simplified Start Date"] = None

    new_bfs["Retired Date"] = new_bfs["Simplified Start Date"] + relining_cycle_0
    #add another date: retirement date after 1 relining
    new_bfs["BF Retirement Date (1 relining)"] = new_bfs["Simplified Start Date"] + relining_cycle_0 + relining_cycle_1

    #add another date: retirement date after 2 relinings
    new_bfs["BF Retirement Date (2 relinings)"] = new_bfs["Simplified Start Date"] + relining_cycle_0 + relining_cycle_1 + relining_cycle_2

    # As for operating BFs, add a flag for new BFs indicating that a custom retirement date has been calculated.
    new_bfs["Custom calculated retirement"] = True
    
    return active_bfs, new_bfs

