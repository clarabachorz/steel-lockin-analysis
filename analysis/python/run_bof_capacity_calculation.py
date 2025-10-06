from analyse_data import analyse_steel_dfs
import pandas as pd
import numpy as np
import os

# obtain directory in which to save files
grandparent_dir = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
# Path to the input_data folder one level above
output_dir = os.path.join(grandparent_dir, "inputdata", "gem")


## REMIND regions
# CAZ: Canada, Australia, New Zealand
# CHA: China and Taiwan
# IND: India
# JPN: Japan
# OAS: Other Asia
# LAM: Latin America and the Caribbean
# MEA: Middle East and North Africa
# REF: Russia and Reforming Economies
# SSA: Sub-Saharan Africa
# USA: United States
# EUR: EU 28
# NEU: Non-EU28 Europe

REMIND_REGIONS = {
    "CAZ" : ["Canada", "Australia", "New Zealand"],
    "CHA" : ["China", "Hong Kong" ,"Taiwan"],
    "IND" : ["India"],
    "JPN" : ["Japan"],
    "OAS" : ["Bangladesh","Indonesia","Cambodia", "South Korea","Sri Lanka", "Myanmar", "Malaysia","Pakistan","Philippines", "North Korea", "Singapore", "Thailand", "Vietnam"],
    "LAM" : ["Argentina", "Bolivia","Brazil", "Chile","Guatemala", "Mexico", "Peru", "Trinidad and Tobago", "Venezuela"],
    "MEA" : ["United Arab Emirates","Bahrain","Algeria","Egypt", "Iran", "Iraq","Kuwait","Libya","Morocco","Oman","Qatar","Saudi Arabia","Syria"],
    "REF" : ["Azerbaijan","Belarus","Moldova","Russia", "Kazakhstan", "Ukraine","Uzbekistan"],
    "SSA" : ["Angola","Ethiopia","Ghana","Kenya","Mozambique","Namibia","Nigeria", "Uganda","South Africa","Zimbabwe"],
    "USA" : ["United States"],
    "EUR" : ["Austria", "Belgium", "Bulgaria","Czech Republic", "Spain", "Finland", "Greece","Hungary", "Luxembourg","Latvia","Netherlands", "Poland","Portugal", "Romania", "Slovakia","Slovenia","Sweden", "France", "Germany", "Italy", "United Kingdom"],
    "NEU" : ["Albania","Bosnia and Herzegovina","Switzerland","North Macedonia","Norway", "Serbia", "Türkiye"]
}

def filter_for_region(df, region):
    """Filters the DataFrame for a specific region or returns the entire DataFrame for 'global'.

    Parameters:
    df (pd.DataFrame): The input DataFrame.
    region (str): The region to filter for. If 'global', the entire DataFrame is returned.

    Returns:
    pd.DataFrame: Filtered DataFrame or the original DataFrame.
    """
    try:
        if region == "global":
            return df
        if region in df["Country/Area"].unique():
            return df.loc[df["Country/Area"] == region, :]
        else:
            # print(
            #     f"Region '{region}' not found in data. Empty df was returned. "
            #     f"Available regions: {list(df['Country/Area'].unique())}"
            # )
            return df.loc[df["Country/Area"] == region, :]
    except KeyError as e:
        print(f"Error: Missing expected column 'Country/Area'. Returning the original DataFrame. ({e})")
        return df
    except Exception as e:
        print(f"Unexpected error: {e}. Returning the original DataFrame.")
        return df

def calculate_age_distribution(steel_dfs, region = ["global"], region_name = None):
    """Calculates the age distribution of steel plants in a given region,
    relative to 2025.

    Args:
        steel_dfs (DataFrame): operating steel plants from GEM
        region (list, optional): Filter BOFS in certain region(s). Defaults to ["global"].

    Returns:
       distribution_df: dataframe containing age and capacity distribution of steel plants in the region(s)
    """
    list_of_dfs = []
    for reg in region:
        #filter the df
        filtered_steel_df = filter_for_region(steel_dfs, reg)

        age_distribution = filtered_steel_df.groupby("Effective Start Date")["Effective Start Date"].value_counts().sort_index()
        capacity_distribution = filtered_steel_df.groupby("Effective Start Date")["Adjusted BOF capacity"].sum().sort_index()

        age_distribution.name = "Number of plants"
        capacity_distribution.name = "Total BOF capacity"

        age_distribution = age_distribution.to_frame()
        capacity_distribution = capacity_distribution.to_frame()

        distribution_df = pd.concat([age_distribution, capacity_distribution], axis = 1)

        #youngest plants are from 2023. We calculate in terms of age.
        distribution_df["Age"] = 2025 - distribution_df.index.get_level_values(0)
        distribution_df["Age"] = distribution_df["Age"].astype(int)
        distribution_df = distribution_df.set_index("Age").sort_index()

        distribution_df["Region"] = reg
        list_of_dfs.append(distribution_df)
    
    full_distribution_df = pd.concat(list_of_dfs)
    full_distribution_df = full_distribution_df.groupby("Age").sum()
    if region != ["global"]:
        full_distribution_df["Region"] = region_name

    return full_distribution_df

def summarise_BOF_capacity_single_region(steel_dfs, oldsteel_dfs, new_steel_dfs, region = "global", relining_cycle_0=20, relining_cycle_1=15, relining_cycle_2=10):
    """Finalises the steel data by grouping capacity additions / retirements into year intervals.
    If required, filters the data to select a given region.

    Args:
        region (str, optional): The region for which the steel data should be plotted. Defaults to "global".

    Returns:
        df: Lumped steel data df with BOF emissions grouped into year intervals.
    """

    #filter for region
    filtered_steel_df = filter_for_region(steel_dfs, region)
    filtered_oldsteel_dfs = filter_for_region(oldsteel_dfs, region)
    filtered_new_steel_df = filter_for_region(new_steel_dfs, region)

    #just for newsteeldfs: remove announced/under contruction rows that have a start date earlier than 2020
    filtered_new_steel_df = filtered_new_steel_df[filtered_new_steel_df["Effective Start Date"] >= 2020]

    #join the two dfs to simplify the analysis
    filtered_steel_df = pd.concat([filtered_steel_df, filtered_oldsteel_dfs, filtered_new_steel_df], axis = 0, ignore_index=True)

    #separately keep rows with no retirement date, which have an important impact on overall capacity uncertainty
    filtered_steel_df_noretirementdate = filtered_steel_df[filtered_steel_df["Effective Retirement Date"].isna()]
    #filter away any rows that have no retirement date
    filtered_steel_df = filtered_steel_df[~filtered_steel_df["Effective Retirement Date"].isna()]

    #filter and keep a copy of rows that have a custom calculated retirement date
    # operating and operating pre-retirement only, as announced and under construction plants are already saved and looked at in another df
    filtered_steel_df_customretirementdate = filtered_steel_df[(filtered_steel_df["Custom calculated retirement"] == True)&(filtered_steel_df["Capacity operating status"].isin(["operating", "operating pre-retirement"]))]
    
    #HERE: CALCULATE THE DIFFERENT COMPONENTS OF TOTAL BOF CAPACITY
    #lump the data by year, to calculate each component of the total BOF capacity
    lumped_steel_dfs = filtered_steel_df.groupby('Effective Start Date')["Adjusted BOF capacity"].sum()
    lumped_steel_retire_dfs = filtered_steel_df.groupby('Effective Retirement Date')["Adjusted BOF capacity"].sum()
    lumped_steel_noretirementdate = filtered_steel_df_noretirementdate.groupby('Effective Start Date')["Adjusted BOF capacity"].sum()
    lumped_steel_custom_retirement_date = filtered_steel_df_customretirementdate.groupby('Effective Retirement Date')["Adjusted BOF capacity"].sum()
    #add possible additional BOF capacity from young BOFs (but already operating) that may undergo a relining cycle
    lumped_steel_retire_dfs_before1relining = filtered_steel_df_customretirementdate[~filtered_steel_df_customretirementdate["Effective Retirement Date (1 relining)"].isna()].groupby('Effective Retirement Date')["Adjusted BOF capacity"].sum()
    lumped_steel_retire_dfs_1relining = filtered_steel_df_customretirementdate[~filtered_steel_df_customretirementdate["Effective Retirement Date (1 relining)"].isna()].groupby('Effective Retirement Date (1 relining)')["Adjusted BOF capacity"].sum()
    #same but for 2 relinings
    lumped_steel_retire_dfs_before2relining = filtered_steel_df_customretirementdate[~filtered_steel_df_customretirementdate["Effective Retirement Date (2 relinings)"].isna()].groupby('Effective Retirement Date')["Adjusted BOF capacity"].sum()
    lumped_steel_retire_dfs_2relining = filtered_steel_df_customretirementdate[~filtered_steel_df_customretirementdate["Effective Retirement Date (2 relinings)"].isna()].groupby('Effective Retirement Date (2 relinings)')["Adjusted BOF capacity"].sum()

    #using only new announcements df (announced, planned and under-construction projects)
    new_lumped_steel_dfs = filtered_new_steel_df.groupby('Effective Start Date')["Adjusted BOF capacity"].sum()
    new_lumped_steel_dfs_announcements = filtered_new_steel_df[filtered_new_steel_df["Capacity operating status"] == "announced"].groupby('Effective Start Date')["Adjusted BOF capacity"].sum()
    new_retired_lumped_steel_dfs = filtered_new_steel_df.groupby('Effective Retirement Date')["Adjusted BOF capacity"].sum()
    new_retired_lumped_steel_dfs_announcements = filtered_new_steel_df[filtered_new_steel_df["Capacity operating status"] == "announced"].groupby('Effective Retirement Date')["Adjusted BOF capacity"].sum()
    new_retired_lumped_steel_dfs_1relining = filtered_new_steel_df.groupby('Effective Retirement Date (1 relining)')["Adjusted BOF capacity"].sum()
    new_retired_lumped_steel_dfs_2relining = filtered_new_steel_df.groupby('Effective Retirement Date (2 relinings)')["Adjusted BOF capacity"].sum()
    new_retired_lumped_steel_dfs_2relining_announcements = filtered_new_steel_df[filtered_new_steel_df["Capacity operating status"] == "announced"].groupby('Effective Retirement Date (2 relinings)')["Adjusted BOF capacity"].sum()
    new_retired_lumped_steel_dfs_1relining_announcements = filtered_new_steel_df[filtered_new_steel_df["Capacity operating status"] == "announced"].groupby('Effective Retirement Date (1 relining)')["Adjusted BOF capacity"].sum()
    # we assume a linear decrease (retirement) for the capacity for which we do not have any data
    #this means the same (proportional) amount of capacity is retired each year, between 2023 and 2023 + relining_cycle_0
    # assume that the plants are still operating in 2023 (latest update of the dataset), which means they would 
    # require re-investment at earliest at 2023 + relining_cycle_0
    lumped_steel_custom_phaseout = pd.Series([lumped_steel_noretirementdate.sum()/ relining_cycle_0] * relining_cycle_0 ,index = np.arange(2023, 2023 + relining_cycle_0, 1))
    
    # CHANGE FORMAT AND COMBINE THE SERIES
    lumped_steel_dfs = lumped_steel_dfs.sort_index()
    lumped_steel_retire_dfs = lumped_steel_retire_dfs.sort_index()
    lumped_steel_noretirementdate = lumped_steel_noretirementdate.sort_index()
    lumped_steel_custom_retirement_date = lumped_steel_custom_retirement_date.sort_index()
    lumped_steel_retire_dfs_before1relining = lumped_steel_retire_dfs_before1relining.sort_index()
    lumped_steel_retire_dfs_1relining = lumped_steel_retire_dfs_1relining.sort_index()
    lumped_steel_retire_dfs_before2relining = lumped_steel_retire_dfs_before2relining.sort_index()
    lumped_steel_retire_dfs_2relining = lumped_steel_retire_dfs_2relining.sort_index()
    lumped_steel_custom_phaseout = lumped_steel_custom_phaseout.sort_index()
    new_lumped_steel_dfs = new_lumped_steel_dfs.sort_index()
    new_lumped_steel_dfs_announcements = new_lumped_steel_dfs_announcements.sort_index()
    new_retired_lumped_steel_dfs = new_retired_lumped_steel_dfs.sort_index()
    new_retired_lumped_steel_dfs_announcements = new_retired_lumped_steel_dfs_announcements.sort_index()
    new_retired_lumped_steel_dfs_1relining = new_retired_lumped_steel_dfs_1relining.sort_index()
    new_retired_lumped_steel_dfs_2relining = new_retired_lumped_steel_dfs_2relining.sort_index()
    new_retired_lumped_steel_dfs_2relining_announcements = new_retired_lumped_steel_dfs_2relining_announcements.sort_index()
    new_retired_lumped_steel_dfs_1relining_announcements = new_retired_lumped_steel_dfs_1relining_announcements.sort_index()

    #rename series
    lumped_steel_dfs.name = "BOF Capacity additions"
    lumped_steel_retire_dfs.name = "Retired BOF capacity"
    lumped_steel_noretirementdate.name = "BOF Capacity additions (no retirement date)"
    lumped_steel_custom_retirement_date.name = "Retired BOF Capacity (custom calculated retirement date)"
    lumped_steel_retire_dfs_before1relining.name = "Retired BOF capacity (operating, 1 relining) before 1 relining"
    lumped_steel_retire_dfs_1relining.name = "Retired BOF capacity (operating, 1 relining)"
    lumped_steel_retire_dfs_before2relining.name = "Retired BOF capacity (operating, 2 relining) before 2 relining"
    lumped_steel_retire_dfs_2relining.name = "Retired BOF capacity (operating, 2 relining)"
    lumped_steel_custom_phaseout.name = "Retired BOF Capacity (no retirement date)"
    new_lumped_steel_dfs.name = "BOF Capacity additions (new projects)"
    new_lumped_steel_dfs_announcements.name = "BOF Capacity additions (new projects, announced only)"
    new_retired_lumped_steel_dfs.name = "Retired BOF capacity (new projects)"
    new_retired_lumped_steel_dfs_announcements.name = "Retired BOF capacity (new projects, announced only)"
    new_retired_lumped_steel_dfs_1relining.name = "Retired BOF capacity (new projects, 1 relining)"
    new_retired_lumped_steel_dfs_2relining.name = "Retired BOF capacity (new projects, 2 relining)"
    new_retired_lumped_steel_dfs_2relining_announcements.name = "Retired BOF capacity (new projects, 2 relining, announced only)"
    new_retired_lumped_steel_dfs_1relining_announcements.name = "Retired BOF capacity (new projects, 1 relining, announced only)"

    lumped_steel_dfs = lumped_steel_dfs.to_frame()
    lumped_steel_retire_dfs = lumped_steel_retire_dfs.to_frame()
    lumped_steel_noretirementdate = lumped_steel_noretirementdate.to_frame()
    lumped_steel_custom_retirement_date = lumped_steel_custom_retirement_date.to_frame()
    lumped_steel_retire_dfs_before1relining = lumped_steel_retire_dfs_before1relining.to_frame()
    lumped_steel_retire_dfs_1relining = lumped_steel_retire_dfs_1relining.to_frame()
    lumped_steel_retire_dfs_before2relining = lumped_steel_retire_dfs_before2relining.to_frame()
    lumped_steel_retire_dfs_2relining = lumped_steel_retire_dfs_2relining.to_frame()
    lumped_steel_custom_phaseout = lumped_steel_custom_phaseout.to_frame()
    new_lumped_steel_dfs = new_lumped_steel_dfs.to_frame()
    new_lumped_steel_dfs_announcements = new_lumped_steel_dfs_announcements.to_frame()
    new_retired_lumped_steel_dfs = new_retired_lumped_steel_dfs.to_frame()
    new_retired_lumped_steel_dfs_announcements = new_retired_lumped_steel_dfs_announcements.to_frame()
    new_retired_lumped_steel_dfs_1relining = new_retired_lumped_steel_dfs_1relining.to_frame()
    new_retired_lumped_steel_dfs_2relining = new_retired_lumped_steel_dfs_2relining.to_frame()
    new_retired_lumped_steel_dfs_2relining_announcements = new_retired_lumped_steel_dfs_2relining_announcements.to_frame()
    new_retired_lumped_steel_dfs_1relining_announcements = new_retired_lumped_steel_dfs_1relining_announcements.to_frame()

    lumped_steel_combine = pd.concat([lumped_steel_dfs, lumped_steel_retire_dfs, lumped_steel_noretirementdate, lumped_steel_custom_retirement_date,
                                    lumped_steel_retire_dfs_before1relining, lumped_steel_retire_dfs_1relining, 
                                    lumped_steel_retire_dfs_before2relining, lumped_steel_retire_dfs_2relining,
                                    lumped_steel_custom_phaseout, new_lumped_steel_dfs, new_lumped_steel_dfs_announcements,
                                    new_retired_lumped_steel_dfs, new_retired_lumped_steel_dfs_announcements, new_retired_lumped_steel_dfs_1relining,
                                    new_retired_lumped_steel_dfs_2relining, new_retired_lumped_steel_dfs_2relining_announcements, new_retired_lumped_steel_dfs_1relining_announcements],
                                    axis = 1).fillna(0)

    #add missing year rows by reindexing the df
    #start must be so low as there are plants that only start operating then. Otherwise the capacities are not calculated properly
    full_index = pd.RangeIndex(start = 1839, stop = 2081, step = 1)
    lumped_steel_combine = lumped_steel_combine.reindex(full_index)
    lumped_steel_combine = lumped_steel_combine.fillna(0)

    #get net BOF capacity changes (additions - retirements)
    lumped_steel_combine["Net BOF capacity"] = lumped_steel_combine["BOF Capacity additions"] - lumped_steel_combine["Retired BOF capacity"]
    
    #add the "mystery" plants and their custom phase out (calculated above)
    lumped_steel_combine["Net BOF capacity"] = lumped_steel_combine["Net BOF capacity"] + lumped_steel_combine["BOF Capacity additions (no retirement date)"] - lumped_steel_combine["Retired BOF Capacity (no retirement date)"]
    
    #yearly steel capacity additions for steel "waves" plot
    # we add the plants that have no retirement date information
    lumped_steel_combine["Yearly BOF Capacity additions"] = lumped_steel_combine["BOF Capacity additions"] + lumped_steel_combine["BOF Capacity additions (no retirement date)"]

    ### CALC CUMULATIVE CAPACITIES###
    #calculate total BOF capacity
    lumped_steel_combine["Remaining BOF capacity"] = lumped_steel_combine["Net BOF capacity"].cumsum()

    #calculate total BOF capacity additions only ! Without retirements
    lumped_steel_combine["Total BOF capacity additions"] = lumped_steel_combine["BOF Capacity additions"].cumsum()
    
    #calculate new BOF additions - from the new_bof_df (announced, planned and under-construction projects)
    lumped_steel_combine["Total BOF capacity additions (new projects)"] = lumped_steel_combine["BOF Capacity additions (new projects)"].cumsum()

    #calculate alternative for plot 2 RIKEN: net new BOF additions, in the case of 1 relining
    lumped_steel_combine["Total net BOF capacity additions (new projects, 1 relining)"] = lumped_steel_combine["BOF Capacity additions (new projects)"].cumsum() - lumped_steel_combine["Retired BOF capacity (new projects, 1 relining)"].cumsum()
    
    #calculate alternative for plot 2 RIKEN: net new BOF additions, in the case of 2 relinings
    lumped_steel_combine["Total net BOF capacity additions (new projects, 2 relining)"] = lumped_steel_combine["BOF Capacity additions (new projects)"].cumsum() - lumped_steel_combine["Retired BOF capacity (new projects, 2 relining)"].cumsum()
    
    #calculate subset for plot 1 RIKEN: net new capacity additions but only announced project part
    lumped_steel_combine["Total net BOF capacity additions (new projects, announced only)"] = lumped_steel_combine["BOF Capacity additions (new projects, announced only)"].cumsum() - lumped_steel_combine["Retired BOF capacity (new projects, announced only)"].cumsum()

    # calc only under construction additions (not cumulative, but net per year)
    lumped_steel_combine["BOF capacity additions (new projects, under construction only)"] = lumped_steel_combine["BOF Capacity additions (new projects)"] - lumped_steel_combine["BOF Capacity additions (new projects, announced only)"]

    #calculate subset for plot 2 RIKEN: net new capacity additions with 2 relinings, only announced projects
    lumped_steel_combine["Total net BOF capacity additions (new projects, 2 relining, announced only)"] = lumped_steel_combine["BOF Capacity additions (new projects, announced only)"].cumsum() - lumped_steel_combine["Retired BOF capacity (new projects, 2 relining, announced only)"].cumsum()

    #calculate subset for plot 2 RIKEN: net new capacity additions with 1 relinings, only announced projects
    lumped_steel_combine["Total net BOF capacity additions (new projects, 1 relining, announced only)"] = lumped_steel_combine["BOF Capacity additions (new projects, announced only)"].cumsum() - lumped_steel_combine["Retired BOF capacity (new projects, 1 relining, announced only)"].cumsum()

    # BOF additions from plants with no retirement date (not in total cap additions)
    lumped_steel_combine["Total BOF capacity additions (no retirement date)"] = lumped_steel_combine["BOF Capacity additions (no retirement date)"].cumsum()

    #total BOF capacity retirements (but excluding plants with no available retirement date)
    # ie these are plants that have a planned retirement date or a custom calculated one
    lumped_steel_combine["Total retired BOF capacity"] = lumped_steel_combine["Retired BOF capacity"].cumsum()
    
    #calculate total retired BOF capacity from these project announcements specifically
    lumped_steel_combine["Total retired BOF capacity (new projects)"] = lumped_steel_combine["Retired BOF capacity (new projects)"].cumsum()

    #calculate total retired BOF capacity from these project announcements specifically, IF there is an extra relining cycle
    lumped_steel_combine["Total retired BOF capacity (new projects, 1 relining)"] = lumped_steel_combine["Retired BOF capacity (new projects, 1 relining)"].cumsum()

    #same for 2 relinings
    lumped_steel_combine["Total retired BOF capacity (new projects, 2 relining)"] = lumped_steel_combine["Retired BOF capacity (new projects, 2 relining)"].cumsum()

    #calculate net removals of plants with no retirement information
    #need to be indicated in the stacked bar plot.
    lumped_steel_combine["Total retired BOF capacity (no retirement date)"] = lumped_steel_combine["Retired BOF Capacity (no retirement date)"].cumsum()

    # calculate net removals of plants that have custom calculated retirement dates 
    # (operating and operating pre-retirement only, as announced and under construction plants are already accounted for)
    lumped_steel_combine["Total retired BOF capacity (custom calculated retirement date)"] = lumped_steel_combine["Retired BOF Capacity (custom calculated retirement date)"].cumsum()

    # calculate net additional capacity, if portion of plants that are operating and could have an additional relining cycle (young plants) operate for this additional time
    # looks weird - but it works. Shows what capacity stays online in this case
    lumped_steel_combine["Additional net BOF capacity (operating, 1 relining)"] = lumped_steel_combine["Retired BOF capacity (operating, 1 relining) before 1 relining"].cumsum() - lumped_steel_combine["Retired BOF capacity (operating, 1 relining)"].cumsum()

    #same as above but with two relinings
    lumped_steel_combine["Additional net BOF capacity (operating, 2 relining)"] = lumped_steel_combine["Retired BOF capacity (operating, 2 relining) before 2 relining"].cumsum() - lumped_steel_combine["Retired BOF capacity (operating, 2 relining)"].cumsum()

    #calculate additional BOF capacity from new projects, if there are 2 relinings
    lumped_steel_combine["Additional net BOF capacity (new projects, 2 relining)"] = lumped_steel_combine["Retired BOF capacity (new projects)"].cumsum() - lumped_steel_combine["Retired BOF capacity (new projects, 2 relining)"].cumsum()
    
    # net remaining BOF cap, with 2 relining assumption. Still missing additional net bof capacity from new plants but not currently needed
    # for now, this column is only needed for 2030 gov targets calculation.
    # therefore, we only take into account new projects under construction by substracting
    # ""Total net BOF capacity additions (new projects, announced only)""
    lumped_steel_combine["Remaining BOF capacity (2 relinings)"] = lumped_steel_combine["Net BOF capacity"].cumsum() + lumped_steel_combine["Additional net BOF capacity (operating, 2 relining)"] + lumped_steel_combine["Additional net BOF capacity (new projects, 2 relining)"] - lumped_steel_combine["Total net BOF capacity additions (new projects, announced only)"]
    
    #add a component grouping capacity additions over 5 years
    # for REMIND analysis
    lumped_steel_combine["Grouped years"] = ((lumped_steel_combine.index - 1851) // 5) * 5 + 1851
    temp_grouped_capadditions = lumped_steel_combine.groupby("Grouped years")["BOF Capacity additions"].sum().rename("5year BOF capacity additions")
    lumped_steel_combine["5year BOF capacity additions"] = lumped_steel_combine["Grouped years"].map(temp_grouped_capadditions) / 5 #divide by 5 to get the average per year

    temp_grouped_capadditions_construction = lumped_steel_combine.groupby("Grouped years")["BOF capacity additions (new projects, under construction only)"].sum().rename("5year BOF capacity additions (new, under construction)")
    lumped_steel_combine["5year BOF capacity additions (new, under construction)"] = lumped_steel_combine["Grouped years"].map(temp_grouped_capadditions_construction) / 5 #divide by 5 to get the average per year
    
    # #add missing year rows by reindexing the df
    # #start must be so low as there are plants that only start operating then. Otherwise the capacities are not calculated properly
    full_index = pd.RangeIndex(start = 1851, stop = 2081, step = 1)
    lumped_steel_combine = lumped_steel_combine.reindex(full_index)
    
    #commented this out. I don't think it's necessary due to the cumsum method
    #add missing years in total BOF capacity using forward fill:
    #lumped_steel_combine["Remaining BOF capacity"] = lumped_steel_combine["Remaining BOF capacity"].ffill()

    return lumped_steel_combine


def summarise_BOF_capacity_for_regions(steel_dfs, new_steel_dfs, old_steel_dfs, region = ["global"], relining_cycle_0=20, relining_cycle_1=15, relining_cycle_2=10, region_name = None, start_date = 2000):
    
    list_of_dfs = []
    
    for reg in region:
        temp_df = summarise_BOF_capacity_single_region(steel_dfs, old_steel_dfs, new_steel_dfs, reg, relining_cycle_0=relining_cycle_0, relining_cycle_1=relining_cycle_1, relining_cycle_2=relining_cycle_2)
        #filter for years before 2020:
        temp_df = temp_df.loc[temp_df.index.isin(np.arange(start_date,2081,1) ), ["Remaining BOF capacity",
                                                                            "Remaining BOF capacity (2 relinings)",
                                                                            "Total BOF capacity additions",
                                                                            "Total retired BOF capacity",
                                                                            "Total BOF capacity additions (no retirement date)",
                                                                            "Total retired BOF capacity (no retirement date)",
                                                                            "Total retired BOF capacity (custom calculated retirement date)",
                                                                            'Additional net BOF capacity (operating, 1 relining)',
                                                                            "Additional net BOF capacity (operating, 2 relining)",
                                                                            "5year BOF capacity additions",
                                                                            "5year BOF capacity additions (new, under construction)",
                                                                            "Total BOF capacity additions (new projects)",
                                                                            "Total net BOF capacity additions (new projects, announced only)", 
                                                                            "Total retired BOF capacity (new projects)",
                                                                            "Total retired BOF capacity (new projects, 1 relining)",
                                                                            "Total retired BOF capacity (new projects, 2 relining)",
                                                                            "Total net BOF capacity additions (new projects, 2 relining)",
                                                                            "Total net BOF capacity additions (new projects, 2 relining, announced only)",
                                                                            "Total net BOF capacity additions (new projects, 1 relining)",
                                                                            "Total net BOF capacity additions (new projects, 1 relining, announced only)",
                                                                            "Yearly BOF Capacity additions"]
                                                                            ].reset_index()
        temp_df["Region"] = reg
        list_of_dfs.append(temp_df)

    df = pd.concat(list_of_dfs)
    df = df.groupby("index").sum()

    if region_name:
        df["Region"] = region_name

    # print(df)
    return df


def obtain_BOF_remaining_capacity_REMIND(*args, **kwargs):
    """Calculate future BOF capacity additions in the format of REMIND variables
    (vm_deltacap and vm_cap). Used to bound the model's short-term additions based on the GEM data.

    Args:
        *args: positional argument for the function
        **kwargs: keyword arguments for the function
    
    Returns:
        Saves a csv file with the REMIND BOF capacity data per REMIND region.
    """

    dfs = []
    for region_code, countries in REMIND_REGIONS.items():
        df = summarise_BOF_capacity_for_regions(*args, **kwargs, region=countries, region_name=region_code)
        dfs.append(df)
    all_remind_regions = pd.concat(dfs).reset_index().rename(columns={"index": "Year"})    
    
    # all capacities are by default in thousand tons per annum
    # REMIND units are in Gtpa
    all_remind_regions["BOF vcap"] = all_remind_regions["Remaining BOF capacity"] / 1000000
    # to fit REMIND, recall that these are the annual additions over a 5 year period. Ie do "5 year capacity additions" *5 for actual capacity installed in, say, 2025-2030.
    all_remind_regions["BOF deltaCap"] = all_remind_regions["5year BOF capacity additions"] / 1000000
    all_remind_regions["BOF deltaCap (new, under construction)"] = all_remind_regions["5year BOF capacity additions (new, under construction)"] / 1000000
    all_remind_regions.rename({"Remaining BOF capacity":"Remaining BOF capacity (Mtpa)"})
    
    # sort and save
    all_remind_regions.sort_values(by = ["Year", "Region"], inplace = True)
    all_remind_regions.to_csv(os.path.join(output_dir, "BOF_capacity_perREMINDregion.csv"), 
                              index = False)

def main():
    """main function to run the analysis and save the results as csvs in the output folder.
    """
    # different retrofit cycle times, in years.
    # RELINING_CYCLE_0 is the time between the plant start and the first relining
    # RELINING_CYCLE_1 is the time between the first and second relining
    # RELINING_CYCLE_2 is the time between the second relining and the plant retirement.

    RELINING_CYCLE_0 = 20
    RELINING_CYCLE_1 = 15
    RELINING_CYCLE_2 = 10

    # calculate main dataframes
    steel_dfs, new_steel_dfs, old_steel_dfs = analyse_steel_dfs(
        relining_cycle_0=RELINING_CYCLE_0, 
        relining_cycle_1=RELINING_CYCLE_1, 
        relining_cycle_2=RELINING_CYCLE_2)

    # save summary table for diagnostics
    all_analyzed_dfs = pd.concat([steel_dfs, new_steel_dfs, old_steel_dfs])
    all_analyzed_dfs.to_csv(os.path.join(output_dir, "summary_table.csv"))

    # convert data to REMIND format: using REMIND regions and REMIND variables
    # this data is only used to constrain the model, and not used for further analysis
    obtain_BOF_remaining_capacity_REMIND(
        steel_dfs, new_steel_dfs, old_steel_dfs,
        relining_cycle_0=RELINING_CYCLE_0,
        relining_cycle_1=RELINING_CYCLE_1,
        relining_cycle_2=RELINING_CYCLE_2,
        start_date = 1900)

    # main function: calculate BOF capacities globally, split in different categories
    # (retired, operating, operating with a relining, new plants etc)
    # this is used to calculate the steel lock-in risk
    GL = summarise_BOF_capacity_for_regions(
        steel_dfs, new_steel_dfs, old_steel_dfs, 
        region = ["global"],
        relining_cycle_0=RELINING_CYCLE_0,
        relining_cycle_1=RELINING_CYCLE_1,
        relining_cycle_2=RELINING_CYCLE_2,
        region_name = None)
    GL.to_csv(os.path.join(output_dir, "global_BOFcap.csv"))

    # calculate age distribution of BOF plants, globally and for key regions
    age_df = calculate_age_distribution(steel_dfs, region = ["global"])
    age_df.to_csv(os.path.join(output_dir, "age_distribution.csv"))
    
    # regional age distributions for key regions
    GROUPS_TO_CALC_AGE_DISTRIBUTION = {
        "IND": REMIND_REGIONS["IND"],
        "CHA": REMIND_REGIONS["CHA"],
        "EUR+RUS":  REMIND_REGIONS["EUR"] + REMIND_REGIONS["NEU"] + REMIND_REGIONS["REF"],
        "JPN":  REMIND_REGIONS["JPN"]
    }
    list_reg_age_df = []
    for region_name, countries in GROUPS_TO_CALC_AGE_DISTRIBUTION.items():
        temp_df = calculate_age_distribution(steel_dfs, region = countries, region_name=region_name)
        list_reg_age_df.append(temp_df)
    reg_age_df = pd.concat(list_reg_age_df, axis = 0).sort_index()
    reg_age_df.to_csv(os.path.join(output_dir, "age_distribution_regions.csv"))

if __name__ == "__main__":
    main()