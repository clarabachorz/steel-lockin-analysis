library(ggplot2)
library(gdx)
require(dplyr)
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(pals)
library(stringr)
library("quitte")
library(zoo)
library(purrr)
library(openxlsx)
library(rlang)
library(ggpattern)


# --- Helper functions for scenario abatement cost analysis ---

#' Clean up lcop df and remove emissions portion (since we are calculating
#' abatement costs)
#'
#' This function cleans up the lcop df, filters the appropriate dates 
#' and removes the emission cost component.
#' @param df Data frame with lcop data
#' @return clean data frame
prepare_lcop_df <- function(df) {
    df %>%
        filter(component != "Emission Cost") %>%
        filter(between(period, 2025, 2075)) %>%
        select(-color) %>%
        mutate(category = ifelse(str_detect(component, regex("Electricity|H2|Hydrogen|Gas|Liquids|Solids", ignore_case = TRUE)),
            "energy",
            "non-energy"))
        # %>%
        # mutate(value = ifelse(route == "DRI-EAF H2", ifelse(component == "Hydrogen", 0, value), value)) %>%
        # mutate(value = ifelse(component == "Electricity", 0, value))
    
}

#' Calculate total (average) LCOP by region and period
#'
#' This function groups the lcop df by period and region
#' and calculates the total (average) LCOP.
#' @param df Data frame with lcop data
#' @return summarized data frame
summarise_lcop_total <- function(df) {
    df %>%
        group_by(period, region, category) %>%
        summarise(value = sum(value, na.rm = TRUE)) %>%
        ungroup()
}

#' Calculate total yearly steel production by region and period
#'
#' This function calculates total steel production from the mif file,
#' by summing over all steel production pathways
#' (which are in the format "Production|Industry|Steel|+")
#' @param df Data frame from the mif, with all variables from the REMIND runs
#' @return summed and filtered data frame
get_total_production <- function(df) {
    df %>%
        filter(grepl("Production|Industry|Steel|+",
                    variable, ignore.case = TRUE, fixed = TRUE)) %>%
        filter(unit == "Mt/yr") %>%
        filter(between(period, 2025, 2075)) %>%
        mutate(variable = gsub("Production|Industry|Steel|+", "", variable)) %>%
        mutate(variable = sub("^\\|\\|\\|\\+\\|", "", 
                            variable, fixed = FALSE)) %>%
        group_by(period, region) %>%
        summarise(total_production = sum(value, na.rm = TRUE)) %>%
        ungroup()
}

#' Calculate total yearly CO2 emissions from steel production
#' by region and period in MtCO2/yr
#'
#' This function calculates total CO2 emissions from steel production
#' from the mif file, by summing over all final energy CO2 emissions
#' attributed to the steel subsector
#' (which are in the format "Emi|CO2|Energy|Demand|Industry|Steel|+").
#' @param df Data frame from the mif, with all variables from the REMIND runs
#' @return summed and filtered data frame with total yearly emissions
get_total_emissions <- function(df) {
    df %>%
        filter(grepl("Emi|CO2|Energy|Demand|Industry|Steel|+",
                    variable, ignore.case = TRUE, fixed = TRUE)) %>%
        filter(between(period, 2025, 2075)) %>%
        select(-model) %>%
        group_by(period, region) %>%
        summarise(total_CO2_yearly = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        select(period, region, total_CO2_yearly)
}

#' Extract CO2 prices
#'
#' This function extracts CO2 prices from the mif file
#' @param df Data frame from the mif, with all variables from the REMIND runs
#' @return filtered data frame with CO2 prices
get_co2_price <- function(df) {
    df %>%
        filter(variable == "Price|Carbon") %>%
        filter(region != "World") %>%
        filter(between(period, 2025, 2075)) %>%
        select(period, region, value) %>%
        rename(co2_price = value)
}

#' Calculate emission difference between two scenarios
#'
#' This function calculates the emission difference between two scenarios
#' @param df1 Data frame with emissions from the 1st scenario
#' @param df2 Data frame with emissions from the 2nd scenario
#' @return Data frame with emission difference
calc_em_difference <- function(df1, df2) {
    df <- df1 %>%
        left_join(df2, by = c("period", "region")) %>%
        mutate(em_diff = total_CO2_yearly.x - total_CO2_yearly.y) %>%
        select(period, region, em_diff)
}

#' Calculate cost difference between two scenarios
#'
#' This function calculates the cost difference between two scenarios
#' @param df1 Data frame with cost from the 1st scenario
#' @param df2 Data frame with cost from the 2nd scenario
#' @return Data frame with cost difference
calc_cost_diff <- function(df1, df2) {
    df <- df1 %>%
        left_join(df2, by = c("period", "region")) %>%
        mutate(cost_diff = total_cost.y - total_cost.x) %>%
        select(period, region, cost_diff)

}

# Calculate total costs (LCO * production)
# in USD/t * Mt = million USD
calc_total_costs <- function(lcop_total, prod) {
    lcop_total %>%
        left_join(prod, by = c("period", "region")) %>%
        mutate(total_cost = value * total_production) %>%
        select(-value, -total_production) %>%
        filter(between(period, 2025, 2075))
}



#' Calculate demand adjustment between two scenarios
#'
#' This function computes the value of demand adjustment by multiplying the average price
#' of the two scenarios with the difference in production between them.
#' @param scenario1_mif Data frame with all variables for scenario 1 (e.g., lock-in)
#' @param scenario2_mif Data frame with all variables for scenario 2 (e.g., fast transition)
#' @param scenario2_prod Production data frame for scenario 2
#' @param scenario1_prod Production data frame for scenario 1
#' @return Data frame with columns period, region, and demand_value (in million USD)
calc_demand_adjustment <- function(scenario1_mif, scenario2_mif, scenario2_prod, scenario1_prod) {
    # Calculate average price (USD/t) for each period and region
    avg_price <- scenario1_mif %>%
        mutate(scenario = "Scenario1") %>%
        filter(grepl("CES Function|CES Price|ue_steel_primary", variable, ignore.case = TRUE, fixed = TRUE)) %>%
        bind_rows(scenario2_mif %>%
                    filter(grepl("CES Function|CES Price|ue_steel_primary", variable, ignore.case = TRUE, fixed = TRUE)) %>%
                    mutate(scenario = "Scenario2")) %>%
        filter(between(period, 2025, 2075)) %>%
        select(-model, -unit) %>%
        mutate(value = value * 1000) %>% # convert to USD/t
        pivot_wider(names_from = scenario, values_from = value) %>%
        mutate(price_average = (Scenario2 + Scenario1) / 2) %>%
        select(period, region, price_average)

    # Calculate production difference (in Mt)
    prod_diff <- scenario2_prod %>%
        left_join(scenario1_prod, by = c("period", "region")) %>%
        mutate(prod_diff = total_production.y - total_production.x) %>%
        select(period, region, prod_diff)

    # Calculate demand value (USD/t * Mt = million USD)
    demand_value <- avg_price %>%
        left_join(prod_diff, by = c("period", "region")) %>%
        mutate(demand_value = price_average * prod_diff) %>%
        select(period, region, demand_value)

    return(demand_value)
}

#' Add the demand correction costs to the second scenario (more ambitious scenario,
#' which typically has lower demand)
#'
#' This function adds the demand adjustment as costs by adding this to the total
#' costs of scenario 2.
#' @param scenario2_costs Data frame with total costs for scenario 2 (e.g., fast transition)
#' @param demand_value Data frame with demand adjustment costs for scenario 2,
#' calculated in calc_demand_adjustment
#' @return Updated scenario2_costs with the demand adjustment costs added
add_demand_adjustment <- function(scenario2_costs, demand_value) {
    scenario2_costs %>%
        left_join(demand_value, by = c("period", "region")) %>%
        # we add the demand adjustment to the "non-energy" category
        mutate(total_cost = ifelse(category == "non-energy", total_cost + demand_value, total_cost)) %>%
        select(-demand_value)
}


#' Get corresponding decade from the year
#'
#' This function computes the corresponding decade to a certain year
#' @param year Year (e.g., 2025)
#' @return Decade as string (e.g., "2020s")
get_decade <- function(year) {
  paste0(floor(year / 10) * 10, "s")
}

#' Calculate cumulated discounted sum of a certain column (eg cost or emission)
#' over a certain time period
#' @param lockin_mif_em Data frame with costs or emissions
#' @param start_date Start year (e.g., 2025) for the discounting, and for the cumulative sum if it is taken
#' @param end_date End year (e.g., 2075) for the discounting, and for the cumulative sum if it is taken
#' @param discount_rate Discount rate (e.g., 0.05 for 5%)
#' @param col_to_sum Column name to sum over (e.g., "total_cost" or "total_CO2_yearly")
#' @param final_col_name Name of the final column with the cumulated discounted sum (e.g., "cost1" or "em1")
#' @param by_decade Logical, whether to return the cumulated sum by decade (TRUE) or not (FALSE)
#' @param not_cumu Logical, whether to return the df before the cumulative sum is taken (TRUE) or return the final cumulated sum (FALSE)
#' @return Data frame with a new column: either discounted (not_cumu) or cumulative discounted over
#' the whole period (by_decade = FALSE) or by decade (by_decade = TRUE)
calc_cumulated_discounted_sum <- function(lockin_mif_em, start_date, end_date, discount_rate, col_to_sum, final_col_name, by_decade = FALSE, not_cumu = FALSE) {
    lockin_mif_em <- lockin_mif_em %>%
        group_by(region) %>%
        complete(period = full_seq(period, 1)) %>%  # Fill every year
        arrange(region, period) %>%
        # in million USD, or in MtCO2
        mutate(value = !!sym(col_to_sum)) %>%
        fill(value, .direction = "down") %>%
        ungroup() %>%
        select(-all_of(col_to_sum)) %>%
        # correct time period. 2025 = 2023-2028 in REMIND
        mutate(period = period - 2) %>% 
        filter(between(period, start_date, end_date)) %>%
        arrange(region, period) %>%
        mutate(discounted_value = value / ((1 + discount_rate) ^ (period - start_date))) %>%
        mutate(decade = get_decade(period))
    if (by_decade) {
        lockin_mif_em <- lockin_mif_em %>%
            group_by(region, decade) %>%
            summarise(!!final_col_name := sum(discounted_value, na.rm = TRUE)) %>%
            ungroup()
        return(lockin_mif_em)
        }
    # option to return the df before the cumulative sum is taken
    if (not_cumu) {
        return(lockin_mif_em)
    }
    lockin_mif_em <- lockin_mif_em %>%
        group_by(region) %>%
        summarise(!!final_col_name := sum(discounted_value, na.rm = TRUE)) %>%
        ungroup()
}

#' Calculate cumulated discounted sum of the cost column: as this is split in energy and non-energy categories
#' @param lockin_mif_em Data frame with costs or emissions
#' @param start_date Start year (e.g., 2025) for the discounting, and for the cumulative sum if it is taken
#' @param end_date End year (e.g., 2075) for the discounting, and for the cumulative sum if it is taken
#' @param discount_rate Discount rate (e.g., 0.05 for 5%)
#' @param col_to_sum Column name to sum over (e.g., "total_cost" or "total_CO2_yearly")
#' @param final_col_name Name of the final column with the cumulated discounted sum (e.g., "cost1" or "em1")
#' @param by_decade Logical, whether to return the cumulated sum by decade (TRUE) or not (FALSE)
#' @param not_cumu Logical, whether to return the df before the cumulative sum is taken (TRUE) or return the final cumulated sum (FALSE)
#' @return Data frame with a new column: either discounted (not_cumu) or cumulative discounted over
#' the whole period (by_decade = FALSE) or by decade (by_decade = TRUE)
calc_cumulated_discounted_sum_costsonly <- function(lockin_mif_em, start_date, end_date, discount_rate, col_to_sum, final_col_name, by_decade = FALSE, not_cumu = FALSE) {
    lockin_mif_em <- lockin_mif_em %>%
        group_by(region, category) %>%
        complete(period = full_seq(period, 1)) %>%  # Fill every year
        arrange(region,category, period) %>%
        # in million USD, or in MtCO2
        mutate(value = !!sym(col_to_sum)) %>%
        fill(value, .direction = "down") %>%
        ungroup() %>%
        select(-all_of(col_to_sum)) %>%
        # correct time period. 2025 = 2023-2028 in REMIND
        mutate(period = period - 2) %>% 
        filter(between(period, start_date, end_date)) %>%
        arrange(region, category, period) %>%
        mutate(discounted_value = value / ((1 + discount_rate) ^ (period - start_date))) %>%
        mutate(decade = get_decade(period))
    if (by_decade) {
        lockin_mif_em <- lockin_mif_em %>%
            group_by(region, category, decade) %>%
            summarise(!!final_col_name := sum(discounted_value, na.rm = TRUE)) %>%
            ungroup()
        return(lockin_mif_em)
        }
    # option to return the df before the cumulative sum is taken
    if (not_cumu) {
        return(lockin_mif_em)
    }
    lockin_mif_em <- lockin_mif_em %>%
        group_by(region, category) %>%
        summarise(!!final_col_name := sum(discounted_value, na.rm = TRUE)) %>%
        ungroup()
}

# --- Main analysis and plotting function ---
#' Run the abatement analysis to extract abatement costs between two scenarios
#' and plot the results
#' @param scen1 List with name, mif_file, lcop_file for the 1st scenario (e.g., lock-in)
#' @param scen2 List with name, mif_file, lcop_file for the 2nd scenario (e.g., fast transition)
#' @param region_to_aggr List with regions to aggregate, e.g., list("Global North" = c("EUR", "NEU", "USA", "JPN", "REF", "CAZ"), "CHA" = c("CHA"), ...)
#' @param start_date Start year (e.g., 2025) for the discounting, and for the cumulative sum if it is taken
#' @param end_date End year (e.g., 2075) for the discounting, and for the cumulative sum if it is taken
#' @param discount_rate Discount rate (e.g., 0.05 for 5%)
#' @return List with fscp_df_grouped (data frame with abatement costs by region) and plot (ggplot object)
run_abatement_analysis <- function(scen1, scen2, region_to_aggr, start_date, end_date, discount_rate) {
    
    # Plot order
    region_order <- c("EUR+NEU+USA+JPN+REF+CAZ", "CHA", "IND", "OASLAM", "MEA", "SSA")


    # Read data
    lcop1 <- read.csv(scen1$lcop_file)
    lcop2 <- read.csv(scen2$lcop_file)
    mif1 <- read.quitte(scen1$mif_file)
    mif2 <- read.quitte(scen2$mif_file)

    # Prepare data
    lcop1_no_em <- prepare_lcop_df(lcop1)
    lcop2_no_em <- prepare_lcop_df(lcop2)
    lcop1_no_em_total <- summarise_lcop_total(lcop1_no_em)
    lcop2_no_em_total <- summarise_lcop_total(lcop2_no_em)
    mif1_prod <- get_total_production(mif1)
    mif2_prod <- get_total_production(mif2)
    mif1_em <- get_total_emissions(mif1)
    mif2_em <- get_total_emissions(mif2)
    costs1 <- calc_total_costs(lcop1_no_em_total, mif1_prod)
    costs2 <- calc_total_costs(lcop2_no_em_total, mif2_prod)
    # Demand adjustment
    # adjusts for the loss of welfare arising from less steel production
    # using delta_production * avg_steel_price
    # if scenario 1 produces more steel than scenario 2, the cost of scenario 2 increases.
    # if scenario 1 produces less steel than scenario 2, the cost of scenario 2 decreases.
    demand_value <- calc_demand_adjustment(mif1, mif2, mif2_prod, mif1_prod)
    costs2_adj <- add_demand_adjustment(costs2, demand_value)

    # Discounted sums
    costs1_disc <- calc_cumulated_discounted_sum_costsonly(costs1, start_date, end_date, discount_rate, "total_cost", "cost1")
    costs2_disc <- calc_cumulated_discounted_sum_costsonly(costs2_adj, start_date, end_date, discount_rate, "total_cost", "cost2")
    em1_total <- calc_cumulated_discounted_sum(mif1_em, start_date, end_date, discount_rate, "total_CO2_yearly", "em1")
    em2_total <- calc_cumulated_discounted_sum(mif2_em, start_date, end_date, discount_rate, "total_CO2_yearly", "em2")
    em1_total_nodiscount <- calc_cumulated_discounted_sum(mif1_em, start_date, end_date, 0, "total_CO2_yearly", "em1_nodiscount")
    em2_total_nodiscount <- calc_cumulated_discounted_sum(mif2_em, start_date, end_date, 0, "total_CO2_yearly", "em2_nodiscount")

    # Calculate abatement cost
    fscp_df <- costs1_disc %>%
        left_join(costs2_disc, by = c("region", "category")) %>%
        left_join(em1_total, by = "region") %>%
        left_join(em2_total, by = "region") %>%
        left_join(em1_total_nodiscount, by = "region") %>%
        left_join(em2_total_nodiscount, by = "region") %>%
        mutate(fscp = (cost2 - cost1) / (em1 - em2)) %>%
        mutate(em_diff = em1_nodiscount - em2_nodiscount)

    # Aggregate regions
    grouped_df <- imap_dfr(region_to_aggr, function(regions, new_region) {
        fscp_df %>%
            filter(region %in% regions) %>%
            group_by(category) %>%
            summarise(
                region = new_region,
                cost1 = sum(cost1, na.rm = TRUE),
                cost2 = sum(cost2, na.rm = TRUE),
                em1 = sum(em1, na.rm = TRUE),
                em2 = sum(em2, na.rm = TRUE),
                em1_nodiscount = sum(em1_nodiscount, na.rm = TRUE),
                em2_nodiscount = sum(em2_nodiscount, na.rm = TRUE)
            ) %>%
            mutate(
                fscp = (cost2 - cost1) / (em1 - em2),
                em_diff = em1_nodiscount - em2_nodiscount
            )
    })
    new_regions_flattened <- unlist(region_to_aggr, use.names = FALSE)
    fscp_df_grouped <- fscp_df %>%
        filter(!region %in% new_regions_flattened) %>%
        bind_rows(grouped_df)

    fscp_df_grouped <- fscp_df_grouped %>%
        mutate(region = factor(region, levels = region_order)) %>%
        arrange(region)

    fscp_df_grouped <- fscp_df_grouped %>%
        select(region, category, fscp, em_diff) %>%
        filter(em_diff > 0) %>%
        # some regions have negative average abatement costs when looking at very small
        # abatement volumes (less than 100 MtCO2)
        filter(!(fscp < 0 & em_diff < 200)) %>%
        mutate(em_diff = em_diff / 1000) # convert to GtCO2
    
    total_fscp <- fscp_df_grouped %>%
        group_by(region) %>%
        summarise(total = sum(fscp)) %>%
        ungroup()

    fscp_df_grouped <- fscp_df_grouped %>%
        left_join(total_fscp, by = "region") %>%
        # calculate ymin (where the rectangles start)
        mutate(category = factor(category, levels = c("non-energy","energy"))) %>%
        group_by(region) %>%
        arrange(region, category) %>%
        mutate(ymin = lag(fscp, default = 0),
               ymax = fscp+ lag(fscp, default = 0)) %>%
        ungroup() %>%
        arrange(total) %>%
        group_by(category) %>%
        mutate(cum_em = cumsum(em_diff),
               xmin = cum_em - em_diff,
            #    region = factor(region, levels = region),
               xmax = cum_em)
        
    region_order_by_total <- total_fscp %>% arrange(total) %>% pull(region)

    region_display_names <- c(
      "EUR+NEU+USA+JPN+REF+CAZ" = "Global North",
      "CHA" = "China",
      "IND" = "India",
      "OASLAM" = "Other Asia & Latin America",
      "MEA" = "Middle East & Africa",
      "SSA" = "Sub-Saharan Africa"
    )

    fscp_df_grouped$region <- factor(fscp_df_grouped$region, levels = region_order_by_total)
    print(fscp_df_grouped$region)

    # Plotting
    plot_abatement_cost <- function(df, discount_rate, start_date, end_date, scen1_name, scen2_name) {
        ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = region, alpha = category)) +
            geom_rect(color = "black") +
            labs(
                title = paste("Average abatement costs:", scen1_name, "to", scen2_name, "\n(discount rate:", discount_rate * 100, "%, dates:", start_date, "-", end_date, ")"),
                x = "Cumulative emission difference (GtCo2)",
                y = "Average abatement cost (USD/tCO2)"
            ) +
            scale_fill_manual(
                name = "Region",
                values = c(
                "CHA" = "#dc050c",
                "IND" = "#f1932d",
                "EUR+NEU+USA+JPN+REF+CAZ" = "#1965b0",
                "OASLAM" = "#f7f056",
                "SSA" = "#4eb265",
                "MEA" = "#ae76a3"),
                labels = region_display_names) +
            scale_alpha_manual(values = c("energy" = 0.6, "non-energy" = 1),
                    name = "Cost",
                    labels = c("energy" = "Energy", "non-energy" = "Non-energy"),
                    guide = guide_legend(reverse = TRUE)) +
            ylim(0, 350) +
            xlim(0,70)
    }

    list(
        fscp_df_grouped = fscp_df_grouped,
        plot = plot_abatement_cost(fscp_df_grouped, discount_rate, start_date, end_date, scen1$name, scen2$name)
    )
}


#' Run the abatement cost and CO2 price comparison analysis to extract
#' the cost difference between two scenarios and the value of avoided CO2 emissions
#' and plot the results
#' @param scen1 List with name, mif_file, lcop_file for the 1st scenario (e.g., lock-in)
#' @param scen2 List with name, mif_file, lcop_file for the 2nd scenario (e.g., fast transition)
#' @param region_to_aggr List with regions to aggregate, e.g., list("Global North" = c("EUR", "NEU", "USA", "JPN", "REF", "CAZ"), "CHA" = c("CHA"), ...)
#' @param start_date Start year (e.g., 2025) for the discounting, and for the cumulative sum if it is taken
#' @param end_date End year (e.g., 2075) for the discounting, and for the cumulative sum if it is taken
#' @param discount_rate Discount rate (e.g., 0.05 for 5%)
#' @return List with total_df_grouped (data frame with cost difference and value of avoided emissions by region) and plot (ggplot object)
run_scenario_cost_andCO2value_comparison <- function(scen1, scen2, region_to_aggr, start_date, end_date, discount_rate) {
    
    # Plot order
    region_order <- c("EUR+NEU+USA+JPN+REF+CAZ", "CHA", "IND", "OASLAM", "MEA", "SSA")

    #region names
    region_display_names <- c(
    "EUR+NEU+USA+JPN+REF+CAZ" = "Global North",
    "CHA" = "China",
    "IND" = "India",
    "OASLAM" = "Other Asia & Latin America",
    "MEA" = "Middle East & Africa",
    "SSA" = "Sub-Saharan Africa"
    )

    # Read data
    lcop1 <- read.csv(scen1$lcop_file)
    lcop2 <- read.csv(scen2$lcop_file)
    mif1 <- read.quitte(scen1$mif_file)
    mif2 <- read.quitte(scen2$mif_file)

    # Prepare data
    lcop1_no_em <- prepare_lcop_df(lcop1)
    lcop2_no_em <- prepare_lcop_df(lcop2)
    lcop1_no_em_total <- summarise_lcop_total(lcop1_no_em)
    lcop2_no_em_total <- summarise_lcop_total(lcop2_no_em)
    mif1_prod <- get_total_production(mif1)
    mif2_prod <- get_total_production(mif2)
    mif1_em <- get_total_emissions(mif1)
    mif2_em <- get_total_emissions(mif2)
    costs1 <- calc_total_costs(lcop1_no_em_total, mif1_prod)
    costs2 <- calc_total_costs(lcop2_no_em_total, mif2_prod)

    # Demand adjustment
    # adjusts for the loss of welfare arising from less steel production
    # using delta_production * avg_steel_price
    # if scenario 1 produces more steel than scenario 2, the cost of scenario 2 increases.
    # if scenario 1 produces less steel than scenario 2, the cost of scenario 2 decreases.
    demand_value <- calc_demand_adjustment(mif1, mif2, mif2_prod, mif1_prod)
    costs2_adj <- add_demand_adjustment(costs2, demand_value)

    # we do not need the different categories (energy and non-energy cost) here. Sum over them:
    costs1 <- costs1 %>%
        group_by(period, region) %>%
        summarise(total_cost = sum(total_cost, na.rm = TRUE)) %>%
        ungroup()
    costs2_adj <- costs2_adj %>%
        group_by(period, region) %>%
        summarise(total_cost = sum(total_cost, na.rm = TRUE)) %>%
        ungroup()

    # calculate co2 price in the least ambitious scenario (scen 1)
    # and multiply by emission difference to get the value of the avoided
    # co2 emissions. MtCO2/yr * USD/tCO2 = million USD/yr
    # (this corresponds to the cost of abating those additional emissions in scenario 1,
    # if they have not been avoided, they are abated at the marginal cost = co2 price of scenario 1)
    mif1_co2price <- get_co2_price(mif1)
    mif2_co2price <- get_co2_price(mif2)
    avg_co2price <- mif1_co2price %>%
        left_join(mif2_co2price, by = c("region", "period")) %>%
        mutate(co2_price = (co2_price.x + co2_price.y) / 2) %>%
        select(period, region, co2_price)

    em_diff <- calc_em_difference(mif1_em, mif2_em)
    value_co2_avoided <- avg_co2price %>%
        left_join(em_diff, by = c("region", "period")) %>%
        mutate(value_em = co2_price * em_diff) %>%
        select(period, region, value_em)

    # get cost difference between the two scenarios
    # including the steel demand adjustment
    # this includes CAPEX (investment costs in new steel plants)
    # and OPEX for the steel sector (coal, hydrogen, gas, iron ore, electricity, scrap etc.)
    cost_diff <- calc_cost_diff(costs1, costs2_adj)


    # Discounted sums
    value_co2_avoided_discounted <- calc_cumulated_discounted_sum(value_co2_avoided, start_date, end_date, discount_rate, "value_em", "value_em")
    cost_diff_discounted <- calc_cumulated_discounted_sum(cost_diff, start_date, end_date, discount_rate, "cost_diff", "cost_diff")


    # Calculate abatement cost
    total_df <- value_co2_avoided_discounted %>%
        left_join(cost_diff_discounted, by = "region") %>%
        # add emissions reduced to exclude regions where emissions are higher in the other scenario
        # (em_diff < 0). Multiply by 5 to go from yearly values to total values.
        left_join(em_diff %>% group_by(region) %>% summarise(em_diff = 5* sum(em_diff, na.rm = TRUE)), by = "region")

    # Aggregate regions
    grouped_df <- imap_dfr(region_to_aggr, function(regions, new_region) {
        total_df %>%
            filter(region %in% regions) %>%
            summarise(
                region = new_region,
                value_em = sum(value_em, na.rm = TRUE),
                cost_diff = sum(cost_diff, na.rm = TRUE),
                em_diff = sum(em_diff, na.rm = TRUE)
            )
    })
    new_regions_flattened <- unlist(region_to_aggr, use.names = FALSE)

    total_df_grouped <- total_df %>%
        filter(!region %in% new_regions_flattened) %>%
        bind_rows(grouped_df)
    
    total_df_grouped <- total_df_grouped %>%
        mutate(region = factor(region, levels = region_order)) %>%
        arrange(region)

    total_df_grouped <- total_df_grouped %>%
        # some regions have negative average abatement costs when looking at very small
        # abatement volumes (less than 200 MtCO2)
        filter(em_diff > 200) %>%
        mutate(em_diff = em_diff / 1000) # convert to GtCO2

    plot_df <- total_df_grouped %>%
        select(region, cost_diff, value_em) %>%
        mutate(region_display = region_display_names[as.character(region)]) %>%
        pivot_longer(cols = c(cost_diff, value_em), names_to = "type", values_to = "value") %>%
        mutate(value = value / 1000) # convert to billion USD 

    # Plotting
    plot_abatement_cost <- function(df, discount_rate, start_date, end_date, scen1_name, scen2_name) {
        ggplot(df, aes(x=region_display, y= value, fill = region_display, pattern = type)) +
            geom_bar_pattern(
                stat = "identity",
                position = position_dodge(width = 0.7),
                width = 0.6,
                # color = "#bdb9b9",
                color = "black",
                pattern_fill = "#c9c4c4",
                pattern_color = "#c9c4c4",
                pattern_angle = 45,
                pattern_density = 0.25,
                pattern_spacing = 0.08,
                pattern_key_scale_factor = 0.5
                ) +
            labs(
                title = paste0(
                    "Economic benefit of ",
                    scen2_name,
                    " versus ",
                    scen1_name),
                # title = bquote(
                #             atop("The value of abating additional CO"[2]~"in"~italic(.(scen2_name))~"  ",
                #             "compared to"~italic(.(scen1_name))~"(discount rate:"~.(discount_rate * 100)~"%)")),
                x = "",
                y = "Costs (bill. USD)",
                pattern = "Cost type"
            ) +
            theme_bw(base_size = 10) +
            scale_pattern_manual(
                values = c("cost_diff" = "none", "value_em" = "stripe"),
                labels = c("cost_diff" = "Steel sector additional costs", "value_em" = "Equivalent mitigation\nin other sectors"),
            ) +
            scale_fill_manual(values = c(
                "China" = "#dc050c",
                "India" = "#f1932d",
                "Global North" = "#1965b0"),
                guide = "none") +
            theme(
                axis.text.x = element_text(hjust = 0.5),
                panel.border = element_blank(),
                panel.grid.major = element_line(linewidth = 0.3, color = "#dbd8d8"),
                panel.grid.minor = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "right",
                legend.title.position = "top",
                legend.title = element_text(hjust = 0.5),
                legend.key.size = unit(0.8,"line")
                )
    }

    list(
        total_df_grouped = total_df_grouped,
        plot = plot_abatement_cost(plot_df, discount_rate, start_date, end_date, scen1$name, scen2$name)
    )
}

#' Run the abatement analysis to extract abatement costs between two scenarios
#' by decade and plot the results
#' @param scen1 List with name, mif_file, lcop_file for the 1st scenario (e.g., lock-in)
#' @param scen2 List with name, mif_file, lcop_file for the 2nd scenario (e.g., fast transition)
#' @param region_to_aggr List with regions to aggregate, e.g., list("Global North" = c("EUR", "NEU", "USA", "JPN", "REF", "CAZ"), "CHA" = c("CHA"), ...)
#' @param start_date Start year (e.g., 2025) for the discounting, and for the cumulative sum if it is taken
#' @param end_date End year (e.g., 2075) for the discounting, and for the cumulative sum if it is taken
#' @param discount_rate Discount rate (e.g., 0.05 for 5%)
#' @return Data frame with fscp_df_grouped (data frame with abatement costs by region and decade)
run_abatement_analysis_per_decade <- function(scen1, scen2, region_to_aggr, start_date, end_date, discount_rate) {
    # Read data
    lcop1 <- read.csv(scen1$lcop_file)
    lcop2 <- read.csv(scen2$lcop_file)
    mif1 <- read.quitte(scen1$mif_file)
    mif2 <- read.quitte(scen2$mif_file)

    # Prepare data
    lcop1_no_em <- prepare_lcop_df(lcop1)
    lcop2_no_em <- prepare_lcop_df(lcop2)
    lcop1_no_em_total <- summarise_lcop_total(lcop1_no_em)
    lcop2_no_em_total <- summarise_lcop_total(lcop2_no_em)
    mif1_prod <- get_total_production(mif1)
    mif2_prod <- get_total_production(mif2)
    mif1_em <- get_total_emissions(mif1)
    mif2_em <- get_total_emissions(mif2)
    costs1 <- calc_total_costs(lcop1_no_em_total, mif1_prod)
    costs2 <- calc_total_costs(lcop2_no_em_total, mif2_prod)

    # Demand adjustment
    # adjusts for the loss of welfare arising from less steel production
    # using delta_production * avg_steel_price
    # if scenario 1 produces more steel than scenario 2, the cost of scenario 2 increases.
    # if scenario 1 produces less steel than scenario 2, the cost of scenario 2 decreases.
    demand_value <- calc_demand_adjustment(mif1, mif2, mif2_prod, mif1_prod)
    costs2_adj <- costs2#add_demand_adjustment(costs2, demand_value)

    # Discounted sums
    costs1_disc <- calc_cumulated_discounted_sum(costs1, start_date, end_date, discount_rate, "total_cost", "cost1", by_decade = TRUE)
    costs2_disc <- calc_cumulated_discounted_sum(costs2_adj, start_date, end_date, discount_rate, "total_cost", "cost2", by_decade = TRUE)
    em1_total <- calc_cumulated_discounted_sum(mif1_em, start_date, end_date, discount_rate, "total_CO2_yearly", "em1", by_decade = TRUE)
    em2_total <- calc_cumulated_discounted_sum(mif2_em, start_date, end_date, discount_rate, "total_CO2_yearly", "em2", by_decade = TRUE)
    em1_total_nodiscount <- calc_cumulated_discounted_sum(mif1_em, start_date, end_date, 0, "total_CO2_yearly", "em1_nodiscount", by_decade = TRUE)
    em2_total_nodiscount <- calc_cumulated_discounted_sum(mif2_em, start_date, end_date, 0, "total_CO2_yearly", "em2_nodiscount", by_decade = TRUE)


    # Calculate abatement cost
    fscp_df <- costs1_disc %>%
        left_join(costs2_disc, by = c("region", "decade")) %>%
        left_join(em1_total, by = c("region", "decade")) %>%
        left_join(em2_total, by = c("region", "decade")) %>%
        left_join(em1_total_nodiscount, by = c("region", "decade")) %>%
        left_join(em2_total_nodiscount, by = c("region", "decade")) %>%
        mutate(fscp = (cost2 - cost1) / (em1 - em2)) %>%
        mutate(em_diff = em1_nodiscount - em2_nodiscount)

    # Aggregate regions
    grouped_df <- imap_dfr(region_to_aggr, function(regions, new_region) {
        fscp_df %>%
            filter(region %in% regions) %>%
            summarise(
                region = new_region,
                cost1 = sum(cost1, na.rm = TRUE),
                cost2 = sum(cost2, na.rm = TRUE),
                em1 = sum(em1, na.rm = TRUE),
                em2 = sum(em2, na.rm = TRUE),
                em1_nodiscount = sum(em1_nodiscount, na.rm = TRUE),
                em2_nodiscount = sum(em2_nodiscount, na.rm = TRUE)
            ) %>%
            mutate(
                fscp = (cost2 - cost1) / (em1 - em2),
                em_diff = em1_nodiscount - em2_nodiscount
            )
    })
    new_regions_flattened <- unlist(region_to_aggr, use.names = FALSE)
    fscp_df_grouped <- fscp_df %>%
        filter(!region %in% new_regions_flattened) %>%
        bind_rows(grouped_df)

    # # Plotting
    # plot_abatement_cost <- function(df, discount_rate, start_date, end_date, scen1_name, scen2_name) {
    #     df <- df %>%
    #         select(region, fscp, em_diff) %>%
    #         arrange(region) %>%
    #         filter(em_diff > 0) %>%
    #         mutate(em_diff = em_diff / 1000)
    #     df <- df[order(df$fscp), ]
    #     df$region <- factor(df$region, levels = df$region)
    #     df$cum_em <- cumsum(df$em_diff)
    #     df$xmin <- df$cum_em - df$em_diff
    #     df$xmax <- df$cum_em
    #     ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = fscp, fill = region)) +
    #         geom_rect(color = "black") +
    #         labs(
    #             title = paste("Average abatement costs:", scen1_name, "to", scen2_name, "\n(discount rate:", discount_rate * 100, "%, dates:", start_date, "-", end_date, ")"),
    #             x = "Cumulative emission difference (GtCo2)",
    #             y = "Average abatement cost (USD/tCO2)"
    #         ) +
    #         theme_minimal() +
    #         scale_fill_manual(values = c(
    #             "CHA" = "#e41c23",
    #             "IND" = "#ff7f00",
    #             "EUR" = "#110a9a",
    #             "EUR+USA+JPN+REF" = "#4870b9",
    #             "OAS" = "#f6da23",
    #             "OASLAM" = "#f6da23",
    #             "NEU" = "#1ea6f0",
    #             "REF" = "#b2df8a",
    #             "USA" = "#33a02c", 
    #             "JPN" = "#f8f2f2",
    #             "LAM" = "#a65628",
    #             "SSA" = "#103b17",
    #             "MEA" = "#0bbda5"))
    # }

    # list(
    #     fscp_df_grouped = fscp_df_grouped,
    #     plot = plot_abatement_cost(fscp_df_grouped, discount_rate, start_date, end_date, scen1$name, scen2$name)
    # )
}