require(ggplot2)
require(tidyverse)
require(mrremind)
require(magclass)
require(waterfalls)

library(dplyr)
library(gridExtra)
library(grid)
library(purrr)
library(waterfalls)


require("quitte")
library("quitte")

# mifdata1 <- read.quitte("inputdata/mif/REMIND_generic_TransitionwLockIn-NPi.mif")

# mifdata2 <- read.quitte("inputdata/mif/REMIND_generic_FastTransition-PkBudg820.mif")

# mifdata3 <- read.quitte("inputdata/mif/REMIND_generic_TransitionwLockIn-PkBudg820.mif")


# mifdata1 <- mifdata1 %>%
#   mutate(scenario = "NPi")

# mifdata2 <- mifdata2 %>%
#   mutate(scenario = "Fast transition")

# mifdata3 <- mifdata3 %>%
#   mutate(scenario = "Extended lock-in")

calc_cumuem <- function(df, region_to_calculate) {
  df %>%
    filter(grepl("Emi|CO2|Energy|Demand|Industry|Steel|+",
                # "Emi|CO2|Energy|Demand|Industry|Steel", # if we want the breakdown of what kind of liquids etc
                 variable,
                 ignore.case = TRUE, 
                 fixed = TRUE)) %>%
    # filter(between(period, 2025, 2070),
    filter(between(period, 2025, 2080),
           region == region_to_calculate) %>%
    select(-model, -region) %>%
    #add up the different components: solid, liquids ..
    group_by(period, scenario) %>%
    summarise(total_CO2_yearly = sum(value, na.rm = TRUE),
          total_CO2_5year = total_CO2_yearly * 5) %>%
    ungroup() %>%
    group_by(scenario) %>%
    mutate(cumu_CO2 = cumsum(total_CO2_5year)) %>%
    summarise(cumu_CO2 = max(cumu_CO2))
}


calc_em <- function(df) {
  df %>%
    filter(grepl("Emi|CO2|Energy|Demand|Industry|Steel|+",
                # "Emi|CO2|Energy|Demand|Industry|Steel", # if we want the breakdown of what kind of liquids etc
                 variable,
                 ignore.case = TRUE, 
                 fixed = TRUE)) %>%
    # filter(between(period, 2025, 2070),
    filter(between(period, 2025, 2080)) %>%
    select(-model) %>%
    #add up the different components: solid, liquids ..
    group_by(period, scenario, region) %>%
    summarise(total_CO2_yearly = sum(value, na.rm = TRUE),
          total_CO2_5year = total_CO2_yearly * 5)
}

#add groups here
region_groups <- list(
  "OASLAM" = c("OAS", "LAM"),
  "EUR+NEU+USA+JPN+REF+CAZ" = c("EUR", "NEU", "USA", "JPN", "REF", "CAZ")
)

# old colors: not used for consistency with Fig 1, but probably look better
region_order = c(
  "CHA" = "#e41c23",
  "IND"="#ff7f00",
  "OAS"="#eea810",
  "OASLAM" = "#eea810",
  "SSA"="#06723e",
  "REF"="#aac90d",
  "LAM"="#c90dbc",
  "JPN"="#d4c7b1",
  "MEA"="#0ada5a",
  "USA"="#08a0e4",
  "EUR"="#0912c3",
  "EUR+NEU+USA+JPN+REF+CAZ" = "#110a9a",
  "NEU"="#c9b60d",
  "CAZ"="#08e4a0")

region_labels <- c(
  "CHA" = "China",
  "SSA" = "Sub-saharan Africa",
  "IND" = "India",
  "EUR+NEU+USA+JPN+REF+CAZ" = "USA, Europe, Russia, Canada,\nNew Zealand, Australia,\nJapan and ex-USSR countries",
  "OASLAM" = "Other Asia and Latin America",
  "MEA" = "Middle East and North Africa"
)

col_useur <- "#110a9a"
col_cha <- "#e41c23"
col_ind <- "#ff7f00"
col_ssa <- "#06723e"
col_oas <- "#eea810"
col_mea <- "#0ada5a"

fill_colors = c(
      "China" = col_cha,
      "Sub-saharan Africa" = col_ssa,
      "India" = col_ind,
      "USA, Europe, Russia, Canada,\nNew Zealand, Australia,\nJapan and ex-USSR countries" = col_useur,
      "Other Asia and Latin America" = col_oas,
      "Middle East and North Africa" = col_mea
)

plot_cascade_cumuem <- function(miflist, df_aac, show_aac_annotations = FALSE) {
  end_year <- 2070

  #map regions
  region_map <- setNames(rep(names(region_groups), lengths(region_groups)),
                        unlist(region_groups))


  # base_list <- list(mifdata1, mifdata2, mifdata3)

  mainscens_df_emi_notcumu <- miflist %>%
    map_dfr(calc_em)

  # Prepare data with total cumulative emissions and region contributions
  plot_df <- mainscens_df_emi_notcumu %>%
    filter(region != "World") %>%
    #here: fill missing timesteps to really calculate between 2025 and 2070 ( and not between 2023 and 2072)
    group_by(region, scenario) %>%
    complete(period = full_seq(period, 1)) %>%  # Fill every year
    arrange(scenario, region, period) %>%
    mutate(value = total_CO2_yearly) %>%
    fill(value, .direction = "down") %>%
    ungroup() %>%
    select(-total_CO2_yearly, -total_CO2_5year) %>%
    mutate(period = period - 2) %>% 
    filter(between(period, 2025, 2070)) %>%
    #group regions and add them up
    mutate(region = as.character(region)) %>%
    mutate(region = ifelse(region %in% names(region_map), region_map[region], region)) %>%
    group_by(region, scenario, period) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    #resume with cumulative emissions calculation
    group_by(region, scenario) %>%
    arrange(period) %>%
    mutate(cumuCO2 = cumsum(value)) %>%
    ungroup() %>%
    arrange(region, scenario, period) %>%
    filter(period == end_year) %>%
    mutate(cumuCO2 = cumuCO2 / 1e3) # Convert to Gt CO2

    # order the average abatement cost df
    df_aac <- df_aac %>%
    mutate(region = factor(region, levels = names(region_order))) %>%
    arrange(region)
    #for aac region position
    region_xpos <- c(
        "CHA" = 0,
        "IND" = 1,
        "OASLAM" = 2,
        "SSA" = 3,
        "MEA" = 4,
        "EUR+NEU+USA+JPN+REF+CAZ" = 5
        )
    # Now assign x positions for annotation (has to be done manually)
    df_aac <- df_aac %>%
          mutate(
            x_pos = case_when(
            scenario1 == "NPi" & scenario2 == "Transition with lock-in" ~ region_xpos[as.character(region)] +2,
            scenario1 == "Transition with lock-in" & scenario2 == "Fast transition" ~ region_xpos[as.character(region)] + 9
            )
        ) %>%
    ungroup() %>%
    mutate(region_label = case_when(
        scenario1 == "Transition with lock-in" & scenario2 == "Fast transition" ~ paste0(region, "_2"),
        TRUE ~ as.character(region)
        ))

  #get region colors from list above
  region_colors <- c(
    "NPi" = "grey", "Transition with lock-in" = "grey", "Fast transition" = "grey",
    region_order,
    setNames(region_order, paste0(names(region_order), "_2"))
  )

  plot_df$region <- factor(plot_df$region, levels = names(region_order))
  plot_df$scenario <- factor(plot_df$scenario, levels = c("NPi", "Transition with lock-in", "Fast transition"))

  # Get total emissions by scenario
  total_df <- plot_df %>%
    group_by(scenario) %>%
    summarise(total = sum(cumuCO2), .groups = "drop")

  # Compute differences between scenarios by region for the cascade
  cascade_df <- plot_df %>%
    select(region, scenario, cumuCO2) %>%
    mutate(region = factor(region, levels = names(region_order))) %>%
    arrange(region, scenario) %>%
    group_by(region) %>%
    mutate(diff = cumuCO2 - lag(cumuCO2, default = first(cumuCO2))) %>%
    ungroup()

  # Build waterfall data: baseline total, region contributions, final totals
  waterfall_data <- bind_rows(
    tibble(label = "NPi", value = total_df$total[total_df$scenario == "NPi"], type = "total"),
    cascade_df %>% filter(scenario == "Transition with lock-in") %>% mutate(label = region, value = diff, type = "region") %>% select(label, value, type),
    # tibble(label = "Transition with lock-in", value = total_df$total[total_df$scenario == "Transition with lock-in"], type = "total"),
    tibble(label = "Transition with lock-in", value = FALSE, type = "total"),
    cascade_df %>% filter(scenario == "Fast transition") %>%
      mutate(label = paste0(region, "_2"), value = diff, type = "region") %>%
      select(label, value, type),
    # tibble(label = "Fast transition", value = total_df$total[total_df$scenario == "Fast transition"], type = "total")
    tibble(label = "Fast transition", value = 0, type = "total")
  )

  waterfall_data <- waterfall_data %>%
    mutate(color = region_colors[label]) %>%
    #remove useless "0" for the scenario totals
    mutate(label2 = ifelse(type == "total", "", paste0(round(value, 0)))) %>%
    # remove 0s
    mutate(label2 = ifelse(label2 == "0", "", label2)) %>%
    #remove "_2" with hacky extra space:
    # mutate(label = gsub("_2", " ", label)) %>%
    select(label, value, type, label2, color) %>%
    mutate(label = factor(label, levels = names(region_colors)))

  # custom x ticks
  labels_vec <- as.character(waterfall_data$label)
  labels_vec <- ifelse(labels_vec %in% c("NPi", "Transition with lock-in", "Fast transition"), labels_vec, " ")

  # Create waterfall chart using ggwaterfall
  wf_plot <- waterfall(waterfall_data, 
            rect_text_labels = waterfall_data$label2,
            rect_text_size = 2.5,
            put_rect_text_outside_when_value_below = 20,
            fill_by_sign = FALSE, fill_colours = waterfall_data$color) +
    # scale_x_discrete(labels = label_fun)+
    labs(
      title = "Breakdown of cumulative CO2 Emissions from steel production in 2070\n(2025 to 2070)",
      x = "Scenario",
      y = "Cumulative emissions (Gt CO2)",
      fill = "Region"
    ) +
    theme_bw(base_size = 25)

  plot_df <- plot_df %>%
    filter(period == end_year) %>%
    #need to specify xpos for the cumulative emissions bars (stacked)
    mutate(
      x_pos = ifelse(
        scenario == "NPi",
        1,
        ifelse(scenario == "Transition with lock-in", 8, 15)))

  # manually calculate the y positions for the aac annotations
  df_aac <- df_aac %>%
    left_join(plot_df %>% select(region, scenario, cumuCO2), 
              by = c("region" = "region", "scenario1" = "scenario")) %>%
    rename(cumuCO2_0 = cumuCO2) %>%
    left_join(plot_df %>% select(region, scenario, cumuCO2), 
              by = c("region" = "region", "scenario2" = "scenario")) %>%
    rename(cumuCO2_1 = cumuCO2) %>%
    mutate(diff_CO2 = cumuCO2_1 - cumuCO2_0) %>%
    select(-cumuCO2_0, -cumuCO2_1) %>%
    arrange(x_pos) %>%
    mutate(diffCO2_previous = lag(diff_CO2, default = 0)) %>%
    mutate(diffCO2_cumu = cumsum(diffCO2_previous)) %>%
    mutate(y_pos = 120 + diffCO2_cumu) %>%
    select(-diffCO2_previous, -diffCO2_cumu)


  plot_df <- plot_df %>%
    mutate(region = recode(region, !!!region_labels))

  # complete the plot with the stacked bars and the aac annotations
  wf_plot <- wf_plot +
    geom_bar(data = plot_df,
            aes(x = x_pos, y = cumuCO2, fill = region),
            width = 0.7,position="stack", stat="identity", alpha = 1, color = "black") +
    scale_fill_manual(
      values =fill_colors,
      breaks = names(fill_colors)) +
    scale_x_discrete(labels = labels_vec) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, color = "#dbd8d8"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      legend.title.position = "top",
      legend.title = element_text(hjust = 0.5),
      legend.key.width = unit(1, "cm"),
      legend.key.height = unit(1, "cm"))

  if(show_aac_annotations){
    wf_plot <- wf_plot +
    # add the average abatement cost labels
    geom_text(
        data = df_aac,
        aes(x = x_pos, y = y_pos, label = paste0("$", round(fscp, 0), "\n/tCO2")),
        color = "black", size = 5, fontface = "bold", inherit.aes = FALSE
    )
  }

  ggsave("figs/cascade_cumu_emi_scen.png", wf_plot, width = 12, height = 8, dpi = 300)
  
  return(wf_plot)


 }

plot_cascade_yearlyem <- function(miflist, year_to_plot= 2050) {

  #get region colors from list above
  region_colors <- c(
    "NPi" = "grey", "Transition with lock-in" = "grey", "Fast transition" = "grey",
    region_order,
    setNames(region_order, paste0(names(region_order), "_2"))
  )

  mainscens_df_emi_notcumu <- miflist %>%
    map_dfr(calc_em)

  # Prepare data with total cumulative emissions and region contributions
  plot_df <- mainscens_df_emi_notcumu %>%
    filter(region != "World") %>%
    select(period, scenario, region, total_CO2_yearly) %>%
    filter(period == year_to_plot)

  #group according to region groups
  plot_df <- plot_df %>%
    mutate(region = as.character(region)) %>%
    mutate(region = ifelse(region %in% names(region_map), region_map[region], region)) %>%
    group_by(region, scenario) %>%
    summarise(total_CO2_yearly = sum(total_CO2_yearly), .groups = "drop") %>%
    mutate(region = factor(region, levels = names(region_order)))


  plot_df$region <- factor(plot_df$region, levels = names(region_order))
  plot_df$scenario <- factor(plot_df$scenario, levels = c("NPi", "Transition with lock-in", "Fast transition"))

  # Get total emissions by scenario
  total_df <- plot_df %>%
    group_by(scenario) %>%
    summarise(total = sum(total_CO2_yearly), .groups = "drop")

  # Compute differences between scenarios by region for the cascade
  cascade_df <- plot_df %>%
    select(region, scenario, total_CO2_yearly) %>%
    mutate(region = factor(region, levels = names(region_order))) %>%
    arrange(region, scenario) %>%
    group_by(region) %>%
    mutate(diff = total_CO2_yearly - lag(total_CO2_yearly, default = first(total_CO2_yearly))) %>%
    ungroup()

  # Build waterfall data: baseline total, region contributions, final totals
  waterfall_data <- bind_rows(
    tibble(label = "NPi", value = total_df$total[total_df$scenario == "NPi"], type = "total"),
    cascade_df %>% filter(scenario == "Transition with lock-in") %>% mutate(label = region, value = diff, type = "region") %>% select(label, value, type),
    tibble(label = "Transition with lock-in", value = FALSE, type = "total"),
    cascade_df %>% filter(scenario == "Fast transition") %>%
      mutate(label = paste0(region, "_2"), value = diff, type = "region") %>%
      select(label, value, type),
    # tibble(label = "Fast transition", value = total_df$total[total_df$scenario == "Fast transition"], type = "total")
    tibble(label = "Fast transition", value = 0, type = "total")
  )

  waterfall_data <- waterfall_data %>%
    mutate(color = region_colors[label]) %>%
    #remove useless "0" for the scenario totals
    mutate(label2 = ifelse(type == "total", "", paste0(round(value, 0)))) %>%
    select(label, value, type, label2, color) %>%
    mutate(label = factor(label, levels = names(region_colors)))

  # custom x ticks
  labels_vec <- as.character(waterfall_data$label)
  labels_vec <- ifelse(labels_vec %in% c("NPi", "Transition with lock-in", "Fast transition"), labels_vec, " ")


  # Create waterfall chart using ggwaterfall
  wf_plot <- waterfall(waterfall_data, 
            rect_text_labels =waterfall_data$label2,
            fill_by_sign = FALSE, fill_colours = waterfall_data$color) +
    # scale_x_discrete(labels = label_fun)+
    labs(
      title = paste0("Breakdown of yearly CO2 Emissions from steel production in ", year_to_plot),
      x = "Scenario",
      y = "Yearly emissions (Mt CO2/yr)",
      fill = "Region"
    ) +
    theme_bw(base_size = 14)

  plot_df <- plot_df %>%
    # filter(period == 2050) %>%
    mutate(x_pos = ifelse(scenario == "NPi", 1, ifelse(scenario == "Transition with lock-in", 8, 15))) #%>%

  wf_plot <- wf_plot +
    geom_bar(data = plot_df,
            aes(x = x_pos, y = total_CO2_yearly, fill = region), 
            width = 0.7,position="stack", stat="identity", alpha = 0.8, color = "black") +
    scale_fill_manual(values =region_colors) +
    scale_x_discrete(labels = labels_vec) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, color = "#dbd8d8"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank())
  print(wf_plot)
  ggsave(paste0("figs/cascade_yearly_emi_",year_to_plot,"_scen.png"), width = 12, height = 8, dpi = 300)
}

