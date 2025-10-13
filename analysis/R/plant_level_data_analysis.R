library(tidyverse)
library(ggplot2)
library(grid)
library(quitte)
require(gridExtra)


raw_data <- read.csv("./inputdata/gem/global_BOFcap.csv")

raw_age_data <- read.csv("./inputdata/gem/age_distribution.csv")

raw_age_data_region <- read.csv("./inputdata/gem/age_distribution_regions.csv")

raw_regional_data <- read.csv("./inputdata/gem/BOF_capacity_perREMINDregion.csv")

df_production_bof <- read.csv("./inputdata/worldsteel/bof_production.csv")
df_production_other <- read.csv("./inputdata/worldsteel/other_production.csv")
df_production_eaf <- read.csv("./inputdata/worldsteel/eaf_production.csv")


clean_column_names <- function(name) {
  name <- gsub("\\.+", "_", name)
  name <- sub("_$", "", name)
}

#### CLEAN UP CAPACITY DATA ####

data <- raw_data %>% 
  rename_all(clean_column_names) %>%
  select(-Region, -X5year_BOF_capacity_additions)

# Calculate Total net existing bof capacity: historic (old) cap additions- retirement of historic plants
# additional "Total_net_BOF_capacity_no_retirement_date" is capacity for which no dates are known (no start date, no announcement date). 
# for there, we simulate a linear decrease in net capacity instead.
data <- data %>%
  mutate(Total_retired_BOF_capacity_old = Total_retired_BOF_capacity - Total_retired_BOF_capacity_new_projects) %>%
  mutate(Total_BOF_capacity_additions_old = Total_BOF_capacity_additions - Total_BOF_capacity_additions_new_projects ) %>%
  mutate(Total_net_BOF_capacity_no_retirement_date = Total_BOF_capacity_additions_no_retirement_date - Total_retired_BOF_capacity_no_retirement_date) %>%
  mutate(Total_net_existing_BOF_capacity = Total_BOF_capacity_additions_old - Total_retired_BOF_capacity_old + Total_net_BOF_capacity_no_retirement_date )
  

# net BOF capacity additions (for new plants) if plants have one relining (lifetime of 2x relining)
# simply a complement to derive the fraction of these new plants that are under construction
data <- data %>%
  mutate(Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction = Total_net_BOF_capacity_additions_new_projects_1_relining - Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only)


# add additional capacity from a 2nd relining, as additional capacity relative to 1 relining
# add the new projects additional capacity, and then the existing capacity
data <- data %>% 
  mutate(Additional_capacity_2_relining_new_projects = Total_net_BOF_capacity_additions_new_projects_2_relining - Total_net_BOF_capacity_additions_new_projects_1_relining,
        Additional_capacity_2_relining_operating = Additional_net_BOF_capacity_operating_2_relining - Additional_net_BOF_capacity_operating_1_relining)

data_long <- data %>%
    pivot_longer(cols = -index, names_to = "Category", values_to = "Capacity")

data_NetBOFcap <- data %>%
    select("Remaining_BOF_capacity", "index") %>%
    pivot_longer(cols = -index, names_to = "Category", values_to = "Capacity")

#convert to Mtpa (/ 1000)
data_NetBOFcap$Capacity <- data_NetBOFcap$Capacity / 1000
data_long$Capacity <- data_long$Capacity / 1000


#### CLEAN UP AGE DATA ####

age_data <- raw_age_data %>%
  rename_all(clean_column_names) %>%
  mutate(Total_BOF_capacity = Total_BOF_capacity / 1000)

age_data_region <- raw_age_data_region %>%
  rename_all(clean_column_names) %>%
  mutate(Total_BOF_capacity = Total_BOF_capacity / 1000)

regional_data <- raw_regional_data %>%
  rename_all(clean_column_names) %>%
  select(Year, Region, Remaining_BOF_capacity, Total_BOF_capacity_additions, Total_BOF_capacity_additions_no_retirement_date, Yearly_BOF_Capacity_additions)

#### DEFINE MAIN PLOTTING PARAMETERS (REGIONAL PLOTS) ####

region_labels <- c(
  "CHA" = "China",
  "SSA" = "Sub-saharan Africa",
  "IND" = "India",
  "USAEUR" = "Global North",
  "OASLAM" = "Other Asia and Latin America",
  "MEA" = "Middle East and North Africa"
)

col_useur <- "#1965b0"
col_cha <- "#dc050c"
col_ind <- "#f1932d"
col_ssa <- "#4eb265"
col_oas <- "#f7f056"
col_mea <- "#ae76a3"

fill_colors = c(
      "China" = col_cha,
      "Sub-saharan Africa" = col_ssa,
      "India" = col_ind,
      "Global North" = col_useur,
      "Other Asia and Latin America" = col_oas,
      "Middle East and North Africa" = col_mea
)


#### CALCULATE YEARLY AND CUMULATIVE EMISSIONS FROM CAPACITY DATA ####
#### key assumptions: capacity factor of 0.8, emissions factor of 2.3 tCO2/t steel for old plants, 2 tCO2/t steel for new plants (BAT)

data_long_filtered_em <- data_long %>%
  filter(Category %in% c(
          "Total_net_existing_BOF_capacity",
          "Additional_net_BOF_capacity_operating_1_relining",
          "Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only",
          "Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction",
          "Additional_capacity_2_relining_new_projects",
          "Additional_capacity_2_relining_operating",
          "index")) %>%
  filter(index >= 2020 & index <2081) %>%
  #new additions are BAT = 2 tCO2/t steel
  mutate(Emissions = ifelse(grepl("new_projects", Category), Capacity * 0.8 * 2, Capacity * 0.8 * 2.3)) %>%
  select(-Capacity) %>%
  pivot_wider(names_from = Category, values_from = Emissions) %>%
  #after emissions have been calculated according to new or old plants, bring together the additional capacity
  # from the second relining in one column
  mutate(Additional_capacity_2_relining = Additional_capacity_2_relining_new_projects + Additional_capacity_2_relining_operating) %>% 
  select(- Additional_capacity_2_relining_new_projects, -Additional_capacity_2_relining_operating) %>%
  pivot_longer(cols = -index, names_to = "Category", values_to = "Emissions")

#take wide format to calculate cumulative caps
data_wide_em <- data_long_filtered_em %>%
  filter(index >= 2025 & index <2081) %>%
  pivot_wider(names_from = Category, values_from = Emissions) 

#clean df
data_wide_em[is.na(data_wide_em)] <- 0

#calculate cumulative emissions
data_wide_em_cumu <- data_wide_em %>%
  mutate(Cumu_oldBOF = cumsum(Total_net_existing_BOF_capacity)) %>%
  mutate(Cumu_oldBOF_1relining = cumsum(Total_net_existing_BOF_capacity + 
                                     Additional_net_BOF_capacity_operating_1_relining)) %>%
  mutate(Cumu_oldBOF_underconstruction = cumsum(Total_net_existing_BOF_capacity +
                                            Additional_net_BOF_capacity_operating_1_relining +
                                            Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction)) %>%
  mutate(Cumu_oldBOF_andnew = cumsum(Total_net_existing_BOF_capacity +
                                              Additional_net_BOF_capacity_operating_1_relining +
                                              Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only +
                                              Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction)) %>%
  mutate(Cumu_oldBOF_new_2relining = cumsum(Total_net_existing_BOF_capacity +
                                              Additional_net_BOF_capacity_operating_1_relining +
                                              Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only +
                                              Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction +
                                              Additional_capacity_2_relining)) %>%
  select(index, Cumu_oldBOF,Cumu_oldBOF_1relining,Cumu_oldBOF_underconstruction, Cumu_oldBOF_andnew, Cumu_oldBOF_new_2relining)

# annoyingly, this needs to be in long format to work
data_long_em_cumu <- data_wide_em_cumu %>%
  pivot_longer(cols = -index, names_to = "Category", values_to = "Emissions")

data_long_em_cumu$Category <- factor(data_long_em_cumu$Category, 
                            levels = c(
                              "Cumu_oldBOF_new_2relining",
                              "Cumu_oldBOF_andnew",
                              "Cumu_oldBOF_underconstruction",
                              "Cumu_oldBOF_1relining",
                              "Cumu_oldBOF"))

#### PLOTTING FUNCTIONS ####

plot_BOF_capacity <- function(save_plot=FALSE){  
  data_long_filtered <- data_long %>%
    filter(index >= 2020 & index <2075) %>%
    filter(Category %in% c("Total_net_existing_BOF_capacity",
            "Additional_net_BOF_capacity_operating_1_relining",
            "Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction",
            "Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only",
            "index"))


  data_long_filtered$Category <- factor(data_long_filtered$Category, 
                              levels = c("Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction",
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only",
                                "Additional_net_BOF_capacity_operating_1_relining",
                                "Total_net_existing_BOF_capacity"))


  p <- ggplot(data_long_filtered, aes(x = factor(index), y = Capacity, fill = Category)) +
    geom_bar(stat = "identity", data = data_long_filtered) +
    scale_fill_manual(values = c("Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction" = "#e41c7d",
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only" = "#b11d36",
                                "Additional_net_BOF_capacity_operating_1_relining" = "grey",
                                "Total_net_existing_BOF_capacity" = "#454444"),
                      labels = c("Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction" = "New BF-BOF project (under construction)",
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only" = "New BF-BOF project (announced)",
                                "Additional_net_BOF_capacity_operating_1_relining" = "Additional operating BF-BOF capacity\nif a relining is carried out",
                                "Total_net_existing_BOF_capacity" = "Operating BF-BOF capacity"))+
    labs(x = "Time period", y = "Global BF-BOF steel-making capacity (Mtpa)", fill = "Bar Legend", color = "Line Legend") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_x_discrete(breaks = seq(2000, 2070, by = 5)) +
    theme(legend.position = "right")
  # print(p)
  if(save_plot){
    ggsave("figs/BOF_capacity_plot.png", width = 10, height = 6, dpi = 300)
  }
}

plot_emissions_1and2relining <- function(save_plot=FALSE){
  x_breaks <- seq(2020, 2085, by = 5)

  data_to_plot <- data_long_filtered_em

  data_cumu_toplot <- data_long_em_cumu %>%
    filter(Category %in% c("Cumu_oldBOF_new_2relining",
                          "Cumu_oldBOF_andnew",
                          "Cumu_oldBOF_1relining",
                          "Cumu_oldBOF"))


  data_to_plot$Category <- factor(data_to_plot$Category, 
                              levels = c(
                                "Additional_capacity_2_relining",
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only",
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction",  
                                "Additional_net_BOF_capacity_operating_1_relining",
                                "Total_net_existing_BOF_capacity"))

  p <- ggplot(data_to_plot, aes(x = factor(index), y = Emissions, fill = Category)) +
    geom_bar(stat = "identity", data = data_to_plot) +
    scale_fill_manual(values = c(
                                "Additional_capacity_2_relining" = "#2d3195",
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction" = "#e41c7d",
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only" = "#b11d36",
                                "Additional_net_BOF_capacity_operating_1_relining" = "grey",
                                "Total_net_existing_BOF_capacity" = "#454444"
                                ),
                      labels = c(
                              "Additional_capacity_2_relining" = "Operating and new BF-BOFs assuming\ntwo relinings (lifetime of 45 years)",
                              "Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction" = "New BF-BOF project\n(under construction)",
                              "Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only" = "New BF-BOF project\n(announced)",
                              "Additional_net_BOF_capacity_operating_1_relining" = "Operating BF-BOF capacity, assuming\none BF relining (lifetime of 35 years)",
                              "Total_net_existing_BOF_capacity" = "Operating BF-BOF capacity, assuming\nplants undergo no BF relinings\nafter 2025"))+
    labs(x = "Time period", y = "Global yearly emissions from BF-BOF steel-making (MtCO2/yr)", fill = "Bar Legend", color = "Line Legend")

  scale_factor <- 27

  p <- p + 
    geom_line(data = data_cumu_toplot, aes(x = factor(index), y = Emissions / scale_factor, group = Category, color = Category), linewidth = 1.3) +#, color = "black", linetype = "dashed") #+
    scale_color_manual(
      values = c(
        "Cumu_oldBOF_new_2relining"= "#2d3195",
        "Cumu_oldBOF_1relining" = "grey",
        "Cumu_oldBOF_andnew" = "#b11d36",
        "Cumu_oldBOF" = "#454444"
      ),
      labels = c(
        "Cumu_oldBOF_new_2relining"="Cumulative CO2 emissions from 2025,\nfrom operating and new BF-BOFs, if two\n relinings take place",
        "Cumu_oldBOF_1relining" = "Cumulative CO2 emissions from 2025,\nfrom operating BF-BOFs, if one\nBF relining takes place",
        "Cumu_oldBOF_andnew" = "Cumulative CO2 emissions from 2025,\nfrom operating, under construction and\nannounced BF-BOFs, if one BF relining\ntakes place",
        "Cumu_oldBOF" = "Cumulative CO2 emissions from 2025,\nfrom operating BF-BOFs, if no new\nrelinings take place after 2025"
      )
    ) +
      scale_y_continuous( limits = c(0, 3100),
      name = "Global yearly emissions from BF-BOF steel-making\n(MtCO2/yr), barplot",
      sec.axis = sec_axis(~ . * scale_factor, name = "Cumulative CO2 Emissions from 2025,\nfrom BF-BOF steel-making (MtCO2, line plots)")
    ) +
    labs(caption = "*BF-BOF is used as a general term; however, a small subset of included BOF facilities operate\nwithout a BF or rely on alternative iron production methods."
          ) +
    # labs(color = "Line Legend") +
    scale_x_discrete(breaks = x_breaks) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
      text = element_text(size = 12),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 10),

      legend.key.height = unit(1.7, "cm"),
      legend.key.width = unit(0.7, "cm"),
      legend.spacing.y = unit(1.6, "cm"),
      legend.margin = margin(8, 8, 8, 8)) +
    guides(
      fill = guide_legend(
        byrow = TRUE,
        title = "Bar Legend",
        keyheight = unit(1.4, "cm"),
        ),
      group = guide_legend(
        byrow = TRUE,
        title = "Line Legend",
        keyheight = unit(2, "cm"),
        )
      )
  #add annotations and arrows
  p <- p +
    geom_segment(aes(
      x = "2079", xend = "2079",
      y = max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF"]) / scale_factor, 
      yend = max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_1relining"]) / scale_factor
    ), arrow = arrow(length = unit(0.4, "cm")), color = "black") +
    
    geom_segment(aes(
      x = "2079", xend = "2079",
      y = max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_1relining"]) / scale_factor, 
      yend = max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_andnew"]) / scale_factor
    ), arrow = arrow(length = unit(0.4, "cm")), color = "black") +

    geom_segment(aes(
      x = "2079", xend = "2079",
      y = max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_andnew"]) / scale_factor, 
      yend = max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_new_2relining"]) / scale_factor
    ), arrow = arrow(length = unit(0.4, "cm")), color = "black") +
    
    annotate("text", x = "2079", y = (max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_1relining"]) + 
                                  max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF"])) / (2 * scale_factor), 
            label = "Effect of one\nBF relining\nleading to a\nplant lifetime\nof 35 years", hjust = 1.2,vjust = 0.4,
              lineheight = 0.9, size = 3.2) +
    
    annotate("text", x = "2079", y = (max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_1relining"]) + 
                                  max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_andnew"])) / (2 * scale_factor), 
            label = "Add planned\nand under\nconstruction\nBF-BOFs", hjust = 1.2,vjust = 0.4,
              lineheight = 0.9, size = 3.2) +
    
    annotate("text", x = "2079", y = (max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_andnew"]) + 
                                  max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_new_2relining"])) / (2 * scale_factor), 
            label = "Effect of two\nBF relinings\n(plant lifetime\nof 45 years)", hjust = 1.2,vjust = 0.4,
              lineheight = 0.9, size = 3.2)
  print(p)
  
  if(save_plot){
    ggsave("figs/BOF_emissions_plot_1_and_2_relinings.png", width = 11, height = 8, dpi = 300)
  }
}

plot_emissions_1relining <- function(save_plot=FALSE){
  x_breaks <- seq(2020, 2075, by = 5)

  data_long_toplot <- data_long_filtered_em %>%
    filter(Category %in% c(
            "Total_net_existing_BOF_capacity",
            "Additional_net_BOF_capacity_operating_1_relining",
            "Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only",
            "Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction",
            "index")) %>%
    filter(index >= 2020 & index <2071)

  data_cumu_toplot <- data_long_em_cumu %>%
    filter(Category %in% c(
                          "Cumu_oldBOF_andnew",
                          "Cumu_oldBOF_underconstruction",
                          "Cumu_oldBOF_1relining",
                          "Cumu_oldBOF")) %>%
    filter(index >= 2025 & index <2071)

  data_long_toplot$Category <- factor(data_long_toplot$Category, 
                              levels = c(
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only",
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction",  
                                "Additional_net_BOF_capacity_operating_1_relining",
                                "Total_net_existing_BOF_capacity"))

  p <- ggplot(data_long_toplot, aes(x = factor(index), y = Emissions, fill = Category)) +
    geom_bar(stat = "identity", data = data_long_toplot) +
    scale_fill_manual(values = c(
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction" = "#e41c7d",
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only" = "#b11d36",
                                "Additional_net_BOF_capacity_operating_1_relining" = "grey",
                                "Total_net_existing_BOF_capacity" = "#454444"
                                ),
                      labels = c(
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_under_construction" = "New BF-BOF project\n(under construction)",
                                "Total_net_BOF_capacity_additions_new_projects_1_relining_announced_only" = "New BF-BOF project\n(announced)",
                                "Additional_net_BOF_capacity_operating_1_relining" = "Operating BF-BOF capacity, assuming\none BF relining (lifetime of 35 years)",
                                "Total_net_existing_BOF_capacity" = "Operating BF-BOF capacity, assuming\nplants undergo no BF relinings\nafter 2025"))+
    labs(x = "Time period", y = "Global yearly emissions from BF-BOF steel-making (MtCO2/yr)", fill = "Bar Legend", color = "Line Legend")

  scale_factor <- 22

  p <- p + 
    geom_line(data = data_cumu_toplot, aes(x = factor(index), y = Emissions / scale_factor, group = Category, color = Category), linewidth = 1.3) +#, color = "black", linetype = "dashed") #+
    scale_color_manual(
      values = c(
        "Cumu_oldBOF_1relining" = "grey",
        "Cumu_oldBOF_andnew" = "#b11d36",
        "Cumu_oldBOF_underconstruction" = "#e41c7d",
        "Cumu_oldBOF" = "#454444"
      ),
      labels = c(
        "Cumu_oldBOF_underconstruction"="Cumulative CO2 emissions from 2025,\nfrom operating and under construction\nBF-BOFs, if one relining takes place",
        "Cumu_oldBOF_1relining" = "Cumulative CO2 emissions from 2025,\nfrom operating BF-BOFs, if one\nBF relining takes place",
        "Cumu_oldBOF_andnew" = "Cumulative CO2 emissions from 2025,\nfrom operating, under construction and\nannounced BF-BOFs, if one BF relining\ntakes place",
        "Cumu_oldBOF" = "Cumulative CO2 emissions from 2025,\nfrom operating BF-BOFs, if no new\nrelinings take place after 2025"
      )
    ) +
      scale_y_continuous( limits = c(0, 3100),
      name = "Global yearly emissions from BF-BOF steel-making\n(MtCO2/yr), barplot",
      sec.axis = sec_axis(~ . * scale_factor, name = "Cumulative CO2 Emissions from 2025,\nfrom BF-BOF steel-making (MtCO2, line plots)")
    ) +
    labs(caption = "*BF-BOF is used as a general term; however, a small subset of included BOF facilities operate\nwithout a BF or rely on alternative iron production methods."
          ) +
    # labs(color = "Line Legend") +
    scale_x_discrete(breaks = x_breaks) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
      text = element_text(size = 12),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 10),

      legend.key.height = unit(1.7, "cm"),
      legend.key.width = unit(0.7, "cm"),
      legend.spacing.y = unit(1.6, "cm"),
      legend.margin = margin(8, 8, 8, 8)) +
    guides(
      fill = guide_legend(
        byrow = TRUE,
        title = "Bar Legend",
        keyheight = unit(1.4, "cm"),
        ),
      group = guide_legend(
        byrow = TRUE,
        title = "Line Legend",
        keyheight = unit(2, "cm"),
        )
      )
  #add annotations and arrows
  p <- p +
    geom_segment(aes(
      x = "2069", xend = "2069",
      y = max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF"]) / scale_factor, 
      yend = max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_1relining"]) / scale_factor
    ), arrow = arrow(length = unit(0.4, "cm")), color = "black") +
    
    geom_segment(aes(
      x = "2069", xend = "2069",
      y = max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_1relining"]) / scale_factor, 
      yend = max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_underconstruction"]) / scale_factor
    ), arrow = arrow(length = unit(0.4, "cm")), color = "black") +

    geom_segment(aes(
      x = "2069", xend = "2069",
      y = max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_underconstruction"]) / scale_factor, 
      yend = max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_andnew"]) / scale_factor
    ), arrow = arrow(length = unit(0.4, "cm")), color = "black") +
    
    annotate("text", x = "2069", y = (max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_1relining"]) + 
                                  max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF"])) / (2 * scale_factor), 
            label = "Reline existing\nBFs (plant lifetime\nof 35 years)", hjust = 1.2,vjust = 0.4,
              lineheight = 0.9, size = 3.2) +
    
    annotate("text", x = "2069", y = (max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_1relining"]) + 
                                  max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_underconstruction"])) / (2 * scale_factor), 
            label = "Add BF-BOFs\nunder construction", hjust = 1.2,vjust = 0.4,
              lineheight = 0.9, size = 3.2) +
    
    annotate("text", x = "2069", y = (max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_underconstruction"]) + 
                                  max(data_cumu_toplot$Emissions[data_cumu_toplot$Category == "Cumu_oldBOF_andnew"])) / (2 * scale_factor), 
            label = "Add planned\nBF-BOFs", hjust = 1.2,vjust = 0.4,
              lineheight = 0.9, size = 3.2) 
            
  print(p)

  if(save_plot){
    ggsave("figs/BOF_emissions_plot_1relining.png", width = 11, height = 8, dpi = 300)
  }
}

plot_regional_production <- function(save_plot=FALSE){
  regions_to_filter <- c("USA", "EUR", "CHA", "IND", "OAS", "SSA", "REF", "NEU", "JPN")

  df_production_regions <- df_production_bof %>%
    mutate(type = "bof") %>%
    bind_rows(df_production_eaf %>%
                mutate(type = "eaf") %>%
                filter(Region=="IND")
                ) %>%
    bind_rows(df_production_other %>%
                mutate(type = "other")) %>%
    rename_all(clean_column_names) %>%
    mutate(value = value / 1e6) #convert from t to Mt

  #load projected production
  projected_production <- read.csv("inputdata/other/primary_steel_prod_default.csv") %>%
    select(region, period, value) %>%
    filter(period > 2025, period <= 2100) %>% #production data goes up to 2022
    mutate(type = "bof") %>%
    rename(Region = region, Historic_Time = period)


  df_production_regions <- df_production_regions %>%
    bind_rows(projected_production)

  df_plot_production <- df_production_regions %>%
    pivot_wider(names_from = Region, values_from = value) %>%
    mutate(USAEUR = USA + EUR + REF + NEU + JPN + CAZ) %>%
    mutate(OASLAM = OAS + LAM) %>%
    select(-OAS, -USA, -EUR, -REF, -NEU, -JPN, -CAZ, -LAM, -MEA) %>%
    pivot_longer(cols = c(CHA, USAEUR, SSA, IND, OASLAM)) %>%
    mutate(Region = recode(name, !!!region_labels)) %>%
    mutate(alpha = ifelse(type == "bof", 0.7, ifelse(type == "eaf", 0.5,0.3))) %>%
    mutate(Region = factor(Region, levels = c("First wave:","Global North",
                                              "China","India",
                                              "Other Asia and Latin America",
                                              "Sub-saharan Africa"
                                              ))) %>%
    filter(Historic_Time < 2051) %>%
    #sum up different production routes (eaf/bof etc)
    group_by(Historic_Time, Region, name) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()

  p <- ggplot(data = df_plot_production,
              aes(x = Historic_Time, y = value, fill = Region)) +
    geom_area(alpha = 1) +
    scale_fill_manual(
      values = fill_colors,
      breaks = names(fill_colors),
      guide = guide_legend(
        # override.aes = list(
        #   fill = c(NA, # for "First wave:" no fill
        #     col_useur, col_cha, col_ind, col_oas, col_ssa
        #   ),
        #   alpha = c(0, 0.7, 0.7, 0.7, 0.7, 0.7)
        # )
      )) +
    # geom_rect(aes(xmin = 1900, xmax = 2022, ymin = 1, ymax = Inf), fill = "#6a6a6951", alpha = 0.15) +
    annotate("rect", xmin = 1900, xmax = 2022, ymin = 1, ymax = Inf, fill = "#6a6a6951", alpha = 0.15) +
    scale_x_continuous(breaks = seq(1900, 2050, by = 25)) +
    theme_bw(base_size = 8) +
    labs(title = "Regional historical primary steel production\n",
        # x = "Year",
        y = "Primary steel production (Mt/yr)",
        colour = "Region") +
    theme(
      # plot.title = element_text(hjust = 0.5, size = 11),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      axis.title.x = element_blank()
    )
    
  p <- p +  
    #add segment and annotation
    #US/EUR
    geom_segment(aes(
      x = 1950, xend = 1958, y = 250, yend = 250), 
      arrow = arrow(length = unit(0.2, "cm")), 
      color = col_useur, show.legend = FALSE) +
    annotate("rect", 
            xmin= 1904, xmax=1950,
            ymin=200, ymax=370,
            alpha=0.4, color = col_useur, fill = "white") +
    annotate("text", x = 1940,
        y = 250,
        label ="1st wave:\nGlobal North, post WW2",
          hjust = 0.8, vjust = 0.1,
          lineheight = 0.9, size = 2.1) +
    #CHA
    geom_segment(aes(
      x = 1995, xend = 2010, y = 670, yend = 670), 
      arrow = arrow(length = unit(0.2, "cm")), 
      color = col_cha, show.legend = FALSE) +
    annotate("rect",
            xmin= 1958, xmax=1995,
            ymin=600, ymax=740,
            alpha=0.4, color = col_cha, fill = "white") +
    annotate("text", x = 1990,
        y = 590,
        label ="2nd wave:\nChina, 2000s",
          hjust = 0.9, vjust = -0.3,
          lineheight = 0.9, size = 2.1) +
    #IND
    geom_segment(aes(
      x = 2005, xend = 2030, y = 1100, yend = 420), 
      arrow = arrow(length = unit(0.2, "cm")), 
      color = col_ind, show.legend = FALSE) +
    annotate("rect", 
            xmin= 1957, xmax=2005,
            ymin=970, ymax=1210,
            alpha=0.4, color = col_ind, fill = "white") +
    annotate("text", x = 2000,
        y = 970,
        label ="3rd wave:\nIndia, other Asia,\nLatin America, from today",
          hjust = 0.9, vjust = -0.2,
          lineheight = 0.9, size = 2.1) +
    #SSA
    geom_segment(aes(
      x = 2007, xend = 2049, y = 1300, yend = 100), 
      arrow = arrow(length = unit(0.2, "cm")), 
      color = col_ssa, show.legend = FALSE) +
    annotate("rect", 
            xmin= 1962, xmax=2007,
            ymin=1240, ymax=1380,
            alpha=0.4, color = col_ssa, fill = "white") +
    annotate("text", x = 2002,
        y = 1250,
        label ="4th wave:\nSSA, near future",
          hjust = 0.9, vjust = -0.1,
          lineheight = 0.9, size = 2.1) +
    geom_vline(xintercept = 2022, linetype = "solid", size = 0.4 , color = "#565555") +
    ylim(0, 1950) +
    annotate("rect", 
            xmin= 2014, xmax=2020,
            ymin=1570, ymax=1950,
            alpha=1, color = "#8d8989", fill = "#ffffff") +
    annotate("text", x = 2017,
        y = 1750,
        label ="Historic data",
        color = "grey50",
        lineheight = 0.9, size = 2.1, angle = 90)+
    annotate("rect", 
            xmin= 2024, xmax=2030,
            ymin=1550, ymax=1950,
            alpha=1, color = "#8d8989", fill = "#ffffff") +
    annotate("text", x = 2027,
        y = 1750,
        label ="Scenario data",
        color = "grey50",
        lineheight = 0.9, size = 2.1, angle = 90) +
    #add circle annotations (1/2/3/4)
    annotate("point", x = 1904, y = 370, shape = 21, size = 5, fill = "white", color = col_useur) +
    annotate("point", x = 1958, y = 740, shape = 21, size = 5, fill = "white", color = col_cha) +
    annotate("point", x = 1957, y = 1190, shape = 21, size = 5, fill = "white", color = col_ind) +
    annotate("point", x = 1962, y = 1380, shape = 21, size = 5, fill = "white", color = col_ssa) +
    #add labels for circles
    annotate("text", x = 1904, y = 365, label = "1", color = col_useur, size = 3) +
    annotate("text", x = 1958, y = 735, label = "2", color = col_cha, size = 3) +
    annotate("text", x = 1957, y = 1185, label = "3", color = col_ind, size = 3) +
    annotate("text", x = 1962, y = 1375, label = "4", color = col_ssa, size = 3)

  if(save_plot){
    ggsave("figs/BOF_regional_production_plot_wprojections.png", p, width = 8, height = 6, dpi = 300)
  }
  return(p)
}


plot_capacity_additions <- function(save_plot=FALSE){
  bins = 2
  start_year = 1900
  end_year = 2055

  bin_years <- seq(start_year, end_year, by = bins)
  #(period - 1900 - 5) / 5 to get index for correct bin year,
  #add to df, group by bin year and sum. 

  #load projected capacity additions
  projected_cap_additions <- read.csv("inputdata/other/primary_steel_prod_default.csv") %>%
    select(region, period, value) %>%
    filter(period > 2020, period <= 2070) %>% #only for lag calculation
    mutate(value = value / 0.8) %>% #convert to capacity
    group_by(region) %>%
    arrange(period) %>%
    mutate(cap_additions = value - lag(value)) %>% #calculate capacity additions
    mutate(cap_additions = ifelse(is.na(cap_additions), value, ifelse(cap_additions < 0, 0, cap_additions))) %>%
    ungroup() %>%
    filter(period > 2030, period <= 2070) %>% #GEM capacity additions go up to 2035
    select(region, period, cap_additions) %>%
    # show projections only for India and SSA, CHA, 
    filter(!(region %in% c("USA","EUR","REF","NEU","JPN","World", "CAZ"))) %>%
    rename(Region = region, Year = period, value = cap_additions)


  #here, we add the capacity additions from plants with little data available ("no retirement date")
  # they are already accounted for in Remaining_BOF_capacity
  df_plot_capacity <- regional_data %>%
    mutate(Total_BOF_capacity_additions = Total_BOF_capacity_additions + Total_BOF_capacity_additions_no_retirement_date) %>%
    select(-Total_BOF_capacity_additions, - Total_BOF_capacity_additions_no_retirement_date, -Remaining_BOF_capacity) %>%
    rename(value = Yearly_BOF_Capacity_additions) %>%
    mutate(value = value /1000) %>%
    filter(case_when(Region %in% c("IND", "SSA", "MEA", "CHA", "LAM", "OAS") ~ Year < 2035,
                                              T  ~ Year <2070 )) 
    #                                           %>%
    # bind_rows(projected_cap_additions)

  df_plot_capacity <- df_plot_capacity %>%
    pivot_wider(names_from = Region, values_from = value) %>%
    mutate(OASLAM = OAS + LAM) %>%
    mutate(USAEUR = USA + EUR + REF + NEU + JPN + CAZ) %>%
    select(-OAS, -USA, -EUR, -REF, -NEU, -JPN, -CAZ, -LAM, -MEA) %>%
    pivot_longer(cols = c(CHA, USAEUR, SSA, IND, OASLAM)) %>%
    mutate(Region = recode(name, !!!region_labels)) %>% 
    mutate(Region = factor(Region, levels = c(
                                          "Sub-saharan Africa",
                                          "Other Asia and Latin America",
                                          "India","China", 
                                          "Global North"))) %>% 
    filter(!(is.na(value)))

  df_plot_capacity <- df_plot_capacity %>%
    mutate(bin_year = bin_years[(Year - (start_year - bins))/bins]) %>%
    group_by(name, Region, bin_year) %>%
    summarise(value = sum(value)) %>%
    ungroup()

  # print(df_plot_capacity %>% filter(Region == "India", bin_year > 2020))


  #label years after 2025
  df_plot_capacity <- df_plot_capacity %>%
    mutate(segment = ifelse(bin_year >= 2025, "New announcements", "Historic capacity"))

  p <- ggplot() +
    geom_bar(data=df_plot_capacity,
              aes(x=bin_year, y=value, fill=Region),
              position = "stack", 
            stat="identity") +
    # scale_linetype_manual(values = c("Historic capacity" = "solid", "New announcements" = "22")) +
    scale_fill_manual(
      values = fill_colors,
      breaks = names(fill_colors)) +
    theme_bw(base_size = 8) +
  labs(
    # title = "Regional BF-BOF capacity additions\n(historical, new announcements as of June 2025\nand scenario data)",
        title = "Regional BF-BOF capacity additions\n(new announcements as of June 2025)",
        # x = "Year",
        y = "BF-BOF capacity additions (Mtpa)",
        fill = "Region") +
    scale_x_continuous(breaks = seq(start_year, end_year, by = 25),
                      limits = c(start_year,end_year)) +
    theme(
      # plot.title = element_text(hjust = 0.5, size = 11),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      axis.text.x = element_text(angle = 0),
      axis.title.x = element_blank()
    ) +
    geom_vline(xintercept = 2025, linetype = "solid", size = 0.4 , color = "#565555") +
    geom_rect(aes(xmin = start_year, xmax = 2025, ymin = 0, ymax = Inf), fill = "#6a6a6951", alpha = 0.15) +
    annotate("rect", 
            xmin= 2015, xmax=2022,
            ymin=260, ymax=340,
            # ymin=130, ymax=170,
            alpha=1, color = "#8d8989", fill = "#ffffff") +
    annotate("text", x = 2018,
        y = 300, #150
        label ="Historic data",
        color = "grey50",
        lineheight = 0.9, size = 2.1, angle = 90)+
    annotate("rect", 
            xmin= 2028, xmax=2036,
            ymin=235, ymax=360,
            # ymin=115, ymax=180,
            alpha=1, color = "#8d8989", fill = "#ffffff") +
    annotate("text", x = 2032,
        y = 300,#150,
        label ="New announcements",
        color = "grey50",
        lineheight = 0.9, size = 2.1, angle = 90)

  if(save_plot){
    ggsave("figs/BOF_regional_capacity_additions_plot.png", p, width = 8, height = 6, dpi = 300)
  }
  return(p)
}

plot_combine_production_and_cap_additions <- function(save_plot=FALSE){
  p1 <- plot_regional_production(save_plot=FALSE)
  p2 <- plot_capacity_additions(save_plot=FALSE)

  combined_plot <- ggarrange(p1, p2,
                            ncol = 2, nrow = 1,
                            widths = c(1.2, 1),
                            labels = c("a", "b"),
                            common.legend = TRUE, legend = "bottom",
                            align = "v")
  # print(combined_plot)
  if(save_plot){
    ggsave("figs/BOF_regional_production_and_capacity_additions_plot.png", combined_plot, width = 180, height = 100, units = "mm", dpi = 300)
  }
}

plot_BFBOF_age_distribution <- function(save_plot=FALSE){
  #clean up age data df: missing ages are 0.
  complete_ages <- data.frame(Age = 0:173)
  age_data <- merge(complete_ages, age_data, by = "Age", all.x = TRUE)

  age_data$Number_of_plants[is.na(age_data$Number_of_plants)] <- 0
  age_data$Total_BOF_capacity[is.na(age_data$Total_BOF_capacity)] <- 0

  #calculate cumulative capacity (age data)
  cumu_cap_age <- age_data %>%
    mutate(Cumu_Total_BOF_capacity = cumsum(Total_BOF_capacity)) %>%
    mutate(Cumu_BOF_capacity_percent = Cumu_Total_BOF_capacity / sum(Total_BOF_capacity) * 100) %>%
    select(Age, Cumu_Total_BOF_capacity, Cumu_BOF_capacity_percent)
  #####

  grouping =  5#years

  age_data_grouped <- age_data %>%
    mutate(Age_Group = case_when(
      Age >= 125 ~ "125+",  # Grouping all 81+ together
      TRUE ~ paste0(floor(Age / grouping) * grouping, "-", floor(Age / grouping) * grouping + grouping -1)
    )) %>%
    group_by(Age_Group) %>%
    summarise(
      Total_Capacity = sum(Total_BOF_capacity)
    ) %>%
    mutate(Age_Group = factor(Age_Group, levels = c(
      paste0(seq(0, 124, by = grouping), "-", seq(grouping-1, 124 + grouping -1, by = grouping)), "125+"
    )))

  # grouping regional age data
  age_data_region_grouped <- age_data_region %>%
    mutate(Age_Group = case_when(
      Age >= 125 ~ "125+",
      TRUE ~ paste0(floor(Age / grouping) * grouping, "-", floor(Age / grouping) * grouping + grouping - 1)
    )) %>%
    group_by(Age_Group, Region) %>%
    summarise(
      Regional_Capacity = sum(Total_BOF_capacity)
    ) %>%
    mutate(Age_Group = factor(Age_Group, levels = levels(age_data_grouped$Age_Group)))


  #grouping cumu capacity data
  cumu_cap_grouped <- cumu_cap_age %>%
    mutate(Age_Group = case_when(
      Age >= 125 ~ "125+",
      TRUE ~ paste0(floor(Age / grouping) * grouping, "-", floor(Age / grouping) * grouping + grouping -1)
    )) %>%
    group_by(Age_Group) %>%
    summarise(Cumu_BOF_capacity_percent = max(Cumu_BOF_capacity_percent)) %>%  # Take max per group
    mutate(Age_Group = factor(Age_Group, levels = levels(age_data_grouped$Age_Group)))  # Align factor levels

  # summarize regional capacities for each Age_Group
  regional_capacity_sum <- age_data_region_grouped %>%
    group_by(Age_Group) %>%
    summarise(Total_Regional_Capacity = sum(Regional_Capacity))

  # merge global and regional data
  plot_data <- left_join(age_data_grouped, regional_capacity_sum, by = "Age_Group") %>%
    mutate(Rest_of_World_Capacity = Total_Capacity - ifelse(is.na(Total_Regional_Capacity), 0, Total_Regional_Capacity)) %>%
    select(-Total_Regional_Capacity)

  # reshape the data for stacked bar plot
  plot_data_long <- plot_data %>%
    gather(key = "Type", value = "Capacity", -Age_Group) %>%
    filter(Type != "Total_Capacity") %>%
    mutate(Type = ifelse(Type == "Rest_of_World_Capacity", "Rest of World", Type))

  # add regional data to the long format data
  plot_data_long <- bind_rows(
    plot_data_long,
    age_data_region_grouped %>%
      select(Age_Group, Region, Regional_Capacity) %>%
      rename(Type = Region, Capacity = Regional_Capacity)
  )

  region_labels <- c(
    "CHA" = "China",
    "IND" = "India",
    "JPN" = "Japan",
    "EUR+RUS" = "Europe, Russia and\nex-USSR countries",
    "Rest of World" = "Rest of World"
  )
  plot_data_long <- plot_data_long %>%
    mutate(Type = recode(Type, !!!region_labels))
  #add cumu capacity data
  plot_data_long <- left_join(plot_data_long, cumu_cap_grouped, by = "Age_Group")


  #calculate max production
  max_production <- max(plot_data_long$Avg_Production, na.rm = TRUE)
  max_capacity <- max(plot_data$Total_Capacity, na.rm = TRUE)
  scale_production <- max_capacity / max_production

  # # Define x-position for manually drawn y-axis (right side)
  # fake_y_axis_x <- length(unique(plot_data_long$Age_Group)) + 1

  # # Define tick marks for Steel Production (manually determined)
  # steel_tick_values <- seq(0, 2000, length.out = 5)
  # scaled_steel_ticks <- steel_tick_values * scale_production 

  age_group_positions <- as.numeric(factor(plot_data_long$Age_Group, levels = levels(plot_data_long$Age_Group)))


  # plot
  p <- ggplot(plot_data_long, aes(x = Age_Group, y = Capacity, fill = Type)) +
    #more annotations: rectangle indicating increase in production
    annotate("rect", 
            xmin= unique(age_group_positions[plot_data_long$Age_Group == "15-19"])-0.4,
            xmax=unique(age_group_positions[plot_data_long$Age_Group == "25-29"])+0.4,
            ymin=0, ymax=Inf, alpha=0.4, fill="#e7ab04")+
      annotate("rect", 
            xmin= unique(age_group_positions[plot_data_long$Age_Group == "50-54"])-0.4,
            xmax=unique(age_group_positions[plot_data_long$Age_Group == "70-74"])+0.4,
            ymin=0, ymax=Inf, alpha=0.4, fill="#e7ab04")+
    geom_bar(stat = "identity") +
    geom_line(aes(y = Cumu_BOF_capacity_percent * max_capacity / 100, group = 1, color = "Cumulative Capacity"), size = 1) +
    annotate("text", x = "50-54", y = max_capacity * 1.1,
            label = "Post-WW2 BF-BOF\nsurge, driven by Europe,\nJapan, Russia and\nex-USSR countries",
            color = "black", size = 3.5, hjust = 0.1, vjust = 0.2) +
    annotate("text", x = "15-19", y = max_capacity * 1.1,
            label = "Recent surge\nof BF-BOFs,\ndriven by China",
            color = "black", size = 3.5, hjust = 0.2, vjust = 0)+
    #expand_limits(x = fake_y_axis_x + 3) +
    theme_minimal() +
    labs(title = "Age Distribution of BF-BOF Plants",
        x = "Age Group (Years)",
        y = "Total Capacity (Mtpa)",
        fill = "Region") +
    guides(fill = guide_legend(title = "Region", nrow = 2)) +
    scale_y_continuous(name = "New annual BF-BOF capacity installed (Mtpa)",
                      limits = c(0, max_capacity+80),
                      sec.axis = sec_axis(~ . * 100 / max_capacity, name = "Cumulative Capacity (%)")) +
    scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.6, direction = -1) +
    theme_classic() +
    scale_color_manual(name = "Line Legend",
                        values = c("Cumulative Capacity" = "black"
                                  #"Steel production" = "red"
                                  ),
                        guide = guide_legend(nrow =1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          )
  print(p)
  if(save_plot){
    ggsave("figs/BOF_age_distribution_plot_wregions.png", width = 10, height = 6, dpi = 300)
  }
}


plot_cumulative_capacity_additions <- function(save_plot=FALSE){
  start_year = 1900
  end_year = 2070

  #load data for post 2030 capacity additions (projections)
  #data gives production, so convert to capacity using 0.8 production factor
  # given in Mt/yr
  projected_cap_additions <- read.csv("inputdata/other/primary_steel_prod_default.csv") %>%
    select(region, period, value) %>%
    filter(period > 2030, period <= 2070) %>% #GEM capacity additions go up to 2035
    mutate(value = value / 0.8) %>% #convert to capacity
    group_by(region) %>%
    arrange(period) %>%
    mutate(cap_additions = value - lag(value)) %>% #calculate capacity additions
    mutate(cap_additions = ifelse(is.na(cap_additions), value, ifelse(cap_additions < 0, 0, cap_additions))) %>%
    mutate(cum_sum = cumsum(cap_additions)) %>%
    ungroup() %>% 
    select(region, period, cum_sum) %>%
    # show projections only for India and SSA
    filter(!(region %in% c("USA","EUR","REF","NEU","JPN", "CHA","World", "CAZ", "LAM", "MEA", "OAS"))) %>%
    # filter(!(region %in% c("World", "CAZ", "LAM", "MEA", "OAS"))) %>%
    rename(Region = region, Year = period, value = cum_sum) 

  #here, we add the capacity additions from plants with little data available ("no retirement date")
  # they are already accounted for in Remaining_BOF_capacity
  df_plot_capacity <- regional_data %>%
    mutate(Total_BOF_capacity_additions = Total_BOF_capacity_additions + Total_BOF_capacity_additions_no_retirement_date) %>%
    # select(-Total_BOF_capacity_additions, - Total_BOF_capacity_additions_no_retirement_date, -Remaining_BOF_capacity) %>%
    # rename(value = Yearly_BOF_Capacity_additions) %>%
    select(-Remaining_BOF_capacity, -Total_BOF_capacity_additions_no_retirement_date, -Yearly_BOF_Capacity_additions) %>%
    rename(value = Total_BOF_capacity_additions) %>%
    mutate(value = value /1000) %>%
    filter(case_when(Region %in% c("IND", "SSA") ~ Year < 2035,
                                              T  ~ Year <2070 )) %>%
    bind_rows(projected_cap_additions)


  df_plot_capacity <- df_plot_capacity %>%
    pivot_wider(names_from = Region, values_from = value) %>%
    mutate(OASLAM = OAS + LAM) %>%
    mutate(USAEUR = USA + EUR + REF + NEU + JPN + CAZ) %>%
    select(-OAS, -USA, -EUR, -REF, -NEU, -JPN, -CAZ, -LAM, -MEA) %>%
    pivot_longer(cols = c(CHA, USAEUR, SSA, IND, OASLAM)) %>%
    #projected values are given in 5y time steps. Remove the NAs that are inbetween
    filter(!(is.na(value)))

  #need to add another value for 2100 for USAEUR and CHA
  rows_to_add <- df_plot_capacity %>%
    filter(name %in% c("USAEUR", "CHA")) %>%
    group_by(name) %>%
    arrange(Year) %>%
    summarise(value = last(value)) %>%
    mutate(Year = 2100)

  df_plot_capacity <- df_plot_capacity %>%
    bind_rows(rows_to_add) %>%
    mutate(Region = recode(name, !!!region_labels))

  #label years after 2025
  df_plot_capacity <- df_plot_capacity %>%
    mutate(segment = ifelse(Year < 2025, "Historic capacity", ifelse(Year < 2032,"New announcements","Scenario data")))

  p <- ggplot() +
    geom_line(data=df_plot_capacity,
              aes(x=Year, y=value, colour=Region,linetype = segment), size = 1) +
    scale_linetype_manual(values = c("Historic capacity" = "solid", "New announcements" = "111111", "Scenario data" = "1343")) +
    scale_color_manual(
      values = fill_colors,
      breaks = names(fill_colors)) +
    theme_bw() +
    labs(title = "Regional BF-BOF cumulative capacity additions\n(historical, new announcements as of June 2025\nand scenario data)",
          x = "Year",
          y = "Cumulative BF-BOF capacity installed (Mtpa)",
          colour = "Region",
          linetype = "Segment") +
    scale_x_continuous(breaks = seq(start_year, end_year, by = 10),
                      limits = c(start_year,end_year)) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
      # legend.position = "bottom",
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      #rotate xticks
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
    geom_vline(xintercept = 2025, linetype = "solid", size = 0.4 , color = "#565555") +
    # geom_rect(aes(xmin = 2025, xmax = end_year, ymin = 0, ymax = Inf), fill = "#6a6a6951", alpha = 0.2)
    geom_rect(aes(xmin = start_year, xmax = 2025, ymin = 0, ymax = Inf), fill = "#6a6a6951", alpha = 0.2) +
    ylim(0, 1380) +
    annotate("rect", 
            xmin= 1990, xmax=2020,
            ymin=1270, ymax=1370,
            alpha=1, color = "#8d8989", fill = "#ffffff") +
    annotate("text", x = 2005,
        y = 1320,
        label ="Historic\ndata",
          lineheight = 0.9, size = 2.5)+
    annotate("rect", 
            xmin= 2030, xmax=2070,
            ymin=1270, ymax=1370,
            alpha=1, color = "#8d8989", fill = "#ffffff") +
    annotate("text", x = 2050,
        y = 1320,
        label ="New announcements+\nscenario data",
          lineheight = 0.9, size = 2.5) +
    #add circles
    annotate("point", x = 1950, y = 253, shape = 21, size = 5, fill = "white", color = col_useur) +
    annotate("point", x = 1995, y = 503, shape = 21, size = 5, fill = "white", color = col_cha) +
    annotate("point", x = 2028, y = 263, shape = 21, size = 5, fill = "white", color = col_ind,) +
    annotate("point", x = 2045, y = 153, shape = 21, size = 5, fill = "white", color = col_ssa) +
      #add labels for circles
    annotate("text", x = 1950, y = 250, label = "1", color = col_useur, size = 3) +
    annotate("text", x = 1995, y = 500, label = "2", color = col_cha, size = 3) +
    annotate("text", x = 2028, y = 260, label = "3", color = col_ind, size = 3) +
    annotate("text", x = 2045, y = 150, label = "4", color = col_ssa, size = 3)


  print(p)
  if(save_plot){
    ggsave("figs/BOF_regional_cumulative_capacity_plot.png", p, width = 8, height = 6, dpi = 300)
  }
}
