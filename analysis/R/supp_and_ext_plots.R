require(ggplot2)
require(tidyverse)
require(mrremind)
require(magclass)

library(dplyr)
library(gridExtra)
library(grid)
library(purrr)
library(ggpubr)
library(gdx)


require("quitte")
library("quitte")


custom_colors <- c(
    "eaf"         = "#44bb99",
    "idr"         = "#99ddff",
    "bf"          = "#7b3e0253",
    "bfcc"        = "#747373",
    "idrcc"       = "#77aadd",
    "bof"         = "#323232"
    )

tech_label <- c(
    "eaf"         = "EAF (secondary)",
    "idr"         = "DRI-EAF",
    # "bf"          = "BF",
    "bfcc"        = "CC retrofit on a BF",
    "idrcc"       = "CC retrofit on a DRI",
    "bof"         = "BF-BOF"
)


calc_steel_cap_add <- function(gdx) {
    # load basic variables needed
    t <- readGDX(gdx,"ttot")
    t <- as.numeric(t[t >= 2010 & t <= 2100])
    tePrc <- readGDX(gdx,"tePrc")

    df.deltacap <- as.quitte(readGDX(gdx, "vm_deltaCap", field = "l", restore_zeros = F)[,t,]) %>%
    filter(all_te %in% tePrc) %>%
    select(scenario, region, period, all_te, value)
}

add_scen_names <- function(df, nameslist) {
    df$scenario <- nameslist
}


plot_REMIND_steel_cap_add <- function(gdxlist) {

    region_groups <- list(
    "OASLAM" = c("OAS", "LAM"),
    "EUR+NEU+USA+JPN+REF+CAZ" = c("EUR", "NEU", "USA", "JPN", "REF", "CAZ")
    )

    # map regions
    region_map <- setNames(rep(names(region_groups), lengths(region_groups)),
                        unlist(region_groups))

    # get scenario names
    scen_names <- gdxlist$name

    # calculate capacity additions for all scenarios
    mainscens_df_cap_add <- gdxlist$gdx %>%
        map(calc_steel_cap_add)
    

    # add scenario names
    mainscens_df_cap_add[[1]]$scenario <- scen_names[[1]]
    mainscens_df_cap_add[[2]]$scenario <- scen_names[[2]]
    mainscens_df_cap_add[[3]]$scenario <- scen_names[[3]]
    mainscens_df_cap_add <- bind_rows(mainscens_df_cap_add)

    # get relative outflwo to convert capacity additions to the same unit basis (t steel)
    o37_relativeOutflow <- as.quitte(readGDX(gdxlist$gdx[[1]],name=c("o37_relativeOutflow"),format="first_found",restore_zeros = F)) %>% 
        select(all_te, opmoPrc, region, period, value) %>% 
        filter(value > 0.) %>%
        rename(relativeOutflow = value) %>%
        # remove duplicates in all_te 
        # (eg. all_te = eaf for primary and secondary steel, which are the same value (1 ton of steel = 1 ton of steel))
        distinct(all_te, region, period, .keep_all = TRUE) %>%
        select(-opmoPrc)
    
    plot_df <- mainscens_df_cap_add %>%
        left_join(o37_relativeOutflow, by = c("all_te", "region", "period")) %>%
        mutate(value = value *1e3 / relativeOutflow) %>%
        select(-relativeOutflow) %>%
        mutate(region = recode(region, !!!region_map)) %>%
        filter(period >= 2025, period <= 2070)
    
    #since we have divided by relativeOutflow, we are now double counting. Eg. 40 Mt of BF cap and 40 Mt of BOF cap
    # yields 40 Mt of BF-BOF steel cap.
    # We also need to fix this with DRI and EAF: cap(EAF sec) = cap(EAF total) - cap(DRI)
    # since all units are in tons of steel

    plot_df <- plot_df %>%
        filter(all_te != "bf") %>%
        # becuase of the region aggregation, need to summarise the cols
        group_by(scenario, period, region, all_te) %>%
        summarise(value = sum(value), .groups = "drop") %>%
        pivot_wider(names_from = all_te, values_from = value) %>%
        mutate(eaf = eaf - idr) %>%
        pivot_longer(cols = c("eaf", "idr", "bfcc", "idrcc", "bof"), names_to = "all_te", values_to = "value")

    plot_df$scenario <- factor(plot_df$scenario, levels = c("Current policies","Transition with lock-in","Fast transition"))
    plot_df$all_te <- factor(plot_df$all_te, levels = c("idr","idrcc","eaf","bfcc","bof"))

    cap_add <- ggplot(plot_df, aes(x = period, y = value, fill = all_te)) +
        geom_bar(stat = "identity", position = "stack") +
        facet_grid(scenario ~ region) +
        scale_fill_manual(values = custom_colors, labels = tech_label) +
        labs(x = "Year", y = "Steel capacity additions [Mt steel/year]", fill = "Steel production technology") +
        theme_minimal() +
        theme(
            strip.text = element_text(size = 8),
            axis.text.x = element_text(angle = 45, hjust = 1)
        )
    # print(cap_add)

    plot_df_ind <- plot_df %>% filter(region == "IND")

    cap_add_ind <- ggplot(plot_df_ind, aes(x = period, y = value, fill = all_te)) +
        geom_bar(stat = "identity", position = "stack") +
        facet_wrap(~ scenario, nrow = 1) +
        scale_fill_manual(values = custom_colors, labels = tech_label) +
        labs(x = "Year", y = "Steel capacity additions [Mt steel/year]", fill = "Steel production technology") +
        theme_minimal() +
        theme(
            strip.text = element_text(size = 8),
            axis.text.x = element_text(angle = 45, hjust = 1)
        )
    print(cap_add_ind)
}