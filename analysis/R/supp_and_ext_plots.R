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

custom_colors_prod <- c(
    "SCRAP-EAF"         = "#ffb200",  # secondary / EAF
    "DRI-H2-EAF"        = "#66cccc",  # DRI + H2
    "DRI-NG-EAF-CCS"    = "#e5e5b2",  # DRI NG + CCS
    "DRI-NG-EAF"        = "#999959",  # DRI NG
    "BF-BOF-CCS"        = "#b2b2b2",  # BF-BOF + CCS
    "BF-BOF"            = "#0c0c0c"   # BF-BOF
    )

region_groups <- list(
    CHA="China",
    IND="India",
    CAZ="Global North",
    EUR="Global North",
    JPN="Global North",
    LAM="Lat. America &\nOth. Asia",
    MEA="Middle East &\nNorth. Africa",
    NEU="Global North",
    OAS="Lat. America &\nOth. Asia",
    REF="Global North",
    SSA="Sub-Saharan Africa",
    USA="Global North",
    World="World"
)

scen_names <- list(
    "TransitionwLockIn-NPi" = "Current Policies",
    "TransitionwLockIn-PkBudg820" = "TwLI",
    "TransitionwLockIn-PkBudg650" = "Transition w lock-in (1.5C,\nlow overshoot)",
    "TransitionwLockIn-PkBudg1000" = "Transition w lock-in (2°C)",
    "FastTransition-PkBudg820" = "Fast transition",
    "FastTransition-PkBudg1000" = "Fast transition (2°C)",
    "FastTransition-PkBudg650" = "Fast transition (1.5C,\nlow overshoot)",
    "TransitionwLockIn_highCCSinjecrate-PkBudg820" = "TwLI (moreCCS)",
    "TransitionwLockIn_lowBio-PkBudg820" = "TwLI (LowBio)",
    "TransitionwLockIn_lowBio_highCCSinjecrate-PkBudg820" = "TwLI (moreCCS & LowBio)"
)

write_images <- function (fig, name, height, width = mm(180), formats = c('png', 'svg')) {
    for (f in formats) {
        if (!f %in% c('png', 'svg', 'pdf', 'ps', 'eps')) {
            stop(paste0('Unknown format', f))
        }
        path <- paste0("./figs/", name, ".", f)
        ggsave(path, plot = fig, width = width, height = height, dpi = dpi, create.dir = TRUE)
    }
}

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

plot_REMIND_steel_production <- function(mif_list, fig_name) {
    df_data <- bind_rows(mif_list) %>% filter(grepl("Production|Industry|Steel|+", variable, fixed = TRUE))

    df_plot <- (
        df_data %>%
        mutate(region = recode(region, !!!region_groups)) %>%
        group_by(model, scenario, region, period, variable, unit) %>%
        summarise(value = sum(value)) %>%
        mutate(variable = sub("Production|Industry|Steel|+|", "", variable, fixed = TRUE)) %>%
        filter(period %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2070)) %>%
        # for now, plot only world steel production
        filter(region == "World")
    )

    # Sort variable by custom order.
    df_plot$variable <- factor(df_plot$variable, levels = c('SCRAP-EAF', 'DRI-H2-EAF', 'DRI-NG-EAF-CCS', 'DRI-NG-EAF', 'BF-BOF-CCS', 'BF-BOF'))
    
    df_rows <- df_plot %>%
        mutate(scenario = recode(scenario, !!!scen_names)) %>%
        mutate(scenario = factor(scenario, levels = c(unlist(scen_names))))

    fig <- ggplot(df_rows, aes(x = scenario, y = value, fill = variable)) +
        geom_bar(stat = "identity") +
        facet_grid(rows=vars(region), cols=vars(period), scales = "free_y") +
        labs(title = NULL, x = NULL, y = ylab) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 7),
          axis.title.y = element_text(size = 9),
          strip.text.x = element_text(size = 8),
          strip.text.y = element_text(size = 8),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          legend.position = "bottom",
          legend.key.height = unit(10, "mm"),
          plot.margin = margin(10, 10, 10, 25)
        ) +
        coord_cartesian(clip = "off") +
        scale_fill_manual(
            values = custom_colors_prod,
            name = NULL,
            labels = c(
                "Secondary steel using an electric arc\nfurnace (SCRAP-EAF)",
                "Primary steel using direct reduction of iron\nwith hydrogen and an electric arc furnace\n(DRI-H2-EAF)",
                "Primary steel using direct reduction of iron\nwith natural gas and an electric arc furnace\nwith carbon capture and storage\n(DRI-NG-EAF-CCS)",
                "Primary steel using direct reduction of iron\nwith natural gas and an electric arc furnace\n(DRI-NG-EAF)",
                "Primary steel using a blast furnace and\na basic oxygen furnace with carbon\ncapture and storage (BF-BOF-CCS)",
                "Primary steel using a blast furnace and\na basic oxygen furnace (BF-BOF). For\nIndia, this includes existing\ncoal-based DRI production."
            ),
        )
    write_images(fig, fig_name, height = mm(140))
    show(fig)
}

plot_co2_prices <- function(mif_list, fig_name) {
    df_data <- bind_rows(mif_list) %>% filter(variable == "Price|Carbon")

    df_plot <- (
        df_data %>%
        mutate(region = recode(region, !!!region_groups)) %>%
        group_by(model, scenario, region, period, variable, unit) %>%
        summarise(value = mean(value)) %>%
        filter(period %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2070))
    )

    df_rows <- df_plot %>%
        mutate(scenario = recode(scenario, !!!scen_names)) %>%
        mutate(scenario = factor(scenario, levels = c(unlist(scen_names))))

    df_world <- df_rows %>% filter(region == "World")
    df_other <- df_rows %>% filter(region != "World")

    p_world <- ggplot(df_world, aes(x = period, y = value, color = scenario)) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 1) +
        facet_wrap(~ region, nrow = 1) +
        labs(title = NULL, x = NULL, y = "Carbon price (USD/tCO2)", color = "Scenario") +
        theme_minimal() +
        theme(
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            axis.title.y = element_text(size = 9),
            strip.text.x = element_text(size = 8),
            strip.text.y = element_text(size = 8),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 7),
            legend.position = "bottom",
            legend.margin = margin(t = -5, b = 0),
            legend.key.height = unit(6, "mm"),
            legend.key.width = unit(6, "mm"),
            plot.margin = margin(10, 10, 10, 10),
            strip.background = element_rect(fill = "grey90", color = NA),
            panel.spacing = unit(6, "pt"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = 0.25)
        ) +
        coord_cartesian(clip = "off") +
        scale_color_manual(
            values = c(
                "Current Policies" = "#231f20",
                "TwLI" = "#bb4430",
                "Fast transition" = "#7ebdc2"
            )
        ) +
        guides(color = guide_legend(
            title.position = "left",
            nrow = 1
        ))


    p_other <- ggplot(df_other, aes(x = period, y = value, color = scenario)) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 1) +
        facet_wrap(~ region, nrow = 2) +
        # labs(title = NULL, x = NULL, y = "Carbon price (USD/tCO2)", color = "Scenario") +
        labs(title = NULL, x = NULL, y = NULL, color = "Scenario") +
        theme_minimal() +
        theme(
            axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
            axis.text.y = element_text(size = 7),
            axis.title.y = element_text(size = 9),
            strip.text.x = element_text(size = 8),
            strip.text.y = element_text(size = 8),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 7),
            legend.position = "bottom",
            legend.margin = margin(t = -5, b = 0),
            legend.key.height = unit(6, "mm"),
            legend.key.width = unit(6, "mm"),
            plot.margin = margin(10, 10, 10, 10),
            strip.background = element_rect(fill = "grey90", color = NA),
            panel.spacing = unit(6, "pt"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = 0.25)
        ) +
        coord_cartesian(clip = "off") +
        scale_color_manual(
            values = c(
                "Current Policies" = "#231f20",
                "TwLI" = "#bb4430",
                "Fast transition" = "#7ebdc2"
            )
        ) +
        guides(color = guide_legend(
            title.position = "left",
            nrow = 1
        ))

    fig <- ggarrange(
        p_world,
        p_other,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    )

    write_images(fig, fig_name, height = mm(140))
    show(fig)
}


plot_energy_prices <- function(mif_list, var, var_name, fig_name) {
    df_data <- bind_rows(mif_list) %>% filter(variable == var)

    df_plot <- (
        df_data %>%
        mutate(region = recode(region, !!!region_groups)) %>%
        group_by(model, scenario, region, period, variable, unit) %>%
        summarise(value = mean(value)) %>%
        filter(period %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2070))
    )

    df_rows <- df_plot %>%
        mutate(scenario = recode(scenario, !!!scen_names)) %>%
        mutate(scenario = factor(scenario, levels = c(unlist(scen_names))))

    df_other <- df_rows %>% filter(region != "World")


    fig <- ggplot(df_other, aes(x = period, y = value, color = scenario)) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 1) +
        facet_wrap(~ region, nrow = 2) +
        labs(title = NULL, x = NULL, y = paste(var_name, "price (USD/GJ)"), color = "Scenario:") +
        theme_minimal() +
        theme(
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            axis.title.y = element_text(size = 9),
            strip.text.x = element_text(size = 8),
            strip.text.y = element_text(size = 8),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 7),
            legend.position = "bottom",
            legend.margin = margin(t = -5, b = 0),
            legend.key.height = unit(6, "mm"),
            legend.key.width = unit(6, "mm"),
            plot.margin = margin(10, 10, 10, 10),
            strip.background = element_rect(fill = "grey90", color = NA),
            panel.spacing = unit(6, "pt"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = 0.25)
        ) +
        coord_cartesian(clip = "off") +
        scale_color_manual(
            values = c(
                "Current Policies" = "#231f20",
                "TwLI" = "#bb4430",
                "Fast transition" = "#7ebdc2"
            )
        ) +
        guides(color = guide_legend(
            title.position = "left",
            nrow = 1
        ))

    write_images(fig, fig_name, height = mm(140))
    show(fig)
}