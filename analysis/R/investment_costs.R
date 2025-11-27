library(ggplot2)
library(gdx)
require(dplyr)
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(quitte)
library(lusweave)
library(ggsci)
library(pals)
library(stringr)
library(ggpubr)

# set up basic lists for plotting
component_order <- c("Fossil gas and biogas\n(excluding hydrogen)",
                    "Coal and biomass solids",
                    "Solar PV\n(for electrolysis)",
                    "Electrolysis",
                    "Solar PV (other steel-\nrelated demand)",
                    "EAF (secondary)",
                    "DRI CC retrofit",
                    "DRI-EAF",
                    "BF CC retrofit",
                    "BF-BOF")

region_names  <- tribble(
  ~region, ~region_name,
  'EUR',   'EU',
  'CHA',   'China',
  'USA',   'USA',
  'IND',   'India',
  'Global', 'Global'
)

scen_order <- c( "Current policies", "Transition with lock-in", "Fast transition")

# component_colors <- c(
#   "Solar PV (other steel-\nrelated demand)" = "#fad231",
#   "Solar PV\n(for electrolysis)" = "#da9e06",
#   "Electrolysis" = "#0f5e0f",
#   "EAF (secondary)" = "#1edf3e",
#   "DRI-EAF" = "#0aacdd",
#   "DRI CC retrofit" = "#7ea151",
#   "BF-BOF" = "#323232",
#   "BF CC retrofit" = "#747373",
#   "Fossil gas and biogas\n(excluding hydrogen)" = "#0d4e8f7b", 
#   "Coal and biomass solids" = "#7b3e0253"
# )

component_colors <- c(
  "Solar PV (other steel-\nrelated demand)" = "#ee8866",
  "Solar PV\n(for electrolysis)" = "#eedd88",
  "Electrolysis" = "#ffaabb",
  "EAF (secondary)" = "#44bb99",
  "DRI-EAF" = "#99ddff",
  "DRI CC retrofit" = "#77aadd",
  "BF-BOF" = "#323232",
  "BF CC retrofit" = "#747373",
  "Fossil gas and biogas\n(excluding hydrogen)" = "#0d4e8f7b", 
  "Coal and biomass solids" = "#7b3e0253"
)

#### HELPER FUNCTIONS

# Use the reference REMIND delta cap df and compare the total capacity additions
# (per tech) to the df calculated in the code
test_deltacap_calculation <- function(df_ref, df_calculated){

  df_ref_sum <- df_ref %>%
    group_by(region, period, all_te) %>%
    summarise(deltacap = sum(deltacap)) %>%
    ungroup()

  df_calculated_sum <- df_calculated %>%
    group_by(region, period, all_te) %>%
    summarise(deltacap = sum(deltacap)) %>%
    ungroup()

  df_compare <- df_ref_sum %>%
    left_join(df_calculated_sum, by = c("region", "period", "all_te"), suffix = c("_ref", "_calc")) %>%
    mutate(diff = deltacap_ref - deltacap_calc) %>%
    filter(period <= 2055, period >= 2025)

  df_diff <- df_compare %>% filter(abs(diff) > 1e-5)

  if (nrow(df_diff) > 0) {
    message("The following values show a substantial difference:")
    print(df_diff)
  }
}

calculate_final_cap_additions_steel <- function(df.deltacap_steel, df.deltacap_eaf) {
  # need to substract the eaf primary additions from the total eaf additions (to derive the secondary eaf investments)
  df.deltacap_all <- df.deltacap_steel %>%
    bind_rows(df.deltacap_eaf)

  df.deltacap_eaf <- df.deltacap_all %>%
    filter(all_te == "eaf") %>%
    mutate(opmoPrc = ifelse(is.na(opmoPrc), "sec", "PRI")) %>%
    pivot_wider(names_from = opmoPrc, values_from = deltacap, values_fill = 0.) %>%
    # sec eaf additions = substract pri eaf additions from total eaf additions
    mutate(sec = sec - PRI) %>%
    pivot_longer(cols = c("PRI", "sec"), names_to = "opmoPrc", values_to = "deltacap")

  #test: if any negative values for sec eaf additions, raise error
  if (any(df.deltacap_eaf$deltacap < -1e-6)) {
    print(df.deltacap_eaf %>% filter(deltacap < -1e-6))
    stop("Error: negative secondary eaf capacity additions calculated")
  }

  df.deltacap_all <- df.deltacap_all %>%
    filter(all_te != "eaf") %>%
    bind_rows(df.deltacap_eaf) 
  
  return(df.deltacap_all)
}

# calculate the appropriate grade of solar available for each region and each year
# this determines the FLH accessible when building X GW of solar in year Y
# for this, we calculate the capacity additions in the differnet grades. 
# We then use this to take a weighted average of the appropriate FLH (capfac)
# for the region and year
calc_FLH_electricity <- function(gdx,t) {
    # FLH given in pm_dataren
  df.capfac_el <- as.quitte(readGDX(gdx, "pm_dataren", restore_zeros = F)[,,,]) %>%
    select(region, value, char, rlf, all_te) %>%
    filter(char == "nur", all_te == "spv") %>%
    rename(capfac = value)

  # capacity given by grade in v_capDistr (compared to vm_cap)
  df.cap_el <- as.quitte(readGDX(gdx, "v_capDistr", field = "l", restore_zeros = F)[,t,]) %>%
    filter(all_te == "spv") %>%
    select(region, period, all_te, value, rlf)

  df.deltacap_el <- df.cap_el %>%
    group_by(region, rlf) %>%
    arrange(period, .by_group = TRUE) %>%
    mutate(deltacap = value - lag(value)) %>%
    ungroup() %>%
    #remove retired capacity information. We are only interested in additions
    mutate(deltacap = ifelse(deltacap < 0, 0, deltacap)) %>%
    #same boundary periods as for the other calculations
    filter(period <= 2055, period >= 2020) %>%
    select(region, period, rlf, deltacap)
  
  # calculate the share of each grade in the capacity additions
  df.grade_share <- df.deltacap_el %>%
    group_by(region, period) %>%
    mutate(grade_share = deltacap / sum(deltacap, na.rm = TRUE)) %>%
    ungroup()

  # from this, extract the average capfac available for all capacity additions in a given region and year
  df.capfac_weighted <- df.grade_share %>%
    left_join(df.capfac_el, by = c("region", "rlf")) %>%
    group_by(region, period) %>%
    summarise(
      avg_capfac = sum(capfac * grade_share, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    rename(capfac = avg_capfac)
  
  return(df.capfac_weighted)
}

# fill rows with 0 capacity requirement with the last non-zero value
# mostly useful for regions where H2 is used early in DRI (2025) and
# then replaced by NG for 4-5 timesteps
fill_missing_capacity <- function(x, max_lag = 4) {
  for (i in 1:max_lag) {
    x <- ifelse(x == 0, dplyr::lag(x, i), x)
  }
  x[is.na(x)] <- 0
  return(x)
}

# calculates the required capacity additions based on the h2/electricity demand
# and the relevant capacity factors
calculate_required_deltacap <- function(df.dem, df.capfac) {
  df.deltacap <- df.dem %>%
    left_join(df.capfac, by=c("region", "period")) %>%
    mutate(cap_required = totaldem / capfac) %>%
    group_by(region) %>%
    arrange(period) %>%
    mutate(existingcap = fill_missing_capacity(cap_required)) %>%
    mutate(deltacap = existingcap - lag(existingcap)) %>%
    mutate(deltacap = ifelse(is.na(deltacap), 0, deltacap)) %>%
    mutate(deltacap = ifelse(deltacap < 0, 0, deltacap)) %>%
    ungroup() %>% 
    select(region, period, deltacap)
  return(df.deltacap)
}


#### Conditions to using this function:
#### can only be used for the short term (from 2025 to 2050-2055), as we do not account for
#### retirement of plants when calculating capacity additions required.
#### For now, this code is also likely inaccurate in regions with substantial secondary steel production
#### in 2025, as we do not account for existing scrap EAF capacity.
#### This is definitely valid to use for: CHA, IND, LAM, OAS and SSA
calc_total_investments <- function(gdx) {
  # calculate additional electrolyser investments required for the steel sector

  # only works for the short term (up to 2050-2055) as we do not account for lifetime of investments

  # initialize useful sets
  tePrc <- readGDX(gdx,"tePrc")
  tePrcAll <- tePrc

  tePrc2opmoPrc <- as.data.frame(readGDX(gdx,"tePrc2opmoPrc")) %>% 
    rename(all_te = tePrc)

  entyFE <- as.vector(readGDX(gdx, name="entyFE"))

  t <- readGDX(gdx,"ttot")
  t <- as.numeric(t[t >= 2010 & t <= 2100])

  # calculate h2 demand from the steel sector
  # specFeDem is given in TWa/Gt.
  df.specFeDemH2 <- as.quitte(readGDX(gdx,name=c("pm_specFeDem"),format="first_found",restore_zeros = F)) %>% 
    filter(value > 0., period %in% t, all_enty == "feh2s") %>%
    select(region, period, all_enty, all_te, opmoPrc, value) %>%
    rename(entyFE = all_enty, specFeDem = value)


  df.specFeDemEl <- as.quitte(readGDX(gdx,name=c("pm_specFeDem"),format="first_found",restore_zeros = F)) %>% 
    filter(value > 0., period %in% t, all_enty == "feels") %>%
    filter(all_te %in% c("idr", "eaf")) %>%
    select(region, period, all_enty, all_te, opmoPrc, value) %>%
    rename(entyFE = all_enty, specFeDem = value)

  df.specFeDemFos <- as.quitte(readGDX(gdx,name=c("pm_specFeDem"),format="first_found",restore_zeros = F)) %>% 
    filter(value > 0., period %in% t, all_enty %in% c("fegas", "fesos")) %>%
    select(region, period, all_enty, all_te, opmoPrc, value) %>%
    rename(entyFE = all_enty, specFeDem = value)

  # get yearly production, Gt/a. outflowPrc (2005) is the avg production between 2002.5 and 2007.5
  df.outflowPrc <- as.quitte(readGDX(gdx, "vm_outflowPrc", field = "l", restore_zeros = F)[,t,]) %>%
    filter(all_te %in% tePrcAll) %>%
    select(region, period, all_te, value, opmoPrc) %>%
    rename(outflowPrc = value)

  # get capacity additions (yearly capacity additions).
  df.deltacap <- as.quitte(readGDX(gdx, "vm_deltaCap", field = "l", restore_zeros = F)[,t,]) 

  # we use this for all steel technologies, except for primary eafs, which need to be deduced from the steel production of this route.

  # delta cap(2030) corresponds to the yearly capacity additions between 2026 and 2030. No re-indexing of values needed:
  # this is consistent with the different definition of outflowPrc (which is over 2027.5 to 2032.5), as long as we consider
  # all capacity additions to come online in the middle of the time step.
  df.deltacap_steel <- df.deltacap %>%
    filter(all_te %in% tePrcAll) %>%
    select(region, period, all_te, value) %>%
    rename(deltacap = value) %>%
    group_by(region, all_te) %>%
    arrange(period) %>%
    # we use the total additions over the entire 5 yr time step.
    # in the last part of the code, this is divided by 5 to get yearly costs
    mutate(deltacap = 5 * deltacap) %>%
    ungroup() %>%
    # after 2050, we have 10 year time steps and we are beyond the standard lifetime
    # of the electrolysers and solar PV,
    # and the calculation is not applicable anymore so we remove those values here to not be confused later
    filter(period < 2060)
  
  #get steel capacity factors
  df.capfac_steel <- as.quitte(readGDX(gdx, "vm_capFac", field = "l", restore_zeros = F)[,t,]) %>%
    filter(all_te %in% tePrc) %>%
    select(region, period, all_te, value) %>%
    rename(capfac = value)

  # in USD/t capacity
  df.CAPEX_steel <- as.quitte(readGDX(gdx, "vm_costTeCapital", field = "l", restore_zeros = F)[,t,tePrcAll]) %>%
      select(region, period, all_te, value) %>%
      filter(all_te %in% c("idr", "eaf", "bf", "bof", "bfcc")) %>%
      rename(capex = value)

  ## CALC STEEL CAPEX

  # get yearly production, Gt/a. outflowPrc (2005) is the avg production between 2002.5 and 2007.5
  # only calculate eaf primary additions
  df.outflowPrc_eaf <- df.outflowPrc %>%
    filter(all_te == "eaf", opmoPrc == "PRI") %>%
    # for consistency with the other deltacap df
    filter(period < 2060) %>%
    select(region, period, outflowPrc) %>%
    rename(totaldem = outflowPrc)

  # REMIND also provide a deltaCap (capacity additions) variable. 
  # But this gives no detail on the operation mode 
  # (eg. whether an eaf is for primary or secondary steel)
  # since we want to calculate the capacity additions for each mode,
  # we derive capacity additions from the production (outflowPrc) and the capacity factor,
  # ONLY for primary eafs (these are scaling up from 2020, so we can calculate this reliably until ~2045,
  # when the plants start retiring).
  # We then subtract these from the total eaf capacity additions
  df.deltacap_eaf <- calculate_required_deltacap(df.outflowPrc_eaf, df.capfac_steel %>% filter(all_te == "eaf") %>% select(-all_te)) %>%
    # add columns to be consistent with the other deltacap df
    mutate(all_te = "eaf", opmoPrc = "PRI")

  # need to substract the eaf primary additions from the total eaf additions (to derive the secondary eaf investments)
  df.deltacap_all <- calculate_final_cap_additions_steel(df.deltacap_steel, df.deltacap_eaf)

  df.Inv_steel_all <- df.deltacap_all %>%
    # mutate(component = paste0("Steel CAPEX (", all_te, "-", opmoPrc, ")") ) %>%
    mutate(component = case_when(
      all_te == "eaf" & opmoPrc == "sec" ~ "EAF (secondary)",
      all_te == "eaf" & opmoPrc == "PRI" ~ "DRI-EAF",
      all_te == "idr" ~ "DRI-EAF",
      all_te == "bfcc" ~ "BF CC retrofit",
      all_te == "idrcc" ~ "DRI CC retrofit",
      all_te == "bf" | all_te == "bof" ~ "BF-BOF",
      TRUE ~ NA_character_
    )) %>%
    left_join(df.CAPEX_steel, by = c("region","period", "all_te")) %>%
    mutate(value = deltacap * capex) %>%
    select(region, period, value, all_te, component) %>%
    group_by(region, period, component) %>%
    summarise(value = sum(value)) %>% 
    ungroup()

  #check that the calculated capacity additions match the REMIND values
  test_deltacap_calculation(df.deltacap_steel, df.deltacap_all)

  ##### NOW, H2 CAPEX: ELECTROLYSIS + ELECTRICTY FOR ELECTROLYSIS
  # calc H2 dem in TWa/a (avg demand between 2002.5 and 2007.5)
  df.FeDemH2 <- df.specFeDemH2 %>%
    left_join(df.outflowPrc, by=c("region", "period", "all_te", "opmoPrc")) %>%
    mutate(totaldem = specFeDem * outflowPrc) %>%
    filter(period <= 2055, period >= 2020) %>%
    select(region, period, totaldem, entyFE)

  # electrolyser capfac to calculate overall electrolysis capacity required
  df.elh2capfac <- as.quitte(readGDX(gdx, "vm_capFac", field = "l", restore_zeros = F)[,t,]) %>%
    filter(all_te == "elh2") %>%
    select(region, period, value) %>%
    rename(capfac = value)

  # eta, electrolyser efficiency
  # use to calculate electricity demand from H2 demand
  df.eta <- as.quitte(readGDX(gdx, "pm_eta_conv", restore_zeros = F)[,t,]) %>%
      filter(all_te == "elh2", value > 0.) %>%
      select(region, period, value) %>%
      rename(eta = value)

  df.elDem_elh2 <- df.FeDemH2 %>%
    left_join(df.eta, by=c("region", "period")) %>%
    mutate(totaldem = totaldem / eta) %>%
    select(region, period, totaldem)

  # calculate FLH available for spv capacity additions for a given period and region
  # this is time dependent, as the grade of solar available to be added changes over time:
  # in early timesteps (2020-2030), the best grades are filled. By 2040-2050, only
  # lower grades are available, which have a lower FLH.
  df.capfac_el <- calc_FLH_electricity(gdx,t)

  # PART1: ELECTROLYSIS CAPEX
  # merge dfs and calculate elh2 (capacity) required before calculating the required capacity additions
  # this likely slightly underestimates required technology additions, as we do not account for retired capacity between timesteps.
  # Since we focus on the short term (within the lifetime of electrolysers) this effect should be small.
  df.deltacap_elh2_h2 <- calculate_required_deltacap(df.FeDemH2, df.elh2capfac) %>%
    mutate(all_te = "elh2",
          component = "Electrolysis")

  # PART 2: ELECTRICITY CAPEX FOR ELECTROLYSIS
  # merge dfs and calculate spv (capacity) required for electrolysis
  # then, calculate the required capacity additions
  df.deltacap_elh2_el <- calculate_required_deltacap(df.elDem_elh2, df.capfac_el) %>%
    mutate(all_te = "spv",
          component = "Solar PV\n(for electrolysis)")

  # get the CAPEX for elh2
  # in USD/(W/a) capacity?? So divided by 1000 relative to generisdatatech.
  # multiplied by TW => tr USD units
  df.CAPEX <- as.quitte(readGDX(gdx, "vm_costTeCapital", field = "l", restore_zeros = F)[,t,]) %>%
  # TODO: filter for spv as well
    filter((all_te == "elh2") | (all_te == "spv")) %>%
    select(region, period, all_te, value) %>%
    rename(capex = value)

  # combine electricity and hydrogen investment requirements
  # calculate total investment costs 
  # vm_cap should be in TW. so final cost unit here is in trUSD
  df.Inv_elh2 <- df.deltacap_elh2_el %>% 
    bind_rows(df.deltacap_elh2_h2) %>% 
    left_join(df.CAPEX, by=c("region", "period", "all_te")) %>%
    mutate(value = deltacap * capex) %>%
    select(region, period, value, component)

  #### NOW: OTHER ELEC DEMAND (EAF + IDR + SEC EAF)

  df.elDem <- df.specFeDemEl %>%
    left_join(df.outflowPrc, by = c("region", "period","all_te", "opmoPrc")) %>%
    mutate(dem = specFeDem * outflowPrc) %>%
    group_by(region, period, entyFE) %>%
    summarise(totaldem = sum(dem))

  # merge dfs and calculate spv (capacity) required for electrolysis
  # then, calculate the required capacity additions
  df.deltacap_elDem <- calculate_required_deltacap(df.elDem, df.capfac_el) %>%
    mutate(all_te = "spv",
          component = "Solar PV (other steel-\nrelated demand)")

  df.Inv_el <- df.deltacap_elDem %>% 
    left_join(df.CAPEX, by=c("region", "period", "all_te")) %>%
    mutate(value = deltacap * capex) %>%
    select(region, period, value, component)

  ### CALC FOSSIL OPEX FEGAS AND FESOS

  df.FosDem <- df.specFeDemFos %>%
    left_join(df.outflowPrc, by = c("region", "period","all_te", "opmoPrc")) %>%
    mutate(dem = specFeDem * outflowPrc) %>%
    group_by(region, period, entyFE) %>%
    summarise(fosdem = sum(dem))

  # calc fuel prices (including CO2 prices)
  df.priceFuel <- as.quitte(readGDX(gdx, "p_FEPrice_by_SE_Sector_EmiMkt", restore_zeros = F)) %>%
    filter(sector == "indst", emiMkt == "ETS", value > 0., period %in% t) %>%
    select(region, period, entySe, all_enty, value) %>%
    rename(entySE = entySe, entyFE = all_enty, priceFuel = value)

  df.demFe <- as.quitte(readGDX(gdx, "vm_demFeSector_afterTax", field = "l", restore_zeros = F)[,t,]) %>%
    # dimensions: vm_demFeSector_afterTax(ttot,regi,entySE,entyFE,"indst",emiMkt)
    filter(all_enty1 %in% entyFE, emi_sectors == "indst", all_emiMkt == "ETS", value > 0.) %>%
    select(period, region, all_enty, all_enty1, value) %>%
    rename(demFE = value, entySE = all_enty, entyFE = all_enty1)
  df.demFETotal = df.demFe  %>% 
    group_by( region, period, entyFE) %>% 
    summarize(demFETotal = sum(demFE)) %>%
    ungroup
  df.SEShares = df.demFe  %>%
    left_join(df.demFETotal, by=c("region", "period", "entyFE")) %>%
    mutate(SEShare = demFE / demFETotal) %>%
    select(-demFE, -demFETotal)

  #comment out to inlude CO2 pricing
  #to use SE prices (no CO2 pricing): 
  df.priceFuel <- as.quitte(readGDX(gdx, "pm_SEPrice", restore_zeros = F)) %>%
      filter(period %in% t) %>%
      select(region, period, all_enty, value) %>%
      rename(entySE = all_enty, priceSE = value)

  df.demFETotal = df.demFe  %>% 
      group_by( region, period, entyFE) %>% 
      summarize(demFETotal = sum(demFE)) %>%
      ungroup

  df.SEShares = df.demFe  %>%
    left_join(df.demFETotal, by=c("region", "period", "entyFE")) %>%
    mutate(SEShare = demFE / demFETotal) %>%
    select(-demFE, -demFETotal)

  df.priceFuel <- df.priceFuel %>%
    left_join(df.SEShares, by=c("region", "period", "entySE")) %>%
    select(-SEShare) %>%
    filter(!is.na(entyFE)) %>%
    rename(priceFuel = priceSE)

  ## end of no CO2 pricing calculation

  df.priceFuel <- df.priceFuel %>%
    left_join(df.SEShares, by=c("region", "period", "entySE", "entyFE")) %>%
    mutate(priceFuel = priceFuel * replace_na(SEShare, 0.)) %>% 
    group_by(region, period, entyFE) %>% 
    summarize(priceFuel = sum(priceFuel)) %>%
    ungroup()
  

  #prices obtained from SE sector are not available before 2030, so we take pm_FEPrice for these
  df.priceFuel_before_2030 <- as.quitte(readGDX(gdx, "pm_FEPrice", restore_zeros = F)) %>%
    filter(sector == "indst", emiMkt == "ETS", value > 0., period %in% t, period<2030) %>%
    select(region, period, all_enty, value) %>%
    rename(entyFE = all_enty, priceFuel2 = value)

  df.FosDem <- df.FosDem %>%
    left_join(df.priceFuel, by=c("region", "period", "entyFE")) %>%
    left_join(df.priceFuel_before_2030, by=c("region", "period", "entyFE")) 
  
  df.FosDem <- df.FosDem %>%
    #if no weighted priceFuel is available, use the priceFuel2 (pm_FEprice)
    mutate(priceFuel = ifelse(is.na(priceFuel), priceFuel2, priceFuel)) %>%
    select(-priceFuel2) %>%
    mutate(value = fosdem * priceFuel *5 ) %>% #*5 because the demand is in TWa/a, and the investments are given over the whole time step (5y)
    # NB. all investments are then translated to USD/yr at the end of the function
    select(region, period, value, entyFE) %>%
    rename(component = entyFE)

  
  df.totalProduction <- df.outflowPrc %>%
  # total steel production is given by the sum of bof and eaf routes
    filter(all_te %in% c("bof", "eaf")) %>%
    group_by(region, period) %>%
    summarise(value = sum(outflowPrc)) %>%
    ungroup() %>%
    mutate(component = "Total production")

  ### BRING EVERYTHING TOGETHER
  df.total_Inv <- df.Inv_steel_all %>%
    bind_rows(df.Inv_elh2) %>%
    bind_rows(df.Inv_el) %>%
    bind_rows(df.FosDem) %>%
    #make value yearly cost. 
    #Only works up to 2060, afterwards model takes 10 yr time steps
    #but this estimation is also only valid up to 2050/2055
    mutate(value = value/5) %>%
    filter(period <= 2055, period >= 2020)
  return(df.total_Inv)
}


calc_cumu_investment_costs <- function(df, discount = 0.05){
  # annualize the costs
  df_discounted <- df %>%
    group_by(region, scenario, component) %>%
    complete(period = full_seq(period, 1)) %>%  # Fill every year
    arrange(region, scenario, component, period) %>%
    mutate(value1 = value) %>%
    fill(value1, .direction = "down") %>%
    ungroup() %>%
    select(-value) %>%
    # use correct time periods. Earliest time period is 2028.
    mutate(period = period - 2) 

  min_period = min(df_discounted$period, na.rm = TRUE)

  df_discounted <- df_discounted  %>%
    # multiply by discount factor and take cumulative sum
    arrange(region, scenario, component, period) %>%
    mutate(discounted_cost = value1 / ((1 + discount) ^ (period - min_period))) %>%
    group_by(region, scenario, component) %>%
    # summarise(total_cost_lockin = sum(discounted_cost, na.rm = TRUE)) %>%
    mutate(cumulative_value = cumsum(discounted_cost)) %>%
    ungroup()
}

combine_investment_costs <- function(scenarios) {
  # scenarios: list of scenario objects, each with $gdx and $name
  # calc_fun: function to calculate investments, e.g. calc_total_investments
  dfs <- lapply(scenarios, function(scen) {
    calc_total_investments(scen$gdx) %>% mutate(scenario = scen$name)
  })
  df.totalcosts <- bind_rows(dfs) %>%
    # filter(period >= 2030, period <= 2045) %>%
    filter(period >= 2025, period <= 2045) %>%
    mutate(period = as.numeric(period)) %>%
    mutate(value = value * 1000) #convert to billion USD
}


plot_invst_costs <- function(df.totalcosts, region_to_plot = "India", save_plot = FALSE){
  if(region_to_plot == "Global"){
    df.totalcosts <- df.totalcosts %>%
      mutate(value = ifelse(is.na(value), 0, value)) %>%
      group_by(period, scenario, component) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(region = "Global")
  }

  plot_df <- df.totalcosts %>%
    filter(region == region_to_plot) %>%
  # # remove fesos and fegas
    filter(!component %in% c("fesos", "fegas")) %>%
    mutate(
      period = as.integer(period),
      component = factor(component, levels = component_order[component_order %in% unique(component)]), 
      scenario = factor(scenario, levels = scen_order)
    )

  # to get y axis limit
  totals <- plot_df %>%
    group_by(period, scenario) %>%
    summarise(total = sum(value, na.rm = TRUE)) %>%
    ungroup()
  max_y <- max(totals$total)

  # get the components in the df
  present_components <- levels(plot_df$component)

  # subset the colors vector
  present_colors <- component_colors[present_components]

  # get average investments required
  avg_investments <- plot_df %>%
  # start average over the first calculated timestep (2030 = 2026-2030 investments)
    filter(period >= 2030) %>%
    group_by(period, scenario) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(scenario) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(type = "Nominal average\n(2030-2045)")

  # plot
  region_plot <- ggplot() +
    geom_bar(data = plot_df,
            aes(x = period, y = value, fill = factor(component)),
            position = "stack", 
            stat="identity", 
            width=3) +
    scale_fill_manual(values = present_colors) +
    facet_wrap(~ scenario, nrow = 3) +
    scale_x_continuous(
      breaks = sort(unique(plot_df$period)),
      labels = sort(unique(plot_df$period))) +
    # add dashed line for average
    # geom_hline(data = avg_investments, aes(yintercept = mean), linetype="dashed", color = "black") +
    # # add label
    # geom_text(data = avg_investments, aes(x = 2037, y = mean + 5,
    #                                       label = paste0("Nominal average investments:\n", round(mean,1), " Bill. USD/yr")),
    #                                       color = "black", size = 4, inherit.aes = FALSE) +
    labs(
      # title = paste0(
      #   region_names$region_name[region_names$region == region_to_plot],
      #   " annual steel sector investments\n\n"),
      x = "",
      y = "Annual investments\n(Bill. USD)",
      fill = "CAPEX"
    ) +
    scale_y_continuous(limits = c(0, max_y * 1.1)) +
    guides(fill=guide_legend(
      ncol=2,
      )) +
    theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.text = element_text(size = 6),
      legend.position = "bottom",
      legend.title.position = "top",
      legend.key.size = unit(0.8,"line"),
      plot.margin = margin(10, 5, 5, 5)
    ) 
  
  avg_plot <- ggplot(avg_investments, aes(x = type, y = mean)) +
    geom_bar(stat = "identity", fill = "#adaaaa", width = 0.8) +
    facet_wrap(~ scenario, nrow = 3) +
    geom_text(aes(label = round(mean, 1)), vjust = -0.5, size = 2.5, color = "grey50") +
    labs(
      # title = paste0(
      #   region_names$region_name[region_names$region == region_to_plot],
      #   " average annual\nsteel sector\ninvestments"),
      # x = "",
      # y = "Investment costs\n(Bill. USD) per year"
    ) +
    theme_bw(base_size = 9) +
    scale_y_continuous(limits = c(0, max_y * 1.1)) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      strip.text = element_text(face = "bold", color = "white"),
      strip.background = element_rect(fill = "white", color = "white"),
      # strip.background = element_blank(),
      plot.margin = margin(10, 10, 98.5, 5)
    )

  combined_plot <- ggarrange(
    region_plot, avg_plot, ncol = 2, widths = c(3, 1),
    labels = c("a", "b"),
    font.label = list(size = 10, face = "bold"))
    
    
  if(save_plot){
    ggsave(
      paste0("figs/investment_costs_v0_", region_to_plot, ".png"),
      plot = combined_plot,
      width = 88, height = 130, unit = "mm",dpi = 300)
    ggsave(
      paste0("figs/investment_costs_v0_", region_to_plot, ".svg"),
      plot = combined_plot,
      width = 88, height = 130, unit = "mm")
  }
}


plot_invst_costs_v2 <- function(df.totalcosts, region_to_plot = "India", save_plot = FALSE){
  if(region_to_plot == "Global"){
    df.totalcosts <- df.totalcosts %>%
      mutate(value = ifelse(is.na(value), 0, value)) %>%
      group_by(period, scenario, component) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(region = "Global")
  }

  plot_df <- df.totalcosts %>%
    filter(region == region_to_plot) %>%
  # # remove fesos and fegas
    filter(!component %in% c("fesos", "fegas")) %>%
    mutate(
      period = as.integer(period),
      component = factor(component, levels = component_order[component_order %in% unique(component)]), 
      scenario = factor(scenario, levels = scen_order)
    )

  # get the components in the df
  present_components <- levels(plot_df$component)

  # subset the colors vector
  present_colors <- component_colors[present_components]

  # get average investments required
  avg_investments <- plot_df %>%
    # start average over the first calculated timestep (2030 = 2026-2030 investments)
    filter(period >= 2030) %>%
    group_by(period, scenario, component) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(scenario, component) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(scenario = ifelse(scenario == "Fast transition", "Fast\nTransition", ifelse(scenario == "Current policies", "Current\npolicies", "Transition\nwith lock-in"))) %>%
    mutate(scenario = factor(scenario, levels = c("Current\npolicies", "Transition\nwith lock-in", "Fast\nTransition")))

  # scenario offset for stacked bar
  scenario_offsets <- setNames(seq(0, by = 1.2, length.out = length(scen_order)), scen_order)
  plot_df <- plot_df %>%
    mutate(period_offset = period + scenario_offsets[as.character(scenario)])
  
  # get year labels, to be added manually
  year_centers <- plot_df %>%
    distinct(period, period_offset) %>%
    group_by(period) %>%
    summarise(x_center = mean(period_offset)) %>%
    arrange(x_center)

  #scenario labels
  scen_labels <- plot_df %>%
    distinct(period_offset, scenario) %>%
    mutate(scenario = ifelse(scenario == "Fast transition", "Fast Transition", ifelse(scenario == "Current policies", "Current policies", "Transition with\nlock-in")))

  totals <- plot_df %>%
    group_by(period_offset, scenario) %>%
    summarise(total = sum(value, na.rm = TRUE)) %>%
    ungroup()

  max_y <- max(totals$total, na.rm = TRUE)
  y_min  <- -0.5 * max(1, max_y)
  y_scen <- y_min * 0.5
  
  # plot
  region_plot <- ggplot(plot_df, aes(x = period_offset, y = value, fill = component)) +
    geom_bar(stat = "identity", position = "stack", width = 1.1) +
    geom_text(
      data = transform(scen_labels, y = y_scen),
      aes(x = period_offset, y = y, label = scenario),
      inherit.aes = FALSE,
      angle = 90,
      vjust = 0.5,
      size = 2,
      fontface = "plain"
    ) +
    scale_fill_manual(values = present_colors) +
    scale_x_continuous(
      breaks = year_centers$x_center,
      labels = year_centers$period
    ) +
    scale_y_continuous(
      limits = c(y_min, max_y),
      # hide negative values (space used for annotations)
      breaks = function(lims) {
        b <- scales::breaks_extended(n=6)(c(0,lims[2]))
        b[b >= 0]
      },
      minor_breaks = NULL,
      expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = paste0(
        region_names$region_name[region_names$region == region_to_plot],
        " annual steel sector investments"),
      subtitle = "(including supply side investments such as solar PV and electrolysis)\n",
      x = "Scenario and year",
      y = "Annual investments\n(Bill. USD/year)",
      fill = "CAPEX"
    ) +
    guides(fill = guide_legend(ncol = 1)) +
    theme_bw(base_size = 8) +
    coord_cartesian(clip = "off") +
    theme(
      panel.border = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.3, color = "#dbd8d8"),
      panel.grid.major.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "right",
      legend.title.position =  "top",
      legend.box.margin = margin(-40, 0, 0, 0),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.size = unit(0.8,"line"),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin = margin(10, 10, 40, 10)
    )


  totals_per_scenario <- avg_investments %>%
    group_by(scenario) %>%
    summarise(label = round(sum(mean, na.rm = TRUE), 1)) %>%
    ungroup()

  avg_plot <- ggplot(avg_investments, aes(x = scenario, y = mean, fill = component)) +
    geom_bar(stat = "identity", position= "stack", width = 0.8) +
    geom_text(
      data = totals_per_scenario,
      aes(x = scenario, y = label + (max_y * 0.1), label = label),
      inherit.aes = FALSE,
      size = 2.5,
      color = "grey50"
    ) +
    labs(
      title = paste0(
        region_names$region_name[region_names$region == region_to_plot],
        " average annual\nsteel sector investments"),
      subtitle = "(2030-2045)",
      x = "Scenario",
      y = "Annual investments\n(Bill. USD/year)"
    ) +
    scale_fill_manual(values = present_colors) +
    scale_y_continuous(
      limits = c(0, max_y),
      breaks = function(lims) {
        b <- scales::breaks_extended(n=6)(c(0,lims[2]))
      },
      ) +
    theme_bw(base_size = 8) +
    guides(fill = guide_legend(ncol = 1)) +
    theme(
      panel.border = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.3, color = "#dbd8d8"),
      panel.grid.major.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "right",
      legend.title.position =  "top",
      legend.margin = margin(0, 0, 0, 0),
      legend.key.size = unit(0.8,"line"),
      plot.margin = margin(10, 15, 60, 10),
      axis.title.y = element_blank()
    )

  combined_plot <- ggarrange(
    region_plot, avg_plot,
    common.legend = TRUE,
    legend = "right",
    labels = c("a", "b"), ncol = 2, nrow=1,
    widths = c(2.5,1),
    font.label = list(size = 12, face = "bold")
    )

  if(save_plot){
    ggsave(
      paste0("figs/investment_costs_", region_to_plot, ".png"),
      plot = combined_plot,
      width = 180, height = 90, units = "mm", dpi = 300)
    ggsave(
      paste0("figs/investment_costs_", region_to_plot, ".svg"),
      plot = combined_plot,
      width = 180, height = 90, unit = "mm")
  }
}


plot_avg_invst_capex <- function(df.totalcosts, final_year,  discount_rate, region_to_plot = "IND", save_plot = FALSE){

  plot_df <- df.totalcosts %>%
    filter(region == region_to_plot)

  plot_df_cumu_invest <- calc_cumu_investment_costs(plot_df, discount = discount_rate)

  plot_df_all <- plot_df_cumu_invest %>%
    mutate(
      period = as.integer(period),
      scenario = factor(scenario, levels = scen_order)
    ) %>%
    filter(period <= final_year) %>%
    group_by(region, scenario, component) %>%
    summarise(mean = max(cumulative_value) / (max(period) - min(period))) %>%
    ungroup()

  plot_df <- plot_df_all %>%
    filter(!component %in% c("fesos", "fegas")) %>%
    mutate(component = factor(component, levels = component_order)) 
    

  total_df <- plot_df %>%
    group_by(region, scenario) %>%
    summarise(total = sum(mean, na.rm = TRUE)) %>%
    ungroup() %>%
    select(region, scenario, total)

  # Area plot
  region_plot <- ggplot() +
    geom_bar(data = plot_df,
            aes(x = scenario, y = mean, fill = factor(component)),
            position = "stack", 
            stat="identity",
            width = 0.5) +
    scale_fill_manual(values = component_colors) +
      # geom_text(aes(scenario, total+6, label = round(total,1), fill = NULL), data = total_df) +
    labs(
      title = paste("Average investment costs for the steel sector in",
                    region_names$region_name[region_names$region == region_to_plot],
                    ", \nbetween today and", final_year),
      x = "Year",
      y = "Annual investments\n(Bill. USD)",
      fill = "CAPEX"
    ) +
    guides(fill=guide_legend(nrow=3)) +
    theme_bw(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) 
  # print(region_plot)

  if(save_plot){
    ggsave(paste0("figs/investment_costs_avg_", region_to_plot, "_", final_year, ".png"), plot = region_plot, width = 12, height = 8, dpi = 300)
  }
}


plot_avg_invst_fossil <- function(df.totalcosts, final_year, discount_rate, region_to_plot = "IND", save_plot = FALSE){
  
  plot_df <- df.totalcosts %>%
    filter(region == region_to_plot)

  plot_df_cumu_invest <- calc_cumu_investment_costs(plot_df, discount = discount_rate)
  
  plot_df_all <- plot_df_cumu_invest %>%
    mutate(
      period = as.integer(period),
      scenario = factor(scenario, levels = scen_order)
    ) %>%
    filter(period <= final_year) %>%
    group_by(region, scenario, component) %>%
    summarise(mean = max(cumulative_value) / (max(period) - min(period))) %>%
    ungroup()

  plot_df <- plot_df_all %>%
    filter(component %in% c("fesos", "fegas")) %>%
    mutate(type = ifelse(component %in% c("fesos", "fegas"), "Fossil OPEX\n(without CO2 pricing)", "Investment costs")) %>%
    mutate(component = gsub("fesos", "Coal and biomass solids", component)) %>%
    mutate(component = gsub("fegas", "Fossil gas and biogas\n(excluding hydrogen)", component)) %>%
    mutate(component = factor(component, levels = component_order)) 
    

  plot_df$type <- factor(plot_df$type, levels = c("Investment costs", "Fossil OPEX\n(without CO2 pricing)")) 

  total_df <- plot_df %>%
    group_by(region, scenario) %>%
    summarise(total = sum(mean, na.rm = TRUE)) %>%
    ungroup() %>%
    select(region, scenario, total)

  # Area plot
  region_plot <- ggplot() +
    geom_bar(data = plot_df,
            aes(x = scenario, y = mean, fill = factor(component)),
            position = "stack", 
            stat="identity") +
    scale_fill_manual(values = component_colors) +
      # geom_text(aes(scenario, total+6, label = round(total,1), fill = NULL), data = total_df) +
    labs(
      title = paste("Additional operational expenditure for the steel sector in", 
                    region_names$region_name[region_names$region == region_to_plot],
                    ", \nbetween today and", final_year),
      x = "Year",
      y = "Investment costs\n(Bill. USD) per year",
      fill = "CAPEX"
    ) +
    guides(fill=guide_legend(nrow=3)) +
    theme_bw(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) 
  # print(region_plot)

  #save
  if(save_plot){
    ggsave(paste0("figs/investment_costs_fossil_avg_", region_to_plot,, "_", final_year, ".png"), plot = region_plot, width = 12, height = 8, dpi = 300)
  }
}