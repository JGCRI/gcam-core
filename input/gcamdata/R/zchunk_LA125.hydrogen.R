# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA125.hydrogen
#'
#' Provides supply sector information, subsector information, technology information for hydrogen sectors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L125.globaltech_coef},  \code{L125.globaltech_cost},  \code{L125.Electrolyzer_IdleRatio_Params}.
#' @details Takes inputs from H2A and generates GCAM's assumptions by technology and year
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter group_by mutate select if_else
#' @importFrom tidyr complete nesting
#' @author GPK/JF/PW July 2021
#'
#'
#' # Note:  NREL H2A v2018 did not include the following H2 production technologies:
#           bio + CCS, coal w/o CCS, coal + CCS (future), nuclear H2 prod,
#           solar electrolysis, and wind electrolysis. See in line comments below for further detail.
# ------------------------------------------------------------------------------
#'
module_energy_LA125.hydrogen <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/H2A_IO_coef_data",
             FILE = "energy/H2A_NE_cost_data",
             FILE = "energy/H2A_electrolyzer_NEcost_CF",
             "L223.GlobalTechCapital_elec",
             "L223.GlobalTechEff_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L125.globaltech_coef",
             "L125.globaltech_cost",
             "L125.Electrolyzer_IdleRatio_Params"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- technology <- capital.overnight <- coal_IGCC_CCS <- coal_IGCC <-
      sector.name <- subsector.name <- IGCC_CCS_no_CCS_2015_ratio <- efficiency <-
      IGCC_CCS <- IGCC_no_CCS <- with_CCS <- without_CCS <- CCS_add_cost <- `2100` <-
      `2015` <- max_improvement <- CCS_sub_eff <- notes <- minicam.energy.input <-
      minicam.non.energy.input <- improvement_to_2040 <- improvement_rate <- cost <-
      min_cost <- improvement_rate_post_2040 <- improve_max <- capacity.factor <- lm <- NULL  # silence package check notes


    H2A_prod_coef <- get_data(all_data, "energy/H2A_IO_coef_data")
    H2A_prod_cost <- get_data(all_data, "energy/H2A_NE_cost_data")
    H2A_electrolyzer_NEcost_CF <- get_data(all_data, "energy/H2A_electrolyzer_NEcost_CF")

    L223.GlobalTechCapital_elec <- get_data(all_data, "L223.GlobalTechCapital_elec")
    L223.GlobalTechEff_elec <- get_data(all_data, "L223.GlobalTechEff_elec")


    # ===================================================

    # Process data

    # A. Calculate base-year cost and energy efficiency ratio of CCS to non CCS for coal and biomass IGCC electricity generation.

    L223.GlobalTechCapital_elec %>%
      filter(technology %in% c("coal (IGCC)", "coal (IGCC CCS)"),
                     year == 2015) %>%
      spread(technology, capital.overnight) %>%
      rename(coal_IGCC = "coal (IGCC)",coal_IGCC_CCS = "coal (IGCC CCS)") %>%
      mutate(IGCC_CCS_no_CCS_2015_ratio = coal_IGCC_CCS / coal_IGCC) %>%
      select(sector.name, subsector.name, IGCC_CCS_no_CCS_2015_ratio) -> elec_IGCC_2015_cost_ratio


    L223.GlobalTechEff_elec %>%
      filter(technology %in% c("coal (IGCC)", "coal (IGCC CCS)","biomass (IGCC)", "biomass (IGCC CCS)"),
             year == 2015 ) %>%
      mutate(technology = if_else(technology %in% c("coal (IGCC CCS)", "biomass (IGCC CCS)"), "IGCC_CCS",
                                  if_else(technology %in% c("coal (IGCC)", "biomass (IGCC)"), "IGCC_no_CCS",
                                          NA_character_))) %>%
      spread(technology, efficiency) %>%
      mutate(IGCC_CCS_no_CCS_2015_ratio = IGCC_CCS / IGCC_no_CCS) %>%
      select(sector.name, subsector.name, IGCC_CCS_no_CCS_2015_ratio) -> elec_IGCC_2015_eff_ratio


    elec_IGCC_2015_eff_ratio %>%
      filter(subsector.name == "biomass") -> elec_IGCC_2015_eff_ratio_bio

    elec_IGCC_2015_eff_ratio %>%
      filter(subsector.name == "coal") -> elec_IGCC_2015_eff_ratio_coal

    # B. Calculate maximum future improvement (2100/2015) of CCS for biomass and coal IGCC electricity technologies
    #    Costs:
    L223.GlobalTechCapital_elec %>%
      filter(technology %in% c("coal (IGCC)", "coal (IGCC CCS)","biomass (IGCC)", "biomass (IGCC CCS)"),
             year %in% c(2015, 2100)) %>%
      mutate(technology = if_else(technology %in% c("coal (IGCC)", "biomass (IGCC)"), "without_CCS",
                                  if_else(technology %in% c("coal (IGCC CCS)", "biomass (IGCC CCS)"), "with_CCS",
                                          NA_character_))) %>%
      spread(technology, capital.overnight) %>%
      mutate(CCS_add_cost = with_CCS - without_CCS) %>%
      select(sector.name, subsector.name, year, CCS_add_cost) %>%
      spread(year, CCS_add_cost) %>%
      mutate(max_improvement = (1 - (`2100` / `2015` )),
             technology = if_else(subsector.name == "coal", "coal (IGCC CCS)",
                                  if_else(subsector.name == "biomass", "biomass (IGCC CCS)",
                                          NA_character_))) %>%
      select(sector.name, subsector.name, technology, max_improvement) -> elec_IGCC_CCS_cost_improvement


    #     Efficiency:
    L223.GlobalTechEff_elec %>%
      filter(technology %in% c("coal (IGCC)", "coal (IGCC CCS)","biomass (IGCC)", "biomass (IGCC CCS)"),
             year %in% c(2015, 2100)) %>%
      mutate(technology = if_else(technology %in% c( "coal (IGCC)", "biomass (IGCC)"), "without_CCS",
                                  if_else(technology %in% c( "coal (IGCC CCS)", "biomass (IGCC CCS)"), "with_CCS",
                                          NA_character_))) %>%
      spread(technology, efficiency) %>%
      mutate(CCS_sub_eff = with_CCS - without_CCS) %>%
      select(sector.name, subsector.name, year, CCS_sub_eff) %>%
      spread(year, CCS_sub_eff) %>%
      mutate(max_improvement = (1 -  (`2100` / `2015`)) ,
             technology = if_else(subsector.name == "coal", "coal (IGCC CCS)",
                                  if_else(subsector.name == "biomass", "biomass (IGCC CCS)",
                                          NA_character_))) %>%
      select(sector.name, subsector.name, technology, max_improvement) -> elec_IGCC_CCS_eff_improvement


    # C. Costs: Calculate max improvement of nuclear power generation capital overnight costs
     L223.GlobalTechCapital_elec %>%
      filter(technology == "Gen_III",
             year %in% c(2015, 2100)) %>%
      spread(year, capital.overnight) %>%
      mutate(max_improvement = (1 - (`2100`  / `2015`))) %>%
      select(sector.name, subsector.name, technology, max_improvement) -> elec_nuclear_cost_improvement




     # D. Convert Units from H2A ($/kg, GJ/kg) to GCAM (1975$/GJ, GJ/GJ)
     H2A_prod_cost %>%
       select(-notes)%>%
       gather_years() %>%
       mutate(value = if_else(units == "$2016/kg H2", value * gdp_deflator(1975,2016),
                              if_else(units == "$2005/kg H2", value * gdp_deflator(1975,2005),
                                      NA_real_)),
              value=value/CONV_GJ_KGH2,
              units="$1975/GJ H2")-> H2A_prod_cost_conv

     H2A_prod_coef %>%
       select(-notes)%>%
       gather_years()%>%
       mutate(value = if_else(units == "GJ hydrogen output / GJ input", value ^ -1, #convert efficiency to coef
                              if_else(units == "GJ in /kg H2 out", value / CONV_GJ_KGH2, #convert to per GJ H2 basis
                                      if_else(units == 'gal / kgH2 out', value / CONV_GJ_KGH2 * CONV_GAL_M3,
                                      NA_real_))),
              units = if_else(minicam.energy.input %in% c('water_td_ind_C','water_td_ind_W'),"M3 water / GJ H2", "GJ input / GJ H2")) -> H2A_prod_coef_conv

     # E. Process H2A data, extrapolating all technologies in H2A to all GCAM model years using the cost and efficiency improvement factors calculated above

     # Base year bio + CCS and coal w/o CCS assumptions were created by applying the ratio between
     # comparable IGCC technologies in the power sector.
     #
     # Coal w/o CCS was given the same improvement rate as the NREL H2A biomass w/o CCS technology.
     #
     # The "difference" (cost adder or efficiency loss) between "CCS" and "no CCS" technology pairs for
     # coal and biomass was then reduced over time by leveraging the reduction in this difference for
     # the comparable IGCC technologies in the power sector.
     #
     # Coal w/CCS and biomass w/CCS were then extended by adding this "difference" (cost adder or efficiency
     # loss) to the non-CCS version of the H2 production technology, for each period.

     H2A_prod_cost_conv %>%
       filter(technology %in% c("biomass to H2", "coal chemical CCS")) -> existing_coal_bio

     existing_coal_bio %>%
       filter(technology == "biomass to H2") -> bio_no_CCS

     bio_no_CCS_impro_2040 <- bio_no_CCS$improvement_to_2040[1]

     bio_no_CCS_max_improv <- bio_no_CCS$max_improvement[1]


     existing_coal_bio %>%
       mutate(value = if_else(subsector.name == "biomass", value * elec_IGCC_2015_cost_ratio$IGCC_CCS_no_CCS_2015_ratio, #inflate bio to bioCCS cost...
                              if_else(subsector.name == "coal", value / elec_IGCC_2015_cost_ratio$IGCC_CCS_no_CCS_2015_ratio,NA_real_)), #and deflate coal chemical CCS to coal chemical cost, using ratio of CCS to no CCS costs for IGCC elec
              technology = if_else(subsector.name == "coal","coal chemical",
                                   if_else(subsector.name == "biomass","biomass to H2 CCS",
                                           NA_character_)),
              improvement_to_2040 = if_else(technology == "coal chemical", bio_no_CCS_impro_2040,#Set coal w/o CCS improvements equal to bio w/o CCS
                                            NA_real_ ),
              max_improvement = if_else(technology == "coal chemical", bio_no_CCS_max_improv,
                                        NA_real_ ))%>%
       select(sector.name, subsector.name, technology, minicam.non.energy.input,
              units, year, value, improvement_to_2040, max_improvement) %>%
       mutate(improvement_to_2040 = approx_fun(year, improvement_to_2040, rule = 2)) -> add_coal_and_bio



     add_coal_and_bio %>%
       filter(technology=='coal chemical') -> coal_chem_costs_scaled

     coal_chem_costs_scaled %>%
       complete(nesting(sector.name, subsector.name, technology,minicam.non.energy.input), year = sort(unique(c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)))) %>%
       arrange(sector.name, subsector.name, technology, minicam.non.energy.input, year) %>%
       group_by(sector.name, subsector.name, technology, minicam.non.energy.input) %>%
       mutate(improvement_rate = (1 - improvement_to_2040[year == 2015]) ^ (1 / (2040 - 2015)) - 1,
              min_cost = value[year == 2015]*(1 - max_improvement[year == 2015]),
              cost = if_else(year <= 2015,value[year == 2015],
                             value[year == 2015]*(1 + improvement_rate) ^ (year - 2015)),
              cost = if_else(cost >= min_cost, cost, min_cost)) %>%
       fill(units,.direction='downup') -> coal_chem_costs_GCAM_years


     H2A_prod_cost_conv %>%
       filter(!(technology %in% c("coal chemical", "biomass to H2 CCS" , "coal chemical CCS"))) -> H2A_NE_cost_add_2015_techs

     # F.Nuclear thermal splitting utilized an earlier version of H2A data (2008). This data was updated by modifying
     #   H2A reactor costs to be consistent with NREL ATB's 2019 data. Max improvement leverages nuclear reactor
     #   improvement from GCAM power sector for Gen_III reactors

    H2A_NE_cost_add_2015_techs %>%
       mutate(max_improvement = if_else(technology == "thermal splitting", elec_nuclear_cost_improvement$max_improvement,
                                        max_improvement)) -> H2A_NE_cost_add_nuclear


     H2A_NE_cost_add_nuclear %>%
       complete(nesting(sector.name, subsector.name, technology,minicam.non.energy.input), year = sort(unique(c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)))) %>%
       arrange(sector.name, subsector.name, technology, minicam.non.energy.input,year) %>%
       group_by(sector.name, subsector.name, technology, minicam.non.energy.input) %>%
       mutate(improvement_to_2040 = approx_fun(year,improvement_to_2040, rule = 2),
              max_improvement = approx_fun(year,max_improvement, rule = 2),
              improvement_rate = (1 - improvement_to_2040)^(1 / (2040 - 2015)) -1, #convert improvement by 2040 to annual compound growth rate
              min_cost = value[year == 2015]*(1 - max_improvement),
              cost = if_else(year <= 2015,value[year==2015],
                             value[year == 2015]*(1 + improvement_rate) ^ (year - 2015)), #apply calculated CAGR from above to calculate cost declination pathway
              cost = if_else(cost >= min_cost,cost,
                             min_cost))%>%
       fill(units,.direction='downup') %>% #fillout strings
       bind_rows(coal_chem_costs_GCAM_years) %>% #add back coal chem
       ungroup() -> H2A_NE_cost_GCAM_years


    # G. Create bio + CCS and extend coal w/CCS

    # First, set bio's CCS tech to the same improvement rate as coal's, otherwise bio + CCS gets cheaper than coal + CCS
    elec_IGCC_CCS_cost_improvement %>%
      filter(subsector.name == 'coal') -> coal_elec_IGCC_CCS_cost_improvement

    max_CCS_cost_improvement <- coal_elec_IGCC_CCS_cost_improvement$max_improvement


    add_coal_and_bio %>% # calculate the incremental cost of CCS
      select(-improvement_to_2040,-max_improvement) %>%
      bind_rows(existing_coal_bio %>% select(-improvement_to_2040,-max_improvement)) %>%
      filter(year==2015) %>%
      mutate(value = if_else(subsector.name == 'biomass',value[technology == 'biomass to H2 CCS'] - value[technology == 'biomass to H2'],#calculate difference between CCS, no CCS techs to get an incremental cost of CCS
                             if_else(subsector.name == 'coal',value[technology == 'coal chemical CCS'] - value[technology == 'coal chemical'],
                                     NA_real_))) %>%
      filter(technology %in% c("biomass to H2 CCS" , "coal chemical CCS"))%>%
      complete(nesting(sector.name, subsector.name, technology,minicam.non.energy.input), year = sort(unique(c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)))) %>%
      mutate(max_improvement = max_CCS_cost_improvement) %>%
      arrange(sector.name, subsector.name, technology, minicam.non.energy.input,year) %>%
      group_by(sector.name, subsector.name, technology, minicam.non.energy.input) %>%
      mutate(improvement_rate = (1 - max_improvement) ^ (1 / (2100 - 2015)) - 1,
             min_cost = value[year == 2015]*(1 - max_improvement),
             cost = if_else(year <= 2015,value[year == 2015],
                            value[year == 2015] * (1 + improvement_rate) ^ (year - 2015)), #apply calculated CAGR from above to calculate cost declination pathway
             ccs_incr_cost = if_else(cost >= min_cost,cost,
                                     min_cost))%>%
      fill(units,.direction='downup') %>%
      ungroup() %>%
      select(subsector.name, year, ccs_incr_cost)-> ccs_incr_cost



    H2A_NE_cost_GCAM_years %>%
      filter(subsector.name %in% c('biomass', 'coal'))%>%
      select(sector.name, subsector.name, technology, minicam.non.energy.input,
                     units, cost, year) %>%
      arrange(sector.name, subsector.name, technology, minicam.non.energy.input,year) %>%
      left_join_error_no_match(ccs_incr_cost,by=c('subsector.name', 'year')) %>%
      mutate(cost = cost + ccs_incr_cost,
             technology = if_else(subsector.name == 'biomass', 'biomass to H2 CCS',
                                  if_else(subsector.name == 'coal', 'coal chemical CCS',
                                          NA_character_))) %>%
      select(-ccs_incr_cost)-> coal_and_bio_w_ccs



    H2A_NE_cost_GCAM_years %>% # add missing CCS technologies with the rest of the data
      select(sector.name, subsector.name, technology, minicam.non.energy.input, units, cost, year) %>%
      bind_rows(coal_and_bio_w_ccs) -> L125.globaltech_cost



    #Coef processing
    # ===================================================


    H2A_prod_coef_conv %>%
      complete(nesting(sector.name, subsector.name, technology,minicam.energy.input), year = sort(unique(c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)))) %>%
      arrange(sector.name, subsector.name, technology, minicam.energy.input, year) %>%
      group_by(sector.name, subsector.name, technology, minicam.energy.input) %>%
      mutate(value = 1 / value) %>%
      mutate(units = if_else(minicam.energy.input %in% c( 'water_td_ind_C','water_td_ind_W' ),'GJ H2 / M3 H2O','GJ H2 / GJ input')) %>%
      fill(units,.direction='downup') %>%
      mutate(improvement_to_2040 = (value[year == 2040] - value[year == 2015]) / value[year == 2015],
             improvement_rate = (1 + improvement_to_2040) ^ (1 / (2040 - 2015)) - 1) %>%
      ungroup() -> H2A_eff_improvement



    # Add 2015 value for coal w/o CCS and bio w/CCS, using same approach as for costs described above.

    H2A_eff_improvement %>%
      filter(technology %in% c("biomass to H2", "coal chemical CCS"))  -> existing_coal_bio_eff

    existing_coal_bio_eff %>%
      filter(technology == 'biomass to H2')  -> bio_no_CCS_eff

    existing_coal_bio_eff %>%
      filter(technology == 'coal chemical CCS')  -> coal_CCS_eff

    bio_no_CCS_eff %>%
      filter(minicam.energy.input == 'elect_td_ind')%>%
      select(improvement_to_2040) %>%
      unique()->bio_no_CCS_improv_2040_elec

    bio_no_CCS_eff %>%
      filter(minicam.energy.input == 'regional natural gas')%>%
      select(improvement_to_2040)%>%
      unique()->bio_no_CCS_improv_2040_NG

    bio_no_CCS_eff %>%
      filter(minicam.energy.input == 'regional biomass') %>%
      select(improvement_to_2040)%>%
      unique() -> bio_no_CCS_improv_2040_bio



    existing_coal_bio_eff %>%
      mutate(value = if_else(subsector.name == "biomass" & !(minicam.energy.input %in% c('water_td_ind_C','water_td_ind_W')),value * elec_IGCC_2015_eff_ratio_bio$IGCC_CCS_no_CCS_2015_ratio, #use efficiency ratios from electricity to derive coefficients for bio CCS, coal w/o CCS
                             if_else(subsector.name == "coal" & !(minicam.energy.input %in% c('water_td_ind_C','water_td_ind_W')),value / elec_IGCC_2015_eff_ratio_coal$IGCC_CCS_no_CCS_2015_ratio,
                                     value)),
             technology = if_else(subsector.name == "coal", "coal chemical",
                                  if_else(subsector.name == "biomass", "biomass to H2 CCS",
                                          NA_character_)),
             improvement_to_2040 = case_when(technology == 'coal chemical'&minicam.energy.input == 'elect_td_ind' ~bio_no_CCS_improv_2040_elec$improvement_to_2040,
                                             technology == 'coal chemical'&minicam.energy.input == 'regional natural gas'~bio_no_CCS_improv_2040_NG$improvement_to_2040,
                                             technology == 'coal chemical'&minicam.energy.input == 'regional coal'~bio_no_CCS_improv_2040_bio$improvement_to_2040, #set coal w/o CCS efficiency improvements equal to bio w/o CCS
                                             TRUE~improvement_to_2040),
             improvement_rate = (1 + improvement_to_2040) ^ (1 / (2040 - 2015)) - 1)-> add_coal_and_bio_eff

    H2A_eff_improvement %>%
      filter(!(technology %in% c("coal chemical", "biomass to H2 CCS"))) %>%
      bind_rows(add_coal_and_bio_eff) %>%
      mutate(max_improvement = round(improvement_to_2040 + 0.1, 2),
             max_improvement = if_else(subsector.name == "nuclear", 0, max_improvement),
             max_improvement = if_else(technology == "coal chemical", 0.075, max_improvement),                #     Coal w/o CCS max improvement set to 7.5%
             max_improvement = if_else( minicam.energy.input %in% c( "water_td_ind_W", "water_td_ind_C" ),    #     Water coefs see no improvement past 2040 H2A assumptions
                                        improvement_to_2040, max_improvement ) ) -> H2A_eff_add_2015_techs


    H2A_eff_add_2015_techs %>%
      filter(sector.name == "H2 central production" & subsector.name == "electricity") -> central_elec_eff_max_imrpov


    central_elec_eff_max_imrpov <- central_elec_eff_max_imrpov$max_improvement[1]

    H2A_eff_add_2015_techs %>%
      #      Forecourt electrolysis max improvement = central electrolysis max improvement - 1%
      mutate(max_improvement = if_else(subsector.name == "forecourt production" & technology == "electrolysis" & !(minicam.energy.input %in% c( "water_td_ind_W", "water_td_ind_C" )),
                                       central_elec_eff_max_imrpov - 0.01,
                                       max_improvement),
      #      Set improvement rate post 2040 to pre-2040 improvement
            improvement_rate_post_2040 = improvement_rate,
      #      Post 2040 improvement rate for central NG w/ and w/o CCS set to 0.3%
            improvement_rate_post_2040 = if_else(sector.name == "H2 central production" & technology %in% c("natural gas steam reforming","natural gas steam reforming CCS"),0.003,
                                                 improvement_rate_post_2040),
      #      Post 2040 improvement rate for forecourt NG wset to 0.45%
            improvement_rate_post_2040 = if_else(subsector.name == "forecourt production" & technology == "natural gas steam reforming", 0.0045,
                                                 improvement_rate_post_2040)) -> H2A_eff_fix_improv

    H2A_eff_fix_improv %>%
      arrange(sector.name, subsector.name, technology, minicam.energy.input,year) %>%
      group_by(sector.name, subsector.name, technology, minicam.energy.input) %>%
      mutate(value = case_when(year<2015 ~ value[year == 2015],
                               (year>=2015) ~ value[year == 2015]*(1 + improvement_rate) ^ (year - 2015),
                               year >= 2040 ~ value[year == 2040]*(1 + improvement_rate_post_2040)^(year - 2040)),
             value = case_when(technology == 'coal chemical' & year>2015~value[year == 2015]*(1 + improvement_rate) ^ (year - 2015),
                               TRUE~value))%>%
      mutate( improve_max = case_when( ( year >= 1975 ) ~ ( value[ year == 2015] * ( 1 + max_improvement ) ) ) ) %>%
      mutate( value = if_else( value > improve_max & improvement_rate > 0, improve_max, value ), # set to max improvement value if exceeded
              #for techs with negative improvement rates (i.e., consuming more input like electricity per unit over time, but presumably less primary input like natural gas),
              #make 2040 value the "least efficient" they can get
              value = case_when( improvement_rate < 0 & year >= 2040 ~ value[year == 2040],
                                 improvement_rate < 0 & year < 2040 ~ value,
                                 improvement_rate >= 0 ~ value )) %>%
      ungroup()-> H2A_eff_GCAM_years

    add_coal_and_bio_eff %>%
      bind_rows(existing_coal_bio_eff) %>%
      select(-improvement_to_2040, -improvement_rate) %>%
      filter(year==2015) %>%
      arrange(sector.name, subsector.name,technology,minicam.energy.input,year) %>%
      group_by(sector.name, subsector.name,minicam.energy.input) -> add_coal_and_bio_eff_2015

    add_coal_and_bio_eff_2015 %>%
      filter(subsector.name == 'biomass') %>%
      mutate(ccs_eff_loss = value[technology == 'biomass to H2 CCS'] - value[technology == 'biomass to H2']) %>%
      ungroup()-> bio_ccs_eff_loss

    add_coal_and_bio_eff_2015 %>%
      filter(subsector.name == 'coal') %>%
      mutate(ccs_eff_loss = value[technology == 'coal chemical CCS'] - value[technology == 'coal chemical'])%>%
      ungroup() -> coal_ccs_eff_loss

    ccs_eff_loss <- bind_rows(bio_ccs_eff_loss,coal_ccs_eff_loss)

    ccs_eff_loss %>%
      filter(technology %in% c('biomass to H2 CCS', 'coal chemical CCS'))%>%
      left_join_error_no_match(elec_IGCC_CCS_eff_improvement %>% select(subsector.name,max_improvement), by='subsector.name') %>%
      complete(nesting(sector.name, subsector.name, technology,minicam.energy.input), year = sort(unique(c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)))) %>%
      arrange(sector.name, subsector.name, technology, minicam.energy.input,year) %>%
      group_by(sector.name, subsector.name, technology, minicam.energy.input) %>%
      mutate(max_improvement = max_improvement[year == 2015],
             improvement_rate = (1 - max_improvement) ^ (1 / (2100 - 2015)) - 1,
             ccs_eff_loss = if_else(year <= 2015, ccs_eff_loss[year == 2015],
                                    ccs_eff_loss[year == 2015]*(1 + improvement_rate) ^ (year - 2015))) %>%
      select(sector.name, subsector.name, minicam.energy.input,year,technology,units,ccs_eff_loss)%>%
      ungroup() %>%
      fill(units,.direction='downup')-> ccs_eff

    H2A_eff_GCAM_years %>%
      filter(technology == 'biomass to H2') %>%
      select(sector.name, subsector.name, technology, minicam.energy.input,units,year,value) %>%
      left_join_error_no_match(ccs_eff%>%select(subsector.name,year,minicam.energy.input,ccs_eff_loss),
                               by=c('subsector.name', 'year', 'minicam.energy.input')) %>%
      mutate(technology='biomass to H2 CCS',
             value = value + ccs_eff_loss)%>%
      select(-ccs_eff_loss)-> bio_w_ccs_eff

    H2A_eff_GCAM_years %>%
      filter(technology == 'coal chemical') %>%
      select(sector.name, subsector.name, technology, minicam.energy.input,units,year,value) %>%
      left_join_error_no_match(ccs_eff%>%select(subsector.name,year,minicam.energy.input,ccs_eff_loss)
                               ,by=c('subsector.name', 'year', 'minicam.energy.input'))%>%
      mutate(technology='coal chemical CCS',
             value = value + ccs_eff_loss)%>%
      select(-ccs_eff_loss)-> coal_w_ccs_eff

    H2A_eff_GCAM_years %>%
      filter(!(technology %in% c("biomass to H2 CCS", "coal chemical CCS"))) %>%
      select(sector.name, subsector.name, technology, minicam.energy.input,units,year,value) %>%
      bind_rows(coal_w_ccs_eff, bio_w_ccs_eff) %>%
      mutate(value = 1 / value,
             units = if_else( minicam.energy.input %in% c( "water_td_ind_W", "water_td_ind_C" ),
                              "M3 water / GJ H2", "GJ input / GJ H2" ) ) -> L125.globaltech_coef

    # H. Wind and solar electrolysis were created by adding the cost of panels and turbines to the H2A electrolysis plant
    # using NREL ATB 2019 data.
    # Relationship between capacity factor and levelized cost of electrolyzers, for estimation of NE costs of direct
    # renewable electrolysis on a region-specific basis
    H2A_electrolyzer_NEcost_CF <- H2A_electrolyzer_NEcost_CF %>%
      mutate(IdleRatio = 1 / capacity.factor)

    IdleRatioIntercept_2015 <- lm(H2A_electrolyzer_NEcost_CF$`2015` ~ H2A_electrolyzer_NEcost_CF$IdleRatio)$coefficients[1]
    IdleRatioSlope_2015 <- lm(H2A_electrolyzer_NEcost_CF$`2015` ~ H2A_electrolyzer_NEcost_CF$IdleRatio)$coefficients[2]
    IdleRatioIntercept_2040 <- lm(H2A_electrolyzer_NEcost_CF$`2040` ~ H2A_electrolyzer_NEcost_CF$IdleRatio)$coefficients[1]
    IdleRatioSlope_2040 <- lm(H2A_electrolyzer_NEcost_CF$`2040` ~ H2A_electrolyzer_NEcost_CF$IdleRatio)$coefficients[2]
    L125.Electrolyzer_IdleRatio_Params <- tibble(
      year = c(2015, 2040),
      slope = c(IdleRatioSlope_2015, IdleRatioSlope_2040),
      intercept = c(IdleRatioIntercept_2015, IdleRatioIntercept_2040)
    )


    # ===================================================
    # Produce outputs

    L125.globaltech_coef %>%
      add_title("Input-output coefficients of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated original data into all model years") %>%
      add_precursors("energy/H2A_IO_coef_data","L223.GlobalTechEff_elec")  ->
      L125.globaltech_coef

    L125.globaltech_cost %>%
      add_title("Costs of global technologies for hydrogen") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L225.GlobalTechCost_h2") %>%
      add_precursors("energy/H2A_NE_cost_data","L223.GlobalTechCapital_elec")  ->
      L125.globaltech_cost

    L125.Electrolyzer_IdleRatio_Params %>%
      add_title("Parameters of linear relationship between idle ratio and NE cost of electrolyzers") %>%
      add_units("2016$/kg H2") %>%
      add_comments("IdleRatio = 1 / Capacity factor; linear model used to estimate levelized cost as fn of reciprocal of CF") %>%
      add_precursors("energy/H2A_electrolyzer_NEcost_CF") ->
      L125.Electrolyzer_IdleRatio_Params

    return_data(L125.globaltech_coef, L125.globaltech_cost, L125.Electrolyzer_IdleRatio_Params)
  } else {
    stop("Unknown command")
  }
}
