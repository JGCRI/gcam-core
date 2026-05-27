# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L103.ag_labor_capital
#'
#' process ILO, USDA, GTAP, FAO and other data to compile employment and capital by Ag major sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L103.Ag_LaborCapital_R_AgMajorSector_Yh}.
#' @details process ILO, USDA, GTAP, FAO and other data to compile employment and capital by Ag major sector
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate mutate_at select rename
#' @author xz 2025
#'
module_aglu_L103.ag_labor_capital <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "common/GCAM_region_names",
      # USDA labor and capital
      FILE =  "aglu/USDA/USDA_InternationAgProductivity_LaborCapital",
      # ILO and USDA labor data processed by L100
      "L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers",
      "L100.Emp_ILOEST_FullISO_ISIC1D_Yh_thouspers",
      ## ILO labor and total employment data
      FILE = "socioeconomics/ILO/ILO_ISIC_sector_mapping",
      # GTAP sectoral capital share
      "L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest",
      # Ag capital data proc
      "L100.FAO_Ag_CapitalStock_Dep_R_Yh_2015MilUSD")

  MODULE_OUTPUTS <-
    c("L103.Ag_LaborCapital_R_AgMajorSector_Yh")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # silence package checks
    scenario <- year <- gdp <- GCAM_region_ID <- account <- Region <- Units <- growth <- timestep <- region <-
      GDP <- pop <- laborproductivity <- NULL


    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Step 1. Ag labor data proc ----

    # Population -> employment -> Aggregated sector -> Detailed sector

    # Main data sources:
    # - PWT: population and total employment (emp) [not used here]
    # - ILO survey: employment by sector (crop & livestock = CL, fish = FISH, forest = FOR, Ag_total = AgT)
    # - ILO Model Estimates (ME): employment, Ag_total
    # - USDA IAP: Ag_total (sectoral coverage uncertain)
    # - FAOSTAT: mirrors ILO sources

    # For this workflow, we primarily use employment data from PWT (L103.Employment_thous_Scen_R_Yh)
    # (better regional coverage than ILO survey).
    # The objective is to downscale total employment to agricultural sectors.
    # longer term goal is to have employment in all sectors, e.g., energy, materials

    # Steps:
    # 0. Calculate agricultural employment share = Ag_total / total employment.[Note that this step was dropped]
    # 1. prepare and merge data from USDA and ILO
    # 2. Further disaggregate Ag_total into CL, FISH, and FOR
    #    using either ILO survey shares or GTAP shares.

    # 3. Derive detailed GCAM sectoral labor for crops and livestock
    # based on GTAP sectoral labor cost shares
    #
    # A key equation for each sector:
    # price x labor cost share  = wage x labor / output
    # unit cost of labor = wage x IO_labor
    # So IO_labor = UC_labor / wage

    # But note that we will need to calculate wage for crop and livestock first
    # Wage = labor compensation / CL labor derived in Step2
    # The total crop & livestock labor compensation is derived based on sum(production x UC_labor)

    # 4. Derive GCAM sectoral labor for primary forest

    # Because we didn't have quality price data for forest (were using export prices)
    # we won't use the strategy above for forest labor IO.
    # Instead, we use wage rates from crop & livestock
    # then UC_labor = IO_labor * wage
    # where as IO_labor is directly derived

    # 5. Derive GCAM sectoral labor for fisheries and aquaculture
    # to be completed [after the fisheries CMP]
    # we will exclude labor here for now.


    # Note: As shares are applied throughout the processing (to PWT.emp),
    # maintaining iso-level regions is not critical;
    # GCAM’s 32 regions will be the target resolution.

    # Caution: GTAP provides value shares, which may not reliably represent
    # volume shares, since assuming a uniform regional wage rate
    # introduces high uncertainty.


    ## 1.1 prepare and merge data from USDA and ILO ----

    ### a Proc USDA labor data (Primary Ag) ----
    USDA_InternationAgProductivity_LaborCapital %>%
      gather_years() %>%
      filter(variable == "Labor_Q_ThousPPL", !is.na(value)) ->
      L103.Emp_USDA_Ag_ISO_Yh_thouspers

    ### b Proc FAOSTAT labor data (Primary Ag + other food system) ----

    # FAOSTAT also has a model estimates
    # however, it seems (base on a few key example) that the primary ag labor
    # from FAO model is the same with ISO EST, but FAO has non-Ag (primary) AFS employment
    # We include this session here for awareness but won't include data proc for now.
    # When we represent full AFS labor later, we can add details.

    ### c ILO data + USDA ----

    # Some cross checking first for 2021

    L100.Emp_ILOEST_FullISO_ISIC1D_Yh_thouspers %>%
      filter(grepl("A:", isic_session1)) %>%
      transmute(iso, country, year, ILOEST = value) %>%
      left_join(
        L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers %>%
          filter(grepl("ISIC4_01|ISIC4_02|ISIC4_03", ISIC_division)) %>%
          group_by(iso, year) %>%
          summarize(ILOLFS = sum(value, na.rm = T), .groups = "drop"),
        by = c("iso", "year")
      ) %>%
      full_join(
        L103.Emp_USDA_Ag_ISO_Yh_thouspers %>%
          select(iso, year, USDA_IAP = value),
        by = c("iso", "year")
      ) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      mutate(Ratio_USDA_ILOEST = USDA_IAP / ILOEST) %>%
      filter(Ratio_USDA_ILOEST > 1.05|Ratio_USDA_ILOEST < 0.9) ->
      Check_BaseYear_AgLabor

    # We will use data entirely from USDA after confirming that it was largely
    # consistent with other source (mainly ILOEST) along with additional improvements

    L103.Emp_USDA_Ag_ISO_Yh_thouspers %>%
      left_join_error_no_match(iso_GCAM_regID %>% distinct(iso, GCAM_region_ID), by = "iso") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      group_by(GCAM_region_ID, region, year) %>%
      summarize(value = sum(value), .groups = "drop") ->
      L103.Emp_GCAM_Ag_R_Yh_thouspers

    ## 1.2. Derive sectoral employment shares for major Ag sectors (CropLivestock, Forest, Fish) ----
    # I.e., employment by major sector over total
    # This is needed later for further downscaling labor to detailed sectors

    ### a. ILO labor shares in Ag major sectors ----

    # Keep only distinct, mapped ISIC–GCAM pairs; both version of ISIC included
    ILO_ISIC_sector_mapping <-
      ILO_ISIC_sector_mapping %>%
      distinct(ISIC_division, GCAM_sector) %>%
      filter(!is.na(GCAM_sector))

    # proc the main ILO data ILO_Employment_by_sex_and_economic_activity_ISIC_level2
    L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers %>%
      inner_join(ILO_ISIC_sector_mapping, by = "ISIC_division") %>%
      # remove ISIC_division versions: aggregate across ISIC variants
      group_by(dplyr::across(-c(country, ISIC_division, value))) %>%
      summarize(value = sum(value), .groups = "drop") ->
      ILO_EMP_iso_thous_Ag_1

    ILO_EMP_iso_thous_Ag_1 %>%
      spread(GCAM_sector, value) %>%
      # remove NA as we will need data for all major sectors
      na.omit %>%
      # adding a data source identifier i.e., ILO survey
      transmute(iso, year,
                CropLive.ILO = crop_livestock,
                For.ILO = forest,
                Fish.ILO = fish,
                AgT.ILO = CropLive.ILO + For.ILO + Fish.ILO,
                emp.ILO = total) ->
      ILO_EMP_iso_thous_Ag

    # Check region coverage
    iso_GCAM_regID %>% select(iso, country_name, GCAM_region_ID) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(ILO_EMP_iso_thous_Ag %>% transmute(iso, ILO_reg = 1) %>%
                  distinct, by = "iso") ->
      Check_iso_coverage

    GCAM_region_names %>%
      anti_join(Check_iso_coverage %>% filter(!is.na(ILO_reg)),
                by = "GCAM_region_ID") ->
      Check_missing_GCAM_Reg
    # ISO data has 134 iso, covering most of the key regions/countries
    # but even if mapping (unbalanced matching) to GCAM regions directly, 4 regions
    # Canada, China, Russia, and Taiwan are missing

    # We will fill in the Ag major sector labor shares using GTAP

    ILO_EMP_iso_thous_Ag %>%
      # add a col of number of obs since if only 1, we cannot use approx_fun
      group_by(iso) %>%
      mutate(obs_count = sum(n())) %>% ungroup %>%
      select(-emp.ILO) %>%
      # complete years and fill/interpolate
      # if we fill after regional aggregation
      # there could be large variation due to missing years at iso levels
      # ILO_EMP_iso_thous_Ag is longer than last base year; need to include those for interpolation
      complete(nesting(iso), year = min(HISTORICAL_YEARS):max(.$year)) %>%
      fill(obs_count, .direction = "updown") %>%
      group_by(iso) %>%
      mutate(across(c(CropLive.ILO, For.ILO, Fish.ILO),
                    ~ if (first(obs_count) > 1) approx_fun(year, .x, rule = 2) else .x,
                    .names = "{.col}")) %>%
      # for iso with 1 obs, using fill
      fill(CropLive.ILO, For.ILO, Fish.ILO, .direction = "updown") %>%
      mutate(AgT.ILO = CropLive.ILO + For.ILO + Fish.ILO) %>%
      ungroup %>%
      # aggregate to GCAM regions
      left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      group_by(region, GCAM_region_ID, year) %>%
      summarize(across(c(CropLive.ILO, For.ILO, Fish.ILO, AgT.ILO), sum), .groups = "drop") %>%
      mutate(across(c(CropLive.ILO, For.ILO, Fish.ILO), ~ .x / AgT.ILO, .names = "{.col}_share")) %>%
      select(region, GCAM_region_ID, year, contains("_share")) %>%
      mutate(AgT.ILO = CropLive.ILO_share + For.ILO_share + Fish.ILO_share) ->
      L103.EMP_AgMajorSector_share_R_Yh_part1_ILO

    ### b. GTAP labor (value) shares by Ag major sector ----

    L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest %>%
      mutate(share = share / 100) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(input == "Labor") %>%
      select(region, GCAM_region_ID, year, GCAM_commodity, share) %>%
      spread(GCAM_commodity, share) %>%
      complete(nesting(region, GCAM_region_ID),
               year = min(HISTORICAL_YEARS):max(L103.EMP_AgMajorSector_share_R_Yh_part1_ILO$year) ) %>%
      group_by(region, GCAM_region_ID) %>%
      rename(For.GTAP = Forest, Fish.GTAP = OtherMeat_Fish) %>%
      mutate(across(c(For.GTAP, Fish.GTAP),~approx_fun(year, .x, rule = 2),
                    .names = "{.col}")) %>%
      ungroup() %>%
      mutate(CropLivestock.GTAP = 1 - For.GTAP - Fish.GTAP) ->
      L103.EMP_AgMajorSector_share_R_Yh_part2_GTAP


    ### c. Combine ILO and GTAP for labor shares by Ag major sector ----

    GCAM_region_names %>%
      anti_join(L103.EMP_AgMajorSector_share_R_Yh_part1_ILO,
                by = "GCAM_region_ID") ->
      Check_missing_GCAM_Reg
    # "Canada", "China", "Russia", "Taiwan"

    # Comparing the two sources
    # (GTAP have good reg coverage but in value shares)
    # ILO is missing 4 GCAM regions
    L103.EMP_AgMajorSector_share_R_Yh_part2_GTAP %>%
      left_join(L103.EMP_AgMajorSector_share_R_Yh_part1_ILO,
                 by = c("region", "GCAM_region_ID", "year")) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      mutate(R.CropLivestock = CropLivestock.GTAP / CropLive.ILO_share,
             R.For = For.GTAP / For.ILO_share) ->
      Check_GTAP_ILO

    # GTAP forest labor share is much larger while croplivestock is comparable to ILO
    # for regions with available data

    #### Adjustment for GTAP-expenditure based sectoral share ----
    # First, we calculate labor and capital share across all primary Ag sectors,
    # and obtain the labor share across primary sectors
    # However, this way, the resulted forestry labor share is higher than literature/observed data
    # For example, in China and Canada (among top 5 forestry resource countries)

    # For China's forestry employment data, see this paper https://www.emerald.com/insight/content/doi/10.1108/FER-03-2023-0003/full/html
    # Based on Fig1, the forestry labor share of primary ag is about 0.044364123 in 2019 for China
    # While the GTAP expenditure based share is 0.080271944

    # For Canada, check the https://cfs.nrcan.gc.ca/statsprofile/employment/CA
    # Forestry and logging employment from survey of employment and Canadian system of national accounts
    # suggest the forestry labor share of parimary ag is 0.137869055 and 0.158797511, respectively.
    # While GTAP based share is 0.260821813.

    # We implement a scaler of 0.5 to adjust GTAP forest labor share for 4 regions missing ILO data
    # And adjust croplivestock data accordingly
    # Regions with ILO data use ILO data.

    GTAP_Forest_Labor_Wage_Scaler <- 0.5

    L103.EMP_AgMajorSector_share_R_Yh_part2_GTAP %>%
      # the 4 ILO missing region
      inner_join(Check_missing_GCAM_Reg, by = c("region", "GCAM_region_ID")) %>%
      mutate(CropLivestock.GTAP = CropLivestock.GTAP + For.GTAP * (1 - GTAP_Forest_Labor_Wage_Scaler),
             For.GTAP = For.GTAP * GTAP_Forest_Labor_Wage_Scaler) %>%
      transmute(region, GCAM_region_ID, year,
                CROPLIVESTOCK = CropLivestock.GTAP,
                FISH = Fish.GTAP,
                FOREST = For.GTAP) %>%
      # bind ILO data for available regions
      bind_rows(
        L103.EMP_AgMajorSector_share_R_Yh_part1_ILO %>%
          transmute(region, GCAM_region_ID, year,
                    CROPLIVESTOCK = CropLive.ILO_share,
                    FISH = Fish.ILO_share,
                    FOREST = For.ILO_share)
        ) %>%
      gather(AgMajorSector, value, FISH, FOREST, CROPLIVESTOCK) ->
      L103.EMP_AgMajorSector_share_R_Yh


    ## 1.3. Derive total Ag labor and Major sector labor ----

    L103.EMP_AgMajorSector_share_R_Yh %>%
      filter(year %in% unique(L103.Emp_GCAM_Ag_R_Yh_thouspers$year)) %>%
      left_join_error_no_match(
        L103.Emp_GCAM_Ag_R_Yh_thouspers %>%
          # unit convert to ppl
          transmute(GCAM_region_ID, year, labor_ppl = 1000 * value),
        by = c("GCAM_region_ID", "year")
      ) %>%
      mutate(factor = "labor_ppl",
             value = labor_ppl * value) %>%
      select(-labor_ppl, -GCAM_region_ID)  ->
      L103.Ag_Labor_AgMajorSector_R_Yh

    # Adding moving average
    L103.Ag_Labor_AgMajorSector_R_Yh %>%
      group_by(region, AgMajorSector, factor) %>%
      mutate(value = if_else(is.na(Moving_average(value, periods = aglu.MODEL_MEAN_PERIOD_LENGTH)),
                             value, Moving_average(value, periods = aglu.MODEL_MEAN_PERIOD_LENGTH))) %>%
      ungroup %>%
      filter(year %in% HISTORICAL_YEARS) ->
      L103.Ag_Labor_AgMajorSector_R_Yh

    ## A few cross validation points
    ## India forestry and logging labor
    # about 780 k in 2022-2023 doi.org/10.1016/j.forpol.2025.103600
    # ILOLFS has 968 in 2021 and glowing fast


    # Step 2. Ag capital data proc ----

    ## 2.1 prepare and merge data from USDA and FAO ----

    # USDA data is already clean regarding historical regions
    # FAO was also cleaned in earlier proc
    # Note that FAO didn't have Taiwan and South America_Northern was incomplete
    USDA_InternationAgProductivity_LaborCapital %>%
      gather_years() %>%
      filter(variable == "Capital_Q_Mil2015USD", !is.na(value)) %>%
      left_join_error_no_match(iso_GCAM_regID %>% distinct(iso, GCAM_region_ID), by = "iso") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      group_by(region, year) %>%
      summarize(value = sum(value), .groups = "drop") ->
      L103.Capital_USDA_Ag_R_Yh_Mil2015USD

    L100.FAO_Ag_CapitalStock_Dep_R_Yh_2015MilUSD %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      group_by(region, year) %>%
      summarize(value = sum(`capital stock`), .groups = "drop") ->
      L103.Capital_FAO_Ag_R_Yh_Mil2015USD


    # We will use the best data from FAO and USDA
    # USDA has data prior to 1995 and generally has better coverage in historical years
    # But for USA and a few other regions, FAO is better in near term
    # It seems USDA IAP likely included only Ag crop and livestock capital (no forest/fish) for USA in near term
    # Thus, we prioritize FAO but fill in missing and make additional adjustments for some regions
    # Generally, we would expect the two sources to be not too different
    # But when FAO > USDA, that is likely okay (USA case)
    # If FAO < 85% of USDA (South America_Northern & Central Asia) or
    # if FAO is NA (Taiwan or before 1995) we will use USDA to replace

    # Note that USDA IAP made a change in the 2026 release for USA (previous version was closer to FAO)

    bind_rows(
      L103.Capital_USDA_Ag_R_Yh_Mil2015USD %>% mutate(source = "USDA_IAP"),
      L103.Capital_FAO_Ag_R_Yh_Mil2015USD %>% mutate(source = "FAOSTAT") ) %>%
      # quick conversion to 1975$ using GDP_deflator
      mutate(value = value * gdp_deflator(gcam.REAL_PRICE_BASE_YEAR, aglu.FAO_USDA_CAPITAL_YEAR)) %>%
      spread(source, value) %>%
      # Fill in NA using USDA (full coverage)
      mutate(value = if_else(is.na(FAOSTAT), USDA_IAP, FAOSTAT),
             Ratio = value / USDA_IAP,
             value = if_else(Ratio < 0.85, USDA_IAP, value) ) ->
      L103.Capital_GCAM_Ag_R_Yh_Mil1975USD_0

    L103.Capital_GCAM_Ag_R_Yh_Mil1975USD_0 %>%
      # Adding moving average
      group_by(region, year) %>%
      mutate(value = if_else(is.na(Moving_average(value, periods = aglu.MODEL_MEAN_PERIOD_LENGTH)),
                             value, Moving_average(value, periods = aglu.MODEL_MEAN_PERIOD_LENGTH))) %>%
      select(region, year, value) %>% ungroup ->
      L103.Capital_GCAM_Ag_R_Yh_Mil1975USD

    ## 2.2 Prepare and apply sectoral shares ----

    L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest %>%
      mutate(share = share / 100) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(input == "Capital") %>%
      select(region, year, sector = GCAM_commodity, share) %>%
      spread(sector, share) %>%
      mutate(AG = 1 - Forest - OtherMeat_Fish) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      select(-GCAM_region_ID) ->
      L103_GTAP_capital_share_bySector_R_Yh

    # Join GTAP capital rent shares across sectors
    L103.Capital_GCAM_Ag_R_Yh_Mil1975USD %>%
      left_join(L103_GTAP_capital_share_bySector_R_Yh,
                by = c("region", "year")) %>%
      group_by(region) %>%
      # fill GTAP share
      mutate(across(c(Forest, OtherMeat_Fish, AG), ~approx_fun(year, .x, rule = 2),
                    .names = "{.col}")) %>%
      # apply shares and covert to 1975$ (not mil.)
      mutate(across(c(Forest, OtherMeat_Fish, AG), ~ .x * value * 10^6)) %>%
      rename(CROPLIVESTOCK = AG,
             FOREST = Forest,
             FISH = OtherMeat_Fish) %>%
      select(-value) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      gather(AgMajorSector, value, FOREST, FISH, CROPLIVESTOCK) %>%
      mutate(factor = "capital_1975USD") ->
      L103.Ag_Capital_AgMajorSector_R_Yh

    # Step 3 Bind and export ----

    bind_rows(
      L103.Ag_Labor_AgMajorSector_R_Yh,
      L103.Ag_Capital_AgMajorSector_R_Yh) ->
      L103.Ag_LaborCapital_R_AgMajorSector_Yh

    L103.Ag_LaborCapital_R_AgMajorSector_Yh  %>%
      add_title("Historical agricultural labor and capital by major sectors crop & livestock, forestry, and, fish by GCAM regions") %>%
      add_units("number of people and 1975$ for capital") %>%
      add_legacy_name("L103.Ag_LaborCapital_R_AgMajorSector_Yh") %>%
      add_precursors(MODULE_INPUTS) ->
      L103.Ag_LaborCapital_R_AgMajorSector_Yh

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
