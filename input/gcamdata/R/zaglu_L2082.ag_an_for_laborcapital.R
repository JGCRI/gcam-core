# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2082.ag_an_for_laborcapital
#'
#' Establishes labor and capital as explicit production inputs for crops, livestock, and forestry;
#' partitions nonLandVariableCost into labor, capital, and residual other costs; and derives
#' regional factor prices (wage and capital rate of return).
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2082.laborcapital_ag_irr_mgmt_value_chain_Yh},
#' \code{L2082.laborcapital_an_value_chain_Yh}, \code{L2082.laborcapital_for_value_chain_Yh},
#' \code{L2082.AgCoef_laborcapital_ag_irr_mgmt_hist}, \code{L2082.AgCoef_laborcapital_bio_irr_mgmt_hist},
#' \code{L2082.StubTechCoef_laborcapital_an_hist}, \code{L2082.AgCoef_laborcapital_for_hist},
#' \code{L2082.laborprice_Yh}, \code{L2082.capitalprice_Yh},
#' \code{L2082.AgCost_ag_irr_mgmt_adj}, \code{L2082.AgCost_bio_irr_mgmt_adj},
#' \code{L2082.StubTechCost_an_adj}, \code{L2082.AgCost_For_adj},
#' \code{L2082.Ag_LaborCapital_R_AgMajorSector_Yh_ValueAdded}.
#' @details This module makes labor and capital endogenous factor inputs in GCAM's agricultural
#' sectors (crops, livestock, forestry), creating explicit factor markets. Previously, the
#' non-land variable cost (NLC) bundled labor, capital, and other inputs together. The module
#' proceeds in seven steps:
#' \enumerate{
#'   \item Derive regional labor and capital shares of NLC from GTAP cost data (L100), interpolated
#'     across all historical years. For crops, shares are computed relative to all non-land,
#'     non-chemical costs. For livestock, only primary-sector costs (excluding feed, processing,
#'     water, and land/pasture) are used, consistent with USDA factor data. Region-specific
#'     calibration adjustments are applied from a configuration CSV
#'     (\code{GTAP_labor_capital_adjustments}) to correct known biases in GTAP cost shares.
#'   \item Apply cost shares to technology-level NLC to obtain labor and capital unit costs.
#'     Minimums are enforced (0.003 1975$/kg for crops; 0.01 1975$/kg for livestock). The
#'     residual NLC (NLC minus labor and capital unit costs) is passed to downstream modules.
#'     Factor expenditures are calculated by multiplying unit costs by production volumes.
#'   \item Derive implicit regional factor prices (wage in K1975$/person; capital rate of return
#'     in 1975$/1975$) by dividing total factor expenditures (crops + livestock) by physical
#'     factor quantities from L103.
#'   \item Convert unit costs to IO coefficients (physical input per unit output) by dividing
#'     by factor prices. For crops, a theta-based method differentiates hi- vs. lo-management
#'     technologies within each IRR/RFD group: hi-tech is made relatively more capital-intensive
#'     (theta_IRR = 0.2, theta_RFD = 0.1), with the delta offset scaled by the production ratio
#'     to conserve approximate total expenditure. Livestock IOs are not further differentiated
#'     because mixed vs. pastoral systems already carry naturally different productivities.
#'   \item Derive forest IO coefficients directly from regional labor and capital quantities
#'     (L103 FOREST sector) divided by total regional forest production. A single IO applies
#'     uniformly across all forest technologies within a region.
#'   \item Verify internal consistency: factor prices implied by the IO-based value chain
#'     must match those calculated in Step 3 within a tolerance of 0.001.
#'   \item Extend the residual NLC to all model years by holding the last historical base year
#'     value constant, for use in downstream GCAM processing.
#' }
#' In GCAM, land value is determined residually (Revenue minus all explicit costs) and is not
#' modified here.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else left_join mutate select
#' @importFrom tidyr replace_na gather
#' @importFrom tibble tibble
#' @author XZ DS Nov 2023; XZ 2025/2026
module_aglu_L2082.ag_an_for_laborcapital <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      # GTAP cost shares
      "L100.GTAPCostShare_AgLU_reg_comm",
      # Labor and capital adjustments by region and commodity
      FILE = "socioeconomics/GTAP/GTAP_labor_capital_adjustments",
      # These costs (non-land costs) will be adjusted as we separate out labor and capital.
      "L2062.AgCost_ag_irr_mgmt_adj",
      "L2062.AgCost_bio_irr_mgmt_adj",
      "L202.StubTechCost_an",
      "L2052.AgCost_For",
      # Production by tech
      "L2012.AgProduction_ag_irr_mgmt",
      "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
      "L2012.AgProduction_For",
      # producer prices
      "L1321.ag_prP_R_C_75USDkg",
      "L1321.an_prP_R_C_75USDkg",
      "L1321.expP_R_F_75USDm3",
      # Regional agricultural labor and capital for downscale (from major to detailed sectors)
      "L103.Ag_LaborCapital_R_AgMajorSector_Yh")

  MODULE_OUTPUTS <-
    c(# combined tables
      "L2082.laborcapital_ag_irr_mgmt_value_chain_Yh",
      "L2082.laborcapital_an_value_chain_Yh",
      "L2082.laborcapital_for_value_chain_Yh",
      # historical IO coefficient of labor and capital in Ag Prod
      "L2082.AgCoef_laborcapital_ag_irr_mgmt_hist",
      "L2082.AgCoef_laborcapital_bio_irr_mgmt_hist",
      "L2082.StubTechCoef_laborcapital_an_hist",
      "L2082.AgCoef_laborcapital_for_hist",
      # labor and capital prices derived using crop and livestock factor compensation and inputs
      "L2082.laborprice_Yh",
      "L2082.capitalprice_Yh",
      # update "other" cost
      "L2082.AgCost_ag_irr_mgmt_adj",
      "L2082.AgCost_bio_irr_mgmt_adj",
      "L2082.StubTechCost_an_adj",
      "L2082.AgCost_For_adj",
      # Updated GCAM regional Ag labor and capital stat
      "L2082.Ag_LaborCapital_R_AgMajorSector_Yh_ValueAdded")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- GCAM_region_ID <- GCAM_commodity <- GLU <- GLU_name <- IRR_RFD <-
      MGMT <- region <- AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <-
      minicam.energy.input <- coefficient <- GCAM_subsector <- NULL  # silence package check notes

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)



    # The non-land variable cost (NLC) from upstream modules (L2062) bundles labor, capital,
    # energy, and materials together. This module separates out labor and capital, producing:
    #   - Labor and capital IO coefficients (physical input per unit output) per technology
    #   - Residual NLC (energy + materials) passed to downstream modules
    #   - Regional factor prices (wage and capital rate of return) derived from expenditure / quantity
    #
    # Land value remains determined residually in GCAM:
    #     Land value = Revenue - NLC_residual - Labor - Capital - Fertilizer
    # Labor and capital become endogenous factor markets; land remains exogenous.



    # Step 1: Derive labor and capital cost shares from GTAP ----

    # GTAP cost shares give the fraction of total production cost attributable to each input
    # (Labor, Capital, Land, AgLU feed, Water, Chemicals, etc.) by region and commodity.
    # Land cost was already incorporated in L2052; here we use GTAP to split the remaining NLC.
    #
    # For livestock: only primary-sector GTAP costs are used (no _Proc inputs, no feed/pasture/water),
    # consistent with USDA factor data which covers on-farm production rather than processing.
    # FAO/USDA prices reflect meat/dairy (processed), but the GTAP primary sector is a closer match
    # to the on-farm value chain captured in GCAM livestock modules.
    #
    # GTAP data covers a limited set of years; shares are interpolated across all HISTORICAL_YEARS
    # and held constant beyond the last available GTAP year.

    COMM_LIVESTOCK <- c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")
    assertthat::assert_that(
      COMM_LIVESTOCK %in% unique(L100.GTAPCostShare_AgLU_reg_comm$GCAM_commodity) %>%
        all()
    )

    L100.GTAPCostShare_AgLU_reg_comm %>%
      # complete year (historical) and simply fill missing
      complete(nesting(GCAM_region_ID, GCAM_commodity, input), year = HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, GCAM_commodity, input) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      left_join_error_no_match(
        GCAM_region_names, by = "GCAM_region_ID") ->
      L2082.Ag_CostShare_R_Yh

    bind_rows(
      # Crops: retain all GTAP inputs except Land, Resource (endowment), and Chemicals
      # (Chemicals are removed because fertilizer is tracked separately in GCAM).
      # AgLU (crop self-use) and Water are retained because they are part of the NLC
      # that L2052 already processed — keeping them ensures the renormalization below
      # yields shares consistent with that cost structure.
      L2082.Ag_CostShare_R_Yh %>%
        filter(!GCAM_commodity %in% COMM_LIVESTOCK) %>%
        filter(!(input %in% c("Land", "Resource", "Chemicals")) ),

      # Livestock: use only primary on-farm GTAP inputs (no processing, no feed, no pasture/water).
      # _Proc inputs (Capital_Proc, Labor_Proc) represent meat/dairy processing costs, which
      # are not captured in GCAM's livestock module and are excluded for consistency with USDA data.
      # AgLU (feed intermediates), Land (pasture), Water, and Resource are also excluded because
      # they represent feed and land costs, not the non-feed NLC being partitioned here.
      L2082.Ag_CostShare_R_Yh %>%
        filter(GCAM_commodity %in% COMM_LIVESTOCK) %>%
        filter(!grepl("_Proc", input)) %>%
        filter(!(input %in% c("AgLU", "Land", "Water", "Resource")) )
    ) %>%
      # Renormalize so shares sum to 1 within each region-commodity-year, then extract Labor and Capital.
      # Result: Labor_Ag and Capital_Ag are shares of the relevant non-land cost pool.
      # E.g., Capital_Ag = 0.30 means 30% of NLC (or non-feed cost for livestock) is attributed to capital.
      group_by(GCAM_region_ID, region, year, GCAM_commodity) %>%
      mutate(value = value / sum(value)) %>% ungroup() %>%
      filter(input %in% c("Labor", "Capital")) %>%
      mutate(input = paste(input, "Ag", sep =  "_")) %>%
      spread(input, value) %>%
      filter(year %in% MODEL_BASE_YEARS) ->
      L2082.Ag_labor_capital_cost_share_R_Yh

    ## Apply regional labor-capital adjustments from configuration file ----
    # GTAP cost shares do not always align well with national factor use data (e.g., USDA IAP,
    # FAO value-added statistics). Region-specific multipliers and share overrides are applied
    # to correct known biases. Adjustments are defined in GTAP_labor_capital_adjustments.csv:
    # one row per region-commodity-condition combination. Each row specifies:
    #   total_multiplier: scales the combined (Labor + Capital) share before splitting
    #   labor_share / capital_share: override shares (as fractions of the scaled total)
    #   capital_direct_multiplier: scales Capital_Ag directly (used when capital_share is NA)
    #   condition: "all", "ratio>1", "ratio<1", or "ratio<0.5" (ratio = Capital_Ag / Labor_Ag)

    assertthat::assert_that(
      all(GTAP_labor_capital_adjustments$region %in%
            unique(L2082.Ag_labor_capital_cost_share_R_Yh$region)),
      msg = "GTAP_labor_capital_adjustments contains region(s) not found in data"
    )

    L2082.Ag_labor_capital_cost_share_R_Yh <-
      L2082.Ag_labor_capital_cost_share_R_Yh %>%
      mutate(
        Ratio_CL = Capital_Ag / Labor_Ag,
        CL_Total_Share = Capital_Ag + Labor_Ag,
        commodity_type = if_else(GCAM_commodity %in% COMM_LIVESTOCK, "livestock", "crops")
      )

    # Apply adjustments for each region and commodity type combination
    for(i in 1:nrow(GTAP_labor_capital_adjustments)) {
      adj <- GTAP_labor_capital_adjustments[i, ]

      # Identify which rows this CSV rule applies to
      # "all" in commodity_type means apply to both livestock and crops
      region_match <- L2082.Ag_labor_capital_cost_share_R_Yh$region == adj$region
      if(adj$commodity_type == "all") {
        commodity_match <- rep(TRUE, nrow(L2082.Ag_labor_capital_cost_share_R_Yh))  # matches all commodity types
      } else {
        commodity_match <- L2082.Ag_labor_capital_cost_share_R_Yh$commodity_type == adj$commodity_type
      }
      mask <- region_match & commodity_match

      if(!any(mask)) next

      # Check condition
      ratio_cl <- L2082.Ag_labor_capital_cost_share_R_Yh$Ratio_CL[mask]
      applies_cond <- case_when(
        adj$condition == "all" ~ TRUE,
        adj$condition == "ratio<1" ~ ratio_cl < 1,
        adj$condition == "ratio>1" ~ ratio_cl > 1,
        adj$condition == "ratio<0.5" ~ ratio_cl < 0.5,
        TRUE ~ FALSE
      )

      final_mask <- mask
      final_mask[mask] <- applies_cond

      if(!any(final_mask)) next

      # Apply adjustments
      cl_total <- L2082.Ag_labor_capital_cost_share_R_Yh$CL_Total_Share[final_mask]

      # Step 1: Apply total_multiplier
      if(!is.na(adj$total_multiplier)) {
        cl_total <- cl_total * adj$total_multiplier
      }

      # Step 2: Apply direct capital multiplier
      if(!is.na(adj$capital_direct_multiplier) & adj$capital_direct_multiplier != 1.0) {
        L2082.Ag_labor_capital_cost_share_R_Yh$Capital_Ag[final_mask] <-
          L2082.Ag_labor_capital_cost_share_R_Yh$Capital_Ag[final_mask] * adj$capital_direct_multiplier
      }

      # Step 3 & 4: Apply labor and capital shares
      if(!is.na(adj$labor_share)) {
        L2082.Ag_labor_capital_cost_share_R_Yh$Labor_Ag[final_mask] <- cl_total * adj$labor_share
      }
      if(!is.na(adj$capital_share)) {
        L2082.Ag_labor_capital_cost_share_R_Yh$Capital_Ag[final_mask] <- cl_total * adj$capital_share
      }
    }

    L2082.Ag_labor_capital_cost_share_R_Yh <-
      L2082.Ag_labor_capital_cost_share_R_Yh %>%
      select(names(L2082.Ag_labor_capital_cost_share_R_Yh)[!names(L2082.Ag_labor_capital_cost_share_R_Yh) %in%
                                                            c("Ratio_CL", "CL_Total_Share", "commodity_type")])

    ## output *L2082.Ag_labor_capital_cost_share_R_Yh ----

    # That is, split nonLandVariableCost into labor, capital costs and the remaining nonLandVariableCost
    # Using predefined cost shares above
    # adding a minimum value of labor and capital costs of 0.003 (crops),
    # 0.01 (livestock) or 2 (forest) 1975$ per unit of output And adjust nonLandVariableCost
    # Then join production and derive labor and capital expenditure

    # Step 2. Process labor and capital demand data at tech levels for crop and livestock ----

    ## a. nonbio crops  ----
    L2062.AgCost_ag_irr_mgmt_adj %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(
        L2082.Ag_labor_capital_cost_share_R_Yh %>%
          rename(AgSupplySector = GCAM_commodity),
        by = c("region", "AgSupplySector", "year")) %>%
      # apply cost shares to get unit cost
      mutate(Labor_UC = nonLandVariableCost * Labor_Ag,
             Capital_UC = nonLandVariableCost * Capital_Ag,
             # upstream processing may have negative other cost
             # Adding a min value of 0.003 for UnitCost of Labor (1975$/kg)
             Labor_UC = pmax(0.003, Labor_UC),
             Capital_UC = pmax(0.003, Capital_UC),
             # if nonLandVariableCost was zero, e.g., no production, change IO to zero as well
             Labor_UC = if_else(nonLandVariableCost == 0, 0, Labor_UC),
             Capital_UC = if_else(nonLandVariableCost == 0, 0, Capital_UC),
             nonLandVariableCost = nonLandVariableCost - Labor_UC - Capital_UC) %>%
      select(-Capital_Ag, -Labor_Ag, -GCAM_region_ID) ->
      L2082.laborcapital_ag_irr_mgmt_value_chain_Yh_0

      # Join production data to calculate factor expenditures for base years
    L2012.AgProduction_ag_irr_mgmt %>%
      transmute(region, year, prod_Mt = calOutputValue, AgProductionTechnology) %>%
      left_join_error_no_match(
        L2082.laborcapital_ag_irr_mgmt_value_chain_Yh_0,
        by = c("region", "AgProductionTechnology", "year") ) %>%
      mutate(Exp_Labor_Bil1975USD = Labor_UC * prod_Mt,
             Exp_Capital_Bil1975USD = Capital_UC * prod_Mt) ->
      L2082.laborcapital_ag_irr_mgmt_value_chain_Yh

    ## b. purpose-grown energy crops  ----
    L2062.AgCost_bio_irr_mgmt_adj %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(
        L2082.Ag_labor_capital_cost_share_R_Yh %>%
          rename(AgSupplySector = GCAM_commodity),
        by = c("region", "AgSupplySector", "year")) %>%
      # apply cost shares to get unit cost
      mutate(Labor_UC = nonLandVariableCost * Labor_Ag,
             Capital_UC = nonLandVariableCost * Capital_Ag,
             # Adding a min value of 0.003 for UnitCost of Labor (1975$/kg)
             Labor_UC = pmax(0.003, Labor_UC),
             Capital_UC = pmax(0.003, Capital_UC),
             # if nonLandVariableCost was zero, e.g., no production, change IO to zero as well
             Labor_UC = if_else(nonLandVariableCost == 0, 0, Labor_UC),
             Capital_UC = if_else(nonLandVariableCost == 0, 0, Capital_UC),
             nonLandVariableCost = nonLandVariableCost - Labor_UC - Capital_UC) %>%
      select(-Capital_Ag, -Labor_Ag, -GCAM_region_ID) ->
      L2082.laborcapital_bio_irr_mgmt_Yh
    # purpose-grown energy crop has no production in hist years
    # So this data is only for updating cost later

    ## c. livestock sectors  ----
    L202.StubTechCost_an %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(
        L2082.Ag_labor_capital_cost_share_R_Yh %>%
          rename(supplysector = GCAM_commodity),
        by = c("region", "supplysector", "year")) %>%
      # apply cost shares to get unit cost
      mutate(Labor_UC = input.cost * Labor_Ag,
             Capital_UC = input.cost * Capital_Ag,
             # upstream processing may have negative other cost
             # Adding a min value of 0.01 for UnitCost of Labor (1975$/kg)
             Labor_UC = pmax(0.01, Labor_UC),
             Capital_UC = pmax(0.01, Capital_UC),
             # if nonLandVariableCost was zero, e.g., no production, change IO to zero as well
             Labor_UC = if_else(input.cost == 0, 0, Labor_UC),
             Capital_UC = if_else(input.cost == 0, 0, Capital_UC),
             input.cost = input.cost - Labor_UC - Capital_UC) %>%
      select(-Capital_Ag, -Labor_Ag, -GCAM_region_ID) ->
      L2082.laborcapital_an_value_chain_Yh_0

    # Join production data to calculate factor expenditures for base years
    L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      transmute(region, year, prod_Mt = value, supplysector = GCAM_commodity,
                subsector = system, stub.technology = feed) %>%
      left_join_error_no_match(
        L2082.laborcapital_an_value_chain_Yh_0,
        by = c("region", "supplysector", "subsector", "stub.technology", "year") ) %>%
      mutate(Exp_Labor_Bil1975USD = Labor_UC * prod_Mt,
             Exp_Capital_Bil1975USD = Capital_UC * prod_Mt) ->
      L2082.laborcapital_an_value_chain_Yh

    ## d. Complete the value chain: add revenue and land cost estimates ----

    ### Producer prices (final base year only, for value chain tracing) ----
    L1321.ag_prP_R_C_75USDkg %>%
      bind_rows(L1321.an_prP_R_C_75USDkg) %>%
      bind_rows(L1321.expP_R_F_75USDm3) %>%
      select(region, GCAM_commodity, calPrice = value) ->
      L2082.AgPrices_R_C_FBY

    ### Estimate land cost for value chain tracing (land share of price) ----
    # Use the GTAP Land (for crops/livestock) or Resource (for forest/fish) share of total cost,
    # renormalized, then multiplied by producer price to get an estimated land cost per unit output.
    # This is used for value chain tracing only and does not affect GCAM's land value calculation
    # (which remains determined residually from revenue minus all other costs).
    L2082.Ag_CostShare_R_Yh %>%
      group_by(region, GCAM_commodity, year) %>%
      mutate(value = value / sum(value)) %>% ungroup() %>%
      filter(input %in% c("Land", "Resource"),
             !(GCAM_commodity %in% c("Forest", "OtherMeat_Fish") & input == "Land"),
             !(!GCAM_commodity %in% c("Forest", "OtherMeat_Fish") & input == "Resource") ) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      right_join(L2082.AgPrices_R_C_FBY,
                 by = c("GCAM_commodity", "region")) %>%
      transmute(region, GCAM_commodity, calPrice, landcostest = value * calPrice) ->
      L2082.AgPrices_R_C_FBY


    ### Assemble crop and livestock value chain (expenditure + revenue by commodity) ----

    L2082.laborcapital_ag_irr_mgmt_value_chain_Yh %>%
      left_join_error_no_match(
        L2082.AgPrices_R_C_FBY %>% rename(AgSupplySector = GCAM_commodity),
        by = c("region", "AgSupplySector")) %>%
      transmute(region, GCAM_commodity = AgSupplySector,  year,
                Exp_Labor_Bil1975USD,
                Exp_Capital_Bil1975USD,
                Exp_Land_Bil1975USD = prod_Mt * landcostest,
                Rev_Bil1975USD = prod_Mt * calPrice) %>%
      group_by(region, GCAM_commodity, year) %>%
      summarize(across(Exp_Labor_Bil1975USD:Rev_Bil1975USD, ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
      mutate(AgMajorSector = "CROP") %>%
      bind_rows(
        L2082.laborcapital_an_value_chain_Yh %>%
          left_join_error_no_match(
            L2082.AgPrices_R_C_FBY %>% rename(supplysector = GCAM_commodity),
            by = c("region", "supplysector")) %>%
          transmute(region, GCAM_commodity = supplysector,  year,
                    Exp_Labor_Bil1975USD,
                    Exp_Capital_Bil1975USD,
                    # estimate pasture land value
                    Exp_Land_Bil1975USD = prod_Mt * landcostest,
                    Rev_Bil1975USD = prod_Mt * calPrice) %>%
          group_by(region, GCAM_commodity, year) %>%
          summarize(across(Exp_Labor_Bil1975USD:Rev_Bil1975USD, ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
          mutate(AgMajorSector = "LIVESTOCK")
      ) ->
      L2082.laborcapital_ag_an_value_chain_C_Yh

    L2082.laborcapital_ag_an_value_chain_C_Yh %>%
      group_by(region, AgMajorSector, year) %>%
      summarize_at(vars(Exp_Labor_Bil1975USD, Exp_Capital_Bil1975USD,
                        Exp_Land_Bil1975USD, Rev_Bil1975USD), sum) %>%
      ungroup ->
      L2082.laborcapital_ag_an_value_chain_C_Yh_Check

    # Step 3: Derive implicit regional factor prices ----
    # Factor price = total expenditure (bil 1975$) / physical factor quantity (from L103)
    #   Labor wage:            K1975$ / person  = (bil 1975$ / Mppl) * 10^6
    #   Capital rate of return: 1975$ / 1975$   = (bil 1975$ / bil 1975$) * 10^9 / 10^9
    # Only crop and livestock expenditures are used here (forest prices are derived from the same prices)

    # Sum labor and capital expenditures across all crop and livestock commodities
    L2082.laborcapital_ag_an_value_chain_C_Yh %>%
      group_by(region, year) %>%
      summarize_at(vars(Exp_Labor_Bil1975USD, Exp_Capital_Bil1975USD), sum) %>%
      ungroup ->
      L2082.laborcapital_cost_R_Yh

    L2082.laborcapital_cost_R_Yh %>%
      # Join quantity
      left_join_error_no_match(
        L103.Ag_LaborCapital_R_AgMajorSector_Yh %>%
          filter(AgMajorSector == "CROPLIVESTOCK") %>%
          spread(factor, value) %>%
          select(region, year, capital_1975USD, labor_ppl),
        by = c("region", "year") ) %>%
      mutate(PriceLabor_K1975USDPerppl = Exp_Labor_Bil1975USD / labor_ppl * 10^6 ,
             PriceCapital_USDPerUSD = Exp_Capital_Bil1975USD / capital_1975USD * 10^9) %>%
      select(region, year, PriceLabor_K1975USDPerppl, PriceCapital_USDPerUSD) ->
      L2082.laborcapital_price

    L2082.laborcapital_price %>%
      select(region, year, price.L = PriceLabor_K1975USDPerppl) ->
      L2082.laborprice_Yh #  unit: 1975 K$/ppl

    L2082.laborcapital_price %>%
      select(region, year, price.K = PriceCapital_USDPerUSD) ->
      L2082.capitalprice_Yh # unit: 1975$/1975$

    # Step 4: Compute IO coefficients (unit cost / factor price) ----

    ## a. Crops: differentiate IO coefficients between hi- and lo-management technologies ----
    # All techs within a region-commodity share the same unit costs from Step 2. A theta parameter
    # shifts capital from lo-tech to hi-tech within each IRR/RFD group, reflecting that
    # high-management farming is more capital-intensive:
    #   delta     = theta * Capital_UC[hi]
    #   Capital_UC[hi] += delta          (hi-tech: more capital)
    #   Capital_UC[lo] -= delta/ProdRatio (lo-tech: less capital, scaled by output ratio
    #                                      to approximately conserve total expenditure)
    #   Labor_UC  shifts in the opposite direction to keep total unit cost constant.
    # theta_IRR > theta_RFD because irrigated systems show greater hi/lo differentiation.

    theta_IRR = 0.2
    theta_RFD = 0.1

    L2082.laborcapital_ag_irr_mgmt_value_chain_Yh %>%
      mutate(Irr_Rfd = if_else(grepl("_RFD_", AgProductionTechnology), "rfd", "irr"),
             level = if_else(grepl("_hi$", AgProductionTechnology), "hi", "lo") ) %>%
      select(-nonLandVariableCost, -Exp_Capital_Bil1975USD, -Exp_Labor_Bil1975USD) %>%
      left_join_error_no_match(
        L2082.laborcapital_price, by = c("region", "year"))%>%
      mutate(theta = if_else(Irr_Rfd == "irr", theta_IRR, theta_RFD)) %>%
      group_by(region, year, AgSupplySector, AgSupplySubsector, Irr_Rfd) %>%
      # ProdRatio = lo-tech production / hi-tech production; used to scale the delta transfer so that
      # total expenditure (UC * production) is approximately conserved. Set to 1 when production is zero.
      mutate(ProdRatio = if_else(prod_Mt == 0, 1, prod_Mt / prod_Mt [level == "hi"]),
             delta = theta * Capital_UC[level == "hi"],
             delta = if_else(level == "hi", delta, -delta/ProdRatio) ) %>%
      mutate(Capital_UC = Capital_UC + delta,
             Labor_UC = Labor_UC - delta) %>%
      ungroup() %>%
      # IO units: Mppl per Mt for labor; B$ per Mt ($/kg) for capital
      mutate(Labor_IO = Labor_UC/PriceLabor_K1975USDPerppl,
             Capital_IO = Capital_UC/PriceCapital_USDPerUSD,
             Labor_input_Mppl = Labor_IO * prod_Mt,
             Capital_input_Bil1975USD = Capital_IO * prod_Mt) ->
      L2082.AgCoef_laborcapital_ag_irr_mgmt_hist_0

    # generate AgCoef
    L2082.AgCoef_laborcapital_ag_irr_mgmt_hist_0 %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year,
             Labor_Ag = Labor_IO,
             Capital_Ag = Capital_IO) %>%
      tidyr::gather(minicam.energy.input, coefficient, Labor_Ag, Capital_Ag) ->
      L2082.AgCoef_laborcapital_ag_irr_mgmt_hist

    # for total labor and capital verification
    L2082.AgCoef_laborcapital_ag_irr_mgmt_hist_0 %>%
      group_by(region, year) %>%
      summarize(Labor_input_Mppl = sum(Labor_input_Mppl),
                Capital_input_Bil1975USD = sum(Capital_input_Bil1975USD),
                .groups = "drop") %>%
      mutate(AgMajorSector = "CROP") ->
      L2082.Ag_LaborCapital_R_AgMajorSector_Yh_CROP


    ## b. Bioenergy crops IO coefficients ----
    # Same theta-based hi/lo differentiation as food crops. Because bioenergy crops have no
    # historical production, ProdRatio is set to 1 (equal weighting between hi and lo tech).

    L2082.laborcapital_bio_irr_mgmt_Yh %>%
      select(-nonLandVariableCost) %>%
      left_join_error_no_match(
        L2082.laborcapital_price, by = c("region", "year")) %>%
      mutate(Irr_Rfd = if_else(grepl("IRR", AgProductionTechnology), "irr", "rfd"),
             level = if_else(grepl("hi", AgProductionTechnology), "hi", "lo")) %>%
      mutate(theta = if_else(Irr_Rfd == "irr", theta_IRR, theta_RFD)) %>%
      group_by(region, year, AgSupplySector, AgSupplySubsector, Irr_Rfd) %>%
      mutate(ProdRatio = 1,
             delta = theta * Capital_UC[level == "hi"],
             delta = if_else(level == "hi", delta, -delta/ProdRatio) ) %>%
      mutate(Capital_UC = Capital_UC + delta,
             Labor_UC = Labor_UC - delta) %>%
      ungroup() %>%
      # IO units: Mppl per Mt for labor; $ / $ for capital
      mutate(Labor_IO = Labor_UC/PriceLabor_K1975USDPerppl,
             Capital_IO = Capital_UC/PriceCapital_USDPerUSD)  %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year,
             Labor_Ag = Labor_IO, Capital_Ag = Capital_IO) %>%
      tidyr::gather(minicam.energy.input, coefficient, Labor_Ag, Capital_Ag) ->
      L2082.AgCoef_laborcapital_bio_irr_mgmt_hist

    ## c. Livestock IO coefficients ----
    # No theta-based hi/lo differentiation is applied for livestock: mixed vs. pastoral systems
    # already carry naturally different productivities and factor intensities via their distinct
    # unit costs from Step 2.

    L2082.laborcapital_an_value_chain_Yh %>%
      select(-input.cost, -minicam.non.energy.input) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(
        L2082.laborcapital_price, by = c("region", "year")) %>%
      # compute IO coefficients
      # IO units: Mppl per Mt for labor; $ / kg for capital
      mutate(Labor_IO = Labor_UC/PriceLabor_K1975USDPerppl,
             Capital_IO = Capital_UC/PriceCapital_USDPerUSD,
             Labor_input_Mppl = Labor_IO * prod_Mt,
             Capital_input_Bil1975USD = Capital_IO * prod_Mt) ->
      L2082.StubTechCoef_laborcapital_an_hist_0

    # generate AgCoef
    L2082.StubTechCoef_laborcapital_an_hist_0 %>%
      select(region, year, supplysector, subsector, stub.technology,
             Labor_Ag = Labor_IO,
             Capital_Ag = Capital_IO) %>%
      tidyr::gather(minicam.energy.input, coefficient, Labor_Ag, Capital_Ag) %>%
      mutate(market.name = region) ->
      L2082.StubTechCoef_laborcapital_an_hist

    # for total labor and capital verification
    L2082.StubTechCoef_laborcapital_an_hist_0 %>%
      group_by(region, year) %>%
      summarize(Labor_input_Mppl = sum(Labor_input_Mppl),
                Capital_input_Bil1975USD = sum(Capital_input_Bil1975USD),
                .groups = "drop") %>%
      mutate(AgMajorSector = "LIVESTOCK") ->
      L2082.Ag_LaborCapital_R_AgMajorSector_Yh_LIVESTOCK



    ## d. Forest IO coefficients ----
    # Forest factor IOs are derived directly from physical quantities (L103 FOREST sector):
    #   Labor IO  = regional labor (persons) / regional production (bm3)     [Mppl / bm3]
    #   Capital IO = regional capital stock (bil 1975$) / regional production  [bil 1975$ / bm3]
    # A single IO applies uniformly to all forest technologies within a region.
    L2012.AgProduction_For %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      group_by(region, year) %>%
      mutate(RegionalProd = sum(calOutputValue)) %>%
      # Join regional total forest labor
      left_join_error_no_match(
        L103.Ag_LaborCapital_R_AgMajorSector_Yh %>%
          filter(AgMajorSector == "FOREST") %>%
          spread(factor, value) %>%
          select(region, year, labor_ppl, capital_1975USD),
        by = c("region", "year")) %>%
      mutate(Labor_IO = if_else(calOutputValue == 0, 0, labor_ppl / 10^6 / RegionalProd) , # labor IO: # mpl / bm3
             Capital_IO = if_else(calOutputValue == 0, 0, capital_1975USD / 10^9 / RegionalProd), # capital IO: # bil 1975$ / bm3
             Labor_input_Mppl = Labor_IO * calOutputValue,
             Capital_input_Bil1975USD = Capital_IO * calOutputValue) ->
      L2082.AgCoef_laborcapital_for_hist_0

    # generate AgCoef
    L2082.AgCoef_laborcapital_for_hist_0 %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year,
             Labor_Ag = Labor_IO, Capital_Ag = Capital_IO) %>%
        tidyr::gather(minicam.energy.input, coefficient, Labor_Ag, Capital_Ag) %>%
        ungroup() ->
      L2082.AgCoef_laborcapital_for_hist

    # for total labor and capital verification
    L2082.AgCoef_laborcapital_for_hist_0 %>%
      group_by(region, year) %>%
      summarize(Labor_input_Mppl = sum(Labor_input_Mppl),
                Capital_input_Bil1975USD = sum(Capital_input_Bil1975USD),
                .groups = "drop") %>%
      mutate(AgMajorSector = "FOREST") ->
      L2082.Ag_LaborCapital_R_AgMajorSector_Yh_FOREST


    # Step 5: Forest value chain — back-calculate unit costs from IO coefficients and factor prices ----
    # (Parallel to Step 2 for crops/livestock, but forest IOs are derived first in Step 4d.)
    # Unit cost = IO coefficient * factor price; subtract from NLC to get residual cost.

    L2082.AgCoef_laborcapital_for_hist %>%
      spread(minicam.energy.input, coefficient) %>%
      left_join_error_no_match(L2082.laborprice_Yh, by = c("region", "year")) %>%
      left_join_error_no_match(L2082.capitalprice_Yh, by = c("region", "year")) %>%
      mutate(Labor_UC = price.L * Labor_Ag,
             Capital_UC = price.K * Capital_Ag) %>%
      left_join_error_no_match(
        L2052.AgCost_For,
        by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology","year")) %>%
      # if nonLandVariableCost was zero, e.g., no production, change IO to zero as well
      mutate(Labor_UC = if_else(nonLandVariableCost == 0, 0, Labor_UC),
             Capital_UC = if_else(nonLandVariableCost == 0, 0, Capital_UC),
             nonLandVariableCost = nonLandVariableCost - Labor_UC - Capital_UC) %>%
      select(-Capital_Ag, -Labor_Ag) %>%
      left_join_error_no_match(
        L2012.AgProduction_For,
        by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%
      mutate(Exp_Labor_Bil1975USD = Labor_UC * calOutputValue, # $/m3 * bm3 = bil1975USD
             Exp_Capital_Bil1975USD = Capital_UC * calOutputValue) ->
      L2082.laborcapital_for_value_chain_Yh

    L2082.laborcapital_for_value_chain_Yh %>%
      left_join_error_no_match(
        L2082.AgPrices_R_C_FBY %>% rename(AgSupplySector = GCAM_commodity),
        by = c("region", "AgSupplySector"))  %>%
      transmute(region, GCAM_commodity = AgSupplySector,  year,
                Exp_Labor_Bil1975USD,
                Exp_Capital_Bil1975USD,
                Exp_Land_Bil1975USD = calOutputValue * landcostest,
                Rev_Bil1975USD = calOutputValue * calPrice) %>%
      group_by(region, GCAM_commodity, year) %>%
      summarize(across(Exp_Labor_Bil1975USD:Rev_Bil1975USD, ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
      mutate(AgMajorSector = "FOREST") %>%
      select(-GCAM_commodity) ->
      L2082.laborcapital_for_value_chain_C_Yh_Check

    # Step 6: Verify internal consistency of value chain and factor prices ----
    # Assemble expenditures and IO-based factor inputs across all three sectors (CROP, LIVESTOCK, FOREST).
    # Factor prices implied by (expenditure / IO-based quantity) must match those from Step 3.

    bind_rows(
      L2082.laborcapital_ag_an_value_chain_C_Yh_Check,
      L2082.laborcapital_for_value_chain_C_Yh_Check) %>%
      left_join_error_no_match(
        L2082.Ag_LaborCapital_R_AgMajorSector_Yh_CROP %>%
          bind_rows(L2082.Ag_LaborCapital_R_AgMajorSector_Yh_LIVESTOCK) %>%
          bind_rows(L2082.Ag_LaborCapital_R_AgMajorSector_Yh_FOREST),
        by = c("region", "year", "AgMajorSector")
      ) %>%
      mutate(PriceLabor_K1975USDPerppl = Exp_Labor_Bil1975USD / Labor_input_Mppl ,
             PriceCapital_USDPerUSD = Exp_Capital_Bil1975USD / Capital_input_Bil1975USD) ->
      L2082.Ag_LaborCapital_R_AgMajorSector_Yh_ValueAdded

    # Assert: prices implied by IO-based value chain must match Step 3 prices within tolerance
    assertthat::assert_that(
      L2082.Ag_LaborCapital_R_AgMajorSector_Yh_ValueAdded %>%
        select(names(L2082.laborcapital_price)) %>%
        gather(var, price1, -region, -year) %>%
        left_join_error_no_match(
          L2082.laborcapital_price %>%
            gather(var, price0, -region, -year),
          by = c("region", "year", "var") ) %>%
        mutate(diff = abs(price1 - price0)) %>%
        filter(diff > 0.001) %>% nrow() == 0 )


    # Step 7: Produce residual NLC outputs for all model years ----
    # The residual NLC (NLC minus labor and capital unit costs) is held constant at the last
    # historical base year value and extended to all MODEL_YEARS for downstream GCAM processing.

    ## a. nonbio crops -----
    L2082.laborcapital_ag_irr_mgmt_value_chain_Yh %>%
      select(names(L2062.AgCost_ag_irr_mgmt_adj)) %>%
      complete(nesting(!!!rlang::syms(setdiff(names(.), c("year", "nonLandVariableCost")))),
               year = MODEL_YEARS) %>%
      group_by_at(vars(-nonLandVariableCost, -year)) %>%
      fill(nonLandVariableCost, .direction = "down") %>%
      ungroup ->
      L2082.AgCost_ag_irr_mgmt_adj

    ## b. purpose-grown energy crops  ----
    L2082.laborcapital_bio_irr_mgmt_Yh %>%
      select(names(L2062.AgCost_bio_irr_mgmt_adj)) %>%
      complete(nesting(!!!rlang::syms(setdiff(names(.), c("year", "nonLandVariableCost")))),
               year = MODEL_YEARS) %>%
      group_by_at(vars(-nonLandVariableCost, -year)) %>%
      fill(nonLandVariableCost, .direction = "down") %>%
      ungroup->
      L2082.AgCost_bio_irr_mgmt_adj

    ## c. livestock sectors  ----
    L2082.laborcapital_an_value_chain_Yh %>%
      select(names(L202.StubTechCost_an)) %>%
      complete(nesting(!!!rlang::syms(setdiff(names(.), c("year", "input.cost")))),
               year = MODEL_YEARS) %>%
      group_by_at(vars(-input.cost, -year)) %>%
      fill(input.cost, .direction = "down") %>%
      ungroup->
      L2082.StubTechCost_an_adj

    ## d. forest ----
    L2082.laborcapital_for_value_chain_Yh %>%
      select(names(L2052.AgCost_For)) %>%
      complete(nesting(!!!rlang::syms(setdiff(names(.), c("year", "nonLandVariableCost")))),
               year = MODEL_YEARS) %>%
      group_by_at(vars(-nonLandVariableCost, -year)) %>%
      fill(nonLandVariableCost, .direction = "down") %>%
      ungroup ->
      L2082.AgCost_For_adj


    # Produce outputs ----
    # value added tracing ----
    L2082.Ag_LaborCapital_R_AgMajorSector_Yh_ValueAdded %>%
      add_title("Ag labor and capital value added and input tracing by CROP, LIVESTOCK, and FOREST") %>%
      add_units("billion 1975 USD / Mppl /price") %>%
      add_comments("Note: expenditure divided by factor price gives factor input level") %>%
      add_legacy_name("L2082.Ag_LaborCapital_R_AgMajorSector_Yh_ValueAdded") %>%
      add_precursors(MODULE_INPUTS) ->
      L2082.Ag_LaborCapital_R_AgMajorSector_Yh_ValueAdded

    ## labor and capital demand data at tech levels  ----
    L2082.laborcapital_ag_irr_mgmt_value_chain_Yh %>%
      add_title("labor and capital expenditure of crop production") %>%
      add_units("billion 1975 USD") %>%
      add_comments("Note: expenditure divided by factor price gives factor input level") %>%
      add_legacy_name("L2082.laborcapital_ag_irr_mgmt_value_chain_Yh") %>%
      add_precursors("L100.GTAPCostShare_AgLU_reg_comm",
                     "L2062.AgCost_ag_irr_mgmt_adj",
                     "L2012.AgProduction_ag_irr_mgmt") ->
      L2082.laborcapital_ag_irr_mgmt_value_chain_Yh

    L2082.laborcapital_an_value_chain_Yh %>%
      add_title("labor and capital expenditure of livestock production") %>%
      add_units("billion 1975 USD") %>%
      add_comments("Note: expenditure divided by factor price gives factor input level") %>%
      add_legacy_name("L2082.laborcapital_an_value_chain_Yh") %>%
      add_precursors("L100.GTAPCostShare_AgLU_reg_comm",
                     "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
                     "L202.StubTechCost_an") ->
      L2082.laborcapital_an_value_chain_Yh

    L2082.laborcapital_for_value_chain_Yh %>%
      add_title("labor and capital expenditure for forest") %>%
      add_units("billion 1975 USD") %>%
      add_comments("Note: expenditure divided by factor price gives factor input level") %>%
      add_legacy_name("L2082.laborcapital_for_value_chain_Yh") %>%
      add_precursors("L2012.AgProduction_For",
                     "L100.GTAPCostShare_AgLU_reg_comm",
                     "L2052.AgCost_For") ->
      L2082.laborcapital_for_value_chain_Yh

    ## factor prices for labor and capital ----
    L2082.laborprice_Yh %>%
      add_title("Regional agricultural labor market price") %>%
      add_units("1975$/pers") %>%
      add_comments("Calibrated historical Ag Labor prices") %>%
      add_precursors("L100.GTAPCostShare_AgLU_reg_comm") ->
      L2082.laborprice_Yh

    L2082.capitalprice_Yh %>%
      add_title("Regional agricultural capital market price") %>%
      add_units("1975 $/$") %>%
      add_comments("Calibrated historical Ag capital prices") %>%
      add_precursors("L100.GTAPCostShare_AgLU_reg_comm") ->
      L2082.capitalprice_Yh

    ## IO coefficients ----
    L2082.AgCoef_laborcapital_ag_irr_mgmt_hist %>%
      add_title("Labor and capital IO coefficients for crop technologies") %>%
      add_units("Mppl per Mt (labor); bil 1975$ per Mt (capital)") %>%
      add_comments("Theta method differentiates hi- vs. lo-management within IRR/RFD groups: hi-tech is more capital-intensive.") %>%
      add_legacy_name("L2082.AgCoef_laborcapital_ag_irr_mgmt_hist") %>%
      add_precursors("L100.GTAPCostShare_AgLU_reg_comm",
                     "L2062.AgCost_ag_irr_mgmt_adj") ->
      L2082.AgCoef_laborcapital_ag_irr_mgmt_hist

    L2082.AgCoef_laborcapital_bio_irr_mgmt_hist %>%
      add_title("Labor and capital IO coefficients for bioenergy crop technologies") %>%
      add_units("Mppl per Mt (labor); bil 1975$ per Mt (capital)") %>%
      add_comments("Same theta-based hi/lo differentiation as food crops; ProdRatio = 1 (no historical production).") %>%
      add_legacy_name("L2082.AgCoef_laborcapital_bio_irr_mgmt_hist") %>%
      add_precursors("L100.GTAPCostShare_AgLU_reg_comm",
                     "L2062.AgCost_bio_irr_mgmt_adj") ->
      L2082.AgCoef_laborcapital_bio_irr_mgmt_hist

    L2082.StubTechCoef_laborcapital_an_hist %>%
      add_title("Labor and capital IO coefficients for livestock technologies") %>%
      add_units("Mppl per Mt (labor); bil 1975$ per Mt (capital)") %>%
      add_comments("No theta differentiation; mixed vs. pastoral systems are naturally differentiated by production system.") %>%
      add_legacy_name("L2082.StubTechCoef_laborcapital_hist") %>%
      add_precursors("L202.StubTechCost_an") ->
      L2082.StubTechCoef_laborcapital_an_hist

    L2082.AgCoef_laborcapital_for_hist %>%
      add_title("Labor and capital IO coefficients for forest technologies") %>%
      add_units("Mppl per bm3 (labor); bil 1975$ per bm3 (capital)") %>%
      add_comments("Uniform IO per region; derived from L103 forest factor quantities divided by regional production.") %>%
      add_legacy_name("L2082.AgCoef_laborcapital_for_hist") %>%
      add_precursors("L2012.AgProduction_For",
                     "L103.Ag_LaborCapital_R_AgMajorSector_Yh") ->
      L2082.AgCoef_laborcapital_for_hist


    ## AgCost adjusted----
    L2082.AgCost_ag_irr_mgmt_adj %>%
      add_title("Residual non-land variable cost for crop technologies (after removing labor and capital)") %>%
      add_units("1975$ per kg") %>%
      add_comments("NLC_residual = NLC_input - Labor_UC - Capital_UC; held constant at last base year for all model years.") %>%
      add_legacy_name("L2082.AgCost_ag_irr_mgmt_adj") %>%
      same_precursors_as(L2082.AgCoef_laborcapital_ag_irr_mgmt_hist)  ->
      L2082.AgCost_ag_irr_mgmt_adj

    L2082.AgCost_bio_irr_mgmt_adj %>%
      add_title("Residual non-land variable cost for bioenergy crop technologies (after removing labor and capital)") %>%
      add_units("1975$ per GJ") %>%
      add_comments("NLC_residual = NLC_input - Labor_UC - Capital_UC; held constant at last base year for all model years.") %>%
      add_legacy_name("L2082.AgCost_bio_irr_mgmt_adj") %>%
      same_precursors_as(L2082.AgCoef_laborcapital_bio_irr_mgmt_hist) ->
      L2082.AgCost_bio_irr_mgmt_adj

    L2082.StubTechCost_an_adj %>%
      add_title("Residual non-feed cost for livestock technologies (after removing labor and capital)") %>%
      add_units("1975$/kg") %>%
      add_comments("input.cost_residual = input.cost - Labor_UC - Capital_UC; covers energy/materials but not feed, land, labor, or capital.") %>%
      add_legacy_name("L2082.StubTechCost_an_adj") %>%
      add_precursors("L100.GTAPCostShare_AgLU_reg_comm",
                     "L202.StubTechCost_an") ->
      L2082.StubTechCost_an_adj

    L2082.AgCost_For_adj %>%
      add_title("Residual non-land variable cost for forest technologies (after removing labor and capital)") %>%
      add_units("1975$ per m3") %>%
      add_comments("NLC_residual = NLC_input - Labor_UC - Capital_UC; Labor_UC and Capital_UC derived from IO coefficients times factor prices.") %>%
      add_legacy_name("L2082.AgCost_For_adj") %>%
      add_precursors("L100.GTAPCostShare_AgLU_reg_comm",
                     "L2052.AgCost_For") ->
      L2082.AgCost_For_adj

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
