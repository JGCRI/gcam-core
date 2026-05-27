# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_ag_capital_tracking_xml
#'
#' Construct XML data structure for \code{ag_capital_tracking.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_capital_tracking.xml}.
#'
#' @author DS Nov 2023 XZ Mar 2026

module_socio_ag_capital_tracking_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      "L100.FAO_Ag_Depreciation_Rate_R_Yh",
      "L2082.capitalprice_Yh")

  MODULE_OUTPUTS <-
    c(XML = "ag_capital_tracking.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # This module generates the capital tracking structure for agriculture ----

    # "Capital_Ag" represents capital demanded in agricultural production at the
    # technology level. The input coefficient is calibrated from base-year observations,
    # so we must represent the corresponding supply of "Capital_Ag".

    # In earlier work (e.g., Sheng et al., 2025 Nature Food), capital supply was treated
    # as unlimited, implying a fixed capital price (cost). Here, we instead specify an
    # endogenous capital supply.

    # The implementation follows the capital tracking approach used in the energy system.
    # We begin with a fixed "input.cost" per unit of capital (analogous to non-land or
    # non-energy costs), which would imply an unlimited capital supply curve if held
    # exogenous and constant over time.

    # In that case, capital stock (volume) would be determined by demand, while the cost
    # would be fully determined by the exogenous "input.cost". In this implementation,
    # however, the cost is endogenized and responds to the interest rate through the
    # investment–savings market clearing.

    # Specifically, the input.cost corresponds to the fixed charge rate (FCR), or gross
    # rate of return. We construct an agricultural capital supply sector to represent
    # this cost, which is then endogenized using the same mechanism developed for energy
    # technologies.

    # Accordingly, a "minicam-non-energy-input" cost is added for "Capital_Ag". The
    # initial value is not critical because a "pMultiplier" is introduced to match
    # observed capital costs in the base years. For now, this multiplier is held
    # constant over time.

    # The capital ratio is set to 100%, along with specified interest rates, payback
    # periods, and depreciation rates. This structure allows the model to calculate
    # new investment required by the aggregated agricultural sectors (on the supply
    # side).

    # The resulting net new agricultural investment demand enters the investment–
    # savings market clearing, where the solved capital rental price is linked to the
    # interest rate.

    # Although the output is named "ag_capital_tracking", it effectively represents
    # agricultural capital supply with endogenous cost adjustment. Capital is therefore
    # no longer fully malleable, as its price is determined at the margin through
    # investment.


    # Prepare macro data table ----
    Ag.Capital.Input <- "Capital_Ag"

    ## template ----

    tibble(input.unit = "billion$",
           output.unit = "billion$",
           price.unit = "1975$/$",
           supplysector = Ag.Capital.Input,
           subsector = Ag.Capital.Input,
           stub.technology = Ag.Capital.Input,
           logit.year.fillout = min(MODEL_BASE_YEARS),
           logit.exponent = gcam.DEFAULT_SUBSECTOR_LOGIT,
           logit.type = NA,
           minicam.non.energy.input = Ag.Capital.Input,
           capital.ratio = 1,
           interest.rate = socioeconomics.DEFAULT_INTEREST_RATE,
           payback.years = 10, # https://www.irs.gov/pub/irs-prior/p946--2019.pdf Appendix B
           invest.unit.conversion = 1,
           tracking.market = socioeconomics.AG_CAPITAL_MARKET_NAME,
           sector.name = Ag.Capital.Input,
           subsector.name = Ag.Capital.Input,
           technology = Ag.Capital.Input,
           share.weight = 1) %>%
      mutate(input.cost = calc_fixed_charge_rate(interest.rate, payback.years)) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      repeat_add_columns(GCAM_region_names %>% select(region)) ->
      Ag_capital_track_R_Yh_template

    ## Join prices and dep.rate ----
    # Join Ag capital rental prices which will be used for scaler (price multiplier calculation)
    # Also join regional depreciation rate from FAO
    Ag_capital_track_R_Yh_template %>%
      left_join_error_no_match(
        L2082.capitalprice_Yh %>% select(region, year, price.K),
        by = c("region", "year")) %>%
      left_join_error_no_match(
        L100.FAO_Ag_Depreciation_Rate_R_Yh %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
          transmute(region, year, depreciation.rate = dep.rate.Ag.fao),
        by = c("region", "year")) ->
      Ag_capital_track_R_Yh

    ## Add future years ----
    Ag_capital_track_R_Yh %>%
      complete(region, year = MODEL_YEARS) %>%
      group_by(region) %>% arrange(year) %>%
      fill(-all_of(c("region", "year")), .direction = "down") %>%
      ungroup() %>%
      mutate(pMult = price.K / input.cost) ->
      Ag_capital_track_R_Y



    # prepare XML exporting data according level 2 names ----
    Ag_capital_track_R_Y %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]]) %>%
      mutate(logit.type = NA) ->
      Ag_capital_sector

    Ag_capital_track_R_Y %>%
      filter(year == min(MODEL_BASE_YEARS)) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      Ag_capital_Subsector_shrwt

    Ag_capital_track_R_Y %>%
      filter(year <= MODEL_FINAL_BASE_YEAR) %>%
      select(LEVEL2_DATA_NAMES[["StubTechShrwt"]]) ->
      Ag_capital_StubTech_shrwt

    Ag_capital_track_R_Y %>%
      filter(year == min(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]]) %>%
      mutate(logit.type = NA) ->
      Ag_capital_Subsector_shrwt_logit

    Ag_capital_track_R_Y %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]) ->
      Ag_capital_GlobalTech_Shrwt

    Ag_capital_track_R_Y %>%
      select(LEVEL2_DATA_NAMES[["StubTechTrackCapital"]]) ->
      Ag_capital_StubTech_TrackCapital

    Ag_capital_track_R_Y %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]]) ->
      Ag_capital_StubTech_Cost

    Ag_capital_track_R_Y %>%
      select(LEVEL2_DATA_NAMES[["TechPmult"]]) ->
      Ag_capital_StubTech_Pmult


    # Produce outputs ----

    create_xml("ag_capital_tracking.xml") %>%
      add_logit_tables_xml(Ag_capital_sector, "Supplysector") %>%
      add_logit_tables_xml(Ag_capital_Subsector_shrwt_logit, "SubsectorLogit") %>%
      add_xml_data(Ag_capital_Subsector_shrwt, "SubsectorShrwtFllt") %>%
      add_xml_data(Ag_capital_GlobalTech_Shrwt, "GlobalTechShrwt") %>%
      add_xml_data(Ag_capital_StubTech_TrackCapital, "StubTechTrackCapital") %>%
      add_xml_data(Ag_capital_StubTech_Cost, "StubTechCost") %>%
      add_xml_data(Ag_capital_StubTech_Pmult, "TechPmult") %>%
      add_precursors(MODULE_INPUTS) ->
      ag_capital_tracking.xml

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
