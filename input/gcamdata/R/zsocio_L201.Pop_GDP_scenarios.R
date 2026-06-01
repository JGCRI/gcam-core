# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L201.Pop_GDP_scenarios
#'
#' GDP, Labor supply, productivity and population by scenario and region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.GDP_Scen}, \code{L201.Pop_Scen},
#' \code{L201.TotalEmployment_Scen}, \code{L201.LaborSupplyTech_Scen},
#' \code{L201.Labor_Rsrc}, \code{L201.Labor_Rsrc_price}, \code{L201.Labor_RsrcCurves},
#' \code{L201.LaborSupplySector}, \code{L201.LaborSupplySubSector}, \code{L201.LaborForceShare_Scen},
#' @details Produces historical and future population and GDP by region and SSP scenario,
#' and generate labor productivity and supply related files.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by lag mutate order_by select transmute
#' @author HM & RH June 2017 DS XZ 2025
module_socio_L201.Pop_GDP_scenarios <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "socioeconomics/gcam_macro_TFP_open",
      FILE = "socioeconomics/A_labor_supply_sector",
      "L101.Pop_thous_Scen_R_Y",
      "L102.gdp_mil90usd_Scen_R_Y",
      "L102.PPP_MER_R",
      "L103.LaborForceShare_Scen_R_Y",
      "L203.NationalAccounts")

  MODULE_OUTPUTS <-
    c("L201.GDP_Scen",
      "L201.Pop_Scen",
      "L201.TotalFactorProductivity_Scen",
      "L201.PPPConvert",
      "L201.TotalEmployment_Scen",
      "L201.LaborSupplyTech_Scen",
      "L201.Labor_Rsrc",
      "L201.Labor_Rsrc_price",
      "L201.Labor_RsrcCurves",
      "L201.Labor_RenewRsrcCalProd",
      "L201.Labor_ResTechShrwt",
      "L201.LaborSupplySector",
      "L201.LaborSupplySubSector",
      "L201.LaborForceShare_Scen")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    . <- GCAM_region_ID <-
      L201.TotalFactorProductivity_Scen <-  L201.Pop_Scen <-  PPPConvert <- PPP_MER <- baseGDP <-
      constRatio <- curr_table <- lag_pcgdp <- rate_pcgdp <- ratio_pcgdp <- region <- scenario <-
      timesteps <- totalPop <- value <- year <- cal.production <- NULL  # silence package check notes

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # (1) L201.Pop_Scen: pop for all scenarios ----

    L201.Pop_Scen <-
      L101.Pop_thous_Scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(totalPop = round(value, socioeconomics.POP_DIGITS)) %>%
      filter(year %in% MODEL_YEARS) %>% # delete unused years
      select(scenario, region, year, totalPop)

    L201.Pop_Scen %>%
      add_title("Population by scenario and region") %>%
      add_units("Thousand persons") %>%
      add_comments("Population by scenario and region") %>%
      add_legacy_name("L201.Pop_Scen") %>%
      add_precursors("common/GCAM_region_names",  "L101.Pop_thous_Scen_R_Y") ->
      L201.Pop_Scen


    # (2) L201.GDP_Scen: GDP for all scenarios ----

    L201.GDP_Scen <-
      L102.gdp_mil90usd_Scen_R_Y %>%
      filter(year %in% MODEL_YEARS) %>% # only keep model years
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(GDP = round(value, socioeconomics.GDP_DIGITS)) %>%
      select(scenario, region, year, GDP)

    L201.GDP_Scen %>%
      add_title("GDP by scenario, region and year") %>%
      add_units("Million 1990USD") %>%
      add_comments("The base GDP which will used when run in fixed or calibrated GDP modes") %>%
      add_comments("When in open GDP mode the actual GDP in modeled years may differ.") %>%
      add_legacy_name("L201.GDP_Scen") %>%
      add_precursors("common/GCAM_region_names",  "L102.gdp_mil90usd_Scen_R_Y") ->
      L201.GDP_Scen


    # (3) L201.TotalFactorProductivity_Scen----

    gcam_macro_TFP_open %>%
      select(scenario, region, year, productivity) %>%
      filter(!is.na(productivity)) ->
      L201.TotalFactorProductivity_Scen

    L201.TotalFactorProductivity_Scen %>%
      add_title("Total factor productivity by SSP and CORE scenarios") %>%
      add_units("Unitless") %>%
      add_comments("Values (if available) are calibrated from prior GCAM runs and therefore") %>%
      add_comments("could potentially no longer match.") %>%
      add_precursors("socioeconomics/gcam_macro_TFP_open") ->
      L201.TotalFactorProductivity_Scen


    # (4) L201.PPPConvert ----

    # Write out the PPP:MER conversion factors (purely used for reporting)
    L201.PPPConvert <- L102.PPP_MER_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      transmute(region,
                PPP.convert = round(PPP_MER, socioeconomics.LABOR_PRODUCTIVITY_DIGITS))

    L201.PPPConvert %>%
      add_title("Conversion factor from MER to PPP") %>%
      add_units("PPP/MER") %>%
      add_comments("Uses division of PPP by MER performed in L102.PPP_MER_R") %>%
      add_legacy_name("L201.PPPConvert") %>%
      add_precursors("common/GCAM_region_names", "L102.PPP_MER_R") ->
      L201.PPPConvert


    # (5) L201.TotalEmployment_Scen and L201.LaborSupplyTech_Scen ----
    ## Total employment and globel supply tech: Labor_Ag and Labor_Materials ----

    L201.Pop_Scen %>%
      left_join_error_no_match(
        L103.LaborForceShare_Scen_R_Y %>%
          filter(year %in% MODEL_YEARS) %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
          select(scenario, region, year, employed.share),
        by = c("scenario", "region", "year")) %>%
      transmute(scenario, region, year,
                fixed.final.demand = socioeconomics.LABOR_MARKET_NAME,
                service = totalPop * employed.share / 1000) ->
      L201.TotalEmployment_Scen

    #* L201.TotalEmployment_Scen
    L201.TotalEmployment_Scen %>%
      add_title("National Accounts data of total employment by SSP and CORE scenarios", overwrite = T) %>%
      add_units("Million persons") %>%
      add_comments("share of employment over population derived based on PWT and SSP database, applied to pop") %>%
      add_precursors("L201.Pop_Scen", "common/GCAM_region_names",
                     "L103.LaborForceShare_Scen_R_Y") ->
      L201.TotalEmployment_Scen

    # Global technology Labor_Ag and Labor_Materials
    L201.TotalEmployment_Scen %>%
      distinct(scenario, sector.name = fixed.final.demand, year) %>%
      mutate(subsector.name = socioeconomics.LABOR_AG_MARKET_NAME,
             technology = socioeconomics.LABOR_AG_MARKET_NAME,
             res.secondary.output = socioeconomics.LABOR_AG_MARKET_NAME,
             output.ratio = 1.0) %>%
      select(c(scenario, LEVEL2_DATA_NAMES[["GlobalTechRESSecOut"]])) %>%
      bind_rows(
        L201.TotalEmployment_Scen %>%
          distinct(scenario, sector.name = fixed.final.demand, year) %>%
          mutate(subsector.name = socioeconomics.LABOR_MATERIALS_MARKET_NAME,
                 technology = socioeconomics.LABOR_MATERIALS_MARKET_NAME,
                 res.secondary.output = socioeconomics.LABOR_MATERIALS_MARKET_NAME,
                 output.ratio = 1.0) %>%
          select(c(scenario, LEVEL2_DATA_NAMES[["GlobalTechRESSecOut"]]))
      ) %>%
      mutate(pMultiplier = -1.0,
             share.weight = 1) ->
      L201.LaborSupplyTech_Scen

    #* L201.LaborSupplyTech_Scen
    L201.LaborSupplyTech_Scen %>%
      add_title("Labor supply technology param by SSP and CORE scenarios", overwrite = T) %>%
      add_units("na") %>%
      add_precursors("L201.TotalEmployment_Scen") ->
      L201.LaborSupplyTech_Scen


    # (6) Other labor supply related files ----

    # Join a table to include all labor market info in base years
    # start with total emp
    L201.TotalEmployment_Scen %>%
      filter(scenario == "SSP2", year %in% MODEL_BASE_YEARS) %>%
      select(-scenario) %>%
      select(region, year, totalEmp_Mppl = service) %>%
      # join pop
      left_join_error_no_match(
        L201.Pop_Scen %>%
          filter(scenario == "SSP2", year %in% MODEL_BASE_YEARS) %>%
          select(region, year, totalPop_Kppl = totalPop),
        by = c("region", "year")
      ) %>%
      # join material labor VA
      left_join_error_no_match(
        L203.NationalAccounts %>%
          filter(year %in% MODEL_BASE_YEARS) %>%
          select(region, year, MaVA_Labor_Mil1990USD = wages,
                 AgVA_Labor_Mil1990USD, AgEmp_Mppl),
        by = c("region", "year")
      ) %>%
      # derive
      mutate(
        MaEmp_Mppl = totalEmp_Mppl - AgEmp_Mppl,
        # 1990 $ / people / year
        W_AG = AgVA_Labor_Mil1990USD / AgEmp_Mppl,
        W_MA = MaVA_Labor_Mil1990USD / MaEmp_Mppl,
        # update historical labor.force.share_MA to be only MA
        # need mil ppl over thous ppl here
        labor.force.share_MA = MaEmp_Mppl / (totalPop_Kppl)
      ) ->
      L201.GCAM_labor_market_R_Yh


    # regional labor supply info
    assertthat::assert_that(
      identical(
        GCAM_region_names %>% distinct(GCAM_region_ID, region) %>% arrange,
        A_labor_supply_sector %>% distinct(GCAM_region_ID, region) %>% arrange)
    )

    L201.LaborSupplySector <-
      A_labor_supply_sector %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]] %>% c("logit.type") )


    #* L201.LaborSupplySector ----
    L201.LaborSupplySector %>%
      add_title("Regional labor supply specification") %>%
      add_units("na") %>%
      add_comments("logit exponents are specified") %>%
      add_legacy_name("L201.LaborSupplySector") %>%
      add_precursors("socioeconomics/A_labor_supply_sector") ->
      L201.LaborSupplySector


    L201.GCAM_labor_market_R_Yh %>%
      select(region, year,
             Labor_Ag = AgEmp_Mppl,
             Labor_Materials = MaEmp_Mppl) %>%
      gather(subsector, value, Labor_Ag, Labor_Materials) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1,
             supplysector = socioeconomics.LABOR_MARKET_NAME,
             apply.to = "share-weight",
             from.year = MODEL_FINAL_BASE_YEAR,
             to.year = max(MODEL_FUTURE_YEARS),
             interpolation.function = "fixed",
             logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = -6,
             logit.type = NA,
             stub.technology = subsector,
             calOutputValue = value, # mil pers
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(subs.share.weight > 0, 1, 0)) %>%
      ungroup() ->
      L201.LaborSupplySubSector

    #* L201.LaborSupplySubSector ----
    L201.LaborSupplySubSector %>%
      add_title("Regional labor supply subsector specification") %>%
      add_units("Million persons") %>%
      add_comments("logit exponents are specified") %>%
      add_legacy_name("L201.LaborSupplySubSector") %>%
      add_precursors("L201.TotalEmployment_Scen",
                     "L201.Pop_Scen",
                     "L203.NationalAccounts") ->
      L201.LaborSupplySubSector


    L201.GCAM_labor_market_R_Yh %>%
      transmute(region, market = region, year, Labor_Ag = W_AG, Labor_Materials = W_MA) %>%
      gather(resource, price, Labor_Ag, Labor_Materials) %>%
      mutate(output.unit = "mil pers",
             price.unit = "1990$/pers") ->
      L201.Labor_Rsrc_price

    #* L201.Labor_Rsrc_price ----
    L201.Labor_Rsrc_price %>%
      add_title("Regional labor supply subsector specification: wage rates", overwrite = T) %>%
      add_units("1990$/pers") %>%
      add_comments("calculated as labor compensation over employment") %>%
      add_legacy_name("L201.LaborSupplySubSector", overwrite = T) %>%
      add_precursors("L201.TotalEmployment_Scen",
                     "L201.Pop_Scen",
                     "L203.NationalAccounts") ->
      L201.Labor_Rsrc_price


    L201.Labor_Rsrc_price %>%
      distinct(region, market, resource, output.unit, price.unit) ->
      L201.Labor_Rsrc


    # kind of funky but we are creating a dummy subresource just so we can have it
    # set the fully calibrated flag and supply curve lower bound of zero
    # we end up needing to include three tables in total to accomplish this:

    # 1. A table with dummy grades, grade.0 in particular with zero cost
    L201.Labor_Rsrc %>%
      distinct(region, resource) %>%
      rename(renewresource = resource) %>%
      mutate(sub.renewable.resource = renewresource) %>%
      tidyr::crossing(tibble::tibble(grade = c("grade.0", "grade.1"),
                                     extractioncost = c(0, 1),
                                     available = 0)) ->
      L201.Labor_RsrcCurves


    # 2. A table with calibration values to set the fully calibrated flag (zero value is sufficient)
    L201.Labor_RsrcCurves %>%
      distinct(region, renewresource, sub.renewable.resource) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      mutate(cal.production = 0) %>%
      select(LEVEL2_DATA_NAMES[["RenewRsrcCalProd"]]) ->
      L201.Labor_RenewRsrcCalProd

    # 3. All subresoures require a technology shell, share weights are sufficient
    L201.Labor_RsrcCurves %>%
      distinct(region, renewresource, sub.renewable.resource) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(resource = renewresource,
             subresource = sub.renewable.resource,
             technology = sub.renewable.resource,
             share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L201.Labor_ResTechShrwt

    #* L201.Labor_Rsrc ----
    L201.Labor_Rsrc %>%
      add_title("Regional labor subsector resource specification", overwrite = T) %>%
      add_units("na") %>%
      add_comments("Labor_Ag and Labor_Materials are added as resources") %>%
      add_legacy_name("L201.Labor_Rsrc", overwrite = T) %>%
      add_precursors("L201.TotalEmployment_Scen",
                     "L201.Pop_Scen",
                     "L203.NationalAccounts") ->
      L201.Labor_Rsrc

    #* L201.Labor_RsrcCurves ----
    L201.Labor_RsrcCurves %>%
      add_title("Graded supply curves for labor resources") %>%
      add_units("na") %>%
      add_comments("Only used to set supply curve lower bound of 0") %>%
      same_attributes_as(L201.Labor_Rsrc) ->
      L201.Labor_RsrcCurves

    #* L201.Labor_RenewRsrcCalProd ----
    L201.Labor_RenewRsrcCalProd %>%
      add_title("Calibrated production (zero) for labor renewable resources in base years") %>%
      add_units("mil pers") %>%
      add_comments("Only used to set the fully-calibrated flag to 1") %>%
      same_attributes_as(L201.Labor_Rsrc) ->
      L201.Labor_RenewRsrcCalProd

    #* L201.Labor_ResTechShrwt ----
    L201.Labor_ResTechShrwt %>%
      add_title("Technology share-weights for labor renewable resources in base years") %>%
      add_units("Unitless") %>%
      add_comments("Share weight of 1 for all years") %>%
      same_attributes_as(L201.Labor_Rsrc) ->
      L201.Labor_ResTechShrwt


    # (7) update L201.LaborForceShare_Scen for MA only----

    # using template from L201.Pop_Scen for scenario/region/year
    L201.Pop_Scen %>%
      filter(year %in% MODEL_YEARS) %>%
      distinct(scenario, region, year) %>%
      # join for historical years and set future value to zero
      left_join(
        L201.GCAM_labor_market_R_Yh %>%
          transmute(region, year, labor.force.share = labor.force.share_MA),
        by = c("region", "year")
      ) %>%
      ## just use zeros
      mutate(across(c(labor.force.share), ~replace_na(., 0))) ->
      L201.LaborForceShare_Scen

    L201.LaborForceShare_Scen %>%
      add_title("National Accounts data of Materials employment share (MA/totPop) by SSP and CORE scenarios",
                overwrite = T) %>%
      add_units("share/1000") %>%
      add_comments("share of Materials employment over population derived based on PWT and SSP database") %>%
      add_precursors("common/GCAM_region_names",
                     "L201.TotalEmployment_Scen",
                     "L201.Pop_Scen",
                     "L203.NationalAccounts") ->
      L201.LaborForceShare_Scen


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}

