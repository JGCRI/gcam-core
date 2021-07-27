# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L221.en_supply
#'
#' Writes all energy supply sector outputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L221.SectorLogitTables[[ curr_table ]]$data}, \code{L221.Supplysector_en}, \code{L221.SectorUseTrialMarket_en}, \code{L221.SubsectorLogitTables[[ curr_table ]]$data}, \code{L221.SubsectorLogit_en}, \code{L221.SubsectorShrwt_en}, \code{L221.SubsectorShrwtFllt_en}, \code{L221.SubsectorInterp_en}, \code{L221.SubsectorInterpTo_en}, \code{L221.StubTech_en}, \code{L221.GlobalTechCoef_en}, \code{L221.GlobalTechCost_en}, \code{L221.GlobalTechShrwt_en}, \code{L221.PrimaryConsKeyword_en}, \code{L221.StubTechFractSecOut_en}, \code{L221.StubTechFractProd_en}, \code{L221.StubTechFractCalPrice_en}, \code{L221.Rsrc_en}, \code{L221.RsrcPrice_en}, \code{L221.Production_unoil}. The corresponding file in the
#' original data system was \code{L221.en_supply.R} (energy level2).
#' @details This chunk creates level 2 output files for energy supply. It creates supply sector information,
#' subsector logit exponents, subsector shareweight and interpolation, and stubtech info by writing assumption file
#' information to all model periods and regions. It creates global tech coef, costs, and shareweights
#' by interpolating assumptions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter full_join if_else group_by inner_join left_join mutate select summarise
#' @importFrom tidyr gather
#' @author JDH Nov 2017
module_energy_L221.en_supply <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "aglu/A_agRegionalTechnology",
             FILE = "energy/A21.sector",
             FILE = "energy/A_regions",
             FILE = "energy/A21.subsector_logit",
             FILE = "energy/A21.subsector_shrwt",
             FILE = "energy/A21.subsector_interp",
             FILE = "energy/A21.globaltech_coef",
             FILE = "energy/A21.globaltech_cost",
             FILE = "energy/A21.globaltech_shrwt",
             FILE = "energy/A21.globaltech_keyword",
             FILE = "energy/A21.globaltech_secout",
             FILE = "energy/A21.rsrc_info",
             "L121.BiomassOilRatios_kgGJ_R_C",
             "L122.in_Mt_R_C_Yh",
             FILE = "aglu/A_an_input_subsector",
             "L108.ag_Feed_Mt_R_C_Y",
             "L202.ag_consP_R_C_75USDkg"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L221.Supplysector_en",
             "L221.SectorUseTrialMarket_en",
             "L221.SubsectorLogit_en",
             "L221.SubsectorShrwt_en",
             "L221.SubsectorShrwtFllt_en",
             "L221.SubsectorInterp_en",
             "L221.SubsectorInterpTo_en",
             "L221.StubTech_en",
             "L221.GlobalTechCoef_en",
             "L221.StubTechCoef_bioOil",
             "L221.GlobalTechCost_en",
             "L221.GlobalTechShrwt_en",
             "L221.PrimaryConsKeyword_en",
             "L221.StubTechFractSecOut_en",
             "L221.StubTechFractProd_en",
             "L221.StubTechFractCalPrice_en",
             "L221.Rsrc_en",
             "L221.RsrcPrice_en",
             "L221.StubTechCalInput_bioOil",
             "L221.StubTechInterp_bioOil",
             "L221.StubTechShrwt_bioOil"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    P1 <- biodiesel <- biomassOil_tech <- calOutputValue <- calPrice <- coef <- coefficient <-
    resource <- ethanol <- feed_price <- fractional.secondary.output <- fuel <-
    input.cost <- market <- minicam.energy.input <- minicam.non.energy.input <-
    object <- output.ratio <- output.unit <- price <- price.unit <- primary.consumption <-
    region <- sector <- sector.name <- share.weight <- stub.technology <- subsector <-
    subsector.name <- subsector.share.weight <- supplysector <- technology <-
    to.value <- tradbio_region <- traded <- unit <- value <- value_fby <- variable <- year <-
    year.fillout <- year.share.weight <- GCAM_commodity <- GCAM_region_ID <-
    GCAM_region_ID.x <- GCAM_region_ID.y <- P0 <- calibrated.value <- tech.share.weight <-
      market.name <- passthru_tech_input <- SecOutRatio <- IOcoef <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_agRegionalTechnology <- get_data(all_data, "aglu/A_agRegionalTechnology")
    A21.sector <- get_data(all_data, "energy/A21.sector", strip_attributes = TRUE)
    A_regions <- get_data(all_data, "energy/A_regions")
    A21.subsector_logit <- get_data(all_data, "energy/A21.subsector_logit", strip_attributes = TRUE)
    A21.subsector_shrwt <- get_data(all_data, "energy/A21.subsector_shrwt", strip_attributes = TRUE)
    A21.subsector_interp <- get_data(all_data, "energy/A21.subsector_interp", strip_attributes = TRUE)
    A21.globaltech_coef <- get_data(all_data, "energy/A21.globaltech_coef")
    A21.globaltech_cost <- get_data(all_data, "energy/A21.globaltech_cost")
    A21.globaltech_shrwt <- get_data(all_data, "energy/A21.globaltech_shrwt", strip_attributes = TRUE)
    A21.globaltech_keyword <- get_data(all_data, "energy/A21.globaltech_keyword", strip_attributes = TRUE)
    A21.globaltech_secout <- get_data(all_data, "energy/A21.globaltech_secout", strip_attributes = TRUE)
    A21.rsrc_info <- get_data(all_data, "energy/A21.rsrc_info", strip_attributes = TRUE)
    L121.BiomassOilRatios_kgGJ_R_C <- get_data(all_data, "L121.BiomassOilRatios_kgGJ_R_C", strip_attributes = TRUE)
    L122.in_Mt_R_C_Yh <- get_data(all_data, "L122.in_Mt_R_C_Yh", strip_attributes = TRUE)
    A_an_input_subsector <- get_data(all_data, "aglu/A_an_input_subsector")
    L108.ag_Feed_Mt_R_C_Y <- get_data(all_data, "L108.ag_Feed_Mt_R_C_Y")
    L202.ag_consP_R_C_75USDkg <- get_data(all_data, "L202.ag_consP_R_C_75USDkg")

    # ===================================================

    # Supplysector information for upstream energy handling sectors
    A21.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), has_traded = TRUE,
                           GCAM_region_names = GCAM_region_names) -> L221.Supplysector_en

    # Supplysector table that indicates to the model to create solved markets for them
    # The traded markets tend to be a good candidate to solve explicitly since they tie together
    # many solved markets
    A21.sector %>%
      write_to_all_regions(c("region", "supplysector", "traded"), has_traded = TRUE,
                          GCAM_region_names = GCAM_region_names) %>%
      filter(traded == TRUE) %>%
      rename(use.trial.market = traded) -> L221.SectorUseTrialMarket_en

    # Subsector information
    # Subsector logit exponents of upstream energy handling sectors
    A21.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), has_traded = TRUE,
                           GCAM_region_names = GCAM_region_names) -> L221.SubsectorLogit_en

    # Subsector shareweights of upstream energy handling sectors
    if(any(!is.na(A21.subsector_shrwt$year))) {
      A21.subsector_shrwt %>%
        write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwt"]]), has_traded = TRUE,
                             GCAM_region_names = GCAM_region_names) %>%
        filter(!is.na(year)) -> L221.SubsectorShrwt_en
    }

    if(any(!is.na(A21.subsector_shrwt$year.fillout))) {
      A21.subsector_shrwt %>%
        write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]), has_traded = TRUE,
                             GCAM_region_names = GCAM_region_names) %>%
        filter(!is.na(year.fillout)) -> L221.SubsectorShrwtFllt_en
    }

    # Subsector shareweight interpolation of upstream energy handling sectors
    if(any(is.na(A21.subsector_interp$to.value))) {
      A21.subsector_interp %>%
        write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterp"]], "to.value"), has_traded = TRUE,
                             GCAM_region_names = GCAM_region_names) %>%
        filter(is.na(to.value)) %>%
        select(-to.value) -> L221.SubsectorInterp_en
    }

    if(any(!is.na(A21.subsector_interp$to.value))) {
      A21.subsector_interp %>%
        write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]]), has_traded = TRUE,
                             GCAM_region_names = GCAM_region_names) %>%
        filter(!is.na(to.value)) -> L221.SubsectorInterpTo_en
    }

    # Technology information
    # Identification of stub technologies (assume those in global tech shareweight table include all techs)
    A21.globaltech_shrwt %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]]), has_traded = FALSE,
                           GCAM_region_names = GCAM_region_names) -> L221.StubTech_en

    # Drop stub technologies for biomassOil techs that do not exist
    A21.globaltech_shrwt %>%
      filter(supplysector == "regional biomassOil") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble("GCAM_region_ID" = GCAM_region_names$GCAM_region_ID, "region" = GCAM_region_names$region)) %>%
      left_join(A_regions %>%
                  rename(technology = biomassOil_tech), by = c("region", "GCAM_region_ID", "technology")) %>%
      filter(is.na(tradbio_region)) %>%
      select(supplysector, subsector, technology, region, GCAM_region_ID) -> L221.rm_biomassOil_techs_R

    # Removing some region/technology pairs that don't have traditional biomass
    L221.StubTech_en %>%
      anti_join(L221.rm_biomassOil_techs_R, by = c("region", "technology")) %>%
      rename(stub.technology = technology) -> L221.StubTech_en

    # Coefficients of global technologies
    # Energy inputs and coefficients of global technologies for upstream energy handling

    A21.globaltech_coef %>%
      gather_years("coef") -> A21.globaltech_coef

    A21.globaltech_coef %>%
      select(supplysector, subsector, technology, minicam.energy.input) %>%
      distinct %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A21.globaltech_coef, by = c("supplysector", "subsector", "technology", "minicam.energy.input", "year")) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value = coef, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, minicam.energy.input, year, coefficient) -> L221.GlobalTechCoef_en

    L221.GlobalTechCoef_en %>%
       mutate(coefficient= if_else(minicam.energy.input =="crude oil", if_else(year <= MODEL_FINAL_BASE_YEAR,1,coefficient),coefficient))->L221.GlobalTechCoef_en

    # Stub technology coefficients - modify the global tech assumptions in regions where the crop characteristics differ
    L221.StubTechCoef_bioOil <- inner_join(L221.GlobalTechCoef_en, L121.BiomassOilRatios_kgGJ_R_C, by = c(technology = "GCAM_commodity")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(coefficient = if_else(is.na(IOcoef), coefficient, IOcoef),
             market.name = region) %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    # Costs of global technologies
    A21.globaltech_cost %>%
      gather_years("input.cost") -> A21.globaltech_cost

    A21.globaltech_cost %>%
      select(supplysector, subsector, technology, minicam.non.energy.input) %>%
      distinct %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A21.globaltech_cost, by = c("supplysector", "subsector", "technology", "minicam.non.energy.input", "year")) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value = input.cost, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, minicam.non.energy.input, year, input.cost) -> L221.GlobalTechCost_en

    # Shareweights of global technologies
    A21.globaltech_shrwt %>%
      gather_years("share.weight") -> A21.globaltech_shrwt

    A21.globaltech_shrwt %>%
      select(supplysector, subsector, technology) %>%
      distinct %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A21.globaltech_shrwt, by = c("supplysector", "subsector", "technology", "year")) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value = share.weight, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year, share.weight) -> L221.GlobalTechShrwt_en

    # Keywords of global technologies
    A21.globaltech_keyword %>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, primary.consumption, year) %>%
      filter(year %in% MODEL_YEARS) -> L221.PrimaryConsKeyword_en

    # Secondary feed outputs of biofuel production technologies
    # NOTE: secondary outputs are only written for the regions/technologies where applicable, so the global tech database can not be used
    # to get the appropriate region/tech combinations written out, first repeat by all regions, then subset as appropriate

    # First build the table with the available technologies.
    # GCAM_commodity -> regional crop name (if a traded crop) -> passthrough supplysector/subsector/technology
    biofuel_feedstock_cropname <- filter(A_agRegionalTechnology, market.name == "regional") %>%
      select(passthru_tech_input = "supplysector", GCAM_commodity = "minicam.energy.input")

    L221.biofuel_types_region <- distinct(L122.in_Mt_R_C_Yh, GCAM_region_ID, GCAM_commodity) %>%
      # join in the regional crop name (resetting to default gcam commodity for crops that aren't traded)
      left_join(biofuel_feedstock_cropname, by = "GCAM_commodity") %>%
      mutate(passthru_tech_input = if_else(is.na(passthru_tech_input), GCAM_commodity, passthru_tech_input)) %>%
      # join in the supplysector/subsector/technology names
      left_join_error_no_match(distinct(select(A21.globaltech_coef, supplysector, subsector, technology, minicam.energy.input)),
                               by = c(passthru_tech_input = "minicam.energy.input")) %>%
      # join in the region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, supplysector, subsector, technology)

    A21.globaltech_secout %>%
      # use inner join in order to drop processing transformations that don't produce animal feed (e.g., sugar cane ethanol, palm oil biodiesel)
      inner_join(L221.biofuel_types_region, by = c("supplysector", "subsector", "technology")) ->
      L221.globaltech_secout_R

    # Store these regions in a separate object
    L221.globaltech_secout_R %>%
      distinct(region) -> L221.ddgs_regions

    L221.BiomassOilSecOut_kgGJ_R_C <- left_join_error_no_match(L121.BiomassOilRatios_kgGJ_R_C, GCAM_region_names,
                                                               by = "GCAM_region_ID") %>%
      select(region, GCAM_commodity, SecOutRatio)

    L221.globaltech_secout_R %>%
      gather_years() %>%
      complete(nesting(supplysector, subsector, technology, fractional.secondary.output, region),
               year = sort(unique(c(year, MODEL_YEARS)))) %>%
      # Interpolate to all years
      group_by(region, supplysector, subsector, technology, fractional.secondary.output) %>%
      mutate(output.ratio = round(approx_fun(year, value, rule = 2), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # replace the region-specific secondary output coefficients where elsewhere indicated
      left_join(L221.BiomassOilSecOut_kgGJ_R_C, by = c("region", technology = "GCAM_commodity")) %>%
      mutate(output.ratio = if_else(is.na(SecOutRatio), output.ratio, SecOutRatio)) %>%
      select(region, supplysector, subsector, stub.technology = technology, fractional.secondary.output,
             output.ratio, year) -> L221.StubTechFractSecOut_en

    # Fraction produced as a fn of DDGS/feedcake price
    # Here we calculate the approximate price of feed in each region. Share of each feed type times the price of the commodity
    # Subset only the feed items that are considered "FeedCrops"
    A_an_input_subsector %>%
      filter(supplysector == "FeedCrops") -> A_an_input_subsector

    # Subset (filter) only the feed items that have tracked prices (i.e., don't include DDGS and feedcakes in this calc)
    L108.ag_Feed_Mt_R_C_Y %>%
      filter(GCAM_commodity %in% A_an_input_subsector$subsector,
             GCAM_commodity %in% L202.ag_consP_R_C_75USDkg$GCAM_commodity) -> L221.ag_Feed_Mt_R_C_Y

    L221.ag_Feed_Mt_R_C_Y %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      group_by(GCAM_region_ID) %>%
      summarise(value_fby = sum(value)) %>%
      ungroup() -> L221.ag_Feed_Mt_R_Yf

    L221.ag_Feed_Mt_R_C_Y %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      left_join(L221.ag_Feed_Mt_R_Yf, by = c("GCAM_region_ID")) %>%
      mutate(value = value / value_fby) %>%
      select(GCAM_region_ID, GCAM_commodity, year, value) %>%
      left_join(rename(L202.ag_consP_R_C_75USDkg, price = value),
                by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      mutate(feed_price = price * value) -> L221.ag_FeedShares_R_C_Yf

    L221.ag_FeedShares_R_C_Yf %>%
      select(GCAM_region_ID, feed_price) %>%
      group_by(GCAM_region_ID) %>%
      summarise(feed_price = sum(feed_price)) %>%
      left_join_error_no_match(A_regions %>%
                  select(region, GCAM_region_ID), by = "GCAM_region_ID") -> L221.ag_FeedPrice_R_Yf


    # Indicate the price points for the DDG/feedcake commodity
    # This is important for ensuring that the secondary output of feedcrops from the bio-refinery feedstock pass-through sectors
    # does not exceed the indigenous market demand for feed in the animal-based commodity production sector
    L221.StubTechFractSecOut_en %>%
      select(-output.ratio) %>%
      mutate(P0 = 0) %>%
      left_join(L221.ag_FeedPrice_R_Yf, by = "region") %>%
      mutate(P1 = round(feed_price, digits = energy.DIGITS_COST)) %>%
      gather(key = "variable", value = "price", P0, P1) %>%
      mutate(fraction.produced = as.numeric( sub("P", "", variable ) )) %>%
      select(-variable, -feed_price, -GCAM_region_ID) -> L221.StubTechFractProd_en

      # Calibrate the price (as a fixed price, not a point on a supply curve) in the base year
      L221.StubTechFractSecOut_en %>%
        filter(year %in% MODEL_BASE_YEARS) %>%                                 # In the base years the fractional secondary outputs are de-activated in order to calibrate the flows
        select(-output.ratio) %>%
        left_join(L221.ag_FeedPrice_R_Yf, by = "region") %>%
        mutate(calPrice = round(feed_price, digits = energy.DIGITS_COST)) %>%
        select(LEVEL2_DATA_NAMES[["StubTechFractCalPrice"]]) -> L221.StubTechFractCalPrice_en


    # Final tables for feedcrop secondary output: the resource
    A21.rsrc_info %>%
      repeat_add_columns(L221.ddgs_regions) %>%
      select(region, resource, output.unit = "output-unit", price.unit = "price-unit", market) %>%
      mutate(market = region) -> L221.Rsrc_en

    # Resource prices are copied from the fractional secondary output calPrice
    L221.StubTechFractCalPrice_en %>%
      select(region, resource = fractional.secondary.output, year, price = calPrice) ->
      L221.RsrcPrice_en


    # Calibration and region specific data

    # GPK 4/26/2019: Region-specific calibrated output of biomassOil technologies
    # Because multiple feedstocks for producing biomassOil are allowed, the quantities are calibrated
    L221.StubTechCalInput_bioOil <- L122.in_Mt_R_C_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      inner_join(distinct(select(A21.globaltech_coef, supplysector, subsector, technology, minicam.energy.input)),
                 by = c(GCAM_commodity = "technology")) %>%
      rename(stub.technology = GCAM_commodity) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      set_subsector_shrwt(value_col = "calibrated.value") %>%
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])

    # Technology share-weight interpolation - for regions with positive calibration values,
    # carry the share-weights forward. All others will get default values (below)
    L221.StubTechInterp_bioOil <- filter(L221.StubTechCalInput_bioOil,
                                         year == max(MODEL_BASE_YEARS),
                                         tech.share.weight == 1) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["StubTechInterp"]])

    # Technology share-weights for regions without calibration values
    L221.StubTechShrwt_bioOil <- filter(L221.StubTechCalInput_bioOil,
                                        year == max(MODEL_BASE_YEARS),
                                        tech.share.weight == 0) %>%
      select(region, supplysector, subsector, stub.technology) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(share.weight = 1)

    # For regions with no agricultural and land use sector (Taiwan), need to remove the passthrough supplysectors for first-gen biofuels
    ag_en <- c("regional corn for ethanol", "regional sugar for ethanol", "regional biomassOil")

    L221.SubsectorLogit_en %>%
      filter(!(region %in% aglu.NO_AGLU_REGIONS & supplysector %in% ag_en)) -> L221.SubsectorLogit_en

    L221.Supplysector_en %>%
      filter(!(region %in% aglu.NO_AGLU_REGIONS & supplysector %in% ag_en)) -> L221.Supplysector_en

    if(exists("L221.SubsectorShrwt_en")) {
      L221.SubsectorShrwt_en %>%
        filter(!(region %in% aglu.NO_AGLU_REGIONS & supplysector %in% ag_en)) -> L221.SubsectorShrwt_en
    }

    if(exists("L221.SubsectorShrwtFllt_en")) {
      L221.SubsectorShrwtFllt_en %>%
        filter(!(region %in% aglu.NO_AGLU_REGIONS & supplysector %in% ag_en)) -> L221.SubsectorShrwtFllt_en
    }

    if(exists("L221.SubsectorInterp_en")) {
      L221.SubsectorInterp_en %>%
        filter(!(region %in% aglu.NO_AGLU_REGIONS & supplysector %in% ag_en)) -> L221.SubsectorInterp_en
    }
    if(exists("L221.SubsectorInterpTo_en")) {
      L221.SubsectorInterpTo_en %>%
        filter(!(region %in% aglu.NO_AGLU_REGIONS & supplysector %in% ag_en)) -> L221.SubsectorInterpTo_en
    }

    L221.StubTech_en %>%
      filter(!(region %in% aglu.NO_AGLU_REGIONS & supplysector %in% ag_en)) -> L221.StubTech_en


    # ===================================================

    # Produce outputs

    L221.Supplysector_en %>%
      add_title("Regional supplysector information") %>%
      add_units("NA") %>%
      add_comments("A21.sector written to all regions") %>%
      add_comments("removed regions with no agricultural and land use sector") %>%
      add_legacy_name("L221.Supplysector_en") %>%
      add_precursors("energy/A21.sector", "common/GCAM_region_names")  ->
      L221.Supplysector_en

    L221.SectorUseTrialMarket_en %>%
      add_title("Supplysector table that indicates to the model to create solved markets") %>%
      add_units("NA") %>%
      add_comments("A21.sector written to all regions, filtered to traded") %>%
      add_legacy_name("L221.SectorUseTrialMarket_en") %>%
      add_precursors("energy/A21.sector", "common/GCAM_region_names")  ->
      L221.SectorUseTrialMarket_en

    L221.SubsectorLogit_en %>%
      add_title("Subsector logit information") %>%
      add_units("NA") %>%
      add_comments("A21.subsector_logit written to all regions") %>%
      add_comments("removed regions with no agricultural and land use sector") %>%
      add_legacy_name("L221.SubsectorLogit_en") %>%
      add_precursors("energy/A21.subsector_logit", "common/GCAM_region_names") ->
      L221.SubsectorLogit_en

    if(exists("L221.SubsectorShrwt_en")) {
    L221.SubsectorShrwt_en %>%
      add_title("Subsector shareweight information") %>%
      add_units("unitless") %>%
      add_comments("A21.subsector_shrwt written to all regions") %>%
      add_comments("removed regions with no agricultural and land use sector") %>%
      add_legacy_name("L221.SubsectorShrwt_en") %>%
      add_precursors("energy/A21.subsector_shrwt", "common/GCAM_region_names")  ->
      L221.SubsectorShrwt_en
    } else {
      # If all values for year are NA, then a blank tibble is produced
      missing_data() %>%
        add_legacy_name("L221.SubsectorShrwt_en") ->
        L221.SubsectorShrwt_en
    }

    if(exists("L221.SubsectorShrwtFllt_en")) {
    L221.SubsectorShrwtFllt_en %>%
      add_title("Subsector shareweight information using year.fillout") %>%
      add_units("unitless") %>%
        add_comments("A21.subsector_shrwt written to all regions") %>%
        add_comments("removed regions with no agricultural and land use sector") %>%
      add_legacy_name("L221.SubsectorShrwtFllt_en") %>%
      add_precursors("energy/A21.subsector_shrwt", "common/GCAM_region_names") ->
      L221.SubsectorShrwtFllt_en
    } else {
      # If all values for year.fillout are NA, then blank tibble is produced
      missing_data() %>%
        add_legacy_name("L221.SubsectorShrwtFllt_en") ->
        L221.SubsectorShrwtFllt_en
    }

    if(exists("L221.SubsectorInterp_en")) {
    L221.SubsectorInterp_en %>%
      add_title("Subsector interpolation information") %>%
      add_units("unitless") %>%
      add_comments("A21.subsector_interp written to all regions") %>%
      add_comments("removed regions with no agricultural and land use sector") %>%
      add_legacy_name("L221.SubsectorInterp_en") %>%
      add_precursors("energy/A21.subsector_interp", "common/GCAM_region_names") ->
      L221.SubsectorInterp_en
    } else {
      # If to.value is not NA anywhere, a blank tibble is produced
      missing_data() %>%
        add_legacy_name("L221.SubsectorInterp_en") ->
        L221.SubsectorInterp_en
    }

    if(exists("L221.SubsectorInterpTo_en")) {
    L221.SubsectorInterpTo_en %>%
        add_title("Subsector interpolation information using to.value") %>%
        add_units("unitless") %>%
        add_comments("A21.subsector_interp written to all regions") %>%
        add_comments("removed regions with no agricultural and land use sector") %>%
      add_legacy_name("L221.SubsectorInterpTo_en") %>%
      add_precursors("energy/A21.subsector_interp", "common/GCAM_region_names") ->
      L221.SubsectorInterpTo_en
    } else {
      # If to.value is NA, a blank tibble is produced
      missing_data() %>%
        add_legacy_name("L221.SubsectorInterpTo_en") ->
        L221.SubsectorInterpTo_en
    }

    L221.StubTech_en %>%
      add_title("Identification of stub technologies") %>%
      add_units("NA") %>%
      add_comments("A21.globaltech_shrwt written to all regions") %>%
      add_comments("removing technologies for biomassOil that don't exist, and regions with no agricultural and land use sector") %>%
      add_legacy_name("L221.StubTech_en") %>%
      add_precursors("energy/A21.globaltech_shrwt", "common/GCAM_region_names", "energy/A_regions") ->
      L221.StubTech_en

    L221.GlobalTechCoef_en %>%
      add_title("Coefficients of global technologies") %>%
      add_units("unitless") %>%
      add_comments("A21.globaltech_coef interpolated to all model years") %>%
      add_legacy_name("L221.GlobalTechCoef_en") %>%
      add_precursors("energy/A21.globaltech_coef") ->
      L221.GlobalTechCoef_en

    L221.StubTechCoef_bioOil %>%
      add_title("Coefficients of selected stub-technologies of biomassOil production") %>%
      add_units("kg/GJ") %>%
      add_comments("Default values from A21.globaltech_coef replaced for selected regions and crops/technologies") %>%
      add_precursors("energy/A21.globaltech_coef", "L121.BiomassOilRatios_kgGJ_R_C") ->
      L221.StubTechCoef_bioOil

    L221.GlobalTechCost_en %>%
      add_title("Costs of global technologies") %>%
      add_units("1975$/GJ") %>%
      add_comments("A21.globaltech_cost interpolated to all model years") %>%
      add_legacy_name("L221.GlobalTechCost_en") %>%
      add_precursors("energy/A21.globaltech_cost") ->
      L221.GlobalTechCost_en

    L221.GlobalTechShrwt_en %>%
      add_title("Shareweights of global technologies") %>%
      add_units("unitless") %>%
      add_comments("A21.globaltech_shrwt interpolated to all model years") %>%
      add_legacy_name("L221.GlobalTechShrwt_en") %>%
      add_precursors("energy/A21.globaltech_shrwt") ->
      L221.GlobalTechShrwt_en

    L221.PrimaryConsKeyword_en %>%
      add_title("Keywords of global technologies") %>%
      add_units("unitless") %>%
      add_comments("A21.globaltech_keyword written to all model periods") %>%
      add_legacy_name("L221.PrimaryConsKeyword_en") %>%
      add_precursors("energy/A21.globaltech_keyword") ->
      L221.PrimaryConsKeyword_en

    L221.StubTechFractSecOut_en %>%
      add_title("Secondary feed outputs of biofuel production technologies") %>%
      add_units("fractions") %>%
      add_comments("Secondary outputs are only written out to relevant regions and technologies") %>%
      add_legacy_name("L221.StubTechFractSecOut_en") %>%
      add_precursors("aglu/A_agRegionalTechnology", "energy/A21.globaltech_secout", "common/GCAM_region_names", "L122.in_Mt_R_C_Yh") ->
      L221.StubTechFractSecOut_en

    L221.StubTechFractProd_en %>%
      add_title("Price and production fraction for secondary feed outputs") %>%
      add_units("1975$, fraction") %>%
      add_comments("Prices, production of feed output from L108.ag_Feed_Mt_R_C_Y and L202.ag_consP_R_C_75USDkg") %>%
      add_legacy_name("L221.StubTechFractProd_en") %>%
      add_precursors("L108.ag_Feed_Mt_R_C_Y", "L202.ag_consP_R_C_75USDkg", "aglu/A_an_input_subsector") ->
      L221.StubTechFractProd_en

    L221.StubTechFractCalPrice_en %>%
      add_title("Calibrated prices of secondary outputs of feed from energy technologies (DDGS and feedcakes)") %>%
      add_units("1975$/kg") %>%
      add_comments("Value only relevant for share-weight calculation") %>%
      add_precursors("L108.ag_Feed_Mt_R_C_Y", "L202.ag_consP_R_C_75USDkg", "aglu/A_an_input_subsector") ->
      L221.StubTechFractCalPrice_en

    L221.Rsrc_en %>%
      add_title("Resource table for feedcrop secondary output") %>%
      add_units("unitless") %>%
      add_comments("A21.rsrc_info written out to regions with secondary feed output") %>%
      add_legacy_name("L221.Rsrc_en") %>%
      add_precursors("energy/A21.rsrc_info") ->
      L221.Rsrc_en

    L221.RsrcPrice_en %>%
      add_title("Resource price for feedcrop secondary output") %>%
      add_units("1975$") %>%
      add_comments("A21.rsrc_info interpolated to all historical model time periods") %>%
      add_legacy_name("L221.RsrcPrice_en") %>%
      same_precursors_as(L221.StubTechFractCalPrice_en) ->
      L221.RsrcPrice_en

    L221.StubTechCalInput_bioOil %>%
      add_title("Calibrated output of biomassOil by feedstock type") %>%
      add_units("Mt/yr") %>%
      add_comments("Calibration is necessary to allow regions to have multiple biomassOil feedstocks") %>%
      add_precursors("L122.in_Mt_R_C_Yh", "energy/A21.globaltech_coef", "common/GCAM_region_names") ->
      L221.StubTechCalInput_bioOil

    L221.StubTechInterp_bioOil %>%
      add_title("biomassOil technology (feedstock type) shareweight interpolation") %>%
      add_units("unitless") %>%
      add_comments("Regions with multiple feedstocks in the base year have their share-weights passed forward") %>%
      same_precursors_as(L221.StubTechCalInput_bioOil) ->
      L221.StubTechInterp_bioOil

    L221.StubTechShrwt_bioOil %>%
      add_title("biomassOil technology (feedstock type) shareweights") %>%
      add_units("unitless") %>%
      add_comments("Regions with zero production in the base year have exogenous share-weights in future years") %>%
      same_precursors_as(L221.StubTechCalInput_bioOil) ->
      L221.StubTechShrwt_bioOil

    return_data(L221.Supplysector_en, L221.SectorUseTrialMarket_en, L221.SubsectorLogit_en,
                L221.SubsectorShrwt_en, L221.SubsectorShrwtFllt_en, L221.SubsectorInterp_en,
                L221.SubsectorInterpTo_en, L221.StubTech_en, L221.GlobalTechCoef_en, L221.StubTechCoef_bioOil,
                L221.GlobalTechCost_en, L221.GlobalTechShrwt_en, L221.PrimaryConsKeyword_en,
                L221.StubTechFractSecOut_en, L221.StubTechFractProd_en, L221.StubTechFractCalPrice_en, L221.Rsrc_en,
                L221.RsrcPrice_en,
                L221.StubTechCalInput_bioOil,
                L221.StubTechInterp_bioOil, L221.StubTechShrwt_bioOil)
  } else {
    stop("Unknown command")
  }
}
