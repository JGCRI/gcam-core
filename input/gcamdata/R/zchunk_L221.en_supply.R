#' module_energy_L221.en_supply
#'
#' Writes all energy supply sector outputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L221.SectorLogitTables[[ curr_table ]]$data}, \code{L221.Supplysector_en}, \code{L221.SectorUseTrialMarket_en}, \code{L221.SubsectorLogitTables[[ curr_table ]]$data}, \code{L221.SubsectorLogit_en}, \code{L221.SubsectorShrwt_en}, \code{L221.SubsectorShrwtFllt_en}, \code{L221.SubsectorInterp_en}, \code{L221.SubsectorInterpTo_en}, \code{L221.StubTech_en}, \code{L221.GlobalTechCoef_en}, \code{L221.GlobalTechCost_en}, \code{L221.GlobalTechShrwt_en}, \code{L221.PrimaryConsKeyword_en}, \code{L221.StubTechFractSecOut_en}, \code{L221.StubTechFractProd_en}, \code{L221.StubTechFractCalPrice_en}, \code{L221.DepRsrc_en}, \code{L221.DepRsrcPrice_en}, \code{L221.TechCoef_en_Traded}, \code{L221.TechCost_en_Traded}, \code{L221.TechShrwt_en_Traded}, \code{L221.StubTechCoef_unoil}, \code{L221.Production_unoil}, \code{L221.StubTechProd_oil_unoil}, \code{L221.StubTechProd_oil_crude}. The corresponding file in the
#' original data system was \code{L221.en_supply.R} (energy level2).
#' @details This chunk creates level 2 output files for energy supply. It creates supply sector information,
#' subsector logit exponents, subsector shareweight and interpolation, and stubtech info by writing assumption file
#' information to all model periods and regions. It creates global tech coef, costs, and shareweights
#' by interpolating assumptions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author JDH Nov 2017
module_energy_L221.en_supply <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
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
             FILE = "energy/A21.tradedtech_coef",
             FILE = "energy/A21.tradedtech_cost",
             FILE = "energy/A21.tradedtech_shrwt",
             "L111.Prod_EJ_R_F_Yh",
             "L121.in_EJ_R_TPES_unoil_Yh",
             "L121.in_EJ_R_TPES_crude_Yh",
             "L122.in_Mt_R_C_Yh",
             FILE = "aglu/A_an_input_subsector",
             "L108.ag_Feed_Mt_R_C_Y",
             "L132.ag_an_For_Prices"))
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
             "L221.GlobalTechCost_en",
             "L221.GlobalTechShrwt_en",
             "L221.PrimaryConsKeyword_en",
             "L221.StubTechFractSecOut_en",
             "L221.StubTechFractProd_en",
             "L221.StubTechFractCalPrice_en",
             "L221.DepRsrc_en",
             "L221.DepRsrcPrice_en",
             "L221.TechCoef_en_Traded",
             "L221.TechCost_en_Traded",
             "L221.TechShrwt_en_Traded",
             "L221.StubTechCoef_unoil",
             "L221.Production_unoil",
             "L221.StubTechProd_oil_unoil",
             "L221.StubTechProd_oil_crude",
             "L221.StubTechCalInput_bioOil",
             "L221.StubTechInterp_bioOil",
             "L221.StubTechShrwt_bioOil"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    P1 <- biodiesel <- biomassOil_tech <- calOutputValue <- calPrice <- coef <- coefficient <-
    depresource <- ethanol <- feed_price <- fractional.secondary.output <- fuel <-
    input.cost <- market <- minicam.energy.input <- minicam.non.energy.input <-
    object <- output.ratio <- output.unit <- price <- price.unit <- primary.consumption <-
    region <- sector <- sector.name <- share.weight <- stub.technology <- subsector <-
    subsector.name <- subsector.share.weight <- supplysector <- technology <-
    to.value <- tradbio_region <- traded <- unit <- value <- value_2010 <- variable <- year <-
    year.fillout <- year.share.weight <- GCAM_commodity <- GCAM_region_ID <-
    GCAM_region_ID.x <- GCAM_region_ID.y <- P0 <- calibrated.value <- tech.share.weight <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A21.sector <- get_data(all_data, "energy/A21.sector")
    A_regions <- get_data(all_data, "energy/A_regions")
    A21.subsector_logit <- get_data(all_data, "energy/A21.subsector_logit")
    A21.subsector_shrwt <- get_data(all_data, "energy/A21.subsector_shrwt")
    A21.subsector_interp <- get_data(all_data, "energy/A21.subsector_interp")
    A21.globaltech_coef <- get_data(all_data, "energy/A21.globaltech_coef")
    A21.globaltech_cost <- get_data(all_data, "energy/A21.globaltech_cost")
    A21.globaltech_shrwt <- get_data(all_data, "energy/A21.globaltech_shrwt")
    A21.globaltech_keyword <- get_data(all_data, "energy/A21.globaltech_keyword")
    A21.globaltech_secout <- get_data(all_data, "energy/A21.globaltech_secout")
    A21.rsrc_info <- get_data(all_data, "energy/A21.rsrc_info")
    A21.tradedtech_coef <- get_data(all_data, "energy/A21.tradedtech_coef")
    A21.tradedtech_cost <- get_data(all_data, "energy/A21.tradedtech_cost")
    A21.tradedtech_shrwt <- get_data(all_data, "energy/A21.tradedtech_shrwt")
    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "L111.Prod_EJ_R_F_Yh")
    L121.in_EJ_R_TPES_unoil_Yh <- get_data(all_data, "L121.in_EJ_R_TPES_unoil_Yh")
    L121.in_EJ_R_TPES_crude_Yh <- get_data(all_data, "L121.in_EJ_R_TPES_crude_Yh")
    L122.in_Mt_R_C_Yh <- get_data(all_data, "L122.in_Mt_R_C_Yh")
    A_an_input_subsector <- get_data(all_data, "aglu/A_an_input_subsector")
    L108.ag_Feed_Mt_R_C_Y <- get_data(all_data, "L108.ag_Feed_Mt_R_C_Y")
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices")

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
    A21.globaltech_secout %>%
      repeat_add_columns(tibble(region = GCAM_region_names$region)) %>%
      left_join(calibrated_techs %>%
                  select(minicam.energy.input, sector), by = c("supplysector" = "minicam.energy.input")) -> L221.globaltech_secout_R

    # For corn ethanol, region + sector is checked. For biodiesel, need to also include the region-specific feedstocks (to exclude palm oil biodiesel producing regions)
    L221.globaltech_secout_R %>%
      # comparison of region + sector for corn ethanol
      left_join(A_regions %>%
                  select(region, ethanol, GCAM_region_ID) %>%
                  rename(sector = ethanol), by = c("region", "sector")) %>%
      # comparison of region + technology for biodeisel
      left_join(A_regions %>%
                  select(region, biodiesel, biomassOil_tech, GCAM_region_ID) %>%
                  rename(sector = biodiesel, technology = biomassOil_tech), by = c("region", "sector", "technology")) %>%
      # filtering for regions with corn ethanol, biodiesel feedstocks
      filter(!is.na(GCAM_region_ID.x) | !is.na(GCAM_region_ID.y)) %>%
      mutate(GCAM_region_ID = if_else(!is.na(GCAM_region_ID.x), GCAM_region_ID.x, GCAM_region_ID.y)) %>%
      select(-GCAM_region_ID.x, -GCAM_region_ID.y) -> L221.globaltech_secout_R

    # Store these regions in a separate object
    L221.globaltech_secout_R %>%
      distinct(GCAM_region_ID) -> L221.ddgs_regions

    L221.globaltech_secout_R %>%
      select(supplysector, subsector, technology, fractional.secondary.output, region, sector, GCAM_region_ID) %>%
      distinct %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = c(MODEL_YEARS))) %>%
      left_join(L221.globaltech_secout_R %>%
                  gather_years("value"),
                by = c("supplysector", "subsector", "technology", "fractional.secondary.output", "region", "sector", "GCAM_region_ID", "year")) %>%
      group_by(supplysector, subsector, technology, fractional.secondary.output, region, sector, GCAM_region_ID) %>%
      mutate(output.ratio = round(approx_fun(year, value, rule = 2))) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(supplysector, subsector, stub.technology = technology, fractional.secondary.output,
             region, output.ratio, year) -> L221.StubTechFractSecOut_en

    # Fraction produced as a fn of DDGS/feedcake price
    # Here we calculate the approximate price of feed in each region. Share of each feed type times the price of the commodity
    # Subset only the feed items that are considered "FeedCrops"
    A_an_input_subsector %>%
      filter(supplysector == "FeedCrops") -> A_an_input_subsector

    # Subset (filter) only the feed items that have tracked prices (i.e., don't include DDGS and feedcakes in this calc)
    L108.ag_Feed_Mt_R_C_Y %>%
      filter(GCAM_commodity %in% A_an_input_subsector$subsector,
             GCAM_commodity %in% L132.ag_an_For_Prices$GCAM_commodity) -> L221.ag_Feed_Mt_R_C_Y

    L221.ag_Feed_Mt_R_C_Y %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      group_by(GCAM_region_ID) %>%
      summarise(value_2010 = sum(value)) %>%
      ungroup() -> L221.ag_Feed_Mt_R_Yf

    L221.ag_Feed_Mt_R_C_Y %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      left_join(L221.ag_Feed_Mt_R_Yf, by = c("GCAM_region_ID")) %>%
      mutate(value = value / value_2010) %>%
      select(GCAM_region_ID, GCAM_commodity, year, value) %>%
      left_join(L132.ag_an_For_Prices %>%
                  select(-unit), by = c("GCAM_commodity")) %>%
      rename(price = calPrice) %>%
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
      repeat_add_columns(tibble(region = c(L221.ddgs_regions$GCAM_region_ID))) %>%
      rename(GCAM_region_ID = region) %>%
      left_join_error_no_match(A_regions %>%
                  select(region, GCAM_region_ID), by = "GCAM_region_ID") %>%
      select(region, depresource, output.unit = "output-unit", price.unit = "price-unit", market) %>%
      mutate(market = region) -> L221.DepRsrc_en

    A21.rsrc_info %>%
      select(depresource, market, output.unit = "output-unit", price.unit = "price-unit") %>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS))) %>%
      left_join(A21.rsrc_info %>%
                  gather(year, value, -depresource, -market, -"output-unit", -"price-unit") %>%
                  rename(output.unit = "output-unit", price.unit = "price-unit") %>%
                  mutate(year = as.numeric(year)), by = c("year", "depresource", "market", "output.unit", "price.unit")) %>%
      group_by(depresource, market, output.unit, price.unit) %>%
      mutate(price = round(approx_fun(year, value, rule = 1), digits = energy.DIGITS_COST)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(-value) %>%
      repeat_add_columns(L221.ddgs_regions) %>%
      left_join(A_regions %>%
                  select(region, GCAM_region_ID), by = c("GCAM_region_ID")) %>%
      select(region, depresource, year, price) -> L221.DepRsrcPrice_en

    # Coefficients of traded technologies
    A21.tradedtech_coef %>%
      select(supplysector, subsector, technology, minicam.energy.input) %>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A21.tradedtech_coef %>%
                  gather(year, value, -supplysector, -subsector, - technology, -minicam.energy.input) %>%
                  mutate(year = as.numeric(year)),
                by = c("year", "supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      select(-value) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["TechCoef"]]), set_market = TRUE, has_traded = TRUE, apply_selected_only = FALSE,
                           GCAM_region_names = GCAM_region_names) %>%
      filter(year %in% MODEL_YEARS) -> L221.TechCoef_en_Traded

    # Costs of traded technologies
    A21.tradedtech_cost %>%
      select(supplysector, subsector, technology, minicam.non.energy.input) %>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A21.tradedtech_cost %>%
                  gather(year, value, -supplysector, -subsector, - technology, -minicam.non.energy.input) %>%
                  mutate(year = as.numeric(year)),
                by = c("year", "supplysector", "subsector", "technology", "minicam.non.energy.input")) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      select(-value) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["TechCost"]]), set_market = FALSE, has_traded = TRUE, apply_selected_only = FALSE,
                           GCAM_region_names = GCAM_region_names) %>%
      filter(year %in% MODEL_YEARS) -> L221.TechCost_en_Traded

    # Shareweights of traded technologies
    A21.tradedtech_shrwt %>%
      select(supplysector, subsector, technology, minicam.energy.input) %>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A21.tradedtech_shrwt %>%
                  gather(year, value, -supplysector, -subsector, - technology, -minicam.energy.input) %>%
                  mutate(year = as.numeric(year)),
                by = c("year", "supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      select(-value) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["TechYr"]], "share.weight"), set_market = FALSE, has_traded = TRUE, apply_selected_only = FALSE,
                           GCAM_region_names = GCAM_region_names) %>%
      filter(year %in% MODEL_YEARS) -> L221.TechShrwt_en_Traded

    # Calibration and region specific data
    # Coefficient and market name of stub technologies for importing traded unconventional oil
    L221.GlobalTechCoef_en %>%
      rename(supplysector = sector.name, subsector = subsector.name) %>%
      filter(minicam.energy.input %in% L221.TechShrwt_en_Traded$supplysector) %>%
      rename(stub.technology = technology) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTechCoef"]]),
                           GCAM_region_names = GCAM_region_names) %>%
      mutate(market.name = gcam.USA_REGION) -> L221.StubTechCoef_unoil

    L111.Prod_EJ_R_F_Yh %>%
      filter(grepl("unconventional", fuel), year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(A_regions %>%
                  select(GCAM_region_ID, region), by = c("GCAM_region_ID")) %>%
      select(GCAM_region_ID, value, year, region) -> L221.Prod_EJ_R_unoil_Yh

    # Calibrated production of unconventional oil
    L221.TechCoef_en_Traded %>%
      filter(supplysector == "traded unconventional oil" & year %in% MODEL_BASE_YEARS) %>%
      left_join(L221.Prod_EJ_R_unoil_Yh %>%
                  rename(market.name = region), by = c("market.name", "year")) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT)) %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]], calOutputValue) %>%
      mutate(calOutputValue = if_else(is.na(calOutputValue), 0, calOutputValue),
             year.share.weight = year,
             subsector.share.weight = if_else(calOutputValue > 0, 1, 0),
             share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      mutate(year.share.weight = year) -> L221.Production_unoil

    # Unconventional oil demand
    L121.in_EJ_R_TPES_unoil_Yh %>%
      left_join(A_regions %>%
                  select(GCAM_region_ID, region), by = "GCAM_region_ID") -> L121.in_EJ_R_TPES_unoil_Yh

    # Calibrated demand of unconventional oil
    L221.StubTech_en %>%
      filter(supplysector == "regional oil" & subsector == "unconventional oil") %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      left_join(L121.in_EJ_R_TPES_unoil_Yh, by = c("region", "year")) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT),
             calOutputValue = if_else(is.na(calOutputValue), 0, calOutputValue),
             year.share.weight = year,
             subsector.share.weight = if_else(calOutputValue > 0, 1, 0),
             share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue, year.share.weight, subsector.share.weight, share.weight) -> L221.StubTechProd_oil_unoil

    # Crude oil demand
    L121.in_EJ_R_TPES_crude_Yh %>%
      left_join(A_regions %>%
                  select(GCAM_region_ID, region), by = "GCAM_region_ID") -> L121.in_EJ_R_TPES_crude_Yh

    # Calibrated demand of crude oil
    L221.StubTech_en %>%
      filter(supplysector == "regional oil" & subsector == "crude oil") %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      left_join(L121.in_EJ_R_TPES_crude_Yh, by = c("region", "year")) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT),
             calOutputValue = if_else(is.na(calOutputValue), 0, calOutputValue),
             year.share.weight = year,
             subsector.share.weight = if_else(calOutputValue > 0, 1, 0),
             share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue, year.share.weight, subsector.share.weight, share.weight) -> L221.StubTechProd_oil_crude

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
      add_comments("A21.globaltech_secout is written out to all regions, then subsetted to relevant region/technologies") %>%
      add_legacy_name("L221.StubTechFractSecOut_en") %>%
      add_precursors("energy/A21.globaltech_secout", "common/GCAM_region_names", "energy/calibrated_techs") ->
      L221.StubTechFractSecOut_en

    L221.StubTechFractProd_en %>%
      add_title("Price and production fraction for secondary feed outputs") %>%
      add_units("1975$, fraction") %>%
      add_comments("Prices, production of feed output from L108.ag_Feed_Mt_R_C_Y and L132.ag_an_For_Prices") %>%
      add_legacy_name("L221.StubTechFractProd_en") %>%
      add_precursors("L108.ag_Feed_Mt_R_C_Y", "L132.ag_an_For_Prices", "aglu/A_an_input_subsector") ->
      L221.StubTechFractProd_en

    L221.StubTechFractCalPrice_en %>%
      add_title("Calibrated prices of secondary outputs of feed from energy technologies (DDGS and feedcakes)") %>%
      add_units("1975$/kg") %>%
      add_comments("Value only relevant for share-weight calculation") %>%
      add_precursors("L108.ag_Feed_Mt_R_C_Y", "L132.ag_an_For_Prices", "aglu/A_an_input_subsector") ->
      L221.StubTechFractCalPrice_en

    L221.DepRsrc_en %>%
      add_title("Resource table for feedcrop secondary output") %>%
      add_units("unitless") %>%
      add_comments("A21.rsrc_info written out to regions with secondary feed output") %>%
      add_legacy_name("L221.DepRsrc_en") %>%
      add_precursors("energy/A21.rsrc_info") ->
      L221.DepRsrc_en

    L221.DepRsrcPrice_en %>%
      add_title("Resource price for feedcrop secondary output") %>%
      add_units("1975$") %>%
      add_comments("A21.rsrc_info interpolated to all historical model time periods") %>%
      add_legacy_name("L221.DepRsrcPrice_en") %>%
      add_precursors("energy/A21.rsrc_info") ->
      L221.DepRsrcPrice_en

    L221.TechCoef_en_Traded %>%
      add_title("Coefficients of traded technologies") %>%
      add_units("unitless") %>%
      add_comments("A21.tradedtech_coef interpolated to all model periods and written out to all regions") %>%
      add_legacy_name("L221.TechCoef_en_Traded") %>%
      add_precursors("energy/A21.tradedtech_coef", "common/GCAM_region_names") ->
      L221.TechCoef_en_Traded

    L221.TechCost_en_Traded %>%
      add_title("Costs of traded technologies") %>%
      add_units("1975$") %>%
      add_comments("A21.tradedtech_cost interpolated to all model periods and written out to all regions") %>%
      add_legacy_name("L221.TechCost_en_Traded") %>%
      add_precursors("energy/A21.tradedtech_cost", "common/GCAM_region_names") ->
      L221.TechCost_en_Traded

    L221.TechShrwt_en_Traded %>%
      add_title("Shareweights of traded technologies") %>%
      add_units("unitless") %>%
      add_comments("A21.tradedtech_shrwt interpolated to all model periods and written out to all regions") %>%
      add_legacy_name("L221.TechShrwt_en_Traded") %>%
      add_precursors("energy/A21.tradedtech_shrwt", "common/GCAM_region_names") ->
      L221.TechShrwt_en_Traded

    L221.StubTechCoef_unoil %>%
      add_title("Coefficient and market name of stub technologies for importing traded unconventional oil") %>%
      add_units("unitless") %>%
      add_comments("L221.GlobalTechCoef_en written to all regions for traded technologies") %>%
      add_legacy_name("L221.StubTechCoef_unoil") %>%
      add_precursors("energy/A21.globaltech_coef", "common/GCAM_region_names", "energy/A21.tradedtech_shrwt") ->
      L221.StubTechCoef_unoil

    L221.Production_unoil %>%
      add_title("Calibrated production of unconventional oil") %>%
      add_units("unitless") %>%
      add_comments("L111.Prod_EJ_R_F_Yh used to determine unconventional oil coefficients") %>%
      add_legacy_name("L221.Production_unoil") %>%
      add_precursors("energy/A21.tradedtech_coef", "L111.Prod_EJ_R_F_Yh") ->
      L221.Production_unoil

    L221.StubTechProd_oil_unoil %>%
      add_title("Calibrated demand of unconventional oil") %>%
      add_units("unitless") %>%
      add_comments("Demand for unoil from L121.in_EJ_R_TPES_unoil_Yh used to determine calibrated output value and shareweights") %>%
      add_legacy_name("L221.StubTechProd_oil_unoil") %>%
      add_precursors("L121.in_EJ_R_TPES_unoil_Yh", "energy/A21.globaltech_shrwt", "common/GCAM_region_names", "energy/A_regions") ->
      L221.StubTechProd_oil_unoil

    L221.StubTechProd_oil_crude %>%
      add_title("Calibrated demand of crude oil") %>%
      add_units("unitless") %>%
      add_comments("Demand for crude from L121.in_EJ_R_TPES_unoil_Yh used to determine calibrated output value and shareweights") %>%
      add_legacy_name("L221.StubTechProd_oil_crude") %>%
      add_precursors("L121.in_EJ_R_TPES_crude_Yh", "energy/A21.globaltech_shrwt", "common/GCAM_region_names", "energy/A_regions") ->
      L221.StubTechProd_oil_crude

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
                L221.SubsectorInterpTo_en, L221.StubTech_en, L221.GlobalTechCoef_en,
                L221.GlobalTechCost_en, L221.GlobalTechShrwt_en, L221.PrimaryConsKeyword_en,
                L221.StubTechFractSecOut_en, L221.StubTechFractProd_en, L221.StubTechFractCalPrice_en, L221.DepRsrc_en,
                L221.DepRsrcPrice_en, L221.TechCoef_en_Traded, L221.TechCost_en_Traded,
                L221.TechShrwt_en_Traded, L221.StubTechCoef_unoil, L221.Production_unoil,
                L221.StubTechProd_oil_unoil, L221.StubTechProd_oil_crude, L221.StubTechCalInput_bioOil,
                L221.StubTechInterp_bioOil, L221.StubTechShrwt_bioOil)
  } else {
    stop("Unknown command")
  }
}
