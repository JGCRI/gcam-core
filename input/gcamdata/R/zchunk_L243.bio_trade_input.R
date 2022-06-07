# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L243.bio_trade_input
#'
#' Establish regionally differentiated trade structure for bioenergy.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L243.DeleteInput_RegBio}, \code{L243.SectorLogitTables[[
#'   curr_table ]]$data}, \code{L243.TechCoef_RegBio}, \code{L243.Supplysector_Bio},
#'   \code{L243.SectorUseTrialMarket_Bio}, \code{L243.SubsectorLogitTables[[ curr_table ]]$data},
#'   \code{L243.SubsectorLogit_Bio}, \code{L243.SubsectorShrwt_TotBio}, \code{L243.SubsectorShrwtFllt_TradedBio},
#'   \code{L243.GlobalTechCoef_TotBio}, \code{L243.GlobalTechShrwt_TotBio}, \code{L243.StubTech_TotBio},
#'   \code{L243.StubTechShrwt_TotBio}, \code{L243.StubTechCoef_ImportedBio}, \code{L243.StubTechCoef_DomesticBio},
#'   \code{L243.TechCoef_TradedBio}, \code{L243.TechShrwt_TradedBio}, \code{L243.SubsectorShrwt_TotBio_SSP4},
#'   \code{L243.SubsectorShrwtFllt_TradedBio_SSP4}, \code{L243.SubsectorShrwt_TotBio_SSP3},
#'   \code{L243.StubTechShrwt_TotBio_SSP3}, \code{L243.Supplysector_reg_SSP4}, \code{L243.SubsectorInterpTo_tra_SSP4}.
#'   The corresponding file in the original data system was \code{L243.bio_trade_input.R} (aglu level2).
#' @details This chunk sets up a structure for regionally differentiated bioenergy trade. Each
#' region consumes a blend of domestic and international bioenergy. Regions can supply to either
#' the domestic or international market. Share weights in the default case depend on the amount of
#' cropland a region has, with the largest region having a share weight of 1. Share weights in SSP3
#' and SSP4 are adjusted to reflect the trade frictions in the SSP storylines.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter full_join if_else group_by left_join mutate select summarize
#' @importFrom tidyr replace_na
#' @author KVC July 2017
module_aglu_L243.bio_trade_input <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "aglu/A_bio_supplysector",
             FILE = "aglu/A_bio_subsector_logit",
             FILE = "aglu/A_bio_subsector",
             FILE = "aglu/A_agRegionalSector",
             FILE = "aglu/A_agTradedSubsector",
             "L120.LC_bm2_R_LT_Yh_GLU",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L243.DeleteInput_RegBio",
             "L243.TechCoef_RegBio",
             "L243.Supplysector_Bio",
             "L243.SectorUseTrialMarket_Bio",
             "L243.SubsectorLogit_Bio",
             "L243.SubsectorShrwt_TotBio",
             "L243.SubsectorShrwtFllt_TradedBio",
             "L243.GlobalTechCoef_TotBio",
             "L243.GlobalTechShrwt_TotBio",
             "L243.StubTech_TotBio",
             "L243.StubTechShrwt_TotBio",
             "L243.StubTechCoef_ImportedBio",
             "L243.StubTechCoef_DomesticBio",
             "L243.TechCoef_TradedBio",
             "L243.TechShrwt_TradedBio",
             "L243.SubsectorShrwt_TotBio_SSP4",
             "L243.SubsectorShrwtFllt_TradedBio_SSP4",
             "L243.SubsectorShrwt_TotBio_SSP3",
             "L243.StubTechShrwt_TotBio_SSP3",
             "L243.Supplysector_reg_SSP4",
             "L243.SubsectorInterpTo_tra_SSP4"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- traded <- subsector.name <- sector.name <- technology <- year <- subsector <-
      share.weight <- supplysector <- MORE <- market.name <- Land_Type <- GCAM_region_ID <-
      value <- Cropland <- year.fillout <- trade.region <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    A_bio_supplysector <- get_data(all_data, "aglu/A_bio_supplysector", strip_attributes = TRUE)
    A_bio_subsector_logit <- get_data(all_data, "aglu/A_bio_subsector_logit", strip_attributes = TRUE)
    A_bio_subsector <- get_data(all_data, "aglu/A_bio_subsector")
    A_agRegionalSector <- get_data(all_data, "aglu/A_agRegionalSector", strip_attributes = TRUE)
    A_agTradedSubsector <- get_data(all_data, "aglu/A_agTradedSubsector", strip_attributes = TRUE)
    L120.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_LT_Yh_GLU")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    # Note: I'm pulling out all of the hard-coded GCAM sector names used in this file to the top so it is clearer
    OLD.REGIONAL.BIOMASS.NAME <- "regional biomass"
    NEW.REGIONAL.BIOMASS.NAME <- "total biomass"
    TRADED.BIOMASS.NAME <- "traded biomass"
    BIOMASS.NAME <- "biomass"
    BIOMASS.TRADE.REGION <- gcam.USA_REGION
    DOMESTIC.BIOMASS.NAME <- "domestic biomass"
    INTERNATIONAL.BIOMASS.NAME <- "imported biomass"

    # First, create a table to delete existing regional biomass input (this needs to include region/sector/subsector/technology/input for all regions & years)
    GCAM_region_names %>%
      select(region) %>%
      mutate(supplysector = OLD.REGIONAL.BIOMASS.NAME, subsector = OLD.REGIONAL.BIOMASS.NAME, technology = OLD.REGIONAL.BIOMASS.NAME) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(minicam.energy.input = BIOMASS.NAME) ->
      L243.DeleteInput_RegBio

    # Set up all of the new supply sectors.
    # Copy sector information to each region.
    # Note that "traded biomass" only goes in the BIOMASS.TRADE.REGION
    A_bio_supplysector %>%
      repeat_add_columns(tibble(region = GCAM_region_names$region)) %>%
      mutate(region = if_else(traded == 1, BIOMASS.TRADE.REGION, region),
             logit.year.fillout = min(MODEL_YEARS)) %>%
      select(-traded) ->
      L243.Supplysector_Bio

    # Now, create new regional biomass with input called "total biomass" with a input-output coefficient of 1 (i.e., a pass through sector)
    L243.DeleteInput_RegBio %>%
      mutate(minicam.energy.input = NEW.REGIONAL.BIOMASS.NAME,
             coefficient = 1,
             market.name = region) ->
      L243.TechCoef_RegBio

    # Set up the technologies for the new regional biomass sector ("total biomass")
    # This sector will include a domestic component and an international component. The domestic component
    # consumes "biomass" directly, while the international consumes "traded biomass".
    # These technologies have input-output coefficients of 1 (i.e., pass through sectors)
    tibble(sector.name = NEW.REGIONAL.BIOMASS.NAME) %>%
      repeat_add_columns(tibble(subsector.name = c(DOMESTIC.BIOMASS.NAME, INTERNATIONAL.BIOMASS.NAME))) %>%
      mutate(technology = subsector.name) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(minicam.energy.input = if_else(subsector.name == DOMESTIC.BIOMASS.NAME, BIOMASS.NAME, TRADED.BIOMASS.NAME),
             coefficient = 1) ->
      L243.GlobalTechCoef_TotBio

    # Add share-weights to the global technologies
    L243.GlobalTechCoef_TotBio %>%
      select(sector.name, subsector.name, technology, year) %>%
      # note the competition between import and domestic happens at the subsector nest
      # so share weights are not consequential here
      mutate(share.weight = 1.0) ->
      L243.GlobalTechShrwt_TotBio

    # Set up stub-technologies
    tibble(supplysector = NEW.REGIONAL.BIOMASS.NAME) %>%
      repeat_add_columns(tibble(subsector = c(DOMESTIC.BIOMASS.NAME, INTERNATIONAL.BIOMASS.NAME))) %>%
      mutate(stub.technology = subsector) %>%
      repeat_add_columns(tibble(region = GCAM_region_names$region)) ->
      L243.StubTech_TotBio

    # Share weights for the regional biomass technologies
    L243.StubTech_TotBio %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) ->
      L243.StubTechShrwt_TotBio

    # Stub technologies for the regional biomass technologies
    L243.StubTech_TotBio %>%
      filter(subsector == INTERNATIONAL.BIOMASS.NAME) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(minicam.energy.input = TRADED.BIOMASS.NAME,
             coefficient = 1,
             market = BIOMASS.TRADE.REGION) ->
      L243.StubTechCoef_ImportedBio

    # Stub technologies for the regional biomass technologies
    L243.StubTech_TotBio %>%
      filter(subsector == DOMESTIC.BIOMASS.NAME) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(minicam.energy.input = BIOMASS.NAME,
             coefficient = 1,
             market = region) ->
      L243.StubTechCoef_DomesticBio

    # The traded markets tend to be a good candidate to solve explicitly since they tie together many solved markets.
    # Set flag so traded biomass uses trial markets
    A_bio_supplysector %>%
      filter(traded == 1) %>%
      select(supplysector) %>%
      mutate(region = BIOMASS.TRADE.REGION, use.trial.market = 1) ->
      L243.SectorUseTrialMarket_Bio

    # Set up all of the new subsectors.
    # Copy subsector information to each region.
    # Note that "traded biomass" only goes in the BIOMASS.TRADE.REGION, but the subsector name includes the original region name
    A_bio_subsector_logit %>%
      repeat_add_columns(tibble(region = GCAM_region_names$region)) %>%
      mutate(subsector = if_else(traded == 1, paste(region, subsector), subsector),
             region = if_else(traded == 1, BIOMASS.TRADE.REGION, region),
             logit.year.fillout = min(MODEL_YEARS)) %>%
      select(-traded) ->
      L243.SubsectorLogit_Bio

    # Share weights for the regional biomass subsectors
    L243.StubTech_TotBio %>%
      select(region, supplysector, subsector) %>%
      repeat_add_columns(tibble(year=MODEL_YEARS)) %>%
      # expecting NAs for all the years that need to be filled in
      left_join(gather_years(A_bio_subsector, "share.weight"), by = c("supplysector", "subsector", "year")) %>%
      group_by(region, supplysector, subsector) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() ->
      L243.SubsectorShrwt_TotBio

    # Input name, market, coeff for traded biomass
    tibble(region = BIOMASS.TRADE.REGION, supplysector = TRADED.BIOMASS.NAME) %>%
      mutate(minicam.energy.input = BIOMASS.NAME,
             coefficient = 1) %>%
      repeat_add_columns(tibble(market.name = GCAM_region_names$region)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(subsector = paste(market.name, TRADED.BIOMASS.NAME),
             technology = subsector) ->
      L243.TechCoef_TradedBio

    # Compute share weights for subsectors based on cropland area. Largest region gets shareweight of 1.
    # This will mean that international biomass supply is roughly proportional to
    # cropland area (larger regions produce more biomass).
    L120.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type == "Cropland", year == max(HISTORICAL_YEARS)) %>%
      group_by(GCAM_region_ID) %>%
      summarize(Cropland = sum(value)) %>%
      ungroup() %>%
      mutate(share.weight = Cropland / max(Cropland)) %>%
      full_join(GCAM_region_names, by = "GCAM_region_ID") %>%       # Using full join to ensure regions without cropland show up
      replace_na(list(share.weight = 0)) %>%                      # Assign regions without land a shareweight of 0 (i.e., they won't contribute to international bio supply)
      select(-GCAM_region_ID, -Cropland) %>%
      mutate(supplysector = TRADED.BIOMASS.NAME,
             subsector = paste(region, TRADED.BIOMASS.NAME),
             region = BIOMASS.TRADE.REGION,
             year.fillout = min(MODEL_YEARS)) ->
      L243.SubsectorShrwtFllt_TradedBio

    # Set share weights for technologies in the traded biomass sector to 1
    # This is because competition is handled at the subsector level
    L243.SubsectorShrwtFllt_TradedBio %>%
      mutate(technology = subsector, share.weight = 1) %>%
      select(-year.fillout) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L243.TechShrwt_TradedBio

    # Set share weights for total biomass in SSP4 low income regions.
    # SSP4 assumes limited trade with low income regions; so, share weights are set to 0.1
    # See Calvin et al. (2017) for documentation. https://doi.org/10.1016/j.gloenvcha.2016.06.010
    L243.SubsectorShrwt_TotBio %>%
      filter(region %in% get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "low"),
             subsector == "imported biomass") %>%
      mutate(share.weight = if_else(year >= aglu.BIO_TRADE_SSP4_YEAR_FILLOUT, 0.1, share.weight)) ->
      L243.SubsectorShrwt_TotBio_SSP4_lo

    L243.SubsectorShrwtFllt_TradedBio %>%
      mutate(trade.region = gsub(" traded biomass", "", subsector)) %>%
      filter(trade.region %in% get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "low")) %>%
      mutate(year.fillout = aglu.BIO_TRADE_SSP4_YEAR_FILLOUT, share.weight = 0.1) %>%
      select(-trade.region) ->
      L243.SubsectorShrwtFllt_TradedBio_SSP4

    # Set share weights for total biomass in SSP4 high and medium income regions.
    # SSP4 assumes free trade within high income regions; so, subsector share weights are set to 0.5
    # (tech shareweights are not relevant as there is only one tech per subsector)
    # See Calvin et al. (2017) for documentation. https://doi.org/10.1016/j.gloenvcha.2016.06.010
    L243.SubsectorShrwt_TotBio %>%
      filter(region %in% c(get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "high"),
                          get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "medium")),
             subsector == "imported biomass") %>%
      mutate(share.weight = if_else(year >= aglu.BIO_TRADE_SSP4_YEAR_FILLOUT, 0.5, share.weight)) ->
      L243.SubsectorShrwt_TotBio_SSP4_hi

    # Bind SSP4 share weights for all regions into single data frames
    bind_rows(L243.SubsectorShrwt_TotBio_SSP4_hi, L243.SubsectorShrwt_TotBio_SSP4_lo) ->
      L243.SubsectorShrwt_TotBio_SSP4

    # Set share weights for total biomass in SSP3.
    # SSP3 assumes limited trade across regions; so, share weights are set to 0.1
    # See Calvin et al. (2017) for documentation. https://doi.org/10.1016/j.gloenvcha.2016.06.010
    L243.SubsectorShrwt_TotBio %>%
      filter(subsector == "imported biomass") %>%
      mutate(share.weight = if_else(year >= aglu.BIO_TRADE_SSP4_YEAR_FILLOUT, 0.1,  share.weight)) ->
      L243.SubsectorShrwt_TotBio_SSP3

    L243.StubTechShrwt_TotBio %>%
      filter(subsector == "imported biomass", year >= aglu.BIO_TRADE_SSP3_YEAR_FILLOUT) %>%
      mutate(share.weight = 0.1) ->
      L243.StubTechShrwt_TotBio_SSP3

    # SSP4 ag trade - reduce the logit exponents for the decision between imports and consumption of domestic
    # production, in low income regions
    SSP4_AG_LOGIT_IMPORT_LOW <- -1.5
    L243.Supplysector_reg_SSP4 <- mutate(A_agRegionalSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           filter(GCAM_region_names, region %in% get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "low"))) %>%
      mutate(logit.exponent = SSP4_AG_LOGIT_IMPORT_LOW)

    # SSP4 ag trade, cont'd - for the export market (sharing regions), decrease the share-weights from their base-year
    # calibrated values towards zero in some year beyond the analysis window
    SSP4_ZERO_EXPORT_YEAR_LOW <- 2150
    A_agTradedSubsector_SSP4 <- mutate(A_agTradedSubsector, to.value = 0,
                                       to.year = SSP4_ZERO_EXPORT_YEAR_LOW,
                                       interpolation.function = "linear")
    L243.SubsectorInterpTo_tra_SSP4 <- write_to_all_regions(A_agTradedSubsector_SSP4,
                                                            LEVEL2_DATA_NAMES[["SubsectorInterpTo"]],
                                                            filter(GCAM_region_names, region %in% get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "low")),
                                                            has_traded = TRUE) %>%
      mutate(region = gcam.USA_REGION)   # need to manually set the region name to gcam.USA_REGION, where the traded commodities are housed


    # Produce outputs
    L243.DeleteInput_RegBio %>%
      add_title("Table of regional biomass sector/subsector/technology/year for deletion") %>%
      add_units("NA") %>%
      add_comments("List all region/year combinations for the regional biomass sector/subsector/technology") %>%
      add_comments("We need to rename the input from 'biomass' to 'total biomass' so we can separate trade.") %>%
      add_comments("We also want to change the market from global to regional.") %>%
      add_comments("The only way to do this in the model is to delete these technologies and re-add them with the right input and market-name.") %>%
      add_legacy_name("L243.DeleteInput_RegBio") %>%
      add_precursors("common/GCAM_region_names") ->
      L243.DeleteInput_RegBio

    L243.TechCoef_RegBio %>%
      add_title("Table creating new 'traded biomass' sector/subsector/technology coefficients") %>%
      add_units("Unitless") %>%
      add_comments("Copy the L243.DeleteInput_RegBio table. Update the name of the input to 'total biomass'.") %>%
      add_comments("Add the input-output coefficient (equals 1 since this is a pass through sector).") %>%
      add_comments("Add the market name (equal to the region name since trade is no longer global).") %>%
      add_legacy_name("L243.TechCoef_RegBio") %>%
      same_precursors_as(L243.DeleteInput_RegBio) ->
      L243.TechCoef_RegBio

    L243.Supplysector_Bio %>%
      add_title("Units and logit exponents for bio trade supply sectors") %>%
      add_units("NA") %>%
      add_comments("Copy bio trade assumption file to all regions") %>%
      add_comments("Replace region name for 'traded biomass' with the trade region (currently USA).") %>%
      add_legacy_name("L243.Supplysector_Bio") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector") ->
      L243.Supplysector_Bio

    L243.SectorUseTrialMarket_Bio %>%
      add_title("Table flagging traded biomass sector to use trial markets") %>%
      add_units("NA") %>%
      add_comments("Create a table with the traded biomass sector and a boolean (1) for use.trial.markets") %>%
      add_legacy_name("L243.SectorUseTrialMarket_Bio") %>%
      add_precursors("aglu/A_bio_supplysector") ->
      L243.SectorUseTrialMarket_Bio

    L243.SubsectorLogit_Bio %>%
      add_title("Logit exponents for bio trade subsectors") %>%
      add_units("unitless") %>%
      add_comments("Copy bio trade subsector assumption file to all regions") %>%
      add_comments("Modify subsector name to include the region for the 'traded biomass' sectors.") %>%
      add_comments("Replace region name for 'traded biomass' with the trade region (currently USA).") %>%
      add_legacy_name("L243.SubsectorLogit_Bio") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_subsector_logit") ->
      L243.SubsectorLogit_Bio

    L243.SubsectorShrwt_TotBio %>%
      add_title("Subsector share weights for the total biomass sector") %>%
      add_units("unitless") %>%
      add_comments("Map the share weights specified in the assumption file to all subsectors") %>%
      add_precursors("aglu/A_bio_subsector") %>%
      same_precursors_as("L243.StubTech_TotBio") ->
      L243.SubsectorShrwt_TotBio

    L243.SubsectorShrwtFllt_TradedBio %>%
      add_title("Subsector shareweights for traded biomass sector") %>%
      add_units("unitless") %>%
      add_comments("Use cropland area in the final historical year to calculate shareweights.") %>%
      add_comments("Share weights are proportional to cropland area, with the region with") %>%
      add_comments("the largest cropland area assigned a share weight of 1.") %>%
      add_legacy_name("L243.SubsectorShrwtFllt_TradedBio") %>%
      add_precursors("common/GCAM_region_names", "L120.LC_bm2_R_LT_Yh_GLU") ->
      L243.SubsectorShrwtFllt_TradedBio

    L243.GlobalTechCoef_TotBio %>%
      add_title("Total biomass technology coefficients") %>%
      add_units("unitless") %>%
      add_comments("Create table with 'domestic' and 'international' biomass subsectors.") %>%
      add_comments("Set minicam.energy.input for both types of subsector.") %>%
      add_comments("Set input-output coefficients to 1.") %>%
      add_comments("Copy to all model years.") %>%
      add_legacy_name("L243.GlobalTechCoef_TotBio") ->
      L243.GlobalTechCoef_TotBio

    L243.GlobalTechShrwt_TotBio %>%
      add_title("Share weights for domestic and imported biomass") %>%
      add_units("unitless") %>%
      add_comments("Copy L243.GlobalTechCoef_TotBio. Set share weights based on an assumption file") %>%
      add_legacy_name("L243.GlobalTechShrwt_TotBio") %>%
      same_precursors_as("L243.GlobalTechCoef_TotBio") ->
      L243.GlobalTechShrwt_TotBio

    L243.StubTech_TotBio %>%
      add_title("Stub technologies for the 'total biomass' sector") %>%
      add_units("NA") %>%
      add_comments("Copy technology names to all regions") %>%
      add_legacy_name("L243.StubTech_TotBio") %>%
      add_precursors("common/GCAM_region_names") ->
      L243.StubTech_TotBio

    L243.StubTechShrwt_TotBio %>%
      add_title("Share weights for the total biomass sector") %>%
      add_units("NA") %>%
      add_comments("Set share weight for 'total biomass' technologies equal to 1 in all regions") %>%
      add_legacy_name("L243.StubTechShrwt_TotBio") %>%
      same_precursors_as("L243.StubTech_TotBio") ->
      L243.StubTechShrwt_TotBio

    L243.StubTechCoef_ImportedBio %>%
      add_title("Input-output coefficients for imported biomass technologies") %>%
      add_units("unitless") %>%
      add_comments("Filter L243.StubTech_TotBio for the imported biomass sector") %>%
      add_comments("Set coefficients to 1 and market name to the traded region (currently USA)") %>%
      add_legacy_name("L243.StubTechCoef_ImportedBio") %>%
      same_precursors_as("L243.StubTech_TotBio") ->
      L243.StubTechCoef_ImportedBio

    L243.StubTechCoef_DomesticBio %>%
      add_title("Input-output coefficients for domestic biomass technologies") %>%
      add_units("unitless") %>%
      add_comments("Filter L243.StubTech_TotBio for the domestic biomass sector") %>%
      add_comments("Set coefficients to 1 and market name to region name") %>%
      add_legacy_name("L243.StubTechCoef_DomesticBio") %>%
      same_precursors_as("L243.StubTech_TotBio") ->
      L243.StubTechCoef_DomesticBio

    L243.TechCoef_TradedBio %>%
      add_title("Input-Output coefficients for traded biomass technologies") %>%
      add_units("unitless") %>%
      add_comments("Assign coefficient of 1 to all traded biomass technologies in all years") %>%
      add_legacy_name("L243.TechCoef_TradedBio") %>%
      add_precursors("common/GCAM_region_names") ->
      L243.TechCoef_TradedBio

    L243.TechShrwt_TradedBio %>%
      add_title("Technology shareweights for the traded biomass sector") %>%
      add_units("unitless") %>%
      add_comments("Use the region & subsector names from 'L243.SubsectorShrwtFllt_TradedBio") %>%
      add_comments("Set all share weights to 1 since competition is handled at subsector level.") %>%
      add_legacy_name("L243.TechShrwt_TradedBio") %>%
      same_precursors_as("L243.SubsectorShrwtFllt_TradedBio") ->
      L243.TechShrwt_TradedBio

    L243.SubsectorShrwt_TotBio_SSP4 %>%
      add_title("Subsector shareweights for total bioenergy in SSP4") %>%
      add_units("unitless") %>%
      add_comments("Uses the structure of L243.SubsectorShrwt_TotBio") %>%
      add_comments("Replace shareweights with assumed coefficients (0.1 for low income, 0.5 for high income)") %>%
      same_precursors_as("L243.SubsectorShrwt_TotBio") %>%
      add_precursors("common/GCAM_region_names", "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L243.SubsectorShrwt_TotBio_SSP4

    L243.SubsectorShrwtFllt_TradedBio_SSP4 %>%
      add_title("Subsector shareweights for traded bioenergy in SSP4") %>%
      add_units("unitless") %>%
      add_comments("Uses the structure of L243.SubsectorShrwtFllt_TradedBio") %>%
      add_comments("Replace shareweights with assumed coefficients (0.1 for low income, 0.5 for high income)") %>%
      add_legacy_name("L243.SubsectorShrwtFllt_TradedBio_SSP4") %>%
      same_precursors_as("L243.SubsectorShrwtFllt_TradedBio") %>%
      add_precursors("common/GCAM_region_names", "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L243.SubsectorShrwtFllt_TradedBio_SSP4

    L243.SubsectorShrwt_TotBio_SSP3 %>%
      add_title("Subsector shareweights for total bioenergy in SSP3") %>%
      add_units("unitless") %>%
      add_comments("Uses the structure of L243.SubsectorShrwt_TotBio") %>%
      add_comments("Replace shareweights with assumed coefficients (0.1)") %>%
      same_precursors_as("L243.SubsectorShrwt_TotBio") ->
      L243.SubsectorShrwt_TotBio_SSP3

    L243.StubTechShrwt_TotBio_SSP3 %>%
      add_title("Technology shareweights for total bioenergy in SSP3") %>%
      add_units("unitless") %>%
      add_comments("Uses the structure of L243.StubTechShrwt_TotBio") %>%
      add_comments("Replace shareweights with assumed coefficients (0.1)") %>%
      add_legacy_name("L243.StubTechShrwt_TotBio_SSP3") %>%
      same_precursors_as("L243.StubTechShrwt_TotBio") ->
      L243.StubTechShrwt_TotBio_SSP3

    L243.Supplysector_reg_SSP4 %>%
      add_title("Supplysector info for regional crop markets w modified logits for SSP4") %>%
      add_units("unitless") %>%
      add_comments("Only applies to low-income regions") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_agRegionalSector") ->
      L243.Supplysector_reg_SSP4

    L243.SubsectorInterpTo_tra_SSP4 %>%
      add_title("Subsector interpolation info for traded crop markets w modified share-weights for SSP4") %>%
      add_units("unitless") %>%
      add_comments("Only applies to low-income regions") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_agTradedSubsector") ->
      L243.SubsectorInterpTo_tra_SSP4

    return_data(L243.DeleteInput_RegBio,
                L243.TechCoef_RegBio,
                L243.Supplysector_Bio,
                L243.SectorUseTrialMarket_Bio,
                L243.SubsectorLogit_Bio,
                L243.SubsectorShrwt_TotBio,
                L243.SubsectorShrwtFllt_TradedBio,
                L243.GlobalTechCoef_TotBio,
                L243.GlobalTechShrwt_TotBio,
                L243.StubTech_TotBio,
                L243.StubTechShrwt_TotBio,
                L243.StubTechCoef_ImportedBio,
                L243.StubTechCoef_DomesticBio,
                L243.TechCoef_TradedBio,
                L243.TechShrwt_TradedBio,
                L243.SubsectorShrwt_TotBio_SSP4,
                L243.SubsectorShrwtFllt_TradedBio_SSP4,
                L243.SubsectorShrwt_TotBio_SSP3,
                L243.StubTechShrwt_TotBio_SSP3,
                L243.Supplysector_reg_SSP4,
                L243.SubsectorInterpTo_tra_SSP4)
  } else {
    stop("Unknown command")
  }
}
