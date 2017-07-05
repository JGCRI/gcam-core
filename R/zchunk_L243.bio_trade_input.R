#' module_aglu_L243.bio_trade_input
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L243.DeleteInput_RegBio}, \code{L243.SectorLogitTables[[ curr_table ]]$data}, \code{L243.TechCoef_RegBio}, \code{L243.Supplysector_Bio}, \code{L243.SectorUseTrialMarket_Bio}, \code{L243.SubsectorLogitTables[[ curr_table ]]$data}, \code{L243.SubsectorLogit_Bio}, \code{L243.SubsectorShrwtFllt_TotBio}, \code{L243.SubsectorShrwtFllt_TradedBio}, \code{L243.GlobalTechCoef_TotBio}, \code{L243.GlobalTechShrwt_TotBio}, \code{L243.StubTech_TotBio}, \code{L243.StubTechShrwt_TotBio}, \code{L243.StubTechCoef_ImportedBio}, \code{L243.StubTechCoef_DomesticBio}, \code{L243.TechCoef_TradedBio}, \code{L243.TechShrwt_TradedBio}, \code{L243.SubsectorShrwtFllt_TotBio_SSP4}, \code{L243.SubsectorShrwtFllt_TradedBio_SSP4}, \code{L243.TechShrwt_TradedBio_SSP4}, \code{L243.StubTechShrwt_TotBio_SSP4}, \code{L243.SubsectorShrwtFllt_TotBio_SSP3}, \code{L243.StubTechShrwt_TotBio_SSP3}. The corresponding file in the
#' original data system was \code{L243.bio_trade_input.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L243.bio_trade_input <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "aglu/A_bio_supplysector",
             FILE = "aglu/A_bio_subsector_logit",
             FILE = "aglu/A_bio_subsector",
             "L120.LC_bm2_R_LT_Yh_GLU",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L243.DeleteInput_RegBio",
             "L243.SectorLogitTables",
             "L243.TechCoef_RegBio",
             "L243.Supplysector_Bio",
             "L243.SectorUseTrialMarket_Bio",
             "L243.SubsectorLogitTables",
             "L243.SubsectorLogit_Bio",
             "L243.SubsectorShrwtFllt_TotBio",
             "L243.SubsectorShrwtFllt_TradedBio",
             "L243.GlobalTechCoef_TotBio",
             "L243.GlobalTechShrwt_TotBio",
             "L243.StubTech_TotBio",
             "L243.StubTechShrwt_TotBio",
             "L243.StubTechCoef_ImportedBio",
             "L243.StubTechCoef_DomesticBio",
             "L243.TechCoef_TradedBio",
             "L243.TechShrwt_TradedBio",
             "L243.SubsectorShrwtFllt_TotBio_SSP4",
             "L243.SubsectorShrwtFllt_TradedBio_SSP4",
             "L243.TechShrwt_TradedBio_SSP4",
             "L243.StubTechShrwt_TotBio_SSP4",
             "L243.SubsectorShrwtFllt_TotBio_SSP3",
             "L243.StubTechShrwt_TotBio_SSP3"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_bio_supplysector <- get_data(all_data, "aglu/A_bio_supplysector")
    A_bio_subsector_logit <- get_data(all_data, "aglu/A_bio_subsector_logit")
    A_bio_subsector <- get_data(all_data, "aglu/A_bio_subsector")
    L120.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_LT_Yh_GLU")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    # Note: I'm pulling out all of the hard-coded GCAM sector names used in this file to the top so it is clearer
    OLD.REGIONAL.BIOMASS.NAME <- "regional biomass"
    NEW.REGIONAL.BIOMASS.NAME <- "total biomass"
    TRADED.BIOMASS.NAME <- "traded biomass"
    BIOMASS.NAME <- "biomass"
    BIOMASS.TRADE.REGION <- "USA"
    DOMESTIC.BIOMASS.NAME <- "domestic biomass"
    INTERNATIONAL.BIOMASS.NAME <- "imported biomass"

    # First, create a table to delete existing regional biomass input (this needs to include region/sector/subsector/technology/input for all regions & years)
    GCAM_region_names %>%
      select(region) %>%
      mutate(supplysector = OLD.REGIONAL.BIOMASS.NAME, subsector = OLD.REGIONAL.BIOMASS.NAME, technology = OLD.REGIONAL.BIOMASS.NAME) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(minicam.energy.input = BIOMASS.NAME) ->
      L243.DeleteInput_RegBio

    # Set up all of the new supply sectors.
    # Copy sector information to each region.
    # Note that "traded biomass" only goes in the BIOMASS.TRADE.REGION
    A_bio_supplysector %>%
      repeat_add_columns(tibble::tibble(region = GCAM_region_names$region)) %>%
      mutate(region = if_else(traded == 1, BIOMASS.TRADE.REGION, region)) %>%
      mutate(logit.year.fillout = min(MODEL_YEARS)) %>%
      select(-traded, -logit.type) ->
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
      repeat_add_columns(tibble::tibble(subsector.name = c(DOMESTIC.BIOMASS.NAME, INTERNATIONAL.BIOMASS.NAME))) %>%
      mutate(technology = subsector.name) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(minicam.energy.input = if_else(subsector.name == DOMESTIC.BIOMASS.NAME, BIOMASS.NAME, TRADED.BIOMASS.NAME)) %>%
      mutate(coefficient = 1) ->
      L243.GlobalTechCoef_TotBio

    # Add share-weights to the global technologies
    L243.GlobalTechCoef_TotBio %>%
      select(sector.name, subsector.name, technology, year) %>%
      left_join_error_no_match(select(A_bio_subsector, subsector, share.weight), by=c("subsector.name" = "subsector")) ->
      L243.GlobalTechShrwt_TotBio

    # Set up stub-technologies
    tibble(supplysector = NEW.REGIONAL.BIOMASS.NAME) %>%
      repeat_add_columns(tibble::tibble(subsector = c(DOMESTIC.BIOMASS.NAME, INTERNATIONAL.BIOMASS.NAME))) %>%
      mutate(stub.technology = subsector) %>%
      repeat_add_columns(tibble::tibble(region = GCAM_region_names$region)) ->
      L243.StubTech_TotBio

    # The traded markets tend to be a good candidate to solve explicitly since they tie together many solved markets.
    # Set flag so traded biomass uses trial markets
    A_bio_supplysector %>%
      filter(traded == 1) %>%
      select(supplysector) %>%
      mutate(region = BIOMASS.TRADE.REGION, use.trial.market = 1) ->
      L243.SectorUseTrialMarket_Bio

    if( OLD_DATA_SYSTEM_BEHAVIOR ) {
      # The old data system repeated the USA row 32 times. The xml conversion only needs the first instance.
      L243.SectorUseTrialMarket_Bio %>%
        repeat_add_columns(tibble::tibble(MORE = 1:32)) %>%
        select(-MORE) ->
        L243.SectorUseTrialMarket_Bio
    }

    # Set up all of the new subsectors.
    # Copy subsector information to each region.
    # Note that "traded biomass" only goes in the BIOMASS.TRADE.REGION, but the subsector name includes the original region name
    A_bio_subsector_logit %>%
      repeat_add_columns(tibble::tibble(region = GCAM_region_names$region)) %>%
      mutate(subsector = if_else(traded == 1, paste(region, subsector), subsector)) %>%
      mutate(region = if_else(traded == 1, BIOMASS.TRADE.REGION, region)) %>%
      mutate(logit.year.fillout = min(MODEL_YEARS)) %>%
      select(-traded, -logit.type) ->
      L243.SubsectorLogit_Bio

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
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.SectorLogitTables") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.SectorLogitTables
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
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.SubsectorLogitTables") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.SubsectorLogitTables
    L243.SubsectorLogit_Bio %>%
      add_title("Logit exponents for bio trade subsectors") %>%
      add_units("unitless") %>%
      add_comments("Copy bio trade subsector assumption file to all regions") %>%
      add_comments("Modify subsector name to include the region for the 'traded biomass' sectors.") %>%
      add_comments("Replace region name for 'traded biomass' with the trade region (currently USA).") %>%
      add_legacy_name("L243.SubsectorLogit_Bio") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_subsector_logit") ->
      L243.SubsectorLogit_Bio
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.SubsectorShrwtFllt_TotBio") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.SubsectorShrwtFllt_TotBio
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.SubsectorShrwtFllt_TradedBio") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
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
      same_precursors_as("L243.GlobalTechCoef_TotBio") %>%
      add_precursors("aglu/A_bio_subsector") ->
      L243.GlobalTechShrwt_TotBio
    L243.StubTech_TotBio %>%
      add_title("Stub technologies for the 'total biomass' sector") %>%
      add_units("NA") %>%
      add_comments("Copy technology names to all regions") %>%
      add_legacy_name("L243.StubTech_TotBio") %>%
      add_precursors("common/GCAM_region_names") ->
      L243.StubTech_TotBio
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.StubTechShrwt_TotBio") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.StubTechShrwt_TotBio
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.StubTechCoef_ImportedBio") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.StubTechCoef_ImportedBio
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.StubTechCoef_DomesticBio") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.StubTechCoef_DomesticBio
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.TechCoef_TradedBio") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.TechCoef_TradedBio
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.TechShrwt_TradedBio") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.TechShrwt_TradedBio
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.SubsectorShrwtFllt_TotBio_SSP4") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.SubsectorShrwtFllt_TotBio_SSP4
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.SubsectorShrwtFllt_TradedBio_SSP4") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.SubsectorShrwtFllt_TradedBio_SSP4
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.TechShrwt_TradedBio_SSP4") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.TechShrwt_TradedBio_SSP4
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.StubTechShrwt_TotBio_SSP4") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.StubTechShrwt_TotBio_SSP4
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.SubsectorShrwtFllt_TotBio_SSP3") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.SubsectorShrwtFllt_TotBio_SSP3
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L243.StubTechShrwt_TotBio_SSP3") %>%
      add_precursors("common/GCAM_region_names", "aglu/A_bio_supplysector", "aglu/A_bio_subsector_logit", "aglu/A_bio_subsector", "L120.LC_bm2_R_LT_Yh_GLU", "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L243.StubTechShrwt_TotBio_SSP3

    return_data(L243.DeleteInput_RegBio, L243.SectorLogitTables, L243.TechCoef_RegBio, L243.Supplysector_Bio, L243.SectorUseTrialMarket_Bio, L243.SubsectorLogitTables, L243.SubsectorLogit_Bio, L243.SubsectorShrwtFllt_TotBio, L243.SubsectorShrwtFllt_TradedBio, L243.GlobalTechCoef_TotBio, L243.GlobalTechShrwt_TotBio, L243.StubTech_TotBio, L243.StubTechShrwt_TotBio, L243.StubTechCoef_ImportedBio, L243.StubTechCoef_DomesticBio, L243.TechCoef_TradedBio, L243.TechShrwt_TradedBio, L243.SubsectorShrwtFllt_TotBio_SSP4, L243.SubsectorShrwtFllt_TradedBio_SSP4, L243.TechShrwt_TradedBio_SSP4, L243.StubTechShrwt_TotBio_SSP4, L243.SubsectorShrwtFllt_TotBio_SSP3, L243.StubTechShrwt_TotBio_SSP3)
  } else {
    stop("Unknown command")
  }
}
