#' module_gcam.usa_L226.en_distribution_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L226.DeleteSupplysector_USAelec}, \code{object}, \code{L226.StubTechCoef_electd_USA}, \code{L226.TechShrwt_electd_USA}, \code{L226.TechCost_electd_USA}, \code{L226.TechCoef_electd_USA}, \code{L226.SectorLogitTables_en_USA[[ curr_table ]]$data}, \code{L226.Supplysector_en_USA}, \code{L226.SubsectorShrwtFllt_en_USA}, \code{L226.SubsectorLogitTables_en_USA[[ curr_table ]]$data}, \code{L226.SubsectorLogit_en_USA}, \code{L226.TechShrwt_en_USA}, \code{L226.TechCoef_en_USA}, \code{L226.TechCost_en_USA}, \code{L226.Ccoef}. The corresponding file in the
#' original data system was \code{L226.en_distribution_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L226.en_distribution_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/A21.sector",
             FILE = "energy/A26.sector",
             FILE = "gcam-usa/EIA_state_energy_prices",
             "L202.CarbonCoef",
             "L226.Supplysector_en",
             "L226.SubsectorLogit_en",
             "L226.SubsectorShrwt_en",
             "L226.SubsectorShrwtFllt_en",
             "L226.SubsectorInterp_en",
             "L226.SubsectorInterpTo_en",
             "L226.GlobalTechCost_en",
             "L226.GlobalTechShrwt_en",
             "L226.StubTechCoef_electd"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L226.DeleteSupplysector_USAelec",
             "L226.StubTechCoef_electd_USA",
             "L226.TechShrwt_electd_USA",
             "L226.TechCost_electd_USA",
             "L226.TechCoef_electd_USA",
             "L226.Supplysector_en_USA",
             "L226.SubsectorShrwtFllt_en_USA",
             "L226.SubsectorLogit_en_USA",
             "L226.TechShrwt_en_USA",
             "L226.TechCoef_en_USA",
             "L226.TechCost_en_USA",
             "L226.Ccoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A21.sector <- get_data(all_data, "energy/A21.sector")
    A26.sector <- get_data(all_data, "energy/A26.sector")
    EIA_state_energy_prices <- get_data(all_data, "gcam-usa/EIA_state_energy_prices")
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef")
    L226.Supplysector_en <- get_data(all_data, "L226.Supplysector_en")
    L226.SubsectorLogit_en <- get_data(all_data, "L226.SubsectorLogit_en")
    L226.SubsectorShrwt_en <- get_data(all_data, "L226.SubsectorShrwt_en")
    L226.SubsectorShrwtFllt_en <- get_data(all_data, "L226.SubsectorShrwtFllt_en")
    L226.SubsectorInterp_en <- get_data(all_data, "L226.SubsectorInterp_en")
    L226.SubsectorInterpTo_en <- get_data(all_data, "L226.SubsectorInterpTo_en")
    L226.GlobalTechCost_en <- get_data(all_data, "L226.GlobalTechCost_en")
    L226.GlobalTechShrwt_en <- get_data(all_data, "L226.GlobalTechShrwt_en")
    L226.StubTechCoef_electd <- get_data(all_data, "L226.StubTechCoef_electd")


    # Build tables

    # Supplysector information

    # PART 1: FUEL HANDLING AND DELIVERY SECTORS

    # L226.Supplysector_en_USA: Supply sector information for energy handling and delivery sectors
    # NOTE: Currently using FERC regions as a proxy for regional energy markets
    A21.sector %>%
      select(supplysector, output.unit, input.unit, price.unit, logit.exponent, logit.type) %>%
      filter(supplysector %in%gcamusa.REGIONAL_FUEL_MARKETS) ->
      A21.tmp

    A26.sector  %>%
      select(supplysector, output.unit, input.unit, price.unit, logit.exponent, logit.type) %>%
      filter(supplysector %in%gcamusa.REGIONAL_FUEL_MARKETS) %>%
      bind_rows(A21.tmp, .) %>%
      repeat_add_columns(tibble(region = unique(states_subregions$grid_region))) %>%
      mutate(logit.year.fillout = min(BASE_YEARS)) ->
      L226.Supplysector_en_USA


    # L226.SubsectorShrwtFllt_en_USA: subsector shareweights of energy handling and delivery
    L226.Supplysector_en_USA %>%
      mutate(subsector = supplysector,
             year.fillout = min(BASE_YEARS),
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(one_of(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])) ->
      L226.SubsectorShrwtFllt_en_USA


    # L226.SubsectorLogit_en_USA
    # NOTE: There is only one tech per subsector so the logit choice does not matter
    L226.SubsectorShrwtFllt_en_USA %>%
      select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
      mutate(logit.year.fillout = min(BASE_YEARS),
             logit.exponent = gcamusa.DEFAULT_LOGITEXP,
             logit.type = NA) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "logit.type"))) ->
      L226.SubsectorLogit_en_USA










    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.DeleteSupplysector_USAelec") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.DeleteSupplysector_USAelec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.StubTechCoef_electd_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.StubTechCoef_electd_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.TechShrwt_electd_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.TechShrwt_electd_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.TechCost_electd_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.TechCost_electd_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.TechCoef_electd_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.TechCoef_electd_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.Supplysector_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.Supplysector_en_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.SubsectorShrwtFllt_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.SubsectorShrwtFllt_en_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.SubsectorLogit_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.SubsectorLogit_en_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.TechShrwt_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.TechShrwt_en_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.TechCoef_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.TechCoef_en_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.TechCost_en_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.TechCost_en_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L226.Ccoef") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/EIA_state_energy_prices",
                     "L202.CarbonCoef",
                     "L226.Supplysector_en",
                     "L226.SubsectorLogit_en",
                     "L226.SubsectorShrwt_en",
                     "L226.SubsectorShrwtFllt_en",
                     "L226.SubsectorInterp_en",
                     "L226.SubsectorInterpTo_en",
                     "L226.GlobalTechCost_en",
                     "L226.GlobalTechShrwt_en",
                     "L226.StubTechCoef_electd") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L226.Ccoef

    return_data(L226.DeleteSupplysector_USAelec, object, L226.StubTechCoef_electd_USA, L226.TechShrwt_electd_USA, L226.TechCost_electd_USA,
                L226.TechCoef_electd_USA, L226.Supplysector_en_USA, L226.SubsectorShrwtFllt_en_USA, L226.SubsectorLogit_en_USA,
                L226.TechShrwt_en_USA, L226.TechCoef_en_USA, L226.TechCost_en_USA, L226.Ccoef)
  } else {
    stop("Unknown command")
  }
}
