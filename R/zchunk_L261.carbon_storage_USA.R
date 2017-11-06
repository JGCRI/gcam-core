#' module_gcam.usa_L261.carbon_storage_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L261.DeleteDepRsrc_USAC}, \code{L261.DeleteSubsector_USAC}, \code{L261.DepRsrc_FERC}, \code{L261.DepRsrcCurves_FERC}, \code{L261.Supplysector_C_USA}, \code{L261.SubsectorLogit_C_USA}, \code{L261.SubsectorShrwtFllt_C_USA}, \code{L261.StubTech_C_USA}, \code{L261.StubTechMarket_C_USA}. The corresponding file in the
#' original data system was \code{L261.carbon_storage_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L261.carbon_storage_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L161.Cstorage_FERC",
             "L261.DepRsrc",
             "L261.Supplysector_C",
             "L261.SubsectorLogit_C",
             "L261.SubsectorShrwtFllt_C",
             "L261.StubTech_C",
             "L261.GlobalTechCoef_C"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L261.DeleteDepRsrc_USAC",
             "L261.DeleteSubsector_USAC",
             "L261.DepRsrc_FERC",
             "L261.DepRsrcCurves_FERC",
             "L261.Supplysector_C_USA",
             "L261.SubsectorLogit_C_USA",
             "L261.SubsectorShrwtFllt_C_USA",
             "L261.StubTech_C_USA",
             "L261.StubTechMarket_C_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L161.Cstorage_FERC <- get_data(all_data, "L161.Cstorage_FERC")
    L261.DepRsrc <- get_data(all_data, "L261.DepRsrc")
    L261.Supplysector_C <- get_data(all_data, "L261.Supplysector_C")
    L261.SubsectorLogit_C <- get_data(all_data, "L261.SubsectorLogit_C")
    L261.SubsectorShrwtFllt_C <- get_data(all_data, "L261.SubsectorShrwtFllt_C")
    L261.StubTech_C <- get_data(all_data, "L261.StubTech_C")
    L261.GlobalTechCoef_C <- get_data(all_data, "L261.GlobalTechCoef_C")

    # ===================================================
    # Carbon storage onshore resources are modeled in the FERC regions with non-zero storage curves
    L161.Cstorage_FERC %>%
      select(grid_region) %>%
      unique %>%
      arrange %>%
      unlist ->
      C_grid_regions

    states_subregions %>%
      select(grid_region) %>%
      filter(!grid_region %in% C_grid_regions) %>%
      unique %>%
      unlist ->
      noC_grid_regions

    # L261.DeleteDepRsrc_USAC: delete onshore carbon storage in the USA
    L261.DepRsrc %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == "USA") %>%
      select(region, depresource) ->
      L261.DeleteDepRsrc_USAC

    # L261.DeleteSubsector_USAC: delete onshore carbon storage subsector of carbon storage sector in the USA
    # NOTE: leaving the offshore here so that the USA hydrogen sector has a carbon storage market
    L261.SubsectorShrwtFllt_C %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == "USA") %>%
      semi_join(L261.DepRsrc, by = c("subsector" = "depresource")) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["Subsector"]]))) ->
      L261.DeleteSubsector_USAC

    grid_Cstorage_nonexist <- paste(noC_grid_regions, L261.DeleteDepRsrc_USAC$depresource[1])

    # L261.DepRsrc_FERC: onshore storage in the FERC regions
    L261.DepRsrc %>%
      filter(region == "USA") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = C_grid_regions)) %>%
      mutate(market = region) ->
      L261.DepRsrc_FERC

   # L261.DepRsrcCurves_FERC: onshore storage supply curves in the FERC regions
    L161.Cstorage_FERC %>%
      mutate(region = grid_region,
             depresource = L261.DepRsrc_FERC$depresource[1],
             subresource = L261.DepRsrc_FERC$depresource[1],
             available = round(MtC, digits = gcamuse.DIGITS_DEPRESOURCE),
             extractioncost = round(Cost_1990USDtC, digits = gcamusa.DIGITS_COST)) %>%
      select(region, depresource, subresource, grade, available, extractioncost) ->
      L261.DepRsrcCurves_FERC

    # L261.Supplysector_C_USA: supplysector info in the states
    L261.Supplysector_C %>%
      filter(region == "USA") %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["Supplysector"]])) ->
      L261.Supplysector_C_USA

    # L261.SubsectorLogit_C_USA: subsector logit info in the states
    L261.SubsectorLogit_C %>%
      filter(region == "USA") %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]])) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
      # NOTE: This table contains logit values in states where no C storage resources may exist at the grid level
      filter(!paste(grid_region, subsector) %in% grid_Cstorage_nonexist) ->
      L261.SubsectorLogit_C_USA

    # L261.SubsectorShrwtFllt_C_USA: subsector shareweight info in the states
    L261.SubsectorShrwtFllt_C %>%
      filter(region == "USA") %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
      # NOTE: This table contains logit values in states where no C storage resources may exist at the grid level
      filter(!paste(grid_region, subsector) %in% grid_Cstorage_nonexist) ->
      L261.SubsectorShrwtFllt_C_USA

    # L261.StubTech_C_USA: stub technology info for the states
    L261.StubTech_C %>%
      filter(region == "USA") %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["StubTech"]])) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
      filter(!paste(grid_region, stub.technology) %in% grid_Cstorage_nonexist) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["StubTech"]])))->
      L261.StubTech_C_USA

    # L261.StubTechMarket_C_USA: stub technology market names for the grid regions
    L261.StubTech_C_USA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(select(L261.GlobalTechCoef_C, -coefficient),
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name", "stub.technology" = "technology", "year")) %>%
      left_join_error_no_match(select(states_subregions, state, market.name = grid_region), by = c("region" = "state")) %>%
      # Offshore carbon storage is from the USA market
      mutate(market.name = replace(market.name, !minicam.energy.input %in% L261.DepRsrc_FERC$depresource, "USA")) ->
      L261.StubTechMarket_C_USA

    # ===================================================

    # Produce outputs
    L261.DeleteDepRsrc_USAC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.DeleteDepRsrc_USAC") %>%
      add_precursors("L261.DepRsrc") ->
      L261.DeleteDepRsrc_USAC

    L261.DeleteSubsector_USAC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.DeleteSubsector_USAC") %>%
      add_precursors("L261.SubsectorShrwtFllt_C",
                     "L261.DepRsrc") ->
      L261.DeleteSubsector_USAC

    L261.DepRsrc_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.DepRsrc_FERC") %>%
      add_precursors("L161.Cstorage_FERC",
                     "L261.DepRsrc") ->
      L261.DepRsrc_FERC

    L261.DepRsrcCurves_FERC %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.DepRsrcCurves_FERC") %>%
      add_precursors("L161.Cstorage_FERC") %>%
      same_precursors_as("L261.DepRsrc_FERC") ->
      L261.DepRsrcCurves_FERC

    L261.Supplysector_C_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.Supplysector_C_USA") %>%
      add_precursors("L261.Supplysector_C") ->
      L261.Supplysector_C_USA

    L261.SubsectorLogit_C_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.SubsectorLogit_C_USA") %>%
      add_precursors("L161.Cstorage_FERC",
                     "gcam-usa/states_subregions",
                     "L261.SubsectorLogit_C") ->
      L261.SubsectorLogit_C_USA

    L261.SubsectorShrwtFllt_C_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.SubsectorShrwtFllt_C_USA") %>%
      add_precursors("L161.Cstorage_FERC",
                     "gcam-usa/states_subregions",
                     "L261.SubsectorShrwtFllt_C") ->
      L261.SubsectorShrwtFllt_C_USA

    L261.StubTech_C_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.StubTech_C_USA") %>%
      add_precursors("L161.Cstorage_FERC",
                     "gcam-usa/states_subregions",
                     "L261.StubTech_C") ->
      L261.StubTech_C_USA

    L261.StubTechMarket_C_USA %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.StubTechMarket_C_USA") %>%
      add_precursors("L261.GlobalTechCoef_C",
                     "L261.DepRsrc_FERC") %>%
      same_precursors_as("L261.StubTech_C_USA") ->
      L261.StubTechMarket_C_USA

    return_data(L261.DeleteDepRsrc_USAC, L261.DeleteSubsector_USAC, L261.DepRsrc_FERC,
                L261.DepRsrcCurves_FERC, L261.Supplysector_C_USA, L261.SubsectorLogit_C_USA,
                L261.SubsectorShrwtFllt_C_USA, L261.StubTech_C_USA, L261.StubTechMarket_C_USA)
  } else {
    stop("Unknown command")
  }
}
