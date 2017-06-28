#' module_aglu_L204.resbio_input
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L204.AgResBio_For}, \code{L204.GlobalResBio_Mill}, \code{L204.AgResBio_ag}, \code{L204.AgResBioCurve_For}, \code{L204.StubResBioCurve_Mill}, \code{L204.AgResBioCurve_ag}. The corresponding file in the
#' original data system was \code{L204.resbio_input.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L204.resbio_input_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/A_agSupplySector",
             FILE = "aglu/A_demand_technology",
             FILE = "aglu/A_resbio_curves",
             FILE = "aglu/A_bio_frac_prod_R",
             "L111.ag_resbio_R_C",
             "L103.ag_Prod_Mt_R_C_Y_GLU",
             "L123.For_Prod_bm3_R_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L204.AgResBio_For",
             "L204.GlobalResBio_Mill",
             "L204.AgResBio_ag",
             "L204.AgResBioCurve_For",
             "L204.StubResBioCurve_Mill",
             "L204.AgResBioCurve_ag"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_agSupplySector <- get_data(all_data, "aglu/A_agSupplySector")
    A_demand_technology <- get_data(all_data, "aglu/A_demand_technology")
    A_resbio_curves <- get_data(all_data, "aglu/A_resbio_curves")
    A_bio_frac_prod_R <- get_data(all_data, "aglu/A_bio_frac_prod_R")
    L111.ag_resbio_R_C <- get_data(all_data, "L111.ag_resbio_R_C")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L123.For_Prod_bm3_R_Y_GLU <- get_data(all_data, "L123.For_Prod_bm3_R_Y_GLU")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    #
    # If you find a mistake/thing to update in the old code and
    # fixing it will change the output data, causing the tests to fail,
    # (i) open an issue on GitHub, (ii) consult with colleagues, and
    # then (iii) code a fix:
    #
    # if(OLD_DATA_SYSTEM_BEHAVIOR) {
    #   ... code that replicates old, incorrect behavior
    # } else {
    #   ... new code with a fix
    # }
    #
    #
    # NOTE: there are `merge` calls in this code. Be careful!
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses repeat_and_add_vector
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L204.AgResBio_For") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L204.AgResBio_For
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L204.GlobalResBio_Mill") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L204.GlobalResBio_Mill
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L204.AgResBio_ag") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L204.AgResBio_ag
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L204.AgResBioCurve_For") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L204.AgResBioCurve_For
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L204.StubResBioCurve_Mill") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L204.StubResBioCurve_Mill
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L204.AgResBioCurve_ag") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L204.AgResBioCurve_ag

    return_data(L204.AgResBio_For, L204.GlobalResBio_Mill, L204.AgResBio_ag, L204.AgResBioCurve_For, L204.StubResBioCurve_Mill, L204.AgResBioCurve_ag)
  } else {
    stop("Unknown command")
  }
}
