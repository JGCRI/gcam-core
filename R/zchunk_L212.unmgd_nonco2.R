#' module_emissions_L212.unmgd_nonco2
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L212.AgSupplySectorLogitTables[[ curr_table ]]$data}, \code{L212.AgSupplySector}, \code{L212.SubsectorLogitTables[[ curr_table ]]$data}, \code{L212.AgSupplySubsector}, \code{L212.ItemName}, \code{L212.GRASSEmissions}, \code{L212.FORESTEmissions_FF}, \code{L212.FORESTEmissions_D}, \code{L212.GRASSEmissionsFactors_BCOC}, \code{L212.FORESTEmissionsFactors_BCOC_FF}, \code{L212.FORESTEmissionsFactors_BCOC_D}, \code{L212.FORESTEmissionsFactors_future}, \code{L212.ItemName_prot}, \code{L212.GRASSEmissions_prot}, \code{L212.GRASSEmissions_noprot}, \code{L212.FORESTEmissions_FF_prot}, \code{L212.FORESTEmissions_FF_noprot}, \code{L212.FORESTEmissions_D_prot}, \code{L212.FORESTEmissions_D_noprot}, \code{L212.GRASSEmissionsFactors_BCOC_prot}, \code{L212.GRASSEmissionsFactors_BCOC_noprot}, \code{L212.FORESTEmissionsFactors_BCOC_FF_prot}, \code{L212.FORESTEmissionsFactors_BCOC_FF_noprot}, \code{L212.FORESTEmissionsFactors_BCOC_D_prot}, \code{L212.FORESTEmissionsFactors_BCOC_D_noprot}. The corresponding file in the
#' original data system was \code{L212.unmgd_nonco2.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L212.unmgd_nonco2_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "emissions/A_regions",
             "L124.nonco2_tg_R_grass_Y_GLU",
             "L124.nonco2_tg_R_forest_Y_GLU",
             "L124.deforest_coefs",
             "L125.bcoc_tgbkm2_R_grass_2000",
             "L125.bcoc_tgbkm2_R_forest_2000",
             "L125.deforest_coefs_bcoc"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L212.AgSupplySectorLogitTables[[ curr_table ]]$data",
             "L212.AgSupplySector",
             "L212.SubsectorLogitTables[[ curr_table ]]$data",
             "L212.AgSupplySubsector",
             "L212.ItemName",
             "L212.GRASSEmissions",
             "L212.FORESTEmissions_FF",
             "L212.FORESTEmissions_D",
             "L212.GRASSEmissionsFactors_BCOC",
             "L212.FORESTEmissionsFactors_BCOC_FF",
             "L212.FORESTEmissionsFactors_BCOC_D",
             "L212.FORESTEmissionsFactors_future",
             "L212.ItemName_prot",
             "L212.GRASSEmissions_prot",
             "L212.GRASSEmissions_noprot",
             "L212.FORESTEmissions_FF_prot",
             "L212.FORESTEmissions_FF_noprot",
             "L212.FORESTEmissions_D_prot",
             "L212.FORESTEmissions_D_noprot",
             "L212.GRASSEmissionsFactors_BCOC_prot",
             "L212.GRASSEmissionsFactors_BCOC_noprot",
             "L212.FORESTEmissionsFactors_BCOC_FF_prot",
             "L212.FORESTEmissionsFactors_BCOC_FF_noprot",
             "L212.FORESTEmissionsFactors_BCOC_D_prot",
             "L212.FORESTEmissionsFactors_BCOC_D_noprot"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_regions <- get_data(all_data, "emissions/A_regions")
    L124.nonco2_tg_R_grass_Y_GLU <- get_data(all_data, "L124.nonco2_tg_R_grass_Y_GLU")
    L124.nonco2_tg_R_forest_Y_GLU <- get_data(all_data, "L124.nonco2_tg_R_forest_Y_GLU")
    L124.deforest_coefs <- get_data(all_data, "L124.deforest_coefs")
    L125.bcoc_tgbkm2_R_grass_2000 <- get_data(all_data, "L125.bcoc_tgbkm2_R_grass_2000")
    L125.bcoc_tgbkm2_R_forest_2000 <- get_data(all_data, "L125.bcoc_tgbkm2_R_forest_2000")
    L125.deforest_coefs_bcoc <- get_data(all_data, "L125.deforest_coefs_bcoc")

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
    # NOTE: This code uses vecpaste
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
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
      add_legacy_name("L212.AgSupplySectorLogitTables[[ curr_table ]]$data") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.AgSupplySectorLogitTables[[ curr_table ]]$data
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.AgSupplySector") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.AgSupplySector
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.SubsectorLogitTables[[ curr_table ]]$data") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.SubsectorLogitTables[[ curr_table ]]$data
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.AgSupplySubsector") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.AgSupplySubsector
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.ItemName") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.ItemName
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissions") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.GRASSEmissions
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissions_FF") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissions_FF
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissions_D") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissions_D
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissionsFactors_BCOC") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.GRASSEmissionsFactors_BCOC
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_FF") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_BCOC_FF
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_D") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_BCOC_D
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_future") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_future
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.ItemName_prot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.ItemName_prot
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissions_prot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.GRASSEmissions_prot
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissions_noprot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.GRASSEmissions_noprot
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissions_FF_prot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissions_FF_prot
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissions_FF_noprot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissions_FF_noprot
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissions_D_prot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissions_D_prot
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissions_D_noprot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissions_D_noprot
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissionsFactors_BCOC_prot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.GRASSEmissionsFactors_BCOC_prot
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissionsFactors_BCOC_noprot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.GRASSEmissionsFactors_BCOC_noprot
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_FF_prot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_BCOC_FF_prot
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_FF_noprot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_BCOC_FF_noprot
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_D_prot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_BCOC_D_prot
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_D_noprot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_BCOC_D_noprot

    return_data(L212.AgSupplySectorLogitTables[[ curr_table ]]$data, L212.AgSupplySector, L212.SubsectorLogitTables[[ curr_table ]]$data, L212.AgSupplySubsector, L212.ItemName, L212.GRASSEmissions, L212.FORESTEmissions_FF, L212.FORESTEmissions_D, L212.GRASSEmissionsFactors_BCOC, L212.FORESTEmissionsFactors_BCOC_FF, L212.FORESTEmissionsFactors_BCOC_D, L212.FORESTEmissionsFactors_future, L212.ItemName_prot, L212.GRASSEmissions_prot, L212.GRASSEmissions_noprot, L212.FORESTEmissions_FF_prot, L212.FORESTEmissions_FF_noprot, L212.FORESTEmissions_D_prot, L212.FORESTEmissions_D_noprot, L212.GRASSEmissionsFactors_BCOC_prot, L212.GRASSEmissionsFactors_BCOC_noprot, L212.FORESTEmissionsFactors_BCOC_FF_prot, L212.FORESTEmissionsFactors_BCOC_FF_noprot, L212.FORESTEmissionsFactors_BCOC_D_prot, L212.FORESTEmissionsFactors_BCOC_D_noprot)
  } else {
    stop("Unknown command")
  }
}
