#' module_aglu_L222.land_input_2
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{curr_table$data}, \code{L222.LN2_Logit}, \code{L222.LN2_HistUnmgdAllocation}, \code{L222.LN2_UnmgdAllocation}, \code{L222.LN2_HistMgdAllocation}, \code{L222.LN2_MgdAllocation}, \code{L222.LN2_UnmgdCarbon}, \code{L222.LN2_MgdCarbon}, \code{L222.LN2_HistUnmgdAllocation_noprot}, \code{L222.LN2_UnmgdAllocation_noprot}, \code{L222.LN2_UnmgdCarbon}, \code{curr_table$data}, \code{L222.LN1_Logit_prot}, \code{L222.LN1_HistUnmgdAllocation_prot}, \code{L222.LN1_UnmgdAllocation_prot}, \code{L222.LN1_UnmgdCarbon_prot}. The corresponding file in the
#' original data system was \code{L222.land_input_2.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L222.land_input_2_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/GCAMLandLeaf_CdensityLT",
             FILE = "aglu/A_LandNode_logit",
             FILE = "aglu/A_LandLeaf_Unmgd2",
             FILE = "aglu/A_LandLeaf2",
             FILE = "aglu/A_LT_Mapping",
             "L121.CarbonContent_kgm2_R_LT_GLU",
             "L125.LC_bm2_R_LT_Yh_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L222.LN2_Logit",
             "L222.LN2_HistUnmgdAllocation",
             "L222.LN2_UnmgdAllocation",
             "L222.LN2_HistMgdAllocation",
             "L222.LN2_MgdAllocation",
             "L222.LN2_UnmgdCarbon",
             "L222.LN2_MgdCarbon",
             "L222.LN2_HistUnmgdAllocation_noprot",
             "L222.LN2_UnmgdAllocation_noprot",
             "L222.LN2_UnmgdCarbon",
              "L222.LN1_Logit_prot",
             "L222.LN1_HistUnmgdAllocation_prot",
             "L222.LN1_UnmgdAllocation_prot",
             "L222.LN1_UnmgdCarbon_prot"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    GCAMLandLeaf_CdensityLT <- get_data(all_data, "aglu/GCAMLandLeaf_CdensityLT")
    A_LandNode_logit <- get_data(all_data, "aglu/A_LandNode_logit")
    A_LandLeaf_Unmgd2 <- get_data(all_data, "aglu/A_LandLeaf_Unmgd2")
    A_LandLeaf2 <- get_data(all_data, "aglu/A_LandLeaf2")
    A_LT_Mapping <- get_data(all_data, "aglu/A_LT_Mapping")
    L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")
    L125.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L125.LC_bm2_R_LT_Yh_GLU")

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
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
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
      add_legacy_name("L222.LN2_Logit") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_Logit

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_HistUnmgdAllocation") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_HistUnmgdAllocation

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_UnmgdAllocation") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_UnmgdAllocation

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_HistMgdAllocation") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_HistMgdAllocation

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_MgdAllocation") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_MgdAllocation

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_UnmgdCarbon") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_UnmgdCarbon

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_MgdCarbon") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_MgdCarbon

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_HistUnmgdAllocation_noprot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_HistUnmgdAllocation_noprot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_UnmgdAllocation_noprot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_UnmgdAllocation_noprot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_UnmgdCarbon") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_UnmgdCarbon

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN1_Logit_prot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN1_Logit_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN1_HistUnmgdAllocation_prot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN1_HistUnmgdAllocation_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN1_UnmgdAllocation_prot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN1_UnmgdAllocation_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN1_UnmgdCarbon_prot") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN1_UnmgdCarbon_prot

    return_data(L222.LN2_Logit, L222.LN2_HistUnmgdAllocation, L222.LN2_UnmgdAllocation,
                L222.LN2_HistMgdAllocation, L222.LN2_MgdAllocation, L222.LN2_UnmgdCarbon, L222.LN2_MgdCarbon,
                L222.LN2_HistUnmgdAllocation_noprot, L222.LN2_UnmgdAllocation_noprot, L222.LN1_Logit_prot,
                L222.LN1_HistUnmgdAllocation_prot, L222.LN1_UnmgdAllocation_prot, L222.LN1_UnmgdCarbon_prot)
  } else {
    stop("Unknown command")
  }
}
