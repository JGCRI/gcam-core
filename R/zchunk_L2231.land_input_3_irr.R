#' module_aglu_L2231.land_input_3_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2231.LN3_LogitTables[[ curr_table_name ]]}, \code{L2231.LN3_Logit}, \code{L2231.LN3_HistUnmgdAllocation}, \code{L2231.LN3_UnmgdAllocation}, \code{L2231.NodeEquiv}, \code{L2231.LN3_NoEmissCarbon}, \code{L2231.LN3_NodeCarbon}, \code{L2231.LN3_HistMgdAllocation_noncrop}, \code{L2231.LN3_MgdAllocation_noncrop}, \code{L2231.LN3_UnmgdCarbon}, \code{L2231.LN3_MgdCarbon_noncrop}. The corresponding file in the
#' original data system was \code{L2231.land_input_3_irr.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2231.land_input_3_irr_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L223.LN3_Logit",
 "L223.LN3_HistUnmgdAllocation",
 "L223.LN3_UnmgdAllocation",
 "L223.NodeEquiv",
 "L223.LN3_NoEmissCarbon",
 "L223.LN3_NodeCarbon",
 "L223.LN3_HistMgdAllocation_noncrop",
 "L223.LN3_MgdAllocation_noncrop",
 "L223.LN3_UnmgdCarbon",
 "L223.LN3_MgdCarbon_noncrop"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2231.LN3_LogitTables[[ curr_table_name ]]",
"L2231.LN3_Logit",
"L2231.LN3_HistUnmgdAllocation",
"L2231.LN3_UnmgdAllocation",
"L2231.NodeEquiv",
"L2231.LN3_NoEmissCarbon",
"L2231.LN3_NodeCarbon",
"L2231.LN3_HistMgdAllocation_noncrop",
"L2231.LN3_MgdAllocation_noncrop",
"L2231.LN3_UnmgdCarbon",
"L2231.LN3_MgdCarbon_noncrop"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      L223.LN3_Logit <- get_data(all_data, "L223.LN3_Logit")
  L223.LN3_HistUnmgdAllocation <- get_data(all_data, "L223.LN3_HistUnmgdAllocation")
  L223.LN3_UnmgdAllocation <- get_data(all_data, "L223.LN3_UnmgdAllocation")
  L223.NodeEquiv <- get_data(all_data, "L223.NodeEquiv")
  L223.LN3_NoEmissCarbon <- get_data(all_data, "L223.LN3_NoEmissCarbon")
  L223.LN3_NodeCarbon <- get_data(all_data, "L223.LN3_NodeCarbon")
  L223.LN3_HistMgdAllocation_noncrop <- get_data(all_data, "L223.LN3_HistMgdAllocation_noncrop")
  L223.LN3_MgdAllocation_noncrop <- get_data(all_data, "L223.LN3_MgdAllocation_noncrop")
  L223.LN3_UnmgdCarbon <- get_data(all_data, "L223.LN3_UnmgdCarbon")
  L223.LN3_MgdCarbon_noncrop <- get_data(all_data, "L223.LN3_MgdCarbon_noncrop")

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
 add_legacy_name("L2231.LN3_LogitTables[[ curr_table_name ]]") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L2231.LN3_LogitTables[[ curr_table_name ]]
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2231.LN3_Logit") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L2231.LN3_Logit
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2231.LN3_HistUnmgdAllocation") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L2231.LN3_HistUnmgdAllocation
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2231.LN3_UnmgdAllocation") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L2231.LN3_UnmgdAllocation
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2231.NodeEquiv") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L2231.NodeEquiv
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2231.LN3_NoEmissCarbon") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L2231.LN3_NoEmissCarbon
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2231.LN3_NodeCarbon") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L2231.LN3_NodeCarbon
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2231.LN3_HistMgdAllocation_noncrop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L2231.LN3_HistMgdAllocation_noncrop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2231.LN3_MgdAllocation_noncrop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L2231.LN3_MgdAllocation_noncrop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2231.LN3_UnmgdCarbon") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L2231.LN3_UnmgdCarbon
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2231.LN3_MgdCarbon_noncrop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L2231.LN3_MgdCarbon_noncrop

    return_data(L2231.LN3_LogitTables[[ curr_table_name ]], L2231.LN3_Logit, L2231.LN3_HistUnmgdAllocation, L2231.LN3_UnmgdAllocation, L2231.NodeEquiv, L2231.LN3_NoEmissCarbon, L2231.LN3_NodeCarbon, L2231.LN3_HistMgdAllocation_noncrop, L2231.LN3_MgdAllocation_noncrop, L2231.LN3_UnmgdCarbon, L2231.LN3_MgdCarbon_noncrop)
  } else {
    stop("Unknown command")
  }
}



