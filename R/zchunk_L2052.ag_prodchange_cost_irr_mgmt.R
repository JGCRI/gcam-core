#' module_aglu_L2052.ag_prodchange_cost_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2052.AgCost_ag_irr_mgmt}, \code{L2052.AgCost_bio_irr_mgmt}, \code{L2052.AgCost_For}, \code{L2052.AgProdChange_ag_irr_ref}, \code{L2052.AgProdChange_bio_irr_ref}, \code{L2052.AgProdChange_irr_high}, \code{L2052.AgProdChange_irr_low}, \code{L2052.AgProdChange_irr_ssp4}. The corresponding file in the
#' original data system was \code{L2052.ag_prodchange_cost_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2052.ag_prodchange_cost_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L112.bio_YieldRate_R_Y_GLU",
             FILE = "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU",
             "L162.ag_YieldRate_R_C_Y_GLU_irr",
             "L162.bio_YieldRate_R_Y_GLU_irr",
             "L164.ag_Cost_75USDkg_C",
             FILE = "temp-data-inject/L123.For_Yield_m3m2_R_GLU",
             FILE = "temp-data-inject/L201.AgYield_bio_grass",
             FILE = "temp-data-inject/L201.AgYield_bio_tree",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2052.AgCost_ag_irr_mgmt",
             "L2052.AgCost_bio_irr_mgmt",
             "L2052.AgCost_For",
             "L2052.AgProdChange_ag_irr_ref",
             "L2052.AgProdChange_bio_irr_ref",
             "L2052.AgProdChange_irr_high",
             "L2052.AgProdChange_irr_low",
             "L2052.AgProdChange_irr_ssp4"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L112.bio_YieldRate_R_Y_GLU <- get_data(all_data, "L112.bio_YieldRate_R_Y_GLU")
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU")
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU")
    L162.ag_YieldRate_R_C_Y_GLU_irr <- get_data(all_data, "L162.ag_YieldRate_R_C_Y_GLU_irr")
    L162.bio_YieldRate_R_Y_GLU_irr <- get_data(all_data, "L162.bio_YieldRate_R_Y_GLU_irr")
    L164.ag_Cost_75USDkg_C <- get_data(all_data, "L164.ag_Cost_75USDkg_C")
    L123.For_Yield_m3m2_R_GLU <- get_data(all_data, "temp-data-inject/L123.For_Yield_m3m2_R_GLU")
    L201.AgYield_bio_grass <- get_data(all_data, "temp-data-inject/L201.AgYield_bio_grass")
    L201.AgYield_bio_tree <- get_data(all_data, "temp-data-inject/L201.AgYield_bio_tree")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

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
      add_legacy_name("L2052.AgCost_ag_irr_mgmt") %>%
      add_precursors("temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU",
                     "L164.ag_Cost_75USDkg_C") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2052.AgCost_ag_irr_mgmt

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgCost_bio_irr_mgmt") %>%
      add_precursors("temp-data-inject/L201.AgYield_bio_grass",
                     "temp-data-inject/L201.AgYield_bio_tree") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2052.AgCost_bio_irr_mgmt

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgCost_For") %>%
      add_precursors("temp-data-inject/L123.For_Yield_m3m2_R_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2052.AgCost_For

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgProdChange_ag_irr_ref") %>%
      add_precursors("L162.ag_YieldRate_R_C_Y_GLU_irr") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2052.AgProdChange_ag_irr_ref

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgProdChange_bio_irr_ref") %>%
      add_precursors("L112.bio_YieldRate_R_Y_GLU",
                     "L162.bio_YieldRate_R_Y_GLU_irr",
                     "temp-data-inject/L201.AgYield_bio_grass",
                     "temp-data-inject/L201.AgYield_bio_tree") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2052.AgProdChange_bio_irr_ref

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgProdChange_irr_high") %>%
      add_precursors("L162.ag_YieldRate_R_C_Y_GLU_irr",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2052.AgProdChange_irr_high

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgProdChange_irr_low") %>%
      add_precursors("L162.ag_YieldRate_R_C_Y_GLU_irr",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2052.AgProdChange_irr_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgProdChange_irr_ssp4") %>%
      add_precursors("L162.ag_YieldRate_R_C_Y_GLU_irr",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2052.AgProdChange_irr_ssp4

    return_data(L2052.AgCost_ag_irr_mgmt, L2052.AgCost_bio_irr_mgmt, L2052.AgCost_For, L2052.AgProdChange_ag_irr_ref, L2052.AgProdChange_bio_irr_ref, L2052.AgProdChange_irr_high, L2052.AgProdChange_irr_low, L2052.AgProdChange_irr_ssp4)
  } else {
    stop("Unknown command")
  }
}
