#' module_aglu_L241.trade_input
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L241.StubAgTradeCoeff_food}, \code{L241.StubAgTradeCoeff_nonfood}, \code{L241.StubAgTradeCoeff_feed}, \code{L241.AgProdTech_RES_output}, \code{L241.RES_Market}. The corresponding file in the
#' original data system was \code{L241.trade_input.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL June 2017
#' @export
module_aglu_L241.trade_input <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "aglu/A_demand_technology",
             FILE = "aglu/A_an_input_technology",
             "L101.ag_kcalg_R_C_Y",
             FILE = "temp-data-inject/L2012.AgProduction_ag_irr_mgmt",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L241.StubAgTradeCoeff_food",
             "L241.StubAgTradeCoeff_nonfood",
             "L241.StubAgTradeCoeff_feed",
             "L241.AgProdTech_RES_output",
             "L241.RES_Market"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_demand_technology <- get_data(all_data, "aglu/A_demand_technology")
    A_an_input_technology <- get_data(all_data, "aglu/A_an_input_technology")
    L101.ag_kcalg_R_C_Y <- get_data(all_data, "L101.ag_kcalg_R_C_Y")
    L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "temp-data-inject/L2012.AgProduction_ag_irr_mgmt")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")


    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.StubAgTradeCoeff_food") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "aglu/A_an_input_technology",
                     "L101.ag_kcalg_R_C_Y",
                     "temp-data-inject/L2012.AgProduction_ag_irr_mgmt",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L241.StubAgTradeCoeff_food

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.StubAgTradeCoeff_nonfood") %>%
      same_precursors_as(L241.StubAgTradeCoeff_food) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L241.StubAgTradeCoeff_nonfood

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.StubAgTradeCoeff_feed") %>%
      same_precursors_as(L241.StubAgTradeCoeff_food) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L241.StubAgTradeCoeff_feed

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.AgProdTech_RES_output") %>%
      same_precursors_as(L241.StubAgTradeCoeff_food) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L241.AgProdTech_RES_output

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L241.RES_Market") %>%
      same_precursors_as(L241.StubAgTradeCoeff_food) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L241.RES_Market

    return_data(L241.StubAgTradeCoeff_food, L241.StubAgTradeCoeff_nonfood,
                L241.StubAgTradeCoeff_feed, L241.AgProdTech_RES_output, L241.RES_Market)
  } else {
    stop("Unknown command")
  }
}
