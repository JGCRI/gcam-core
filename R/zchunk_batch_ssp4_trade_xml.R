#' module_aglu_batch_ssp4_trade_xml
#'
#' Construct XML data structure for \code{ssp4_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ssp4_trade.xml}. The corresponding file in the
#' original data system was \code{batch_ssp4_trade.xml.R} (aglu XML).
module_aglu_batch_ssp4_trade_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L241.StubAgTradeCoeff_food",
             "L241.StubAgTradeCoeff_nonfood",
             "L241.StubAgTradeCoeff_feed",
             "L241.AgProdTech_RES_output",
             "L241.RES_Market"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ssp4_trade.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L241.StubAgTradeCoeff_food <- get_data(all_data, "L241.StubAgTradeCoeff_food")
    L241.StubAgTradeCoeff_nonfood <- get_data(all_data, "L241.StubAgTradeCoeff_nonfood")
    L241.StubAgTradeCoeff_feed <- get_data(all_data, "L241.StubAgTradeCoeff_feed")
    L241.AgProdTech_RES_output <- get_data(all_data, "L241.AgProdTech_RES_output")
    L241.RES_Market <- get_data(all_data, "L241.RES_Market")

    # ===================================================

    # Produce outputs
    create_xml("ssp4_trade.xml") %>%
      add_xml_data(L241.StubAgTradeCoeff_food, "StubTechCoef_NM") %>%
      add_xml_data(L241.StubAgTradeCoeff_nonfood, "StubTechCoef_NM") %>%
      add_xml_data(L241.StubAgTradeCoeff_feed, "StubTechCoef_NM") %>%
      add_xml_data(L241.AgProdTech_RES_output, "AgRES") %>%
      add_xml_data(L241.RES_Market, "PortfolioStdConstraint") %>%
      add_precursors("L241.StubAgTradeCoeff_food", "L241.StubAgTradeCoeff_nonfood", "L241.StubAgTradeCoeff_feed", "L241.AgProdTech_RES_output", "L241.RES_Market") ->
      ssp4_trade.xml

    return_data(ssp4_trade.xml)
  } else {
    stop("Unknown command")
  }
}
