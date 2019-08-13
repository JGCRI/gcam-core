#' module_gcamindia_batch_en_prices_xml
#'
#' Construct XML data structure for \code{en_prices_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_prices_india.xml}. The corresponding file in the
#' original data system was \code{batch_en_prices_india_xml.R} (gcamindia XML).
module_gcamindia_batch_en_prices_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L226.india_state_Supplysector_en",
             "L226.india_state_SubsectorShrwtFllt_en",
             "L226.india_state_SubsectorLogit_en",
             "L226.india_state_TechShrwt_en",
             "L226.india_state_TechCoef_en",
             "L226.india_state_TechCost_en",
             "L226.india_state_Ccoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_prices_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    compVal <- passthrough.sector <- share.weight <-
      supplysector <- technology <- NULL # silence package check notes

    # Load required inputs
    L226.india_state_Supplysector_en <- get_data(all_data, "L226.india_state_Supplysector_en")
    L226.india_state_SubsectorShrwtFllt_en <- get_data(all_data, "L226.india_state_SubsectorShrwtFllt_en")
    L226.india_state_SubsectorLogit_en <- get_data(all_data, "L226.india_state_SubsectorLogit_en")
    L226.india_state_TechShrwt_en <- get_data(all_data, "L226.india_state_TechShrwt_en")
    L226.india_state_TechCoef_en <- get_data(all_data, "L226.india_state_TechCoef_en")
    L226.india_state_TechCost_en <- get_data(all_data, "L226.india_state_TechCost_en")
    L226.india_state_Ccoef <- get_data(all_data, "L226.india_state_Ccoef")

    # ===================================================
    # Rename tibble columns to match the L2 data header information.
    L226.india_state_Ccoef <- rename(L226.india_state_Ccoef, PrimaryFuelCO2Coef.name = supplysector)

    # Produce outputs
    create_xml("en_prices_india.xml") %>%
      add_logit_tables_xml(L226.india_state_Supplysector_en, "Supplysector") %>%
      add_xml_data(L226.india_state_SubsectorShrwtFllt_en, "SubsectorShrwtFllt") %>%
      add_logit_tables_xml(L226.india_state_SubsectorLogit_en, "SubsectorLogit") %>%
      add_xml_data(L226.india_state_TechShrwt_en, "TechShrwt") %>%
      add_xml_data(L226.india_state_TechCoef_en, "TechCoef") %>%
      add_xml_data(L226.india_state_TechCost_en, "TechCost") %>%
      add_xml_data(L226.india_state_Ccoef, "CarbonCoef") %>%
      add_precursors("L226.india_state_Supplysector_en",
                     "L226.india_state_SubsectorShrwtFllt_en",
                     "L226.india_state_SubsectorLogit_en",
                     "L226.india_state_TechShrwt_en",
                     "L226.india_state_TechCoef_en",
                     "L226.india_state_TechCost_en",
                     "L226.india_state_Ccoef") ->
      en_prices_india.xml

    return_data(en_prices_india.xml)
  } else {
    stop("Unknown command")
  }
}
