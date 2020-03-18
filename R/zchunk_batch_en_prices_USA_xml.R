# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_en_prices_USA_xml
#'
#' Construct XML data structure for \code{en_prices_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_prices_USA.xml}. The corresponding file in the
#' original data system was \code{batch_en_prices_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_en_prices_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L226.Supplysector_en_USA",
             "L226.SubsectorShrwtFllt_en_USA",
             "L226.SubsectorLogit_en_USA",
             "L226.TechShrwt_en_USA",
             "L226.TechCoef_en_USA",
             "L226.TechCost_en_USA",
             "L226.Ccoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_prices_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    compVal <- passthrough.sector <- share.weight <-
      supplysector <- technology <- NULL # silence package check notes

    # Load required inputs
    L226.Supplysector_en_USA <- get_data(all_data, "L226.Supplysector_en_USA")
    L226.SubsectorShrwtFllt_en_USA <- get_data(all_data, "L226.SubsectorShrwtFllt_en_USA")
    L226.SubsectorLogit_en_USA <- get_data(all_data, "L226.SubsectorLogit_en_USA")
    L226.TechShrwt_en_USA <- get_data(all_data, "L226.TechShrwt_en_USA")
    L226.TechCoef_en_USA <- get_data(all_data, "L226.TechCoef_en_USA")
    L226.TechCost_en_USA <- get_data(all_data, "L226.TechCost_en_USA")
    L226.Ccoef <- get_data(all_data, "L226.Ccoef")

    # ===================================================
    # Rename tibble columns to match the L2 data header information.
    L226.Ccoef <- rename(L226.Ccoef, PrimaryFuelCO2Coef.name = supplysector)

    # Produce outputs
    create_xml("en_prices_USA.xml") %>%
      add_logit_tables_xml(L226.Supplysector_en_USA, "Supplysector") %>%
      add_xml_data(L226.SubsectorShrwtFllt_en_USA, "SubsectorShrwtFllt") %>%
      add_logit_tables_xml(L226.SubsectorLogit_en_USA, "SubsectorLogit") %>%
      add_xml_data(L226.TechShrwt_en_USA, "TechShrwt") %>%
      add_xml_data(L226.TechCoef_en_USA, "TechCoef") %>%
      add_xml_data(L226.TechCost_en_USA, "TechCost") %>%
      add_xml_data(L226.Ccoef, "CarbonCoef") %>%
      add_precursors("L226.Supplysector_en_USA",
                     "L226.SubsectorShrwtFllt_en_USA",
                     "L226.SubsectorLogit_en_USA",
                     "L226.TechShrwt_en_USA",
                     "L226.TechCoef_en_USA",
                     "L226.TechCost_en_USA",
                     "L226.Ccoef") ->
      en_prices_USA.xml

    return_data(en_prices_USA.xml)
  } else {
    stop("Unknown command")
  }
}
