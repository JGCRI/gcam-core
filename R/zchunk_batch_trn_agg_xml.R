#' module_socio_batch_trn_agg_xml
#'
#' Construct XML data structure for \code{trn_agg.xml} and all the SSP and gSSP ones as well.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{trn_agg.xml}. The corresponding file in the
#' original data system was \code{batch_trn_agg.xml.R} (socio XML).
module_socio_batch_trn_agg_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L252.IncomeElasticity_trn_gSSP1",
             "L252.IncomeElasticity_trn_gSSP2",
             "L252.IncomeElasticity_trn_gSSP3",
             "L252.IncomeElasticity_trn_gSSP4",
             "L252.IncomeElasticity_trn_gSSP5",
             "L252.IncomeElasticity_trn_SSP1",
             "L252.IncomeElasticity_trn_SSP2",
             "L252.IncomeElasticity_trn_SSP3",
             "L252.IncomeElasticity_trn_SSP4",
             "L252.IncomeElasticity_trn_SSP5"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "trn_agg_gSSP1.xml",
             XML = "trn_agg_gSSP2.xml",
             XML = "trn_agg_gSSP3.xml",
             XML = "trn_agg_gSSP4.xml",
             XML = "trn_agg_gSSP5.xml",
             XML = "trn_agg_SSP1.xml",
             XML = "trn_agg_SSP2.xml",
             XML = "trn_agg_SSP3.xml",
             XML = "trn_agg_SSP4.xml",
             XML = "trn_agg_SSP5.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L252.IncomeElasticity_trn_gSSP1 <- get_data(all_data, "L252.IncomeElasticity_trn_gSSP1")
    L252.IncomeElasticity_trn_gSSP2 <- get_data(all_data, "L252.IncomeElasticity_trn_gSSP2")
    L252.IncomeElasticity_trn_gSSP3 <- get_data(all_data, "L252.IncomeElasticity_trn_gSSP3")
    L252.IncomeElasticity_trn_gSSP4 <- get_data(all_data, "L252.IncomeElasticity_trn_gSSP4")
    L252.IncomeElasticity_trn_gSSP5 <- get_data(all_data, "L252.IncomeElasticity_trn_gSSP5")
    L252.IncomeElasticity_trn_SSP1 <- get_data(all_data, "L252.IncomeElasticity_trn_SSP1")
    L252.IncomeElasticity_trn_SSP2 <- get_data(all_data, "L252.IncomeElasticity_trn_SSP2")
    L252.IncomeElasticity_trn_SSP3 <- get_data(all_data, "L252.IncomeElasticity_trn_SSP3")
    L252.IncomeElasticity_trn_SSP4 <- get_data(all_data, "L252.IncomeElasticity_trn_SSP4")
    L252.IncomeElasticity_trn_SSP5 <- get_data(all_data, "L252.IncomeElasticity_trn_SSP5")

    # ===================================================

    # Produce outputs
    create_xml("trn_agg_gSSP1.xml") %>%
      add_xml_data(L252.IncomeElasticity_trn_gSSP1, "IncomeElasticity") %>%
      add_precursors("L252.IncomeElasticity_trn_gSSP1") ->
      trn_agg_gSSP1.xml
    create_xml("trn_agg_gSSP2.xml") %>%
      add_xml_data(L252.IncomeElasticity_trn_gSSP2, "IncomeElasticity") %>%
      add_precursors("L252.IncomeElasticity_trn_gSSP2") ->
      trn_agg_gSSP2.xml
    create_xml("trn_agg_gSSP3.xml") %>%
      add_xml_data(L252.IncomeElasticity_trn_gSSP3, "IncomeElasticity") %>%
      add_precursors("L252.IncomeElasticity_trn_gSSP3") ->
      trn_agg_gSSP3.xml
    create_xml("trn_agg_gSSP4.xml") %>%
      add_xml_data(L252.IncomeElasticity_trn_gSSP4, "IncomeElasticity") %>%
      add_precursors("L252.IncomeElasticity_trn_gSSP4") ->
      trn_agg_gSSP4.xml
    create_xml("trn_agg_gSSP5.xml") %>%
      add_xml_data(L252.IncomeElasticity_trn_gSSP5, "IncomeElasticity") %>%
      add_precursors("L252.IncomeElasticity_trn_gSSP5") ->
      trn_agg_gSSP5.xml

    create_xml("trn_agg_SSP1.xml") %>%
      add_xml_data(L252.IncomeElasticity_trn_SSP1, "IncomeElasticity") %>%
      add_precursors("L252.IncomeElasticity_trn_SSP1") ->
      trn_agg_SSP1.xml
    create_xml("trn_agg_SSP2.xml") %>%
      add_xml_data(L252.IncomeElasticity_trn_SSP2, "IncomeElasticity") %>%
      add_precursors("L252.IncomeElasticity_trn_SSP2") ->
      trn_agg_SSP2.xml
    create_xml("trn_agg_SSP3.xml") %>%
      add_xml_data(L252.IncomeElasticity_trn_SSP3, "IncomeElasticity") %>%
      add_precursors("L252.IncomeElasticity_trn_SSP3") ->
      trn_agg_SSP3.xml
    create_xml("trn_agg_SSP4.xml") %>%
      add_xml_data(L252.IncomeElasticity_trn_SSP4, "IncomeElasticity") %>%
      add_precursors("L252.IncomeElasticity_trn_SSP4") ->
      trn_agg_SSP4.xml
    create_xml("trn_agg_SSP5.xml") %>%
      add_xml_data(L252.IncomeElasticity_trn_SSP5, "IncomeElasticity") %>%
      add_precursors("L252.IncomeElasticity_trn_SSP5") ->
      trn_agg_SSP5.xml

    return_data(trn_agg_gSSP1.xml, trn_agg_gSSP2.xml, trn_agg_gSSP3.xml, trn_agg_gSSP4.xml, trn_agg_gSSP5.xml,
                trn_agg_SSP1.xml, trn_agg_SSP2.xml, trn_agg_SSP3.xml, trn_agg_SSP4.xml, trn_agg_SSP5.xml)
  } else {
    stop("Unknown command")
  }
}
