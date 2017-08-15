#' module_socio_batch_bld_agg_xml
#'
#' Construct XML data structure for \code{bld_agg.xml} and all the SSP and gSSP ones as well.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{bld_agg.xml}. The corresponding file in the
#' original data system was \code{batch_bld_agg.xml.R} (socio XML).
module_socio_batch_bld_agg_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L242.IncomeElasticity_bld_gSSP1",
             "L242.IncomeElasticity_bld_gSSP2",
             "L242.IncomeElasticity_bld_gSSP3",
             "L242.IncomeElasticity_bld_gSSP4",
             "L242.IncomeElasticity_bld_gSSP5",
             "L242.IncomeElasticity_bld_SSP1",
             "L242.IncomeElasticity_bld_SSP2",
             "L242.IncomeElasticity_bld_SSP3",
             "L242.IncomeElasticity_bld_SSP4",
             "L242.IncomeElasticity_bld_SSP5"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "bld_agg_gSSP1.xml",
             XML = "bld_agg_gSSP2.xml",
             XML = "bld_agg_gSSP3.xml",
             XML = "bld_agg_gSSP4.xml",
             XML = "bld_agg_gSSP5.xml",
             XML = "bld_agg_SSP1.xml",
             XML = "bld_agg_SSP2.xml",
             XML = "bld_agg_SSP3.xml",
             XML = "bld_agg_SSP4.xml",
             XML = "bld_agg_SSP5.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L242.IncomeElasticity_bld_gSSP1 <- get_data(all_data, "L242.IncomeElasticity_bld_gSSP1")
    L242.IncomeElasticity_bld_gSSP2 <- get_data(all_data, "L242.IncomeElasticity_bld_gSSP2")
    L242.IncomeElasticity_bld_gSSP3 <- get_data(all_data, "L242.IncomeElasticity_bld_gSSP3")
    L242.IncomeElasticity_bld_gSSP4 <- get_data(all_data, "L242.IncomeElasticity_bld_gSSP4")
    L242.IncomeElasticity_bld_gSSP5 <- get_data(all_data, "L242.IncomeElasticity_bld_gSSP5")
    L242.IncomeElasticity_bld_SSP1 <- get_data(all_data, "L242.IncomeElasticity_bld_SSP1")
    L242.IncomeElasticity_bld_SSP2 <- get_data(all_data, "L242.IncomeElasticity_bld_SSP2")
    L242.IncomeElasticity_bld_SSP3 <- get_data(all_data, "L242.IncomeElasticity_bld_SSP3")
    L242.IncomeElasticity_bld_SSP4 <- get_data(all_data, "L242.IncomeElasticity_bld_SSP4")
    L242.IncomeElasticity_bld_SSP5 <- get_data(all_data, "L242.IncomeElasticity_bld_SSP5")

    # ===================================================

    # Produce outputs
    create_xml("bld_agg_gSSP1.xml") %>%
      add_xml_data(L242.IncomeElasticity_bld_gSSP1, "IncomeElasticity") %>%
      add_precursors("L242.IncomeElasticity_bld_gSSP1") ->
      bld_agg_gSSP1.xml
    create_xml("bld_agg_gSSP2.xml") %>%
      add_xml_data(L242.IncomeElasticity_bld_gSSP2, "IncomeElasticity") %>%
      add_precursors("L242.IncomeElasticity_bld_gSSP2") ->
      bld_agg_gSSP2.xml
    create_xml("bld_agg_gSSP3.xml") %>%
      add_xml_data(L242.IncomeElasticity_bld_gSSP3, "IncomeElasticity") %>%
      add_precursors("L242.IncomeElasticity_bld_gSSP3") ->
      bld_agg_gSSP3.xml
    create_xml("bld_agg_gSSP4.xml") %>%
      add_xml_data(L242.IncomeElasticity_bld_gSSP4, "IncomeElasticity") %>%
      add_precursors("L242.IncomeElasticity_bld_gSSP4") ->
      bld_agg_gSSP4.xml
    create_xml("bld_agg_gSSP5.xml") %>%
      add_xml_data(L242.IncomeElasticity_bld_gSSP5, "IncomeElasticity") %>%
      add_precursors("L242.IncomeElasticity_bld_gSSP5") ->
      bld_agg_gSSP5.xml

    create_xml("bld_agg_SSP1.xml") %>%
      add_xml_data(L242.IncomeElasticity_bld_SSP1, "IncomeElasticity") %>%
      add_precursors("L242.IncomeElasticity_bld_SSP1") ->
      bld_agg_SSP1.xml
    create_xml("bld_agg_SSP2.xml") %>%
      add_xml_data(L242.IncomeElasticity_bld_SSP2, "IncomeElasticity") %>%
      add_precursors("L242.IncomeElasticity_bld_SSP2") ->
      bld_agg_SSP2.xml
    create_xml("bld_agg_SSP3.xml") %>%
      add_xml_data(L242.IncomeElasticity_bld_SSP3, "IncomeElasticity") %>%
      add_precursors("L242.IncomeElasticity_bld_SSP3") ->
      bld_agg_SSP3.xml
    create_xml("bld_agg_SSP4.xml") %>%
      add_xml_data(L242.IncomeElasticity_bld_SSP4, "IncomeElasticity") %>%
      add_precursors("L242.IncomeElasticity_bld_SSP4") ->
      bld_agg_SSP4.xml
    create_xml("bld_agg_SSP5.xml") %>%
      add_xml_data(L242.IncomeElasticity_bld_SSP5, "IncomeElasticity") %>%
      add_precursors("L242.IncomeElasticity_bld_SSP5") ->
      bld_agg_SSP5.xml

    return_data(bld_agg_gSSP1.xml, bld_agg_gSSP2.xml, bld_agg_gSSP3.xml, bld_agg_gSSP4.xml, bld_agg_gSSP5.xml,
                bld_agg_SSP1.xml, bld_agg_SSP2.xml, bld_agg_SSP3.xml, bld_agg_SSP4.xml, bld_agg_SSP5.xml)
  } else {
    stop("Unknown command")
  }
}
