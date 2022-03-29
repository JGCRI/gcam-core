# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ssp3_bio_trade_xml
#'
#' Construct XML data structure for \code{ssp3_bio_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ssp3_bio_trade.xml}. The corresponding file in the
#' original data system was \code{batch_ssp3_bio_trade.xml.R} (aglu XML).
module_aglu_batch_ssp3_bio_trade_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L243.SubsectorShrwt_TotBio_SSP3",
             "L243.StubTechShrwt_TotBio_SSP3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ssp3_bio_trade.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L243.SubsectorShrwt_TotBio_SSP3 <- get_data(all_data, "L243.SubsectorShrwt_TotBio_SSP3")
    L243.StubTechShrwt_TotBio_SSP3 <- get_data(all_data, "L243.StubTechShrwt_TotBio_SSP3")

    # ===================================================

    # Produce outputs
    create_xml("ssp3_bio_trade.xml") %>%
      add_xml_data(L243.SubsectorShrwt_TotBio_SSP3, "SubsectorShrwt") %>%
      add_xml_data(L243.StubTechShrwt_TotBio_SSP3, "StubTechShrwt") %>%
      add_precursors("L243.SubsectorShrwt_TotBio_SSP3",
                     "L243.StubTechShrwt_TotBio_SSP3") ->
      ssp3_bio_trade.xml

    return_data(ssp3_bio_trade.xml)
  } else {
    stop("Unknown command")
  }
}
