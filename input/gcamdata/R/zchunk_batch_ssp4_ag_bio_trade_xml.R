# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ssp4_bio_trade_xml
#'
#' Construct XML data structure for \code{ssp4_ag_bio_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ssp4_ag_bio_trade.xml}. The corresponding file in the
#' original data system was \code{batch_ssp4_ag_bio_trade.xml.R} (aglu XML).
module_aglu_batch_ssp4_ag_bio_trade_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L243.SubsectorShrwt_TotBio_SSP4",
             "L243.SubsectorShrwtFllt_TradedBio_SSP4",
             "L243.Supplysector_reg_SSP4",
             "L243.SubsectorInterpTo_tra_SSP4"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ssp4_ag_bio_trade.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L243.SubsectorShrwt_TotBio_SSP4 <- get_data(all_data, "L243.SubsectorShrwt_TotBio_SSP4")
    L243.SubsectorShrwtFllt_TradedBio_SSP4 <- get_data(all_data, "L243.SubsectorShrwtFllt_TradedBio_SSP4")
    L243.Supplysector_reg_SSP4 <- get_data(all_data, "L243.Supplysector_reg_SSP4")
    L243.SubsectorInterpTo_tra_SSP4 <- get_data(all_data, "L243.SubsectorInterpTo_tra_SSP4")

    # ===================================================

    # Produce outputs
    create_xml("ssp4_ag_bio_trade.xml") %>%
      add_xml_data(L243.SubsectorShrwt_TotBio_SSP4, "SubsectorShrwt") %>%
      add_xml_data(L243.SubsectorShrwtFllt_TradedBio_SSP4, "SubsectorShrwtFllt") %>%
      add_logit_tables_xml(L243.Supplysector_reg_SSP4, "Supplysector") %>%
      add_xml_data(L243.SubsectorInterpTo_tra_SSP4, "SubsectorInterpTo") %>%
      add_precursors("L243.SubsectorShrwt_TotBio_SSP4",
                     "L243.SubsectorShrwtFllt_TradedBio_SSP4",
                     "L243.Supplysector_reg_SSP4",
                     "L243.SubsectorInterpTo_tra_SSP4") ->
      ssp4_ag_bio_trade.xml

    return_data(ssp4_ag_bio_trade.xml)
  } else {
    stop("Unknown command")
  }
}
