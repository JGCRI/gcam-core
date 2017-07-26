#' module_aglu_batch_ag_prodchange_ssp1_IRR_MGMT.xml
#'
#' Construct XML data structure for \code{ag_prodchange_ssp1_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_prodchange_ssp1_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_ag_prodchange_ssp1_IRR_MGMT.xml.R} (aglu XML).
module_aglu_batch_ag_prodchange_ssp1_IRR_MGMT.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L2052.AgProdChange_irr_high"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ag_prodchange_ssp1_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2052.AgProdChange_irr_high <- get_data(all_data, "L2052.AgProdChange_irr_high")

    # ===================================================

    # Produce outputs
    create_xml("ag_prodchange_ssp1_IRR_MGMT.xml") %>%
      add_xml_data(L2052.AgProdChange_irr_high, "AgProdChange") %>%
      add_precursors("L2052.AgProdChange_irr_high") ->
      ag_prodchange_ssp1_IRR_MGMT.xml

    return_data(ag_prodchange_ssp1_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}
