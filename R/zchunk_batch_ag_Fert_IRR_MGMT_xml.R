#' module_aglu_batch_ag_Fert_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{ag_Fert_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_Fert_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_ag_Fert_IRR_MGMT.xml} (aglu XML).
module_socio_batch_bld_agg_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2062.AgCoef_Fert_ag_irr_mgmt",
             "L2062.AgCoef_Fert_bio_irr_mgmt",
             "L2062.AgCost_ag_irr_mgmt_adj",
             "L2062.AgCost_bio_irr_mgmt_adj"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ag_Fert_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2062.AgCoef_Fert_ag_irr_mgmt <- get_data(all_data, "L2062.AgCoef_Fert_ag_irr_mgmt")
    L2062.AgCoef_Fert_bio_irr_mgmt <- get_data(all_data, "L2062.AgCoef_Fert_bio_irr_mgmt")
    L2062.AgCost_ag_irr_mgmt_adj <- get_data(all_data, "L2062.AgCost_ag_irr_mgmt_adj")
    L2062.AgCost_bio_irr_mgmt_adj <- get_data(all_data, "L2062.AgCost_bio_irr_mgmt_adj")
    # ===================================================

    # Produce outputs
    create_xml("ag_Fert_IRR_MGMT.xml") %>%
      add_xml_data(L2062.AgCoef_Fert_ag_irr_mgmt, "AgCoef") %>%
      add_xml_data(L2062.AgCoef_Fert_bio_irr_mgmt, "AgCoef") %>%
      add_xml_data(L2062.AgCost_ag_irr_mgmt_adj, "AgCost") %>%
      add_xml_data(L2062.AgCost_bio_irr_mgmt_adj, "AgCost") %>%
      add_precursors("L2062.AgCoef_Fert_ag_irr_mgmt", "L2062.AgCoef_Fert_bio_irr_mgmt",
                     "L2062.AgCost_ag_irr_mgmt_adj", "L2062.AgCost_bio_irr_mgmt_adj") ->
      ag_Fert_IRR_MGMT.xml

    return_data(ag_Fert_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}
