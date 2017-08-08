#' module_aglu_batch_ag_For_Past_bio_base_IRR_MGMT.xml
#'
#' Construct XML data structure for \code{ag_For_Past_bio_base_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_For_Past_bio_base_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_ag_For_Past_bio_base_IRR_MGMT.xml.R} (aglu XML).
module_aglu_batch_ag_For_Past_bio_base_IRR_MGMT.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2012.AgSupplySubsector",
             "L2012.AgProduction_ag_irr_mgmt",
             "L2012.AgProduction_For",
             "L2012.AgProduction_Past",
             "L2012.AgHAtoCL_irr_mgmt",
             "L2012.AgYield_bio_ref"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ag_For_Past_bio_base_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2012.AgSupplySubsector <- get_data(all_data, "L2012.AgSupplySubsector")
    L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2012.AgProduction_ag_irr_mgmt")
    L2012.AgProduction_For <- get_data(all_data, "L2012.AgProduction_For")
    L2012.AgProduction_Past <- get_data(all_data, "L2012.AgProduction_Past")
    L2012.AgHAtoCL_irr_mgmt <- get_data(all_data, "L2012.AgHAtoCL_irr_mgmt")
    L2012.AgYield_bio_ref <- get_data(all_data, "L2012.AgYield_bio_ref")

    # ===================================================

    # Produce outputs
    create_xml("ag_For_Past_bio_base_IRR_MGMT.xml") %>%
      add_xml_data(L2012.AgSupplySubsector,"AgSupplySubsector") %>%
      add_xml_data(L2012.AgProduction_ag_irr_mgmt,"AgProduction") %>%
      add_xml_data(L2012.AgProduction_For,"AgProduction") %>%
      add_xml_data(L2012.AgProduction_Past,"AgProduction") %>%
      add_xml_data(L2012.AgHAtoCL_irr_mgmt,"AgHAtoCL") %>%
      add_xml_data(L2012.AgYield_bio_ref,"AgYield") %>%
      add_precursors("L2012.AgSupplySubsector", "L2012.AgProduction_ag_irr_mgmt", "L2012.AgProduction_For", "L2012.AgProduction_Past", "L2012.AgHAtoCL_irr_mgmt", "L2012.AgYield_bio_ref") ->
      ag_For_Past_bio_base_IRR_MGMT.xml

    return_data(ag_For_Past_bio_base_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}
