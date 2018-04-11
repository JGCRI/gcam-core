#' module_aglu_batch_ag_For_Past_bio_base_xml
#'
#' Construct XML data structure for \code{ag_For_Past_bio_base.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_For_Past_bio_base.xml}. The corresponding file in the
#' original data system was \code{batch_ag_For_Past_bio_base_xml.R} (aglu XML).
module_aglu_batch_ag_For_Past_bio_base_xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L201.AgSupplySubsector",
              "L201.AgProduction_ag",
              "L201.AgProduction_For",
              "L201.AgProduction_Past",
              "L201.AgHAtoCL",
              "L201.AgYield_bio_grass",
              "L201.AgYield_bio_tree"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ag_For_Past_bio_base.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.AgSupplySubsector <- get_data(all_data, "L201.AgSupplySubsector")
    L201.AgProduction_ag <- get_data(all_data, "L201.AgProduction_ag")
    L201.AgProduction_For <- get_data(all_data, "L201.AgProduction_For")
    L201.AgProduction_Past <- get_data(all_data, "L201.AgProduction_Past")
    L201.AgHAtoCL <- get_data(all_data, "L201.AgHAtoCL")
    L201.AgYield_bio_grass <- get_data(all_data, "L201.AgYield_bio_grass")
    L201.AgYield_bio_tree <- get_data(all_data, "L201.AgYield_bio_tree")

    # ===================================================

    # Produce outputs
    create_xml("ag_For_Past_bio_base.xml") %>%
      add_xml_data(L201.AgSupplySubsector,"AgSupplySubsector") %>%
      add_xml_data(L201.AgProduction_ag,"AgProduction") %>%
      add_xml_data(L201.AgProduction_For,"AgProduction") %>%
      add_xml_data(L201.AgProduction_Past,"AgProduction") %>%
      add_xml_data(L201.AgHAtoCL,"AgHAtoCL") %>%
      add_xml_data(L201.AgYield_bio_grass,"AgYield") %>%
      add_xml_data(L201.AgYield_bio_tree,"AgYield") %>%
      add_precursors("L201.AgSupplySubsector", "L201.AgProduction_ag", "L201.AgProduction_For", "L201.AgProduction_Past", "L201.AgHAtoCL", "L201.AgYield_bio_grass", "L201.AgYield_bio_tree") ->
      ag_For_Past_bio_base.xml

    return_data(ag_For_Past_bio_base.xml)
  } else {
    stop("Unknown command")
  }
}
