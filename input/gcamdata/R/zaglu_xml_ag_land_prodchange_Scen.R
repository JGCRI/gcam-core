# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_ag_land_prodchange_Scen_xml
#'
#' Construct XML data structure for \code{ag_land_prodchange_ref_IRR_MGMT.xml} and other SSP scenarios.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs.
module_aglu_ag_land_prodchange_Scen_xml <- function(command, ...) {

  SSP_NUMS <- 1:5

  MODULE_INPUTS <-
    c("L2053.AgProdChange_ag_irr_ref",
      "L2053.AgProdChange_bio_irr_ref",
      "L2053.AgProdChange_irr_high",
      "L2053.AgProdChange_irr_low",
      "L2053.AgProdChange_irr_ssp4")

  MODULE_OUTPUTS <-
    setNames(
      c("ag_land_prodchange_ref_IRR_MGMT.xml",
        paste0("ag_land_prodchange_ssp", SSP_NUMS, "_IRR_MGMT.xml") ),
      rep("XML", 6))

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    # Produce outputs ----
    create_xml("ag_land_prodchange_ref_IRR_MGMT.xml") %>%
      add_xml_data(L2053.AgProdChange_ag_irr_ref, "AgProdChange") %>%
      add_xml_data(L2053.AgProdChange_bio_irr_ref, "AgProdChange") %>%
      add_precursors("L2053.AgProdChange_ag_irr_ref",
                     "L2053.AgProdChange_bio_irr_ref") ->
      ag_land_prodchange_ref_IRR_MGMT.xml

    create_xml("ag_land_prodchange_ssp1_IRR_MGMT.xml") %>%
      add_xml_data(L2053.AgProdChange_irr_high, "AgProdChange") %>%
      add_precursors("L2053.AgProdChange_irr_high") ->
      ag_land_prodchange_ssp1_IRR_MGMT.xml

    create_xml("ag_land_prodchange_ssp2_IRR_MGMT.xml") %>%
      add_xml_data(L2053.AgProdChange_ag_irr_ref, "AgProdChange") %>%
      add_precursors("L2053.AgProdChange_ag_irr_ref") ->
      ag_land_prodchange_ssp2_IRR_MGMT.xml

    create_xml("ag_land_prodchange_ssp3_IRR_MGMT.xml") %>%
      add_xml_data(L2053.AgProdChange_irr_low, "AgProdChange") %>%
      add_precursors("L2053.AgProdChange_irr_low") ->
      ag_land_prodchange_ssp3_IRR_MGMT.xml

    create_xml("ag_land_prodchange_ssp4_IRR_MGMT.xml") %>%
      add_xml_data(L2053.AgProdChange_irr_ssp4, "AgProdChange") %>%
      add_precursors("L2053.AgProdChange_irr_ssp4") ->
      ag_land_prodchange_ssp4_IRR_MGMT.xml

    create_xml("ag_land_prodchange_ssp5_IRR_MGMT.xml") %>%
      add_xml_data(L2053.AgProdChange_irr_high, "AgProdChange") %>%
      add_precursors("L2053.AgProdChange_irr_high") ->
      ag_land_prodchange_ssp5_IRR_MGMT.xml


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
