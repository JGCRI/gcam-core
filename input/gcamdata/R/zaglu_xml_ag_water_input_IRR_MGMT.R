# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_ag_water_input_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{ag_water_input_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_water_input_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_ag_water_input_IRR_MGMT_xml.R} (aglu XML).
module_aglu_ag_water_input_IRR_MGMT_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2072.AgCoef_IrrBphysWater_ag_mgmt",
      "L2072.AgCoef_IrrWaterWdraw_ag_mgmt",
      "L2072.AgCoef_IrrWaterCons_ag_mgmt",
      "L2072.AgCoef_RfdBphysWater_ag_mgmt",
      "L2072.AgCoef_BphysWater_bio_mgmt",
      "L2072.AgCoef_IrrWaterWdraw_bio_mgmt",
      "L2072.AgCoef_IrrWaterCons_bio_mgmt")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ag_water_input_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    # Produce outputs ----
    create_xml("ag_water_input_IRR_MGMT.xml") %>%
      add_xml_data(L2072.AgCoef_IrrBphysWater_ag_mgmt, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_IrrWaterWdraw_ag_mgmt, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_IrrWaterCons_ag_mgmt, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_RfdBphysWater_ag_mgmt, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_BphysWater_bio_mgmt, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_IrrWaterWdraw_bio_mgmt, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_IrrWaterCons_bio_mgmt, "AgCoef") %>%
      add_precursors("L2072.AgCoef_IrrBphysWater_ag_mgmt",
                     "L2072.AgCoef_IrrWaterWdraw_ag_mgmt",
                     "L2072.AgCoef_IrrWaterCons_ag_mgmt",
                     "L2072.AgCoef_RfdBphysWater_ag_mgmt",
                     "L2072.AgCoef_BphysWater_bio_mgmt",
                     "L2072.AgCoef_IrrWaterWdraw_bio_mgmt",
                     "L2072.AgCoef_IrrWaterCons_bio_mgmt") ->
      ag_water_input_IRR_MGMT.xml

    return_data(ag_water_input_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}
