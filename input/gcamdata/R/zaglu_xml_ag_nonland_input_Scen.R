# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_ag_nonland_input_Scen_xml
#'
#' Construct XML data structure for \code{ag_input_fert_MGMT.xml},
#' \code{ag_input_water_IRR_MGMT.xml}, \code{ag_input_laborcapital_IRR_MGMT.xml}
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_input_fert_MGMT.xml},
#' \code{ag_input_water_IRR_MGMT.xml}, \code{ag_input_laborcapital_IRR_MGMT.xml}.
module_aglu_ag_nonland_input_Scen_xml <- function(command, ...) {

  MODULE_INPUTS_Fert <-
    c("L2062.AgCoef_Fert_ag_irr_mgmt",
      "L2062.AgCoef_Fert_bio_irr_mgmt")

  MODULE_INPUTS_Water <-
    c("L2072.AgCoef_IrrBphysWater_ag_mgmt",
      "L2072.AgCoef_IrrWaterWdraw_ag_mgmt",
      "L2072.AgCoef_IrrWaterCons_ag_mgmt",
      "L2072.AgCoef_RfdBphysWater_ag_mgmt",
      "L2072.AgCoef_BphysWater_bio_mgmt",
      "L2072.AgCoef_IrrWaterWdraw_bio_mgmt",
      "L2072.AgCoef_IrrWaterCons_bio_mgmt")

  MODULE_INPUTS_ValueAdded <-
    c("L2083.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA",
      "L2083.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA",
      "L2083.AgCoef_laborcapital_for_tfp_MA")

  MODULE_INPUTS <-
    c(MODULE_INPUTS_Fert,
      MODULE_INPUTS_Water,
      MODULE_INPUTS_ValueAdded)

  MODULE_OUTPUTS <-
    c(XML = "ag_input_fert_MGMT.xml",
      XML = "ag_input_water_IRR_MGMT.xml",
      XML = "ag_input_laborcapital_IRR_MGMT.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    # Produce outputs

    create_xml("ag_input_fert_MGMT.xml") %>%
      add_xml_data(L2062.AgCoef_Fert_ag_irr_mgmt, "AgCoef") %>%
      add_xml_data(L2062.AgCoef_Fert_bio_irr_mgmt, "AgCoef") %>%
      add_precursors(MODULE_INPUTS_Fert) ->
      ag_input_fert_MGMT.xml

    create_xml("ag_input_water_IRR_MGMT.xml") %>%
      add_xml_data(L2072.AgCoef_IrrBphysWater_ag_mgmt, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_IrrWaterWdraw_ag_mgmt, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_IrrWaterCons_ag_mgmt, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_RfdBphysWater_ag_mgmt, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_BphysWater_bio_mgmt, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_IrrWaterWdraw_bio_mgmt, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_IrrWaterCons_bio_mgmt, "AgCoef") %>%
      add_precursors(MODULE_INPUTS_Water) ->
      ag_input_water_IRR_MGMT.xml

    create_xml("ag_input_laborcapital_IRR_MGMT.xml") %>%
      add_xml_data(L2083.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA, "AgCoef") %>%
      add_xml_data(L2083.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA, "AgPriceConversion") %>%
      add_xml_data(L2083.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA, "AgCoef") %>%
      add_xml_data(L2083.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA, "AgPriceConversion") %>%
      add_xml_data(L2083.AgCoef_laborcapital_for_tfp_MA, "AgCoef") %>%
      add_xml_data(L2083.AgCoef_laborcapital_for_tfp_MA, "AgPriceConversion") %>%
      add_precursors(MODULE_INPUTS_ValueAdded) ->
      ag_input_laborcapital_IRR_MGMT.xml

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
