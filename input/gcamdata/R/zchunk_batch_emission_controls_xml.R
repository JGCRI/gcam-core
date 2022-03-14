# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_batch_emission_controls_xml
#'
#' Construct XML data structure for \code{emission_factor_controls.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{EF_controls.xml}, \code{EF_controls_user.xml}
#' \code{EF_controls_USA.xml}, \code{EF_controls_user_USA.xml}

module_emissions_batch_emission_controls_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L253.EF_retrofit",
             "L253.EF_NSPS_new_vintage",
             "L253.Retrofit_off",
             "L253.delete_gdp_control",
             "L253.user_EF_retrofit",
             "L253.user_EF_NSPS_new_vintage",
             "L253.user_Retrofit_off",
             "L253.user_delete_gdp_control",
             "L253.EF_retrofit_USA",
             "L253.EF_NSPS_new_vintage_USA",
             "L253.Retrofit_off_USA",
             "L253.delete_gdp_control_USA",
             "L253.user_EF_retrofit_USA",
             "L253.user_EF_NSPS_new_vintage_USA",
             "L253.user_Retrofit_off_USA",
             "L253.user_delete_gdp_control_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "emission_factor_controls.xml",
             XML = "emission_factor_controls_user.xml",
             XML = "emission_factor_controls_USA.xml",
             XML = "emission_factor_controls_user_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L253.EF_retrofit <- get_data(all_data, "L253.EF_retrofit", strip_attributes = TRUE)
    L253.Retrofit_off <- get_data(all_data, "L253.Retrofit_off", strip_attributes = TRUE)
    L253.EF_new <- get_data(all_data, "L253.EF_NSPS_new_vintage", strip_attributes = TRUE)
    L253.delete_gdp_control <- get_data(all_data, "L253.delete_gdp_control", strip_attributes = TRUE)
    L253.user_EF_retrofit <- get_data(all_data, "L253.user_EF_retrofit", strip_attributes = TRUE)
    L253.user_Retrofit_off<- get_data(all_data, "L253.user_Retrofit_off", strip_attributes = TRUE)
    L253.user_EF_new <- get_data(all_data, "L253.user_EF_NSPS_new_vintage", strip_attributes = TRUE)
    L253.user_delete_gdp_control <- get_data(all_data, "L253.user_delete_gdp_control", strip_attributes = TRUE)
    L253.EF_retrofit_USA <- get_data(all_data, "L253.EF_retrofit_USA", strip_attributes = TRUE)
    L253.Retrofit_off_USA <- get_data(all_data, "L253.Retrofit_off_USA", strip_attributes = TRUE)
    L253.EF_new_USA <- get_data(all_data, "L253.EF_NSPS_new_vintage_USA", strip_attributes = TRUE)
    L253.delete_gdp_control_USA <- get_data(all_data, "L253.delete_gdp_control_USA", strip_attributes = TRUE)
    L253.user_EF_retrofit_USA <- get_data(all_data, "L253.user_EF_retrofit_USA", strip_attributes = TRUE)
    L253.user_Retrofit_off_USA<- get_data(all_data, "L253.user_Retrofit_off_USA", strip_attributes = TRUE)
    L253.user_EF_new_USA <- get_data(all_data, "L253.user_EF_NSPS_new_vintage_USA", strip_attributes = TRUE)
    L253.user_delete_gdp_control_USA <- get_data(all_data, "L253.user_delete_gdp_control_USA", strip_attributes = TRUE)
    # ===================================================

    # Produce outputs
    create_xml("emission_factor_controls.xml") %>%
      add_xml_data(L253.EF_retrofit, "EF_Retrofit") %>%
      add_xml_data(L253.EF_new, "EF_NSPS") %>%
      add_xml_data(L253.delete_gdp_control, "DeleteGDPControl") %>%
      add_xml_data(L253.Retrofit_off, "RetrofitOff") %>%
      add_precursors("L253.EF_retrofit",
                     "L253.EF_NSPS_new_vintage",
                     "L253.Retrofit_off",
                     "L253.delete_gdp_control") -> emission_factor_controls.xml

    create_xml("emission_factor_controls_user.xml") %>%
      add_xml_data(L253.user_EF_retrofit, "EF_Retrofit") %>%
      add_xml_data(L253.user_EF_new, "EF_NSPS") %>%
      add_xml_data(L253.user_delete_gdp_control, "DeleteGDPControl") %>%
      add_xml_data(L253.user_Retrofit_off, "RetrofitOff") %>%
      add_precursors("L253.user_EF_retrofit",
                     "L253.user_EF_NSPS_new_vintage",
                     "L253.user_Retrofit_off",
                     "L253.user_delete_gdp_control") -> emission_factor_controls_user.xml

    create_xml("emission_factor_controls_USA.xml") %>%
      add_xml_data(L253.EF_retrofit_USA, "EF_Retrofit") %>%
      add_xml_data(L253.EF_new_USA, "EF_NSPS") %>%
      add_xml_data(L253.delete_gdp_control_USA, "DeleteGDPControl") %>%
      add_xml_data(L253.Retrofit_off_USA, "RetrofitOff") %>%
      add_precursors("L253.EF_retrofit_USA",
                     "L253.EF_NSPS_new_vintage_USA",
                     "L253.Retrofit_off_USA",
                     "L253.delete_gdp_control_USA") -> emission_factor_controls_USA.xml

    create_xml("emission_factor_controls_user_USA.xml") %>%
      add_xml_data(L253.user_EF_retrofit_USA, "EF_Retrofit") %>%
      add_xml_data(L253.user_EF_new_USA, "EF_NSPS") %>%
      add_xml_data(L253.user_delete_gdp_control_USA, "DeleteGDPControl") %>%
      add_xml_data(L253.user_Retrofit_off_USA, "RetrofitOff") %>%
      add_precursors("L253.user_EF_retrofit_USA",
                     "L253.user_EF_NSPS_new_vintage_USA",
                     "L253.user_Retrofit_off_USA",
                     "L253.user_delete_gdp_control_USA") -> emission_factor_controls_user_USA.xml

    return_data(emission_factor_controls.xml,
                emission_factor_controls_user.xml,
                emission_factor_controls_USA.xml,
                emission_factor_controls_user_USA.xml)

  } else {
    stop("Unknown command")
  }
}
