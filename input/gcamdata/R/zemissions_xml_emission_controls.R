# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_emission_controls_xml
#'
#' Construct XML data structure for \code{emission_factor_controls.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{EF_controls.xml}, \code{EF_controls_user.xml}
#' \code{EF_controls_USA.xml}, \code{EF_controls_user_USA.xml}

module_emissions_emission_controls_xml <- function(command, ...) {

  MODULE_INPUTS <- c("L253.EF_retrofit",
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
                     "L253.user_delete_gdp_control_USA")

  MODULE_OUTPUTS <- c(XML = "emission_factor_controls.xml",
                      XML = "emission_factor_controls_user.xml",
                      XML = "emission_factor_controls_USA.xml",
                      XML = "emission_factor_controls_user_USA.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Split into nesting/ non-nesting dataframes
    for(i in 1:length(MODULE_INPUTS)){
      # Load required inputs
      df <- get_data(all_data, MODULE_INPUTS[i], strip_attributes = TRUE)
      if("nesting.subsector" %in% names(df)){
        assign(paste0(MODULE_INPUTS[i], "_nesting"), df %>% filter(!is.na(nesting.subsector)) %>%
          rename(subsector0 = nesting.subsector))
        assign(MODULE_INPUTS[i], df %>% filter(is.na(nesting.subsector)) %>%
          select(-nesting.subsector))
      }
      else assign(MODULE_INPUTS[i], df)
    }

    # Produce outputs
    # Output 1
    create_xml("emission_factor_controls.xml") %>%
      add_xml_data(L253.EF_retrofit, "EF_Retrofit") %>%
      add_xml_data(L253.EF_NSPS_new_vintage, "EF_NSPS") %>%
      add_xml_data(L253.delete_gdp_control, "DeleteGDPControl") %>%
      add_xml_data(L253.Retrofit_off, "RetrofitOff") %>%
      add_precursors("L253.EF_retrofit",
                     "L253.EF_NSPS_new_vintage",
                     "L253.Retrofit_off",
                     "L253.delete_gdp_control") -> emission_factor_controls.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data
    if(exists("L253.EF_retrofit_nesting")) {
      emission_factor_controls.xml <- emission_factor_controls.xml %>%
        add_xml_data_generate_levels(L253.EF_retrofit_nesting, "EF_Retrofit", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.EF_retrofit_nesting")
    }
    if(exists("L253.EF_NSPS_new_vintage_nesting")) {
      emission_factor_controls.xml <- emission_factor_controls.xml %>%
        add_xml_data_generate_levels(L253.EF_NSPS_new_vintage_nesting, "EF_NSPS", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.EF_NSPS_new_vintage_nesting")
    }
    if(exists("L253.delete_gdp_control_nesting")) {
      emission_factor_controls.xml <- emission_factor_controls.xml %>%
        add_xml_data_generate_levels(L253.delete_gdp_control_nesting, "DeleteGDPControl", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.delete_gdp_control_nesting")
    }
    if(exists("L253.Retrofit_off_nesting")) {
      emission_factor_controls.xml <- emission_factor_controls.xml %>%
        add_xml_data_generate_levels(L253.Retrofit_off_nesting, "RetrofitOff", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.Retrofit_off_nesting")
    }

    # Output 2
    create_xml("emission_factor_controls_user.xml") %>%
      add_xml_data(L253.user_EF_retrofit, "EF_Retrofit") %>%
      add_xml_data(L253.user_EF_NSPS_new_vintage, "EF_NSPS") %>%
      add_xml_data(L253.user_delete_gdp_control, "DeleteGDPControl") %>%
      add_xml_data(L253.user_Retrofit_off, "RetrofitOff") %>%
      add_precursors("L253.user_EF_retrofit",
                     "L253.user_EF_NSPS_new_vintage",
                     "L253.user_Retrofit_off",
                     "L253.user_delete_gdp_control") -> emission_factor_controls_user.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data
    if(exists("L253.user_EF_retrofit_nesting")) {
      emission_factor_controls_user.xml <- emission_factor_controls_user.xml %>%
        add_xml_data_generate_levels(L253.user_EF_retrofit_nesting, "EF_Retrofit", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.user_EF_retrofit_nesting")
    }
    if(exists("L253.user_EF_NSPS_new_vintage_nesting")) {
      emission_factor_controls_user.xml <- emission_factor_controls_user.xml %>%
        add_xml_data_generate_levels(L253.user_EF_NSPS_new_vintage_nesting, "EF_NSPS", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.user_EF_NSPS_new_vintage_nesting")
    }
    if(exists("L253.user_delete_gdp_control_nesting")) {
      emission_factor_controls_user.xml <- emission_factor_controls_user.xml %>%
        add_xml_data_generate_levels(L253.user_delete_gdp_control_nesting, "DeleteGDPControl", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.user_delete_gdp_control_nesting")
    }
    if(exists("L253.user_Retrofit_off_nesting")) {
      emission_factor_controls_user.xml <- emission_factor_controls_user.xml %>%
        add_xml_data_generate_levels(L253.user_Retrofit_off_nesting, "RetrofitOff", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.user_Retrofit_off_nesting")
    }

    # Output #3
    create_xml("emission_factor_controls_USA.xml") %>%
      add_xml_data(L253.EF_retrofit_USA, "EF_Retrofit") %>%
      add_xml_data(L253.EF_NSPS_new_vintage_USA, "EF_NSPS") %>%
      add_xml_data(L253.delete_gdp_control_USA, "DeleteGDPControl") %>%
      add_xml_data(L253.Retrofit_off_USA, "RetrofitOff") %>%
      add_precursors("L253.EF_retrofit_USA",
                     "L253.EF_NSPS_new_vintage_USA",
                     "L253.Retrofit_off_USA",
                     "L253.delete_gdp_control_USA") -> emission_factor_controls_USA.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data
    if(exists("L253.EF_retrofit_USA_nesting")) {
      emission_factor_controls_USA.xml <- emission_factor_controls_USA.xml %>%
        add_xml_data_generate_levels(L253.EF_retrofit_USA_nesting, "EF_Retrofit", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.EF_retrofit_USA_nesting")
    }
    if(exists("L253.EF_NSPS_new_vintage_USA_nesting")) {
      emission_factor_controls_USA.xml <- emission_factor_controls_USA.xml %>%
        add_xml_data_generate_levels(L253.EF_NSPS_new_vintage_USA_nesting, "EF_NSPS", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.EF_NSPS_new_vintage_USA_nesting")
    }
    if(exists("L253.delete_gdp_control_USA_nesting")) {
      emission_factor_controls_USA.xml <- emission_factor_controls_USA.xml %>%
        add_xml_data_generate_levels(L253.delete_gdp_control_USA_nesting, "DeleteGDPControl", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.delete_gdp_control_USA_nesting")
    }
    if(exists("L253.Retrofit_off_USA_nesting")) {
      emission_factor_controls_USA.xml <- emission_factor_controls_USA.xml %>%
        add_xml_data_generate_levels(L253.Retrofit_off_USA_nesting, "RetrofitOff", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.Retrofit_off_USA_nesting")
    }

    # Output 4
    create_xml("emission_factor_controls_user_USA.xml") %>%
      add_xml_data(L253.user_EF_retrofit_USA, "EF_Retrofit") %>%
      add_xml_data(L253.user_EF_NSPS_new_vintage_USA, "EF_NSPS") %>%
      add_xml_data(L253.user_delete_gdp_control_USA, "DeleteGDPControl") %>%
      add_xml_data(L253.user_Retrofit_off_USA, "RetrofitOff") %>%
      add_precursors("L253.user_EF_retrofit_USA",
                     "L253.user_EF_NSPS_new_vintage_USA",
                     "L253.user_Retrofit_off_USA",
                     "L253.user_delete_gdp_control_USA") -> emission_factor_controls_user_USA.xml

    # Some data inputs may not actually contain data. If so, do not add_xml_data
    if(exists("L253.user_EF_retrofit_USA_nesting")) {
      emission_factor_controls_user_USA.xml <- emission_factor_controls_user_USA.xml %>%
        add_xml_data_generate_levels(L253.user_EF_retrofit_USA_nesting, "EF_Retrofit", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.user_EF_retrofit_USA_nesting")
    }
    if(exists("L253.user_EF_NSPS_new_vintage_USA_nesting")) {
      emission_factor_controls_user_USA.xml <- emission_factor_controls_user_USA.xml %>%
        add_xml_data_generate_levels(L253.user_EF_NSPS_new_vintage_USA_nesting, "EF_NSPS", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.user_EF_NSPS_new_vintage_USA_nesting")
    }
    if(exists("L253.user_delete_gdp_control_USA_nesting")) {
      emission_factor_controls_user_USA.xml <- emission_factor_controls_user_USA.xml %>%
        add_xml_data_generate_levels(L253.user_delete_gdp_control_USA_nesting, "DeleteGDPControl", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.user_delete_gdp_control_USA_nesting")
    }
    if(exists("L253.user_Retrofit_off_USA_nesting")) {
      emission_factor_controls_user_USA.xml <- emission_factor_controls_user_USA.xml %>%
        add_xml_data_generate_levels(L253.user_Retrofit_off_USA_nesting, "RetrofitOff", "subsector","nesting-subsector",1,FALSE) %>%
        add_precursors("L253.user_Retrofit_off_USA_nesting")
    }

    return_data(emission_factor_controls.xml,
                emission_factor_controls_user.xml,
                emission_factor_controls_USA.xml,
                emission_factor_controls_user_USA.xml)

  } else {
    stop("Unknown command")
  }
}
