#' module_gcamusa_batch_elc_emissions_USA_xml
#'
#' Construct XML data structure for \code{elc_emissions_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{elc_emissions_USA.xml}. The corresponding file in the
#' original data system was \code{batch_elc_nonghg_USA.R} (gcamusa XML).
module_gcamusa_batch_elc_emissions_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L272.nonghg_elec_tech_coeff_USA",
             "L2722.nonghg_elec_tech_coeff_USA_linear_control",
             "L2722.nonghg_elec_tech_coeff_USA_linear_control_off"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elc_emissions_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L272.nonghg_elec_tech_coeff_USA <- get_data(all_data, "L272.nonghg_elec_tech_coeff_USA")
    L2722.nonghg_elec_tech_coeff_USA_linear_control <- get_data(all_data, "L2722.nonghg_elec_tech_coeff_USA_linear_control")
    L2722.nonghg_elec_tech_coeff_USA_linear_control_off <- get_data(all_data, "L2722.nonghg_elec_tech_coeff_USA_linear_control_off")

    # ===================================================
    # Rename columns to meet the LEVEL2_DATA_NAMES header requirements
    L272.nonghg_elec_tech_coeff_USA <- rename(L272.nonghg_elec_tech_coeff_USA, stub.technology = technology)
    L2722.nonghg_elec_tech_coeff_USA_linear_control <- rename(L2722.nonghg_elec_tech_coeff_USA_linear_control, stub.technology = technology)
    L2722.nonghg_elec_tech_coeff_USA_linear_control_off <- rename(L2722.nonghg_elec_tech_coeff_USA_linear_control_off, stub.technology = technology)


    # Produce outputs
    create_xml("elc_emissions_USA.xml") %>%
      add_xml_data_generate_levels(L272.nonghg_elec_tech_coeff_USA, "InputEmissCoeff", "subsector", "nesting-subsector", 1, F) %>%
      add_xml_data_generate_levels(L2722.nonghg_elec_tech_coeff_USA_linear_control, "LinearCtrl", "subsector", "nesting-subsector", 1, F) %>%
      add_xml_data_generate_levels(L2722.nonghg_elec_tech_coeff_USA_linear_control_off, "RetrofitOff", "subsector", "nesting-subsector", 1, F) %>%
      add_precursors("L272.nonghg_elec_tech_coeff_USA",
                     "L2722.nonghg_elec_tech_coeff_USA_linear_control",
                     "L2722.nonghg_elec_tech_coeff_USA_linear_control_off") ->
      elc_emissions_USA.xml

    return_data(elc_emissions_USA.xml)
  } else {
    stop("Unknown command")
  }
}
