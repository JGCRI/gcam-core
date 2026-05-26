#' module_gcamusa_batch_fossil_res_nonghg_emissions_USA_xml
#'
#' Construct XML data structure for \code{fossil_res_emissions_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{fossil_res_emissions_USA.xml}. The corresponding file in the
#' original data system was \code{batch_en_nonghg_emissions_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_fossil_res_nonghg_emissions_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L273.nonghg_state_fossil_res_EF_USA",
             "L273.ResReadInControl_nonghg_res_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "fossil_res_emissions_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L273.nonghg_state_fossil_res_EF_USA <- get_data(all_data, "L273.nonghg_state_fossil_res_EF_USA", strip_attributes = TRUE)
    L273.ResReadInControl_nonghg_res_USA <- get_data(all_data, "L273.ResReadInControl_nonghg_res_USA", strip_attributes = TRUE)

    # ===================================================

    # Produce outputs
    create_xml("fossil_res_emissions_USA.xml") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L273.nonghg_state_fossil_res_EF_USA, "ResEmissCoef") %>%
      add_xml_data(L273.ResReadInControl_nonghg_res_USA, "ResReadInControl") %>%
      add_precursors("L273.nonghg_state_fossil_res_EF_USA",
                     "L273.ResReadInControl_nonghg_res_USA") ->
      fossil_res_emissions_USA.xml

    return_data(fossil_res_emissions_USA.xml)
  } else {
    stop("Unknown command")
  }
}
