#' module_gcamusa_batch_industry_adv_addon_USA_xml
#'
#' Construct XML data structure for \code{industry_adv_addon_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_adv_addon_USA.xml}.
#' The corresponding file in the original data system was \code{batch_industry_USA_adv_addon.xml} (gcamusa XML batch).
module_gcamusa_batch_industry_adv_addon_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L232.StubTechEff_ind_adv_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_adv_addon_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L232.StubTechEff_ind_adv_USA <- get_data(all_data, "L232.StubTechEff_ind_adv_USA")

    # ===================================================

    # Produce outputs
    create_xml("industry_adv_addon_USA.xml") %>%
      add_xml_data(L232.StubTechEff_ind_adv_USA, "StubTechEff") %>%
      add_precursors("L232.StubTechEff_ind_adv_USA") ->
      industry_adv_addon_USA.xml

    return_data(industry_adv_addon_USA.xml)
  } else {
    stop("Unknown command")
  }
}
