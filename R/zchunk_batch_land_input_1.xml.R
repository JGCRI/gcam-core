#' module_aglu_batch_land_input_1.xml
#'
#' Construct XML data structure for \code{land_input_1.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{land_input_1.xml}. The corresponding file in the
#' original data system was \code{batch_land_input_1.xml.R} (aglu XML).
module_aglu_batch_land_input_1.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L221.LN0_Logit",
              "L221.LN0_Land",
              "L221.LN0_SoilTimeScale",
              "L221.LN1_ValueLogit",
              "L221.LN1_HistUnmgdAllocation",
              "L221.LN1_UnmgdAllocation",
              "L221.LN1_UnmgdCarbon"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "land_input_1.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L221.LN0_Logit <- get_data(all_data, "L221.LN0_Logit")
    L221.LN0_Land <- get_data(all_data, "L221.LN0_Land")
    L221.LN0_SoilTimeScale <- get_data(all_data, "L221.LN0_SoilTimeScale")
    L221.LN1_ValueLogit <- get_data(all_data, "L221.LN1_ValueLogit")
    L221.LN1_HistUnmgdAllocation <- get_data(all_data, "L221.LN1_HistUnmgdAllocation")
    L221.LN1_UnmgdAllocation <- get_data(all_data, "L221.LN1_UnmgdAllocation")
    L221.LN1_UnmgdCarbon <- get_data(all_data, "L221.LN1_UnmgdCarbon")

    # ===================================================

    # Produce outputs
    create_xml("land_input_1.xml") %>%
      add_logit_tables_xml(L221.LN0_Logit, "LN0_Logit") %>%
      add_xml_data(L221.LN0_Land, "LN0_Land") %>%
      add_xml_data(L221.LN0_SoilTimeScale, "LN0_SoilTimeScale") %>%
      add_logit_tables_xml(L221.LN1_ValueLogit, "LN1_ValueLogit", "LN1_Logit") %>%
      add_xml_data(L221.LN1_HistUnmgdAllocation, "LN1_HistUnmgdAllocation") %>%
      add_xml_data(L221.LN1_UnmgdAllocation, "LN1_UnmgdAllocation") %>%
      add_xml_data(L221.LN1_UnmgdCarbon, "LN1_UnmgdCarbon") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L221.LN0_Logit", "L221.LN0_Land", "L221.LN0_SoilTimeScale", "L221.LN1_ValueLogit", "L221.LN1_HistUnmgdAllocation", "L221.LN1_UnmgdAllocation", "L221.LN1_UnmgdCarbon") ->
      land_input_1.xml

    return_data(land_input_1.xml)
  } else {
    stop("Unknown command")
  }
}
