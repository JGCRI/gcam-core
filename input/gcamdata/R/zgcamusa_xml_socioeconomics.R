# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_socioeconomics_xml
#'
#' Construct XML data structure for \code{socioeconomics_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_USA.xml}. The corresponding file in the
#' original data system was \code{batch_socioeconomics_USA.xml} (gcamusa XML).
module_gcamusa_socioeconomics_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.Pop_GCAMUSA",
             "L201.GDP_GCAMUSA",
             "L201.Pop_national_updated_USA",
             "L201.GDP_national_updated_USA",
             "L201.DeleteNestedCESMacro_USA",
             "L201.LaborMaterials_UnlimitRsrc_USA",
             "L201.LaborMaterials_UnlimitRsrcPrice_USA",
             "L201.DeleteRsrc_USA",
             "L201.BasePriceIgnoreRegion_USA",
             "L201.CapResource_USA",
             "L201.CapResourcePrice_USA",
             "L201.CapitalLink_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.Pop_GCAMUSA <- get_data(all_data, "L201.Pop_GCAMUSA")
    L201.GDP_GCAMUSA <- get_data(all_data, "L201.GDP_GCAMUSA")
    L201.Pop_national_updated_USA <- get_data(all_data, "L201.Pop_national_updated_USA")
    L201.GDP_national_updated_USA <- get_data(all_data, "L201.GDP_national_updated_USA")
    L201.DeleteNestedCESMacro_USA <- get_data(all_data, "L201.DeleteNestedCESMacro_USA")
    L201.LaborMaterials_UnlimitRsrc_USA <- get_data(all_data, "L201.LaborMaterials_UnlimitRsrc_USA")
    L201.LaborMaterials_UnlimitRsrcPrice_USA <- get_data(all_data, "L201.LaborMaterials_UnlimitRsrcPrice_USA")
    L201.DeleteRsrc_USA <- get_data(all_data, "L201.DeleteRsrc_USA")
    L201.BasePriceIgnoreRegion_USA <- get_data(all_data, "L201.BasePriceIgnoreRegion_USA") %>%
      mutate(dummy = ignore.region)
    L201.CapResource_USA <- get_data(all_data, "L201.CapResource_USA")
    L201.CapResourcePrice_USA <- get_data(all_data, "L201.CapResourcePrice_USA")
    L201.CapitalLink_USA <- get_data(all_data, "L201.CapitalLink_USA")

    # ===================================================

    # Produce outputs
    create_xml("socioeconomics_USA.xml") %>%
      add_xml_data(L201.Pop_national_updated_USA, "Pop") %>%
      add_xml_data(L201.GDP_national_updated_USA, "GDP") %>%
      add_xml_data(L201.DeleteRsrc_USA, "DeleteRsrc") %>%
      add_xml_data(L201.DeleteNestedCESMacro_USA, "DeleteNestedCESMacro") %>%
      add_xml_data(L201.Pop_GCAMUSA, "Pop") %>%
      add_xml_data(L201.GDP_GCAMUSA, "GDP") %>%
      add_xml_data(L201.LaborMaterials_UnlimitRsrc_USA, "UnlimitRsrc") %>%
      add_xml_data(L201.LaborMaterials_UnlimitRsrcPrice_USA, "UnlimitRsrcPrice") %>%
      add_xml_data(L201.BasePriceIgnoreRegion_USA, "BasePriceIgnoreRegion") %>%
      add_xml_data(L201.CapResource_USA, "UnlimitRsrc") %>%
      add_xml_data(L201.CapResourcePrice_USA, "UnlimitRsrcPrice") %>%
      add_xml_data(L201.CapitalLink_USA, "GHGConstrLink") %>%
      add_precursors("L201.Pop_GCAMUSA",
                     "L201.GDP_GCAMUSA",
                     "L201.Pop_national_updated_USA",
                     "L201.GDP_national_updated_USA",
                     "L201.DeleteNestedCESMacro_USA",
                     "L201.LaborMaterials_UnlimitRsrc_USA",
                     "L201.LaborMaterials_UnlimitRsrcPrice_USA",
                     "L201.DeleteRsrc_USA",
                     "L201.BasePriceIgnoreRegion_USA",
                     "L201.CapResource_USA",
                     "L201.CapResourcePrice_USA",
                     "L201.CapitalLink_USA") ->
      socioeconomics_USA.xml

    return_data(socioeconomics_USA.xml)
  } else {
    stop("Unknown command")
  }
}
