# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_geothermal_fixed_USA_xml
#'
#' Construct XML data structure for \code{geothermal_fixed_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{geothermal_fixed_USA.xml}.
module_gcamusa_batch_geothermal_fixed_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2245.DeleteRenewRsrc_USAgeo",
             "L2245.DeleteNestingSubsector1_USAgeo",
             "L2245.GlobalTechShrwt_USAgeo",
             "L2245.DeleteGlobalTech_USAgeo",
             "L2245.SubsectorLogit_USAgeo",
             "L2245.SubsectorLogit_cool_USAgeo",
             "L2245.StubTechFixOut_geothermal_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "geothermal_fixed_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    subsector0 <- "nesting-subsector" <- NULL  # silence package check notes

    # Load required inputs
    L2245.DeleteRenewRsrc_USAgeo <- get_data(all_data, "L2245.DeleteRenewRsrc_USAgeo")
    L2245.DeleteNestingSubsector1_USAgeo <- get_data(all_data, "L2245.DeleteNestingSubsector1_USAgeo")
    L2245.GlobalTechShrwt_USAgeo <- get_data(all_data, "L2245.GlobalTechShrwt_USAgeo")
    L2245.DeleteGlobalTech_USAgeo <- get_data(all_data, "L2245.DeleteGlobalTech_USAgeo")
    L2245.SubsectorLogit_USAgeo <- get_data(all_data, "L2245.SubsectorLogit_USAgeo")
    L2245.SubsectorLogit_cool_USAgeo <- get_data(all_data, "L2245.SubsectorLogit_cool_USAgeo")
    L2245.StubTechFixOut_geothermal_USA <- get_data(all_data, "L2245.StubTechFixOut_geothermal_USA")

    ## In order to delete the entire nesting subsector of geothermal fuel types in GCAM-USA
    ## we must change the subsector heading title to be nesting-subsector.
    ## This structure is currently not set up to be processed in 'add_xml_data_generate_levels'
    ## therefore we preprocess the data here.
    L2245.DeleteNestingSubsector1_USAgeo %>% rename("nesting-subsector"="subsector0") ->
      L2245.DeleteNestingSubsector1_USAgeo

    L2245.SubsectorLogit_USAgeo <- rename(L2245.SubsectorLogit_USAgeo, subsector = subsector0)

    # ===================================================

    # Produce outputs
    create_xml("geothermal_fixed_USA.xml") %>%
      add_xml_data(L2245.DeleteRenewRsrc_USAgeo, "DeleteRenewRsrc") %>%
      add_xml_data(L2245.DeleteNestingSubsector1_USAgeo,
                   "DeleteNestingSubsector1") %>%
      add_xml_data(L2245.DeleteGlobalTech_USAgeo, "DeleteGlobalTech") %>%
      add_xml_data(L2245.GlobalTechShrwt_USAgeo, "GlobalTechShrwt") %>%
      add_logit_tables_xml_generate_levels(L2245.SubsectorLogit_cool_USAgeo,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2245.StubTechFixOut_geothermal_USA%>% rename(stub.technology = technology),
                                   "StubTechFixOut","subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L2245.SubsectorLogit_USAgeo, "SubsectorLogit") %>%
      add_precursors("L2245.DeleteRenewRsrc_USAgeo",
                     "L2245.DeleteNestingSubsector1_USAgeo",
                     "L2245.GlobalTechShrwt_USAgeo",
                     "L2245.DeleteGlobalTech_USAgeo",
                     "L2245.SubsectorLogit_USAgeo",
                     "L2245.SubsectorLogit_cool_USAgeo",
                     "L2245.StubTechFixOut_geothermal_USA") ->
      geothermal_fixed_USA.xml

    return_data(geothermal_fixed_USA.xml)
  } else {
    stop("Unknown command")
  }
}
