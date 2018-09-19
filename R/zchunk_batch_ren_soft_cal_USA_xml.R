#' module_gcamusa_batch_ren_soft_cal_USA_xml
#'
#' Construct XML data structure for \code{ren_soft_cal_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ren_soft_cal_USA.xml}.
#' The corresponding file in the original data system was \code{batch_ren_soft_cal_USA.xml} (gcamusa XML batch).
module_gcamusa_batch_ren_soft_cal_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2243.StubTechFixOut_ren_soft_cal_2015_USA",
             "L2243.GlobalTechShrwt_ren_soft_cal_2015_USA",
             "L2243.PrimaryRenewKeyword_ren_soft_cal_2015_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ren_soft_cal_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2243.StubTechFixOut_ren_soft_cal_2015_USA <- get_data(all_data, "L2243.StubTechFixOut_ren_soft_cal_2015_USA")
    L2243.GlobalTechShrwt_ren_soft_cal_2015_USA <- get_data(all_data, "L2243.GlobalTechShrwt_ren_soft_cal_2015_USA")
    L2243.PrimaryRenewKeyword_ren_soft_cal_2015_USA <- get_data(all_data, "L2243.PrimaryRenewKeyword_ren_soft_cal_2015_USA")

    # ===================================================

    # Produce outputs
    create_xml("ren_soft_cal_USA.xml") %>%
      add_xml_data(L2243.StubTechFixOut_ren_soft_cal_2015_USA, "StubTechFixOut") %>%
      add_xml_data(L2243.GlobalTechShrwt_ren_soft_cal_2015_USA, "GlobalTechShrwt") %>%
      add_xml_data(L2243.PrimaryRenewKeyword_ren_soft_cal_2015_USA, "PrimaryRenewKeyword") %>%
      add_precursors("L2243.StubTechFixOut_ren_soft_cal_2015_USA",
                     "L2243.GlobalTechShrwt_ren_soft_cal_2015_USA",
                     "L2243.PrimaryRenewKeyword_ren_soft_cal_2015_USA") ->
      ren_soft_cal_USA.xml

    return_data(ren_soft_cal_USA.xml)
  } else {
    stop("Unknown command")
  }
}
