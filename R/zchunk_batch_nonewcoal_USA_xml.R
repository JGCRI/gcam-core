#' module_gcamusa_batch_nonewcoal_USA_xml
#'
#' Construct XML data structure for \code{nonewcoal_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{nonewcoal_USA.xml}. The corresponding file in the
#' original data system was \code{batch_nonewcoal_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_nonewcoal_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2231.StubTechShrwt_nonewcoal_USA",
             "L2231.StubTechShrwt_nonewcoal_nongen_USA" ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "nonewcoal_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2231.StubTechShrwt_nonewcoal_USA <- get_data(all_data, "L2231.StubTechShrwt_nonewcoal_USA")
    L2231.StubTechShrwt_nonewcoal_nongen_USA <- get_data(all_data, "L2231.StubTechShrwt_nonewcoal_nongen_USA")

    # Produce outputs
    create_xml("nonewcoal_USA.xml") %>%
      add_xml_data(L2231.StubTechShrwt_nonewcoal_nongen_USA, "StubTechShrwt") %>%
      add_xml_data_generate_levels(L2231.StubTechShrwt_nonewcoal_USA%>% rename(stub.technology = technology), "StubTechShrwt","subsector","nesting-subsector",1,FALSE) %>%
      add_precursors("L2231.StubTechShrwt_nonewcoal_USA",
                     "L2231.StubTechShrwt_nonewcoal_nongen_USA") ->
      nonewcoal_USA.xml

    return_data(nonewcoal_USA.xml)
  } else {
    stop("Unknown command")
  }
}
