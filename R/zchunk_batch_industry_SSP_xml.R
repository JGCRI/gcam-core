#' module_energy_batch_industry_SSP_xml
#'
#' Construct XML data structures for all the \code{industry_SSP.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_GCAM3.xml}, \code{industry_SSP1.xml}, \code{industry_SSP2.xml}, \code{industry_SSP3.xml},
#' \code{industry_SSP4.xml}, \code{industry_SSP5.xml}, \code{industry_gSSP1.xml}, \code{industry_gSSP2.xml},
#' \code{industry_gSSP3.xml}, \code{industry_gSSP4.xml}, and \code{industry_gSSP5.xml}.
module_energy_batch_industry_SSP_xml <- function(command, ...) {

  INCOME_ELASTICITY_INPUTS <- c("GCAM3",
                                paste0("gSSP", 1:5),
                                paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste("L232.IncomeElasticity_ind", tolower(INCOME_ELASTICITY_INPUTS), sep = "_")))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(paste("industry", toupper(INCOME_ELASTICITY_INPUTS), sep = "_")))
  } else if(command == driver.MAKE) {

    # Silence package checks
    industry_GCAM3.xml <- industry_SSP1.xml <- industry_SSP2.xml <- industry_SSP3.xml <-
      industry_SSP4.xml <- industry_SSP5.xml<- industry_gSSP1.xml<- industry_gSSP2.xml<-
      industry_gSSP3.xml<- industry_gSSP4.xml <- industry_gSSP5.xml <- NULL

    all_data <- list(...)[[1]]

    # Loop through all the GCAM3, SSP, and gSSP objects and build the corresponding XML structure
    for(iei in INCOME_ELASTICITY_INPUTS) {
      data_obj <- paste0("L232.IncomeElasticity_ind_", tolower(INCOME_ELASTICITY_INPUTS))
      xmlfn <- paste0("industry_", toupper(INCOME_ELASTICITY_INPUTS))

      create_xml(xmlfn) %>%
        add_xml_data(get_data(all_data, data_obj), "IncomeElasticity") %>%
        add_precursors() ->
        xml_obj

      # Assign output to output name
      assign(xmlfn, xml_obj)
    }

    return_data(industry_GCAM3.xml,
                industry_SSP1.xml, industry_SSP2.xml, industry_SSP3.xml, industry_SSP4.xml, industry_SSP5.xml,
                industry_gSSP1.xml, industry_gSSP2.xml, industry_gSSP3.xml, industry_gSSP4.xml, industry_gSSP5.xml)
  } else {
    stop("Unknown command")
  }
}
