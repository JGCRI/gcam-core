#' module_energy_batch_industry_incelas_SSP_xml
#'
#' Construct XML data structures for all the \code{industry_incelas_SSP.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_incelas_gcam3.xml}, \code{industry_incelas_ssp1.xml}, \code{industry_incelas_ssp2.xml}, \code{industry_incelas_ssp3.xml},
#' \code{industry_incelas_ssp4.xml}, \code{industry_incelas_ssp5.xml}, \code{industry_incelas_gssp1.xml}, \code{industry_incelas_gssp2.xml},
#' \code{industry_incelas_gssp3.xml}, \code{industry_incelas_gssp4.xml}, and \code{industry_incelas_gssp5.xml}.
module_energy_batch_industry_incelas_SSP_xml <- function(command, ...) {

  INCOME_ELASTICITY_INPUTS <- c("GCAM3",
                                paste0("gSSP", 1:5),
                                paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste("L232.IncomeElasticity_ind", tolower(INCOME_ELASTICITY_INPUTS), sep = "_")))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_incelas_gcam3.xml",
             XML = "industry_incelas_gssp1.xml",
             XML = "industry_incelas_gssp2.xml",
             XML = "industry_incelas_gssp3.xml",
             XML = "industry_incelas_gssp4.xml",
             XML = "industry_incelas_gssp5.xml",
             XML = "industry_incelas_ssp1.xml",
             XML = "industry_incelas_ssp2.xml",
             XML = "industry_incelas_ssp3.xml",
             XML = "industry_incelas_ssp4.xml",
             XML = "industry_incelas_ssp5.xml"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    industry_incelas_gcam3.xml <- industry_incelas_ssp1.xml <- industry_incelas_ssp2.xml <- industry_incelas_ssp3.xml <-
      industry_incelas_ssp4.xml <- industry_incelas_ssp5.xml<- industry_incelas_gssp1.xml<- industry_incelas_gssp2.xml<-
      industry_incelas_gssp3.xml<- industry_incelas_gssp4.xml <- industry_incelas_gssp5.xml <- NULL

    all_data <- list(...)[[1]]

    # Loop through all the GCAM3, SSP, and gSSP objects and build the corresponding XML structure
    for(iei in INCOME_ELASTICITY_INPUTS) {
      data_obj <- paste0("L232.IncomeElasticity_ind_", tolower(iei))
      xmlfn <- paste0("industry_incelas_", tolower(iei), '.xml')

      create_xml(xmlfn) %>%
        add_xml_data(get_data(all_data, data_obj), "IncomeElasticity") %>%
        add_precursors(paste0("L232.IncomeElasticity_ind_", tolower(iei))) ->
        xml_obj

      # Assign output to output name
      assign(xmlfn, xml_obj)
    }

    return_data(industry_incelas_gcam3.xml,
                industry_incelas_ssp1.xml, industry_incelas_ssp2.xml, industry_incelas_ssp3.xml, industry_incelas_ssp4.xml, industry_incelas_ssp5.xml,
                industry_incelas_gssp1.xml, industry_incelas_gssp2.xml, industry_incelas_gssp3.xml, industry_incelas_gssp4.xml, industry_incelas_gssp5.xml)
  } else {
    stop("Unknown command")
  }
}
