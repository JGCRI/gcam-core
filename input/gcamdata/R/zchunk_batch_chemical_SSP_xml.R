# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_chemical_incelas_SSP_xml
#'
#' Construct XML data structures for all the \code{chemical_incelas_SSP.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{chemical_incelas_gcam3.xml}, \code{chemical_incelas_ssp1.xml}, \code{chemical_incelas_ssp2.xml}, \code{chemical_incelas_ssp3.xml},
#' \code{chemical_incelas_ssp4.xml}, \code{chemical_incelas_ssp5.xml}, \code{chemical_incelas_gssp1.xml}, \code{chemical_incelas_gssp2.xml},
#' \code{chemical_incelas_gssp3.xml}, \code{chemical_incelas_gssp4.xml}, and \code{chemical_incelas_gssp5.xml}.
module_energy_batch_chemical_incelas_SSP_xml <- function(command, ...) {

  INCOME_ELASTICITY_INPUTS <- c("GCAM3",
                                paste0("gSSP", 1:5),
                                paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste("L2325.chemical_incelas", tolower(INCOME_ELASTICITY_INPUTS), sep = "_")))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "chemical_incelas_gcam3.xml",
             XML = "chemical_incelas_gssp1.xml",
             XML = "chemical_incelas_gssp2.xml",
             XML = "chemical_incelas_gssp3.xml",
             XML = "chemical_incelas_gssp4.xml",
             XML = "chemical_incelas_gssp5.xml",
             XML = "chemical_incelas_ssp1.xml",
             XML = "chemical_incelas_ssp2.xml",
             XML = "chemical_incelas_ssp3.xml",
             XML = "chemical_incelas_ssp4.xml",
             XML = "chemical_incelas_ssp5.xml"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    chemical_incelas_gcam3.xml <- chemical_incelas_ssp1.xml <- chemical_incelas_ssp2.xml <- chemical_incelas_ssp3.xml <-
      chemical_incelas_ssp4.xml <- chemical_incelas_ssp5.xml<- chemical_incelas_gssp1.xml<- chemical_incelas_gssp2.xml<-
      chemical_incelas_gssp3.xml<- chemical_incelas_gssp4.xml <- chemical_incelas_gssp5.xml <- NULL

    all_data <- list(...)[[1]]

    # Loop through all the GCAM3, SSP, and gSSP objects and build the corresponding XML structure
    for(iei in INCOME_ELASTICITY_INPUTS) {
      data_obj <- paste0("L2325.chemical_incelas_", tolower(iei))
      xmlfn <- paste0("chemical_incelas_",tolower(iei), '.xml')

      create_xml(xmlfn) %>%
        add_xml_data(get_data(all_data, data_obj), "IncomeElasticity") %>%
        add_precursors(paste0("L2325.chemical_incelas_", tolower(iei))) ->
        xml_obj

      # Assign output to output name
      assign(xmlfn, xml_obj)
    }

    return_data(chemical_incelas_gcam3.xml,
                chemical_incelas_ssp1.xml, chemical_incelas_ssp2.xml, chemical_incelas_ssp3.xml, chemical_incelas_ssp4.xml, chemical_incelas_ssp5.xml,
                chemical_incelas_gssp1.xml, chemical_incelas_gssp2.xml, chemical_incelas_gssp3.xml, chemical_incelas_gssp4.xml, chemical_incelas_gssp5.xml)
  } else {
    stop("Unknown command")
  }
}
