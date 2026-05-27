# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_IncomeElasticity_xml
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_incelas_SSP[1-5].xml}.
module_socio_IncomeElasticity_xml <- function(command, ...) {

  SSP_NUMS <- 1:5

  MODULE_INPUTS <-
    c("L2321.IncomeElasticity_cement_Scen",
      "L2323.IncomeElasticity_iron_steel_Scen",
      "L2324.IncomeElasticity_Off_road_Scen",
      "L2325.IncomeElasticity_chemical_Scen",
      "L2326.IncomeElasticity_aluminum_Scen",
      "L2327.IncomeElasticity_paper_Scen",
      "L232.IncomeElasticity_ind_Scen")
  # other sectors to be collected later
  # food and nonfood: e.g., L203.IncomeElasticity
  # multiple consumers are used in building
  # building "L242.IncomeElasticity_bld_Scen"
  # transport: "L254.IncomeElasticity_trn"
  # water municipal: "L245.IncomeElasticity"
  # ToDo: consider collecting all income and price elasticity for final end use sectors

  MODULE_OUTPUTS <-
    setNames(
      c(paste0("socioeconomics_incelas_ssp", SSP_NUMS, ".xml") ),
      rep("XML", 5))

  # paste0("bld_agg_SSP", SSP_NUMS, ".xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Loop through all the SSP, and gSSP objects and build the corresponding XML structure

    for(ssp in SSP_NUMS) {

      ssp_name <- paste0("SSP", ssp)
      xmlfn <- paste0("socioeconomics_incelas_ssp", ssp, '.xml')

      create_xml(xmlfn) %>%
        ## cement ----
        add_xml_data(L2321.IncomeElasticity_cement_Scen %>% filter(scenario == ssp_name), "IncomeElasticity") %>%
        ## iron_steel ----
        add_xml_data(L2323.IncomeElasticity_iron_steel_Scen %>% filter(scenario == ssp_name), "IncomeElasticity") %>%
        ## off road: ag energy, mining, construction----
        add_xml_data(L2324.IncomeElasticity_Off_road_Scen %>% filter(scenario == ssp_name), "IncomeElasticity") %>%
        ## chemical ----
        add_xml_data(L2325.IncomeElasticity_chemical_Scen %>% filter(scenario == ssp_name), "IncomeElasticity") %>%
        ## aluminum ----
        add_xml_data(L2326.IncomeElasticity_aluminum_Scen %>% filter(scenario == ssp_name), "IncomeElasticity") %>%
        ## paper ----
        add_xml_data(L2327.IncomeElasticity_paper_Scen %>% filter(scenario == ssp_name), "IncomeElasticity") %>%
        ## Other ind ----
        add_xml_data(L232.IncomeElasticity_ind_Scen %>% filter(scenario == ssp_name), "IncomeElasticity") %>%
        add_precursors(MODULE_INPUTS) ->
        xml_obj

      # Assign output to output name
      assign(xmlfn, xml_obj)
    }

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
