#' module_emissions_L2112.ag_nonco2_IRR_MGMT
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{object}, \code{L2112.AWBEmissions}, \code{L2112.AGREmissions}. The corresponding file in the
#' original data system was \code{L2112.ag_nonco2_IRR_MGMT.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L2112.ag_nonco2_IRR_MGMT <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2111.AGREmissions",
             "L2111.AGRBio",
             "L2111.AWB_BCOC_EmissCoeff",
             "L2111.nonghg_max_reduction",
             "L2111.nonghg_steepness",
             "L2111.AWBEmissions",
             FILE = "temp-data-inject/L211.AnEmissions",
             FILE = "temp-data-inject/L211.AnNH3Emissions",
             FILE = "temp-data-inject/L2012.AgProduction_ag_irr_mgmt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2112.AWBEmissions",
             "L2112.AGREmissions"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2111.AGREmissions <- get_data(all_data, "L2111.AGREmissions")
    L2111.AGRBio <- get_data(all_data, "L2111.AGRBio")
    L2111.AWB_BCOC_EmissCoeff <- get_data(all_data, "L2111.AWB_BCOC_EmissCoeff")
    L2111.nonghg_max_reduction <- get_data(all_data, "L2111.nonghg_max_reduction")
    L2111.nonghg_steepness <- get_data(all_data, "L2111.nonghg_steepness")
    L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "temp-data-inject/L2012.AgProduction_ag_irr_mgmt")
    L211.AnEmissions <- get_data(all_data, "temp-data-inject/L211.AnEmissions")
    L211.AnNH3Emissions <- get_data(all_data, "temp-data-inject/L211.AnNH3Emissions")

    # ===================================================

    # 2. Build tables for CSVs
    #Pass through the animal tables
    L2111.AGRBio %>%
      repeat_add_columns(tibble::tibble(level = c("hi", "lo"))) %>%
      unite(AgProductionTechnology, AgProductionTechnology, level, sep = "_") ->
      L2112.AGRBio

    L2111.AWB_BCOC_EmissCoeff %>%
      repeat_add_columns(tibble::tibble(level = c("hi", "lo"))) %>%
      unite(AgProductionTechnology, AgProductionTechnology, level, sep = "_") ->
      L2112.AWB_BCOC_EmissCoeff

    L2111.nonghg_max_reduction %>%
      repeat_add_columns(tibble::tibble(level = c("hi", "lo"))) %>%
      unite(AgProductionTechnology, AgProductionTechnology, level, sep = "_") ->
      L2112.nonghg_max_reduction

    L2111.nonghg_steepness %>%
      repeat_add_columns(tibble::tibble(level = c("hi", "lo"))) %>%
      unite(AgProductionTechnology, AgProductionTechnology, level, sep = "_") ->
      L2112.nonghg_steepness


    #For the tables whose emissions are read as quantities rather than rates, disaggregate emissions on the basis of production
    L2012.AgProduction_ag_irr_mgmt %>%
      filter(year == max(BASE_YEARS)) %>%
      mutate(AgProductionTechnology = gsub("_hi|_lo", "", AgProductionTechnology)) %>%
      group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year) %>%
      summarise(total = sum(calOutputValue)) ->
      L2112.AgProduction_ag_irr_nomgmt

    L2012.AgProduction_ag_irr_mgmt %>%
      filter(year == max(BASE_YEARS)) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology_level = AgProductionTechnology, year, calOutputValue) %>%
      mutate(m_tech = gsub("_hi|_lo", "", AgProductionTechnology_level)) ->
      L2112.AgProduction_ag

    # MAY BE ABLE TO DROP THE YEARS.. ALL IN 2010..
    L2112.AgProduction_ag %>%
      left_join_error_no_match(L2112.AgProduction_ag_irr_nomgmt,
                               by = c("region", "AgSupplySector", "year", "AgSupplySubsector", c("m_tech" = "AgProductionTechnology" ))) %>%
      mutate(share_tech = calOutputValue / total) %>%
      rename( AgProductionTechnology =  AgProductionTechnology_level) ->
      L2112.AgProduction_ag

    #These shares can now be matched in to the emissions quantities, and multiplied through
    L2111.AWBEmissions %>%
      bind_rows(L2111.AGREmissions) %>%
      repeat_add_columns(tibble::tibble(level = c("hi", "lo"))) %>%
      unite(AgProductionTechnology_level, AgProductionTechnology, level, sep = "_", remove = FALSE) ->
      L2112.awb_agr_emissions


    L2112.AgProduction_ag %>%
      select(-year, -AgSupplySubsector) %>%
      left_join(L2112.awb_agr_emissions, by = c("region", "AgSupplySector", "AgProductionTechnology" = "AgProductionTechnology_level")) ->
      L2112.awb_agr_emissions


    #Where shares allocated to lo/hi are NA but emissions are positive, split it 50/50 between the techs. For all others, set share to zero
    L2112.awb_agr_emissions %>%
      mutate(share_tech = if_else(is.na(share_tech) & input.emissions > 1e-6, 0.5, share_tech)) %>%
      mutate(share_tech = if_else(is.na(share_tech), 0, share_tech)) %>%
      mutate(input.emissions  = input.emissions * share_tech) %>%
      select(-share_tech, -total, -m_tech, -calOutputValue, -AgProductionTechnology) %>%
      rename(AgProductionTechnology = `AgProductionTechnology.y`)->
      L2112.awb_agr_emissions

    L2112.awb_agr_emissions %>%
      filter(grepl( "AWB", Non.CO2 )) %>%
      rename(value = `input.emissions`)->
      L2112.AWBEmissions

    L2112.awb_agr_emissions %>%
      filter(!grepl( "AWB", Non.CO2 )) %>%
      rename(value = `input.emissions`)->
      L2112.AGREmissions

    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all

#
#     create_xml("aglu_emissions_IRR_MGMT.xml") %>%
#       add_xml_data(L2112.AGRBio, "L2112.AGRBio") %>%
#       add_xml_data(L2112.AWB_BCOC_EmissCoeff, "L2112.AWB_BCOC_EmissCoeff") %>%
#       add_xml_data(L2112.nonghg_max_reduction, "L2112.nonghg_max_reduction") %>%
#       add_xml_data(L2112.nonghg_steepness, "L2112.nonghg_steepness") ->
#       add_precursors("common/GCAM_region_names", "emissions/A_regions","L2111.AWBEmissions",
#                      "L2111.AGREmissions", "L2111.AGRBio", "L2111.AWB_BCOC_EmissCoeff",
#                      "L2111.nonghg_max_reduction", "L2111.nonghg_steepness",
#                      "temp-data-inject/L2012.AgProduction_ag_irr_mgmt",
#                      "temp-data-inject/L2111.AnEmissions",
#                      "temp-data-inject/L2111.AnNH3Emissions") ->
#       batch_all_aglu_emissions_IRR_MGMT.xml
#     return_data(aglu_emissions_IRR_MGMT.xml)
    L2112.AWBEmissions %>%
      add_title("bleh") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2112.AWBEmissions") %>%
      add_precursors("L2111.AWBEmissions",
        "L2111.AGREmissions",
        "L2111.AGRBio",
        "L2111.AWB_BCOC_EmissCoeff",
        "L2111.nonghg_max_reduction",
        "L2111.nonghg_steepness", "temp-data-inject/L211.AnEmissions",
        "temp-data-inject/L211.AnNH3Emissions", "temp-data-inject/L2012.AgProduction_ag_irr_mgmt") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_SUM_TEST, FLAG_SUM_TEST) ->
      L2112.AWBEmissions
    L2112.AGREmissions %>%
      add_title("bleh") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2112.AGREmissions") %>%
      add_precursors("L2111.AWBEmissions",
        "L2111.AGREmissions",
        "L2111.AGRBio",
        "L2111.AWB_BCOC_EmissCoeff",
        "L2111.nonghg_max_reduction",
        "L2111.nonghg_steepness", "temp-data-inject/L211.AnEmissions",
        "temp-data-inject/L211.AnNH3Emissions", "temp-data-inject/L2012.AgProduction_ag_irr_mgmt") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_SUM_TEST) ->
      L2112.AGREmissions

    return_data(L2112.AWBEmissions, L2112.AGREmissions)
  } else {
    stop("Unknown command")
  }
}
