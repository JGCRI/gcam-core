#' module_gcamusa_L231.proc_sector_USA
#'
#' Writes urban & industrial processing sector outputs for USA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:\code{L231.DeleteSupplysector_industry_USA}, \code{L231.DeleteSupplysector_urban_processes_USA},
#' \code{L231.DeleteFinalDemand_urban_processes_USA}, \code{L231.UnlimitRsrc_USA}, \code{L231.UnlimitRsrcPrice_USA}, \code{L231.FinalDemand_urb_USA},
#' \code{L231.Supplysector_urb_ind_USA}, \code{L231.SubsectorLogit_urb_ind_USA},
#' \code{L231.SubsectorShrwt_urb_ind_USA}, \code{L231.SubsectorShrwtFllt_urb_ind_USA},
#' \code{L231.SubsectorInterp_urb_ind_USA}, \code{L231.SubsectorInterpTo_urb_ind_USA},
#' \code{L231.StubTech_urb_ind_USA}, \code{L231.RegionalTechCalValue_urb_ind_USA}, \code{L231.IndCoef_USA}.
#' @details For urban processes and industrial processes, subsectors
#' and supplysectors, produces logit, shareweights, and interpolation files by writing assumption file data to all regions.
#' Writes out coefficients and prices for misc emissions sources from assumption file data.
#' Outputs urban processing demand using constants. StubTech mapping file created from assumption mappings.
#' Regional technology calibration values created with constant. Industry input-output coefficient file created.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BY September 2019

module_gcamusa_L231.proc_sector_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE="emissions/A31.rsrc_info",
             "L231.FinalDemand_urb",
             FILE="emissions/A31.sector",
             "L231.IndCoef",
             FILE="gcam-usa/states_subregions",
             FILE="emissions/A31.subsector_logit",
             FILE="emissions/A31.subsector_shrwt",
             FILE="emissions/A31.subsector_interp",
             "L231.StubTech_urb_ind",
             "L231.GlobalTechCost_urb_ind",
             "L231.Ind_globaltech_eff",
             "L132.in_EJ_state_indnochp_F",
             "L132.in_EJ_state_indchp_F",
             "L132.in_EJ_state_indfeed_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L231.DeleteSupplysector_industry_USA",
             "L231.DeleteSupplysector_urban_processes_USA",
             "L231.DeleteFinalDemand_urban_processes_USA",
             "L231.UnlimitRsrc_USA",
             "L231.UnlimitRsrcPrice_USA",
             "L231.FinalDemand_urb_USA",
             "L231.Supplysector_urb_ind_USA",
             "L231.SubsectorLogit_urb_ind_USA",
             "L231.SubsectorShrwt_urb_ind_USA",
             "L231.SubsectorShrwtFllt_urb_ind_USA",
             "L231.SubsectorInterp_urb_ind_USA",
             "L231.SubsectorInterpTo_urb_ind_USA",
             "L231.StubTech_urb_ind_USA",
             "L231.RegionalTechCalValue_urb_ind_USA",
             "L231.IndCoef_USA"
             ))
  } else if(command == driver.MAKE) {

    # Silence package checks
    year <- value <- share.weight <- efficiency <- input.cost <- coefficient <-
      year.fillout <- to.value <- supplysector <- subsector <- technology <- minicam.energy.input <-
      minicam.non.energy.input <- region <- sector.name <- subsector.name <- calibrated.value <-
      market <- resource_type <- resource <- `output-unit` <- `price-unit` <- capacity.factor <-
      secondary.output <- state <- service <- ind_proc_input <- ind_output <- sector <- fuel <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A31.rsrc_info <- get_data(all_data, "emissions/A31.rsrc_info") %>%
      gather_years
    L231.FinalDemand_urb <- get_data(all_data, "L231.FinalDemand_urb", strip_attributes = TRUE)
    A31.sector <- get_data(all_data, "emissions/A31.sector", strip_attributes = TRUE)
    L231.IndCoef <- get_data(all_data, "L231.IndCoef", strip_attributes = TRUE)
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A31.subsector_logit <- get_data(all_data, "emissions/A31.subsector_logit", strip_attributes = TRUE)
    A31.subsector_shrwt <- get_data(all_data, "emissions/A31.subsector_shrwt", strip_attributes = TRUE)
    A31.subsector_interp <- get_data(all_data, "emissions/A31.subsector_interp", strip_attributes = TRUE)
    L231.StubTech_urb_ind <- get_data(all_data, "L231.StubTech_urb_ind", strip_attributes = TRUE)
    L231.GlobalTechCost_urb_ind <- get_data(all_data, "L231.GlobalTechCost_urb_ind", strip_attributes = TRUE)
    L231.Ind_globaltech_eff <- get_data(all_data, "L231.Ind_globaltech_eff", strip_attributes = TRUE)
    L132.in_EJ_state_indnochp_F <- get_data(all_data, "L132.in_EJ_state_indnochp_F", strip_attributes = TRUE)
    L132.in_EJ_state_indchp_F <- get_data(all_data, "L132.in_EJ_state_indchp_F", strip_attributes = TRUE)
    L132.in_EJ_state_indfeed_F <- get_data(all_data, "L132.in_EJ_state_indfeed_F", strip_attributes = TRUE)

    # ===================================================

    # Delete industry supplysector at the USA level
    L231.DeleteSupplysector_industry_USA <- L231.IndCoef %>%
      filter(region == gcam.USA_REGION) %>%
      select(region, supplysector) %>%
      distinct()

    # Delete urban processes supplysector at the USA level
    L231.DeleteSupplysector_urban_processes_USA <- tibble(region = gcam.USA_REGION, supplysector = A31.sector$supplysector) %>%
      filter( supplysector == "urban processes" )

    # Delete urban processes supplysector at the USA level
    L231.DeleteFinalDemand_urban_processes_USA <- L231.FinalDemand_urb %>%
      filter(region == gcam.USA_REGION) %>%
      select(region, energy.final.demand) %>%
      distinct()

    # L231.FinalDemand_urb_USA:
    # Final demand information for urban processes sector for all U.S. states (copy the parameters)
    L231.FinalDemand_urb_USA <- L231.FinalDemand_urb %>%
      select(-region) %>%
      distinct() %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["FinalDemandInfo"]])

    # L231.Supplysector_ind_USA:
    # Supply sector information for urban & industrial processes sectors for all U.S. states
    L231.Supplysector_urb_ind_USA <- A31.sector %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME))

    # Subsector information
    # L231.SubsectorLogit_urb_ind_USA:
    # Subsector logit exponents of urban & industrial processes sectors for all U.S. states
    L231.SubsectorLogit_urb_ind_USA <- A31.subsector_logit %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME))

    # L231.SubsectorShrwt_urb_ind_USA and L231.SubsectorShrwtFllt_urb_ind_USA:
    # Subsector shareweights of urban & industrial processes sectors for all U.S. states
    if(any(!is.na(A31.subsector_shrwt$year))) {
      L231.SubsectorShrwt_urb_ind_USA <- A31.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_states(LEVEL2_DATA_NAMES[["SubsectorShrwt"]])
    }
    if(any(!is.na(A31.subsector_shrwt$year.fillout))) {
      L231.SubsectorShrwtFllt_urb_ind_USA <- A31.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_states(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])
    }

    # L231.SubsectorInterp_urb_ind_USA and L231.SubsectorInterpTo_urb_ind_USA:
    # Subsector shareweight interpolation of urban & industrial processes sector for all U.S. states
    if(any(is.na(A31.subsector_interp$to.value))) {
      L231.SubsectorInterp_urb_ind_USA <- A31.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_states(LEVEL2_DATA_NAMES[["SubsectorInterp"]])
    }
    if(any(!is.na(A31.subsector_interp$to.value))) {
      L231.SubsectorInterpTo_urb_ind_USA <- A31.subsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_states(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]])
    }

    # Technology information
    # L231.StubTech_urb_ind_USA:
    # Identification of stub technologies of urban & industrial processes sectors for all U.S. States
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    L231.StubTech_urb_ind_USA <- L231.StubTech_urb_ind %>%
      select(-region) %>%
      distinct() %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["StubTech"]])

    # Calibration and region-specific data
    # (NOTE: these parameters are still being copied across all regions/states)
    # L231.RegionalTechCalValue_urb_ind_USA: calibrated input of urban & industrial processes technologies
    L231.RegionalTechCalValue_urb_ind_USA <- L231.GlobalTechCost_urb_ind %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(-minicam.non.energy.input, -input.cost) %>%
      # Assign values to all regions
      repeat_add_columns(tibble(region = states_subregions$state)) %>%
      mutate(minicam.energy.input = emissions.REG_TECH_CAL_VALUE_MINICAM_ENERGY_INPUT,
             calibrated.value = emissions.REG_TECH_CAL_VALUE) %>%
      select(region, sector.name, subsector.name, technology, year, minicam.energy.input, calibrated.value)

    # Resource Information
    # Interpolate to specified historical years, as necessary
    L231.rsrc_info_USA <- A31.rsrc_info %>%
      select(-year, -value) %>%
      distinct() %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      left_join(A31.rsrc_info, by = c("resource", "resource_type",
                                      "market", "output-unit", "price-unit", "capacity.factor", "year")) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      filter(year %in% MODEL_YEARS) %>%
      repeat_add_columns(tibble(region = states_subregions$state)) %>%
      mutate(market = replace(market, market == "regional", region[market == "regional"]))

    # L231.UnlimitRsrc_USA:
    # output unit, price unit, and market for unlimited resources for all U.S. states
    L231.UnlimitRsrc_USA <- L231.rsrc_info_USA %>%
      filter(resource_type == "unlimited-resource") %>%
      select(-year, - value) %>%
      distinct %>%
      select(region, unlimited.resource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market, capacity.factor)

    # L231.UnlimitRsrcPrice_USA:
    # prices for unlimited resources for all U.S. states
    L231.UnlimitRsrcPrice_USA <- L231.rsrc_info_USA %>%
      filter(resource_type == "unlimited-resource") %>%
      select(region, unlimited.resource = resource, year, price = value)

    # L231.IndCoef_USA:
    # coefficient on industrial processes as an input to the industry sector for all U.S. states
    # Coefficient = 0.008 / change in industry output from 1990 (0.008 is the sum of calvalue)
    # Now combine input energy info and join with efficiency values
    L231.IndCoef_Yb_USA <- bind_rows(L132.in_EJ_state_indchp_F %>%
                                mutate(sector = "other industrial energy use"),
                              L132.in_EJ_state_indnochp_F %>%
                                mutate(sector = "other industrial energy use"),
                              L132.in_EJ_state_indfeed_F %>%
                                mutate(sector = "other industrial feedstocks")) %>%
      group_by(state, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      left_join_keep_first_only(L231.Ind_globaltech_eff, by = c("sector", "fuel", "year")) %>%
      # Calculate service as energy * efficiency
      mutate(service = value * efficiency) %>%
      na.omit() %>%
      group_by(state, year) %>%
      summarise(ind_output = sum(service)) %>%
      ungroup() %>%
      mutate(ind_proc_input = emissions.IND_PROC_INPUT,
             coefficient = ind_proc_input / ind_output,
             supplysector = "other industry",
             subsector = "other industry",
             technology = "other industry",
             minicam.energy.input = emissions.IND_PROC_MINICAM_ENERGY_INPUT)

    # Interpolate coefficients to model years
    L231.IndCoef_USA <- L231.IndCoef_Yb_USA %>%
      select(-year, -coefficient, -ind_output, -ind_proc_input) %>%
      distinct %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join(L231.IndCoef_Yb_USA, by = c("state", "supplysector", "subsector", "technology", "minicam.energy.input", "year")) %>%
      rename(region = state) %>%
      group_by(region, technology) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 2)) %>%
      ungroup() %>%
      select(region, supplysector, subsector, technology, year, minicam.energy.input, coefficient)

    # ===================================================

    # Produce outputs
    L231.DeleteSupplysector_industry_USA %>%
      add_title("Delete industry supplysector for the USA region") %>%
      add_units("Unitless") %>%
      add_comments("Info from L231.IndCoef, filtered to USA region") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("L231.IndCoef") ->
      L231.DeleteSupplysector_industry_USA

    L231.DeleteSupplysector_urban_processes_USA %>%
      add_title("Delete urban processes supplysector for the USA region") %>%
      add_units("Unitless") %>%
      add_comments("Info from A31.sector") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("emissions/A31.sector") ->
    L231.DeleteSupplysector_urban_processes_USA

    L231.DeleteFinalDemand_urban_processes_USA %>%
      add_title("Delete urban processes final demand for the USA region") %>%
      add_units("Unitless") %>%
      add_comments("Info from L231.FinalDemand_urb, filtered to USA region") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("L231.FinalDemand_urb") ->
      L231.DeleteFinalDemand_urban_processes_USA

    L231.UnlimitRsrc_USA %>%
      add_title("Processing Resource Capacity Factors for all U.S. states") %>%
      add_units("Unitless") %>%
      add_comments("Data from A31.rsrc_info") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("emissions/A31.rsrc_info",
                     "gcam-usa/states_subregions") ->
      L231.UnlimitRsrc_USA

    L231.UnlimitRsrcPrice_USA %>%
      add_title("Processing Resource Prices for all U.S. states") %>%
      add_units("units") %>%
      add_comments("Interpolated data from A31.rsrc_info") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("emissions/A31.rsrc_info",
                     "gcam-usa/states_subregions") ->
      L231.UnlimitRsrcPrice_USA

    L231.FinalDemand_urb_USA %>%
      add_title("Urban Processes Final Energy Demand for all U.S. states") %>%
      add_units("units") %>%
      add_comments("Constants for base service, income elasticity, and aeei") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("L231.FinalDemand_urb") ->
      L231.FinalDemand_urb_USA

    L231.Supplysector_urb_ind_USA %>%
      add_title("Urban and Industrial Processes Supplysector Logit Info for all U.S. states") %>%
      add_units("Unitless") %>%
      add_comments("A31.sector written to all states") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("emissions/A31.sector") ->
      L231.Supplysector_urb_ind_USA

    L231.SubsectorLogit_urb_ind_USA %>%
      add_title("Urban and Industrial Processes Subsector Logit Info for all U.S. states") %>%
      add_units("Unitless") %>%
      add_comments("A31.subsector_logit written to all states") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("emissions/A31.subsector_logit") ->
      L231.SubsectorLogit_urb_ind_USA

    if(exists("L231.SubsectorShrwt_urb_ind_USA")) {
      L231.SubsectorShrwt_urb_ind_USA %>%
        add_title("Urban and Industrial Processes Subsector Shareweights for all U.S. states") %>%
        add_units("Unitless") %>%
        add_comments("A31.subsector_shrwt written to all states") %>%
        add_legacy_name("NA - new chunk") %>%
        add_precursors("emissions/A31.subsector_shrwt") ->
        L231.SubsectorShrwt_urb_ind_USA
    } else {
      missing_data() %>%
        add_legacy_name("L231.SubsectorShrwt_ind_USA") ->
        L231.SubsectorShrwt_urb_ind_USA
    }

    if(exists("L231.SubsectorShrwtFllt_urb_ind_USA")) {
      L231.SubsectorShrwtFllt_urb_ind_USA %>%
        add_title("Urban and Industrial Processes Subsector Shareweights for all U.S. states") %>%
        add_units("Unitless") %>%
        add_comments("A31.subsector_shrwt written to all states") %>%
        add_legacy_name("NA - new chunk") %>%
        add_precursors("emissions/A31.subsector_shrwt") ->
        L231.SubsectorShrwtFllt_urb_ind_USA
    } else {
      missing_data() %>%
        add_legacy_name("L231.SubsectorShrwtFllt_urb_ind_USA")->
        L231.SubsectorShrwtFllt_urb_ind_USA
    }

    if(exists("L231.SubsectorInterp_urb_ind_USA")) {
      L231.SubsectorInterp_urb_ind_USA %>%
        add_title("Urban and Industrial Processes Subsector Shareweight Interpolation for all U.S. states") %>%
        add_units("NA") %>%
        add_comments("A31.subsector_interp written to all states") %>%
        add_legacy_name("NA - new chunk") %>%
        add_precursors("emissions/A31.subsector_interp")  ->
        L231.SubsectorInterp_urb_ind_USA
    } else {
      missing_data() %>%
        add_legacy_name("L231.SubsectorInterp_urb_ind_USA") ->
        L231.SubsectorInterp_urb_ind_USA
    }

    if(exists("L231.SubsectorInterpTo_urb_ind_USA")) {
      L231.SubsectorInterpTo_urb_ind_USA %>%
        add_title("Urban and Industrial Processes Subsector Shareweight Interpolation for all U.S. states") %>%
        add_units("NA") %>%
        add_comments("A31.subsector_interp written to all states") %>%
        add_legacy_name("NA - new chunk") %>%
        add_precursors("emissions/A31.subsector_interp") ->
        L231.SubsectorInterpTo_urb_ind_USA
    } else {
      missing_data() %>%
        add_legacy_name("L231.SubsectorInterpTo_urb_ind_USA") ->
        L231.SubsectorInterpTo_urb_ind_USA
    }

    L231.StubTech_urb_ind_USA %>%
      add_title("Urban and Industrial Processes Stub Technology Map for all U.S. states") %>%
      add_units("NA") %>%
      add_comments("L231.StubTech_urb_ind written to all states") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("L231.StubTech_urb_ind") ->
      L231.StubTech_urb_ind_USA

    L231.RegionalTechCalValue_urb_ind_USA %>%
      add_title("Urban and Industrial Processes Global Technology Calibrated Values for all U.S. states") %>%
      add_units("units") %>%
      add_comments("A31.globaltech_cost interpolated to base years and written to all states") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("L231.GlobalTechCost_urb_ind",
                     "gcam-usa/states_subregions") ->
      L231.RegionalTechCalValue_urb_ind_USA

    L231.IndCoef_USA %>%
      add_title("Industrial Processes Input-Output Coefficients for all U.S. states") %>%
      add_units("units") %>%
      add_comments("Coefficients equal to constant input value divided by calculated output value based on state level industrial energy and feedstock use") %>%
      add_legacy_name("NA - new chunk") %>%
      add_precursors("L231.Ind_globaltech_eff",
                     "L132.in_EJ_state_indchp_F",
                     "L132.in_EJ_state_indnochp_F",
                     "L132.in_EJ_state_indfeed_F") ->
      L231.IndCoef_USA

    return_data(L231.DeleteSupplysector_industry_USA,
                L231.DeleteSupplysector_urban_processes_USA,
                L231.DeleteFinalDemand_urban_processes_USA,
                L231.UnlimitRsrc_USA,
                L231.UnlimitRsrcPrice_USA,
                L231.FinalDemand_urb_USA,
                L231.Supplysector_urb_ind_USA,
                L231.SubsectorLogit_urb_ind_USA,
                L231.SubsectorShrwt_urb_ind_USA,
                L231.SubsectorShrwtFllt_urb_ind_USA,
                L231.SubsectorInterp_urb_ind_USA,
                L231.SubsectorInterpTo_urb_ind_USA,
                L231.StubTech_urb_ind_USA,
                L231.RegionalTechCalValue_urb_ind_USA,
                L231.IndCoef_USA)
  } else {
    stop("Unknown command")
  }
}
