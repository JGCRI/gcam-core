# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L231.proc_sector
#'
#' Writes urban & industrial processing sector outputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L231.UnlimitRsrc}, \code{L231.UnlimitRsrcPrice}, \code{L231.FinalDemand_urb}, \code{L231.SectorLogitTables[[ curr_table ]]$data}, \code{L231.Supplysector_urb_ind}, \code{L231.SubsectorLogitTables[[ curr_table ]]$data}, \code{L231.SubsectorLogit_urb_ind}, \code{L231.SubsectorShrwt_urb_ind}, \code{L231.SubsectorShrwtFllt_urb_ind}, \code{L231.SubsectorInterp_urb_ind}, \code{L231.SubsectorInterpTo_urb_ind}, \code{L231.StubTech_urb_ind}, \code{L231.GlobalTechShrwt_urb_ind}, \code{L231.GlobalTechEff_urb_ind}, \code{L231.GlobalTechCoef_urb_ind}, \code{L231.GlobalTechCost_urb_ind}, \code{L231.RegionalTechCalValue_urb_ind}, \code{L231.IndCoef}. The corresponding file in the
#' original data system was \code{L231.proc_sector.R} (emissions level2).
#' @details For urban processes and industrial processes, produces global technology coefficients,
#' costs, efficiencies, and shareweights by interpolating assumption file data. For urb & ind. processing subsectors
#' and supplysectors, produces logit, shareweights, and interpolation files by writing assumption file data to all regions.
#' Writes out coefficients and prices for misc emissions sources from assumption file data.
#' Outputs urban processing demand using constants. StubTech mapping file created from assumption mappings.
#' Regional technology calibration values created with constant. Industry input-output coefficient file created.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter group_by left_join mutate select summarise
#' @author RLH July 2017
module_emissions_L231.proc_sector <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions",
             FILE = "emissions/A31.rsrc_info", # Units and source
             FILE = "emissions/A31.sector", # source
             FILE = "emissions/A31.subsector_logit", # source
             FILE = "emissions/A31.subsector_shrwt", # source
             FILE = "emissions/A31.subsector_interp", # source
             FILE = "emissions/A31.globaltech_shrwt", # source
             FILE = "emissions/A31.globaltech_eff", # Units and source
             FILE = "emissions/A31.globaltech_cost", # Units and source
             FILE = "emissions/A31.globaltech_coef", # Units and source
             FILE = "energy/A32.globaltech_eff", # Units and source
             "L1322.in_EJ_R_indfeed_F_Yh",
             "L1322.in_EJ_R_indenergy_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L231.UnlimitRsrc",
             "L231.UnlimitRsrcPrice",
             "L231.FinalDemand_urb",
             "L231.Supplysector_urb_ind",
             "L231.SubsectorLogit_urb_ind",
             "L231.SubsectorShrwt_urb_ind",
             "L231.SubsectorShrwtFllt_urb_ind",
             "L231.SubsectorInterp_urb_ind",
             "L231.SubsectorInterpTo_urb_ind",
             "L231.StubTech_urb_ind",
             "L231.GlobalTechShrwt_urb_ind",
             "L231.GlobalTechEff_urb_ind",
             "L231.GlobalTechCoef_urb_ind",
             "L231.GlobalTechCost_urb_ind",
             "L231.RegionalTechCalValue_urb_ind",
             "L231.IndCoef",
             "L231.Ind_globaltech_eff"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    year <- value <- share.weight <- efficiency <- input.cost <- coefficient <-
      year.fillout <- to.value <- supplysector <- subsector <- technology <- minicam.energy.input <-
      minicam.non.energy.input <- region <- sector.name <- subsector.name <- calibrated.value <-
      market <- resource_type <- resource <- `output-unit` <- `price-unit` <- capacity.factor <-
      secondary.output <- GCAM_region_ID <- service <- ind_proc_input <- ind_output <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "emissions/A_regions")
    A31.rsrc_info <- get_data(all_data, "emissions/A31.rsrc_info") %>%
      gather_years
    A31.sector <- get_data(all_data, "emissions/A31.sector", strip_attributes = TRUE)
    A31.subsector_logit <- get_data(all_data, "emissions/A31.subsector_logit", strip_attributes = TRUE)
    A31.subsector_shrwt <- get_data(all_data, "emissions/A31.subsector_shrwt", strip_attributes = TRUE)
    A31.subsector_interp <- get_data(all_data, "emissions/A31.subsector_interp", strip_attributes = TRUE)
    A31.globaltech_shrwt <- get_data(all_data, "emissions/A31.globaltech_shrwt") %>%
      gather_years(value_col = "share.weight")
    A31.globaltech_eff <- get_data(all_data, "emissions/A31.globaltech_eff") %>%
      gather_years(value_col = "efficiency")
    A31.globaltech_cost <- get_data(all_data, "emissions/A31.globaltech_cost") %>%
      gather_years(value_col = "input.cost")
    A31.globaltech_coef <- get_data(all_data, "emissions/A31.globaltech_coef") %>%
      gather_years(value_col = "coefficient")
    A32.globaltech_eff <- get_data(all_data, "energy/A32.globaltech_eff") %>%
      gather_years
    L1322.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indfeed_F_Yh")
    L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh")

    # ===================================================
    # L231.FinalDemand_urb: Final demand information for urban processes sector
    L231.FinalDemand_urb <- tibble(region = A_regions$region,
                                   energy.final.demand = "urban processes",
                                   perCapitaBased = emissions.URBAN_PROCESS_PERCAPITABASED,
                                   income.elasticity = emissions.URBAN_PROCESS_INCOME_ELASTICITY,
                                   base.service = emissions.URBAN_PROCESS_BASE_SERVICE,
                                   aeei = emissions.URBAN_PROCESS_AEEI # Autonomous Energy Efficiency Improvement
    ) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS))

    # L231.Supplysector_ind: Supply sector information for urban & industrial processes sectors
    L231.Supplysector_urb_ind <- A31.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names )

    # 2b. Subsector information
    # L231.SubsectorLogit_urb_ind: Subsector logit exponents of urban & industrial processes sectors
    L231.SubsectorLogit_urb_ind <- A31.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names)

    # L231.SubsectorShrwt_urb_ind and L231.SubsectorShrwtFllt_urb_ind: Subsector shareweights of urban & industrial processes sectors
    if(any(!is.na(A31.subsector_shrwt$year))) {
      L231.SubsectorShrwt_urb_ind <- A31.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwt"]], GCAM_region_names = GCAM_region_names)
    }
    if(any(!is.na(A31.subsector_shrwt$year.fillout))) {
      L231.SubsectorShrwtFllt_urb_ind <- A31.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names = GCAM_region_names)
    }

    # L231.SubsectorInterp_urb_ind and L231.SubsectorInterpTo_urb_ind: Subsector shareweight interpolation of urban & industrial processes sector
    if(any(is.na(A31.subsector_interp$to.value))) {
      L231.SubsectorInterp_urb_ind <- A31.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names = GCAM_region_names)
    }
    if(any(!is.na(A31.subsector_interp$to.value))) {
      L231.SubsectorInterpTo_urb_ind <- A31.subsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], GCAM_region_names = GCAM_region_names)
    }

    # Technology information
    # L231.StubTech_urb_ind: Identification of stub technologies of urban & industrial processes sectors
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    L231.StubTech_urb_ind <- A31.globaltech_shrwt %>%
      select(supplysector, subsector, technology) %>%
      distinct %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]] , GCAM_region_names = GCAM_region_names) %>%
      rename(stub.technology = technology)

    # L231.GlobalTechShrwt_urb_ind: Shareweights of global urban & industrial processes sector technologies
    L231.GlobalTechShrwt_urb_ind <- A31.globaltech_shrwt %>%
      select(supplysector, subsector, technology) %>%
      distinct %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A31.globaltech_shrwt, by = c("supplysector", "subsector", "technology", "year")) %>%
      mutate(share.weight = approx_fun(year, value = share.weight, rule = 1)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year, share.weight)

    # L231.GlobalTechEff_urb_ind: Energy inputs and coefficients of global urban & industrial processes technologies
    L231.GlobalTechEff_urb_ind <- A31.globaltech_eff %>%
      select(-year, -efficiency) %>%
      distinct %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A31.globaltech_eff, by = c("supplysector", "subsector", "technology", "year", "minicam.energy.input")) %>%
      mutate(efficiency = approx_fun(year, value = efficiency, rule = 1)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year, minicam.energy.input, efficiency)

    # Coefficients on global industry sector technologies (not energy-use or feedstocks)
    # L231.GlobalTechCoef_urb_ind: Energy inputs and coefficients of global urban & industrial processes technologies
    L231.GlobalTechCoef_urb_ind <- A31.globaltech_coef %>%
      select(-year, -coefficient) %>%
      distinct %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A31.globaltech_coef, by = c("supplysector", "subsector", "technology", "year", "minicam.energy.input")) %>%
      mutate(coefficient = approx_fun(year, value = coefficient, rule = 1)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year, minicam.energy.input, coefficient)

    # Costs of global technologies
    # L231.GlobalTechCost_urb_ind: Capital costs of global urban & industrial processes technologies
    L231.GlobalTechCost_urb_ind <- A31.globaltech_cost %>%
      select(-year, -input.cost) %>%
      distinct %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A31.globaltech_cost, by = c("supplysector", "subsector", "technology", "year", "minicam.non.energy.input")) %>%
      mutate(input.cost = approx_fun(year, value = input.cost, rule = 1)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year, minicam.non.energy.input, input.cost)

    # Calibration and region-specific data
    # L231.StubTechCalInput_calvalue: calibrated input of urban & industrial processes technologies
    L231.RegionalTechCalValue_urb_ind <- L231.GlobalTechCost_urb_ind %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(-minicam.non.energy.input, -input.cost) %>%
      # Assign values to all regions
      repeat_add_columns(tibble(region = A_regions$region)) %>%
      mutate(minicam.energy.input = "misc emissions sources",
             calibrated.value = emissions.INDURB_PROCESS_MISCEMISSIONS_CALVAL) %>%
      select(region, sector.name, subsector.name, technology, year, minicam.energy.input, calibrated.value)

    # Resource Information
    # Interpolate to specified historical years, as necessary
    L231.rsrc_info <- A31.rsrc_info %>%
      select(-year, -value) %>%
      distinct() %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      left_join(A31.rsrc_info, by = c("resource", "resource_type",
                                      "market", "output-unit", "price-unit", "capacity.factor", "year")) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      filter(year %in% MODEL_YEARS) %>%
      repeat_add_columns(GCAM_region_names) %>%
      mutate(market = replace(market, market == "regional", region[market == "regional"]))

    # L231.UnlimitRsrc: output unit, price unit, and market for unlimited resources
    L231.UnlimitRsrc <- L231.rsrc_info %>%
      filter(resource_type == "unlimited-resource") %>%
      select(-year, - value) %>%
      distinct %>%
      select(region, unlimited.resource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market, capacity.factor)

    # L231.UnlimitRsrcPrice: prices for unlimited resources
    L231.UnlimitRsrcPrice <- L231.rsrc_info %>%
      filter(resource_type == "unlimited-resource") %>%
      select(region, unlimited.resource = resource, year, price = value)

    # L231.IndCoef: coefficient on industrial processes as an input to the industry sector
    # Coefficient = sum of calibrated value / change in industry output from 1990
    # First, interpolate A32.globaltech_eff efficiency values to all years
    L231.Ind_globaltech_eff <- A32.globaltech_eff %>%
      select(-year, -value) %>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A32.globaltech_eff, by = c("supplysector", "subsector", "technology", "minicam.energy.input",
                                           "secondary.output", "year")) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input,
               secondary.output) %>%
      mutate(value = round(approx_fun(year, value, rule = 1), 3)) %>%
      ungroup %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector = supplysector, fuel = subsector, year, efficiency = value) %>%
      distinct()

    # Next, calculate the sum of the calibrated value
    sum.calvalue <- L231.RegionalTechCalValue_urb_ind %>%
      filter(sector.name == "industrial processes") %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      group_by(GCAM_region_ID,  year) %>%
      summarise(ind_proc_input = sum(calibrated.value)) %>%
      ungroup()

    # Now combine input energy info and join with efficiency values
    L231.IndCoef <- bind_rows(L1322.in_EJ_R_indenergy_F_Yh %>%
                                mutate(sector = "other industrial energy use"),
                              L1322.in_EJ_R_indfeed_F_Yh %>%
                                mutate(sector = "other industrial feedstocks")) %>%
      left_join_keep_first_only(L231.Ind_globaltech_eff, by = c("sector", "fuel", "year")) %>%
      # Calculate service as energy * efficiency
      mutate(service = value * efficiency) %>%
      na.omit() %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(ind_output = sum(service)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(sum.calvalue, by = c("GCAM_region_ID", "year")) %>%
      mutate(coefficient = ind_proc_input / ind_output,
             supplysector = "other industry",
             subsector = "other industry",
             technology = "other industry",

             minicam.energy.input = "industrial processes") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    # Interpolate coefficients to model years
    L231.IndCoef <- L231.IndCoef %>%
      select(-year, -coefficient, -ind_output, -ind_proc_input) %>%
      distinct %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join(L231.IndCoef, by = c("GCAM_region_ID", "region", "supplysector", "subsector", "technology", "minicam.energy.input", "year")) %>%
      group_by(region, technology) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 2)) %>%
      ungroup() %>%
      select(region, supplysector, subsector, technology, year, minicam.energy.input, coefficient)

    # ===================================================

    # Produce outputs
    L231.UnlimitRsrc %>%
      add_title("Processing Resource Capacity Factors") %>%
      add_units("Unitless") %>%
      add_comments("Data from A31.rsrc_info") %>%
      add_legacy_name("L231.UnlimitRsrc") %>%
      add_precursors("emissions/A31.rsrc_info") ->
      L231.UnlimitRsrc

    L231.UnlimitRsrcPrice %>%
      add_title("Processing Resource Prices") %>%
      add_units("units") %>%
      add_comments("Interpolated data from A31.rsrc_info") %>%
      add_legacy_name("L231.UnlimitRsrcPrice") %>%
      add_precursors("emissions/A31.rsrc_info") ->
      L231.UnlimitRsrcPrice

    L231.FinalDemand_urb %>%
      add_title("Urban Processes Final Energy Demand") %>%
      add_units("units") %>%
      add_comments("Constants for base service, income elasticity, and aeei") %>%
      add_legacy_name("L231.FinalDemand_urb") %>%
      add_precursors("emissions/A_regions") ->
      L231.FinalDemand_urb

    L231.Supplysector_urb_ind %>%
      add_title("Urban and Industrial Processes Supplysector Logit Info") %>%
      add_units("Unitless") %>%
      add_comments("A31.sector written to all regions") %>%
      add_legacy_name("L231.Supplysector_urb_ind") %>%
      add_precursors("emissions/A31.sector", "common/GCAM_region_names") ->
      L231.Supplysector_urb_ind

    L231.SubsectorLogit_urb_ind %>%
      add_title("Urban and Industrial Processes Subsector Logit Info") %>%
      add_units("Unitless") %>%
      add_comments("A31.subsector_logit written to all regions") %>%
      add_legacy_name("L231.SubsectorLogit_urb_ind") %>%
      add_precursors("emissions/A31.subsector_logit", "common/GCAM_region_names") ->
      L231.SubsectorLogit_urb_ind

    if(exists("L231.SubsectorShrwt_urb_ind")) {
      L231.SubsectorShrwt_urb_ind %>%
        add_title("Urban and Industrial Processes Subsector Shareweights") %>%
        add_units("Unitless") %>%
        add_comments("A31.subsector_shrwt written to all regions") %>%
        # This was a mistake in the old data system - the wrong name was assigned, but because it didn't exist, no error was thrown
        add_legacy_name("L231.SubsectorShrwt_ind") %>%
        add_precursors("emissions/A31.subsector_shrwt", "common/GCAM_region_names") ->
        L231.SubsectorShrwt_urb_ind
    } else {
      missing_data() %>%
        add_legacy_name("L231.SubsectorShrwt_ind") ->
        L231.SubsectorShrwt_urb_ind
    }

    if(exists("L231.SubsectorShrwtFllt_urb_ind")) {
      L231.SubsectorShrwtFllt_urb_ind %>%
        add_title("Urban and Industrial Processes Subsector Shareweights") %>%
        add_units("Unitless") %>%
        add_comments("A31.subsector_shrwt written to all regions") %>%
        add_legacy_name("L231.SubsectorShrwtFllt_urb_ind") %>%
        add_precursors("emissions/A31.subsector_shrwt", "common/GCAM_region_names") ->
        L231.SubsectorShrwtFllt_urb_ind
    } else {
      missing_data() %>%
        add_legacy_name("L231.SubsectorShrwtFllt_urb_ind")->
        L231.SubsectorShrwtFllt_urb_ind
    }

    if(exists("L231.SubsectorInterp_urb_ind")) {
      L231.SubsectorInterp_urb_ind %>%
        add_title("Urban and Industrial Processes Subsector Shareweight Interpolation") %>%
        add_units("NA") %>%
        add_comments("A31.subsector_interp written to all regions") %>%
        add_legacy_name("L231.SubsectorInterp_urb_ind") %>%
        add_precursors("emissions/A31.subsector_interp", "common/GCAM_region_names")  ->
        L231.SubsectorInterp_urb_ind
    } else {
      missing_data() %>%
        add_legacy_name("L231.SubsectorInterp_urb_ind") ->
        L231.SubsectorInterp_urb_ind
    }

    if(exists("L231.SubsectorInterpTo_urb_ind")) {
      L231.SubsectorInterpTo_urb_ind %>%
        add_title("Urban and Industrial Processes Subsector Shareweight Interpolation") %>%
        add_units("NA") %>%
        add_comments("A31.subsector_interp written to all regions") %>%
        add_legacy_name("L231.SubsectorInterpTo_urb_ind") %>%
        add_precursors("emissions/A31.subsector_interp", "common/GCAM_region_names") ->
        L231.SubsectorInterpTo_urb_ind
    } else {
      missing_data() %>%
        add_legacy_name("L231.SubsectorInterpTo_urb_ind") ->
        L231.SubsectorInterpTo_urb_ind
    }

    L231.StubTech_urb_ind %>%
      add_title("Urban and Industrial Processes Stub Technology Map") %>%
      add_units("NA") %>%
      add_comments("A31.globaltech_shrwt filtered and written to all regions") %>%
      add_legacy_name("L231.StubTech_urb_ind") %>%
      add_precursors("emissions/A31.globaltech_shrwt", "common/GCAM_region_names") ->
      L231.StubTech_urb_ind

    L231.GlobalTechShrwt_urb_ind %>%
      add_title("Urban and Industrial Processes Global Technology Shareweights") %>%
      add_units("Unitless") %>%
      add_comments("A31.globaltech_shrwt interpolated to model years") %>%
      add_legacy_name("L231.GlobalTechShrwt_urb_ind") %>%
      add_precursors("emissions/A31.globaltech_shrwt") ->
      L231.GlobalTechShrwt_urb_ind

    L231.GlobalTechEff_urb_ind %>%
      add_title("Urban and Industrial Processes Global Technology Efficiency") %>%
      add_units("Unitless") %>%
      add_comments("A31.globaltech_eff interpolated to model years") %>%
      add_legacy_name("L231.GlobalTechEff_urb_ind") %>%
      add_precursors("emissions/A31.globaltech_eff")->
      L231.GlobalTechEff_urb_ind

    L231.GlobalTechCoef_urb_ind %>%
      add_title("Urban and Industrial Processes Global Technology Coefficients") %>%
      add_units("units") %>%
      add_comments("A31.globaltech_coef interpolated to model years") %>%
      add_legacy_name("L231.GlobalTechCoef_urb_ind") %>%
      add_precursors("emissions/A31.globaltech_coef") ->
      L231.GlobalTechCoef_urb_ind

    L231.GlobalTechCost_urb_ind %>%
      add_title("Urban and Industrial Processes Global Technology Costs") %>%
      add_units("units") %>%
      add_comments("A31.globaltech_cost interpolated to model years") %>%
      add_legacy_name("L231.GlobalTechCost_urb_ind") %>%
      add_precursors("emissions/A31.globaltech_cost") ->
      L231.GlobalTechCost_urb_ind

    L231.RegionalTechCalValue_urb_ind %>%
      add_title("Urban and Industrial Processes Global Technology Calibrated Values") %>%
      add_units("units") %>%
      add_comments("A31.globaltech_cost interpolated to base years and written to all regions") %>%
      add_legacy_name("L231.RegionalTechCalValue_urb_ind") %>%
      add_precursors("emissions/A31.globaltech_cost", "emissions/A_regions") ->
      L231.RegionalTechCalValue_urb_ind

    L231.IndCoef %>%
      add_title("Industrial Processes Input-Output Coefficients") %>%
      add_units("units") %>%
      add_comments("Coefficients equal to constant input value divided by calculated output value") %>%
      add_legacy_name("L231.IndCoef") %>%
      add_precursors("energy/A32.globaltech_eff",
                     "L1322.in_EJ_R_indfeed_F_Yh",
                     "L1322.in_EJ_R_indenergy_F_Yh",
                     "common/GCAM_region_names") ->
      L231.IndCoef

    L231.Ind_globaltech_eff %>%
      add_title("Industrial efficiency values for all years") %>%
      add_units("NA") %>%
      add_comments("A32.globaltech_eff efficiency values interpolated to all years") %>%
      add_legacy_name("L231.IndCoef") %>%
      add_precursors("energy/A32.globaltech_eff") ->
      L231.Ind_globaltech_eff

    return_data(L231.UnlimitRsrc, L231.UnlimitRsrcPrice, L231.FinalDemand_urb, L231.Supplysector_urb_ind, L231.SubsectorLogit_urb_ind,
                L231.SubsectorShrwt_urb_ind, L231.SubsectorShrwtFllt_urb_ind, L231.SubsectorInterp_urb_ind, L231.SubsectorInterpTo_urb_ind,
                L231.StubTech_urb_ind, L231.GlobalTechShrwt_urb_ind, L231.GlobalTechEff_urb_ind, L231.GlobalTechCoef_urb_ind,
                L231.GlobalTechCost_urb_ind, L231.RegionalTechCalValue_urb_ind, L231.IndCoef, L231.Ind_globaltech_eff)
  } else {
    stop("Unknown command")
  }
}
