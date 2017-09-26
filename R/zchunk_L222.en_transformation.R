#' module_energy_L222.en_transformation
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L222.SectorLogitTables[[ curr_table ]]$data}, \code{L222.Supplysector_en}, \code{L222.SubsectorLogitTables[[ curr_table ]]$data}, \code{L222.SubsectorLogit_en}, \code{L222.SubsectorShrwt_en}, \code{L222.SubsectorShrwtFllt_en}, \code{L222.SubsectorInterp_en}, \code{L222.SubsectorInterpTo_en}, \code{L222.StubTech_en}, \code{L222.GlobalTechInterp_en}, \code{L222.GlobalTechCoef_en}, \code{L222.GlobalTechCost_en}, \code{L222.GlobalTechShrwt_en}, \code{L222.GlobalTechCapture_en}, \code{L222.GlobalTechShutdown_en}, \code{L222.GlobalTechSCurve_en}, \code{L222.GlobalTechLifetime_en}, \code{L222.GlobalTechProfitShutdown_en}, \code{L222.StubTechProd_gasproc}, \code{L222.StubTechProd_refining}, \code{L222.StubTechCoef_refining}, \code{L222.GlobalTechCost_low_en}. The corresponding file in the
#' original data system was \code{L222.en_transformation.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L222.en_transformation <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/mappings/fuel_energy_input",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "energy/A22.sector",
             FILE = "energy/A22.subsector_logit",
             FILE = "energy/A22.subsector_shrwt",
             FILE = "energy/A22.subsector_interp",
             FILE = "energy/A22.globaltech_coef",
             FILE = "energy/A22.globaltech_cost",
             # Note: Low indicates low tech. Costs are actually higher than core
             FILE = "energy/A22.globaltech_cost_low",
             FILE = "energy/A22.globaltech_shrwt",
             FILE = "energy/A22.globaltech_interp",
             FILE = "energy/A22.globaltech_co2capture",
             FILE = "energy/A22.globaltech_retirement",
             "L122.out_EJ_R_gasproc_F_Yh",
             "L122.out_EJ_R_refining_F_Yh",
             "L122.IO_R_oilrefining_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L222.Supplysector_en",
             "L222.SubsectorLogit_en",
             "L222.SubsectorShrwt_en",
             "L222.SubsectorShrwtFllt_en",
             "L222.SubsectorInterp_en",
             "L222.SubsectorInterpTo_en",
             "L222.StubTech_en",
             "L222.GlobalTechInterp_en",
             "L222.GlobalTechCoef_en",
             "L222.GlobalTechCost_en",
             "L222.GlobalTechShrwt_en",
             "L222.GlobalTechCapture_en",
             "L222.GlobalTechShutdown_en",
             "L222.GlobalTechSCurve_en",
             "L222.GlobalTechLifetime_en",
             "L222.GlobalTechProfitShutdown_en",
             "L222.StubTechProd_gasproc",
             "L222.StubTechProd_refining",
             "L222.StubTechCoef_refining",
             "L222.GlobalTechCost_low_en"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    fuel_energy_input <- get_data(all_data, "energy/mappings/fuel_energy_input")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A_regions <- get_data(all_data, "energy/A_regions")
    A22.sector <- get_data(all_data, "energy/A22.sector")
    A22.subsector_logit <- get_data(all_data, "energy/A22.subsector_logit")
    A22.subsector_shrwt <- get_data(all_data, "energy/A22.subsector_shrwt")
    A22.subsector_interp <- get_data(all_data, "energy/A22.subsector_interp")
    A22.globaltech_coef <- get_data(all_data, "energy/A22.globaltech_coef")
    A22.globaltech_cost <- get_data(all_data, "energy/A22.globaltech_cost")
    A22.globaltech_cost_low  <- get_data(all_data, "energy/A22.globaltech_cost_low")
    A22.globaltech_shrwt <- get_data(all_data, "energy/A22.globaltech_shrwt")
    A22.globaltech_interp <- get_data(all_data, "energy/A22.globaltech_interp")
    A22.globaltech_co2capture <- get_data(all_data, "energy/A22.globaltech_co2capture")
    A22.globaltech_retirement <- get_data(all_data, "energy/A22.globaltech_retirement")
    L122.out_EJ_R_gasproc_F_Yh <- get_data(all_data, "L122.out_EJ_R_gasproc_F_Yh")
    L122.out_EJ_R_refining_F_Yh <- get_data(all_data, "L122.out_EJ_R_refining_F_Yh")
    L122.IO_R_oilrefining_F_Yh <- get_data(all_data, "L122.IO_R_oilrefining_F_Yh")

    # ===================================================
    browser()

    # 2. Build tables for CSVs
    # 2a. Supplysector information
    # L222.Supplysector_en: Supply sector information for energy transformation sectors

    A22.sector %>%
      write_to_all_regions(c("region", "supplysector", "output.unit", "input.unit", "price.unit", "logit.year.fillout", "logit.exponent"), GCAM_region_names) ->
      L222.Supplysector_en

    # 2b. Subsector information
    # L222.SubsectorLogit_en: Subsector logit exponents of energy transformation sectors

    A22.subsector_logit %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "logit.year.fillout", "logit.exponent"), GCAM_region_names)
      L222.SubsectorLogit_en

    if (any(!is.na(A22.subsector_shrwt$year))){
      A22.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(c("region", "supplysector", "subsector", "year", "share.weight"), GCAM_region_names) ->
        L226.SubsectorShrwt_en
    }

    if(any(!is.na(A22.subsector_shrwt$year.fillout))){
      A22.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(c("region", "supplysector", "subsector", "year.fillout", "share.weight"), GCAM_region_names) ->
        L226.SubsectorShrwt_en
    }

    # L222.SubsectorInterp_en and L222.SubsectorInterpTo_en: Subsector shareweight interpolation of energy transformation sectors

    if(any(is.na(A22.subsector_interp$to.value))){
      A22.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(c("region", "supplysector", "subsector", "apply.to", "from.year", "to.year", "interpolation.function"), GCAM_region_names) ->
        L222.SubsectorInterp_en
    }

    if(any(!is.na(A22.subsector_interp$to.value))){
      A22.subsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(c("region", "supplysector", "subsector", "apply.to", "from.year", "to.year", "to.value", "interpolation.function"), GCAM_region_names) ->
        L222.SubsectorInterpTo_en
    }

    # 2c. Technology information
    # L222.StubTech_en: Identification of stub technologies of energy transformation

    A22.globaltech_shrwt %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "technology"), GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L222.StubTech_en





    # L222.GlobalTechInterp_en: Technology shareweight interpolation of energy transformation sectors
    A22.globaltech_interp %>%
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L222.GlobalTechInterp_en

    # L222.GlobalTechCoef_en: Energy inputs and coefficients of global technologies for energy transformation

    A22.globaltech_coef %>%
      gather(year, coefficient, -supplysector, -subsector, -technology, -minicam.energy.input) %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, BASE_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(as.numeric(year), coefficient)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT))->
      L222.GlobalTechEff_en

    # L222.GlobalTechCost_en: Costs of global technologies for energy transformation
    A22.globaltech_cost %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector, input.cost = value) %>%
      mutate(input.cost = round(input.cost, energy.DIGITS_COST)) ->
      L222.GlobalTechCost_en

    # L222.GlobalTechCost_low_en: Costs of global technologies for energy transformation -- low tech/high cost option
    A22.globaltech_cost_low %>%
      gather(year, input.cost, -supplysector, -subsector, -technology, -minicam.non.energy.input) %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, MODEL_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(as.numeric(year), input.cost)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(input.cost = round(input.cost, energy.DIGITS_COST)) ->
      L222.GlobalTechCost_low_en

    # L222.GlobalTechShrwt_en: Shareweights of global technologies for energy transformation
    A22.globaltech_shrwt %>%
      gather(year, share.weight, -supplysector, -subsector, -technology) %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(as.numeric(year), share.weight)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L222.GlobalTechShrwt_en

    # L222.GlobalTechCapture_en: CO2 capture fractions from global technologies for energy transformation
    # No need to consider historical periods here
    A22.globaltech_co2capture %>%
      gather(year, remove.fraction, -supplysector, -subsector, -technology) %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(as.numeric(year), remove.fraction)) %>%
      ungroup() %>%
      filter(year %in% FUTURE_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      # Rounds the fraction to two digits and adds the name of the carbon storage market
      mutate(remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION), storage.market = energy.CO2.STORAGE.MARKET) ->
      L222.GlobalTechCapture_en

    # Retirement information
    A22.globaltech_retirement %>%
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L222.globaltech_retirement_base

    # Copies base year retirment information into all future years and appends back onto itself
    L222.globaltech_retirement_base %>%
      filter(year == max(BASE_YEARS)) %>%
      repeat_add_columns(tibble("year" = as.character(FUTURE_YEARS))) %>%
      select(-year.x) %>%
      rename(year = year.y) %>%
      bind_rows(L222.globaltech_retirement_base) ->
      L222.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L222.globaltech_retirement for each of these functions and creates a separate level 2 file for each
    # All of these options have different headers, and all are allowed
    if (any(!is.na(L222.globaltech_retirement$shutdown.rate))){
      L222.globaltech_retirement %>%
        filter(!is.na(L222.globaltech_retirement$shutdown.rate)) %>%
        select(sector.name, subsector.name, technology, lifetime, shutdown.rate) ->
        L222.GlobalTechShutdown_en
    }

    if (any(!is.na(L222.globaltech_retirement$half.life))){
      L222.globaltech_retirement %>%
        filter(!is.na(L222.globaltech_retirement$half.life)) %>%
        select(sector.name, subsector.name, technology, lifetime, steepness, half.life) ->
        L222.GlobalTechShutdown_en
    }

    # L222.GlobalTechLifetime_en: Global tech lifetime
    if (any(is.na(L222.globaltech_retirement$shutdown.rate) & is.na(L222.globaltech_retirement$half.life))){
      L222.globaltech_retirement %>%
        filter(is.na(L222.globaltech_retirement$shutdown.rate) & is.na(L222.globaltech_retirement$half.life)) %>%
        select(sector.name, subsector.name, technology, lifetime) ->
        L222.GlobalTechLifetime_en
    }

    # L222.GlobalTechProfitShutdown_en: Global tech profit shutdown decider and parameters
    if (any(!is.na(L222.globaltech_retirement$median.shutdown.point))){
      L222.globaltech_retirement %>%
        filter(!is.na(L222.globaltech_retirement$median.shutdown.point)) %>%
        select(sector.name, subsector.name, technology, median.shutdown.point, profit.shutdown.steepness) ->
        L222.GlobalTechLifetime_en
    }

    #2d. Calibration and region-specific data
    #  generate base year calibrated outputs of gas processing by interpolating from historical values
    L122.out_EJ_R_gasproc_F_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, BASE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(as.numeric(year), value)) %>%
      ungroup() %>%
      filter(year %in% BASE_YEARS) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L222.out_EJ_R_gasproc_F_Yh_base

    # append matching calibrated technology sector/subsector/technology to calibrated outputs of gas processing
    calibrated_techs %>%
      filter(paste(sector, fuel) %in% paste(L222.out_EJ_R_gasproc_F_Yh_base$sector, L222.out_EJ_R_gasproc_F_Yh_base$fuel)) %>%
      select(sector, fuel, supplysector, subsector, technology) %>%
      left_join(L222.out_EJ_R_gasproc_F_Yh_base, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L222.out_EJ_R_gasproc_F_Yh

    # L222.StubTechProd_gasproc: calibrated output of gas processing technologies -- writes to all regions, adds calibrated tech info
    A22.globaltech_coef %>%
      filter(supplysector == "gas processing") %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "technology", "minicam.energy.input"), GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      repeat_add_columns(tibble("year" = BASE_YEARS)) %>%
      left_join_error_no_match(L222.out_EJ_R_gasproc_F_Yh, by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      # rounds outputs and adds year column for shareweights
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT), year.share.weight = year) %>%
      select(-sector, -GCAM_region_ID, -fuel, -value, -minicam.energy.input) %>%
      # sets shareweight to 1 if output exists, otherwise 0
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0)) ->
      L222.StubTechProd_gasproc

    # THE FOLLOWING IS IN THE ORIGINAL BUT ALREADY OMITS THOSE REGIONS WITH NO COAL GAS, SO THERE ARE NO MISSING VALUES, JUST MISSING ROWS
    # IS THIS A MISTAKE?
    # "#Coal to gas isn't included in all regions so replace missing values
    # L222.StubTechProd_gasproc$calOutputValue[ is.na( L222.StubTechProd_gasproc$calOutputValue ) ] <- 0"

    ##### L222.StubTechProd_gasproc <- set_subsector_shrwt( L222.StubTechProd_gasproc )

    # Oil refining calibrated output by technology
    # interpolates values of IO coefficients for base years from historical values
    L122.out_EJ_R_refining_F_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, BASE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(as.numeric(year), value)) %>%
      ungroup() %>%
      filter(year %in% BASE_YEARS) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L222.out_EJ_R_refining_F_Yh

    # L222.StubTechProd_refining: calibrated output of refining technologies
    # matches calibrated tech info (sector, subsector, stub.technology) to refining outputs for base years and adds to output file
    calibrated_techs %>%
      filter(paste(sector, fuel) %in% paste(L222.out_EJ_R_refining_F_Yh$sector, L222.out_EJ_R_refining_F_Yh$fuel)) %>%
      select(sector, fuel, supplysector, subsector, technology) %>%
      left_join(L222.out_EJ_R_refining_F_Yh, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) %>%
      # rounds and renames outputs and adds year column for shareweights
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT), year.share.weight = year) %>%
      select(-sector, -GCAM_region_ID, -fuel, -value) %>%
      # sets shareweight to 1 if output exists, otherwise 0
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0)) ->
      L222.StubTechProd_refining

    ##### L222.StubTechProd_refining <- set_subsector_shrwt( L222.StubTechProd_refining )

    # L222.StubTechCoef_refining: calibrated input-output coefficients of oil refining by region and input
    # interpolates values of IO coefficients for base years from historical values
    L122.IO_R_oilrefining_F_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, BASE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(as.numeric(year), value)) %>%
      ungroup() %>%
      filter(year %in% BASE_YEARS) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L222.IO_R_oilrefining_F_Yh

    # matches calibrated tech info (sector, subsector, stub.technology) to input-output coefficients for base years
    calibrated_techs %>%
      filter(paste(sector, fuel) %in% paste(L222.IO_R_oilrefining_F_Yh$sector, L222.IO_R_oilrefining_F_Yh$fuel)) %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      left_join(L222.IO_R_oilrefining_F_Yh, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology)  %>%
      # rounds and renames outputs and adds market name
      mutate(coefficient = round(value, energy.DIGITS_COEFFICIENT), market.name = region) %>%
      select(-sector, -GCAM_region_ID, -fuel, -value) ->
      L222.StubTechProd_refining

    # ====================OLD=============

      printlog( "L222.StubTech_en: Identification of stub technologies of energy transformation" )
      #Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
      L222.StubTech_en <- write_to_all_regions( A22.globaltech_shrwt, names_Tech )
      names( L222.StubTech_en ) <- names_StubTech

      #Drop region x technology combinations that are not applicable
      firstgenbio_techs <- c( "corn ethanol", "sugarbeet ethanol", "sugar cane ethanol", "biodiesel" )
      L222.StubTech_en <- subset( L222.StubTech_en, stub.technology %!in% firstgenbio_techs |
                                    paste( region, stub.technology ) %in%
                                    c( paste( A_regions$region, A_regions$ethanol ),
                                       paste( A_regions$region, A_regions$biodiesel ) ) )

    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.Supplysector_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.Supplysector_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.SubsectorLogit_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.SubsectorLogit_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.SubsectorShrwt_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.SubsectorShrwt_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.SubsectorShrwtFllt_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.SubsectorShrwtFllt_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.SubsectorInterp_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.SubsectorInterp_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.SubsectorInterpTo_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.SubsectorInterpTo_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.StubTech_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.StubTech_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.GlobalTechInterp_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.GlobalTechInterp_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.GlobalTechCoef_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.GlobalTechCoef_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.GlobalTechCost_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.GlobalTechCost_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.GlobalTechShrwt_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.GlobalTechShrwt_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.GlobalTechCapture_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.GlobalTechCapture_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.GlobalTechShutdown_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.GlobalTechShutdown_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.GlobalTechSCurve_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.GlobalTechSCurve_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.GlobalTechLifetime_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.GlobalTechLifetime_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.GlobalTechProfitShutdown_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.GlobalTechProfitShutdown_en

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.StubTechProd_gasproc") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.StubTechProd_gasproc

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.StubTechProd_refining") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.StubTechProd_refining

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.StubTechCoef_refining") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.StubTechCoef_refining

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.GlobalTechCost_low_en") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.GlobalTechCost_low_en

    return_data(L222.Supplysector_en, L222.SubsectorLogit_en, L222.SubsectorShrwt_en,
                L222.SubsectorShrwtFllt_en, L222.SubsectorInterp_en, L222.SubsectorInterpTo_en,
                L222.StubTech_en, L222.GlobalTechInterp_en, L222.GlobalTechCoef_en, L222.GlobalTechCost_en,
                L222.GlobalTechShrwt_en, L222.GlobalTechCapture_en, L222.GlobalTechShutdown_en,
                L222.GlobalTechSCurve_en, L222.GlobalTechLifetime_en, L222.GlobalTechProfitShutdown_en,
                L222.StubTechProd_gasproc, L222.StubTechProd_refining, L222.StubTechCoef_refining,
                L222.GlobalTechCost_low_en)
  } else {
    stop("Unknown command")
  }
}
