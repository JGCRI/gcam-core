# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L211.ag_nonco2
#'
#' Processes agriculture, agricultural waste burning, and animal emissions, adding regions and sectors/technologies.
#' By region and ag sector/technology, writes out N2O emissions coefficients for biomass and BC/OC emissions coefficients for ag waste burning.
#' Writes out non-GHG maximum emissions coefficient reduction and steepness,
#' a shape parameter that reduces emissions coefficient as function of per-capita GDP, by region and ag sector/technology.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L211.AWBEmissions}, \code{L211.AGREmissions}, \code{L211.AnEmissions}, \code{L211.AnNH3Emissions}, \code{L211.AGRBio}, \code{L211.AWB_BCOC_EmissCoeff}, \code{L211.nonghg_max_reduction}, \code{L211.nonghg_steepness}. The corresponding file in the
#' original data system was \code{L211.ag_nonco2.R} (emissions level2).
#' @details Processes agriculture, agricultural waste burning, and animal emissions, adding regions and sectors/technologies.
#' By region and ag sector/technology, writes out N2O emissions coefficients for biomass and BC/OC emissions coefficients for ag waste burning.
#' Writes out non-GHG maximum emissions coefficient reduction and steepness,
#' a shape parameter that reduces emissions coefficient as function of per-capita GDP, by region and ag sector/technology.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter mutate select
#' @author RH July 2017
module_emissions_L211.ag_nonco2 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "emissions/A_regions",
             "L2052.AgCost_bio_irr_mgmt",
             "L113.ghg_tg_R_an_C_Sys_Fd_Yh",
             "L115.nh3_tg_R_an_C_Sys_Fd_Yh",
             "L121.nonco2_tg_R_awb_C_Y_GLU",
             "L122.ghg_tg_R_agr_C_Y_GLU",
             "L123.bcoc_tgmt_R_awb_2000",
             FILE = "emissions/A11.max_reduction",
             FILE = "emissions/A11.steepness"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L211.AWBEmissions",
             "L211.AGREmissions",
             "L211.AnEmissions",
             "L211.AnNH3Emissions",
             "L211.AGRBio",
             "L211.AWB_BCOC_EmissCoeff",
             "L211.nonghg_max_reduction",
             "L211.nonghg_steepness"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    year <- value <- GCAM_commodity <- AgSupplySector <- GLU <- AgSupplySubsector <-
      input.emissions <- region <- AgProductionTechnology <- Non.CO2 <- nonLandVariableCost <-
      bio_N2O_coef <- supplysector <- subsector <- stub.technology <- emfact <-
      emiss.coef <- max.reduction <- GCAM_subsector <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping", strip_attributes = TRUE)
    A_regions <- get_data(all_data, "emissions/A_regions")
    L2052.AgCost_bio_irr_mgmt <- get_data(all_data, "L2052.AgCost_bio_irr_mgmt", strip_attributes = TRUE)
    L113.ghg_tg_R_an_C_Sys_Fd_Yh <- get_data(all_data, "L113.ghg_tg_R_an_C_Sys_Fd_Yh", strip_attributes = TRUE)
    L115.nh3_tg_R_an_C_Sys_Fd_Yh <- get_data(all_data, "L115.nh3_tg_R_an_C_Sys_Fd_Yh", strip_attributes = TRUE)
    L121.nonco2_tg_R_awb_C_Y_GLU <- get_data(all_data, "L121.nonco2_tg_R_awb_C_Y_GLU", strip_attributes = TRUE) %>%
      replace_GLU(basin_to_country_mapping)
    L122.ghg_tg_R_agr_C_Y_GLU <- get_data(all_data, "L122.ghg_tg_R_agr_C_Y_GLU", strip_attributes = TRUE) %>%
      replace_GLU(basin_to_country_mapping)
    L123.bcoc_tgmt_R_awb_2000 <- get_data(all_data, "L123.bcoc_tgmt_R_awb_2000", strip_attributes = TRUE) %>%
      replace_GLU(basin_to_country_mapping)
    A11.max_reduction <- get_data(all_data, "emissions/A11.max_reduction", strip_attributes = TRUE)
    A11.steepness <- get_data(all_data, "emissions/A11.steepness", strip_attributes = TRUE)

    # ===================================================
    # L211.AWBEmissions: Agricultural Waste Burning emissions in all regions
    L211.AWBEmissions <- L121.nonco2_tg_R_awb_C_Y_GLU %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      # Add region, supplysector, subsector and tech names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(input.emissions = value,
             AgSupplySector = GCAM_commodity) %>%
      mutate(AgSupplySubsector = paste(GCAM_subsector, GLU, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = AgSupplySubsector,
             input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, Non.CO2, input.emissions) %>%
      # Rename SO2 to regional SO2
      rename_SO2(A_regions, is_awb = TRUE)

    # L211.AGREmissions: Agricultural emissions in all regions
    L211.AGREmissions <- L122.ghg_tg_R_agr_C_Y_GLU %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      # Add region, supplysector, subsector and tech names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(input.emissions = value,
             AgSupplySector = GCAM_commodity) %>%
      mutate(AgSupplySubsector = paste(GCAM_subsector, GLU, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = AgSupplySubsector,
             input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, Non.CO2, input.emissions)

    # L211.AGR: N2O emissions coefficients for biomass in all regions
    # Map in coefficients from assumption file
    L211.AGRBio <- L2052.AgCost_bio_irr_mgmt %>%
      filter(year == emissions.CTRL_BASE_YEAR) %>%
      select(-nonLandVariableCost) %>%
      mutate(AgProductionTechnology = AgSupplySubsector) %>%
      distinct %>%
      mutate(Non.CO2 = "N2O_AGR") %>%
      left_join_error_no_match(A_regions %>% select(region, bio_N2O_coef),
                               by = "region")

    # L211.AnEmissions: Animal emissions in all regions
    L211.AnEmissions <- L113.ghg_tg_R_an_C_Sys_Fd_Yh %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      rename(input.emissions = value) %>%
      # Add region
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, input.emissions) %>%
      filter(region != aglu.NO_AGLU_REGIONS)

    # L211.AnNH3Emissions: Animal NH3 emissions in all regions
    L211.AnNH3Emissions <- L115.nh3_tg_R_an_C_Sys_Fd_Yh %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      rename(input.emissions = value) %>%
      # Add region
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, input.emissions) %>%
      filter(region != aglu.NO_AGLU_REGIONS)

    # L211.AWB_BCOC_EmissCoeff: BC / OC AWB emissions coefficients in all regions
    # Add region name & replicate for all commodities & base years
    L211.AWB_BCOC_EmissCoeff <- L123.bcoc_tgmt_R_awb_2000 %>%
      # Add region, supplysector, subsector and tech names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(AgSupplySector = GCAM_commodity,
             emiss.coef = emfact) %>%
      mutate(AgSupplySubsector = paste(GCAM_subsector, GLU, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = AgSupplySubsector,
             emiss.coef = round(emiss.coef, emissions.DIGITS_EMISSIONS)) %>%
      # Repeat for model base years
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology,
             year, Non.CO2, emiss.coef)

    # L211.nonghg_max_reduction: maximum emissions coefficient reduction for ag technologies in all regions
    L211.nonghg_max_reduction <- bind_rows(L211.AWB_BCOC_EmissCoeff %>%
                                             select(-emiss.coef),
                                           L211.AWBEmissions %>%
                                             select(-input.emissions)) %>%
      filter(year == emissions.CTRL_BASE_YEAR) %>%
      mutate(ctrl.name = "GDP_control") %>%
      left_join_error_no_match(A11.max_reduction, by = "AgSupplySector")

    # L211.nonghg_steepness: steepness of reduction, as function of per-capita GDP, for agricultural technologies in all regions
    L211.nonghg_steepness <- L211.nonghg_max_reduction %>%
      select(-max.reduction) %>%
      left_join_error_no_match(A11.steepness, by = "AgSupplySector")
    # ===================================================

    # Produce outputs
    L211.AWBEmissions %>%
      add_title("Agricultural waste burning emissions by GCAM region, agricultural technology, and historical year") %>%
      add_units("Tg") %>%
      add_comments("Region and ag technology added to ag waste burning emissions") %>%
      add_legacy_name("L211.AWBEmissions") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping",
                     "emissions/A_regions", "L121.nonco2_tg_R_awb_C_Y_GLU") ->
      L211.AWBEmissions
    L211.AGREmissions %>%
      add_title("Agriculture emissions by GCAM region, agricultural technology, and historical year") %>%
      add_units("Tg") %>%
      add_comments("Region and ag technology added to ag emissions") %>%
      add_legacy_name("L211.AGREmissions") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping",
                     "L122.ghg_tg_R_agr_C_Y_GLU") ->
      L211.AGREmissions
    L211.AnEmissions %>%
      add_title("Animal GHG emissions by region, supplysector, subsector, stub.technology and historical year") %>%
      add_units("Tg") %>%
      add_comments("Filtered L113.ghg_tg_R_an_C_Sys_Fd_Yh by year, added region, and rounded value") %>%
      add_legacy_name("L211.AnEmissions") %>%
      add_precursors("common/GCAM_region_names", "L113.ghg_tg_R_an_C_Sys_Fd_Yh") ->
      L211.AnEmissions
    L211.AnNH3Emissions %>%
      add_title("Animal NH3 emissions by region, supplysector, subsector, stub.technology and historical year") %>%
      add_units("Tg") %>%
      add_comments("Filtered L115.nh3_tg_R_an_C_Sys_Fd_Yh by year, added region, and rounded value") %>%
      add_legacy_name("L211.AnNH3Emissions") %>%
      add_precursors("common/GCAM_region_names", "L115.nh3_tg_R_an_C_Sys_Fd_Yh") ->
      L211.AnNH3Emissions
    L211.AGRBio %>%
      add_title("Bio N2O Emissions Coefficients by region and technology") %>%
      add_units("kg N2O per GJ bioenergy") %>%
      add_comments("Assumption emissions coefficients applied by region") %>%
      add_legacy_name("L211.AGRBio") %>%
      add_precursors("emissions/A_regions",
                     "L2052.AgCost_bio_irr_mgmt") ->
      L211.AGRBio
    L211.AWB_BCOC_EmissCoeff %>%
      add_title("Emission factors for BC and OC emissions by region and agricultural technology") %>%
      add_units("kt/Mt") %>%
      add_comments("Added ag technology and region to L123.bcoc_tgmt_R_awb_2000") %>%
      add_legacy_name("L211.AWB_BCOC_EmissCoeff") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping",
                     "L123.bcoc_tgmt_R_awb_2000") ->
      L211.AWB_BCOC_EmissCoeff
    L211.nonghg_max_reduction %>%
      add_title("Non-GHG maximum emissions coefficient reduction") %>%
      add_units("Percent reduction from base-year emissions coefficient") %>%
      add_comments("Emissions reductions added by AgSupplySector") %>%
      add_legacy_name("L211.nonghg_max_reduction") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping",
                     "L123.bcoc_tgmt_R_awb_2000", "emissions/A11.max_reduction",
                     "emissions/A_regions", "L121.nonco2_tg_R_awb_C_Y_GLU") ->
      L211.nonghg_max_reduction
    L211.nonghg_steepness %>%
      add_title("Steepness of non-GHG emissions reduction") %>%
      add_units("Unitless") %>%
      add_comments("Steepness added by AgSupplySector") %>%
      add_legacy_name("L211.nonghg_steepness") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping",
                     "L123.bcoc_tgmt_R_awb_2000", "emissions/A11.max_reduction",
                     "emissions/A_regions", "L121.nonco2_tg_R_awb_C_Y_GLU",
                     "emissions/A11.steepness") ->
      L211.nonghg_steepness

    return_data(L211.AWBEmissions, L211.AGREmissions, L211.AnEmissions, L211.AnNH3Emissions, L211.AGRBio, L211.AWB_BCOC_EmissCoeff, L211.nonghg_max_reduction, L211.nonghg_steepness)
  } else {
    stop("Unknown command")
  }
}
