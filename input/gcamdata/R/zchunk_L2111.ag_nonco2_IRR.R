# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L2111.ag_nonco2_IRR
#'
#' Creates agricultural, agricultural waste burning and animal non-CO2 emissions.
#' Writes biomass N2O coefficients, ag waste burning BC/OC coefficients, non-GHG maximum coefficient reduction, and non-GHG steepness by agricultural technology.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2111.AWBEmissions}, \code{L2111.AGREmissions}, \code{L2111.AGRBio}, \code{L2111.AWB_BCOC_EmissCoeff}, \code{L2111.nonghg_max_reduction}, \code{L2111.nonghg_steepness}. The corresponding file in the
#' original data system was \code{L2111.ag_nonco2_IRR.R} (emissions level2).
#' @details Writes out agricultural, agricultural waste burning, and animal non-CO2 emissions. Writes biomass N2O coefficients,
#' ag waste burning BC/OC coefficients, non-GHG maximum coefficient reduction, and non-GHG steepness by agricultural technology.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @author RLH July 2017
module_emissions_L2111.ag_nonco2_IRR <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "emissions/A_regions",
             "L1211.nonco2_tg_R_awb_C_Y_GLU_IRR",
             "L1221.ghg_tg_R_agr_C_Y_GLU_IRR",
             "L211.AGRBio",
             "L211.AWB_BCOC_EmissCoeff",
             "L211.nonghg_max_reduction",
             "L211.nonghg_steepness"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2111.AWBEmissions",
             "L2111.AGREmissions",
             "L2111.AGRBio",
             "L2111.AWB_BCOC_EmissCoeff",
             "L2111.nonghg_max_reduction",
             "L2111.nonghg_steepness"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    SO2_name <- Non.CO2 <- AgProductionTechnology <- AgSupplySector <-
      AgSupplySubsector <- GCAM_commodity <- GLU <- GLU_code <- Non.CO2 <-
      input.emissions <- region <- year <- GLU_name <- value <- GCAM_subsector <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_regions <- get_data(all_data, "emissions/A_regions")
    L1211.nonco2_tg_R_awb_C_Y_GLU_IRR <- get_data(all_data, "L1211.nonco2_tg_R_awb_C_Y_GLU_IRR", strip_attributes = TRUE) %>%
      # Replace GLU code with GLU name
      replace_GLU(basin_to_country_mapping)
    L1221.ghg_tg_R_agr_C_Y_GLU_IRR <- get_data(all_data, "L1221.ghg_tg_R_agr_C_Y_GLU_IRR", strip_attributes = TRUE) %>%
      # Replace GLU code with GLU name
      replace_GLU(basin_to_country_mapping)
    L211.AGRBio <- get_data(all_data, "L211.AGRBio", strip_attributes = TRUE)
    L211.AWB_BCOC_EmissCoeff <- get_data(all_data, "L211.AWB_BCOC_EmissCoeff", strip_attributes = TRUE)
    L211.nonghg_max_reduction <- get_data(all_data, "L211.nonghg_max_reduction", strip_attributes = TRUE)
    L211.nonghg_steepness <- get_data(all_data, "L211.nonghg_steepness", strip_attributes = TRUE)

    # ===================================================
    # L2111.AWBEmissions: AWB emissions in all regions
    L2111.AWBEmissions <- L1211.nonco2_tg_R_awb_C_Y_GLU_IRR %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      # Add region names and Ag columns
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(AgSupplySector = GCAM_commodity,
             input.emissions = value) %>%
      mutate(AgSupplySubsector = paste(GCAM_subsector, GLU, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = paste(AgSupplySubsector, Irr_Rfd, sep = aglu.IRR_DELIMITER),
             input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) %>%
      # Rename SO2 emissions
      rename_SO2(A_regions, is_awb = TRUE) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year,
             Non.CO2, input.emissions)

    # L2111.AGREmissions: ag AGR emissions in all regions
    L2111.AGREmissions <- L1221.ghg_tg_R_agr_C_Y_GLU_IRR %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      # Add region names and Ag columns
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(AgSupplySector = GCAM_commodity,
             input.emissions = value) %>%
      mutate(AgSupplySubsector = paste(GCAM_subsector, GLU, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = paste(AgSupplySubsector, Irr_Rfd, sep = aglu.IRR_DELIMITER),
             input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year,
             Non.CO2, input.emissions)

    # L2111.AGRBio, L2111.AWB_BCOC_EmissCoeff, L2111.nonghg_max_reduction, L2111.nonghg_steepness: repeat by irr/rfd and copy
    Irr_Rfd <- tibble(Irr_Rfd = c("IRR", "RFD"))
    L2111.AGRBio <- repeat_add_columns(L211.AGRBio, Irr_Rfd) %>%
      mutate(AgProductionTechnology = paste(AgSupplySubsector, Irr_Rfd, sep = aglu.IRR_DELIMITER)) %>%
      select(-Irr_Rfd)

    L2111.AWB_BCOC_EmissCoeff <- repeat_add_columns(L211.AWB_BCOC_EmissCoeff, Irr_Rfd) %>%
      mutate(AgProductionTechnology = paste(AgSupplySubsector, Irr_Rfd, sep = aglu.IRR_DELIMITER)) %>%
      select(-Irr_Rfd)

    L2111.nonghg_max_reduction <- repeat_add_columns(L211.nonghg_max_reduction, Irr_Rfd) %>%
      mutate(AgProductionTechnology = paste(AgSupplySubsector, Irr_Rfd, sep = aglu.IRR_DELIMITER)) %>%
      select(-Irr_Rfd)

    L2111.nonghg_steepness <- repeat_add_columns(L211.nonghg_steepness, Irr_Rfd) %>%
      mutate(AgProductionTechnology = paste(AgSupplySubsector, Irr_Rfd, sep = aglu.IRR_DELIMITER)) %>%
      select(-Irr_Rfd)
    # ===================================================

    # Produce outputs
    L2111.AWBEmissions %>%
      add_title("Agricultural Waste Burning Emissions") %>%
      add_units("Tg") %>%
      add_comments("Add region and SO2 name to AWB emissions") %>%
      add_legacy_name("L2111.AWBEmissions") %>%
      add_precursors("L1211.nonco2_tg_R_awb_C_Y_GLU_IRR", "common/GCAM_region_names", "emissions/A_regions") ->
      L2111.AWBEmissions
    L2111.AGREmissions %>%
      add_title("Agricultural Emissions") %>%
      add_units("Tg") %>%
      add_comments("Added region to AGR emissions") %>%
      add_legacy_name("L2111.AGREmissions") %>%
      add_precursors("L1221.ghg_tg_R_agr_C_Y_GLU_IRR", "water/basin_to_country_mapping",
                     "common/GCAM_region_names") ->
      L2111.AGREmissions
    L2111.AGRBio %>%
      add_title("Bio N2O Coefficients by region and technology") %>%
      add_units("kg N2O per GJ bioenergy") %>%
      add_comments("L211.AGRBio repeated by IRR and RFD technologies") %>%
      add_legacy_name("L2111.AGRBio") %>%
      add_precursors("L211.AGRBio") ->
      L2111.AGRBio
    L2111.AWB_BCOC_EmissCoeff %>%
      add_title("Agricultural Waste Burning BC/OC Emissions Coefficients") %>%
      add_units("kt/Mt") %>%
      add_comments("L211.AWB_BCOC_EmissCoeff repeated by IRR and RFD technologies") %>%
      add_legacy_name("L2111.AWB_BCOC_EmissCoeff") %>%
      add_precursors("L211.AWB_BCOC_EmissCoeff") ->
      L2111.AWB_BCOC_EmissCoeff
    L2111.nonghg_max_reduction %>%
      add_title("Non-GHG maximum emissions coefficient reduction by agricultural technology") %>%
      add_units("Percent reduction from base-year emissions coefficient") %>%
      add_comments("L211.nonghg_max_reduction repeated by IRR and RFD technologies") %>%
      add_legacy_name("L2111.nonghg_max_reduction") %>%
      add_precursors("L211.nonghg_max_reduction") ->
      L2111.nonghg_max_reduction
    L2111.nonghg_steepness %>%
      add_title("Steepness of non-GHG emissions reduction for agricultural technologies") %>%
      add_units("Unitless") %>%
      add_comments("L211.nonghg_steepness repeated by IRR and RFD technologies") %>%
      add_legacy_name("L2111.nonghg_steepness") %>%
      add_precursors("L211.nonghg_steepness") ->
      L2111.nonghg_steepness

    return_data(L2111.AWBEmissions, L2111.AGREmissions, L2111.AGRBio, L2111.AWB_BCOC_EmissCoeff, L2111.nonghg_max_reduction, L2111.nonghg_steepness)
  } else {
    stop("Unknown command")
  }
}



