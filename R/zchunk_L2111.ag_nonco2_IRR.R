#' module_emissions_L2111.ag_nonco2_IRR
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2111.AWBEmissions}, \code{L2111.AGREmissions}, \code{L2111.AnEmissions}, \code{L2111.AnNH3Emissions}, \code{L2111.AGRBio}, \code{L2111.AWB_BCOC_EmissCoeff}, \code{L2111.nonghg_max_reduction}, \code{L2111.nonghg_steepness}. The corresponding file in the
#' original data system was \code{L2111.ag_nonco2_IRR.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH July 2017
#' @export
module_emissions_L2111.ag_nonco2_IRR <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "emissions/A_regions",
             "L1211.nonco2_tg_R_awb_C_Y_GLU_IRR",
             FILE = "temp-data-inject/L1221.ghg_tg_R_agr_C_Y_GLU_IRR",
             FILE = "temp-data-inject/L211.AnEmissions",
             FILE = "temp-data-inject/L211.AnNH3Emissions",
             FILE = "temp-data-inject/L211.AGRBio",
             FILE = "temp-data-inject/L211.AWB_BCOC_EmissCoeff",
             FILE = "temp-data-inject/L211.nonghg_max_reduction",
             FILE = "temp-data-inject/L211.nonghg_steepness" ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2111.AWBEmissions",
             "L2111.AGREmissions",
             "L2111.AnEmissions",
             "L2111.AnNH3Emissions",
             "L2111.AGRBio",
             "L2111.AWB_BCOC_EmissCoeff",
             "L2111.nonghg_max_reduction",
             "L2111.nonghg_steepness"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_regions <- get_data(all_data, "emissions/A_regions")
    L1211.nonco2_tg_R_awb_C_Y_GLU_IRR <- get_data(all_data, "L1211.nonco2_tg_R_awb_C_Y_GLU_IRR")
    L1221.ghg_tg_R_agr_C_Y_GLU_IRR <- get_data(all_data, "temp-data-inject/L1221.ghg_tg_R_agr_C_Y_GLU_IRR") %>%
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5))) %>%
      # Replace GLU code with GLU name
      left_join_error_no_match(basin_to_country_mapping %>% select(GLU = GLU_code, GLU_name),
                               by = "GLU") %>%
      select(-GLU) %>%
      rename(GLU = GLU_name)
    L211.AnEmissions <- get_data(all_data, "temp-data-inject/L211.AnEmissions")
    L211.AnNH3Emissions <- get_data(all_data, "temp-data-inject/L211.AnNH3Emissions")
    L211.AGRBio <- get_data(all_data, "temp-data-inject/L211.AGRBio")
    L211.AWB_BCOC_EmissCoeff <- get_data(all_data, "temp-data-inject/L211.AWB_BCOC_EmissCoeff")
    L211.nonghg_max_reduction <- get_data(all_data, "temp-data-inject/L211.nonghg_max_reduction")
    L211.nonghg_steepness <- get_data(all_data, "temp-data-inject/L211.nonghg_steepness")

      # ===================================================
    # L2111.AWBEmissions: AWB emissions in all regions
    # Interpolate and add region name
    # Note: interpolate takes ages, so I'm not using interpolate_and_melt
    L2111.AWBEmissions <- L1211.nonco2_tg_R_awb_C_Y_GLU_IRR %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(AgSupplySector = GCAM_commodity,
             input.emissions = value) %>%
      mutate(AgSupplySubsector = paste(AgSupplySector, GLU, sep = "_"),
             AgProductionTechnology = paste(AgSupplySubsector, Irr_Rfd, sep = "_"),
             input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) %>%
      left_join_error_no_match(A_regions %>% select(region, SO2_name), by = "region") %>%
      mutate(SO2_name = paste0(SO2_name, "_AWB"),
             Non.CO2 = if_else(Non.CO2 == "SO2_AWB", SO2_name, Non.CO2)) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year,
             Non.CO2, input.emissions)

    # L2111.AGREmissions: ag AGR emissions in all regions
    L2111.AGREmissions <- L1221.ghg_tg_R_agr_C_Y_GLU_IRR %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(AgSupplySector = GCAM_commodity,
             input.emissions = value) %>%
      mutate(AgSupplySubsector = paste(AgSupplySector, GLU, sep = "_"),
             AgProductionTechnology = paste(AgSupplySubsector, Irr_Rfd, sep = "_"),
             input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year,
             Non.CO2, input.emissions)

    # L2111.AnEmissions and L2111.AnNH3Emissions: Copy tables exactly (irr/rfd irrelevant)
    L2111.AnEmissions <- L211.AnEmissions
    L2111.AnNH3Emissions <- L211.AnNH3Emissions

    # L2111.AGRBio, L2111.AWB_BCOC_EmissCoeff, L2111.nonghg_max_reduction, L2111.nonghg_steepness: repeat by irr/rfd and copy
    Irr_Rfd <- tibble(Irr_Rfd = c("IRR", "RFD"))
    # L2111.AGRBio <- repeat_and_add_vector( L211.AGRBio, irr, c( "IRR", "RFD" ) )
    # L2111.AGRBio[[agtech]] <- paste( L2111.AGRBio[[agsubs]], L2111.AGRBio[[irr]], sep = irr_delimiter )
    # L2111.AGRBio[[irr]] <- NULL
    #
    # L2111.AWB_BCOC_EmissCoeff <- repeat_and_add_vector( L211.AWB_BCOC_EmissCoeff, irr, c( "IRR", "RFD" ) )
    # L2111.AWB_BCOC_EmissCoeff[[agtech]] <- paste( L2111.AWB_BCOC_EmissCoeff[[agsubs]], L2111.AWB_BCOC_EmissCoeff[[irr]], sep = irr_delimiter )
    # L2111.AWB_BCOC_EmissCoeff[[irr]] <- NULL
    #
    # L2111.nonghg_max_reduction <- repeat_and_add_vector( L211.nonghg_max_reduction, irr, c( "IRR", "RFD" ) )
    # L2111.nonghg_max_reduction[[agtech]] <- paste( L2111.nonghg_max_reduction[[agsubs]], L2111.nonghg_max_reduction[[irr]], sep = irr_delimiter )
    # L2111.nonghg_max_reduction[[irr]] <- NULL
    #
    # L2111.nonghg_steepness <- repeat_and_add_vector( L211.nonghg_steepness, irr, c( "IRR", "RFD" ) )
    # L2111.nonghg_steepness[[agtech]] <- paste( L2111.nonghg_steepness[[agsubs]], L2111.nonghg_steepness[[irr]], sep = irr_delimiter )
    # L2111.nonghg_steepness[[irr]] <- NULL
      # ===================================================

      # Produce outputs
      tibble() %>%
        add_title("descriptive title of data") %>%
        add_units("units") %>%
        add_comments("comments describing how data generated") %>%
        add_comments("can be multiple lines") %>%
        add_legacy_name("L2111.AWBEmissions") %>%
        add_precursors("L1211.nonco2_tg_R_awb_C_Y_GLU_IRR", "common/GCAM_region_names",
                       "water/basin_to_country_mapping", "emissions/A_regions") %>%
        # typical flags, but there are others--see `constants.R`
        add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
        L2111.AWBEmissions
      tibble() %>%
        add_title("descriptive title of data") %>%
        add_units("units") %>%
        add_comments("comments describing how data generated") %>%
        add_comments("can be multiple lines") %>%
        add_legacy_name("L2111.AGREmissions") %>%
        add_precursors("temp-data-inject/L1221.ghg_tg_R_agr_C_Y_GLU_IRR") %>%
        # typical flags, but there are others--see `constants.R`
        add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
        L2111.AGREmissions
      tibble() %>%
        add_title("descriptive title of data") %>%
        add_units("units") %>%
        add_comments("comments describing how data generated") %>%
        add_comments("can be multiple lines") %>%
        add_legacy_name("L2111.AnEmissions") %>%
        add_precursors("temp-data-inject/L211.AnEmissions") %>%
        # typical flags, but there are others--see `constants.R`
        add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
        L2111.AnEmissions
      tibble() %>%
        add_title("descriptive title of data") %>%
        add_units("units") %>%
        add_comments("comments describing how data generated") %>%
        add_comments("can be multiple lines") %>%
        add_legacy_name("L2111.AnNH3Emissions") %>%
        add_precursors("temp-data-inject/L211.AnNH3Emissions") %>%
        # typical flags, but there are others--see `constants.R`
        add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
        L2111.AnNH3Emissions
      tibble() %>%
        add_title("descriptive title of data") %>%
        add_units("units") %>%
        add_comments("comments describing how data generated") %>%
        add_comments("can be multiple lines") %>%
        add_legacy_name("L2111.AGRBio") %>%
        add_precursors("temp-data-inject/L211.AGRBio") %>%
        # typical flags, but there are others--see `constants.R`
        add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
        L2111.AGRBio
      tibble() %>%
        add_title("descriptive title of data") %>%
        add_units("units") %>%
        add_comments("comments describing how data generated") %>%
        add_comments("can be multiple lines") %>%
        add_legacy_name("L2111.AWB_BCOC_EmissCoeff") %>%
        add_precursors("temp-data-inject/L211.AWB_BCOC_EmissCoeff") %>%
        # typical flags, but there are others--see `constants.R`
        add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
        L2111.AWB_BCOC_EmissCoeff
      tibble() %>%
        add_title("descriptive title of data") %>%
        add_units("units") %>%
        add_comments("comments describing how data generated") %>%
        add_comments("can be multiple lines") %>%
        add_legacy_name("L2111.nonghg_max_reduction") %>%
        add_precursors("temp-data-inject/L211.nonghg_max_reduction") %>%
        # typical flags, but there are others--see `constants.R`
        add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
        L2111.nonghg_max_reduction
      tibble() %>%
        add_title("descriptive title of data") %>%
        add_units("units") %>%
        add_comments("comments describing how data generated") %>%
        add_comments("can be multiple lines") %>%
        add_legacy_name("L2111.nonghg_steepness") %>%
        add_precursors("temp-data-inject/L211.nonghg_steepness") %>%
        # typical flags, but there are others--see `constants.R`
        add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
        L2111.nonghg_steepness

    return_data(L2111.AWBEmissions, L2111.AGREmissions, L2111.AnEmissions, L2111.AnNH3Emissions, L2111.AGRBio, L2111.AWB_BCOC_EmissCoeff, L2111.nonghg_max_reduction, L2111.nonghg_steepness)
  } else {
    stop("Unknown command")
  }
}



