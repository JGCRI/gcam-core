# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2062.ag_Fert_irr_mgmt
#'
#' Specifies fertilizer coefficients for all technologies; adjusts nonLandVariableCost to remove fertilizer cost.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2062.AgCoef_Fert_ag_irr_mgmt}, \code{L2062.AgCoef_Fert_bio_irr_mgmt}, \code{L2062.AgCost_ag_irr_mgmt_adj}, \code{L2062.AgCost_bio_irr_mgmt_adj}. The corresponding file in the
#' original data system was \code{L2062.ag_Fert_irr_mgmt.R} (aglu level2).
#' @details This chunk maps the fertilizer coefficients calculated in LB142 to all agricultural technologies.
#' We assume coefficients (in kgN per kgCrop) are equal for all four technologies (irr v rfd; hi v lo).
#' Adjust nonLandVariableCost to remove the now explicitly computed fertilizer cost.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else left_join mutate select
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @author KVC June 2017
module_aglu_L2062.ag_Fert_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "common/GCAM_region_names",
              FILE = "water/basin_to_country_mapping",
              FILE = "aglu/A_Fodderbio_chars",
              "L142.ag_Fert_IO_R_C_Y_GLU",
              "L2052.AgCost_ag_irr_mgmt",
              "L2052.AgCost_bio_irr_mgmt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2062.AgCoef_Fert_ag_irr_mgmt",
             "L2062.AgCoef_Fert_bio_irr_mgmt",
             "L2062.AgCost_ag_irr_mgmt_adj",
             "L2062.AgCost_bio_irr_mgmt_adj"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- GCAM_region_ID <- GCAM_commodity <- GLU <- GLU_name <- IRR_RFD <-
      MGMT <- region <- AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <-
      minicam.energy.input <- coefficient <- WaterContent <- nonLandVariableCost <-
      FertCost <- GCAM_subsector <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_Fodderbio_chars <- get_data(all_data, "aglu/A_Fodderbio_chars")
    L142.ag_Fert_IO_R_C_Y_GLU <- get_data(all_data, "L142.ag_Fert_IO_R_C_Y_GLU", strip_attributes = TRUE)
    L2052.AgCost_ag_irr_mgmt <- get_data(all_data, "L2052.AgCost_ag_irr_mgmt", strip_attributes = TRUE)
    L2052.AgCost_bio_irr_mgmt <- get_data(all_data, "L2052.AgCost_bio_irr_mgmt", strip_attributes = TRUE)

    # Process Fertilizer Coefficients: Copy coefficients to all four technologies (irr/rfd + hi/lo)
    L142.ag_Fert_IO_R_C_Y_GLU %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[ c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%

      # Copy coefficients to all four technologies
      repeat_add_columns(tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      repeat_add_columns(tibble(MGMT = c("hi", "lo"))) %>%

      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_subsector, GLU_name, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = paste(paste(AgSupplySubsector, IRR_RFD, sep = aglu.IRR_DELIMITER),
                                            MGMT, sep = aglu.MGMT_DELIMITER)) %>%

      # Add name of minicam.energy.input
      mutate(minicam.energy.input = "N fertilizer") %>%
      rename(coefficient = value) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, minicam.energy.input, year, coefficient) ->
      L2062.AgCoef_Fert_ag_irr_mgmt

    # Copy final base year coefficients to all future years, bind with historic coefficients, then remove zeroes
    # Note: this assumes constant fertilizer coefficients in the future
    L2062.AgCoef_Fert_ag_irr_mgmt %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(L2062.AgCoef_Fert_ag_irr_mgmt) %>%
      filter(coefficient > 0) ->
      L2062.AgCoef_Fert_ag_irr_mgmt

    # Calculate fertilizer coefficients for grassy bioenergy crops
    A_Fodderbio_chars %>%
      filter(GCAM_commodity == "biomassGrass") %>%
      mutate(coefficient = (aglu.BIO_GRASS_FERT_IO_GNM2 * CONV_G_KG / aglu.BIO_GRASS_YIELD_KGCM2    # Convert from application per unit area to per unit carbon
                            * aglu.CCONTENT_CELLULOSE * (1 - WaterContent))                         # Convert from carbon to wet biomass
             / (aglu.BIO_ENERGY_CONTENT_GJT * CONV_KG_T)) ->                         # Convert from biomass to energy
      bio_grass_coef

    # Calculate fertilizer coefficients for tree bioenergy crops
    A_Fodderbio_chars %>%
      filter(GCAM_commodity == "biomassTree") %>%
      mutate(coefficient = (aglu.BIO_TREE_FERT_IO_GNM2 * CONV_G_KG / aglu.BIO_TREE_YIELD_KGCM2    # Convert from application per unit area to per unit carbon
                            * aglu.CCONTENT_CELLULOSE * (1 - WaterContent))                         # Convert from carbon to wet biomass
             / (aglu.BIO_ENERGY_CONTENT_GJT * CONV_KG_T)) ->                         # Convert from biomass to energy
      bio_tree_coef

    # Map fertilizer coefficients to all bioenergy technologies
    L2052.AgCost_bio_irr_mgmt %>%
      select(-nonLandVariableCost) %>%                  # We are just using this data.frame to get the region/sector/tech names
      mutate(minicam.energy.input = "N fertilizer",
             coefficient = if_else(grepl("^biomassGrass", AgSupplySubsector),
                                   bio_grass_coef$coefficient, bio_tree_coef$coefficient)) ->
      L2062.AgCoef_Fert_bio_irr_mgmt

    # Adjust nonLandVariableCost to separate fertilizer cost (which is accounted for specifically)
    L2052.AgCost_ag_irr_mgmt %>%
      # Note: using left_join because there are instances with cost but no fertilizer use.
      left_join(L2062.AgCoef_Fert_ag_irr_mgmt,
                by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%

      # Set fertilizer coefficient to zero when missing. This will lead to zero fertilizer cost.
      replace_na(list(coefficient = 0)) %>%

      # Calculate fertilizer cost using a fixed value (specified in constants.R in current $ per ton of NH3)
      # and the fertilizer coefficient calculated above. Subtract from original nonLandVariableCost.
      mutate(FertCost = coefficient * aglu.FERT_PRICE * gdp_deflator(1975, aglu.FERT_PRICE_YEAR) * CONV_KG_T / CONV_NH3_N,
             nonLandVariableCost = round(nonLandVariableCost - FertCost, aglu.DIGITS_CALPRICE)) %>%
      select(-minicam.energy.input, -coefficient, -FertCost) ->
      L2062.AgCost_ag_irr_mgmt_adj

    # Adjust nonLandVariableCost to separate fertilizer cost (which is accounted for specifically)
    L2052.AgCost_bio_irr_mgmt %>%
      # Note: using left_join because there are instances with cost but no fertilizer use
      left_join(L2062.AgCoef_Fert_bio_irr_mgmt,
                by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%

      # Set fertilizer coefficient to zero when missing. This will lead to zero fertilizer cost.
      replace_na(list(coefficient = 0)) %>%

      # Calculate fertilizer cost using a fixed value (specified in constants.R in 2007$ per ton of NH3)
      # and the fertilizer coefficient calculated above. Subtract from original nonLandVariableCost.
      mutate(FertCost = coefficient * aglu.FERT_PRICE * gdp_deflator(1975, 2007) * CONV_KG_T / CONV_NH3_N,
             nonLandVariableCost = round(nonLandVariableCost - FertCost, aglu.DIGITS_CALPRICE)) %>%
      select(-minicam.energy.input, -coefficient, -FertCost) ->
      L2062.AgCost_bio_irr_mgmt_adj

    # Produce outputs
    L2062.AgCoef_Fert_ag_irr_mgmt %>%
      add_title("Fertilizer coefficients for agricultural technologies") %>%
      add_units("kgN per kg crop") %>%
      add_comments("Map fertilizer coefficients in L142.ag_Fert_IO_R_C_Y_GLU to all technologies") %>%
      add_comments("Note: we are using the same coefficient for all four management technologies (irrigated, rainfed, hi and lo") %>%
      add_legacy_name("L2062.AgCoef_Fert_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L142.ag_Fert_IO_R_C_Y_GLU") ->
      L2062.AgCoef_Fert_ag_irr_mgmt
    L2062.AgCoef_Fert_bio_irr_mgmt %>%
      add_title("Fertilizer coefficients for bioenergy technologies") %>%
      add_units("kgN per GJ") %>%
      add_comments("Compute bioenergy fertilizer coefficients from read-in constants") %>%
      add_comments("Note: L2052.AgCost_bio_irr_mgmt is only used to identify all bioenergy technologies") %>%
      add_legacy_name("L2062.AgCoef_Fert_bio_irr_mgmt") %>%
      add_precursors("aglu/A_Fodderbio_chars",
                     "L2052.AgCost_bio_irr_mgmt") ->
      L2062.AgCoef_Fert_bio_irr_mgmt
    L2062.AgCost_ag_irr_mgmt_adj %>%
      add_title("Adjusted non-land variable cost for agricultural technologies") %>%
      add_units("1975$ per kg") %>%
      add_comments("Subtract cost of fertilizer from non-land variable cost.") %>%
      add_comments("Fertilizer costs is computed using a fixed NH3 cost and the fertilizer coefficient") %>%
      add_legacy_name("L2062.AgCost_ag_irr_mgmt_adj") %>%
      same_precursors_as(L2062.AgCoef_Fert_ag_irr_mgmt) %>%
      add_precursors("L2052.AgCost_ag_irr_mgmt") ->
      L2062.AgCost_ag_irr_mgmt_adj
    L2062.AgCost_bio_irr_mgmt_adj %>%
      add_title("Adjusted non-land variable cost for agricultural technologies") %>%
      add_units("1975$ per GJ") %>%
      add_comments("Subtract cost of fertilizer from non-land variable cost.") %>%
      add_comments("Fertilizer costs is computed using a fixed NH3 cost and the fertilizer coefficient") %>%
      add_legacy_name("L2062.AgCost_bio_irr_mgmt_adj") %>%
      same_precursors_as(L2062.AgCoef_Fert_bio_irr_mgmt) %>%
      add_precursors("L2052.AgCost_bio_irr_mgmt")  ->
      L2062.AgCost_bio_irr_mgmt_adj

    return_data(L2062.AgCoef_Fert_ag_irr_mgmt, L2062.AgCoef_Fert_bio_irr_mgmt, L2062.AgCost_ag_irr_mgmt_adj, L2062.AgCost_bio_irr_mgmt_adj)
  } else {
    stop("Unknown command")
  }
}
