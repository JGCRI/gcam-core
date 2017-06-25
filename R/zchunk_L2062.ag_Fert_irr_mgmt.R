#' module_aglu_L2062.ag_Fert_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2062.AgCoef_Fert_ag_irr_mgmt}, \code{L2062.AgCoef_Fert_bio_irr_mgmt}, \code{L2062.AgCost_ag_irr_mgmt_adj}, \code{L2062.AgCost_bio_irr_mgmt_adj}. The corresponding file in the
#' original data system was \code{L2062.ag_Fert_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2062.ag_Fert_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "common/GCAM_region_names",
              FILE = "water/basin_to_country_mapping",
              FILE = "aglu/A_Fodderbio_chars",
              FILE = "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU",
              FILE = "temp-data-inject/L2052.AgCost_ag_irr_mgmt",
              FILE = "temp-data-inject/L2052.AgCost_bio_irr_mgmt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2062.AgCoef_Fert_ag_irr_mgmt",
             "L2062.AgCoef_Fert_bio_irr_mgmt",
             "L2062.AgCost_ag_irr_mgmt_adj",
             "L2062.AgCost_bio_irr_mgmt_adj"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_Fodderbio_chars <- get_data(all_data, "aglu/A_Fodderbio_chars")
    L142.ag_Fert_IO_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU")
    L2052.AgCost_ag_irr_mgmt <- get_data(all_data, "temp-data-inject/L2052.AgCost_ag_irr_mgmt")
    L2052.AgCost_bio_irr_mgmt <- get_data(all_data, "temp-data-inject/L2052.AgCost_bio_irr_mgmt")

    # TEMPORARY: tidying until chunk LB142 complete
    L142.ag_Fert_IO_R_C_Y_GLU %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L142.ag_Fert_IO_R_C_Y_GLU

    # Process Fertilizer Coefficients: Copy coefficients to all four technologies (irr/rfd + hi/lo)
    L142.ag_Fert_IO_R_C_Y_GLU %>%
      filter(year %in% BASE_YEARS) %>%
      left_join(GCAM_region_names, by="GCAM_region_ID") %>%
      left_join(basin_to_country_mapping[ c("GLU_code", "GLU_name")], by=c("GLU" = "GLU_code")) %>%
      repeat_add_columns(tibble::tibble(IRR_RFD = c("IRR", "RFD")) ) %>%
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo")) ) %>%
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep="_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, IRR_RFD, MGMT, sep="_")) %>%
      mutate(minicam.energy.input = "N fertilizer") %>%
      rename(coefficient = value) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, minicam.energy.input, year, coefficient) ->
      L2062.AgCoef_Fert_ag_irr_mgmt

    # Copy 2010 coefficients to all future years, bind with historic coefficients, then remove zeroes
    L2062.AgCoef_Fert_ag_irr_mgmt %>%
      filter(year == max(BASE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble::tibble(year = FUTURE_YEARS)) %>%
      bind_rows(L2062.AgCoef_Fert_ag_irr_mgmt) %>%
      filter(coefficient > 0) ->
      L2062.AgCoef_Fert_ag_irr_mgmt

    # Calculate fertilizer coefficients for bioenergy crops
    # bio_grass_Fert_IO_kgNGJ <- round(
    #   bio_grass_Fert_IO_gNm2 * conv_g_kg / bio_grass_Yield_kgCm2 *          #convert from application rate per unit area to per unit carbon
    #     Ccontent_cellulose * (1 - A_Fodderbio_chars$WaterContent[ A_Fodderbio_chars$GCAM_commodity == "biomass_grass" ] ) /    # convert from carbon to wet biomass
    #     ( bio_GJt * conv_kg_t ), #convert from biomass to energy
    #   digits_Fert_IO )
    # bio_tree_Fert_IO_kgNGJ <- round(
    #   bio_tree_Fert_IO_gNm2 * conv_g_kg / bio_tree_Yield_kgCm2 *          #convert from application rate per unit area to per unit carbon
    #     Ccontent_cellulose * (1 - A_Fodderbio_chars$WaterContent[ A_Fodderbio_chars$GCAM_commodity == "biomass_tree" ] ) /    # convert from carbon to wet biomass
    #     ( bio_GJt * conv_kg_t ), #convert from biomass to energy
    #   digits_Fert_IO )
    #

    # Produce outputs
    L2062.AgCoef_Fert_ag_irr_mgmt %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2062.AgCoef_Fert_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping", "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU") ->
      L2062.AgCoef_Fert_ag_irr_mgmt
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2062.AgCoef_Fert_bio_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping", "aglu/A_Fodderbio_chars",
                     "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU", "temp-data-inject/L205.AgCost_ag", "temp-data-inject/L205.AgCost_bio") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L2062.AgCoef_Fert_bio_irr_mgmt
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2062.AgCost_ag_irr_mgmt_adj") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping", "aglu/A_Fodderbio_chars",
                     "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU", "temp-data-inject/L205.AgCost_ag", "temp-data-inject/L205.AgCost_bio") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L2062.AgCost_ag_irr_mgmt_adj
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2062.AgCost_bio_irr_mgmt_adj") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping", "aglu/A_Fodderbio_chars",
                     "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU", "temp-data-inject/L205.AgCost_ag", "temp-data-inject/L205.AgCost_bio") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L2062.AgCost_bio_irr_mgmt_adj

    return_data(L2062.AgCoef_Fert_ag_irr_mgmt, L2062.AgCoef_Fert_bio_irr_mgmt, L2062.AgCost_ag_irr_mgmt_adj, L2062.AgCost_bio_irr_mgmt_adj)
  } else {
    stop("Unknown command")
  }
}
