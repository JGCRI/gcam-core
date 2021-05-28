# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L1211.nonco2_awb_R_S_T_Y_IRR
#'
#' Produce irrigated and rainfed production shares, and agricultural waste burning emissions by irrigated & rainfed production shares.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1211.nonco2_tg_R_awb_C_Y_GLU_IRR}, \code{L1211.ag_irrShare_R_C_Y_GLU_irr}. The corresponding file in the
#' original data system was \code{L1211.nonco2_awb_R_S_T_Y_IRR.R} (emissions level1).
#' @details First, we compute the irrigated and rainfed production shares within region, GLU, and crop.
#' Second, we sum the irrigated and rainfed production shares over the historical years to get the total by region, GLU, and crop.
#' Third, we divide the irrigated and rainfed production shares over the historical years to get the share of irrigated and rainfed production within region, GLU, and crop.
#' Finally, we multiply non-CO2 emissions within region, GLU, and crop by irrigated and rainfed production shares.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by intersect mutate select summarise
#' @importFrom data.table data.table
#' @importFrom tidyr replace_na
#' @author CDL April 2017
module_emissions_L1211.nonco2_awb_R_S_T_Y_IRR <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L161.ag_irrProd_Mt_R_C_Y_GLU",
             "L161.ag_rfdProd_Mt_R_C_Y_GLU",
             "L121.nonco2_tg_R_awb_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1211.nonco2_tg_R_awb_C_Y_GLU_IRR",
             "L1211.ag_irrShare_R_C_Y_GLU_irr"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- GCAM_commodity <- GLU <- Non.CO2 <-
      value.x <- value.y <- i.value <- GCAM_subsector <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrProd_Mt_R_C_Y_GLU", strip_attributes = TRUE)
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_rfdProd_Mt_R_C_Y_GLU")
    L121.nonco2_tg_R_awb_C_Y_GLU <- get_data(all_data, "L121.nonco2_tg_R_awb_C_Y_GLU")


    # ===================================================

    # Downscaling emissions to irrigated and rainfed technologies on the basis of production

    # Next 3 steps create L1211.ag_irrShare_R_C_Y_GLU_irr

    # First, compute irrigated and rainfed production shares, within region/GLU/crop
    # Adding column of IRR and RFD
    L161.ag_irrProd_Mt_R_C_Y_GLU %>%
      mutate(Irr_Rfd = "IRR") %>%
      bind_rows(mutate(L161.ag_rfdProd_Mt_R_C_Y_GLU, Irr_Rfd = "RFD")) ->
      L1211.ag_Prod_Mt_R_C_Y_GLU_irr

    # Second, aggregate to get the total by region/GLU/crop
    L1211.ag_Prod_Mt_R_C_Y_GLU_irr %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year) %>%
      summarise(value = sum(value)) ->
      L1211.ag_Prod_Mt_R_C_Y_GLU

    # Third, divide to get the share of irr/rfd within region/GLU/crop
    L1211.ag_Prod_Mt_R_C_Y_GLU_irr %>%
      # Need to filter for historical years to ensure the join will work, ie. there will be a 1 to 1 match
      # Note this step was NOT in the original data system
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(L1211.ag_Prod_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "year")) %>%
      # value.x is irr and rfd production shares, within each region/GLU/crop
      # value.y is now the total (rfd+irr) production
      mutate(value = value.x / value.y) %>%
      select(-value.x, -value.y) %>%
      replace_na(list(value = 0)) ->
      L1211.ag_irrShare_R_C_Y_GLU_irr

    # This section creates L1211.nonco2_tg_R_awb_C_Y_GLU_IRR

    # Multiply emissions by region/GLU/crop/nonCO2 by irr/rfd production shares
    # Non-CO2 emissions by R_C_GLU_irr = non-CO2 emissions by R_C_GLU * irrShare
    L121.nonco2_tg_R_awb_C_Y_GLU %>%
      ## Need to filter for historical years to ensure the join will work, ie. there will be a 1 to 1 match
      ## Note this step was NOT in the original data system
      filter(year %in% intersect(HISTORICAL_YEARS, emissions.EDGAR_YEARS)) %>%
      repeat_add_columns(tibble::tibble(Irr_Rfd = c("IRR", "RFD"))) %>%
      fast_left_join(L1211.ag_irrShare_R_C_Y_GLU_irr,
                     by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "year", "Irr_Rfd")) %>%
      rename(value.x = i.value, value.y = value) %>%
      mutate(value = value.x * value.y) %>%
      select(-value.x, -value.y) ->
      L1211.nonco2_tg_R_awb_C_Y_GLU_IRR

    # ===================================================

    # Produce outputs
    L1211.nonco2_tg_R_awb_C_Y_GLU_IRR%>%
      add_title("Ag waste burning emissions by GCAM region / commodity / GLU / irrigation level / historical year") %>%
      add_units("Tg") %>%
      add_comments("Multiply non-CO2 emissions by region/GLU/crop/non-CO2 by irr/rfd production shares") %>%
      add_legacy_name("L1211.nonco2_tg_R_awb_C_Y_GLU_IRR") %>%
      add_precursors("L121.nonco2_tg_R_awb_C_Y_GLU") ->
      L1211.nonco2_tg_R_awb_C_Y_GLU_IRR

    L1211.ag_irrShare_R_C_Y_GLU_irr %>%
      add_title("Irrigated and rainfed production shares by GCAM region / commodity / GLU / historical year") %>%
      add_units("Unitless") %>%
      add_comments("First, computing irrigated and rainfed production shares, within region/GLU/crop") %>%
      add_comments("Second, aggregate to get the total by region/GLU/crop") %>%
      add_comments("Third, divide to get the share of irr/rfd within region/GLU/crop") %>%
      add_legacy_name("L1211.ag_irrShare_R_C_Y_GLU_irr") %>%
      add_precursors("L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "L161.ag_rfdProd_Mt_R_C_Y_GLU") ->
      L1211.ag_irrShare_R_C_Y_GLU_irr

    return_data(L1211.nonco2_tg_R_awb_C_Y_GLU_IRR, L1211.ag_irrShare_R_C_Y_GLU_irr)
  } else {
    stop("Unknown command")
  }
}
