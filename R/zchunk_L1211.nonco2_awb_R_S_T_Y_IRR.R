#' module_emissions_L1211.nonco2_awb_R_S_T_Y_IRR
#'
#' This chunk produces irrigated and rainfed production shares, and agricultural waste burning emissions by irrigated & rainfed production shares.
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
#' Finally, we multiply nonCO2 emissions within region, GLU, and crop by irrigated and rainfed production shares.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author CDL April 2017
module_emissions_L1211.nonco2_awb_R_S_T_Y_IRR <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L121.nonco2_tg_R_awb_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1211.nonco2_tg_R_awb_C_Y_GLU_IRR",
             "L1211.ag_irrShare_R_C_Y_GLU_irr"))
  } else if(command == driver.MAKE) {


    all_data <- list(...)[[1]]

    # Load required inputs
    # Temporary - these next two lines should be removed when 'real' data are available
    get_data(all_data, "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L161.ag_irrProd_Mt_R_C_Y_GLU
    # Temporary - these next two lines should be removed when 'real' data are available
    get_data(all_data, "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L161.ag_rfdProd_Mt_R_C_Y_GLU
    # Temporary - these next two lines should be removed when 'real' data are available
    get_data(all_data, "temp-data-inject/L121.nonco2_tg_R_awb_C_Y_GLU") %>%
      gather(year, value, -GCAM_region_ID, -Non.CO2, -GCAM_commodity, -GLU) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L121.nonco2_tg_R_awb_C_Y_GLU

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
      group_by(GCAM_region_ID, GCAM_commodity, GLU, year) %>%
      summarise(value = sum(value))  ->
      L1211.ag_Prod_Mt_R_C_Y_GLU

    # Third, divide to get the share of irr/rfd within region/GLU/crop
    L1211.ag_Prod_Mt_R_C_Y_GLU_irr %>%
      # Need to filter for historical years to ensure the join will work, ie. there will be a 1 to 1 match
      # Note this step was NOT in the original data system
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(L1211.ag_Prod_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity","GLU", "year")) %>%
      # value.y is now the total (rfd+irr) production
      mutate(value = value.x / value.y) %>%
      select(-value.x, -value.y) %>%
      mutate(value = if_else(is.na(value), 0, value)) ->
      L1211.ag_irrShare_R_C_Y_GLU_irr

    # This section creates L1211.nonco2_tg_R_awb_C_Y_GLU_IRR

    # Multiply emissions by region/GLU/crop/nonCO2 by irr/rfd production shares
    # NonCO2 emissions by R_C_GLU_irr = nonCO2 emissions by R_C_GLU * irrShare
    L121.nonco2_tg_R_awb_C_Y_GLU %>%
      repeat_add_columns(tibble::tibble(Irr_Rfd = c("IRR","RFD") )) %>%
      # Need to filter for historical years to ensure the join will work, ie. there will be a 1 to 1 match
      # Note this step was NOT in the original data system
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(L1211.ag_irrShare_R_C_Y_GLU_irr, by = c("GCAM_region_ID", "GCAM_commodity","GLU", "year", "Irr_Rfd")) %>%
      mutate(value = value.x * value.y) %>%
      select(-value.x, -value.y) %>%
      filter(year %in% EDGAR_YEARS) ->
      L1211.nonco2_tg_R_awb_C_Y_GLU_IRR

    # ===================================================

    # Produce outputs
    L1211.nonco2_tg_R_awb_C_Y_GLU_IRR%>%
      add_title("Ag waste burning emissions by GCAM region / commodity / GLU / irrigation level / historical year") %>%
      add_units("Teragram (Tg)") %>%
      add_comments("Multiply nonCO2 emissions by region/GLU/crop/nonCO2 by irr/rfd production shares") %>%
      add_legacy_name("L1211.nonco2_tg_R_awb_C_Y_GLU_IRR") %>%
      add_precursors("temp-data-inject/L121.nonco2_tg_R_awb_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1211.nonco2_tg_R_awb_C_Y_GLU_IRR
    L1211.ag_irrShare_R_C_Y_GLU_irr %>%
      add_title("Irrigated and rainfed production shares by GCAM region / commodity / GLU / historical year") %>%
      add_units("Unitless") %>%
      add_comments("First, computing irrigated and rainfed production shares, within region/GLU/crop") %>%
      add_comments("Second, aggregate to get the total by region/GLU/crop") %>%
      add_comments("Third, divide to get the share of irr/rfd within region/GLU/crop") %>%
      add_legacy_name("L1211.ag_irrShare_R_C_Y_GLU_irr") %>%
      add_precursors("temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1211.ag_irrShare_R_C_Y_GLU_irr

    return_data(L1211.nonco2_tg_R_awb_C_Y_GLU_IRR, L1211.ag_irrShare_R_C_Y_GLU_irr)
  } else {
    stop("Unknown command")
  }
}

