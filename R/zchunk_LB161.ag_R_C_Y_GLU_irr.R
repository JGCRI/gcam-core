#' module_aglu_LB161.ag_R_C_Y_GLU_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.ag_irrProd_Mt_R_C_Y_GLU}, \code{L161.ag_rfdProd_Mt_R_C_Y_GLU}, \code{L161.ag_irrHA_bm2_R_C_Y_GLU}, \code{L161.ag_rfdHA_bm2_R_C_Y_GLU}, \code{L161.ag_irrYield_kgm2_R_C_Y_GLU}, \code{L161.ag_rfdYield_kgm2_R_C_Y_GLU}, \code{L161.ag_irrHA_frac_R_C_GLU}. The corresponding file in the
#' original data system was \code{LB161.ag_R_C_Y_GLU_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB161.ag_R_C_Y_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L103.ag_Prod_Mt_R_C_Y_GLU",
             "L103.ag_HA_bm2_R_C_Y_GLU",
             "L152.ag_irrProd_Mt_R_C_GLU",
             "L152.ag_irrHA_bm2_R_C_GLU",
             "L152.ag_rfdProd_Mt_R_C_GLU",
             "L152.ag_rfdHA_bm2_R_C_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L161.ag_irrProd_Mt_R_C_Y_GLU",
             "L161.ag_rfdProd_Mt_R_C_Y_GLU",
             "L161.ag_irrHA_bm2_R_C_Y_GLU",
             "L161.ag_rfdHA_bm2_R_C_Y_GLU",
             "L161.ag_irrYield_kgm2_R_C_Y_GLU",
             "L161.ag_rfdYield_kgm2_R_C_Y_GLU",
             "L161.ag_irrHA_frac_R_C_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L103.ag_HA_bm2_R_C_Y_GLU <- get_data(all_data, "L103.ag_HA_bm2_R_C_Y_GLU")
    L152.ag_irrProd_Mt_R_C_GLU <- get_data(all_data, "L152.ag_irrProd_Mt_R_C_GLU")
    L152.ag_irrHA_bm2_R_C_GLU <- get_data(all_data, "L152.ag_irrHA_bm2_R_C_GLU")
    L152.ag_rfdProd_Mt_R_C_GLU <- get_data(all_data, "L152.ag_rfdProd_Mt_R_C_GLU")
    L152.ag_rfdHA_bm2_R_C_GLU <- get_data(all_data, "L152.ag_rfdHA_bm2_R_C_GLU")

    # Compute fraction of production that is irrigated for each GCAM region / commodity / GLU
    L152.ag_irrProd_Mt_R_C_GLU %>%
      left_join_error_no_match(L152.ag_rfdProd_Mt_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      mutate(irrProd_frac = irrProd / (irrProd + rfdProd)) %>%
      # Repeat fraction table by number of historical years, and multiply fraction table by total annual production
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      right_join(L103.ag_Prod_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
      # Calculate irrigated (rainfed) production by multiplying total by fraction irrigated (rainfed)
      mutate(irrProd = value * irrProd_frac, rfdProd = value * (1 - irrProd_frac)) %>%
      select(-irrProd_frac, -value) ->
      L161.ag_Prod_Mt_R_C_Y_GLU

    # Compute fraction of harvested area that is irrigated for each GCAM region / commodity / GLU
    L152.ag_irrHA_bm2_R_C_GLU %>%
      left_join_error_no_match(L152.ag_rfdHA_bm2_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      mutate(irrHA_frac = irrHA / (irrHA + rfdHA)) ->
      L161.ag_irrHA_frac_R_C_GLU

    L161.ag_irrHA_frac_R_C_GLU %>%
      # Repeat fraction table by number of historical years, and multiply fraction table by total annual production
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      right_join(L103.ag_HA_bm2_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
      # Calculate irrigated (rainfed) production by multiplying total by fraction irrigated (rainfed)
      mutate(irrHA = value * irrHA_frac, rfdHA = value * (1 - irrHA_frac)) %>%
      select(-irrHA_frac, -value) ->
      L161.ag_HA_bm2_R_C_Y_GLU

    # Calculate yield in kilograms per square meter by region / crop / year / GLU
    L161.ag_Prod_Mt_R_C_Y_GLU %>%
      left_join_error_no_match(L161.ag_HA_bm2_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
      mutate(irrYield = irrProd / irrHA, rfdYield = rfdProd / rfdHA) %>%
      replace_na(list(irrYield = 0, rfdYield = 0)) %>%
      select(-irrProd, -rfdProd, -irrHA, -rfdHA) ->
      L161.ag_Yield_kgm2_R_C_Y_GLU

    # Produce outputs
    L161.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value = irrProd) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      add_precursors("L103.ag_Prod_Mt_R_C_Y_GLU",
                     "L152.ag_irrProd_Mt_R_C_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_irrProd_Mt_R_C_Y_GLU

    L161.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value = rfdProd) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      add_precursors("L103.ag_Prod_Mt_R_C_Y_GLU",
                     "L152.ag_rfdProd_Mt_R_C_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_rfdProd_Mt_R_C_Y_GLU

    L161.ag_HA_bm2_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value = irrHA) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_irrHA_bm2_R_C_Y_GLU") %>%
      add_precursors("L103.ag_HA_bm2_R_C_Y_GLU",
                     "L152.ag_irrHA_bm2_R_C_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_irrHA_bm2_R_C_Y_GLU

    L161.ag_HA_bm2_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value = rfdHA) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_rfdHA_bm2_R_C_Y_GLU") %>%
      add_precursors("L103.ag_HA_bm2_R_C_Y_GLU",
                     "L152.ag_rfdHA_bm2_R_C_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_rfdHA_bm2_R_C_Y_GLU

    L161.ag_Yield_kgm2_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value = irrYield) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_irrYield_kgm2_R_C_Y_GLU") %>%
      same_precursors_as("L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      same_precursors_as("L161.ag_irrHA_bm2_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_irrYield_kgm2_R_C_Y_GLU

    L161.ag_Yield_kgm2_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value = rfdYield) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_rfdYield_kgm2_R_C_Y_GLU") %>%
      same_precursors_as("L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      same_precursors_as("L161.ag_rfdHA_bm2_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_rfdYield_kgm2_R_C_Y_GLU

    L161.ag_irrHA_frac_R_C_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_irrHA_frac_R_C_GLU") %>%
      add_precursors("L152.ag_irrHA_bm2_R_C_GLU",
                     "L152.ag_rfdHA_bm2_R_C_GLU") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L161.ag_irrHA_frac_R_C_GLU

    return_data(L161.ag_irrProd_Mt_R_C_Y_GLU, L161.ag_rfdProd_Mt_R_C_Y_GLU, L161.ag_irrHA_bm2_R_C_Y_GLU, L161.ag_rfdHA_bm2_R_C_Y_GLU, L161.ag_irrYield_kgm2_R_C_Y_GLU, L161.ag_rfdYield_kgm2_R_C_Y_GLU, L161.ag_irrHA_frac_R_C_GLU)
  } else {
    stop("Unknown command")
  }
}
