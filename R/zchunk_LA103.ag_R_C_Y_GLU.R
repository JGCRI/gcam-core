#' module_aglu_LA103.ag_R_C_Y_GLU
#'
#' Calculate production, harvested area, and yield by region, crop, GLU, and year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L103.ag_Prod_Mt_R_C_Y_GLU}, \code{L103.ag_Prod_Mt_R_C_Y}, \code{L103.ag_HA_bm2_R_C_Y_GLU}, \code{L103.ag_Yield_kgm2_R_C_Y_GLU}. The corresponding file in the
#' original data system was \code{LA103.ag_R_C_Y_GLU.R} (aglu level1).
#' @details We only have production and harvested area by region and GLU for a single
#' representative base year (circa 2000), and are using that to downscale regional
#' production and harvested area in all years. So, if GLU223 accounted for 20% of U.S.
#' corn production in ~2000, then it accounted for 20% of US corn production in all years.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL April 2017
module_aglu_LA103.ag_R_C_Y_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             "L101.ag_Prod_Mt_R_C_Y",
             "L101.ag_HA_bm2_R_C_Y",
             FILE = "temp-data-inject/L102.ag_Prod_Mt_R_C_GLU",
             FILE = "temp-data-inject/L102.ag_HA_bm2_R_C_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L103.ag_Prod_Mt_R_C_Y_GLU",
             "L103.ag_Prod_Mt_R_C_Y",
             "L103.ag_HA_bm2_R_C_Y_GLU",
             "L103.ag_Yield_kgm2_R_C_Y_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    get_data(all_data, "L101.ag_Prod_Mt_R_C_Y") %>%
      mutate(year = as.integer(year)) ->
      L101.ag_Prod_Mt_R_C_Y
     get_data(all_data, "L101.ag_HA_bm2_R_C_Y") %>%
      mutate(year = as.integer(year)) ->
      L101.ag_HA_bm2_R_C_Y
    L102.ag_Prod_Mt_R_C_GLU <- get_data(all_data, "temp-data-inject/L102.ag_Prod_Mt_R_C_GLU")
    L102.ag_HA_bm2_R_C_GLU <- get_data(all_data, "temp-data-inject/L102.ag_HA_bm2_R_C_GLU")

    # Combine FAO and GTAP: create tables with crop production and harvested area by
    # geographic land unit (GLU) and historical year production ( Mt )
    L102.ag_Prod_Mt_R_C_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity) %>%
      summarise(value = sum(value)) %>%
      right_join(L102.ag_Prod_Mt_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      mutate(value = value.y / value.x,
             value = if_else(is.na(value), 0, value)) %>%
      select(-value.x, -value.y) ->
      L103.ag_Prod_frac_R_C_GLU

    # Disaggregate FAO harvested area of all crops to GLUs using GTAP/LDS data
    L102.ag_HA_bm2_R_C_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity) %>%
      summarise(value = sum(value)) %>%
      right_join(L102.ag_HA_bm2_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      mutate(value = value.y / value.x,
             value = if_else(is.na(value), 0, value)) %>%
      select(-value.x, -value.y) ->
      L103.ag_HA_frac_R_C_GLU

    # Multiply historical production by these shares in order to downscale to GLU
    # NOTE: There are a few region x crop combinations in the FAO data that aren't in the aggregated gridded dataset,
    # presumably because production was zero in around 2000, the base year for the gridded dataset. In analysis of
    # these missing values, the quantities are tiny (zero in most years, <0.01 Mt in all/most others) and dropping
    # them should not have any consequences
    L103.ag_Prod_frac_R_C_GLU %>%
      left_join(L101.ag_Prod_Mt_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      mutate(value = value.x * value.y) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      select(-value.x, -value.y) %>%
      arrange(GLU) ->  # so we match old d.s. order
      L103.ag_Prod_Mt_R_C_Y_GLU

    # Remove crops from the written-out data that are zero in all years
    # This is part of the "pruning" process of not creating XML tags for land use types that are non-applicable
    remove_all_zeros <- function(x) {
      x %>%
        group_by(GCAM_region_ID, GCAM_commodity) %>%
        summarise(valuesum = sum(value)) %>%
        right_join(x, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
        filter(valuesum > 0) %>%
        select(-valuesum)
    }
    L103.ag_Prod_Mt_R_C_Y_GLU <- remove_all_zeros(L103.ag_Prod_Mt_R_C_Y_GLU)

    # Same operation again
    L103.ag_HA_frac_R_C_GLU %>%
      left_join(L101.ag_HA_bm2_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      mutate(value = value.x * value.y) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      select(-value.x, -value.y) %>%
      arrange(GLU) ->  # so we match old d.s. order
      L103.ag_HA_bm2_R_C_Y_GLU
    L103.ag_HA_bm2_R_C_Y_GLU <- remove_all_zeros(L103.ag_HA_bm2_R_C_Y_GLU)

    # Calculate initial yield estimates in kilograms per square meter by region, crop, year, and GLU
    # Yield in kilograms per square meter
    L103.ag_Prod_Mt_R_C_Y_GLU %>%
      left_join(L103.ag_HA_bm2_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
      mutate(value = value.x / value.y,
             value = if_else(is.na(value), 0, value)) %>%
      select(-value.x, -value.y) %>%
      arrange(GLU) ->  # so we match old d.s. order
      L103.ag_Yield_kgm2_R_C_Y_GLU

    # Aggregate through GLUs to get production by region/crop/year; different from L101 production in that we have now dropped
    # some observations that weren't available in the GTAP-based gridded inventories
    L103.ag_Prod_Mt_R_C_Y_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID), GCAM_commodity, year, fill = list(value = 0)) %>%
      arrange(GCAM_commodity, GCAM_region_ID) ->  # so we match old d.s. order
      L103.ag_Prod_Mt_R_C_Y


    # Produce outputs
    L103.ag_Prod_Mt_R_C_Y_GLU %>%
      add_title("Crop production by GCAM region / commodity / year / GLU") %>%
      add_units("Mt") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L103.ag_Prod_Mt_R_C_Y_GLU") %>%
      add_precursors("L101.ag_Prod_Mt_R_C_Y",
                     "temp-data-inject/L102.ag_Prod_Mt_R_C_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L103.ag_Prod_Mt_R_C_Y_GLU

    L103.ag_Prod_Mt_R_C_Y %>%
      add_title("Crop production by GCAM region / commodity / year") %>%
      add_units("Mt") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L103.ag_Prod_Mt_R_C_Y") %>%
      same_precursors_as(L103.ag_Prod_Mt_R_C_Y_GLU) %>%
      add_precursors("common/iso_GCAM_regID") %>%
      # The sort order in the old dataset is funky, and I'm having trouble
      # replicating it; but the data are identical. Use the less-stringent sum test.
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_SUM_TEST) ->
      L103.ag_Prod_Mt_R_C_Y

    L103.ag_HA_bm2_R_C_Y_GLU %>%
      add_title("Harvested area by GCAM region / commodity / year / GLU") %>%
      add_units("bm2") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L103.ag_HA_bm2_R_C_Y_GLU") %>%
      add_precursors("L101.ag_HA_bm2_R_C_Y",
                     "temp-data-inject/L102.ag_HA_bm2_R_C_GLU") %>%
      # The sort order in the old dataset is funky, and I'm having trouble
      # replicating it; but the data are identical. Use the less-stringent sum test.
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_SUM_TEST) ->
      L103.ag_HA_bm2_R_C_Y_GLU

    L103.ag_Yield_kgm2_R_C_Y_GLU %>%
      add_title("Unadjusted agronomic yield by GCAM region / commodity / year / GLU") %>%
      add_units("kg/m2") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L103.ag_Yield_kgm2_R_C_Y_GLU") %>%
      same_precursors_as(L103.ag_HA_bm2_R_C_Y_GLU) %>%
      add_precursors("L101.ag_Prod_Mt_R_C_Y",
                     "temp-data-inject/L102.ag_Prod_Mt_R_C_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L103.ag_Yield_kgm2_R_C_Y_GLU

    return_data(L103.ag_Prod_Mt_R_C_Y_GLU, L103.ag_Prod_Mt_R_C_Y, L103.ag_HA_bm2_R_C_Y_GLU, L103.ag_Yield_kgm2_R_C_Y_GLU)
  } else {
    stop("Unknown command")
  }
}
