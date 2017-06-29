#' module_aglu_LB142.ag_Fert_IO_R_C_Y_GLU
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.ag_Fert_Prod_MtN_ctry_Y}, \code{L142.ag_Fert_NetExp_MtN_R_Y}, \code{L142.ag_Fert_IO_R_C_Y_GLU}. The corresponding file in the
#' original data system was \code{LB142.ag_Fert_IO_R_C_Y_GLU.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB142.ag_Fert_IO_R_C_Y_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             "L100.LDS_ag_prod_t",
             "L100.FAO_Fert_Cons_tN",
             "L100.FAO_Fert_Prod_tN",
             "L103.ag_Prod_Mt_R_C_Y_GLU",
             "L141.ag_Fert_Cons_MtN_ctry_crop"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L142.ag_Fert_Prod_MtN_ctry_Y",
             "L142.ag_Fert_NetExp_MtN_R_Y",
             "L142.ag_Fert_IO_R_C_Y_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L100.FAO_Fert_Cons_tN <- get_data(all_data, "L100.FAO_Fert_Cons_tN")
    L100.FAO_Fert_Prod_tN <- get_data(all_data, "L100.FAO_Fert_Prod_tN")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L141.ag_Fert_Cons_MtN_ctry_crop <- get_data(all_data, "L141.ag_Fert_Cons_MtN_ctry_crop")

    Fert_name <- "N fertilizer"
    # Compiling fertilizer production and consumption by country and by GCAM region
    # Adjusting production so that global production equals consumption
    L100.FAO_Fert_Prod_tN %>%
      select(iso, year, prod = value) %>%
      full_join(select(L100.FAO_Fert_Cons_tN, iso, year, cons = value), by = c("iso", "year")) %>%
      replace_na(list(prod = 0, cons = 0)) %>%
      group_by(year) %>%
      summarise(prod = sum(prod), cons = sum(cons)) %>%
      ungroup() %>%
      mutate(adj = cons / prod) %>%
      select(year, adj) ->
      L142.ag_Fert_Prod_adj

    L100.FAO_Fert_Prod_tN %>%
      select(iso, year, value) %>%
      left_join_error_no_match(L142.ag_Fert_Prod_adj, by = "year") %>%
      mutate(value = value * adj,
             value = value * CONV_T_MT) %>%
      select(-adj) ->
      L142.ag_Fert_Prod_MtN_ctry_Y

    L142.ag_Fert_Prod_MtN_ctry_Y %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(GCAM_commodity = Fert_name) %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->          # Fill in missing region with 0
      L142.ag_Fert_Prod_MtN_R_Y

    L100.FAO_Fert_Cons_tN %>%
      select(iso, year, value) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value),
                value = value * CONV_T_MT) %>%
      ungroup() %>%
      mutate(GCAM_commodity = Fert_name) %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->          # Fill in missing region with 0
      L142.ag_Fert_Cons_MtN_R_Y

    # Calculating net exports of N fertilizer
    L142.ag_Fert_Prod_MtN_R_Y %>%
      rename(prod = value) %>%
      left_join_error_no_match(L142.ag_Fert_Cons_MtN_R_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
      mutate(value = prod - value) %>%
      select(-prod) ->
      L142.ag_Fert_NetExp_MtN_R_Y

    # Downscale fertilizer demands by country and crop to GLU
    # NOTE: Allocate fertilizer consumption to GLUs on the basis of production, not harvested area
    L100.LDS_ag_prod_t %>%
      group_by(iso, GTAP_crop) %>%
      summarise(total = sum(value)) %>%
      ungroup() -> L142.ag_Prod_t_ctry_crop

    L100.LDS_ag_prod_t %>%
      left_join_error_no_match(L142.ag_Prod_t_ctry_crop, by = c("iso", "GTAP_crop")) %>%
      left_join(L141.ag_Fert_Cons_MtN_ctry_crop, by = c("iso", "GTAP_crop")) %>%
      # Calculate production share by GLU
      mutate(Prod_share = value / total,
             # Downscale fertilizer consumption to GLU based on production share
             Fert_Cons_MtN = Fert_Cons_MtN * Prod_share) %>%
      replace_na(list(Fert_Cons_MtN = 0)) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      left_join(FAO_ag_items_PRODSTAT, by = "GTAP_crop") %>%
      # Aggregate fertilizer demands by GCAM region, commodity, and GLU
      group_by(GCAM_region_ID, GCAM_commodity, GLU) %>%
      summarise(Fert_Cons_MtN = sum(Fert_Cons_MtN)) %>%
      ungroup() %>%
      # Match in production
      left_join(filter(L103.ag_Prod_Mt_R_C_Y_GLU, year %in% BASE_YEAR_IFA), by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      # Calculate unscaled coefficients as unscaled fertilizer demands divided by production
      mutate(Fert_IO_unscaled = Fert_Cons_MtN / value,
             Fert_IO_unscaled = replace(Fert_IO_unscaled, Fert_IO_unscaled == Inf, 0)) %>%
      replace_na(list(Fert_IO_unscaled = 0)) %>%
      select(-year, -value, -Fert_Cons_MtN) %>%
      # match these coefficients in and compute unscaled historical Nfert
      right_join(L103.ag_Prod_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      # Nfert consumption as production multiply coefficients
      mutate(Fert_Cons_MtN_unscaled = value * Fert_IO_unscaled) ->
      L142.ag_Fert_Cons_MtN_R_C_Y_GLU

    # Compute region/year scalers so that consumption balances
    L142.ag_Fert_Cons_MtN_R_C_Y_GLU %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(Fert_Cons_MtN_unscaled = sum(Fert_Cons_MtN_unscaled)) %>%
      ungroup() %>%
      left_join_error_no_match(L142.ag_Fert_Cons_MtN_R_Y, by = c("GCAM_region_ID", "year")) %>%
      mutate(scaler = value / Fert_Cons_MtN_unscaled) %>%
      replace_na(list(scaler = 0)) %>%
      select(GCAM_region_ID, year, scaler) %>%
      right_join(L142.ag_Fert_Cons_MtN_R_C_Y_GLU, by = c("GCAM_region_ID", "year")) %>%
      # Calculate scaled consumption
      mutate(Fert_Cons_MtN = Fert_Cons_MtN_unscaled * scaler,
             Fert_IO = Fert_IO_unscaled * scaler) %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value = Fert_IO) ->
      L142.ag_Fert_IO_R_C_Y_GLU

    # Check to make sure that the fertilizer inputs do not blink in and out (if present in any year, need to be present in all years)
    L142.ag_Fert_IO_R_C_Y_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity, GLU) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(value != 0) %>%
      select(-value) %>%
      unique() ->
      L142.Fert_IO_check
    L142.ag_Fert_IO_R_C_Y_GLU %>%
      semi_join(L142.Fert_IO_check, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) ->
      L142.Fert_IO_check

    if( any( L142.Fert_IO_check$value == 0 ) ){
      stop( "Fertilizer input-output coefficients need to be specified in all historical years")
    }


    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L142.ag_Fert_Prod_MtN_ctry_Y %>%
      add_title("Fertilizer production by country / year") %>%
      add_units("Unit = MtN") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L142.ag_Fert_Prod_MtN_ctry_Y") %>%
      add_precursors("L100.FAO_Fert_Cons_tN",
                     "L100.FAO_Fert_Prod_tN") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L142.ag_Fert_Prod_MtN_ctry_Y

    L142.ag_Fert_NetExp_MtN_R_Y %>%
      add_title("Fertilizer net exports by GCAM region / year") %>%
      add_units("Unit = MtN") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L142.ag_Fert_NetExp_MtN_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_Fert_Cons_tN",
                     "L100.FAO_Fert_Prod_tN") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L142.ag_Fert_NetExp_MtN_R_Y

    L142.ag_Fert_IO_R_C_Y_GLU %>%
      add_title("Fertilizer input-output coefficients by GCAM region / crop / year / GLU") %>%
      add_units("Unitless IO") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L142.ag_Fert_IO_R_C_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L100.LDS_ag_prod_t",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "L141.ag_Fert_Cons_MtN_ctry_crop") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L142.ag_Fert_IO_R_C_Y_GLU

    return_data(L142.ag_Fert_Prod_MtN_ctry_Y, L142.ag_Fert_NetExp_MtN_R_Y, L142.ag_Fert_IO_R_C_Y_GLU)
  } else {
    stop("Unknown command")
  }
}
