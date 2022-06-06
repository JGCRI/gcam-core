# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB142.ag_Fert_IO_R_C_Y_GLU
#'
#' Calculate the adjusted fertilizer production by country / year, fertilizer net exports by GCAM region / year,
#' and fertilizer input-output coefficients by GCAM region / commodity / year / GLU.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.ag_Fert_Prod_MtN_ctry_Y}, \code{L142.ag_Fert_NetExp_MtN_R_Y}, \code{L142.ag_Fert_IO_R_C_Y_GLU}. The corresponding file in the
#' original data system was \code{LB142.ag_Fert_IO_R_C_Y_GLU.R} (aglu level1).
#' @details This chunk calculates fertilizer prodcution by country / year (adjusted to global total consumption),
#' fertilizer net exports by GCAM region / year as production minus consumption, and fertilizer input-output coefficients
#' by GCAM region / commodity / year / GLU.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter full_join group_by left_join mutate right_join select semi_join summarise
#' @importFrom tidyr complete replace_na
#' @author RC June 2017
module_aglu_LB142.ag_Fert_IO_R_C_Y_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             "L100.LDS_ag_prod_t",
             "L100.FAO_Fert_Cons_tN",
             "L100.FAO_Fert_Prod_tN",
             "L101.ag_Prod_Mt_R_C_Y_GLU",
             "L141.ag_Fert_Cons_MtN_ctry_crop"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L142.ag_Fert_Prod_MtN_ctry_Y",
             "L142.ag_Fert_NetExp_MtN_R_Y",
             "L142.ag_Fert_IO_R_C_Y_GLU"))
  } else if(command == driver.MAKE) {

    Fert_Cons_MtN <- Fert_Cons_MtN_unscaled <- Fert_IO <- Fert_IO_unscaled <- Prod_share <-
      prod <- cons <- total <- adj <- scaler <- GCAM_commodity <- GCAM_region_ID <- GTAP_crop <-
      GLU <- iso <- value <- year <- GCAM_subsector <- NULL   # silence package checks

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L100.FAO_Fert_Cons_tN <- get_data(all_data, "L100.FAO_Fert_Cons_tN")
    L100.FAO_Fert_Prod_tN <- get_data(all_data, "L100.FAO_Fert_Prod_tN", strip_attributes = TRUE)
    L101.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L101.ag_Prod_Mt_R_C_Y_GLU")
    L141.ag_Fert_Cons_MtN_ctry_crop <- get_data(all_data, "L141.ag_Fert_Cons_MtN_ctry_crop")

    # Compile N fertilizer production and consumption by country, and adjust country production so that production and consumption balance globally
    L100.FAO_Fert_Prod_tN %>%
      select(iso, year, prod = value) %>%
      # Combine with fertilizer consumption, use full_join to keep all observations, such as ones only have consumption
      full_join(select(L100.FAO_Fert_Cons_tN, iso, year, cons = value), by = c("iso", "year")) %>%
      replace_na(list(prod = 0, cons = 0)) %>%
      group_by(year) %>%
      # Calculate the global total production and consumption
      summarise(prod = sum(prod), cons = sum(cons)) %>%
      ungroup() %>%
      # Calculate the rate to adjust production so that global production equals consumption
      mutate(adj = cons / prod) %>%
      select(year, adj) ->
      L142.ag_Fert_Prod_adj

    L100.FAO_Fert_Prod_tN %>%
      select(iso, year, value) %>%
      left_join_error_no_match(L142.ag_Fert_Prod_adj, by = "year") %>%   # Match in the rates for adjustment
      mutate(value = value * adj,                                        # Adjust production
             value = value * CONV_T_MT) %>%                              # Convert unit of production from tons to million tons of Nitrogen
      select(-adj) ->
      L142.ag_Fert_Prod_MtN_ctry_Y

    # Aggregate N fertilizer adjusted production and consumption to GCAM region level to calculate net exports
    L142.ag_Fert_Prod_MtN_ctry_Y %>%
      rename(prod = value) %>%
      # Combine with fertilizer consumption, use full_join to keep all observations, such as ones only have consumption
      full_join(select(L100.FAO_Fert_Cons_tN, iso, year, cons = value), by = c("iso", "year")) %>%
      replace_na(list(prod = 0, cons = 0)) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%           # Match in GCAM region ID
      group_by(GCAM_region_ID, year) %>%
      summarise(prod = sum(prod), cons = sum(cons)) %>%                  # Aggregate to region total
      ungroup() %>%                                                      # Ungroup before complete
      mutate(cons = cons * CONV_T_MT,                                    # Convert unit of consumption from tons to million tons of Nitrogen
             value = prod - cons,                                        # Calculate net exports as production minus consumption
             GCAM_commodity = aglu.FERT_NAME) %>%                        # Add GCAM commodity category for N fertilizer
      select(-prod) %>%                                                  # Only regional consumption and net exports are needed
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(cons = 0, value = 0)) ->  # Fill in missing regions with 0
      L142.ag_Fert_MtN_R_Y

    # Separate the table for consumption by region / year
    L142.ag_Fert_MtN_R_Y %>%
      select(-value) ->
      L142.ag_Fert_Cons_MtN_R_Y

    # Separate the table for net exports by region / year
    L142.ag_Fert_MtN_R_Y %>%
      select(-cons) ->
      L142.ag_Fert_NetExp_MtN_R_Y

    # Calculate fertilizer input-output coefficients, scaled so that consumption of fertilizer balance
    # First, downscale fertilizer demands by country and crop to GLU
    # NOTE: Allocate fertilizer consumption to GLUs on the basis of production, not harvested area
    # Calculate agriculture prodcution total by country and crop
    L100.LDS_ag_prod_t %>%
      group_by(iso, GTAP_crop) %>%
      summarise(total = sum(value)) %>%
      ungroup() ->
      L142.ag_Prod_t_ctry_crop

    # Start with agricultural production by country / crop / GLU, calculate the production share of GLU to country,
    # Use the share to downscale country fertilizer demand to GLU.
    L100.LDS_ag_prod_t %>%
      # Match in country total production
      left_join_error_no_match(L142.ag_Prod_t_ctry_crop, by = c("iso", "GTAP_crop")) %>%
      # Match in country fertilizer consumption by crop
      left_join(L141.ag_Fert_Cons_MtN_ctry_crop, by = c("iso", "GTAP_crop")) %>%
      # Calculate production share of GLU to country total
      mutate(Prod_share = value / total,
             # Downscale fertilizer demands to GLU using the production share
             Fert_Cons_MtN = Fert_Cons_MtN * Prod_share) %>%
      replace_na(list(Fert_Cons_MtN = 0)) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      # Map in GCAM commodity, creates NA, use left_join instead of left_join_error_no_match
      left_join(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity, GCAM_subsector), by = "GTAP_crop") %>%
      # Drop crops not belong to GCAM commodity
      filter(!is.na(GCAM_commodity)) %>%
      # Aggregate fertilizer demands by GCAM region, commodity, and GLU
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      summarise(Fert_Cons_MtN = sum(Fert_Cons_MtN)) %>%
      ungroup() %>%
      # Match in agricultural production by GCAM region / commodity / GLU in the base year; this creates NAs
      left_join(filter(L101.ag_Prod_Mt_R_C_Y_GLU, year %in% aglu.BASE_YEAR_IFA),
                by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      # Calculate unscaled input-output coefficients as unscaled fertilizer demands divided by agricultural production
      mutate(Fert_IO_unscaled = Fert_Cons_MtN / value,
             Fert_IO_unscaled = replace(Fert_IO_unscaled, Fert_IO_unscaled == Inf, 0)) %>%
      select(-year, -value, -Fert_Cons_MtN) %>%
      # Match these coefficients into historical agricultural production (right_join: same coefficients for all years)
      right_join(L101.ag_Prod_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      # Calculate unscaled fertilizer consumption by year as production multiplied by input-output coefficients
      # GPK note 09/2018 - moving the replace_na() command from a few lines up to here, in order to accommodate missing
      # values which can occur from country/crop observations in FAOSTAT but not Monfreda/LDS (e.g., Puerto Rico rice)
      replace_na(list(Fert_IO_unscaled = 0)) %>%
      mutate(Fert_Cons_MtN_unscaled = value * Fert_IO_unscaled) ->
      L142.ag_Fert_Cons_MtN_R_C_Y_GLU

    # Compute region/year scalers so that sum of fertilizer consumption for all commodities equals total consumption
    L142.ag_Fert_Cons_MtN_R_C_Y_GLU %>%
      group_by(GCAM_region_ID, year) %>%
      # Caclulate total unscaled regional consumption
      summarise(Fert_Cons_MtN_unscaled = sum(Fert_Cons_MtN_unscaled)) %>%
      ungroup() %>%
      # Match in historical total consumption by region
      left_join_error_no_match(L142.ag_Fert_Cons_MtN_R_Y, by = c("GCAM_region_ID", "year")) %>%
      # Calculate the regional scaler so that consumption balance
      mutate(scaler = cons / Fert_Cons_MtN_unscaled) %>%
      replace_na(list(scaler = 0)) %>%
      select(GCAM_region_ID, year, scaler) %>%
      # Match the scalers to unscaled fertilizer consumption by region/commodity/GLU (right_join: same scaler for individual region)
      right_join(L142.ag_Fert_Cons_MtN_R_C_Y_GLU, by = c("GCAM_region_ID", "year")) %>%
      # Calculate scaled consumption
      mutate(Fert_Cons_MtN = Fert_Cons_MtN_unscaled * scaler,
             # Calculate the scalced input-output coefficient
             Fert_IO = Fert_IO_unscaled * scaler) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = Fert_IO) ->
      L142.ag_Fert_IO_R_C_Y_GLU

    # Check to make sure that the fertilizer inputs do not blink in and out (if present in any year, need to be present in all years)
    L142.ag_Fert_IO_R_C_Y_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      summarise(value = sum(value)) %>%                 # Get the total of all years
      ungroup() %>%
      filter(value != 0) %>%                            # Filter the region/commodity/GLU that are not completely missing for all years
      select(-value) %>%
      unique() ->                                       # Select the region/commodity/GLU combinations presented
      L142.Fert_IO_check
    L142.ag_Fert_IO_R_C_Y_GLU %>%
      # Filter the observations with the selected region/commodity/GLU combinations
      semi_join(L142.Fert_IO_check, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) ->
      L142.Fert_IO_check

    # For those region/commodity/GLU that are not completely missing for all years, no missing for any year
    if(any(L142.Fert_IO_check$value == 0)) {
      stop("Fertilizer input-output coefficients need to be specified in all historical years")
    }

    # Produce outputs
    L142.ag_Fert_Prod_MtN_ctry_Y %>%
      add_title("Fertilizer production by country / year") %>%
      add_units("Unit = MtN") %>%
      add_comments("Fertilizer production by country is adjusted so that global total production equals consumption") %>%
      add_comments("Units are converted from tons to million tons of Nitrogen") %>%
      add_legacy_name("L142.ag_Fert_Prod_MtN_ctry_Y") %>%
      add_precursors("L100.FAO_Fert_Cons_tN",
                     "L100.FAO_Fert_Prod_tN") ->
      L142.ag_Fert_Prod_MtN_ctry_Y

    L142.ag_Fert_NetExp_MtN_R_Y %>%
      add_title("Fertilizer net exports by GCAM region / year") %>%
      add_units("Unit = MtN") %>%
      add_comments("Fertilizer consumption and adjusted production are aggregated from country to GCAM region level") %>%
      add_comments("Net exports are calculated as production minus consumption, in million tons of Nitrogen") %>%
      add_legacy_name("L142.ag_Fert_NetExp_MtN_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_Fert_Cons_tN",
                     "L100.FAO_Fert_Prod_tN") ->
      L142.ag_Fert_NetExp_MtN_R_Y

    L142.ag_Fert_IO_R_C_Y_GLU %>%
      add_title("Fertilizer input-output coefficients by GCAM region / crop / year / GLU") %>%
      add_units("Unitless IO") %>%
      add_comments("Fertilizer demands are downscaled to GLU based on agriculture production share in each country") %>%
      add_comments("Input-output coefficients for each crop are first calculated as fertilizer demands divided by agriculture production in the base year") %>%
      add_comments("And then are scaled so that regional total fertilizer consumptions are balanced") %>%
      add_legacy_name("L142.ag_Fert_IO_R_C_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L100.LDS_ag_prod_t",
                     "L101.ag_Prod_Mt_R_C_Y_GLU",
                     "L141.ag_Fert_Cons_MtN_ctry_crop") ->
      L142.ag_Fert_IO_R_C_Y_GLU

    return_data(L142.ag_Fert_Prod_MtN_ctry_Y, L142.ag_Fert_NetExp_MtN_R_Y, L142.ag_Fert_IO_R_C_Y_GLU)
  } else {
    stop("Unknown command")
  }
}
