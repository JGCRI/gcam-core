#' module_aglu_LB151.ag_MIRCA_ctry_C_GLU_irr
#'
#' Separates country and harvested area into irrigated and rainfed by country, GLU, and GTAP_crop.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L151.ag_irrHA_ha_ctry_crop}, \code{L151.ag_rfdHA_ha_ctry_crop}, \code{L151.ag_irrProd_t_ctry_crop}, \code{L151.ag_rfdProd_t_ctry_crop}. The corresponding file in the
#' original data system was \code{LB151.ag_MIRCA_ctry_C_GLU_irr.R} (aglu level1).
#' @details Separates country and harvested area into irrigated and rainfed by country, GLU, and GTAP_crop.
#' This chunk uses MIRCA irrigated and rainfed harvested area to separate harvested area from the LDS. This
#' is then combined with ratios of irrigated to rainfed yield from the FAO CROSIT database to compute
#' irrigated and rainfed production.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread replace_na
#' @author KVC April 2017
#' @export
module_aglu_LB151.ag_MIRCA_ctry_C_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             FILE = "aglu/FAO/FAO_ag_CROSIT",
             "L100.LDS_ag_HA_ha",
             "L100.LDS_ag_prod_t",
             "L100.MIRCA_irrHA_ha",
             "L100.MIRCA_rfdHA_ha"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L151.ag_irrHA_ha_ctry_crop",
             "L151.ag_rfdHA_ha_ctry_crop",
             "L151.ag_irrProd_t_ctry_crop",
             "L151.ag_rfdProd_t_ctry_crop"))
  } else if(command == driver.MAKE) {

    MIRCA_crop <- value <- HA_irr <- HA_rfd <- iso <- GLU <- irrshareHA <-
        avg_irrshare <- totHA <- irrHA <- rfdHA <- CROSIT_country_ID <-
        GTAP_crop <- CROSIT_cropID <- country_ID <- crop_ID <-
        Yield_kgHa_irrigated <- Yield_kgHa_rainfed <- year <- yieldratio <-
        irrshareProd <- rfdProd <- irrProd <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    FAO_ag_CROSIT <- get_data(all_data, "aglu/FAO/FAO_ag_CROSIT")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L100.MIRCA_irrHA_ha <- get_data(all_data, "L100.MIRCA_irrHA_ha")
    L100.MIRCA_rfdHA_ha <- get_data(all_data, "L100.MIRCA_rfdHA_ha")

    # Compute shares of irrigated and rainfed harvested area from MIRCA data
    # Shares are by country (iso), land unit (GLU), and MIRCA_crop (different from GTAP crop or GCAM_commodity)
    # NOTE: MIRCA data is for a single year. We use the shares, but not the actual land areas.
     L100.MIRCA_irrHA_ha %>%
      mutate(MIRCA_crop = paste0( "Crop", MIRCA_crop)) %>%                    # Convert name of crop to include "Crop"
      rename(HA_irr = value) ->                                               # Rename column to facilitate a future join
      MIRCA_irrHA_ha                                                          # Save temporarily

    L100.MIRCA_rfdHA_ha %>%
      mutate(MIRCA_crop = paste0( "Crop", MIRCA_crop)) %>%                    # Convert name of crop to include "Crop"
      rename(HA_rfd = value) %>%                                              # Rename column to facilitate a future join
      # Join dataframes. Note we want all country/crop combinations even those that only exist in one of the two dataframes.
      full_join(MIRCA_irrHA_ha, by = c( "iso", "GLU", "MIRCA_crop")) %>%
      replace_na(list(HA_irr = 0, HA_rfd = 0 )) %>%                           # Replace NA's with 0. These are expected.
      mutate(irrshareHA = HA_irr / ( HA_irr + HA_rfd )) ->                    # Calculate share of irrigated harvested area
      ag_irrshareHA_ctry_Cmir_GLU

    # Compute average share of irrigation by country and GLU.
    # This will be used as the default share in the event of missing values.
    ag_irrshareHA_ctry_Cmir_GLU %>%
      group_by(iso, GLU) %>%
      summarize(HA_irr = sum( HA_irr ), HA_rfd = sum( HA_rfd )) %>%           # Calculate total harvested area by country/GLU
      mutate(avg_irrshare = HA_irr / ( HA_irr + HA_rfd )) %>%                 # Calculate share of irrigated harvested area
      select(-HA_irr, -HA_rfd) ->                                             # Remove extra columns
      Avg_irrshareHA

    # Compute harvested area for irrigated and rainfed land by country, GTAP crop, and GLU
    L100.LDS_ag_HA_ha %>%
      rename(totHA = value) %>%
      left_join(FAO_ag_items_PRODSTAT[ c( "GTAP_crop", "MIRCA_crop" ) ], by = "GTAP_crop") %>%    # Map GTAP crops to MIRCA crops
      left_join(ag_irrshareHA_ctry_Cmir_GLU, by = c( "iso", "GLU", "MIRCA_crop" )) %>%            # Map crop-specific irrigation share
      left_join(Avg_irrshareHA, by = c( "iso", "GLU" )) %>%                                       # Map generic irrigation share for cases of missing crops
      mutate(irrshareHA = if_else( is.na( irrshareHA ), avg_irrshare, irrshareHA)) %>%            # First try setting missing values to average irrigation share
      mutate(irrshareHA = if_else( is.na( irrshareHA ), 0, irrshareHA)) %>%                       # If that doesn't work, set irrigation share to 0 (all rainfed)
      mutate(irrHA = totHA * irrshareHA, rfdHA = totHA * (1 - irrshareHA)) %>%                    # Calculate irrigated and rainfed HA from shares and total harvested area
      select(-totHA, -MIRCA_crop, -HA_rfd, -HA_irr, -irrshareHA, - avg_irrshare) ->               # Remove extra columns
      ag_HA_ha_ctry_crop

    # Split irrigated and rainfed harvested area into separate dataframes
    ag_HA_ha_ctry_crop %>%
      select(-irrHA) ->                                                         # Remove extra columns
      L151.ag_rfdHA_ha_ctry_crop

    ag_HA_ha_ctry_crop %>%
      select(-rfdHA) ->                                                         # Remove extra columns
      L151.ag_irrHA_ha_ctry_crop

    # Calculate production on irrigated and rainfed land, by country, GTAP crop, and GLU
    # Use the ratio of irrigated to rainfed yield from the FAO CROSIT database,
    # in combination with the harvested area data computed above to separate irrigated
    # and rainfed production.
    # First, prepare the country mapping file. The old data system uses the first entry when duplicated
    # iso codes are found (which happens a lot in this file). Dplyr will include both identical entries,
    # which we don't want.
    AGLU_ctry %>%
      select(iso, CROSIT_country_ID) %>%
      distinct(iso, .keep_all=TRUE) ->
      Unique_AGLU_ctry

    # Next, Calculate the yield ratio for each country and crop, based on the CROSIT database
    L100.LDS_ag_HA_ha %>%
      select(iso, GLU, GTAP_crop) %>%
      left_join(select(Unique_AGLU_ctry, iso, CROSIT_country_ID), by = "iso") %>%                                      # Map in CROSIT country ids
      left_join(select(FAO_ag_items_PRODSTAT, GTAP_crop, CROSIT_cropID), by = "GTAP_crop") %>%                         # Map in CROSIT crop ids
      left_join(select(FAO_ag_CROSIT, country_ID, crop_ID, Yield_kgHa_irrigated, Yield_kgHa_rainfed, year),            # Map in CROSIT irrigated and rainfed yields
                 by = c( "CROSIT_country_ID" = "country_ID", "CROSIT_cropID" = "crop_ID")) %>%

      # CROSIT data includes historic and future years, we only want the historic year. Additionally, CROSIT data is missing some crops
      # we don't want to remove those. We'll set their yield ratio to 1 later.
      filter(year == CROSIT_HISTORICAL_YEAR | is.na(year)) %>%
      mutate(yieldratio = Yield_kgHa_irrigated / Yield_kgHa_rainfed) %>%                                               # Compute yield ratio
      select(-Yield_kgHa_irrigated, -Yield_kgHa_rainfed) %>%
      mutate(yieldratio=if_else( is.na(yieldratio), 1, yieldratio)) %>%                                                # Replace NAs with 1
      mutate(yieldratio=if_else( yieldratio==0, 1, yieldratio)) %>%                                                    # Replace zeros with 1
      mutate(yieldratio=if_else( is.infinite(yieldratio), 1, yieldratio))  ->                                          # Replace Infs with 1
      Yieldratio_ctry_crop

    # Now, use the yield ratio to solve for the production shares (irrigated versus rainfed)
    # This calculation preserves the harvested area shares and yield ratios computed above.
    ag_HA_ha_ctry_crop %>%
      left_join(select(Yieldratio_ctry_crop, iso, GLU, GTAP_crop, yieldratio), by = c( "iso", "GLU", "GTAP_crop")) %>%          # Map in the yield ratio
      mutate(irrshareHA = irrHA / (irrHA + rfdHA)) %>%                                                                          # Compute the share of irrigated harvested area
      mutate(irrshareProd = (irrshareHA * yieldratio)/((irrshareHA * yieldratio) + (1 - irrshareHA))) %>%                       # Compute the share of irrigated production
      right_join(L100.LDS_ag_prod_t, by=c("iso", "GLU", "GTAP_crop")) %>%                                                       # Map in the total production
      mutate(irrProd=value*irrshareProd, rfdProd=value*(1-irrshareProd)) ->                                                     # Compute irrigated and rainfed production
      ag_Prod_t_ctry_crop

    # Split irrigated and rainfed production into separate dataframes
    ag_Prod_t_ctry_crop %>%
      select(iso,GLU,GTAP_crop,rfdProd) ->
      L151.ag_rfdProd_t_ctry_crop

    ag_Prod_t_ctry_crop %>%
      select(iso,GLU,GTAP_crop,irrProd) ->
      L151.ag_irrProd_t_ctry_crop

    L151.ag_irrHA_ha_ctry_crop %>%
      add_title("Irrigated harvested area by country, GTAP crop, GLU") %>%
      add_units("Hectares") %>%
      add_comments("Uses shares of irrigated area from MIRCA, combined with total harvested area, to calculate irrigated harvested area") %>%
      add_comments("Data is for a single year (circa 2000)") %>%
      add_legacy_name("L151.ag_irrHA_ha_ctry_crop") %>%
      add_precursors("aglu/FAO/FAO_ag_items_PRODSTAT", "L100.LDS_ag_HA_ha", "L100.MIRCA_irrHA_ha", "L100.MIRCA_rfdHA_ha") ->
      L151.ag_irrHA_ha_ctry_crop

    L151.ag_rfdHA_ha_ctry_crop %>%
      add_title("Irrigated harvested area by country, GTAP crop, GLU") %>%
      add_units("Hectares") %>%
      add_comments("Uses shares of rainfed area from MIRCA, combined with total harvested area, to calculate rainfed harvested area") %>%
      add_comments("Data is for a single year (circa 2000)") %>%
      add_legacy_name("L151.ag_rfdHA_ha_ctry_crop") %>%
      add_precursors("aglu/FAO/FAO_ag_items_PRODSTAT", "L100.LDS_ag_HA_ha", "L100.MIRCA_irrHA_ha", "L100.MIRCA_rfdHA_ha") ->
      L151.ag_rfdHA_ha_ctry_crop

    L151.ag_irrProd_t_ctry_crop %>%
      add_title("Irrigated production by country, GTAP crop, GLU") %>%
      add_units("tons") %>%
      add_comments("Combines the MIRCA shares of irrigated/rainfed area with the FAO CROSIT ratios of irrigated to rainfed yield") %>%
      add_comments("to compute irrigated production shares. These are then combined with total production to calculate irrigated production.") %>%
      add_comments("In the event of missing yield information, irrigated and rainfed yields are assumed equal (and thus production shares equal)") %>%
      add_comments("Data is for a single year (circa 2000)") %>%
      add_legacy_name("L151.ag_irrProd_t_ctry_crop") %>%
      same_precursors_as("L151.ag_irrHA_ha_ctry_crop") %>%
      add_precursors("aglu/AGLU_ctry","aglu/FAO/FAO_ag_CROSIT","L100.LDS_ag_prod_t") ->
      L151.ag_irrProd_t_ctry_crop

    L151.ag_rfdProd_t_ctry_crop %>%
      add_title("Rainfed production by country, GTAP crop, GLU") %>%
      add_units("tons") %>%
      add_comments("Combines the MIRCA shares of irrigated/rainfed area with the FAO CROSIT ratios of irrigated to rainfed yield") %>%
      add_comments("to compute rainfed production shares. These are then combined with total production to calculate rainfed production.") %>%
      add_comments("In the event of missing yield information, irrigated and rainfed yields are assumed equal (and thus production shares equal)") %>%
      add_comments("Data is for a single year (circa 2000)") %>%
      add_legacy_name("L151.ag_rfdProd_t_ctry_crop") %>%
      same_precursors_as("L151.ag_rfdHA_ha_ctry_crop") %>%
      add_precursors("aglu/AGLU_ctry","aglu/FAO/FAO_ag_CROSIT","L100.LDS_ag_prod_t") ->
      L151.ag_rfdProd_t_ctry_crop

    return_data(L151.ag_irrHA_ha_ctry_crop, L151.ag_rfdHA_ha_ctry_crop, L151.ag_irrProd_t_ctry_crop, L151.ag_rfdProd_t_ctry_crop)
  } else {
    stop("Unknown command")
  }
}
