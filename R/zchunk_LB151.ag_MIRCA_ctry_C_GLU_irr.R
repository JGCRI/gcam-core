#' module_aglu_LB151.ag_MIRCA_ctry_C_GLU_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L151.ag_irrHA_ha_ctry_crop}, \code{L151.ag_rfdHA_ha_ctry_crop}, \code{L151.ag_irrProd_t_ctry_crop}, \code{L151.ag_rfdProd_t_ctry_crop}. The corresponding file in the
#' original data system was \code{LB151.ag_MIRCA_ctry_C_GLU_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread replace_na
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB151.ag_MIRCA_ctry_C_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO_ag_items_PRODSTAT",
             FILE = "aglu/FAO_ag_CROSIT",
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

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    FAO_ag_CROSIT <- get_data(all_data, "aglu/FAO_ag_CROSIT")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L100.MIRCA_irrHA_ha <- get_data(all_data, "L100.MIRCA_irrHA_ha")
    L100.MIRCA_rfdHA_ha <- get_data(all_data, "L100.MIRCA_rfdHA_ha")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    #
    # If you find a mistake/thing to update in the old code and
    # fixing it will change the output data, causing the tests to fail,
    # (i) open an issue on GitHub, (ii) consult with colleagues, and
    # then (iii) code a fix:
    #
    # if(OLD_DATA_SYSTEM_BEHAVIOR) {
    #   ... code that replicates old, incorrect behavior
    # } else {
    #   ... new code with a fix
    # }
    #
    #
    # NOTE: there are `merge` calls in this code. Be careful!
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses vecpaste
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # ===================================================

    # Compute shares of irrigated and rainfed harvested area from MIRCA data
    # Shares are by country (iso), land unit (GLU), and MIRCA_crop (different from GTAP crop or GCAM_commodity)
    # NOTE: MIRCA data is for a single year. We use the shares, but not the actual land areas.
     L100.MIRCA_irrHA_ha %>% 
      mutate( MIRCA_crop = paste0( "Crop", MIRCA_crop )) %>%                    # Convert name of crop to include "Crop"
      rename( HA_irr = value ) ->                                               # Rename column to facilitate a future join
      MIRCA_irrHA_ha                                                            # Save temporarily
    
    L100.MIRCA_rfdHA_ha %>% 
      mutate( MIRCA_crop = paste0( "Crop", MIRCA_crop )) %>%                    # Convert name of crop to include "Crop"
      rename( HA_rfd = value ) %>%                                              # Rename column to facilitate a future join
      # Join dataframes. Note we want all country/crop combinations even those that only exist in one of the two dataframes.
      full_join( MIRCA_irrHA_ha, by = c( "iso", "GLU", "MIRCA_crop") ) %>%
      replace_na( list(HA_irr = 0, HA_rfd = 0 ) ) %>%                           # Replace NA's with 0. These are expected.
      mutate( irrshareHA = HA_irr / ( HA_irr + HA_rfd ) ) ->                    # Calculate share of irrigated harvested area
      ag_irrshareHA_ctry_Cmir_GLU
          
    # Compute average share of irrigation by country and GLU. 
    # This will be used as the default share in the event of missing values.
    ag_irrshareHA_ctry_Cmir_GLU %>%
      group_by( iso, GLU ) %>%
      summarize( HA_irr = sum( HA_irr ), HA_rfd = sum( HA_rfd ) ) %>%           # Calculate total harvested area by country/GLU
      mutate( avg_irrshare = HA_irr / ( HA_irr + HA_rfd ) ) %>%                 # Calculate share of irrigated harvested area
      select(-HA_irr, -HA_rfd ) ->                                              # Remove extra columns
      Avg_irrshareHA
    
    # Compute harvested area for irrigated and rainfed land by country, GTAP crop, and GLU
    L100.LDS_ag_HA_ha %>%
      rename( totHA = value ) %>%
      left_join( FAO_ag_items_PRODSTAT[ c( "GTAP_crop", "MIRCA_crop" ) ], by = "GTAP_crop" ) %>%   # Map GTAP crops to MIRCA crops
      left_join( ag_irrshareHA_ctry_Cmir_GLU, by = c( "iso", "GLU", "MIRCA_crop" ) ) %>%           # Map crop-specific irrigation share 
      left_join( Avg_irrshareHA, by = c( "iso", "GLU" )  ) %>%                                     # Map generic irrigation share for cases of missing crops
      mutate( irrshareHA = if_else( is.na( irrshareHA ), avg_irrshare, irrshareHA) ) %>%           # First try setting missing values to average irrigation share
      mutate( irrshareHA = if_else( is.na( irrshareHA ), 0, irrshareHA) ) %>%                      # If that doesn't work, set irrigation share to 0 (all rainfed)
      mutate( irrHA = totHA * irrshareHA, rfdHA = totHA * (1 - irrshareHA) ) %>%                   # Calculate irrigated and rainfed HA from shares and total harvested area
      select(-totHA, -MIRCA_crop, -HA_rfd, -HA_irr, -irrshareHA, - avg_irrshare ) ->               # Remove extra columns
      ag_HA_ha_ctry_crop
    
    # Split irrigated and rainfed harvested area into separate dataframes
    ag_HA_ha_ctry_crop %>%
      select(-irrHA) ->                                                         # Remove extra columns
      L151.ag_rfdHA_ha_ctry_crop

    ag_HA_ha_ctry_crop %>%
      select(-rfdHA) ->                                                         # Remove extra columns
      L151.ag_irrHA_ha_ctry_crop
 
    
    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L151.ag_irrHA_ha_ctry_crop %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L151.ag_irrHA_ha_ctry_crop") %>%
      add_precursors("aglu/FAO_ag_items_PRODSTAT","L100.LDS_ag_HA_ha","L100.MIRCA_irrHA_ha","L100.MIRCA_rfdHA_ha") ->
      L151.ag_irrHA_ha_ctry_crop
    L151.ag_rfdHA_ha_ctry_crop %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L151.ag_rfdHA_ha_ctry_crop") %>%
      add_precursors("aglu/FAO_ag_items_PRODSTAT","L100.LDS_ag_HA_ha","L100.MIRCA_irrHA_ha","L100.MIRCA_rfdHA_ha") ->
      L151.ag_rfdHA_ha_ctry_crop
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L151.ag_irrProd_t_ctry_crop") %>%
      add_precursors("common/iso_GCAM_regID","aglu/AGLU_ctry","aglu/FAO_ag_items_PRODSTAT","aglu/FAO_ag_CROSIT",
                     "L100.LDS_ag_HA_ha","L100.LDS_ag_prod_t","L100.MIRCA_irrHA_ha","L100.MIRCA_rfdHA_ha") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L151.ag_irrProd_t_ctry_crop
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L151.ag_rfdProd_t_ctry_crop") %>%
      add_precursors("common/iso_GCAM_regID","aglu/AGLU_ctry","aglu/FAO_ag_items_PRODSTAT","aglu/FAO_ag_CROSIT",
                     "L100.LDS_ag_HA_ha","L100.LDS_ag_prod_t","L100.MIRCA_irrHA_ha","L100.MIRCA_rfdHA_ha") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L151.ag_rfdProd_t_ctry_crop

    return_data(L151.ag_irrHA_ha_ctry_crop, L151.ag_rfdHA_ha_ctry_crop, L151.ag_irrProd_t_ctry_crop, L151.ag_rfdProd_t_ctry_crop)
  } else {
    stop("Unknown command")
  }
}
