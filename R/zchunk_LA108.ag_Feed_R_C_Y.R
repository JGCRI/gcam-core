#' module_aglu_LA108.ag_Feed_R_C_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L108.ag_Feed_Mt_R_C_Y}, \code{L108.ag_NetExp_Mt_R_FodderHerb_Y}. The corresponding file in the
#' original data system was \code{LA108.ag_Feed_R_C_Y.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LA108.ag_Feed_R_C_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO_ag_items_cal_SUA",
             "L100.FAO_ag_Feed_t",
             "L103.ag_Prod_Mt_R_C_Y",
             "L107.an_Feed_Mt_R_C_Sys_Fd_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L108.ag_Feed_Mt_R_C_Y",
             "L108.ag_NetExp_Mt_R_FodderHerb_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_cal_SUA <- get_data(all_data, "aglu/FAO_ag_items_cal_SUA")
    L100.FAO_ag_Feed_t <- get_data(all_data, "L100.FAO_ag_Feed_t")
    L103.ag_Prod_Mt_R_C_Y <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y")
    L107.an_Feed_Mt_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_Feed_Mt_R_C_Sys_Fd_Y")

    # TEMP: Fix integer year columns
    L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
      ungroup() %>%
      mutate(year = as.integer(year) ) ->
      L107.an_Feed_Mt_R_C_Sys_Fd_Y

    L100.FAO_ag_Feed_t %>%
      ungroup() %>%
      mutate(year = as.integer(year) ) ->
      L100.FAO_ag_Feed_t

    # Part 1: FEEDCROPS
    # Compute regional feedcrop demands by GCAM region, commodity, and year in Mt/yr.
    # Use crop-specific information from FAO, in combination with feed totals from IMAGE,
    # to calculate region/crop specific information. This ensures totals match IMAGE.
    # First, calculate FAO crop-region totals
    L100.FAO_ag_Feed_t %>%
      select(iso, item, year, value) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                                     # Map in GCAM region ID
      left_join(select(FAO_ag_items_cal_SUA, item, GCAM_commodity), by = "item") %>%               # Map in GCAM commodity
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarize(value = sum(value)) %>%                                                            # Aggregate by crop, region, year
      mutate(value = value * CONV_TON_MEGATON) %>%                                                 # Convert from tons to Mt
      ungroup() %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->                                    # Fill in missing region/commodity combinations with 0
      ag_Feed_Mt_R_Cnf_Y

    # Then, calculate feedcrop shares by crop within each region
    ag_Feed_Mt_R_Cnf_Y %>%
      group_by(GCAM_region_ID, year) %>%
      summarize(total = sum(value)) %>%                                                            # Aggregate to compute regional totals
      right_join(ag_Feed_Mt_R_Cnf_Y, by = c("GCAM_region_ID", "year")) %>%                         # Map back in the crop/region specific FAO data
      mutate(Feedfrac = value / total) %>%                                                         # Calculate each crop's share of total feed in a region
      mutate(Feedfrac = if_else(is.na(Feedfrac), 0, Feedfrac)) ->                                  # Replace missing data with 0 (assumes no share for those crops)
      ag_Feedfrac_R_Cnf_Y
    # KVC: Can probably blend this with DF above

    # Compute aggregate demand by region, feed, and year from the IMAGE data
    L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
      group_by(GCAM_region_ID, feed, year) %>%
      summarize(value = sum(value)) %>%                                                           # Compute total feed by IMAGE feed system, region, year
      ungroup() ->                                                                                # Ungrouping so "feed" column can be deleted later
      an_Feed_Mt_R_C_Y                                                                            # Save this dataframe for use in each feed category later

    # Feedcrop demand: compute feedcrop demand by region, crop, and year using IMAGE totals and feed fractions computed above
    an_Feed_Mt_R_C_Y %>%
      filter(feed == "FeedCrops") %>%                                                              # Filter to only include "FeedCrops"
      right_join(select(ag_Feedfrac_R_Cnf_Y, GCAM_region_ID, GCAM_commodity, year, Feedfrac),
                 by = c("GCAM_region_ID", "year" ) ) %>%                                           # Map in feed fractions computed from FAO data
      mutate(value = value * Feedfrac) %>%                                                         # Compute FAO-IMAGE adjusted feed crop demand
      select(-feed, -Feedfrac) ->
      ag_Feed_Mt_R_Cnf_Y_adj

    # FODDERHERB/RESIDUE
    # Part 2: Calculating FodderHerb and Residue balances by region and year

    # First compute differences between FodderHerb production and FodderHerb_Residue demand.
    # This will be used to compute Residue supply, and to adjust OtherUses of FodderHerb_Residue.
    L103.ag_Prod_Mt_R_C_Y %>%
      filter(GCAM_commodity == "FodderHerb") %>%                                                                # Filter production data for "FodderHerb"
      rename(FodderHerb = value) %>%
      left_join(filter(an_Feed_Mt_R_C_Y,feed=="FodderHerb_Residue"), by = c("GCAM_region_ID","year")) %>%       # Map in demands for FodderHerb_Residue
      rename(FodderHerb_Residue = value) %>%
      mutate(residual = FodderHerb - FodderHerb_Residue ) %>%                                                   # Compute difference in FodderHerb production and FodderHerb_Residue demand
      select(-GCAM_commodity, -feed, -FodderHerb, -FodderHerb_Residue) ->
      ag_Residual_Mt_R_FodderHerbResidue_Y

    # Calculate the global net residual
    ag_Residual_Mt_R_FodderHerbResidue_Y %>%
      group_by(year) %>%
      summarize(total_residual = sum(residual)) ->
      ag_Residual_Mt_glbl_FodderHerbResidue_Y

    # Separate into positive and negative residuals; then calculate regional share of each
    # Positive residuals occur in regions where FodderHerb production exceeds FodderHerb_Residue demand
    # Calculate OtherUses of FodderHerb
    # NOTE: When global FodderHerb production exceeds FodderHerb_Residue demand, the excess supply is mapped to other net uses
    ag_Residual_Mt_R_FodderHerbResidue_Y %>%
      mutate(residual = if_else( residual < 0, 0, residual)) %>%                                                # Replace any negative residuals with 0
      group_by(year) %>%
      mutate(share = residual / sum(residual))  %>%                                                             # Compute share of residual in each region
      left_join(ag_Residual_Mt_glbl_FodderHerbResidue_Y, by = "year") %>%                                       # Map in global total residual
      mutate(total_residual = if_else( total_residual < 0, 0, total_residual )) %>%                             # Set all negative residuals to zero
      mutate(value = share * total_residual, GCAM_commodity = "FodderHerb") %>%                                 # Compute regional residual, set commodity name to FodderHerb
      select(-residual, -share, -total_residual) ->
      ag_OtherUses_Mt_R_FodderHerb_Y

    # Negative residuals occur in regions where FodderHerb production is less than FodderHerb_Residue demand
    ag_Residual_Mt_R_FodderHerbResidue_Y %>%
      mutate(residual = if_else( residual > 0, 0, residual )) %>%                                               # Replace any positive residuals with 0
      group_by(year) %>%
      mutate(share = residual / sum(residual)) ->                                                               # Compute share of residual in each region
      ag_NegResidual_share_R_FodderHerbResidue_Y                                                                # This dataframe is used to set Residue supply later

    ag_Residual_Mt_glbl_FodderHerbResidue_Y %>%
      mutate(total_residual = if_else( total_residual > 0, 0, total_residual )) %>%
      mutate(total_residual = total_residual * -1 ) ->
      ag_NegResidual_Mt_glbl_FodderHerbResidue_Y
    # KVC: I think this can be blended with code above.

    # KVC -- Move these notes to a more appropriate location.
    # NOTE: Global non-food uses are apportioned to regions according to relative shares of excess supply
    # NOTE: Global Residue production is apportioned to regions according to relative shares of excess demand

    # Calculate residue supply for the FodderHerb_Residue feed category
    # NOTE: When global FodderHerb production is less than FodderHerb_Residue demand, the excess demand is supplied by Residue
    ag_NegResidual_share_R_FodderHerbResidue_Y %>%
      left_join(ag_NegResidual_Mt_glbl_FodderHerbResidue_Y, by = "year") %>%
      mutate(value = share * total_residual, GCAM_commodity = "Residue") %>%
      select(-residual, -share, -total_residual) ->
      ag_Feed_Mt_R_Residue_Y
    # KVC: I think this can be blended with code above.

    # Calculate Feed from FodderHerb = FodderHerb_Residue minus Residue
    ag_Feed_Mt_R_Residue_Y %>%
      rename(Residue = value) %>%
      left_join(filter(an_Feed_Mt_R_C_Y,feed=="FodderHerb_Residue"), by = c("GCAM_region_ID","year")) %>%
      mutate(FodderHerb_Residue = value) %>%
      mutate(value = FodderHerb_Residue - Residue, GCAM_commodity = "FodderHerb") %>%
      select(-feed, -Residue, -FodderHerb_Residue) ->
      ag_Feed_Mt_R_FodderHerb_Y

    # PASTURE & FODDERGRASS
    # Part 3: Calculating Pasture and FodderGrass inputs by region and year

    # Calculate regional FodderGrass production
    L103.ag_Prod_Mt_R_C_Y %>%
      filter(GCAM_commodity == "FodderGrass") ->
      ag_Prod_Mt_R_FodderGrass_Y
    # Can replace future uses of this with filter

    # Calculate regional demands of grass (Pasture_FodderGrass)
    # Pasture demand is equal to Pasture_FodderGrass demand minus FodderGrass production within each region
    an_Feed_Mt_R_C_Y %>%
      filter(feed == "Pasture_FodderGrass") %>%
      rename(PastFodderGrass_Demand = value) %>%
      left_join(ag_Prod_Mt_R_FodderGrass_Y, by = c( "GCAM_region_ID", "year")) %>%
      mutate(value = PastFodderGrass_Demand - value, GCAM_commodity = "Pasture") %>%
      select(-feed, -PastFodderGrass_Demand) ->
      ag_Feed_Mt_R_Past_Y

    # Where pasture demands are negative, set pasture to zero and treat this quantity of foddergrass demand as an other use
    ag_Feed_Mt_R_Past_Y %>%
      mutate(value = value * -1, GCAM_commodity = "FodderGrass") %>%
      mutate(value = if_else( value < 0, 0, value)) ->
      ag_OtherUses_Mt_R_FodderGrass_Y

    # Then, zero out all negative demands
    ag_Feed_Mt_R_Past_Y %>%
      mutate(value = if_else( value < 0, 0, value)) ->
      ag_Feed_Mt_R_Past_Y

    # FodderGrass used as feed = FodderGrass production - other uses
    ag_Prod_Mt_R_FodderGrass_Y %>%
      rename(Production = value) %>%
      left_join(ag_OtherUses_Mt_R_FodderGrass_Y, by = c("GCAM_commodity", "GCAM_region_ID", "year")) %>%
      mutate(value = Production - value) %>%
      select(-Production) ->
      ag_Feed_Mt_R_FodderGrass_Y
    # Blend with 2 df above?

    # SCAVENGING & OTHER
    # Part 4: Scavenging and other inputs
    # Regional demands of scavenging_other determine the supplies; no calculations are needed here
    an_Feed_Mt_R_C_Y %>%
      filter(feed == "Scavenging_Other") %>%
      rename(GCAM_commodity = feed) ->
      ag_Feed_Mt_R_ScvgOthr_Y

    # Merge all feed sources into a single table
    ag_Feed_Mt_R_Cnf_Y_adj %>%
      bind_rows(ag_Feed_Mt_R_FodderHerb_Y) %>%
      bind_rows(ag_Feed_Mt_R_Residue_Y) %>%
      bind_rows(ag_Feed_Mt_R_FodderGrass_Y) %>%
      bind_rows(ag_Feed_Mt_R_Past_Y) %>%
      bind_rows(ag_Feed_Mt_R_ScvgOthr_Y) ->
      ag_Feed_Mt_R_C_Y

    # Compute net exports of FodderHerb
    L103.ag_Prod_Mt_R_C_Y %>%
      filter(GCAM_commodity == "FodderHerb") %>%                                                          # Start with production of FodderHerb
      rename(Production = value) %>%
      left_join(ag_Feed_Mt_R_FodderHerb_Y, by=c("GCAM_commodity", "GCAM_region_ID", "year")) %>%          # Map in feed demand
      rename(Feed = value) %>%
      left_join(ag_OtherUses_Mt_R_FodderHerb_Y, by=c("GCAM_commodity", "GCAM_region_ID", "year")) %>%     # Map in demand for other uses
      rename(OtherUses = value) %>%
      mutate(value = Production - Feed - OtherUses) %>%                                                   # Compute net exports as production - all domestic demands (feed + other use)
      select(-Production, -OtherUses, -Feed) ->
      ag_NetExp_Mt_R_FodderHerb_Y

    # Produce outputs
    ag_Feed_Mt_R_C_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L108.ag_Feed_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/FAO_ag_items_cal_SUA", "L100.FAO_ag_Feed_t",
                     "L103.ag_Prod_Mt_R_C_Y", "L107.an_Feed_Mt_R_C_Sys_Fd_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L108.ag_Feed_Mt_R_C_Y
    ag_NetExp_Mt_R_FodderHerb_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L108.ag_NetExp_Mt_R_FodderHerb_Y") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/FAO_ag_items_cal_SUA", "L100.FAO_ag_Feed_t",
                     "L103.ag_Prod_Mt_R_C_Y", "L107.an_Feed_Mt_R_C_Sys_Fd_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L108.ag_NetExp_Mt_R_FodderHerb_Y

    return_data(L108.ag_Feed_Mt_R_C_Y, L108.ag_NetExp_Mt_R_FodderHerb_Y)
  } else {
    stop("Unknown command")
  }
}
