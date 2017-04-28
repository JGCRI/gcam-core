#' module_aglu_LA108.ag_Feed_R_C_Y
#'
#' Compute (1) feed by GCAM commodity, region, and year, and (2) net exports of FodderHerb.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L108.ag_Feed_Mt_R_C_Y}, \code{L108.ag_NetExp_Mt_R_FodderHerb_Y}. The corresponding file in the
#' original data system was \code{LA108.ag_Feed_R_C_Y.R} (aglu level1).
#' @details Computes (1) feed by GCAM commodity, region, and year, and (2) net exports of FodderHerb.
#' The feed calculations combine IMAGE and FAO data together, using FAO to disaggregate IMAGE to GCAM_commodity.
#' Some adjustments are needed to deal with inconsistencies between IMAGE demand & FAO supply.
#' Excess supply is mapped to OtherUses. Excess demand is mapped to other sources (Residue, Pasture).
#' This information is used to calculate net exports of FodderHerb too.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KVC April 2017
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

    # Part 1: FEEDCROPS
    # Compute regional feedcrop demands by GCAM region, commodity, and year in Mt/yr.
    # Use crop-specific information from FAO, in combination with feed totals from IMAGE,
    # to calculate region/crop specific information. This ensures totals match IMAGE and shares match FAO.
    # First, calculate FAO totals by crop, region, and year. Then, use this compute % of feed from each crop in each region/year.
    L100.FAO_ag_Feed_t %>%
      select(iso, item, year, value) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                                     # Map in GCAM region ID
      left_join(select(FAO_ag_items_cal_SUA, item, GCAM_commodity), by = "item") %>%               # Map in GCAM commodity
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarize(value = sum(value)) %>%                                                            # Aggregate by crop, region, year
      mutate(value = value * CONV_TON_MEGATON) %>%                                                 # Convert from tons to Mt
      ungroup() %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) %>%                                   # Fill in missing region/commodity combinations with 0
      group_by(GCAM_region_ID, year) %>%
      mutate(Feedfrac = value / sum(value)) %>%                                                    # Calculate each crop's share of total feed in a region
      mutate(Feedfrac = if_else(is.na(Feedfrac), 0, Feedfrac)) ->                                  # Replace missing data with 0 (assumes no share for those crops)
      ag_Feed_Mt_R_Cnf_Y

    # Compute aggregate demand by region, feed, and year from the IMAGE data
    L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
      group_by(GCAM_region_ID, feed, year) %>%
      summarize(value = sum(value)) %>%                                                           # Compute total feed by IMAGE feed system, region, year
      ungroup() ->                                                                                # Ungrouping so "feed" column can be deleted later
      an_Feed_Mt_R_C_Y                                                                            # Save this dataframe for use in each feed category later

    # Finally, compute feedcrop demand by region, crop, and year using IMAGE totals and feed fractions computed above
    an_Feed_Mt_R_C_Y %>%
      filter(feed == "FeedCrops") %>%                                                              # Filter to only include "FeedCrops"
      right_join(select(ag_Feed_Mt_R_Cnf_Y, GCAM_region_ID, GCAM_commodity, year, Feedfrac),
                 by = c("GCAM_region_ID", "year" ) ) %>%                                           # Map in feed fractions computed from FAO data
      mutate(value = value * Feedfrac) %>%                                                         # Compute FAO-IMAGE adjusted feed crop demand
      select(-feed, -Feedfrac) ->
      ag_Feed_Mt_R_Cnf_Y_adj

    # FODDERHERB/RESIDUE
    # Part 2: Calculating FodderHerb and Residue balances by region and year
    # First, compute differences between FodderHerb production and FodderHerb_Residue demand.
    # This will be used to compute Residue supply, and to adjust OtherUses of FodderHerb_Residue.
    L103.ag_Prod_Mt_R_C_Y %>%
      filter(GCAM_commodity == "FodderHerb") %>%                                                                # Filter production data for "FodderHerb"
      rename(FodderHerb = value) %>%
      left_join(filter(an_Feed_Mt_R_C_Y,feed=="FodderHerb_Residue"), by = c("GCAM_region_ID","year")) %>%       # Map in demands for FodderHerb_Residue
      rename(FodderHerb_Residue = value) %>%
      mutate(residual = FodderHerb - FodderHerb_Residue ) %>%                                                   # Compute difference in FodderHerb production and FodderHerb_Residue demand
      select(-GCAM_commodity, -feed, -FodderHerb, -FodderHerb_Residue) ->
      ag_Residual_Mt_R_FodderHerbResidue_Y

    # Calculate the global net residual. This is the amount that needs to be apportioned out of the various supplies & demands.
    ag_Residual_Mt_R_FodderHerbResidue_Y %>%
      group_by(year) %>%
      summarize(total_residual = sum(residual)) ->
      ag_Residual_Mt_glbl_FodderHerbResidue_Y

    # Calculate OtherUses of FodderHerb.
    # Other Uses of FodderHerb occur when FodderHerb production exceeds FodderHerb_Residue demand (i.e., residual > 0)
    ag_Residual_Mt_R_FodderHerbResidue_Y %>%
      mutate(residual = if_else( residual < 0, 0, residual)) %>%                                                # Replace any negative residuals with 0
      group_by(year) %>%
      mutate(share = residual / sum(residual))  %>%                                                             # Compute share of residual in each region
      left_join(ag_Residual_Mt_glbl_FodderHerbResidue_Y, by = "year") %>%                                       # Map in global total residual
      mutate(total_residual = if_else( total_residual < 0, 0, total_residual )) %>%                             # Set all negative residuals to zero
      mutate(value = share * total_residual, GCAM_commodity = "FodderHerb") %>%                                 # Compute regional residual, set commodity name to FodderHerb
      select(-residual, -share, -total_residual) ->
      ag_OtherUses_Mt_R_FodderHerb_Y

    # Calculate supply of residue
    # Residue supply is set to the difference between FodderHerb_Residue demand and FodderHerb production,
    # when FodderHerb production is less than FodderHerb_Residue demand (i.e., residual < 0)
    ag_Residual_Mt_R_FodderHerbResidue_Y %>%
      mutate(residual = if_else( residual > 0, 0, residual )) %>%                                               # Replace any positive residuals with 0
      group_by(year) %>%
      mutate(share = residual / sum(residual)) %>%                                                              # Compute share of residual in each region
      left_join(ag_Residual_Mt_glbl_FodderHerbResidue_Y, by = "year") %>%                                       # Map in global total residual
      mutate(total_residual = if_else( total_residual > 0, 0, total_residual )) %>%                             # Set all positive residuals to zero
      mutate(total_residual = total_residual * -1 ) %>%
      mutate(value = share * total_residual, GCAM_commodity = "Residue") %>%                                    # Compute regional residual, set commodity name to Residue
      select(-residual, -share, -total_residual) ->
      ag_Feed_Mt_R_Residue_Y

    # Now, adjust feed from FodderHerb to ensure production and consumption of FodderHerb_Residue match globally
    # i.e., FodderHerb equals FodderHerb_Residue minus Residue
    ag_Feed_Mt_R_Residue_Y %>%
      rename(Residue = value) %>%                                                                               # Start with newly calculated Residue supply
      left_join(filter(an_Feed_Mt_R_C_Y,feed=="FodderHerb_Residue"), by = c("GCAM_region_ID","year")) %>%       # Map in FodderHerb_Residue demand
      mutate(FodderHerb_Residue = value) %>%
      mutate(value = FodderHerb_Residue - Residue, GCAM_commodity = "FodderHerb") %>%                           # Compute FodderHerb = FodderHerb_Residue - Residue
      select(-feed, -Residue, -FodderHerb_Residue) ->
      ag_Feed_Mt_R_FodderHerb_Y

    # PASTURE & FODDERGRASS
    # Part 3: Calculating Pasture and FodderGrass feed inputs by region and year

    # First, compute pasture feed, which is equal to Pasture_FodderGrass demand minus FodderGrass production within each region
    an_Feed_Mt_R_C_Y %>%
      filter(feed == "Pasture_FodderGrass") %>%                                                                 # Start with Pasture_FodderGrass demand
      rename(PastFodderGrass_Demand = value) %>%
      left_join(filter(L103.ag_Prod_Mt_R_C_Y, GCAM_commodity == "FodderGrass"),
                by = c( "GCAM_region_ID", "year")) %>%                                                          # Map in FodderGrass production
      mutate(value = PastFodderGrass_Demand - value, GCAM_commodity = "Pasture") %>%                            # Compute Pasture supply as difference
      select(-feed, -PastFodderGrass_Demand) ->
      ag_Feed_Mt_R_Past_Y

    # If pasture demands are negative, this means FodderGrass production exceeds Pasture_FodderGrass demands
    # When this occurs, set pasture to zero and treat this quantity of foddergrass demand as an other use
    ag_Feed_Mt_R_Past_Y %>%
      mutate(value = value * -1, GCAM_commodity = "FodderGrass") %>%                                            # Compute other uses of FodderGrass as excess supply
      mutate(value = if_else( value < 0, 0, value)) ->                                                          # Zero out all other entries
      ag_OtherUses_Mt_R_FodderGrass_Y

    # Now, adjust the pasture feeds to remove these other uses.
    ag_Feed_Mt_R_Past_Y %>%
      mutate(value = if_else( value < 0, 0, value)) ->
      ag_Feed_Mt_R_Past_Y

    # Adjust FodderGrass feed to reflect the shift in production from feed to other uses (calculated above)
    # FodderGrass used as feed = FodderGrass production - other uses
    L103.ag_Prod_Mt_R_C_Y %>%
      filter(GCAM_commodity == "FodderGrass") %>%                                                          # Start with production of FodderGrass
      rename(Production = value) %>%
      left_join(ag_OtherUses_Mt_R_FodderGrass_Y, by = c("GCAM_commodity", "GCAM_region_ID", "year")) %>%   # Map in other uses
      mutate(value = Production - value) %>%                                                               # Adjust feed supply from FodderGrass
      select(-Production) ->
      ag_Feed_Mt_R_FodderGrass_Y

    # SCAVENGING & OTHER
    # Part 4: Scavenging and other inputs
    # Supply of scavenging_other is equal to demand
    an_Feed_Mt_R_C_Y %>%
      filter(feed == "Scavenging_Other") %>%
      rename(GCAM_commodity = feed) ->
      ag_Feed_Mt_R_ScvgOthr_Y

    # Part 5: Merge all feed sources into a single table
    ag_Feed_Mt_R_Cnf_Y_adj %>%
      bind_rows(ag_Feed_Mt_R_FodderHerb_Y) %>%
      bind_rows(ag_Feed_Mt_R_Residue_Y) %>%
      bind_rows(ag_Feed_Mt_R_FodderGrass_Y) %>%
      bind_rows(ag_Feed_Mt_R_Past_Y) %>%
      bind_rows(ag_Feed_Mt_R_ScvgOthr_Y) ->
      ag_Feed_Mt_R_C_Y

    # Part 6: Compute net exports of FodderHerb
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
      add_title("Feed use by GCAM region, commodity, and year") %>%
      add_units("Mt/yr") %>%
      add_comments("Total feed from FeedCrops, Pasture, FodderGrass, FodderHerb, Residue, and Scavenging.") %>%
      add_comments("FeedCrops: use IMAGE totals, using FAO to disaggregate to individual crops") %>%
      add_comments("Pasture: calculated as the difference between Pasture_FodderGrass demand in IMAGE and FodderGrass production in FAO") %>%
      add_comments("FodderGrass: based on FAO production, but adjusted if this exceeds Pasture_FodderGrass demand in IMAGE") %>%
      add_comments("Residue: calculated as the difference between FodderHerb_Residue demand in IMAGE and FodderHerb production in FAO") %>%
      add_comments("FodderHerb: based on FAO production, but adjusted if this exceeds FodderHerb_Residue demand in IMAGE") %>%
      add_comments("Note: excess FodderGrass and FodderHerb production are mapped to OtherUses") %>%
      add_legacy_name("L108.ag_Feed_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/FAO_ag_items_cal_SUA", "L100.FAO_ag_Feed_t",
                     "L103.ag_Prod_Mt_R_C_Y", "L107.an_Feed_Mt_R_C_Sys_Fd_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L108.ag_Feed_Mt_R_C_Y
    ag_NetExp_Mt_R_FodderHerb_Y %>%
      add_title("Net exports of FodderHerb by GCAM region and year ") %>%
      add_units("Mt/yr") %>%
      add_comments("Net Exports are equal to Production - Feed - OtherUses") %>%
      add_comments("Production: adjusted from FAO production data to ensure supply isn't larger than demand") %>%
      add_comments("Feed: based on IMAGE animal feed data") %>%
      add_comments("OtherUses: any excess production of FodderHerb from FAO (above the FodderHerb_Residue demand from IMAGE)") %>%
      add_legacy_name("L108.ag_NetExp_Mt_R_FodderHerb_Y") %>%
      same_precursors_as("L108.ag_Feed_Mt_R_C_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L108.ag_NetExp_Mt_R_FodderHerb_Y

    return_data(L108.ag_Feed_Mt_R_C_Y, L108.ag_NetExp_Mt_R_FodderHerb_Y)
  } else {
    stop("Unknown command")
  }
}
