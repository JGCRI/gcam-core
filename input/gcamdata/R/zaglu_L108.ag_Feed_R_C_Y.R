# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L108.ag_Feed_R_C_Y
#'
#' Compute (1) feed by GCAM commodity, region, and year, and (2) net exports of FodderHerb.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L108.ag_Feed_Mt_R_C_Y}, \code{L108.ag_NetExp_Mt_R_FodderHerb_Y}, \code{L107.an_Feed_Mt_R_C_Sys_Fd_Y_adj}
#' @details Computes (1) feed by GCAM commodity, region, and year, and (2) net exports of FodderHerb.
#' The feed calculations combine IMAGE and FAO data together, using FAO to disaggregate IMAGE to GCAM_commodity.
#' Some adjustments are needed to deal with inconsistencies between IMAGE demand & FAO supply.
#' Excess supply is mapped to OtherUses. Excess demand is mapped to other sources (Residue, Pasture).
#' This information is used to calculate net exports of FodderHerb too.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by left_join mutate right_join select summarize
#' @importFrom tidyr complete nesting replace_na
#' @author KVC April 2017 XZ 2022
module_aglu_L108.ag_Feed_R_C_Y <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      "L101.ag_Feed_Mt_R_C_Y",
      "L101.ag_Prod_Mt_R_C_Y",
      "L107.an_Feed_Mt_R_C_Sys_Fd_Y",
      "L122.FeedOut_Mt_R_C_Yh")

  MODULE_OUTPUTS <-
    c("L108.ag_Feed_Mt_R_C_Y",
      "L108.ag_NetExp_Mt_R_FodderHerb_Y",
      "L108.an_Feed_Mt_R_C_Sys_Fd_Y_adj")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    iso <- item <- year <- value <- GCAM_commodity <- GCAM_region_ID <- feed <-
      Feedfrac <- FodderHerb <- FodderHerb_Residue <- residual <-
      total_residual <- share <- Residue <- PastFodderGrass_Demand <-
      Production <- Feed <- OtherUses <- fractional.secondary.output <-
      feedcakes <- ddgs_feedcakes <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Compute aggregate demand by region, feed, and year from the IMAGE data
    L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
      group_by(GCAM_region_ID, feed, year) %>%
      summarize(value = sum(value)) %>%                                                           # Compute total feed by IMAGE feed system, region, year
      ungroup() ->                                                                                # Ungrouping so "feed" column can be deleted later
      an_Feed_Mt_R_C_Y                                                                            # Save this dataframe for use in each feed category later


    # Part 1: Compute regional feedcrop demands by GCAM region, commodity, and year in Mt/yr ----
    # FEEDCROPS

    # 4/12/2019 revision - include DDGS and feedcakes. This is somewhat complicated. FAOSTAT includes oilcrop feedcakes
    # (e.g., soybean, rapeseed) that are co-produced with oils, irrespective of whether the oil is later used to produce
    # biofuels. As such, the secondary output of feed from "biomassOil" is included in the reported oil crop feed
    # numbers. However, DDGS from corn ethanol production are excluded from reported Corn -> Feed in FAOSTAT. For this
    # reason, the methods below treat DDGS and feedcakes differently. Specifically, DDGS and biofuel-related feedcakes
    # are both added as a new source of animal feed, but the biofuel-related feedcakes quantity is deducted from the
    # prior reported oilcrop -> Feed totals.

    # First, determine the oilcrop feedcakes from biofuel production that are tracked as secondary outputs
    # The method assumes that DDGS come from corn, and anything else reported is oilcrop-related.
    L108.feedcakes <- filter(L122.FeedOut_Mt_R_C_Yh,
                             GCAM_commodity != "Corn",
                             year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
      select(GCAM_region_ID, GCAM_commodity, year, feedcakes = value)


    L108.DDGS_feedcakes <- select(L122.FeedOut_Mt_R_C_Yh, -GCAM_commodity) %>%
      filter(year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
      rename(GCAM_commodity = fractional.secondary.output) %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      complete(nesting(GCAM_commodity, year), GCAM_region_ID = sort(unique(iso_GCAM_regID$GCAM_region_ID)),
               fill = list(value = 0)) %>%
      select(GCAM_region_ID, GCAM_commodity, year, value)



    # Use crop-specific information from FAO, in combination with feed totals from IMAGE,
    # to calculate region/crop specific information. This ensures totals match IMAGE and shares match FAO.
    # First, calculate FAO totals by crop, region, and year. Then, use this compute % of feed from each crop in each region/year.
    L101.ag_Feed_Mt_R_C_Y %>%
      left_join(L108.feedcakes, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%            # Bring in the feedcakes quantities to deduct from the estimated oil crops -> feed
      mutate(value = if_else(is.na(feedcakes), value, value - feedcakes)) %>%
      select(-feedcakes) %>%
      group_by(GCAM_region_ID, year) %>%
      mutate(Feedfrac = value / sum(value)) %>%                                                    # Calculate each crop's share of total feed in a region
      replace_na(list(Feedfrac = 0)) %>%      # Replace missing data with 0 (assumes no share for those crops)
      ungroup() ->
      ag_Feed_Mt_R_Cnf_Y

    # If any of the secondary output feedcakes of some GCAM commodity used for biodiesel production exceed the
    # reported use of that commodity as feed in FAOSTAT, the method above returns negative estimates of feed demand.
    # This should be addressed by looking specifically at the data, and likely reducing the secondary output coefficient
    # of the problematic process(es). Failure to do so will result in negative calibration values read to GCAM and
    # model solution failure.

    # XZ: 09-23-2022 comment this out to address this issue (if exist) using other use in LB109 balance
    # if(any(ag_Feed_Mt_R_Cnf_Y$value < 0)){
    #   stop("Negative feed demands due to secondary output feedcakes exceeding regional feed usage")
    # }


    # 4/12/2019 revision, cont'd: because DDGS and biofuel-related feedcakes are coming from calibrated energy
    # technologies in quantities that can not be scaled, the values in L108.DDGS_feedcakes need to be subtracted from
    # this IMAGE-based animal feed total, with the remainder apportioned to the (non-DDGS and biofuel-related feedcakes).
    # The steps below deduct the DDGS and feedcakes totals from the animal feed quantity computed above.

    # Original METHOD
    # Compute feedcrop demand by region, crop, and year using IMAGE totals, minus DDGS and feedcakes, multiplied by feed
    # fractions computed above
    # an_Feed_Mt_R_C_Y %>%
    #   filter(feed == "FeedCrops") %>%                                                              # Filter to only include "FeedCrops"
    #   left_join_error_no_match(select(L108.DDGS_feedcakes, GCAM_region_ID, year, ddgs_feedcakes = value),
    #                            by = c("GCAM_region_ID", "year")) %>%
    #   mutate(value = value - ddgs_feedcakes) %>%
    #   select(-ddgs_feedcakes) %>%
    #   right_join(select(ag_Feed_Mt_R_Cnf_Y, GCAM_region_ID, GCAM_commodity, year, Feedfrac),
    #              by = c("GCAM_region_ID", "year")) %>%                                             # Map in feed fractions computed from FAO data
    #   mutate(value = value * Feedfrac) %>%                                                         # Compute FAO-IMAGE adjusted feed crop demand
    #   select(-feed, -Feedfrac) %>%
    #   bind_rows(L108.DDGS_feedcakes) ->                                                            # Bind with the DDGS and feedcakes commodity(s)
    #   ag_Feed_Mt_R_Cnf_Y_adj

    # 9/18/2022 Update to the NEW method:
    # Instead of preserving the IO coefficient (dry matter basis) from IMAGE (2005 base year or recent updates)
    # we preserve the feed crop implied by FAO and computed DDGS data
    # and rescale the feed to update IO coefficient at GCAM regional level
    # the original method is commented out but kept above
    # The code is generalized so that method can be switched smoothly

    ag_Feed_Mt_R_Cnf_Y %>%
      select(-Feedfrac) %>%
      bind_rows(L108.DDGS_feedcakes) ->
      ag_Feed_Mt_R_Cnf_Y_adj


    # FODDERHERB/RESIDUE
    # Part 2: Calculating FodderHerb and Residue balances by region and year ----
    # Note that the processing here connects regional demand of FodderHerb_Residue to global supply of FodderHerb
    # First, compute differences between FodderHerb production and FodderHerb_Residue demand.
    # This will be used to compute Residue supply, and to adjust OtherUses of FodderHerb_Residue.
    L101.ag_Prod_Mt_R_C_Y %>%
      filter(GCAM_commodity == "FodderHerb") %>%                                                                  # Filter production data for "FodderHerb"
      rename(FodderHerb = value) %>%
      left_join(filter(an_Feed_Mt_R_C_Y,feed == "FodderHerb_Residue") %>%
                  rename(FodderHerb_Residue = value),
                by = c("GCAM_region_ID", "year")) %>%       # Map in demands for FodderHerb_Residue
      mutate(residual = FodderHerb - FodderHerb_Residue) %>%                                                      # Compute difference in FodderHerb production and FodderHerb_Residue demand
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
      mutate(residual = if_else(residual < 0, 0, residual)) %>%                                                 # Replace any negative residuals with 0
      group_by(year) %>%
      mutate(share = residual / sum(residual)) %>%                                                             # Compute share of residual in each region
      left_join(ag_Residual_Mt_glbl_FodderHerbResidue_Y, by = "year") %>%                                       # Map in global total residual
      mutate(total_residual = if_else(total_residual < 0, 0, total_residual),                               # Set all negative residuals to zero
             value = share * total_residual, GCAM_commodity = "FodderHerb") %>%                                 # Compute regional residual, set commodity name to FodderHerb
      select(-residual, -share, -total_residual) ->
      ag_OtherUses_Mt_R_FodderHerb_Y

    # Calculate supply of residue
    # Residue supply is set to the difference between FodderHerb_Residue demand and FodderHerb production,
    # when FodderHerb production is less than FodderHerb_Residue demand (i.e., residual < 0)
    ag_Residual_Mt_R_FodderHerbResidue_Y %>%
      mutate(residual = if_else(residual > 0, 0, residual)) %>%                                                 # Replace any positive residuals with 0
      group_by(year) %>%
      mutate(share = residual / sum(residual)) %>%                                                              # Compute share of residual in each region
      left_join(ag_Residual_Mt_glbl_FodderHerbResidue_Y, by = "year") %>%                                       # Map in global total residual
      mutate(total_residual = if_else(total_residual > 0, 0, total_residual),                              # Set all positive residuals to zero
             total_residual = -total_residual,
             value = share * total_residual, GCAM_commodity = "Residue") %>%                                    # Compute regional residual, set commodity name to Residue
      select(-residual, -share, -total_residual) ->
      ag_Feed_Mt_R_Residue_Y

    # Now, adjust feed from FodderHerb to ensure production and consumption of FodderHerb_Residue match globally
    # i.e., FodderHerb equals FodderHerb_Residue minus Residue
    ag_Feed_Mt_R_Residue_Y %>%
      rename(Residue = value) %>%                                                                               # Start with newly calculated Residue supply
      left_join(filter(an_Feed_Mt_R_C_Y, feed == "FodderHerb_Residue"), by = c("GCAM_region_ID", "year")) %>%     # Map in FodderHerb_Residue demand
      mutate(FodderHerb_Residue = value,
             value = FodderHerb_Residue - Residue, GCAM_commodity = "FodderHerb") %>%                           # Compute FodderHerb = FodderHerb_Residue - Residue
      select(-feed, -Residue, -FodderHerb_Residue) ->
      ag_Feed_Mt_R_FodderHerb_Y

    # PASTURE & FODDERGRASS
    # Part 3: Calculating Pasture and FodderGrass feed inputs by region and year ----


    # compute pasture feed, which is equal to Pasture_FodderGrass demand minus FodderGrass production within each region
    # adding minimum pasture share over Pasture_FodderGrass to avoid zero pasture adjustments
    # that is more FodderGrass is moved to other uses when minimium is reached in historical data
    # This share is ~30% in the USA. But using 10% to be conservative
    # Other use of FodderGrass will be reflected in L109 balance
    # Note that Japan has no pasture land after 2004, the demand should also be removed

    iso_GCAM_regID %>% filter(iso %in% aglu.Zero_Min_PastureFeed_Share_iso) %>%
      distinct(GCAM_region_ID) %>% pull -> aglu.Zero_Min_PastureFeed_Share_region_ID

    assertthat::assert_that(
      iso_GCAM_regID %>% filter(GCAM_region_ID %in% aglu.Zero_Min_PastureFeed_Share_region_ID) %>%
        distinct(iso) %>% pull %in% aglu.Zero_Min_PastureFeed_Share_iso %>% all,
      msg = "aglu.Zero_Min_PastureFeed_Share_iso should have unique GCAM_region_ID is this adjustment"
    )

    an_Feed_Mt_R_C_Y %>%
      filter(feed == "Pasture_FodderGrass") %>%                                                                 # Start with Pasture_FodderGrass demand
      rename(PastFodderGrass_Demand = value) %>%
      left_join(filter(L101.ag_Prod_Mt_R_C_Y, GCAM_commodity == "FodderGrass"),
                by = c("GCAM_region_ID", "year")) %>%                                                          # Map in FodderGrass production
      mutate(MinPasture = if_else(GCAM_region_ID %in% aglu.Zero_Min_PastureFeed_Share_region_ID, 0,
                                  PastFodderGrass_Demand * aglu.Min_Share_PastureFeed_in_PastureFodderGrass),
             Pasture = if_else(PastFodderGrass_Demand - value < MinPasture,
                               MinPasture,
                             PastFodderGrass_Demand - value),
             FodderGrass = PastFodderGrass_Demand - Pasture) %>%
      select(-MinPasture, -value, -PastFodderGrass_Demand) ->
      an_Feed_Mt_R_C_Y_Pasture_FodderGrass

    if(any(an_Feed_Mt_R_C_Y_Pasture_FodderGrass$Pasture < 0) |
       any(an_Feed_Mt_R_C_Y_Pasture_FodderGrass$FodderGrass < 0)){
      stop("Negative Pasture or FodderGrass demand in an_Feed_Mt_R_C_Y_Pasture_FodderGrass")
    }

    ag_Feed_Mt_R_Past_Y <-
      an_Feed_Mt_R_C_Y_Pasture_FodderGrass %>%
      transmute(GCAM_region_ID, year, value = Pasture) %>%
      mutate(GCAM_commodity = "Pasture") %>%
      # remove all pasture demand in Japan since there is no land
      mutate(value = if_else(GCAM_region_ID %in% aglu.Zero_Min_PastureFeed_Share_region_ID,
                             0, value))

    ag_Feed_Mt_R_FodderGrass_Y <-
      an_Feed_Mt_R_C_Y_Pasture_FodderGrass %>%
      transmute(GCAM_region_ID, year, value = FodderGrass) %>%
      mutate(GCAM_commodity = "FodderGrass")

    # Update L107.an_Feed_Mt_R_C_Sys_Fd_Y with the adjusted pasture
    ag_Feed_Mt_R_Past_Y %>%
      bind_rows(ag_Feed_Mt_R_FodderGrass_Y) %>%
      group_by(GCAM_region_ID, year) %>%
      summarize(UpdateValue = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(
        L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
          filter(feed == "Pasture_FodderGrass") %>%
          group_by(GCAM_region_ID, year) %>%
          summarize(value = sum(value)) %>%
          ungroup(),
        by = c("GCAM_region_ID", "year")
      ) %>% mutate(scaler = UpdateValue / value) %>%
      select(GCAM_region_ID, year, scaler) %>%
      right_join(
        L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
          filter(feed == "Pasture_FodderGrass"),
        by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value * scaler) %>%
      select(-scaler) %>%
      bind_rows(L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
                  filter(feed != "Pasture_FodderGrass"))->
      L107.an_Feed_Mt_R_C_Sys_Fd_Y


    # SCAVENGING & OTHER
    # Part 4: Scavenging and other inputs ----
    # Supply of scavenging_other is equal to demand
    an_Feed_Mt_R_C_Y %>%
      filter(feed == "Scavenging_Other") %>%
      rename(GCAM_commodity = feed) ->
      ag_Feed_Mt_R_ScvgOthr_Y

    # Part 5: Merge all feed sources into a single table ----
    ag_Feed_Mt_R_Cnf_Y_adj %>%
      bind_rows(ag_Feed_Mt_R_FodderHerb_Y) %>%
      bind_rows(ag_Feed_Mt_R_Residue_Y) %>%
      bind_rows(ag_Feed_Mt_R_FodderGrass_Y) %>%
      bind_rows(ag_Feed_Mt_R_Past_Y) %>%
      bind_rows(ag_Feed_Mt_R_ScvgOthr_Y) ->
      L108.ag_Feed_Mt_R_C_Y




    # Part 6: Compute net exports of FodderHerb ----
    L101.ag_Prod_Mt_R_C_Y %>%
      filter(GCAM_commodity == "FodderHerb") %>%                                                          # Start with production of FodderHerb
      rename(Production = value) %>%
      left_join(ag_Feed_Mt_R_FodderHerb_Y, by = c("GCAM_commodity", "GCAM_region_ID", "year")) %>%        # Map in feed demand
      rename(Feed = value) %>%
      left_join(ag_OtherUses_Mt_R_FodderHerb_Y, by = c("GCAM_commodity", "GCAM_region_ID", "year")) %>%   # Map in demand for other uses
      rename(OtherUses = value) %>%
      mutate(value = Production - Feed - OtherUses) %>%                                                   # Compute net exports as production - all domestic demands (feed + other use)
      select(-Production, -OtherUses, -Feed) ->
      L108.ag_NetExp_Mt_R_FodderHerb_Y

    # Part 7: Scale feed demand in L107.an_Feed_Mt_R_C_Sys_Fd_Y_adj ----

    # Only FeedCrops is adjusted now to use FAO data directly instead of those
    # computed based on IMAGE IO coefficients.
    # If scaled, preserving FAO feed crop data.
    # Otherwise, preserving IMAGE IO coefficients

    an_Feed_Mt_R_C_Y %>%
      filter(feed == "FeedCrops") %>%
      left_join_error_no_match(
        ag_Feed_Mt_R_Cnf_Y_adj %>%
          group_by(GCAM_region_ID, year) %>%
          summarize(value_adj = sum(value), .groups = "drop"),
        by = c("GCAM_region_ID", "year") ) %>%
      mutate(Scaler = if_else(value == 0 & value_adj == 0, 1, value_adj / value)) %>%
      select(-value, -value_adj)->
      FeedCrops_Scaler

    L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
      filter(feed == "FeedCrops") %>%
      left_join_error_no_match(FeedCrops_Scaler,
                               by = c("GCAM_region_ID", "year", "feed")) %>%
      mutate(value = value * Scaler) %>%
      select(-Scaler) %>%
      bind_rows(
        L107.an_Feed_Mt_R_C_Sys_Fd_Y %>%
          filter(feed != "FeedCrops") ) ->
      L108.an_Feed_Mt_R_C_Sys_Fd_Y_adj

    # Produce outputs
    L108.ag_Feed_Mt_R_C_Y %>%
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
      add_precursors("common/iso_GCAM_regID", "L101.ag_Feed_Mt_R_C_Y",
                     "L101.ag_Prod_Mt_R_C_Y", "L107.an_Feed_Mt_R_C_Sys_Fd_Y", "L122.FeedOut_Mt_R_C_Yh") ->
      L108.ag_Feed_Mt_R_C_Y

    L108.ag_NetExp_Mt_R_FodderHerb_Y %>%
      add_title("Net exports of FodderHerb by GCAM region and year ") %>%
      add_units("Mt/yr") %>%
      add_comments("Net Exports are equal to Production - Feed - OtherUses") %>%
      add_comments("Production: adjusted from FAO production data to ensure supply isn't larger than demand") %>%
      add_comments("Feed: based on IMAGE animal feed data") %>%
      add_comments("OtherUses: any excess production of FodderHerb from FAO (above the FodderHerb_Residue demand from IMAGE)") %>%
      add_legacy_name("L108.ag_NetExp_Mt_R_FodderHerb_Y") %>%
      same_precursors_as("L108.ag_Feed_Mt_R_C_Y") ->
      L108.ag_NetExp_Mt_R_FodderHerb_Y

    L108.an_Feed_Mt_R_C_Sys_Fd_Y_adj %>%
      add_title("Adjusted feed demand based on FAO data") %>%
      add_units("Mt/yr") %>%
      add_comments("Feed: based on IMAGE animal feed data") %>%
      add_legacy_name("L108.an_Feed_Mt_R_C_Sys_Fd_Y_adj") %>%
      add_precursors("L107.an_Feed_Mt_R_C_Sys_Fd_Y",
                     "L101.ag_Feed_Mt_R_C_Y",
                     "L122.FeedOut_Mt_R_C_Yh") ->
      L108.an_Feed_Mt_R_C_Sys_Fd_Y_adj


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
