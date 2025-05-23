# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L110.For_FAO_R_Y
#'
#' Build FAO forestry production, export, import, and consumption data for every GCAM region in each year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L110.For_ALL_bm3_R_Y}. The corresponding file in the
#' original data system was \code{LB110.For_FAO_R_Y.R} (aglu level1).
#' @details This module builds Forest Production, Net Export (FAO Exports - FAO Imports), and
#' Consumption  (Production - Net Exports) information for every GCAM region in each year, from
#' FAO Production, Export, and Import data. FAO Production data is used to scale Net Exports
#' at the Region level such that Global Production equals Consumption.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by mutate select summarise summarise_all
#' @author MC and ACS March 2017
module_aglu_L110.For_FAO_R_Y <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      "L100.FAO_For_Prod_m3",
      "L100.FAO_For_Imp_m3",
      "L100.FAO_For_Exp_m3",
      FILE="aglu/A_forest_mapping")

  MODULE_OUTPUTS <-
    c("L110.For_ALL_bm3_R_Y","L110.IO_Coefs_pulp")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    value <- flow <- GCAM_region_ID <- GCAM_commodity <- year <- Prod_bm3 <-
      NetExp_bm3 <- Cons_bm3 <- Cons_scaler <- . <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # indicate flow on each tibble - flow is a directional quantity indicating net export or production
    # Old comment: Indicate the flow on each table and combine (rbind). Multiply imports by -1 and call both imports and exports the same flow
    L100.FAO_For_Prod_m3 %>%    # take the production tibble
      mutate(flow = "Prod_m3") -> # add the flow column and make every entry = "Prod_m3"
      L100.FAO_For_Prod_m3      # store in the production tibble

    L100.FAO_For_Exp_m3 %>%       # take the export tibble
      mutate(flow = "NetExp_m3") -> # add the flow identifier because this is a component of net export = export - import
      L100.FAO_For_Exp_m3         # store in the export tibble

    # For imports, we also multiply value by -1, to represent them as a negative export, making
    # it easier to sum up exports-imports later
    L100.FAO_For_Imp_m3 %>%         # take the import tibble
      mutate(flow = "NetExp_m3",  # add the flow identifier because this is a component of net export = export - import
             value = -value) ->       # make the values negative so that it gets subtracted when we aggregate by flow=NetExp
      L100.FAO_For_Imp_m3           # store in the import tibble

    # Combine all three L100.FAO_For_X tibbles into a single tibble
    # this tibble will be summed over by flow later, calculating export-import to give net export
    L110.FAO_For_ALL_m3 <- bind_rows(L100.FAO_For_Prod_m3, L100.FAO_For_Imp_m3, L100.FAO_For_Exp_m3)

    # Lines 41-60 in original file
    # Add regionID lookup vectors and GCAM_commodity label to the For_ALL tibble
    # convert units and aggregate over flow to form Production and Net Export in units of bm3
    # Use spread to have columns for Prod and NetExp instead of flow and value
    # Add a column for the variable consumption, Cons=Prod-NetExp
    L110.FAO_For_ALL_m3 %>%
      left_join_error_no_match(A_forest_mapping, by = c("item")) %>%
      # take the combined tibble
      # do a left join on For_ALL tibble, match up the iso labels from the iso tibble,
      #   This appends to the For_ALL tibble all of the information from the iso_GCAM_regID, including the column we actually
      #   want, GCAM_region_ID. This column is all that we save:
      mutate(
             # add the forest commodity label
             value = CONV_M3_BM3 * value*tonnes_to_m3,                 # convert the value units from m3 to bm3, had to add this constant to constants.R
             flow = sub("_m3", "_bm3", flow)) %>%         # update the labels in flow to reflect the new units of bm3
      #
      # we don't care about other identifiers anymore, only region id, commodity, flow and year corresponding to value:
      select(GCAM_region_ID, GCAM_commodity, flow, year, value) %>%
      #
      # these two lines (group_by and summarise) compute Export-Import to give net exports, and leave the Prod alone,
      #   for each commodity (just forest) in each GCAM region:
      group_by(GCAM_region_ID, GCAM_commodity, flow, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      #
      # build volumetric mass balance tibble:
      #   instead of having flow and value for each year, region, commodity, we now want NetExp and Prod columns for each year, region, commodity.
      #   The new DSR eliminates the use of L110.For_ALL_bm3_R_Y.prelim
      spread(flow, value) %>%
      #
      mutate(Prod_bm3= if_else(is.na(Prod_bm3),0,Prod_bm3),
             NetExp_bm3= if_else(is.na(NetExp_bm3),0,NetExp_bm3),
             Cons_bm3 = Prod_bm3 - NetExp_bm3) %>%                                     # form a new variable, consumption Cons = Prod-NetExp
      select(GCAM_region_ID, GCAM_commodity, year, Prod_bm3, NetExp_bm3, Cons_bm3) ->  # reorder columns
      L110.For_ALL_bm3_R_Y                                                             # save it in the region year R_Y tibble

    # Lines 62 - 64 in original file
    # Form Global values by summing over regions for each Prod, NetExp, Cons in tibble L110.For_ALL_bm3_R_Y;
    # Then use the Global values to calculate the global consumption scaler that satisfies Global Production = Consumption.
    # It is a scaler on consumption rather than production because production data is prioritized over consumption data when
    # there is a mismatch.
    L110.For_ALL_bm3_R_Y %>%                     # take the region year R_Y tibble
      group_by(GCAM_commodity, year) %>%         # For each commodity and year
      summarise_all(sum) %>%                     # sum over each remaining column
      ungroup() %>%
      select(-GCAM_region_ID) %>%                # drop the region id, since we are calculating a global quantity
      mutate(Cons_scaler = Prod_bm3 / Cons_bm3) -> # add a variable for the consumption scaler
      L110.For_ALL_bm3_glbl_Y                    # store in the global tibble

    # Lines 65-71 in original file
    # Use the global consumption scaler in tibble L110.For_ALL_bm3_glbl_Y to scale regions
    L110.For_ALL_bm3_R_Y %>%                                               # take the region year R_Y tibble
      left_join_error_no_match(select(L110.For_ALL_bm3_glbl_Y, year, GCAM_commodity, Cons_scaler),
                               by = c("year", "GCAM_commodity")) %>%
      # ^ Use Left Join to add the Global Cons_scaler corresponding to each year in each region. This command adds
      #   more information than we need or care about, so only keep the Cons_scaler column that matters
      mutate(Cons_bm3 = Cons_bm3 * Cons_scaler,                       # Use the scaler to recalculate Cons_bm3
             NetExp_bm3 = Prod_bm3 - Cons_bm3) %>%                       # and recalculate NetExp from the new Cons_bm3
      select(-Cons_scaler) ->                                              # remove the Cons_scaler column to form the adjusted mass balance tibble
      L110.For_ALL_bm3_R_Y                                                 # store in the region year R_Y tibble


    # Lines 74-78 in original file
    # old comment: Translate to full table for any regions with no forest data
    L110.For_ALL_bm3_R_Y %>%                                                   # take the region year R_Y table
      tidyr::complete(GCAM_region_ID = unique(iso_GCAM_regID[['GCAM_region_ID']]), # use complete to make sure every commodity (forest)
                      tidyr::nesting(GCAM_commodity, year),                          # and year is represented for every GCAM region.
                      fill = list(Prod_bm3 = 0, NetExp_bm3 = 0, Cons_bm3 = 0)) %>%     # Fill in the new regions with 0 Prod, NetExp, Cons
      unique ->
      L110.For_ALL_bm3_R_Y

    ##Add calculations of wood pulp IO coefficients

    #First separate out roundwood consumption
    L110.For_ALL_bm3_R_Y %>%
      filter(GCAM_commodity==aglu.FOREST_SUPPLY_SECTOR) %>%
      select(GCAM_region_ID,year,roundwood_cons=Cons_bm3)->L110.Roundwood_Cons

    #Join the same with commoditties.
    L110.For_ALL_bm3_R_Y %>%
      filter(GCAM_commodity %in% aglu.FOREST_COMMODITIES) %>%
      select(GCAM_region_ID,year,GCAM_commodity,Prod_bm3) %>%
      spread(GCAM_commodity,Prod_bm3) %>%
      left_join_error_no_match(L110.Roundwood_Cons, by = c("GCAM_region_ID","year")) %>%
      #Assume that pulpwood has a coeff of 5.14 sawtimber is the remaining. There are a couple of adjustments that need to be made.
      mutate(#First adjust sawnwood production here
             #sawnwood= if_else(sawnwood > 2 *roundwood_cons, roundwood_cons *0.05,sawnwood),
             after_pulp = roundwood_cons-(woodpulp*aglu.FOREST_PULP_CONVERSION),
             #If a country does not have enough roundwood cons to produce saw, increase it.
             roundwood_cons=if_else(after_pulp <0, woodpulp*aglu.FOREST_PULP_CONVERSION*1.1,roundwood_cons),
             after_pulp = roundwood_cons-(woodpulp*aglu.FOREST_PULP_CONVERSION),
             #Now calculate pulp IO here
             IO=after_pulp/sawnwood,
             #We are going to run in a scenario where the coef is less than 1 in some places.
             IO= if_else(IO < 1,1,IO),
             #Add a max value on the IO here,
             IO= if_else(IO > 10,10,IO),
             IO= if_else(sawnwood==0, 0,IO),
             roundwood_cons=(woodpulp*aglu.FOREST_PULP_CONVERSION)+(IO*sawnwood)) ->L110.IO_Coefs_pulp

    #Since we increased roundwood cons in some places, increase production proportionately
    L110.For_ALL_bm3_R_Y %>%
      filter(GCAM_commodity==aglu.FOREST_SUPPLY_SECTOR) %>%
      left_join_error_no_match(L110.IO_Coefs_pulp %>% select(GCAM_region_ID,year,roundwood_cons), by = c("GCAM_region_ID","year")) %>%
      mutate(diff=roundwood_cons-Cons_bm3,
             Prod_bm3= Prod_bm3+diff,
             Cons_bm3=roundwood_cons) %>%
      select(colnames(L110.For_ALL_bm3_R_Y))->L110.For_ALL_bm3_R_Y_Primary

    L110.For_ALL_bm3_R_Y %>%
      filter(GCAM_commodity!=aglu.FOREST_SUPPLY_SECTOR) %>%
      bind_rows(L110.For_ALL_bm3_R_Y_Primary)->L110.For_ALL_bm3_R_Y



    # Produce outputs

    # Move this code from L240 to here to reduce dependency ----
    # Back out gross trade using forest export
    # FAO does not provide primary roundwood bilateral trade data. We use export data to back calculate gross trade.
    # replace_na here only affect Taiwan, which we did not have trade data.
    L110.For_ALL_bm3_R_Y %>%
      left_join(
        L100.FAO_For_Exp_m3 %>%
          left_join_error_no_match(A_forest_mapping, by = c("item")) %>%
          mutate(                   # add the forest commodity label
                 value = CONV_M3_BM3 * value,                 # convert the value units from m3 to bm3, had to add this constant to constants.R
                 flow = "GrossExp") %>%
          select(GCAM_region_ID, GCAM_commodity, flow, year, value) %>%
          group_by(GCAM_region_ID, GCAM_commodity, flow, year) %>%
          summarise(value = sum(value)) %>%
          ungroup() %>%
          spread(flow, value),
        by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
      replace_na(list(GrossExp = 0)) %>%
      mutate(GrossImp_Mt = if_else(GrossExp - NetExp_bm3 > 0, GrossExp - NetExp_bm3, 0),
             GrossExp_Mt = if_else(GrossExp - NetExp_bm3 > 0, GrossExp, NetExp_bm3)) %>%
      select(-GrossExp) ->
      L110.For_ALL_bm3_R_Y


    # Produce outputs ----
    L110.For_ALL_bm3_R_Y %>%
      add_title("Forest products mass balance by GCAM region / year") %>%
      add_units("billion cubic meters (bm3)") %>%
      add_comments("FAO production data is used to scale Net Exports (FAO Exports - FAO Imports) at the Region level") %>%
      add_comments("such that Global Production equals Consumption (Production - Net Exports).") %>%
      add_legacy_name("L110.For_ALL_bm3_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_For_Prod_m3",
                     "L100.FAO_For_Imp_m3",
                     "L100.FAO_For_Exp_m3",
                     "aglu/A_forest_mapping") ->
      L110.For_ALL_bm3_R_Y

    L110.IO_Coefs_pulp %>%
      select(GCAM_region_ID,year,IO) %>%
      add_title("Wood pulp IO coefficients in m3/tonne") %>%
      add_units("m3/tonne") %>%
      add_comments("FAO production data is used to scale Net Exports (FAO Exports - FAO Imports) at the Region level") %>%
      add_comments("such that Global Production equals Consumption (Production - Net Exports).") %>%
      add_legacy_name("L110.IO_Coefs_pulp") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_For_Prod_m3",
                     "L100.FAO_For_Imp_m3",
                     "L100.FAO_For_Exp_m3",
                     "aglu/A_forest_mapping") ->
      L110.IO_Coefs_pulp

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
