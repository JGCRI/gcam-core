#' module_aglu_LB110.For_FAO_R_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L110.For_ALL_bm3_R_Y}. The corresponding file in the
#' original data system was \code{LB110.For_FAO_R_Y.R} (aglu level1).
#' @details This module builds Forest Production, Net Export (FAO Exports - FAO Imports), and
#' Consumption  (Production - Net Exports) information for every GCAM region in each year, from
#' FAO Production, Export, and Import data.FAO Production data is used to scale Net Exports
#' at the Region level such that Global Production equals Consumption.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MC and ACS March 2017
module_aglu_LB110.For_FAO_R_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             "L100.FAO_For_Prod_m3",
             "L100.FAO_For_Imp_m3",
             "L100.FAO_For_Exp_m3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L110.For_ALL_bm3_R_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L100.FAO_For_Prod_m3 <- get_data(all_data, "L100.FAO_For_Prod_m3")
    L100.FAO_For_Imp_m3 <- get_data(all_data, "L100.FAO_For_Imp_m3")
    L100.FAO_For_Exp_m3 <- get_data(all_data, "L100.FAO_For_Exp_m3")




    # Lines 34-39 in original file
    # indicate flow on each tibble - flow is a directional quantity indicating net export or production
    # Old comment: 2. Perform computations
    # Old comment: Indicate the flow on each table and combine (rbind). Multiply imports by -1 and call both imports and exports the same flow
    L100.FAO_For_Prod_m3 %>%    # take the production tibble
      mutate(flow="Prod_m3") -> # add the flow column and make every entry = "Prod_m3"
      L100.FAO_For_Prod_m3      # store in the production tibble

    L100.FAO_For_Exp_m3 %>%       # take the export tibble
      mutate(flow="NetExp_m3") -> # add the flow identifier because this is a component of net export = export - import
      L100.FAO_For_Exp_m3         # store in the export tibble

    # for imports, we also multiply value by -1, to represent them as a negative export, making
    # it easier to sum up exports-imports later
    L100.FAO_For_Imp_m3 %>%         # take the import tibble
      mutate(flow="NetExp_m3") %>%  # add the flow identifier because this is a component of net export = export - import
      mutate(value=-value) ->       # make the values negative so that it gets subtracted when we aggregate by flow=NetExp
      L100.FAO_For_Imp_m3           # store in the import tibble

    # combine all three L100.FAO_For_X tibbles into a single tibble
    # this tibble will be summed over by flow later, calculating export-import to give net export
    L110.FAO_For_ALL_m3 <- rbind( L100.FAO_For_Prod_m3, L100.FAO_For_Imp_m3, L100.FAO_For_Exp_m3 )


    # Lines 41-60 in original file
    # Add regionID lookup vectors and GCAM_commodity label to the For_ALL tibble
    # convert units and aggregate over flow to form Production and Net Export in units of bm3
    # Use spread to have columns for Prod and NetExp instead of flow and value
    # Add a column for the variable consumption, Cons=Prod-NetExp
    L110.FAO_For_ALL_m3 %>%                                   # take the combined tibble
      # do a left join on For_ALL tibble, match up the iso labels from the iso tibble,
      #   This appends to the For_ALL tibble all of the information from the iso_GCAM_regID, including the column we actually
      #   want, GCAM_region_ID. This column is all that we save:
      mutate(GCAM_region_ID =  left_join( L110.FAO_For_ALL_m3, iso_GCAM_regID, by = c("iso"))$GCAM_region_ID ) %>%
      #
      mutate(GCAM_commodity = "Forest") %>%                   # add the forest commodity label
      mutate(value = CONV_M3_BM3*value) %>%                   # convert the value units from m3 to bm3, had to add this constant to constants.R
      mutate(flow = sub( "_m3", "_bm3", flow ) )  %>%         # update the labels in flow to reflect the new units of bm3
      #
      #we don't care about other identifiers anymore, only region id, commodity, flow and year corresponding to value:
      select(GCAM_region_ID, GCAM_commodity, flow, year, value) %>%
      #
      # these two lines (group_by and summarise) compute Export-Import to give net exports, and leave the Prod alone,
      #   for each commodity (just forest) in each GCAM region:
      group_by(GCAM_region_ID, GCAM_commodity, flow,year) %>%
      summarise(value = sum(value)) %>%
      #
      # build volumetric mass balance tibble:
      #   instead of having flow and value for each year, region, commodity, we now want NetExp and Prod columns for each year, region, commodity.
      #   The new DSR eliminates the use of L110.For_ALL_bm3_R_Y.prelim
      spread(flow, value) %>%
      #
      mutate(Cons_bm3 = Prod_bm3 - NetExp_bm3) %>%                                     # form a new variable, consumption Cons = Prod-NetExp
      select(GCAM_region_ID, GCAM_commodity, year, Prod_bm3, NetExp_bm3, Cons_bm3) ->  # reorder columns
      L110.For_ALL_bm3_R_Y                                                             # save it in the region year R_Y tibble


    # Lines 62 - 64 in original file
    # Form Global values by summing over regions for each Prod, NetExp, Cons in tibble L110.For_ALL_bm3_R_Y;
    # Then use the Global values to calculate the global consumption scaler that satisfies Global Production = Consumption
    L110.For_ALL_bm3_R_Y %>%                     # take the region year R_Y tibble
      group_by(GCAM_commodity, year) %>%         # For each commodity and year
      summarise_all(sum) %>%                     # sum over each remaining column
      select(-GCAM_region_ID) %>%                # drop the region id, since we are calculating a global quantity
      mutate(Cons_scaler = Prod_bm3/Cons_bm3) -> # add a variable for the consumption scaler
      L110.For_ALL_bm3_glbl_Y                    # store in the global tibble


    # Lines 65-71 in original file
    # Use the global consumption scaler in tibble L110.For_ALL_bm3_glbl_Y  to scale regions
    L110.For_ALL_bm3_glbl_Y %>%
      select(year,GCAM_commodity, Cons_scaler) ->
      dummyGlobalTable
    L110.For_ALL_bm3_R_Y %>%                                               # take the region year R_Y tibble
      left_join(., dummyGlobalTable, by = c("year","GCAM_commodity")) %>%
      # ^ Use Left Join to add the Global Cons_scaler corresponding to each year in each region. This command adds
      #   more information than we need or care about, so only keep the Cons_scaler column that matters
      mutate(Cons_bm3 = Cons_bm3 * Cons_scaler)  %>%                       # Use the scaler to recalculate Cons_bm3
      mutate(NetExp_bm3 = Prod_bm3 - Cons_bm3)   %>%                       # and recalculate NetExp from the new Cons_bm3
      ###printlog( "Building adjusted forest mass balance table" )
      select(-Cons_scaler) ->                                              # remove the Cons_scaler column to form the adjusted mass balance tibble
      L110.For_ALL_bm3_R_Y                                                 # store in the region year R_Y tibble


    # Lines 74-78 in original file
    # old comment: Translate to full table for any regions with no forest data
    L110.For_ALL_bm3_R_Y %>%                                                   # take the region year R_Y table
      tidyr::complete( GCAM_region_ID = unique( iso_GCAM_regID$GCAM_region_ID),# use complete to make sure every commodity (forest)
                tidyr::nesting(GCAM_commodity, year),                          # and year is represented for every GCAM region.
                fill = list(Prod_bm3=0, NetExp_bm3 = 0, Cons_bm3 = 0) ) ->     # Fill in the new regions with 0 Prod, NetExp, Cons.
      L110.For_ALL_bm3_R_Y                                                     # store in the R_Y table.

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L110.For_ALL_bm3_R_Y %>%
      add_title("Forest products mass balance by GCAM region / year") %>%
      add_units("bm3") %>%
      add_comments( "FAO production data is used to scale Net Exports (FAO Exports - FAO Imports) at the Region level" ) %>%
      add_comments("such that Global Production equals Consumption (Production - Net Exports).") %>%
      add_legacy_name("L110.For_ALL_bm3_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_For_Prod_m3",
                     "L100.FAO_For_Imp_m3",
                     "L100.FAO_For_Exp_m3") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L110.For_ALL_bm3_R_Y

    return_data(L110.For_ALL_bm3_R_Y)
  } else {
    stop("Unknown command")
  }
}


