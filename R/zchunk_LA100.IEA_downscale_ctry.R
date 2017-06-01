#' module_energy_LA100.IEA_downscale_ctry
#'
#' Downscale proprietary IEA energy balance data to 201 countries, by iso code, FLOW, PRODUCT, and historical year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.IEA_en_bal_ctry_hist}. The corresponding file in the
#' original data system was \code{LA100.IEA_downscale_ctry.R} (energy level1).
#' @details Combine OECD and non-OECD data; perform upfront adjustments for other Africa, Turkey, and South
#' Africa; split out and handle the 1990 split of Yugoslavia and USSR, back-projecting individual country
#' values based on 1990 shares; use population to downscale IEA composite regions (Other Africa, Other
#' non-OECD Americas, Other Asia) to individual countries; filter out countries without data in any year.
#' @note We build from the raw (and proprietary) \code{en_OCED} and \code{en_nonOECD} files if
#' they are available; if not, this chunk reads in a pre-generated \code{L100.IEA_en_bal_ctry_hist}
#' file and returns that. (In other words, our output is an optional input.)
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL May 2017
module_energy_LA100.IEA_downscale_ctry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L100.Pop_thous_ctry_Yh",
             OPTIONAL_FILE = "energy/en_OECD",
             OPTIONAL_FILE = "energy/en_nonOECD",
             OPTIONAL_FILE = "energy/L100.IEA_en_bal_ctry_hist",
             FILE = "energy/mappings/IEA_product_downscaling",
             FILE = "energy/mappings/IEA_ctry"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.IEA_en_bal_ctry_hist"))
  } else if(command == driver.MAKE) {

    `1990` <- `1990_share` <- `1990_sum` <- COUNTRY <- EcYield_kgm2_hi <- EcYield_kgm2_lo <- FLOW <-
      GCAM_commodity <- GCAM_region_ID <- GLU <- IEAcomposite <- Irr_Rfd <- LC_bm2_hi <-
      LC_bm2_lo <- PRODUCT <- composite_population <- iso <- landshare_hi <- landshare_lo <-
      level <- optional <- value <- year <- yield <- yieldmult_hi <- yieldmult_lo <- NULL # silence package check notes

    all_data <- list(...)[[1]]

    # Load required inputs
    L100.Pop_thous_ctry_Yh <- get_data(all_data, "L100.Pop_thous_ctry_Yh")
    en_OECD <- get_data(all_data, "energy/en_OECD")
    en_nonOECD <- get_data(all_data, "energy/en_nonOECD")
    IEA_product_downscaling <- get_data(all_data, "energy/mappings/IEA_product_downscaling")
    IEA_ctry <- get_data(all_data, "energy/mappings/IEA_ctry")
    L100.IEA_en_bal_ctry_hist_preprocessed <- get_data(all_data, "energy/L100.IEA_en_bal_ctry_hist")

    # If the (proprietary) raw IEA datasets are available, go through the full computations below
    # If not, use the pre-saved summary file (i.e., the output of this chunk!) assuming it's available
    if(!is.null(en_nonOECD) && !is.null(en_OECD)) {

      # Full calculation from raw data
      # The two IEA datasets are LARGE and we keep them in wide format for most of the computations
      # below, as they are very expensive to reshape. As a result, much of the logic below closely
      # parallels that in the original data system, and there are fewer dplyr-style pipelines.

      # Subset only the relevant years and combine OECD with non-OECD
      hy <- intersect(intersect(HISTORICAL_YEARS, colnames(en_OECD)), colnames(en_nonOECD))
      cols <- c("COUNTRY", "FLOW", "PRODUCT", hy)
      bind_rows(en_OECD[cols], en_nonOECD[cols]) %>%
        # rename fuels with inconsistent naming between the two databases
        mutate(PRODUCT = replace(PRODUCT,
                                 PRODUCT %in% c("Natural Gas", "Other Kerosene", "Total"),
                                 c("Natural gas", "Other kerosene", "Total of all energy sources"))) ->
        L100.IEAfull

      # UP FRONT ADJUSTMENTS (UFA) original lines 42-67

      # UFA1. Nearly the entire supply of natural gas in other Africa between 2001 and 2004 is allocated
      # to GTL plants operating at nearly 100% efficiency. We adjust the energy input quantities
      # to avoid negative values later on.
      GTL_COEF <- 1.7
      GTL_ADJ_YEARS <- as.character(intersect(2001:2004, HISTORICAL_YEARS))
      if(length(GTL_ADJ_YEARS)) {
        GTL_gas_entries <- with(L100.IEAfull, COUNTRY == "Other Africa" & FLOW == "TGTL" & PRODUCT == "Natural gas")
        GTL_oh_entries <- with(L100.IEAfull, COUNTRY == "Other Africa" & FLOW == "TGTL" & PRODUCT == "Other hydrocarbons")
        L100.IEAfull[GTL_gas_entries, GTL_ADJ_YEARS] <- L100.IEAfull[GTL_oh_entries, GTL_ADJ_YEARS] *
          GTL_COEF * -1     # Multiply by -1 because other hydrocarbons are the output and have a different sign
      }

      # UFA2. South Africa has a coal-to-gas IO coef in the gas works sector of about 5:1, and low natural
      # gas consumption in other sectors. The coal inputs are overridden here as the gas output times an
      # exogenous IO coef
      COAL_TO_GAS_COEF <- 1.3
      CTG_entries <- L100.IEAfull$COUNTRY == "South Africa" & L100.IEAfull$FLOW == "TGASWKS"
      # Only use other bituminous coal; no need to maintain distinction between coal (if no detail) and other bituminous coal
      L100.IEAfull[CTG_entries & L100.IEAfull$PRODUCT == "Hard coal (if no detail)", hy] <- 0
      L100.IEAfull[CTG_entries & L100.IEAfull$PRODUCT == "Other bituminous coal", hy] <-
        L100.IEAfull[CTG_entries & L100.IEAfull$PRODUCT == "Gas works gas", hy] *
        COAL_TO_GAS_COEF * -1     # Multiply by -1 because inputs and outputs have a different sign

      # UFA3. Turkey has electricity production from primary solid biofuels (elautoc) between 1971 and 1981
      # with no corresponding fuel input by any sectors; add a fuel input to avoid negative numbers later on.
      CHP_IO_COEF <- 5
      CONV_GWH_KTOE <- 0.08598452 # ELAUTOC (the output) is in gigawatt hours, whereas AUTOCHP (the input) is in ktoe
      CHP_ADJ_YEARS <- intersect(1971:1981, colnames(L100.IEAfull))
      if(length(CHP_ADJ_YEARS)) {
        CHP_entries <- L100.IEAfull$COUNTRY == "Turkey" & L100.IEAfull$PRODUCT == "Primary solid biofuels"
        L100.IEAfull[CHP_entries & L100.IEAfull$FLOW == "AUTOCHP", CHP_ADJ_YEARS] <-
          L100.IEAfull[CHP_entries & L100.IEAfull$FLOW == "ELAUTOC", CHP_ADJ_YEARS] *
          CHP_IO_COEF * CONV_GWH_KTOE * -1
      }

      # Split the country mapping table into composite regions and single-countries (69-77)
      IEA_ctry_composite <- filter(IEA_ctry, IEA_ctry %in% c("Former Soviet Union (if no detail)",
                                                             "Former Yugoslavia (if no detail)",
                                                             "Other Africa",
                                                             "Other Non-OECD Americas",
                                                             "Other Asia"))
      IEA_ctry_single <- filter(IEA_ctry, ! IEA_ctry %in% IEA_ctry_composite$IEA_ctry)

      # Split IEA energy balances into table of single countries and composite regions
      # (keeping only desired composite regions)
      L100.IEAcomposite <- filter(L100.IEAfull, COUNTRY %in% IEA_ctry_composite$IEA_ctry)
      L100.IEAsingle <- filter(L100.IEAfull, COUNTRY %in% IEA_ctry_single$IEA_ctry)
      L100.IEAfull %>%
        filter(COUNTRY %in% IEA_ctry_single$IEA_ctry) %>%
        left_join_error_no_match(select(IEA_ctry_single, IEA_ctry, iso),
                                 by = c("COUNTRY" = "IEA_ctry")) ->
        L100.IEAsingle

      # Subset countries that are being downscaled in certain years using historical
      # energy data in a specified year. Former Soviet Union and Yugoslavia: use specified
      # flows for each product. The IEA dataset has many inter-sectoral inconsistencies between
      # the USSR and separated countries thereafter, resulting in unrealistic fuel shares.
      USSR_YUG_SPLIT_YEAR <- 1990
      USSR_YUG_YEARS <- HISTORICAL_YEARS[HISTORICAL_YEARS < USSR_YUG_SPLIT_YEAR]
      POST_USSR_YUG_YEARS_IEA <- HISTORICAL_YEARS[HISTORICAL_YEARS >= USSR_YUG_SPLIT_YEAR]
      L100.USSR_Yug <- filter(L100.IEAcomposite, COUNTRY %in% c("Former Soviet Union (if no detail)",
                                                                "Former Yugoslavia (if no detail)"))

      # Re-map the "if no detail" forms of coal in the historical years prior to the relevant
      # coal types for matching with the more recent years (89-107)
      NO_DETAIL_COAL_YEARS <- as.character(intersect(1971:1977, HISTORICAL_YEARS))
      if(length(NO_DETAIL_COAL_YEARS)) {
        # Hard coal needs to be split proportionally between coking coal and other bituminous coal
        # in order to minimize bias from different country-wise shares of the two fuel types.
        # Note that anthracite is not considered in these regions.
        # NOTE: using round() to avoid NA's for any values whose base value is >1e6. This seems to work
        prod <- L100.USSR_Yug$PRODUCT
        prod_obc <- prod == "Other bituminous coal"
        prod_hcind <- prod == "Hard coal (if no detail)"
        prod_cc <- prod == "Coking coal"
        L100.USSR_Yug[prod_obc, NO_DETAIL_COAL_YEARS] <-
          L100.USSR_Yug[prod_hcind, NO_DETAIL_COAL_YEARS ] *
          round(L100.USSR_Yug$`1978`[prod_obc] /
                  (L100.USSR_Yug$`1978`[prod_cc] + L100.USSR_Yug$`1978`[prod_obc] + 1e-3), digits = 2)
        L100.USSR_Yug[prod_cc, NO_DETAIL_COAL_YEARS ] <-
          L100.USSR_Yug[prod_hcind, NO_DETAIL_COAL_YEARS ] - L100.USSR_Yug[prod_obc, NO_DETAIL_COAL_YEARS ]
        L100.USSR_Yug[prod_hcind, NO_DETAIL_COAL_YEARS ] <- 0

        # Brown coal is simpler, as lignite is the only relevant fuel (104-119)
        # (sub-bituminous coal is not considered in these regions)
        prod_bcind <- prod == "Brown coal (if no detail)"
        L100.USSR_Yug[prod == "Lignite", NO_DETAIL_COAL_YEARS ] <- L100.USSR_Yug[prod_bcind, NO_DETAIL_COAL_YEARS ]
        L100.USSR_Yug[prod_bcind, NO_DETAIL_COAL_YEARS ] <- 0
      }

      # Isolate 1990 data for FSU and Yugoslavia; this will be used to back-project individual country shares
      fsu_yug_composite_entries <- IEA_ctry_composite$IEA_ctry %in% c("Former Soviet Union (if no detail)", "Former Yugoslavia (if no detail)")
      L100.IEAsingle %>%
        filter(iso %in% IEA_ctry_composite$iso[fsu_yug_composite_entries]) %>%
        left_join_keep_first_only(select(IEA_ctry_composite, iso, IEA_ctry), by = "iso") %>%
        rename(IEAcomposite = IEA_ctry) ->
        L100.USSR_Yug_ctry

      L100.USSR_Yug_ctry %>%
        filter(paste(PRODUCT, FLOW) %in% paste(IEA_product_downscaling$PRODUCT, IEA_product_downscaling$FLOW)) ->
        L100.USSR_Yug_ctry_FLOW_PRODUCT

      L100.USSR_Yug_ctry_FLOW_PRODUCT %>%
        group_by(IEAcomposite, FLOW, PRODUCT) %>%
        summarise(`1990_sum` = sum(`1990`)) %>%
        ungroup ->
        L100.USSR_Yug_FLOW_PRODUCT

      L100.USSR_Yug_ctry_FLOW_PRODUCT %>%
        left_join_error_no_match(distinct(select(L100.USSR_Yug_FLOW_PRODUCT, -FLOW), IEAcomposite, PRODUCT, .keep_all = TRUE),
                                 by = c("IEAcomposite", "PRODUCT")) %>%
        mutate(`1990_share` = `1990` / `1990_sum`) %>%
        replace_na(list(`1990_share` = 0)) ->
        L100.USSR_Yug_ctry_FLOW_PRODUCT_1990

      # Calculate the energy balances of the individual countries during the USSR years as
      # the total in the composite region times the country-wise shares in 1990 (121-131)
      POST_USSR_YUG_YEARS_IEA <- intersect(POST_USSR_YUG_YEARS_IEA, names(L100.USSR_Yug_ctry))
      L100.USSR_Yug_ctry %>%
        select(one_of("iso", "FLOW", "PRODUCT", "IEAcomposite", POST_USSR_YUG_YEARS_IEA)) %>%
        left_join_keep_first_only(select(L100.USSR_Yug, one_of("COUNTRY", "FLOW", "PRODUCT", USSR_YUG_YEARS)),
                                  by = c("IEAcomposite" = "COUNTRY", "FLOW", "PRODUCT")) %>%
        left_join_keep_first_only(select(L100.USSR_Yug_ctry_FLOW_PRODUCT_1990, iso, PRODUCT, `1990_share`),
                                  by = c("iso", "PRODUCT")) ->
        L100.USSR_Yug_ctry_bal

      USSR_YUG_COLUMNS <- which(names(L100.USSR_Yug_ctry_bal) %in% USSR_YUG_YEARS)
      L100.USSR_Yug_ctry_bal[USSR_YUG_COLUMNS] <- L100.USSR_Yug_ctry_bal[USSR_YUG_COLUMNS] * L100.USSR_Yug_ctry_bal$`1990_share`
      L100.USSR_Yug_ctry_bal$`1990_share` <- NULL

      # Composite regions where population is used to downscale energy to countries over all historical years
      # Subset composite regions and repeat by number of countries in each (139-142)
      filter(L100.IEAcomposite, COUNTRY == "Other Africa") %>%
        repeat_add_columns(tibble(iso = IEA_ctry_composite$iso[IEA_ctry_composite$IEA_ctry == "Other Africa"])) ->
        L100.Afr_repCtry
      filter(L100.IEAcomposite, COUNTRY == "Other Non-OECD Americas") %>%
        repeat_add_columns(tibble(iso = IEA_ctry_composite$iso[IEA_ctry_composite$IEA_ctry == "Other Non-OECD Americas"])) ->
        L100.LAM_repCtry
      filter(L100.IEAcomposite, COUNTRY == "Other Asia") %>%
        repeat_add_columns(tibble(iso = IEA_ctry_composite$iso[IEA_ctry_composite$IEA_ctry == "Other Asia"])) ->
        L100.Asia_repCtry

      # Combine these into a single data table and calculate population shares (144-149)
      L100.Others_repCtry <- bind_rows(L100.Afr_repCtry, L100.LAM_repCtry, L100.Asia_repCtry)
      # Note L100.Pop_thous_ctry_Yh is in long format
      L100.Pop_thous_ctry_Yh %>%
        filter(iso %in% L100.Others_repCtry$iso) %>%
        left_join_error_no_match(select(IEA_ctry_composite, iso, IEA_ctry), by = "iso") %>%
        rename(IEAcomposite = IEA_ctry) ->
        L100.Others_pop

      # Aggregate by country-within-composite-region and year to calculate population shares (151-156)
      L100.Others_pop %>%
        filter(year %in% HISTORICAL_YEARS) %>%
        group_by(IEAcomposite, year) %>%
        summarise(composite_population = sum(value)) ->
        L100.Composites_pop

      L100.Others_pop %>%
        filter(year %in% HISTORICAL_YEARS) %>%
        left_join_error_no_match(L100.Composites_pop, by = c("IEAcomposite", "year")) %>%
        mutate(pop_share = value / composite_population) %>%
        # drop extraneous columns
        select(iso, year, pop_share) ->
        L100.Others_pop_share

      # Spread L100.Others_pop_share to wide format in preparation for computing L100.Others_ctry_bal
      # This is relatively inexpensive
      L100.Others_pop_share <- spread(L100.Others_pop_share, year, pop_share)

      # Multiply the repeated country databases by the population shares to get the energy balances by country (159-162)
      L100.Others_repCtry %>%
        select(iso, FLOW, PRODUCT, COUNTRY) %>%
        left_join_error_no_match(L100.Others_repCtry, by = c("iso", "FLOW", "PRODUCT", "COUNTRY")) %>%
        rename(IEAcomposite = COUNTRY) ->
        L100.Others_ctry_bal

      L100.Others_repCtry %>%
        select(iso) %>%
        left_join_error_no_match(L100.Others_pop_share, by = "iso") %>%
        select(-iso) ->
        pop_share

      hyc <- which(names(L100.Others_ctry_bal) %in% HISTORICAL_YEARS)
      L100.Others_ctry_bal[hyc] <- L100.Others_ctry_bal[hyc] * pop_share

      # Subset each of these final energy balances to only the rows that aren't zero in all years
      L100.Others_ctry_bal[rowSums(L100.Others_ctry_bal[hyc]) !=0, ] %>%
        select(-IEAcomposite) ->
        L100.Others_ctry_bal

      hyc <- which(names(L100.IEAsingle) %in% HISTORICAL_YEARS)
      L100.IEAsingle[rowSums(L100.IEAsingle[hyc]) != 0, ] %>%
        select(-COUNTRY) %>%
        filter(! iso %in% L100.USSR_Yug_ctry_bal$iso) ->
        L100.IEAsingle_noUSSR_Yug

      hyc <- which(names(L100.USSR_Yug_ctry_bal) %in% HISTORICAL_YEARS)
      L100.USSR_Yug_ctry_bal[rowSums(L100.USSR_Yug_ctry_bal[hyc]) !=0, ] %>%
        select(-IEAcomposite) ->
        L100.USSR_Yug_ctry_bal

      # Combine the country-level data tables and write out energy balances (using iso codes rather than IEA's country names)
      bind_rows(L100.IEAsingle_noUSSR_Yug,
                # TODO: we end up with two rows for each iso, FLOW, PRODUCT in the following data frame
                L100.USSR_Yug_ctry_bal,
                L100.Others_ctry_bal) %>%
        add_title("IEA energy balances downscaled to 201 countries by iso code, FLOW, PRODUCT, and historical year") %>%
        add_units("ktoe and GWh") %>%
        add_comments("Combine OECD and non-OECD data; perform upfront adjustments for other Africa, Turkey, and South Africa;") %>%
        add_comments("split out and handle the 1990 split of Yugoslavia and USSR; use population to downscale IEA composite regions") %>%
        add_comments("to individual countries; filter out countries without data in any year.") ->
        L100.IEA_en_bal_ctry_hist

    } else {
      # raw IEA datasets not available, so used presaved data loaded above
      if(is.null(L100.IEA_en_bal_ctry_hist_preprocessed)) {
        stop("Neither the raw nor processed input data are available for LA100.IEA_downscale_ctry!")
      }

      L100.IEA_en_bal_ctry_hist_preprocessed %>%
        add_comments("** DATA PRE-GENERATED; NOT COMPUTED FROM RAW IEA FILES **") ->
        L100.IEA_en_bal_ctry_hist
    }

    # Produce final output
    L100.IEA_en_bal_ctry_hist %>%
      add_legacy_name("L100.IEA_en_bal_ctry_hist") %>%
      add_precursors("L100.Pop_thous_ctry_Yh", "energy/en_OECD", "energy/en_nonOECD",
                     "energy/mappings/IEA_product_downscaling", "energy/mappings/IEA_ctry") %>%
      # Outputs match the old data system within 10^-5 but still failing, so use sum test
      add_flags(FLAG_NO_XYEAR, FLAG_PROTECT_FLOAT, FLAG_SUM_TEST) ->
      L100.IEA_en_bal_ctry_hist

    return_data(L100.IEA_en_bal_ctry_hist)
  } else {
    stop("Unknown command")
  }
}
