#' module_energy_LA100.IEA_downscale_ctry
#'
#' Downscale IEA energy balances to 201 countries, by iso, FLOW, PRODUCT, and historical year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.IEA_en_bal_ctry_hist}. The corresponding file in the
#' original data system was \code{LA100.IEA_downscale_ctry.R} (energy level1).
#' @details We build from the raw (and proprietary) \code{en_OCED} and \code{en_nonOECD} files if
#' they are available; if not, this chunk reads in a pre-generated \code{L100.IEA_en_bal_ctry_hist}
#' file and returns that. (In other words, our output is an optional input.)
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL May 2017
#' @export
module_energy_LA100.IEA_downscale_ctry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L100.Pop_thous_ctry_Yh",
              OPTIONAL_FILE = "energy/en_OECD",
              OPTIONAL_FILE = "energy/en_nonOECD",
              OPTIONAL_FILE = "energy/L100.IEA_en_bal_ctry_hist",
              FILE = "energy/mappings/IEA_product_downscaling",
              FILE = "energy/mappings/IEA_ctry"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.IEA_en_bal_ctry_hist"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L100.Pop_thous_ctry_Yh <- get_data(all_data, "L100.Pop_thous_ctry_Yh")
    en_OECD <- get_data(all_data, "energy/en_OECD")
    en_nonOECD <- get_data(all_data, "energy/en_nonOECD")
    IEA_product_downscaling <- get_data(all_data, "energy/mappings/IEA_product_downscaling")
    IEA_ctry <- get_data(all_data, "energy/mappings/IEA_ctry")
    L100.IEA_en_bal_ctry_hist <- get_data(all_data, "energy/L100.IEA_en_bal_ctry_hist")

    # If the (proprietary) raw IEA datasets are available, go through the full computations below
    # If not, use the pre-saved summary file (i.e., the output of this chunk!) assuming it's available
    if(!is.na(en_nonOECD) && !is.na(en_OECD)) {

      # Full calculation from raw data
      # The two IEA datasets are LARGE and we keep them in wide format for most of the computations
      # below, as they are very expensive to reshape. As a result, much of the logic below closely
      # parallels that in the original data system, and there's relatively few dplyr-style pipelines.

      # Subset only the relevant years and combine OECD with non-OECD
      hy <- as.character(HISTORICAL_YEARS)
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
      GTL_ADJ_YEARS <- as.character(2001:2004)
      GTL_entries <- L100.IEAfull$COUNTRY == "Other Africa" & L100.IEAfull$FLOW == "TGTL" & L100.IEAfull$PRODUCT == "Natural gas"
      L100.IEAfull[GTL_entries, GTL_ADJ_YEARS] <- L100.IEAfull[GTL_entries, GTL_ADJ_YEARS] *
        GTL_COEF * -1     # Multiply by -1 because other hydrocarbons are the output and have a different sign

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
      CHP_ADJ_YEARS <- as.character(1971:1981)
      CHP_entries <- L100.IEAfull$COUNTRY == "Turkey" & L100.IEAfull$PRODUCT == "Primary solid biofuels"
      L100.IEAfull[CHP_entries & L100.IEAfull$FLOW == "AUTOCHP", CHP_ADJ_YEARS] <-
        L100.IEAfull[CHP_entries & L100.IEAfull$FLOW == "ELAUTOC", CHP_ADJ_YEARS] *
        CHP_IO_COEF * CONV_GWH_KTOE * -1

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
        left_join_error_no_match(IEA_ctry_single, by = c("COUNTRY" = "IEA_ctry")) ->
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
      NO_DETAIL_COAL_YEARS <- as.character(1971:1977)

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


      fsu_yug_composite_entries <- IEA_ctry_composite$IEA_ctry %in% c("Former Soviet Union (if no detail)", "Former Yugoslavia (if no detail)")
      L100.IEAsingle %>%
        filter(iso %in% IEA_ctry_composite$iso[fsu_yug_composite_entries]) %>%
        left_join_keep_first_only(select(IEA_ctry_composite, iso, IEA_ctry), by = "iso") %>%
        rename(IEAcomposite = IEA_ctry) ->
        L100.USSR_Yug_ctry

      L100.USSR_Yug_ctry %>%
        filter(PRODUCT %in% IEA_product_downscaling$PRODUCT, FLOW %in% IEA_product_downscaling$FLOW) ->
        L100.USSR_Yug_ctry_FLOW_PRODUCT

      L100.USSR_Yug_ctry_FLOW_PRODUCT %>%
        group_by(IEAcomposite, FLOW, PRODUCT) %>%
        summarise(`1990_sum` = sum(`1990`)) ->
        L100.USSR_Yug_FLOW_PRODUCT

      if(0) {

        # not working
        # check steps above and give more sensible names?

        L100.USSR_Yug_ctry_FLOW_PRODUCT %>%
          left_join_error_no_match(distinct(L100.USSR_Yug_FLOW_PRODUCT, IEAcomposite, PRODUCT, .keep_all = TRUE),
                                   by = c("IEAcomposite", "PRODUCT")) %>%
          mutate(`1990_share` = `1990` / `1990_sum`) %>%
          replace_na(list(`1990_share` = 0)) ->
          L100.USSR_Yug_ctry_FLOW_PRODUCT

        # Calculate the energy balances of the individual countries during the USSR years as
        # the total in the composite region times the country-wise shares in 1990 (121-131)
        L100.USSR_Yug_ctry %>%
          select_(c("iso", "FLOW", "PRODUCT", "IEAcomposite", USSR_YUG_YEARS)) %>%
          left_join_error_no_match(L100.USSR_Yug) ->
          L100.USSR_Yug_ctry_bal

        # Composite regions where population is used to downscale energy to countries over all historical years
        # Subset composite regions
        L100.Afr <- filter(L100.IEAcomposite, COUNTRY == "Other Africa")
        L100.LAM <- filter(L100.IEAcomposite, COUNTRY == "Other Non-OECD Americas")
        L100.Asia <- filter(L100.IEAcomposite, COUNTRY == "Other Asia")

        # Repeat by number of countries in each (139-142)
        L100.Afr_repCtry <- repeat_and_add_vector( L100.Afr, "iso", IEA_ctry_composite$iso[ IEA_ctry_composite$IEA_ctry == "Other Africa" ] )
        L100.LAM_repCtry <- repeat_and_add_vector( L100.LAM, "iso", IEA_ctry_composite$iso[ IEA_ctry_composite$IEA_ctry == "Other Non-OECD Americas" ] )
        L100.Asia_repCtry <- repeat_and_add_vector( L100.Asia, "iso", IEA_ctry_composite$iso[ IEA_ctry_composite$IEA_ctry == "Other Asia" ] )

        # Combine these into a single data table and calculate population shares (144-149)
        L100.Others_repCtry <- bind_rows(L100.Afr_repCtry, L100.LAM_repCtry, L100.Asia_repCtry)
        L100.Pop_thous_ctry_Yh %>%
          filter(iso %in% L100.Others_repCtry$iso) %>%
          left_join_error_no_match(select(IEA_ctry_composite, iso, IEA_ctry), by = "iso") ->
          L100.Others_pop

        # Aggregate by country-within-composite-region and year to calculate population shares (151-156)

        L100.Composites_pop <- aggregate( L100.Others_pop[ X_historical_years ], by=as.list( L100.Others_pop[ "IEAcomposite" ] ), sum )
        L100.Others_pop_share <- L100.Others_pop[ "iso" ]
        L100.Others_pop_share[ X_historical_years ] <-  L100.Others_pop[ X_historical_years ] / L100.Composites_pop[
          match( L100.Others_pop$IEAcomposite, L100.Composites_pop$IEAcomposite ),
          X_historical_years ]


      }

      # End

      L100.IEA_en_bal_ctry_hist <- tibble()

    } else {
      # raw IEA datasets not available, so used presaved data loaded above
      if(is.na(L100.IEA_en_bal_ctry_hist)) {
        stop("Neither the raw nor processed input data are available!")
      }
      L100.IEA_en_bal_ctry_hist <- add_comments(L100.IEA_en_bal_ctry_hist, "** DATA PRE-GENERATED; NOT COMPUTED FROM RAW IEA FILES **")
    }

    # Produce final output
    L100.IEA_en_bal_ctry_hist %>%
      add_title("IEA energy balances downscaled to 201 countries (iso / FLOW / PRODUCT / historical year)") %>%
      add_units("ktoe and GWh") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L100.IEA_en_bal_ctry_hist") %>%
      add_precursors("L100.Pop_thous_ctry_Yh", "energy/en_OECD", "energy/en_nonOECD",
                     "energy/mappings/IEA_product_downscaling", "energy/mappings/IEA_ctry") ->
      L100.IEA_en_bal_ctry_hist

    return_data(L100.IEA_en_bal_ctry_hist)
  } else {
    stop("Unknown command")
  }
}
