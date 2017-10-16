#' module_gcam.usa_L2322.Fert_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2322.DeleteSubsector_USAFert}, \code{L2322.FinalEnergyKeyword_USAFert}, \code{L2322.FinalEnergyKeyword_Fert_USA}, \code{L2322.StubTech_Fert_USA}, \code{L2322.SubsectorLogit_USAFert}, \code{L2322.SubsectorShrwtFllt_USAFert}, \code{L2322.TechShrwt_USAFert}, \code{L2322.Production_USAFert}, \code{L2322.TechCoef_USAFer}, \code{L2322.StubTechProd_Fert_USA}, \code{L2322.StubTechCoef_Fert_USA}, \code{L2322.StubTechMarket_Fert_USA}, \code{L2322.SubsectorLogit_Fert_USA}, \code{L2322.Supplysector_Fert_USA}, \code{L2322.SubsectorShrwtFllt_Fert_USA}, \code{L2322.SubsectorInterp_Fert_USA},\code{L2322.SubsectorInterp_USAFert}
#'  The corresponding file in the original data system was \code{L2322.Fert_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L2322.Fert_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A322.globaltech_coef",
             "L2322.Supplysector_Fert",
             "L2322.FinalEnergyKeyword_Fert",
             "L2322.SubsectorLogit_Fert",
             "L2322.SubsectorShrwtFllt_Fert",
             "L2322.SubsectorInterp_Fert",
             "L2322.StubTech_Fert",
             "L1322.IO_GJkg_state_Fert_F_Yh",
             "L1322.out_Mt_state_Fert_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2322.DeleteSubsector_USAFert",
             "L2322.FinalEnergyKeyword_USAFert",
             "L2322.FinalEnergyKeyword_Fert_USA",
             "L2322.StubTech_Fert_USA",
             "L2322.SubsectorLogit_USAFert",
             "L2322.SubsectorShrwtFllt_USAFert",
             "L2322.TechShrwt_USAFert",
             "L2322.Production_USAFert",
             "L2322.TechCoef_USAFert",
             "L2322.StubTechProd_Fert_USA",
             "L2322.StubTechCoef_Fert_USA",
             "L2322.StubTechMarket_Fert_USA",
             "L2322.SubsectorLogit_Fert_USA",
             "L2322.Supplysector_Fert_USA",
             "L2322.SubsectorShrwtFllt_Fert_USA",
             "L2322.SubsectorInterp_Fert_USA",
             "L2322.SubsectorInterp_USAFert"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A322.globaltech_coef <- get_data(all_data, "energy/A322.globaltech_coef")
    L2322.Supplysector_Fert <- get_data(all_data, "L2322.Supplysector_Fert")
    L2322.FinalEnergyKeyword_Fert <- get_data(all_data, "L2322.FinalEnergyKeyword_Fert")
    L2322.SubsectorLogit_Fert <- get_data(all_data, "L2322.SubsectorLogit_Fert")
    L2322.SubsectorShrwtFllt_Fert <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert")
    L2322.SubsectorInterp_Fert <- get_data(all_data, "L2322.SubsectorInterp_Fert")
    L2322.StubTech_Fert <- get_data(all_data, "L2322.StubTech_Fert")
    L1322.IO_GJkg_state_Fert_F_Yh <- get_data(all_data, "L1322.IO_GJkg_state_Fert_F_Yh")
    L1322.out_Mt_state_Fert_Yh <- get_data(all_data, "L1322.out_Mt_state_Fert_Yh")

    # Silence pacakge checks
    regions <- supplysector <- subsector <- state <- value <- year <- subs.share.weights <-
      technology <- share.weight.year <- minicam.energy.input <- coefficient <- market.name <-
      sector <- fuel <- stub.technology <- grid_region <- NULL


    # ===================================================

    # add some commenets about how those two things were removed from the upstreadm chunk
    # as per discussion with Page and Kate.

    # In the GCAM region USA N fertilizer is retained as a sector, as is the Imports subsector
    # but the the fuel subsecotrs will be deleted and replaced with state subsectors. Subset the
    # subsector logit exponents of fertilizer sector for the fuel seubsecotrs to be removed in
    # GCAM-USA.
    L2322.SubsectorLogit_Fert %>%
      filter(region == "USA", supplysector == aglu.FERT_NAME, subsector != "Imports") %>%
      mutate(region = region) %>%
      select(region, supplysector, subsector) ->
      L2322.DeleteSubsector_USAFert

    # Subset the supply sector keywords for fertilizer sector in the USA region.
    L2322.FinalEnergyKeyword_Fert %>%
      filter(region == "USA") %>%
      mutate(`final.energy` = "none") ->
      L2322.FinalEnergyKeyword_USAFert


    # Since N fertilizer sectors are only created in states where the NAICS shipping information
    # indicates fertlizer production, create a tibble of the fertlizer producing states. This
    # tibble will be used to create the N fertlizer tables for GCAM-USA.
    L1322.out_Mt_state_Fert_Yh %>%
      select(state) %>%
      distinct ->
      Fert_states

    # Select the supply sector information for fertilizer sector for the US and expand to all of the
    # sates that are fterlizer producers then create subsector from state and fertlizer name.
    L2322.Supplysector_Fert %>%
      filter(region == "USA", supplysector == aglu.FERT_NAME) %>%
      select(region, supplysector) %>%
      repeat_add_columns(Fert_states) %>%
      mutate(subsector = paste(state, aglu.FERT_NAME)) ->
      L2322.Supplysector_Fert_states

    # Now add the logit table information to the state feterlizer supply sector data frame.
    L2322.Supplysector_Fert_states %>%
      mutate(logit.year.fillout = min(MODEL_YEARS)) %>%
      mutate(logit.exponent = gcamusa.FERT_LOGIT_EXP) %>%
      mutate("logit.type" = NA) %>%
      select("region", "supplysector", "subsector", "logit.year.fillout", "logit.exponent") ->
      L2322.SubsectorLogit_USAFert

    # Create the subsector default shareweights for the US.
    L2322.SubsectorLogit_USAFert %>%
      select("region", "supplysector", "subsector") %>%
      mutate(year.fillout = min(BASE_YEARS)) %>%
      mutate(share.weight = 1) ->
      L2322.SubsectorShrwtFllt_USAFert

    # Create the subsector default tecnhology shareweights for the US
    # for interplation.
    L2322.SubsectorLogit_USAFert %>%
      select(region, supplysector, subsector) %>%
      mutate(`apply.to` = "share-weight") %>%
      mutate(`from.year` = max(BASE_YEARS)) %>%
      mutate(`to.year` = max(MODEL_YEARS)) %>%
      mutate(`interpolation.function` = "fixed") ->
      L2322.SubsectorInterp_USAFert

    # Create the
    L2322.SubsectorLogit_USAFert %>%
      select("region", "supplysector", "subsector") %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) ->
      L2322.TechShrwt_USAFert

    # printlog( "L2322.Production_USAFert: calibrated production in USA region fertilizer sector (consuming output of states)" )
    L1322.out_Mt_state_Fert_Yh %>%
      mutate(region = "USA") %>%
      mutate(calOutputValue = signif(value, DIGITS_LAND_USE)) %>%
      select(-value) %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(supplysector = aglu.FERT_NAME) %>%
      unite(subsector, state, supplysector, sep = " ", remove = FALSE) ->
      L2322.Production_USAFert

    L2322.Production_USAFert %>%
      mutate(technology = subsector) %>%
      mutate(input = aglu.FERT_NAME) %>%
      mutate(share.weight.year = year) %>%
      mutate(subs.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      mutate(tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, subsector, technology, year, calOutputValue,
             `share.weight.year`, `subs.share.weight`,
             `tech.share.weight`) ->
      L2322.Production_USAFert

    # printlog( "L2322.TechCoef_USAFert: coefficients of USA region fertilizer" )
    L2322.TechShrwt_USAFert %>%
      mutate(`minicam.energy.input` = aglu.FERT_NAME) %>%
      mutate(coefficient = 1) %>%
      mutate(market.name = substr(start = 1, stop = 2, subsector)) %>%
      select(region, supplysector, subsector, technology, year,
             `minicam.energy.input`, coefficient, `market.name`) ->
      L2322.TechCoef_USAFert

    # From the old data system I have no idea what do to with this nonsense

    Fert_USA_processing <- function(data, Fert_states) {

      # Check to see if the data frame needs to be processed.
      check_df <- filter(data, region == "USA" & supplysector == aglu.FERT_NAME)

      if(nrow(check_df) == 0){
        new_data <- data
      } else {
        # state-level Exports_fertilizer sector should be excluded

        df_names <- names(data)

        data %>%
          filter(region == "USA", supplysector == aglu.FERT_NAME) %>%
          write_to_all_states(names = df_names) %>%
          filter(region %in% Fert_states[["state"]]) ->
          new_df

        # Subsecotr if statement
        check_subsector <- c("subsector" %in% names(new_df))
        if(check_subsector) {
          new_df %>%
            dplyr::filter(grepl("gas", subsector)) ->
            new_df
        }
      } # the if else statement

      return(new_df)
    } # end of the function

    L2322.FinalEnergyKeyword_Fert_USA <- Fert_USA_processing(L2322.FinalEnergyKeyword_Fert, Fert_states)
    L2322.Supplysector_Fert_USA <- Fert_USA_processing(L2322.Supplysector_Fert, Fert_states)
    L2322.SubsectorLogit_Fert_USA <- Fert_USA_processing(L2322.SubsectorLogit_Fert, Fert_states)
    L2322.StubTech_Fert_USA <- Fert_USA_processing(L2322.StubTech_Fert, Fert_states)

    if(exists("L2322.SubsectorShrwtFllt_Fert")){
      L2322.SubsectorShrwtFllt_Fert_USA <- Fert_USA_processing(L2322.SubsectorShrwtFllt_Fert, Fert_states)
      }
    if(exists("L2322.SubsectorInterp_Fert")){
      L2322.SubsectorInterp_Fert_USA <- Fert_USA_processing(L2322.SubsectorInterp_Fert, Fert_states)
      }

    # printlog( "L2322.StubTechProd_Fert_USA: calibrated fertilizer production by state" )
    L1322.out_Mt_state_Fert_Yh %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(calOutputValue = signif(value, digits = aglu.DIGITS_CALOUTPUT)) %>%
      mutate(region = state) ->
      L2322.StubTechProd_Fert_USA

      L2322.StubTechProd_Fert_USA %>%
        left_join_error_no_match(calibrated_techs %>% select(sector, fuel, supplysector, technology, subsector),
                  by = c("fuel", "sector")) ->
        L2322.StubTechProd_Fert_USA

    L2322.StubTechProd_Fert_USA %>%
      mutate(`stub.technology` = technology) %>%
      mutate(`share.weight.year` = year) %>%
      mutate(`subs.share.weight` = if_else(calOutputValue > 0, 1, 0)) %>%
      mutate(`tech.share.weight` = `subs.share.weight`) %>%
      select(region, supplysector, subsector, `stub.technology`, year, calOutputValue, `share.weight.year`, `subs.share.weight`, `tech.share.weight`) ->
      L2322.StubTechProd_Fert_USA

    #printlog( "L2322.StubTechCoef_Fert_USA: coefficients of fertilizer production technologies" )
    #Only historical periods. Need another table to write out the market in the future years
    L1322.IO_GJkg_state_Fert_F_Yh %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(coefficient = signif(value, DIGITS_LAND_USE)) %>%
      mutate(region = state) %>%
      select(-value) ->
      L2322.StubTechCoef_Fert_USA


    L2322.StubTechCoef_Fert_USA %>%
      left_join(calibrated_techs %>% select(supplysector, subsector, technology, `minicam.energy.input`, sector, fuel),
                by = c("fuel", "sector")) ->
      L2322.StubTechCoef_Fert_USA

    L2322.StubTechCoef_Fert_USA %>%
      mutate(`stub.technology` = technology) %>%
      mutate(`market.name` = "USA") %>%
      select(region, supplysector, subsector, `stub.technology`,
             year, `minicam.energy.input`, coefficient, `market.name`) ->
      L2322.StubTechCoef_Fert_USA


    if(gcamusa.USE_REGIONAL_FUEL_MARKETS){

      L2322.StubTechCoef_Fert_USA %>%
        filter(`minicam.energy.input` %in% gcamusa.REGIONAL_FUEL_MARKETS) %>%
        left_join_error_no_match(states_subregions %>% select(state, grid_region),
                                 by = c("region" = "state")) %>%
        select(-`market.name`) %>%
        rename(`market.name` = grid_region) ->
        L2322.StubTechCoef_Fert_USA

    }


  #  printlog( "L2322.StubTechMarket_Fert_USA: market for the fuel inputs to the state fertilizer sectors" )
    L2322.StubTech_Fert_USA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L2322.StubTechMarket_Fert_USA

    L2322.StubTechMarket_Fert_USA %>%
      left_join_error_no_match(A322.globaltech_coef %>%
                                 select(supplysector, subsector, technology, `minicam.energy.input`),
                               by = c("supplysector", "subsector", c("stub.technology" = "technology"))) %>%
      mutate(`market.name` = "USA") ->
      L2322.StubTechMarket_Fert_USA


    if(gcamusa.USE_REGIONAL_FUEL_MARKETS){

      L2322.StubTechMarket_Fert_USA %>%
        filter(`minicam.energy.input` %in% gcamusa.REGIONAL_FUEL_MARKETS) %>%
        select(-`market.name`) %>%
        left_join_error_no_match(states_subregions %>% select(region = state, `market.name` = grid_region),
                                 by = "region") ->
        L2322.StubTechMarket_Fert_USA

    }


    # ===================================================

    # Produce outputs
    L2322.DeleteSubsector_USAFert %>%
      add_title("Subsector logit exponents of fertilizer sector to remove from GCAM-USA") %>%
      add_units("NA") %>%
      add_comments("Subset L2322.SubsectorLogit_Fert for all observation other than subsector Imports and supplysector N fertlizer in the US") %>%
      add_legacy_name("L2322.DeleteSubsector_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "L2322.Supplysector_Fert",
                     "L2322.FinalEnergyKeyword_Fert",
                     "L2322.SubsectorLogit_Fert",
                     "L2322.SubsectorShrwtFllt_Fert",
                     "L2322.SubsectorInterp_Fert",
                     "L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") ->
      L2322.DeleteSubsector_USAFert

    L2322.FinalEnergyKeyword_USAFert %>%
      add_title("Supply sector keywords for fertilizer sector for GCAM USA") %>%
      add_units("NA") %>%
      add_comments("Supply sector keywords for fertilizer from L2322.FinalEnergyKeyword_Fert are subset for the USA region and expanded to all states with fertlizer cenus data.") %>%
      add_legacy_name("L2322.FinalEnergyKeyword_USAFert") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.FinalEnergyKeyword_USAFert

    L2322.FinalEnergyKeyword_Fert_USA %>%
      add_title("") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2322.FinalEnergyKeyword_Fert_USA") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.FinalEnergyKeyword_Fert_USA

    L2322.StubTech_Fert_USA %>%
      add_title("") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2322.StubTech_Fert_USA") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.StubTech_Fert_USA

    L2322.SubsectorLogit_USAFert %>%
      add_title("Subsector logit exponents of fertilizer sector for GCAM USA") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector in the USA region, the subsector logit exponents are expanded for US states with fertlizer census data.") %>%
      add_legacy_name("L2322.SubsectorLogit_USAFert") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.SubsectorLogit_USAFert

    L2322.SubsectorShrwtFllt_USAFert %>%
      add_title("") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_USAFert") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.SubsectorShrwtFllt_USAFert

    L2322.SubsectorInterp_USAFert %>%
      add_title("") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2322.SubsectorInterp_USAFert") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.SubsectorInterp_USAFert

    L2322.TechShrwt_USAFert %>%
      add_title("") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2322.TechShrwt_USAFert") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.TechShrwt_USAFert

    L2322.Production_USAFert %>%
      add_title("") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2322.Production_USAFert") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.Production_USAFert

    L2322.TechCoef_USAFert %>%
      add_title("") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2322.TechCoef_USAFert") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.TechCoef_USAFert

    L2322.StubTechProd_Fert_USA %>%
      add_title("") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2322.StubTechProd_Fert_USA") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.StubTechProd_Fert_USA

    L2322.StubTechCoef_Fert_USA %>%
      add_title("") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2322.StubTechCoef_Fert_USA") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.StubTechCoef_Fert_USA

    L2322.StubTechMarket_Fert_USA %>%
      add_title("") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2322.StubTechMarket_Fert_USA") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.StubTechMarket_Fert_USA

    L2322.SubsectorLogit_Fert_USA %>%
      add_title("") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2322.SubsectorLogit_Fert_USA") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.SubsectorLogit_Fert_USA

    L2322.Supplysector_Fert_USA %>%
      add_title("") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2322.Supplysector_Fert_USA") %>%
      add_precursors("L1322.out_Mt_state_Fert_Yh") ->
      L2322.Supplysector_Fert_USA



    if(exists("L2322.SubsectorShrwtFllt_Fert_USA")) {
      L2322.SubsectorShrwtFllt_Fert_USA %>%
        add_title("") %>%
        add_units("NA") %>%
        add_comments("") %>%
        add_comments("") %>%
        add_legacy_name("L2322.SubsectorShrwtFllt_Fert_USA") %>%
        add_precursors("L1322.out_Mt_state_Fert_Yh") ->
        L2322.SubsectorShrwtFllt_Fert_USA
    } else {
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L2322.SubsectorShrwtFllt_Fert_USA") ->
        L2322.SubsectorShrwtFllt_Fert_USA
    }

    if(exists("L2322.SubsectorInterp_Fert_USA")) {
      L2322.SubsectorInterp_Fert_USA %>%
        add_title("") %>%
        add_units("NA") %>%
        add_comments("") %>%
        add_comments("") %>%
        add_legacy_name("L2322.SubsectorInterp_Fert_USA") %>%
        add_precursors("L1322.out_Mt_state_Fert_Yh") ->
        L2322.SubsectorInterp_Fert_USA
    } else {
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Unitless") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L2322.SubsectorInterp_Fert_USA") ->
        L2322.SubsectorInterp_Fert_USA
    }

    return_data(L2322.DeleteSubsector_USAFert,
                L2322.FinalEnergyKeyword_USAFert, L2322.FinalEnergyKeyword_Fert_USA,
                L2322.StubTech_Fert_USA,
                L2322.SubsectorLogit_USAFert,
                L2322.SubsectorShrwtFllt_USAFert,
                L2322.TechShrwt_USAFert,
                L2322.Production_USAFert,
                L2322.TechCoef_USAFert,
                L2322.StubTechProd_Fert_USA,
                L2322.StubTechCoef_Fert_USA,
                L2322.StubTechMarket_Fert_USA,
                L2322.SubsectorLogit_Fert_USA,
                L2322.Supplysector_Fert_USA,
                L2322.SubsectorShrwtFllt_Fert_USA,
                L2322.SubsectorInterp_Fert_USA,
                L2322.SubsectorInterp_USAFert)
    } else {
      stop("Unknown command")
    }
  }
