#' module_gcam.usa_L2322.Fert_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2322.DeleteSubsector_USAFert}, \code{object}, \code{L2322.FinalEnergyKeyword_USAFert}, \code{L2322.SubsectorLogitTables_USAFert[[ curr_table ]]$data}, \code{L2322.SubsectorLogit_USAFert}, \code{L2322.SubsectorShrwtFllt_USAFert}, \code{L2322.SubsectorInterp_USAFert}, \code{L2322.TechShrwt_USAFert}, \code{L2322.Production_USAFert}, \code{L2322.TechCoef_USAFert}, \code{L2322.StubTechProd_Fert_USA}, \code{L2322.StubTechCoef_Fert_USA}, \code{L2322.StubTechMarket_Fert_USA}. The corresponding file in the
#' original data system was \code{L2322.Fert_USA.R} (gcam-usa level2).
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
          #   "L2322.Supplysector_Fert",
          FILE = "temp-data-inject/L2322.Supplysector_Fert",
          #   "L2322.FinalEnergyKeyword_Fert",
          FILE = "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
          #   "L2322.SubsectorLogit_Fert",
          FILE = "temp-data-inject/L2322.SubsectorLogit_Fert",
          #   "L2322.SubsectorShrwt_Fert", --- FLAG idk if this exsists in the old ds
          #   "L2322.SubsectorShrwtFllt_Fert",
          FILE = "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
          #   "L2322.SubsectorInterp_Fert",
          FILE = "temp-data-inject/L2322.SubsectorInterp_Fert",
          #  "L2322.SubsectorInterpTo_Fert", --- FLAG idk if this exists in the old ds
          #  "L2322.StubTech_Fert",
          FILE = "temp-data-inject/L2322.StubTech_Fert",
             "L1322.IO_GJkg_state_Fert_F_Yh", # product in the new ds
             "L1322.out_Mt_state_Fert_Yh")) # product in the new ds
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2322.DeleteSubsector_USAFert",
             "L2322.FinalEnergyKeyword_USAFert",
             "L2322.SubsectorLogit_USAFert",
             "L2322.SubsectorShrwtFllt_USAFert",
             "L2322.SubsectorInterp_USAFert",
             "L2322.TechShrwt_USAFert",
             "L2322.Production_USAFert",
             "L2322.TechCoef_USAFert",
             "L2322.StubTechProd_Fert_USA",
             "L2322.StubTechCoef_Fert_USA",
             "L2322.StubTechMarket_Fert_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A322.globaltech_coef <- get_data(all_data, "energy/A322.globaltech_coef")

    # L2322.Supplysector_Fert <- get_data(all_data, "L2322.Supplysector_Fert")
    L2322.Supplysector_Fert <- get_data(all_data, "temp-data-inject/L2322.Supplysector_Fert")


    # L2322.FinalEnergyKeyword_Fert <- get_data(all_data, "L2322.FinalEnergyKeyword_Fert")
    L2322.FinalEnergyKeyword_Fert <- get_data(all_data, "temp-data-inject/L2322.FinalEnergyKeyword_Fert")

    # L2322.SubsectorLogit_Fert <- get_data(all_data, "L2322.SubsectorLogit_Fert")
    L2322.SubsectorLogit_Fert <- get_data(all_data, "temp-data-inject/L2322.SubsectorLogit_Fert")

    # L2322.SubsectorShrwtFllt_Fert <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert")
    L2322.SubsectorShrwtFllt_Fert <- get_data(all_data, "temp-data-inject/L2322.SubsectorShrwtFllt_Fert")

    # L2322.SubsectorInterp_Fert <- get_data(all_data, "L2322.SubsectorInterp_Fert")
    L2322.SubsectorInterp_Fert <- get_data(all_data, "temp-data-inject/L2322.SubsectorInterp_Fert")

    # L2322.StubTech_Fert <- get_data(all_data, "L2322.StubTech_Fert")
    L2322.StubTech_Fert <- get_data(all_data, "temp-data-inject/L2322.StubTech_Fert")

    L1322.IO_GJkg_state_Fert_F_Yh <- get_data(all_data, "L1322.IO_GJkg_state_Fert_F_Yh")
    L1322.out_Mt_state_Fert_Yh <- get_data(all_data, "L1322.out_Mt_state_Fert_Yh")

    # ===================================================

    stop()

    # 2. Perform computations
    # For fertilizer, we keep the USA sector because its output is consumed by the techs in the aglu module
    L2322.SubsectorLogit_Fert %>%
      filter(region == "USA", supplysector == aglu.FERT_NAME, subsector != "Imports") %>%
      select(region, supplysector, subsector) ->
      L2322.DeleteSubsector_USAFert2

    #Remove the keyword
    L2322.FinalEnergyKeyword_Fert %>%
      filter(region == "USA") %>%
      mutate(`final.energy` = "none") ->
      L2322.FinalEnergyKeyword_USAFert


    # printlog( "NOTE: N fertilizer sectors are only created in states where the Census data indicate production" )
    L1322.out_Mt_state_Fert_Yh %>%
      select(state) %>%
      distinct ->
      Fert_states

    # The USA N fertilizer sector is logited among the states that produce this commodity
    # Write out each state's fertilizer sector as a subsector in the USA's fertilizer sector
    L2322.Supplysector_Fert %>%
      filter(region == "USA", supplysector == aglu.FERT_NAME) %>%
      select(region, supplysector) %>%
      repeat_add_columns(Fert_states) %>%
      mutate(subsector = paste(state, aglu.FERT_NAME)) %>% # there are lots of different ways to do this... idk figure out which one is best
      mutate(logit.year.fillout = min(HISTORICAL_YEARS)) %>%
      mutate(logit.exponent = gcamusa.FERT_LOGIT_EXP) ->
      L2322.SubsectorLogit_USAFert

    # I dont feel good about this
    L2322.SubsectorLogit_USAFert %>%
      mutate(logit.type = NA) ->
      L2322.SubsectorLogitTables_USAFert

    L2322.SubsectorLogit_USAFert %>%
      select("region", "supplysector", "subsector", "logit.year.fillout", "logit.exponent") ->
      L2322.SubsectorLogit_USAFert


    # #Subsector shareweights
    # printlog( "L2322.SubsectorShrwtFllt_USAFert: subsector default shareweights, USA region" )
    L2322.SubsectorLogit_USAFert %>%
      select("region", "supplysector", "subsector") %>%
      mutate(year.fillout = min(BASE_YEARS)) %>%
      mutate(share.weight = 1) ->
      L2322.SubsectorShrwtFllt_USAFert

    # printlog( "L2322.SubsectorInterp_USAFert: subsector shareweight interpolation, USA region" )
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


    # printlog( "All tables for which processing is identical are done in a for loop")
    # printlog( "NOTE: writing out the tables in this step as well")
    # this is copied and pasted from the old ds... there may be a better way to do thi
    L2322.tables <- list( L2322.FinalEnergyKeyword_Fert = L2322.FinalEnergyKeyword_Fert,
                          L2322.Supplysector_Fert = L2322.Supplysector_Fert,
                          L2322.SubsectorLogit_Fert = L2322.SubsectorLogit_Fert,
                          L2322.StubTech_Fert = L2322.StubTech_Fert)

    if(!is.null( L2322.SubsectorInterp_Fert )) {
      L2322.tables[["L2322.SubsectorInterp_Fert"]] <- L2322.SubsectorInterp_Fert
    }

    if(!is.null( L2322.SubsectorShrwtFllt_Fert )) {
      L2322.tables[["L2322.SubsectorShrwtFllt_Fert"]] <- L2322.SubsectorShrwtFllt_Fert
    }

    if(OLD_DATA_SYSTEM_BEHAVIOR){
      L2322.tables[["L2322.SubsectorShrwt_Fert"]] <- as.null()
      L2322.tables[["L2322.SubsectorInterpTo_Fert"]] <- as.null()
    }

    # skip???
    # # The logit functions should be processed before any other table that needs to read logit exponents
    # L2322.tables <- c( read_logit_fn_tables( "ENERGY_LEVEL2_DATA", "L2322.Supplysector_", skip=4, include.equiv.table=T ),
    #                    read_logit_fn_tables( "ENERGY_LEVEL2_DATA", "L2322.SubsectorLogit_", skip=4, include.equiv.table=F ),
    #                    L2322.tables )

#
    for(i in 1:length(L2322.tables)){
      if(!is.null(L2322.tables[[i]])){}
      name <- paste0(names(L2322.tables[i]), "_USA")

      cond_1 <- grepl(x = name, pattern = "EQUIV_TABLE")

      L2322.tables[[i]] %>%
        filter(region == "USA", supplysector == aglu.FERT_NAME) ->
        df

      cond_2 <- ifelse(nrow(df) == 0, TRUE, FALSE)

      if(any(cond_1, cond_2))
      object <- L2322.tables[[i]]
    } else {
      # state-level Exports_fertilizer sector should be excluded

      }

    # printlog( "L2322.StubTechProd_Fert_USA: calibrated fertilizer production by state" )
    L1322.out_Mt_state_Fert_Yh %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(calOutputValue = signif(value, digits = aglu.DIGITS_CALOUTPUT))



    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.DeleteSubsector_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.DeleteSubsector_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.FinalEnergyKeyword_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.FinalEnergyKeyword_USAFert

      tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.SubsectorLogit_USAFert") %>%
        add_precursors("gcam-usa/states_subregions",
                       "energy/calibrated_techs",
                       "energy/A322.globaltech_coef",
                       "temp-data-inject/L2322.Supplysector_Fert",
                       "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                       "temp-data-inject/L2322.SubsectorLogit_Fert",
                       "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                       "temp-data-inject/L2322.SubsectorInterp_Fert",
                       "temp-data-inject/L2322.StubTech_Fert",
                       "L1322.IO_GJkg_state_Fert_F_Yh",
                       "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.SubsectorLogit_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.SubsectorShrwtFllt_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.SubsectorInterp_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.SubsectorInterp_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.TechShrwt_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.TechShrwt_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.Production_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.Production_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.TechCoef_USAFert") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.TechCoef_USAFert

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.StubTechProd_Fert_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.StubTechProd_Fert_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.StubTechCoef_Fert_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.StubTechCoef_Fert_USA

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2322.StubTechMarket_Fert_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "energy/calibrated_techs",
                     "energy/A322.globaltech_coef",
                     "temp-data-inject/L2322.Supplysector_Fert",
                     "temp-data-inject/L2322.FinalEnergyKeyword_Fert",
                     "temp-data-inject/L2322.SubsectorLogit_Fert",
                     "temp-data-inject/L2322.SubsectorShrwtFllt_Fert",
                     "temp-data-inject/L2322.SubsectorInterp_Fert",
                     "temp-data-inject/L2322.StubTech_Fert",
                     "L1322.IO_GJkg_state_Fert_F_Yh",
                     "L1322.out_Mt_state_Fert_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2322.StubTechMarket_Fert_USA

    return_data(L2322.DeleteSubsector_USAFert,
                L2322.FinalEnergyKeyword_USAFert,
                L2322.SubsectorLogit_USAFert,
                L2322.SubsectorShrwtFllt_USAFert,
                L2322.SubsectorInterp_USAFert,
                L2322.TechShrwt_USAFert,
                L2322.Production_USAFert,
                L2322.TechCoef_USAFert,
                L2322.StubTechProd_Fert_USA,
                L2322.StubTechCoef_Fert_USA,
                L2322.StubTechMarket_Fert_USA)
  } else {
    stop("Unknown command")
  }
}
