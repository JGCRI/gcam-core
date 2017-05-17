#' module_energy_LA100.IEA_downscale_ctry
#'
#' TODO: input file documentation.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.IEA_en_bal_ctry_hist}. The corresponding file in the
#' original data system was \code{LA100.IEA_downscale_ctry.R} (energy level1).
#' @details Describe in detail what this chunk does.
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
              #              OPTIONAL_FILE = "L100.IEA_en_bal_ctry_hist",
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

    # Perform computations
    # Subset only the relevant years and combine OECD with non-OECD
    # The two IEA datasets are LARGE and we keep them in wide format for now--
    # too expensive to reshape
    hy <- as.character(HISTORICAL_YEARS)
    cols <- c("COUNTRY", "FLOW", "PRODUCT", hy)
    bind_rows(en_OECD[cols], en_nonOECD[cols]) %>%
      # rename fuels with inconsistent naming between the two databases
      mutate(PRODUCT = replace(PRODUCT,
                               PRODUCT %in% c("Natural Gas", "Other Kerosene", "Total"),
                               c("Natural gas", "Other kerosene", "Total of all energy sources"))) ->
      L100.IEAfull

    # UP FRONT ADJUSTMENTS (UFA)

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


    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L100.IEA_en_bal_ctry_hist") %>%
      add_precursors("L100.Pop_thous_ctry_Yh", "energy/en_OECD", "energy/en_nonOECD",
                     "energy/mappings/IEA_product_downscaling", "energy/mappings/IEA_ctry") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.IEA_en_bal_ctry_hist

    return_data(L100.IEA_en_bal_ctry_hist)
  } else {
    stop("Unknown command")
  }
}



