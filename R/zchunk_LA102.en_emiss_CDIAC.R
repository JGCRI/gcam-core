#' module_energy_LA102.en_emiss_CDIAC
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.CO2_Mt_R_F_Yh}, \code{L102.Ccoef_kgCGJ_R_F_Yh}, \code{L102.Ccoef_kgCGJ_F_Yh}. The corresponding file in the
#' original data system was \code{LA102.en_emiss_CDIAC.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_LA102.en_emiss_CDIAC <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/mappings/CDIAC_fuel",
             FILE = "energy/A32.nonenergy_Cseq",
             "L100.CDIAC_CO2_ctry_hist",
             FILE = "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.CO2_Mt_R_F_Yh",
             "L102.Ccoef_kgCGJ_R_F_Yh",
             "L102.Ccoef_kgCGJ_F_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    CDIAC_fuel <- get_data(all_data, "emissions/mappings/CDIAC_fuel")
    A32.nonenergy_Cseq <- get_data(all_data, "energy/A32.nonenergy_Cseq")
    L100.CDIAC_CO2_ctry_hist <- get_data(all_data, "L100.CDIAC_CO2_ctry_hist")
    get_data(all_data, "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5))) ->   # change Xyear to year
      L1011.en_bal_EJ_R_Si_Fi_Yh

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    stop()

    # 2. Perform computations
    iso_GCAM_regID %>%
    select(iso, GCAM_region_ID) -> iso_GCAM_regID_reg

    L100.CDIAC_CO2_ctry_hist %>%
    gather(fuel, value, -iso, -year) %>%
    left_join_error_no_match(iso_GCAM_regID_reg) %>%
    rename(CDIAC_fuel = `fuel`) %>%
    left_join_error_no_match(CDIAC_fuel) -> L102.CDIAC_CO2_ctry_hist_matched

    #Aggregate CO2 emissions by GCAM region and fuel
    L102.CDIAC_CO2_ctry_hist_matched %>%
    group_by(GCAM_region_ID, fuel, year) %>%
    summarise(value = sum(value) * CONV_KT_MT) -> L102.CO2_Mt_R_F_Yh

    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
    filter(sector == "TPES" & L1011.en_bal_EJ_R_Si_Fi_Yh$fuel %in% L102.CO2_Mt_R_F_Yh$fuel) -> L102.en_TPES_EJ_R_Fi_Yh

    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
    filter(sector == "in_industry_feedstocks" & L1011.en_bal_EJ_R_Si_Fi_Yh$fuel %in% L102.CO2_Mt_R_F_Yh$fuel) -> L102.en_feedstocks_EJ_R_Fi_Yh



    #Calculate regional and global CO2 emissions coefficients by fuel
    #Calculate the TPES by fuel, deducting non-energy use of fuels that does not result in CO2 emissions
    L102.en_TPES_EJ_R_Fi_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "TPES" & L1011.en_bal_EJ_R_Si_Fi_Yh$fuel %in% L102.CO2_Mt_R_F_Yh$fuel )
    L102.en_feedstocks_EJ_R_Fi_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "in_industry_feedstocks" & L1011.en_bal_EJ_R_Si_Fi_Yh$fuel %in% L102.CO2_Mt_R_F_Yh$fuel )
    L102.en_sequestered_EJ_R_Fi_Yh <- data.frame(
      L102.en_feedstocks_EJ_R_Fi_Yh[ R_S_F ],
      L102.en_feedstocks_EJ_R_Fi_Yh[ X_CO2_historical_years ] *
        A32.nonenergy_Cseq$remove.fraction[ match( L102.en_feedstocks_EJ_R_Fi_Yh$fuel, A32.nonenergy_Cseq$subsector ) ] )
    L102.en_emitted_EJ_R_Fi_Yh <- data.frame( #creates a data frame subtracting sequestered amount from TPES
      L102.en_TPES_EJ_R_Fi_Yh[ R_S_F ],
      L102.en_TPES_EJ_R_Fi_Yh[ X_CO2_historical_years ] -
        L102.en_sequestered_EJ_R_Fi_Yh[
          match( vecpaste( L102.en_TPES_EJ_R_Fi_Yh[ R_F ] ), vecpaste( L102.en_sequestered_EJ_R_Fi_Yh[ R_F ] ) ),
          X_CO2_historical_years ] )

    #Calculate the emissions coefficients by fuel, using only the energy whose carbon is assumed to be emitted
    #regional
    L102.Ccoef_kgCGJ_R_F_Yh <- data.frame(
      L102.en_emitted_EJ_R_Fi_Yh[ R_F ],
      L102.CO2_Mt_R_F_Yh[ match( vecpaste( L102.en_emitted_EJ_R_Fi_Yh[ R_F ] ), vecpaste( L102.CO2_Mt_R_F_Yh[ R_F ] ) ), X_CO2_historical_years ] /
        L102.en_emitted_EJ_R_Fi_Yh[ X_CO2_historical_years ] )

    ## reset to defaults wherever NAs result from 0 energy consumption
    L102.Ccoef_kgCGJ_R_F_Yh[ L102.Ccoef_kgCGJ_R_F_Yh$fuel == "gas", ][is.na( L102.Ccoef_kgCGJ_R_F_Yh[ L102.Ccoef_kgCGJ_R_F_Yh$fuel == "gas", ] ) ] <- default_gas_Ccoef
    L102.Ccoef_kgCGJ_R_F_Yh[ L102.Ccoef_kgCGJ_R_F_Yh$fuel == "coal", ][is.na( L102.Ccoef_kgCGJ_R_F_Yh[ L102.Ccoef_kgCGJ_R_F_Yh$fuel == "coal", ] ) ] <- default_coal_Ccoef
    L102.Ccoef_kgCGJ_R_F_Yh[ L102.Ccoef_kgCGJ_R_F_Yh$fuel == "refined liquids", ][
      is.na( L102.Ccoef_kgCGJ_R_F_Yh[ L102.Ccoef_kgCGJ_R_F_Yh$fuel == "refined liquids", ] ) ] <- default_liquids_Ccoef

    #global
    L102.en_emitted_EJ_Fi_Yh <- aggregate( L102.en_emitted_EJ_R_Fi_Yh[ X_CO2_historical_years ],
                                           by=as.list( L102.en_emitted_EJ_R_Fi_Yh[ "fuel" ] ), sum )
    L102.CO2_Mt_F_Yh <- aggregate( L102.CO2_Mt_R_F_Yh[ X_CO2_historical_years ],
                                   by=as.list( L102.CO2_Mt_R_F_Yh[ "fuel" ] ), sum )
    L102.Ccoef_kgCGJ_F_Yh <- data.frame(
      L102.en_emitted_EJ_Fi_Yh[ "fuel" ],
      L102.CO2_Mt_F_Yh[ match( L102.en_emitted_EJ_Fi_Yh$fuel, L102.CO2_Mt_F_Yh$fuel ), X_CO2_historical_years ] /
        L102.en_emitted_EJ_Fi_Yh[ X_CO2_historical_years ] )
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
      add_legacy_name("L102.CO2_Mt_R_F_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.CO2_Mt_R_F_Yh

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L102.Ccoef_kgCGJ_R_F_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.Ccoef_kgCGJ_R_F_Yh

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L102.Ccoef_kgCGJ_F_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L102.Ccoef_kgCGJ_F_Yh

    return_data(L102.CO2_Mt_R_F_Yh, L102.Ccoef_kgCGJ_R_F_Yh, L102.Ccoef_kgCGJ_F_Yh)
  } else {
    stop("Unknown command")
  }
}
