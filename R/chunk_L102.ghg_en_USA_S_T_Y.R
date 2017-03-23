#' module_emissions_L102.ghg_en_USA_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.ghg_tgej_USA_en_Sepa_F_2005}. The corresponding file in the
#' original data system was \code{L102.ghg_en_USA_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L102.ghg_en_USA_S_T_Y_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
FILE = "energy/IEA_flow_sector",
FILE = "energy/IEA_product_fuel",
FILE = "emissions/EPA_ghg_tech",
FILE = "emissions/GCAM_sector_tech",
 "L101.in_EJ_R_en_Si_F_Yh",
FILE = "emissions/EPA_FCCC_GHG_2005"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.ghg_tgej_USA_en_Sepa_F_2005"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  IEA_flow_sector <- get_data(all_data, "energy/IEA_flow_sector")
  IEA_product_fuel <- get_data(all_data, "energy/IEA_product_fuel")
  EPA_ghg_tech <- get_data(all_data, "emissions/EPA_ghg_tech")
  GCAM_sector_tech <- get_data(all_data, "emissions/GCAM_sector_tech")
  L101.in_EJ_R_en_Si_F_Yh <- get_data(all_data, "L101.in_EJ_R_en_Si_F_Yh")
  EPA_FCCC_GHG_2005 <- get_data(all_data, "emissions/EPA_FCCC_GHG_2005")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    #
    # If you find a mistake/thing to update in the old code and
    # fixing it will change the output data, causing the tests to fail,
    # (i) open an issue on GitHub, (ii) consult with colleagues, and
    # then (iii) code a fix:
    #
    # if(OLD_DATA_SYSTEM_BEHAVIOR) {
    #   ... code that replicates old, incorrect behavior
    # } else {
    #   ... new code with a fix
    # }
    #
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
 add_legacy_name("L102.ghg_tgej_USA_en_Sepa_F_2005") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L102.ghg_tgej_USA_en_Sepa_F_2005

    return_data(L102.ghg_tgej_USA_en_Sepa_F_2005)
  } else {
    stop("Unknown command")
  }
}



