#' module_data_EDGAR_gases
#'
#' Dedicated data chunk to read \code{EDGAR} csv files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{EDGAR_gases}.
#' @details This reads all the EDGAR csv files and binds them together.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author BBL May 2017
module_data_EDGAR_gases <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/EDGAR/EDGAR_NH3",
             FILE = "emissions/EDGAR/EDGAR_CH4",
             FILE = "emissions/EDGAR/EDGAR_N2O",
             FILE = "emissions/EDGAR/EDGAR_NMVOC",
             FILE = "emissions/EDGAR/EDGAR_NOx",
             FILE = "emissions/EDGAR/EDGAR_SO2",
             FILE = "emissions/EDGAR/EDGAR_CO"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("EDGAR_gases"))
  } else if(command == driver.MAKE) {


    year <- value <- Non.CO2 <- `IPCC-Annex` <- `World Region` <- ISO_A3 <-
            Name <- IPCC <- IPCC_description <- NULL # silence package check.

    all_data <- list(...)[[1]]

    get_data(all_data, "emissions/EDGAR/EDGAR_NH3") %>%
      mutate(Non.CO2 = "NH3") ->
      EDGAR_NH3
    get_data(all_data, "emissions/EDGAR/EDGAR_CH4") %>%
      mutate(Non.CO2 = "CH4")->
      EDGAR_CH4
    get_data(all_data, "emissions/EDGAR/EDGAR_N2O") %>%
      mutate(Non.CO2 = "N2O")->
      EDGAR_N2O
    get_data(all_data, "emissions/EDGAR/EDGAR_NMVOC") %>%
      mutate(Non.CO2 = "NMVOC")->
      EDGAR_VOC
    get_data(all_data, "emissions/EDGAR/EDGAR_NOx") %>%
      mutate(Non.CO2 = "NOx")->
      EDGAR_NOx
    get_data(all_data, "emissions/EDGAR/EDGAR_SO2") %>%
      mutate(Non.CO2 = "SO2")->
      EDGAR_SO2
    get_data(all_data, "emissions/EDGAR/EDGAR_CO") %>%
      mutate(Non.CO2 = "CO")->
      EDGAR_CO

    bind_rows(EDGAR_NH3, EDGAR_CH4, EDGAR_N2O, EDGAR_VOC, EDGAR_NOx, EDGAR_SO2, EDGAR_CO) %>%
      gather_years %>%
      add_title("EDGAR greenhouse gas emissions by country and main source") %>%
      add_units("Gg") %>%
      add_comments("Read from EDGAR csv files EDGAR_NH3, EDGAR_CH4, EDGAR_N2O, EDGAR_NMVOC, EDGAR_NOx, EDGAR_SO2, EDGAR_CO") %>%
      add_legacy_name("N/A") %>%
      add_precursors("emissions/EDGAR/EDGAR_NH3", "emissions/EDGAR/EDGAR_CH4",
                     "emissions/EDGAR/EDGAR_N2O", "emissions/EDGAR/EDGAR_NMVOC",
                     "emissions/EDGAR/EDGAR_NOx", "emissions/EDGAR/EDGAR_SO2",
                     "emissions/EDGAR/EDGAR_CO") %>%
      add_flags(FLAG_NO_OUTPUT) ->
      EDGAR_gases

    return_data(EDGAR_gases)
  } else {
    stop("Unknown command")
  }
}
