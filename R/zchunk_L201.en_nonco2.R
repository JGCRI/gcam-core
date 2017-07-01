#' module_emissions_L201.en_nonco2
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.en_pol_emissions}, \code{L201.en_ghg_emissions}, \code{L201.en_bcoc_emissions}, \code{L201.nonghg_max_reduction}, \code{L201.nonghg_steepness}, \code{L201.nonghg_max_reduction_res}, \code{L201.nonghg_steepness_res}, \code{L201.nonghg_res}, \code{L201.ghg_res}. The corresponding file in the
#' original data system was \code{L201.en_nonco2.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL July 2017
#' @export
module_emissions_L201.en_nonco2 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions",
             FILE = "energy/A_regions",
             FILE = "temp-data-inject/L111.nonghg_tg_R_en_S_F_Yh",
             FILE = "temp-data-inject/L111.nonghg_tgej_R_en_S_F_Yh",
             FILE = "temp-data-inject/L112.ghg_tg_R_en_S_F_Yh",
             FILE = "temp-data-inject/L112.ghg_tgej_R_en_S_F_Yh",
             "L114.bcoc_tgej_R_en_S_F_2000",
             "L151.nonghg_ctrl_R_en_S_T",
             FILE = "emissions/A51.steepness",
             FILE = "temp-data-inject/L244.DeleteThermalService"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.en_pol_emissions",
             "L201.en_ghg_emissions",
             "L201.en_bcoc_emissions",
             "L201.nonghg_max_reduction",
             "L201.nonghg_steepness",
             "L201.nonghg_max_reduction_res",
             "L201.nonghg_steepness_res",
             "L201.nonghg_res",
             "L201.ghg_res"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "emissions/A_regions")
    A_regions <- get_data(all_data, "energy/A_regions")
    L111.nonghg_tg_R_en_S_F_Yh <- get_data(all_data, "temp-data-inject/L111.nonghg_tg_R_en_S_F_Yh")
    L111.nonghg_tgej_R_en_S_F_Yh <- get_data(all_data, "temp-data-inject/L111.nonghg_tgej_R_en_S_F_Yh")
    L112.ghg_tg_R_en_S_F_Yh <- get_data(all_data, "temp-data-inject/L112.ghg_tg_R_en_S_F_Yh")
    L112.ghg_tgej_R_en_S_F_Yh <- get_data(all_data, "temp-data-inject/L112.ghg_tgej_R_en_S_F_Yh")
    L114.bcoc_tgej_R_en_S_F_2000 <- get_data(all_data, "L114.bcoc_tgej_R_en_S_F_2000")
    L151.nonghg_ctrl_R_en_S_T <- get_data(all_data, "L151.nonghg_ctrl_R_en_S_T")
    A51.steepness <- get_data(all_data, "emissions/A51.steepness")
    L244.DeleteThermalService <- get_data(all_data, "temp-data-inject/L244.DeleteThermalService")


    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.en_pol_emissions") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/A_regions",
                     "energy/A_regions",
                     "temp-data-inject/L111.nonghg_tg_R_en_S_F_Yh",
                     "temp-data-inject/L111.nonghg_tgej_R_en_S_F_Yh",
                     "temp-data-inject/L112.ghg_tg_R_en_S_F_Yh",
                     "temp-data-inject/L112.ghg_tgej_R_en_S_F_Yh",
                     "L114.bcoc_tgej_R_en_S_F_2000",
                     "L151.nonghg_ctrl_R_en_S_T",
                     "emissions/A51.steepness",
                     "temp-data-inject/L244.DeleteThermalService") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.en_pol_emissions

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.en_ghg_emissions") %>%
      same_precursors_as(L201.en_pol_emissions) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.en_ghg_emissions

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.en_bcoc_emissions") %>%
      same_precursors_as(L201.en_pol_emissions) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.en_bcoc_emissions

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.nonghg_max_reduction") %>%
      same_precursors_as(L201.en_pol_emissions) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.nonghg_max_reduction

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.nonghg_steepness") %>%
      same_precursors_as(L201.en_pol_emissions) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.nonghg_steepness

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.nonghg_max_reduction_res") %>%
      same_precursors_as(L201.en_pol_emissions) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.nonghg_max_reduction_res

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.nonghg_steepness_res") %>%
      same_precursors_as(L201.en_pol_emissions) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.nonghg_steepness_res

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.nonghg_res") %>%
      same_precursors_as(L201.en_pol_emissions) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.nonghg_res

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.ghg_res") %>%
      same_precursors_as(L201.en_pol_emissions) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.ghg_res

    return_data(L201.en_pol_emissions, L201.en_ghg_emissions, L201.en_bcoc_emissions, L201.nonghg_max_reduction, L201.nonghg_steepness, L201.nonghg_max_reduction_res, L201.nonghg_steepness_res, L201.nonghg_res, L201.ghg_res)
  } else {
    stop("Unknown command")
  }
}
