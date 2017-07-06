#' module_aglu_L2042.resbio_input_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2042.AgResBio_For}, \code{L2042.AgResBioCurve_For}, \code{L2042.GlobalResBio_Mill}, \code{L2042.StubResBioCurve_Mill}, \code{L2042.AgResBio_ag_irr_mgmt}, \code{L2042.AgResBioCurve_ag_irr_mgmt}. The corresponding file in the
#' original data system was \code{L2042.resbio_input_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2042.resbio_input_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/A_agSupplySector",
             FILE = "aglu/A_demand_technology",
             FILE = "aglu/A_resbio_curves",
             FILE = "aglu/A_bio_frac_prod_R",
             "L111.ag_resbio_R_C",
             "L103.ag_Prod_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2042.AgResBio_For",
             "L2042.AgResBioCurve_For",
             "L2042.GlobalResBio_Mill",
             "L2042.StubResBioCurve_Mill",
             "L2042.AgResBio_ag_irr_mgmt",
             "L2042.AgResBioCurve_ag_irr_mgmt"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_agSupplySector <- get_data(all_data, "aglu/A_agSupplySector")
    A_demand_technology <- get_data(all_data, "aglu/A_demand_technology")
    A_resbio_curves <- get_data(all_data, "aglu/A_resbio_curves")
    A_bio_frac_prod_R <- get_data(all_data, "aglu/A_bio_frac_prod_R")
    L111.ag_resbio_R_C <- get_data(all_data, "L111.ag_resbio_R_C")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L123.For_Prod_bm3_R_Y_GLU <- get_data(all_data, "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU")


    # L2041.AgResBio_For <- get_data(all_data, "L2041.AgResBio_For")
    # L2041.GlobalResBio_Mill <- get_data(all_data, "L2041.GlobalResBio_Mill")
    # L2041.AgResBio_ag_irr <- get_data(all_data, "L2041.AgResBio_ag_irr")
    # L2041.AgResBioCurve_For <- get_data(all_data, "L2041.AgResBioCurve_For")
    # L2041.StubResBioCurve_Mill <- get_data(all_data, "L2041.StubResBioCurve_Mill")
    # L2041.AgResBioCurve_ag_irr <- get_data(all_data, "L2041.AgResBioCurve_ag_irr")


    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2042.AgResBio_For") %>%
      add_precursors("common/GCAM_region_names",
                     "L2041.AgResBio_For",
                     "L2041.GlobalResBio_Mill",
                     "L2041.AgResBio_ag_irr",
                     "L2041.AgResBioCurve_For",
                     "L2041.StubResBioCurve_Mill",
                     "L2041.AgResBioCurve_ag_irr") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2042.AgResBio_For
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2042.AgResBioCurve_For") %>%
      add_precursors("common/GCAM_region_names",
                     "L2041.AgResBio_For",
                     "L2041.GlobalResBio_Mill",
                     "L2041.AgResBio_ag_irr",
                     "L2041.AgResBioCurve_For",
                     "L2041.StubResBioCurve_Mill",
                     "L2041.AgResBioCurve_ag_irr") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2042.AgResBioCurve_For
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2042.GlobalResBio_Mill") %>%
      add_precursors("common/GCAM_region_names",
                     "L2041.AgResBio_For",
                     "L2041.GlobalResBio_Mill",
                     "L2041.AgResBio_ag_irr",
                     "L2041.AgResBioCurve_For",
                     "L2041.StubResBioCurve_Mill",
                     "L2041.AgResBioCurve_ag_irr") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2042.GlobalResBio_Mill
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2042.StubResBioCurve_Mill") %>%
      add_precursors("common/GCAM_region_names",
                     "L2041.AgResBio_For",
                     "L2041.GlobalResBio_Mill",
                     "L2041.AgResBio_ag_irr",
                     "L2041.AgResBioCurve_For",
                     "L2041.StubResBioCurve_Mill",
                     "L2041.AgResBioCurve_ag_irr") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2042.StubResBioCurve_Mill
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2042.AgResBio_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "L2041.AgResBio_For",
                     "L2041.GlobalResBio_Mill",
                     "L2041.AgResBio_ag_irr",
                     "L2041.AgResBioCurve_For",
                     "L2041.StubResBioCurve_Mill",
                     "L2041.AgResBioCurve_ag_irr") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2042.AgResBio_ag_irr_mgmt
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2042.AgResBioCurve_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "L2041.AgResBio_For",
                     "L2041.GlobalResBio_Mill",
                     "L2041.AgResBio_ag_irr",
                     "L2041.AgResBioCurve_For",
                     "L2041.StubResBioCurve_Mill",
                     "L2041.AgResBioCurve_ag_irr") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2042.AgResBioCurve_ag_irr_mgmt

    return_data(L2042.AgResBio_For, L2042.AgResBioCurve_For, L2042.GlobalResBio_Mill, L2042.StubResBioCurve_Mill, L2042.AgResBio_ag_irr_mgmt, L2042.AgResBioCurve_ag_irr_mgmt)
  } else {
    stop("Unknown command")
  }
}
