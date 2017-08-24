#' module_emissions_L252.MACC
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L252.ResMAC_fos}, \code{L252.AgMAC}, \code{L252.MAC_an}, \code{L252.MAC_prc}, \code{L252.MAC_higwp}, \code{L252.MAC_Ag_TC_SSP1}, \code{L252.MAC_An_TC_SSP1}, \code{L252.MAC_prc_TC_SSP1}, \code{L252.MAC_res_TC_SSP1}, \code{L252.MAC_Ag_TC_SSP2}, \code{L252.MAC_An_TC_SSP2}, \code{L252.MAC_prc_TC_SSP2}, \code{L252.MAC_res_TC_SSP2}, \code{L252.MAC_Ag_TC_SSP5}, \code{L252.MAC_An_TC_SSP5}, \code{L252.MAC_prc_TC_SSP5}, \code{L252.MAC_res_TC_SSP5}. The corresponding file in the
#' original data system was \code{L252.MACC.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH August 2017
#' @export
module_emissions_L252.MACC <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions",
             FILE = "emissions/A_MACC_TechChange",
             FILE = "emissions/mappings/GCAM_sector_tech",
             FILE = "emissions/HFC_Abate_GV", # source and units
             "L152.MAC_pct_R_S_Proc_EPA",
             "L201.ghg_res",
             "L211.AGREmissions",
             "L211.AnEmissions",
             "L211.AGRBio",
             "L232.nonco2_prc",
             "L241.hfc_all",
             "L241.pfc_all"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L252.ResMAC_fos",
             "L252.AgMAC",
             "L252.MAC_an",
             "L252.MAC_prc",
             "L252.MAC_higwp",
             "L252.MAC_Ag_TC_SSP1",
             "L252.MAC_An_TC_SSP1",
             "L252.MAC_prc_TC_SSP1",
             "L252.MAC_res_TC_SSP1",
             "L252.MAC_Ag_TC_SSP2",
             "L252.MAC_An_TC_SSP2",
             "L252.MAC_prc_TC_SSP2",
             "L252.MAC_res_TC_SSP2",
             "L252.MAC_Ag_TC_SSP5",
             "L252.MAC_An_TC_SSP5",
             "L252.MAC_prc_TC_SSP5",
             "L252.MAC_res_TC_SSP5"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "emissions/A_regions")
    A_MACC_TechChange <- get_data(all_data, "emissions/A_MACC_TechChange")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    HFC_Abate_GV <- get_data(all_data, "emissions/HFC_Abate_GV")
    L152.MAC_pct_R_S_Proc_EPA <- get_data(all_data, "L152.MAC_pct_R_S_Proc_EPA")
    L201.ghg_res <- get_data(all_data, "L201.ghg_res")
    L211.AGREmissions <- get_data(all_data, "L211.AGREmissions")
    L211.AnEmissions <- get_data(all_data, "L211.AnEmissions")
    L211.AGRBio <- get_data(all_data, "L211.AGRBio")
    L232.nonco2_prc <- get_data(all_data, "L232.nonco2_prc")
    L241.hfc_all <- get_data(all_data, "L241.hfc_all")
    L241.pfc_all <- get_data(all_data, "L241.pfc_all")

    # ===================================================
    # Prepare the table with all MAC curves for matching
    L252.MAC_pct_R_S_Proc_EPA <- L152.MAC_pct_R_S_Proc_EPA %>%
      gather(tax, mac.reduction, matches("[0-9]+")) %>%
      mutate(tax = as.numeric(tax))

    # L252.ResMAC_fos: Fossil resource MAC curves
    # NOTE: only applying the fossil resource MAC curves to the CH4 emissions
    L252.ResMAC_fos <- L201.ghg_res %>%
      select(-emiss.coef) %>%
      filter(Non.CO2 == "CH4") %>%
      left_join_error_no_match(GCAM_sector_tech %>%
                                 filter(sector == "out_resources") %>%
                                 select(EPA_MACC_Sector, subsector),
                               by = c("depresource" = "subsector")) %>%
      repeat_add_columns()


    L252.ResMAC_fos <- repeat_and_add_vector( L252.ResMAC_fos, "tax", MAC_taxes )
    L252.ResMAC_fos <- L252.ResMAC_fos[ order( L252.ResMAC_fos$region, L252.ResMAC_fos$depresource ), ]
    L252.ResMAC_fos$mac.reduction <- NA #until we have the EPA region for matching
    L252.ResMAC_fos$EPA_region <- A_regions$MAC_region[ match( L252.ResMAC_fos$region, A_regions$region ) ]
    L252.ResMAC_fos$mac.reduction <- round(
      L252.MAC_pct_R_S_Proc_EPA$mac.reduction[
        match( vecpaste( L252.ResMAC_fos[ c( "EPA_region", "mac.control", "tax" ) ] ),
               vecpaste( L252.MAC_pct_R_S_Proc_EPA[ c( "EPA_region", "Process", "tax" ) ] ) ) ],
      digits_MACC )
    L252.ResMAC_fos <- na.omit( L252.ResMAC_fos )
    # ===================================================

    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.ResMAC_fos") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/A_regions",
                     "emissions/A_MACC_TechChange",
                     "emissions/mappings/GCAM_sector_tech",
                     "emissions/HFC_Abate_GV", # source and units
                     "L152.MAC_pct_R_S_Proc_EPA",
                     "L201.ghg_res",
                     "L211.AGREmissions",
                     "L211.AnEmissions",
                     "L211.AGRBio",
                     "L232.nonco2_prc",
                     "L241.hfc_all",
                     "L241.pfc_all") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.ResMAC_fos

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.AgMAC") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.AgMAC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_an") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_an

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_prc") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_prc

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_higwp") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_higwp

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_Ag_TC_SSP1") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_Ag_TC_SSP1

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_An_TC_SSP1") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_An_TC_SSP1

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_prc_TC_SSP1") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_prc_TC_SSP1

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_res_TC_SSP1") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_res_TC_SSP1

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_Ag_TC_SSP2") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_Ag_TC_SSP2

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_An_TC_SSP2") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_An_TC_SSP2

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_prc_TC_SSP2") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_prc_TC_SSP2

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_res_TC_SSP2") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_res_TC_SSP2

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_Ag_TC_SSP5") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_Ag_TC_SSP5

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_An_TC_SSP5") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_An_TC_SSP5

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_prc_TC_SSP5") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_prc_TC_SSP5

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L252.MAC_res_TC_SSP5") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L252.MAC_res_TC_SSP5

    return_data(L252.ResMAC_fos, L252.AgMAC, L252.MAC_an, L252.MAC_prc, L252.MAC_higwp, L252.MAC_Ag_TC_SSP1, L252.MAC_An_TC_SSP1, L252.MAC_prc_TC_SSP1, L252.MAC_res_TC_SSP1, L252.MAC_Ag_TC_SSP2, L252.MAC_An_TC_SSP2, L252.MAC_prc_TC_SSP2, L252.MAC_res_TC_SSP2, L252.MAC_Ag_TC_SSP5, L252.MAC_An_TC_SSP5, L252.MAC_prc_TC_SSP5, L252.MAC_res_TC_SSP5)
  } else {
    stop("Unknown command")
  }
}



