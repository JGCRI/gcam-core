#' module_gcamusa_L270.nonghg_nei_to_gcam
#'
#' Produce tables containing non-GHG emissions for all model base years by sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L270.nonghg_tg_state_elec_F_Yb}, \code{L270.nonghg_tg_state_refinery_F_Yb}, \code{L270.nonghg_tg_state_bld_F_Yb},
#' \code{L270.nonghg_tg_state_indenergy_F_Yb}, \code{L270.nonghg_tg_state_othertrn_F_Yb}, \code{L270.nonghg_tg_state_prc_F_Yb}.
#' @details This chunk isolates sectoral non-ghg input emissions by U.S. state / sector / fuel / pollutant / year from NEI.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MAW February 2022

module_gcamusa_L270.nonghg_nei_to_gcam <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/emissions/NEI_pollutant_mapping",
             FILE = "gcam-usa/emissions/CEDS_GCAM_fuel",
             FILE = "gcam-usa/emissions/CEDS_GCAM_transport",
             "L170.NEI_1990_2017_GCAM_sectors"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L270.nonghg_tg_state_elec_F_Yb",
             "L270.nonghg_tg_state_refinery_F_Yb",
             "L270.nonghg_tg_state_bld_F_Yb",
             "L270.nonghg_tg_state_indenergy_F_Yb",
             "L270.nonghg_tg_state_othertrn_F_Yb",
             "L270.nonghg_tg_state_prc_F_Yb"))
  } else if(command == driver.MAKE) {

    # silence package check
    GCAM_sector <- GCAM_fuel <- pollutant <-emissions <- state <-
      sector <- fuel <- Non.CO2 <- value <- year <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    NEI_pollutant_mapping <- get_data( all_data, "gcam-usa/emissions/NEI_pollutant_mapping" )
    CEDS_GCAM_fuel <- get_data( all_data, "gcam-usa/emissions/CEDS_GCAM_fuel" )
    CEDS_GCAM_transport <- get_data( all_data, "gcam-usa/emissions/CEDS_GCAM_transport" )
    NEI_1990_2017_GCAM_sectors <- get_data( all_data, "L170.NEI_1990_2017_GCAM_sectors" )

    # Perform computations
    # This script assumes the data has been pre-processed. So all that needs to be done is
    # to convert to the correct sector/fuel organization and convert units
    # Note: This could be refined further in the future to run the function once and then filter by sector.
    # Elec
    elec_names <- c( "elec_heat" )
    L270.nonghg_tg_state_elec_F_Yb <- NEI_to_GCAM( NEI_1990_2017_GCAM_sectors, CEDS_GCAM_fuel, NEI_pollutant_mapping, elec_names )

    # Refinery / related
    refinery_names <- c( "petroleum_production", "petroleum_refining", "petroleum_distribution",
                         "ethanol_production", "NG_production_distribution", "biodiesel_production" )
    L270.nonghg_tg_state_refinery_F_Yb <- NEI_to_GCAM( NEI_1990_2017_GCAM_sectors, CEDS_GCAM_fuel, NEI_pollutant_mapping, refinery_names )

    # Buildings
    bld_names <- grep( "building", unique( NEI_1990_2017_GCAM_sectors$GCAM_sector ), value = T )
    L270.nonghg_tg_state_bld_F_Yb <- NEI_to_GCAM( NEI_1990_2017_GCAM_sectors, CEDS_GCAM_fuel, NEI_pollutant_mapping, bld_names ) %>%
      mutate( sector = gsub( "building_", "", sector ) )

    # Industrial energy
    indenergy_names <- c( "industry_energy" )
    L270.nonghg_tg_state_indenergy_F_Yb <- NEI_to_GCAM( NEI_1990_2017_GCAM_sectors, CEDS_GCAM_fuel, NEI_pollutant_mapping, indenergy_names )

    # Other transportation
    othertrn_names <- c ("trn_domestic ship", "trn_domestic air", "trn_rail" )
    L270.nonghg_tg_state_othertrn_F_Yb <- NEI_to_GCAM( NEI_1990_2017_GCAM_sectors, CEDS_GCAM_transport, NEI_pollutant_mapping, othertrn_names )

    # Process
    process_names <- c( gcamusa.IND_PROC_EM_NEI_GCAM_SECTORS, gcamusa.URB_PROC_EM_NEI_GCAM_SECTORS, gcamusa.CEMENT_NEI_GCAM_SECTORS )
    L270.nonghg_tg_state_prc_F_Yb <- NEI_to_GCAM( NEI_1990_2017_GCAM_sectors, CEDS_GCAM_fuel, NEI_pollutant_mapping, process_names )

    # ===================================================
    # Produce outputs

    L270.nonghg_tg_state_elec_F_Yb %>%
      add_title("Base-year electricity non-ghg input emissions") %>%
      add_units("Tg") %>%
      add_comments("Base-year electricity non-ghg input emissions by U.S. state / fuel / pollutant / year") %>%
      add_legacy_name("LB172.nonghg_elc_USA") %>%
      add_precursors("gcam-usa/emissions/CEDS_GCAM_fuel", "gcam-usa/emissions/NEI_pollutant_mapping",
                     "L170.NEI_1990_2017_GCAM_sectors") ->
      L270.nonghg_tg_state_elec_F_Yb

    L270.nonghg_tg_state_refinery_F_Yb %>%
      add_title("Base-year refining related non-ghg input emissions by U.S. state / sector / fuel / pollutant / year") %>%
      add_units("Tg") %>%
      add_comments("Base-year refining related non-ghg input emissions by U.S. state / sector / fuel / pollutant / year") %>%
      add_precursors("gcam-usa/emissions/NEI_pollutant_mapping",
                     "gcam-usa/emissions/CEDS_GCAM_fuel",
                     "L170.NEI_1990_2017_GCAM_sectors") ->
      L270.nonghg_tg_state_refinery_F_Yb

    L270.nonghg_tg_state_bld_F_Yb %>%
      add_title("Base-year buildings sector non-ghg input emissions by U.S. state / sector / fuel / pollutant / year") %>%
      add_units("Tg") %>%
      add_comments("Base-year buildings sector non-ghg input emissions by U.S. state / sector / fuel / pollutant / year") %>%
      add_legacy_name("L270.nonghg_tg_state_bld_F_Yb") %>%
      add_precursors("gcam-usa/emissions/NEI_pollutant_mapping",
                     "gcam-usa/emissions/CEDS_GCAM_fuel",
                     "L170.NEI_1990_2017_GCAM_sectors") ->
      L270.nonghg_tg_state_bld_F_Yb

    L270.nonghg_tg_state_indenergy_F_Yb %>%
      add_title("Base-year industrial energy use sector non-ghg input emission factor by U.S. state / sector / fuel / pollutant / year") %>%
      add_units("Tg") %>%
      add_comments("Base-year industrial energy use sector non-ghg input emission factor by U.S. state / sector / fuel / pollutant / year") %>%
      add_legacy_name("L270.nonghg_tg_state_indenergy_F_Yb") %>%
      add_precursors("gcam-usa/emissions/NEI_pollutant_mapping",
                     "gcam-usa/emissions/CEDS_GCAM_fuel",
                     "L170.NEI_1990_2017_GCAM_sectors") ->
      L270.nonghg_tg_state_indenergy_F_Yb

    L270.nonghg_tg_state_othertrn_F_Yb %>%
      add_title("Base-year domestic aviation & ship & rail non-ghg input emission by U.S. state / fuel / pollutant / year") %>%
      add_units("Tg") %>%
      add_comments("Base-year domestic aviation & ship & rail non-ghg input emission by U.S. state / fuel / pollutant / year") %>%
      add_legacy_name("L270.nonghg_tg_state_othertrn_F_Yb") %>%
      add_precursors("gcam-usa/emissions/NEI_pollutant_mapping",
                     "gcam-usa/emissions/CEDS_GCAM_transport",
                     "L170.NEI_1990_2017_GCAM_sectors") ->
      L270.nonghg_tg_state_othertrn_F_Yb

    L270.nonghg_tg_state_prc_F_Yb %>%
      add_title("Base-year process non-ghg input emissions by U.S. state / sector / fuel / pollutant / year") %>%
      add_units("Tg") %>%
      add_comments("Base-year process non-ghg input emissions by U.S. state / sector / fuel / pollutant / year") %>%
      add_precursors("gcam-usa/emissions/NEI_pollutant_mapping",
                     "gcam-usa/emissions/CEDS_GCAM_fuel",
                     "L170.NEI_1990_2017_GCAM_sectors") ->
      L270.nonghg_tg_state_prc_F_Yb

    return_data(L270.nonghg_tg_state_elec_F_Yb,
                L270.nonghg_tg_state_refinery_F_Yb,
                L270.nonghg_tg_state_bld_F_Yb,
                L270.nonghg_tg_state_indenergy_F_Yb,
                L270.nonghg_tg_state_othertrn_F_Yb,
                L270.nonghg_tg_state_prc_F_Yb)
  } else {
    stop("Unknown command")
  }
}
