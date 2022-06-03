#' module_gcamusa_L273.nonghg_refinery_USA
#'
#' Non-GHG input emissions parameters for refining sector in the USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L273.nonghg_state_refinery_USA}
#' @details This chunk calculates Non-GHG emissions parameters for refining technologies in the USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MAW March 2022

module_gcamusa_L273.nonghg_refinery_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L270.nonghg_tg_state_refinery_F_Yb",
             "L222.StubTech_en_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L273.nonghg_state_refinery_USA"))
  } else if(command == driver.MAKE) {

    # silence check package notes
    year <- region <- supplysector <- subsector <- calibrated.value <- Non.CO2 <- value <- input.emissions <-
      fuel_input <- emiss.coef <- fuel <- sector <- emiss.coeff <- stub.technology <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions", strip_attributes = TRUE)
    L270.nonghg_tg_state_refinery_F_Yb <- get_data(all_data, "L270.nonghg_tg_state_refinery_F_Yb", strip_attributes = TRUE)
    L222.StubTech_en_USA <- get_data(all_data, "L222.StubTech_en_USA", strip_attributes = TRUE)

    # ===================================================
    # Perform computations

    ### Petroleum Refining: oil refining
    # We have petroleum refining emissions by state, so just need to
    # assign them to the correct supplysector / subsector / technology
    pet_ref_emissions <- L270.nonghg_tg_state_refinery_F_Yb %>%
      filter( sector == "petroleum_refining",
              year %in% MODEL_YEARS,
              state %in% gcamusa.STATES) %>%
      select( -c( fuel, sector ) ) %>%
      group_by( state, Non.CO2, year ) %>%
      mutate( value = sum( value ) ) %>%
      rename( input.emissions = value ) %>%
      distinct() %>%
      ungroup()

    # create structure
    pet_ref_structure <- L222.StubTech_en_USA %>%
      filter( stub.technology == "oil refining" ) %>%
      select( -region ) %>%
      repeat_add_columns( tibble::tibble( pet_ref_emissions$year) ) %>%
      rename( year = `pet_ref_emissions$year` ) %>%
      distinct( supplysector, subsector, stub.technology, year )

    pet_ref_final <- pet_ref_emissions %>%
      left_join_error_no_match( pet_ref_structure, by = "year" )


    ### ethanol_production: corn ethanol
    # We have ethanol production emissions by state, so just need to
    # assign them to the correct supplysector / subsector / technology
    ethanol_prod_emissions <- L270.nonghg_tg_state_refinery_F_Yb %>%
      filter( sector == "ethanol_production",
              year %in% MODEL_YEARS ) %>%
      select( -c( fuel, sector ) ) %>%
      group_by( state, Non.CO2, year ) %>%
      mutate( value = sum( value ) ) %>%
      rename( input.emissions = value ) %>%
      distinct() %>%
      ungroup()

    # create structure (like in en_transformation_USA.xml)
    ethanol_prod_structure <- L222.StubTech_en_USA %>%
      filter( stub.technology == "corn ethanol" ) %>%
      select( -region ) %>%
      repeat_add_columns( tibble::tibble( pet_ref_emissions$year) ) %>%
      rename( year = `pet_ref_emissions$year` ) %>%
      distinct( supplysector, subsector, stub.technology, year )

    ethanol_prod_final <- ethanol_prod_emissions %>%
      left_join_error_no_match( ethanol_prod_structure, by = "year" )

    ### biodiesel_production: biodiesel
    # We have biodiesel production emissions by state, so just need to
    # assign them to the correct supplysector / subsector / technology
    bio_prod_emissions <- L270.nonghg_tg_state_refinery_F_Yb %>%
      filter( sector == "biodiesel_production",
              year %in% MODEL_YEARS ) %>%
      select( -c( fuel, sector ) ) %>%
      group_by( state, Non.CO2, year ) %>%
      mutate( value = sum( value ) ) %>%
      rename( input.emissions = value ) %>%
      distinct() %>%
      ungroup()

    # create structure (like in en_transformation_USA.xml)
    bio_prod_structure <- L222.StubTech_en_USA %>%
      filter( stub.technology == "biodiesel" ) %>%
      select( -region ) %>%
      repeat_add_columns( tibble::tibble( pet_ref_emissions$year) ) %>%
      rename( year = `pet_ref_emissions$year` ) %>%
      distinct( supplysector, subsector, stub.technology, year )

    bio_prod_final <- bio_prod_emissions %>%
      left_join_error_no_match( bio_prod_structure, by = "year" )


    ### combine tables
    # State Level
    L273.nonghg_state_refinery_USA <- bio_prod_final %>%
      bind_rows( ethanol_prod_final, pet_ref_final ) %>%
      rename( region = state ) %>%
      #change SO2 to SO2_1
      mutate( Non.CO2 = gsub( "SO2", "SO2_1", Non.CO2 ) ) %>%
      # sum input.emissions, in case we have multiple entries that need to be combined into one emission
      group_by( region, Non.CO2, year, supplysector, subsector, stub.technology ) %>%
      mutate( input.emissions = sum(input.emissions) ) %>%
      distinct() %>%
      ungroup()

    # TODO: other techs: default emissions factors from GREET
    # TODO: or use Ellie's method, dropping EFs into folder


    # ===================================================

    # Produce outputs

    L273.nonghg_state_refinery_USA %>%
      add_title("Non-GHG input emissions parameters for refining technologies in the USA") %>%
      add_units("Tg") %>%
      add_comments("Emissions at the USA level") %>%
      add_precursors(FILE="gcam-usa/states_subregions",
                     "L270.nonghg_tg_state_refinery_F_Yb",
                     "L222.StubTech_en_USA") ->
      L273.nonghg_state_refinery_USA

    return_data(L273.nonghg_state_refinery_USA)

  } else {
    stop("Unknown command")
  }
}
