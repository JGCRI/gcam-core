# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L253.emission_controls
#'
#' Allow user to specify NSPS and existing vintage retrofits for regions
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L253.EF_retrofit}, \code{L253.EF_NSPS_new_vintage},
#' \code{L253.Retrofit_off}, \code{L253.delete_gdp_control},
#' \code{L253.user_EF_retrofit}, \code{L253.user_EF_NSPS_new_vintage},
#' \code{L253.user_Retrofit_off}, \code{L253.user_delete_gdp_control},
#' \code{L253.EF_retrofit_USA}, \code{L253.EF_NSPS_new_vintage_USA},
#' \code{L253.Retrofit_off_USA}, \code{L253.delete_gdp_control_USA},
#' \code{L253.user_EF_retrofit_USA}, \code{L253.user_EF_NSPS_new_vintage_USA},
#' \code{L253.user_Retrofit_off_USA}, \code{L253.user_delete_gdp_control_USA},
#' @author ENL August 2021

module_emissions_L253.emission_controls <- function(command, ...) {

  # Get list of files from user drop folder, which will become inputs
  list.files("inst/extdata/emissions/user_emission_controls", "*.csv", full.names = TRUE)  %>%
    sub('inst/extdata/', '', .) ->
    user_em_control_files
  names(user_em_control_files) <- rep("FILE", length(user_em_control_files))

  if(command == driver.DECLARE_INPUTS) {
    # List of all data that we need, either from csv input or another chunk
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions",
             FILE = "gcam-usa/states_subregions",
             FILE = "emissions/emission_controls/A53.em_ctrl_param_china_industry",
             FILE = "emissions/emission_controls/A53.em_ctrl_param_intl_shipping",
             FILE = "emissions/emission_controls/A53.em_ctrl_param_dom_shipping",
             user_em_control_files, # All files in user_emission_controls folder
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L201.nonghg_steepness",
             "L223.StubTechEff_elec",
             "L223.GlobalTechEff_elec",
             "L224.Supplysector_heat"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L253.EF_retrofit",
             "L253.EF_NSPS_new_vintage",
             "L253.Retrofit_off",
             "L253.delete_gdp_control",
             "L253.user_EF_retrofit",
             "L253.user_EF_NSPS_new_vintage",
             "L253.user_Retrofit_off",
             "L253.user_delete_gdp_control",
             "L253.EF_retrofit_USA",
             "L253.EF_NSPS_new_vintage_USA",
             "L253.Retrofit_off_USA",
             "L253.delete_gdp_control_USA",
             "L253.user_EF_retrofit_USA",
             "L253.user_EF_NSPS_new_vintage_USA",
             "L253.user_Retrofit_off_USA",
             "L253.user_delete_gdp_control_USA"))

  } else if(command == driver.MAKE) {

    # Silencing package checks
    . <- year <- supplysector <- MAC_region <- state <- state_name <-
      scenario <- value <- SO2_name <- region <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    non_co2_region_info <- get_data(all_data, "emissions/A_regions")
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    pcGDP_MER <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y", strip_attributes = TRUE)
    L201.nonghg_steepness <- get_data(all_data, "L201.nonghg_steepness", strip_attributes = TRUE)
    base_year_eff <- get_data(all_data, "L223.StubTechEff_elec", strip_attributes = TRUE)
    future_year_eff <- get_data(all_data, "L223.GlobalTechEff_elec", strip_attributes = TRUE) %>%
      filter(year %in% MODEL_FUTURE_YEARS)
    dist_heat_regions <- get_data(all_data, "L224.Supplysector_heat", strip_attributes = TRUE) %>%
      select(region, supplysector)

    # Load all inputs that contains A53. We do this to simplify the process of moving data
    # from the user drop folder to the core. To move data to the core, the user just has to
    # name them containing "A53" and include the files in driver.DECLARE_INPUTS
    em_ctrl_inputs <- names(all_data[grep("A53", names(all_data))])
    lapply(em_ctrl_inputs, function(name, all_data) {
      get_data(all_data, name, strip_attributes = TRUE)
    }, all_data) %>% bind_rows() -> em_ctrl_parameters

    # Load user data from drop folder
    lapply(user_em_control_files, function(name, all_data) {
      get_data(all_data, name, strip_attributes = TRUE)
    }, all_data) %>% bind_rows() -> user_em_ctrl_parameters

    # Create mapping between meta region and GCAM-region for all cases listed below
    # Users can specify MAC regions (specified in emissions/A_regions.csv) which will then be applied to all corresponding GCAM regions,
    # USA state names or abbreviations which will be applied to GCAM-USA states,
    # or, global pararamters "All" for all GCAM regions, and "All states" for all GCAM-USA states.
    non_co2_region_info %>%
      select(region, MAC_region) %>%
      rename(GCAM_region = region,
             meta_region = MAC_region) -> meta_region_map

    states_subregions %>%
      select(state, state_name) %>%
      rename(GCAM_region = state,
             meta_region = state_name) -> gcamusa_region_map

    gcamusa_region_map %>%
      bind_rows(mutate(gcamusa_region_map, meta_region = "All states")) -> gcamusa_region_map

    meta_region_map %>%
      bind_rows(mutate(meta_region_map, meta_region = "All"), gcamusa_region_map) -> meta_region_map

    # Join GDP data with GCAM region by the region ID, and filter to only include data we need
    # Convert GDP data to 2015$ to be consistent with the units expected in the emission control input files
    # The scenario can be switched to another by substituting the new scenario with "gSSP2" here
    pcGDP_MER %>%
      filter(scenario == "gSSP2",
             year %in% MODEL_YEARS) %>%
      left_join_error_no_match(non_co2_region_info, by = "GCAM_region_ID") %>%
      select(year, value, region) %>%
      mutate(value = value * gdp_deflator(2015, 1990)) %>%
      spread(year, value) -> pcGDP_MER

    # Get regional SO2 species names
    # SO2 names for GCAM-USA states are the same as the name for USA region in global GCAM
    non_co2_region_info %>%
      select(region, SO2_name) -> so2_reg_names_global

    states_subregions %>% select(state) %>%
      rename(region = state) %>%
      mutate(SO2_name = so2_reg_names_global[[which(so2_reg_names_global$region == "USA"), "SO2_name"]]) -> so2_reg_names_states

    bind_rows(so2_reg_names_global, so2_reg_names_states) -> so2_reg_names

    # Function that processes emission control data
    process_em_control_data <- function(em_control_data) {

      # Silence package checks
      GCAM_region <- supplysector <- region <- Non.CO2 <- SO2_name <-
        tail <- pcGDP_start_NSPS <- NSPS_start_year <- NSPS_em_coeff <- year <- GDP <-
        id <- retrofit_start_year <- pcGDP_start_retrofit <- subsector <-
        stub.technology <- linear.control <- retrofit_time <- retrofit_vintage <-
        retrofit_em_coeff <- start.year <- final.emissions.coefficient <- emiss.coef <-
        head <- period <- end.year <- efficiency.y <- efficiency.x <- efficiency <-
        gdp.control <- disable.em.control <- NULL

      # Only run if there is emission control data
      if(length(em_control_data) > 0){

        # Combine emission control data with region mapping. If data has meta region mapping, keep corresponding GCAM region
        # Otherwise, keep original region.
        # Using left_join instead of left_join_error_no_match because not all regions have meta region mapping
        em_control_data %>%
          left_join(meta_region_map, by = c("region" = "meta_region")) %>%
          filter(GCAM_region %in% dist_heat_regions$region |
                   !supplysector %in% dist_heat_regions$supplysector) %>%
          mutate(region = if_else(is.na(GCAM_region), region, GCAM_region)) %>%
          semi_join(L201.nonghg_steepness,
                    by = c("region", "supplysector", "subsector", "stub.technology")) %>%
          select(-GCAM_region) -> em_control_data

        # Stop if regions aren't valid regions
        all_GCAM_regions <- all(em_control_data$region %in% c(GCAM_region_names$region, states_subregions$state))
        if(!all_GCAM_regions){
          stop("Emission control regions not valid GCAM or MAC regions")
        }

        # Name linear control based on the non-CO2 and replace SO2s with corresponding regional SO2 name
        em_control_data %>%
          mutate(linear.control = paste0(Non.CO2, "_control_retrofits")) %>%
          left_join_error_no_match(so2_reg_names, by = "region") %>%
          mutate(Non.CO2 = if_else(Non.CO2 == "SO2" & !region %in% states_subregions$state, SO2_name, Non.CO2)) %>%
          select(-SO2_name) -> em_control_data

        # Model years, including the last base year, that retrofits can be applied
        # Note NSPS can be applied to all model future years which is already defined
        retrofit_years <- c(tail(MODEL_BASE_YEARS, n=1), MODEL_FUTURE_YEARS)

        # RETROFITS
        # Extract only the data we need for retrofits from emission control data. Use row number to keep track of order.
        # Join in regional GDP data and filter for retrofit years. If there is no user-inputted retrofit start year,
        # we calculate it by finding all years where GDP is at least the inputted start GDP value, and keep the first
        # occurrence, which is the year at which each region reaches pcGDP of the "pcGDP_start_retrofit" value
        em_control_data %>%
          mutate(id = row_number()) %>%
          select(-c(pcGDP_start_NSPS, NSPS_start_year, NSPS_em_coeff, 'Notes and sources')) %>%
          left_join_error_no_match(pcGDP_MER, by = "region") %>%
          tidyr::gather(year, GDP, as.character(MODEL_YEARS)) %>%
          arrange(id) %>%
          filter(year %in% retrofit_years) %>%
          mutate(retrofit_start_year = if_else(is.na(retrofit_start_year) & GDP >= pcGDP_start_retrofit,
                                               as.numeric(year), retrofit_start_year)) %>%
          select(-year, -GDP, -id) %>%
          drop_na(retrofit_start_year) %>%
          distinct(region, supplysector, subsector, stub.technology, Non.CO2,
                   linear.control, pcGDP_start_retrofit, retrofit_time,
                   retrofit_vintage, retrofit_em_coeff, .keep_all = TRUE) -> L253.EF_retrofit

        # Rename to GCAM input names and calculate end year by adding retrofit time to start year
        # If a region doesn't have retrofits (emissions coefficient of -1), remove row
        L253.EF_retrofit %>%
          rename(final.emissions.coefficient = retrofit_em_coeff,
                 start.year = retrofit_start_year) %>%
          mutate(retrofit_time = as.numeric(retrofit_time),
                 end.year = start.year + retrofit_time) %>%
          filter(final.emissions.coefficient != -1) -> L253.EF_retrofit

        # NSPS
        # Similar approach for NSPS
        # Select NSPS data, get NSPS start year from either user-inputted data or calculated from GDP data
        em_control_data %>%
          mutate(id = row_number()) %>%
          select(-c(pcGDP_start_retrofit, retrofit_start_year, retrofit_time, retrofit_vintage, retrofit_em_coeff, 'Notes and sources')) %>%
          left_join_error_no_match(pcGDP_MER, by = "region")%>%
          tidyr::gather(year, GDP, as.character(MODEL_YEARS)) %>%
          arrange(id) %>%
          filter(year %in% MODEL_FUTURE_YEARS) %>%
          mutate(NSPS_start_year = if_else(is.na(NSPS_start_year) & GDP >= pcGDP_start_NSPS,
                                          as.numeric(year), NSPS_start_year)) %>%
          select(-year, -GDP, -id) %>%
          drop_na(NSPS_start_year) %>%
          distinct(region, supplysector, subsector, stub.technology, Non.CO2,
                   linear.control, pcGDP_start_NSPS, NSPS_em_coeff, .keep_all = TRUE) -> L253.EF_NSPS_new_vintage

        # Rename to GCAM input names and remove regions without an NSPS
        L253.EF_NSPS_new_vintage %>%
          rename(emiss.coef = NSPS_em_coeff,
                 period = NSPS_start_year) %>%
          filter(emiss.coef != -1) -> L253.EF_NSPS_new_vintage

        # Remove the default generic control since more specific control is in place
        em_control_data %>%
          select(region, supplysector, subsector, stub.technology, linear.control, Non.CO2) %>%
          mutate(period = head(MODEL_YEARS, n=1),
                 gdp.control = "GDP_control") -> L253.delete_gdp_control

        # If there are multiple controls on same emissions and technologies within regions, keep latest data.
        # distinct() keeps first occurrence, but we want to keep the last, so we reverse all rows,
        # remove duplicates, and then reverse back to original order
        L253.EF_retrofit %>% arrange(desc(row_number())) %>%
          distinct(region, supplysector, subsector, stub.technology, Non.CO2,
                   retrofit_vintage, linear.control, .keep_all = TRUE) %>%
          arrange(desc(row_number())) -> L253.EF_retrofit

        L253.EF_NSPS_new_vintage %>% arrange(desc(row_number())) %>%
          distinct(region, supplysector, subsector, stub.technology, Non.CO2,
                   linear.control, period, .keep_all = TRUE) %>%
          arrange(desc(row_number())) -> L253.EF_NSPS_new_vintage

        # Define a utility function that returns the next model year.
        # Because we vectorize the function, the argument can be a vector (or column) of years
        # If year passed in is last model year, return last model year
        get_next_model_year <- Vectorize(function(year) {
          if(!year %in% MODEL_YEARS) {
            stop("Year is not a GCAM model year")
          }
          else if(year != tail(MODEL_YEARS, n = 1)){
            MODEL_YEARS[which(MODEL_YEARS == year)+1]
          }
          else {return(year)}
        })

        # Turn off retrofits after end year of the last retrofit for that emission/technology/region
        L253.EF_retrofit %>%
          arrange(desc(retrofit_vintage)) %>%
          distinct(region, supplysector, subsector, stub.technology, Non.CO2,
                   pcGDP_start_retrofit, start.year, retrofit_time, final.emissions.coefficient,
                   linear.control, end.year, .keep_all = TRUE) %>%
          mutate(period = get_next_model_year(retrofit_vintage),
                 start.year = period,
                 end.year = tail(MODEL_FUTURE_YEARS, n=1),
                 disable.em.control = 1) -> L253.Retrofit_off

        # Convert emission factors relative to the output unit where needed
        # Divide by efficiency for electricity technologies which have output drivers
        # Using left_join instead of left_join_error_no_match because not all sectors use output drivers
        # Currently the only other sector that uses output drivers is the resource sector which we do not deal with
        L253.EF_retrofit %>%
          left_join(base_year_eff, by = c("region", "supplysector", "subsector", "stub.technology",
                                          "retrofit_vintage" = "year")) %>%
          left_join(future_year_eff, by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                           "stub.technology" = "technology", "retrofit_vintage" = "year")) %>%
          # For techs without efficiencies, we divide by 1 (no change)
          mutate(efficiency.y = if_else(is.na(efficiency.y), 1, efficiency.y),
                 final.emissions.coefficient = if_else(is.na(efficiency.x), final.emissions.coefficient/efficiency.y,
                                                       final.emissions.coefficient/efficiency.x)) -> L253.EF_retrofit_2

        # We do the same for NSPS, but since NSPS can only be applied in future model years, we only join in future efficiency data
        L253.EF_NSPS_new_vintage %>%
          left_join(future_year_eff, by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                       "stub.technology" = "technology", "period" = "year")) %>%
          mutate(efficiency = if_else(is.na(efficiency), 1, efficiency),
                 emiss.coef = emiss.coef/efficiency) -> L253.EF_NSPS_new_vintage

        # Select/order only the relevant columns
        # Note that although some sectors have different column names (eg tranSubsector instead of subsector in transportation)
        # the C++ won't check for name mismatches so using these column names is fine
        L253.EF_retrofit <- select(L253.EF_retrofit, region, supplysector, subsector, stub.technology, retrofit_vintage, Non.CO2, linear.control, start.year, end.year, final.emissions.coefficient)
        L253.EF_NSPS_new_vintage <- select(L253.EF_NSPS_new_vintage, region, supplysector, subsector, stub.technology, period, Non.CO2, emiss.coef)
        L253.delete_gdp_control <- select(L253.delete_gdp_control, region, supplysector, subsector, stub.technology, period, Non.CO2, gdp.control)
        L253.Retrofit_off <- select(L253.Retrofit_off, region, supplysector, subsector, stub.technology, period, Non.CO2, linear.control, start.year, end.year, disable.em.control)

        # Separate out US State emission controls
        L253.EF_retrofit_USA <- filter(L253.EF_retrofit, region %in% states_subregions$state)
        L253.EF_NSPS_new_vintage_USA <- filter(L253.EF_NSPS_new_vintage, region %in% states_subregions$state)
        L253.delete_gdp_control_USA <- filter(L253.delete_gdp_control, region %in% states_subregions$state)
        L253.Retrofit_off_USA <- filter(L253.Retrofit_off, region %in% states_subregions$state)
        L253.EF_retrofit <- filter(L253.EF_retrofit, !region %in% states_subregions$state)
        L253.EF_NSPS_new_vintage <- filter(L253.EF_NSPS_new_vintage, !region %in% states_subregions$state)
        L253.delete_gdp_control <- filter(L253.delete_gdp_control, !region %in% states_subregions$state)
        L253.Retrofit_off <- filter(L253.Retrofit_off, !region %in% states_subregions$state)

        # Produce outputs
        # If there is no user data, we set input list to NA and it will be filtered out during add_precursors
        if(length(user_em_control_files) == 0){
          user_em_control_files <- NA
        }

        # Create empty output if there is no data
        if(exists("L253.EF_retrofit")) {
          L253.EF_retrofit %>%
            add_title("Emission Control Retrofits") %>%
            add_units("Various") %>%
            add_comments("Retrofit emission controls") -> L253.EF_retrofit

          # Add precursors. Using do.call to add precursors from vectors
          L253.EF_retrofit_precursors <- list(L253.EF_retrofit)
          precursors <- na.omit(c("common/GCAM_region_names",
                                  "emissions/A_regions",
                                  user_em_control_files,
                                  em_ctrl_inputs,
                                  "L102.pcgdp_thous90USD_Scen_R_Y",
                                  "L201.nonghg_steepness",
                                  "L223.StubTechEff_elec",
                                  "L223.GlobalTechEff_elec",
                                  "L224.Supplysector_heat"))
          L253.EF_retrofit_precursors[precursors] <- precursors
          do.call(add_precursors, L253.EF_retrofit_precursors) -> L253.EF_retrofit
        } else {
          missing_data() -> L253.EF_retrofit
        }

        if(exists("L253.EF_NSPS_new_vintage")) {
          L253.EF_NSPS_new_vintage %>%
            add_title("NSPS Emission Controls") %>%
            add_units("Various") %>%
            add_comments("New Source Performance Standard Emission controls") %>%
            same_precursors_as(L253.EF_retrofit) -> L253.EF_NSPS_new_vintage
        } else {
          missing_data() -> L253.EF_NSPS_new_vintage
        }

        if(exists("L253.Retrofit_off")) {
          L253.Retrofit_off %>%
            add_title("Retrofit Off") %>%
            add_units("Various") %>%
            add_comments("Turn off retrofit") -> L253.Retrofit_off

          L253.Retrofit_off_precursors <- list(L253.Retrofit_off)
          precursors <- na.omit(c("common/GCAM_region_names",
                                  "emissions/A_regions",
                                  user_em_control_files,
                                  em_ctrl_inputs,
                                  "L102.pcgdp_thous90USD_Scen_R_Y"))
          L253.Retrofit_off_precursors[precursors] <- precursors
          do.call(add_precursors, L253.Retrofit_off_precursors) -> L253.Retrofit_off
        } else {
          missing_data() -> L253.Retrofit_off
        }

        if(exists("L253.delete_gdp_control")) {
          L253.delete_gdp_control %>%
            add_title("Delete GDP Control") %>%
            add_units("Various") %>%
            add_comments("Delete GDP control because new control is in place") %>%
            same_precursors_as(L253.Retrofit_off) -> L253.delete_gdp_control
        } else {
          missing_data() -> L253.delete_gdp_control
        }

        if(exists("L253.EF_retrofit_USA")) {
          L253.EF_retrofit_USA %>%
            add_title("U.S. States Emission Control Retrofits") %>%
            add_units("Various") %>%
            add_comments("U.S. states retrofit emission controls") -> L253.EF_retrofit_USA

          L253.EF_retrofit_USA_precursors <- list(L253.EF_retrofit_USA)
          precursors <- na.omit(c("common/GCAM_region_names",
                                  "emissions/A_regions",
                                  "gcam-usa/states_subregions",
                                  user_em_control_files,
                                  em_ctrl_inputs,
                                  "L102.pcgdp_thous90USD_Scen_R_Y",
                                  "L201.nonghg_steepness",
                                  "L223.StubTechEff_elec",
                                  "L223.GlobalTechEff_elec",
                                  "L224.Supplysector_heat"))
          L253.EF_retrofit_USA_precursors[precursors] <- precursors
          do.call(add_precursors, L253.EF_retrofit_USA_precursors) -> L253.EF_retrofit_USA
        } else {
          missing_data() -> L253.EF_retrofit_USA
        }

        if(exists("L253.EF_NSPS_new_vintage_USA")) {
          L253.EF_NSPS_new_vintage_USA %>%
            add_title("U.S. States NSPS Emission Controls") %>%
            add_units("Various") %>%
            add_comments("U.S. states New Source Performance Standard Emission controls") %>%
            same_precursors_as(L253.EF_retrofit_USA) -> L253.EF_NSPS_new_vintage_USA
        } else {
          missing_data() -> L253.EF_NSPS_new_vintage_USA
        }

        if(exists("L253.Retrofit_off_USA")) {
          L253.Retrofit_off_USA %>%
            add_title("U.S. States Retrofit Off") %>%
            add_units("Various") %>%
            add_comments("Turn off retrofit in U.S. states") -> L253.Retrofit_off_USA

          L253.Retrofit_off_USA_precursors <- list(L253.Retrofit_off_USA)
          precursors <- na.omit(c("common/GCAM_region_names",
                                  "emissions/A_regions",
                                  "gcam-usa/states_subregions",
                                  user_em_control_files,
                                  em_ctrl_inputs,
                                  "L102.pcgdp_thous90USD_Scen_R_Y"))
          L253.Retrofit_off_USA_precursors[precursors] <- precursors
          do.call(add_precursors, L253.Retrofit_off_USA_precursors) -> L253.Retrofit_off_USA
        } else {
          missing_data() -> L253.Retrofit_off_USA
        }

        if(exists("L253.delete_gdp_control_USA")) {
          L253.delete_gdp_control_USA %>%
            add_title("U.S. States Delete GDP Control") %>%
            add_units("Various") %>%
            add_comments("Delete GDP control in U.S. states because new control is in place") %>%
            same_precursors_as(L253.Retrofit_off_USA) -> L253.delete_gdp_control_USA
        } else {
          missing_data() -> L253.delete_gdp_control_USA
        }


      } else {
        # There is no emissions control data, so we set missing data to each output
        missing_data() -> L253.EF_retrofit
        missing_data() -> L253.EF_NSPS_new_vintage
        missing_data() -> L253.Retrofit_off
        missing_data() -> L253.delete_gdp_control
        missing_data() -> L253.EF_retrofit_USA
        missing_data() -> L253.EF_NSPS_new_vintage_USA
        missing_data() -> L253.Retrofit_off_USA
        missing_data() -> L253.delete_gdp_control_USA
      } # end ifelse

      # Get all outputs from function into list
      out <- list(L253.EF_retrofit = L253.EF_retrofit,
                  L253.EF_NSPS_new_vintage = L253.EF_NSPS_new_vintage,
                  L253.Retrofit_off = L253.Retrofit_off,
                  L253.delete_gdp_control = L253.delete_gdp_control,
                  L253.EF_retrofit_USA = L253.EF_retrofit_USA,
                  L253.EF_NSPS_new_vintage_USA = L253.EF_NSPS_new_vintage_USA,
                  L253.Retrofit_off_USA = L253.Retrofit_off_USA,
                  L253.delete_gdp_control_USA = L253.delete_gdp_control_USA)

      return(out)
    } # end process_em_control_data

    # Run function twice, one for core emissions control data and one for user emissions control data
    outputs <- process_em_control_data(em_ctrl_parameters)
    outputs_user <- process_em_control_data(user_em_ctrl_parameters)

    # Get specific outputs from outputs list
    L253.EF_retrofit <- outputs$L253.EF_retrofit
    L253.EF_NSPS_new_vintage <- outputs$L253.EF_NSPS_new_vintage
    L253.Retrofit_off  <- outputs$L253.Retrofit_off
    L253.delete_gdp_control <- outputs$L253.delete_gdp_control
    L253.user_EF_retrofit <- outputs_user$L253.EF_retrofit
    L253.user_EF_NSPS_new_vintage <- outputs_user$L253.EF_NSPS_new_vintage
    L253.user_Retrofit_off  <- outputs_user$L253.Retrofit_off
    L253.user_delete_gdp_control <- outputs_user$L253.delete_gdp_control
    L253.EF_retrofit_USA <- outputs$L253.EF_retrofit_USA
    L253.EF_NSPS_new_vintage_USA <- outputs$L253.EF_NSPS_new_vintage_USA
    L253.Retrofit_off_USA  <- outputs$L253.Retrofit_off_USA
    L253.delete_gdp_control_USA <- outputs$L253.delete_gdp_control_USA
    L253.user_EF_retrofit_USA<- outputs_user$L253.EF_retrofit_USA
    L253.user_EF_NSPS_new_vintage_USA <- outputs_user$L253.EF_NSPS_new_vintage_USA
    L253.user_Retrofit_off_USA  <- outputs_user$L253.Retrofit_off_USA
    L253.user_delete_gdp_control_USA <- outputs_user$L253.delete_gdp_control_USA

    return_data(L253.EF_retrofit,
                L253.EF_NSPS_new_vintage,
                L253.Retrofit_off,
                L253.delete_gdp_control,
                L253.user_EF_retrofit,
                L253.user_EF_NSPS_new_vintage,
                L253.user_Retrofit_off,
                L253.user_delete_gdp_control,
                L253.EF_retrofit_USA,
                L253.EF_NSPS_new_vintage_USA,
                L253.Retrofit_off_USA,
                L253.delete_gdp_control_USA,
                L253.user_EF_retrofit_USA,
                L253.user_EF_NSPS_new_vintage_USA,
                L253.user_Retrofit_off_USA,
                L253.user_delete_gdp_control_USA)

  } else {
    stop("Unknown command")
  }
}
