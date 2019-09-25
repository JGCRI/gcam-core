#' module_gcamusa_LA1233.Process_UCS_data_ref
#'
#' This script reads in and processes "Union of Concerned Scientists. 2012. UCS EW3 Energy-Water Database V.1.3" www.ucsusa.org/ew3database - Main Data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{LA1233.CoolingSystemShares_RG3_ref}, The corresponding file in the
#' original data system was \code{LA1233.Process_UCS_data_ref.R} (gcam-usa-processing-code level1).
#' @details This script reads in and processes "Union of Concerned Scientists. 2012. UCS EW3 Energy-Water
#' Database V.1.3" - Main Data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @importFrom tibble as_tibble tibble
#' @author LL March 2017, ZK Sep 2018, NTG Aug 2019

module_gcamusa_LA1233.Process_UCS_data_ref <- function(command, ...) {
 if(command == driver.DECLARE_INPUTS) {
  return(c(FILE = "gcam-usa/UCS_Database",
       FILE = "gcam-usa/states_subregions",
       FILE = "gcam-usa/elec_tech_water_map"
       ))
 } else if(command == driver.DECLARE_OUTPUTS) {
  return(c("LA1233.CoolingSystemShares_RG3_ref"))
 } else if(command == driver.MAKE) {

  all_data <- list(...)[[1]]

  # Silence package notes
  plant_type <- water_type <- fuel <- technology <- State <- NEMS <- cooling_system <-
   Fuel <- 'Reported Water Source (Type)' <- 'Generation Technology' <- 'Cooling Technology' <-
   state <- 'First Year of Operation' <- 'Nameplate Capacity (MW)' <- Cap_MW <- Cap_MW_US <- Cap_MW_Final <- year <-
   . <- 'cooling pond' <- 'dry cooling' <- dry_hybrid <- none <- 'once through' <- recirculating <-
   sum_Cap_MW <- sum_byTech_Cap_MW <- Cap_MW_State <- Cap_MW_Total <- NULL

  # == == == == == == == == == == == == == == == == == == == == == == == == == =
  # Load required inputs

  #' Note 1: The states_subregions raw file was modified to include NEMS region mappings.
  #' In states_subregion.csv row 65, West Virginia Central East Grid has subregion4 as South, insted of Midwest
  #' as in the harmonization folder.
  #' Note 2: The file elec_tech_water_map has changed and has different mappings.
  #' Note 3: UCS_Database used blanks instead of N/A in new database. Also the "generation technology" column was missing
  #' and needed to be added to the new database.

  UCS_Database <- get_data(all_data, "gcam-usa/UCS_Database") # Union of concerned scientists Energy Water Database
  states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
  elec_tech_water_map <- get_data(all_data, "gcam-usa/elec_tech_water_map")

  # == == == == == == == == == == == == == == == == == == == == == == == == == =
  # Modify Data (Remove NAs, reclassify Types, Change Fuel and Cooling Technologies to GCAM syntax)
  # Note: In original code some of these modifications were done in the for loop at line 98.
  # These modifications have been shifted here outside that loop.

  UCS_Database %>%
    filter((!is.na(Fuel) | Fuel != "NA") & `Reported Water Source (Type)` != "Unknown") %>%
    filter(!`Generation Technology`%in% c("CT", "WN")) %>% #Remove Combustion Turbine and Wind
    mutate(`Reported Water Source (Type)` = case_when(
                  `Reported Water Source (Type)` %in% c("Surface Water", "Municipal", "Groundwater", "Unknown Freshwater",
                                                        "Waste Water", "GW/Waste Water", "GW/Surface Water",
                                                        "GW/Municipal", "GW/fresh") ~ "fresh",
                  `Reported Water Source (Type)` %in% c("Ocean", "Unknown Ocean", "Unknown seawater") ~ "seawater",
                  `Cooling Technology` == "Dry Cooled" ~ "fresh",
                  TRUE ~ `Reported Water Source (Type)`),
            # GCAM Fuels
            Fuel = case_when(Fuel == "Hydropower" ~ "hydro",
                             Fuel == "Natural Gas" ~ "gas",
                             Fuel == "Oil" ~ "refined liquids",
                             TRUE ~ tolower(Fuel)),
            `Cooling Technology` = case_when(`Cooling Technology` == "Dry Cooled" ~ "dry cooling",
                                              TRUE ~ `Cooling Technology`),
            `Cooling Technology` = gsub("-", " ", `Cooling Technology`),
            `Cooling Technology` = tolower(`Cooling Technology`)) ->
   UCS_Database_Edited


  # NOTE: In original code after filtering and renaming, data left with only 10330 rows.
  # In new code we have 13890 rows. If we open the .csv file and manually filter the rows
  # to remove NA fuel and "unknown" Reported water source) we get 13890 rows.

  # Create data frame for complete set of technologies for each state and join NEMS region
  elec_tech_water_map %>%
   select(plant_type, cooling_system, water_type, fuel, technology) %>%
   distinct %>%
   repeat_add_columns(tibble(state = unique(states_subregions$state))) %>%
   left_join_error_no_match(states_subregions %>%
         select(state, NEMS), by = "state") %>%
   rename(State = state) ->
  complete_tech

  # NOTE: Original Script edits offshore states. Line 89-95 of original code.
  # Offshore wind is not included in this proposal and will be added in future proposals.

  # == == == == == == == == == == == == == == == == == == == == == == == == == =
  # Calculate Cooling technology share

  #---------------------------------
  # Historical cooling shares
  # For each year calculate cumulative cooling share upto that year & modify names to GCAM syntax

  cooling_share <- NULL; # initiate cooling share data frame

  for(year_i in gcamusa.UCS_WATER_COEFFICIENTS_FIRST_HISTORICAL_YEAR:gcamusa.UCS_WATER_COEFFICIENTS_FINAL_HISTORICAL_YEAR){

  UCS_Database_Edited %>%
   filter(`First Year of Operation` <= year_i) %>%
   group_by(State, Fuel, `Generation Technology`, `Cooling Technology`, `Reported Water Source (Type)`) %>%
   summarise(Cap_MW = sum(`Nameplate Capacity (MW)`)) %>%
   ungroup() %>%
   mutate(plant_type = case_when((Fuel == "gas" & `Generation Technology` == "CC") ~ "combined cycle",
                (Fuel == "refined liquids" & `Generation Technology` == "CC") ~ "CC",
                (Fuel == "solar" & `Generation Technology` == "SU") ~ "CSP",
                (Fuel %in% c("solar", "wind", "hydro")) ~ "no cooling",
                (Fuel %in% c("gas", "refined liquids", "biomass")) ~ "fossil non-coal",
                TRUE ~ Fuel)) %>%
   group_by(State, plant_type, `Cooling Technology`, `Reported Water Source (Type)`, Fuel) %>%
   summarise(Cap_MW = sum(Cap_MW)) %>%
   ungroup() ->
   capacity_tech

  # Note: Values are different when compared with old script. However, when manually summing from original
  # UCS_database_ref.csv the values match results from this script.

  capacity_tech %>%
   group_by(State, plant_type, Fuel) %>%
   summarise(Cap_MW = sum(Cap_MW)) %>%
   ungroup() ->
  capacity_tech_state


  capacity_tech %>%
    left_join_error_no_match(capacity_tech_state %>% rename (Cap_MW_State = Cap_MW),
                             by = c("State", "plant_type", "Fuel")) %>%
    mutate(Cap_MW = Cap_MW/Cap_MW_State,
          year = year_i) %>%
    select(-Cap_MW_State) ->
   capacity_tech_cooling


  cooling_share <- bind_rows(cooling_share, capacity_tech_cooling)

 } # Close loop for years


  cooling_share %>%
   distinct %>%
   as_tibble() %>%
   left_join_error_no_match(states_subregions %>%
                              select(State = state, NEMS),
                            by = "State") ->
   cooling_share

  # Repeat cooling technology distribution for all technologies
  # using left_join because cooling_share does not have any wind technologies and gives and error with the match
  left_join((complete_tech %>%
                              distinct %>%
                              rename(Fuel = fuel,
                                     `Reported Water Source (Type)` = water_type,
                                     `Cooling Technology` = cooling_system)),
                              cooling_share,
                           by = c("plant_type", "Fuel", "State", "NEMS", "Reported Water Source (Type)", "Cooling Technology")) %>%
    na.omit ->
   cooling_share_historical

  #-----------------------------------------
  # Calculate Cooling Shares for future years
  # Note: In original code for some reason states were dropped and analysis done by NEMS regions
  # and then later states added in again. Here analysis is done by state.


  UCS_Database_Edited %>%
   filter(`First Year of Operation` > 1999) %>%
   group_by(State, Fuel, `Generation Technology`, `Cooling Technology`, `Reported Water Source (Type)`) %>%
   summarise(Cap_MW = sum(`Nameplate Capacity (MW)`)) %>%
   ungroup() %>%
   mutate(plant_type = case_when((Fuel == "gas" & `Generation Technology` == "CC") ~ "combined cycle",
                (Fuel == "refined liquids" & `Generation Technology` == "CC") ~ "CC",
                (Fuel == "solar" & `Generation Technology` == "SU") ~ "CSP",
                (Fuel %in% c("solar", "wind", "hydro")) ~ "no cooling",
                (Fuel %in% c("gas", "refined liquids", "biomass")) ~ "fossil non-coal",
                TRUE ~ Fuel)) %>%
   group_by(State, plant_type, `Cooling Technology`, `Reported Water Source (Type)`, Fuel) %>%
   summarise(Cap_MW = sum(Cap_MW)) %>%
   ungroup() ->
  capacity_tech_future

  # Compute shares of cooling technology by fuel
  capacity_tech_future %>%
   group_by(Fuel, `Cooling Technology`, `Reported Water Source (Type)`) %>%
   summarise(Cap_MW = sum(Cap_MW)) %>%
   ungroup() ->
  capacity_tech_future_US

  # Calculate Total for US by fuel
  capacity_tech_future_US %>%
   group_by(Fuel) %>%
   summarise(Cap_MW = sum(Cap_MW)) %>%
   ungroup() ->
  capacity_tech_future_US_Total

  # Set year = the first futue year as we have changed 2010 to now be represented by
  # historical cooling shares
  capacity_tech_future_US %>%
    left_join_error_no_match(capacity_tech_future_US_Total %>%
                               rename (Cap_MW_Total = Cap_MW),
                             by = c("Fuel")) %>%
    mutate(Cap_MW = Cap_MW/Cap_MW_Total,
           year = gcamusa.UCS_WATER_COEFFICIENTS_FIRST_FUTURE_YEAR) %>%
    select(-Cap_MW_Total) ->
    cooling_share_future_US


  # Aggregate by State & Fuel
  capacity_tech_future %>%
   group_by(State, plant_type, Fuel) %>%
   summarise(Cap_MW = sum(Cap_MW)) %>%
   ungroup() ->
   capacity_tech_future_state

  capacity_tech_future %>%
    left_join_error_no_match(capacity_tech_future_state %>%
                               rename (Cap_MW_Total = Cap_MW),
                             by = c("State", "Fuel", "plant_type")) %>%
   mutate(Cap_MW = Cap_MW/Cap_MW_Total) %>%
   select(-Cap_MW_Total)->
   cooling_share_future_state

  # --------------------------------
  # Cooling Share complete list
  # If cooling share availble by State, use that else
  # assume the cooling share for US total

  cooling_share_future_state %>%
    full_join(cooling_share_future_US %>%
                rename(Cap_MW_US = Cap_MW),
              by = c("Cooling Technology", "Reported Water Source (Type)", "Fuel")) ->
   cooling_share_future

  complete_tech %>%
    rename(Fuel = fuel, `Cooling Technology` = cooling_system, `Reported Water Source (Type)` = water_type) %>%
    distinct %>%
    # Using left_join becaue cooling_share_future does not have wind as a fuel in it
    left_join(cooling_share_future,
              by = c("Fuel", "Cooling Technology", "plant_type", "Reported Water Source (Type)", "State")) %>%
    mutate(Cap_MW = case_when(is.na(Cap_MW) ~ 0, TRUE ~ Cap_MW),
           Cap_MW_US = case_when(is.na(Cap_MW_US) ~ 0, TRUE ~ Cap_MW_US),
           Cap_MW_Final = case_when((Cap_MW == 0 & Cap_MW_US == 0) ~ 0,
                                    (Cap_MW == 0 & Cap_MW_US>0) ~ Cap_MW_US,
                                    TRUE ~ Cap_MW)) %>%
    select(-Cap_MW, -Cap_MW_US, Cap_MW = Cap_MW_Final) %>%
    mutate(`Cooling Technology` = case_when(plant_type == "no cooling" ~ "none", TRUE ~ `Cooling Technology`),
           `Reported Water Source (Type)` = case_when(plant_type == "no cooling" ~ "fresh", TRUE ~ `Reported Water Source (Type)`),
           year = gcamusa.UCS_WATER_COEFFICIENTS_FIRST_FUTURE_YEAR) ->
   cooling_share_future_complete_tech_FIRST_FUTURE_YEAR

  # repeat and add years final claibration year, first future year, final future year (from constants.R)
  cooling_share_future_complete_tech_FIRST_FUTURE_YEAR %>%
    bind_rows(cooling_share_future_complete_tech_FIRST_FUTURE_YEAR %>% mutate(year = gcamusa.UCS_WATER_COEFFICIENTS_FINAL_FUTURE_YEAR)) ->
    cooling_share_future_complete_tech

  # Combine into one Cooling Share Data Frame
  # All 'no cooling' has cooling_type none and is assigned 1
  # As per original script all historical Gen_III is made 0
  cooling_share_historical %>%
    bind_rows(cooling_share_historical %>% filter(year == gcamusa.UCS_WATER_COEFFICIENTS_FINAL_CALIBRATION_YEAR) %>% mutate(year = gcamusa.UCS_WATER_COEFFICIENTS_FINAL_HISTORICAL_YEAR)) %>%
    bind_rows(cooling_share_future_complete_tech) %>%
    distinct %>%
    mutate(Cap_MW = case_when(plant_type == "no cooling" ~ 1,
                              (technology == "Gen_III" & year < gcamusa.UCS_WATER_COEFFICIENTS_FINAL_CALIBRATION_YEAR) ~ 0,
                              TRUE ~ Cap_MW)) ->
  complete_tech_cooling_share

  # ----------------------------------------
  # Assumptions for Unassigned Cooling technologies in the Future
  # If sum of cooling technologies across water_types is less than 1, then assign all remaining shares to recirculating
  # The Following assumptions are defined in constants.R and based on:
  # "Davies, Evan GR, Page Kyle, and James A. Edmonds. "An integrated assessment of global and regional water
  # demands for electricity generation to 2095." Advances in Water Resources 52 (2013): 296-313."
  # https://www.sciencedirect.com/science/article/pii/S0309170812003028?via%3Dihub

  # Sum of cooling techs across seawater and freshwater do no exceed 1
  # CSP storage and regular after year gcamusa.UCS_WATER_COEFFICIENTS_FIRST_FUTURE_YEAR has recirculating assigned to 1.
  # In original script all fresh water geothermal recirculating is assigned 1
  # In original script both dry-hybrid and recirculaitng were assigned 1 which did not add up..

  # Group and get summed capacity by technology
  complete_tech_cooling_share %>%
    group_by(plant_type, Fuel, technology, State, NEMS, year) %>%
    summarise(sum_byTech_Cap_MW = sum(Cap_MW, na.rm = TRUE)) %>%
    ungroup()->
    complete_tech_cooling_share_groupByTech

  # Join with complete list of technologies and adjust values based on assumptions listed above
  complete_tech_cooling_share %>%
    spread(key = `Cooling Technology`, value = Cap_MW) %>%
    left_join_error_no_match(complete_tech_cooling_share_groupByTech,
                           by = c("plant_type", "Fuel", "technology", "State", "NEMS", "year")) %>%
    mutate(sum_Cap_MW = rowSums(select(., `cooling pond`, `dry cooling`, dry_hybrid, none,
                                       `once through`, recirculating), na.rm = TRUE),
           `dry cooling` = case_when((!is.na(`dry cooling`) & sum_byTech_Cap_MW <
                                        gcamusa.UCS_WATER_COEFFICIENTS_FUTURE_ASSUMPTION_RECIRCULATING &
                                        year == gcamusa.UCS_WATER_COEFFICIENTS_FIRST_FUTURE_YEAR &
                                        (Fuel == "gas" | Fuel == "biomass" |
                                           Fuel == "coal" | Fuel == "refined liquids") &
                                        `Reported Water Source (Type)` == "fresh") ~
                                       gcamusa.UCS_WATER_COEFFICIENTS_FUTURE_ASSUMPTION_DRY_COOLING,
                                     TRUE ~ `dry cooling`),
           `cooling pond` = case_when((!is.na(`cooling pond`) & sum_byTech_Cap_MW <
                                         gcamusa.UCS_WATER_COEFFICIENTS_FUTURE_ASSUMPTION_RECIRCULATING &
                                         year == gcamusa.UCS_WATER_COEFFICIENTS_FIRST_FUTURE_YEAR &
                                         (Fuel == "gas" | Fuel == "nuclear" | Fuel == "biomass" | Fuel == "coal" |
                                            Fuel == "refined liquids") &
                                         `Reported Water Source (Type)` == "fresh") ~
                                        gcamusa.UCS_WATER_COEFFICIENTS_FUTURE_ASSUMPTION_COOLING_POND,
                                      TRUE ~ `cooling pond`),
           `once through` = case_when((!is.na(`once through`) & sum_byTech_Cap_MW <
                                       gcamusa.UCS_WATER_COEFFICIENTS_FUTURE_ASSUMPTION_RECIRCULATING &
                                         year == gcamusa.UCS_WATER_COEFFICIENTS_FIRST_FUTURE_YEAR &
                                         (Fuel == "gas" | Fuel == "nuclear" | Fuel == "biomass" |
                                                           Fuel == "coal" | Fuel == "refined liquids") &
                                         `Reported Water Source (Type)` == "seawater") ~
                                        gcamusa.UCS_WATER_COEFFICIENTS_FUTURE_ASSUMPTION_ONCE_THROUGH_SEAWATER,
                                      TRUE ~ `once through`),
           sum_Cap_MW = rowSums(select(., `cooling pond`, `dry cooling`, dry_hybrid, none, `once through`, recirculating), na.rm = TRUE)) %>%
    ungroup() ->
   complete_tech_cooling_share_Edited


  complete_tech_cooling_share_Edited %>%
    select(-sum_Cap_MW, -sum_byTech_Cap_MW)%>%
    gather(key = `Cooling Technology`,
           value = Cap_MW, -Fuel, -technology, -State, -plant_type, -`Reported Water Source (Type)`, -year, -NEMS) %>%
    group_by(plant_type, Fuel, technology, State, NEMS, year) %>%
    summarise(sum_byTech_Cap_MW = sum(Cap_MW, na.rm = TRUE)) %>%
    ungroup() ->
    complete_tech_cooling_share_Edited_sumbyTech


  # Adjmusts made to account for all cases of sum_byTech_Cap_MW, as two cases were missing
  complete_tech_cooling_share_Edited %>%
    select(-sum_byTech_Cap_MW) %>%
    left_join_error_no_match(complete_tech_cooling_share_Edited_sumbyTech,
                             by = c("plant_type", "Fuel", "technology", "State", "NEMS", "year")) %>%
    mutate(recirculating = case_when(is.na(recirculating) ~ 0,
                                     TRUE ~ recirculating),
           recirculating = case_when((sum_byTech_Cap_MW != 0 & sum_byTech_Cap_MW != 1 & `Reported Water Source (Type)` == "fresh") ~
                                       recirculating + (1 - sum_byTech_Cap_MW),
                                     (Fuel == "solar CSP" & `Reported Water Source (Type)` == "fresh") ~ recirculating+(1-sum_byTech_Cap_MW),
                                     (Fuel == "geothermal" & `Reported Water Source (Type)` == "fresh") ~ recirculating+(1-sum_byTech_Cap_MW),
                                     sum_byTech_Cap_MW == 0 ~ 1,
                                     sum_byTech_Cap_MW == 1 ~ recirculating,
                                     TRUE ~ recirculating),
           sum_Cap_MW = rowSums(select(., `cooling pond`, `dry cooling`, dry_hybrid, none, `once through`, recirculating), na.rm = TRUE)) %>%
    select(-sum_Cap_MW, -sum_byTech_Cap_MW) %>%
    gather(key = `Cooling Technology`, value = Cap_MW, -Fuel, -technology, -State, -plant_type, -`Reported Water Source (Type)`, -year, -NEMS) %>%
    distinct() %>%
    spread(key = year, value = Cap_MW) %>%
    # Simple extrapolation from first future year to final future year
    mutate(!!as.name(gcamusa.UCS_WATER_COEFFICIENTS_FINAL_CALIBRATION_YEAR) := !!as.name(gcamusa.UCS_WATER_COEFFICIENTS_FINAL_HISTORICAL_YEAR),
           !!as.name(gcamusa.UCS_WATER_COEFFICIENTS_FINAL_FUTURE_YEAR) := !!as.name(gcamusa.UCS_WATER_COEFFICIENTS_FIRST_FUTURE_YEAR)) %>%
    rename(fuel = Fuel, water_type = `Reported Water Source (Type)`, cooling_system = `Cooling Technology`) %>%
    replace(., is.na(.), 0) %>%
    ungroup() ->
   LA1233.CoolingSystemShares_RG3_ref


  # Future development: code related to off-shore wind technologies will go here.


  # NOTES: Differences from original script data output
  # Note 1: All "no cooling" plant_types are now assinged "fresh" water_type. In old data wind
  # and rooftop_pv were assigned "none" water_type while hydro, PV, PV_storage were assigned "fresh".
  # Note 2: In CA for fuel == coal the original script uses a US average distribution. However, in this script when
  # future technologies do not exist in a state (past 1999) in the UCS database then it is assumed that those technologies do not have
  # a representative share based on the US average. The distribution is based on the assumptions made above for dry cooling, recirculating,
  # once through.
  # Note 3: rooftop_pv in the original script has no shares assigned to it. In this script all technologies have summed shares == 1. In this
  # case the cooling system is "none" with a share of 1.


  # == == == == == == == == == == == == == == == == == == == == == == == == == =
  # Produce Outputs

  LA1233.CoolingSystemShares_RG3_ref %>%
   add_title("Historical and future power plant cooling technology shares by state") %>%
   add_units("ratio") %>%
   add_comments("Written by LA1233.Process_UCS_data_ref.R") %>%
   add_legacy_name("LA1233.CoolingSystemShares_RG3_ref") %>%
   add_precursors(
           "gcam-usa/UCS_Database",
           "gcam-usa/states_subregions",
           "gcam-usa/elec_tech_water_map"
           ) ->
   LA1233.CoolingSystemShares_RG3_ref

  return_data(LA1233.CoolingSystemShares_RG3_ref)
 } else {
  stop("Unknown command")
 }
}
