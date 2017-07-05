#' module_water_L145.water.demand.municipal
#'
#' It is used to generate 3 files:
#' 1. Municipal water withdrawals by GCAM_region_ID for all historical years
#' 2. Municipal water base deleivery cost by GCAM_region_ID
#' 3. Municipal water use efficiency by GCAM_region_ID for all years
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L145.municipal_water_R_W_Yh_km3}, \code{L145.municipal_water_cost_R_75USD_m3}, \code{L145.municipal_water_eff_R_Y}. The corresponding file in the
#' original data system was \code{L145.water.demand.municipal.R} (water level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YL May 2017
#' @export

module_water_L145.water.demand.municipal <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "aglu/AGLU_ctry",
             FILE = "water/FAO_municipal_water_AQUASTAT",
             FILE = "water/IBNET_municipal_water_cost_USDm3",
             FILE = "water/municipal_water_use_efficiency",
             FILE = "water/manufacturing_water_mapping"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L145.municipal_water_R_W_Yh_km3",
             "L145.municipal_water_cost_R_75USD_m3",
             "L145.municipal_water_eff_R_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_municipal_water_AQUASTAT <- get_data(all_data, "water/FAO_municipal_water_AQUASTAT")
    IBNET_municipal_water_cost_USDm3 <- get_data(all_data, "water/IBNET_municipal_water_cost_USDm3")
    municipal_water_use_efficiency <- get_data(all_data, "water/municipal_water_use_efficiency")
    manufacturing_water_mapping <- get_data(all_data, "water/manufacturing_water_mapping")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    #
    # Aggregate FAO_municipal_water_AQUASTAT to GCAM regions and fill out the years for missing data.

    #Map FAO country to GCAM region ID
    iso_GCAM_regID %>% select(iso, GCAM_region_ID) %>%
      inner_join(AGLU_ctry[,c("iso","FAO_country")], by ="iso") -> FAO_ctry_GCAM_region_ID

    FAO_municipal_water_AQUASTAT %>%
      rename ( FAO_country =Area ) %>%
      inner_join(FAO_ctry_GCAM_region_ID[, c( "GCAM_region_ID", "FAO_country" ) ], by ="FAO_country") -> ctry_municipal_W

    #Aggregate to regions and fill out missing years using rule=2
    ctry_municipal_W %>%
      group_by(GCAM_region_ID, Year) %>%
      summarise(Value = sum(Value)) %>%
      ungroup() %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               Year = HISTORICAL_YEARS,
               fill = list(Value = NA))%>%
      group_by(GCAM_region_ID)%>%
      mutate(Value = approx_fun(Year, Value, rule=2)) %>%
      rename(value = Value, year =Year) %>%
      mutate(water_type ="water withdrawals")-> municipal_water_R_W_Yh_km3

    # Come up with GCAM reginal average prices starting with the country level IBNET data.
    # Note since the years are all over the place we will just use the average across years too.
    #Come up with regional municipal prices from IBNET data
    IBNET_municipal_water_cost_USDm3 %>%
      mutate(FAO_country = country)%>%
      inner_join(FAO_ctry_GCAM_region_ID, by="FAO_country")%>%
      mutate(expenditure = cost * consumption)  %>%
      group_by(GCAM_region_ID)%>%
      summarise(expenditure = sum(expenditure), consumption=sum(consumption))%>%
      mutate(input.cost = expenditure/consumption)%>%
      select(GCAM_region_ID, input.cost)-> municipal_water_cost_R_75USD_m3

    # The IBNET data is incomplete and so it is very possible that we have entire GCAM regions in which none
    # of the contained countries had any reported data.
    stopifnot( nrow( municipal_water_cost_R_75USD_m3 ) == nrow( GCAM_region_names ) )

    # Map water use efficiencies from the continent scale to GCAM regions
    # Note names were changed to match manufacturing continent to avoid the need for an additional mapping

    #create the variable year2 for temporal interpolation
    year2 <- sort(c(1900, 1940, 1950, 1960, 1970, 1980, 1990, 1995, 2000,MODEL_YEARS[-2]))

    #Map municipal water use efficiencies to GCAM regions
    municipal_water_use_efficiency %>%
      gather(variable,value,-continent)%>%
      mutate(variable = as.numeric(variable))%>%
      rename (year=variable) %>%
      inner_join(manufacturing_water_mapping, by = "continent") %>%
      inner_join(GCAM_region_names, by = "region") %>%
      select(-continent, -GCAM_region_ID) %>%
      complete(region = unique(GCAM_region_names$region),
               year = year2,
               fill = list(value = NA)) %>%
      group_by(region) %>%
      #Fillout the coefficients for all years using rule=2
      mutate(coefficient = approx_fun(year, value, rule=2)) %>%
      left_join_keep_first_only(GCAM_region_names, by="region")%>%
      filter(year>=1975, year != 1980, year !=1995, year !=2000)%>%
      ungroup() %>%
      select(GCAM_region_ID,year,coefficient) %>%
      arrange(GCAM_region_ID, year) -> municipal_water_eff_R_Y


    # NOTE: there are `merge` calls in this code. Be careful!
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    municipal_water_R_W_Yh_km3 %>%
      add_title("Municipal water withdrawals by GCAM_region_ID for all historical years ") %>%
      add_units("km^3") %>%
      add_comments("Aggregate FAO_municipal_water_AQUASTAT to GCAM regions and fill out the years for missing data by rule=2") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L145.municipal_water_R_W_Yh_km3") %>%
      add_precursors("common/iso_GCAM_regID", "common/GCAM_region_names", "aglu/AGLU_ctry",
                     "water/FAO_municipal_water_AQUASTAT",
                     "water/IBNET_municipal_water_cost_USDm3",
                     "water/municipal_water_use_efficiency",
                     "water/manufacturing_water_mapping") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L145.municipal_water_R_W_Yh_km3


    municipal_water_cost_R_75USD_m3 %>%
      add_title("Municipal water base deleivery cost by GCAM_region_ID") %>%
      add_units("1975$/m^3") %>%
      add_comments("Gnereate GCAM reginal average prices starting with the country-level IBNET data") %>%
      add_comments("1. get country-level expenditure (cost * consumption);
                   2. sum up country-level expenditure and cost to get region-level expenditure and cost;
                   3. divide region-level expenditure by region-level consumption to get region-level cost") %>%
      add_legacy_name("L145.municipal_water_cost_R_75USD_m3") %>%
      add_precursors("common/iso_GCAM_regID", "common/GCAM_region_names", "aglu/AGLU_ctry",
                     "water/FAO_municipal_water_AQUASTAT",
                     "water/IBNET_municipal_water_cost_USDm3",
                     "water/municipal_water_use_efficiency",
                     "water/manufacturing_water_mapping") ->L145.municipal_water_cost_R_75USD_m3


    municipal_water_eff_R_Y %>%
      add_title("Municipal water use efficiency by GCAM_region_ID for all years") %>%
      add_units("%") %>%
      add_comments("The data is generated through:") %>%
      add_comments("1. Map water use efficiencies from the continent scale to GCAM regions;
                   2. Fillout the coefficients for all years using rule=2") %>%
      add_legacy_name("L145.municipal_water_eff_R_Y") %>%
      add_precursors("common/iso_GCAM_regID", "common/GCAM_region_names", "aglu/AGLU_ctry",
                     "water/FAO_municipal_water_AQUASTAT",
                     "water/IBNET_municipal_water_cost_USDm3",
                     "water/municipal_water_use_efficiency",
                     "water/manufacturing_water_mapping") ->  L145.municipal_water_eff_R_Y

    return_data(L145.municipal_water_R_W_Yh_km3, L145.municipal_water_cost_R_75USD_m3, L145.municipal_water_eff_R_Y)
  } else {
    stop("Unknown command")
  }
}
