#' module_aglu_LA105B.food_demand_bias
#'
#' Calculate bias corrections for food demand staples and non staples
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L105.an_Food_Mt_R_C_Y}, \code{L105.an_Food_Pcal_R_C_Y}, \code{L105.an_kcalg_R_C_Y}, \code{L105.an_Prod_Mt_R_C_Y}, \code{L105.an_Prod_Mt_ctry_C_Y}. The corresponding file in the
#' original data system was \code{LA105.an_FAO_R_C_Y.R} (aglu level1).
#' @details This chunk aggregates FAO animal products food consumption and production data up to GCAM commodities and GCAM regions,
#' and calculates the average animal products caloric content by GCAM region / commodity / year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC June 2017
module_aglu_LA105B.food_demand_bias <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/Food_Demand_Training_Data",
             FILE = "aglu/Food_demand_parameters"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L105B.Food_Demand_Bias"
             ))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    Country <- D_driver <- Deforest <- DeforestEmiss <- FF_driver <- ForestFire <-
      ForestFireEmiss <- GCAM_commodity <- GCAM_region_ID <- GLU <- GTAP_crop <- HA <-
      HA_kha_irrigated <- HA_kha_rainfed <- Irr_Rfd <- Land_Type <- Mcal_t <- Mt <- Mult <-
      Non.CO2 <- Pcal <- PctForestFire <- Prod_kt_irrigated <- Prod_kt_rainfed <- Prod_mod <-
      YieldRate <- YieldRatio <- YieldRatio_lag <- Yield_kgHa_irrigated <-
      Yield_kgHa_rainfed <- country_ID <- crop_ID <- defaultRate <- em_factor <- irrHA <- iso <-
      lagyear <- lcf <- pop2 <- rfdHA <- sav <- setNames <- technology <- timestep <- value <- year <-
      yield_kgHa <- NULL   # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    Food_Demand_Training_Data <- get_data(all_data, "aglu/Food_Demand_Training_Data")
    Food_demand_parameters <- get_data(all_data, "aglu/Food_demand_parameters")

    # Process Demand Data

    Food_Demand_Training_Data %>%
      #Some cleaning. This is also applied when calculating the parameters
      #filter(Tot_cal>1.7) %>%
      #filter(ns_usd_p1000cal<20) %>%
      #Join in ISO
      left_join(iso_GCAM_regID %>% select(iso,GCAM_region_ID),by= c("iso")) %>%
      na.omit() %>%
      #Now group_by GCAM_region_ID, year and calculate values
      group_by(GCAM_region_ID,year) %>%
      mutate(Y = sum(pop_thous*gdp_pcap_thous2005usd)/sum(pop_thous),
             Ps = (sum(s_cons_thous_t*cons_price_s)/sum(s_cal_pcap_day_thous*pop_thous*365))*0.365,
             Pn = (sum(ns_cons_thous_t*cons_price_ns)/sum(ns_cal_pcap_day_thous*pop_thous*365))*0.365,
             Qs = sum(s_cal_pcap_day_thous*pop_thous)/sum(pop_thous),
             Qn = sum(ns_cal_pcap_day_thous*pop_thous)/sum(pop_thous)) %>%
      ungroup() %>%
      select(GCAM_region_ID,year,Ps,Pn,Qs,Qn,Y) %>%
      distinct()->Food_demand_Data_agg

    region_id <- c(unique(Food_demand_Data_agg$GCAM_region_ID))

    parameter_vector <- vec2param(Food_demand_parameters$params_vector.par)

    compute.bias.corrections <- function(params, obs)
    {
      . <- NULL

      #obs <- prepare.obs(obs.trn)
      obs <- split(obs, obs$rgn)
      params$bc <- sapply(obs, . %>% compute.bc.rgn(params))
    }

    Food_Demand_Bias <- compute.bias.corrections(params=parameter_vector,obs = Food_demand_Data_agg %>% rename(rgn= GCAM_REGION_ID) %>% filter(year<= 2010))

    L105B.Food_Demand_Bias <- as_tibble(t(Food_Demand_Bias))
    L105B.Food_Demand_Bias$GCAM_region_ID <- region_id

    L105B.Food_Demand_Bias %>%
      rename (FoodDemand_Staples=s, FoodDemand_NonStaples=n) %>%
      add_row(GCAM_region_ID= 28, FoodDemand_Staples=1, FoodDemand_NonStaples=1) %>%
      add_row(GCAM_region_ID= 30, FoodDemand_Staples=1, FoodDemand_NonStaples=1)->L105B.Food_Demand_Bias



    # Produce outputs
    L105B.Food_Demand_Bias %>%
      add_title("Food_Demand_Bias") %>%
      add_units("NA") %>%
      add_comments("Bias for the food demand model for staples and non-staples ") %>%
      add_legacy_name("L105B.Food_Demand_Bias") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/Food_Demand_Training_Data",
                     "aglu/Food_demand_parameters") ->
      L105B.Food_Demand_Bias

    return_data(L105B.Food_Demand_Bias)
  } else {
    stop("Unknown command")
  }
}
