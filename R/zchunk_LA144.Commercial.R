#' module_gcam.usa_LA144.Commercial
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.flsp_bm2_state_comm}, \code{L144.in_EJ_state_comm_F_U_Y}. The corresponding file in the
#' original data system was \code{LA144.Commercial.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH September 2017
#' @export
module_gcam.usa_LA144.Commercial <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/Census_pop_hist",
             FILE = "gcam-usa/CBECS_variables",
             FILE = "gcam-usa/EIA_AEO_Tab5",
             FILE = "gcam-usa/EIA_distheat", # efficiency units
             FILE = "gcam-usa/PNNL_Commext_elec",
             FILE = "gcam-usa/CBECS_1979_1983",
             FILE = "gcam-usa/CBECS_1986",
             FILE = "gcam-usa/CBECS_1989",
             FILE = "gcam-usa/CBECS_1992",
             FILE = "gcam-usa/CBECS_1995",
             FILE = "gcam-usa/CBECS_1999",
             FILE = "gcam-usa/CBECS_2003",
             FILE = "temp-data-inject/L142.in_EJ_state_bld_F",
             FILE = "temp-data-inject/L143.share_state_Pop_CDD_sR9",
             FILE = "temp-data-inject/L143.share_state_Pop_HDD_sR9"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.flsp_bm2_state_comm",
             "L144.in_EJ_state_comm_F_U_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    Census_pop_hist <- get_data(all_data, "gcam-usa/Census_pop_hist")
    CBECS_variables <- get_data(all_data, "gcam-usa/CBECS_variables")
    EIA_AEO_Tab5 <- get_data(all_data, "gcam-usa/EIA_AEO_Tab5")
    EIA_distheat <- get_data(all_data, "gcam-usa/EIA_distheat")
    PNNL_Commext_elec <- get_data(all_data, "gcam-usa/PNNL_Commext_elec")
    CBECS_1979_1983 <- get_data(all_data, "gcam-usa/CBECS_1979_1983")
    CBECS_1986 <- get_data(all_data, "gcam-usa/CBECS_1986")
    CBECS_1989 <- get_data(all_data, "gcam-usa/CBECS_1989")
    CBECS_1992 <- get_data(all_data, "gcam-usa/CBECS_1992")
    CBECS_1995 <- get_data(all_data, "gcam-usa/CBECS_1995")
    CBECS_1999 <- get_data(all_data, "gcam-usa/CBECS_1999")
    CBECS_2003 <- get_data(all_data, "gcam-usa/CBECS_2003")
    L142.in_EJ_state_bld_F <- get_data(all_data, "temp-data-inject/L142.in_EJ_state_bld_F") %>%
      # temp-data-inject
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))
    L143.share_state_Pop_CDD_sR9 <- get_data(all_data, "temp-data-inject/L143.share_state_Pop_CDD_sR9") %>%
      # temp-data-inject
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))
    L143.share_state_Pop_HDD_sR9 <- get_data(all_data, "temp-data-inject/L143.share_state_Pop_HDD_sR9") %>%
      # temp-data-inject
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))

    # ===================================================
    # 2a. PREPARATION AND CLEANING OF CBECS DATABASES
    # The 1979 and 1983 only have floorspace by census region; they aren't microdata and don't have energy information
    L144.CBECS_all <- list(CBECS_1986, CBECS_1989, CBECS_1992, CBECS_1995, CBECS_1999, CBECS_2003)
    names(L144.CBECS_all) <- paste0("CBECS", c( 1986, 1989, 1992, 1995, 1999, 2003))

    # In order to be able to work with these data across years, the "edition" number needs to be removed from all
    # variable names. E.g., re-naming square footage from "SQFT3" in 1986 and "SQFT4" in 1989 to "SQFT" in all.
    L144.CBECS_all <- lapply(L144.CBECS_all, setNames, sub("[0-9]{1}", "", names(CBECS_1986)))

    # Add a vector specifying the census region (subregion4) and census division (subregion9)
    # Census regions (subregion4) are used for 1979-1986 floorspace, as the first editions didn't have census divisions (subregion9)
    states_subregions_CBECS <- states_subregions %>%
      select(subregion4, subregion9, REGION, DIVISION) %>%
      distinct()
    L144.CBECS_all <- lapply(L144.CBECS_all, function(df){
      df <- df %>%
        left_join_error_no_match(states_subregions %>%
                                   select(subregion4, subregion9, REGION, DIVISION),
                                 by = c("REGION", "CENDIV" = "DIVISION"))
    })

     for( i in seq( L144.CBECS_all ) ){
      L144.CBECS_all[[i]][[ "subregion4" ]] <- states_subregions$subregion4[
        match( L144.CBECS_all[[i]][[ "REGION" ]], states_subregions$REGION ) ]
      L144.CBECS_all[[i]][[ "subregion9" ]] <- states_subregions$subregion9[
        match( L144.CBECS_all[[i]][[ "CENDIV" ]], states_subregions$DIVISION ) ]
    }

    #Convert all missing value strings to 0 in all databases
    L144.CBECS_all[["CBECS1992"]][is.na( L144.CBECS_all[["CBECS1992"]] ) ] <- 0
    L144.CBECS_all[["CBECS1995"]][L144.CBECS_all[["CBECS1995"]] == 1e14] <- 0
    L144.CBECS_all[["CBECS2003"]][is.na( L144.CBECS_all[["CBECS2003"]] ) ] <- 0

    #Aggregate population to the subregion9 and subregion9 levels for calculation of per-capita values
    Census_pop_hist[ c( "subregion4", "subregion9" ) ] <- states_subregions[
      match( Census_pop_hist$state, states_subregions$state ),
      c( "subregion4", "subregion9" ) ]
    L144.pop_sR4 <- aggregate( Census_pop_hist[ X_historical_years ], by=as.list( Census_pop_hist[ "subregion4" ] ), sum )
    L144.pop_sR9 <- aggregate( Census_pop_hist[ X_historical_years ], by=as.list( Census_pop_hist[ "subregion9" ] ), sum )


    # ===================================================

    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L144.flsp_bm2_state_comm") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop_hist",
                     "gcam-usa/CBECS_variables",
                     "gcam-usa/EIA_AEO_Tab5",
                     "gcam-usa/EIA_distheat",
                     "gcam-usa/PNNL_Commext_elec",
                     "gcam-usa/CBECS_1979_1983",
                     "gcam-usa/CBECS_1986",
                     "gcam-usa/CBECS_1989",
                     "gcam-usa/CBECS_1992",
                     "gcam-usa/CBECS_1995",
                     "gcam-usa/CBECS_1999",
                     "gcam-usa/CBECS_2003",
                     "temp-data-inject/L142.in_EJ_state_bld_F",
                     "temp-data-inject/L143.share_state_Pop_CDD_sR9",
                     "temp-data-inject/L143.share_state_Pop_HDD_sR9") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L144.flsp_bm2_state_comm

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L144.in_EJ_state_comm_F_U_Y") %>%
      add_precursors("gcam-usa/states_subregions") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L144.in_EJ_state_comm_F_U_Y

    return_data(L144.flsp_bm2_state_comm, L144.in_EJ_state_comm_F_U_Y)
  } else {
    stop("Unknown command")
  }
}
