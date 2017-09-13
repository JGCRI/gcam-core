#' module_gcam.usa_LA144.Residential
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.flsp_bm2_state_res}, \code{L144.in_EJ_state_res_F_U_Y}. The corresponding file in the
#' original data system was \code{LA144.Residential.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH September 2017
#' @export
module_gcam.usa_LA144.Residential <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/RECS_variables",
             FILE = "gcam-usa/EIA_AEO_fuels",
             FILE = "gcam-usa/EIA_AEO_services",
             FILE = "gcam-usa/Census_pop_hist",
             FILE = "gcam-usa/EIA_AEO_Tab4", # check units
             FILE = "gcam-usa/RECS_1979", # Finish documenting
             FILE = "gcam-usa/RECS_1984", # Finish documenting
             FILE = "gcam-usa/RECS_1990", # Finish documenting
             FILE = "gcam-usa/RECS_1993", # Finish documenting
             FILE = "gcam-usa/RECS_1997", # Finish documenting
             FILE = "gcam-usa/RECS_2001", # Finish documenting
             FILE = "gcam-usa/RECS_2005", # Finish documenting
             FILE = "gcam-usa/RECS_2009", # Finish documenting
             "L142.in_EJ_state_bld_F",
             FILE = "temp-data-inject/L143.share_state_Pop_CDD_sR13",
             FILE = "temp-data-inject/L143.share_state_Pop_HDD_sR13"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.flsp_bm2_state_res",
             "L144.in_EJ_state_res_F_U_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    RECS_variables <- get_data(all_data, "gcam-usa/RECS_variables")
    EIA_AEO_fuels <- get_data(all_data, "gcam-usa/EIA_AEO_fuels")
    EIA_AEO_services <- get_data(all_data, "gcam-usa/EIA_AEO_services")
    Census_pop_hist <- get_data(all_data, "gcam-usa/Census_pop_hist") %>%
      gather(year, value, matches(YEAR_PATTERN))
    EIA_AEO_Tab4 <- get_data(all_data, "gcam-usa/EIA_AEO_Tab4")
    RECS_1979 <- get_data(all_data, "gcam-usa/RECS_1979")
    RECS_1984 <- get_data(all_data, "gcam-usa/RECS_1984")
    RECS_1990 <- get_data(all_data, "gcam-usa/RECS_1990")
    RECS_1993 <- get_data(all_data, "gcam-usa/RECS_1993")
    RECS_1997 <- get_data(all_data, "gcam-usa/RECS_1997")
    RECS_2001 <- get_data(all_data, "gcam-usa/RECS_2001")
    RECS_2005 <- get_data(all_data, "gcam-usa/RECS_2005")
    RECS_2009 <- get_data(all_data, "gcam-usa/RECS_2009")
    L142.in_EJ_state_bld_F <- get_data(all_data, "L142.in_EJ_state_bld_F")
    L143.share_state_Pop_CDD_sR13 <- get_data(all_data, "temp-data-inject/L143.share_state_Pop_CDD_sR13") %>%
    # temp-data-inject code
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))
    L143.share_state_Pop_HDD_sR13 <- get_data(all_data, "temp-data-inject/L143.share_state_Pop_HDD_sR13") %>%
      # temp-data-inject code
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))

    # ===================================================
    # a) PREPARATION AND CLEANING OF RECS DATABASES
    # All RECS data has different columns, so we create a list rather than bind_rows
    L144.RECS_all <- list(RECS_1979, RECS_1984, RECS_1990, RECS_1993, RECS_1997, RECS_2001, RECS_2005, RECS_2009)
    names(L144.RECS_all) <- paste0("RECS", c(1979, 1984, 1990, 1993, 1997, 2001, 2005, 2009))

    # Add year column to each tibble in list
    for (i in 1:length(L144.RECS_all)){
      L144.RECS_all[[i]]$year <- substr(names(L144.RECS_all[i]), 5, 8) %>% as.integer()
    }

    # Add a vector specifying the census division (subregion9)
    L144.RECS_all <- L144.RECS_all %>%
      lapply(function(df){
        # 2009 had different census division numbers
        if (unique(df$year) == 2009){
          left_join_error_no_match(df,
                                   states_subregions %>% select(subregion9, DIVISION2009) %>% distinct,
                                   by = c("DIVISION" = "DIVISION2009"))
        }else{
          left_join_error_no_match(df,
                    states_subregions %>% select(subregion9, DIVISION) %>% distinct,
                    by = "DIVISION")
        }
      })

    # Add a vector specifying the census division plus four large states (subregion13)
    L144.RECS_all <- L144.RECS_all %>%
      lapply(function(df){
        # 2009 had different census division numbers
        if ("LRGSTATE" %in% names(df)){
          left_join_error_no_match(df,
                                   states_subregions %>% select(subregion13, LRGSTATE, subregion9) %>% distinct,
                                   by = c("LRGSTATE", "subregion9"))
        }else{
          df
        }
      })

    # The 2009 RECS uses 27 "reportable domains", which consist of 16 single states and 11 small clusters of states.
    # while this would enhance the geographic specificity of the RECS data, the small sample sizes cause obviously incorrect
    # data on aggregation (e.g., states with < 300 ft2 of residential floorspace per capita).
    # For this reason, we use subregion13, consistent with the 1993-2005 RECS
    L144.RECS_all$RECS2009 <- L144.RECS_all$RECS2009 %>%
      left_join_error_no_match(states_subregions %>%
                                 select(subregion13, REPORTABLE_DOMAIN) %>%
                                 distinct(), by = "REPORTABLE_DOMAIN")

    # Convert all missing value strings to 0 in all databases
    L144.RECS_all[["RECS1990"]][L144.RECS_all[["RECS1990"]] == 9999999] <- 0
    L144.RECS_all[["RECS2005"]][L144.RECS_all[["RECS2005"]] == 9999999] <- 0
    L144.RECS_all[["RECS2005"]][is.na( L144.RECS_all[["RECS2005"]] ) ] <- 0

    # Aggregate population to the subregion9 and subregion13 levels for calculation of per-capita values
    L144.Census_pop_hist <- Census_pop_hist %>%
      left_join_error_no_match(states_subregions, by = "state") %>%
      select(state, year, value, subregion9, subregion13)

    L144.pop_sR13 <- L144.Census_pop_hist %>%
      group_by(subregion13, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    L144.pop_sR9 <- L144.Census_pop_hist %>%
      group_by(subregion9, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # b) FLOORSPACE BY STATE AND YEAR
    # Estimating total floorspace by census division (subregion9) and subregion13 if available, in each RECS year where floorspace is available
    # The variable names differ by edition, but in no case does a variable name mean one thing in one edition and another thing in a later edition
    # HOUSEHOLDS is only in the 1984 edition. 1984 also has a different unit on the weight
    flsp_vars <- c("UNHEATED", "HOMEAREA", "SQFTREG", "TOTSQFT")
    L144.flsp_bm2_sR9 <- L144.RECS_all %>%
      lapply(function(df){
        if("HOUSEHOLDS" %in% names(df)){
          flsp_var <- names(df)[which(names(df) %in% flsp_vars)]
          df %>%
            select_("year", "subregion9", "HOUSEHOLDS", flsp_var) %>%
            mutate(variable = flsp_var) %>%
            group_by(year, subregion9) %>%
            summarise(value = sum(value * HOUSEHOLDS * conv_milft2_m2))
        }
        if("NWEIGHT" %in% names(df)){

        }else{
          tibble()
        }
      })

    L144.flsp_bm2_sR13 <- L144.RECS_all %>%
      lapply(function(df){
        if("subregion13" %in% names(df)){

        }else{
          tibble()
        }
      })






    for( i in seq( L144.RECS_all ) ){
      if( "HOUSEHOLDS" %in% names( L144.RECS_all[[i]] ) ){
        data_year <- substr( names( L144.RECS_all )[i], 5, 8 )
        flsp_var <- names( L144.RECS_all[[i]] )[ names( L144.RECS_all[[i]] ) %in% c( "UNHEATED", "HOMEAREA", "SQFTREG", "TOTSQFT" ) ]
        object <- aggregate_recs(
          L144.RECS_all[[i]], variables = flsp_var, year = paste0( "X", data_year ),
          weight_variable = "HOUSEHOLDS", region_variable = "subregion9", unit_conv = conv_milft2_m2  )
        object$pcflsp_m2 <- object[[ flsp_var ]] / L144.pop_sR9[[ paste0( "X", data_year ) ]]
        objectname <- paste0( "L144.flsp_bm2_sR9_", names( L144.RECS_all )[i] )
        assign( objectname, object )
      }
      if( "NWEIGHT" %in% names( L144.RECS_all[[i]] ) ){
        data_year <- substr( names( L144.RECS_all )[i], 5, 8 )
        flsp_var <- names( L144.RECS_all[[i]] )[ names( L144.RECS_all[[i]] ) %in% c( "UNHEATED", "HOMEAREA", "SQFTREG", "TOTSQFT" ) ]
        object <- aggregate_recs(
          L144.RECS_all[[i]], variables = flsp_var, year = paste0( "X", data_year ),
          weight_variable = "NWEIGHT", region_variable = "subregion9", unit_conv = conv_ft2_m2  )
        object$pcflsp_m2 <- object[[ flsp_var ]] / L144.pop_sR9[[ paste0( "X", data_year ) ]]
        objectname <- paste0( "L144.flsp_bm2_sR9_", names( L144.RECS_all )[i] )
        assign( objectname, object )
      }
      if( "subregion13" %in% names( L144.RECS_all[[i]] ) ){
        data_year <- substr( names( L144.RECS_all )[i], 5, 8 )
        flsp_var <- names( L144.RECS_all[[i]] )[ names( L144.RECS_all[[i]] ) %in% c( "UNHEATED", "HOMEAREA", "SQFTREG", "TOTSQFT" ) ]
        object <- aggregate_recs(
          L144.RECS_all[[i]], variables = flsp_var, year = paste0( "X", data_year ),
          weight_variable = "NWEIGHT", region_variable = "subregion13", unit_conv = conv_ft2_m2  )
        object$pcflsp_m2 <- object[[ flsp_var ]] / L144.pop_sR13[[ paste0( "X", data_year ) ]]
        objectname <- paste0( "L144.flsp_bm2_sR13_", names( L144.RECS_all )[i] )
        assign( objectname, object )
      }
    }

    # ===================================================

    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L144.flsp_bm2_state_res") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/RECS_variables",
                     "gcam-usa/EIA_AEO_fuels",
                     "gcam-usa/EIA_AEO_services",
                     "gcam-usa/Census_pop_hist",
                     "gcam-usa/EIA_AEO_Tab4",
                     "gcam-usa/RECS_1979",
                     "gcam-usa/RECS_1984",
                     "gcam-usa/RECS_1990",
                     "gcam-usa/RECS_1993",
                     "gcam-usa/RECS_1997",
                     "gcam-usa/RECS_2001",
                     "gcam-usa/RECS_2005",
                     "gcam-usa/RECS_2009",
                     "L142.in_EJ_state_bld_F",
                     "temp-data-inject/L143.share_state_Pop_CDD_sR13",
                     "temp-data-inject/L143.share_state_Pop_HDD_sR13") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L144.flsp_bm2_state_res

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L144.in_EJ_state_res_F_U_Y") %>%
      add_precursors("gcam-usa/states_subregions") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L144.in_EJ_state_res_F_U_Y

    return_data(L144.flsp_bm2_state_res, L144.in_EJ_state_res_F_U_Y)
  } else {
    stop("Unknown command")
  }
}
