#' module_water_L210.water.demand.primary
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.TechCoef}. The corresponding file in the
#' original data system was \code{L210.water.demand.primary.R} (water level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_water_L210.water.demand.primary <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/A03.sector",
             FILE = "energy/A21.globaltech_coef",
             FILE = "energy/A22.globaltech_coef",
             "L110.water_demand_primary_R_S_W_m3_GJ"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L210.TechCoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A03.sector <- get_data(all_data, "water/A03.sector")
    A21.globaltech_coef <- get_data(all_data, "energy/A21.globaltech_coef")
    A22.globaltech_coef <- get_data(all_data, "energy/A22.globaltech_coef")
    L110.water_demand_primary_R_S_W_m3_GJ <- get_data(all_data, "L110.water_demand_primary_R_S_W_m3_GJ")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    #
    A21.globaltech_coef %>% select(supplysector,subsector,technology) -> A21.tmp1
    A22.globaltech_coef %>% filter( grepl('nuclear', supplysector)) %>% select(supplysector,subsector,technology) -> A22.tmp
    A21.A22.tmp <- rbind(A21.tmp1,A22.tmp)
    L210.TechCoef.v1 <- merge(L110.water_demand_primary_R_S_W_m3_GJ,A21.A22.tmp,by="supplysector")

    # L210.TechCoef.v1 %>% filter( grepl('nuclear', supplysector))

    # Avoid double acounting unconventional oil in the regional oil sector as it is already
    # accounted for in unconventional oil production.
    dontuse1 <- data.frame(supp=c("regional oil"))
    dontuse2 <- data.frame(subs=c("unconventional oil"))
    L210.TechCoef.v1  %>% filter(!(supplysector %in% dontuse1$supp & subsector %in% dontuse2$subs)) -> L210.TechCoef.v2
    L210.TechCoef.v2 %>% mutate(water_sector ="mining") -> L210.TechCoef.v3




    ############################
    #old data system
    #############################
    L210.TechCoef$minicam.energy.input <- get_water_inputs_for_mapping( L210.TechCoef, A03.sector )
    L210.TechCoef <- merge( L210.TechCoef, GCAM_region_names )
    L210.TechCoef$market.name <- L210.TechCoef[[reg]]
    # Set the coef for all years
    L210.orig_num_rows <- nrow( L210.TechCoef )
    L210.TechCoef <- L210.TechCoef[ rep( 1:nrow( L210.TechCoef ), times=length( model_years ) ), ]
    L210.TechCoef$year <- model_years[ sort( rep( 1:length( model_years ), times=L210.orig_num_rows ) ) ]
    L210.TechCoef <- L210.TechCoef[, names_TechCoef ]


    # If you find a mistake/thing to update in the old code and
    # fixing it will change the output data, causing the tests to fail,
    # (i) open an issue on GitHub, (ii) consult with colleagues, and
    # then (iii) code a fix:
    #
    # if(OLD_DATA_SYSTEM_BEHAVIOR) {
    #   ... code that replicates old, incorrect behavior
    # } else {
    #   ... new code with a fix
    # }
    #
    #
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
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L210.TechCoef") %>%
      add_precursors("common/GCAM_region_names", "water/A03.sector", "energy/A21.globaltech_coef",
                      "energy/A22.globaltech_coef","L110.water_demand_primary_R_S_W_m3_GJ")                      %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L210.TechCoef

    return_data(L210.TechCoef)
  } else {
    stop("Unknown command")
  }
}
