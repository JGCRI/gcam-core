#' module_water_L233.water.demand.livestock
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L233.TechCoef}. The corresponding file in the
#' original data system was \code{L233.water.demand.livestock.R} (water level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_water_L233.water.demand.livestock <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/A03.sector",
             FILE = "aglu/A_an_technology",
             "L133.water_demand_livestock_R_C_W_km3_Mt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L233.TechCoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A03.sector <- get_data(all_data, "water/A03.sector")
    A_an_technology <- get_data(all_data, "aglu/A_an_technology")
    L133.water_demand_livestock_R_C_W_km3_Mt <- get_data(all_data, "L133.water_demand_livestock_R_C_W_km3_Mt")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    #Just read in water coefficients for all years


    # L233.TechCoef <- merge( L133.water_demand_livestock_R_C_W_km3_Mt, A_an_technology[, c( supp, subs, tech ) ], by.x="GCAM_commodity", by.y=supp )
    # names(L233.TechCoef)[names(L233.TechCoef) == "GCAM_commodity"] <- supp
    # L233.TechCoef[[water_sector]] <- "Livestock"
    # L233.TechCoef$minicam.energy.input <- get_water_inputs_for_mapping( L233.TechCoef, A03.sector )
    # L233.TechCoef <- merge( L233.TechCoef, GCAM_region_names )
    # L233.TechCoef$market.name <- L233.TechCoef[[reg]]
    # # Set the coef for all years
    # L233.orig_num_rows <- nrow( L233.TechCoef )
    # L233.TechCoef <- L233.TechCoef[ rep( 1:nrow( L233.TechCoef ), times=length( model_years ) ), ]
    # L233.TechCoef$year <- model_years[ sort( rep( 1:length( model_years ), times=L233.orig_num_rows ) ) ]
    # L233.TechCoef <- L233.TechCoef[, names_TechCoef ]

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
      add_legacy_name("L233.TechCoef") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L233.TechCoef

    return_data(L233.TechCoef)
  } else {
    stop("Unknown command")
  }
}
