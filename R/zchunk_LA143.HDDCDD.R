#' module_energy_LA143.HDDCDD
#'
#' Reads in HDDCDD country level data and returns at GCAM region via population weighting
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L143.HDDCDD_scen_R_Y}, \code{L143.HDDCDD_scen_RG3_Y}, \code{L143.HDDCDD_scen_ctry_Y}. The corresponding file in the
#' original data system was \code{LA143.HDDCDD.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH April 2017
#' @export
module_energy_LA143.HDDCDD <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/GIS_ctry",
             FILE = "L101.Pop_thous_GCAM3_ctry_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L143.HDDCDD_scen_R_Y",
             "L143.HDDCDD_scen_RG3_Y",
             "L143.HDDCDD_scen_ctry_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GIS_ctry <- get_data(all_data, "energy/GIS_ctry")
    L101.Pop_thous_GCAM3_ctry_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_ctry_Y") %>% rename(population = value)

    # ===================================================

    # 1b. reading HDDCDD files in a loop as we do not know their names or how many there are
    GISfilepath <- "./inst/extdata/energy/GIS/"
    GISfiles <- list.files( GISfilepath )
    #GISfiles.list <- list()

    # Identifying and getting base year (2010) data to remove from HDDCDD_data
    base_year_HDDCDD_data <-  grep( "_2010", GISfiles )

    # Combining all HDDCDD files into one tibble
    data <- tibble(file = GISfiles[-base_year_HDDCDD_data]) %>% # create a data frame
      # holding the file names
      mutate(file_contents = purrr::map(paste0(GISfilepath,file),          # read files into
                                        ~ readr::read_csv(.)) # a new data column
      )


    HDDCDD_data <- tidyr::unnest(data)

    # for( i in 1:length(GISfiles)){
    #   #index <- which( GISfiles == i )
    #   # GISfiles.list[[index]] <- read.csv( paste0(GISfilepath, i))
    #   # GISfiles.list[[index]]$file <- sub(".csv","",i)
    #
    #
    #   GISfiles.list[[i]] <- read.csv( paste0(GISfilepath, GISfiles[i]))
    #   GISfiles.list[[i]]$file <- sub(".csv","",GISfiles[i])
    #
    # }


    #--------------------------------------------------------------------------

    #Currently the HDDCDD data stops at 2099. If this is the case, add 2100
    if( !"2100" %in% names( HDDCDD_data ) ){
      HDDCDD_data <- HDDCDD_data %>% mutate(`2100` = `2099`)
    }

    # Convert data to long format and add in id variables
    HDDCDD_data <- HDDCDD_data %>%
      gather(year, value, num_range(prefix = "",range = 1971:2100)) %>%
      mutate(
        # Assuming that the variable is the 3rd word in the file name (separated by "_")
        variable = stringr::str_split_fixed(file, "_",5)[,3],
        # Assuming that the GCM comes after "DD_" and is 6 letters
        GCM = stringr::str_split_fixed(file, "DD_",2)[,2] %>% substr(1,6),
        # Assuming that the last word (separated by "_") is the scenario, and that there are 4 or 5 "_" in the file name
        SRES = if_else(stringr::str_count(file, "_") == 4,
                       stringr::str_split_fixed(file, "_",5)[,5],
                       stringr::str_split_fixed(file, "_",6)[,6]),
        # Set all negative values to 0
        value = if_else(value < 0, 0, value)
      )

    # Add in country iso
    L143.HDDCDD_scen_ctry_Y <- HDDCDD_data %>%
      # Drop file name
      select(-file) %>%
      # Filter only useful years
      filter( year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      # Remove apostrophe in Cote d'Ivoire and add in country iso by country name
      mutate(country = if_else(country == "Cote d'Ivoire", "Cote dIvoire", country)) %>%
      left_join_error_no_match(GIS_ctry)

    # Serbia and Montenegro are currently combined. Copy to separated countries, assigning the same HDD and CDD to each
    if( "scg" %in% L143.HDDCDD_scen_ctry_Y$iso ){
      # Create Serbia tibble
      L143.HDDCDD_scen_srb_Y <- L143.HDDCDD_scen_ctry_Y %>%
        filter(iso == "scg" ) %>%
        mutate(iso = "srb")

      # Insert Serbia tibble, change 'scg' iso to 'mne' iso
      L143.HDDCDD_scen_ctry_Y <- L143.HDDCDD_scen_ctry_Y %>%
        bind_rows(L143.HDDCDD_scen_srb_Y) %>%
        mutate(iso = if_else(iso == "scg", "mne", iso))
    }

    # Add population data and region data
    L143.wtHDDCDD_scen_ctry_Y <- L143.HDDCDD_scen_ctry_Y %>%
      mutate(year = as.numeric(year)) %>%
      # Join with population data converted to long format
      left_join_error_no_match(L101.Pop_thous_GCAM3_ctry_Y) %>%
      # Join with region ID data
      left_join_error_no_match(iso_GCAM_regID)

    # Old behavior divides total population weighted DD by total population in region, but
    # total population in region includes countries that don't have degree days recorded.
    # New behavior finds weighted mean with DD as values and population as weights.
    # Old behavior also drops Cote D'Ivoire, this is fixed in new behavior
    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      # Join region data with population data, aggregate by region_ID, GCAM3 regions
      L101.Pop_thous_GCAM3_ctry_Y <- L101.Pop_thous_GCAM3_ctry_Y %>%
        left_join_error_no_match(iso_GCAM_regID)

      # Aggregate population data to GCAM 4 region
      L101.Pop_thous_GCAM3_R_Y <- L101.Pop_thous_GCAM3_ctry_Y %>%
        group_by(GCAM_region_ID, year) %>%
        summarise(aggpop = sum(population))

      # Sum weighted degree day by GCAM 4 regions and divide by population
      L143.HDDCDD_scen_R_Y <- L143.wtHDDCDD_scen_ctry_Y %>%
        # Need to remove Cote dIvoire because it was accidently dropped in old data system
        filter(country_name != "Cote dIvoire") %>%
        group_by(GCAM_region_ID, SRES, GCM, variable, year) %>%
        summarise(wtDD = sum(value * population)) %>%
        left_join_error_no_match(L101.Pop_thous_GCAM3_R_Y) %>%
        mutate(DD = wtDD/aggpop) %>%
        select(-wtDD, -aggpop)

      # Aggregate population data to GCAM 3 region
      L101.Pop_thous_GCAM3_RG3_Y <- L101.Pop_thous_GCAM3_ctry_Y %>%
        group_by(region_GCAM3, year) %>%
        summarise(aggpop = sum(population))

      # Sum weighted degree day by GCAM 3 regions and divide by population
      L143.HDDCDD_scen_RG3_Y <- L143.wtHDDCDD_scen_ctry_Y %>%
        filter(country_name != "Cote dIvoire") %>%
        group_by(region_GCAM3, SRES, GCM, variable, year) %>%
        summarise(wtDD = sum(value * population)) %>%
        left_join_error_no_match(L101.Pop_thous_GCAM3_RG3_Y) %>%
        mutate(DD = wtDD/aggpop)%>%
        select(-wtDD, -aggpop)


    } else {

      # Calculate weighted degree day by GCAM 4 regions
      L143.HDDCDD_scen_R_Y <- L143.wtHDDCDD_scen_ctry_Y %>%
        group_by(GCAM_region_ID, SRES, GCM, variable, year) %>%
        summarise(wtDD = weighted.mean(value, population))

      # Calculate weighted degree day by GCAM 3 regions
      L143.HDDCDD_scen_RG3_Y <- L143.wtHDDCDD_scen_ctry_Y %>%
        group_by(region_GCAM3, SRES, GCM, variable, year) %>%
        summarise(wtDD = weighted.mean(value, population))
    }



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
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses vecpaste
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses repeat_and_add_vector
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L143.HDDCDD_scen_R_Y  %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L143.HDDCDD_scen_R_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/GIS_ctry") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L143.HDDCDD_scen_R_Y
    L143.HDDCDD_scen_RG3_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L143.HDDCDD_scen_RG3_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/GIS_ctry", "L101.Pop_thous_GCAM3_ctry_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L143.HDDCDD_scen_RG3_Y
    L143.HDDCDD_scen_ctry_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L143.HDDCDD_scen_ctry_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/GIS_ctry") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L143.HDDCDD_scen_ctry_Y

    return_data(L143.HDDCDD_scen_R_Y, L143.HDDCDD_scen_RG3_Y, L143.HDDCDD_scen_ctry_Y)
  } else {
    stop("Unknown command")
  }
}
