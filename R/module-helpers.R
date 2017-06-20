# module-helpers.R
# Module specific helper functions

#' set_water_input_name
#'
#' Get the appropriate minicam.energy.input name to use in the GCAM supplysector.
#'
#' @param water_sector A character vector containing water sector information
#' @param water_type A character vector containing water type information
#' @param water_mapping A tibble with a mapping between \code{water.sector} and  \code{supplysector}
#' @param GLU An optional character vector containing GLU information (only used for irrigation with mapped water types)
#' @details Get the appropriate minicam.energy.input name to use in the GCAM supplysector
#' by looking up using a mapping to the water.sector and water_type. The minicam.energy.input
#' name to use will have to be some water mapping sector for water_types that are "mapped".
#' @return A vector of names of form supplysector_watertype or supplysector_GLU_watertype.
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @importFrom assertthat assert_that
#' @author BBL April 2017
set_water_input_name <- function(water_sector, water_type, water_mapping, GLU = NA_character_) {

  water.sector <- supplysector <- wt_short <- new_name <- . <- NULL  # silence package check.

  # Sanity checks
  assert_that(is.character(water_sector))
  assert_that(is.character(water_type))
  assert_that(length(water_sector) == length(water_type))
  assert_that(tibble::is_tibble(water_mapping))
  assert_that(all(c("water.sector", "supplysector") %in% names(water_mapping)))
  assert_that(is.character(GLU))

  # If there's an irrigation sector w/ mapped water type, need a GLU
  if(any(water_sector == IRRIGATION & water_type %in% MAPPED_WATER_TYPES)) {
    assert_that(all(!is.na(GLU)))
    assert_that(length(GLU) == length(water_sector))
  }

  tibble(water_sector, water_type, GLU) %>%
    # Add in the base mapped sector name and short water names
    left_join_error_no_match(select(water_mapping, water.sector, supplysector), by = c("water_sector" = "water.sector")) %>%
    mutate(wt_short = MAPPED_WATER_TYPES_SHORT[water_type],
           # non-mapped water_types keep their names unchanged
           new_name = if_else(water_type %in% MAPPED_WATER_TYPES, NA_character_, water_type),
           # non-irrigation mapped types
           new_name = if_else(water_sector != IRRIGATION & water_type %in% MAPPED_WATER_TYPES,
                              paste(supplysector, wt_short, sep = "_"), new_name),
           # irrigation mapped types - needs the GLU column
           new_name = if_else(water_sector == IRRIGATION & water_type %in% MAPPED_WATER_TYPES,
                              paste(supplysector, GLU, wt_short, sep = "_"), new_name)) %>%
    .$new_name
}


#' rename_SO2
#'
#' Add a suffix to the SO2 gas name indicating which of four major world regions to assign the emissions to
#'
#' @param x A tibble, with columns \code{region} and \code{Non.CO2}
#' @param so2_map A tibble, with columns \code{region} and \code{SO2_name}
#' @param is_awb Logical flag - use "_AWB" suffix?
#' @return Data object with \code{Non.CO2} changed to SO2 name for SO2 data.
#' @details Add a suffix to the SO2 gas name indicating which of four major world regions to assign the emissions to,
#' as Hector considers the geographic location of sulfur emissions. Any code writing out CSVs for conversion to XML
#' handling SO2 related data should use this function. Agricultural waste burning emissions already have a suffix
#' assigned (_AWB), so in this case, the SO2 region number is assigned between the "SO2" and "AWB" strings.
#' @importFrom tibble is_tibble
#' @author BBL May 2017
rename_SO2 <- function(x, so2_map, is_awb = FALSE) {

  Non.CO2 <- SO2_name <- region <- . <- NULL # silence package checks.

  assert_that(is_tibble(x))
  assert_that(is_tibble(so2_map))
  assert_that(is.logical(is_awb))

  extension <- if_else(is_awb, "_AWB", "")
  data_so2 <- filter(x, Non.CO2 == paste0("SO2", extension))
  data_notso2 <- filter(x, Non.CO2 != paste0("SO2", extension))

  so2_map %>%
    mutate(SO2_name = paste0(SO2_name, extension)) %>%
    # pull so2_map information into SO2 data
    select(region, SO2_name) %>%
    left_join_error_no_match(data_so2, ., by = "region") %>%
    rename(Non.CO2 = SO2_name) %>%
    bind_rows(data_notso2)
}


#' get_logit_fn_tables
#'
#' Generate a list of tables that sets the appropriate discrete choice function to use.
#'
#' @param data Data to partition by logit.type, tibble
#' @param names Column names to use out of data, character
#' @param base_header Base header that is used for the logit type tables which will get
#'  pasted with what the logit.type for each table, character
#' @param GCAM_region_names GCAM region names and ID numbers, tibble
#' @param default_logit_type Default logit function to use if the user did not specify one, character
#' @param include_equiv_table Include EQUIV_TABLE as well? Logical
#' @param write_all_regions Write each table to all regions? Logical
#' @param ... Other parameters to pass to \code{\link{write_to_all_regions}}
#' @details The data has to be partitioned into multiple tables, one for each logit.type. Each element of
#' the returned list has two variables: 1) the header for the table, and 2) the data for the table.
#' If requested, an additional \code{EQUIV_TABLE} table will be added so that the tables that contain the logit
#' exponents do not themselves have to know what logit.type they are using and thus do not need to be
#' partitioned.
#' @return The tables TODO NEEED BETTER DOCUMENTATION
get_logit_fn_tables <- function(data, names, base_header, GCAM_region_names,
                                include_equiv_table = TRUE,
                                write_all_regions = FALSE,
                                default_logit_type = gcam.LOGIT_TYPES[1], ...) {
  assert_that(is_tibble(data))
  assert_that(is.character(names))
  assert_that(is.character(base_header))
  assert_that(is_tibble(GCAM_region_names))
  assert_that(is.logical(include_equiv_table))
  assert_that(is.logical(write_all_regions))
  assert_that(is.character(default_logit_type))

  # Set the logit type to the default if currently unspecified
  data$logit.type[is.na(data$logit.type)] <- default_logit_type

  # Note it is safer to create tables for all valid logit types rather than just the
  # ones included in unique(data$logit.type) even if it results in an empty table
  # since if we switch all from one type to the other and do not clean out the level
  # 2 CSV and batch file we will be left with both defined for all rows which is bad.

  tables <- list()

  # Create the EQUIV_TABLE table which allows the Model Interface to be ambiguous about what the
  # actual logit type is when setting the logit exponent.
  if(include_equiv_table) {
    tables[[gcam.EQUIV_TABLE]]$header <- gcam.EQUIV_TABLE
    d <- tibble(group.name = "LogitType")
    tag <- 1
    for(lname in c("dummy-logit-tag", gcam.LOGIT_TYPES)) {
      d[paste0("tag", tag)] <- lname
      tag <- tag + 1
    }
    tables[[gcam.EQUIV_TABLE]]$data <- d
  }

  # Loop through each of the logit types and create a table for it
  # using the appropriate header name and writing to all regions if requested.
  for(curr_logit_type in gcam.LOGIT_TYPES) {
    tables[[curr_logit_type]]$header <- paste0(base_header, curr_logit_type)
    currdata <- data[data$logit.type == curr_logit_type,]
    if(write_all_regions) {
      if(nrow(currdata) > 0) {
        currdata <- write_to_all_regions(currdata, names, GCAM_region_names, ...)
      } else {
        currdata <- bind_cols(currdata, tibble(region = character(0)))
      }
    }
    tables[[curr_logit_type]]$data <- currdata[, names]
  }

  tables
}


#' write_to_all_regions
#'
#' @param data Data set to operate on, tibble
#' @param names Column names to return, character vector
#' @param GCAM_region_names GCAM region names and ID numbers, tibble
#' @param has_traded TODO, logical
#' @param apply_selected_only Only apply to traded region \code{gcam.USA_CODE} (1)? Logical
#' @param set_market Overwrite \code{market} column with \code{region} data? Logical
#' @return Filled out data with TODO
write_to_all_regions <- function(data, names, GCAM_region_names, has_traded = FALSE,
                                 apply_selected_only = TRUE, set_market = FALSE) {
  assert_that(is_tibble(data))
  assert_that(is.character(names))
  assert_that(is_tibble(GCAM_region_names))
  assert_that(is.logical(has_traded))
  assert_that(is.logical(apply_selected_only))
  assert_that(is.logical(set_market))

  GCAM_region_ID <- NULL  # silence package check notes

  if("logit.year.fillout" %in% names) {
    data$logit.year.fillout <- "start-year"
  }
  if("price.exp.year.fillout" %in% names) {
    data$price.exp.year.fillout <- "start-year"
  }

  data %>%
    set_years %>%
    repeat_add_columns(select(GCAM_region_names, GCAM_region_ID)) %>%
    left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
    data_new

  if("market.name" %in% names) {
    data_new$market.name <- data_new$region
  }
  if(has_traded) {
    if(set_market) {
      data_new$market.name <- data_new$region
    }
    data_new <- set_traded_names(data_new, GCAM_region_names$region, apply_selected_only)
  }
  data_new[names]
}


#' set_traded_names
#'
#' Convert names of traded secondary goods to be contained within region 1, with region appended to subsector and tech names
#'
#' @param data Data set to operate on, tibble
#' @param GCAM_region_names GCAM region names, ordered character vector
#' @param apply_selected_only Only apply to traded region \code{gcam.USA_CODE} (1)? Logical
#' @return Modified data
set_traded_names <- function(data, GCAM_region_names, apply_selected_only = TRUE) {
  assert_that(is_tibble(data))
  assert_that(is.character(GCAM_region_names))
  assert_that(is.logical(apply_selected_only))

  sel <- TRUE # select all rows
  if(apply_selected_only) {
    sel <- data$traded == gcam.USA_CODE  # by default selected only
  }

  if("subsector" %in% names(data)) {
    data$subsector[sel] <- paste(data$region[sel], data$subsector[sel])
  }
  if("technology" %in% names(data)) {
    data$technology[sel] <- paste(data$region[sel], data$technology[sel])
  }
  if("region" %in% names(data)) {
    data$region[sel] <- GCAM_region_names[gcam.USA_CODE]
  }

  data
}


#' set_years
#'
#' Replace text descriptions of years with numerical values
#'
#' @param data Data with entries to be replaced, tibble
#' @return Modified tibble with \code{start-year}, \code{final-calibration-year}, etc., converted to 'numeric' values
#' @note The new 'numeric' values are actually characters; this helper function doesn't touch column types.
set_years <- function(data) {
  assert_that(is_tibble(data))
  data[data == "start-year"] <- min(BASE_YEARS)
  data[data == "final-calibration-year"] <- max(BASE_YEARS)
  data[data == "final-historical-year"] <- max(HISTORICAL_YEARS)
  data[data == "initial-future-year"] <- min(FUTURE_YEARS)
  data[data == "initial-nonhistorical-year"] <- min(MODEL_YEARS[MODEL_YEARS > max(HISTORICAL_YEARS)])
  data[data == "end-year"] <- max(FUTURE_YEARS)
  data
}
