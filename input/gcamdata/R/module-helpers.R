# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

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
#' @importFrom dplyr if_else mutate select
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
  if(any(water_sector == water.IRRIGATION & water_type %in% water.MAPPED_WATER_TYPES)) {
    assert_that(all(!is.na(GLU)))
    assert_that(length(GLU) == length(water_sector))
  }

  tibble(water_sector, water_type, GLU) %>%
    # Add in the base mapped sector name and short water names
    left_join_error_no_match(select(water_mapping, water.sector, supplysector), by = c("water_sector" = "water.sector")) %>%
    mutate(wt_short = water.MAPPED_WATER_TYPES_SHORT[water_type],
           # non-mapped water_types keep their names unchanged
           new_name = if_else(water_type %in% water.MAPPED_WATER_TYPES, NA_character_, water_type),
           # non-irrigation mapped types
           new_name = if_else(water_sector != water.IRRIGATION & water_type %in% water.MAPPED_WATER_TYPES,
                              paste(supplysector, wt_short, sep = "_"), new_name),
           # irrigation mapped types - needs the GLU column
           new_name = if_else(water_sector == water.IRRIGATION & water_type %in% water.MAPPED_WATER_TYPES,
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
#' @importFrom dplyr bind_rows filter mutate rename select
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
    mutate(Non.CO2 = SO2_name) %>%
    select(-SO2_name) %>%
    bind_rows(data_notso2)
}

#' write_to_all_regions
#'
#' Copy data table to all regions, selecting which columns to keep.
#'
#' @param data Base tibble to start from
#' @param names Character vector indicating the column names of the returned tibble
#' @param GCAM_region_names Tibble with GCAM region names and ID numbers
#' @param has_traded Logical indicating whether any rows in the base table have "traded" goods; if true,
#' \code{\link{set_traded_names}} will be called
#' @param apply_selected_only Logical indicating whether \code{\link{set_traded_names}} is applied to
#' the whole tibble, or only selected rows
#' @param set_market Logical indicating whether to create a \code{market.name} column whose values are equal
#' to \code{region} prior to \code{\link{set_traded_names}} re-setting \code{region} names
#' @note Used for data that GCAM contains within each region, but whose values are not actually differentiated by region.
#' @return Tibble with data written out to all GCAM regions.
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
#' Re-set region names in tables with traded secondary goods so that the traded secondary goods are all contained
#' within one specified region, with the (actual) region name prepended to the subsector and technology names (where applicable)
#'
#' @param data Tibble to operate on
#' @param GCAM_region_names Tibble with GCAM region names and ID numbers
#' @param apply_selected_only Logical indicating whether region/subsector/technology re-assignment is applied to
#' the whole tibble, or only selected rows
#' @return Tibble returned with modified region, subsector, and/or technology information.
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
#' Replace text descriptions of years in exogenous input CSVs with numerical values. This allows model time periods
#' to be modified without requiring similar modifications in many input CSV tables.
#'
#' @param data Tibble with text descriptions of model time periods to be replaced with numerical values.
#' @details Text strings include \code{start-year}, \code{final-calibration-year}, \code{final-historical-year},
#' \code{initial-future-year}, \code{initial-nonhistorical-year}, and \code{end-year}.
#' @return Modified tibble with 'numerical' values instead of text.
#' @note The returned 'numerical' values are actually characters; this helper function doesn't touch column types.
set_years <- function(data) {
  ## silence package check.
  . <- NULL

  assert_that(is_tibble(data))
  year_recode <- c("start-year" =  min(MODEL_BASE_YEARS),
                   "final-calibration-year" = max(MODEL_BASE_YEARS),
                   "final-historical-year" = as.numeric(max(HISTORICAL_YEARS)),
                   "initial-future-year" = min(MODEL_FUTURE_YEARS),
                   "initial-nonhistorical-year" = min(MODEL_YEARS[MODEL_YEARS > max(HISTORICAL_YEARS)]),
                   "end-year" = max(MODEL_FUTURE_YEARS))
  if(nrow(data)) {
    data %>%
      dplyr::mutate_if(list(~ any(. %in% names(year_recode))), list(~ dplyr::recode(., !!!year_recode, .default=suppressWarnings(as.numeric(.))))) ->
      data
  }
  data
}


#' write_to_all_states
#'
#' write out data to all states
#'
#' @param data Base tibble to start from
#' @param names Character vector indicating the column names of the returned tibble
#' @param region_list Character vector containing names of regions for which USA national data is repeated
#' @note Used for USA national data by GCAM region, which is repeated for each US state
#' @note Contains an argument which allows user to specify a different region list.
#' @note For example, this is occasionally used to write all USA data to GCAM-USA grid regions.
#' @return Tibble with data written out to all USA states
#' @importFrom dplyr mutate select
write_to_all_states <- function(data, names, region_list = gcamusa.STATES) {

  assert_that(is_tibble(data))
  assert_that(is.character(names))
  assert_that(is.character(region_list))

  region <- NULL  # silence package check notes

  if("logit.year.fillout" %in% names) {
    data$logit.year.fillout <- "start-year"
  }

  if("price.exp.year.fillout" %in% names) {
    data$price.exp.year.fillout <- "start-year"
  }

  data %>%
    set_years %>%
    mutate(region = NULL) %>% # remove region column if it exists
    repeat_add_columns(tibble(region = region_list)) %>%
    select(names)
}


#' set_subsector_shrwt
#'
#' Calculate subsector shareweights in calibration periods, where subsectors may have multiple technologies
#'
#' @param data Tibble to operate on
#' @param value_col Column with values to be used in setting subsector share-weights
#' @return Tibble returned with a new column of calculated subsector shareweights.
#' @importFrom dplyr group_by mutate select summarise ungroup
set_subsector_shrwt <- function(data, value_col = "calOutputValue") {

  value_col <- rlang::sym(value_col)

  assert_that(is_tibble(data))

  region <- supplysector <- subsector <- year <- calOutputValue_agg <-
    subs.share.weight <- NULL  # silence package check notes

  data_aggregated <- data %>%
    group_by(region, supplysector, subsector, year) %>%
    summarise(calOutputValue_agg = sum(!!value_col)) %>%
    ungroup

  data %>%
    left_join_error_no_match(data_aggregated, by = c("region", "supplysector", "subsector", "year")) %>%
    mutate(subs.share.weight = if_else(calOutputValue_agg > 0, 1, 0)) %>%
    select(-calOutputValue_agg)
}


#' add_node_leaf_names
#'
#' Match in the node and leaf names from a land nesting table
#'
#' @param data Data, tibble
#' @param nesting_table Table of node names (as column names) and leaf data (contents), tibble
#' @param leaf_name Name of leaf name column in \code{nesting_table}, character
#' @param ... Names of columns to add leaf nodes for, character
#' @param LT_name Name of land type column in \code{data}, character
#' @param append_GLU Append GLU column to new leaf name columns? Logical
#' @return Data with new leaf name columns added.
add_node_leaf_names <- function(data, nesting_table, leaf_name, ..., LT_name = "Land_Type", append_GLU = TRUE) {
  assert_that(is_tibble(data))
  assert_that(is_tibble(nesting_table))
  assert_that(is.character(leaf_name))
  assert_that(leaf_name %in% names(nesting_table))
  assert_that(is.character(LT_name))
  assert_that(LT_name %in% names(data))
  assert_that(is.logical(append_GLU))

  data$LandAllocatorRoot <- "root"
  dots <- list(...)
  for(x in dots) {
    assert_that(x %in% names(nesting_table))
    data[[x]] <- nesting_table[[x]][match(data[[LT_name]], nesting_table[[leaf_name]])]
  }

  data[[leaf_name]] <- data[[LT_name]]

  if(append_GLU) {
    data <- append_GLU(data, leaf_name, ...)
  }
  if("Irr_Rfd" %in% names(data)) {
    data[[leaf_name]] <- paste(data[[leaf_name]], data[["Irr_Rfd"]], sep = aglu.IRR_DELIMITER)
  }
  data
}


#' append_GLU
#'
#' Append GLU to all specified variables
#'
#' @param data Data, a tibble
#' @param ... Names of variables to concatenate with \code{GLU} column, character
#' @return A tibble with the \code{...} variable names concatenated with the \code{GLU}.
append_GLU <- function(data, ...) {
  assert_that(is_tibble(data))
  dots <- list(...)
  for(x in dots) {
    assert_that(x %in% names(data))
    data[[x]] <- paste(data[[x]], data[["GLU"]], sep = aglu.LT_GLU_DELIMITER)
  }
  data
}


#' replace_GLU
#'
#' Replace GLU numerical codes with names, and vice versa
#'
#' @param d A tibble with a column named "GLU"
#' @param map A tibble with columns \code{GLU_code} and \code{GLU_name}
#' @param GLU_pattern Regular expression string to identify the GLU codes
#' @return A tibble with codes substituted for pattern, or vice versa, depending on the original
#' contents of the \code{GLU} column.
replace_GLU <- function(d, map, GLU_pattern = "^GLU[0-9]{3}$") {
  assert_that(is_tibble(d))
  assert_that("GLU" %in% names(d))
  assert_that(is_tibble(map))
  assert_that(all(c("GLU_code", "GLU_name") %in% names(map)))
  assert_that(!any(duplicated(map$GLU_code)))
  assert_that(!any(duplicated(map$GLU_name)))
  assert_that(is.character(GLU_pattern))

  # Determine the direction of the change based on character string matching in the first element
  if(all(grepl(GLU_pattern, d$GLU))) {
    d$GLU <- map$GLU_name[match(d$GLU, map$GLU_code)]  # switch from GLU numerical codes to names
  } else {
    d$GLU <- map$GLU_code[match(d$GLU, map$GLU_name)]  # switch from GLU names to numerical codes
  }
  d
}


#' add_carbon_info
#'
#' function to translate from soil, veg, and mature age data (already in a table) to the required read-in model parameters
#'
#' @param data = input data tibble to receive carbon info
#' @param carbon_info_table = table with veg and soil carbon densities, and mature.age
#' @param matchvars =  a character vector for by = in left_join(data, carbon_info_table, by = ...)
#' @return the original table with carbon density info added
#' @importFrom dplyr left_join mutate rename
add_carbon_info <- function( data, carbon_info_table, matchvars = c("region", "GLU", "Cdensity_LT" = "Land_Type")) {

  GCAM_region_names <- veg_c <- soil_c <- hist.veg.carbon.density <- hist.soil.carbon.density <-
    mature.age <- GCAM_region_ID <- NULL  # silence package check notes

  if(!("region" %in% names(carbon_info_table))) {
    carbon_info_table %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      carbon_info_table
  }

  data %>%
    left_join(carbon_info_table, by = matchvars) %>%
    rename(hist.veg.carbon.density = veg_c,
           hist.soil.carbon.density = soil_c) %>%
    mutate(hist.veg.carbon.density = round(hist.veg.carbon.density, aglu.DIGITS_C_DENSITY),
           hist.soil.carbon.density = round(hist.soil.carbon.density, aglu.DIGITS_C_DENSITY),
           mature.age = round(mature.age, aglu.DIGITS_MATUREAGE),
           mature.age.year.fillout = min(MODEL_BASE_YEARS),
           veg.carbon.density = hist.veg.carbon.density,
           soil.carbon.density = hist.soil.carbon.density,
           min.veg.carbon.density = aglu.MIN_VEG_CARBON_DENSITY,
           min.soil.carbon.density = aglu.MIN_SOIL_CARBON_DENSITY)
}

#' reduce_mgd_carbon
#'
#' Reduce the carbon density of a managed land type from its unmanaged land
#' type's carbon density using constant multipliers
#'
#' @param data Unput data tibble to adjust carbon densities for
#' @param LTfor Land_Type name to use for Forest land types
#' @param LTpast Land_Type name to use for Pasture land types
#' @return The original table with carbon density adjusted for the managed land types
#' @importFrom dplyr mutate
reduce_mgd_carbon <- function( data, LTfor = "Forest", LTpast = "Pasture") {

  Land_Type <- hist.veg.carbon.density <- veg.carbon.density <-
    hist.soil.carbon.density <- soil.carbon.density <- NULL # silence package check notes

  data %>%
    mutate(hist.veg.carbon.density = if_else(Land_Type == LTpast, hist.veg.carbon.density * aglu.CVEG_MULT_UNMGDPAST_MGDPAST, hist.veg.carbon.density),
           veg.carbon.density = if_else(Land_Type == LTpast, veg.carbon.density * aglu.CVEG_MULT_UNMGDPAST_MGDPAST, veg.carbon.density),
           hist.soil.carbon.density = if_else(Land_Type == LTpast, hist.soil.carbon.density * aglu.CSOIL_MULT_UNMGDPAST_MGDPAST, hist.soil.carbon.density),
           soil.carbon.density = if_else(Land_Type == LTpast, soil.carbon.density * aglu.CSOIL_MULT_UNMGDPAST_MGDPAST, soil.carbon.density),
           hist.veg.carbon.density = if_else(Land_Type == LTfor, hist.veg.carbon.density * aglu.CVEG_MULT_UNMGDFOR_MGDFOR, hist.veg.carbon.density),
           veg.carbon.density = if_else(Land_Type == LTfor, veg.carbon.density * aglu.CVEG_MULT_UNMGDFOR_MGDFOR, veg.carbon.density),
           hist.soil.carbon.density = if_else(Land_Type == LTfor, hist.soil.carbon.density * aglu.CSOIL_MULT_UNMGDFOR_MGDFOR, hist.soil.carbon.density),
           soil.carbon.density = if_else(Land_Type == LTfor, soil.carbon.density * aglu.CSOIL_MULT_UNMGDFOR_MGDFOR, soil.carbon.density))
}


#' get_ssp_regions
#'
#' Get regions for different income groups in SSP4 2010 (by default)
#'
#' @param pcGDP A tibble with per capita GDP estimates, including columns \code{GCAM_region_ID},
#' \code{scenario}, \code{year}, and \code{value}
#' @param reg_names A tibble with columns \code{GCAM_region_ID} and \code{region}
#' @param income_group A string indicating which region group (low, medium, high)
#' @param ssp_filter A string indicating which SSP to filter to (SSP4 by default)
#' @param year_filter An integer indicating which year to use (2010 by default)
#' @return A character vector of region names belonging to the specified income group.
#' @importFrom dplyr filter mutate select
get_ssp_regions <- function(pcGDP, reg_names, income_group,
                            ssp_filter = "SSP4", year_filter = aglu.PCGDP_YEAR) {
  assert_that(is_tibble(pcGDP))
  assert_that(is_tibble(reg_names))
  assert_that(is.character(income_group))
  assert_that(is.character(ssp_filter))
  assert_that(is.numeric(year_filter))

  value <- scenario <- year <- GCAM_region_ID <- region <- NULL  # silence package check notes

  pcGDP %>%
    left_join_error_no_match(reg_names, by = "GCAM_region_ID") %>%
    mutate(value = value * gdp_deflator(year_filter, 1990)) %>%
    filter(scenario == ssp_filter, year == year_filter) %>%
    select(GCAM_region_ID, value, region) ->
    pcGDP_yf

  if(income_group == "low") {
    regions <- filter(pcGDP_yf, value < aglu.LOW_GROWTH_PCGDP)
  } else if(income_group == "high") {
    regions <- filter(pcGDP_yf, value > aglu.HIGH_GROWTH_PCGDP)
  } else if(income_group == "medium") {
    regions <- filter(pcGDP_yf, value < aglu.HIGH_GROWTH_PCGDP, value > aglu.LOW_GROWTH_PCGDP)
  } else {
    stop("Unknown income_group!")
  }

  regions$region
}

#' fill_exp_decay_extrapolate
#'
#' Takes a wide format tibble with years as columns, coverts to long format, and
#' ensures values are filled in for all \code{out_years} using the following rules:
#'   - Linearly interpolated for missing values that have end points
#'   - Extrapolated using an exponential decay function paramaterized by the columns
#'     \code{improvement.rate} and \code{improvement.max} using the following formulation
#'     \code{v_0*max+(v_0-v_0*max)*(1-rate)^(y-y_0)}
#'   - For rows that specify a char value in the column \code{improvement.shadow.technology}
#'     exponential decay will be calculated on the difference between the values calculated
#'     by left joining with itself on the column \code{improvement.shadow.technology} with
#'     the column \code{technology}.  In other words for shadowing technologies the decay is
#'     only applied to the difference in the values in the last year in which one was
#'     specified. This is to allow for instance a Gas CC plant to have cost reductions at a
#'     moderate pace but a Gas CC+CCS can have rapid cost reductions to the CCS portion of
#'     the cost.
#'
#' @param d The wide format tibble with values under year columns that will be filled
#' to ensure the given years are present using the rules mentioned above.
#' @param out_years A vector of integers specifying which years must have values in the
#' output data.
#' @return The filled out data set in long format.  The years will be in the \code{year}
#' column and will include all values in \code{out_years} and the filled in values will
#' be in the \code{value} column.  All extrapolation parameters will be cleaned out.
#' @importFrom tibble has_name
#' @importFrom dplyr bind_rows filter mutate rename select ungroup
#' @importFrom tidyr complete
#' @importFrom assertthat assert_that
#' @author Pralit Patel
fill_exp_decay_extrapolate <- function(d, out_years) {
  . <- value <- year <- improvement.rate <- improvement.max <-
    improvement.shadow.technology <- technology <- year_base <-
    value_base <- shadow.value <- NULL  # silence package check notes

  # Some error checking first
  assert_that(has_name(d, "improvement.rate"))
  assert_that(has_name(d, "improvement.max"))
  # either improvement.rate/max are both NA or neither are
  assert_that(all(is.na(d$improvement.rate) == is.na(d$improvement.max)))

  # Note we want to allow use of the improvement.shadow.technology feature to be
  # optional so we will check if they have not provided the column and fill NAs
  # if not to avoid error
  if(!has_name(d, "improvement.shadow.technology")) {
    d %>%
      mutate(improvement.shadow.technology = as.character(NA)) ->
      d
  }

  # The first step is to linearly interpolate missing values that are in between
  # values which are specified (approx_fun rule=1)
  d <- gather_years(d)

  d %>%
    # We just need to "complete" the years to include all years in d and out_years.
    # However, finding a way to programmatically select columns for use in nest/nesting
    # that is compatible across versions of tidyr is impossible.
    # Instead we replicate the underlying steps of complete: expand, then left_join
    # so we can directly use the basic dplyr/tidyr functions which have more
    # reliable column select behavior.
    select(-year, -value) %>%
    distinct() %>%
    repeat_add_columns(tibble(year=c(unique(c(d$year, out_years))))) %>%
    left_join(d, by=names(.)) %>%
    # for the purposes of interpolating (and later extrapolating) we would like
    # to just group by everything except year and value
    dplyr::group_by_at(dplyr::vars(-year, -value)) %>%
    # we must also arrange for consistency with the old complete behavior
    arrange(year, .by_group = TRUE) %>%
    # finally do the linearly interpolation between values which are specified
    mutate(value = approx_fun(year, value, rule = 1)) ->
    d

  # Rows in which improvement.max/rate is not specified should not be extrapolated,
  # so split those out for now
  d %>%
    filter(is.na(improvement.max) | is.na(improvement.rate)) ->
    d_no_extrap

  d %>%
    dplyr::setdiff(d_no_extrap) ->
    d_extrap

  # First partition the technologies that are not "shadowing" another technology
  # and apply the exponential decay extrapolation to them
  d_extrap %>%
    filter(is.na(improvement.shadow.technology)) %>%
    # figure out the last specified year from which we will be extrapolating
    # (adding a -Inf in case there are no extrapolation years, to avoid a warning)
    mutate(year_base = max(c(-Inf, year[!is.na(value)]))) %>%
    # fill out the last specified value from which we will be extrapolating
    mutate(value_base = value[year == year_base]) %>%
    # calculate the exponential decay to extrapolate a new value from the last
    # specified value
    mutate(value = if_else(is.na(value),
                           value_base * improvement.max + (value_base - value_base * improvement.max) *
                             (1.0 - improvement.rate ) ^ (year - year_base),
                           value)) %>%
    select(-year_base, -value_base) %>%
    ungroup() ->
    d_nonshadowed

  # Now we can calculate the exponential decay extrapolation for technologies that
  # were shadowing another
  d_nonshadowed %>%
    # Merge the "shadow" technologies onto those that specified one in the "improvement.shadow.technology"
    select(technology, year, value) %>%
    rename(shadow.value = value) %>%
    left_join_error_no_match(d_extrap %>% filter(!is.na(improvement.shadow.technology)), .,
                             by = c("improvement.shadow.technology" = "technology", "year" = "year")) %>%
    # figure out the last specified year from which we will be extrapolating
    # (adding a -Inf in case there are no extrapolation years, to avoid a warning)
    mutate(year_base = max(c(-Inf, year[!is.na(value)])),
           # for shadowing technologies the decay is only applied to the difference
           # in the values in the last year in which one was specified
           # this is to allow for instance a Gas CC plant to have cost reductions at
           # a moderate pace but a Gas CC+CCS can have rapid cost reductions to
           # the CCS portion of the cost
           value_base = value - shadow.value,
           value_base = value_base[year == year_base],
           value = if_else(is.na(value),
                           shadow.value +
                             value_base * improvement.max + (value_base - value_base * improvement.max ) *
                             (1.0 - improvement.rate) ^ (year - year_base),
                           value)) %>%
    # drop the extra columns created for the shadow / exp decay calculation
    select(names(d_nonshadowed)) %>%
    ungroup() ->
    d_shadowed

  # Pull all the data together and drop extrapolation parameters.
  bind_rows(ungroup(d_no_extrap), d_nonshadowed, d_shadowed) %>%
    select(-matches('improvement.')) %>%
    filter(year %in% out_years)
}


#' downscale_FAO_country
#'
#' Helper function to downscale the countries that separated into
#' multiple modern countries (e.g. USSR).
#'
#' @param data Data to downscale, tibble
#' @param country_name Pre-dissolution country name, character
#' @param dissolution_year Year of country dissolution, integer
#' @param years Years to operate on, integer vector
#' @importFrom dplyr filter group_by select summarise_all ungroup
#' @importFrom stats aggregate
#' @return Downscaled data.
downscale_FAO_country <- function(data, country_name, dissolution_year, years = aglu.AGLU_HISTORICAL_YEARS) {

  assert_that(is_tibble(data))
  assert_that(is.character(country_name))
  assert_that(is.integer(dissolution_year))
  assert_that(is.integer(years))
  assert_that(dissolution_year %in% years)

  countries <- item <- element <- NULL                     # silence package check notes

  # Compute the ratio for all years leading up to the dissolution year, and including it
  # I.e. normalizing the time series by the value in the dissolution year
  ctry_years <- years[years < dissolution_year]
  yrs <- as.character(c(ctry_years, dissolution_year))
  data %>%
    select(item, element, yrs) %>%
    group_by(item, element) %>%
    summarise_all(sum) %>%
    ungroup ->
    data_ratio

  data_ratio[yrs] <- data_ratio[yrs] / data_ratio[[as.character(dissolution_year)]]

  # Use these ratios to project the post-dissolution country data backwards in time
  newyrs <- as.character(ctry_years)
  data_new <- filter(data, countries != country_name)
  data_new[newyrs] <- data_new[[as.character(dissolution_year)]] *
    data_ratio[match(paste(data_new[["item"]], data_new[["element"]]),
                     paste(data_ratio[["item"]], data_ratio[["element"]])), newyrs]
  data_new[newyrs][is.na(data_new[newyrs])] <- 0
  data_new
}



#' Moving average
#' @description function to calculate moving average
#'
#' @param x A data frame contain the variable for calculation
#' @param periods An odd number of the periods in MA. The default is 5, i.e., 2 lags and 2 leads
#'
#' @return A data frame
#' @export

Moving_average <- function(x, periods = 5){
  if (periods == 1) {
    return(x)
  }

  if ((periods %% 2) == 0) {
    stop("Periods should be an odd value")
  } else{
    (x +
       Reduce(`+`, lapply(seq(1, (periods -1 )/2), function(a){lag(x, n = a)})) +
       Reduce(`+`,lapply(seq(1, (periods -1 )/2), function(a){lead(x, n = a)}))
    )/periods
  }
}

# Function to dissaggregate dissolved regions in historical years ----
# copyed in gcamdata

#' FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION
#'
#' @param .DF dataframe to disaggregate
#' @param AFFECTED_AREA_CODE  FAO area codes for regions affected; first one should be pre-dissolved region (e.g., USSR) followed by post-dissolved regions.
#' @param YEAR_DISSOLVE_DONE  First year after dissolution
#' @param YEAR_AFTER_DISSOLVE_ACCOUNT Number of years of data after dissolution used for sharing historical data
#'
#' @return Disaggregated data for the historical period for of the dissolved region
#'
FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION <-
  function(.DF,
           AFFECTED_AREA_CODE, #first one should be dissolved area
           YEAR_DISSOLVE_DONE,
           # using 3 year data after dissolution for sharing
           YEAR_AFTER_DISSOLVE_ACCOUNT = 3){

    .DF %>%
      # filter dissolved region related areas by their years
      filter((area_code %in% AFFECTED_AREA_CODE[-1] & year >= YEAR_DISSOLVE_DONE)|
               (area_code %in% AFFECTED_AREA_CODE[1] & year <= YEAR_DISSOLVE_DONE)) ->
      .DF1

    Number_of_Regions_After_Dissolution <- AFFECTED_AREA_CODE %>% length -1

    .DF1 %>% filter(year < YEAR_DISSOLVE_DONE) %>%
      select(-area_code, -area) %>%
      right_join(
        .DF1 %>% filter(year %in% c(YEAR_DISSOLVE_DONE:(YEAR_DISSOLVE_DONE + YEAR_AFTER_DISSOLVE_ACCOUNT))) %>%
          dplyr::group_by_at(dplyr::vars(-year, -value)) %>%
          replace_na(list(value = 0)) %>%
          summarise(value = sum(value)) %>% ungroup() %>%
          dplyr::group_by_at(dplyr::vars(-value, -area, -area_code)) %>%
          mutate(Share = value/sum(value)) %>%
          # using average share if data after dissolved does not exist
          mutate(NODATA = if_else(sum(value) == 0, T, F)) %>%
          mutate(Share = if_else(NODATA == T, 1/Number_of_Regions_After_Dissolution, Share)) %>%
          ungroup() %>%select(-value, -NODATA),
        by = names(.) %>% setdiff(c("year", "value", "area", "area_code"))
      ) %>% mutate(value = value * Share) %>% select(-Share)

  }


#' FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL
#'
#' @param .DF
#' @param SUDAN2012_BREAK If T break Sudan before 2012 based on 2013- 2016 data
#' @param SUDAN2012_MERGE If T merge South Sudan into Sudan
#'
#' @return data with historical periods of dissolved region disaggregated to small pieces.

FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL <- function(.DF,
                                                       SUDAN2012_BREAK = F,
                                                       SUDAN2012_MERGE = T){

  assertthat::assert_that("area_code" %in% names(.DF),
                          msg = "Date frame is required and need a col of area_code")

  # Define area code based on FAO ----
  # first one is dissolved area
  # In 1991 USSR(228) collapsed into 15 countries
  area_code_USSR = c(228, 1, 52, 57, 63, 73, 108, 113, 119, 126, 146, 185, 208, 213, 230, 235)
  # first one is Russia

  # In 1992 Yugoslav SFR dissolved into 5 countries
  # Yugoslav SFR (248)
  # Croatia (98)
  # North Macedonia (154)
  # Slovenia (198)
  # Bosnia and Herzegovina (80)
  # Serbia and Montenegro (186)
  # In 2006 further broke into 2:
  # Montenegro (273)
  # Serbia (272)
  # These regions will be merged for all years in data as most models aggregated them into a single region
  area_code_Yugoslav <- c(248, 98, 154, 198, 80, 186)
  area_code_SerbiaandMontenegro <- c(186, 273, 272)
  # In 1999/2000 Belgium-Luxembourg (15) partitioned in 1999 to 255 (Belgium) and 256 (Luxembourg)
  area_code_Belgium_Luxembourg <- c(15, 255, 256)
  # In 1993 Czechoslovakia (51) to Czechia (167) and Slovakia (199)
  area_code_Czechoslovakia <- c(51, 167, 199)
  # In 2011 Sudan (former) (206) broke into South Sudan (277) and Sudan (276)
  area_code_Sudan <- c(206, 276, 277)
  # Ethiopia PDR (62) dissolved into Ethiopia (238) and Eritrea (178) in 1993
  area_code_Ethiopia <- c(62, 238, 178)

  .DF %>%
    # remove Yugoslav by their years first and area_code_SerbiaandMontenegro later
    filter(!(area_code %in% area_code_Yugoslav[1] )) %>%
    bind_rows(FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF, area_code_Yugoslav, 1992, 3))->
    .DF1


  FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF1, area_code_USSR, 1992, 3) %>%
    bind_rows(FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF1, area_code_SerbiaandMontenegro, 2006, 3)) %>%
    bind_rows(FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF1, area_code_Belgium_Luxembourg, 2000, 3)) %>%
    bind_rows(FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF1, area_code_Czechoslovakia, 1993, 3)) %>%
    bind_rows(FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF1, area_code_Ethiopia, 1993, 3)) ->
    DF_FAO_AREA_DISAGGREGATE_HIST

  .DF1 %>%
    # remove USSR by their years
    filter(!(area_code %in% area_code_USSR[1])) %>%
    # remove Serbia & Montenegro by their years
    filter(!(area_code %in% area_code_SerbiaandMontenegro[1] )) %>%
    # remove Belgium_Luxembourg by their years
    filter(!(area_code %in% area_code_Belgium_Luxembourg[1])) %>%
    # remove area_code_Czechoslovakia by their years
    filter(!(area_code %in% area_code_Czechoslovakia[1] )) %>%
    # remove area_code_Ethiopia by their years
    filter(!(area_code %in% area_code_Ethiopia[1] )) %>%
    bind_rows(DF_FAO_AREA_DISAGGREGATE_HIST) ->
    .DF2

  if (SUDAN2012_BREAK == T) {
    .DF2 %>%
      # remove area_code_Sudan by their years
      filter(!(area_code %in% area_code_Sudan[1] )) %>%
      bind_rows(FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF1, area_code_Sudan, 2012, 3)) ->
      .DF2
  }

  if (SUDAN2012_MERGE == T) {

    .DF2 %>%
      mutate(area_code = replace(area_code, area_code %in% area_code_Sudan, area_code_Sudan[1])) %>%
      dplyr::group_by_at(dplyr::vars(-value, -area)) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
      ungroup() %>%
      # Get area back
      left_join(.DF2 %>% distinct(area, area_code), by = "area_code") -> .DF2
  }

  return(.DF2)

}

#' evaluate_smooth_res_curve
#'
#' Helper function to calculate the smooth renewable resource supply available at a particular price point from
#' the relevant smooth renewable resource curve parameters (curve exponent, mid-price, maximum sub-resource).
#' supply = ((p - base.price) ^ curve.exponent) / (mid.price ^ curve.exponent + ((p - base.price) ^ curve.exponent)) * maxSubResource
#' Note that all of these can be vectors
#' The functional form of GCAM's smooth renewable resource curve is documented at:
#' http://jgcri.github.io/gcam-doc/energy.html#renewable-resources
#' @param curve.exponent smooth renewable resource curve shape parameter, numeric
#' @param mid.price the price at which 50 percent of the maximum available resource is produced, numeric
#' @param base.price the minimum cost of producing (generating electricity from) the resource
#' @param maxSubResource the maximum quantity of energy that could be produced at any price, numeric
#' @param p price, numeric
#' @return quantity of the resource supplied (i.e. quantity of electricity produced from said resource)
evaluate_smooth_res_curve <- function(curve.exponent, mid.price, base.price, maxSubResource, p) {

  supply <- ((p - base.price) ^ curve.exponent) / (mid.price ^ curve.exponent + ((p - base.price) ^ curve.exponent)) * maxSubResource
  # zero out the supply where the price was less than the base.price
  supply[p < base.price] <- 0
  supply

}


#' smooth_res_curve_approx_error
#'
#' Helper function to check how well a set of smooth renewable curve parameters matches the supply-points
#' from which the curve parameters are generated.
#' In gcamdata, this function is used in combination with stats::optimize to minimize the error of the
#' smooth renewable curve fit relative to the supply-points. Note that the first argument
#' (curve.exponent) is the one that is changed by optimize when trying to minimize the error.
#' The functional form of GCAM's smooth renewable resource curve is documented at:
#' http://jgcri.github.io/gcam-doc/energy.html#renewable-resources
#'
#' @param curve.exponent smooth renewable resource curve shape parameter, numeric
#' @param mid.price the price at which 50 percent of the maximum available resource is produced, numeric
#' @param base.price the minimum cost of producing (generating electricity from) the resource
#' @param maxSubResource the maximum quantity of energy that could be produced at any price, numeric
#' @param supply_points a tibble of price and supply points along a resource curve in a region, numeric
#' @return cross product of errors
smooth_res_curve_approx_error <- function(curve.exponent, mid.price, base.price, maxSubResource, supply_points) {

  f_p <- evaluate_smooth_res_curve(curve.exponent, mid.price, base.price, maxSubResource, supply_points$price)
  error <- f_p - supply_points$supply
  crossprod(error, error)

}

#' NEI_to_GCAM
#'
#' Helper function to convert EPA National Emissions Inventory (NEI) emissions to GCAM emissions in GCAM-USA
#' Used for emissions in several sectors
#' This function allows the user to specify what GCAM sectors should be filtered from the NEI data, maps to GCAM
#' fuels and pollutants, converts from TON to Tg, and aggregates emissions by state, sector, fuel, year, and pollutant.
#' @param NEI_data Base tibble to start from (NEI data)
#' @param CEDS_GCAM_fuel CEDS to GCAM fuel mapping file
#' @param NEI_pollutant_mapping NEI to GCAM pollutant mapping file
#' @param names Character vector indicating the column names of the returned tibble
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter left_join rename mutate group_by select summarise_all ungroup
#' @return tibble with corresponding GCAM sectors

NEI_to_GCAM <- function(NEI_data, CEDS_GCAM_fuel, NEI_pollutant_mapping, names) {

  # silence package check notes
  GCAM_sector <- GCAM_fuel <- pollutant <- emissions <- state <- sector <-
    fuel <- Non.CO2 <- year <- value <- NULL

  assert_that(is_tibble(NEI_data))
  assert_that(is_tibble(CEDS_GCAM_fuel))
  assert_that(is_tibble(NEI_pollutant_mapping))
  assert_that(is.character(names))
  assert_that(has_name(NEI_data, "GCAM_sector"))
  assert_that(has_name(CEDS_GCAM_fuel, "CEDS_Fuel"))
  assert_that(has_name(NEI_pollutant_mapping, "NEI_pollutant"))

  data_new <- NEI_data %>%
    #subset relevant sectors
    filter(GCAM_sector %in% names) %>%
    # using left_join because original CEDS fuel in NEI has one called "Process", there's no GCAM fuel corresponding to that, so there will be NA values
    # OK to omit, missing values will be dropped later
    left_join(CEDS_GCAM_fuel, by = "CEDS_Fuel") %>%
    rename(fuel = GCAM_fuel) %>%
    na.omit %>%
    rename(NEI_pollutant = pollutant) %>%
    # Match on NEI pollutants, using left_join because missing values will be produced
    # The original NEI include filterable PM2.5 and PM10, but here we only need primary ones
    # OK to omit those filterables
    left_join(NEI_pollutant_mapping, by = "NEI_pollutant") %>%
    na.omit %>%
    # Convert from short ton to thousand short tons and then to Tg
    mutate(emissions = emissions / 1000 * CONV_TST_TG, unit = "Tg") %>%
    # generate file tibble based on standard GCAM column names, sum emissions for the same state/sector/fuel/species
    rename(sector = GCAM_sector) %>%
    group_by(state, sector, fuel, year, Non.CO2) %>%
    summarise(value = sum(emissions)) %>%
    ungroup %>%
    select(state, sector, fuel, Non.CO2, year, value)

}

#' compute_BC_OC
#'
#' Helper function to compute BC and OC EFs from PM2.5 and a mapping file with BC OC fraction content by sector/subsector/technology
#' Used for emissions in several sectors.
#' @param df tibble which contains PM2.5 data to be used to get BC and OC data
#' @param BC_OC_assumptions tibble which contains BC and OC fractions
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter left_join rename mutate group_by select summarise_all ungroup
#' @return tibble with BC and OC rows added

compute_BC_OC <- function(df, BC_OC_assumptions) {
  #There is no data for BC/OC in the base year, so use fractions of PM2.5 to calculate BC/OC emission factors.
  #Compute BC/OC emission factors in the base year based on PM2.5 emission factors
  #BC + OC is a sub-category of PM2.5, and in some cases most of PM2.5

  #Note: BC/OC fractions are given for various level of sector, subsector, and stub.technology.
  #All levels are not necessarily present for each

  # silence package check notes
  BC_fraction <- OC_fraction <- subsector.y <- subsector.x <- technology <-
    BC_fraction_SST <- BC_fraction_ST <- BC_fraction_SS <- emiss.coef <-
    OC_fraction_SST <- OC_fraction_ST <- OC_fraction_SS <- Non.CO2 <- NULL

  assert_that(is_tibble(df))
  assert_that(has_name(df, "Non.CO2"))

  BC_df <- df %>%
    filter(Non.CO2 == "PM2.5") %>%
    # Cannot do left_join_error_no_match because there are some NA values that will be retained
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "subsector", "stub.technology" = "technology", "year")) %>%
    mutate(BC_fraction_SST = BC_fraction) %>%
    select(-BC_fraction, -OC_fraction) %>%
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "stub.technology" = "technology", "year")) %>%
    mutate(BC_fraction_ST = BC_fraction) %>%
    select(-BC_fraction, -OC_fraction, -subsector.y) %>%
    rename(subsector = subsector.x) %>%
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "subsector", "year")) %>%
    mutate(BC_fraction_SS = BC_fraction) %>%
    select(-BC_fraction, -OC_fraction, -technology) %>%
    #Hierarchy of values to be chosen:
    #1. If all sector, subsector, and technology information is available
    #2. If sector and technology information is available
    #3. If sector and subsector information is available
    mutate(BC_fraction = if_else(is.na(BC_fraction_SST) & is.na(BC_fraction_ST), BC_fraction_SS,
                                 if_else(is.na(BC_fraction_SST), BC_fraction_ST, BC_fraction_SST)),
           #Compute emissions coefficients for BC
           emiss.coef = emiss.coef * BC_fraction,Non.CO2 = "BC") %>%
    select(-BC_fraction, -BC_fraction_SST, -BC_fraction_ST, -BC_fraction_SS) %>%
    distinct()

  OC_df <- df %>%
    filter(Non.CO2 == "PM2.5") %>%
    # Cannot do left_join_error_no_match because there are some NA values that will be retained
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "subsector", "stub.technology" = "technology", "year")) %>%
    mutate(OC_fraction_SST = OC_fraction) %>%
    select(-BC_fraction, -OC_fraction) %>%
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "stub.technology" = "technology", "year")) %>%
    mutate(OC_fraction_ST = OC_fraction) %>%
    select(-BC_fraction, -OC_fraction, -subsector.y) %>%
    rename(subsector = subsector.x) %>%
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "subsector", "year")) %>%
    mutate(OC_fraction_SS = OC_fraction) %>%
    select(-BC_fraction, -OC_fraction, -technology) %>%
    #Hierarchy of values to be chosen:
    #1. If all sector, subsector, and technology information is available
    #2. If sector and technology information is available
    #3. If sector and subsector information is available
    mutate(OC_fraction = if_else(is.na(OC_fraction_SST) & is.na(OC_fraction_ST), OC_fraction_SS,
                                 if_else(is.na(OC_fraction_SST), OC_fraction_ST, OC_fraction_SST)),
           #Compute emissions coefficients for BC
           emiss.coef = emiss.coef * OC_fraction, Non.CO2 = "OC") %>%
    select(-OC_fraction, -OC_fraction_SST, -OC_fraction_ST, -OC_fraction_SS) %>%
    distinct()

  # Bind base year BC/OC fractions to other EFs
  df <- bind_rows(df, BC_df, OC_df)
  return (df)

}

#' compute_BC_OC_transport
#'
#' Helper function to compute BC and OC EFs from PM2.5 and a mapping file with BC OC fraction content by sector/tranSubsector/technology
#' Used for emissions in the transportation sectors (onroad and nonroad)
#' @param df tibble which contains PM2.5 data to be used to get BC and OC data
#' @param BC_OC_assumptions tibble which contains BC and OC fractions
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter left_join rename mutate group_by select summarise_all ungroup
#' @return tibble with BC and OC rows added

compute_BC_OC_transport <- function(df, BC_OC_assumptions) {
  #This is specific for the transport sector, necessary due to different variable naming conventions.
  #There is no data for BC/OC in the base year, so use fractions of PM2.5 to calculate BC/OC emission factors.
  #Compute BC/OC emission factors in the base year based on PM2.5 emission factors
  #BC + OC is a sub-category of PM2.5, and in some cases most of PM2.5

  #Note: BC/OC fractions are given for various level of sector, tranSubsector, and stub.technology.
  #All levels are not necessarily present for each

  # silence package check notes
  BC_fraction <- OC_fraction <- tranSubsector.y <- tranSubsector.x <- technology <-
    BC_fraction_SST <- BC_fraction_ST <- BC_fraction_SS <- emiss.coef <-
    OC_fraction_SST <- OC_fraction_ST <- OC_fraction_SS <- Non.CO2 <- NULL

  assert_that(is_tibble(df))
  assert_that(has_name(df, "Non.CO2"))

  BC_df <- df %>%
    filter(Non.CO2 == "PM2.5") %>%
    # Cannot do left_join_error_no_match because there are some NA values that will be retained
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "tranSubsector", "stub.technology" = "technology", "year")) %>%
    mutate(BC_fraction_SST = BC_fraction) %>%
    select(-BC_fraction, -OC_fraction) %>%
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "stub.technology" = "technology", "year")) %>%
    mutate(BC_fraction_ST = BC_fraction) %>%
    select(-BC_fraction, -OC_fraction, -tranSubsector.y) %>%
    rename(tranSubsector = tranSubsector.x) %>%
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "tranSubsector", "year")) %>%
    mutate(BC_fraction_SS = BC_fraction) %>%
    select(-BC_fraction, -OC_fraction, -technology) %>%
    #Hierarchy of values to be chosen:
    #1. If all sector, tranSubsector, and technology information is available
    #2. If sector and technology information is available
    #3. If sector and tranSubsector information is available
    mutate(BC_fraction = if_else(is.na(BC_fraction_SST) & is.na(BC_fraction_ST), BC_fraction_SS,
                                 if_else(is.na(BC_fraction_SST), BC_fraction_ST, BC_fraction_SST)),
           #Compute emissions coefficients for BC
           emiss.coef = emiss.coef * BC_fraction,Non.CO2 = "BC") %>%
    select(-BC_fraction, -BC_fraction_SST, -BC_fraction_ST, -BC_fraction_SS) %>%
    distinct()

  OC_df <- df %>%
    filter(Non.CO2 == "PM2.5") %>%
    # Cannot do left_join_error_no_match because there are some NA values that will be retained
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "tranSubsector", "stub.technology" = "technology", "year")) %>%
    mutate(OC_fraction_SST = OC_fraction) %>%
    select(-BC_fraction, -OC_fraction) %>%
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "stub.technology" = "technology", "year")) %>%
    mutate(OC_fraction_ST = OC_fraction) %>%
    select(-BC_fraction, -OC_fraction, -tranSubsector.y) %>%
    rename(tranSubsector = tranSubsector.x) %>%
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "tranSubsector", "year")) %>%
    mutate(OC_fraction_SS = OC_fraction) %>%
    select(-BC_fraction, -OC_fraction, -technology) %>%
    #Hierarchy of values to be chosen:
    #1. If all sector, tranSubsector, and technology information is available
    #2. If sector and technology information is available
    #3. If sector and tranSubsector information is available
    mutate(OC_fraction = if_else(is.na(OC_fraction_SST) & is.na(OC_fraction_ST), OC_fraction_SS,
                                 if_else(is.na(OC_fraction_SST), OC_fraction_ST, OC_fraction_SST)),
           #Compute emissions coefficients for BC
           emiss.coef = emiss.coef * OC_fraction, Non.CO2 = "OC") %>%
    select(-OC_fraction, -OC_fraction_SST, -OC_fraction_ST, -OC_fraction_SS) %>%
    distinct()

  # Bind base year BC/OC fractions to other EFs
  df <- bind_rows(df, BC_df, OC_df)
  return (df)

}

#' compute_BC_OC_elc
#'
#' Helper function to compute BC and OC EFs from PM2.5 and a mapping file with BC OC fraction content by sector/subsector/technology
#' Used for emissions in the electric sector
#' @param df tibble which contains PM2.5 data to be used to get BC and OC data
#' @param BC_OC_assumptions tibble which contains BC and OC fractions
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter left_join rename mutate group_by select summarise_all ungroup
#' @return tibble with BC and OC rows added
#'
compute_BC_OC_elc <- function(df, BC_OC_assumptions) {
  #This is specific for the electric gen sector, necessary due to different variable naming conventions.
  #There is no data for BC/OC in the base year, so use fractions of PM2.5 to calculate BC/OC emission factors.
  #Compute BC/OC emission factors in the base year based on PM2.5 emission factors
  #BC + OC is a sub-category of PM2.5, and in some cases most of PM2.5

  #Note: BC/OC fractions are given for various level of sector, subsector, and stub.technology.
  #All levels are not necessarily present for each

  # silence package check notes
  BC_fraction <- OC_fraction <- subsector.y <- subsector.x <- technology <-
    BC_fraction_SST <- BC_fraction_ST <- BC_fraction_SS <- emiss.coef <-
    OC_fraction_SST <- OC_fraction_ST <- OC_fraction_SS <- Non.CO2 <- NULL

  assert_that(is_tibble(df))
  assert_that(has_name(df, "Non.CO2"))

  BC_df <- df %>%
    filter(Non.CO2 == "PM2.5") %>%
    # Cannot do left_join_error_no_match because there are some NA values that will be retained
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "subsector0" = "subsector", "stub.technology" = "technology", "year")) %>%
    mutate(BC_fraction_SST = BC_fraction) %>%
    select(-BC_fraction, -OC_fraction) %>%
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "stub.technology" = "technology", "year")) %>%
    mutate(BC_fraction_ST = BC_fraction) %>%
    select(-BC_fraction, -OC_fraction, -subsector.y) %>%
    rename(subsector = subsector.x) %>%
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "subsector0" = "subsector", "year")) %>%
    mutate(BC_fraction_SS = BC_fraction) %>%
    select(-BC_fraction, -OC_fraction, -technology) %>%
    #Hierarchy of values to be chosen:
    #1. If all sector, subsector, and technology information is available
    #2. If sector and technology information is available
    #3. If sector and subsector information is available
    mutate(BC_fraction = if_else(is.na(BC_fraction_SST) & is.na(BC_fraction_ST), BC_fraction_SS,
                                 if_else(is.na(BC_fraction_SST), BC_fraction_ST, BC_fraction_SST)),
           #Compute emissions coefficients for BC
           emiss.coef = emiss.coef * BC_fraction,Non.CO2 = "BC") %>%
    select(-BC_fraction, -BC_fraction_SST, -BC_fraction_ST, -BC_fraction_SS) %>%
    distinct()

  OC_df <- df %>%
    filter(Non.CO2 == "PM2.5") %>%
    # Cannot do left_join_error_no_match because there are some NA values that will be retained
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "subsector0" = "subsector", "stub.technology" = "technology", "year")) %>%
    mutate(OC_fraction_SST = OC_fraction) %>%
    select(-BC_fraction, -OC_fraction) %>%
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "stub.technology" = "technology", "year")) %>%
    mutate(OC_fraction_ST = OC_fraction) %>%
    select(-BC_fraction, -OC_fraction, -subsector.y) %>%
    rename(subsector = subsector.x) %>%
    left_join(BC_OC_assumptions,
              by = c("supplysector" = "sector", "subsector0" = "subsector", "year")) %>%
    mutate(OC_fraction_SS = OC_fraction) %>%
    select(-BC_fraction, -OC_fraction, -technology) %>%
    #Hierarchy of values to be chosen:
    #1. If all sector, subsector, and technology information is available
    #2. If sector and technology information is available
    #3. If sector and subsector information is available
    mutate(OC_fraction = if_else(is.na(OC_fraction_SST) & is.na(OC_fraction_ST), OC_fraction_SS,
                                 if_else(is.na(OC_fraction_SST), OC_fraction_ST, OC_fraction_SST)),
           #Compute emissions coefficients for BC
           emiss.coef = emiss.coef * OC_fraction, Non.CO2 = "OC") %>%
    select(-OC_fraction, -OC_fraction_SST, -OC_fraction_ST, -OC_fraction_SS) %>%
    distinct()

  # Bind base year BC/OC fractions to other EFs
  df <- bind_rows(df, BC_df, OC_df)
  return (df)

}
