# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L203.water.mapping
#'
#' Mapping of water consumption/withdrawal to sectoral demands.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L203.Supplysector}, \code{L203.SubsectorLogit}, \code{L203.SubsectorShrwtFllt}, \code{L203.TechShrwt}, \code{L203.TechCoef}. The corresponding file in the
#' original data system was \code{L203.water.mapping.R} (water level2).
#' @details Generates water mapping sector input files to group demands by sectors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else left_join mutate select
#' @importFrom tidyr gather spread
#' @author ST August 2017
module_water_L203.water.mapping <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_to_country_mapping",
             "L125.LC_bm2_R_GLU",
             "L165.ag_IrrEff_R",
             FILE = "common/GCAM_region_names",
             FILE = "water/A03.sector"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L203.Supplysector",
             "L203.SubsectorLogit",
             "L203.SubsectorShrwtFllt",
             "L203.TechShrwt",
             "L203.TechCoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    L125.LC_bm2_R_GLU <- get_data(all_data, "L125.LC_bm2_R_GLU")
    L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A03.sector <- get_data(all_data, "water/A03.sector")

    GCAM_region_ID <- GLU <- GLU_code <- GLU_name <- water.sector <-
      water_type <- supplysector <- field.eff <- conveyance.eff <-
      coefficient <- region <- NULL  # silence package check notes

    # Create tibble with all possible mapping sectors...

    # (a) irrigation sectors
    L125.LC_bm2_R_GLU %>% ungroup %>%
      select(GCAM_region_ID, GLU) %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GLU_name), by = c("GLU" = "GLU_code")) %>%
      # ^^ join GLU names, which will replace GLU codes in this tibble
      select(-GLU) %>% rename(GLU = GLU_name) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      repeat_add_columns(filter(A03.sector, water.sector %in% water.IRRIGATION)) %>%
      repeat_add_columns(tibble(water_type = c("water consumption", "water withdrawals"))) %>%
      mutate(supplysector = set_water_input_name(water.sector, water_type, A03.sector, GLU)) ->
      L203.mapping_irr

    # (b) non-irrigation sectors
    GCAM_region_names %>%
      repeat_add_columns(filter(A03.sector, !(water.sector %in% water.IRRIGATION))) %>%
      mutate(GLU = NA) %>%
      repeat_add_columns(tibble(water_type = c("water consumption", "water withdrawals"))) %>%
      mutate(supplysector = set_water_input_name(water.sector, water_type, A03.sector)) ->
      L203.mapping_nonirr

    # (c) combine irrigation and non-irrigation sectors and add additional required columns
    L203.mapping_irr %>%
      bind_rows(L203.mapping_nonirr) %>%
      mutate(coefficient = 1,
             subsector = supplysector,
             technology = supplysector,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      arrange(GCAM_region_ID) %>%
      left_join(select(L165.ag_IrrEff_R, -field.eff), by = "GCAM_region_ID") %>%
      # ^^ non-restrictive join required (NA values generated for region 30, Taiwan)
      mutate(coefficient = if_else(water.sector == water.IRRIGATION & water_type == "water withdrawals",
                                   1 / conveyance.eff, coefficient)) %>%
      # ^^ conveyance losses for irrigation--applied to withdrawals only
      # Note: Conveyance losses are taken out of agriculture withdrawals and...
      # ... instead applied to water distribution sectors (water_td_irr). This means that to get total...
      # ... ag withdrawals for reporting (i.e., when querying GCAM results)...
      # ... it is necessary to include the conveyance loss.
      select(-conveyance.eff) ->
      L203.mapping_all

    # Sector information
    L203.mapping_all %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME) ->
      L203.Supplysector

    # Subsector logit exponents for mapping sector
    L203.mapping_all %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME) ->
      L203.SubsectorLogit

    # Subsector share weights to 1 (no competition)
    L203.mapping_all %>%
      mutate(share.weight = 1,
             year.fillout = first(MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      L203.SubsectorShrwtFllt

    # Pass-through technology to the water resource (no competition)
    L203.mapping_all %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]]) ->
      L203.TechShrwt

    # Pass-through technology to the water resource
    L203.mapping_all %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(minicam.energy.input = water_type,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L203.TechCoef


    # OUTPUTS
    # possible future amendment: consider creating water mapping files by demand type ...
    # ...(irr, dom, ind, ...) rather than by variable (see issue #663 on dsr gcamdata repo)

    L203.Supplysector %>%
      add_title("Water sector information") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.Supplysector") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L125.LC_bm2_R_GLU",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector") ->
      L203.Supplysector
    L203.SubsectorLogit %>%
      add_title("Water subsector logit exponents for mapping sector") %>%
      add_units("Unitless") %>%
      add_comments("Subsector info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.SubsectorLogit") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L125.LC_bm2_R_GLU",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector") ->
      L203.SubsectorLogit
    L203.SubsectorShrwtFllt %>%
      add_title("Water subsector share weights") %>%
      add_units("Unitless") %>%
      add_comments("Subsector shareweights expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.SubsectorShrwtFllt") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L125.LC_bm2_R_GLU",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector") ->
      L203.SubsectorShrwtFllt
    L203.TechShrwt %>%
      add_title("Water technology shareweights") %>%
      add_units("Unitless") %>%
      add_comments("Technology shareweights expanded to GLU regions and water demand sectors") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.TechShrwt") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L125.LC_bm2_R_GLU",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector") ->
      L203.TechShrwt
    L203.TechCoef %>%
      add_title("Water technology coefficients") %>%
      add_units("Unitless") %>%
      add_comments("Technology info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L125.LC_bm2_R_GLU",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector") ->
      L203.TechCoef

    return_data(L203.Supplysector, L203.SubsectorLogit, L203.SubsectorShrwtFllt, L203.TechShrwt, L203.TechCoef)
  } else {
    stop("Unknown command")
  }
}
