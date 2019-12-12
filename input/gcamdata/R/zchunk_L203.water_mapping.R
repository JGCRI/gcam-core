#' module_water_L203.water_mapping
#'
#' Mapping of water consumption/withdrawal to sectoral demands.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L203.Supplysector}, \code{L203.SubsectorLogit}, \code{L203.SubsectorShrwtFllt}, \code{L203.TechShrwt}, \code{L203.TechCoef},
#' \code{L203.TechPmult}, \code{L203.TechDesalCoef}, \code{L203.TechDesalShrwt}, \code{L203.TechDesalCost}. The corresponding file in the
#' original data system was \code{L203.water_mapping.R} (water level2).
#' @details Generates water mapping input files that map demands by sectors to basins.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select first
#' @importFrom tidyr gather spread
#' @author ST August 2017 / ST Oct 2018
module_water_L203.water_mapping <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_to_country_mapping",
             FILE = "common/GCAM_region_names",
             FILE = "water/A03.sector",
             FILE = "water/basin_ID",
             "L165.ag_IrrEff_R",
             "L103.water_mapping_R_GLU_B_W_Ws_share",
             "L103.water_mapping_R_B_W_Ws_share"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L203.Supplysector",
             "L203.SubsectorLogit",
             "L203.SubsectorShrwtFllt",
             "L203.TechShrwt",
             "L203.TechCoef",
             "L203.TechPmult",
             "L203.TechDesalCoef",
             "L203.TechDesalShrwt",
             "L203.TechDesalCost"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_ID <- get_data(all_data, "water/basin_ID")
    A03.sector <- get_data(all_data, "water/A03.sector")
    L103.water_mapping_R_GLU_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_GLU_B_W_Ws_share")
    L103.water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share")
    L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R")


    GCAM_region_ID <- GLU <- GLU_code <- GLU_name <- water.sector <-
      water_type <- supplysector <- field.eff <- conveyance.eff <-
      coefficient <- region <- NULL  # silence package check notes

    # Create tibble with all possible mapping sectors...

    # (a) irrigation sectors
    L103.water_mapping_R_GLU_B_W_Ws_share %>%
      mutate(GLU = sprintf("GLU%03d", GLU)) %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU = GLU_code, GLU_name, GCAM_basin_ID),
                               by = "GLU") %>% select(-GLU) %>%
      rename(GLU = GLU_name,
             basin_id = GCAM_basin_ID) ->
      L203.mapping_irr

    # (b) non-irrigation sectors
    L103.water_mapping_R_B_W_Ws_share %>%
      mutate(GLU = NA) ->
      L203.mapping_nonirr

    # (c) combine irrigation and non-irrigation sectors and add additional required columns
    bind_rows(
      L203.mapping_irr,
      L203.mapping_nonirr
    ) %>%
    left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(A03.sector, by = c("water_sector" = "water.sector")) %>%
      # ^^ non-restrictive join required (NA values generated for logit type)
      mutate(wt_short = if_else(water_type == "water consumption", "C", "W")) %>%
      mutate(supplysector = if_else(water_sector != water.IRRIGATION,
             paste(supplysector, wt_short, sep = "_"),
             paste(supplysector, GLU, wt_short, sep = "_"))) %>%
      left_join_error_no_match(basin_ID, by = "basin_id") %>%
      mutate(coefficient = water.MAPPING_COEF,
             pMult = water.MAPPING_PMULT,
             subsector = basin_name,
             technology = basin_name,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      left_join(select(L165.ag_IrrEff_R, -field.eff), by = "GCAM_region_ID") %>%
      # ^^ non-restrictive join required (NA values generated for region 30, Taiwan)
      mutate(coefficient = if_else(water_sector == water.IRRIGATION & water_type == "water withdrawals",
                                   1 / conveyance.eff, coefficient)) %>%
      # ^^ conveyance losses for irrigation--applied to withdrawals only
      # Note: Conveyance losses are taken out of agriculture withdrawals and...
      # ... instead applied to water distribution sectors (water_td_irr). This means that to get total...
      # ... ag withdrawals for reporting (i.e., when querying GCAM results)...
      # ... it is necessary to include the conveyance loss.
      mutate(pMult = if_else(water_sector == water.IRRIGATION & water_type == "water withdrawals",
                             water.IRR_PRICE_SUBSIDY_MULT, pMult)) %>%
      select(-conveyance.eff) ->
      L203.mapping_all

    # Sector information
    L203.mapping_all %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME) ->
      L203.Supplysector

    # Subsector logit exponents for mapping sector
    L203.mapping_all %>%
      # over-ride logit exponent
      mutate(logit.exponent = water.LOGIT_EXP) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME) ->
      L203.SubsectorLogit

    # Subsector share weights (which in this case are the mapping shares)
    L203.mapping_all %>%
      mutate(share.weight = share,
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
      mutate(minicam.energy.input = paste(technology, water_type, sep = "_"),
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L203.TechCoef

    # Pass-through technology water price adjust if there one
    L203.mapping_all %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["TechPmult"]]) ->
      L203.TechPmult

    # Desalination technology for non-irrigation sectors
    L203.TechCoef %>%
      filter(!grepl("_irr_", supplysector)) %>%
      mutate(technology = "desalination",
             minicam.energy.input = "desalination") ->
      L203.TechDesalCoef

    L203.TechShrwt %>%
      filter(!grepl("_irr_", supplysector)) %>%
      mutate(technology = "desalination") ->
      L203.TechDesalShrwt

    L203.TechDesalShrwt %>%
      rename(minicam.non.energy.input = share.weight) %>%
      mutate(minicam.non.energy.input = "final cost",
             input.cost = water.DESALINATION_PRICE) ->
      L203.TechDesalCost


    # OUTPUTS
    # possible future amendment: consider creating water mapping files by demand type ...
    # ...(irr, dom, ind, ...) rather than by variable (see issue #663 on dsr gcamdata repo)

    L203.Supplysector %>%
      add_title("Water sector information") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.Supplysector") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector",
                     "water/basin_ID",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "L103.water_mapping_R_B_W_Ws_share") ->
      L203.Supplysector
    L203.SubsectorLogit %>%
      add_title("Water subsector logit exponents for mapping sector") %>%
      add_units("Unitless") %>%
      add_comments("Subsector info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.SubsectorLogit") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector",
                     "water/basin_ID",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "L103.water_mapping_R_B_W_Ws_share") ->
      L203.SubsectorLogit
    L203.SubsectorShrwtFllt %>%
      add_title("Water subsector share weights") %>%
      add_units("Unitless") %>%
      add_comments("Subsector shareweights expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.SubsectorShrwtFllt") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector",
                     "water/basin_ID",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "L103.water_mapping_R_B_W_Ws_share") ->
      L203.SubsectorShrwtFllt
    L203.TechShrwt %>%
      add_title("Water technology shareweights") %>%
      add_units("Unitless") %>%
      add_comments("Technology shareweights expanded to GLU regions and water demand sectors") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.TechShrwt") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector",
                     "water/basin_ID",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "L103.water_mapping_R_B_W_Ws_share") ->
      L203.TechShrwt
    L203.TechCoef %>%
      add_title("Water technology coefficients") %>%
      add_units("Unitless") %>%
      add_comments("Technology info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector",
                     "water/basin_ID",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "L103.water_mapping_R_B_W_Ws_share") ->
      L203.TechCoef
    L203.TechPmult %>%
      add_title("Water technology price multipliers") %>%
      add_units("Unitless") %>%
      add_comments("Technology info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector",
                     "water/basin_ID",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "L103.water_mapping_R_B_W_Ws_share") ->
      L203.TechPmult
    L203.TechDesalCoef %>%
      add_title("Water technology desal coefficients") %>%
      add_units("Unitless") %>%
      add_comments("filtered for non-irrigation") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "L103.water_mapping_R_B_W_Ws_share",
                     "water/basin_ID") ->
      L203.TechDesalCoef
    L203.TechDesalShrwt %>%
      add_title("Water technology desal shareweights") %>%
      add_units("Unitless") %>%
      add_comments("filtered for non-irrigation") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "water/basin_ID",
                     "L103.water_mapping_R_B_W_Ws_share") ->
      L203.TechDesalShrwt
    L203.TechDesalCost %>%
      add_title("Water technology desal costs") %>%
      add_units("Unitless") %>%
      add_comments("filtered for non-irrigation") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L165.ag_IrrEff_R",
                     "common/GCAM_region_names",
                     "water/A03.sector",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "L103.water_mapping_R_B_W_Ws_share",
                     "water/basin_ID") ->
      L203.TechDesalCost

    return_data(L203.Supplysector, L203.SubsectorLogit, L203.SubsectorShrwtFllt, L203.TechShrwt, L203.TechCoef,
                L203.TechPmult, L203.TechDesalCoef, L203.TechDesalShrwt, L203.TechDesalCost)
  } else {
    stop("Unknown command")
  }
}
