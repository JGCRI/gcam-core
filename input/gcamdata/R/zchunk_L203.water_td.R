#' module_water_L203.water_td
#'
#' Water distribution sectors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L203.Supplysector_watertd},
#'   \code{L203.SubsectorLogit_watertd}, \code{L203.SubsectorShrwtFllt_watertd}, \code{L203.SubsectorInterp_watertd},
#'   \code{L203.TechShrwt_watertd}, \code{L203.TechCoef_watertd}, \code{L203.Production_watertd}. The corresponding file
#'   in the original data system was \code{L203.water.mapping.R} (water level2).
#' @details Generates water mapping sector input files to group demands by sectors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join mutate rename summarise select ungroup
#' @importFrom tidyr replace_na
#' @author ST August 2017 (revised GPK January 2019)
module_water_L203.water_td <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "water/A03.sector",
             FILE = "water/basin_to_country_mapping",
             FILE = "water/water_td_sectors",
             "L125.LC_bm2_R_GLU",
             "L132.water_km3_R_ind_Yh",
             "L145.municipal_water_R_W_Yh_km3",
             "L165.ag_IrrEff_R",
             "L173.in_desal_km3_ctry_ind_Yh",
             "L173.in_desal_km3_ctry_muni_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L203.Supplysector_watertd",
             "L203.SubsectorLogit_watertd",
             "L203.SubsectorShrwtFllt_watertd",
             "L203.SubsectorInterp_watertd",
             "L203.TechShrwt_watertd",
             "L203.TechCoef_watertd",
             "L203.Production_watertd"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A03.sector <- get_data(all_data, "water/A03.sector")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    water_td_sectors <- get_data(all_data, "water/water_td_sectors")
    L125.LC_bm2_R_GLU <- get_data(all_data, "L125.LC_bm2_R_GLU")
    L132.water_km3_R_ind_Yh <- get_data(all_data, "L132.water_km3_R_ind_Yh")
    L145.municipal_water_R_W_Yh_km3 <- get_data(all_data, "L145.municipal_water_R_W_Yh_km3")
    L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R")
    L173.in_desal_km3_ctry_ind_Yh <- get_data(all_data, "L173.in_desal_km3_ctry_ind_Yh")
    L173.in_desal_km3_ctry_muni_Yh <- get_data(all_data, "L173.in_desal_km3_ctry_muni_Yh")

    GCAM_region_ID <- GLU <- GLU_code <- GLU_name <- water.sector <-
      water_type <- supplysector <- field.eff <- conveyance.eff <-
      coefficient <- region <- has.desal.input <- supplysector_root <-
      subsector <- technology <- desal_ind_km3 <- desal_muni_km3 <-
      year <- iso <- desal_km3 <- water_km3 <- withdrawals <- total_water_km3 <-
      calOutputValue <- desal_water_km3 <- NULL  # silence package check notes

    # Create tibble with all possible mapping sectors...

    # Irrigated and non-irrigated are done separately, as the former are done by basin
    L203.water_td_info_irr <- select(L125.LC_bm2_R_GLU, GCAM_region_ID, GLU) %>%
      mutate(water.sector = water.IRRIGATION) %>%
      left_join_error_no_match(water_td_sectors, by = "water.sector") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GLU_name), by = c(GLU = "GLU_code")) %>%
      repeat_add_columns(tibble(water_type = c("water withdrawals", "water consumption"))) %>%
      mutate(supplysector_root = supplysector,
             supplysector = set_water_input_name(water.sector, water_type, water_td_sectors, GLU = GLU_name),
             subsector = water_type,
             technology = water_type)

    L203.water_td_info <- filter(water_td_sectors, water.sector != water.IRRIGATION) %>%
      repeat_add_columns(GCAM_region_names) %>%
      repeat_add_columns(tibble(water_type = c("water withdrawals", "water consumption"))) %>%
      mutate(supplysector_root = supplysector,
             supplysector = set_water_input_name(water.sector, water_type, water_td_sectors),
             subsector = water_type,
             technology = water_type) %>%
      bind_rows(L203.water_td_info_irr)

    L203.water_td_info_desal <- filter(L203.water_td_info, has.desal.input == 1 & water_type == "water withdrawals") %>%
      mutate(subsector = water.DESAL,
             technology = water.DESAL,
             water_type = water.DESAL)

    L203.water_td_info <- bind_rows(L203.water_td_info, L203.water_td_info_desal) %>%
      select(region, water.sector, supplysector_root, supplysector, subsector, technology, water_type)

    # Sector information
    L203.Supplysector_watertd <- left_join_error_no_match(L203.water_td_info, A03.sector,
                                                          by = c(supplysector_root = "supplysector"),
                                                          ignore_columns = "logit.type") %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)

    L203.SubsectorLogit_watertd <- mutate(L203.water_td_info,
                                          logit.exponent = gcam.DEFAULT_TECH_LOGIT,
                                          logit.type = NA,
                                          logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)


    # Default subsector share weights (fill out 1 from start-year)
    L203.SubsectorShrwtFllt_watertd <- L203.SubsectorLogit_watertd %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) %>%
    select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])

    # Subsector share-weight interpolation (for competition between desalinated water and surface/ground freshwater)
    L203.SubsectorInterp_watertd <- L203.SubsectorLogit_watertd %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["SubsectorInterp"]])

    # Technology information
    # L203.TechCoef_watertd: Water input-output coefficients of T&D sectors
    # At present, conveyance losses are only indicated for irrigation technologies. These IO coefficients are calculated
    # as the water flow volume divided by the conveyance efficiency.
    L203.ag_IrrEff_R <- left_join(L165.ag_IrrEff_R, GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, conveyance.eff)

    L203.TechCoef_watertd <- L203.water_td_info %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(minicam.energy.input = water_type) %>%
      left_join(L203.ag_IrrEff_R, by = "region") %>%
      replace_na(list(coefficient = 1)) %>%
      mutate(coefficient = if_else(water.sector == water.IRRIGATION,
                                   round(1 / conveyance.eff, energy.DIGITS_COEFFICIENT),
                                   1)) %>%
      mutate(market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]])

    # Shareweights of water T&D technologies
    L203.TechShrwt_watertd <- mutate(L203.TechCoef_watertd, share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]])

    # Calibrated flows of water through the T&D sectors
    # First, compile and compute the appropriate flow volumes. Desal techs' calibrated outputs are already
    # elsewhere computed; freshwater "water withdrawals" are equal to the total withdrawals minus desal.
    L173.in_desal_km3_ctry_ind_Yh <- mutate(L173.in_desal_km3_ctry_ind_Yh, water.sector = water.MANUFACTURING) %>%
      rename(desal_km3 = desal_ind_km3)
    L173.in_desal_km3_ctry_muni_Yh <- mutate(L173.in_desal_km3_ctry_muni_Yh, water.sector = water.MUNICIPAL) %>%
      rename(desal_km3 = desal_muni_km3)
    L203.in_desal_km3_ctry_S_Yh <- bind_rows(L173.in_desal_km3_ctry_ind_Yh, L173.in_desal_km3_ctry_muni_Yh)
    L203.watertd_desal <- filter(L203.in_desal_km3_ctry_S_Yh, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(water_type = water.DESAL) %>%
      group_by(region, year, water_type, water.sector) %>%
      summarise(calOutputValue = round(sum(desal_km3), energy.DIGITS_CALOUTPUT)) %>%
      ungroup() %>%
      complete(region = GCAM_region_names$region, year, water_type, water.sector) %>%
      replace_na(list(calOutputValue = 0))

    # Prepare the water withdrawals by industry and municipal sectors for merging, merge, and deduct
    # the input of desalinated water above to calibrate the input from "water withdrawals"
    L203.water_km3_R_ind_Yh <- mutate(L132.water_km3_R_ind_Yh, water.sector = water.MANUFACTURING) %>%
      filter(water_type == "water withdrawals") %>%
      rename(total_water_km3 = water_km3)

    L203.watertd_fresh <- mutate(L145.municipal_water_R_W_Yh_km3, water.sector = water.MUNICIPAL,
                                      water_type = "water withdrawals") %>%
      rename(total_water_km3 = withdrawals) %>%
      bind_rows(L203.water_km3_R_ind_Yh) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(total_water_km3 = round(total_water_km3, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(select(rename(L203.watertd_desal, desal_water_km3 = calOutputValue), -water_type),
                by = c("region", "year", "water.sector")) %>%
      mutate(calOutputValue = if_else(is.na(desal_water_km3), total_water_km3, total_water_km3 - desal_water_km3)) %>%
      select(region, water_type, water.sector, year, calOutputValue)

    # Calibrated output: just merge the desal and non-desal
    L203.Production_watertd <- bind_rows(L203.watertd_fresh, L203.watertd_desal) %>%
      left_join_error_no_match(L203.water_td_info,
                               by = c("region", "water.sector", "water_type")) %>%
      mutate(subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # ===================================================

    # OUTPUTS
    # possible future amendment: consider creating water mapping files by demand type ...
    # ...(irr, muni, ind, ...) rather than by variable (see issue #663 on dsr gcamdata repo)

    L203.Supplysector_watertd %>%
      add_title("Water T&D sector information") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector info for each water demand type; irrigation demands by land use region (GLU)") %>%
      add_legacy_name("L203.Supplysector") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A03.sector",
                     "water/basin_to_country_mapping",
                     "water/water_td_sectors",
                     "L125.LC_bm2_R_GLU") ->
      L203.Supplysector_watertd

    L203.SubsectorLogit_watertd %>%
      add_title("Subsector logit exponents for water T&D sectors") %>%
      add_units("Unitless") %>%
      add_comments("Pass-through subsectors; there is no actual technology-level competition") %>%
      add_legacy_name("L203.SubsectorLogit") %>%
      same_precursors_as(L203.Supplysector_watertd) ->
      L203.SubsectorLogit_watertd

    L203.SubsectorShrwtFllt_watertd %>%
      add_title("Water subsector default share weights") %>%
      add_units("Unitless") %>%
      add_comments("Share-weights for competing subsectors are over-written by calibration and interpolation") %>%
      add_legacy_name("L203.SubsectorShrwtFllt") %>%
      same_precursors_as(L203.Supplysector_watertd) ->
      L203.SubsectorShrwtFllt_watertd

    L203.SubsectorInterp_watertd %>%
      add_title("Water subsector share-weight interpolation") %>%
      add_units("Unitless") %>%
      add_comments("These share-weights relate to the competition between desalinated water and surface/ground freshwater in the municipal and industrial sectors") %>%
      same_precursors_as(L203.Supplysector_watertd) ->
      L203.SubsectorInterp_watertd

    L203.TechCoef_watertd %>%
      add_title("Water technology coefficients") %>%
      add_units("Unitless (m3 / m3)") %>%
      add_comments("Coefficients on irrigated technologies reflect conveyance losses") %>%
      add_legacy_name("L203.TechCoef") %>%
      same_precursors_as(L203.Supplysector_watertd) %>%
      add_precursors("L165.ag_IrrEff_R") ->
      L203.TechCoef_watertd

    L203.TechShrwt_watertd %>%
      add_title("Water technology shareweights") %>%
      add_units("Unitless") %>%
      add_comments("Technology shareweights expanded to GLU regions and water demand sectors") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.TechShrwt") %>%
      same_precursors_as(L203.TechCoef_watertd) ->
      L203.TechShrwt_watertd

    L203.Production_watertd %>%
      add_title("Water technology calibration") %>%
      add_units("km3/yr") %>%
      add_comments("Calibrates the competition between desalinated water and surface/ground water in the municipal and industrial sectors") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.TechShrwt") %>%
      same_precursors_as(L203.TechCoef_watertd) %>%
      add_precursors("common/iso_GCAM_regID",
                     "L132.water_km3_R_ind_Yh",
                     "L145.municipal_water_R_W_Yh_km3",
                     "L173.in_desal_km3_ctry_ind_Yh",
                     "L173.in_desal_km3_ctry_muni_Yh") ->
      L203.Production_watertd

    return_data(L203.Supplysector_watertd,
                L203.SubsectorLogit_watertd,
                L203.SubsectorShrwtFllt_watertd,
                L203.SubsectorInterp_watertd,
                L203.TechShrwt_watertd,
                L203.TechCoef_watertd,
                L203.Production_watertd)
  } else {
    stop("Unknown command")
  }
}
