# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

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
             FILE = "water/A03.subsector",
             FILE = "water/basin_to_country_mapping",
             FILE = "water/water_td_sectors",
             "L103.water_mapping_R_GLU_B_W_Ws_share",
             "L103.water_mapping_R_B_W_Ws_share",
             "L110.in_km3_water_primary",
             "L1233.wdraw_km3_R_elec",
             "L1233.wcons_km3_R_elec",
             "L133.water_demand_livestock_R_B_W_km3",
             "L125.LC_bm2_R_GLU",
             "L132.water_km3_R_ind_Yh",
             "L145.municipal_water_R_W_Yh_km3",
             "L165.ag_IrrEff_R",
             "L171.share_R_desal_basin",
             "L173.in_desal_km3_ctry_ind_Yh",
             "L173.in_desal_km3_ctry_muni_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L203.Supplysector_watertd",
             "L203.SubsectorLogit_watertd",
             "L203.SubsectorShrwtFllt_watertd",
             "L203.SubsectorInterp_watertd",
             "L203.TechShrwt_watertd",
             "L203.TechInterp_watertd",
             "L203.TechCoef_watertd",
             "L203.TechPmult_watertd",
             "L203.Production_watertd",
             "L203.Supplysector_desal_basin",
             "L203.SubsectorLogit_desal_basin",
             "L203.SubsectorShrwtFllt_desal_basin",
             "L203.TechShrwt_desal_basin",
             "L203.TechCoef_desal_basin"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A03.sector <- get_data(all_data, "water/A03.sector")
    A03.subsector <- get_data(all_data, "water/A03.subsector")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    water_td_sectors <- get_data(all_data, "water/water_td_sectors")
    L103.water_mapping_R_GLU_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_GLU_B_W_Ws_share", strip_attributes = TRUE)
    L103.water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share", strip_attributes = TRUE)
    L110.in_km3_water_primary <- get_data(all_data, "L110.in_km3_water_primary", strip_attributes = TRUE)
    L1233.wdraw_km3_R_elec <- get_data(all_data, "L1233.wdraw_km3_R_elec", strip_attributes = TRUE)
    L1233.wcons_km3_R_elec <- get_data(all_data, "L1233.wcons_km3_R_elec", strip_attributes = TRUE)
    L133.water_demand_livestock_R_B_W_km3 <- get_data(all_data, "L133.water_demand_livestock_R_B_W_km3", strip_attributes = TRUE)
    L125.LC_bm2_R_GLU <- get_data(all_data, "L125.LC_bm2_R_GLU", strip_attributes = TRUE)
    L132.water_km3_R_ind_Yh <- get_data(all_data, "L132.water_km3_R_ind_Yh", strip_attributes = TRUE)
    L145.municipal_water_R_W_Yh_km3 <- get_data(all_data, "L145.municipal_water_R_W_Yh_km3", strip_attributes = TRUE)
    L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R", strip_attributes = TRUE)
    L171.share_R_desal_basin <- get_data(all_data, "L171.share_R_desal_basin", strip_attributes = TRUE)
    L173.in_desal_km3_ctry_ind_Yh <- get_data(all_data, "L173.in_desal_km3_ctry_ind_Yh", strip_attributes = TRUE)
    L173.in_desal_km3_ctry_muni_Yh <- get_data(all_data, "L173.in_desal_km3_ctry_muni_Yh", strip_attributes = TRUE)

    GCAM_region_ID <- GLU <- GLU_code <- GLU_name <- water.sector <-
      water_type <- supplysector <- field.eff <- conveyance.eff <-
      coefficient <- region <- has.desal.input <- supplysector_root <-
      subsector <- technology <- desal_ind_km3 <- desal_muni_km3 <-
      year <- iso <- desal_km3 <- water_km3 <- withdrawals <- total_water_km3 <-
      calOutputValue <- desal_water_km3 <- GCAM_basin_ID <- basin_name <-
      water_sector <- share <- basin_share <- desal_share <- water_type_fresh <-
      withdrawals_share <- water_type_desal <- value <- water_type_share <- NULL  # silence package check notes

    # Create tibble with all possible mapping sectors...

    # Irrigated and non-irrigated are done separately, as the former are done by basin
    L203.water_td_info_irr <- select(L125.LC_bm2_R_GLU, GCAM_region_ID, GLU) %>%
      mutate(water.sector = water.IRRIGATION) %>%
      left_join_error_no_match(water_td_sectors, by = "water.sector") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GCAM_basin_ID, GLU_code, GLU_name),
                               by = c(GLU = "GLU_code")) %>%
      mutate(basin_name = GLU_name) %>%
      repeat_add_columns(tibble(water_type = c("water withdrawals", "water consumption"))) %>%
      left_join_error_no_match(L103.water_mapping_R_GLU_B_W_Ws_share,
                               by = c("GCAM_region_ID", GCAM_basin_ID = "GLU", "water_type", water.sector = "water_sector")) %>%
      mutate(supplysector_root = supplysector,
             supplysector = set_water_input_name(water.sector, water_type, water_td_sectors, GLU = GLU_name),
             subsector = basin_name,
             technology = water_type)

    L203.water_td_info <- L103.water_mapping_R_B_W_Ws_share %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GCAM_basin_ID, GLU_code, GLU_name), by = "GCAM_basin_ID") %>%
      mutate(basin_name = GLU_name) %>%
      rename(water.sector = water_sector,
             GLU = GLU_code,
             basin_share = share) %>%
      left_join_error_no_match(water_td_sectors, by = "water.sector") %>%
      mutate(supplysector_root = supplysector,
             supplysector = set_water_input_name(water.sector, water_type, water_td_sectors),
             subsector = basin_name,
             technology = water_type) %>%
      bind_rows(L203.water_td_info_irr) %>%
      select(region, water.sector, supplysector_root, supplysector, subsector, technology, water_type, basin_share, has.desal.input)

    # For desalination, only create subsectors for basins where desalination exists
    L203.desal_basins <- L171.share_R_desal_basin %>%
      rename(basin_share = share) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GCAM_basin_ID, subsector = GLU_name),
                               by = "GCAM_basin_ID") %>%
      select(region, subsector, basin_share)

    # L203.water_td_info is first filtered to the water sectors that can take an input of desalinated water, and second,
    # using inner_join, this table is further filtered to only basins where desalination occurs, while joining in
    # the basin-within-region share of desalinated water production for each region
    L203.water_td_info_desal <- filter(L203.water_td_info, has.desal.input == 1 & water_type == "water withdrawals") %>%
      select(-basin_share) %>%
      inner_join(L203.desal_basins, by = c("region", "subsector")) %>%
      mutate(technology = water.DESAL,
             water_type = water.DESAL) %>%
      distinct()

    L203.water_td_info <- bind_rows(L203.water_td_info, L203.water_td_info_desal) %>%
      select(-has.desal.input)

    # Sector information
    L203.Supplysector_watertd <- left_join_error_no_match(L203.water_td_info, A03.sector,
                                                          by = c(supplysector_root = "supplysector"),
                                                          ignore_columns = "logit.type") %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME) %>%
      distinct()

    L203.SubsectorLogit_watertd <- L203.water_td_info %>%
      left_join_error_no_match(select(A03.subsector, -subsector),
                               by = c(supplysector_root = "supplysector"),
                               ignore_columns = "logit.type") %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)


    # Default subsector share weights (fill out 1 from start-year)
    L203.SubsectorShrwtFllt_watertd <- L203.SubsectorLogit_watertd %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1.0) %>%
    select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])

    # Subsector share-weight interpolation (for competition between basins - fixed share-weight interpolation)
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
      left_join(L203.ag_IrrEff_R, by = "region") %>%
      mutate(minicam.energy.input = paste(subsector, water_type, sep = "_"),
             coefficient = if_else(water.sector == water.IRRIGATION,
                                   round(1 / conveyance.eff, energy.DIGITS_COEFFICIENT),
                                   1)) %>%
      mutate(market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]])

    # Shareweights of water T&D technologies - for competition between desal and water withdrawals.
    # read in a default value of 1, with a fixed interpolation rule from the final model base year
    L203.TechInterp_watertd <- L203.water_td_info %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["TechInterp"]])

    L203.TechShrwt_watertd <- mutate(L203.water_td_info, share.weight = 1) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]])

    L203.TechPmult_watertd <- subset(L203.water_td_info, water.sector == "Irrigation" & water_type == "water withdrawals") %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(pMult = water.IRR_PRICE_SUBSIDY_MULT) %>%
      select(LEVEL2_DATA_NAMES[["TechPmult"]])

    # Calibrated flows of water through the T&D sectors
    # Calibration quantities are only read in for non-irrigation sectors, as the irrigation sectors are just pass-thru
    # For sectors that use desalinated water, the quantities read in to each technology (sector, basin, and water_type)
    # are equal to the value of water demand by the given sector, multiplied by basin share and multiplied by water_type
    # share (desal versus freshwater withdrawals). The first part below computes the water_type shares by region, from
    # the total water withdrawals by sector and the desal water inputs. Note that the basin-wise share of withdrawals do
    # not consider use of desalinated water (as per the definition of "withdrawals").
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
      summarise(desal_km3 = sum(desal_km3)) %>%
      ungroup() %>%
      complete(region = GCAM_region_names$region, year, water_type, water.sector) %>%
      replace_na(list(desal_km3 = 0))

    # Prepare the water withdrawals by industry and municipal sectors for merging, merge, and deduct
    # the input of desalinated water above to calibrate the input from "water withdrawals"
    L203.water_km3_R_ind_Yh <- mutate(L132.water_km3_R_ind_Yh, water.sector = water.MANUFACTURING) %>%
      filter(water_type == "water withdrawals") %>%
      rename(total_water_km3 = water_km3)

    L203.water_type_shares <- mutate(L145.municipal_water_R_W_Yh_km3, water.sector = water.MUNICIPAL,
                                      water_type = "water withdrawals") %>%
      rename(total_water_km3 = withdrawals) %>%
      bind_rows(L203.water_km3_R_ind_Yh) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(total_water_km3 = round(total_water_km3, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(L203.watertd_desal,
                by = c("region", "year", "water.sector"),
                suffix = c("_fresh", "_desal")) %>%
      mutate(desal_share = desal_km3 / total_water_km3,
             withdrawals_share = 1 - desal_share)

    L203.water_type_shares <- bind_rows(
      select(L203.water_type_shares, region, water_type = water_type_fresh, water.sector, year, water_type_share = withdrawals_share),
      select(L203.water_type_shares, region, water_type = water_type_desal, water.sector, year, water_type_share = desal_share)
    )

    # Total withdrawal and consumption volumes by each region and sector
    L203.water_km3_primary <- L110.in_km3_water_primary %>%
      mutate(water.sector = "Mining")
    L1233.wcons_km3_R_elec <- filter(L1233.wcons_km3_R_elec, water_type == "fresh") %>%
      mutate(water_type = "water consumption")
    L1233.wdraw_km3_R_elec <- filter(L1233.wdraw_km3_R_elec, water_type == "fresh") %>%
      mutate(water_type = "water withdrawals")
    L203.water_km3_elec <- bind_rows(L1233.wdraw_km3_R_elec, L1233.wcons_km3_R_elec) %>%
      mutate(water.sector = "Electricity")
    L203.water_km3_ind <- L132.water_km3_R_ind_Yh %>%
      rename(value = water_km3) %>%
      mutate(water.sector = water.MANUFACTURING)
    L203.water_km3_livestock <- L133.water_demand_livestock_R_B_W_km3 %>%
      mutate(water.sector = "Livestock")
    L203.water_km3_muni <- L145.municipal_water_R_W_Yh_km3 %>%
      gather(-GCAM_region_ID, -year, key = water_type, value = value) %>%
      mutate(water.sector = water.MUNICIPAL,
             water_type = paste("water", water_type, sep = " "))

    L203.all_calibrated_sectors <- bind_rows(
      L203.water_km3_primary,
      L203.water_km3_elec,
      L203.water_km3_ind,
      L203.water_km3_livestock,
      L203.water_km3_muni) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(supplysector = set_water_input_name(water.sector, water_type, water_td_sectors)) %>%
      group_by(region, supplysector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Calibrated output: join together the basin_shares, water_type_shares, and total output volumes
    L203.Production_watertd <- L203.water_td_info %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      left_join(L203.water_type_shares, by = c("region", "water.sector", "water_type", "year")) %>%
      replace_na(list(water_type_share = 1)) %>%
      inner_join(L203.all_calibrated_sectors,
                by = c("region", "supplysector", "year")) %>%
      mutate(calOutputValue = round(value * water_type_share * basin_share, water.DIGITS_TD_FLOWS),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      set_subsector_shrwt(value_col = "calOutputValue") %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # Final step - build desalination pass-through sectors, for tracking the use of desalinated water by basin
    L203.R_desal_basin <- L171.share_R_desal_basin %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GCAM_basin_ID, basin_name = "GLU_name"),
                               by = "GCAM_basin_ID") %>%
      select(region, basin_name)

    L203.Supplysector_desal_basin <-
      tibble(region = L203.R_desal_basin$region,
             supplysector = paste(L203.R_desal_basin$basin_name, water.DESAL, sep = "_"),
             output.unit = A03.sector$output.unit[1],
             input.unit = A03.sector$input.unit[1],
             price.unit = A03.sector$price.unit[1],
             logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcam.DEFAULT_SUBSECTOR_LOGIT,
             logit.type = NA_character_)

    L203.SubsectorLogit_desal_basin <- L203.Supplysector_desal_basin %>%
      select(region, supplysector) %>%
      mutate(subsector = supplysector,
             logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcam.DEFAULT_TECH_LOGIT,
             logit.type = NA_character_)

    L203.SubsectorShrwtFllt_desal_basin <- L203.SubsectorLogit_desal_basin %>%
      select(region, supplysector, subsector) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1)

    L203.TechShrwt_desal_basin <- L203.SubsectorShrwtFllt_desal_basin %>%
      select(region, supplysector, subsector) %>%
      mutate(technology = supplysector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1)

    L203.TechCoef_desal_basin <- L203.TechShrwt_desal_basin %>%
      select(region, supplysector, subsector, technology, year) %>%
      mutate(minicam.energy.input = water.DESAL,
             coefficient = 1,
             market.name = region)

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
      same_precursors_as(L203.Supplysector_watertd) %>%
      add_precursors("water/A03.subsector",
                     "L171.share_R_desal_basin")->
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

    L203.TechInterp_watertd %>%
      add_title("Water technology shareweight interpolation") %>%
      add_units("Unitless") %>%
      add_comments("For competition between desal and freshwater withdrawals") %>%
      same_precursors_as(L203.TechCoef_watertd) ->
      L203.TechInterp_watertd

    L203.TechShrwt_watertd %>%
      add_title("Water technology shareweights") %>%
      add_units("Unitless") %>%
      add_comments("Technology shareweights expanded to GLU regions and water demand sectors") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.TechShrwt") %>%
      same_precursors_as(L203.TechCoef_watertd) ->
      L203.TechShrwt_watertd

    L203.TechPmult_watertd %>%
      add_title("Water t&d sector price multipliers") %>%
      add_units("Unitless") %>%
      add_comments("Subsidies implemented as multipliers") %>%
      same_precursors_as(L203.TechCoef_watertd) ->
      L203.TechPmult_watertd

    L203.Production_watertd %>%
      add_title("Water technology calibration") %>%
      add_units("km3/yr") %>%
      add_comments("Calibrates the competition between desalinated water and surface/ground water in the municipal and industrial sectors") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.TechShrwt") %>%
      same_precursors_as(L203.TechCoef_watertd) %>%
      add_precursors("common/iso_GCAM_regID",
                     "L103.water_mapping_R_GLU_B_W_Ws_share",
                     "L103.water_mapping_R_B_W_Ws_share",
                     "L110.in_km3_water_primary",
                     "L1233.wdraw_km3_R_elec",
                     "L1233.wcons_km3_R_elec",
                     "L132.water_km3_R_ind_Yh",
                     "L133.water_demand_livestock_R_B_W_km3",
                     "L145.municipal_water_R_W_Yh_km3",
                     "L171.share_R_desal_basin",
                     "L173.in_desal_km3_ctry_ind_Yh",
                     "L173.in_desal_km3_ctry_muni_Yh") ->
      L203.Production_watertd

    L203.Supplysector_desal_basin %>%
      add_title("Desalination basin passthru sector information") %>%
      add_units("Unitless") %>%
      add_comments("These sectors are intended to help explicitly track desalination by basin") %>%
      add_precursors("L171.share_R_desal_basin") ->
      L203.Supplysector_desal_basin

    L203.SubsectorLogit_desal_basin %>%
      add_title("Desalination basin passthru subsector logit exponents") %>%
      add_units("Unitless") %>%
      add_comments("Model default values; no competition represented") %>%
      add_precursors("L171.share_R_desal_basin") ->
      L203.SubsectorLogit_desal_basin

    L203.SubsectorShrwtFllt_desal_basin %>%
      add_title("Desalination basin passthru subsector share-weights") %>%
      add_units("Unitless") %>%
      add_comments("Pass-through; all years available") %>%
      add_precursors("L171.share_R_desal_basin") ->
      L203.SubsectorShrwtFllt_desal_basin

    L203.TechShrwt_desal_basin %>%
      add_title("Desalination basin passthru technology share-weights") %>%
      add_units("Unitless") %>%
      add_comments("Pass-through; all years available") %>%
      add_precursors("L171.share_R_desal_basin") ->
      L203.TechShrwt_desal_basin

    L203.TechCoef_desal_basin %>%
      add_title("Desalination basin passthru technology share-weights") %>%
      add_units("Unitless") %>%
      add_comments("Pass-through; all years available") %>%
      add_precursors("L171.share_R_desal_basin") ->
      L203.TechCoef_desal_basin

    return_data(L203.Supplysector_watertd,
                L203.SubsectorLogit_watertd,
                L203.SubsectorShrwtFllt_watertd,
                L203.SubsectorInterp_watertd,
                L203.TechShrwt_watertd,
                L203.TechInterp_watertd,
                L203.TechCoef_watertd,
                L203.TechPmult_watertd,
                L203.Production_watertd,
                L203.Supplysector_desal_basin,
                L203.SubsectorLogit_desal_basin,
                L203.SubsectorShrwtFllt_desal_basin,
                L203.TechShrwt_desal_basin,
                L203.TechCoef_desal_basin)
  } else {
    stop("Unknown command")
  }
}
