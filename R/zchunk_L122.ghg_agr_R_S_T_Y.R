#' module_emissions_L122.ghg_agr_R_S_T_Y
#'
#' Calculates agricultural emissions shares and downscales EDGAR agricultural emissions data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.EmissShare_R_C_Y_GLU}, \code{L122.ghg_tg_R_agr_C_Y_GLU}. The corresponding file in the
#' original data system was \code{L122.ghg_agr_R_S_T_Y} (emissions level1).
#' @details Calculates agriculture emissions shares by GCAM region, commodity, GLU, and historical year.
#' Downscales EDGAR agricultural emissions to GCAM region, commodity, GLU, and historical year
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH July 2017

module_emissions_L122.ghg_agr_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector",
             "L103.ag_Prod_Mt_R_C_Y_GLU",
             "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
             "L142.ag_Fert_IO_R_C_Y_GLU",
             FILE = "emissions/EDGAR/EDGAR_CH4",
             FILE = "emissions/EDGAR/EDGAR_N2O",
             FILE = "emissions/EDGAR/EDGAR_NH3",
             FILE = "emissions/EDGAR/EDGAR_NOx"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L122.EmissShare_R_C_Y_GLU",
             "L122.ghg_tg_R_agr_C_Y_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU")
    L142.ag_Fert_IO_R_C_Y_GLU <- get_data(all_data, "L142.ag_Fert_IO_R_C_Y_GLU")
    EDGAR_CH4 <- get_data(all_data, "emissions/EDGAR/EDGAR_CH4") %>%
      mutate(Non.CO2 = "CH4_AGR")
    EDGAR_N2O <- get_data(all_data, "emissions/EDGAR/EDGAR_N2O") %>%
      mutate(Non.CO2 = "N2O_AGR")
    EDGAR_NH3 <- get_data(all_data, "emissions/EDGAR/EDGAR_NH3") %>%
      mutate(Non.CO2 = "NH3_AGR")
    EDGAR_NOx <- get_data(all_data, "emissions/EDGAR/EDGAR_NOx") %>%
      mutate(Non.CO2 = "NOx_AGR")

    # ===================================================
    # Compute shares of regional cropland allocation by crop type, and regional production of each crop by each GLU
    # In the downscaling from (geopolitical) region to crop and GLU, we use land area to go from region to region/crop, and
    # production to go from region/crop to region/GLU/crop

    # Land area shares (region/crop within region)
    L122.CropAreaShare_R_C_Y <- L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(year, GCAM_region_ID, GCAM_commodity) %>%
      summarise(value = sum(value)) %>%
      group_by(year, GCAM_region_ID) %>%
      # Crop area by region and year
      mutate(crop_area_total = sum(value)) %>%
      ungroup() %>%
      mutate(crop_area_share = value / crop_area_total)

    # Production shares (region/GLU/crop within region/crop)
    L122.ProdGLUshare_R_C_Y_GLU <- L103.ag_Prod_Mt_R_C_Y_GLU %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      # Production by crop, region, and year
      mutate(Prod_R_C = sum(value)) %>%
      ungroup() %>%
      mutate(prod_share_GLU = value / Prod_R_C) %>%
      replace_na(list(prod_share_GLU = 0))

    # Emissions shares: product of region/crop shares and region/crop/glu shares
    L122.EmissShare_R_C_Y_GLU <- L122.ProdGLUshare_R_C_Y_GLU %>%
      left_join_error_no_match(L122.CropAreaShare_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
      transmute(GCAM_region_ID, GCAM_commodity, year, GLU, emiss_share = prod_share_GLU * crop_area_share)

    # Compute EDGAR emissions by region
    L122.EDGAR <- bind_rows(EDGAR_CH4, EDGAR_N2O, EDGAR_NH3, EDGAR_NOx) %>%
      # Peat fire and forest fire post burn decay not in EDGAR_sector
      left_join(EDGAR_sector %>% select(IPCC, sector = agg_sector), by = "IPCC") %>%
      standardize_iso(col = "ISO_A3") %>%
      change_iso_code('rou', 'rom') %>%
      # umi, bvt, sgs, iot, atf, and hmd not in iso_GCAM_regID
      left_join(iso_GCAM_regID, by = "iso") %>%
      na.omit() %>%
      gather(year, value , matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      select(GCAM_region_ID, sector, Non.CO2, year, value) %>%
      filter(year %in% emissions.EDGAR_HISTORICAL) %>%
      # Aggregate and convert to Tg
      group_by(GCAM_region_ID, Non.CO2, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      mutate(value = value * CONV_GG_TG)

    # Filter out agricultural sectors
    L122.EDGAR_agr <- L122.EDGAR %>%
      filter(sector %in% emissions.AGR_SECTORS)

    # Compute emissions from rice by GCAM region, commodity, and GLU
    L122.EDGAR_rice <- L122.EDGAR_agr %>%
      filter(sector == "rice")

    # Compute share of rice production by GLU in each region / year
    L122.ag_Prod_Mt_R_rice_Y_GLU <- L103.ag_Prod_Mt_R_C_Y_GLU %>%
      filter(GCAM_commodity == "Rice", year %in% emissions.EDGAR_HISTORICAL) %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      # Total production by region, commodity, and year for calculating share
      mutate(total_prod = sum(value)) %>%
      ungroup() %>%
      transmute(GCAM_region_ID, GCAM_commodity, GLU, year, prod_share = value / total_prod)

    # Multiply total emissions by production share
    L122.ghg_tg_R_rice_Y_GLU <- L122.ag_Prod_Mt_R_rice_Y_GLU %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L122.EDGAR_rice$Non.CO2))) %>%
      left_join_error_no_match(L122.EDGAR_rice, by = c("GCAM_region_ID", "Non.CO2", "year")) %>%
      transmute(GCAM_region_ID, GCAM_commodity, year, GLU, Non.CO2,
                emissions = value * prod_share, type = "Rice")

    # Compute emissions from soils by GCAM region, commodity, and GLU
    L122.EDGAR_soil <- L122.EDGAR_agr %>%
      filter(sector == "soil")

    # Multiply total emissions by production share
    L122.ghgsoil_tg_R_C_Y_GLU <- L122.EmissShare_R_C_Y_GLU %>%
      filter(year %in% emissions.EDGAR_HISTORICAL) %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L122.EDGAR_soil$Non.CO2))) %>%
      left_join_error_no_match(L122.EDGAR_soil, by = c("GCAM_region_ID", "year", "Non.CO2")) %>%
      transmute(GCAM_region_ID, GCAM_commodity, year, GLU, Non.CO2,
                emissions = value * emiss_share, type = "Soil")

    # Compute emissions from fertilizer by GCAM region, commodity, and GLU
    L122.EDGAR_fert <- L122.EDGAR_agr %>%
      filter(sector == "fertilizer")

    #Compute fertilizer by crop
    L122.fert_Mt_R_C_Y_GLU <- L103.ag_Prod_Mt_R_C_Y_GLU %>%
      filter(year %in% emissions.EDGAR_YEARS) %>%
      rename(ag_production = value) %>%
      left_join_error_no_match(L142.ag_Fert_IO_R_C_Y_GLU %>%
                                 filter(year %in% emissions.EDGAR_YEARS), ., # Order flipped to deal with timeshift
                               by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
      mutate(fertilizer = ag_production * value) %>%
      group_by(GCAM_region_ID, year) %>%
      # Total fertilizer by region and year for calculating share
      mutate(tot_fertilizer = sum(fertilizer)) %>%
      ungroup() %>%
      transmute(GCAM_region_ID, GCAM_commodity, year, GLU,
        fert_share = fertilizer / tot_fertilizer)

    L122.ghgfert_tg_R_C_Y_GLU <- L122.fert_Mt_R_C_Y_GLU %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L122.EDGAR_fert$Non.CO2))) %>%
      left_join_error_no_match(L122.EDGAR_fert, by = c("GCAM_region_ID", "year", "Non.CO2")) %>%
      transmute(GCAM_region_ID, GCAM_commodity, year, GLU, Non.CO2,
                emissions = value * fert_share, type = "Fertilizer")

    # Bind together dataframes & aggregate
    L122.ghg_tg_R_agr_C_Y_GLU <- bind_rows( L122.ghg_tg_R_rice_Y_GLU, L122.ghgsoil_tg_R_C_Y_GLU, L122.ghgfert_tg_R_C_Y_GLU) %>%
      group_by(GCAM_region_ID, GCAM_commodity, year, GLU, Non.CO2) %>%
      summarise(value = sum(emissions)) %>%
      ungroup()

    # ===================================================
    # Produce outputs
    L122.EmissShare_R_C_Y_GLU %>%
      add_title("Agriculture emissions shares by GCAM region, commodity, GLU, and historical year") %>%
      add_units("unitless share") %>%
      add_comments("Multiply region/crop area shares by region/crop/GLU production shares") %>%
      add_legacy_name("L122.EmissShare_R_C_Y_GLU") %>%
      add_precursors("L103.ag_Prod_Mt_R_C_Y_GLU", "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU") %>%
      add_flags(FLAG_YEAR_COL_XYEARS) ->
      L122.EmissShare_R_C_Y_GLU

    L122.ghg_tg_R_agr_C_Y_GLU %>%
      add_title("Agriculture emissions by GCAM region, commodity, GLU, and historical year") %>%
      add_units("Tg") %>%
      add_comments("EDGAR emissions shared out by crop production") %>%
      add_legacy_name("L122.ghg_tg_R_agr_C_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector",
                     "L103.ag_Prod_Mt_R_C_Y_GLU", "L142.ag_Fert_IO_R_C_Y_GLU",
                     "emissions/EDGAR/EDGAR_CH4", "emissions/EDGAR/EDGAR_N2O", "emissions/EDGAR/EDGAR_NH3", "emissions/EDGAR/EDGAR_NOx") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.ghg_tg_R_agr_C_Y_GLU

    return_data(L122.EmissShare_R_C_Y_GLU, L122.ghg_tg_R_agr_C_Y_GLU)
  } else {
    stop("Unknown command")
  }
}
