#' module_emissions_L122.ghg_agr_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.EmissShare_R_C_Y_GLU}, \code{L122.ghg_tg_R_agr_C_Y_GLU}. The corresponding file in the
#' original data system was \code{L122.ghg_agr_R_S_T_Y} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L122.ghg_agr_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_nation",
             FILE = "emissions/EDGAR/EDGAR_sector",
             "L103.ag_Prod_Mt_R_C_Y_GLU",
             "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
             FILE = "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU",
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
    EDGAR_nation <- get_data(all_data, "emissions/EDGAR/EDGAR_nation") %>%
      distinct()
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU")
    L142.ag_Fert_IO_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU")
    EDGAR_CH4 <- get_data(all_data, "emissions/EDGAR/EDGAR_CH4") %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(Non.CO2 = "CH4_AGR")
    EDGAR_N2O <- get_data(all_data, "emissions/EDGAR/EDGAR_N2O") %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(Non.CO2 = "N20_AGR")
    EDGAR_NH3 <- get_data(all_data, "emissions/EDGAR/EDGAR_NH3") %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(Non.CO2 = "NH3_AGR")
    EDGAR_NOx <- get_data(all_data, "emissions/EDGAR/EDGAR_NOx") %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(Non.CO2 = "NOx_AGR")

    # ===================================================
    # Compute shares of regional cropland allocation by crop type, and regional production of each crop by each GLU
    # In the downscaling from (geopolitical) region to crop and GLU, we use land area to go from region to region/crop, and
    # production to go from region/crop to region/GLU/crop

    # Land area shares (region/crop within region)
    L122.CropAreaShare_R_C_Y <- L122.LC_bm2_R_HarvCropLand_C_Y_GLU %>%
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
      transmute(GCAM_region_ID, GCAM_commodity, GLU, year, emiss_share = prod_share_GLU * crop_area_share)

    L122.EmissShare_R_C_Y_GLU$emiss_share <- with( L122.EmissShare_R_C_Y_GLU, prod_share_GLU * crop_area_share )
    L122.EmissShare_R_C_Y_GLU <- L122.EmissShare_R_C_Y_GLU[ c( R_C_Y_GLU, "emiss_share" ) ]

    # Compute EDGAR emissions by region
    L122.EDGAR <- bind_rows(EDGAR_CH4, EDGAR_N2O, EDGAR_NH3, EDGAR_NOx) %>%
      # Peat fire and forest fire post burn decay not in EDGAR_sector
      fast_left_join(EDGAR_sector %>% select(IPCC, sector = agg_sector), by = "IPCC") %>%
      # ATA, SEA, and AIR not in EDGAR_nation
      fast_left_join(EDGAR_nation, by = "ISO_A3") %>%
      # umi, bvt, sgs, iot, atf, and hmd not in iso_GCAM_regID
      fast_left_join(iso_GCAM_regID, by = "iso") %>%
      group_by(sector, Non.CO2, iso, IPCC) %>%
      filter(!any(is.na(value)))
      select(GCAM_region_ID, sector, Non.CO2, year, value) %>%
      group_by(GCAM_region_ID, sector, Non.CO2) %>%
      filter(!any(is.na(value)))

    #Drop unnecessary columns, aggregate by region, and melt
    L122.EDGAR <- na.omit( L122.EDGAR )
    L122.EDGAR <- aggregate( L122.EDGAR[ X_EDGAR_historical_years ],
                             by = L122.EDGAR[ c( "GCAM_region_ID", "Non.CO2", "sector")], sum)
    L122.EDGAR.melt <- melt( L122.EDGAR, id.vars = c( "GCAM_region_ID", "Non.CO2", "sector" ), variable.name = Y)

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
      add_legacy_name("L122.EmissShare_R_C_Y_GLU") %>%
      add_precursors("L103.ag_Prod_Mt_R_C_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.EmissShare_R_C_Y_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L122.ghg_tg_R_agr_C_Y_GLU") %>%
      add_precursors("L103.ag_Prod_Mt_R_C_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.ghg_tg_R_agr_C_Y_GLU

    return_data(L122.EmissShare_R_C_Y_GLU, L122.ghg_tg_R_agr_C_Y_GLU)
  } else {
    stop("Unknown command")
  }
}
