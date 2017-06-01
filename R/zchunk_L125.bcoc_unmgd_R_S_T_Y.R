#' module_emissions_L125.bcoc_unmgd_R_S_T_Y
#'
#' Historical emissions from unmanaged land by land cover type, computed from RCP emissions data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L125.bcoc_tgbkm2_R_grass_2000}, \code{L125.bcoc_tgbkm2_R_forest_2000}, \code{L125.deforest_coefs_bcoc}. The corresponding file in the
#' original data system was \code{L125.bcoc_unmgd_R_S_T_Y.R} (emissions level1).
#' @details Calculate BC/OC emissions from unmanaged lands (savanna burning, forest fires, deforestation).
#' Downscale EDGAR regional emissions to GLU using shares of land area in each GLU within each region.
#' Divide forest-related emissions into deforestation and forest fires using GFED data
#' Compute global average deforestation emissions coefficients using deforestation from 2000 to 2005 using RCP emissions.
#' Note: Non-CO2s are calculated in a separate chunk.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RMH May 2017

module_emissions_L125.bcoc_unmgd_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
             FILE = "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",
             FILE = "emissions/RCP_BC_2000",
             FILE = "emissions/RCP_OC_2000",
             FILE = "emissions/GFED_ForestFire_BC",
             FILE = "emissions/GFED_Deforest_BC",
             FILE = "emissions/GFED_ForestFire_OC",
             FILE = "emissions/GFED_Deforest_OC"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L125.bcoc_tgbkm2_R_grass_2000",
             "L125.bcoc_tgbkm2_R_forest_2000",
             "L125.deforest_coefs_bcoc"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L124.LC_bm2_R_Grass_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj") %>%
    # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -Land_Type, -GLU) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5)))  # change Xyear to year
    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -Land_Type, -GLU) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5)))  # change Xyear to year
    RCP_BC_2000 <- get_data(all_data, "emissions/RCP_BC_2000")
    RCP_OC_2000 <- get_data(all_data, "emissions/RCP_OC_2000")
    GFED_ForestFire_BC <- get_data(all_data, "emissions/GFED_ForestFire_BC")
    GFED_Deforest_BC <- get_data(all_data, "emissions/GFED_Deforest_BC")
    GFED_ForestFire_OC <- get_data(all_data, "emissions/GFED_ForestFire_OC")
    GFED_Deforest_OC <- get_data(all_data, "emissions/GFED_Deforest_OC")

    # GFED emissions data: Map to GCAM Region (define function for this task)
    # Add column for emission species
    # Select only year 2000. We use GFED data to estimate a ratio - use year 2000
    # We don't know the units on this data, but we're only using it to estimate a ration, not
    # actual emissions or emission factors
    # Aggregate by gcam region

    # Define function to map from EDGAR to GCAM, and select year 2000,
    # rename the value column by forest fire type, and add a Non.CO2 emission species
    edgar_to_gcam <- function(x, fire_type, em_name){ x %>%
      mutate(iso = tolower(Country), Country = NULL) %>%
        change_iso_code('rou', 'rom') %>% # Convert Romania iso code to pre-2002 value
        left_join(iso_GCAM_regID, by = "iso") %>% # There are a set of iso codes in the GFED data that don't exist in the GCAM region mapping. Remove.
        filter(!is.na(GCAM_region_ID)) %>%
        group_by(GCAM_region_ID) %>%
        summarize_at(vars(`2000`), sum) %>%
        mutate_(.dots = stats::setNames(list(~`2000`), fire_type)) %>%
        select(-`2000`) %>%
        mutate(Non.CO2 = em_name)
    }

    # Map all 4 GFED files to GCAM regions
    L125.GFED_ForestFire_BC <- GFED_ForestFire_BC %>%
      edgar_to_gcam("ForestFire", "BC")
    L125.GFED_ForestFire_OC <- GFED_ForestFire_OC %>%
      edgar_to_gcam("ForestFire", "OC")
    L125.GFED_Deforest_BC <- GFED_Deforest_BC %>%
      edgar_to_gcam("Deforest", "BC")
    L125.GFED_Deforest_OC <- GFED_Deforest_OC %>%
      edgar_to_gcam("Deforest", "OC")

    # Calculate forest fire percent of total emissions from GFED data
    # Join forest fire and deforestation emission for BC and OC
    # Combine into one dataframe, then calculate percentage
    L125.GFED_ALL <- bind_rows(
      left_join_error_no_match(L125.GFED_ForestFire_BC, L125.GFED_Deforest_BC, by = c("GCAM_region_ID", "Non.CO2")),
      left_join_error_no_match(L125.GFED_ForestFire_OC, L125.GFED_Deforest_OC, by = c("GCAM_region_ID", "Non.CO2"))) %>%
      mutate(PctForestFire = ForestFire/(ForestFire+Deforest)) %>% # calculate % forest first of total emissions
      mutate(PctForestFire = replace(PctForestFire, is.na(PctForestFire),1)) %>%
      # There are regions where GFED data is zero for both forest fires and deforestation, leading to NAs
      # Assume those missing values are places with 100% forest fires since these are easier to model in GCAM
      arrange(GCAM_region_ID, Non.CO2, ForestFire, Deforest, PctForestFire)

    # RCP emissions by GCAM region
    # Add em species identifier, map to GCAM region, convert from kg to Tg,
    # aggregate to GCAM region
    RCP_BC_2000 <- RCP_BC_2000 %>% mutate(Non.CO2 = "BC")
    RCP_OC_2000 <- RCP_OC_2000 %>% mutate(Non.CO2 = "OC")
    L125.RCP <- bind_rows(RCP_BC_2000, RCP_OC_2000) %>%
      left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
      select(GCAM_region_ID, Non.CO2, lcf, sav) %>%
      mutate(lcf = lcf*CONV_KG_TO_TG, sav = sav*CONV_KG_TO_TG) %>% # convert from kg to Tg
      group_by(GCAM_region_ID, Non.CO2) %>%
      summarize_each(funs(sum)) # sum sav and lcf emissions by GCAM region

    # Compute grassland emissions factors by GCAM region
    # Because grassland and forest fire emissions scale with land quantity, the coefs can be computed at the regional level
    # and applied equally to all land use regions (GLUs) thereafter.
    # (However deforestation emissions are assigned from the region
    # to the GLUs according to relative shares of deforested land, different from forest land cover.)

    L125.bcoc_tgbkm2_R_grass_2000 <- L124.LC_bm2_R_Grass_Yh_GLU_adj %>%
      filter(year == 2000) %>%
      group_by(GCAM_region_ID, Land_Type, year) %>%
      summarize_at(vars(value),sum) %>% # aggregate grassland land area by regions/land type
      repeat_add_columns(tibble::tibble(Non.CO2 = unique(L125.RCP$Non.CO2))) %>% # repeat for both BC and OC
      left_join_error_no_match(L125.RCP %>% select(-lcf), by = c("GCAM_region_ID", "Non.CO2")) %>% # add emissions to land region area
      mutate(em_factor = sav/value) %>% # calculate emission factor (emissions/area)
      select(GCAM_region_ID, Land_Type, Non.CO2, em_factor) %>%
      arrange(Non.CO2, Land_Type,GCAM_region_ID)

    # Compute forest fire emissions factors by GCAM region
    # aggregate grassland area by region/land type
    # Compute total change in forest area from 2000 to 2005, and average annualized coefficients (emissions / change in land area / number of years)
    L125.bcoc_tgbkm2_R_forestfire_2000 <- L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
      filter(year == 2000) %>%
      select(-year) %>%
      group_by(GCAM_region_ID, Land_Type) %>%
      summarize_at(vars(value),sum) %>% # aggregate grassland area to regions/land type
      dplyr::rename(FF_driver = value)
  # select only deforestation coefficient years (used to estiamte rate of change - D-driver)
    L125.bcoc_tgbkm2_R_GLU_defor_2000 <- L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
      filter(year %in% emissions.DEFOREST_COEF_YEARS) %>%
      group_by(GCAM_region_ID, Land_Type, GLU) %>%
      spread(year, value) %>%
      mutate(D_driver = pmax(`2000`-`2005`,0) / (emissions.DEFOREST_COEF_YEARS[2] - emissions.DEFOREST_COEF_YEARS[1]))
  # aggregate the D_driver by region and land type
    L125.bcoc_tgbkm2_R_defor_2000 <-  L125.bcoc_tgbkm2_R_GLU_defor_2000 %>%
      group_by(GCAM_region_ID, Land_Type) %>%
      summarize_at(vars(D_driver),sum)
  # calculate deforestation and forest fire emission factors
    L125.bcoc_tgbkm2_R_forest_2000_calculations <- left_join_error_no_match(L125.bcoc_tgbkm2_R_forestfire_2000,
                                                               L125.bcoc_tgbkm2_R_defor_2000,
                                                               by = c("GCAM_region_ID", "Land_Type")) %>%
      repeat_add_columns(tibble::tibble(Non.CO2 = unique(L125.RCP$Non.CO2))) %>% # repeat for both BC and OC
      left_join_error_no_match(L125.RCP %>% select(-sav), by = c("GCAM_region_ID", "Non.CO2")) %>% # add emissions to land regions
      left_join(L125.GFED_ALL %>% select(-Deforest, -ForestFire), by = c("GCAM_region_ID", "Non.CO2")) %>%
      mutate(ForestFireEmiss = lcf*PctForestFire, # calc forest fire emissions (RCP emissions x GFED ratios)
             DeforestEmiss =  lcf - ForestFireEmiss,  # calc desforestation emissions
             ForestFire = ForestFireEmiss / FF_driver,  # calc forest EF
             Deforest = DeforestEmiss / D_driver) # calc deforestation EF

  # format calculated EFs for output
    L125.bcoc_tgbkm2_R_forest_2000 <- L125.bcoc_tgbkm2_R_forest_2000_calculations %>%
      select(GCAM_region_ID, Land_Type, Non.CO2, ForestFire, Deforest) %>%
      gather(technology, em_factor, -GCAM_region_ID, -Land_Type, -Non.CO2) %>%
      mutate(em_factor = replace(em_factor, is.nan(em_factor), 0)) %>%
      mutate(em_factor = replace(em_factor, is.infinite(em_factor), 0)) %>%
      arrange(Non.CO2, Land_Type,GCAM_region_ID)

  # Calculating average default deforestation emissions factors (to be used in future periods)
    L125.deforest_coefs_bcoc <- L125.bcoc_tgbkm2_R_forest_2000_calculations %>%
      group_by(Non.CO2) %>%
      summarize_at(vars(D_driver, DeforestEmiss), sum) %>%
      mutate(emiss.coef = DeforestEmiss / D_driver)

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L125.bcoc_tgbkm2_R_grass_2000 <- L125.bcoc_tgbkm2_R_grass_2000 %>%
      add_title("BC/OC grassland burning emissions factors by GCAM region / 2000") %>%
      add_units("Tg / bm2") %>%
      add_comments("Grassland EFs from RCP emissions and land area") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L125.bcoc_tgbkm2_R_grass_2000") %>%
      add_precursors("common/iso_GCAM_regID",
                     "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",
                     "emissions/RCP_BC_2000",
                     "emissions/RCP_OC_2000",
                     "emissions/GFED_ForestFire_BC",
                     "emissions/GFED_Deforest_BC",
                     "emissions/GFED_ForestFire_OC",
                     "emissions/GFED_Deforest_OC")

    L125.bcoc_tgbkm2_R_forest_2000 <- L125.bcoc_tgbkm2_R_forest_2000 %>%
      add_title("BC/OC forest fires and deforestation emissions factors by GCAM region / 2000") %>%
      add_units("Tg / bm2") %>%
      add_comments("") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L125.bcoc_tgbkm2_R_forest_2000") %>%
      add_precursors("common/iso_GCAM_regID",
                     "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
                     "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",
                     "emissions/RCP_BC_2000",
                     "emissions/RCP_OC_2000",
                     "emissions/GFED_ForestFire_BC",
                     "emissions/GFED_Deforest_BC",
                     "emissions/GFED_ForestFire_OC",
                     "emissions/GFED_Deforest_OC")

    L125.deforest_coefs_bcoc <- L125.deforest_coefs_bcoc %>%
      add_title("Default deforestation coefficients by BC and OC") %>%
        add_units("kg/m2/yr") %>%
        add_comments("comments describing how data generated") %>%
        add_comments("can be multiple lines") %>%
        add_legacy_name("L125.deforest_coefs_bcoc") %>%
        add_precursors("common/iso_GCAM_regID",
                       "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
                       "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",
                       "emissions/RCP_BC_2000",
                       "emissions/RCP_OC_2000",
                       "emissions/GFED_ForestFire_BC",
                       "emissions/GFED_Deforest_BC",
                       "emissions/GFED_ForestFire_OC",
                       "emissions/GFED_Deforest_OC")

    return_data(L125.bcoc_tgbkm2_R_grass_2000, L125.bcoc_tgbkm2_R_forest_2000, L125.deforest_coefs_bcoc)
  } else {
    stop("Unknown command")
  }
}
