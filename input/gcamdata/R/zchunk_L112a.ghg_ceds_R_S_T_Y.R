#' module_emissions_L112.ceds_ghg_en_R_S_T_Y
#'
#' Calculates emissions and emissions factors using EPA emissions factors and scales to EDGAR emissions. #**** is this accurate?
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L112.cedsghg_tg_R_en_S_F_Yh}, \code{L112.cedsghg_tgej_R_en_S_F_Yh}. The corresponding file in the
#' original data system was \code{L112.ghg_en_R_S_T_Y.R} (emissions level1).
#' @details Calculates emissions using EPA emissions factors and energy data. Then scales to EDGAR emissions and calculates emissions factors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select mutate_all
#' @importFrom tidyr gather spread
#' @author CWR Oct. 2018 / YO Mar. 2020

module_emissions_L112.ceds_ghg_en_R_S_T_Y <- function(command, ...) {
  if(driver.EMISSIONS_SOURCE == "EDGAR") {
    if(command == driver.DECLARE_INPUTS) {
      return(NULL)
    } else if(command == driver.DECLARE_OUTPUTS) {
      return(NULL)
    } else if(command == driver.MAKE) {
      return_data()
    } else {
      stop("Unknown command")
    }}
  else {
    if(command == driver.DECLARE_INPUTS) {
      return(c(FILE = "common/GCAM_region_names",
               FILE = "common/iso_GCAM_regID",
               FILE = "emissions/mappings/GCAM_sector_tech_CEDS",
               FILE = "emissions/mappings/GCAM_sector_tech_CEDS_revised",
               FILE = "energy/mappings/UCD_techs",
               FILE = "emissions/mappings/UCD_techs_emissions_revised",
               FILE = "energy/calibrated_techs",
               FILE = "energy/calibrated_techs_bld_det",
               FILE = "emissions/mappings/Trn_subsector",
               FILE = "emissions/mappings/Trn_subsector_revised",
               FILE = "emissions/CEDS/CEDS_sector_tech",
               FILE = "emissions/CEDS/CEDS_sector_tech_revised",
               FILE = "emissions/EPA_FCCC_IndProc_2005",
               FILE = "emissions/mappings/calibrated_outresources",
               "L102.ceds_GFED_nonco2_tg_R_S_F",
               "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
               "L101.in_EJ_R_en_Si_F_Yh",
               "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
               "L101.ag_Prod_Mt_R_C_Y_GLU",
               "L111.ag_resbio_R_C",
               "L103.ghg_tgmt_USA_an_Sepa_F_2005",
               "L124.LC_bm2_R_Grass_Yh_GLU_adj",
               "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",

               FILE = "emissions/CEDS/ceds_sector_map",
               FILE = "emissions/CEDS/ceds_fuel_map",

               FILE = "emissions/EPA_country_map",
               # EPA scaling process 2020
               FILE = "emissions/EPA/EPA_2019_raw",
               FILE = "emissions/EPA_CH4N2O_map",
               FILE = "energy/rsrc_fol_prod_vintage"))
    } else if(command == driver.DECLARE_OUTPUTS) {
      return(c("L111.nonghg_tg_R_en_S_F_Yh",
               "L111.nonghg_tgej_R_en_S_F_Yh",
               "L112.ghg_tg_R_en_S_F_Yh",
               "L112.ghg_tgej_R_en_S_F_Yh",
               "L113.ghg_tg_R_an_C_Sys_Fd_Yh",
               "L115.nh3_tg_R_an_C_Sys_Fd_Yh",
               "L121.nonco2_tg_R_awb_C_Y_GLU",
               "L121.AWBshare_R_C_Y_GLU",
               "L122.ghg_tg_R_agr_C_Y_GLU",
               "L122.EmissShare_R_C_Y_GLU",
               "L124.nonco2_tg_R_grass_Y_GLU",
               "L124.nonco2_tg_R_forest_Y_GLU",
               "L124.deforest_coefs",
               "L131.nonco2_tg_R_prc_S_S_Yh"))
    } else if(command == driver.MAKE) {

      all_data <- list(...)[[1]]
      #kbn 2019/11/11 We don't need the code below this line.
      L112.CEDS_GCAM <- get_data(all_data, "L102.ceds_GFED_nonco2_tg_R_S_F")

      # Optionally gets pre-built CEDS data
      if(is.null(L112.CEDS_GCAM)) {
       #Proprietary IEA energy data are not available, so used saved outputs
      L112.CEDS_GCAM <- prebuilt_data("L102.ceds_GFED_nonco2_tg_R_S_F")
      } else {
      }

      # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
      iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")

      if (energy.TRAN_UCD_MODE=="rev.mode"){
        GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech_CEDS_revised") %>% distinct()
      }else{
      GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech_CEDS")}

      #kbn 2019/11/11 Add code below so that we can use revised sub-sectors from transportation model
      if (energy.TRAN_UCD_MODE=="rev.mode"){
      Trn_subsector <- get_data(all_data, "emissions/mappings/Trn_subsector_revised")}else{

        Trn_subsector <- get_data(all_data, "emissions/mappings/Trn_subsector")

      }


      if (energy.TRAN_UCD_MODE=="rev.mode"){
        UCD_techs <- get_data(all_data, "emissions/mappings/UCD_techs_emissions_revised")
      }else{
      UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs")}
      calibrated_techs <- get_data(all_data, "energy/calibrated_techs")

      calibrated_techs_bld_det <- get_data(all_data, "energy/calibrated_techs_bld_det")
      L101.in_EJ_R_en_Si_F_Yh <- get_data(all_data, "L101.in_EJ_R_en_Si_F_Yh") %>%
        gather_years(value_col = "energy")
      CEDS_sector_map <- get_data(all_data, "emissions/CEDS/ceds_sector_map")
      CEDS_fuel_map <- get_data(all_data, "emissions/CEDS/ceds_fuel_map")

      #kbn 2019/11/11 Add in transport flexibility below
      if (energy.TRAN_UCD_MODE=="rev.mode"){
      CEDS_sector_tech <- get_data(all_data, "emissions/CEDS/CEDS_sector_tech_revised") %>% distinct()}else{

        CEDS_sector_tech <- get_data(all_data, "emissions/CEDS/CEDS_sector_tech")
      }

      calibrated_outresources <- get_data(all_data, "emissions/mappings/calibrated_outresources")

      L124.LC_bm2_R_Grass_Yh_GLU_adj <- get_data(all_data, "L124.LC_bm2_R_Grass_Yh_GLU_adj")
      L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- get_data(all_data, "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj")
      L107.an_Prod_Mt_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_Prod_Mt_R_C_Sys_Fd_Y")
      L101.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L101.ag_Prod_Mt_R_C_Y_GLU")
      L111.ag_resbio_R_C <- get_data(all_data, "L111.ag_resbio_R_C")
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU")
      #L142.ag_Fert_IO_R_C_Y_GLU <- get_data(all_data, "L142.ag_Fert_IO_R_C_Y_GLU")

      # Temporary to run for testing and matching Animal emissions
      L103.ghg_tgmt_USA_an_Sepa_F_2005 <- get_data(all_data, "L103.ghg_tgmt_USA_an_Sepa_F_2005")

       #kbn adding notin for later calculations
      `%notin%` <- Negate(`%in%`)

      # ===========================
      # Part 1:Combustion Energy Emissions
      # ===========================

      # Filter down to combustion emissions plus fugitive process emissions from combustion resource production (out_resources)
      L112.CEDS_GCAM %>%
        filter(CEDS_agg_fuel != "process" | CEDS_agg_sector %in% c("oil_gas", "coal")) ->
        L112.CEDS_GCAM_emissions

      # PREPARE ENERGY FOR MATCHING TO EMISSIONS
      # ----------------------------------------

      # Splits energy balances out for industry sector and maps to final GCAM sectors
      L101.in_EJ_R_en_Si_F_Yh %>%
        #Need to add three missing out_resources rows that are not included in base calibrated techs file
        left_join(calibrated_techs %>% bind_rows(calibrated_outresources) %>% select(-secondary.output), by = c("sector", "fuel", "technology")) %>%
        rename(stub.technology = technology) %>%
        select(GCAM_region_ID, year, energy, supplysector, subsector, stub.technology) %>%
        na.omit() ->
        L112.in_EJ_R_en_S_F_Yh_calibtech

      # Splits energy balances out for building sector and maps to final GCAM sectors
      L101.in_EJ_R_en_Si_F_Yh %>%
        left_join(calibrated_techs_bld_det %>% select(sector, fuel, service, supplysector, subsector, technology) %>% rename(stub.technology = technology), by = c("sector" = "service", "fuel")) %>%
        na.omit() %>%
        select(GCAM_region_ID, year, energy, supplysector, subsector, stub.technology) ->
        L112.in_EJ_R_en_S_F_Yh_calib_bld

      # Splits energy balances out for transport sector and maps to final GCAM sectors
      L101.in_EJ_R_en_Si_F_Yh %>%
        left_join(Trn_subsector, by = c("fuel")) %>%
        left_join_keep_first_only(UCD_techs %>% select(UCD_sector, mode, size.class, fuel, supplysector, tranSubsector, tranTechnology) %>% rename(technology = fuel, subsector = tranSubsector, stub.technology = tranTechnology),
                                  by = c("sector" = "UCD_sector", "mode", "size.class", "technology")) %>%
        na.omit() %>%
        select(GCAM_region_ID, year, energy, supplysector, subsector, stub.technology) ->
        L112.in_EJ_R_en_S_F_Yh_calib_trn



      # Rebind separate sectors into master list
      L112.in_EJ_R_en_S_F_Yh_calibtech %>%
        bind_rows(L112.in_EJ_R_en_S_F_Yh_calib_bld, L112.in_EJ_R_en_S_F_Yh_calib_trn) ->
        L112.in_EJ_R_en_S_F_Yh_calib_all

      # Compute energy by sector, subsector, and technology
      L112.in_EJ_R_en_S_F_Yh_calib_all %>%
        group_by(GCAM_region_ID, year, supplysector, subsector, stub.technology) %>%
        summarise(energy = sum(energy)) %>%
        ungroup() ->
        L112.in_EJ_R_en_S_F_Yh_calib_all


      # MATCH ENERGY AND EMISSIONS TO AGGREGATE EMISSIONS TO SPLIT OUT EMISSIONS BY GCAM SECTORS
      # ========================================================================================

      # Append CEDS sector/fuel combinations to GCAM energy
      L112.in_EJ_R_en_S_F_Yh_calib_all %>%
        filter(stub.technology %notin% c(emissions.ZERO_EM_TECH)) %>%
        #We will drop all electricity sectors here
        left_join_error_no_match(CEDS_sector_tech, by = c("supplysector", "subsector", "stub.technology")) ->L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy




      # Aggregate GCAM energy to CEDS sector/fuel combinations and compute the total energy by CEDS sector
      L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy %>%
        group_by(GCAM_region_ID, year, CEDS_agg_sector, CEDS_agg_fuel) %>%
        summarise(totalenergy = sum(energy)) %>%
        ungroup() ->
        L112.in_EJ_R_en_S_F_Yh_calib_CEDS_NAs


      # Replace all base energy values in sectors with a total energy of 0 with 1,
      # allowing shares to be calculated for those sectors in the absence of energy
      L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy %>%
        left_join_error_no_match(L112.in_EJ_R_en_S_F_Yh_calib_CEDS_NAs,
                                 by = c("GCAM_region_ID", "year", "CEDS_agg_sector", "CEDS_agg_fuel")) %>%
        mutate(energy = if_else(totalenergy == 0, 1, energy)) %>%
        select(-totalenergy) ->
        L112.in_EJ_R_en_S_F_Yh_calib_newbase

      # Calculate new total energy including dummy values for zero total energy sectors
      L112.in_EJ_R_en_S_F_Yh_calib_newbase %>%
        group_by(GCAM_region_ID, year, CEDS_agg_sector, CEDS_agg_fuel) %>%
        summarise(totalenergy = sum(energy)) %>%
        ungroup() ->
        L112.in_EJ_R_en_S_F_Yh_calib_CEDStotals

      # Compute the share of energy in GCAM sector by CEDS sector
      L112.in_EJ_R_en_S_F_Yh_calib_newbase %>%
        left_join_error_no_match(L112.in_EJ_R_en_S_F_Yh_calib_CEDStotals,
                                 by = c("GCAM_region_ID", "year", "CEDS_agg_sector", "CEDS_agg_fuel")) %>%
        mutate(enshare = energy/totalenergy) ->
        L112.in_EJ_R_en_S_F_Yh_calib_enshare

      # Attach CEDS emissions to those sector fuel combos
      #kbn 2019/11/11 changed nomenclature below
      L112.in_EJ_R_en_S_F_Yh_calib_enshare %>%
        left_join(L112.CEDS_GCAM_emissions,
                  by = c("GCAM_region_ID", "year", "CEDS_agg_sector", "CEDS_agg_fuel")) ->
        L112.CEDSGCAM_emissions

      L112.CEDSGCAM_emissions %>%
        mutate(GCAMemissions = emissions * enshare) ->
        L112.CEDSGCAM_computedemissions



      #
      L112.CEDSGCAM_computedemissions -> L112.CEDSGCAM_computedemissions_complete

      L112.CEDSGCAM_computedemissions_complete %>%
        select(GCAM_region_ID, Non.CO2, year, supplysector, subsector, stub.technology, GCAMemissions) %>%
        rename(emissions = GCAMemissions) ->
        L112.nonco2_tg_R_en_S_F_Yh

      # REMAINING ISSUES:
      # TRN_OTHER SECTOR HAS NO MATCH. THIS IS CURRENTLY DROPPED--THIS IS LIKELY CORRECTLY DROPPED BUT COULD ASSIGNED SOMEWHERE.
      # TOTAL EMISSIONS BEFORE AND AFTER OF NON-PROCESS ENERGY SECTORS IS .4 LARGER IN INITIAL DATASET

      # GENERATE COMBUSTION EMISSIONS FACTORS BY MULTIPLYING EMISSIONS BY DRIVER


      # ========================================================================

      #--------------------------------------------------------------------------------------
      # Now join emissions and energy data together to calculate emissions factors
      L112.nonco2_tg_R_en_S_F_Yh %>%
        left_join_error_no_match(L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy,
                                 by = c("GCAM_region_ID", "supplysector", "subsector", "stub.technology", "year")) %>%
        mutate(emfact = emissions / energy) %>%
        select(GCAM_region_ID, Non.CO2, year, supplysector, subsector, stub.technology, emfact) ->
        #Replaces NAs with zeroes (places where zero emissions and energy lead to 0/0 = NaN)
        L112.nonco2_tgej_R_en_S_F_Yh_withNAs

      # Generates global median emissions factors
      L112.nonco2_tgej_R_en_S_F_Yh_withNAs %>%
        replace_na(list(emfact = 0)) %>%
        group_by(year, Non.CO2, supplysector, subsector, stub.technology) %>%
        summarise(emfact = median(emfact)) %>%
        ungroup() %>%
        rename(globalemfact = emfact) ->
        L112.nonco2_tgej_R_en_S_F_Yh_globalmedian

      # Replaces all emissions factors above a given value (currently 1000) or that are NAs with the global median emissions factor for that year, non.CO2, and technology
      L112.nonco2_tgej_R_en_S_F_Yh_withNAs %>%
        left_join_error_no_match(L112.nonco2_tgej_R_en_S_F_Yh_globalmedian, by = c("year", "Non.CO2", "supplysector", "subsector", "stub.technology")) %>%
        mutate(emfact = if_else(emfact > 1000 | is.na(emfact), globalemfact, emfact)) %>%
        select(GCAM_region_ID, Non.CO2, year, supplysector, subsector, stub.technology, emfact) %>%
        # This line is only for testing and needs to be replaced with the solution for below
        mutate(emfact = if_else(is.infinite(emfact), 1, emfact)) ->
        L112.nonco2_tgej_R_en_S_F_Yh
      # This largely works but need to figure out what's going on with trucks 1-6t in region 19.
      # Also need to set up rule to determine cutoff point for emissions factor replacement


      # =======================
      # Process Emissions
      # =======================

      # Calculate process emissions drivers
      # -----------------------------------


      # Subset CEDS process emissions and match to GCAM drivers

      L112.CEDS_GCAM %>%
        filter(CEDS_agg_sector %in% c("industry_processes", "chemicals", "landfills", "wastewater", "aerosols",
                                      "metals", "foams", "solvents", "semiconductors")) ->
        L112.CEDS_GCAM_Proc


      # Fourth: Map in all data and compute emissions (EDGAR_emissions * tech_share).

      # THIS IS THE NEW CODE TO TAKE THE SEPARATE NITRIC AND ADIPIC SECTORS AND BREAK THEM OUT AUTOMATICALLY.

      GCAM_sector_tech %>%
        select(supplysector, subsector, stub.technology, EDGAR_agg_sector, EPA_agg_sector, EPA_agg_fuel_ghg) %>%
        filter(EDGAR_agg_sector %in% c("industry_processes" , "chemicals", "landfills", "wastewater",  # Filter for the agg sectors in EDGAR for all NonCO2s.
                                        "solvents")) %>%
        repeat_add_columns(tibble(Non.CO2 = unique(L112.CEDS_GCAM_Proc$Non.CO2))) %>%
        group_by(supplysector, subsector, stub.technology, Non.CO2) %>%
        # left_join(L131.nonco2_pct_R_prc_S_S_2005,  # Then combine with the EPA sector information
        #           by = c("supplysector", "subsector", "stub.technology",
        #                  "Non.CO2", "EPA_agg_sector", "EDGAR_agg_sector", "EPA_agg_fuel_ghg")) %>%
        repeat_add_columns(tibble(year = emissions.CEDS_YEARS)) %>%
        repeat_add_columns(tibble(GCAM_region_ID = GCAM_region_names$GCAM_region_ID)) %>%
        group_by(GCAM_region_ID, EDGAR_agg_sector, Non.CO2, year) %>%
        left_join_error_no_match(L112.CEDS_GCAM_Proc, by = c("GCAM_region_ID", "EDGAR_agg_sector" = "CEDS_agg_sector", "Non.CO2", "year")) %>%
        na.omit() %>%  # delete rows with NA's
        # Have to figure out way to carry through and share out nitric and adipic acid here -- will now be coming from CEDS sector
        mutate(input.emissions = emissions) %>%  # Calculate emissions
        # select(-sector_emissions, -tech_emissions.x) %>%
        group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year) %>%
        summarise(value = sum(input.emissions)) %>% # Calculate total emissions
        ungroup() %>%
        replace_na(list(value = 0)) ->
        L131.nonco2_tg_R_prc_S_S_Yh



      # Final sector outputs
      # "adipic acid" "HCFC_22_Prod" "nitric acid" "other industrial processes" "solvents"
      # "landfills" "wastewater treatment"

      # ===================================================
      # Animal Emissions
      # ===================================================
      # Computing unscaled emissions by country and technology
      # using animal production from L107.an_Prod_Mt_R_C_SYS_Fd_Y
      # and EPA emissions factos.

      L112.CEDS_GCAM %>%
        filter(CEDS_agg_sector == "Animals") %>%
        mutate(Non.CO2 = paste(Non.CO2,"_AGR",sep="")) ->
        L112.CEDS_GCAM_An

      L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
        rename(production = value) %>%
        left_join(GCAM_sector_tech, by = c("GCAM_commodity" = "sector", "system" = "fuel", "feed" = "technology")) %>%
        select(GCAM_region_ID, GCAM_commodity, system, feed, year, production, EPA_agg_sector, EDGAR_agg_sector) %>%
        repeat_add_columns(tibble::tibble(Non.CO2 = unique(L112.CEDS_GCAM_An$Non.CO2))) %>%  # Add Gas Name and AGR for agriculture
        # match in emissions factors, using left_join and dropping fuel column
        # MAYBE USE EMISSIONS FACTOR FOR N20 FROM CEDS??? WOULD HAVE TO READ THIS IN AS AN INPUT ABOVE. WOULD NEED SECTOR DEFINITION IMPROVED IN CEDS FOR THAT TO BE USABLE
        left_join(L103.ghg_tgmt_USA_an_Sepa_F_2005, by = c("EPA_agg_sector" = "sector")) %>%
        mutate(epa_emissions = production * ch4_em_factor) %>%  # compute unscaled emissions
        select(-fuel) %>%
        na.omit() %>%
        rename(CEDS_agg_sector = EDGAR_agg_sector) ->
        L113.ghg_tg_R_an_C_Sys_Fd_Yh.mlt

      # Aggregate by sector and region
      L113.ghg_tg_R_an_C_Sys_Fd_Yh.mlt %>%
        group_by(GCAM_region_ID, Non.CO2, CEDS_agg_sector, year) %>%
        summarize(EPA_emissions = sum(epa_emissions)) %>%
        ungroup()  ->
        L113.ghg_tg_R_an_C_Yh.mlt

      # Scale EPA emissions by tech to match CEDS totals

      # First compute scalers
      L113.ghg_tg_R_an_C_Yh.mlt %>%
        left_join(L112.CEDS_GCAM_An, by = c("year", "GCAM_region_ID", "Non.CO2", "CEDS_agg_sector")) %>%
        rename(CEDS_emissions = emissions) %>%
        mutate(scalar = CEDS_emissions / EPA_emissions) ->
        L113.emiss_scalar

      # Second scale EPA emissions
      L113.ghg_tg_R_an_C_Sys_Fd_Yh.mlt %>%
        left_join(L113.emiss_scalar, by = c("GCAM_region_ID", "Non.CO2", "CEDS_agg_sector", "year")) %>%
        mutate(emissions = epa_emissions * scalar) %>%
        select(-EPA_emissions, -CEDS_emissions) %>%
        filter(year %in% emissions.EDGAR_YEARS) %>%
        replace_na(list(emissions = 0)) %>%
        select(GCAM_region_ID, Non.CO2, supplysector = GCAM_commodity, subsector = system, stub.technology = feed,
               value = emissions, year) ->
        L113.ghg_tg_R_an_C_Sys_Fd_Yh_full

      # ==============================
      # Agricultural Emissions
      # ==============================

      # Compute shares of regional cropland allocation by crop type, and regional production of each crop by each GLU
      # In the downscaling from (geopolitical) region to crop and GLU, we use land area to go from region to region/crop, and
      # production to go from region/crop to region/GLU/crop
      # ----------------------------------------------------

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
      L122.ProdGLUshare_R_C_Y_GLU <- L101.ag_Prod_Mt_R_C_Y_GLU %>%
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

      # Match emissions to drivers
      # --------------------------

      # Filter out agricultural sectors
      L112.CEDS_GCAM %>%
        filter(CEDS_agg_sector %in% emissions.AGR_SECTORS) %>%
        mutate(Non.CO2 = paste(Non.CO2,"_AGR",sep="")) ->
        L112.CEDS_GCAM_agr

      # Compute emissions from rice by GCAM region, commodity, and GLU
      L112.CEDS_GCAM_agr %>%
        filter(CEDS_agg_sector == "rice") ->
        L112.CEDS_GCAM_rice

      # Compute share of rice production by GLU in each region / year
      L101.ag_Prod_Mt_R_C_Y_GLU %>%
        filter(GCAM_commodity == "Rice", year %in% emissions.CEDS_YEARS) %>%
        group_by(GCAM_region_ID, GCAM_commodity, year) %>%
        # Total production by region, commodity, and year for calculating share
        mutate(total_prod = sum(value)) %>%
        ungroup() %>%
        transmute(GCAM_region_ID, GCAM_commodity, GLU, year, prod_share = value / total_prod) ->
        L122.ag_Prod_Mt_R_rice_Y_GLU

      # Multiply total emissions by production share
      L122.ag_Prod_Mt_R_rice_Y_GLU %>%
        repeat_add_columns(tibble(Non.CO2 = unique(L112.CEDS_GCAM_rice$Non.CO2))) %>%
        left_join_error_no_match(L112.CEDS_GCAM_rice, by = c("GCAM_region_ID", "Non.CO2", "year")) %>%
        transmute(GCAM_region_ID, GCAM_commodity, year, GLU, Non.CO2,
                  emissions = emissions * prod_share, type = "Rice") ->
        L122.ghg_tg_R_rice_Y_GLU

      # Compute emissions from soils by GCAM region, commodity, and GLU
      L112.CEDS_GCAM_agr %>%
        filter(CEDS_agg_sector == "soil") ->
        L112.CEDS_GCAM_soil

      # Multiply total emissions by production share
      L122.EmissShare_R_C_Y_GLU %>%
        filter(year %in% emissions.CEDS_YEARS) %>%
        repeat_add_columns(tibble(Non.CO2 = unique(L112.CEDS_GCAM_soil$Non.CO2))) %>%
        left_join_error_no_match(L112.CEDS_GCAM_soil, by = c("GCAM_region_ID", "year", "Non.CO2")) %>%
        transmute(GCAM_region_ID, GCAM_commodity, year, GLU, Non.CO2,
                  emissions = emissions * emiss_share, type = "Soil") ->
        L122.ghgsoil_tg_R_C_Y_GLU



      # Bind together dataframes & aggregate
      L122.ghg_tg_R_agr_C_Y_GLU_full <- bind_rows( L122.ghg_tg_R_rice_Y_GLU, L122.ghgsoil_tg_R_C_Y_GLU#, L122.ghgfert_tg_R_C_Y_GLU
      ) %>%
        group_by(GCAM_region_ID, GCAM_commodity, year, GLU, Non.CO2) %>%
        summarise(value = sum(emissions)) %>%
        ungroup()


      # ===================================================
      # AGRICULTURAL WASTE BURNING
      # ===================================================

      # Select agricultural waste burning emissions

      L112.CEDS_GCAM %>%
        filter(CEDS_agg_sector == "ag_waste_burning") %>%
        mutate(Non.CO2 = paste(Non.CO2,"_AWB",sep="")) ->
        L112.CEDS_GCAM_awb

      # Calculate AWB Drivers
      # ---------------------

      # Agricultural waste burning emissions by region are assigned to crops and land use regions on the basis of excess dry biomass...
      # estimated from production, harvest index, and water content

      # Match weighted average residue biomass parameters with crop prodcution.
      L101.ag_Prod_Mt_R_C_Y_GLU %>%
        left_join(L111.ag_resbio_R_C, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
        select(-c(ErosCtrl_tHa, ResEnergy_GJt, Root_Shoot)) ->
        L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU

      # Set the default harvest index of 1 and water content to 0.15 for fiber and fodder crops, in order to use
      # harvest index of 1 and water content to caculate burnable excess biomass in next step.
      L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU %>%
        replace_na(list(HarvestIndex = 1, WaterContent = 0.15)) ->
        L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_replaced

      # Burnable excess biomass is equal to ((biomass production / HarvestIndex) - biomass production) * (1 - WaterContent)
      # For root crops, the calculation could be done differently, if the root mass is excluded from the denominator of the reported harvest index.
      # This doesn't seem to be the case in the literature--while for other crops, root mass is excluded from the harvest index, it is included for potatoes.
      # If excluded, then the harvest index could be greater than 1 (if the tubers weigh more than the above-ground shoots), and the above calculation would
      # return a negative number. None of the crops in the underlying harvested index database have values greater than 1 so this isn't currently an issue.
      L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_replaced %>%
        mutate(burnable = ((value / HarvestIndex) - value) * (1 - WaterContent)) ->
        L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_burn

      # Aggregate the burnable excess biomass by GCAM region and year.
      L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_burn %>%
        group_by(GCAM_region_ID, year) %>%
        summarise(value = sum(burnable)) %>%
        ungroup() ->
        L112.ag_ExcessDryBiomass_Mt_R_Y

      # Calculate the share by production technology of each region's burnable excess biomass (AWB_emiss_share).
      # This will be used to create the ag waste burning share of emissions, for downscaling regional emissions to region/GLU/crop
      L112.ag_ExcessDryBiomass_Mt_R_Y %>%
        rename(total_excess_bio = value) %>%
        left_join(L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_burn, by = c("GCAM_region_ID", "year")) %>%
        mutate(AWB_emiss_share = burnable / total_excess_bio) %>%
        select(GCAM_region_ID, GCAM_commodity, year, GLU, AWB_emiss_share) ->
        L112.AWBshare_R_C_GLU

      # Calculate AWB Emissions
      # -----------------------


      # Compute agricultural waste burning emissions by GCAM region, commodity, and GLU

      # Add gas name to AWB production shares to prepare for matching with emissions
      L112.AWBshare_R_C_GLU %>%
        repeat_add_columns(tibble::tibble(`Non.CO2` = unique(L112.CEDS_GCAM_awb$Non.CO2))) ->
        L112.nonco2_tg_R_awb_C_Y_GLU

      # Estimate ag waste burning emissions using the estimated share (fraction) times total regional AWB emissions
      # Emissions(R, GLU, crop) =  regional total  * AWB share
      L112.nonco2_tg_R_awb_C_Y_GLU %>%
        left_join(L112.CEDS_GCAM_awb, by = c("GCAM_region_ID", "year", "Non.CO2")) %>%
        rename(total_emiss = emissions) %>%
        mutate(emissions = total_emiss * AWB_emiss_share) ->
        L112.nonco2_tg_R_awb_C_Y_GLU_total

      # Subset only the historical years in CEDS, and reshape for write-out
      L112.nonco2_tg_R_awb_C_Y_GLU_total %>%
        filter(year %in% emissions.CEDS_YEARS) %>%
        select(GCAM_region_ID, Non.CO2, GCAM_commodity, GLU, year, value = emissions) ->
        L112.nonco2_tg_R_awb_C_Y_GLU

      # Calculates BC/OC emissions factors for a separate data frame output. bc-oc emissions / ag residue
      L112.nonco2_tg_R_awb_C_Y_GLU %>%
        filter(Non.CO2 %in% c("BC_AWB", "OC_AWB")) %>%
        rename(awb_emission = value) %>%
        # NEED TO REEXAMINE THE DRIVER HERE. IT LIKELY ACTUALLY NEEDS TO BE EXCESS BURNABLE DRY BIOMASS, POSSIBLY L112.ag_ExcessDryBiomass_Mt_R_Y
        left_join_error_no_match(L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_burn, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
        # Calculate emission factor, which is
        mutate(emfact = awb_emission / burnable) %>%
        select(GCAM_region_ID, Non.CO2, GCAM_commodity, GLU, year, emfact) %>%
        # Replace NaNs with zeros
        mutate_all(funs(replace(., is.na(.), 0))) ->
        L112.bcoc_tgej_R_awb_C_Y_GLU
      # END AGRICULTURAL WASTE BURNING
      # ==============================


      # UNMANAGED LAND EMISSIONS
      # ========================

      # Select agricultural waste burning emissions

      L112.CEDS_GCAM %>%
        filter(CEDS_agg_sector %in% c("forest", "grassland", "deforest")) %>%
        select(-CEDS_agg_fuel) %>%
        rename(sector = CEDS_agg_sector) ->
        L112.CEDS_GCAM_unmgd

      # Part 1: Grassland burning
      # Downscale regional grassland burning emissions to GLU based on the share of land in each GLU
      L124.LC_bm2_R_Grass_Yh_GLU_adj %>%
        group_by(GCAM_region_ID, year) %>%
        mutate(land_share = value / sum(value)) %>%                                                           # Compute the share of regional grassland in each GLU
        select(-value) %>%
        # There are regions (e.g., region #3) where we have grassland area, but no emissions. Use inner join to remove
        inner_join(filter(L112.CEDS_GCAM_unmgd, sector == "grassland"), by = c("GCAM_region_ID", "year")) %>%         # Map in EDGAR grassland emissions
        mutate(emissions = emissions * land_share) %>%                                                                # Compute emissions by GLU using EDGAR totals and land shares
        ungroup() %>%
        select(-sector, -land_share) ->
        L124.nonco2_tg_R_grass_Y_GLU_full

      # Part 2: Forest fires and deforestation
      # Calculate share of forest emissions from forest fires versus deforestation using GFED data.
      # Bind all GFED data together, aggregate by GCAM region/gas/year, calculate share of forest fire versus deforestation
      # Note the odd spaces after some of the mutates below is to avoid tripping test for consecutive mutates
      L112.CEDS_GCAM_unmgd %>%
        filter(sector == "forest") %>%
        rename(forestfire = emissions) %>%
        left_join(L112.CEDS_GCAM_unmgd %>% filter(sector == "deforest") %>% rename(deforest = emissions),
                                 by = c("GCAM_region_ID", "Non.CO2", "year")) %>%
        mutate(deforest=if_else(is.na(deforest),0,deforest)) %>%
        select(GCAM_region_ID, Non.CO2, year, forestfire, deforest) %>%
        mutate(PctForestFire = forestfire / (forestfire + deforest)) %>%                               # Compute share of emissions from forest fires
        # There are regions where GFED data is zero for both forest fires and deforestation, leading to NAs
        # Assume those missing values are places with 100% forest fires since these are easier to model in GCAM
        replace_na(list(PctForestFire = 1)) %>%
        mutate(PctDeforest = 1 - PctForestFire) %>%                                                    # Compute share of emissions from deforestation
        select(-forestfire, -deforest) ->
        FireShares_R_G_Y

      # Downscale regional forest burning emissions to GLU based on the share of land in each GLU
      # Use GFED to separate into forest fires and deforestation, which have different drivers in GCAM
      L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
        group_by(GCAM_region_ID, year) %>%
        mutate(land_share = value / sum(value)) %>%                                                      # Compute share of regional forest area in each GLU
        na.omit() %>%
        select(-value) %>%
        # There are places with land area but no emissions and vice versa. Use an inner_join to only get places with both.
        # Note: this means that some regions get zero emissions coefficients in the historic period (future deforestation emissions coefs are defined below)
        inner_join(filter(L112.CEDS_GCAM_unmgd, sector == "forest"), by = c("GCAM_region_ID", "year")) %>%       # Map in EDGAR emissions information
        mutate(emissions = emissions * land_share) %>%                                                           # Compute forest emissions from EDGAR totals and land shares
        select(-sector, -land_share) %>%
        left_join(FireShares_R_G_Y, by = c("GCAM_region_ID", "Non.CO2", "year")) %>%                     # Map in GFED fire shares
        # Assume missing values mean 100% forest fires since these are easier to model in GCAM
        replace_na(list(PctForestFire = 1)) %>%
        replace_na(list(PctDeforest = 0)) %>%
        mutate(ForestFire = emissions * PctForestFire,
               Deforest = emissions * PctDeforest) %>%                                                       # Compute deforestation emissions
        ungroup() %>%
        select(-emissions, -PctForestFire, -PctDeforest) %>%
        gather(technology, value, -GCAM_region_ID, -GLU, -Land_Type, -Non.CO2, -year) ->
        L124.nonco2_tg_R_forest_Y_GLU_full

      # Compute global average deforestation emissions coefficients
      # These coefficients are used for future model time periods.
      # Compute total change in forest area from 2000 to 2005, total global emissions, and average annualized coefficients (emissions / change in land area / number of years)
      L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
        filter(year %in% emissions.DEFOREST_COEF_YEARS) %>%                                             # Get years that we'll use for deforestation calculation (as of 5/14/17 this was 2000 & 2005)
        mutate(year = if_else(year == min(emissions.DEFOREST_COEF_YEARS), "year1", "year2")) %>%        # Rename years so we can use them as column headings (this also makes this robust to changes in years later)
        spread(year, value) %>%                                                                         # Spread so years are separate columns
        mutate(driver = (year1 - year2) / (emissions.DEFOREST_COEF_YEARS[2] - emissions.DEFOREST_COEF_YEARS[1]),    # Compute average annual deforestation rates (change in forest area / number of years)
               driver = if_else(driver < 0, 0, driver)) %>%                                             # Deforestation emissions only happen if forest area decreases
        repeat_add_columns(tibble(Non.CO2 = unique(L124.nonco2_tg_R_forest_Y_GLU_full$Non.CO2))) %>%                                                                # Add in rows for all required emissions
        left_join(filter(L124.nonco2_tg_R_forest_Y_GLU_full,                                                 # Map in EDGAR deforestation emissions for the final deforestation year (as of 5/14/17 this was 2005)
                         year == emissions.DEFOREST_COEF_YEARS[2],
                         technology == "Deforest"), by = c("GCAM_region_ID", "Land_Type", "GLU", "Non.CO2")) %>%
        replace_na(list(value = 0)) %>%                                                                 # Note: "value" are the emissions calculated above
        mutate(value = if_else(driver == 0, 0, value)) %>%                                              # Zero out emissions in places where there wasn't any deforestation
        group_by(Land_Type, technology, Non.CO2) %>%
        summarize(driver = sum(driver), emissions = sum(value)) %>%                                     # Calculate global total emissions and deforestation
        mutate(emiss.coef = emissions / driver) %>%                                                     # Calculate average annual deforestation emissions coefficients
        ungroup() %>%
        na.omit() ->
        L124.deforest_coefs_full



      # CLEANING UP FINAL OUTPUTS TO MATCH OLD DATA SYSTEM LEVEL 1 OUTPUTS
      # ==================================================================

      # Because of the diverse drivers and data sources, old level 1 outputs from EDGAR were separated into multiple data frames and sources.
      # Some of these are preserved but many are different. This code slices the prepared CEDS emissions data to match the preexisting level 1 data
      # outputs to allow level two code to continue working unchanged.

      L112.nonco2_tg_R_en_S_F_Yh %>%
        filter(Non.CO2 %in% c("CH4", "N2O")) %>%
        select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emissions) ->
        L112.ghg_tg_R_en_S_F_Yh

      L112.nonco2_tg_R_en_S_F_Yh %>%
        filter(!(Non.CO2 %in% c("CH4", "N2O", "BC", "OC"))) %>%
        select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emissions) ->
        L111.nonghg_tg_R_en_S_F_Yh

      L112.nonco2_tgej_R_en_S_F_Yh %>%
        filter(Non.CO2 %in% c("CH4", "N2O")) %>%
        select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emfact) ->
        L112.ghg_tgej_R_en_S_F_Yh


      L112.nonco2_tgej_R_en_S_F_Yh %>%
        filter(!(Non.CO2 %in% c("CH4", "N2O", "BC", "OC"))) %>%
        select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emfact) ->
        L111.nonghg_tgej_R_en_S_F_Yh

      L112.nonco2_tgej_R_en_S_F_Yh %>%
        filter(Non.CO2 %in% c("BC", "OC")) %>%
        filter(year == 2000) %>%
        select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emfact) ->
        L114.bcoc_tgej_R_en_S_F_2000

      # Animal NH3 emissions
      L113.ghg_tg_R_an_C_Sys_Fd_Yh_full %>%
        filter(Non.CO2 == "NH3_AGR") -> L115.nh3_tg_R_an_C_Sys_Fd_Yh

      # Animal all other emissions
      L113.ghg_tg_R_an_C_Sys_Fd_Yh_full %>%
        filter(Non.CO2 != "NH3_AGR") -> L113.ghg_tg_R_an_C_Sys_Fd_Yh

      # Ag Waste Burning
      L112.AWBshare_R_C_GLU ->
        L121.AWBshare_R_C_GLU

      L112.nonco2_tg_R_awb_C_Y_GLU %>%
        filter(!(Non.CO2 %in% c("BC_AWB", "OC_AWB"))) ->
        L121.nonco2_tg_R_awb_C_Y_GLU

      L122.EmissShare_R_C_Y_GLU -> L122.EmissShare_R_C_Y_GLU

      # AGR emissions: Filter for gases present in original data set
      L122.ghg_tg_R_agr_C_Y_GLU_full -> # %>%
        # filter(Non.CO2 %in% c("NH3_AGR", "N2O_AGR", "NOx_AGR", "CH4_AGR")) ->
        L122.ghg_tg_R_agr_C_Y_GLU


      # BCOC emissions from Ag Waste Burning
      L112.bcoc_tgej_R_awb_C_Y_GLU %>%
        filter(year == 2000) %>%
        select(-year) ->
        L123.bcoc_tgmt_R_awb_2000

      L124.deforest_coefs_full %>%
        filter(!(Non.CO2 %in% c("BC", "OC"))) ->
        L124.deforest_coefs

      # Filter out BC/OC for its own output
      L124.nonco2_tg_R_grass_Y_GLU_full %>%
        rename(value = emissions) %>%
        filter(!(Non.CO2 %in% c("BC", "OC"))) ->
        L124.nonco2_tg_R_grass_Y_GLU

      #Filter out BC/OC for its own output
      L124.nonco2_tg_R_forest_Y_GLU_full %>%
        filter(!(Non.CO2 %in% c("BC", "OC"))) ->
        L124.nonco2_tg_R_forest_Y_GLU

      # Filter out BC/OC for its own output
      L124.nonco2_tg_R_grass_Y_GLU_full %>%
        rename(value = emissions) %>%
        filter(Non.CO2 %in% c("BC", "OC")) %>%
        filter(year == 2000) ->
        L124.bcoc_tg_R_grass_Y_GLU

      # IF YOU TURN THIS BACK ON YOULL NEED TO GENERATE THE EMISSIONS FACTORS HERE BY READING IN THE DRIVER
      # L124.bcoc_tg_R_grass_Y_GLU %>%
      #   L125.bcoc_tgbkm2_R_grass_2000

      #Filter out BC/OC for its own output
      L124.nonco2_tg_R_forest_Y_GLU_full %>%
        filter(Non.CO2 %in% c("BC", "OC")) %>%
        filter(year == 2000) ->
        L125.bcoc_tgbkm2_R_forest_2000

      # Filter out BC OC for its own output
      L124.deforest_coefs_full %>%
        filter(Non.CO2 %in% c("BC", "OC")) ->
        L125.deforest_coefs_bcoc



      # Full outputs list - DONE means nothing. these are all successfully being read as outputs and in the xml, they may not match totals
      # DONE"L111.nonghg_tg_R_en_S_F_Yh",
      # DONE"L111.nonghg_tgej_R_en_S_F_Yh",
      # DONE"L112.ceds_ghg_tg_R_en_S_F_Yh",
      # DONE"L112.ceds_ghg_tgej_R_en_S_F_Yh",
      # DONE "L113.ghg_tg_R_an_C_Sys_Fd_Yh"
      # ISSUE: Uses EPA 2005 data to set baseline emissions factors that are then scaled to CEDS totals
      # "L114.bcoc_tgej_R_en_S_F_2000",
      # DONE "L115.nh3_tg_R_an_C_Sys_Fd_Yh", check match
      # DONE "L121.nonco2_tg_R_awb_C_Y_GLU",
      # L121.AWBshare_R_C_Y_GLU
      # DONE "L122.ghg_tg_R_agr_C_Y_GLU",
      # DONE "L122.EmissShare_R_C_Y_GLU",
      # DONE "L123.bcoc_tgmt_R_awb_2000", this is matching now
      # DONE "L124.nonco2_tg_R_grass_Y_GLU",
      # DONE "L124.nonco2_tg_R_forest_Y_GLU",
      # DONE "L124.deforest_coefs"
      # DONE "L125.bcoc_tgbkm2_R_grass_2000",
      # DONE "L125.bcoc_tgbkm2_R_forest_2000",
      # DONE "L125.deforest_coefs_bcoc",
      # DONE "L131.nonco2_tg_R_prc_S_S_Yh"
      # ISSUE: Can we remove downscaling by EPA 2005 data. Nitric and Adipic acid split, but other industry processes as well

      # NOTE: Chunk producing max reduction to reach min coefficient set by L151. Need to check output of that when finished: "L151.nonghg_ctrl_R_en_S_T",



      # ===============
      # Produce outputs
      L111.nonghg_tg_R_en_S_F_Yh %>%
        add_title("Non-ghg emission totals by GCAM sector, fuel, technology, and driver type for CEDS historical years.") %>%
        add_units("Tg") %>%
        add_comments("Compute unscaled non-ghg emissions by country and technology, and CEDS emissions by region and sector.") %>%
        add_comments("Then, scale EPA emissions by tech to match EDGAR totals, compute international shipping and international aviation emissions, ") %>%
        add_comments("and finally calculate non-ghg emission totals by GCAM sector, fuel, technology, and driver type for EDGAR historical years.") %>%
        add_legacy_name("L111.nonghg_tg_R_en_S_F_Yh") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","energy/mappings/UCD_techs","energy/calibrated_techs","energy/calibrated_techs_bld_det",
                       "emissions/mappings/Trn_subsector","emissions/CEDS/CEDS_sector_tech","emissions/mappings/Trn_subsector_revised",
                       "emissions/mappings/GCAM_sector_tech_CEDS","emissions/mappings/calibrated_outresources","emissions/mappings/GCAM_sector_tech_CEDS_revised",
                       "L101.in_EJ_R_en_Si_F_Yh","emissions/CEDS/CEDS_sector_tech_revised","emissions/mappings/UCD_techs_emissions_revised") ->
        L111.nonghg_tg_R_en_S_F_Yh

      L111.nonghg_tgej_R_en_S_F_Yh %>%
        add_title("Non-ghg emission total shares by GCAM sector, fuel, technology, and driver type for CEDS historical years.") %>%
        add_units("Tg/EJ") %>%
        add_comments("Use non-ghg emission totals by GCAM sector, fuel, technology, and driver type for CEDS historical years to derive emission shares.") %>%
        add_legacy_name("L111.nonghg_tgej_R_en_S_F_Yh") %>%
        same_precursors_as("L111.nonghg_tg_R_en_S_F_Yh") ->
        L111.nonghg_tgej_R_en_S_F_Yh

      L112.ghg_tg_R_en_S_F_Yh %>%
        add_title("GHG emissions by energy sector, gas, region, and historical year") %>%
        add_units("Tg") %>%
        add_comments("Emissions calculated with CEDS totals scaled to EPA 2019 totals") %>%
        add_legacy_name("L112.ghg_tg_R_en_S_F_Yh") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","energy/mappings/UCD_techs","energy/calibrated_techs","energy/calibrated_techs_bld_det",
                       "emissions/mappings/Trn_subsector","emissions/CEDS/CEDS_sector_tech","emissions/mappings/calibrated_outresources",
                       "L101.in_EJ_R_en_Si_F_Yh", "emissions/EPA/EPA_2019_raw", "emissions/EPA_CH4N2O_map","emissions/mappings/Trn_subsector_revised",
                       "emissions/EPA_country_map","emissions/CEDS/CEDS_sector_tech_revised","emissions/mappings/UCD_techs_emissions_revised") ->
        L112.ghg_tg_R_en_S_F_Yh

      L112.ghg_tgej_R_en_S_F_Yh %>%
        add_title("GHG emissions factors by energy sector, gas, region, and historical year") %>%
        add_units("Tg/EJ") %>%
        add_comments("Emissions calculated with EPA emissions factors and scaled to EDGAR totals") %>%
        add_comments("Then, emissions factors computed by dividing calculated emissions by energy data") %>%
        add_legacy_name("L112.ghg_tgej_R_en_S_F_Yh") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","energy/mappings/UCD_techs","energy/calibrated_techs","energy/calibrated_techs_bld_det",
                       "emissions/mappings/Trn_subsector","emissions/CEDS/CEDS_sector_tech","emissions/mappings/calibrated_outresources","emissions/mappings/Trn_subsector_revised",
                       "L101.in_EJ_R_en_Si_F_Yh", "emissions/EPA/EPA_2019_raw", "emissions/EPA_CH4N2O_map",
                        "energy/rsrc_fol_prod_vintage","emissions/EPA_country_map","emissions/CEDS/CEDS_sector_tech_revised","emissions/mappings/UCD_techs_emissions_revised") ->
        L112.ghg_tgej_R_en_S_F_Yh

      L113.ghg_tg_R_an_C_Sys_Fd_Yh %>%
        add_title("Animal GHG emissions (CH4 and N2O) by GCAM region / sector / technology / historical year") %>%
        add_units("Tg") %>%
        add_comments("First: compute unscaled emissions by country and technology") %>%
        add_comments("Second: match in emissions factors from EPA") %>%
        add_comments("Third: compute unscaled emissions (production * emfactors) and aggregate by sector and region") %>%
        add_comments("Fourth: compute EDGAR emissions by region and sector") %>%
        add_comments("Fifth: scale EPA emissions by tech to match EDGAR") %>%
        add_legacy_name("L113.ghg_tg_R_an_C_Sys_Fd_Yh") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech", "emissions/CEDS/CEDS_sector_tech_revised",
                       "emissions/mappings/GCAM_sector_tech_CEDS", "L107.an_Prod_Mt_R_C_Sys_Fd_Y","emissions/mappings/GCAM_sector_tech_CEDS_revised",
                       "L103.ghg_tgmt_USA_an_Sepa_F_2005") ->
        L113.ghg_tg_R_an_C_Sys_Fd_Yh


      L115.nh3_tg_R_an_C_Sys_Fd_Yh %>%
        add_title(" Animal NH3 emissions by GCAM region / sector / technology / historical year") %>%
        add_units("Tg") %>%
        add_comments("Annual animal NH3 emissions is computed using CEDS emissions and FAO animal production.") %>%
        add_legacy_name("L115.nh3_tg_R_an_C_Sys_Fd_Yh") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech","emissions/CEDS/CEDS_sector_tech_revised",
                       "L107.an_Prod_Mt_R_C_Sys_Fd_Y","L107.an_Prod_Mt_R_C_Sys_Fd_Y") ->
        L115.nh3_tg_R_an_C_Sys_Fd_Yh

      L121.AWBshare_R_C_GLU %>%
        add_title("Ag waste burning share of emissions by GCAM region / commodity / GLU / historical year") %>%
        add_units("unitless share") %>%
        add_comments("Agricultural waste burning emissions by region are assigned to crops and land use regions on the basis of excess dry biomass...") %>%
        add_comments("estimated from production, harvest index, and water content") %>%
        add_legacy_name("L121.AWBshare_R_C_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech","emissions/CEDS/CEDS_sector_tech_revised",
                       "L101.ag_Prod_Mt_R_C_Y_GLU", "L111.ag_resbio_R_C") ->
        L121.AWBshare_R_C_Y_GLU

      L121.nonco2_tg_R_awb_C_Y_GLU %>%
        add_title("Ag waste burning emissions by GCAM region / commodity / GLU / historical year") %>%
        add_units("Unit = Tg") %>%
        add_comments("Agricultural waste burning emissions by region are assigned to crops and land use regions on the basis of excess dry biomass...") %>%
        add_comments("estimated from production, harvest index, and water content") %>%
        add_legacy_name("L121.nonco2_tg_R_awb_C_Y_GLU") %>%
        same_precursors_as("L121.AWBshare_R_C_Y_GLU") ->
        L121.nonco2_tg_R_awb_C_Y_GLU

      L122.EmissShare_R_C_Y_GLU %>%
        add_title("Agriculture emissions shares by GCAM region, commodity, GLU, and historical year") %>%
        add_units("unitless share") %>%
        add_comments("Multiply region/crop area shares by region/crop/GLU production shares") %>%
        add_legacy_name("L122.EmissShare_R_C_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech","emissions/CEDS/CEDS_sector_tech_revised",
                       "L101.ag_Prod_Mt_R_C_Y_GLU","L111.ag_resbio_R_C","L122.LC_bm2_R_HarvCropLand_C_Yh_GLU") ->
        L122.EmissShare_R_C_Y_GLU

      L122.ghg_tg_R_agr_C_Y_GLU %>%
        add_title("Agriculture emissions by GCAM region, commodity, GLU, and historical year") %>%
        add_units("Tg") %>%
        add_comments("EDGAR emissions shared out by crop production") %>%
        add_legacy_name("L122.ghg_tg_R_agr_C_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech","emissions/CEDS/CEDS_sector_tech_revised",
                       "L124.LC_bm2_R_Grass_Yh_GLU_adj","L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj") ->
        L122.ghg_tg_R_agr_C_Y_GLU


      L124.nonco2_tg_R_grass_Y_GLU %>%
        add_title("Grassland fire emissions by GCAM region, gas, and historical year") %>%
        add_units("Tg/yr") %>%
        add_comments("EDGAR grassland emissions are downscaled to GLU using shares of grassland area.") %>%
        add_legacy_name("L124.nonco2_tg_R_grass_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech",
                       "L124.LC_bm2_R_Grass_Yh_GLU_adj") ->
        L124.nonco2_tg_R_grass_Y_GLU

      L124.nonco2_tg_R_forest_Y_GLU %>%
        add_title("Forest fire and deforestation emissions by GCAM region, gas, and historical year") %>%
        add_units("Tg/yr") %>%
        add_comments("EDGAR forest emissions are downscaled to GLU using shares of forest area.") %>%
        add_comments("These emissions are then separated into forest fire and deforestation using GFED data.") %>%
        add_legacy_name("L124.nonco2_tg_R_forest_Y_GLU") %>%
        add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech",
                       "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj") ->
        L124.nonco2_tg_R_forest_Y_GLU

      L124.deforest_coefs %>%
        add_title("Default deforestation coefficients by Non-CO2 species") %>%
        add_units("Tg/yr") %>%
        add_comments("Global average deforestation coefficients are calculated from global emissions and global deforestation.") %>%
        add_legacy_name("L124.deforest_coefs") %>%
        same_precursors_as("L124.nonco2_tg_R_forest_Y_GLU") ->
        L124.deforest_coefs


      L131.nonco2_tg_R_prc_S_S_Yh %>%
        add_title("GHG emissions by GCAM region / sector / technology / historical year") %>%
        add_units("Tg") %>%
        add_comments("Calculate historical emissions from the processing sector by GCAM ") %>%
        add_comments("technology computed from CEDS emissions data and scaled to EPA 2019") %>%
        add_comments("for CH4 and NO2 industry processes") %>%
        add_legacy_name("L131.nonco2_tg_R_prc_S_S_Yh") %>%
        add_precursors("emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                       "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech",
                       "emissions/EPA_FCCC_IndProc_2005"
                       ) ->
        L131.nonco2_tg_R_prc_S_S_Yh

      return_data(L111.nonghg_tg_R_en_S_F_Yh, L111.nonghg_tgej_R_en_S_F_Yh, L112.ghg_tg_R_en_S_F_Yh, L112.ghg_tgej_R_en_S_F_Yh, L113.ghg_tg_R_an_C_Sys_Fd_Yh, L115.nh3_tg_R_an_C_Sys_Fd_Yh, L121.nonco2_tg_R_awb_C_Y_GLU,
                  L121.AWBshare_R_C_Y_GLU, L122.ghg_tg_R_agr_C_Y_GLU, L122.EmissShare_R_C_Y_GLU, L124.nonco2_tg_R_grass_Y_GLU, L124.nonco2_tg_R_forest_Y_GLU, L124.deforest_coefs,
                  L131.nonco2_tg_R_prc_S_S_Yh) # TURNED OFF BCOC OUTPUTS: L125.bcoc_tgbkm2_R_grass_2000,L125.bcoc_tgbkm2_R_forest_2000,L125.deforest_coefs_bcoc,L123.bcoc_tgmt_R_awb_2000, L114.bcoc_tgej_R_en_S_F_2000,
    } else {
      stop("Unknown command")
    }
  }
}
