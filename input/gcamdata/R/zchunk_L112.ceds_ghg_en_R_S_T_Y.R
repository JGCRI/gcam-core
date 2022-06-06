# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L112.ceds_ghg_en_R_S_T_Y
#'
#' Calculates emissions and emissions factors using CEDS and GFED emissions data for GCAM sectors and technologies. The chunk contains processing for the following,
#' Part 1 : Distributing CEDS road transport emissions into different modes using the GAINS emissions data
#' Part 2 : Calculation of emission factors, input emissions for all combustion emission types.
#' Part 3 : Calculation of input emissions for process emission sectors
#' Part 4 : Calculation of animal sector emissions
#' Part 5 : Calculation of cropland emissions.
#' Part 6 : Calculation of agricultural waste burning emissions.
#' Part 7 : Calculation of input emissions, deforestation co-efs for unmanaged land emissions sectors.
#' Part 8 : Scale CH4 and N2O emissions to 2019 EPA nonCO2 report
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L112.cedsghg_tg_R_en_S_F_Yh}, \code{L112.cedsghg_tgej_R_en_S_F_Yh}. The corresponding file in the
#' original data system was \code{L112.ghg_en_R_S_T_Y.R} (emissions level1).
#' @details Calculates emissions using CEDS emissions and GCAM sectors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select mutate_all
#' @importFrom tidyr gather spread
#' @importFrom tibble tibble
#' @author CWR Oct. 2018 , YO Mar. 2020, KBN 2020
module_emissions_L112.ceds_ghg_en_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             FILE = "emissions/mappings/CEDS_sector_tech_proc",
             FILE = "emissions/mappings/CEDS_sector_tech_proc_revised",
             FILE = "energy/mappings/UCD_techs",
             FILE = "emissions/mappings/UCD_techs_emissions_revised",
             FILE = "energy/calibrated_techs",
             FILE = "energy/calibrated_techs_bld_det",
             FILE = "emissions/mappings/Trn_subsector",
             FILE = "emissions/mappings/Trn_subsector_revised",
             FILE = "emissions/CEDS/CEDS_sector_tech_combustion",
             FILE = "emissions/CEDS/CEDS_sector_tech_combustion_revised",
             FILE = "emissions/EPA_FCCC_IndProc_2005",
             FILE = "emissions/mappings/calibrated_outresources",
             FILE="emissions/CEDS/gains_iso_sector_emissions",
             FILE="emissions/CEDS/gains_iso_fuel_emissions",
             "L102.ceds_GFED_nonco2_tg_R_S_F",
             "L102.ceds_int_shipping_nonco2_tg_S_F",
             "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
             "L101.in_EJ_R_en_Si_F_Yh",
             "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
             "L101.ag_Prod_Mt_R_C_Y_GLU",
             "L111.ag_resbio_R_C",
             "L111.Prod_EJ_R_F_Yh",
             "L103.ghg_tgmt_USA_an_Sepa_F_2005",
             "L124.LC_bm2_R_Grass_Yh_GLU_adj",
             "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",
             "L154.IEA_histfut_data_times_UCD_shares",
             "L1326.in_EJ_R_indenergy_F_Yh",
             "L1323.in_EJ_R_iron_steel_F_Y",
             "L1324.in_EJ_R_Off_road_F_Y",
             "L1325.in_EJ_R_chemical_F_Y",
             "L1326.in_EJ_R_aluminum_Yh",
             "L270.nonghg_tg_state_refinery_F_Yb",
             FILE = "emissions/CEDS/ceds_sector_map",
             FILE = "emissions/CEDS/ceds_fuel_map",
             # EPA scaling process 2020
             FILE = "emissions/EPA_country_map",
             FILE = "emissions/EPA/EPA_2019_raw",
             FILE = "emissions/EPA_CH4N2O_map",
             FILE = "emissions/GCAM_EPA_CH4N2O_energy_map",
             # BC OC assumption files
             FILE = "gcam-usa/emissions/BC_OC_assumptions",
             FILE = "gcam-usa/emissions/BCOC_PM25_ratios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L111.nonghg_tg_R_en_S_F_Yh",
             "L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP",
             "L112.ghg_tg_R_en_S_F_Yh",
             "L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP",
             "L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy",
             "L113.ghg_tg_R_an_C_Sys_Fd_Yh",
             "L115.nh3_tg_R_an_C_Sys_Fd_Yh",
             "L121.nonco2_tg_R_awb_C_Y_GLU",
             "L121.AWBshare_R_C_Y_GLU",
             "L122.ghg_tg_R_agr_C_Y_GLU",
             "L122.EmissShare_R_C_Y_GLU",
             "L124.nonco2_tg_R_grass_Y_GLU",
             "L124.nonco2_tg_R_forest_Y_GLU",
             "L124.deforest_coefs",
             "L131.nonco2_tg_R_prc_S_S_Yh",
             "L125.bcoc_tgbkm2_R_grass_2000",
             "L125.bcoc_tgbkm2_R_forest_2000",
             "L125.deforest_coefs_bcoc"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    #silence packages
    lifetime <- emscaler <- ej_vintage <- tg_vintage <- tg_i_total <- tg_i <- ej_i <- value_adj <-
      tot_emissions <- EPA_sector <- final_year  <- year_operate <- timestep <-  avg.prod.lifetime  <-  resource  <- gas <- country <- fuel <- CEDS_sector <-
      em <- iso <- unit <- year <- emissions <- sector <- Non.CO2 <- GCAM_region_ID <- CEDS_agg_sector <-
      CEDS_agg_fuel <- GCAM_region <- rev.mode <- rev_size.class <- UCD_fuel <- size.class <- UCD_technology <- UCD_sector <- sector_weight <-
      dieseloil <- lightoil <- mode_weight <- sum_sector_weight <- sum_mode_weight <- secondary.output <- technology <- energy <- supplysector <-
      subsector <- stub.technology <- sector <- service <- tranSubsector <- tranTechnology <- totalenergy <- enshare <- GCAMemissions <- D_driver <-
      `2005` <- `2000` <- em_factor <- year2 <- year1 <- Land_Type <- PctDeforest <- PctForestFire <- forestfire <- deforest <- land_share <- awb_emission <-
      total_emiss <- AWB_emiss_share <- total_excess_bio <- burnable <- WaterContent <- HarvestIndex <- Root_Shoot <- ResEnergy_GJt <- ErosCtrl_tHa <-
      emiss_share <- prod_share <- total_prod <- crop_area_share <- prod_share_GLU <- GLU <- Prod_R_C <- crop_area_total <- scalar <- EPA_emissions <-
      CEDS_emissions <- epa_emissions <-  ch4_em_factor <- production <- feed <- GCAM_commodity <- input.emissions <- EPA_agg_fuel_ghg <- EPA_agg_sector <-
      EDGAR_agg_sector <- globalemfact <- emfact <- value <- em_fact <- . <- FF_driver <- natural_gas <- UCD_category <- Non.co2 <-
      quantile <- upper <- GCAM_subsector <- value_median <- share_in_global_ship <- main.fuel <- CEDS_agg_fuel_remapped <- NULL


    #Get CEDS_GFED data
    L112.CEDS_GCAM_no_intl_shipping <- get_data(all_data, "L102.ceds_GFED_nonco2_tg_R_S_F")

    #Get CEDS international shipping data
    L112.CEDS_intl_shipping <- get_data(all_data, "L102.ceds_int_shipping_nonco2_tg_S_F")
    Int_shipping_IEA_EIA <- get_data(all_data, "L154.IEA_histfut_data_times_UCD_shares") %>% filter(UCD_category=="trn_international ship")

    #Get NEI data for crude oil and natural gas, and BC OC fractions for this
    NEI_tg_oilgas_state_Yb <- get_data(all_data, "L270.nonghg_tg_state_refinery_F_Yb") %>% filter(sector == "NG_production_distribution" | sector == "petroleum_production")
    BC_OC_assumptions <- get_data(all_data, "gcam-usa/emissions/BC_OC_assumptions")
    BCOC_PM25_ratios <- get_data(all_data, "gcam-usa/emissions/BCOC_PM25_ratios") %>%
      # removing columns we do not use, and sectors that aren't mapped to GCAM sectors
      select( -c( "Region", "Gains_Sector" ) ) %>%
      filter( !is.na( sector ) )

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")

    #Process data for international shipping to disaggregate to the GCAM regions
    L112.CEDS_intl_shipping %>%
      right_join(Int_shipping_IEA_EIA %>% select(iso,year,value) %>% filter(year <= max(HISTORICAL_YEARS)), by=c("year")) %>%
      mutate(emissions=if_else(is.na(emissions),0,emissions)) %>%
      group_by(Non.CO2,year,sector,fuel) %>%
      mutate(share_in_global_ship= value/sum(value)) %>%
      ungroup() %>%
      # Converts kt(gg) to Teragrams. Multiply by iso's share in international shipping consumption.
      mutate(emissions = (emissions * CONV_GG_TG)*share_in_global_ship) %>%
      select(-share_in_global_ship,-value) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, Non.CO2, CEDS_agg_sector, CEDS_agg_fuel, year) %>%
      summarise(emissions = sum(emissions)) %>%
      ungroup() %>%
      na.omit()->CEDS_int_shipping

    #Combine emissions from all other CEDS sectors with emissions from international shipping
    L112.CEDS_GCAM_no_intl_shipping %>%
      bind_rows(CEDS_int_shipping)->L112.CEDS_GCAM

    #In case of tanker loading emissions which are classified as process emissions, transfer them to refined liquids. Same for processs industrial energy emissions
    L112.CEDS_GCAM %>%
      mutate(CEDS_agg_fuel=if_else(CEDS_agg_sector=="trn_intl_ship",if_else(CEDS_agg_fuel=="process","refined liquids",CEDS_agg_fuel),CEDS_agg_fuel)) %>%
      # Note that these are 1A1bc other energy transformation emissions, which are from both refineries and processes such as coal coke plants. These will be broken out in CEDS in the future in more detail.
      mutate(CEDS_agg_fuel=if_else(CEDS_agg_sector=="industry_energy",if_else(CEDS_agg_fuel=="process","refined liquids",CEDS_agg_fuel),CEDS_agg_fuel)) %>%
      #Only keep nitric and adipic acids for N2O. For the rest, put these in generic industrial processes.
      mutate(CEDS_agg_sector=if_else(Non.CO2 !="N2O",if_else(CEDS_agg_fuel=="process",
                                                             if_else(CEDS_agg_sector=="chemicals_nitric","industry_processes",CEDS_agg_sector),CEDS_agg_sector),CEDS_agg_sector)) %>%
      mutate(CEDS_agg_sector=if_else(Non.CO2 !="N2O",if_else(CEDS_agg_fuel=="process",
                                                             if_else(CEDS_agg_sector=="chemicals_adipic","industry_processes",CEDS_agg_sector),CEDS_agg_sector),CEDS_agg_sector)) %>%
      group_by(GCAM_region_ID, Non.CO2, CEDS_agg_sector, CEDS_agg_fuel, year) %>%
      summarise(emissions = sum(emissions)) %>%
      ungroup() %>%
      na.omit() %>%
      #filter data for final model base year, since we may not have GCAM activity data beyond the latest base year.
      filter(year<= max(HISTORICAL_YEARS))->L112.CEDS_GCAM

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")

    #Get GAINS sector and fuel emissions by iso. Also get IEA energy data by iso.
    GAINS_sector <- get_data(all_data,"emissions/CEDS/gains_iso_sector_emissions")
    #Separate out GAINS em factors for NG
    GAINS_fuel <- get_data(all_data,"emissions/CEDS/gains_iso_fuel_emissions") %>%  select(-natural_gas)
    GAINS_fuel_NG <- get_data(all_data,"emissions/CEDS/gains_iso_fuel_emissions") %>%  select(-dieseloil,-lightoil)
    IEA_Ctry_data <- get_data(all_data,"L154.IEA_histfut_data_times_UCD_shares")

    #If using revised size classes, use revised data else use old data
    if (energy.TRAN_UCD_MODE == "rev.mode"){
      IEA_Ctry_data %>% rename(mode=rev.mode,size.class=rev_size.class)->IEA_Ctry_data
    }
    #kbn Add code below so that we can use revised sub-sectors from transportation model
    if (energy.TRAN_UCD_MODE == "rev.mode"){
      GCAM_sector_tech <- get_data(all_data, "emissions/mappings/CEDS_sector_tech_proc_revised") %>% distinct()
    }else{
      GCAM_sector_tech <- get_data(all_data, "emissions/mappings/CEDS_sector_tech_proc")}

    #kbn Add code below so that we can use revised sub-sectors from transportation model
    if (energy.TRAN_UCD_MODE == "rev.mode"){
      Trn_subsector <- get_data(all_data, "emissions/mappings/Trn_subsector_revised")
    }else{


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
    L1326.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1326.in_EJ_R_indenergy_F_Yh")
    L1323.in_EJ_R_iron_steel_F_Y <- get_data(all_data, "L1323.in_EJ_R_iron_steel_F_Y")
    L1324.in_EJ_R_Off_road_F_Y <- get_data(all_data, "L1324.in_EJ_R_Off_road_F_Y")
    L1325.in_EJ_R_chemical_F_Y <- get_data(all_data, "L1325.in_EJ_R_chemical_F_Y")
    L1326.in_EJ_R_aluminum_Yh <- get_data(all_data, "L1326.in_EJ_R_aluminum_Yh")
    CEDS_sector_map <- get_data(all_data, "emissions/CEDS/ceds_sector_map")
    CEDS_fuel_map <- get_data(all_data, "emissions/CEDS/ceds_fuel_map")

    #kbn Add in transport flexibility below
    if (energy.TRAN_UCD_MODE=="rev.mode"){
      CEDS_sector_tech <- get_data(all_data, "emissions/CEDS/CEDS_sector_tech_combustion_revised") %>% distinct()
    }else{

      CEDS_sector_tech <- get_data(all_data, "emissions/CEDS/CEDS_sector_tech_combustion")
    }

    calibrated_outresources <- get_data(all_data, "emissions/mappings/calibrated_outresources")

    L124.LC_bm2_R_Grass_Yh_GLU_adj <- get_data(all_data, "L124.LC_bm2_R_Grass_Yh_GLU_adj",strip_attributes = TRUE)
    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- get_data(all_data, "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",strip_attributes = TRUE)
    L107.an_Prod_Mt_R_C_Sys_Fd_Y <- get_data(all_data, "L107.an_Prod_Mt_R_C_Sys_Fd_Y",strip_attributes = TRUE)
    L101.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L101.ag_Prod_Mt_R_C_Y_GLU",strip_attributes = TRUE)
    L111.ag_resbio_R_C <- get_data(all_data, "L111.ag_resbio_R_C",strip_attributes = TRUE)
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",strip_attributes = TRUE)
    L103.ghg_tgmt_USA_an_Sepa_F_2005 <- get_data(all_data, "L103.ghg_tgmt_USA_an_Sepa_F_2005",strip_attributes = TRUE)

    # EPA Raw CH4 and N2O data files
    # YO 2020 EPA scaling
    EPA_master <- get_data(all_data, "emissions/EPA/EPA_2019_raw")
    EPA_CH4N2O_map <- get_data(all_data, "emissions/EPA_CH4N2O_map")
    EPA_country_map <- get_data(all_data, "emissions/EPA_country_map")
    GCAM_EPA_CH4N2O_map <- get_data(all_data, "emissions/GCAM_EPA_CH4N2O_energy_map")
    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "L111.Prod_EJ_R_F_Yh",strip_attributes = TRUE)

    #kbn calculate emissions for different modes here
    # ===========================
    # Part 1: Distributing road emissions from CEDS into different modes using GAINS data #
    # ===========================

    #Define special modes based on choices by user. With the old size classes, we had 2 modes, LDV_2W & LDV_3W. These,
    #were consolidated into 1 category with the new modes (LDV_2W_3W). We need the adjustment below so emissions
    # are calculated appropriately.

    if(energy.TRAN_UCD_MODE=="rev.mode"){

      LDV_2W_3W_modes <- c("LDV_2W_3W")

    }else{

      LDV_2W_3W_modes <- c("LDV_2W","LDV_3W")

    }


    #Filter out road emissions
    L112.CEDS_Road_emissions <- L112.CEDS_GCAM %>%  filter(CEDS_agg_sector == "trn_road")
    L112.CEDS_GCAM<- L112.CEDS_GCAM %>%  filter(CEDS_agg_sector != "trn_road")

    #Clean IEA data, filter to road emissions and  remove zero emission technologies.
    IEA_Ctry_data %>%
      #Use only historical years
      filter(year <= max(HISTORICAL_YEARS)) %>%
      filter(UCD_category=="trn_road and rail") %>%
      filter(!mode %in% c("Rail","HSR")) %>%
      select(-UCD_fuel,-fuel,-size.class) %>%
      rename(fuel=UCD_technology) %>%
      #NG is treated separately.
      filter(!fuel %in% c(emissions.ZERO_EM_TECH,"NG")) %>%
      mutate(fuel =if_else(fuel=="Hybrid Liquids","Liquids",fuel))->Clean_IEA_ctry_data

    #Calculate GAINS sector weights which we can use on CEDS data to distribute emissions into Passenger and Freight.
    Clean_IEA_ctry_data %>%
      group_by(iso,UCD_sector,year) %>%
      mutate(value=sum(value)) %>%
      ungroup() %>%
      select(iso,UCD_sector,year,value,GCAM_region_ID) %>%
      distinct() %>%
      repeat_add_columns(tibble(Non.co2 = unique(GAINS_sector$Non.co2))) %>%
      left_join(GAINS_sector %>% gather("UCD_sector","em_fact","Freight":"Passenger"),by=c("iso","year","Non.co2","UCD_sector")) %>%
      na.omit() %>%
      filter(UCD_sector != "Motorcycle") %>%
      group_by(Non.co2,GCAM_region_ID,year,UCD_sector) %>%
      mutate(sector_weight=sum(em_fact*value)) %>%
      ungroup() %>%
      select(Non.co2,GCAM_region_ID,year,UCD_sector,sector_weight) %>%
      #Use these sector weights to split CEDS emissions into passenger and freight
      distinct() ->GAINS_sector_weights

    #Calculate GAINS mode weights which we can use on CEDS data to distribute emissions into different modes.
    Clean_IEA_ctry_data %>%
      group_by(iso,mode,year,UCD_sector) %>%
      mutate(value=sum(value)) %>%
      ungroup() %>%
      select(iso,mode,year,value,GCAM_region_ID,UCD_sector) %>%
      distinct() %>%
      repeat_add_columns(tibble(Non.co2 = unique(GAINS_fuel$Non.co2))) %>%
      left_join(GAINS_fuel , by=c("Non.co2","iso","year")) %>%
      na.omit() %>%
      #Now calculate mode wights here
      group_by(Non.co2,GCAM_region_ID,year,mode,UCD_sector) %>%
      #For LDV_2W_3W use a weighted average of light and heavy diesel oil. For Trucks use heavy diesel oil and for passenger buses use light oil.
      mutate(mode_weight=if_else(mode=="Bus",sum(dieseloil*value),
                                 if_else(mode %in% c(LDV_2W_3W_modes),sum(((lightoil+dieseloil)/2)*value),
                                         if_else(mode=="Truck",sum(dieseloil*value),sum(lightoil*value))))) %>%
      ungroup() %>%
      select(Non.co2,GCAM_region_ID,year,mode,mode_weight,UCD_sector) %>%
      distinct()->GAINS_mode_weights

    #Complete distribution here by sector, mode.
    L112.CEDS_Road_emissions %>%
      rename(Non.co2=Non.CO2) %>%
      #Compute emissions by non.co2
      group_by(GCAM_region_ID,year,Non.co2) %>%
      mutate(emissions=sum(emissions)) %>%
      ungroup() %>%
      select(GCAM_region_ID,year,Non.co2,emissions) %>%
      distinct() %>%
      #Join in sector weights
      left_join(GAINS_sector_weights, by=c("GCAM_region_ID","year","Non.co2")) %>%
      na.omit() %>%
      #First split emissions into Passenger and Freight %>%
      group_by(GCAM_region_ID,year,Non.co2) %>%
      mutate(sum_sector_weight=sum(sector_weight)) %>%
      ungroup() %>%
      mutate(emissions=(emissions*sector_weight)/sum_sector_weight) %>%
      left_join(GAINS_mode_weights, by=c("GCAM_region_ID","year","Non.co2","UCD_sector")) %>%
      na.omit() %>%
      #Now do mode weight calculation
      group_by(GCAM_region_ID,year,Non.co2,UCD_sector) %>%
      mutate(sum_mode_weight=sum(mode_weight)) %>%
      ungroup() %>%
      mutate(emissions=(emissions*mode_weight)/sum_mode_weight) %>%
      ungroup() %>%
      mutate(CEDS_agg_fuel=paste0("refined liquids")) %>%
      rename(CEDS_agg_sector=mode,Non.CO2=Non.co2) %>%
      select(GCAM_region_ID,year,Non.CO2,CEDS_agg_sector,CEDS_agg_fuel,emissions) %>% distinct()->L112.CEDS_GCAM_Road_Emissions_GAINS

    #Bind new GAINS weighted emissions into CEDS emissions
    L112.CEDS_GCAM %>%  bind_rows(L112.CEDS_GCAM_Road_Emissions_GAINS)->L112.CEDS_GCAM



    #Adjustment for NG emissions factors
    #Since the values of NG emissions are low and the energy values for NG technologies are also very low, computing emissions
    #factors like we do for the other technologies may result in unreasonably high numbers. So, we take the emissions factors from GAINS,
    # and aggregate up to GCAM regions using IEA data for transportation NG as weights. The dataframe returned by the block below,
    #contains both energy and emission factors. We use the appropriate columns to blend with base data.

    #First, clean IEA data
    IEA_Ctry_data %>%
      #Use only historical years
      filter(year <= max(HISTORICAL_YEARS)) %>%
      filter(UCD_category=="trn_road and rail") %>%
      filter(!mode %in% c("Rail","HSR")) %>%
      select(-UCD_fuel,-fuel) %>%
      rename(fuel=UCD_technology) %>%
      filter(fuel %in% c("NG"))->Clean_IEA_ctry_data_NG

    # Now calculate emissions factors
    Clean_IEA_ctry_data_NG %>%
      group_by(iso,mode,size.class,year,UCD_sector,fuel) %>%
      mutate(value=sum(value)) %>%
      ungroup() %>%
      select(iso,mode,year,value,GCAM_region_ID,UCD_sector,fuel,size.class) %>%
      distinct() %>%
      repeat_add_columns(tibble(Non.co2 = unique(GAINS_fuel_NG$Non.co2))) %>%
      left_join(GAINS_fuel_NG , by=c("Non.co2","iso","year")) %>%
      na.omit() %>%
      rename(em=natural_gas) %>%
      #Now calculate mode wights here
      group_by(Non.co2,GCAM_region_ID,year,mode,UCD_sector,size.class,fuel) %>%
      mutate(em_factor= sum(em*value)/sum(value), energy= sum(value)) %>%
      ungroup() %>%
      select(Non.co2,GCAM_region_ID,year,mode,em_factor,UCD_sector,fuel,size.class, energy) %>%
      distinct() %>%
      rename(Non.CO2=Non.co2, stub.technology=fuel,value=em_factor) %>%
      #Values from GAINS are in kt/ej. Convert to Tg/ej.
      mutate(value=if_else(is.na(value),0,0.001*value),energy=if_else(is.na(energy),0,energy)) %>%
      left_join_keep_first_only(UCD_techs %>% select(-fuel) %>% rename(stub.technology=UCD_technology,subsector=tranSubsector),by=c("mode","size.class","UCD_sector","stub.technology")) %>%
      select(GCAM_region_ID,Non.CO2,supplysector,stub.technology,year,value,subsector,energy)->GAINS_NG_em_factors

    # ===========================
    #Part 2:Combustion Energy Emissions#
    # ===========================

    # Filter down to combustion emissions plus fugitive process emissions from combustion resource production (out_resources)
    L112.CEDS_GCAM %>%
      filter(CEDS_agg_fuel != "process" | CEDS_agg_sector %in% c("oil_gas", "coal")) ->
      L112.CEDS_GCAM_emissions_comb

    # USA oil_gas
    # ----------------------------------------
    # Replace CEDS oil_gas emissions with those from NEI for GCAM region 1, USA
    # Aggregate natural gas production and distribution, and petroleum production NEI emissions nationally
    # to get oil_gas emissions for USA
    NEI_tg_oilgas_USA_Yb.noBCOC <- NEI_tg_oilgas_state_Yb %>%
      group_by(Non.CO2, year) %>%
      mutate(value=sum(value)) %>%
      # We want one value for each pollutant and year
      distinct(Non.CO2,year,value) %>%
      ungroup() %>%
      # add columns that specify what sector / tech these emissions are (necessary for BC OC function)
      # since we are aggregated natural gas and oil production, and they have the same BC OC fractions,
      # we just need to assign either natural gas or oil as the sector
      mutate(supplysector = "petroleum_production", subsector = as.character(NA), stub.technology = as.character(NA)) %>%
      # these are input emissions, not emission coefficients. Just naming it this for use in compute_BC_OC
      rename(emiss.coef = value)

    # Use fractions of PM2.5 to calculate BC/OC emissions.
    # We need to modify the BC_OC_assumptions table, as the BCOC_PM25_ratios table has updated values that are time dependent
    # If there are sector/subsector/tech combos that are in BCOC_PM25_ratios, we want to replace those entries in
    # the BC_OC_assumptions table. We also need to extend the data.
    # Extrapolate the data to future model years, and format the table
    BCOC_PM25_ratios_ext <- BCOC_PM25_ratios %>%
      gather_years() %>%
      complete(nesting(Parameter,sector,subsector,technology), year = MODEL_YEARS) %>%
      # extrapolate missing years
      group_by(Parameter,sector,subsector,technology) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      spread( Parameter, value ) %>%
      rename( "BC_fraction" = `BC Fraction`,
              "OC_fraction" = `OC Fraction`)

    BC_OC_assumptions_ext <- BC_OC_assumptions %>%
      mutate( year = min( MODEL_BASE_YEARS ) ) %>%
      complete(nesting(sector,subsector,technology, BC_fraction, OC_fraction), year = MODEL_YEARS)

    # Join the tables, keeping values from BC_OC_assumptions_ext that do not appear in BCOC_PM25_ratios
    BC_OC_assumptions_years <- BC_OC_assumptions_ext %>%
      anti_join( BCOC_PM25_ratios_ext, by = c("sector", "subsector", "technology", "year") ) %>%
      # bind to BCOC_PM25_assumptions
      bind_rows( BCOC_PM25_ratios_ext ) %>%
      # since CEDS has years between model base years, we want to fill this table in
      complete(nesting(sector,subsector,technology), year = HISTORICAL_YEARS) %>%
      # extrapolate missing years
      group_by(sector,subsector,technology) %>%
      mutate(BC_fraction = approx_fun(year, BC_fraction, rule = 2),
             OC_fraction = approx_fun(year, OC_fraction, rule = 2)) %>%
      ungroup()

    # Compute BC and OC EFs based off of PM2.5
    NEI_tg_oilgas_USA_Yb <- compute_BC_OC(NEI_tg_oilgas_USA_Yb.noBCOC, BC_OC_assumptions_years) %>%
      # change emiss.coef to emissions, since that is what these are
      rename(emissions = emiss.coef) %>%
      # select columns we want to keep
      select(Non.CO2, year, emissions)
    # NA values are BC and OC for 2016 and 2017, and will be removed down the line

    # Filter oil_gas, USA, out of the CEDS emissions
    L112.CEDS_GCAM_emissions_NoOG_USA <- L112.CEDS_GCAM_emissions_comb %>%
      filter(CEDS_agg_sector != "oil_gas" | GCAM_region_ID != 1 )

    # Make a table that has oil_gas emissions for USA for the years and pollutants not in NEI
    # We will keep these from CEDS
    `%!in%` <- Negate(`%in%`)
    L112.CEDS_GCAM_emissions_OG_USA_keep <- L112.CEDS_GCAM_emissions_comb %>%
      filter(CEDS_agg_sector == "oil_gas", GCAM_region_ID == 1,
             Non.CO2 %!in% unique(NEI_tg_oilgas_USA_Yb$Non.CO2) |
               year %!in% unique(NEI_tg_oilgas_USA_Yb$year),
             # we also want to remove years before 1990, and just use NEI 1990 values pulled back
             # for all pollutants other than the ones in this table (CH4 and N2O) to avoid
             # a discontinuity between CEDS and NEI
             year >= min(NEI_tg_oilgas_USA_Yb$year))

    # Make a table that has oil_gas emissions for USA for the years and pollutants in NEI
    # We will replace the emissions in CEDS with those from NEI, using the structure from the CEDS table
    OilGas_structure <- L112.CEDS_GCAM_emissions_OG_USA_keep %>%
      select(-c("Non.CO2","emissions")) %>%
      distinct()

    L112.CEDS_GCAM_emissions_pull_back <- NEI_tg_oilgas_USA_Yb %>%
      # filter for HISTORICAL_YEARS so left_join_error_no_match works
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(OilGas_structure, by="year") %>%
      # bind back the table that has CH4 and N2O CEDS oil_gas emissions
      bind_rows(L112.CEDS_GCAM_emissions_OG_USA_keep)

    # Filter the table for the oldest year we have, and apply those values for the previous historical years
    L112.CEDS_GCAM_emissions <- L112.CEDS_GCAM_emissions_pull_back %>%
      filter(year == min(year)) %>%
      # add in the historical years for each entry
      complete(nesting(Non.CO2, GCAM_region_ID, CEDS_agg_sector, CEDS_agg_fuel), year = HISTORICAL_YEARS) %>%
      # extrapolate missing years
      group_by(Non.CO2, GCAM_region_ID, CEDS_agg_sector, CEDS_agg_fuel) %>%
      mutate(emissions = approx_fun_constant(year, emissions, rule = 2)) %>%
      ungroup() %>%
      # remove years we have in the dataframe above, since those change over time
      filter(year %!in% L112.CEDS_GCAM_emissions_pull_back$year) %>%
      # bind back the dataframe that has the other historical years, and the table with all other sector / region emissions
      bind_rows(L112.CEDS_GCAM_emissions_pull_back, L112.CEDS_GCAM_emissions_NoOG_USA)

    # PREPARE ENERGY FOR MATCHING TO EMISSIONS
    # ----------------------------------------

    #### Modification for detailed industry:
    # Replace industry_energy in L101.in_EJ_R_en_Si_F_Yh with energy in detailed industrial sectors and remaining industrial energy

    # prepare detailed industry energy use for matching and filter out feedstocks
    L1323.in_EJ_R_iron_steel_F_Y %>%
      mutate(sector = "iron and steel") %>%
      filter(!fuel %in% c("scrap")) ->
      L1323.in_EJ_R_iron_steel_F_Y

    L1324.in_EJ_R_Off_road_F_Y %>%
      mutate(technology = fuel) %>%
      filter(!grepl("feedstocks", sector)) ->
      L1324.in_EJ_R_Off_road_F_Y

    L1325.in_EJ_R_chemical_F_Y %>%
      filter(!grepl("feedstocks", sector)) %>%
      mutate(technology = fuel) ->
      L1325.in_EJ_R_chemical_F_Y

    L1326.in_EJ_R_aluminum_Yh %>%
      mutate(technology = fuel) ->
      L1326.in_EJ_R_aluminum_Yh

    L1326.in_EJ_R_indenergy_F_Yh %>% mutate(technology = fuel) %>%
      bind_rows(L1323.in_EJ_R_iron_steel_F_Y,
                L1324.in_EJ_R_Off_road_F_Y,
                L1325.in_EJ_R_chemical_F_Y,
                L1326.in_EJ_R_aluminum_Yh) %>%
      complete(nesting(GCAM_region_ID, sector, fuel, technology), year = HISTORICAL_YEARS) %>%
      replace_na(list(value = 0)) %>%
      select(GCAM_region_ID, fuel, technology, sector, year, energy = value) ->
      detailed_industry_en

    # Replace industrial energy use with energy in detailed industry sectors and remaining other industry
    L101.in_EJ_R_en_Si_F_Yh %>%
      filter(!sector == "industry_energy") %>%
      bind_rows(detailed_industry_en) ->
      L101.in_EJ_R_en_Si_F_Yh

    # Get main combustion fuel in iron and steel in base year by region and technology
    # Non-CO2 emissions will be assigned to this fuel for iron and steel, since it can't be broken out by subsector, technology, and fuel
    L1323.in_EJ_R_iron_steel_F_Y %>%
      filter(year == max(MODEL_BASE_YEARS),
             # filter out electricity
             !fuel %in% emissions.ZERO_EM_TECH) %>%
      group_by(GCAM_region_ID, sector, technology) %>%
      mutate(main.fuel = fuel[which.max(value)]) %>%
      select(GCAM_region_ID, supplysector = sector, stub.technology = technology, main.fuel) %>%
      unique() %>%
      mutate(CEDS_agg_fuel = main.fuel) ->
      ironsteel_main_fuel_BY

    # Splits energy balances out for industry sector and maps to final GCAM sectors
    L101.in_EJ_R_en_Si_F_Yh %>%
      left_join(calibrated_techs %>% bind_rows(calibrated_outresources) %>% select(-secondary.output), by = c("sector", "fuel", "technology")) %>%
      # Replace subsector with fuel to preserve both in dataframe. Subsector will be added back later in L201
      mutate(subsector = if_else(sector == "iron and steel", fuel, subsector)) %>%
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
      #We will drop all electricity sectors here

      filter(!stub.technology %in% c(emissions.ZERO_EM_TECH),
             !subsector %in% c(emissions.ZERO_EM_TECH, "heat")) %>%
      left_join_error_no_match(CEDS_sector_tech, by = c("supplysector", "subsector", "stub.technology")) ->
      L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy


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
    L112.in_EJ_R_en_S_F_Yh_calib_enshare %>%
      left_join(L112.CEDS_GCAM_emissions,
                by = c("GCAM_region_ID", "year", "CEDS_agg_sector", "CEDS_agg_fuel")) ->
      L112.CEDSGCAM_emissions

    L112.CEDSGCAM_emissions %>%
      mutate(GCAMemissions = emissions * enshare) ->
      L112.CEDSGCAM_computedemissions

    # because of additional level of detail in iron and steel sector, map emissions to the main fuel for each technology and region
    # (current structure can't handle multiple inputs for each technology)
    L112.CEDSGCAM_computedemissions %>%
      filter(supplysector == "iron and steel") %>%
      select(-CEDS_agg_fuel) %>%
      left_join(ironsteel_main_fuel_BY, by = c("GCAM_region_ID", "supplysector", "stub.technology")) %>%
      mutate(subsector = main.fuel) %>%
      group_by(GCAM_region_ID, year, supplysector, subsector, stub.technology, CEDS_agg_sector, CEDS_agg_fuel, Non.CO2) %>%
      summarise(emissions = sum(emissions),
                GCAMemissions = sum(GCAMemissions)) %>%
      ungroup ->
      L112.CEDSGCAM_computedemissions_steel_adj

    L112.CEDSGCAM_computedemissions %>%
      filter(!supplysector == "iron and steel") %>%
      bind_rows(L112.CEDSGCAM_computedemissions_steel_adj) ->
      L112.CEDSGCAM_computedemissions_complete

    L112.CEDSGCAM_computedemissions_complete %>%
      select(GCAM_region_ID, Non.CO2, year, supplysector, subsector, stub.technology, GCAMemissions) %>%
      rename(emissions = GCAMemissions) ->
      L112.nonco2_tg_R_en_S_F_Yh



    # Now join emissions and energy data together to calculate emissions factors

    # first also map iron and steel energy use to the main fuel by technology and region, like we did for emissions
    L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy %>%
      left_join((ironsteel_main_fuel_BY %>% rename(CEDS_agg_fuel_remapped = CEDS_agg_fuel)),
                by = c("GCAM_region_ID", "supplysector", "stub.technology")) %>%
      mutate(subsector = if_else(supplysector == "iron and steel", main.fuel, subsector),
             CEDS_agg_fuel = if_else(supplysector == "iron and steel", CEDS_agg_fuel_remapped, CEDS_agg_fuel)) %>%
      group_by(GCAM_region_ID, year, supplysector, subsector, stub.technology, CEDS_agg_sector, CEDS_agg_fuel) %>%
      summarise(energy = sum(energy)) %>%
      ungroup() ->
      L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy

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
      mutate(emfact = median(emfact), upper = quantile(emfact,0.95)) %>%
      ungroup() %>%
      rename(globalemfact = emfact) %>%
      select(year, Non.CO2, supplysector, subsector, stub.technology, globalemfact, upper) %>%
      distinct()->
      L112.nonco2_tgej_R_en_S_F_Yh_globalmedian

    # Replaces all emissions factors above a given value or that are NAs with the global median emissions factor for that year, non.CO2, and technology
    L112.nonco2_tgej_R_en_S_F_Yh_withNAs %>%
      left_join_error_no_match(L112.nonco2_tgej_R_en_S_F_Yh_globalmedian, by = c("year", "Non.CO2", "supplysector", "subsector", "stub.technology")) %>%
      #There are two adjustments here. First, we check if the supply sector is related to fossil fuels, in that case, if the emfact is above 95th percentile, we replace with the global median.
      #If not, we just compare with our threshold of 1000 tg/ej and make the replacements accordingly. These adjustments are structured given that fossil fuel production may increase rapidly in some regions (even though absolute increase may be low).
      mutate(emfact = if_else(supplysector == "out_resources", if_else(emfact >upper | is.na(emfact) , globalemfact, emfact),
                              if_else(emfact >  emissions.HIGH_EM_FACTOR_THRESHOLD | is.na(emfact) , globalemfact, emfact))) %>%
      select(GCAM_region_ID, Non.CO2, year, supplysector, subsector, stub.technology, emfact) %>%
      mutate(emfact = if_else(is.infinite(emfact), 1, emfact)) ->
      L112.nonco2_tgej_R_en_S_F_Yh


    # =======================
    # Part 3: Process Emissions
    # =======================

    # Calculate process emissions drivers
    # -----------------------------------


    # Subset CEDS process emissions and match to GCAM drivers

    L112.CEDS_GCAM %>%
      filter(CEDS_agg_sector %in% c("industry_processes", "landfills","waste_incineration" ,"wastewater", "aerosols",
                                    "metals", "foams", "solvents", "semiconductors","chemicals_nitric","chemicals_adipic")) ->
      L112.CEDS_GCAM_Proc

    GCAM_sector_tech %>%
      select(supplysector, subsector, stub.technology, EDGAR_agg_sector, EPA_agg_sector, EPA_agg_fuel_ghg) %>%
      filter(EDGAR_agg_sector %in% c("industry_processes" , "chemicals", "landfills", "wastewater",  # Filter for the agg sectors in EDGAR column for all NonCO2s.
                                     "solvents","chemicals_adipic","chemicals_nitric","waste_incineration")) %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L112.CEDS_GCAM_Proc$Non.CO2))) %>%
      repeat_add_columns(tibble(year = emissions.CEDS_YEARS)) %>%
      repeat_add_columns(tibble(GCAM_region_ID = GCAM_region_names$GCAM_region_ID)) %>%
      #Remove nitric and adipic acids for all gases except N2O
      left_join(L112.CEDS_GCAM_Proc, by = c("GCAM_region_ID", "EDGAR_agg_sector" = "CEDS_agg_sector", "Non.CO2", "year")) %>%
      mutate(emissions=if_else(is.na(emissions),0,emissions)) %>%
      na.omit() %>%  # delete rows with NA's
      mutate(input.emissions = emissions) %>%  # Calculate emissions
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year) %>%
      summarise(value = sum(input.emissions)) %>% # Calculate total emissions
      ungroup()->L131.nonco2_tg_R_prc_S_S_Yh

    # Note: Final sector outputs
    # "nitric acid" "other industrial processes" "solvents"
    # "landfills" "wastewater treatment" "waste_incineration"

    # ===================================================
    # Part 4: Animal Emissions
    # ===================================================
    # Computing unscaled emissions by country and technology
    # using animal production from L107.an_Prod_Mt_R_C_SYS_Fd_Y
    # and EPA emissions factors that are scaled up to CEDS emissions.

    #First get Animal emissions from CEDS
    L112.CEDS_GCAM %>%
      filter(CEDS_agg_sector == "Animals") %>%
      mutate(Non.CO2 = paste(Non.CO2,"_AGR",sep="")) ->
      L112.CEDS_GCAM_An

    #Compute EPA animal emissions.
    L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
      rename(production = value) %>%
      left_join(GCAM_sector_tech, by = c("GCAM_commodity" = "sector", "system" = "fuel", "feed" = "technology")) %>%
      select(GCAM_region_ID, GCAM_commodity, system, feed, year, production, EPA_agg_sector, EDGAR_agg_sector) %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L112.CEDS_GCAM_An$Non.CO2))) %>%  # Add Gas Name and AGR for agriculture
      # match in emissions factors, using left_join and dropping fuel column
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
      filter(year %in% emissions.CEDS_YEARS) %>%
      replace_na(list(emissions = 0)) %>%
      select(GCAM_region_ID, Non.CO2, supplysector = GCAM_commodity, subsector = system, stub.technology = feed,
             value = emissions, year) ->
      L113.ghg_tg_R_an_C_Sys_Fd_Yh_full

    # ==============================
    # Part 5: Cropland Emissions
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
      transmute(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, GLU, emiss_share = prod_share_GLU * crop_area_share)

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
      transmute(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, prod_share = value / total_prod) ->
      L122.ag_Prod_Mt_R_rice_Y_GLU

    # Multiply total emissions by production share
    L122.ag_Prod_Mt_R_rice_Y_GLU %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L112.CEDS_GCAM_rice$Non.CO2))) %>%
      left_join_error_no_match(L112.CEDS_GCAM_rice, by = c("GCAM_region_ID", "Non.CO2", "year")) %>%
      transmute(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, GLU, Non.CO2,
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
      transmute(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, GLU, Non.CO2,
                emissions = emissions * emiss_share, type = "Soil") ->
      L122.ghgsoil_tg_R_C_Y_GLU

    # Bind together dataframes & aggregate
    L122.ghg_tg_R_agr_C_Y_GLU_full <- bind_rows( L122.ghg_tg_R_rice_Y_GLU, L122.ghgsoil_tg_R_C_Y_GLU#, L122.ghgfert_tg_R_C_Y_GLU
    ) %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, GLU, Non.CO2) %>%
      summarise(value = sum(emissions)) %>%
      ungroup()

    # ===================================================
    # Part 6: AGRICULTURAL WASTE BURNING
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
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, GLU, AWB_emiss_share) ->
      L112.AWBshare_R_C_GLU

    # Calculate AWB Emissions
    # -----------------------


    # Compute agricultural waste burning emissions by GCAM region, commodity, and GLU

    # Add gas name to AWB production shares to prepare for matching with emissions
    L112.AWBshare_R_C_GLU %>%
      repeat_add_columns(tibble(`Non.CO2` = unique(L112.CEDS_GCAM_awb$Non.CO2))) ->
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
      select(GCAM_region_ID, Non.CO2, GCAM_commodity, GCAM_subsector, GLU, year, value = emissions) ->
      L112.nonco2_tg_R_awb_C_Y_GLU

    # Calculates BC/OC emissions factors for a separate data frame output. bc-oc emissions / ag residue
    L112.nonco2_tg_R_awb_C_Y_GLU %>%
      filter(Non.CO2 %in% c("BC_AWB", "OC_AWB")) %>%
      rename(awb_emission = value) %>%
      left_join_error_no_match(L112.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_burn,
                               by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "year")) %>%
      # Calculate emission factor, which is
      mutate(emfact = awb_emission / burnable) %>%
      select(GCAM_region_ID, Non.CO2, GCAM_commodity, GLU, year, emfact) %>%
      # Replace NaNs with zeros
      mutate_all(list(~ replace(., is.na(.), 0))) ->
      L112.bcoc_tgej_R_awb_C_Y_GLU
    # END AGRICULTURAL WASTE BURNING


    # ==============================
    # Part 7: UNMANAGED LAND EMISSIONS
    # ========================

    # Select unmanaged land emissions.

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
      inner_join(filter(L112.CEDS_GCAM_unmgd, sector == "grassland"), by = c("GCAM_region_ID", "year")) %>%         # Map in CEDS grassland emissions
      mutate(emissions = emissions * land_share) %>%                                                                # Compute emissions by GLU using CEDS totals and land shares
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

    L112.CEDS_GCAM_unmgd %>%
      filter(sector %in% c("forest","deforest")) %>%
      group_by(GCAM_region_ID, year,Non.CO2) %>%
      mutate(emissions=sum(emissions)) %>%
      ungroup() %>%
      select(-sector) %>%
      distinct()->L112.CEDS_GCAM_unmgd_Forest_Deforest

    # Downscale regional forest burning emissions to GLU based on the share of land in each GLU
    # Use GFED to separate into forest fires and deforestation, which have different drivers in GCAM
    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
      group_by(GCAM_region_ID, year) %>%
      mutate(land_share = value / sum(value)) %>%                                                      # Compute share of regional forest area in each GLU
      na.omit() %>%
      select(-value) %>%
      # There are places with land area but no emissions and vice versa. Use an inner_join to only get places with both.
      # Note: this means that some regions get zero emissions coefficients in the historic period (future deforestation emissions coefs are defined below)
      inner_join(L112.CEDS_GCAM_unmgd_Forest_Deforest, by = c("GCAM_region_ID", "year")) %>%       # Map in CEDS emissions information
      mutate(emissions = emissions * land_share) %>%                                                           # Compute forest emissions from CEDS totals and land shares
      select(-land_share) %>%
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
      left_join(filter(L124.nonco2_tg_R_forest_Y_GLU_full,                                                 # Map in CEDS deforestation emissions for the final deforestation year (as of 5/14/17 this was 2005)
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

    # Because of the diverse drivers and data sources, old level 1 outputs from CEDS were separated into multiple data frames and sources.
    # Some of these are preserved but many are different. This code slices the prepared CEDS emissions data to match the preexisting level 1 data
    # outputs to allow level two code to continue working unchanged.

    L112.nonco2_tg_R_en_S_F_Yh %>%
      filter(Non.CO2 %in% c("CH4", "N2O")) %>%
      select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emissions) %>%
      bind_rows(GAINS_NG_em_factors %>% mutate(value=energy*value) %>%
                  filter(Non.CO2 %in% c("CH4", "N2O")) %>% select(-energy))->
      L112.ghg_tg_R_en_S_F_Yh

    L112.nonco2_tg_R_en_S_F_Yh %>%
      filter(!(Non.CO2 %in% c("CH4", "N2O"))) %>%
      select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emissions) %>%
      bind_rows(GAINS_NG_em_factors %>% mutate(value=energy*value) %>%
                  filter(!(Non.CO2 %in% c("CH4", "N2O","CO2"))) %>% select(-energy))->L111.nonghg_tg_R_en_S_F_Yh



    L112.nonco2_tgej_R_en_S_F_Yh %>%
      filter(Non.CO2 %in% c("CH4", "N2O")) %>%
      select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emfact) %>%
      bind_rows(GAINS_NG_em_factors %>% filter(Non.CO2 %in% c("CH4", "N2O")) %>% select(-energy))->L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP


    L112.nonco2_tgej_R_en_S_F_Yh %>%
      filter(!(Non.CO2 %in% c("CH4", "N2O", "CO2"))) %>%
      select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, year, value = emfact) %>%
      bind_rows(GAINS_NG_em_factors %>% filter(!(Non.CO2 %in% c("CH4", "N2O"))) %>% select(-energy))->
      L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP


    # Animal NH3 emissions
    L113.ghg_tg_R_an_C_Sys_Fd_Yh_full %>%
      filter(Non.CO2 == "NH3_AGR") -> L115.nh3_tg_R_an_C_Sys_Fd_Yh

    # Animal all other emissions
    L113.ghg_tg_R_an_C_Sys_Fd_Yh_full %>%
      filter(Non.CO2 != "NH3_AGR") -> L113.ghg_tg_R_an_C_Sys_Fd_Yh

    # Ag Waste Burning
    L112.AWBshare_R_C_GLU ->
      L121.AWBshare_R_C_GLU

    L112.nonco2_tg_R_awb_C_Y_GLU ->
      L121.nonco2_tg_R_awb_C_Y_GLU


    L122.EmissShare_R_C_Y_GLU -> L122.EmissShare_R_C_Y_GLU

    # AGR emissions: Filter for gases present in original data set
    L122.ghg_tg_R_agr_C_Y_GLU_full ->
      L122.ghg_tg_R_agr_C_Y_GLU

    L124.deforest_coefs_full->L124.deforest_coefs

    L124.nonco2_tg_R_grass_Y_GLU_full %>%
      rename(value = emissions) ->L124.nonco2_tg_R_grass_Y_GLU

    L124.nonco2_tg_R_forest_Y_GLU_full ->
      L124.nonco2_tg_R_forest_Y_GLU


    L124.nonco2_tg_R_grass_Y_GLU_full %>%
      rename(value = emissions) ->L124.bcoc_tg_R_grass_Y_GLU

    L124.bcoc_tg_R_grass_Y_GLU %>%
      group_by(GCAM_region_ID, Non.CO2,year,Land_Type) %>%
      mutate(value=sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID, Non.CO2,year,Land_Type,value) %>%
      distinct()->L124.bcoc_tg_R_grass_Y_GLU

    L125.bcoc_tgbkm2_R_grass_2000 <- L124.LC_bm2_R_Grass_Yh_GLU_adj %>%
      group_by(GCAM_region_ID, Land_Type, year) %>%
      mutate(value=sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID, Land_Type, year,value) %>%
      distinct() %>% # aggregate grassland land area by regions/land type
      repeat_add_columns(tibble(Non.CO2 = unique(L124.bcoc_tg_R_grass_Y_GLU$Non.CO2))) %>%
      filter(year %in% c(L124.bcoc_tg_R_grass_Y_GLU$year)) %>% # repeat for both BC and OC
      inner_join(L124.bcoc_tg_R_grass_Y_GLU %>% rename(em=value) %>% filter(year %in% c(L124.LC_bm2_R_Grass_Yh_GLU_adj$year)), by = c("GCAM_region_ID", "Non.CO2","year","Land_Type")) %>% # add emissions to land region area
      mutate(em_factor = em / value) %>% # calculate emission factor (emissions/area)
      select(GCAM_region_ID, Land_Type, Non.CO2, em_factor,year) %>%
      arrange(Non.CO2, Land_Type,GCAM_region_ID,year) %>%
      ungroup()

    #Filter out BC/OC for its own output
    L124.nonco2_tg_R_forest_Y_GLU_full %>%
      filter(Non.CO2 %in% c("BC", "OC"))-> L125.bcoc_tgbkm2_R_forest_2000_data

    #Calculate BCOC for forest fires and deforestation
    #First get driver for forests
    L125.bcoc_tgbkm2_R_forestfire_2000 <- L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
      filter(year == 2000) %>%
      group_by(GCAM_region_ID, Land_Type) %>%
      summarise(FF_driver = sum(value))

    #Now get driver for deforest
    L125.bcoc_tgbkm2_R_GLU_defor_2000 <- L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
      filter(year %in% emissions.DEFOREST_COEF_YEARS) %>%      # select only deforestation coefficient years (used to estiamte rate of change - D-driver)
      group_by(GCAM_region_ID, Land_Type, GLU) %>%
      spread(year, value) %>%
      mutate(D_driver = pmax(`2000` - `2005`, 0) / (emissions.DEFOREST_COEF_YEARS[2] - emissions.DEFOREST_COEF_YEARS[1]))

    L125.bcoc_tgbkm2_R_defor_2000 <-  L125.bcoc_tgbkm2_R_GLU_defor_2000 %>%
      group_by(GCAM_region_ID, Land_Type) %>%
      summarise_at(vars(D_driver), sum)


    L125.bcoc_tgbkm2_R_forest_2000_data %>%
      filter(year==2000) %>%
      filter(technology=="ForestFire") %>%
      group_by(GCAM_region_ID,Land_Type,year,Non.CO2,technology) %>%
      mutate(value=sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID,Land_Type,year,Non.CO2,technology,value) %>%
      distinct() %>%
      #Join in forest fire data
      inner_join(L125.bcoc_tgbkm2_R_forestfire_2000,by=c("GCAM_region_ID","Land_Type")) %>%
      group_by("GCAM_region_ID","Land_Type","technology","Non.CO2") %>%
      mutate(em_factor=(value)/(FF_driver)) %>%
      ungroup() %>%
      select(GCAM_region_ID,Land_Type,Non.CO2,technology,em_factor) %>%
      distinct()->BC_OC_Forest

    L125.bcoc_tgbkm2_R_forest_2000_data %>%
      filter(year==2000) %>%
      filter(technology=="Deforest") %>%
      group_by(GCAM_region_ID,Land_Type,year,Non.CO2,technology) %>%
      mutate(value=sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID,Land_Type,year,Non.CO2,technology,value) %>%
      distinct() %>%
      #Join in de-forest data
      inner_join(L125.bcoc_tgbkm2_R_defor_2000,by=c("GCAM_region_ID", "Land_Type")) %>%
      group_by("GCAM_region_ID","Land_Type","technology","Non.CO2") %>%
      mutate(em_factor=(value)/(D_driver)) %>%
      ungroup() %>%
      select(GCAM_region_ID,Land_Type,Non.CO2,technology,em_factor) %>%
      distinct()->BC_OC_Deforest

    L125.bcoc_tgbkm2_R_forest_2000<-bind_rows(BC_OC_Deforest,BC_OC_Forest) %>%
      mutate(em_factor=if_else(is.finite(em_factor),em_factor,0))


    # Filter out BC OC for its own output
    L124.deforest_coefs_full %>%
      filter(Non.CO2 %in% c("BC", "OC")) ->L125.deforest_coefs_bcoc

    # ==============================
    # Part 8: SCALE TO EPA
    # ========================

    if(emissions.NONCO2.EPA.SCALING){
      # EPA scaling process
      # Scale CH4 and N2O emissions to EPA 2019 mitigation report (BAU scenario)
      # Source: https://www.epa.gov/global-mitigation-non-co2-greenhouse-gases
      # The following tables will be updated by the order below:
      # 1) L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP (resource production emission factors)
      # 2) L131.nonco2_tg_R_prc_S_S_Yh (industrial processes and urban processes input emissions)
      # 3) L113.ghg_tg_R_an_C_Sys_Fd_Yh (agriculture livestock - input emissions)
      # 4) L121.nonco2_tg_R_awb_C_Y_GLU (agriculture waster burning - input emissions)
      # 5) L122.ghg_tg_R_agr_C_Y_GLU (agriculture crop - input emissions)
      # 6) L112.ghg_tg_R_en_S_F_Yh (energy combustion-related - input emission)

      # Part 0: clean EPA nonCO2 data and define short-cut functions
      #---------------------------------------------------------------------------------------------------------------

      # remove Cameroon industrial process N2O emissions
      # because it is a known data error after communicating with EPA
      EPA_master %<>%
        mutate(value = if_else(country == "Cameroon" & source == "OtherIPPU" & gas == "N2O", 0, value))

      # define three functions to perform EPA scaling processes

      # function (1): isolate EPA emissions for specific sector
      FUN_isolate_EPA_sector <- function(EPA_SECTOR, EPA_SOURCE = NA, use.Source = F){
        # EPA data has source/subsource levels of information
        # for resource, energy etc we scale at EPA source level (Coal, Gas, Oil, Combusion)
        # for agriculture etc we scale at EPA subsource level, e.g. Agricultre-livestocks, Agriculture-rice etc.
        EPA_master %>%
          filter(sector %in% EPA_SECTOR & gas %in% c("CH4", "N2O") & (!use.Source | source %in% EPA_SOURCE)) %>%
          left_join_error_no_match(EPA_CH4N2O_map, by = c("sector", "source", "subsource")) %>%
          left_join_error_no_match(EPA_country_map, by = c("country" = "EPA_country")) %>%
          group_by(GCAM_region_ID, EPA_sector, year, gas) %>%
          summarise(EPA_emissions = sum(value)) %>%
          ungroup() ->
          result

        return(result)
      }

      # function (2): calculate EPA scalers based on current GCAM data and EPA aggregated emission data
      FUN_cal_EPA_scalers <- function(DATA, EPA_DATA){
        DATA %>%
          group_by(GCAM_region_ID, EPA_sector, Non.CO2, year) %>%
          summarise(tot_emissions = sum(emissions)) %>%
          ungroup() %>%
          mutate(tot_emissions = if_else(Non.CO2 == "CH4", tot_emissions * emissions.CH4.GWP.AR4,
                                         tot_emissions * emissions.N2O.GWP.AR4)) %>%
          # using left_join becuase EPA emissions starts from 1990
          left_join(EPA_DATA, by = c("GCAM_region_ID", "EPA_sector", "Non.CO2" = "gas", "year")) %>%
          # safeguard: if GCAM data is missing for some year then directly using EPA data
          mutate(tot_emissions = if_else(is.na(tot_emissions), EPA_emissions, tot_emissions)) %>%
          # EPA emissions are only from 1990 to 2015, and some regions may do not have EPA emissions
          # e.g. oil production emissions in region 16
          # so here need to define rules to make sure values from all years and all regions are scaled
          # 1) for 1990-2015, if EPA emissions are completely missing, then use CEDS values
          mutate(EPA_emissions = if_else(year >= min(emissions.EPA_BAU_HIST_YEAR) & is.na(EPA_emissions), tot_emissions, EPA_emissions)) %>%
          # compute scalers (will generate inf and NAs)
          mutate(emscaler = EPA_emissions / tot_emissions) %>%
          # remove na and inf within EPA covered period
          mutate(emscaler = if_else((is.na(emscaler) | is.infinite(emscaler)) & year >= min(emissions.EPA_BAU_HIST_YEAR), 1, emscaler)) %>%
          # 2) after properly scaled 1990-2015 values (for each year), extrapolate scalers using rule 2
          # using rule 1 will still result in NAs
          # at this step we will not worry about outliers of scalers, which will be properly handled in later processes
          group_by(GCAM_region_ID, EPA_sector, Non.CO2) %>%
          mutate(emscaler = if_else(is.na(emscaler), approx_fun(year, emscaler, rule = 2), emscaler)) %>%
          ungroup() %>%
          select(-EPA_emissions, -tot_emissions)
      }

      # function (3): perform EPA scaling based on the calculated EPA scalers
      FUN_scale_to_EPA <- function(DATA, EPA_SCALER){
        DATA %>%
          left_join_error_no_match(GCAM_EPA_CH4N2O_map,
                                   by = c("supplysector", "subsector", "stub.technology")) %>%
          left_join_error_no_match(EPA_SCALER,
                                   by = c("GCAM_region_ID", "Non.CO2", "year", "EPA_sector")) %>%
          mutate(emissions = emscaler * emissions) %>%
          select(-EPA_sector, -emscaler)
      }

      # Part 1: L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP (resource production emission factors)
      #---------------------------------------------------------------------------------------------------------------


      # 1) Isolate EPA resource production emissions

      L112.EPA_CH4N2O_energy <- FUN_isolate_EPA_sector(EPA_SECTOR = c("Energy"))

      # 2) Calculate scalers for resource emissions by EPA_sector by year and region
      L112.nonco2_tg_R_en_S_F_Yh %>%
        filter(supplysector == "out_resources" & Non.CO2 %in% c("CH4", "N2O")) %>%
        left_join_error_no_match(GCAM_EPA_CH4N2O_map, by = c("supplysector", "subsector", "stub.technology")) %>%
        FUN_cal_EPA_scalers(EPA_DATA = L112.EPA_CH4N2O_energy) ->
        L112.nonco2_tg_R_en_S_F_Yh_EPAscaler

      # 3) Do the actual scaling to obtain historical emissions
      L112.nonco2_tg_R_en_S_F_Yh %>%
        filter(supplysector == "out_resources" & Non.CO2 %in% c("CH4", "N2O")) %>%
        FUN_scale_to_EPA(EPA_SCALER = L112.nonco2_tg_R_en_S_F_Yh_EPAscaler) ->
        L112.nonco2_tg_R_en_S_F_Yh_resource

      # use updated emissions to calculate emission factors based on activity data
      L112.nonco2_tg_R_en_S_F_Yh_resource %>%
        # Group by fuels here to get total value. We do this because, unconventional oil now is a technology within crude oil.
        left_join_error_no_match(L111.Prod_EJ_R_F_Yh %>%
                                   group_by(GCAM_region_ID, year,sector,fuel) %>%
                                   summarise(value = sum(value)) %>%
                                   ungroup() %>%
                                   select(GCAM_region_ID, year,sector,fuel, value) %>%
                                   distinct(),
                                 by = c("GCAM_region_ID", "year", "supplysector" = "sector", "subsector" = "fuel")) %>%
        mutate(value_adj = if_else(value == 0.0  |  is.na(value), 0.0, emissions / value )) %>%
        select(-emissions, -value) ->
        L112.ghg_tgej_R_en_S_F_Yh_adj

      # since this is emission factors, we can directly check outliers through its own distribution
      # obtain global median as replacement
      L112.ghg_tgej_R_en_S_F_Yh_adj_median <- L112.ghg_tgej_R_en_S_F_Yh_adj %>%
        group_by(Non.CO2, year, supplysector, subsector, stub.technology) %>%
        summarise(value_median = median(value_adj)) %>%
        ungroup()

      # check outliers 2 * IQR
      L112.ghg_tgej_R_en_S_F_Yh_adj %>%
        group_by(Non.CO2, year, supplysector, subsector, stub.technology) %>%
        mutate(upper = median(value_adj) + 2 * (quantile(value_adj, 0.75) - quantile(value_adj, 0.25))) %>%
        ungroup() %>%
        left_join_error_no_match(L112.ghg_tgej_R_en_S_F_Yh_adj_median,
                                 by = c("Non.CO2", "year", "supplysector", "subsector", "stub.technology")) %>%
        mutate(value_adj = if_else(value_adj > upper, upper, value_adj)) %>%
        select(-upper, -value_median) ->
        L112.ghg_tgej_R_en_S_F_Yh_adj_noOutlier

      # 4) update L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP for resource production
      L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP %>%
        # produce NA on purpose, since the original data also contain combustion related emission factors
        # we just update resource emission factors
        left_join(L112.ghg_tgej_R_en_S_F_Yh_adj_noOutlier,
                  by = c("GCAM_region_ID", "Non.CO2", "supplysector", "subsector", "stub.technology", "year")) %>%
        mutate(value = if_else(is.na(value_adj), value, value_adj)) %>%
        select(-value_adj) ->
        L112.ghg_tgej_R_en_S_F_Yh_update


      # update the original table
      L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP <- L112.ghg_tgej_R_en_S_F_Yh_update

      # Part 2: L131.nonco2_tg_R_prc_S_S_Yh (industrial processes and urban processes input emissions)

      #---------------------------------------------------------------------------------------------------------------

      # 1) Isolate EPA emissions for industrial processes and urban processes

      L131.EPA_CH4N2O_proc <- FUN_isolate_EPA_sector(EPA_SECTOR = c("Industrial Processes", "Waste"))

      # 2) Calculate scalers for process-related emissions by EPA_sector by year and region
      L131.nonco2_tg_R_prc_S_S_Yh %>%
        filter(supplysector %in% c("industrial processes", "urban processes") & Non.CO2 %in% c("CH4", "N2O")) ->
        L131.nonco2_tg_R_prc_S_S_Yh_change

      L131.nonco2_tg_R_prc_S_S_Yh_change %>%
        rename(emissions = value) %>%
        left_join_error_no_match(GCAM_EPA_CH4N2O_map,
                                 by = c("supplysector", "subsector", "stub.technology")) %>%
        FUN_cal_EPA_scalers(EPA_DATA = L131.EPA_CH4N2O_proc) ->
        L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler

      # 3) Handling outliers, if any scaler is greater than a threshold, will replace its scaler as 1
      # so just keep using CEDS emission
      # for industry process emisison, choose scaler as emissions.EPA.SCALING.THRESHOLD
      # mostly filter out Region 4 - industrial other, and some region 32

      L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler %>%
        mutate(emscaler = if_else(emscaler >= emissions.EPA.SCALING.THRESHOLD, 1, emscaler)) ->
        L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler

      # 4) Do the actual scaling for industrial and urban processes emissions
      L131.nonco2_tg_R_prc_S_S_Yh_change %>%
        rename(emissions = value) %>%
        FUN_scale_to_EPA(EPA_SCALER = L131.nonco2_tg_R_prc_S_S_Yh_EPAscaler) %>%
        rename(value = emissions) ->
        L131.nonco2_tg_R_prc_S_S_Yh_update

      L131.nonco2_tg_R_prc_S_S_Yh %>%
        filter(!(supplysector %in% c("industrial processes", "urban processes") & Non.CO2 %in% c("CH4", "N2O"))) %>%
        bind_rows(L131.nonco2_tg_R_prc_S_S_Yh_update) ->
        L131.nonco2_tg_R_prc_S_S_Yh_adj

      # update the origional dataset
      L131.nonco2_tg_R_prc_S_S_Yh <- L131.nonco2_tg_R_prc_S_S_Yh_adj

      # Part 3: L113.ghg_tg_R_an_C_Sys_Fd_Yh (agriculture livestock input emissions)
      #---------------------------------------------------------------------------------------------------------------

      # 1) Isolate EPA emissions for agriculture livestock emissions
      L131.EPA_CH4N2O_livestocks <- FUN_isolate_EPA_sector(EPA_SECTOR = c("Agriculture"),
                                                           EPA_SOURCE = c("Livestock"),
                                                           use.Source = T)

      # 2) Calculate scalers for livestocks emissions by EPA_sector by year and region
      L113.ghg_tg_R_an_C_Sys_Fd_Yh %>%
        filter(Non.CO2 %in% c("CH4_AGR", "N2O_AGR")) ->
        L113.ghg_tg_R_an_C_Sys_Fd_Yh_change

      L113.ghg_tg_R_an_C_Sys_Fd_Yh_change %>%
        mutate(Non.CO2 = substr(Non.CO2, 1, 3),
               EPA_sector = "Agriculture") %>%
        rename(emissions = value) %>%
        FUN_cal_EPA_scalers(EPA_DATA = L131.EPA_CH4N2O_livestocks) %>%
        mutate(Non.CO2 = paste0(Non.CO2, "_AGR")) ->
        L113.ghg_tg_R_an_C_Sys_Fd_Yh_EPAscaler

      # 3) do the actual scaling for livestock-related emissions
      L113.ghg_tg_R_an_C_Sys_Fd_Yh_change %>%
        mutate(EPA_sector = "Agriculture") %>%
        left_join_error_no_match(L113.ghg_tg_R_an_C_Sys_Fd_Yh_EPAscaler,
                                 by = c("GCAM_region_ID", "Non.CO2", "year", "EPA_sector")) %>%
        mutate(value = value * emscaler) %>%
        select(-EPA_sector, -emscaler) ->
        L113.ghg_tg_R_an_C_Sys_Fd_Yh_update

      L113.ghg_tg_R_an_C_Sys_Fd_Yh %>%
        filter(!(Non.CO2 %in% c("CH4_AGR", "N2O_AGR"))) %>%
        bind_rows(L113.ghg_tg_R_an_C_Sys_Fd_Yh_update) ->
        L113.ghg_tg_R_an_C_Sys_Fd_Yh_adj

      # update the origional dataset
      L113.ghg_tg_R_an_C_Sys_Fd_Yh <- L113.ghg_tg_R_an_C_Sys_Fd_Yh_adj

      # Part 4: L121.nonco2_tg_R_awb_C_Y_GLU (agriculture waster burning)
      #---------------------------------------------------------------------------------------------------------------

      # 1) isolate EPA emissions for agriculture waster burning emissions
      L121.EPA_CH4N2O_awb <- FUN_isolate_EPA_sector(EPA_SECTOR = c("Agriculture"),
                                                    EPA_SOURCE = c("OtherAg"),
                                                    use.Source = T)

      # 2) Calculate scalers for agriculture emissions by EPA_sector by year and region
      L121.nonco2_tg_R_awb_C_Y_GLU %>%
        filter(Non.CO2 %in% c("CH4_AWB", "N2O_AWB")) ->
        L121.nonco2_tg_R_awb_C_Y_GLU_change

      L121.nonco2_tg_R_awb_C_Y_GLU_change  %>%
        mutate(Non.CO2 = substr(Non.CO2, 1, 3),
               EPA_sector = "Agriculture") %>%
        rename(emissions = value) %>%
        FUN_cal_EPA_scalers(EPA_DATA = L121.EPA_CH4N2O_awb) %>%
        mutate(Non.CO2 = paste0(Non.CO2, "_AWB")) ->
        L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler

      # 3) Handling outliers, if any scaler is greater than a threshold, will replace its scaler as 1
      # so just keep using CEDS emission
      # for agriculture waste burning, choose scaler as emissions.EPA.SCALING.THRESHOLD
      # just filter out Region 26

      L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler %>%
        mutate(emscaler = if_else(emscaler >= emissions.EPA.SCALING.THRESHOLD, 1, emscaler)) ->
        L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler

      # 4) do the actual scaling for agriculture emissions
      L121.nonco2_tg_R_awb_C_Y_GLU_change %>%
        mutate(EPA_sector = "Agriculture") %>%
        left_join_error_no_match(L121.nonco2_tg_R_awb_C_Y_GLU_EPAscaler,
                                 by = c("GCAM_region_ID", "Non.CO2", "year", "EPA_sector")) %>%
        mutate(value = value * emscaler) %>%
        select(-EPA_sector, -emscaler) ->
        L121.nonco2_tg_R_awb_C_Y_GLU_update

      L121.nonco2_tg_R_awb_C_Y_GLU %>%
        filter(!(Non.CO2 %in% c("CH4_AWB", "N2O_AWB"))) %>%
        bind_rows(L121.nonco2_tg_R_awb_C_Y_GLU_update) ->
        L121.nonco2_tg_R_awb_C_Y_GLU_adj

      # update the origional dataset
      L121.nonco2_tg_R_awb_C_Y_GLU <- L121.nonco2_tg_R_awb_C_Y_GLU_adj


      # Part 5: L122.ghg_tg_R_agr_C_Y_GLU (agriculture rice - input emissions)
      #---------------------------------------------------------------------------------------------------------------

      # 1) Isolate EPA emissions for agriculture rice emissions
      L131.EPA_CH4N2O_agr <- FUN_isolate_EPA_sector(EPA_SECTOR = c("Agriculture"),
                                                    EPA_SOURCE = c("Rice", "AgSoils"),
                                                    use.Source = T)

      # 2) Calculate scalers for agriculture emissions by EPA_sector by year and region
      L122.ghg_tg_R_agr_C_Y_GLU %>%
        filter(Non.CO2 %in% c("CH4_AGR", "N2O_AGR")) ->
        L122.ghg_tg_R_agr_C_Y_GLU_change

      L122.ghg_tg_R_agr_C_Y_GLU_change %>%
        mutate(Non.CO2 = substr(Non.CO2, 1, 3),
               EPA_sector = "Agriculture") %>%
        rename(emissions = value) %>%
        FUN_cal_EPA_scalers(EPA_DATA = L131.EPA_CH4N2O_agr) %>%
        mutate(Non.CO2 = paste0(Non.CO2, "_AGR")) ->
        L122.ghg_tg_R_agr_C_Y_GLU_EPAscaler

      # 3) do the actual scaling for agriculture crop emissions
      L122.ghg_tg_R_agr_C_Y_GLU_change %>%
        mutate(EPA_sector = "Agriculture") %>%
        left_join_error_no_match(L122.ghg_tg_R_agr_C_Y_GLU_EPAscaler,
                                 by = c("GCAM_region_ID", "year", "Non.CO2", "EPA_sector")) %>%
        mutate(value = value * emscaler) %>%
        select(-EPA_sector, -emscaler) ->
        L122.ghg_tg_R_agr_C_Y_GLU_update

      L122.ghg_tg_R_agr_C_Y_GLU %>%
        filter(!(Non.CO2 %in% c("CH4_AGR", "N2O_AGR"))) %>%
        bind_rows(L122.ghg_tg_R_agr_C_Y_GLU_update) ->
        L122.ghg_tg_R_agr_C_Y_GLU_adj

      # update the origional dataset
      L122.ghg_tg_R_agr_C_Y_GLU <- L122.ghg_tg_R_agr_C_Y_GLU_adj

      # Part 6: L112.ghg_tg_R_en_S_F_Yh (energy combustion-related - input emission)
      #---------------------------------------------------------------------------------------------------------------
      # L112.EPA_CH4N2O_energy already contains combustion-related emissions processed from EPA

      # 1) Calculate scalers for combustion-related emissions by EPA_sector by year and region
      L112.ghg_tg_R_en_S_F_Yh %>%
        left_join_error_no_match(GCAM_EPA_CH4N2O_map,
                                 by = c("supplysector", "subsector", "stub.technology")) %>%
        rename(emissions = value) %>%
        FUN_cal_EPA_scalers(EPA_DATA = L112.EPA_CH4N2O_energy) ->
        L112.ghg_tg_R_en_S_F_Yh_EPAscaler

      # 3) Handling outliers, if any scaler is greater than a threshold, will replace its scaler as 1
      # so just keep using CEDS emission
      # for combustion, choose scaler as emissions.EPA.SCALING.THRESHOLD.COMBUSTION
      # becuase EPA only has the entire combustion sector as a whole catagory
      # mostly filter out Region 4, 5, 27

      L112.ghg_tg_R_en_S_F_Yh_EPAscaler %>%
        mutate(emscaler = if_else(emscaler >= emissions.EPA.SCALING.THRESHOLD.COMBUSTION, 1, emscaler)) ->
        L112.ghg_tg_R_en_S_F_Yh_EPAscaler

      # 4) Do the actual scaling for combustion-related emissions
      L112.ghg_tg_R_en_S_F_Yh %>%
        rename(emissions = value) %>%
        FUN_scale_to_EPA(EPA_SCALER = L112.ghg_tg_R_en_S_F_Yh_EPAscaler) %>%
        rename(value = emissions) ->
        L112.ghg_tg_R_en_S_F_Yh_adj

      L112.ghg_tg_R_en_S_F_Yh <- L112.ghg_tg_R_en_S_F_Yh_adj

      # END OF EPA SCALING PROCESS
      # -------------------------------------------------------------------------------------------------------------
    }

    # ===============
    # Produce outputs
    L111.nonghg_tg_R_en_S_F_Yh %>%
      na.omit() %>%
      add_title("Non-ghg emission totals by GCAM sector, fuel, technology, and driver type for CEDS historical years.") %>%
      add_units("Tg") %>%
      add_comments("Compute unscaled non-ghg emissions by country and technology, and CEDS emissions by region and sector.") %>%
      add_comments("and finally calculate non-ghg emission totals by GCAM sector, fuel, technology, and driver type for CEDS historical years.") %>%
      add_legacy_name("L111.nonghg_tg_R_en_S_F_Yh") %>%
      add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F", "L102.ceds_int_shipping_nonco2_tg_S_F", "emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                     "common/iso_GCAM_regID","energy/mappings/UCD_techs","energy/calibrated_techs","energy/calibrated_techs_bld_det",
                     "emissions/mappings/Trn_subsector","emissions/CEDS/CEDS_sector_tech_combustion","emissions/mappings/Trn_subsector_revised",
                     "emissions/mappings/CEDS_sector_tech_proc","emissions/mappings/calibrated_outresources","emissions/mappings/CEDS_sector_tech_proc_revised",
                     "L101.in_EJ_R_en_Si_F_Yh", "L1326.in_EJ_R_indenergy_F_Yh", "L1323.in_EJ_R_iron_steel_F_Y", "L1324.in_EJ_R_Off_road_F_Y",
                     "L1325.in_EJ_R_chemical_F_Y", "L1326.in_EJ_R_aluminum_Yh","emissions/CEDS/CEDS_sector_tech_combustion_revised",
                     "emissions/mappings/UCD_techs_emissions_revised","L154.IEA_histfut_data_times_UCD_shares",
                     "emissions/CEDS/gains_iso_sector_emissions","emissions/CEDS/gains_iso_fuel_emissions",
                     "L270.nonghg_tg_state_refinery_F_Yb", "gcam-usa/emissions/BC_OC_assumptions", "gcam-usa/emissions/BCOC_PM25_ratios") ->
      L111.nonghg_tg_R_en_S_F_Yh

    L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP %>%
      na.omit() %>%
      add_title("Non-ghg emission total shares by GCAM sector, fuel, technology, and driver type for CEDS historical years.") %>%
      add_units("Tg/EJ") %>%
      add_comments("Use non-ghg emission totals by GCAM sector, fuel, technology, and driver type for CEDS historical years to derive emission shares.") %>%
      add_legacy_name("L111.nonghg_tgej_R_en_S_F_Yh") %>%
      same_precursors_as("L111.nonghg_tg_R_en_S_F_Yh") ->
      L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP

    L112.ghg_tg_R_en_S_F_Yh %>%
      add_title("GHG emissions by energy sector, gas, region, and historical year") %>%
      add_units("Tg") %>%
      add_comments("Emissions calculated with CEDS totals") %>%
      add_legacy_name("L112.ghg_tg_R_en_S_F_Yh") %>%
      add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","L102.ceds_int_shipping_nonco2_tg_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                     "common/iso_GCAM_regID","energy/mappings/UCD_techs","energy/calibrated_techs","energy/calibrated_techs_bld_det",
                     "emissions/mappings/Trn_subsector","emissions/CEDS/CEDS_sector_tech_combustion","emissions/mappings/calibrated_outresources",
                     "L101.in_EJ_R_en_Si_F_Yh", "L1326.in_EJ_R_indenergy_F_Yh", "L1323.in_EJ_R_iron_steel_F_Y", "L1324.in_EJ_R_Off_road_F_Y",
                     "L1325.in_EJ_R_chemical_F_Y", "L1326.in_EJ_R_aluminum_Yh","emissions/mappings/Trn_subsector_revised",
                     "L101.in_EJ_R_en_Si_F_Yh", "emissions/mappings/Trn_subsector_revised", "emissions/EPA/EPA_2019_raw", "emissions/EPA_CH4N2O_map",
                     "emissions/CEDS/CEDS_sector_tech_combustion_revised","emissions/mappings/UCD_techs_emissions_revised","L154.IEA_histfut_data_times_UCD_shares",
                     "emissions/CEDS/gains_iso_sector_emissions","emissions/CEDS/gains_iso_fuel_emissions",
                     "L270.nonghg_tg_state_refinery_F_Yb", "gcam-usa/emissions/BC_OC_assumptions", "gcam-usa/emissions/BCOC_PM25_ratios") ->
      L112.ghg_tg_R_en_S_F_Yh

    L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP %>%
      add_title("GHG emissions factors by energy sector, gas, region, and historical year") %>%
      add_units("Tg/EJ") %>%
      add_comments("Emissions calculated with CEDS emissions factors.") %>%
      add_comments("Then, emissions factors computed by dividing calculated emissions by energy data") %>%
      add_legacy_name("L112.ghg_tgej_R_en_S_F_Yh") %>%
      add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F",
                     "L102.ceds_int_shipping_nonco2_tg_S_F",
                     "emissions/CEDS/ceds_sector_map",
                     "emissions/CEDS/ceds_fuel_map",
                     "common/GCAM_region_names",
                     "common/iso_GCAM_regID",
                     "energy/mappings/UCD_techs",
                     "energy/calibrated_techs",
                     "energy/calibrated_techs_bld_det",
                     "emissions/mappings/Trn_subsector",
                     "emissions/CEDS/CEDS_sector_tech_combustion",
                     "emissions/mappings/calibrated_outresources",
                     "emissions/mappings/Trn_subsector_revised",
                     "L101.in_EJ_R_en_Si_F_Yh",
                     "L1326.in_EJ_R_indenergy_F_Yh",
                     "L1323.in_EJ_R_iron_steel_F_Y",
                     "L1324.in_EJ_R_Off_road_F_Y",
                     "L1325.in_EJ_R_chemical_F_Y",
                     "L1326.in_EJ_R_aluminum_Yh",
                     "emissions/EPA/EPA_2019_raw",
                     "emissions/EPA_CH4N2O_map",
                     "L111.Prod_EJ_R_F_Yh",
                     "emissions/EPA_country_map",
                     "emissions/CEDS/CEDS_sector_tech_combustion_revised",
                     "emissions/mappings/UCD_techs_emissions_revised",
                     "L270.nonghg_tg_state_refinery_F_Yb",
                     "gcam-usa/emissions/BC_OC_assumptions",
                     "gcam-usa/emissions/BCOC_PM25_ratios") ->
      L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP

    L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy %>%
      add_title("GCAM energy by GCAM region / sector / subsector / technology / historical year also mapped to CEDS sector / fuel") %>%
      add_units("EJ") %>%
      add_comments("Intermediate input that will be used to calculate new EFs for resource production in the USA in L273.nonghg_refinery_USA") %>%
      add_legacy_name("L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy") %>%
      add_precursors("L101.in_EJ_R_en_Si_F_Yh",
                     "energy/mappings/UCD_techs",
                     "energy/calibrated_techs_bld_det",
                     "emissions/mappings/Trn_subsector",
                     "emissions/CEDS/CEDS_sector_tech_combustion",
                     "emissions/CEDS/CEDS_sector_tech_combustion_revised") ->
      L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy

    L113.ghg_tg_R_an_C_Sys_Fd_Yh %>%
      add_title("Animal GHG emissions (CH4 and N2O) by GCAM region / sector / technology / historical year") %>%
      add_units("Tg") %>%
      add_comments("First: compute unscaled emissions by country and technology") %>%
      add_comments("Second: match in emissions factors from EPA") %>%
      add_comments("Third: compute unscaled emissions (production * emfactors) and aggregate by sector and region") %>%
      add_comments("Fourth: compute CEDS emissions by region and sector") %>%
      add_comments("Fifth: scale EPA emissions by tech to match CEDS") %>%
      add_legacy_name("L113.ghg_tg_R_an_C_Sys_Fd_Yh") %>%
      add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","L102.ceds_int_shipping_nonco2_tg_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                     "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech_combustion", "emissions/CEDS/CEDS_sector_tech_combustion_revised",
                     "emissions/mappings/CEDS_sector_tech_proc", "L107.an_Prod_Mt_R_C_Sys_Fd_Y","emissions/mappings/CEDS_sector_tech_proc_revised",
                     "L103.ghg_tgmt_USA_an_Sepa_F_2005") ->
      L113.ghg_tg_R_an_C_Sys_Fd_Yh


    L115.nh3_tg_R_an_C_Sys_Fd_Yh %>%
      add_title(" Animal NH3 emissions by GCAM region / sector / technology / historical year") %>%
      add_units("Tg") %>%
      add_comments("Annual animal NH3 emissions is computed using CEDS emissions and FAO animal production.") %>%
      add_legacy_name("L115.nh3_tg_R_an_C_Sys_Fd_Yh") %>%
      add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","L102.ceds_int_shipping_nonco2_tg_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                     "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech_combustion","emissions/CEDS/CEDS_sector_tech_combustion_revised",
                     "L107.an_Prod_Mt_R_C_Sys_Fd_Y","L107.an_Prod_Mt_R_C_Sys_Fd_Y") ->
      L115.nh3_tg_R_an_C_Sys_Fd_Yh

    L121.AWBshare_R_C_GLU %>%
      add_title("Ag waste burning share of emissions by GCAM region / commodity / GLU / historical year") %>%
      add_units("unitless share") %>%
      add_comments("Agricultural waste burning emissions by region are assigned to crops and land use regions on the basis of excess dry biomass...") %>%
      add_comments("estimated from production, harvest index, and water content") %>%
      add_legacy_name("L121.AWBshare_R_C_Y_GLU") %>%
      add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                     "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech_combustion","emissions/CEDS/CEDS_sector_tech_combustion_revised",
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
                     "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech_combustion","emissions/CEDS/CEDS_sector_tech_combustion_revised",
                     "L101.ag_Prod_Mt_R_C_Y_GLU","L111.ag_resbio_R_C","L122.LC_bm2_R_HarvCropLand_C_Yh_GLU") ->
      L122.EmissShare_R_C_Y_GLU

    L122.ghg_tg_R_agr_C_Y_GLU %>%
      add_title("Agriculture emissions by GCAM region, commodity, GLU, and historical year") %>%
      add_units("Tg") %>%
      add_comments("CEDS emissions shared out by crop production") %>%
      add_legacy_name("L122.ghg_tg_R_agr_C_Y_GLU") %>%
      add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                     "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech_combustion","emissions/CEDS/CEDS_sector_tech_combustion_revised",
                     "L124.LC_bm2_R_Grass_Yh_GLU_adj","L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj") ->
      L122.ghg_tg_R_agr_C_Y_GLU


    L124.nonco2_tg_R_grass_Y_GLU %>%
      add_title("Grassland fire emissions by GCAM region, gas, and historical year") %>%
      add_units("Tg/yr") %>%
      add_comments("CEDS-GFED grassland emissions are downscaled to GLU using shares of grassland area.") %>%
      add_legacy_name("L124.nonco2_tg_R_grass_Y_GLU") %>%
      add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                     "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech_combustion",
                     "L124.LC_bm2_R_Grass_Yh_GLU_adj") ->
      L124.nonco2_tg_R_grass_Y_GLU

    L125.bcoc_tgbkm2_R_grass_2000 %>%
      add_title("Grassland fire emissions for BCOC by GCAM region, gas, and historical year") %>%
      add_units("Tg/yr") %>%
      add_comments("CEDS grassland emissions are downscaled to GLU using shares of grassland area.") %>%
      add_legacy_name("L124.nonco2_tg_R_grass_Y_GLU") %>%
      add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                     "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech_combustion",
                     "L124.LC_bm2_R_Grass_Yh_GLU_adj")->L125.bcoc_tgbkm2_R_grass_2000

    L125.bcoc_tgbkm2_R_forest_2000 %>%
      add_title("Forest fire emissions for BCOC by GCAM region, gas, and historical year") %>%
      add_units("Tg/yr") %>%
      add_comments("CEDS grassland emissions are downscaled to GLU using shares of grassland area.") %>%
      add_legacy_name("L124.nonco2_tg_R_grass_Y_GLU") %>%
      add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                     "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech_combustion","emissions/CEDS/CEDS_sector_tech_combustion_revised",
                     "L124.LC_bm2_R_Grass_Yh_GLU_adj")->L125.bcoc_tgbkm2_R_forest_2000

    L125.deforest_coefs_bcoc %>%
      add_title("Forest fire emissions for BCOC by GCAM region, gas, and historical year") %>%
      add_units("Tg/yr") %>%
      add_comments("CEDS grassland emissions are downscaled to GLU using shares of grassland area.") %>%
      add_legacy_name("L124.nonco2_tg_R_grass_Y_GLU") %>%
      add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                     "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech_combustion","emissions/CEDS/CEDS_sector_tech_combustion_revised",
                     "L124.LC_bm2_R_Grass_Yh_GLU_adj")->L125.deforest_coefs_bcoc

    L124.nonco2_tg_R_forest_Y_GLU %>%
      add_title("Forest fire and deforestation emissions by GCAM region, gas, and historical year") %>%
      add_units("Tg/yr") %>%
      add_comments("CEDS forest emissions are downscaled to GLU using shares of forest area.") %>%
      add_comments("These emissions are then separated into forest fire and deforestation using GFED data.") %>%
      add_legacy_name("L124.nonco2_tg_R_forest_Y_GLU") %>%
      add_precursors("L102.ceds_GFED_nonco2_tg_R_S_F","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                     "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech_combustion","emissions/CEDS/CEDS_sector_tech_combustion_revised",
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
                     "common/iso_GCAM_regID","emissions/CEDS/CEDS_sector_tech_combustion","emissions/CEDS/CEDS_sector_tech_combustion_revised",
                     "emissions/EPA_FCCC_IndProc_2005", "emissions/EPA/EPA_2019_raw", "emissions/EPA_CH4N2O_map",
                     "emissions/GCAM_EPA_CH4N2O_energy_map"
      ) ->
      L131.nonco2_tg_R_prc_S_S_Yh

    return_data(L111.nonghg_tg_R_en_S_F_Yh, L111.nonghg_tgej_R_en_S_F_Yh_infered_combEF_AP, L112.ghg_tg_R_en_S_F_Yh, L112.ghg_tgej_R_en_S_F_Yh_infered_combEF_AP, L112.in_EJ_R_en_S_F_Yh_calib_all_baseenergy, L113.ghg_tg_R_an_C_Sys_Fd_Yh, L115.nh3_tg_R_an_C_Sys_Fd_Yh, L121.nonco2_tg_R_awb_C_Y_GLU,
                L121.AWBshare_R_C_Y_GLU, L122.ghg_tg_R_agr_C_Y_GLU, L122.EmissShare_R_C_Y_GLU, L124.nonco2_tg_R_grass_Y_GLU, L124.nonco2_tg_R_forest_Y_GLU, L124.deforest_coefs,
                L131.nonco2_tg_R_prc_S_S_Yh,L125.bcoc_tgbkm2_R_grass_2000,L125.bcoc_tgbkm2_R_forest_2000,L125.deforest_coefs_bcoc)
  } else {
    stop("Unknown command")
  }
}
