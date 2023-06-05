# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1323.iron_steel
#'
#' Sets up input, output, and IO coefficients for iron and steel and subtracts input energy from industry energy use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1323.out_Mt_R_iron_steel_Yh}, \code{L1323.IO_GJkg_R_iron_steel_F_Yh}, \code{L1323.in_EJ_R_iron_steel_F_Y}, \code{L1323.in_EJ_R_indenergy_F_Yh}, \code{L1323.SubsectorInterp_iron_steel}. The corresponding file in the
#' original data system was \code{LA1323.iron_steel.R} (energy level1).
#' @details This chunk generates input, output, IO coefficients, and subsector shareweights for the iron and steel sector. It begins by downscaling Worrell regional data from 1994
#' to set up process emissions factors that are multiplied by country emissions from CDIAC to determine production. Limestone consumption is calculated from the same downscaling.
#' IEA fuelshares and heat and electricity are used to determine energy use by fuel. Energy inputs are then subtracted from industrial energy use and any resulting negative values
#' are dealt with by moving their accounting to the iron and steel sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select semi_join summarise summarise_all
#' @importFrom tidyr gather spread
#' @author Yang Liu Sep 2019, Siddarth Durga April 2023
module_energy_L1323.iron_steel <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/steel_prod_process",
             FILE = "energy/steel_intensity",
             FILE = "energy/WSA_direct_reduced_iron_2008_2019.csv",
             FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             FILE = "energy/A323.subsector_interp",
             "L1012.en_bal_EJ_R_Si_Fi_Yh",
             "L1322.in_EJ_R_indenergy_F_Yh",
             "LB1092.Tradebalance_iron_steel_Mt_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1323.out_Mt_R_iron_steel_Yh",
             "L1323.IO_GJkg_R_iron_steel_F_Yh",
             "L1323.in_EJ_R_iron_steel_F_Y",
             "L1323.in_EJ_R_indenergy_F_Yh",
             "L1323.SubsectorInterp_iron_steel"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    raw <- subsector <- minicam.energy.input <- Country <- sector <-
      share <- value <- iron_steel <- year <- value.y <- value.x <- iso <- unit_prod <-
      GCAM_region_ID <- fuel <- industry <- output <- energy_use <- scalar <- coefficient <-
      Unit <- technology <- '2008' <- '2009' <- '2010' <-'2011' <- '2012' <- '2013' <- '2014' <- '2015' <-
      '2016' <- '2017' <- '2018' <- '2019' <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    #All_steel <- get_data(all_data, "energy/steel_prod", strip_attributes = TRUE)
    All_steel <- get_data(all_data, "energy/steel_prod_process", strip_attributes = TRUE)
    DRI_stats <- get_data(all_data, "energy/WSA_direct_reduced_iron_2008_2019.csv", strip_attributes = TRUE)
    A323.subsector_interp <- get_data(all_data, "energy/A323.subsector_interp", strip_attributes = TRUE)
    steel_intensity <- get_data(all_data, "energy/steel_intensity", strip_attributes = TRUE)
    L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh", strip_attributes = TRUE)
    LB1092.Tradebalance_iron_steel_Mt_R_Y <- get_data(all_data, "LB1092.Tradebalance_iron_steel_Mt_R_Y", strip_attributes = TRUE)
    L1012.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")

    #Estimate DRI (direct reduced iron) consumption from country-wise WSA DRI production, imports, and exports data
    DRI_stats %>%
      gather(year,value,-metric,-country_name)%>%
      spread(metric,value)%>%
      replace(is.na(.), 0) %>%
      mutate(year = as.numeric(year),
             DRI_consumption=`DRI production`- `DRI exports` + `DRI imports`,
             DRI_consumption=ifelse(DRI_consumption<0,0,DRI_consumption))%>%
      select(country_name,year,DRI_consumption)-> DRI_stats

    #Calculate EAF-Scrap and EAF-DRI based steel production using DRI consumption data
    All_steel %>%
      left_join(DRI_stats,by=c("country_name","year")) %>%
      replace(is.na(.), 0) %>%
      rename(`EAF with DRI`=DRI_consumption) %>%
      mutate(`EAF with scrap`=EAF-`EAF with DRI`,
             `EAF with DRI`=ifelse(`EAF with scrap`<=0,EAF,`EAF with DRI`),
             `EAF with scrap`=ifelse(`EAF with scrap`<0,0,`EAF with scrap`))%>%
      select(-EAF)%>%
      left_join(iso_GCAM_regID,by="country_name")%>%
      select(-GCAM_region_ID,-country_name,-region_GCAM3)-> All_steel


    # ===================================================
    # 2. Perform computations
    # ===================================================
    # Recalculate steel production by technology across years to be consistent
    # with the iron and steel trade balance (consumption = production - exports + imports)
    # Change steel production to long format and aggregate to region level
    All_steel %>%
      left_join(iso_GCAM_regID, by = "iso") %>%
      #aggregate the production to regional level
      group_by(GCAM_region_ID, year) %>%
      summarise(BLASTFUR=sum(BLASTFUR),`EAF with scrap`=sum(`EAF with scrap`),
                `EAF with DRI`=sum(`EAF with DRI`))-> All_steel

      #Obtain the index of GCAM_regions and sub sectors that are calibrated to zero steel production in the base-year
      All_steel %>%
        filter(year==MODEL_FINAL_BASE_YEAR & (`EAF with scrap`==0| BLASTFUR==0 | `EAF with DRI`==0)) %>%
        left_join(GCAM_region_names,by=c("GCAM_region_ID"))-> L1323.index

      #add a minimal steel production value (0.5% of the total) to technologies in the base-year where they are calibrated to zero
      All_steel %>%
        mutate(BLASTFUR=ifelse(BLASTFUR==0 & year == MODEL_FINAL_BASE_YEAR,(BLASTFUR+`EAF with scrap`+`EAF with DRI`)*0.005,BLASTFUR),
               `EAF with scrap`=ifelse(`EAF with scrap`==0 & year == MODEL_FINAL_BASE_YEAR,(BLASTFUR+`EAF with scrap`+`EAF with DRI`)*0.005,`EAF with scrap`),
               `EAF with DRI`=ifelse(`EAF with DRI`==0 & year == MODEL_FINAL_BASE_YEAR,(BLASTFUR+`EAF with scrap`+`EAF with DRI`)*0.005,`EAF with DRI`),
               #calculate the percentage of BLASTFUR, EAF with scrap, and EAF with DRI across regions and years
               BLASTFUR_pct=BLASTFUR/(BLASTFUR+`EAF with scrap`+`EAF with DRI`),
               EAF_scrap_pct=`EAF with scrap`/(BLASTFUR+`EAF with scrap`+`EAF with DRI`),
               EAF_DRI_pct=`EAF with DRI`/(BLASTFUR+`EAF with scrap`+`EAF with DRI`)) %>%
        #join the WSA total steel production data from LB1092.Tradebalance_iron_steel_Mt_R_Y
        left_join(GCAM_region_names,by="GCAM_region_ID") %>%
        left_join(LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
                    filter(metric == "production") %>%
                    rename(region=GCAM_region,production=value),by=c("region","year")) %>%
        #recalculate the steel production by technologies and regions
        #For example, steel produced from BLASTFUR is equal to WSA total production multiplied by percent BLASTFUR
        mutate(BLASTFUR=BLASTFUR_pct*(1/CONV_KT_MT)*production,
               `EAF with scrap`=EAF_scrap_pct*(1/CONV_KT_MT)*production,
               `EAF with DRI`=EAF_DRI_pct*(1/CONV_KT_MT)*production) %>%
        ungroup()%>%
        select(GCAM_region_ID,year,BLASTFUR,`EAF with scrap`,`EAF with DRI`)%>%
        #convert from wide to long
        gather(subsector,value,-year,-GCAM_region_ID) %>%
        #convert unit from kt to mt
        mutate(value = value * CONV_KT_MT) %>%
        select(GCAM_region_ID, year,subsector, value) -> L1323.out_Mt_R_iron_steel_Yh

   # L2323.SubsectorInterp_iron_steel: Subsector shareweight interpolation of iron and steel sector
      A323.subsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) ->
        L1323.SubsectorInterp_iron_steel

   # linearly interpolate the share weights to 1 by 2100 for GCAM_regions and sub sectors that were calibrated to zero in the base-year
   # It is important to linearly interpolate these sub sector share weights to 1 since we add a negligible steel production to these sub sectors in the base-year for future growth (line 78),
   # which leads to incredibly low share-weights in the base-year and therefore no future expansion
      L1323.SubsectorInterp_iron_steel$to.year[which(L1323.SubsectorInterp_iron_steel$region %in% unique((L1323.index %>%
                                                                   filter(BLASTFUR == 0 |`EAF with scrap`== 0 |`EAF with DRI` == 0))$region))] <- 2100

      L1323.SubsectorInterp_iron_steel$interpolation.function[which(L1323.SubsectorInterp_iron_steel$region %in% unique((L1323.index %>%
                                                                                                            filter(BLASTFUR == 0 |`EAF with scrap`== 0 |`EAF with DRI` == 0))$region))] <- "linear"
    # Get steel energy use from IEA energy balances
    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(grepl("steel", sector)) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(sector = "iron and steel") ->
      en_steel

    # Map fuel in iron and steel sector
    en_steel %>%
      left_join(select(enduse_fuel_aggregation, fuel, industry), by = "fuel") %>%
      select(-fuel, fuel = industry) %>%
      na.omit() %>%
      group_by(GCAM_region_ID, year, sector, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      en_steel

    # Calculate bottom-up energy consumption = production * intensity from literature
    L1323.out_Mt_R_iron_steel_Yh %>%
      rename(output = value) %>%
      left_join(steel_intensity %>% select(-subsector,-ratio), by = c("subsector"="technology")) %>%
      mutate(value = value * CONV_GJ_EJ / CONV_T_MT,
                    energy_use = output * value,
                    unit = "EJ") ->
      Intensity_literature

    # Scaler: IEA's estimates of fuel consumption divided by bottom-up estimate of energy consumption
    Intensity_literature %>%
      group_by(GCAM_region_ID, year, fuel) %>%
      dplyr::summarise(energy_use = sum(energy_use)) %>%
      ungroup() %>%
      left_join(en_steel %>% select(GCAM_region_ID, year, fuel, value),by = c("GCAM_region_ID","fuel",  "year"))%>%
      mutate(value = replace_na(value,0), #replace NA IEA data with zero
             value= if_else(value == 0 & energy_use > 0, energy_use, value), #if bottom-up calculation is non-zero and IEA value is zero, then set IEA value = bottom-up value
             scalar = replace_na(value / energy_use, 1), #calculate scalar = IEA data/bottom-up data, if NA replace scaler = 1
             scalar = if_else(energy_use == 0 & value > 0, 1, scalar),  #if IEA data is non-zero, but bottom-up data is zero; set scaler = 1
             scalar = if_else(scalar>=6,1,scalar), #if IEA data is 6 times higher or lower than bottom-up calculation; then do not scale the results (i.e., scaler = 1)
             scalar = if_else(scalar<=0.16,1,scalar)) -> Scaler




    # Intensity scaled = Intensity from the literature times scaler.
    Intensity_literature %>%
      left_join(Scaler %>% select(GCAM_region_ID, year, fuel, scalar),by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(coefficient = value * scalar) %>%
      select(GCAM_region_ID, year, subsector, fuel, coefficient, Unit) ->
      Intensity_scaled

    IO_iron_steel <- steel_intensity %>%
      select(subsector, technology, fuel,ratio) %>%
      distinct() %>%
      mutate(sector = "iron and steel") %>%
      repeat_add_columns(select(iso_GCAM_regID, GCAM_region_ID) %>% distinct(GCAM_region_ID)) %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      left_join(Intensity_scaled, by = c("GCAM_region_ID", "year", "subsector", "fuel")) %>%
      na.omit %>%
      mutate(coefficient=coefficient*ratio)%>%
      select(-ratio)


     # Use IO to calculate energy input
    L1323.out_Mt_R_iron_steel_Yh %>%
      mutate(technology = subsector) %>%
      left_join(IO_iron_steel, by = c("subsector","technology","year","GCAM_region_ID")) %>%
      mutate(value = value * coefficient) %>%
      select(GCAM_region_ID, supplysector = "sector", year, subsector, technology, fuel, "value") ->
      L1323.in_EJ_R_iron_steel_F_Y

	  IO_iron_steel %>%
      select(GCAM_region_ID, year, supplysector = "sector", subsector, technology, fuel, coefficient) ->
      L1323.IO_GJkg_R_iron_steel_F_Yh

	# Subtract iron and steel energy use from other industrial energy use
    L1322.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1323.in_EJ_R_iron_steel_F_Y %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value,0),
             value = raw - value , raw = NULL) ->
      L1323.in_EJ_R_indenergy_F_Yh_tmp

    L1323.in_EJ_R_indenergy_F_Yh_tmp %>%
      mutate(value = if_else(value > 0 , value, 0)) ->
      L1323.in_EJ_R_indenergy_F_Yh

    #Adjust negative energy use

    # Identify rows with negative energy use
    L1323.in_EJ_R_indenergy_F_Yh_tmp %>%
      filter(value < 0) %>%
      select(-sector) ->
      negative

    # revise IO coefficients to zero for rows with negative energy use
    L1323.IO_GJkg_R_iron_steel_F_Yh %>%
      left_join(negative,by = c("GCAM_region_ID", "year", "fuel")) %>%
      mutate(coefficient = if_else(replace_na(value, 0) < 0, 0, coefficient),value = NULL) ->
      L1323.IO_GJkg_R_iron_steel_F_Yh

    #Recalculate

    # Recalculate the input steel energy with revised IO coefficients
    L1323.out_Mt_R_iron_steel_Yh %>%
      # 10/11/2019 gpk modification: in order to avoid assigning output (and energy consumption) to technologies that do
      # not exist in the base years, we specify a "technology" column which is equal to the subsector. Note that this
      # method assumes that the techs with market share in the base years have the same name as their parent subsectors
      mutate(technology = subsector) %>%
      left_join(L1323.IO_GJkg_R_iron_steel_F_Yh, by = c("subsector", "technology", "GCAM_region_ID", "year")) %>%
      mutate(value = value * coefficient) %>%
      select(GCAM_region_ID, year, subsector, technology, fuel, value) ->
      L1323.in_EJ_R_iron_steel_F_Y

    # Redo the iron and steel energy use and other industrial energy use subtraction
    L1322.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1323.in_EJ_R_iron_steel_F_Y %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      replace_na(list(value = 0)) %>%
      mutate(value = raw - value , raw = NULL) ->
      L1323.in_EJ_R_indenergy_F_Yh

    # ===================================================
    # Produce outputs

    L1323.out_Mt_R_iron_steel_Yh %>%
      add_title("Historical steel outputs by region, fuel, and year") %>%
      add_units("Mt iron_steel") %>%
      add_comments("Outputs are collecting from World steel association and then aggregating to GCAM regions") %>%
      add_legacy_name("L1323.out_Mt_R_iron_steel_Yh") %>%
      add_precursors( "energy/steel_prod_process", "energy/WSA_direct_reduced_iron_2008_2019.csv","common/iso_GCAM_regID",
                      "LB1092.Tradebalance_iron_steel_Mt_R_Y","common/GCAM_region_names") ->
      L1323.out_Mt_R_iron_steel_Yh

    L1323.IO_GJkg_R_iron_steel_F_Yh %>%
      add_title("Input-output coefficients for steel production") %>%
      add_units("GJ/kg steel") %>%
      add_comments("IO coefficients for steel") %>%
      add_legacy_name("L1323.IO_GJkg_R_iron_steel_F_Yh") %>%
      add_precursors( "energy/steel_prod_process", "energy/steel_intensity", "L1012.en_bal_EJ_R_Si_Fi_Yh", "energy/mappings/enduse_fuel_aggregation") ->
      L1323.IO_GJkg_R_iron_steel_F_Yh

    L1323.in_EJ_R_iron_steel_F_Y %>%
      add_title("Historical input energy use for the iron and steel sector") %>%
      add_units("Exajoules") %>%
      add_comments("Calculated by steel production and IO coefficients") %>%
      add_legacy_name("L1323.in_EJ_R_iron_steel_F_Y") %>%
      add_precursors("energy/steel_prod_process","energy/steel_intensity", "L1012.en_bal_EJ_R_Si_Fi_Yh", "energy/mappings/enduse_fuel_aggregation") ->
      L1323.in_EJ_R_iron_steel_F_Y

    L1323.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted iron and steel energy use from industrial energy use values in L1322.in_EJ_R_indenergy_F_Yh") %>%
      add_comments("To determine adjusted input energy for industrial energy use") %>%
      add_legacy_name("L1323.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("L1322.in_EJ_R_indenergy_F_Yh", "energy/steel_prod_process", "L1012.en_bal_EJ_R_Si_Fi_Yh", "energy/mappings/enduse_fuel_aggregation") ->
      L1323.in_EJ_R_indenergy_F_Yh

    L1323.SubsectorInterp_iron_steel %>%
      add_title("Subsector shareweight interpolation of iron and steel sector") %>%
      add_units("NA") %>%
      add_comments("For iron and steel sector, the subsector shareweight interpolation function infromation from A323.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L1323.SubsectorInterp_iron_steel") %>%
      add_precursors("energy/A323.subsector_interp", "common/GCAM_region_names") ->
      L1323.SubsectorInterp_iron_steel



    return_data(L1323.SubsectorInterp_iron_steel,L1323.out_Mt_R_iron_steel_Yh, L1323.IO_GJkg_R_iron_steel_F_Yh, L1323.in_EJ_R_iron_steel_F_Y, L1323.in_EJ_R_indenergy_F_Yh)

  } else {
    stop("Unknown command")
  }
}

