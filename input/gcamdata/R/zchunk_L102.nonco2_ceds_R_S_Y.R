# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L102.nonco2_ceds_R_S_Y
#'
#' Calculates emissions using CEDS and CMIP emissions data for all sectors and fuels and aggregates to GCAM regions. Note that the outputs of this chunk are a part of the prebuilt data.
#' To change the ouputs of this chunk, add CEDS data to the CEDS folder under emissions and rebuild prebuilt data.
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.ceds_nonco2_tg_R_S_F}, \code{L102.ceds_int_shipping_nonco2_tg_S_F}.
#' @details Calculates emissions using CEDS and CMIP data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @importFrom data.table frollmean
#' @author CWR May 2019, KBN June 2020
module_emissions_L102.nonco2_ceds_R_S_Y <- function(command, ...) {
    if(command == driver.DECLARE_INPUTS) {
      return(c(FILE = "common/GCAM_region_names",
               FILE = "common/iso_GCAM_regID",
               OPTIONAL_FILE = "emissions/CEDS/CH4_total_CEDS_emissions",
               FILE = "emissions/CEDS/GFED-CMIP6_LUC_emissions",
               FILE = "emissions/CEDS/LULUC_to_sector_Mapping",
               OPTIONAL_FILE = "emissions/CEDS/BC_total_CEDS_emissions",
               OPTIONAL_FILE = "emissions/CEDS/OC_total_CEDS_emissions",
               OPTIONAL_FILE = "emissions/CEDS/CO_total_CEDS_emissions",
               OPTIONAL_FILE = "emissions/CEDS/NH3_total_CEDS_emissions",
               OPTIONAL_FILE = "emissions/CEDS/NMVOC_total_CEDS_emissions",
               OPTIONAL_FILE = "emissions/CEDS/NOx_total_CEDS_emissions",
               OPTIONAL_FILE = "emissions/CEDS/SO2_total_CEDS_emissions",
               OPTIONAL_FILE = "emissions/CEDS/N2O_total_CEDS_emissions",
               FILE = "emissions/CEDS/ceds_sector_map",
               FILE = "emissions/CEDS/ceds_fuel_map",
               "L154.IEA_histfut_data_times_UCD_shares"))
    } else if(command == driver.DECLARE_OUTPUTS) {
      return(c("L102.ceds_GFED_nonco2_tg_R_S_F",
               "L102.ceds_int_shipping_nonco2_tg_S_F"))
    } else if(command == driver.MAKE) {

      # Silence package checks
      iso <- em <- CEDS_sector <- fuel <- unit <- year <- UCD_category <- value <- GCAM_region_ID <-
        emissions <- sector <- Non.CO2 <- CEDS_agg_sector <- CEDS_agg_fuel <- share_in_global_ship <- NULL


      all_data <- list(...)[[1]]

      # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
      iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
      CEDS_CH4 <- get_data(all_data, "emissions/CEDS/CH4_total_CEDS_emissions")
      CEDS_BC <- get_data(all_data, "emissions/CEDS/BC_total_CEDS_emissions")
      CEDS_NMVOC <- get_data(all_data, "emissions/CEDS/NMVOC_total_CEDS_emissions")
      CEDS_NH3 <- get_data(all_data, "emissions/CEDS/NH3_total_CEDS_emissions")
      CEDS_OC <- get_data(all_data, "emissions/CEDS/OC_total_CEDS_emissions")
      CEDS_CO <- get_data(all_data, "emissions/CEDS/CO_total_CEDS_emissions")
      CEDS_NOx <- get_data(all_data, "emissions/CEDS/NOx_total_CEDS_emissions")
      CEDS_SO2 <- get_data(all_data, "emissions/CEDS/SO2_total_CEDS_emissions")
      #Introducing N2O
      CEDS_N2O <- get_data(all_data, "emissions/CEDS/N2O_total_CEDS_emissions")

      CEDS_sector_map <- get_data(all_data, "emissions/CEDS/ceds_sector_map")
      CEDS_fuel_map <- get_data(all_data, "emissions/CEDS/ceds_fuel_map")
      CMIP_unmgd_emissions <- get_data(all_data, "emissions/CEDS/GFED-CMIP6_LUC_emissions") %>%
        gather_years(value_col = "emissions")
      CMIP_sector_map <- get_data(all_data, "emissions/CEDS/LULUC_to_sector_Mapping")

      # If the (proprietary) raw CEDS datasets are available, go through the full computations below
      # If not, use the pre-saved summary file (i.e., the output of this chunk!) assuming it's available
      if(!is.null(CEDS_CH4) && !is.null(CEDS_BC) && !is.null(CEDS_OC) && !is.null(CEDS_CO) && !is.null(CEDS_NMVOC) && !is.null(CEDS_NH3) && !is.null(CEDS_NOx) && !is.null(CEDS_SO2)) {

        #gather years
        CEDS_CH4 <- CEDS_CH4 %>%
          gather_years(value_col = "emissions")
        CEDS_BC <- CEDS_BC %>%
          gather_years(value_col = "emissions")
        CEDS_NMVOC <- CEDS_NMVOC %>%
          gather_years(value_col = "emissions")
        CEDS_NH3 <- CEDS_NH3 %>%
          gather_years(value_col = "emissions")
        CEDS_OC <- CEDS_OC %>%
          gather_years(value_col = "emissions")
        CEDS_CO <- CEDS_CO %>%
          gather_years(value_col = "emissions")
        CEDS_NOx <- CEDS_NOx %>%
          gather_years(value_col = "emissions")
        CEDS_SO2 <- CEDS_SO2 %>%
          gather_years(value_col = "emissions")
        #Introducing N2O
        CEDS_N2O <- CEDS_N2O %>%
          gather_years(value_col = "emissions")




        # Add non-CO2 gas names to CEDS data
        CEDS_CH4$Non.CO2 <- "CH4"
        CEDS_BC$Non.CO2 <- "BC"
        CEDS_NMVOC$Non.CO2 <- "NMVOC"
        CEDS_NH3$Non.CO2 <- "NH3"
        CEDS_OC$Non.CO2 <- "OC"
        CEDS_CO$Non.CO2 <- "CO"
        CEDS_SO2$Non.CO2 <- "SO2"
        CEDS_NOx$Non.CO2 <- "NOx"
        #Introducing N2O
        CEDS_N2O$Non.CO2 <- "N2O"
        # Prepare unmanaged forest emissions from CMIP to be combined with CEDS data set
        CMIP_unmgd_emissions %>%
          filter(!iso %in% c(emissions.GFED_NODATA)) %>%
          distinct() %>%
          left_join(CMIP_sector_map, by = c("sector" = "LULUC_sector_abr")) %>%
          na.omit() %>%
          mutate(fuel = "process") %>%
          filter(em != "CO2") %>%
          select(iso, CEDS_sector, fuel, unit, year, emissions, em) %>%
          rename(units = unit, Non.CO2 = em, sector = CEDS_sector) %>%
          group_by(iso, sector, fuel, units, Non.CO2) %>%
          mutate(emissions=frollmean(emissions,emissions.UNMGD_LAND_AVG_YRS)) %>%
          ungroup() %>%
          na.omit() %>%
          filter(year %in% emissions.CEDS_YEARS)->L102.CMIP_unmgd_emissions


        # Compute CEDS emissions by region and sector
        bind_rows(CEDS_CH4 ,CEDS_BC, CEDS_NMVOC, CEDS_NH3, CEDS_OC, CEDS_NOx, CEDS_SO2, CEDS_CO, CEDS_N2O) -> CEDS_data

        CEDS_data %>%
          #ISO code for Serbia is different in CEDS. Change this to GCAM iso for Serbia so that left_join_error_no_match won't fail.
          mutate(iso=if_else(iso=="srb (kosovo)","srb",iso)) %>%
          filter(iso != "global")->CEDS_allgas

        unique_iso<-c(unique(CEDS_allgas$iso))

        #Process data for international shipping
        CEDS_data %>%
          filter(iso == "global") %>%
          select(-iso) %>%
          left_join(CEDS_sector_map, by = c("sector" = "CEDS_sector")) %>%
          left_join(CEDS_fuel_map, by = c("fuel" = "CEDS_fuel")) %>%
          filter(CEDS_agg_sector=="trn_intl_ship",CEDS_agg_fuel =="refined liquids") %>%
          gather_years %>%
          filter(year %in% emissions.CEDS_YEARS) %>%
          filter(year <= max(HISTORICAL_YEARS), emissions > 0)->CEDS_int_shipping


        CEDS_allgas %>%
          filter(!(CEDS_allgas$sector %in% unique(L102.CMIP_unmgd_emissions$sector))) %>%
          bind_rows(L102.CMIP_unmgd_emissions) %>%
          left_join(CEDS_sector_map, by = c("sector" = "CEDS_sector")) %>%
          left_join(CEDS_fuel_map, by = c("fuel" = "CEDS_fuel")) %>%
          #Final checks for iso codes for Romania ,Kosovo and Netherlands Antilles.Kosovo emissions will be aggregated to Serbia.
          change_iso_code('rou', 'rom') %>%
          change_iso_code('srb (kosovo)', 'srb') %>%
          change_iso_code('sxm', 'ant') %>%
          na.omit() %>%
          gather_years %>%
          filter(year %in% emissions.CEDS_YEARS) %>%
          # Converts kt(gg) to Teragrams
          mutate(emissions = emissions * CONV_GG_TG)->L102.CEDS


        # Aggregate by region, GHG, and CEDS sector
        #kbn Adding adjustment for international shipping emissions that are mapped to process. These are now mapped to diesel oil since we don't have driver data for these.
        L102.CEDS %>%
          left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
          group_by(GCAM_region_ID, Non.CO2, CEDS_agg_sector, CEDS_agg_fuel, year) %>%
          summarise(emissions = sum(emissions)) %>%
          ungroup() %>%
          na.omit() %>%
          add_comments("Calculate historical emissions from all sectors by sector and fuel from CEDS and CMIP data. CMIP is used for unmanaged lands") ->
          L102.CEDS_GCAM_GFED

        # PRODUCE OUTPUTS
        # ===============

        L102.CEDS_GCAM_GFED %>%
          add_title("CEDS non-CO2 emissions by GCAM region and CEDS sector / fuel / historical year",overwrite = TRUE) %>%
          add_units("Tg") %>%
          add_precursors("emissions/CEDS/BC_total_CEDS_emissions","emissions/CEDS/OC_total_CEDS_emissions","emissions/CEDS/CO_total_CEDS_emissions",
                         "emissions/CEDS/NH3_total_CEDS_emissions","emissions/CEDS/NMVOC_total_CEDS_emissions","emissions/CEDS/NOx_total_CEDS_emissions",
                         "emissions/CEDS/SO2_total_CEDS_emissions","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map", "common/GCAM_region_names",
                         "common/iso_GCAM_regID","emissions/CEDS/CH4_total_CEDS_emissions","emissions/CEDS/GFED-CMIP6_LUC_emissions","emissions/CEDS/LULUC_to_sector_Mapping","emissions/CEDS/N2O_total_CEDS_emissions",
                         "L154.IEA_histfut_data_times_UCD_shares") ->
          L102.ceds_GFED_nonco2_tg_R_S_F

        CEDS_int_shipping %>%
          add_title("CEDS international shipping non-CO2 emissions by CEDS sector / fuel / historical year",overwrite = TRUE) %>%
          add_comments("CEDS international shipping non-CO2 emissions by CEDS sector / fuel / historical year") %>%
          add_units("Tg") %>%
          add_precursors("emissions/CEDS/BC_total_CEDS_emissions","emissions/CEDS/OC_total_CEDS_emissions","emissions/CEDS/CO_total_CEDS_emissions",
                         "emissions/CEDS/NH3_total_CEDS_emissions","emissions/CEDS/NMVOC_total_CEDS_emissions","emissions/CEDS/NOx_total_CEDS_emissions",
                         "emissions/CEDS/SO2_total_CEDS_emissions","emissions/CEDS/ceds_sector_map","emissions/CEDS/ceds_fuel_map",
                         "emissions/CEDS/CH4_total_CEDS_emissions") ->
          L102.ceds_int_shipping_nonco2_tg_S_F

        # verify the calculated data matches the prebuilt version if not a warning
        # will be generated and should only be ignored if the underly CEDS data
        # actually changed
        verify_identical_prebuilt(L102.ceds_GFED_nonco2_tg_R_S_F)
        verify_identical_prebuilt(L102.ceds_int_shipping_nonco2_tg_S_F)


      }
      else {
        # raw CEDS datasets not available, so we will use the prebuilt version
        L102.ceds_GFED_nonco2_tg_R_S_F <- prebuilt_data("L102.ceds_GFED_nonco2_tg_R_S_F")
        L102.ceds_int_shipping_nonco2_tg_S_F <- prebuilt_data("L102.ceds_int_shipping_nonco2_tg_S_F")
      }

      return_data(L102.ceds_GFED_nonco2_tg_R_S_F, L102.ceds_int_shipping_nonco2_tg_S_F)
    } else {
      stop("Unknown command")
    }
}
