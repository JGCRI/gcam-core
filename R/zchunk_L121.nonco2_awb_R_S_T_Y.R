#' module_emissions_L121.nonco2_awb_R_S_T_Y
#'
#' Agricultural waste burning emissions by region are assigned to crops and land use regions on the basis of excess dry biomass estimated from production, harvest index, and water content.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L121.AWBshare_R_C_Y_GLU}, \code{L121.nonco2_tg_R_awb_C_Y_GLU}. The corresponding file in the
#' original data system was \code{L121.nonco2_awb_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KD May 2017

module_emissions_L121.nonco2_awb_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector",
             "L103.ag_Prod_Mt_R_C_Y_GLU",
             "L111.ag_resbio_R_C",
             FILE = "emissions/EDGAR/EDGAR_SO2",
             FILE = "emissions/EDGAR/EDGAR_CO",
             FILE = "emissions/EDGAR/EDGAR_NOx",
             FILE = "emissions/EDGAR/EDGAR_NMVOC",
             FILE = "emissions/EDGAR/EDGAR_CH4",
             FILE = "emissions/EDGAR/EDGAR_N2O",
             FILE = "emissions/EDGAR/EDGAR_NH3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L121.AWBshare_R_C_Y_GLU",
             "L121.nonco2_tg_R_awb_C_Y_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L111.ag_resbio_R_C <- get_data(all_data, "L111.ag_resbio_R_C")
    EDGAR_SO2 <- get_data(all_data, "emissions/EDGAR/EDGAR_SO2")
    EDGAR_CO <- get_data(all_data, "emissions/EDGAR/EDGAR_CO")
    EDGAR_NOx <- get_data(all_data, "emissions/EDGAR/EDGAR_NOx")
    EDGAR_NMVOC <- get_data(all_data, "emissions/EDGAR/EDGAR_NMVOC")
    EDGAR_CH4 <- get_data(all_data, "emissions/EDGAR/EDGAR_CH4")
    EDGAR_N2O <- get_data(all_data, "emissions/EDGAR/EDGAR_N2O")
    EDGAR_NH3 <- get_data(all_data, "emissions/EDGAR/EDGAR_NH3")

    # ===================================================

    # Agricultural waste burning emissions by region are assigned to crops and land use regions on the basis of excess dry biomass...
    # estimated from production, harvest index, and water content

    # Match weighted average residue biomass parameters with crop prodcution.
    L103.ag_Prod_Mt_R_C_Y_GLU %>%
      left_join(L111.ag_resbio_R_C, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      select(-c(ErosCtrl_tHa, ResEnergy_GJt, Root_Shoot)) ->
      L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU

    # Set the default harvest index of 1 and water content to 0.15 for fiber and fodder crops, in order to use
    # harvest index of 1 and water content to caculate burnable excess biomass in next step.
    L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU %>%
      mutate(HarvestIndex = replace(HarvestIndex, is.na(HarvestIndex), 1)) %>%
      mutate(WaterContent = replace(WaterContent, is.na(WaterContent), 0.15)) ->
      L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_replaced

    # Burnable excess biomass is equal to ( ( biomass production / HarvestIndex ) - biomass production ) * ( 1 - WaterContent )
    # For root crops, the calculation could be done differently, if the root mass is excluded from the denominator of the reported harvest index.
    # This doesn't seem to be the case in the literature--while for other crops, root mass is excluded from the harvest index, it is included for potatoes.
    # If excluded, then the harvest index could be greater than 1 (if the tubers weigh more than the above-ground shoots), and the above calculation would
    # return a negative number. None of the crops in the underlying harvested index database have values greater than 1 so this isn't currently an issue.
    L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_replaced %>%
      mutate(burnable = (( value / HarvestIndex ) - value ) * ( 1 - WaterContent )) ->
      L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_burn

    # Aggregate the burnable excess biomass by GCAM region and year.
    L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_burn %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(burnable)) ->
      L112.ag_ExcessDryBiomass_Mt_R_Y


    # Find the burning share of excess biomass that is burnable (AWB_emiss_share). This will be used to make
    # table of ag waste burning share of emissions, for downscaling regional emissions to region/GLU/crop
    L112.ag_ExcessDryBiomass_Mt_R_Y %>%
      rename(total_excess_bio = value) %>%
      left_join(L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU_burn, by = c("GCAM_region_ID", "year")) %>%
      mutate(AWB_emiss_share = burnable / total_excess_bio) %>%
      select(GCAM_region_ID, GCAM_commodity, year, GLU, AWB_emiss_share) ->
      L121.AWBshare_R_C_GLU


    # Compute EDGAR emissions by region

    # Adding variable names to all EDGAR data sets
    EDGAR_SO2 %>%
      mutate(Non.CO2 = "SO2_AWB") ->
      EDGAR_SO2

    EDGAR_CO %>%
      mutate(Non.CO2 = "CO_AWB") ->
      EDGAR_CO

    EDGAR_NOx %>%
      mutate(Non.CO2 = "NOx_AWB") ->
      EDGAR_NOx

    EDGAR_NMVOC %>%
      mutate(Non.CO2 = "NMVOC_AWB") %>%
      select(-c(`2009`, `2010`)) ->
      EDGAR_VOC

    EDGAR_CH4 %>%
      mutate(Non.CO2 = "CH4_AWB") ->
      EDGAR_CH4

    EDGAR_N2O %>%
      mutate(Non.CO2 = "N2O_AWB") ->
      EDGAR_N2O

    EDGAR_NH3 %>%
      mutate(Non.CO2 = "NH3_AWB") ->
      EDGAR_NH3

    # Match all of the EDGAR emissions with agg sector and IPCC region, since there is
    # emission data for each region and sector can use left_join.
    bind_rows(EDGAR_SO2, EDGAR_CO, EDGAR_NOx, EDGAR_VOC, EDGAR_CH4, EDGAR_N2O, EDGAR_NH3) %>%
      left_join( select(EDGAR_sector, c(IPCC, sector = agg_sector)), by = "IPCC" ) ->
      L121.EDGAR

    # Swtiching from EDGAR iso to GCAM_region_ID
    L121.EDGAR %>%
      standardize_iso(col = "ISO_A3") %>%
      change_iso_code('rou', 'rom') %>%
      left_join(iso_GCAM_regID, by = "iso") ->
      L121.EDGAR_iso

    # Aggergate EDGAR emissions by GCAM region and sector.
    L121.EDGAR_iso %>%
      select( -c(`IPCC-Annex`, `World Region`, iso, Name, IPCC, IPCC_description, region_GCAM3, country_name)) %>%
      na.omit %>%
      gather(year, value, -c(GCAM_region_ID, `Non.CO2`, sector)) %>%
      group_by(GCAM_region_ID, `Non.CO2`, sector, year) %>%
      summarise( value = sum(as.numeric(value))) ->
      L121.EDGAR_R_G_s_y_v

    # Convert emissions to Tg
    L121.EDGAR_R_G_s_y_v %>%
      mutate(value = value * CONV_GG_TG) ->
      L121.EDGAR_TG_R_G_s_y_v

    # Select agricultural waste burning emissions
    L121.EDGAR_TG_R_G_s_y_v %>%
      filter(sector == "ag_waste_burning") ->
      L121.EDGAR_awb

    # Compute agricultural waste burning emissions by GCAM region, commodity, and GLU

    # Add gas name to agricultural waste burning emissions
    L121.AWBshare_R_C_GLU %>%
      repeat_add_columns(tibble::tibble(`Non.CO2` = c("SO2_AWB", "NOx_AWB", "CO_AWB", "NMVOC_AWB", "CH4_AWB", "N2O_AWB", "NH3_AWB"))) ->
      L121.nonco2_tg_R_awb_C_Y_GLU

    # Find the regional ag waste burning emissions from the estimated share (fraction)
    # burning emissions * the total ag emissions from
    L121.nonco2_tg_R_awb_C_Y_GLU %>%
      mutate(year = as.character(year)) %>%
      left_join( L121.EDGAR_awb, by = c("GCAM_region_ID", "year", "Non.CO2")) %>%
      rename(total_emiss = value) %>%
      mutate(emissions = total_emiss * AWB_emiss_share) %>%
      select(-sector) ->
      L121.nonco2_tg_R_awb_C_Y_GLU_total

    #Subset only the historical years in EDGAR, and reshape for write-out
    L121.nonco2_tg_R_awb_C_Y_GLU_total %>%
      filter(year %in% emissions.EDGAR_HISTOIRCAL) %>%
      select(GCAM_region_ID, Non.CO2, GCAM_commodity, GLU, year, value = emissions) ->
      L121.nonco2_tg_R_awb_C_Y_GLU

    # Format the ag waste bruning emission estimates for write-output
    L121.AWBshare_R_C_GLU %>%
      rename(value = AWB_emiss_share) ->
      L121.AWBshare_R_C_GLU

    # ===================================================

    # Produce outputs
    L121.AWBshare_R_C_GLU %>%
      add_title("Ag waste burning share of emissions by GCAM region / commodity / GLU / historical year") %>%
      add_units("unitless share") %>%
      add_comments("Agricultural waste burning emissions by region are assigned to crops and land use regions on the basis of excess dry biomass...") %>%
      add_comments("estimated from production, harvest index, and water content") %>%
      add_legacy_name("L121.AWBshare_R_C_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_precursors("emissions/EDGAR/EDGAR_sector", "emissions/EDGAR/EDGAR_SO2","emissions/EDGAR/EDGAR_CO",
                     "emissions/EDGAR/EDGAR_NOx", "emissions/EDGAR/EDGAR_NMVOC", "emissions/EDGAR/EDGAR_CH4",
                     "emissions/EDGAR/EDGAR_N2O", "emissions/EDGAR/EDGAR_NH3") %>%
      add_precursors("L103.ag_Prod_Mt_R_C_Y_GLU", "L111.ag_resbio_R_C") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L121.AWBshare_R_C_Y_GLU

    L121.nonco2_tg_R_awb_C_Y_GLU %>%
      add_title("Ag waste burning emissions by GCAM region / commodity / GLU / historical year") %>%
      add_units("Unit = Tg") %>%
      add_comments("Agricultural waste burning emissions by region are assigned to crops and land use regions on the basis of excess dry biomass...") %>%
      add_comments("estimated from production, harvest index, and water content") %>%
      add_legacy_name("L121.nonco2_tg_R_awb_C_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_precursors("emissions/EDGAR/EDGAR_sector", "emissions/EDGAR/EDGAR_SO2","emissions/EDGAR/EDGAR_CO",
                     "emissions/EDGAR/EDGAR_NOx", "emissions/EDGAR/EDGAR_NMVOC", "emissions/EDGAR/EDGAR_CH4",
                     "emissions/EDGAR/EDGAR_N2O", "emissions/EDGAR/EDGAR_NH3") %>%
      add_precursors("L103.ag_Prod_Mt_R_C_Y_GLU", "L111.ag_resbio_R_C") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L121.nonco2_tg_R_awb_C_Y_GLU

    return_data(L121.AWBshare_R_C_Y_GLU, L121.nonco2_tg_R_awb_C_Y_GLU)
  } else {
    stop("Unknown command")
  }
}
