#' module_emissions_L123.bcoc_awb_R_S_T_Y
#'
#' Produces BC and OC AWB emission factors for each Ag production technology.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L123.bcoc_tgmt_R_awb_2000}. The corresponding file in the
#' original data system was \code{L123.bcoc_awb_R_S_T_Y.R} (emissions level1).
#' @details Use year 2000 BC and OC emissions from agricultural waste burning on fields (AWB) to calculate emission factors for each ag production technology.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author SJS May 2017

module_emissions_L123.bcoc_awb_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             "L103.ag_Prod_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L121.AWBshare_R_C_Y_GLU",
             FILE = "emissions/RCP_BC_2000",
             FILE = "emissions/RCP_OC_2000"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.bcoc_tgmt_R_awb_2000"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    get_data(all_data, "temp-data-inject/L121.AWBshare_R_C_Y_GLU") %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L121.AWBshare_R_C_Y_GLU
    RCP_BC_2000 <- get_data(all_data, "emissions/RCP_BC_2000")
    RCP_OC_2000 <- get_data(all_data, "emissions/RCP_OC_2000")

    # Extract 2000 emissions data for AWB emissions and assign emission species identifier
    RCP_BC_2000 %>%
      select(iso, awb) %>%
      mutate(Non.CO2 = "BC_AWB") -> BC_AWB_2000

    RCP_OC_2000 %>%
      select(iso, awb) %>%
      mutate(Non.CO2 = "OC_AWB") -> OC_AWB_2000

    # Aggregate BC and OC emissions to GCAM regions and convert to Tg
    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      # Old system used match and ended up with just the first value instead of summing all values
      bind_rows(OC_AWB_2000, BC_AWB_2000) %>%
      # Not all iso's will necesarilly have an input value. This is ok, but use left_join becuase of this.
      left_join(iso_GCAM_regID, by = "iso") %>%
      group_by(Non.CO2, GCAM_region_ID) %>%
      summarise(awb = first(awb))  %>%
      filter(!is.na(awb)) %>%
      mutate(awb = awb * CONV_KG_TO_TG) %>%
      ungroup() ->
      BCOC_AWB_2000
    } else {
        bind_rows(OC_AWB_2000, BC_AWB_2000) %>%
        # Not all iso's will necesarilly have an input value. This is ok, but use left_join becuase of this.
        left_join(iso_GCAM_regID, by = "iso") %>%
        group_by(Non.CO2, GCAM_region_ID) %>%
        summarize_if(is.numeric , sum) %>%
        filter(!is.na(awb)) %>%
        mutate(awb = awb * CONV_KG_TO_TG) %>%
        ungroup() ->
        BCOC_AWB_2000
   }

    # Extract only 2000 data from ag production data
    filter(L103.ag_Prod_Mt_R_C_Y_GLU, year == 2000) %>%
      select(-year)  ->
      L103.ag_Prod_Mt_R_C_Y_GLU_2000

    # Allocate aggreate regional year 2000 AWB emissions into GCAM crop and GLU
    # This is done using shares from L121.AWBshare_R_C_Y_GLU, which is calculated in another chunk
    filter(L121.AWBshare_R_C_Y_GLU, year == "2000")  %>%
      select(-year) %>%
      # Add BC and OC emissions data
      repeat_add_columns(tibble::tibble(Non.CO2 = c("BC_AWB","OC_AWB"))) %>%
      left_join_error_no_match(BCOC_AWB_2000, by = c("GCAM_region_ID", "Non.CO2")) %>%
      # Calculate AWB emissions by crop and GLU by sharing out regional total awb emissions
      # by the share for each individual GCAM technology
      mutate(awb_emission = AWB_emiss_share * awb) %>%
      # Add production data
      left_join_error_no_match(L103.ag_Prod_Mt_R_C_Y_GLU_2000, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      # Calculate emission factor, which is
      mutate(emfact = awb_emission / value) %>%
      select(-awb_emission, -value, -AWB_emiss_share, -awb)  %>%
      # Replace NaNs with zeros
      mutate_all(funs(replace(., is.na(.), 0))) ->
      L121.AWB_Emissions_FactorR_C_Y_GLU

    # Produce outputs
    L121.AWB_Emissions_FactorR_C_Y_GLU %>%
      add_title("Emission factors for BC and OC emissions by GCAM GLU and crop") %>%
      add_units("kt/Mt") %>%
      add_comments("Derived from RCP year 2000 AWB emissions by region and allocated according to above ground non-harvested biomass") %>%
      add_legacy_name("L123.bcoc_tgmt_R_awb_2000") %>%
      add_precursors("common/iso_GCAM_regID",
                     "emissions/RCP_BC_2000",
                     "emissions/RCP_OC_2000",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "temp-data-inject/L121.AWBshare_R_C_Y_GLU") ->
      L123.bcoc_tgmt_R_awb_2000

    return_data(L123.bcoc_tgmt_R_awb_2000)
  } else {
    stop("Unknown command")
  }
}
