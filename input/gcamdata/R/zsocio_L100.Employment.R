# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L100.Employment
#'
#' process ILO, USDA and other data to compile employment by sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers},
#' \code{L100.Emp_ILOEST_FullISO_ISIC1D_Yh_thouspers},
#' \code{L100.Emp_USDA_Ag_ISO_Yh_thouspers}
#' @details process raw ILO or other data source for employment by sector
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate mutate_at select rename
#' @author xz 2025
#'
module_socio_L100.Employment <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "common/GCAM_region_names",
      # ILO employment data and mappings
      ## Employment be sector from Labor Force Statistics (LFS)
      FILE = "socioeconomics/ILO/ILO_Employment_by_sex_and_economic_activity_ISIC_level2",
      ## Employment from ILO Modelled Estimates (ILOEST)
      FILE = "socioeconomics/ILO/ILO_Employment_by_sex_and_economic_activity_ILO_modelled_estimates",
      ## Sector mapping
      FILE = "socioeconomics/ILO/ILO_ISIC_sector_mapping",
      ## Country mapping
      FILE = "socioeconomics/ILO/ILO_country_iso_mapping")

  MODULE_OUTPUTS <-
    c("L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers",
      "L100.Emp_ILOEST_FullISO_ISIC1D_Yh_thouspers")


  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # silence package checks
    scenario <- year <- gdp <- GCAM_region_ID <- account <- Region <- Units <- growth <- timestep <- region <-
      GDP <- pop <- laborproductivity <- NULL


    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # 1. Pro ILO data ----

    # Two ILO dataset are processed here:
    # Labor Force Statistics (LFS) and ILO Modelled Estimates (ILOEST)
    # They have different regional coverage and sectoral aggregation

    ## Clean and check the sector and regional mappings ----

    # Keep only distinct, mapped ISIC–GCAM pairs; both version of ISIC included
    ILO_ISIC_sector_mapping <-
      ILO_ISIC_sector_mapping %>%
      distinct(ISIC_division, GCAM_sector) %>%
      filter(!is.na(GCAM_sector))

    # Harmonize country names and ISO codes

    ILO_country_iso_mapping <-
      ILO_country_iso_mapping %>%
      transmute(
        iso     = tolower(`ISO3 Code`),
        country = Country ) %>%
      # Fix ILO country mapping label encoding issues
      mutate(
        country = dplyr::case_when(
          country == "C te d Ivoire" ~ "Côte d'Ivoire",
          country == "T rkiye"       ~ "Türkiye",
          country == "R union"       ~ "Réunion",
          TRUE                       ~ country )
      )

    ## 1.1 ILO LFS ----

    ILO_Employment_by_sex_and_economic_activity_ISIC_level2 %>%
      gather_years() %>%
      na.omit() %>%
      # data should only include Total, but filter just in case
      filter(sex.label == "Total") %>%
      transmute(country = ref_area.label,
                ISIC_division = classif1.label,
                year, value) %>%
      # clean up version naming
      mutate(
        ISIC_division = gsub("^Economic activity \\(ISIC-Rev.4\\), 2 digit level: ",
                             "ISIC4_", ISIC_division),
        ISIC_division = gsub("^Economic activity \\(ISIC-Rev.3.1\\), 2 digit level: ",
                             "ISIC3.1_", ISIC_division)  ) %>%
      left_join_error_no_match(ILO_country_iso_mapping, by = "country") ->
      L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers

    ### + USA Fish adjustments ----
    # The ILO survey fish labor in the USA is too large:
    # ~600+ thousand for the past 20+ years.
    # Likely includes processing sectors.
    # From IBIS or other sources, total fish should be ~76 thousand in 2024
    # (68.7 capture & 7.5 aquaculture).

    USA_Fish_Labor_Final_Base_Year <-
      L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers %>%
      filter(iso == "usa",
             # ISIC4_03 is the fishing ISIC code
             grepl("ISIC4_03", ISIC_division),
             year == MODEL_FINAL_BASE_YEAR) %>%
      pull(value)

    if (!is.na(USA_Fish_Labor_Final_Base_Year) &&
        USA_Fish_Labor_Final_Base_Year > 600) {

      # Scale USA fish labor (~1/8) to match an approximate target of 76 thousand
      target_usa_fish_thous <- 76
      scale_factor <- USA_Fish_Labor_Final_Base_Year / target_usa_fish_thous

      L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers <-
        L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers %>%
        mutate(value = if_else(
          grepl("ISIC4_03", ISIC_division) & iso == "usa",
          value / scale_factor, value))
    }

    # assert the value is fine
    assertthat::assert_that(
      L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers %>%
        filter(iso == "usa",  grepl("ISIC4_03", ISIC_division), year == MODEL_FINAL_BASE_YEAR) %>%
        pull(value) < 100
    )

    ### + check India forest labor  ----


    ## * L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers ----

    ## 1.2 ILOEST ----

    ILO_Employment_by_sex_and_economic_activity_ILO_modelled_estimates %>%
      gather_years() %>%
      na.omit() %>%
      # keep relevant ISIC 1 digit (level 1) data for total sex
      # remove aggregated sectors
      filter(sex.label == "Total") %>%
      transmute(country = ref_area.label,
                sector = classif1.label,
                year, value) %>%
      filter(grepl("Detailed", sector),
             !grepl("Total", sector)) %>%
      # clean up sector labels
      mutate(sector = gsub("Economic activity \\(Detailed\\): ", "", sector)) %>%
      separate(col = sector, into = c("isic_session", "isic_session_lab"), sep = " ~ISIC rev.4 ") %>%
      mutate(isic_session1 = paste0(isic_session_lab, ": ", isic_session) ) %>%
      # Join iso and GCAM region mappings
      # using left join here as we expect na for aggregated regions
      left_join(ILO_country_iso_mapping, by = "country") %>%
      # removed aggregated regions, e.g., world
      filter(!is.na(iso)) %>%
      # iso (cha) is not an official code; "Channel Islands" is removed here for simplicity
      filter(iso != "cha") %>%
      left_join_error_no_match(iso_GCAM_regID %>% distinct(iso, GCAM_region_ID), by = "iso") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L100.Emp_ILOEST_FullISO_ISIC1D_Yh_thouspers

    ## * L100.Emp_ILOEST_FullISO_ISIC1D_Yh_thouspers ----


    # Produce outputs ----
    L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers  %>%
      add_title("Historical employment by ISIC Ag sector from ILO LFS data") %>%
      add_comments("ILO LFS is survey based data that covers partial ISO. It includes more sectoral details compared to model estimates (EST)") %>%
      add_units("thousand persons") %>%
      add_legacy_name("L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers") %>%
      add_precursors(MODULE_INPUTS) ->
      L100.Emp_ILOLFS_PartialISO_ISIC2D_Yh_thouspers

    L100.Emp_ILOEST_FullISO_ISIC1D_Yh_thouspers  %>%
      add_title("Historical employment by ISIC Ag sector from ILO EST data") %>%
      add_comments("ILO EST is the model estiamte data that covers all ISO. It complements the LFS data") %>%
      add_units("thousand persons") %>%
      add_legacy_name("L100.Emp_ILOEST_FullISO_ISIC1D_Yh_thouspers") %>%
      add_precursors(MODULE_INPUTS) ->
      L100.Emp_ILOEST_FullISO_ISIC1D_Yh_thouspers







    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
