#' module_emissions_L252.MACC
#'
#' Creates marginal abatement cost curves "MACC", for fossil resources, agriculture, animals, and processing.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L252.ResMAC_fos}, \code{L252.AgMAC}, \code{L252.MAC_an}, \code{L252.MAC_prc},
#' \code{L252.MAC_higwp}, \code{L252.MAC_Ag_TC_SSP1}, \code{L252.MAC_An_TC_SSP1}, \code{L252.MAC_prc_TC_SSP1},
#' \code{L252.MAC_res_TC_SSP1}, \code{L252.MAC_Ag_TC_SSP2}, \code{L252.MAC_An_TC_SSP2}, \code{L252.MAC_prc_TC_SSP2},
#' \code{L252.MAC_res_TC_SSP2}, \code{L252.MAC_Ag_TC_SSP5}, \code{L252.MAC_An_TC_SSP5}, \code{L252.MAC_prc_TC_SSP5},
#' \code{L252.MAC_res_TC_SSP5}. The corresponding file in the
#' original data system was \code{L252.MACC.R} (emissions level2).
#' @details Creates marginal abatement cost curves "MACC", for fossil resources, agriculture, animals, and processing.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH August 2017
module_emissions_L252.MACC <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_regions",
             FILE = "emissions/A_MACC_TechChange",
             FILE = "emissions/mappings/GCAM_sector_tech",
             FILE = "emissions/HFC_Abate_GV",
             FILE = "emissions/GV_mac_reduction",
             "L152.MAC_pct_R_S_Proc_EPA",
             "L201.ghg_res",
             "L211.AGREmissions",
             "L211.AnEmissions",
             "L211.AGRBio",
             "L232.nonco2_prc",
             "L241.hfc_all",
             "L241.pfc_all"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L252.ResMAC_fos",
             "L252.AgMAC",
             "L252.MAC_an",
             "L252.MAC_prc",
             "L252.MAC_higwp",
             "L252.MAC_Ag_TC_SSP1",
             "L252.MAC_An_TC_SSP1",
             "L252.MAC_prc_TC_SSP1",
             "L252.MAC_res_TC_SSP1",
             "L252.MAC_Ag_TC_SSP2",
             "L252.MAC_An_TC_SSP2",
             "L252.MAC_prc_TC_SSP2",
             "L252.MAC_res_TC_SSP2",
             "L252.MAC_Ag_TC_SSP5",
             "L252.MAC_An_TC_SSP5",
             "L252.MAC_prc_TC_SSP5",
             "L252.MAC_res_TC_SSP5"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    . <- AgProductionTechnology <- AgSupplySector <- AgSupplySubsector <- EPA_MACC_Sector <- EPA_region <-
      GV_year <- MAC_region <- Non.CO2 <- PCT_ABATE <- Process <- Species <- Year <- bio_N2O_coef <-
      resource <- emiss.coef <- input.emissions <- mac.control <- mac.reduction <- region <-
      scenario <- sector <- stub.technology <- subsector <- supplysector <- tax <- tech_change <-
      market.name <- year <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "emissions/A_regions")
    A_MACC_TechChange <- get_data(all_data, "emissions/A_MACC_TechChange")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    HFC_Abate_GV <- get_data(all_data, "emissions/HFC_Abate_GV")
    GV_mac_reduction <- get_data(all_data, "emissions/GV_mac_reduction")
    L152.MAC_pct_R_S_Proc_EPA <- get_data(all_data, "L152.MAC_pct_R_S_Proc_EPA")
    L201.ghg_res <- get_data(all_data, "L201.ghg_res")
    L211.AGREmissions <- get_data(all_data, "L211.AGREmissions")
    L211.AnEmissions <- get_data(all_data, "L211.AnEmissions")
    L211.AGRBio <- get_data(all_data, "L211.AGRBio")
    L232.nonco2_prc <- get_data(all_data, "L232.nonco2_prc")
    L241.hfc_all <- get_data(all_data, "L241.hfc_all")
    L241.pfc_all <- get_data(all_data, "L241.pfc_all")

    # ===================================================
    # Prepare the table with all MAC curves for matching
    # This contains all tax and mac.reduction values
    L252.MAC_pct_R_S_Proc_EPA <- L152.MAC_pct_R_S_Proc_EPA %>%
      gather(tax, mac.reduction, matches("^[0-9]+$")) %>%
      mutate(tax = as.numeric(tax)) %>%
      rename(mac.control = Process)

    MAC_taxes <- unique(L252.MAC_pct_R_S_Proc_EPA$tax)

    # This is a function to add in the mac.reduction curves to data
    # Function needed because these steps are repeated 5 times
    mac_reduction_adder <- function(df, order, error_no_match = TRUE) {
      df <- df %>%
        # Add tax values
        repeat_add_columns(tibble(tax = MAC_taxes)) %>%
        arrange_("region", order) %>%
        # Join in EPA regions
        left_join_error_no_match(A_regions %>%
                                   select(region, EPA_region = MAC_region),
                                 by = "region")
      # Next, add in mac.reduction values
      if(error_no_match) {
        # Usually we use left_join_error_no_match
        df <- df %>%
          left_join_error_no_match(L252.MAC_pct_R_S_Proc_EPA, by = c("EPA_region", "mac.control", "tax")) %>%
          mutate(mac.reduction = round(mac.reduction, 3))
      } else {
        # There are times where the data does not match, so using left_join is necessary
        df <- df %>%
          left_join(L252.MAC_pct_R_S_Proc_EPA, by = c("EPA_region", "mac.control", "tax")) %>%
                      mutate(mac.reduction = round(mac.reduction, 3))
      }
      return(df)
    }

    # L252.ResMAC_fos: Fossil resource MAC curves
    # NOTE: only applying the fossil resource MAC curves to the CH4 emissions
    L252.ResMAC_fos <- L201.ghg_res %>%
      select(-emiss.coef) %>%
      filter(Non.CO2 == "CH4") %>%
      # Add in mac.control
      left_join_error_no_match(GCAM_sector_tech %>%
                                 filter(sector == "out_resources") %>%
                                 select(mac.control = EPA_MACC_Sector, subsector),
                               by = c("resource" = "subsector")) %>%
      mac_reduction_adder(order = "resource") %>%
      # Add column for market variable
      mutate(market.name = emissions.MAC_MARKET) %>%
      # Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
      select(LEVEL2_DATA_NAMES[["ResMAC"]])

    # L252.AgMAC: Agricultural abatement (including bioenergy)
    L252.AgMAC <- L211.AGREmissions %>%
      select(-input.emissions) %>%
      bind_rows(L211.AGRBio %>%
                  select(-bio_N2O_coef)) %>%
      filter(year == min(L211.AGREmissions$year),
             Non.CO2 %in% emissions.AG_MACC_GHG_NAMES) %>%
      # Add in mac.control
      left_join_error_no_match(GCAM_sector_tech %>%
                                 select(mac.control = EPA_MACC_Sector, supplysector) %>%
                                 distinct, # taking distinct values because there were repeats for AEZs
                               by = c("AgSupplySector" = "supplysector")) %>%
      mac_reduction_adder(order = "AgProductionTechnology") %>%
      # Add column for market variable
      mutate(market.name = emissions.MAC_MARKET) %>%
      # Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, Non.CO2,
             mac.control, tax, mac.reduction, market.name)

    # L252.MAC_an: Abatement from animal production
    L252.MAC_an <- L211.AnEmissions %>%
      select(-input.emissions) %>%
      filter(year == min(L211.AnEmissions$year),
             Non.CO2 %in% emissions.AG_MACC_GHG_NAMES) %>%
      # Add in mac.control
      left_join_error_no_match(GCAM_sector_tech %>%
                                 select(mac.control = EPA_MACC_Sector, supplysector) %>%
                                 distinct, # taking distinct values because there are repeats for different technologies
                               by = "supplysector") %>%
      mac_reduction_adder(order = c("supplysector", "subsector", "stub.technology", "Non.CO2")) %>%
      # Add column for market variable
      mutate(market.name = emissions.MAC_MARKET) %>%
      # Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control,
             tax, mac.reduction, market.name)

    # L252.MAC_prc: Abatement from industrial and urban processes
    L252.MAC_prc <- L232.nonco2_prc %>%
      select(-input.emissions) %>%
      filter(year == min(L232.nonco2_prc$year),
             Non.CO2 %in% emissions.GHG_NAMES) %>%
      # Add in mac.control
      # Using left_join b/c mac.control for "other industrial processes" is NA
      left_join(GCAM_sector_tech %>%
                                 select(mac.control = EPA_MACC_Sector, supplysector, subsector, stub.technology),
                               by = c("supplysector", "subsector", "stub.technology")) %>%

      mac_reduction_adder(order = c("supplysector", "subsector", "stub.technology", "Non.CO2"),
                          # error_no_match is F, which means we use left_join(L252.MAC_pct_R_S_Proc_EPA)
                          # because not all mac.controls and regions in L252.MAC_pct_R_S_Proc_EPA
                          error_no_match = FALSE) %>%
      na.omit() %>%
      # Add column for market variable
      mutate(market.name = emissions.MAC_MARKET) %>%
      # Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control, tax, mac.reduction, market.name)

    # L252.MAC_higwp: Abatement from HFCs, PFCs, and SF6
    L252.MAC_higwp <- bind_rows(L241.hfc_all, L241.pfc_all) %>%
      select(-input.emissions) %>%
      filter(year == min(.$year)) %>%
      # Add in mac.control
      # Using left_join b/c mac.control for "other industrial processes" is NA
      left_join(GCAM_sector_tech %>%
                                 select(mac.control = EPA_MACC_Sector, supplysector, subsector, stub.technology),
                               by = c("supplysector", "subsector", "stub.technology")) %>%
      mac_reduction_adder(order = c("supplysector", "subsector", "stub.technology", "Non.CO2"),
                          # error_no_match is F, which means we use left_join(L252.MAC_pct_R_S_Proc_EPA)
                          # because not all mac.controls and regions in L252.MAC_pct_R_S_Proc_EPA
                          error_no_match = FALSE) %>%
      na.omit() %>%
      # Add column for market variable
      mutate(market.name = emissions.MAC_MARKET) %>%
      # Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control, tax, mac.reduction, market.name)


    # These steps will be completed if we choose to replace our HiGWP data with data from Guus Velders
    if(emissions.USE_GV_MAC) {
      # L252.MAC_higwp_GV: Abatement from HFCs, PFCs, and SF6 using Guus Velders data for HFCs
      # Filter our PFCs
      L252.MAC_pfc <- L252.MAC_higwp %>%
        filter(Non.CO2 %in% c("C2F6", "CF4", "SF6"))

      # Table of abatement potentials
      L252.HFC_Abate_GV <- HFC_Abate_GV %>%
        filter(Species == "Total_HFCs",
               Year %in% unique(GV_mac_reduction$GV_year)) %>%
        select(Year, mac.reduction = PCT_ABATE)

      L252.MAC_hfc <- L252.MAC_higwp %>%
        filter(!(Non.CO2 %in% c("C2F6", "CF4", "SF6")),
               tax == 0) %>%
        # Remove our tax and mac.reduction
        select(-tax, -mac.reduction) %>%
        # Add in GV tax and mac.reduction
        repeat_add_columns(tibble(tax = GV_mac_reduction$tax)) %>%
        left_join_error_no_match(GV_mac_reduction, by = "tax") %>%
        # left_join because some GV_years, but not L252.HFC_Abate_GV Years, are 0, indicating that mac.reduction should also be 0
        left_join(L252.HFC_Abate_GV, by = c("GV_year" = "Year")) %>%
        # Replace mac.reduction for tax 0 with 0
        mutate(mac.reduction = replace(mac.reduction, tax == 0, 0)) %>%
        select(-GV_year)

      L252.MAC_higwp <- bind_rows( L252.MAC_pfc,L252.MAC_hfc)
    }

    # L252.MAC_TC: Tech Change on MACCs
    # For all tibbles, add in scenarios and tech change, then split by scenario and add in documentation
    L252.MAC_Ag_TC <- L252.AgMAC %>%
      repeat_add_columns(tibble(scenario = unique(A_MACC_TechChange$scenario))) %>%
      left_join_error_no_match(A_MACC_TechChange %>% rename(tech.change = tech_change),
                               by = c("scenario", "mac.control" = "MAC")) %>%
      split(.$scenario) %>%
      lapply(function(df) {
        df %>%
          add_title("Marginal Abatement Cost Curves with Technology Changes for Agriculture") %>%
          add_units("tax: 1990 USD; mac.reduction: % reduction; tech_change: Unitless") %>%
          add_comments("Category data from L211.AGREmissions and L211.AGRBio given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
          add_comments("Technology change data added in from A_MACC_TechChange") %>%
          add_precursors("emissions/A_regions", "emissions/mappings/GCAM_sector_tech",
                         "L152.MAC_pct_R_S_Proc_EPA", "L211.AGREmissions", "L211.AGRBio", "emissions/A_MACC_TechChange") %>%
          select(-scenario)
      })


    L252.MAC_An_TC <- L252.MAC_an %>%
      repeat_add_columns(tibble(scenario = unique(A_MACC_TechChange$scenario))) %>%
      left_join_error_no_match(A_MACC_TechChange %>% rename(tech.change = tech_change),
                               by = c("scenario", "mac.control" = "MAC")) %>%
      split(.$scenario) %>%
      lapply(function(df) {
        df %>%
          add_title("Marginal Abatement Cost Curves with Technology Changes for Animals") %>%
          add_units("tax: 1990 USD; mac.reduction: % reduction; tech_change: Unitless") %>%
          add_comments("Category data from L211.AnEmissions given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
          add_comments("Technology change data added in from A_MACC_TechChange") %>%
          add_precursors("emissions/A_regions", "emissions/mappings/GCAM_sector_tech",
                         "L152.MAC_pct_R_S_Proc_EPA", "L211.AnEmissions", "emissions/A_MACC_TechChange") %>%
          select(-scenario)
      })

    L252.MAC_prc_TC <- L252.MAC_prc %>%
      repeat_add_columns(tibble(scenario = unique(A_MACC_TechChange$scenario))) %>%
      left_join(A_MACC_TechChange %>% rename(tech.change = tech_change),
                by = c("scenario", "mac.control" = "MAC")) %>%
      na.omit %>%
      split(.$scenario) %>%
      lapply(function(df) {
        df %>%
          add_title("Marginal Abatement Cost Curves with Technology Changes for Industrial and Urban Processing Greenhouse Gases") %>%
          add_units("tax: 1990 USD; mac.reduction: % reduction; tech_change: Unitless") %>%
          add_comments("Category data from L232.nonco2_prc given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
          add_comments("Technology change data added in from A_MACC_TechChange") %>%
          add_precursors("emissions/A_regions", "emissions/mappings/GCAM_sector_tech",
                         "L152.MAC_pct_R_S_Proc_EPA", "L232.nonco2_prc", "emissions/A_MACC_TechChange") %>%
          select(-scenario)
      })

    L252.MAC_res_TC <- L252.ResMAC_fos %>%
      repeat_add_columns(tibble(scenario = unique(A_MACC_TechChange$scenario))) %>%
      left_join_error_no_match(A_MACC_TechChange %>% rename(tech.change = tech_change),
                               by = c("scenario", "mac.control" = "MAC")) %>%
      split(.$scenario) %>%
      lapply(function(df) {
        df %>%
          add_title("Marginal Abatement Cost Curves with Technology Changes for Fossil Resources") %>%
          add_units("tax: 1990 USD; mac.reduction: % reduction; tech_change: Unitless") %>%
          add_comments("Category data from L201.ghg_res given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
          add_comments("Technology change data added in from A_MACC_TechChange") %>%
          add_precursors("emissions/A_regions", "emissions/mappings/GCAM_sector_tech",
                         "L152.MAC_pct_R_S_Proc_EPA", "L201.ghg_res", "emissions/A_MACC_TechChange") %>%
          select(-scenario)
      })
    # ===================================================

    # Produce outputs
    L252.ResMAC_fos %>%
      add_title("Marginal Abatement Cost Curves for Fossil Resources") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L201.ghg_res given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.ResMAC_fos") %>%
      add_precursors("emissions/A_regions", "emissions/mappings/GCAM_sector_tech",
                     "L152.MAC_pct_R_S_Proc_EPA", "L201.ghg_res") ->
      L252.ResMAC_fos

    L252.AgMAC %>%
      add_title("Marginal Abatement Cost Curves for Agriculture") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L211.AGREmissions and L211.AGRBio given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.AgMAC") %>%
      add_precursors("emissions/A_regions", "emissions/mappings/GCAM_sector_tech",
                     "L152.MAC_pct_R_S_Proc_EPA", "L211.AGREmissions", "L211.AGRBio") ->
      L252.AgMAC

    L252.MAC_an %>%
      add_title("Marginal Abatement Cost Curves for Animals") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L211.AnEmissions given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.MAC_an") %>%
      add_precursors("emissions/A_regions", "emissions/mappings/GCAM_sector_tech",
                     "L152.MAC_pct_R_S_Proc_EPA", "L211.AnEmissions") ->
      L252.MAC_an

    L252.MAC_prc %>%
      add_title("Marginal Abatement Cost Curves for Industrial and Urban Processing Greenhouse Gases") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L232.nonco2_prc given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.MAC_prc") %>%
      add_precursors("emissions/A_regions", "emissions/mappings/GCAM_sector_tech",
                     "L152.MAC_pct_R_S_Proc_EPA", "L232.nonco2_prc") ->
      L252.MAC_prc

    L252.MAC_higwp %>%
      add_title("Marginal Abatement Cost Curves for High GWP Gases") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L241.hfc_all and L241.pfc_all given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_comments("If using Guus Velders data, tax and mac.reduction values taken from HFC_Abate_GV and GV_mac_reduction") %>%
      add_legacy_name("L252.MAC_higwp") %>%
      add_precursors("emissions/A_regions", "emissions/mappings/GCAM_sector_tech",
                     "L152.MAC_pct_R_S_Proc_EPA", "L241.hfc_all", "L241.pfc_all",
                     "emissions/HFC_Abate_GV", "emissions/GV_mac_reduction") ->
      L252.MAC_higwp

    L252.MAC_Ag_TC[["SSP1"]] %>%
      add_legacy_name("L252.MAC_Ag_TC_SSP1") ->
      L252.MAC_Ag_TC_SSP1

    L252.MAC_An_TC[["SSP1"]] %>%
      add_legacy_name("L252.MAC_An_TC_SSP1") ->
      L252.MAC_An_TC_SSP1

    L252.MAC_prc_TC[["SSP1"]] %>%
      add_legacy_name("L252.MAC_prc_TC_SSP1") ->
      L252.MAC_prc_TC_SSP1

    L252.MAC_res_TC[["SSP1"]] %>%
      add_legacy_name("L252.MAC_res_TC_SSP1") ->
      L252.MAC_res_TC_SSP1

    L252.MAC_Ag_TC[["SSP2"]] %>%
      add_legacy_name("L252.MAC_Ag_TC_SSP2") ->
      L252.MAC_Ag_TC_SSP2

    L252.MAC_An_TC[["SSP2"]] %>%
      add_legacy_name("L252.MAC_An_TC_SSP2") ->
      L252.MAC_An_TC_SSP2

    L252.MAC_prc_TC[["SSP2"]] %>%
      add_legacy_name("L252.MAC_prc_TC_SSP2") ->
      L252.MAC_prc_TC_SSP2

    L252.MAC_res_TC[["SSP2"]] %>%
      add_legacy_name("L252.MAC_res_TC_SSP2") ->
      L252.MAC_res_TC_SSP2

    L252.MAC_Ag_TC[["SSP5"]] %>%
      add_legacy_name("L252.MAC_Ag_TC_SSP5") ->
      L252.MAC_Ag_TC_SSP5

    L252.MAC_An_TC[["SSP5"]] %>%
      add_legacy_name("L252.MAC_An_TC_SSP5") ->
      L252.MAC_An_TC_SSP5

    L252.MAC_prc_TC[["SSP5"]] %>%
      add_legacy_name("L252.MAC_prc_TC_SSP5") ->
      L252.MAC_prc_TC_SSP5

    L252.MAC_res_TC[["SSP5"]] %>%
      add_legacy_name("L252.MAC_res_TC_SSP5") ->
      L252.MAC_res_TC_SSP5

    return_data(L252.ResMAC_fos, L252.AgMAC, L252.MAC_an, L252.MAC_prc, L252.MAC_higwp, L252.MAC_Ag_TC_SSP1,
                L252.MAC_An_TC_SSP1, L252.MAC_prc_TC_SSP1, L252.MAC_res_TC_SSP1, L252.MAC_Ag_TC_SSP2,
                L252.MAC_An_TC_SSP2, L252.MAC_prc_TC_SSP2, L252.MAC_res_TC_SSP2, L252.MAC_Ag_TC_SSP5,
                L252.MAC_An_TC_SSP5, L252.MAC_prc_TC_SSP5, L252.MAC_res_TC_SSP5)
  } else {
    stop("Unknown command")
  }
}
