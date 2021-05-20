# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L252.MACC
#'
#' Creates marginal abatement cost curves "MACC", for fossil resources, agriculture, animals, and processing.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L252.ResMAC_fos}, \code{L252.AgMAC}, \code{L252.MAC_an}, \code{L252.MAC_prc},
#' \code{L252.MAC_higwp}, \code{L252.ResMAC_fos_tc_average}, \code{L252.AgMAC_tc_average}, \code{L252.MAC_an_tc_average},
#' \code{L252.MAC_prc_tc_average}, \code{L252.MAC_higwp_tc_average},
#' \code{L252.MAC_Ag_TC_SSP1}, \code{L252.MAC_An_TC_SSP1}, \code{L252.MAC_prc_TC_SSP1}, \code{L252.MAC_higwp_TC_SSP1},
#' \code{L252.MAC_res_TC_SSP1}, \code{L252.MAC_Ag_TC_SSP2}, \code{L252.MAC_An_TC_SSP2}, \code{L252.MAC_prc_TC_SSP2},
#' \code{L252.MAC_res_TC_SSP2}, \code{L252.MAC_higwp_TC_SSP2}, \code{L252.MAC_Ag_TC_SSP5}, \code{L252.MAC_An_TC_SSP5},
#' \code{L252.MAC_prc_TC_SSP5}, \code{L252.MAC_res_TC_SSP5}, \code{L252.MAC_higwp_TC_SSP5}. The corresponding file in the
#' original data system was \code{L252.MACC.R} (emissions level2).
#' @details Creates marginal abatement cost curves "MACC", for fossil resources, agriculture, animals, and processing.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter left_join matches mutate select slice
#' @importFrom tidyr gather
#' @author RH August 2017
module_emissions_L252.MACC <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_regions",
             FILE = "emissions/A_MACC_TechChange_omit",
             FILE = "emissions/A_MACC_TechChange_SSP_Mult",
             FILE = "emissions/mappings/CEDS_sector_tech_proc",
             FILE = "emissions/mappings/CEDS_sector_tech_proc_revised",
             FILE = "common/GCAM_region_names",
             FILE = "emissions/EPA_MACC_PhaseInTime",
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
             "L252.ResMAC_fos_phaseInTime",
             "L252.MAC_prc_phaseInTime",
             "L252.MAC_higwp_phaseInTime",
             "L252.ResMAC_fos_tc_average",
             "L252.AgMAC_tc_average",
             "L252.MAC_an_tc_average",
             "L252.MAC_prc_tc_average",
             "L252.MAC_higwp_tc_average",
             "L252.MAC_Ag_TC_SSP1",
             "L252.MAC_An_TC_SSP1",
             "L252.MAC_prc_TC_SSP1",
             "L252.MAC_res_TC_SSP1",
             "L252.MAC_higwp_TC_SSP1",
             "L252.MAC_Ag_TC_SSP2",
             "L252.MAC_An_TC_SSP2",
             "L252.MAC_prc_TC_SSP2",
             "L252.MAC_res_TC_SSP2",
             "L252.MAC_higwp_TC_SSP2",
             "L252.MAC_Ag_TC_SSP5",
             "L252.MAC_An_TC_SSP5",
             "L252.MAC_prc_TC_SSP5",
             "L252.MAC_res_TC_SSP5",
             "L252.MAC_higwp_TC_SSP5"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    . <- AgProductionTechnology <- AgSupplySector <- AgSupplySubsector <- EPA_MACC_Sector <- EPA_region <-
      MAC_region <- Non.CO2 <- PCT_ABATE <- Process <- Species <- Year <- bio_N2O_coef <-
      resource <- emiss.coef <- input.emissions <- mac.control <- mac.reduction <- region <-
      scenario <- sector <- stub.technology <- subsector <- supplysector <- tax <- tech_change <-
      market.name <- year <- Irr_Rfd <- mgmt <- tech.change.year <- tech.change <- Non.CO2.join <- multiplier <- NULL

    all_data <- list(...)[[1]]

    #silence packages
    base.year <- subresource <- technology <- key <- mac.reduction.base <- Sector <- GCAM_region_ID <- EPA_country <- iso <- EPA_sector <- NULL


    # Load required inputs
    A_regions <- get_data(all_data, "emissions/A_regions")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_MACC_TechChange_omit <- get_data(all_data, "emissions/A_MACC_TechChange_omit")
    A_MACC_TechChange_SSP_Mult <- get_data(all_data, "emissions/A_MACC_TechChange_SSP_Mult")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/CEDS_sector_tech_proc")
    if (energy.TRAN_UCD_MODE == "rev.mode"){
      GCAM_sector_tech <- get_data(all_data, "emissions/mappings/CEDS_sector_tech_proc_revised")

    }

    L152.MAC_pct_R_S_Proc_EPA <- get_data(all_data, "L152.MAC_pct_R_S_Proc_EPA")
    L201.ghg_res <- get_data(all_data, "L201.ghg_res", strip_attributes = TRUE)
    L211.AGREmissions <- get_data(all_data, "L211.AGREmissions", strip_attributes = TRUE)
    L211.AnEmissions <- get_data(all_data, "L211.AnEmissions", strip_attributes = TRUE)
    L211.AGRBio <- get_data(all_data, "L211.AGRBio", strip_attributes = TRUE)
    L232.nonco2_prc <- get_data(all_data, "L232.nonco2_prc", strip_attributes = TRUE)
    L241.hfc_all <- get_data(all_data, "L241.hfc_all", strip_attributes = TRUE)
    L241.pfc_all <- get_data(all_data, "L241.pfc_all", strip_attributes = TRUE)
    EPA_MACC_PhaseInTime <- get_data(all_data, "emissions/EPA_MACC_PhaseInTime")


    # update MAC using 2019 EPA
    # Prepare the table with all MAC curves for matching
    # This contains all tax and mac.reduction values
    L152.MAC_pct_R_S_Proc_EPA %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID, -Sector) ->
      L252.MAC_pct_R_S_Proc_EPA

    MAC_taxes <- unique(L252.MAC_pct_R_S_Proc_EPA$tax)

    # This is a function to add in the mac.reduction curves to data
    # Function needed because these steps are repeated 5 times

    mac_reduction_adder <- function(df, error_no_match = TRUE) {
      df <- df %>%
        # Add tax values
        repeat_add_columns(tibble(tax = MAC_taxes)) %>%
        repeat_add_columns(tibble(year = emissions.EPA_MACC_YEAR)) %>%
        # we don't need MACs for calibration years
        filter(year %notin% MODEL_BASE_YEARS)
      # Next, add in mac.reduction values
      if(error_no_match) {
        # Usually we use left_join_error_no_match
        df <- df %>%
          left_join_error_no_match(L252.MAC_pct_R_S_Proc_EPA, by = c("region", "mac.control", "tax", "year")) %>%
          mutate(mac.reduction = round(mac.reduction, emissions.DIGITS_MACC))
      } else {
        # There are times where the data does not match, so using left_join is necessary
        df <- df %>%
          left_join(L252.MAC_pct_R_S_Proc_EPA, by = c("region", "mac.control", "tax", "year")) %>%
          mutate(mac.reduction = round(mac.reduction, emissions.DIGITS_MACC))
      }
      return(df)
    }

    # Part 1: calculate year-specific technological change for all region/sector
    # --------------------------------------------------------------------------------------------------------
    # L252.ResMAC_fos: Fossil resource MAC curves
    # NOTE: only applying the fossil resource MAC curves to the CH4 emissions
    # L252.ResMAC_fos_full contains year-specific MACs for all future modeling periods till 2050
    L252.ResMAC_fos_full <- L201.ghg_res %>%
      select(-emiss.coef, -year) %>%
      distinct() %>%
      filter(Non.CO2 == "CH4") %>%
      # Add in mac.control
      left_join_error_no_match(GCAM_sector_tech %>%
                                 filter(sector == "out_resources") %>%
                                 select(mac.control = EPA_MACC_Sector, subsector),
                               by = c("resource" = "subsector")) %>%
      mac_reduction_adder %>%
      arrange(region, resource) %>%
      # Add column for market variable
      mutate(market.name = emissions.MAC_MARKET) %>%
      # Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
      select(LEVEL2_DATA_NAMES[["ResMAC"]])

    # L252.AgMAC: Agricultural abatement (including bioenergy)
    # L252.AgMAC_full contains year-specific MACs for all future modeling periods till 2050
    L252.AgMAC_full <- L211.AGREmissions %>%
      select(-input.emissions, -year) %>%
      bind_rows(L211.AGRBio %>%
                  select(-bio_N2O_coef, -year)) %>%
      distinct() %>%
      filter(Non.CO2 %in% emissions.AG_MACC_GHG_NAMES) %>%
      # Add in mac.control
      left_join_error_no_match(GCAM_sector_tech %>%
                                 select(mac.control = EPA_MACC_Sector, supplysector) %>%
                                 distinct, # taking distinct values because there were repeats for AEZs
                               by = c("AgSupplySector" = "supplysector")) %>%
      mac_reduction_adder %>%
      arrange(region, AgProductionTechnology) %>%
      # Add column for market variable
      mutate(market.name = emissions.MAC_MARKET) %>%
      repeat_add_columns(tibble(Irr_Rfd = paste0(aglu.IRR_DELIMITER, c("IRR", "RFD")))) %>%
      repeat_add_columns(tibble(mgmt = paste0(aglu.MGMT_DELIMITER, c("lo", "hi")))) %>%
      unite(AgProductionTechnology, AgProductionTechnology, Irr_Rfd, mgmt, sep = "") %>%
      # Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, Non.CO2,
             mac.control, tax, mac.reduction, market.name)

    # L252.MAC_an: Abatement from animal production
    # L252.MAC_an_full contains year-specific MACs for all future modeling periods till 2050
    L252.MAC_an_full <- L211.AnEmissions %>%
      select(-input.emissions, -year) %>%
      distinct() %>%
      filter(Non.CO2 %in% emissions.AG_MACC_GHG_NAMES) %>%
      # Add in mac.control
      left_join_error_no_match(GCAM_sector_tech %>%
                                 select(mac.control = EPA_MACC_Sector, supplysector) %>%
                                 distinct, # taking distinct values because there are repeats for different technologies
                               by = "supplysector") %>%
      mac_reduction_adder %>%
      arrange(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      # Add column for market variable
      mutate(market.name = emissions.MAC_MARKET) %>%
      # Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control,
             tax, mac.reduction, market.name)

    # L252.MAC_prc: Abatement from industrial and urban processes
    # L252.MAC_prc_full contains year-specific MACs for all future modeling periods till 2050
    L252.MAC_prc_full <- L232.nonco2_prc %>%
      select(-input.emissions, -year) %>%
      distinct() %>%
      filter(Non.CO2 %in% emissions.GHG_NAMES) %>%
      # Add in mac.control
      # Using left_join b/c mac.control for "other industrial processes" is NA
      left_join(GCAM_sector_tech %>%
                  select(mac.control = EPA_MACC_Sector, supplysector, subsector, stub.technology),
                by = c("supplysector", "subsector", "stub.technology")) %>%
      mac_reduction_adder(# error_no_match is F, which means we use left_join(L252.MAC_pct_R_S_Proc_EPA)
                          # because not all mac.controls and regions in L252.MAC_pct_R_S_Proc_EPA
                          error_no_match = FALSE) %>%
      arrange(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      na.omit() %>%
      # Add column for market variable
      mutate(market.name = emissions.MAC_MARKET) %>%
      # Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control, tax, mac.reduction, market.name)

    # L252.MAC_higwp: Abatement from HFCs, PFCs, and SF6
    # L252.MAC_higwp_full contains year-specific MACs for all future modeling periods till 2050
    L252.MAC_higwp_full <- bind_rows(L241.hfc_all, L241.pfc_all) %>%
      select(-input.emissions, -year) %>%
      distinct() %>%
      # Add in mac.control
      # Using left_join b/c mac.control for "other industrial processes" is NA
      left_join(GCAM_sector_tech %>%
                  select(mac.control = EPA_MACC_Sector, supplysector, subsector, stub.technology),
                by = c("supplysector", "subsector", "stub.technology")) %>%
      # use error_no_match = FALSE becuase not all regions/sectors have the same MAC controls
      mac_reduction_adder(error_no_match = FALSE) %>%
      arrange(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      # Add column for market variable
      mutate(market.name = emissions.MAC_MARKET) %>%
      # Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control, tax, mac.reduction, market.name)

    # combine MACs from all sectors together, including year-specific MACs from 2020 to 2050
    # in order to backward calculate tech.change for future periods relative to base MAC values
    L252.MAC_summary <- L252.MAC_higwp_full %>%
      bind_rows(L252.MAC_prc_full) %>%
      bind_rows(L252.MAC_an_full) %>%
      bind_rows(L252.AgMAC_full %>%
                  rename(supplysector = AgSupplySector,
                         subsector = AgSupplySubsector,
                         stub.technology = AgProductionTechnology)) %>%
      bind_rows(L252.ResMAC_fos_full %>%
                  rename(supplysector = resource,
                         subsector = subresource,
                         stub.technology = technology)) %>%
      filter(tax == max(tax)) %>%
      select(-tax, -market.name) %>%
      distinct()

    # some region/sector have mac.reductions of zeros at all prices (so no MAC for these region/sector)
    # here remove unnecessary region/technology without any mac.reduction
    L252.MAC_remove <- L252.MAC_summary %>%
      group_by(region, supplysector, subsector, stub.technology, Non.CO2, mac.control) %>%
      summarise(sum = sum(mac.reduction)) %>%
      ungroup() %>%
      filter(sum == 0) %>%
      select(region, supplysector, subsector, stub.technology, Non.CO2, mac.control) %>%
      distinct() %>%
      mutate(key = paste(region, supplysector, subsector, stub.technology, Non.CO2, sep = "-"))

    # We will only defind MAC one time at a base MAC year, which is the first non-all-zero-MAC year
    # first non-all-zero-MAC year can be different acorss region/sector but mostly is 2020
    L252.MAC_base_TC <- L252.MAC_summary %>%
      mutate(key = paste(region, supplysector, subsector, stub.technology, Non.CO2, sep = "-")) %>%
      filter(!(key %in% L252.MAC_remove$key)) %>%
      select(-key, -mac.control) %>%
      filter(mac.reduction > 0) %>%
      rename(mac.reduction.base = mac.reduction) %>%
      # use left_join to only keep omit != 1 rows (NAs)
      # here cooling' mitigation potential increased a lot (leading to tech.change > 1), so use the
      # second non-all-zero-MAC year has its MAC Tech.change base year
      left_join(A_MACC_TechChange_omit, by = c("supplysector", "year")) %>%
      filter(is.na(omit)) %>%
      select(-omit) %>%
      # first tech.change year is the next modeling period of the MAC defined years
      # here add 5 years so they can be used to match with their corresponding "next period"
      mutate(year = year + emissions.EPA_TC_TimeStep)

    # based on EPA's year's specific MACs (2020-2050 by every 5 year) to backward calculate technology.change
    # these are tech.change before 2050, based on actual EPA MAC data
    L252.MAC_summary_TC_before2050 <- L252.MAC_summary %>%
      mutate(key = paste(region, supplysector, subsector, stub.technology, Non.CO2, sep = "-")) %>%
      filter(!(key %in% L252.MAC_remove$key)) %>%
      select(-key) %>%
      filter(mac.reduction > 0) %>%
      left_join(L252.MAC_base_TC,
                by = c("region", "supplysector", "subsector", "stub.technology", "Non.CO2", "year")) %>%
      na.omit() %>%
      mutate(tech.change = (mac.reduction / mac.reduction.base)^(1/emissions.EPA_TC_TimeStep) - 1) %>%
      # round to 4 digits otherwise it will be > 10 digits and long in xmls
      mutate(tech.change = round(tech.change, emissions.DIGITS_MACC_TC)) %>%
      replace_na(list(tech.change = 0)) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control, tech.change) %>%
      mutate(key = paste(supplysector, subsector, Non.CO2, sep = "-")) %>%
      # replace negative tech.change into 0s
      mutate(tech.change = ifelse(tech.change < 0, 0, tech.change))


    # since the current EPA MAC data (2019 version) only covers 2020 to 2050
    # we assume constant post-2050 TCs as the average of pre-2050 values

    # copy and paste average values for all future years, assuming constant tech.change (the average tech.change)
    L252.MAC_summary_TC_post2050_average <- L252.MAC_summary_TC_before2050 %>%
      group_by(region, supplysector, subsector, stub.technology, Non.CO2, mac.control, key) %>%
      summarise(tech.change = mean(tech.change)) %>%
      ungroup() %>%
      repeat_add_columns(tibble(year = emissions.EPA_MACC_FUTURE_YEAR))

    # combine together
    # L252.MAC_summary_TC_average contains TC for all future periods greater than MAC base-year for each region/sector
    L252.MAC_summary_TC_average <- rbind(L252.MAC_summary_TC_before2050, L252.MAC_summary_TC_post2050_average)


    # Part 2: calculate the MAC curve for each region/sector, so the TCs above can be applied to
    # therefore we only need to define MAC once in a base year, combined with year-specific TCs to adjust it for future years
    # --------------------------------------------------------------------------------------------------------

    # find the first non-all-zero-MAC year, maybe different by region/sector
    # here conduct this process at this combined tibble (L252.MAC_summary) containing all sectors
    # the idea is creating a varible called "key" containing the combined information of sector-subsector-technology-gas-base.year
    # both the one-time MAC (MAC at base year) and all TCs will be defined at this sector/subsector/technology/gas/base.year in xmls
    L252.MAC_base_year <- L252.MAC_summary %>%
      # use left_join to only keep omit != 1 rows (NAs)
      # here cooling' mitigation potential increased a lot (leading to tech.change > 1), so use the
      # second non-all-zero-MAC year has its MAC Tech.change base year
      left_join(A_MACC_TechChange_omit, by = c("supplysector", "year")) %>%
      filter(is.na(omit)) %>%
      select(-omit) %>%
      mutate(key = paste(region, supplysector, subsector, stub.technology, Non.CO2, sep = "-")) %>%
      filter(!(key %in% L252.MAC_remove$key)) %>%
      filter(mac.reduction > 0) %>%
      group_by(key) %>%
      arrange() %>%
      slice(1) %>%
      ungroup() %>%
      rename(mac.reduction.base = mac.reduction) %>%
      rename(base.year = year) %>%
      mutate(key = paste(region, supplysector, subsector, stub.technology, Non.CO2, base.year, sep = "-")) %>%
      select(key, base.year)

    # next will create MAC file for each broad catagory as usual

    # 1) AgMAC for agriculture
    # base-year MAC curve
    L252.AgMAC <- L252.AgMAC_full %>%
      mutate(key = paste(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, Non.CO2, year, sep = "-")) %>%
      left_join(L252.MAC_base_year, by = "key") %>%
      na.omit() %>%
      select(-key, -base.year)

    # combination of sector/subsector/gas used to isolate TC rows from the combined TC file (L252.MAC_summary_TC_average)
    Ag_key <- L252.AgMAC %>%
      select(AgSupplySector, AgSupplySubsector, Non.CO2) %>%
      distinct() %>%
      mutate(key = paste(AgSupplySector, AgSupplySubsector, Non.CO2, sep = "-"))

    L252.AgMAC_tc_average <- L252.MAC_summary_TC_average %>%
      filter(key %in% Ag_key$key) %>%
      rename(AgSupplySector = supplysector,
             AgSupplySubsector = subsector,
             AgProductionTechnology = stub.technology,
             tech.change.year = year) %>%
      left_join_error_no_match(L252.AgMAC %>% select(-tax, -mac.reduction) %>% distinct(),
                               by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "Non.CO2", "mac.control")) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, Non.CO2, mac.control, tech.change.year, tech.change)

    # 2) L252.MAC_an for livestock
    # base-year MAC curve
    L252.MAC_an <- L252.MAC_an_full %>%
      mutate(key = paste(region, supplysector, subsector, stub.technology, Non.CO2, year, sep = "-")) %>%
      left_join(L252.MAC_base_year, by = "key") %>%
      na.omit() %>%
      select(-key, -base.year)

    # combination of sector/subsector/gas used to isolate TC rows from the combined TC file (L252.MAC_summary_TC_average)
    An_key <- L252.MAC_an %>%
      select(supplysector, subsector, Non.CO2) %>%
      distinct() %>%
      mutate(key = paste(supplysector, subsector, Non.CO2, sep = "-"))

    L252.MAC_an_tc_average <- L252.MAC_summary_TC_average %>%
      filter(key %in% An_key$key) %>%
      rename(tech.change.year = year) %>%
      left_join_error_no_match(L252.MAC_an %>% select(-tax, -mac.reduction) %>% distinct(),
                               by = c("region", "supplysector", "subsector", "stub.technology", "Non.CO2", "mac.control")) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control, tech.change.year, tech.change)

    # 3) L252.MAC_prc for process emissions
    # base-year MAC curve
    L252.MAC_prc <- L252.MAC_prc_full %>%
      mutate(key = paste(region, supplysector, subsector, stub.technology, Non.CO2, year, sep = "-")) %>%
      left_join(L252.MAC_base_year, by = "key") %>%
      na.omit() %>%
      select(-key, -base.year)

    # combination of sector/subsector/gas used to isolate TC rows from the combined TC file (L252.MAC_summary_TC_average)
    Proc_key <- L252.MAC_prc %>%
      select(supplysector, subsector, Non.CO2) %>%
      distinct() %>%
      mutate(key = paste(supplysector, subsector, Non.CO2, sep = "-"))

    L252.MAC_prc_tc_average <- L252.MAC_summary_TC_average %>%
      filter(key %in% Proc_key$key) %>%
      rename(tech.change.year = year) %>%
      left_join_error_no_match(L252.MAC_prc %>% select(-tax, -mac.reduction) %>% distinct(),
                               by = c("region", "supplysector", "subsector", "stub.technology", "Non.CO2", "mac.control")) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control, tech.change.year, tech.change)

    # 4) L252.MAC_higwp for F-gases
    # base-year MAC curve
    L252.MAC_higwp <- L252.MAC_higwp_full %>%
      mutate(key = paste(region, supplysector, subsector, stub.technology, Non.CO2, year, sep = "-")) %>%
      left_join(L252.MAC_base_year, by = "key") %>%
      na.omit() %>%
      select(-key, -base.year)

    # combination of sector/subsector/gas used to isolate TC rows from the combined TC file (L252.MAC_summary_TC_average)
    HighGwp_key <- L252.MAC_higwp %>%
      select(supplysector, subsector, Non.CO2) %>%
      distinct() %>%
      mutate(key = paste(supplysector, subsector, Non.CO2, sep = "-"))

    L252.MAC_higwp_tc_average <- L252.MAC_summary_TC_average %>%
      filter(key %in% HighGwp_key$key) %>%
      rename(tech.change.year = year) %>%
      left_join_error_no_match(L252.MAC_higwp %>% select(-tax, -mac.reduction) %>% distinct(),
                               by = c("region", "supplysector", "subsector", "stub.technology", "Non.CO2", "mac.control")) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control, tech.change.year, tech.change)

    # 5) L252.ResMAC_fos for resource production
    # base-year MAC curve
    L252.ResMAC_fos <- L252.ResMAC_fos_full %>%
      mutate(key = paste(region, resource, subresource, technology, Non.CO2, year, sep = "-")) %>%
      left_join(L252.MAC_base_year, by = "key") %>%
      na.omit() %>%
      select(-key, -base.year) %>%
      mutate(year = emissions.CTRL_BASE_YEAR)

    # combination of sector/subsector/gas used to isolate TC rows from the combined TC file (L252.MAC_summary_TC_average)
    Res_key <- L252.ResMAC_fos %>%
      select(resource, subresource, Non.CO2) %>%
      distinct() %>%
      mutate(key = paste(resource, subresource, Non.CO2, sep = "-"))

    L252.ResMAC_fos_tc_average <- L252.MAC_summary_TC_average %>%
      filter(key %in% Res_key$key) %>%
      rename(resource = supplysector,
             subresource = subsector,
             technology = stub.technology,
             tech.change.year = year) %>%
      mutate(year = emissions.CTRL_BASE_YEAR) %>%
      select(region, resource, subresource, technology, year, Non.CO2, mac.control, tech.change.year, tech.change)


    # create mac-phase-in-time for MAC data

    # explicitly process this for separated tibbles in case in these phase-in-fractions varied by sector
    # currently only apply to energy and industry process sectors
    # NOTE: agricultre sectors will not apply phaseInTime
    # phase-in-fraction should be just applied to the year when MAC read in
    # so it would be convenient to just use the current technology-change data files to replace columns

    L252.ResMAC_fos_tc_average %>%
      select(region, resource, subresource, technology, year, Non.CO2, mac.control) %>%
      distinct() %>%
      left_join_error_no_match(EPA_MACC_PhaseInTime, by = c("mac.control")) ->
      L252.ResMAC_fos_phaseInTime

    L252.MAC_higwp_tc_average %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control) %>%
      distinct() %>%
      left_join_error_no_match(EPA_MACC_PhaseInTime, by = c("mac.control")) ->
      L252.MAC_higwp_phaseInTime

    L252.MAC_prc_tc_average %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control) %>%
      distinct() %>%
      left_join_error_no_match(EPA_MACC_PhaseInTime, by = c("mac.control")) ->
      L252.MAC_prc_phaseInTime

    # Put the tech change pipeline into a helper function since it will be repeated for each
    # of the emissions sectors
    # here we directly process the XXX_tc tables for SSPs
    adjust_tech_change_ssp <- function(df) {
      df %>%
        repeat_add_columns(tibble(scenario = A_MACC_TechChange_SSP_Mult$scenario)) %>%
        left_join_error_no_match(A_MACC_TechChange_SSP_Mult, by = c("scenario")) %>%
        mutate(tech.change = tech.change * multiplier) %>%
        select(-multiplier)
    }

    # L252.MAC_TC: Tech Change on MACCs
    # For all tibbles, add in scenarios and tech change, then split by scenario and add in documentation
    L252.MAC_Ag_TC <- L252.AgMAC_tc_average %>%
      adjust_tech_change_ssp() %>%
      split(.$scenario) %>%
      lapply(function(df) {
        df %>%
          add_title("Marginal Abatement Cost Curves with Technology Changes for Agriculture crop") %>%
          add_units("tax: 1990 USD; mac.reduction: % reduction; tech_change: Unitless") %>%
          add_comments("adjust L252.AgMAC_tc_average to reflect variations in SSPs") %>%
          add_comments("Technology change data added in from A_MACC_TechChange_SSP_Mult") %>%
          same_precursors_as("L252.AgMAC_tc_average") %>%
          add_precursors("emissions/A_MACC_TechChange_SSP_Mult") %>%
          select(-scenario)
      })


    L252.MAC_An_TC <- L252.MAC_an_tc_average %>%
      adjust_tech_change_ssp() %>%
      split(.$scenario) %>%
      lapply(function(df) {
        df %>%
          add_title("Marginal Abatement Cost Curves with Technology Changes for Animals") %>%
          add_units("tax: 1990 USD; mac.reduction: % reduction; tech_change: Unitless") %>%
          add_comments("adjust L252.MAC_an_tc_average to reflect variations in SSPs") %>%
          add_comments("Technology change data added in from A_MACC_TechChange_SSP_Mult") %>%
          same_precursors_as("L252.MAC_an_tc_average") %>%
          add_precursors("emissions/A_MACC_TechChange_SSP_Mult") %>%
          select(-scenario)
      })

    L252.MAC_prc_TC <- L252.MAC_prc_tc_average %>%
      adjust_tech_change_ssp() %>%
      split(.$scenario) %>%
      lapply(function(df) {
        df %>%
          add_title("Marginal Abatement Cost Curves with Technology Changes for Industrial and Urban Processing Greenhouse Gases") %>%
          add_units("tax: 1990 USD; mac.reduction: % reduction; tech_change: Unitless") %>%
          add_comments("adjust L252.MAC_prc_tc_average to reflect variations in SSPs") %>%
          add_comments("Technology change data added in from A_MACC_TechChange_SSP_Mult") %>%
          same_precursors_as("L252.MAC_prc_tc_average") %>%
          add_precursors("emissions/A_MACC_TechChange_SSP_Mult") %>%
          select(-scenario)
      })

    L252.MAC_res_TC <- L252.ResMAC_fos_tc_average %>%
      adjust_tech_change_ssp() %>%
      split(.$scenario) %>%
      lapply(function(df) {
        df %>%
          add_title("Marginal Abatement Cost Curves with Technology Changes for Fossil Resources") %>%
          add_units("tax: 1990 USD; mac.reduction: % reduction; tech_change: Unitless") %>%
          add_comments("adjust L252.ResMAC_fos_tc_average to reflect variations in SSPs") %>%
          add_comments("Technology change data added in from A_MACC_TechChange") %>%
          same_precursors_as("L252.ResMAC_fos_tc_average") %>%
          add_precursors("emissions/A_MACC_TechChange_SSP_Mult") %>%
          select(-scenario)
      })

    L252.MAC_higwp_TC <- L252.MAC_higwp_tc_average %>%
      adjust_tech_change_ssp() %>%
      split(.$scenario) %>%
      lapply(function(df) {
        df %>%
          add_title("Marginal Abatement Cost Curves with Technology Changes for F-gases") %>%
          add_units("tax: 1990 USD; mac.reduction: % reduction; tech_change: Unitless") %>%
          add_comments("adjust L252.MAC_higwp_tc_average to reflect variations in SSPs") %>%
          add_comments("Technology change data added in from A_MACC_TechChange") %>%
          same_precursors_as("L252.MAC_higwp_tc_average") %>%
          add_precursors("emissions/A_MACC_TechChange_SSP_Mult") %>%
          select(-scenario)
      })

    # ===================================================

    # Produce outputs
    L252.ResMAC_fos %>%
      add_title("Marginal Abatement Cost Curves for Fossil Resources based on EPA 2020 level") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L201.ghg_res given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.ResMAC_fos") %>%
      add_precursors("emissions/A_regions", "emissions/mappings/CEDS_sector_tech_proc", "emissions/mappings/CEDS_sector_tech_proc_revised",
                     "L152.MAC_pct_R_S_Proc_EPA", "L201.ghg_res") ->
      L252.ResMAC_fos

    L252.ResMAC_fos_phaseInTime %>%
      add_title("mac.phase.in.time for Fossil Resources MAC") %>%
      add_units("years of fully phasing in MAC reductions") %>%
      add_comments("smooth phase in for early modeling periods") %>%
      add_legacy_name("L252.ResMAC_fos_phaseInTime") %>%
      same_precursors_as("L252.ResMAC_fos_tc") %>%
      add_precursors("emissions/EPA_MACC_PhaseInTime") ->
      L252.ResMAC_fos_phaseInTime

    L252.ResMAC_fos_tc_average %>%
      add_title("Tech.change for Fossil Resources MAC based on EPA 2020 level") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L201.ghg_res given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.ResMAC_fos") %>%
      same_precursors_as("L252.ResMAC_fos_tc") ->
      L252.ResMAC_fos_tc_average

    L252.AgMAC %>%
      add_title("Marginal Abatement Cost Curves for Agriculture based on EPA 2020 level") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L211.AGREmissions and L211.AGRBio given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.AgMAC") %>%
      add_precursors("emissions/A_regions", "emissions/mappings/CEDS_sector_tech_proc", "emissions/mappings/CEDS_sector_tech_proc_revised",
                     "L152.MAC_pct_R_S_Proc_EPA", "L211.AGREmissions", "L211.AGRBio") ->
      L252.AgMAC

    L252.AgMAC_tc_average %>%
      add_title("tech.change for Agriculture MAC based on EPA 2020 level") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L211.AGREmissions and L211.AGRBio given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.AgMAC") %>%
      same_precursors_as("L252.AgMAC_tc") ->
      L252.AgMAC_tc_average

    L252.MAC_an %>%
      add_title("Marginal Abatement Cost Curves for Animals based on EPA 2020 level") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L211.AnEmissions given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.MAC_an") %>%
      add_precursors("emissions/A_regions", "emissions/mappings/CEDS_sector_tech_proc", "emissions/mappings/CEDS_sector_tech_proc_revised",
                     "L152.MAC_pct_R_S_Proc_EPA", "L211.AnEmissions") ->
      L252.MAC_an

    L252.MAC_an_tc_average %>%
      add_title("Tech.chage for Animals MAC based on EPA 2020 level") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L211.AnEmissions given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.MAC_an") %>%
      same_precursors_as("L252.MAC_an_tc") ->
      L252.MAC_an_tc_average

    L252.MAC_prc %>%
      add_title("Marginal Abatement Cost Curves for Industrial and Urban Processing Greenhouse Gases based on EPA 2020 level") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L232.nonco2_prc given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.MAC_prc") %>%
      add_precursors("emissions/A_regions", "emissions/mappings/CEDS_sector_tech_proc", "emissions/mappings/CEDS_sector_tech_proc_revised",
                     "L152.MAC_pct_R_S_Proc_EPA", "L232.nonco2_prc") ->
      L252.MAC_prc

    L252.MAC_prc_phaseInTime %>%
      add_title("mac.phase.in.time for Industrial and Urban Processing Greenhouse Gases MAC") %>%
      add_units("years of fully phasing in MAC reductions") %>%
      add_comments("smooth phase in for early modeling periods") %>%
      add_legacy_name("L252.MAC_prc_phaseInTime") %>%
      same_precursors_as("L252.MAC_prc_tc") %>%
      add_precursors("emissions/EPA_MACC_PhaseInTime") ->
      L252.MAC_prc_phaseInTime

    L252.MAC_prc_tc_average %>%
      add_title("tech.change for Industrial and Urban Processing Greenhouse Gases MAC based on EPA 2020 level") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L232.nonco2_prc given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.MAC_prc") %>%
      same_precursors_as("L252.MAC_prc_tc") ->
      L252.MAC_prc_tc_average

    L252.MAC_higwp %>%
      add_title("Marginal Abatement Cost Curves for High GWP Gases based on EPA 2020 level") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L241.hfc_all and L241.pfc_all given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.MAC_higwp") %>%
      add_precursors("emissions/A_regions", "emissions/mappings/CEDS_sector_tech_proc", "emissions/mappings/CEDS_sector_tech_proc_revised",
                     "L152.MAC_pct_R_S_Proc_EPA", "L241.hfc_all", "L241.pfc_all", "common/GCAM_region_names",
                     "emissions/A_MACC_TechChange_omit") ->
      L252.MAC_higwp

    L252.MAC_higwp_phaseInTime %>%
      add_title("mac.phase.in.time for High GWP Gases MAC") %>%
      add_units("years of fully phasing in MAC reductions") %>%
      add_comments("smooth phase in for early modeling periods") %>%
      add_legacy_name("L252.MAC_higwp_phaseInTime") %>%
      same_precursors_as("L252.MAC_higwp_tc") %>%
      add_precursors("emissions/EPA_MACC_PhaseInTime") ->
      L252.MAC_higwp_phaseInTime

    L252.MAC_higwp_tc_average %>%
      add_title("tech.change for High GWP Gases MAC based on EPA 2020 level") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("Category data from L241.hfc_all and L241.pfc_all given tax and mac.reduction data from L152.MAC_pct_R_S_Proc_EPA") %>%
      add_legacy_name("L252.MAC_higwp") %>%
      same_precursors_as("L252.MAC_higwp_tc") ->
      L252.MAC_higwp_tc_average

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

    L252.MAC_higwp_TC[["SSP1"]] %>%
      add_legacy_name("L252.MAC_higwp_TC_SSP1") ->
      L252.MAC_higwp_TC_SSP1

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

    L252.MAC_higwp_TC[["SSP2"]] %>%
      add_legacy_name("L252.MAC_higwp_TC_SSP2") ->
      L252.MAC_higwp_TC_SSP2

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

    L252.MAC_higwp_TC[["SSP5"]] %>%
      add_legacy_name("L252.MAC_higwp_TC_SSP5") ->
      L252.MAC_higwp_TC_SSP5

    return_data(L252.ResMAC_fos, L252.AgMAC, L252.MAC_an, L252.MAC_prc, L252.MAC_higwp,
                L252.ResMAC_fos_tc_average, L252.AgMAC_tc_average, L252.MAC_an_tc_average,
                L252.MAC_prc_tc_average, L252.MAC_higwp_tc_average,
                L252.ResMAC_fos_phaseInTime, L252.MAC_prc_phaseInTime, L252.MAC_higwp_phaseInTime,
                L252.MAC_Ag_TC_SSP1, L252.MAC_An_TC_SSP1, L252.MAC_prc_TC_SSP1, L252.MAC_res_TC_SSP1, L252.MAC_higwp_TC_SSP1,
                L252.MAC_Ag_TC_SSP2, L252.MAC_An_TC_SSP2, L252.MAC_prc_TC_SSP2, L252.MAC_res_TC_SSP2, L252.MAC_higwp_TC_SSP2,
                L252.MAC_Ag_TC_SSP5, L252.MAC_An_TC_SSP5, L252.MAC_prc_TC_SSP5, L252.MAC_res_TC_SSP5, L252.MAC_higwp_TC_SSP5)
  } else {
    stop("Unknown command")
  }
}
