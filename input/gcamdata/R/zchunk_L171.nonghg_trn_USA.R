#' module_gcamusa_L171.nonghg_trn_USA
#'
#' Emissions factors for transportation by MARKAL technology in the USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y}, \code{L171.nonco2_tgpkm_state_trn_SMarkal_F_Y}. The corresponding file in the
#' original data system was \code{LA171.nonCO2_trn_USA_S_T_Y.R} (gcam-usa level1).
#' @details Emissions factors in Tg/million pass-km for transportation by MARKAL technology in the USA.
#' In L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y, the evolution of EFs are computed with two different methods
#' 1) For the future evolution of fleet EFs for vehicles existing in the base year, we account for each
#' individual vintage degrading as they age, and older vintages retiring.
#' 2) For the degradation of future year EFs, EFs are taken directly from MARKAL aside from a unit conversion.
#' In L171.nonco2_tgpkm_state_trn_SMarkal_F_Y, EFs are computed with two different methods:
#' 1) Base year EFs are the weighted means of the vintaged data using the age fractions as weights.
#' 2) Future year EFs are taken directly from MARKAL for the year = vintage.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BY Aug 2019, MAW March 2022

module_gcamusa_L171.nonghg_trn_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE="gcam-usa/emissions/MARKAL_MOVES_class",
             FILE="gcam-usa/emissions/MOVES_vehicle_age_fractions",
             FILE="gcam-usa/emissions/MOVES_src_type_reg_class_fractions",
             FILE="gcam-usa/emissions/MOVES_source_type_pop",
             FILE="gcam-usa/emissions/MOVES_VMT_dist",
             FILE="gcam-usa/emissions/MOVES_VMT_dist_missing_mapping",
             FILE="gcam-usa/emissions/MARKAL_LDV_EFs_gpm",
             "L254.StubTranTechLoadFactor",
             FILE="gcam-usa/emissions/trnMARKAL_UCD_mapping",
             FILE="gcam-usa/emissions/MARKAL_fuel_name_code",
             FILE="gcam-usa/emissions/GREET2014_LDV_CNG_EFs_tgEJ",
             FILE="gcam-usa/emissions/MARKAL_LDV_eff_vmtMJ",
             FILE="gcam-usa/emissions/MARKAL_LDV_eff_missing_mapping",
             FILE="gcam-usa/emissions/MARKAL_HDV_EFs_gpm",
             FILE="gcam-usa/states_subregions",
             FILE="gcam-usa/emissions/MOVES_EV_Efs_ORD",
             FILE="gcam-usa/emissions/NEI_pollutant_mapping",
             FILE="gcam-usa/emissions/MARKAL_GCAM_mapping",
             FILE="gcam-usa/emissions/MOVES_motorcycle_data"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y",
             "L171.nonco2_tgpkm_state_trn_SMarkal_F_Y"))
  } else if(command == driver.MAKE) {

    # Silence package check.
    yearID <- sourceTypeID <- ageID <- Vintage <- ageFraction <- modelYearID <- regClassID <- stmyFraction <-
      sourceTypePopulation <- MOVES_to_MARKAL_Mapping <- MARKAL_Class <- regClassPop <- sourceTypeID.new <-
      Class.orig <- Class.new <- MOVES.sourceTypeID <- Age <- Class.Of.Car <- Miles.Traveled <- MOVES_Reg_Class <-
      VMT.Weight <- VMT.Weighted.Age.Frac <- sourceTypePopulation.agg <- share <- year <- variable <- value <- Class <-
      Fuel <- pollutant <- region <- size.class <- UCD_technology <- loadFactor <- Fuel_name <- MARKAL_mode <- UCD_mode <-
      `2010` <- `2015` <- Technology <- Unit <- Fuel.orig <- Fuel.new <- value.EF <- value.EF_vmtMJ <- value.eff_vmtMJ <- `2005` <- UCD_fuel <-
      UCD_class <- fuel <- vintage <- state <- DIVISION <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    MARKAL_MOVES_class <- get_data(all_data, "gcam-usa/emissions/MARKAL_MOVES_class")
    MOVES_vehicle_age_fractions <- get_data(all_data, "gcam-usa/emissions/MOVES_vehicle_age_fractions")
    MOVES_src_type_reg_class_fractions <- get_data(all_data, "gcam-usa/emissions/MOVES_src_type_reg_class_fractions")
    MOVES_source_type_pop <- get_data(all_data, "gcam-usa/emissions/MOVES_source_type_pop")
    MOVES_vmt_dist <- get_data(all_data, "gcam-usa/emissions/MOVES_VMT_dist")
    MOVES_vmt_dist_missing_mapping <- get_data(all_data, "gcam-usa/emissions/MOVES_VMT_dist_missing_mapping")
    MARKAL_LDV_EFs_gpm <- get_data(all_data, "gcam-usa/emissions/MARKAL_LDV_EFs_gpm")
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor")
    trnMARKAL_UCD_mapping <- get_data(all_data, "gcam-usa/emissions/trnMARKAL_UCD_mapping")
    MARKAL_fuel_name_code <- get_data(all_data, "gcam-usa/emissions/MARKAL_fuel_name_code")
    GREET2014_LDV_CNG_EFs_tgEJ <- get_data(all_data, "gcam-usa/emissions/GREET2014_LDV_CNG_EFs_tgEJ")
    MARKAL_LDV_eff_vmtMJ <- get_data(all_data, "gcam-usa/emissions/MARKAL_LDV_eff_vmtMJ")
    MARKAL_LDV_eff_missing_mapping <- get_data(all_data, "gcam-usa/emissions/MARKAL_LDV_eff_missing_mapping")
    MARKAL_HDV_EFs_gpm <- get_data(all_data, "gcam-usa/emissions/MARKAL_HDV_EFs_gpm")
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    MOVES_EV_Efs_ORD <- get_data(all_data, "gcam-usa/emissions/MOVES_EV_Efs_ORD")
    NEI_pollutant_mapping <- get_data(all_data, "gcam-usa/emissions/NEI_pollutant_mapping")
    MARKAL_GCAM_mapping <- get_data(all_data, "gcam-usa/emissions/MARKAL_GCAM_mapping")
    MOVES_motorcycle_data <- get_data(all_data, "gcam-usa/emissions/MOVES_motorcycle_data")
    # -----------------------------------------------------------------------------

    # Perform computations
    # =================================================================================
    # 1. Age and VMT fraction data
    # This section assigns age fractions from MOVES to each vehicle class by year / vintage
    # The purpose of assigning age fractions is so we can weight EFs by age fraction of
    # each vintage existing in a given year to accurately capture the fleet EF
    # =================================================================================

    # 1a. Age fraction data
    # =================================================================================
    # The base year vehicle emissions must incorporate past vintages. Here we will assign age fractions to
    # each vehicle vintage in the base years from the MOVES assumptions

    # Group 1: Vehicles that do not have a regulatory class. Includes cars, SUVs, buses, heavy duty (but not pickup/commercial) trucks
    group1_sourceTypeID <- unique(MARKAL_MOVES_class$MOVES_Source_Type[is.na(MARKAL_MOVES_class$MOVES_Reg_Class)])

    # Select vintages from the MOVES data
    MOVES_trn_age_fractions.group1 <- MOVES_vehicle_age_fractions %>%
      filter(yearID %in% MODEL_YEARS, sourceTypeID %in% group1_sourceTypeID) %>%
      mutate(Vintage = yearID - ageID) %>%
      # Only including vintages >= 1990 and years >= 2005.
      filter(Vintage >= gcamusa.MOVES_MIN_VINTAGE_YEAR & yearID >= min(gcamusa.MOVES_BASE_YEAR_CLASSES)) %>%
      # Assign vintages into five-year bins since emissions factor data is provided in 5 year increments.
      # Each bin gets its year and the 4 years preceding it (i.e. 2010 has 2006-2010).
      mutate(Vintage = ceiling(Vintage/gcamusa.TRN_EF_timestep)*gcamusa.TRN_EF_timestep) %>%
      # Aggregate so the age fractions still add up to one
      group_by(sourceTypeID, yearID, Vintage) %>%
      summarise(ageFraction = sum(ageFraction))%>%
      mutate(Age = yearID-Vintage) %>%
      ungroup()

    # Group 2: Vehicles that have a regulatory class. Pickup and commercial truck MARKAL classes. Grouped by MOVES source type and regulatory class.
    group2_sourceTypeID <- unique(MARKAL_MOVES_class$MOVES_Source_Type[!(is.na(MARKAL_MOVES_class$MOVES_Reg_Class))])
    group2_regClassID <- unique(MARKAL_MOVES_class$MOVES_Reg_Class)

    # Fix MOVES reg class fractions: in raw data, there are multiple entries for sourceTypeID/year/regClassID combinations.
    # Fix by summarizing data
    MOVES_src_type_reg_class_fractions <- MOVES_src_type_reg_class_fractions %>%
      group_by(sourceTypeID, modelYearID, regClassID)%>%
      summarise(stmyFraction = sum(stmyFraction))%>%
      ungroup()

    # Get populations (number of vehicles) by source type, vintage and year
    MOVES_source_type_pop_vintaged <- MOVES_source_type_pop %>%
      repeat_add_columns(tibble::tibble(ageID = unique(MOVES_vehicle_age_fractions$ageID))) %>%
      arrange(ageID) %>%
      # use left_join_keep_first_only, there will be 1 unique combination of sourceTypeID, yearID, ageID
      left_join_keep_first_only(MOVES_vehicle_age_fractions, by = c("sourceTypeID", "yearID", "ageID")) %>%
      mutate(sourceTypePopulation = sourceTypePopulation * ageFraction, modelYearID = yearID - ageID) %>%
      na.omit()

    # Get these vintaged populations (number of vehicles) by regulatory class, source type, vintage and year
    MOVES_regClass_pop_vintaged <- MOVES_source_type_pop_vintaged %>%
      filter(yearID %in% MODEL_YEARS & yearID >= min(gcamusa.MOVES_BASE_YEAR_CLASSES)) %>%
      repeat_add_columns(tibble::tibble(regClassID = unique(MOVES_src_type_reg_class_fractions$regClassID))) %>%
      arrange(regClassID) %>%
      # use left_join_keep_first_only, there will be 1 unique combination of sourceTypeID, modelYearID, regClassID
      left_join_keep_first_only(MOVES_src_type_reg_class_fractions, by = c("sourceTypeID", "modelYearID", "regClassID")) %>%
      mutate(regClassPop = sourceTypePopulation * stmyFraction) %>%
      na.omit() %>%
      # Keep only group 2 vehicles
      filter(regClassID %in% group2_regClassID, sourceTypeID %in% group2_sourceTypeID) %>%
      # Map to MARKAL vehicle types
      left_join_error_no_match(select(MARKAL_MOVES_class, -MOVES_to_MARKAL_Mapping), by = c("sourceTypeID" = "MOVES_Source_Type", "regClassID" = "MOVES_Reg_Class")) %>%
      group_by(MARKAL_Class, yearID, ageID)%>%
      summarise(regClassPop = sum(regClassPop)) %>%
      ungroup() %>%
      group_by(MARKAL_Class, yearID)%>%
      # Get age fractions by MARKAL class and year
      mutate(sumRegClassPop = sum(regClassPop),
             ageFraction = (regClassPop / sumRegClassPop)) %>%
      ungroup() %>%
      select(-regClassPop)

    MARKAL_trn_age_fractions.group2 <- MOVES_regClass_pop_vintaged %>%
      mutate(Vintage = yearID - ageID) %>%
      filter(ageID <= gcamusa.MOVES_MAX_AGE) %>%
      # Assign vintages into five-year bins since emissions factor data is provided in 5 year increments.
      # Each bin gets its year and the 4 years preceding it (i.e. 2010 has 2006-2010).
      mutate(Vintage = ceiling(Vintage/gcamusa.TRN_EF_timestep)*gcamusa.TRN_EF_timestep) %>%
      # Aggregate so the age fractions still add up to one
      group_by(MARKAL_Class, yearID, Vintage) %>%
      summarise(ageFraction = sum(ageFraction))%>%
      mutate(Age = yearID-Vintage) %>%
      ungroup()


    # 1b. VMT Fraction Data
    # =================================================================================
    # Calculate weights based on VMT-by-age, and update vintage age fractions with this weight

    # MOVES data omits some source classes we need. For these classes, use VMT distributions of similar data
    MOVES_vmt_dist.missing <- MOVES_vmt_dist %>%
      # Need to use left_join because only a few vehicle classes are being mapped, the rest will be NA
      left_join(MOVES_vmt_dist_missing_mapping, by = c("MOVES.sourceTypeID" = "sourceTypeID.orig")) %>%
      na.omit() %>%
      mutate(MOVES.sourceTypeID = sourceTypeID.new) %>%
      select(-Class.orig, -Class.new, -sourceTypeID.new)

    # Bind "missing" categories to main table. These categories have been assigned data from similar classes
    MOVES_vmt_dist.group1 <- bind_rows(MOVES_vmt_dist, MOVES_vmt_dist.missing) %>%
      filter(MOVES.sourceTypeID %in% group1_sourceTypeID,
             # Remove vintages over 25 (might be skewing results)
             Age <= gcamusa.MOVES_MAX_AGE) %>%
      # Group ages in 5-year intervals.
      mutate(Age = 5*floor(Age/5)) %>%
      rename(Miles.Traveled = "Miles Traveled") %>%
      # Aggregate by vintage
      group_by(Age, Class.Of.Car, MOVES.sourceTypeID) %>%
      summarise(Miles.Traveled = mean(Miles.Traveled)) %>%
      ungroup()%>%
      # Calculate Weight; normalize to 2005 or 2010
      group_by(Class.Of.Car, MOVES.sourceTypeID)%>%
      # VMT weight is a ratio by age for each vehicle compared to a new vehicle (age = 0) of the same class
      mutate(VMT.Weight = Miles.Traveled/Miles.Traveled[Age==0]) %>%
      ungroup()%>%
      select( -c( Class.Of.Car, Miles.Traveled ) )

    # For group2 sourcetypeIDs, aggregate based on MARKAL class
    MARKAL_vmt_dist.group2 <- MOVES_vmt_dist %>%
      filter(MOVES.sourceTypeID %in% group2_sourceTypeID,
             # Remove vintages over 25 (might be skewing results)
             Age <= gcamusa.MOVES_MAX_AGE) %>%
      # using left join as some NAs will be retained (some vehicles do not have a reg class)
      left_join(select(MARKAL_MOVES_class, -MOVES_to_MARKAL_Mapping), by = c("MOVES.sourceTypeID" = "MOVES_Source_Type")) %>%
      # retain only group 2 (with regulatory class)
      filter(!is.na(MOVES_Reg_Class)) %>%
      # Group ages in 5-year intervals.
      mutate(Age = 5*floor(Age/5)) %>%
      rename(Miles.Traveled = "Miles Traveled") %>%
      # Commercial trucks will not be weighted by VMT distribution
      filter(MARKAL_Class != "Commercial truck") %>%
      group_by(MARKAL_Class, Age)%>%
      summarise(Miles.Traveled = sum(Miles.Traveled))%>%
      ungroup()%>%
      group_by(MARKAL_Class)%>%
      # VMT weight is a ratio by age for each vehicle compared to a new vehicle (age = 0) of the same class
      mutate(VMT.Weight = Miles.Traveled / Miles.Traveled[Age==0])%>%
      ungroup()%>%
      select(-Miles.Traveled)

    # 1c. Join VMT with Age fraction data
    # =================================================================================
    MOVES_trn_vmt_wt_age_fractions.group1 <- MOVES_trn_age_fractions.group1 %>%
      full_join(MOVES_vmt_dist.group1, by = c("sourceTypeID" = "MOVES.sourceTypeID", "Age" = "Age")) %>%
      na.omit() %>% #FOR NOW, omitting NAs. hese exist because of discrepancy between VMT and age fraction data that is filtered out
      # Multiply Age fraction by VMT weight, except for motorcycles (which do not exist)
      # mutate(VMT.Weighted.Age.Frac = if_else(is.na(VMT.Weight), ageFraction, ageFraction*VMT.Weight))
      mutate(VMT.Weighted.Age.Frac = ageFraction*VMT.Weight)

    MARKAL_trn_vmt_wt_age_fractions.group2 <- MARKAL_trn_age_fractions.group2 %>%
      full_join(MARKAL_vmt_dist.group2, by = c("MARKAL_Class", "Age")) %>%
      mutate(VMT.Weighted.Age.Frac = if_else(is.na(VMT.Weight), ageFraction, ageFraction*VMT.Weight)) %>%
      filter(!is.na(VMT.Weighted.Age.Frac)) #FOR NOW, omitting NAs. These exist because of discrepancy between VMT and age fraction data that is filtered out

    # =================================================================================
    # 2. MOVES to MARKAL Mapping for Age Fractions
    # This section maps the processed MOVES age fractions to MARKAL classes, since the
    # emission factors are in terms of MARKAL classes.

    # =================================================================================
    # 2a. Single MOVES sourceTypeID to multiple MARKAL classes
    # =================================================================================
    # In MARKAL_MOVES_class file, "A" in MOVES_to_MARKAL_Mapping column designated a single MOVES sourceTypeID to multiple MARKAL classes

    MOVES_groupA_fractions <- MARKAL_MOVES_class %>%
      filter(MOVES_to_MARKAL_Mapping == "A") %>%
      # need to use left join because we are changing the number of rows in the table, having a row for every vintage group and year
      left_join(MOVES_trn_vmt_wt_age_fractions.group1, by = c("MOVES_Source_Type" = "sourceTypeID")) %>%
      select( c( MARKAL_Class, yearID, Vintage, VMT.Weighted.Age.Frac ) )

    # 2b. Multiple MOVES sourceTypeIDs to single MARKAL class
    # =================================================================================

    # For MARKAL classes including buses and light and heavy duty trucks, data from multiple
    # MOVES source categories were used, so the age fractions must be multiplied by shares of each source type ID
    # out of the total vehicle population of the source IDs used in the corresponding MARKAL class.

    MOVES_srcIDs <- unique(MARKAL_MOVES_class$MOVES_Source_Type[MARKAL_MOVES_class$MOVES_to_MARKAL_Mapping == "B"])

    # First compute the total vehicle population for these MARKAL classes in the relevant years
    MOVES_agg_pop <- MOVES_source_type_pop %>%
      # need to do left join, changing the number of rows, having a row for each MARKAL_Class
      left_join(MARKAL_MOVES_class,  by = c("sourceTypeID" = "MOVES_Source_Type")) %>%
      filter(MOVES_to_MARKAL_Mapping == "B" & yearID %in% MOVES_trn_age_fractions.group1$yearID)  %>%
      group_by(MARKAL_Class, yearID) %>%
      summarise(sourceTypePopulation.agg = sum(sourceTypePopulation))

    # Compute shares of the source ID categories out of the corresponding total vehicle population
    MOVES_pop_shares <- MOVES_source_type_pop %>%
      # need to do left join, changing the number of rows, having a row for each MARKAL_Class
      left_join(MARKAL_MOVES_class,  by = c("sourceTypeID" = "MOVES_Source_Type")) %>%
      filter(MOVES_to_MARKAL_Mapping == "B" & yearID %in% MOVES_trn_age_fractions.group1$yearID)  %>%
      left_join_error_no_match(MOVES_agg_pop, by = c("yearID", "MARKAL_Class")) %>%
      mutate(share = sourceTypePopulation / sourceTypePopulation.agg) %>%
      select( c( yearID, sourceTypeID, MARKAL_Class, share ) )

    # Now multiply the source ID shares by the age fractions to yield new age fractions for the aggregate MARKAL category
    # Update VMT weighted age frac accordingly.
    MOVES_groupB_fractions <- MOVES_trn_vmt_wt_age_fractions.group1 %>%
      filter(sourceTypeID %in% MOVES_srcIDs) %>%
      left_join_error_no_match(MOVES_pop_shares, by = c("sourceTypeID", "yearID")) %>%
      mutate(VMT.Weighted.Age.Frac = VMT.Weighted.Age.Frac * share) %>%
      group_by(MARKAL_Class, yearID,Vintage) %>%
      summarise(VMT.Weighted.Age.Frac = sum(VMT.Weighted.Age.Frac))

    # Bind group A and B fractions together as group 1
    MOVES_trn_fractions.group1 <- bind_rows(MOVES_groupA_fractions, MOVES_groupB_fractions) %>%
      rename(year=yearID)

    MARKAL_trn_fractions.group2 <- MARKAL_trn_vmt_wt_age_fractions.group2 %>%
      rename(year=yearID)%>%
      select( c( year, Vintage, VMT.Weighted.Age.Frac, MARKAL_Class ) )%>%
      group_by(year, MARKAL_Class)%>%
      mutate(VMT.Weighted.Age.Frac = VMT.Weighted.Age.Frac / sum(VMT.Weighted.Age.Frac))%>%
      ungroup()


    MARKAL_trn_fractions <- bind_rows(MOVES_trn_fractions.group1, MARKAL_trn_fractions.group2)

    # =================================================================================
    # 3. Prepare Emission Factors
    # This section computes adjusted emission factors weighted by age fraction as well
    # as emission factors to use for the linear control and converts them to GCAM units
    # =================================================================================

    # 3a. Light Duty Vehicles
    # =================================================================================

    # Gather the raw data
    MARKAL_LDV_EFs_gpm.long <- MARKAL_LDV_EFs_gpm %>%
      tidyr::gather(variable, value, -Class, -Fuel, -Vintage, convert=T) %>%
      separate(variable, into = c("pollutant", "year", "region"), sep="\\.", convert = T) %>%
      mutate(pollutant = gsub("PM2_5", "PM2.5", pollutant)) %>%
      ###NOTE: filtering out some fuels for now for lack of efficiency/service demand data, and also the CO2 data
      filter(pollutant != "CO2" & !(Fuel %in% gcamusa.MARKAL_LDV_FILTER_OUT_FUELS))

    # Gather a table for use in calculating degradation of EFs for future vintages
    L171.LDV_USA_emiss_degrades <- MARKAL_LDV_EFs_gpm.long %>%
      filter(Vintage > max(gcamusa.TRAN_MODEL_BASE_YEARS) & !is.na(value))

    # Clean up and subset base year vintages
    MARKAL_LDV_EFs_gpm_Yb <- MARKAL_LDV_EFs_gpm.long %>%
      filter(year %in% gcamusa.TRAN_MODEL_BASE_YEARS & Vintage <= year & year,
             year - Vintage <= gcamusa.MARKAL_DEGRADE_YEARS) %>%
      ###MISSING VALUES: emission factors pollutants with no data for ELC vehicles, and 2010 EFs for 1990 vintages
      na.omit() %>%
      # Match base year age fractions onto the table containing the EFs in the base years
      left_join_error_no_match(MARKAL_trn_fractions, by = c("Class" = "MARKAL_Class", "Vintage", "year")) %>%
      rename(ageFraction = VMT.Weighted.Age.Frac)

    # The base year EFs will be weighted means of the vintaged data using the age fractions as weights
    # The EFs here represent all existing vehicles in each base year
    MARKAL_LDV_EFs_gpm_Yb.avg <- MARKAL_LDV_EFs_gpm_Yb %>%
      group_by(region, Class, Fuel, pollutant, year) %>%
      summarise(value = weighted.mean(value, ageFraction))

    # For the current base year, calculate the future evolution of fleet EFs for vehicles existing in the
    # base year. Two counteracting forces are happening here: 1. Each individual vintage is degrading as
    # they age, resulting in an increased EF and 2. older vintages are retiring, resulting in a decreased EF.
    # We need to account for both of these forces to capture an accurate net-evolution
    MARKAL_LDV_EFs_gpm_CYb <- MARKAL_LDV_EFs_gpm.long %>%
      filter(year >= max(gcamusa.TRAN_MODEL_BASE_YEARS) & Vintage <= max(gcamusa.TRAN_MODEL_BASE_YEARS)) %>%
      filter( year - Vintage <= gcamusa.MARKAL_DEGRADE_YEARS ) %>%
      ###MISSING VALUES: emission factors pollutants with no data for ELC vehicles, and 2010 EFs for 1990 vintages
      na.omit() %>%
      # Match base year age fractions onto the table containing the EFs in the base years
      left_join_error_no_match(MARKAL_trn_fractions, by = c("Class" = "MARKAL_Class", "Vintage", "year")) %>%
      mutate(AF.Weighted.EF = VMT.Weighted.Age.Frac * value) %>%
      group_by(region, Class, Fuel, pollutant, year) %>%
      mutate(value = sum(AF.Weighted.EF)) %>%
      distinct(value, region, Class, Fuel, Vintage, pollutant, year ) %>%
      # filter for the base year vintage, as this entry has values for how EFs for vehicles
      # existing in the base year will evolve
      filter( Vintage == max(gcamusa.TRAN_MODEL_BASE_YEARS))

    # Clean up and subset future vintages for emissions coefficients
    MARKAL_LDV_EFs_gpm_Yf <- MARKAL_LDV_EFs_gpm.long %>%
      filter(Vintage >= min(gcamusa.TRAN_MODEL_FUTURE_YEARS) & Vintage == year & !is.na(value)) %>%
      na.omit() %>%
      select(-Vintage)

    # Prepare Load factor
    StubTranTechLoadFactor_USA <- L254.StubTranTechLoadFactor %>%
      filter(region == "USA",
             sce == "CORE") %>%
      select( -c( region, sce ) )

    # Make a table with the base and future year emission factors
    MARKAL_LDV_EFs_gpm_Y.long <- bind_rows(MARKAL_LDV_EFs_gpm_Yb.avg, MARKAL_LDV_EFs_gpm_Yf)

    MARKAL_LDV_EFs_tgpkm_Y.long <- MARKAL_LDV_EFs_gpm_Y.long %>%
      # Match UCD sector to MARKAL LDV EFs
      # Note: We lose EFs from PH20E, PH20G, PH40E, and PH40G  from compact car due to lack of
      # matching UCD fuel. This would get cut anyways in L271, so this shouldn't make a difference to emissions.
      left_join_keep_first_only(trnMARKAL_UCD_mapping, by = c("Class" = "MARKAL_class", "Fuel" = "MARKAL_fuel")) %>%
      rename(UCD_class = size.class, UCD_fuel = UCD_technology) %>%
      left_join_keep_first_only(StubTranTechLoadFactor_USA, by = c("UCD_class" = "tranSubsector", "UCD_fuel" = "stub.technology", "year")) %>%
      # Convert from g per vehicle miles to Tg per million passenger km
      mutate(value = (value * CONV_PERS_MILPERS * CONV_G_TG) / (loadFactor * CONV_MI_KM)) %>%
      left_join_keep_first_only(MARKAL_fuel_name_code, by = c("Fuel" = "Fuel_code")) %>%
      select( c( region, Class, Fuel, pollutant, year, value, Fuel_name ) )

    # Import LDV CNG emission factors from GREET data and copy for all categories in the LDV table
    GREET_LDV_CNG_EFs_tgEJ_Y <- GREET2014_LDV_CNG_EFs_tgEJ %>%
      repeat_add_columns(tibble::tibble(region = unique(MARKAL_LDV_EFs_tgpkm_Y.long$region))) %>%
      repeat_add_columns(tibble::tibble(Class = unique(MARKAL_LDV_EFs_tgpkm_Y.long$Class))) %>%
      repeat_add_columns(tibble::tibble(year = unique(MARKAL_LDV_EFs_tgpkm_Y.long$year))) %>%
      mutate(Fuel_name = "Natural Gas") %>%
      left_join_error_no_match(trnMARKAL_UCD_mapping, by = c("Class" = "MARKAL_class", "Fuel" = "MARKAL_fuel")) %>%
      select( -c( MARKAL_mode, UCD_mode ) ) %>%
      rename(UCD_class = size.class, UCD_fuel = UCD_technology)

    # Prepare Efficiency Data
    MARKAL_LDV_eff_vmtMJ <- MARKAL_LDV_eff_vmtMJ %>%
      # Manually linearly extrapolate backwards to add 2005
      mutate(`2005` = 2*`2010` - `2015`) %>%
      select( -c( Technology, Unit ) ) %>%
      tidyr::gather(year, value, -Class, -Fuel) %>%
      mutate( year = as.numeric( year ) )

    # No efficiency data for some classes needed. Replace with similar classes (from mapping file)
    # effs respectively.
    # Must use left_join because changing the number of rows, having a row for each year
    MARKAL_LDV_eff_vmtMJ_missing <- MARKAL_LDV_eff_missing_mapping %>%
      left_join(MARKAL_LDV_eff_vmtMJ, by = c("Class.orig" = "Class", "Fuel.orig" = "Fuel")) %>%
      select( -c( Class.orig, Fuel.orig ) ) %>%
      rename(Class = Class.new, Fuel = Fuel.new)

    MARKAL_LDV_eff_vmtMJ <- bind_rows(MARKAL_LDV_eff_vmtMJ, MARKAL_LDV_eff_vmtMJ_missing)

    # Convert CNG emission factors from Tg/EJ to Tg/million pass-km, the output EF unit
    GREET_LDV_CNG_EFs_tgpkm_Y <- GREET_LDV_CNG_EFs_tgEJ_Y %>%
      left_join_error_no_match(MARKAL_LDV_eff_vmtMJ, by = c("Fuel", "Class", "year"), suffix = c(".EF", ".eff_vmtMJ")) %>%
      left_join_error_no_match(StubTranTechLoadFactor_USA, by = c("UCD_fuel" = "stub.technology", "UCD_class" = "tranSubsector", "year")) %>%
      mutate(value.EF = (value.EF * CONV_MJ_EJ * CONV_PERS_MILPERS) / (value.eff_vmtMJ * CONV_MI_KM * loadFactor)) %>%
      rename(value = value.EF) %>%
      select( c( pollutant, Fuel, value, region, Class, year, Fuel_name ) )

    # Bind CNG data onto MARKAL table, spread to wide format
    MARKAL_LDV_EFs_tgpkm_Y <- bind_rows(MARKAL_LDV_EFs_tgpkm_Y.long, GREET_LDV_CNG_EFs_tgpkm_Y) %>%
      spread(key = year,value) %>%
      ###MISSING VALUES: ELC Minivans in 2005 and all vehicles using E85 in 2005. Fill with 2010 data for now
      # Also fill in any zeroes in 2005 with 2010 data
      mutate(`2005` = if_else(is.na(`2005`),`2010`,
                            if_else(`2005` == 0, `2010`, `2005`))) %>%
      #MISSING VALUES: Filter out the following Mini car fuels: DSL, E10, E15 because they are not included in input emissions due to lack of load factor
      filter(!(Class == "Mini car" & Fuel %in% gcamusa.MARKAL_MINICAR_FILTER_OUT_FUELS)) %>%
      select(-Fuel_name) %>%
      arrange(region, Class, Fuel, pollutant)

    # Bind base year EF evolution with future year EF degradation and convert table
    # containing emission coefficient evolution/degradation paths to Tg/million pass-km
    L171.LDV_USA_emiss_degrades_tgpkm <- L171.LDV_USA_emiss_degrades %>%
      bind_rows(MARKAL_LDV_EFs_gpm_CYb) %>%
      left_join_keep_first_only(trnMARKAL_UCD_mapping, by = c("Class" = "MARKAL_class", "Fuel" = "MARKAL_fuel")) %>%
      rename(UCD_class = size.class, UCD_fuel = UCD_technology) %>%
      left_join_keep_first_only(StubTranTechLoadFactor_USA, by = c("UCD_class" = "tranSubsector", "UCD_fuel" = "stub.technology", "year")) %>%
      # Convert from g per vehicle miles to Tg per million passenger km
      mutate(value = (value * CONV_PERS_MILPERS * CONV_G_TG) / (loadFactor * CONV_MI_KM)) %>%
      left_join_keep_first_only(MARKAL_fuel_name_code, by = c("Fuel" = "Fuel_code")) %>%
      ###MISSING VALUES: where there is no load factor data for Mini car fuels other than gasoline or electricity. Remove for now
      na.omit() %>%
      select( c( Class, Fuel, Vintage, pollutant, year, region, value, Fuel_name ) )

    # Add a Vintage column to the CNG data and bind it to the table containing the degradation paths
    GREET_LDV_CNG_EFs_tgpkm_Y.vintage <- GREET_LDV_CNG_EFs_tgpkm_Y %>%
      repeat_add_columns(tibble::tibble(Vintage = unique(L171.LDV_USA_emiss_degrades_tgpkm$Vintage)))

    L171.LDV_USA_emiss_degrades_tgpkm <- bind_rows(L171.LDV_USA_emiss_degrades_tgpkm, GREET_LDV_CNG_EFs_tgpkm_Y.vintage) %>%
      ###MISSING VALUES: Filter out the following Mini car fuels: DSL, E10, E15 because they are not included in input emissions due to lack of load factor
      filter(!(Class == "Mini car" & Fuel %in% gcamusa.MARKAL_MINICAR_FILTER_OUT_FUELS))

    # 3b. Heavy Duty Vehicles
    # =================================================================================
    # Gather the raw data
    MARKAL_HDV_EFs_gpm.long <- MARKAL_HDV_EFs_gpm %>%
      tidyr::gather(variable ,value, -Class, -Fuel, -Vintage, convert=T) %>%
      separate(variable, into = c("pollutant", "year", "region"), sep="\\.", convert = T) %>%
      mutate(pollutant = gsub("PM2_5", "PM2.5", pollutant)) %>%
      ###NOTE: filtering out the CO2 data
      filter(pollutant != "CO2")

    # Gather a table for use in calculating degradation of EFs for each vintage
    L171.HDV_USA_emiss_degrades <- MARKAL_HDV_EFs_gpm.long %>%
      filter(Vintage > max(gcamusa.TRAN_MODEL_BASE_YEARS) & !is.na(value))

    # Clean up and subset base year vintages
    MARKAL_HDV_EFs_gpm_Yb <- MARKAL_HDV_EFs_gpm.long %>%
      filter(year %in% gcamusa.TRAN_MODEL_BASE_YEARS & Vintage <= year & year,
             year - Vintage <= gcamusa.MARKAL_DEGRADE_YEARS) %>%
      # Match base year age fractions onto the table containing the EFs in the base years
      left_join_error_no_match(MARKAL_trn_fractions, by = c("Class" = "MARKAL_Class", "Vintage", "year")) %>%
      rename(ageFraction = VMT.Weighted.Age.Frac)

    # The base year EFs will be weighted means of the vintaged data using the age fractions as weights
    MARKAL_HDV_EFs_gpm_Yb.avg <- MARKAL_HDV_EFs_gpm_Yb %>%
      group_by(region, Class, Fuel, pollutant, year) %>%
      summarise(value = weighted.mean(value, ageFraction))

    # For the current base year, calculate the future evolution of fleet EFs for vehicles existing in the
    # base year. Two counteracting forces are happening here: 1. Each individual vintage is degrading as
    # they age, resulting in an increased EF and 2. older vintages are retiring, resulting in a decreased EF.
    # We need to account for both of these forces to capture an accurate net-evolution
    MARKAL_HDV_EFs_gpm_CYb <- MARKAL_HDV_EFs_gpm.long %>%
      filter(year >= max(gcamusa.TRAN_MODEL_BASE_YEARS) & Vintage <= max(gcamusa.TRAN_MODEL_BASE_YEARS)) %>%
      filter( year - Vintage <= gcamusa.MARKAL_DEGRADE_YEARS ) %>%
      ###MISSING VALUES: emission factors pollutants with no data for ELC vehicles, and 2010 EFs for 1990 vintages
      na.omit() %>%
      # Match base year age fractions onto the table containing the EFs in the base years
      left_join_error_no_match(MARKAL_trn_fractions, by = c("Class" = "MARKAL_Class", "Vintage", "year")) %>%
      mutate(AF.Weighted.EF = VMT.Weighted.Age.Frac * value) %>%
      group_by(region, Class, Fuel, pollutant, year) %>%
      mutate(value = sum(AF.Weighted.EF)) %>%
      distinct(value, region, Class, Fuel, Vintage, pollutant, year ) %>%
      # filter for the base year vintage, as this entry has values for how EFs for vehicles
      # existing in the base year will evolve
      filter( Vintage == max(gcamusa.TRAN_MODEL_BASE_YEARS))

    # Clean up and subset future vintages for emissions coefficients
    MARKAL_HDV_EFs_gpm_Yf <- MARKAL_HDV_EFs_gpm.long %>%
      filter(Vintage >= min(gcamusa.TRAN_MODEL_FUTURE_YEARS) & Vintage == year & !is.na(value)) %>%
      na.omit() %>%
      select(-Vintage)

    # Make a table with the base and future year emission factors
     MARKAL_HDV_EFs_gpm_Y.long <- bind_rows(MARKAL_HDV_EFs_gpm_Yb.avg, MARKAL_HDV_EFs_gpm_Yf)

    # Convert to output-based EFs
    # Use MARKAL vehicle load factors to convert to Tg/million pass-km
    MARKAL_HDV_EFs_tgpkm_Y.long <- MARKAL_HDV_EFs_gpm_Y.long %>%
      left_join_keep_first_only(trnMARKAL_UCD_mapping, by = c("Class" = "MARKAL_class", "Fuel" = "MARKAL_fuel")) %>%
      rename(UCD_class = size.class, UCD_fuel = UCD_technology) %>%
      # mutate some fuels to match correct load factors
      mutate(UCD_fuel = if_else(UCD_fuel=="Hybrid Liquids", "Liquids", UCD_fuel),
             UCD_class = if_else(UCD_class=="All", "Bus", UCD_class)) %>%
      left_join_keep_first_only(StubTranTechLoadFactor_USA, by = c("UCD_class" = "tranSubsector", "UCD_fuel" = "stub.technology", "year")) %>%
      # Convert from g per vehicle miles to Tg per million passenger km
      mutate(value = (value * CONV_PERS_MILPERS * CONV_G_TG) / (loadFactor * CONV_MI_KM)) %>%
      left_join_keep_first_only(MARKAL_fuel_name_code, by = c("Fuel" = "Fuel_code")) %>%
      select( c( region, Class, Fuel, pollutant, year, value, Fuel_name ) )

    # Spread to wide format
    MARKAL_HDV_EFs_tgpkm_Y <- MARKAL_HDV_EFs_tgpkm_Y.long %>%
      spread(key = year, value) %>%
      select(-Fuel_name) %>%
      arrange(region,Class,Fuel,pollutant)

    # Bind base year EF evolution with future year EF degradation and convert table
    # containing emission coefficient evolution/degradation paths to Tg/million pass-km
    L171.HDV_USA_emiss_degrades_tgpkm <- L171.HDV_USA_emiss_degrades %>%
      bind_rows(MARKAL_HDV_EFs_gpm_CYb) %>%
      left_join_keep_first_only(trnMARKAL_UCD_mapping, by = c("Class" = "MARKAL_class", "Fuel" = "MARKAL_fuel")) %>%
      rename(UCD_class = size.class, UCD_fuel = UCD_technology) %>%
      # mutate some fuels to match correct load factors
      mutate(UCD_fuel = if_else(UCD_fuel == "Hybrid Liquids", "Liquids", UCD_fuel),
             UCD_class = if_else(UCD_class == "All", "Bus", UCD_class)) %>%
      left_join_keep_first_only(StubTranTechLoadFactor_USA, by = c("UCD_class" = "tranSubsector", "UCD_fuel" = "stub.technology", "year")) %>%
      # Convert from g per vehicle miles to Tg per million passenger km
      mutate(value = (value * CONV_PERS_MILPERS * CONV_G_TG) / (loadFactor * CONV_MI_KM)) %>%
      left_join_keep_first_only(MARKAL_fuel_name_code, by = c("Fuel" = "Fuel_code")) %>%
      select( c( Class, Fuel, Vintage, pollutant, year, region, value, Fuel_name ) )

    # 3c. Motorcycles
    # =================================================================================
    # Process the raw data
    # Motorcycle EFs are not in MARKAL, so we use MOVES and calculate EFs
    motorcycle_gpm.long <- MOVES_motorcycle_data %>%
      #rename columns for consistency
      rename( Vintage = ModelYr,
              year = Year ) %>%
      #make df long
      tidyr::gather( key = "pollutant", value = "emissions", -c( year, State, Vintage, Distance ) ) %>%
      #convert distance from miles to meters
      mutate( Distance = Distance * CONV_MILE_KM * 1000 ) %>%
      #changing pollutant names for consistency
      mutate( pollutant = gsub( "Total_PM25", "PM2.5", pollutant ),
              pollutant = gsub( "Total_PM10", "PM10", pollutant ) )

    # Gather a table for use in calculating degradation of EFs for each vintage
    # Degradation tables for LDV and HDV have year and vintage in 5 year bins, but having every vintage (yearly) is ok
    # since we use it for a linear fit degradation
    L171.Moto_USA_emiss_degrades <- motorcycle_gpm.long %>%
      filter( Vintage >= max(gcamusa.TRAN_MODEL_BASE_YEARS) ) %>%
      mutate( value = emissions / Distance ) %>%
      select( -c( Distance, emissions ) ) %>%
      #get national average EFs
      group_by( year, Vintage, pollutant ) %>%
      mutate( value = sum( value ) ) %>%
      distinct( year, Vintage, pollutant, value ) %>%
      ungroup() %>%
      #adding class column
      #we will give it class "Motorcycle" which will later be mapped to UCD class 2W and 3W
      mutate( Class = "Motorcycle",
              #adding fuel column
              Fuel = "GSL" ) %>%
      #add the EFs to every "division" for consistency with MARKAL format, later to be apportioned to states
      repeat_add_columns( tibble::tibble( region = unique( states_subregions$DIVISION ) ) )


    # Base year EFs
    # TODO: Make this more robust
    # For each of the historical years, calculate a distance-weighted EF
    # for all of the vehicles that exist in that year.
      # All vehicles existing in 1990 will be given the year 1990
    motorcycle_gpm.long_1990 <- motorcycle_gpm.long %>%
      filter( year == 2010 & Vintage <= 1990 ) %>%
      mutate( year = 1990 )

      # All vehicles existing in 2005 will be given the year 2005
    motorcycle_gpm.long_2005 <- motorcycle_gpm.long %>%
      filter( year == 2010 & Vintage <= 2005 ) %>%
      mutate( year = 2005 )

      # bind the tables back together to calculate distance-weighted EFs
    motorcycle_gpm.long_Yb <- motorcycle_gpm.long %>%
      bind_rows( motorcycle_gpm.long_1990, motorcycle_gpm.long_2005 ) %>%
      # filter for GCAM historical years
      filter( year %in% gcamusa.TRAN_MODEL_BASE_YEARS ) %>%
      # calculate EFs
      mutate( EF = emissions / Distance ) %>%
      # sum Distance across year and pollutant, and get a share to weight EFs by
      group_by( year, pollutant, State ) %>%
      mutate( distance_sum = sum( Distance ),
              distance_share = Distance / distance_sum,
              weighted_EF = EF * distance_share ) %>%
      # sum weighted EFs to get one EF per year, pollutant, and state
      group_by( year, pollutant, State ) %>%
      mutate( state_EF = sum( weighted_EF ) ) %>%
      distinct( year, State, pollutant, state_EF ) %>%
      # get a national average EF for each year and pollutant
      group_by( year, pollutant ) %>%
      mutate( value = mean( state_EF ) ) %>%
      distinct( year, pollutant, value ) %>%
      ungroup()

    # Future year EFs
    # For each of the future years, assign EFs for YEAR = VINTAGE
    motorcycle_gpm.long_Yf <- motorcycle_gpm.long %>%
      # calculate EFs
      mutate( EF = emissions / Distance ) %>%
      # filter for GCAM future years
      filter( year %in% gcamusa.TRAN_MODEL_FUTURE_YEARS & Vintage == year ) %>%
      # get a national average EF for each year and pollutant
      group_by( year, pollutant ) %>%
      mutate( value = mean( EF ) ) %>%
      distinct( year, pollutant, value ) %>%
      ungroup()

    # binding historical and future tables and formatting
    motorcycle_EFs_gpm.long <- motorcycle_gpm.long_Yb %>%
      bind_rows( motorcycle_gpm.long_Yf ) %>%
      #adding class column
      #we will give it class "Motorcycle" which will later be mapped to UCD class 2W and 3W
      mutate( Class = "Motorcycle",
      #adding fuel column
            Fuel = "GSL" ) %>%
      #add the EFs to every "division" for consistency with MARKAL format, later to be apportioned to states
      repeat_add_columns( tibble::tibble( region = unique( states_subregions$DIVISION ) ) )

    # Convert to output-based EFs
    motorcycle_EFs_tgpkm_Y <- motorcycle_EFs_gpm.long %>%
      # Match UCD sector to motorcycle EFs
      left_join_keep_first_only(trnMARKAL_UCD_mapping, by = c("Class" = "MARKAL_class", "Fuel" = "MARKAL_fuel")) %>%
      rename(UCD_class = size.class, UCD_fuel = UCD_technology) %>%
      left_join_keep_first_only(StubTranTechLoadFactor_USA, by = c("UCD_class" = "tranSubsector", "UCD_fuel" = "stub.technology", "year")) %>%
      # Convert from g per vehicle miles to Tg per million passenger km
      mutate(value = (value * CONV_PERS_MILPERS * CONV_G_TG) / (loadFactor * CONV_MI_KM)) %>%
      left_join_keep_first_only(MARKAL_fuel_name_code, by = c("Fuel" = "Fuel_code")) %>%
      #select columns to keep
      select( c( region, Class, Fuel, pollutant, year, value ) ) %>%
      #spread to wide format
      spread( key = year, value )

    # Also convert table containing emission coefficient degradation paths to Tg/million pass-km
    L171.Moto_USA_emiss_degrades_tgpkm <- L171.Moto_USA_emiss_degrades %>%
      left_join_keep_first_only(trnMARKAL_UCD_mapping, by = c("Class" = "MARKAL_class", "Fuel" = "MARKAL_fuel")) %>%
      rename(UCD_class = size.class, UCD_fuel = UCD_technology) %>%
      left_join_keep_first_only(StubTranTechLoadFactor_USA, by = c("UCD_class" = "tranSubsector", "UCD_fuel" = "stub.technology", "year")) %>%
      # Convert from g per vehicle miles to Tg per million passenger km
      mutate(value = (value * CONV_PERS_MILPERS * CONV_G_TG) / (loadFactor * CONV_MI_KM)) %>%
      left_join_keep_first_only(MARKAL_fuel_name_code, by = c("Fuel" = "Fuel_code")) %>%
      select( c( Class, Fuel, Vintage, pollutant, year, region, value, Fuel_name ) )


    # 3d. Apportion to states
    # Here, we assume that emission factors are uniform in their census regions, as MARKAL
    # provides EFs at the census region level.
    # =================================================================================
    # Combine LDV, HDV, and Moto tables
    MARKAL_trn_EFs_tgpkm_Y <- bind_rows(MARKAL_LDV_EFs_tgpkm_Y, MARKAL_HDV_EFs_tgpkm_Y, motorcycle_EFs_tgpkm_Y) %>%
      ungroup()

    L171.trn_USA_emiss_degrades <- bind_rows(L171.HDV_USA_emiss_degrades_tgpkm, L171.LDV_USA_emiss_degrades_tgpkm,
                                             L171.Moto_USA_emiss_degrades_tgpkm)

    # For now keep the degradation rates at the census region level to save time in level2 processing
    L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y <- L171.trn_USA_emiss_degrades %>%
      rename(class = Class, fuel = Fuel, vintage = Vintage) %>%
      select( c( region, class, fuel, vintage, pollutant, year, value ) ) %>%
      arrange(region, class, fuel, vintage, pollutant)%>%
      mutate(year = as.numeric(year)) %>%
      na.omit()

    state_census_region <- states_subregions %>%
      select( c( state, DIVISION ) )

    # Write the emission factors to each state, assuming they are uniform in their census regions
    L171.nonco2_tgpkm_state_trn_SMarkal_F_Y_all <- MARKAL_trn_EFs_tgpkm_Y %>%
      # need to use left join because expanding number of rows based on number of states in each census region
      left_join(state_census_region, by = c("region" = "DIVISION")) %>%
      rename(class = Class, fuel = Fuel) %>%
      select( c( state, class, fuel, pollutant, as.character( gcamusa.TRN_MARKAL_EMISSION_YEARS ) ) ) %>%
      arrange(state, class, fuel, pollutant)%>%
      na.omit()

    # Update PM Efs for electric vehicles
    L171.nonco2_tgpkm_state_trn_SMarkal_F_Y_EVEF_keep <- L171.nonco2_tgpkm_state_trn_SMarkal_F_Y_all %>%
      filter(!(fuel == "ELC" & pollutant %in% c("PM2.5","PM10")))

    L171.nonco2_tgpkm_state_trn_SMarkal_F_Y <- bind_rows(L171.nonco2_tgpkm_state_trn_SMarkal_F_Y_EVEF_keep,
                                                         MOVES_EV_Efs_ORD)


    # ===================================================
    # Produce outputs
    L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y <- L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y %>%
      add_title("Transportation non-co2 emission factor by U.S. census region / MARKAL vehicle class / fuel / vintage / pollutant / year") %>%
      add_units("Tg / million pass-km or Tg / million ton-km") %>%
      add_comments("Transportation non-co2 emission factor by U.S. census region / MARKAL vehicle class / fuel / vintage / pollutant / year.
                   Used for EF degradation/evolution. The base year captures the future evolution of fleet EFs for vehicles existing in the
                   base year") %>%
      add_legacy_name("L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y") %>%
      add_precursors("gcam-usa/emissions/MARKAL_MOVES_class",
                     "gcam-usa/emissions/MOVES_vehicle_age_fractions",
                     "gcam-usa/emissions/MOVES_src_type_reg_class_fractions",
                     "gcam-usa/emissions/MOVES_source_type_pop",
                     "gcam-usa/emissions/MOVES_VMT_dist",
                     "gcam-usa/emissions/MOVES_VMT_dist_missing_mapping",
                     "gcam-usa/emissions/MARKAL_LDV_EFs_gpm",
                     "L254.StubTranTechLoadFactor",
                     "gcam-usa/emissions/trnMARKAL_UCD_mapping",
                     "gcam-usa/emissions/MARKAL_fuel_name_code",
                     "gcam-usa/emissions/GREET2014_LDV_CNG_EFs_tgEJ",
                     "gcam-usa/emissions/MARKAL_LDV_eff_vmtMJ",
                     "gcam-usa/emissions/MARKAL_LDV_eff_missing_mapping",
                     "gcam-usa/emissions/MARKAL_HDV_EFs_gpm",
                     "gcam-usa/states_subregions",
                     "gcam-usa/emissions/MOVES_motorcycle_data")

    L171.nonco2_tgpkm_state_trn_SMarkal_F_Y <- L171.nonco2_tgpkm_state_trn_SMarkal_F_Y %>%
      add_title("Transportation non-co2 emission factor by U.S. state / MARKAL vehicle class / fuel / pollutant / year") %>%
      add_units("Tg / million pass-km or Tg / million ton-km") %>%
      add_comments("Transportation non-co2 emission factor by U.S. state / MARKAL vehicle class / fuel / pollutant / year. Base
                year EFs are averages weighted by vintage, age fraction, and vehicle miles traveled. Future year EFs are directly
                from the MARKAL input for that year and vintage") %>%
      add_legacy_name("L171.nonco2_tgpkm_censusR_trn_SMarkal_F_Y") %>%
      add_precursors("gcam-usa/emissions/MARKAL_MOVES_class",
                     "gcam-usa/emissions/MOVES_vehicle_age_fractions",
                     "gcam-usa/emissions/MOVES_src_type_reg_class_fractions",
                     "gcam-usa/emissions/MOVES_source_type_pop",
                     "gcam-usa/emissions/MOVES_VMT_dist",
                     "gcam-usa/emissions/MOVES_VMT_dist_missing_mapping",
                     "gcam-usa/emissions/MARKAL_LDV_EFs_gpm",
                     "L254.StubTranTechLoadFactor",
                     "gcam-usa/emissions/trnMARKAL_UCD_mapping",
                     "gcam-usa/emissions/MARKAL_fuel_name_code",
                     "gcam-usa/emissions/GREET2014_LDV_CNG_EFs_tgEJ",
                     "gcam-usa/emissions/MARKAL_LDV_eff_vmtMJ",
                     "gcam-usa/emissions/MARKAL_LDV_eff_missing_mapping",
                     "gcam-usa/emissions/MARKAL_HDV_EFs_gpm",
                     "gcam-usa/states_subregions",
                     "gcam-usa/emissions/MOVES_EV_Efs_ORD",
                     "gcam-usa/emissions/NEI_pollutant_mapping",
                     "gcam-usa/emissions/MARKAL_GCAM_mapping",
                     "gcam-usa/emissions/MOVES_motorcycle_data")



    return_data(L171.nonco2_tgpkm_censusR_trn_SMarkal_F_V_Y,
                L171.nonco2_tgpkm_state_trn_SMarkal_F_Y)
  } else {
    stop("Unknown command")
  }
}
