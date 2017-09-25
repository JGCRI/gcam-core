#' module_energy_L254.transportation_UCD
#'
#' Calculate transportation data using information from the global UCD transportation technology database.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L254.Supplysector_trn}, \code{L254.FinalEnergyKeyword_trn}, \code{L254.tranSubsectorLogit},
#' \code{L254.tranSubsectorShrwt}, \code{L254.tranSubsectorShrwtFllt}, \code{L254.tranSubsectorInterp},
#' \code{L254.tranSubsectorInterpTo}, \code{L254.tranSubsectorSpeed}, \code{L254.tranSubsectorSpeed_passthru},
#' \code{L254.tranSubsectorSpeed_noVOTT}, \code{L254.tranSubsectorSpeed_nonmotor}, \code{L254.tranSubsectorVOTT},
#' \code{L254.tranSubsectorFuelPref}, \code{L254.StubTranTech}, \code{L254.StubTech_passthru}, \code{L254.StubTech_nonmotor},
#' \code{L254.GlobalTechShrwt_passthru}, \code{L254.GlobalTechShrwt_nonmotor}, \code{L254.GlobalTechCoef_passthru},
#' \code{L254.GlobalRenewTech_nonmotor}, \code{L254.GlobalTranTechInterp}, \code{L254.GlobalTranTechShrwt},
#' \code{L254.GlobalTranTechSCurve}, \code{L254.StubTranTechCalInput}, \code{L254.StubTranTechLoadFactor},
#' \code{L254.StubTranTechCost}, \code{L254.StubTranTechCoef}, \code{L254.StubTechCalInput_passthru},
#' \code{L254.StubTechProd_nonmotor}, \code{L254.PerCapitaBased_trn}, \code{L254.PriceElasticity_trn},
#' \code{L254.IncomeElasticity_trn}, \code{L254.BaseService_trn}. The corresponding file in the
#' original data system was \code{L254.transportation_UCD.R} (energy level2).
#' @details Due to the asymmetrical nature of the transportation sectors in the various regions, we can't simply write
#' generic information to all regions. Instead, technology information is read from the global UCD transportation
#' technology database, and supplysector and subsector attributes are matched in from lookup tables.
#' @importFrom assertthat assert_that
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AJS September 2017
#' @export
module_energy_L254.transportation_UCD <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/mappings/UCD_techs",
             FILE = "energy/A54.demand",
             FILE = "energy/A54.demand_ssp1",
             FILE = "energy/A54.sector",
             FILE = "energy/A54.tranSubsector_logit",
             FILE = "energy/A54.tranSubsector_shrwt",
             FILE = "energy/A54.tranSubsector_interp",
             FILE = "energy/A54.tranSubsector_VOTT",
             FILE = "energy/A54.tranSubsector_VOTT_ssp1",
             FILE = "energy/A54.globaltech_passthru",
             FILE = "energy/A54.globaltech_nonmotor",
             FILE = "energy/A54.globaltranTech_shrwt",
             FILE = "energy/A54.globaltranTech_interp",
             FILE = "energy/A54.globaltranTech_retire",
             "L154.in_EJ_R_trn_m_sz_tech_F_Yh",
             "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y",
             "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
             "L154.loadfactor_R_trn_m_sz_tech_F_Y",
             "L154.speed_kmhr_R_trn_m_sz_tech_F_Y",
             "L154.out_mpkm_R_trn_nonmotor_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwt",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp",
             "L254.tranSubsectorInterpTo",
             "L254.tranSubsectorSpeed",
             "L254.tranSubsectorSpeed_passthru",
             "L254.tranSubsectorSpeed_noVOTT",
             "L254.tranSubsectorSpeed_nonmotor",
             "L254.tranSubsectorVOTT",
             "L254.tranSubsectorFuelPref",
             "L254.StubTranTech",
             "L254.StubTech_passthru",
             "L254.StubTech_nonmotor",
             "L254.GlobalTechShrwt_passthru",
             "L254.GlobalTechShrwt_nonmotor",
             "L254.GlobalTechCoef_passthru",
             "L254.GlobalRenewTech_nonmotor",
             "L254.GlobalTranTechInterp",
             "L254.GlobalTranTechShrwt",
             "L254.GlobalTranTechSCurve",
             "L254.StubTranTechCalInput",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCost",
             "L254.StubTranTechCoef",
             "L254.StubTechCalInput_passthru",
             "L254.StubTechProd_nonmotor",
             "L254.PerCapitaBased_trn",
             "L254.PriceElasticity_trn",
             "L254.IncomeElasticity_trn",
             "L254.BaseService_trn"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs")
    A54.demand <- get_data(all_data, "energy/A54.demand")
    # The below variable (energy.TRN_SSP) controls which scenario to run, as only one scenario can be run at a time.
    # This is a special case, and the way this is executed will likely change in the future.
    # Note that the variable is defined in constants.R
    if(energy.TRN_SSP == "SSP1") {
      A54.demand <- get_data(all_data, "energy/A54.demand_ssp1")
    }
    A54.sector <- get_data(all_data, "energy/A54.sector")
    A54.tranSubsector_logit <- get_data(all_data, "energy/A54.tranSubsector_logit")
    A54.tranSubsector_shrwt <- get_data(all_data, "energy/A54.tranSubsector_shrwt")
    A54.tranSubsector_interp <- get_data(all_data, "energy/A54.tranSubsector_interp")
    A54.tranSubsector_VOTT <- get_data(all_data, "energy/A54.tranSubsector_VOTT")
    if(energy.TRN_SSP == "SSP1") {
      A54.tranSubsector_VOTT <- get_data(all_data, "energy/A54.tranSubsector_VOTT_ssp1")
    }
    A54.globaltech_passthru <- get_data(all_data, "energy/A54.globaltech_passthru")
    A54.globaltech_nonmotor <- get_data(all_data, "energy/A54.globaltech_nonmotor")
    A54.globaltranTech_shrwt <- get_data(all_data, "energy/A54.globaltranTech_shrwt")
    A54.globaltranTech_interp <- get_data(all_data, "energy/A54.globaltranTech_interp")
    A54.globaltranTech_retire <- get_data(all_data, "energy/A54.globaltranTech_retire")
    L154.in_EJ_R_trn_m_sz_tech_F_Yh <- get_data(all_data, "L154.in_EJ_R_trn_m_sz_tech_F_Yh")
    L154.cost_usdvkm_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y")
    L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y")
    L154.loadfactor_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.loadfactor_R_trn_m_sz_tech_F_Y")
    L154.speed_kmhr_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.speed_kmhr_R_trn_m_sz_tech_F_Y")
    L154.out_mpkm_R_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_R_trn_nonmotor_Yh")

    # ===================================================
    # Silence package notes
    . <- UCD_technology <- Y <- addTimeValue <- base.service <-
      calOutputValue <- calibrated.value <- calibrated.value_2 <- coefficient <-
      curr_table <- energy.final.demand <- from.year <- fuelprefElasticity <- input <-
      loadFactor <- minicam.energy.input <- model_base_years <- names_tranSubsector <-
      output <- r_mei <- r_ss <- r_ss_ts <- r_ss_ts_st <- region <- repeat_and_add_vector <-
      share.weight <- speed.source <- stub.technology <- supplysector <- tech <-
      technology <- time.value.multiplier <- to.value <- to.year <- tranSubsector <-
      tranSubsector.x <- tranTechnology <- year <- year.fillout <- value <- output_agg <-
      output_cum <- share.weight.year <- subs.share.weight <- tech.share.weight <- NULL


    # PART A: BUILDING TRANSPORTATION SECTORS FROM THE TECHNOLOGY LEVEL UP
    # L254.StubTranTech: Transportation stub technologies (built from technologies with coefficients in the UCD database)
    UCD_techID <- c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")

    L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y %>%
      select(GCAM_region_ID, one_of(UCD_techID)) %>%
      # Match in region names and UCD techs
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      unique() %>%
      rename(stub.technology = tranTechnology) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTranTech"]])) ->
      L254.StubTranTech # OUTPUT

    # Write the pass-through technologies to all regions
    # First, create two lists to filter technologies later
    L254.StubTranTech %>%
      mutate(r_ss = paste(region, supplysector)) %>%
      .[["r_ss"]] %>%
      unique() ->
      LIST_r_ss

    # A54.globaltech_passthru reports transportation technology defaults (all parameters; pass-through technologies only)
    LIST_ss <- unique(A54.globaltech_passthru$supplysector)

    A54.globaltech_passthru %>%
      rename(stub.technology = technology) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTranTech"]], "minicam.energy.input"),
                           GCAM_region_names = GCAM_region_names) %>%
      # Of the pass-throughs, subset only the ones whose "input" actually exists in the given region OR
      # ones whose input is in the list of pass-through technologies.
      mutate(r_mei = paste(region, minicam.energy.input)) %>%
      filter((r_mei %in% LIST_r_ss) | (minicam.energy.input %in% LIST_ss)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTranTech"]])) ->
      L254.StubTech_passthru # OUTPUT

    # Write the non-motorized technologies to all regions
    A54.globaltech_nonmotor %>%
      rename(stub.technology = technology) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["StubTranTech"]],
                           GCAM_region_names = GCAM_region_names) ->
      L254.StubTech_nonmotor # OUTPUT

    # L254.Supplysector_trn: Supply sector information for transportation sector
    # Writing the generic supplysector table to all regions may generate combinations that don't apply
    # Creates lists again to filter
    L254.StubTech_passthru %>%
      mutate(r_ss = paste(region, supplysector)) %>%
      .[["r_ss"]] %>%
      unique() ->
      LIST_r_ss_passthru

    L254.StubTech_nonmotor %>%
      mutate(r_ss = paste(region, supplysector)) %>%
      .[["r_ss"]] %>%
      unique() ->
      LIST_r_ss_nonmotor

    LIST_r_ss_all <- unique(c(LIST_r_ss, LIST_r_ss_passthru, LIST_r_ss_nonmotor))

    A54.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Supplysector"]],
                           GCAM_region_names = GCAM_region_names) %>%
      # Subset only the combinations of region and supplysector that are available in the stub technology table
      mutate(r_ss = paste(region, supplysector)) %>%
      filter(r_ss %in% LIST_r_ss_all) %>%
      select(one_of(LEVEL2_DATA_NAMES[["Supplysector"]])) ->
      L254.Supplysector_trn # OUTPUT

    # L254.FinalEnergyKeyword_trn: Supply sector keywords for transportation sector
    L254.Supplysector_trn %>%
      left_join_keep_first_only(A54.sector, by = "supplysector") %>%
      select(one_of(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]])) ->
      L254.FinalEnergyKeyword_trn # OUTPUT


    # PART B: SUBSECTOR INFORMATION
    # L254.tranSubsectorLogit: Subsector logit exponents of transportation sector
    LEVEL2_DATA_NAMES[["tranSubsector"]] <- c("region", "supplysector", "tranSubsector")

    L254.StubTranTech %>%
      mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
      .[["r_ss_ts"]] %>%
      unique() ->
      LIST_r_ss_ts

    L254.StubTech_passthru %>%
      mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
      .[["r_ss_ts"]] %>%
      unique() ->
      LIST_r_ss_ts_passthru

    L254.StubTech_nonmotor %>%
      mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
      .[["r_ss_ts"]] %>%
      unique() ->
      LIST_r_ss_ts_nonmotor

    LIST_r_ss_ts_all <- unique(c(LIST_r_ss_ts, LIST_r_ss_ts_passthru, LIST_r_ss_ts_nonmotor))

    # A54.tranSubsector_logit reports transportation default subsector logit exponents
    A54.tranSubsector_logit %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorLogit"]],
                           GCAM_region_names = GCAM_region_names) %>%
      # Subset only the combinations of region, supplysector, and tranSubsector that are available
      mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
      filter(r_ss_ts %in% LIST_r_ss_ts_all) %>%
      select(one_of(LEVEL2_DATA_NAMES[["tranSubsectorLogit"]])) ->
      L254.tranSubsectorLogit # OUTPUT

    # L254.tranSubsectorShrwt and L254.tranSubsectorShrwtFllt: Subsector shareweights of transportation sector
    if(any(!is.na(A54.tranSubsector_shrwt$year))) {
      A54.tranSubsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorShrwt"]],
                             GCAM_region_names = GCAM_region_names) %>%
        # Subset only the combinations of region, supplysector, and tranSubsector that are available
        mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
        filter(r_ss_ts %in% LIST_r_ss_ts_all) %>%
        select(one_of(LEVEL2_DATA_NAMES[["tranSubsectorShrwt"]])) ->
        L254.tranSubsectorShrwt # OUTPUT
    }

    if(any(!is.na(A54.tranSubsector_shrwt$year.fillout))) {
      A54.tranSubsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorShrwtFllt"]],
                             GCAM_region_names = GCAM_region_names) %>%
        # Subset only the combinations of region, supplysector, and tranSubsector that are available
        mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
        filter(r_ss_ts %in% LIST_r_ss_ts_all) %>%
        select(one_of(LEVEL2_DATA_NAMES[["tranSubsectorShrwtFllt"]])) ->
        L254.tranSubsectorShrwtFllt # OUTPUT
    }

    # L254.tranSubsectorInterp and L254.tranSubsectorInterpTo: Subsector shareweight interpolation of transportation sector
    if(any(is.na(A54.tranSubsector_interp$to.value))) {
      A54.tranSubsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorInterp"]],
                             GCAM_region_names = GCAM_region_names) %>%
        # Subset only the combinations of region, supplysector, and tranSubsector that are available
        mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
        filter(r_ss_ts %in% LIST_r_ss_ts_all) %>%
        select(one_of(LEVEL2_DATA_NAMES[["tranSubsectorInterp"]])) ->
        L254.tranSubsectorInterp # OUTPUT
    }

    if(any(!is.na(A54.tranSubsector_interp$to.value))) {
      A54.tranSubsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorInterpTo"]],
                             GCAM_region_names = GCAM_region_names) %>%
        # Subset only the combinations of region, supplysector, and tranSubsector that are available
        mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
        filter(r_ss_ts %in% LIST_r_ss_ts_all) %>%
        select(one_of(LEVEL2_DATA_NAMES[["tranSubsectorInterpTo"]])) ->
        L254.tranSubsectorInterp # OUTPUT
    }

    # L254.tranSubsectorSpeed: Speeds of transportation modes (not including pass-through sectors)
    L154.speed_kmhr_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      # Match in GCAM region names and UCD technologies
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      mutate(speed = round(value, energy.DIGITS_SPEED)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["tranSubsectorSpeed"]])) ->
      L254.tranSubsectorSpeed # OUTPUT

    # This does not include the pass-through tranSectors
    # Pass-through tranSubsectors for which time value is added are assigned a sector from which to get their speed.
    # L254.tranSubsectorSpeed_passthru: speeds of pass-through transportation subsectors
    # A54.tranSubsector_VOTT reports transportation default subsector value of time in transit multipliers (VOTT = wage rate * this factor)
    A54.tranSubsector_VOTT %>%
      filter(!is.na(speed.source)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "speed.source"),
                           GCAM_region_names = GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Match in speed
      left_join_keep_first_only(L254.tranSubsectorSpeed, by = c("region", "speed.source" = "supplysector",
                                                                "year")) %>%
      rename(tranSubsector = tranSubsector.x) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "year", "speed"))) ->
      L254.tranSubsectorSpeed_passthru

    # L254.tranSubsectorSpeed_noVOTT: Speeds of transportation subsectors whose time value is not considered
    # NOTE: This step should be unnecessary. Currently there is no model default value for speed, and a subsector
    # with no speed read in will cause a model crash, even for modes such as freight where time value is not modeled
    # Start with all observed subsectors in the transportation module
    L254.tranSubsectorSpeed %>%
      mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
      .[["r_ss_ts"]] %>%
      unique() ->
      LIST_r_ss_ts_speed

    L254.tranSubsectorSpeed_passthru %>%
      mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
      .[["r_ss_ts"]] %>%
      unique() ->
      LIST_r_ss_ts_speed_passthru

    LIST_r_ss_ts_speed_all <- unique(c(LIST_r_ss_ts_speed, LIST_r_ss_ts_speed_passthru, LIST_r_ss_ts_nonmotor))

    L254.StubTranTech %>%
      select(one_of(LEVEL2_DATA_NAMES[["tranSubsector"]])) %>%
      # First subset table
      bind_rows(select(L254.StubTech_passthru, one_of(LEVEL2_DATA_NAMES[["tranSubsector"]]))) %>%
      unique() %>%
      # Subset only those whose speeds have not already been specified
      mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
      filter(!r_ss_ts %in% LIST_r_ss_ts_speed_all) %>%
      # Repeat by the number of model time periods
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Write in a default value for speed
      mutate(speed = 1) %>%
      select(-r_ss_ts) ->
      L254.tranSubsectorSpeed_noVOTT

    # L254.tranSubsectorSpeed_nonmotor: Speeds of non-motorized transportation subsectors
    A54.globaltech_nonmotor %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "year", "speed"),
                           GCAM_region_names = GCAM_region_names) ->
      L254.tranSubsectorSpeed_nonmotor

    # L254.tranSubsectorVOTT: Value of time in transit parameterization
    # NOTE: These are currently considered time- and region-independent characteristics
    A54.tranSubsector_VOTT %>%
      filter(addTimeValue == 1) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "addTimeValue", "time.value.multiplier"),
                           GCAM_region_names = GCAM_region_names) %>%
      mutate(year.fillout = min(MODEL_YEARS)) %>%
      # Subset only the combinations of region, supplysector, and tranSubsector
      mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
      filter(r_ss_ts %in% LIST_r_ss_ts_all) %>%
      select(one_of(LEVEL2_DATA_NAMES[["tranSubsector"]]), year.fillout, addTimeValue, time.value.multiplier) ->
      L254.tranSubsectorVOTT

    # L254.tranSubsectorFuelPref: Subsector preferences that are tied to GDP (unrelated to time value)
    A54.tranSubsector_VOTT %>%
      filter(fuelprefElasticity != 0) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "fuelprefElasticity"),
                           GCAM_region_names = GCAM_region_names) %>%
      mutate(year.fillout = min(MODEL_YEARS)) %>%
      # Subset only the combinations of region, supplysector, and tranSubsector that are available
      mutate(r_ss_ts = paste(region, supplysector, tranSubsector)) %>%
      filter(r_ss_ts %in% LIST_r_ss_ts_all) %>%
      select(one_of(LEVEL2_DATA_NAMES[["tranSubsector"]]), year.fillout, fuelprefElasticity) ->
      L254.tranSubsectorFuelPref # OUTPUT


    # PART C: TECHNOLOGY INFORMATION: GLOBAL TECHNOLOGIES (i.e., not tranTechnologies)
    # L254.GlobalTechShrwt_passthru: Shareweights of global transportation sector technologies (not tranTechnologies)
    A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTechYr"]]), share.weight) ->
      L254.GlobalTechShrwt_passthru

    # L254.GlobalTechShrwt_nonmotor: Shareweights of non-motorized global transportation sector technologies (not tranTechnologies)
    A54.globaltech_nonmotor %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTechYr"]]), share.weight) ->
      L254.GlobalTechShrwt_nonmotor

    # L254.GlobalTechCoef_passthru: Coefficients of global transportation sector technologies (not tranTechnologies)
    A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTechCoef"]])) ->
      L254.GlobalTechCoef_passthru

    # L254.GlobalRenewTech_nonmotor: Renewable inputs to non-motorized transportation technologies
    A54.globaltech_nonmotor %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalRenewTech"]])) ->
      L254.GlobalRenewTech_nonmotor


    # PART D: TECHNOLOGY INFORMATION - GLOBAL TRANTECHNOLOGIES
    # L254.GlobalTranTechInterp: Shareweight interpolation of global tranTechnologies
    A54.globaltranTech_interp %>%
      mutate(supplysector = supplysector) %>%  # create new tibble, stripping attributes
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTranTechInterp"]])) ->
      L254.GlobalTranTechInterp

    # L254.GlobalTranTechShrwt: Shareweights of global tranTechnologies
    A54.globaltranTech_shrwt %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      # Expand table to include all model years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, tranSubsector, tranTechnology)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, tranSubsector, tranTechnology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2),
             share.weight = round(share.weight, energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTranTechShrwt"]])) ->
      L254.GlobalTranTechShrwt # OUTPUT

    # L254.GlobalTranTechSCurve: Retirement of global tranTechnologies
    # A54.globaltranTech_retire reports transportation technology retirement parameters. Only applies to vintaged technologies
    A54.globaltranTech_retire %>%
      set_years() %>%
      filter(year < max(year)) %>%
      mutate(year = as.numeric(year)) ->
      L254.GlobalTranTechSCurve_1

    # Copy the final year forward to all future time periods
    L254.GlobalTranTechSCurve_MAX_YEAR <- max(L254.GlobalTranTechSCurve_1$year)

    A54.globaltranTech_retire %>%
      set_years() %>%
      filter(year == max(year)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(year > L254.GlobalTranTechSCurve_MAX_YEAR) %>%
      bind_rows(L254.GlobalTranTechSCurve_1) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTranTechSCurve"]])) ->
      L254.GlobalTranTechSCurve # OUTPUT


    # PART E: CALIBRATION AND REGION-SPECIFIC DATA
    # L254.StubTranTechCalInput: calibrated input of tranTechnologies
    # L154.in_EJ_R_trn_m_sz_tech_F_Yh reports transportation energy consumption by GCAM region / mode / size class / technology / fuel / historical year
    L154.in_EJ_R_trn_m_sz_tech_F_Yh %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(UCD_techs, by = c("UCD_sector", "mode", "size.class",
                                                 "UCD_technology", "UCD_fuel")) %>%
      select(region, supplysector, tranSubsector, stub.technology = UCD_technology,
             year, minicam.energy.input, calibrated.value) ->
      L254.StubTranTechCalInput_basetable

    # Aggregate to set subsector share weights according to region, supplysector, tranSubsector, year combination
    L254.StubTranTechCalInput_basetable %>%
      group_by(region, supplysector, tranSubsector, year) %>%
      summarise(calibrated.value_2 = sum(calibrated.value)) %>%
      ungroup() %>%
      mutate(subs.share.weight = if_else(calibrated.value_2 > 0, 1, 0)) %>%
      select(-calibrated.value_2) ->
      L254.StubTranTechCalInput_Shareweight

    L254.StubTranTechCalInput_basetable %>%
      # Match in subsector share weights
      left_join_error_no_match(L254.StubTranTechCalInput_Shareweight, by = c("region", "supplysector",
                                                                             "tranSubsector", "year")) %>%
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTranTechCalInput"]])) ->
      L254.StubTranTechCalInput # OUTPUT

    # L254.StubTranTechLoadFactor: tranTechnology load factors (all periods)
    # L154.loadfactor_R_trn_m_sz_tech_F_Y reports load factors by GCAM region / mode / size class / technology / fuel / year
    L154.loadfactor_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(loadFactor = round(value, energy.DIGITS_LOADFACTOR)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      rename(stub.technology = UCD_technology) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTranTechLoadFactor"]])) ->
      L254.StubTranTechLoadFactor # OUTPUT

    # L254.StubTranTechCost: tranTechnology costs (all periods)
    # L154.cost_usdvkm_R_trn_m_sz_tech_F_Y reports non-fuel cost by GCAM region / mode / size class / technology / fuel / year "
    L154.cost_usdvkm_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(input.cost = round((value / gdp_deflator(2005, 1990)), energy.DIGITS_COST)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      rename(stub.technology = UCD_technology) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTranTechCost"]])) ->
      L254.StubTranTechCost # OUTPUT

    # L254.StubTranTechCoef: tranTechnology coefficients (intensities; all periods)
    # L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y reports vehicle energy intensity by GCAM region / mode / size class / technology / fuel / year
    L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(coefficient = round((value * CONV_MJ_BTU), energy.DIGITS_COEFFICIENT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      rename(stub.technology = UCD_technology) %>%
      # Currently, the market names for the fuels will be the same as the region
      mutate(market.name = region) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTranTechCoef"]])) ->
      L254.StubTranTechCoef # OUTPUT

    # L254.StubTechCalInput_passthru: calibrated input of passthrough technologies
    # First, need to calculate the service output for all tranTechnologies (= calInput * loadFactor * unit_conversion / (coef * unit conversion))
    L254.StubTranTechCalInput %>%
      select(-contains("share")) %>%
      left_join_error_no_match(L254.StubTranTechLoadFactor, by = c("region", "supplysector", "tranSubsector",
                                                                   "stub.technology", "year")) %>%
      left_join_error_no_match(L254.StubTranTechCoef, by = c("region", "supplysector", "tranSubsector",
                                                             "stub.technology", "minicam.energy.input", "year")) %>%
      mutate(output = calibrated.value * loadFactor * CONV_EJ_GJ / (coefficient * CONV_BTU_KJ)) %>%
      select(region, supplysector, tranSubsector, stub.technology, year, minicam.energy.input,
             calibrated.value, loadFactor, coefficient, output) ->
      L254.StubTranTechOutput

    # The next step is to bind rows with all pass-through technologies on to this table
    # Write all possible pass-through technologies to all regions
    L254.StubTech_passthru %>%
      mutate(r_ss_ts_st = paste(region, supplysector, tranSubsector, stub.technology)) %>%
      .[["r_ss_ts_st"]] %>%
      unique() ->
      LIST_r_ss_ts_st

    A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "technology", "year", "minicam.energy.input"),
                           GCAM_region_names = GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      # Subset only the passthrough technologies that are applicable in each region
      mutate(r_ss_ts_st = paste(region, supplysector, tranSubsector, stub.technology)) %>%
      filter(r_ss_ts_st %in% LIST_r_ss_ts_st) %>%
      select(-r_ss_ts_st) %>%
      # Start with a 0 value for output, and bind this to the table of output by tranTechnology (using only columns whose names match)
      mutate(output = 0) %>%
      bind_rows(
        select(L254.StubTranTechOutput, one_of(c(LEVEL2_DATA_NAMES[["tranSubsector"]]),
                                               "stub.technology", "year", "output", "minicam.energy.input"))) ->
      L254.StubTechCalInput_passthru_all_rows

    L254.StubTechCalInput_passthru_all_rows %>%
      group_by(region, year, supplysector) %>%
      summarise(output_agg = sum(output)) %>%
      ungroup() ->
      L254.StubTechCalInput_passthru_agg

    L254.StubTechCalInput_passthru_all_rows %>%
      left_join(L254.StubTechCalInput_passthru_agg, by = c("region", "year",
                                                           "minicam.energy.input" ="supplysector")) %>%
      # Then, remove the technologies that are not pass-through sectors, and rename "output" to "calibrated.value"
      mutate(r_ss_ts_st = paste(region, supplysector, tranSubsector, stub.technology)) %>%
      filter(r_ss_ts_st %in% LIST_r_ss_ts_st) %>%
      select(-r_ss_ts_st) %>%
      arrange(desc(minicam.energy.input)) %>%
      group_by(region, year) %>%
      mutate(output_cum = cumsum(output_agg)) %>%
      ungroup() ->
      L254.StubTechCalInput_passthru_cum

    LIST_supplysector <- unique(L254.StubTechCalInput_passthru_cum$supplysector)

    L254.StubTechCalInput_passthru_cum %>%
      mutate(calibrated.value = if_else(minicam.energy.input %in% LIST_supplysector,
                                        output_cum, output_agg)) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(region, supplysector, tranSubsector, stub.technology, year,
             minicam.energy.input, calibrated.value, share.weight.year,
             subs.share.weight, tech.share.weight) ->
      L254.StubTechCalInput_passthru # OUTPUT


    # PART F: NON-MOTORIZED TRANSPORTATION - SERVICE OUTPUT
    # L254.StubTechProd_nonmotor: service output of non-motorized transportation technologies
    # L154.out_mpkm_R_trn_nonmotor_Yh reports service output by GCAM region / non-motorized transport mode / year
    L154.out_mpkm_R_trn_nonmotor_Yh %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_MPKM)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(A54.globaltech_nonmotor, by = c("mode" = "tranSubsector")) %>%
      rename(stub.technology = technology, tranSubsector = mode) %>%
      # There is no need to match shareweights to the calOutputValue because no region should ever have a 0 here
      select(one_of(LEVEL2_DATA_NAMES[["StubTranTech"]]), year, calOutputValue) ->
      L254.StubTechProd_nonmotor

    # L254.PerCapitaBased_trn: per-capita based flag for transportation final demand
    A54.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]],
                           GCAM_region_names = GCAM_region_names) ->
      L254.PerCapitaBased_trn # OUTPUT

    # L254.PriceElasticity_trn: price elasticity of transportation final demand" )
    # Price elasticities are only applied to future periods. Application in base years will cause solution failure
    A54.demand %>%
      repeat_add_columns(tibble(year = FUTURE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]],
                           GCAM_region_names = GCAM_region_names) ->
      L254.PriceElasticity_trn # OUTPUT

    # L254.IncomeElasticity_trn: Income elasticity of transportation final demand
    # Income elasticities are only applied to future periods
    A54.demand %>%
      repeat_add_columns(tibble(year = FUTURE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["IncomeElasticity"]],
                           GCAM_region_names = GCAM_region_names) ->
      L254.IncomeElasticity_trn # OUTPUT

    # L254.BaseService_trn: Base-year service output of transportation final demand
    L254.StubTranTechOutput %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTranTech"]]), year, output) %>%
      bind_rows(
        select(L254.StubTechProd_nonmotor, one_of(LEVEL2_DATA_NAMES[["StubTranTech"]]), year, calOutputValue)) %>%
      mutate(base.service = if_else(!is.na(output), output, calOutputValue)) %>%
      # Match in energy.final.demand from transportation supplysector information
      # NAs will be introduced, so use left-join
      left_join(A54.sector, by = "supplysector") %>%
      # Aggregate base-year service output to region, energy.final.demand, and year
      group_by(region, energy.final.demand, year) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() ->
      L254.BaseService_trn # OUTPUT

    # ===================================================

    L254.Supplysector_trn %>%
      add_title("Supply sector information for transportation sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector information was written for all GCAM regions and subset for the combinations of region and supplysector that are available in the stub technology table") %>%
      add_legacy_name("L254.Supplysector_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.sector", "energy/A54.globaltech_nonmotor", "energy/A54.globaltech_passthru") ->
      L254.Supplysector_trn

    L254.FinalEnergyKeyword_trn %>%
      add_title("Supply sector keywords for transportation sector") %>%
      add_units("NA") %>%
      add_comments("Final energy names were matched to supply sector information") %>%
      add_legacy_name("L254.FinalEnergyKeyword_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.sector", "energy/A54.globaltech_nonmotor") ->
      L254.FinalEnergyKeyword_trn

    L254.tranSubsectorLogit %>%
      add_title("Subsector logit exponents of transportation sector") %>%
      add_units("Unitless") %>%
      add_comments("Transportation default subsector logit exponents were written for all regions") %>%
      add_legacy_name("L254.tranSubsectorLogit") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.tranSubsector_logit", "energy/A54.globaltech_nonmotor") ->
      L254.tranSubsectorLogit

    if(OLD_DATA_SYSTEM_BEHAVIOR) {

      # Old code applied the if function to the wrong data (L254.SubsectorShrwt_trn instead of L254.tranSubsectorShrwt)
      if(exists("L254.SubsectorShrwt_trn")) {
        L254.tranSubsectorShrwt %>%
          add_title("Subsector shareweights of transportation sector") %>%
          add_units("Unitless") %>%
          add_comments("Subsector shareweights of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
          add_comments("Only rows with an entry for year were selected") %>%
          add_legacy_name("L254.tranSubsectorShrwt") %>%
          add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                         "energy/A54.tranSubsector_shrwt", "energy/A54.globaltech_nonmotor") ->
          L254.tranSubsectorShrwt
      } else {
        tibble(x = NA) %>%
          add_title("Data not created") %>%
          add_units("Data not created") %>%
          add_comments("Data not created") %>%
          add_legacy_name("L254.tranSubsectorShrwt") %>%
          add_flags(FLAG_NO_TEST) ->
          L254.tranSubsectorShrwt
      }

    } else {

      if(exists("L254.tranSubsectorShrwt")) {
        L254.tranSubsectorShrwt %>%
          add_title("Subsector shareweights of transportation sector") %>%
          add_units("Unitless") %>%
          add_comments("Subsector shareweights of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
          add_comments("Only rows with an entry for year were selected") %>%
          add_legacy_name("L254.tranSubsectorShrwt") %>%
          add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                         "energy/A54.tranSubsector_shrwt", "energy/A54.globaltech_nonmotor") ->
          L254.tranSubsectorShrwt
      } else {
        tibble(x = NA) %>%
          add_title("Data not created") %>%
          add_units("Data not created") %>%
          add_comments("Data not created") %>%
          add_legacy_name("L254.tranSubsectorShrwt") %>%
          add_flags(FLAG_NO_TEST) ->
          L254.tranSubsectorShrwt
      }
    }

    if(exists("L254.tranSubsectorShrwtFllt")) {
      L254.tranSubsectorShrwtFllt %>%
        add_title("Subsector shareweights of transportation sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweights of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
        add_comments("Only rows with an entry for year.fillout were selected") %>%
        add_legacy_name("L254.tranSubsectorShrwtFllt") %>%
        add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                       "energy/A54.tranSubsector_shrwt", "energy/A54.globaltech_nonmotor") ->
        L254.tranSubsectorShrwtFllt
    } else {
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Data not created") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L254.tranSubsectorShrwtFllt") %>%
        add_flags(FLAG_NO_TEST) ->
        L254.tranSubsectorShrwtFllt
    }

    if(exists("L254.tranSubsectorInterp")) {
      L254.tranSubsectorInterp %>%
        add_title("Subsector shareweight interpolation of transportation sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweight interpoloation data of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
        add_comments("Only rows without an entry for to.value were selected") %>%
        add_legacy_name("L254.tranSubsectorInterp") %>%
        add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                       "energy/A54.tranSubsector_interp", "energy/A54.globaltech_nonmotor") ->
        L254.tranSubsectorInterp
    } else {
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Data not created") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L254.tranSubsectorInterp") %>%
        add_flags(FLAG_NO_TEST) ->
        L254.tranSubsectorInterp
    }

    if(exists("L254.tranSubsectorInterpTo")) {
      L254.tranSubsectorInterpTo %>%
        add_title("Subsector shareweight interpolation of transportation sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweight interpoloation data of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
        add_comments("Only rows with an entry for to.value were selected") %>%
        add_legacy_name("L254.tranSubsectorInterpTo") %>%
        add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                       "energy/A54.tranSubsector_interp", "energy/A54.globaltech_nonmotor") ->
        L254.tranSubsectorInterpTo
    } else {
      tibble(x = NA) %>%
        add_title("Data not created") %>%
        add_units("Data not created") %>%
        add_comments("Data not created") %>%
        add_legacy_name("L254.tranSubsectorInterpTo") %>%
        add_flags(FLAG_NO_TEST) ->
        L254.tranSubsectorInterpTo
    }

    L254.tranSubsectorSpeed %>%
      add_title("Speeds of transportation modes (not including pass-through sectors)") %>%
      add_units("km / hr") %>%
      add_comments("Speed information was written for all regions and mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.tranSubsectorSpeed") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.speed_kmhr_R_trn_m_sz_tech_F_Y") ->
      L254.tranSubsectorSpeed

    L254.tranSubsectorSpeed_passthru %>%
      add_title("Speeds of pass-through transportation subsectors") %>%
      add_units("km / hr") %>%
      add_comments("Transportation default subsector value of time in transit (VOTT) multipliers were written for all regions and model years") %>%
      add_comments("Speeds were matched in by region, supplysector, and year") %>%
      add_legacy_name("L254.tranSubsectorSpeed_passthru") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.tranSubsector_VOTT",
                     "energy/mappings/UCD_techs", "L154.speed_kmhr_R_trn_m_sz_tech_F_Y") ->
      L254.tranSubsectorSpeed_passthru

    L254.tranSubsectorSpeed_noVOTT %>%
      add_title("Speeds of transportation subsectors whose time value is not considered") %>%
      add_units("km / hr") %>%
      add_comments("Sector data was subsetted for only those whose speeds have not already been specified") %>%
      add_legacy_name("L254.tranSubsectorSpeed_noVOTT") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.globaltech_passthru") ->
      L254.tranSubsectorSpeed_noVOTT

    L254.tranSubsectorSpeed_nonmotor %>%
      add_title("Speeds of non-motorized transportation subsectors") %>%
      add_units("km / hr") %>%
      add_comments("Data was written for all model years and regions") %>%
      add_legacy_name("L254.tranSubsectorSpeed_nonmotor") %>%
      add_precursors("energy/A54.globaltech_nonmotor") ->
      L254.tranSubsectorSpeed_nonmotor

    L254.tranSubsectorVOTT %>%
      add_title("Value of time in transit parameterization") %>%
      add_units("Unitless") %>%
      add_comments("Data was written for all regions") %>%
      add_comments("Year.fillout was populated with minimum model year") %>%
      add_legacy_name("L254.tranSubsectorVOTT") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.tranSubsector_VOTT",
                     "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.globaltech_passthru", "energy/A54.globaltech_nonmotor") ->
      L254.tranSubsectorVOTT

    L254.tranSubsectorFuelPref %>%
      add_title("Subsector preferences that are tied to GDP (unrelated to time value)") %>%
      add_units("Unitless") %>%
      add_comments("Data was written for all regions") %>%
      add_comments("Year.fillout was populated with minimum model year") %>%
      add_legacy_name("L254.tranSubsectorFuelPref") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A54.tranSubsector_VOTT", "energy/A54.tranSubsector_VOTT_ssp1",
                     "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.globaltech_passthru", "energy/A54.globaltech_nonmotor") ->
      L254.tranSubsectorFuelPref

    L254.StubTranTech %>%
      add_title("Transportation stub technologies (built from technologies with coefficients in the UCD database)") %>%
      add_units("NA") %>%
      add_comments("Data was written for all regions and mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.StubTranTech") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y") ->
      L254.StubTranTech

    L254.StubTech_passthru %>%
      add_title("Transportation stub technologies (passthru)") %>%
      add_units("NA") %>%
      add_comments("Data was written for all regions and subsetted for only the ones whose input actually exists in the given region or ones whose input is in the list of pass-through technologies") %>%
      add_legacy_name("L254.StubTech_passthru") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.globaltech_passthru") ->
      L254.StubTech_passthru

    L254.StubTech_nonmotor %>%
      add_title("Non-motorized transportation stub technologies") %>%
      add_units("NA") %>%
      add_comments("Data was written for all regions") %>%
      add_legacy_name("L254.StubTech_nonmotor") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.globaltech_nonmotor") ->
      L254.StubTech_nonmotor

    L254.GlobalTechShrwt_passthru %>%
      add_title("Shareweights of global transportation sector technologies (not tranTechnologies)") %>%
      add_units("Unitless") %>%
      add_comments("Data was written for all model years") %>%
      add_legacy_name("L254.GlobalTechShrwt_passthru") %>%
      add_precursors("energy/A54.globaltech_passthru") ->
      L254.GlobalTechShrwt_passthru

    L254.GlobalTechShrwt_nonmotor %>%
      add_title("Shareweights of non-motorized global transportation sector technologies (not tranTechnologies)") %>%
      add_units("Unitless") %>%
      add_comments("Data was written for all model years") %>%
      add_legacy_name("L254.GlobalTechShrwt_nonmotor") %>%
      add_precursors("energy/A54.globaltech_nonmotor") ->
      L254.GlobalTechShrwt_nonmotor

    L254.GlobalTechCoef_passthru %>%
      add_title("Coefficients of global transportation sector technologies (not tranTechnologies)") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients were written for all model years") %>%
      add_legacy_name("L254.GlobalTechCoef_passthru") %>%
      add_precursors("energy/A54.globaltech_passthru") ->
      L254.GlobalTechCoef_passthru

    L254.GlobalRenewTech_nonmotor %>%
      add_title("Renewable inputs to non-motorized transportation technologies") %>%
      add_units("NA") %>%
      add_comments("Renewable inputs were written for all model years") %>%
      add_legacy_name("L254.GlobalRenewTech_nonmotor") %>%
      add_precursors("energy/A54.globaltech_nonmotor") ->
      L254.GlobalRenewTech_nonmotor

    L254.GlobalTranTechInterp %>%
      add_title("Shareweight interpolation of global tranTechnologies") %>%
      add_units("NA") %>%
      add_comments("Populated placeholders for final calibration year and end year") %>%
      add_legacy_name("L254.GlobalTranTechInterp") %>%
      add_precursors("energy/A54.globaltranTech_interp") ->
      L254.GlobalTranTechInterp

    L254.GlobalTranTechShrwt %>%
      add_title("Shareweights of global tranTechnologies") %>%
      add_units("Unitless") %>%
      add_comments("Data was subsetted for model years") %>%
      add_legacy_name("L254.GlobalTranTechShrwt") %>%
      add_precursors("energy/A54.globaltranTech_shrwt") ->
      L254.GlobalTranTechShrwt

    L254.GlobalTranTechSCurve %>%
      add_title("Retirement of global tranTechnologies") %>%
      add_units("lifetime and half-life in years") %>%
      add_comments("Retirement parameters in the final year of the base data were carried forward to all future time periods") %>%
      add_legacy_name("L254.GlobalTranTechSCurve") %>%
      add_precursors("energy/A54.globaltranTech_retire") ->
      L254.GlobalTranTechSCurve

    L254.StubTranTechCalInput %>%
      add_title("Calibrated input of tranTechnologies") %>%
      add_units("Unitless") %>%
      add_comments("Share weights were calculate by aggregating energy consumption to the region, supplysector, tranSubsector, year level") %>%
      add_legacy_name("L254.StubTranTechCalInput") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.in_EJ_R_trn_m_sz_tech_F_Yh") ->
      L254.StubTranTechCalInput

    L254.StubTranTechLoadFactor %>%
      add_title("TranTechnology load factors (all periods)") %>%
      add_units("person/vehicle and tonnes/vehicle") %>%
      add_comments("Data was subsetted to model years and mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.StubTranTechLoadFactor") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.loadfactor_R_trn_m_sz_tech_F_Y") ->
      L254.StubTranTechLoadFactor

    L254.StubTranTechCost %>%
      add_title("TranTechnology costs (all periods)") %>%
      add_units("$1990USD / vkm") %>%
      add_comments("Non-fuel cost was adjusted to 1990") %>%
      add_comments("Transportation cost table was mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.StubTranTechCost") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y") ->
      L254.StubTranTechCost

    L254.StubTranTechCoef %>%
      add_title("TranTechnology coefficients (intensities; all periods)") %>%
      add_units("BTU / vkm") %>%
      add_comments("MJ was converted to BTU") %>%
      add_comments("Vehicle energy intensity information was mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.StubTranTechCoef") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y") ->
      L254.StubTranTechCoef

    L254.StubTechCalInput_passthru %>%
      add_title("Calibrated input of passthrough technologies") %>%
      add_units("Unitless") %>%
      add_comments("Pass-through transportationtechnologies were written to all regions") %>%
      add_comments("Share weights were calculated from calibrated output values") %>%
      add_legacy_name("L254.StubTechCalInput_passthru") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs",
                     "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y", "energy/A54.globaltech_passthru") ->
      L254.StubTechCalInput_passthru

    L254.StubTechProd_nonmotor %>%
      add_title("Service output of non-motorized transportation technologies") %>%
      add_units("Million pass-km") %>%
      add_comments("Supply sector and stub.technology information was added to non-motorized service output information") %>%
      add_legacy_name("L254.StubTechProd_nonmotor") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.sector", "L154.out_mpkm_R_trn_nonmotor_Yh") ->
      L254.StubTechProd_nonmotor

    L254.PerCapitaBased_trn %>%
      add_title("Per-capita based flag for transportation final demand") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flag information written for all GCAM regions") %>%
      add_legacy_name("L254.PerCapitaBased_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.demand", "energy/A54.demand_ssp1") ->
      L254.PerCapitaBased_trn

    L254.PriceElasticity_trn %>%
      add_title("Price elasticity of transportation final demand") %>%
      add_units("Unitless") %>%
      add_comments("Price elasticity information written for all GCAM regions and model future years") %>%
      add_legacy_name("L254.PriceElasticity_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.demand") ->
      L254.PriceElasticity_trn

    L254.IncomeElasticity_trn %>%
      add_title("Income elasticity of transportation final demand") %>%
      add_units("Unitless") %>%
      add_comments("Income elasticity information written for all GCAM regions and model future years") %>%
      add_legacy_name("L254.IncomeElasticity_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.demand") ->
      L254.IncomeElasticity_trn

    L254.BaseService_trn %>%
      add_title("Base-year service output of transportation final demand") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_legacy_name("L254.BaseService_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.sector", "energy/mappings/UCD_techs",
                     "L154.out_mpkm_R_trn_nonmotor_Yh", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "L154.loadfactor_R_trn_m_sz_tech_F_Y", "L154.in_EJ_R_trn_m_sz_tech_F_Yh") ->
      L254.BaseService_trn

    return_data(L254.Supplysector_trn, L254.FinalEnergyKeyword_trn, L254.tranSubsectorLogit,
                L254.tranSubsectorShrwt, L254.tranSubsectorShrwtFllt, L254.tranSubsectorInterp,
                L254.tranSubsectorInterpTo, L254.tranSubsectorSpeed, L254.tranSubsectorSpeed_passthru,
                L254.tranSubsectorSpeed_noVOTT, L254.tranSubsectorSpeed_nonmotor, L254.tranSubsectorVOTT,
                L254.tranSubsectorFuelPref, L254.StubTranTech, L254.StubTech_passthru, L254.StubTech_nonmotor,
                L254.GlobalTechShrwt_passthru, L254.GlobalTechShrwt_nonmotor, L254.GlobalTechCoef_passthru,
                L254.GlobalRenewTech_nonmotor, L254.GlobalTranTechInterp, L254.GlobalTranTechShrwt,
                L254.GlobalTranTechSCurve, L254.StubTranTechCalInput, L254.StubTranTechLoadFactor,
                L254.StubTranTechCost, L254.StubTranTechCoef, L254.StubTechCalInput_passthru,
                L254.StubTechProd_nonmotor, L254.PerCapitaBased_trn, L254.PriceElasticity_trn,
                L254.IncomeElasticity_trn, L254.BaseService_trn)
  } else {
    stop("Unknown command")
  }
}
