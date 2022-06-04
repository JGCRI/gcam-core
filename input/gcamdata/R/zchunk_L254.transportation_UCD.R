# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

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
#' @importFrom dplyr anti_join arrange bind_rows filter if_else group_by left_join mutate one_of pull select semi_join summarise contains desc
#' @importFrom tidyr complete nesting
#' @author AJS September 2017
module_energy_L254.transportation_UCD <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/mappings/UCD_techs",
             FILE = "energy/mappings/UCD_techs_revised",
             FILE = "energy/A54.demand",
             FILE = "energy/A54.demand_ssp1",
             FILE = "energy/A54.sector",
             FILE = "energy/A54.tranSubsector_logit",
             FILE = "energy/A54.tranSubsector_shrwt",
             FILE = "energy/A54.tranSubsector_interp",
             FILE = "energy/A54.tranSubsector_VOTT",
             FILE = "energy/A54.tranSubsector_VOTT_ssp1",
             FILE=  "energy/mappings/UCD_size_class_revisions",
             FILE = "energy/A54.tranSubsector_VOTT_ssp1_revised",
             FILE = "energy/A54.tranSubsector_VOTT_revised",
             FILE = "energy/A54.tranSubsector_interp_revised",
             FILE = "energy/A54.tranSubsector_shrwt_revised",
             FILE = "energy/A54.tranSubsector_logit_revised",
             FILE = "energy/A54.globaltranTech_retire_revised",
             FILE = "energy/A54.globaltranTech_shrwt_revised",
             FILE=  "energy/A54.globaltranTech_interp_revised",
             FILE = "energy/A54.globaltech_passthru",
             FILE = "energy/A54.globaltech_passthru_revised",
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
             "L254.GlobalTranTechProfitShutdown",
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

    # Silence package notes
    GCAM_region_ID <- tranTechnology <- region <- supplysector <- . <- technology <- minicam.energy.input <- r_mei <-
      year <- year.fillout <- to.value <- value <- speed.source <- tranSubsector.x <- addTimeValue <- time.value.multiplier <-
      fuelprefElasticity <- tranSubsector <- share.weight <- calibrated.value <- subs.share.weight <- loadFactor <-
      coefficient <- stub.technology <- output <- output_agg <- output_cum <- share.weight.year <- tech.share.weight <-
      calOutputValue <- energy.final.demand <- base.service <- object <- r_ss <- UCD_region <- size.class <- sce <-
      steepness <- profit.shutdown.steepness <- NULL

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names",strip_attributes = TRUE)
    UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs",strip_attributes = TRUE)
    A54.demand <- get_data(all_data, "energy/A54.demand",strip_attributes = TRUE) %>% mutate(sce=paste0("CORE"))
    # This is a special case for SSP1, and the way this is executed will likely change in the future.
    # Note that the variable is defined in constants.R
    A54.demand_SSP1 <- get_data(all_data, "energy/A54.demand_ssp1",strip_attributes = TRUE)%>% mutate(sce=paste0("SSP1"))
    A54.demand<-bind_rows(A54.demand,A54.demand_SSP1)

    A54.sector <- get_data(all_data, "energy/A54.sector",strip_attributes = TRUE)
    #kbn 2019-10-11 Insert code to use revised versions for subsectors below
    Size_class_New<- get_data(all_data, "energy/mappings/UCD_size_class_revisions",strip_attributes = TRUE) %>%
                     select(-UCD_region) %>%
                     distinct()
    #kbn 2020-03-26 If the user selects the revised modes and size classes, use the revised mapping files.
    if (toString(energy.TRAN_UCD_MODE)=='rev.mode'){

      UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs_revised")

      UCD_techs<-UCD_techs %>%
                 inner_join(Size_class_New, by=c("mode","size.class"))%>%
                 select(-mode,-size.class)%>%
                 distinct()

      colnames(UCD_techs)[colnames(UCD_techs)=='rev_size.class']<-'size.class'
      colnames(UCD_techs)[colnames(UCD_techs)=='rev.mode']<-'mode'
    }
    if (toString(energy.TRAN_UCD_MODE)=='rev.mode'){
      A54.tranSubsector_logit <- get_data(all_data, "energy/A54.tranSubsector_logit_revised",strip_attributes = TRUE)
      A54.tranSubsector_shrwt <- get_data(all_data, "energy/A54.tranSubsector_shrwt_revised",strip_attributes = TRUE)
      A54.tranSubsector_interp <- get_data(all_data, "energy/A54.tranSubsector_interp_revised",strip_attributes = TRUE)
      A54.tranSubsector_VOTT <- get_data(all_data, "energy/A54.tranSubsector_VOTT_revised",strip_attributes = TRUE) %>% mutate(sce=paste0("CORE"))

      A54.tranSubsector_VOTT_SSP1 <- get_data(all_data, "energy/A54.tranSubsector_VOTT_ssp1_revised",strip_attributes = TRUE) %>% mutate(sce=paste0("SSP1"))
      A54.tranSubsector_VOTT<- bind_rows(A54.tranSubsector_VOTT,A54.tranSubsector_VOTT_SSP1)

      A54.globaltranTech_retire <- get_data(all_data, "energy/A54.globaltranTech_retire_revised",strip_attributes = TRUE)
      A54.globaltranTech_shrwt <- get_data(all_data, "energy/A54.globaltranTech_shrwt_revised",strip_attributes = TRUE)
      A54.globaltranTech_interp <- get_data(all_data, "energy/A54.globaltranTech_interp_revised",strip_attributes = TRUE)
      A54.globaltech_passthru <- get_data(all_data, "energy/A54.globaltech_passthru_revised",strip_attributes = TRUE)
    }
    else {A54.tranSubsector_logit <- get_data(all_data, "energy/A54.tranSubsector_logit",strip_attributes = TRUE)
    A54.tranSubsector_shrwt <- get_data(all_data, "energy/A54.tranSubsector_shrwt",strip_attributes = TRUE)
    A54.tranSubsector_interp <- get_data(all_data, "energy/A54.tranSubsector_interp",strip_attributes = TRUE)
    A54.tranSubsector_VOTT <- get_data(all_data, "energy/A54.tranSubsector_VOTT",strip_attributes = TRUE)

    A54.tranSubsector_VOTT <- get_data(all_data, "energy/A54.tranSubsector_VOTT",strip_attributes = TRUE)
    A54.tranSubsector_VOTT_SSP1 <- get_data(all_data, "energy/A54.tranSubsector_VOTT_ssp1",strip_attributes = TRUE) %>% mutate(sce=paste0("SSP1"))
    A54.tranSubsector_VOTT<- bind_rows(A54.tranSubsector_VOTT,A54.tranSubsector_VOTT_SSP1)

    A54.globaltranTech_retire <- get_data(all_data, "energy/A54.globaltranTech_retire",strip_attributes = TRUE)
    A54.globaltranTech_shrwt <- get_data(all_data, "energy/A54.globaltranTech_shrwt",strip_attributes = TRUE)
    A54.globaltranTech_interp <- get_data(all_data, "energy/A54.globaltranTech_interp",strip_attributes = TRUE)
    A54.globaltech_passthru <- get_data(all_data, "energy/A54.globaltech_passthru",strip_attributes = TRUE)
    }



    A54.globaltech_nonmotor <- get_data(all_data, "energy/A54.globaltech_nonmotor",strip_attributes = TRUE)



    L154.in_EJ_R_trn_m_sz_tech_F_Yh <- get_data(all_data, "L154.in_EJ_R_trn_m_sz_tech_F_Yh",strip_attributes = TRUE)
    L154.cost_usdvkm_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y",strip_attributes = TRUE)
    L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",strip_attributes = TRUE)
    L154.loadfactor_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.loadfactor_R_trn_m_sz_tech_F_Y",strip_attributes = TRUE)
    L154.speed_kmhr_R_trn_m_sz_tech_F_Y <- get_data(all_data, "L154.speed_kmhr_R_trn_m_sz_tech_F_Y",strip_attributes = TRUE)
    L154.out_mpkm_R_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_R_trn_nonmotor_Yh",strip_attributes = TRUE)

    # ===================================================

    # PART A: BUILDING TRANSPORTATION SECTORS FROM THE TECHNOLOGY LEVEL UP
    # L254.StubTranTech: Transportation stub technologies (built from technologies with coefficients in the UCD database)

    #kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly- We are now calculating values for the transportation variables for all SSPs in LA154.
    # We will also calculate calibrated values, co-efficients by SSPs. Everywhere, we will split out results from the core and the SSPs and calculate values for
    # all scenarios separately. See search string kbn 2020-02-06 for changes. We are calculating these separate values for sectors and subsectors (Part A, Part B),
    # andcalibration and region specific data (Part E).

    #kbn 2019-10-14 Switching to left_join_keep_first, since we have fewer mode categories now.
    #kbn 2020-06-02 Adding sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y %>%
      select(GCAM_region_ID, one_of(c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel","sce"))) %>%
      # Match in region names and UCD techs
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_keep_first_only(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      unique() %>%
      rename(stub.technology = tranTechnology) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTech"]],sce) ->
      L254.StubTranTech # OUTPUT

    # Write the pass-through technologies to all regions
    # First, create two lists to filter technologies later
    L254.StubTranTech %>%
      mutate(r_ss = paste(region, supplysector)) %>%
      pull(r_ss) %>%
      unique() ->
      LIST_r_ss

    # A54.globaltech_passthru reports transportation technology defaults (all parameters; pass-through technologies only)

    A54.globaltech_passthru %>%
      rename(stub.technology = technology) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTranTech"]], "minicam.energy.input"), GCAM_region_names = GCAM_region_names) %>%
      # Of the pass-throughs, subset only the ones whose "input" actually exists in the given region OR
      # ones whose input is in the list of pass-through technologies.
      mutate(r_mei = paste(region, minicam.energy.input)) %>%
      filter((r_mei %in% LIST_r_ss) |
               (minicam.energy.input %in% A54.globaltech_passthru$supplysector)) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTech"]]) %>%
      mutate(sce = paste0("CORE"))->
      L254.StubTech_passthru # OUTPUT

    # Write the non-motorized technologies to all regions
    A54.globaltech_nonmotor %>%
      rename(stub.technology = technology) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["StubTranTech"]], GCAM_region_names = GCAM_region_names) %>%
      mutate(sce = paste0("CORE"))->
      L254.StubTech_nonmotor # OUTPUT

    # L254.Supplysector_trn: Supply sector information for transportation sector
    # Writing the generic supplysector table to all regions may generate combinations that don't apply

    # Use this for filtering below
    r_ss_all <- bind_rows(L254.StubTranTech, L254.StubTech_passthru, L254.StubTech_nonmotor)

    A54.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names) %>%
      # Subset only the combinations of region and supplysector that are available in the stub technology table
      mutate(sce = paste0("CORE")) %>%
      semi_join(r_ss_all, by = c("region", "supplysector","sce")) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME,sce)) ->
      L254.Supplysector_trn # OUTPUT

    # L254.FinalEnergyKeyword_trn: Supply sector keywords for transportation sector
    L254.Supplysector_trn %>%
      left_join_keep_first_only(A54.sector, by = "supplysector") %>%
      select(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]],sce) %>%
      mutate(sce=paste0("CORE"))->
      L254.FinalEnergyKeyword_trn # OUTPUT


    # PART B: SUBSECTOR INFORMATION
    # L254.tranSubsectorLogit: Subsector logit exponents of transportation sector
    LEVEL2_DATA_NAMES[["tranSubsector"]] <- c("region", "supplysector", "tranSubsector")

    # Use this for filtering datasets below
    r_ss_ts_all <- bind_rows(L254.StubTranTech, L254.StubTech_passthru, L254.StubTech_nonmotor)

    # A54.tranSubsector_logit reports transportation default subsector logit exponents
    A54.tranSubsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names = GCAM_region_names) %>%
      mutate(sce = paste0("CORE") ) %>%
      # Subset only the combinations of region, supplysector, and tranSubsector that are available
      semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
      select(c(LEVEL2_DATA_NAMES[["tranSubsectorLogit"]], LOGIT_TYPE_COLNAME),sce) ->
      L254.tranSubsectorLogit # OUTPUT

    # L254.tranSubsectorShrwt and L254.tranSubsectorShrwtFllt: Subsector shareweights of transportation sector
    if(any(!is.na(A54.tranSubsector_shrwt$year))) {
      A54.tranSubsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorShrwt"]], GCAM_region_names = GCAM_region_names) %>%
        mutate(sce = paste0("CORE"),year = as.integer(year)) %>%
        # Subset only the combinations of region, supplysector, and tranSubsector that are available
        semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
        select(LEVEL2_DATA_NAMES[["tranSubsectorShrwt"]],sce) ->
        L254.tranSubsectorShrwt # OUTPUT
    }


    if(any(!is.na(A54.tranSubsector_shrwt$year.fillout))) {
      A54.tranSubsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorShrwtFllt"]], GCAM_region_names = GCAM_region_names) %>%
        mutate(sce = paste0("CORE"),year.fillout = as.integer(year.fillout)) %>%
        # Subset only the combinations of region, supplysector, and tranSubsector that are available
        semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
        select(LEVEL2_DATA_NAMES[["tranSubsectorShrwtFllt"]],sce) ->
        L254.tranSubsectorShrwtFllt # OUTPUT
    }

    # L254.tranSubsectorInterp and L254.tranSubsectorInterpTo: Subsector shareweight interpolation of transportation sector
    if(any(is.na(A54.tranSubsector_interp$to.value))) {
      A54.tranSubsector_interp %>%
        filter(is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorInterp"]], GCAM_region_names = GCAM_region_names) %>%
        mutate(sce = paste0("CORE")) %>%
        # Subset only the combinations of region, supplysector, and tranSubsector that are available
        semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
        select(LEVEL2_DATA_NAMES[["tranSubsectorInterp"]],sce) ->
        L254.tranSubsectorInterp # OUTPUT
    }

    if(any(!is.na(A54.tranSubsector_interp$to.value))) {
      A54.tranSubsector_interp %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["tranSubsectorInterpTo"]], GCAM_region_names = GCAM_region_names) %>%
        # Subset only the combinations of region, supplysector, and tranSubsector that are available
        semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
        select(LEVEL2_DATA_NAMES[["tranSubsectorInterpTo"]],sce) ->
        L254.tranSubsectorInterp # OUTPUT
    }

    # L254.tranSubsectorSpeed: Speeds of transportation modes (not including pass-through sectors)
    #kbn 2019-10-14 Switching to left_join_keep_first, since we have fewer mode categories now.
    #kbn 2020-06-02 Updating with sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L154.speed_kmhr_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      # Match in GCAM region names and UCD technologies
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_keep_first_only(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      mutate(speed = round(value, energy.DIGITS_SPEED)) %>%
      select(LEVEL2_DATA_NAMES[["tranSubsectorSpeed"]],sce) %>%
      distinct()->
      L254.tranSubsectorSpeed # OUTPUT

    # This does not include the pass-through tranSectors
    # Pass-through tranSubsectors for which time value is added are assigned a sector from which to get their speed.
    # L254.tranSubsectorSpeed_passthru: speeds of pass-through transportation subsectors
    # A54.tranSubsector_VOTT reports transportation default subsector value of time in transit multipliers (VOTT = wage rate * this factor)
    A54.tranSubsector_VOTT %>%
      filter(!is.na(speed.source)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "speed.source","sce"), GCAM_region_names = GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      #mutate(sce= paste0("CORE")) %>%
      # Match in speed
      left_join_keep_first_only(L254.tranSubsectorSpeed, by = c("region", "speed.source" = "supplysector", "year","sce")) %>%
      rename(tranSubsector = tranSubsector.x) %>%
      select(LEVEL2_DATA_NAMES[["tranSubsector"]], "year", "speed","sce") %>%
      #We won't have historical data on speed for the SSPs
      na.omit()->
      L254.tranSubsectorSpeed_passthru

    # L254.tranSubsectorSpeed_noVOTT: Speeds of transportation subsectors whose time value is not considered
    # NOTE: This step should be unnecessary. Currently there is no model default value for speed, and a subsector
    # with no speed read in will cause a model crash, even for modes such as freight where time value is not modeled
    # Start with all observed subsectors in the transportation module
    # Use this for filtering below
    r_ss_ts_speed_all <- bind_rows(L254.tranSubsectorSpeed, L254.tranSubsectorSpeed_passthru)

    #kbn 2020-02-06 Adding sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L254.StubTranTech %>%
      select(LEVEL2_DATA_NAMES[["tranSubsector"]],sce) %>%
      # First subset table
      bind_rows(select(L254.StubTech_passthru , one_of(LEVEL2_DATA_NAMES[["tranSubsector"]]),sce)) %>%
      unique() %>%
      # Subset only those whose speeds have NOT already been specified
      anti_join(r_ss_ts_speed_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
      # Repeat by the number of model time periods
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Write in a default value for speed
      mutate(speed = 1) ->
      L254.tranSubsectorSpeed_noVOTT

    # L254.tranSubsectorSpeed_nonmotor: Speeds of non-motorized transportation subsectors
    A54.globaltech_nonmotor %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "year", "speed"), GCAM_region_names = GCAM_region_names) %>%
      mutate(sce= paste0("CORE"))->
      L254.tranSubsectorSpeed_nonmotor

    # L254.tranSubsectorVOTT: Value of time in transit parameterization
    # NOTE: These are currently considered time- and region-independent characteristics
    A54.tranSubsector_VOTT %>%
      filter(addTimeValue == 1) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "addTimeValue", "time.value.multiplier","sce"),
                           GCAM_region_names = GCAM_region_names) %>%
      mutate(year.fillout = min(MODEL_YEARS),
             sce= if_else(is.na(sce),"CORE",sce)) %>%
      # Subset only the combinations of region, supplysector, and tranSubsector
      semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector","sce")) %>%
      select(LEVEL2_DATA_NAMES[["tranSubsector"]], year.fillout, addTimeValue, time.value.multiplier,sce) %>%
      na.omit()->
      L254.tranSubsectorVOTT

    # L254.tranSubsectorFuelPref: Subsector preferences that are tied to GDP (unrelated to time value)
    A54.tranSubsector_VOTT %>%
      filter(fuelprefElasticity != 0) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "fuelprefElasticity","sce"),
                           GCAM_region_names = GCAM_region_names) %>%
      mutate(year.fillout = min(MODEL_YEARS)) %>%
      #mutate(sce = paste0("CORE")) %>%
      # Subset only the combinations of region, supplysector, and tranSubsector that are available
      semi_join(r_ss_ts_all, by = c("region", "supplysector", "tranSubsector")) %>%
      select(LEVEL2_DATA_NAMES[["tranSubsector"]], year.fillout, fuelprefElasticity,sce) %>% na.omit() ->
      L254.tranSubsectorFuelPref # OUTPUT


    # PART C: TECHNOLOGY INFORMATION: GLOBAL TECHNOLOGIES (i.e., not tranTechnologies)
    # L254.GlobalTechShrwt_passthru: Shareweights of global transportation sector technologies (not tranTechnologies)
    A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(sce= paste0("CORE")) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight) ->
      L254.GlobalTechShrwt_passthru

    # L254.GlobalTechShrwt_nonmotor: Shareweights of non-motorized global transportation sector technologies (not tranTechnologies)
    A54.globaltech_nonmotor %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(sce =paste0("CORE")) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight) ->
      L254.GlobalTechShrwt_nonmotor

    # L254.GlobalTechCoef_passthru: Coefficients of global transportation sector technologies (not tranTechnologies)
    A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(sce =paste0("CORE")) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L254.GlobalTechCoef_passthru

    # L254.GlobalRenewTech_nonmotor: Renewable inputs to non-motorized transportation technologies
    A54.globaltech_nonmotor %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(sce =paste0("CORE")) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalRenewTech"]]) ->
      L254.GlobalRenewTech_nonmotor


    # PART D: TECHNOLOGY INFORMATION - GLOBAL TRANTECHNOLOGIES
    # L254.GlobalTranTechInterp: Shareweight interpolation of global tranTechnologies
    A54.globaltranTech_interp %>%
      mutate(supplysector = supplysector) %>%  # create new tibble, stripping attributes
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechInterp"]],"sce") ->
      L254.GlobalTranTechInterp

    # L254.GlobalTranTechShrwt: Shareweights of global tranTechnologies
    A54.globaltranTech_shrwt %>%
      filter(sce=="CORE") %>%
      select(-sce) %>%
      gather_years %>%
      # Expand table to include all model years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, tranSubsector, tranTechnology)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, tranSubsector, tranTechnology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2),
             share.weight = round(share.weight, energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(sce= paste0("CORE")) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechShrwt"]],sce) ->
      L254.GlobalTranTechShrwt_CORE # OUTPUT


    # A54.globaltranTech_shrwt %>%
    #   filter(sce=="highEV") %>%
    #   select(-sce) %>%
    #   gather_years %>%
    #   # Expand table to include all model years
    #   complete(year = c(year, MODEL_YEARS), nesting(supplysector, tranSubsector, tranTechnology)) %>%
    #   # Extrapolate to fill out values for all years
    #   # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
    #   group_by(supplysector, tranSubsector, tranTechnology) %>%
    #   mutate(share.weight = approx_fun(year, value, rule = 2),
    #          share.weight = round(share.weight, energy.DIGITS_SHRWT)) %>%
    #   ungroup() %>%
    #   filter(year %in% MODEL_YEARS) %>%
    #   mutate(sce= paste0("highEV")) %>%
    #   rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
    #   select(LEVEL2_DATA_NAMES[["GlobalTranTechShrwt"]],sce) ->
    #   L254.GlobalTranTechShrwt_highEV # OUTPUT

    #L254.GlobalTranTechShrwt <- bind_rows(L254.GlobalTranTechShrwt_highEV,L254.GlobalTranTechShrwt_CORE)
    L254.GlobalTranTechShrwt <- L254.GlobalTranTechShrwt_CORE

    # L254.GlobalTranTechSCurve and L254.GlobalTranTechProfitShutdown: Retirement of global tranTechnologies
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
      rename(sector.name = supplysector, subsector.name = tranSubsector) ->
      A54.globaltranTech_retire

    A54.globaltranTech_retire %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechSCurve"]]) ->
      L254.GlobalTranTechSCurve # OUTPUT

    A54.globaltranTech_retire %>%
      select(-steepness) %>%
      rename(steepness = profit.shutdown.steepness) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechProfitShutdown"]]) ->
      L254.GlobalTranTechProfitShutdown


    # PART E: CALIBRATION AND REGION-SPECIFIC DATA
    # L254.StubTranTechCalInput: calibrated input of tranTechnologies
    # L154.in_EJ_R_trn_m_sz_tech_F_Yh reports transportation energy consumption by GCAM region / mode / size class / technology / fuel / historical year
    #kbn 2019-10-14 Switching to left_join_keep_first, since we have fewer mode categories now.
    L154.in_EJ_R_trn_m_sz_tech_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_keep_first_only(UCD_techs, by = c("UCD_sector", "mode", "size.class",
                                                 "UCD_technology", "UCD_fuel")) %>%
      select(region, supplysector, tranSubsector, stub.technology = tranTechnology,
             year, minicam.energy.input, calibrated.value) ->
      L254.StubTranTechCalInput_basetable

    #kbn 2020-02-06 Energy intensity are not separated by SSPs. So, just copying information from CORE to all SSPs.
    # L254.StubTranTechCalInput_basetable<- bind_rows(L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("CORE")),
    #                                                 L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("SSP1")),
    #                                                 L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("SSP3")),
    #                                                 L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("SSP5")),
    #                                                 L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("highEV")))
    L254.StubTranTechCalInput_basetable<- bind_rows(L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("CORE")),
                                                    L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("SSP1")),
                                                    L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("SSP3")),
                                                    L254.StubTranTechCalInput_basetable %>% mutate(sce= paste0("SSP5")))

    # Aggregate to set subsector share weights according to region, supplysector, tranSubsector, year combination
    # kbn 2020-02-06 Add sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L254.StubTranTechCalInput_basetable %>%
      group_by(region, supplysector, tranSubsector, year,sce) %>%
      summarise(subs.share.weight = sum(calibrated.value)) %>%
      ungroup() %>%
      mutate(subs.share.weight = if_else(subs.share.weight > 0, 1, 0))->L254.StubTranTechCalInput_Shareweight

    L254.StubTranTechCalInput_basetable %>%
      # Match in subsector share weights
      #Add sce below kbn 2020-06-02 (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
      left_join_error_no_match(L254.StubTranTechCalInput_Shareweight, by = c("region", "supplysector",
                                                                             "tranSubsector", "year","sce")) %>%
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      #Add sce below kbn 2020-06-02 (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
      select(LEVEL2_DATA_NAMES[["StubTranTechCalInput"]],sce) ->
      L254.StubTranTechCalInput # OUTPUT

    # L254.StubTranTechLoadFactor: tranTechnology load factors (all periods)
    # L154.loadfactor_R_trn_m_sz_tech_F_Y reports load factors by GCAM region / mode / size class / technology / fuel / year
    #kbn 2019-10-14 Switching to left_join_keep_first, since we have fewer mode categories now.
    #kbn 2020-06-02 Adding sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L154.loadfactor_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(loadFactor = round(value, energy.DIGITS_LOADFACTOR)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_keep_first_only(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      rename(stub.technology = tranTechnology) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechLoadFactor"]],sce) ->
      L254.StubTranTechLoadFactor # OUTPUT

    # L254.StubTranTechCost: tranTechnology costs (all periods)
    # L154.cost_usdvkm_R_trn_m_sz_tech_F_Y reports non-fuel cost by GCAM region / mode / size class / technology / fuel / year "
    ##kbn 2019-10-14 Switching to left_join_keep_first, since we have fewer mode categories now.
    #kbn 2020-06-02 Updating with sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L154.cost_usdvkm_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(input.cost = round((value / gdp_deflator(2005, 1990)), energy.DIGITS_COST)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_keep_first_only(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      rename(stub.technology = tranTechnology) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechCost"]],sce) ->
      L254.StubTranTechCost # OUTPUT

    # L254.StubTranTechCoef: tranTechnology coefficients (intensities; all periods)
    # L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y reports vehicle energy intensity by GCAM region / mode / size class / technology / fuel / year
    #kbn 2019-10-14 Switching to left_join_keep_first, since we have fewer mode categories now.
    L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(coefficient = round((value * CONV_MJ_BTU), energy.DIGITS_COEFFICIENT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_keep_first_only(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      rename(stub.technology = tranTechnology) %>%
      # Currently, the market names for the fuels will be the same as the region
      mutate(market.name = region) %>%
      #kbn 2020-06-02 adding sce here (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
      select(LEVEL2_DATA_NAMES[["StubTranTechCoef"]],sce) ->
      L254.StubTranTechCoef # OUTPUT

    # L254.StubTechCalInput_passthru: calibrated input of passthrough technologies
    # First, need to calculate the service output for all tranTechnologies (= calInput * loadFactor * unit_conversion / (coef * unit conversion))
    #Switching to normal left_joins below since we have more
    #2020


    L254.StubTranTechCalInput %>%

      select(-contains("share")) %>%
      left_join(L254.StubTranTechLoadFactor, by = c("region", "supplysector", "tranSubsector",
                                                                   "stub.technology", "year", "sce")) %>%
      left_join(L254.StubTranTechCoef, by = c("region", "supplysector", "tranSubsector",
                                                             "stub.technology", "minicam.energy.input", "year","sce")) %>%
      mutate(loadFactor=if_else(is.na(loadFactor),0,loadFactor),
             coefficient=if_else(is.na(coefficient),0,coefficient),
             output = calibrated.value * loadFactor * CONV_EJ_GJ / (coefficient * CONV_BTU_KJ),
             output = if_else(is.na(output),0,output)) %>%
      select(region, supplysector, tranSubsector, stub.technology, year, minicam.energy.input,
             calibrated.value, loadFactor, coefficient, output,sce) ->
      L254.StubTranTechOutput

    # The next step is to bind rows with all pass-through technologies on to this table
    A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["tranSubsector"]], "technology", "year", "minicam.energy.input"),
                           GCAM_region_names = GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      mutate(sce =paste0("CORE")) %>%
      # Subset only the passthrough technologies that are applicable in each region
      semi_join(L254.StubTech_passthru, by = c("region", "supplysector", "tranSubsector", "stub.technology","sce")) %>%
      # Start with a 0 value for output, and bind this to the table of output by tranTechnology (using only columns whose names match)
      mutate(output = 0) %>%
      bind_rows(
        select(L254.StubTranTechOutput, one_of(c(LEVEL2_DATA_NAMES[["tranSubsector"]]),
                                               "stub.technology", "year", "output", "minicam.energy.input","sce"))) ->
      L254.StubTechCalInput_passthru_all_rows

    #kbn 2020-06-02 Adding sce below (See description of changes using search string kbn 2020-06-02 Making changes to generate xmls for SSPs flexibly)
    L254.StubTechCalInput_passthru_all_rows %>%
      group_by(region, year, supplysector,sce) %>%
      summarise(output_agg = sum(output)) %>%
      ungroup() ->
      L254.StubTechCalInput_passthru_agg

    L254.StubTechCalInput_passthru_all_rows %>%
      left_join(L254.StubTechCalInput_passthru_agg, by = c("region", "year", "minicam.energy.input" = "supplysector","sce")) %>%
      # remove the technologies that are not pass-through sectors
      semi_join(L254.StubTech_passthru, by = c("region", "supplysector", "tranSubsector", "stub.technology","sce")) %>%
      # compute cumulative sum for use below
      arrange(desc(minicam.energy.input)) %>%
      group_by(region, year) %>%
      mutate(output_cum = cumsum(output_agg)) %>%
      ungroup() ->
      L254.StubTechCalInput_passthru_cum

    LIST_supplysector <- unique(L254.StubTechCalInput_passthru_cum$supplysector)

    L254.StubTechCalInput_passthru_cum %>%
      mutate(calibrated.value = if_else(minicam.energy.input %in% LIST_supplysector,
                                        output_cum, output_agg),
             share.weight.year = year,
             subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, tranSubsector, stub.technology, year,
             minicam.energy.input, calibrated.value, share.weight.year,
             subs.share.weight, tech.share.weight,sce) ->
      L254.StubTechCalInput_passthru # OUTPUT

    # PART F: NON-MOTORIZED TRANSPORTATION - SERVICE OUTPUT
    # L254.StubTechProd_nonmotor: service output of non-motorized transportation technologies
    # L154.out_mpkm_R_trn_nonmotor_Yh reports service output by GCAM region / non-motorized transport mode / year
    L154.out_mpkm_R_trn_nonmotor_Yh %>%
      mutate(sce= paste0("CORE")) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_MPKM)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(A54.globaltech_nonmotor, by = c("mode" = "tranSubsector")) %>%
      rename(stub.technology = technology, tranSubsector = mode) %>%
      # There is no need to match shareweights to the calOutputValue because no region should ever have a 0 here
      select(LEVEL2_DATA_NAMES[["StubTranTech"]], year, calOutputValue,sce) ->
      L254.StubTechProd_nonmotor

    # L254.PerCapitaBased_trn: per-capita based flag for transportation final demand
    A54.demand %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["PerCapitaBased"]],"sce"), GCAM_region_names = GCAM_region_names) %>% na.omit() ->
      L254.PerCapitaBased_trn # OUTPUT

    # L254.PriceElasticity_trn: price elasticity of transportation final demand")
    # Price elasticities are only applied to future periods. Application in base years will cause solution failure
    A54.demand %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["PriceElasticity"]],"sce"), GCAM_region_names = GCAM_region_names) %>% na.omit() ->
      L254.PriceElasticity_trn # OUTPUT

    # L254.IncomeElasticity_trn: Income elasticity of transportation final demand
    # Income elasticities are only applied to future periods
    A54.demand %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["IncomeElasticity"]],"sce"), GCAM_region_names = GCAM_region_names) %>% na.omit() ->
      L254.IncomeElasticity_trn # OUTPUT

    # L254.BaseService_trn: Base-year service output of transportation final demand
    L254.StubTranTechOutput %>%
      select(LEVEL2_DATA_NAMES[["StubTranTech"]], year, output,sce) %>%
      bind_rows(
        select(L254.StubTechProd_nonmotor, one_of(LEVEL2_DATA_NAMES[["StubTranTech"]]), year, calOutputValue,sce)) %>%
      mutate(base.service = if_else(!is.na(output), output, calOutputValue)) %>%
      # Match in energy.final.demand from transportation supplysector information
      # NAs will be introduced, so use left-join
      left_join(A54.sector, by = "supplysector") %>%
      # Aggregate base-year service output to region, energy.final.demand, and year
      group_by(region, energy.final.demand, year,sce) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() %>%
      filter(sce=="CORE") ->
      #kbn 2020-06-02 Base service values only needed for CORE.
      L254.BaseService_trn # OUTPUT





    # ===================================================

    L254.Supplysector_trn %>%
      add_title("Supply sector information for transportation sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector information was written for all GCAM regions and subset for the combinations of region and supplysector that are available in the stub technology table") %>%
      add_legacy_name("L254.Supplysector_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.sector", "energy/A54.globaltech_nonmotor", "energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised") ->
      L254.Supplysector_trn

    L254.FinalEnergyKeyword_trn %>%
      add_title("Supply sector keywords for transportation sector") %>%
      add_units("NA") %>%
      add_comments("Final energy names were matched to supply sector information") %>%
      add_legacy_name("L254.FinalEnergyKeyword_trn") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.sector", "energy/A54.globaltech_nonmotor") ->
      L254.FinalEnergyKeyword_trn

    L254.tranSubsectorLogit %>%
      add_title("Subsector logit exponents of transportation sector") %>%
      add_units("Unitless") %>%
      add_comments("Transportation default subsector logit exponents were written for all regions") %>%
      add_legacy_name("L254.tranSubsectorLogit") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.tranSubsector_logit", "energy/A54.tranSubsector_logit_revised", "energy/A54.globaltech_nonmotor") ->
      L254.tranSubsectorLogit

      if(exists("L254.tranSubsectorShrwt")) {
        L254.tranSubsectorShrwt %>%
          add_title("Subsector shareweights of transportation sector") %>%
          add_units("Unitless") %>%
          add_comments("Subsector shareweights of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
          add_comments("Only rows with an entry for year were selected") %>%
          add_legacy_name("L254.tranSubsectorShrwt") %>%
          add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                         "energy/A54.tranSubsector_shrwt", "energy/A54.tranSubsector_shrwt_revised", "energy/A54.globaltech_nonmotor") ->
          L254.tranSubsectorShrwt
      } else {
        missing_data() %>%
          add_legacy_name("L254.tranSubsectorShrwt") ->
          L254.tranSubsectorShrwt
      }


    if(exists("L254.tranSubsectorShrwtFllt")) {
      L254.tranSubsectorShrwtFllt %>%
        add_title("Subsector shareweights of transportation sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweights of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
        add_comments("Only rows with an entry for year.fillout were selected") %>%
        add_legacy_name("L254.tranSubsectorShrwtFllt") %>%
        add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                       "energy/A54.tranSubsector_shrwt", "energy/A54.globaltech_nonmotor") ->
        L254.tranSubsectorShrwtFllt
    } else {
      missing_data() %>%
        add_legacy_name("L254.tranSubsectorShrwtFllt") ->
        L254.tranSubsectorShrwtFllt
    }

    if(exists("L254.tranSubsectorInterp")) {
      L254.tranSubsectorInterp %>%
        add_title("Subsector shareweight interpolation of transportation sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweight interpoloation data of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
        add_comments("Only rows without an entry for to.value were selected") %>%
        add_legacy_name("L254.tranSubsectorInterp") %>%
        add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                       "energy/A54.tranSubsector_interp", "energy/A54.tranSubsector_interp_revised", "energy/A54.globaltech_nonmotor") ->
        L254.tranSubsectorInterp
    } else {
      missing_data() %>%
        add_legacy_name("L254.tranSubsectorInterp") ->
        L254.tranSubsectorInterp
    }

    if(exists("L254.tranSubsectorInterpTo")) {
      L254.tranSubsectorInterpTo %>%
        add_title("Subsector shareweight interpolation of transportation sector") %>%
        add_units("Unitless") %>%
        add_comments("Subsector shareweight interpoloation data of transportation sector were written for all regions and subset for existing combinations of region, supplysector, and tranSubsector") %>%
        add_comments("Only rows with an entry for to.value were selected") %>%
        add_legacy_name("L254.tranSubsectorInterpTo") %>%
        add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                       "energy/A54.tranSubsector_interp", "energy/A54.globaltech_nonmotor") ->
        L254.tranSubsectorInterpTo
    } else {
      missing_data() %>%
        add_legacy_name("L254.tranSubsectorInterpTo") ->
        L254.tranSubsectorInterpTo
    }

    L254.tranSubsectorSpeed %>%
      add_title("Speeds of transportation modes (not including pass-through sectors)") %>%
      add_units("km / hr") %>%
      add_comments("Speed information was written for all regions and mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.tranSubsectorSpeed") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.speed_kmhr_R_trn_m_sz_tech_F_Y") ->
      L254.tranSubsectorSpeed

    L254.tranSubsectorSpeed_passthru %>%
      add_title("Speeds of pass-through transportation subsectors") %>%
      add_units("km / hr") %>%
      add_comments("Transportation default subsector value of time in transit (VOTT) multipliers were written for all regions and model years") %>%
      add_comments("Speeds were matched in by region, supplysector, and year") %>%
      add_legacy_name("L254.tranSubsectorSpeed_passthru") %>%
      add_precursors("common/GCAM_region_names", "energy/A54.tranSubsector_VOTT", "energy/A54.tranSubsector_VOTT_revised",
                     "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.speed_kmhr_R_trn_m_sz_tech_F_Y") ->
      L254.tranSubsectorSpeed_passthru

    L254.tranSubsectorSpeed_noVOTT %>%
      add_title("Speeds of transportation subsectors whose time value is not considered") %>%
      add_units("km / hr") %>%
      add_comments("Sector data was subsetted for only those whose speeds have not already been specified") %>%
      add_legacy_name("L254.tranSubsectorSpeed_noVOTT") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised") ->
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
      add_precursors("common/GCAM_region_names", "energy/A54.tranSubsector_VOTT", "energy/A54.tranSubsector_VOTT_revised",
                     "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised", "energy/A54.globaltech_nonmotor") ->
      L254.tranSubsectorVOTT

    L254.tranSubsectorFuelPref %>%
      add_title("Subsector preferences that are tied to GDP (unrelated to time value)") %>%
      add_units("Unitless") %>%
      add_comments("Data was written for all regions") %>%
      add_comments("Year.fillout was populated with minimum model year") %>%
      add_legacy_name("L254.tranSubsectorFuelPref") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A54.tranSubsector_VOTT", "energy/A54.tranSubsector_VOTT_revised", "energy/A54.tranSubsector_VOTT_ssp1", "energy/A54.tranSubsector_VOTT_ssp1_revised",
                     "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised", "energy/A54.globaltech_nonmotor") ->
      L254.tranSubsectorFuelPref

    L254.StubTranTech %>%
      add_title("Transportation stub technologies (built from technologies with coefficients in the UCD database)") %>%
      add_units("NA") %>%
      add_comments("Data was written for all regions and mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.StubTranTech") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y") ->
      L254.StubTranTech

    L254.StubTech_passthru %>%
      add_title("Transportation stub technologies (passthru)") %>%
      add_units("NA") %>%
      add_comments("Data was written for all regions and subsetted for only the ones whose input actually exists in the given region or ones whose input is in the list of pass-through technologies") %>%
      add_legacy_name("L254.StubTech_passthru") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
                     "energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised") ->
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
      add_precursors("energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised") ->
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
      add_precursors("energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised") ->
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
      add_precursors("energy/A54.globaltranTech_interp", "energy/A54.globaltranTech_interp_revised") ->
      L254.GlobalTranTechInterp

    L254.GlobalTranTechShrwt %>%
      add_title("Shareweights of global tranTechnologies") %>%
      add_units("Unitless") %>%
      add_comments("Data was subsetted for model years") %>%
      add_legacy_name("L254.GlobalTranTechShrwt") %>%
      add_precursors("energy/A54.globaltranTech_shrwt", "energy/A54.globaltranTech_shrwt_revised") ->
      L254.GlobalTranTechShrwt

    L254.GlobalTranTechSCurve %>%
      add_title("Retirement of global tranTechnologies") %>%
      add_units("lifetime and half-life in years") %>%
      add_comments("Retirement parameters in the final year of the base data were carried forward to all future time periods") %>%
      add_legacy_name("L254.GlobalTranTechSCurve") %>%
      add_precursors("energy/A54.globaltranTech_retire", "energy/A54.globaltranTech_retire_revised") ->
      L254.GlobalTranTechSCurve

    L254.GlobalTranTechProfitShutdown %>%
      add_title("Profit shutdown parameters of global tranTechnologies") %>%
      add_units("unitless function parameters") %>%
      add_comments("Profit shutdown parameters in the final year of the base data were carried forward to all future time periods") %>%
      same_precursors_as(L254.GlobalTranTechSCurve) ->
      L254.GlobalTranTechProfitShutdown

    L254.StubTranTechCalInput %>%
      add_title("Calibrated input of tranTechnologies") %>%
      add_units("Unitless") %>%
      add_comments("Share weights were calculate by aggregating energy consumption to the region, supplysector, tranSubsector, year level") %>%
      add_legacy_name("L254.StubTranTechCalInput") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.in_EJ_R_trn_m_sz_tech_F_Yh") ->
      L254.StubTranTechCalInput

    L254.StubTranTechLoadFactor %>%
      add_title("TranTechnology load factors (all periods)") %>%
      add_units("person/vehicle and tonnes/vehicle") %>%
      add_comments("Data was subsetted to model years and mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.StubTranTechLoadFactor") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.loadfactor_R_trn_m_sz_tech_F_Y") ->
      L254.StubTranTechLoadFactor

    L254.StubTranTechCost %>%
      add_title("TranTechnology costs (all periods)") %>%
      add_units("$1990USD / vkm") %>%
      add_comments("Non-fuel cost was adjusted to 1990") %>%
      add_comments("Transportation cost table was mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.StubTranTechCost") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y") ->
      L254.StubTranTechCost

    L254.StubTranTechCoef %>%
      add_title("TranTechnology coefficients (intensities; all periods)") %>%
      add_units("BTU / vkm") %>%
      add_comments("MJ was converted to BTU") %>%
      add_comments("Vehicle energy intensity information was mapped from UCD technology to GCAM technology") %>%
      add_legacy_name("L254.StubTranTechCoef") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y") ->
      L254.StubTranTechCoef

    L254.StubTechCalInput_passthru %>%
      add_title("Calibrated input of passthrough technologies") %>%
      add_units("Unitless") %>%
      add_comments("Pass-through transportationtechnologies were written to all regions") %>%
      add_comments("Share weights were calculated from calibrated output values") %>%
      add_legacy_name("L254.StubTechCalInput_passthru") %>%
      add_precursors("common/GCAM_region_names", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions",
                     "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y", "energy/A54.globaltech_passthru", "energy/A54.globaltech_passthru_revised") ->
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
      add_precursors("common/GCAM_region_names", "energy/A54.sector", "energy/mappings/UCD_techs", "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_size_class_revisions",
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
                L254.GlobalTranTechSCurve, L254.GlobalTranTechProfitShutdown, L254.StubTranTechCalInput, L254.StubTranTechLoadFactor,
                L254.StubTranTechCost, L254.StubTranTechCoef, L254.StubTechCalInput_passthru,
                L254.StubTechProd_nonmotor, L254.PerCapitaBased_trn, L254.PriceElasticity_trn,
                L254.IncomeElasticity_trn, L254.BaseService_trn)
  } else {
    stop("Unknown command")
  }
}
