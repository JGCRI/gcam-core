# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L2233.electricity_water
#'
#' Generates GCAM model inputs for electricity sector with cooling system types disaggregated.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2233.StubTech_elecPassthru}, \code{L2233.StubTechProd_elecPassthru},
#' \code{L2233.GlobalPassThroughTech}, \code{L2233.GlobalTechEff_elecPassthru},
#'  \code{L2233.GlobalTechShrwt_elecPassthru}, \code{L2233.GlobalIntTechCapital_elec},
#'  \code{L2233.GlobalTechCapital_elecPassthru}, \code{L2233.GlobalIntTechOMfixed_elec},
#' \code{L2233.GlobalTechOMfixed_elecPassthru}, \code{L2233.GlobalIntTechOMvar_elec},
#'  \code{L2233.GlobalTechOMvar_elecPassthru}, \code{L2233.GlobalTechInterp_elecPassthru},
#'  \code{L2233.PassThroughSector_elec_cool},
#' \code{L2233.Supplysector_elec_cool}, \code{L2233.ElecReserve_elec_cool},
#' \code{L2233.SubsectorShrwtFllt_elec_cool}, \code{L2233.SubsectorLogit_elec_cool},
#'  \code{L2233.StubTech_elec_cool}, \code{L2233.StubTechEff_elec_cool},
#' \code{L2233.StubTechProd_elec_cool}, \code{L2233.StubTechCapFactor_elec_cool}, \code{L2233.StubTechFixOut_hydro},
#'   \code{L2233.StubTechShrwt_elec_cool}, \code{L2233.GlobalTechCapital_elec_cool},
#'  \code{L2233.GlobalIntTechCapital_elec_cool}, \code{L2233.GlobalTechCoef_elec_cool},
#' \code{L2233.GlobalIntTechCoef_elec_cool}, \code{L2233.InputEmissCoeff_hist_elecPassthru},
#'  \code{L2233.InputEmissCoeff_fut_elecPassthru}, \code{L2233.AvgFossilEffKeyword_elec_cool},
#'  \code{L2233.DeleteCreditInput_elec}, \code{L2233.CreditInput_elec}.
#'
#'
#' The corresponding file in the
#' original data system was \code{L2233.electricity_water.R} (water level2).
#' @details Disaggregates electricity sector for all cooling system types.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter first if_else group_by left_join mutate right_join select summarise first
#' @importFrom tidyr complete gather nesting
#' @author ST June 2017
module_water_L2233.electricity_water <- function(command, ...) {

  # Read in 24 L223 file names
  L223_fileNames <- c("AvgFossilEffKeyword_elec", "GlobalIntTechBackup_elec", "GlobalIntTechCapital_elec",
                      "GlobalIntTechEff_elec", "GlobalIntTechLifetime_elec", "GlobalIntTechOMfixed_elec",
                      "GlobalIntTechOMvar_elec", "GlobalIntTechShrwt_elec", "GlobalTechCapture_elec",
                      "GlobalTechCapital_elec", "GlobalTechEff_elec", "GlobalTechInterp_elec",
                      "GlobalTechLifetime_elec", "GlobalTechOMfixed_elec", "GlobalTechOMvar_elec",
                      "GlobalTechProfitShutdown_elec", "GlobalTechSCurve_elec", "GlobalTechShrwt_elec",
                      "GlobalIntTechCapFac_elec", "GlobalTechCapFac_elec",
                      "PrimaryRenewKeyword_elec", "PrimaryRenewKeywordInt_elec", "StubTech_elec",
                      "StubTechEff_elec", "StubTechCapFactor_elec", "StubTechFixOut_hydro", "Supplysector_elec")

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A23.globalinttech",
             FILE = "energy/A23.globaltech_shrwt",
             FILE = "energy/A23.sector",
             FILE = "water/elec_tech_water_map",
             FILE = "water/A03.sector",
             FILE = "water/A23.CoolingSystemCosts",
             FILE = "water/Macknick_elec_water_m3MWh",
             "L1231.out_EJ_R_elec_F_tech_Yh",
             "L1233.out_EJ_R_elec_F_tech_Yh_cool",
             "L1233.shrwt_R_elec_cool_Yf",
             "L223.StubTechEff_elec",
             "L223.StubTech_elec",

             "L270.CreditInput_elec",
             paste0("L223.", L223_fileNames)
    ))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2233.StubTech_elecPassthru",
             "L2233.StubTechProd_elecPassthru",
             "L2233.GlobalPassThroughTech",
             "L2233.GlobalTechEff_elecPassthru",
             "L2233.GlobalTechShrwt_elecPassthru",
             "L2233.GlobalIntTechCapital_elec",
             "L2233.GlobalTechCapital_elecPassthru",
             "L2233.GlobalIntTechOMfixed_elec",
             "L2233.GlobalTechOMfixed_elecPassthru",
             "L2233.GlobalIntTechOMvar_elec",
             "L2233.GlobalTechOMvar_elecPassthru",
             "L2233.GlobalTechInterp_elecPassthru",
             "L2233.PassThroughSector_elec_cool",
             "L2233.Supplysector_elec_cool",
             "L2233.ElecReserve_elec_cool",
             "L2233.SubsectorShrwtFllt_elec_cool",
             "L2233.SubsectorLogit_elec_cool",
             "L2233.StubTech_elec_cool",
             "L2233.StubTechEff_elec_cool",
             "L2233.StubTechProd_elec_cool",
             "L2233.StubTechCapFactor_elec_cool",
             "L2233.StubTechFixOut_hydro",
             "L2233.StubTechShrwt_elec_cool",
             "L2233.GlobalTechCapital_elec_cool",
             "L2233.GlobalIntTechCapital_elec_cool",
             "L2233.GlobalTechCoef_elec_cool",
             "L2233.GlobalIntTechCoef_elec_cool",
             "L2233.AvgFossilEffKeyword_elec_cool",
             "L2233.GlobalIntTechBackup_elec_cool",
             "L2233.GlobalIntTechEff_elec_cool",
             "L2233.GlobalIntTechLifetime_elec_cool",
             "L2233.GlobalIntTechShrwt_elec_cool",
             "L2233.GlobalIntTechCapFac_elec_cool",
             "L2233.GlobalTechCapFac_elec_cool",
             "L2233.GlobalTechCapture_elec_cool",
             "L2233.GlobalTechEff_elec_cool",
             "L2233.GlobalTechLifetime_elec_cool",
             "L2233.GlobalTechProfitShutdown_elec_cool",
             "L2233.GlobalTechSCurve_elec_cool",
             "L2233.GlobalTechShrwt_elec_cool",
             "L2233.PrimaryRenewKeyword_elec_cool",
             "L2233.PrimaryRenewKeywordInt_elec_cool",
             "L2233.DeleteCreditInput_elec",
             "L2233.CreditInput_elec"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    from.supplysector <- from.subsector <- from.technology <- to.supplysector <-
      to.subsector <- to.technology <- year <- share.weight <- supplysector <-
      subsector <- technology <- minicam.energy.input <- calibration <-
      secondary.output <- value <- region <- sector <- calOutputValue <-
      subs.share.weight <- average.grid.capacity.factor <- plant_type <-
      cooling_system <- water_type <- fuel <- sector.name <- subsector.name <-
      input.capital <- capital.overnight_USD2005 <- capital.overnight <-
      water_withdrawals <- water_consumption <- coefficient <- water_sector <-
      share.weight.year <- emiss.coef <- efficiency <- emiss.coeff <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A23.globalinttech <- get_data(all_data, "energy/A23.globalinttech")
    A23.globaltech_shrwt <- get_data(all_data, "energy/A23.globaltech_shrwt")
    A23.sector <- get_data(all_data, "energy/A23.sector")
    elec_tech_water_map <- get_data(all_data, "water/elec_tech_water_map",strip_attributes = TRUE)
    A03.sector <- get_data(all_data, "water/A03.sector",strip_attributes = TRUE)
    A23.CoolingSystemCosts <- get_data(all_data, "water/A23.CoolingSystemCosts",strip_attributes = TRUE)
    Macknick_elec_water_m3MWh <- get_data(all_data, "water/Macknick_elec_water_m3MWh",strip_attributes = TRUE)
    L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh",strip_attributes = TRUE)
    L1233.out_EJ_R_elec_F_tech_Yh_cool <- get_data(all_data, "L1233.out_EJ_R_elec_F_tech_Yh_cool",strip_attributes = TRUE)
    L1233.shrwt_R_elec_cool_Yf <- get_data(all_data, "L1233.shrwt_R_elec_cool_Yf",strip_attributes = TRUE)
    L223.StubTech_elec <- get_data(all_data, "L223.StubTech_elec",strip_attributes = TRUE)
    L223.StubTechEff_elec <- get_data(all_data, "L223.StubTechEff_elec",strip_attributes = TRUE)

    L270.CreditInput_elec <- get_data(all_data, "L270.CreditInput_elec",strip_attributes = TRUE)

    # Use get_data function with sapply to read in all "L223." inputs at once
    get_data_rev <- function(name, all_data) get_data(all_data, name)
    L223_data <- sapply(paste0("L223.", L223_fileNames), get_data_rev, all_data = all_data)
    names(L223_data) <- L223_fileNames

    # ===================================================

    ## BUILD TWO TABLES WITH ALL POSSIBLE TECHNOLOGIES FROM THE OLD AND NEW STRUCTURES

    # First table (L2233.TechMap) includes all technologies
    elec_tech_water_map %>%
      select(from.supplysector, from.subsector, from.technology,
             to.supplysector, to.subsector, to.technology) -> L2233.TechMap

    # Second table (L2233.TechMapYr) repeats for all model years
    L2233.TechMap %>%
      repeat_add_columns(tibble(year = as.integer(MODEL_YEARS))) -> L2233.TechMapYr


    ## PART 1: PASS-THOUGH TECHNOLOGIES IN THE EXISTING ELECTRICITY SECTOR

    # Note: The technologies in the electricity sector all keep their own name;
    # the only parameters read here are shareweights and minicam-energy-inputs,
    # which are equal to the "to.supplysector" of the new generation technology.
    L2233.TechMapYr %>%
      filter(from.supplysector != to.supplysector) %>%
      # ^^ filter only technologies that are moving to a different sector
      mutate(minicam.energy.input = to.supplysector,
             sector.name = from.supplysector,
             subsector.name = from.subsector,
             technology = from.technology,
             efficiency = 1) -> L2233.GlobalTechEffShrwt_elecPassthru
    # ^^ Share-weights of pass-through technologies in elec. sector

    L2233.GlobalTechEffShrwt_elecPassthru %>%
      select(LEVEL2_DATA_NAMES[["GlobalPassThroughTech"]]) -> L2233.GlobalPassThroughTech # --OUTPUT--

    # Match in the global tech shareweights from the assumptions to the electric sector
    A23.globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology),
               year = c(year, MODEL_YEARS)) %>%
      # ^^ fill out with all model years...
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1)) %>%
      # ^^ ... and interpolate the share weights using existing years
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      # ^^ rename for upcoming join...
      right_join(L2233.GlobalTechEffShrwt_elecPassthru,
                 by = c("year", "sector.name", "subsector.name", "technology")) ->
      L2233.GlobalTechEffShrwt_elecPassthru
    # ^^ this tibble is subsetted to give the following two outputs...

    L2233.GlobalTechEffShrwt_elecPassthru %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) ->
      L2233.GlobalTechEff_elecPassthru # --OUTPUT--

    L2233.GlobalTechEffShrwt_elecPassthru %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]) ->
      L2233.GlobalTechShrwt_elecPassthru # --OUTPUT--

    L1231.out_EJ_R_elec_F_tech_Yh %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(calibrated_techs,
                                      - minicam.energy.input,
                                      -calibration, -secondary.output),
                               by = c("sector", "fuel", "technology")) %>%
      rename(stub.technology = technology, calOutputValue = value) %>%
      mutate(share.weight.year = year) -> StubTechProd_elecPassthru

    # The following replaces the "set_subsector_shrwt" function from the legacy code.
    # We essentially aggregate output for subsectors with multiple technologies and then assign a
    # shareweight of 1 for any technology residing in a subsector that has aggregated output > 0.
    StubTechProd_elecPassthru %>%
      group_by(region, sector, subsector, year) %>%
      summarise(subs.share.weight = sum(calOutputValue)) %>% ungroup %>%
      left_join(StubTechProd_elecPassthru, by = c("region", "sector", "subsector", "year")) %>%
      mutate(subs.share.weight = if_else(subs.share.weight > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      filter(subsector != "hydro") %>%  # << hydro is fixed output (doesn't need calibrating here)
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2233.StubTechProd_elecPassthru # --OUTPUT--


    ## PART 2: SUPPLYSECTOR AND SUBSECTOR INFORMATION IN THE NEW ELEC SECTORS

    L2233.TechMap %>%
      select(to.supplysector) %>%
      filter(!(to.supplysector %in% L223_data$StubTech_elec[["supplysector"]])) %>%
      unique() %>%
      rename(supplysector = to.supplysector) -> L2233.elec_cool_supplysectors
    # ^^ generates single column of elec technologies for expansion of Supplysector_elec...

    L223_data$Supplysector_elec %>%
      filter(supplysector == "electricity") %>%
      select(-supplysector) %>%
      repeat_add_columns(L2233.elec_cool_supplysectors) %>%
      mutate(logit.type = NA) -> L2233.supplysector_info
    # ^^ note: logit exponent doesn't matter here because there's no subsector competition

    L2233.supplysector_info %>%
      rename(pass.through.sector = supplysector) %>%
      mutate(marginal.revenue.sector = "electricity",
             marginal.revenue.market = region) %>%
      select(LEVEL2_DATA_NAMES[["PassThroughSector"]]) ->
      L2233.PassThroughSector_elec_cool # --OUTPUT--

    L2233.supplysector_info -> L2233.Supplysector_elec_cool # --OUTPUT--

    L2233.TechMap %>%
      select(from.supplysector, from.subsector, from.technology, to.supplysector) %>%
      filter(from.supplysector %in% A23.globalinttech$supplysector,
             from.subsector %in% A23.globalinttech$subsector,
             from.technology %in% A23.globalinttech$technology) %>%
      select(to.supplysector) %>% unique -> L2233.elec_cool_Int_supplysectors

    L2233.supplysector_info %>%
      filter(supplysector %in% L2233.elec_cool_Int_supplysectors$to.supplysector) %>%
      mutate(electricity.reserve.margin = unique(A23.sector$electricity.reserve.margin),
             average.grid.capacity.factor = unique(A23.sector$average.grid.capacity.factor)) %>%
      # ^^ Margin and capacity factor assumed to be same as for electricity and elect_td_bld
      select(LEVEL2_DATA_NAMES[["ElecReserve"]]) ->
      L2233.ElecReserve_elec_cool # --OUTPUT--

    elec_tech_water_map %>%
      select(to.supplysector, to.subsector) %>%
      unique %>%
      rename(supplysector = to.supplysector, subsector = to.subsector) ->
      supply_sub_elec_mapping
    # ^^ sets up simple mapping for supply to subsector for following section

    L2233.supplysector_info %>%
      left_join_error_no_match(supply_sub_elec_mapping, by = "supplysector") %>%
      mutate(year.fillout = first(MODEL_BASE_YEARS),
             share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      L2233.SubsectorShrwtFllt_elec_cool # --OUTPUT--

    L2233.SubsectorShrwtFllt_elec_cool %>%
      mutate(logit.year.fillout = first(MODEL_BASE_YEARS),
             logit.exponent = water.COOLING_SYSTEM_LOGIT) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]]) %>%
      mutate(logit.type = NA) -> L2233.SubsectorLogit_elec_cool # --OUTPUT--


    ## PART 3: GLOBAL TECHNOLOGY INFORMATION RE-ASSIGNED TO NEW STRUCTURE

    # Notes: (1) All global technology information from the prior electricity sector...
    #            ...is carried over to the new locations;
    #        (2) We use the string "sector.name" to identify tables with global technologies...
    #            ... since not all have "GlobalTech" in their name

    # Filter the L223 files list for those with "sector.name" as a column header
    sctrFltr <- sapply(L223_data, function(x) "sector.name" %in% colnames(x))
    L2233.Elec_tables_globaltech <- L223_data[sctrFltr]

    # If the table has cost information, then we keep it in the passthrough technology...
    # ... because we don't want the whole-plant costs to be bundled with the cooling...
    # ... system costs. Keeping these together would mean that cooling system decisions...
    # ... would be made on < 1% cost differences, which would require very high logit...
    # ... to get any behavior at all.

    # GPK edit 7/31/2019 - share-weight interpolation information also needs to stay in the pass-thru tech.
    # Failure to do so will move the interpolation from competition between e.g. NGCC and NG single cycle
    # to the cooling system types (e.g., competing once-through flow versus recirculating). See JIRA 340.
    cost_SWInterp_Fltr <- sapply(L2233.Elec_tables_globaltech, function(x)
      grepl("Capital", get_legacy_name(x)) |
        grepl("OM", get_legacy_name(x)) |
        grepl("Interp", get_legacy_name(x)))
    L2233.Elec_tables_globaltech_cost_interp <- L2233.Elec_tables_globaltech[cost_SWInterp_Fltr]
    # ^^ filters for tables with capital, fixed O&M and variable O&M, and share-weight interpolation

    # We reset the intermittent technology to standard technologies that remain in ...
    # ... the electricity sector. For an intermittent technology to be moved to a new ...
    # ... sector, the new passtru tech in the electricity sector is no longer an ...
    # ... intermittent technology (this would double count). Thus, for all L2233.Elec_tables_globaltech_cost_interp, ...
    # ... we change "intermittent.technology" to "technology".
    elec_tech_water_map %>%
      select(-plant_type, - cooling_system, - water_type,
             -sector, -fuel, -technology, -minicam.energy.input) %>%
      rename(sector.name = from.supplysector,
             subsector.name = from.subsector,
             technology = from.technology) %>%  unique -> elec_tech_water_map_
    # ^^ reduce and rename elec_tech_water_map for joining to all tables in L2233.Elec_tables_globaltech_cost_interp

    resetTech <- function(x) {
      names(x)[names(x) == "intermittent.technology"] <- "technology"
      x %>% left_join_keep_first_only(elec_tech_water_map_, by = c("sector.name", "subsector.name", "technology"))
      # ^^ left_join_keep_first_only applied for to.technology column (maintaining arbitrary column from legacy code)
    }

    L2233.Elec_tables_glbTechCost_expanded <- sapply(L2233.Elec_tables_globaltech_cost_interp, resetTech)
    # ^^ resetTech used to join tables in L2233.Elec_tables_globaltech_cost_interp to elec_tech_water_map

    L2233.Elec_tables_glbTechCost_expanded$GlobalTechCapital_elec %>%
      bind_rows(L2233.Elec_tables_glbTechCost_expanded$GlobalIntTechCapital_elec) ->
      GlobalTechCapital_elecPassthru
    L2233.Elec_tables_glbTechCost_expanded$GlobalTechOMfixed_elec %>%
      bind_rows(L2233.Elec_tables_glbTechCost_expanded$GlobalIntTechOMfixed_elec) ->
      GlobalTechOMfixed_elecPassthru
    L2233.Elec_tables_glbTechCost_expanded$GlobalTechOMvar_elec %>%
      bind_rows(L2233.Elec_tables_glbTechCost_expanded$GlobalIntTechOMvar_elec) ->
      GlobalTechOMvar_elecPassthru

    # 7/31/2019 note - none of the intermittent technologies currently have share-weight interpolation assumptions
    # Check to make sure that this is the case
    stopifnot(is.null(L2233.Elec_tables_glbTechCost_expanded$GlobalIntTechInterp_elec))
    L2233.Elec_tables_glbTechCost_expanded$GlobalTechInterp_elec ->
      GlobalTechInterp_elecPassThru

    # The following section partitions the intermittent and standard technologies.
    # Intermittent technologies are not moved to different supplysector/subsector/technology.

    # Capital costs of intermittent technologies applied in the electricity sector...
    GlobalTechCapital_elecPassthru %>%
      filter(sector.name == to.supplysector & sector.name %in% A23.globalinttech$supplysector) %>%
      filter(subsector.name == to.subsector & subsector.name %in% A23.globalinttech$subsector) %>%
      filter(technology == to.technology & technology %in% A23.globalinttech$technology) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]]) ->
      L2233.GlobalIntTechCapital_elec # --OUTPUT--

    # Capital costs of passthru technologies in the electricity sector...
    # Note: generating costs applied here; cooling sys techs only have cooling sys costs
    GlobalTechCapital_elecPassthru %>%
      filter(!(sector.name %in% L2233.GlobalIntTechCapital_elec$sector.name &
                 subsector.name %in% L2233.GlobalIntTechCapital_elec$subsector.name &
                 technology %in% L2233.GlobalIntTechCapital_elec$technology)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]]) ->
      L2233.GlobalTechCapital_elecPassthru # --OUTPUT--

    # OMfixed costs of intermittent technologies applied in the electricity sector
    GlobalTechOMfixed_elecPassthru %>%
      filter(sector.name == to.supplysector & sector.name %in% A23.globalinttech$supplysector) %>%
      filter(subsector.name == to.subsector & subsector.name %in% A23.globalinttech$subsector) %>%
      filter(technology == to.technology & technology %in% A23.globalinttech$technology) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechOMfixed"]]) ->
      L2233.GlobalIntTechOMfixed_elec # --OUTPUT--

    # OMfixed costs of passthru technologies in the electricity sector
    GlobalTechOMfixed_elecPassthru %>%
      filter(!(sector.name %in% L2233.GlobalIntTechCapital_elec$sector.name &
                 subsector.name %in% L2233.GlobalIntTechCapital_elec$subsector.name &
                 technology %in% L2233.GlobalIntTechCapital_elec$technology)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechOMfixed"]]) ->
      L2233.GlobalTechOMfixed_elecPassthru # --OUTPUT--

    # OMvar costs of intermittent technologies applied in the electricity sector
    GlobalTechOMvar_elecPassthru %>%
      filter(sector.name == to.supplysector & sector.name %in% A23.globalinttech$supplysector) %>%
      filter(subsector.name == to.subsector & subsector.name %in% A23.globalinttech$subsector) %>%
      filter(technology == to.technology & technology %in% A23.globalinttech$technology) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechOMvar"]]) ->
      L2233.GlobalIntTechOMvar_elec # --OUTPUT--

    # OMvar costs of passthru technologies in the electricity sector
    GlobalTechOMvar_elecPassthru %>%
      filter(!(sector.name %in% L2233.GlobalIntTechCapital_elec$sector.name &
                 subsector.name %in% L2233.GlobalIntTechCapital_elec$subsector.name &
                 technology %in% L2233.GlobalIntTechCapital_elec$technology)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechOMvar"]]) ->
      L2233.GlobalTechOMvar_elecPassthru # --OUTPUT--

    GlobalTechInterp_elecPassThru %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInterp"]]) ->
      L2233.GlobalTechInterp_elecPassthru

    # filter Elec_tables_globaltech for those *without* costs or share-weight interpolation
    L2233.Elec_tables_globaltech_nocost <- L2233.Elec_tables_globaltech[!cost_SWInterp_Fltr]

    # Note: The following function is used to repeat manipulation of all 14 tables contained...
    # ... in L2233.Elec_tables_globaltech_nocost. All tables contain fewer technologies...
    # ... than required for the number of cooling system options, so we work backwards...
    # ... from the tables with all possible global technology names and years available.
    prepGlobalTechNoCostOutputs <- function(elecTableName) {
      from.supplysector <- from.subsector <- from.technology <- year <- to.supplysector <-
        to.subsector <- to.technology <- technology <- efficiency <- NULL  # silence package check notes

      tableName <- paste0("L2233.", elecTableName, "_cool")
      elecTable <- L2233.Elec_tables_globaltech_nocost[[which(names(L2233.Elec_tables_globaltech_nocost) == elecTableName)]]
      names(elecTable)[names(elecTable) == "intermittent.technology"] <- "technology"
      defCols <- names(elecTable) %in% c("sector.name", "subsector.name", "technology", "year")
      nondataCols <- names(elecTable)[defCols]
      dataCols <- names(elecTable)[!defCols]
      if(!("year" %in% nondataCols)) {
        # setting year to any model year allows the join from L2233.TechMapYr.
        # Note that the year column is not in the nondataCols, and as such will be dropped in select().
        elecTable$year <- MODEL_YEARS[1]
      }
      if(tableName == "L2233.GlobalIntTechEff_elec_cool") {
        elecTable <- filter(elecTable, minicam.energy.input != "backup_electricity")
        # ^^ want the following left_join to catch distributed_solar, not backup_electricity
      }
      L2233.TechMapYr %>%
        filter(from.supplysector %in% elecTable$sector.name,
               from.subsector %in% elecTable$subsector.name,
               from.technology %in% elecTable$technology,
               year %in% elecTable$year) %>%
        left_join_keep_first_only(elecTable, by = c("from.supplysector" = "sector.name",
                                                    "from.subsector" = "subsector.name",
                                                    "from.technology" = "technology",
                                                    "year")) %>%
        rename(sector.name = to.supplysector,
               subsector.name = to.subsector,
               technology = to.technology) %>%
        select(nondataCols, dataCols) %>%
        unique %>%
        na.omit -> newTable
      if("efficiency" %in% names(newTable)) {
        mutate(newTable,
               efficiency = if_else(grepl("dry", technology),
                                    efficiency * water.DRY_COOLING_EFF_ADJ, as.double(efficiency))) ->
          newTable
      }
      newTable %>%
        add_comments("Auto-generated by prepGlobalTechNoCostOutputs function in L2233.electricity_water") %>%
        add_legacy_name(tableName) %>%
        add_precursors(paste0("L223.", elecTableName),
                       "water/elec_tech_water_map")
    }

    L2233.Elec_tables_globaltech_nocost_ <- sapply(names(L2233.Elec_tables_globaltech_nocost),
                                                   prepGlobalTechNoCostOutputs) # --OUTPUTS (Elec_tables_globaltech_nocost_)--

    # Capital costs of cooling systems only
    A23.CoolingSystemCosts %>%
      gather_years %>%
      complete(nesting(cooling_system, input.capital),
               year = c(year, MODEL_YEARS)) %>%
      group_by(cooling_system, input.capital) %>%
      mutate(value = approx_fun(year, value)) %>% ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% rename(capital.overnight_USD2005 = value) %>%
      mutate(capital.overnight = round(capital.overnight_USD2005 * gdp_deflator(1975, 2005), 0)) ->
      L2233.CoolingSystemCosts

    elec_tech_water_map %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(L2233.CoolingSystemCosts, by = c("cooling_system", "year")) %>%
      mutate(fixed.charge.rate = water.COOLING_SYSTEM_FCR, capacity.factor = water.COOLING_SYSTEM_CAPACITY_FACTOR) %>%
      select(-technology) %>%
      rename(sector.name = to.supplysector,
             subsector.name = to.subsector,
             technology = to.technology) %>%
      filter(capital.overnight > 0) ->
      L2233.GlobalTechCapital_elec_cool_all # includes both standard and intermittent.


    # L2233.GlobalTechCapital_elec_cool_all needs to be partitioned into standard into intermittent techs
    L2233.GlobalTechCapital_elec_cool_all %>%
      filter(from.supplysector %in% A23.globalinttech$supplysector &
               from.subsector %in% A23.globalinttech$subsector &
               from.technology %in% A23.globalinttech$technology) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]]) ->
      L2233.GlobalIntTechCapital_elec_cool # --OUTPUT--
    L2233.GlobalTechCapital_elec_cool_all %>%
      filter(!(from.supplysector %in% A23.globalinttech$supplysector &
                 from.subsector %in% A23.globalinttech$subsector &
                 from.technology %in% A23.globalinttech$technology)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]]) ->
      L2233.GlobalTechCapital_elec_cool # --OUTPUT--

    # Water demand coefficients of electric technologies
    elec_tech_water_map %>%
      select(to.supplysector, to.subsector, to.technology, fuel, technology, cooling_system, water_type,
             from.supplysector,  from.subsector, from.technology) %>%
      filter(water_type != "none") %>%
      left_join_error_no_match(select(Macknick_elec_water_m3MWh, -sector),
                               by = c("fuel", "technology", "cooling_system", "water_type")) %>%
      mutate(water_withdrawals = round(water_withdrawals / CONV_MWH_GJ, 7),
             water_consumption = round(water_consumption / CONV_MWH_GJ, 7)) %>%
      gather(minicam.energy.input, coefficient,
             -to.supplysector, -to.subsector, -to.technology, -fuel, -technology, -cooling_system, -water_type,
             -from.supplysector, -from.subsector, -from.technology) %>%
      select(-technology) %>%
      # ^^ technology column dropped to prevent duplicate variable names...
      rename(sector.name = to.supplysector, subsector.name = to.subsector, technology = to.technology) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(minicam.energy.input = sub("_", " ", minicam.energy.input)) %>%
      filter(!(water_type == "seawater" & minicam.energy.input == "water consumption")) %>%
      # ^^ seawater is not relevant for consumption since scarcity isn't an issue
      mutate(minicam.energy.input = if_else(water_type == "seawater", "seawater", minicam.energy.input),
             water_sector = "Electricity",
             minicam.energy.input = set_water_input_name(water_sector, minicam.energy.input, A03.sector)) %>%
      select(-fuel, -cooling_system, -water_type, -water_sector) ->
      L2233.GlobalTechCoef_elec_cool_all

    # L2233.GlobalTechCoef_elec_cool_all needs to be partitioned into standard and intermittent techs
    L2233.GlobalTechCoef_elec_cool_all %>%
      filter(!(from.supplysector %in% A23.globalinttech$supplysector &
                 from.subsector %in% A23.globalinttech$subsector &
                 from.technology %in% A23.globalinttech$technology)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2233.GlobalTechCoef_elec_cool # --OUTPUT--

    L2233.GlobalTechCoef_elec_cool_all %>%
      filter(from.supplysector %in% A23.globalinttech$supplysector &
               from.subsector %in% A23.globalinttech$subsector &
               from.technology %in% A23.globalinttech$technology) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2233.GlobalIntTechCoef_elec_cool # --OUTPUT--

    # Upstream electricity sector that includes pass-thru technologies for calling pass-thru sectors
    L223.StubTech_elec %>% mutate(region = region) -> L2233.StubTech_elecPassthru

    # PART 4: STUB TECHNOLOGY INFORMATION IN THE NEW SECTOR STRUCTURE

    # Stub technologies of cooling system options
    L2233.TechMap %>%
      repeat_add_columns(GCAM_region_names) %>%
      rename(supplysector = to.supplysector,
             subsector = to.subsector,
             stub.technology = to.technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTech"]]) ->
      L2233.StubTech_elec_cool  # --OUTPUT--

    # Stub technololgy shareweights for cooling system options
    L1233.shrwt_R_elec_cool_Yf %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(elec_tech_water_map,
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      select(-from.supplysector, -from.subsector, -from.technology, -plant_type, -minicam.energy.input) %>%
      rename(share.weight = value,supplysector = to.supplysector,
             subsector = to.subsector, stub.technology = to.technology) %>%
      mutate(year = as.integer(year)) ->
      L2233.shrwt_R_elec_cool_Yf
    L2233.StubTech_elec_cool %>% repeat_add_columns(tibble(year = as.integer(MODEL_FUTURE_YEARS))) %>%
      left_join(L2233.shrwt_R_elec_cool_Yf,
                by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      # ^^ non-restrictive join required for NA values associated with technologies without cooling systems (e.g., wind)
      select(LEVEL2_DATA_NAMES[["StubTechShrwt"]]) %>%
      na.omit -> L2233.StubTechShrwt_elec_cool  # --OUTPUT--

    # Calibrated efficiencies of the cooling system options
    L2233.TechMapYr %>%
      repeat_add_columns(GCAM_region_names) %>%
      filter(from.supplysector %in% L223.StubTechEff_elec$supplysector,
             from.subsector %in% L223.StubTechEff_elec$subsector,
             from.technology %in% L223.StubTechEff_elec$stub.technology,
             year %in% L223.StubTechEff_elec$year) %>%
      left_join_error_no_match(L223.StubTechEff_elec,
                               by = c("region", "from.supplysector" = "supplysector",
                                      "from.subsector" = "subsector", "from.technology" = "stub.technology", "year")) %>%
      rename(supplysector = to.supplysector, subsector = to.subsector, stub.technology = to.technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechEff"]]) ->
      L2233.StubTechEff_elec_cool  # --OUTPUT--

    # Electricity technology calibration
    L1233.out_EJ_R_elec_F_tech_Yh_cool %>%
      filter(year %in% MODEL_BASE_YEARS) %>% rename(calOutputValue = value) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(elec_tech_water_map,
                                      -from.supplysector, -from.subsector, -from.technology, -minicam.energy.input),
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type", "plant_type")) %>%
      rename(supplysector = to.supplysector, subsector = to.subsector, stub.technology = to.technology) %>%
      mutate(share.weight.year = year,
             calOutputValue = round(calOutputValue, 7)) ->
      L2233.out_EJ_R_elec_F_tech_Yh_cool

    L2233.out_EJ_R_elec_F_tech_Yh_cool %>%
      group_by(region, supplysector, subsector, share.weight.year) %>%
      summarise(value = sum(calOutputValue)) %>% ungroup %>%
      mutate(subs.share.weight = if_else(value > 0, 1, 0)) %>%
      select(-value) -> out_EJ_R_elec_F_tech_Yh_cool_agg
    # ^^ replaces set_subsector_shrwt function in old system. Used to assign 0/1...
    # ...shareweight to each region/supplysector/subsector/year group

    L2233.out_EJ_R_elec_F_tech_Yh_cool %>%
      left_join_error_no_match(out_EJ_R_elec_F_tech_Yh_cool_agg,
                               by =  c("share.weight.year", "region", "supplysector", "subsector")) %>%
      mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2233.StubTechProd_elec_cool  # --OUTPUT-- (note: hydro is removed when written to output)

    # Stub tech (regionally-specific) capacity factors for renewable technologies with cooling systems (i.e. CSP)
    # extract L223.StubTechCapFactor_elec from L223_data for convenience
    L223.StubTechCapFactor_elec <- L223_data$StubTechCapFactor_elec

    L2233.TechMapYr %>%
      # we only need techs with cooling systems from this list;
      # L223.StubTechCapFactor_elec already contains capacity factors for techs without cooling systems
      filter(from.supplysector != to.supplysector) %>%
      repeat_add_columns(GCAM_region_names) %>%
      filter(from.supplysector %in% L223.StubTechCapFactor_elec$supplysector,
             from.subsector %in% L223.StubTechCapFactor_elec$subsector,
             from.technology %in% L223.StubTechCapFactor_elec$stub.technology,
             year %in% L223.StubTechCapFactor_elec$year) %>%
      left_join_error_no_match(L223.StubTechCapFactor_elec,
                               by = c("region", "from.supplysector" = "supplysector",
                                      "from.subsector" = "subsector", "from.technology" = "stub.technology", "year")) %>%
      rename(supplysector = to.supplysector, subsector = to.subsector, stub.technology = to.technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCapFactor"]]) ->
      L2233.StubTechCapFactor_elec_cool  # --OUTPUT--

    # Hydropower fixed output for base and future periods
    L2233.StubTechProd_elec_cool %>%
      filter(subsector == "hydro") %>%
      rename(fixedOutput = calOutputValue) %>%
      mutate(subs.share.weight = 0, tech.share.weight = 0) %>%
      bind_rows(L223_data$StubTechFixOut_hydro) %>%
      # ^^ binds base years data onto future years
      select(LEVEL2_DATA_NAMES[["StubTechFixOut"]]) ->
      L2233.StubTechFixOut_hydro  # --OUTPUT--


    # BC and OC emissions coefficients from electric power plants
    # Note: Emissions coefficients use "input-driver" and the input is assumed to be the fuel.
    # Here we switch the input of these technologies to the power output.



    # Recreate liquids limit electricity inputs generated by module_energy_L270.limits
    # by moving them from the electric sector to the cooling techs.  See
    # module_energy_L270.limits for more information about this policy in general.
    L270.CreditInput_elec %>%
      # a mutate will force a copy and drop meta data from L270.CreditInput_elec
      # which we do not care to bring along
      mutate(sector.name = sector.name) %>%
      select(-coefficient) ->
      L2233.DeleteCreditInput_elec

    L270.CreditInput_elec %>%
      left_join(L2233.TechMap, by = c("sector.name" = "from.supplysector",
                                      "subsector.name" = "from.subsector",
                                      "technology" = "from.technology")) %>%
      mutate(sector.name = to.supplysector,
             subsector.name = to.subsector,
             technology = to.technology) ->
      L2233.CreditInput_elec
    L2233.CreditInput_elec <- L2233.CreditInput_elec[,LEVEL2_DATA_NAMES[["GlobalTechCoef"]]]


    ## L2233.Elec_tables_globaltech_nocost_ outputs...
    ## Note: comments, precursors and legacy_name auto-generated in prepGlobalTechNoCostOutputs function

    L2233.Elec_tables_globaltech_nocost_$AvgFossilEffKeyword_elec %>%
      add_title("Average efficiencies of fossil fuels") %>%
      add_units("unitless") ->
      L2233.AvgFossilEffKeyword_elec_cool

    L2233.Elec_tables_globaltech_nocost_$GlobalIntTechBackup_elec %>%
      add_title("Capital costs of backup technologies for intermittent techs") %>%
      add_units("1975 USD/kW/yr") ->
      L2233.GlobalIntTechBackup_elec_cool

    L2233.Elec_tables_globaltech_nocost_$GlobalIntTechEff_elec %>%
      add_title("Cooling efficiencies of intermittent electricity generating technologies") %>%
      add_units("Unitless") ->
      L2233.GlobalIntTechEff_elec_cool

    L2233.Elec_tables_globaltech_nocost_$GlobalIntTechLifetime_elec %>%
      add_title("Lifetimes of intermittent electricity generating technologies") %>%
      add_units("Years") ->
      L2233.GlobalIntTechLifetime_elec_cool

    L2233.Elec_tables_globaltech_nocost_$GlobalIntTechShrwt_elec %>%
      add_title("Shareweights of intermittent electricity generating technologies") %>%
      add_units("Unitless") ->
      L2233.GlobalIntTechShrwt_elec_cool

    L2233.Elec_tables_globaltech_nocost_$GlobalTechCapture_elec %>%
      add_title("Storage markets and remove fractions for CCS tech by cooling type") %>%
      add_units("Unitless") ->
      L2233.GlobalTechCapture_elec_cool

    L2233.Elec_tables_globaltech_nocost_$GlobalTechEff_elec %>%
      add_title("Cooling efficiencies for electricity generating technologies") %>%
      add_units("Unitless") ->
      L2233.GlobalTechEff_elec_cool

    L2233.Elec_tables_globaltech_nocost_$GlobalTechLifetime_elec %>%
      add_title("Lifetimes for standard electricity generating technologies") %>%
      add_units("Years") ->
      L2233.GlobalTechLifetime_elec_cool

    L2233.Elec_tables_globaltech_nocost_$GlobalTechProfitShutdown_elec %>%
      add_title("Shutdown points and profit shutdown steepness for standard electricity generating technologies") %>%
      add_units("Unitless") ->
      L2233.GlobalTechProfitShutdown_elec_cool

    L2233.Elec_tables_globaltech_nocost_$GlobalTechSCurve_elec %>%
      add_title("Global tech lifetime for techs with s-curve retirement function") %>%
      add_units("Lifetime in years, half-life in years") ->
      L2233.GlobalTechSCurve_elec_cool

    L2233.Elec_tables_globaltech_nocost_$GlobalTechShrwt_elec %>%
      add_title("Global shareweights for non-intermittent technologies for the electricity sector") %>%
      add_units("Unitless") ->
      L2233.GlobalTechShrwt_elec_cool

    L2233.Elec_tables_globaltech_nocost_$PrimaryRenewKeyword_elec %>%
      add_title("keywords for non-intermittent renewable technologies for the electricity sector") %>%
      add_units("NA") ->
      L2233.PrimaryRenewKeyword_elec_cool

    L2233.Elec_tables_globaltech_nocost_$PrimaryRenewKeywordInt_elec %>%
      add_title("keywords for intermittent renewable technologies") %>%
      add_units("NA") ->
      L2233.PrimaryRenewKeywordInt_elec_cool

    L2233.Elec_tables_globaltech_nocost_$GlobalIntTechCapFac_elec %>%
      add_title("Interpolated intermittent technologies") %>%
      add_units("NA") %>%
      add_precursors("L223.GlobalIntTechCapFac_elec") ->
      L2233.GlobalIntTechCapFac_elec_cool

    L2233.Elec_tables_globaltech_nocost_$GlobalTechCapFac_elec %>%
      add_title("Interpolated non-intermittent technologies") %>%
      add_units("NA") %>%
      add_precursors("L223.GlobalTechCapFac_elec") ->
      L2233.GlobalTechCapFac_elec_cool

    # ===================================================

    L2233.GlobalPassThroughTech %>%
      add_title("Electricity sector, subsector, technology mapping") %>%
      add_units("NA") %>%
      add_comments("Generated by expansion of water/elec_tech_water_map for all years (plus filter and renaming of columns)") %>%
      add_legacy_name("L2233.GlobalPassThroughTech") %>%
      add_precursors("water/elec_tech_water_map") ->
      L2233.GlobalPassThroughTech

    L2233.GlobalTechEff_elecPassthru %>%
      add_title("Input name and efficiency of pass-through technologies in the electricity sector") %>%
      add_units("Unitless") %>%
      add_comments("Composed directly from input data") %>%
      add_legacy_name("L2233.GlobalTechEff_elecPassthru") %>%
      add_precursors("energy/A23.globaltech_shrwt", "water/elec_tech_water_map") ->
      L2233.GlobalTechEff_elecPassthru

    L2233.GlobalTechShrwt_elecPassthru %>%
      add_title("Share-weights of pass-through technologies in the electricity sector") %>%
      add_units("units") %>%
      add_comments("Composed directly from input data") %>%
      add_legacy_name("L2233.GlobalTechShrwt_elecPassthru") %>%
      add_precursors("energy/A23.globaltech_shrwt", "water/elec_tech_water_map") ->
      L2233.GlobalTechShrwt_elecPassthru

    L2233.StubTechProd_elecPassthru %>%
      add_title("Calibrated electricity flow through the pass-through technologies") %>%
      add_units("EJ") %>%
      add_comments("Composed directly from input data") %>%
      add_legacy_name("L2233.StubTechProd_elecPassthru") %>%
      add_precursors("energy/calibrated_techs",
                     "L1231.out_EJ_R_elec_F_tech_Yh",
                     "common/GCAM_region_names") ->
      L2233.StubTechProd_elecPassthru

    L2233.PassThroughSector_elec_cool %>%
      add_title("Pass-through sectors with marginal revenue sector and markets", overwrite = TRUE) %>%
      add_units("NA") %>%
      add_comments("Marginal revenue set to electricity; market set to region") %>%
      add_legacy_name("L2233.PassThroughSector_elec_cool", overwrite = TRUE) %>%
      add_precursors("L223.Supplysector_elec",
                     "water/elec_tech_water_map",
                     "L223.StubTech_elec") ->
      L2233.PassThroughSector_elec_cool

    L2233.Supplysector_elec_cool %>%
      add_title("Supplysector information for elec cooling", overwrite = TRUE) %>%
      add_units("NA") %>%
      add_comments("Composed directly from input data") %>%
      add_legacy_name("L2233.Supplysector_elec_cool", overwrite = TRUE) %>%
      add_precursors("L223.Supplysector_elec",
                     "water/elec_tech_water_map",
                     "L223.StubTech_elec") ->
      L2233.Supplysector_elec_cool

    L2233.ElecReserve_elec_cool %>%
      add_title("Electricity reserve margin and average grid capacity factor", overwrite = TRUE) %>%
      add_units("Unitless") %>%
      add_comments("Factors assumed to be same as for electricity and elect_td_bld") %>%
      add_legacy_name("L2233.ElecReserve_elec_cool", overwrite = TRUE) %>%
      add_precursors("energy/A23.sector",
                     "L223.Supplysector_elec",
                     "water/elec_tech_water_map",
                     "L223.StubTech_elec") ->
      L2233.ElecReserve_elec_cool

    L2233.SubsectorShrwtFllt_elec_cool %>%
      add_title("Subsector information for electricity cooling", overwrite = TRUE) %>%
      add_units("Unitless") %>%
      add_comments("Assumes that the subsectors are just pass-through") %>%
      add_comments("Assumes cooling system competition takes place at the technology level") %>%
      add_legacy_name("L2233.SubsectorShrwtFllt_elec_cool", overwrite = TRUE) %>%
      add_precursors("water/elec_tech_water_map",
                     "L223.Supplysector_elec",
                     "L223.StubTech_elec") ->
      L2233.SubsectorShrwtFllt_elec_cool

    L2233.SubsectorLogit_elec_cool %>%
      add_title("Logit exponent for cooling system choice", overwrite = TRUE) %>%
      add_units("Unitless") %>%
      add_comments("Default cooling system logit applied to all sectors") %>%
      add_legacy_name("L2233.SubsectorLogit_elec_cool", overwrite = TRUE) %>%
      add_precursors("water/elec_tech_water_map",
                     "L223.Supplysector_elec",
                     "L223.StubTech_elec") ->
      L2233.SubsectorLogit_elec_cool

    attr(L2233.GlobalIntTechCapital_elec, ATTR_PRECURSORS) <- NULL
    L2233.GlobalIntTechCapital_elec %>%
      add_title("Capital costs for intermittent electricity generating tech", overwrite = TRUE) %>%
      add_units("1975$US/kW") %>%
      add_comments("Composed directly from input data") %>%
      add_legacy_name("L2233.GlobalIntTechCapital_elec", overwrite = TRUE) %>%
      add_precursors("water/elec_tech_water_map",
                     "energy/A23.globalinttech",
                     "L223.GlobalIntTechCapital_elec") ->
      L2233.GlobalIntTechCapital_elec

    attr(L2233.GlobalTechCapital_elecPassthru, ATTR_PRECURSORS) <- NULL
    L2233.GlobalTechCapital_elecPassthru %>%
      add_title("Capital costs for standard electricity generating tech", overwrite = TRUE) %>%
      add_units("1975$US/kW") %>%
      add_comments("Composed directly from input data") %>%
      add_legacy_name("L2233.GlobalTechCapital_elecPassthru", overwrite = TRUE) %>%
      add_precursors("water/elec_tech_water_map",
                     "energy/A23.globalinttech",
                     "L223.GlobalTechCapital_elec") ->
      L2233.GlobalTechCapital_elecPassthru

    attr(L2233.GlobalIntTechOMfixed_elec, ATTR_PRECURSORS) <- NULL
    L2233.GlobalIntTechOMfixed_elec %>%
      add_title("Operation and maintainance costs (fixed) for intermittent electricity generating tech", overwrite = TRUE) %>%
      add_units("1975$US/kW/yr") %>%
      add_comments("Composed directly from input data") %>%
      add_legacy_name("L2233.GlobalIntTechOMfixed_elec", overwrite = TRUE) %>%
      add_precursors("water/elec_tech_water_map",
                     "energy/A23.globalinttech",
                     "L223.GlobalIntTechOMfixed_elec") ->
      L2233.GlobalIntTechOMfixed_elec

    attr(L2233.GlobalTechOMfixed_elecPassthru, ATTR_PRECURSORS) <- NULL
    L2233.GlobalTechOMfixed_elecPassthru %>%
      add_title("Operation and maintainance costs (fixed) for standard electricity generating tech", overwrite = TRUE) %>%
      add_units("1975$US/kW/year") %>%
      add_comments("Composed directly from input data") %>%
      add_legacy_name("L2233.GlobalTechOMfixed_elecPassthru", overwrite = TRUE) %>%
      add_precursors("water/elec_tech_water_map",
                     "energy/A23.globalinttech",
                     "L223.GlobalTechOMfixed_elec") ->
      L2233.GlobalTechOMfixed_elecPassthru

    attr(L2233.GlobalIntTechOMvar_elec, ATTR_PRECURSORS) <- NULL
    L2233.GlobalIntTechOMvar_elec %>%
      add_title("Operation and maintainance costs (variable) for intermittent electricity generating tech", overwrite = TRUE) %>%
      add_units("1975$US/MWh") %>%
      add_comments("Composed directly from input data") %>%
      add_legacy_name("L2233.GlobalIntTechOMvar_elec", overwrite = TRUE) %>%
      add_precursors("water/elec_tech_water_map",
                     "energy/A23.globalinttech",
                     "L223.GlobalIntTechOMvar_elec") ->
      L2233.GlobalIntTechOMvar_elec

    attr(L2233.GlobalTechOMvar_elecPassthru, ATTR_PRECURSORS) <- NULL
    L2233.GlobalTechOMvar_elecPassthru %>%
      add_title("Operation and maintainance costs (variable) for standard electricity generating tech", overwrite = TRUE) %>%
      add_units("1975$US/MWh") %>%
      add_comments("Composed directly from input data") %>%
      add_legacy_name("L2233.GlobalTechOMvar_elecPassthru", overwrite = TRUE) %>%
      add_precursors("water/elec_tech_water_map",
                     "energy/A23.globalinttech",
                     "L223.GlobalTechOMvar_elec") ->
      L2233.GlobalTechOMvar_elecPassthru

    attr(L2233.GlobalTechInterp_elecPassthru, ATTR_PRECURSORS) <- NULL
    L2233.GlobalTechInterp_elecPassthru %>%
      add_title("Technology share-weight interpolation for standard electricity generating tech", overwrite = TRUE) %>%
      add_units("unitless") %>%
      add_comments("Composed directly from input data") %>%
      add_precursors("water/elec_tech_water_map",
                     "L223.GlobalTechInterp_elec") ->
      L2233.GlobalTechInterp_elecPassthru

    L2233.StubTech_elecPassthru %>%
      add_title("Stub technologies for electricity sector", overwrite = TRUE) %>%
      add_units("NA") %>%
      add_comments("Technologies repeated across regions") %>%
      add_legacy_name("L2233.StubTech_elecPassthru", overwrite = TRUE) %>%
      add_precursors("L223.StubTech_elec") ->
      L2233.StubTech_elecPassthru

    L2233.StubTech_elec_cool %>%
      add_title("Stub technologies for cooling system options") %>%
      add_units("NA") %>%
      add_comments("Technologies repeated across regions") %>%
      add_legacy_name("L2233.StubTech_elec_cool") %>%
      add_precursors("common/GCAM_region_names",
                     "water/elec_tech_water_map") ->
      L2233.StubTech_elec_cool

    L2233.StubTechShrwt_elec_cool %>%
      add_title("Stub technology shareweights for cooling system options") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights joined to L2233.StubTech_elec_cool") %>%
      add_legacy_name("L2233.StubTechShrwt_elec_cool") %>%
      add_precursors("common/GCAM_region_names",
                     "water/elec_tech_water_map",
                     "L1233.shrwt_R_elec_cool_Yf") ->
      L2233.StubTechShrwt_elec_cool

    L2233.StubTechEff_elec_cool %>%
      add_title("Calibrated efficiencies of the cooling system options") %>%
      add_units("Unitless") %>%
      add_comments("Expansion of elec_water technologies and their efficiencies for all regions/years") %>%
      add_legacy_name("L2233.StubTechEff_elec_cool") %>%
      add_precursors("common/GCAM_region_names",
                     "water/elec_tech_water_map", "L223.StubTechEff_elec") ->
      L2233.StubTechEff_elec_cool

    filter(L2233.StubTechProd_elec_cool, subsector != "hydro") %>%
      add_title("Calibrated output of the cooling system options") %>%
      add_units("calOutputValue in EJ; share weights are unitless") %>%
      add_comments("Share weights set to 1 if technology level or aggregated subsector level output > 0") %>%
      add_legacy_name("L2233.StubTechProd_elec_cool") %>%
      add_precursors("common/GCAM_region_names",
                     "water/elec_tech_water_map",
                     "L1233.out_EJ_R_elec_F_tech_Yh_cool") ->
      L2233.StubTechProd_elec_cool

    L2233.StubTechCapFactor_elec_cool %>%
      add_title("Region-specific capacity factors of variable technologies with cooling systems") %>%
      add_units("unitless fraction") %>%
      add_comments("Assumptions copied from L223.StubTechCapFactor_elec") %>%
      add_legacy_name("L223.StubTechCapFactor_elec") %>%
      add_precursors("L223.StubTechCapFactor_elec") ->
      L2233.StubTechCapFactor_elec_cool

    L2233.StubTechFixOut_hydro %>%
      add_title("Fixed output for hydropower") %>%
      add_units("EJ") %>%
      add_comments("Created by binding base future year fixed hydro outputs") %>%
      add_legacy_name("L2233.StubTechFixOut_hydro") %>%
      add_precursors("common/GCAM_region_names",
                     "water/elec_tech_water_map",
                     "L1233.out_EJ_R_elec_F_tech_Yh_cool",
                     "L223.StubTechFixOut_hydro") ->
      L2233.StubTechFixOut_hydro

    L2233.GlobalTechCapital_elec_cool %>%
      add_title("Overnight capital, charge rates and capacity factors for standard cooling technologies") %>%
      add_units("1975USD") %>%
      add_comments("Capital costs converted from 2005USD") %>%
      add_comments("Fixed input assumptions for capacity factor and FCR") %>%
      add_legacy_name("L2233.GlobalTechCapital_elec_cool") %>%
      add_precursors("water/elec_tech_water_map",
                     "water/A23.CoolingSystemCosts") ->
      L2233.GlobalTechCapital_elec_cool

    L2233.GlobalIntTechCapital_elec_cool %>%
      add_title("Overnight capital, charge rates and capacity factors for standard cooling technologies") %>%
      add_units("1975USD") %>%
      add_comments("Capital costs converted from 2005USD") %>%
      add_comments("Fixed input assumptions for capacity factor and FCR") %>%
      add_legacy_name("L2233.GlobalIntTechCapital_elec_cool") %>%
      add_precursors("water/elec_tech_water_map",
                     "water/A23.CoolingSystemCosts") ->
      L2233.GlobalIntTechCapital_elec_cool

    L2233.GlobalTechCoef_elec_cool %>%
      add_title("Coefficients for water elec sector water withdrawal and consumption (standard techs)") %>%
      add_units("m^3/GJ") %>%
      add_comments("Water footprints mapped onto supplysector-subsector-technology tables") %>%
      add_comments("m^3/MWh converted to m^3/GJ") %>%
      add_legacy_name("L2233.GlobalTechCoef_elec_cool") %>%
      add_precursors("water/elec_tech_water_map",
                     "water/Macknick_elec_water_m3MWh",
                     "water/A03.sector") ->
      L2233.GlobalTechCoef_elec_cool

    L2233.GlobalIntTechCoef_elec_cool %>%
      add_title("Coefficients for water elec sector water withdrawal and consumption (intermittent techs)") %>%
      add_units("m^3/GJ") %>%
      add_comments("Water footprints mapped onto supplysector-subsector-technology tables") %>%
      add_comments("m^3/MWh converted to m^3/GJ") %>%
      add_legacy_name("L2233.GlobalTechCoef_elec_cool") %>%
      add_precursors("water/elec_tech_water_map",
                     "water/Macknick_elec_water_m3MWh",
                     "water/A03.sector") ->
      L2233.GlobalIntTechCoef_elec_cool



    L2233.DeleteCreditInput_elec %>%
      add_title("Remove the oil-credits inputs from the old elec tech") %>%
      add_units("NA") %>%
      add_comments("Remove the old oil-credits inputs so we can re-create them in the cooling tech") %>%
      add_legacy_name("L2233.DeleteCreditInput_elec") %>%
      add_precursors("L270.CreditInput_elec") ->
      L2233.DeleteCreditInput_elec

    L2233.CreditInput_elec %>%
      add_title("Re-create the old oil-credits inputs in the cooling tech") %>%
      add_units("Elec coef * constraint") %>%
      add_comments("Unmodified oil-credits inputs moved from the old elec tech into the cooling tech") %>%
      add_legacy_name("L2233.CreditInput_elec") %>%
      add_precursors("L270.CreditInput_elec", "water/elec_tech_water_map") ->
      L2233.CreditInput_elec

    return_data(L2233.StubTech_elecPassthru,
                L2233.StubTechProd_elecPassthru,
                L2233.GlobalPassThroughTech,
                L2233.GlobalTechEff_elecPassthru,
                L2233.GlobalTechShrwt_elecPassthru,
                L2233.GlobalIntTechCapital_elec,
                L2233.GlobalTechCapital_elecPassthru,
                L2233.GlobalIntTechOMfixed_elec,
                L2233.GlobalTechOMfixed_elecPassthru,
                L2233.GlobalIntTechOMvar_elec,
                L2233.GlobalTechOMvar_elecPassthru,
                L2233.GlobalTechInterp_elecPassthru,
                L2233.PassThroughSector_elec_cool,
                L2233.Supplysector_elec_cool,
                L2233.ElecReserve_elec_cool,
                L2233.SubsectorShrwtFllt_elec_cool,
                L2233.SubsectorLogit_elec_cool,
                L2233.StubTech_elec_cool,
                L2233.StubTechEff_elec_cool,
                L2233.StubTechProd_elec_cool,
                L2233.StubTechCapFactor_elec_cool,
                L2233.StubTechFixOut_hydro,
                L2233.StubTechShrwt_elec_cool,
                L2233.GlobalTechCapital_elec_cool,
                L2233.GlobalIntTechCapital_elec_cool,
                L2233.GlobalTechCoef_elec_cool,
                L2233.GlobalIntTechCoef_elec_cool,
                L2233.AvgFossilEffKeyword_elec_cool,
                L2233.GlobalIntTechBackup_elec_cool,
                L2233.GlobalIntTechEff_elec_cool,
                L2233.GlobalIntTechLifetime_elec_cool,
                L2233.GlobalIntTechShrwt_elec_cool,
                L2233.GlobalIntTechCapFac_elec_cool,
                L2233.GlobalTechCapFac_elec_cool,
                L2233.GlobalTechCapture_elec_cool,
                L2233.GlobalTechEff_elec_cool,
                L2233.GlobalTechLifetime_elec_cool,
                L2233.GlobalTechProfitShutdown_elec_cool,
                L2233.GlobalTechSCurve_elec_cool,
                L2233.GlobalTechShrwt_elec_cool,
                L2233.PrimaryRenewKeyword_elec_cool,
                L2233.PrimaryRenewKeywordInt_elec_cool,
                L2233.DeleteCreditInput_elec,
                L2233.CreditInput_elec)
  } else {
    stop("Unknown command")
  }
}
