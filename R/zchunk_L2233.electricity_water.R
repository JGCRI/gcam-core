#' module_water_L2233.electricity_water
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2233.EQUIV_TABLE}, \code{L2233.SectorNodeEquiv}, \code{L2233.TechNodeEquiv}, \code{L2233.StubTech_elecPassthru}, \code{L2233.StubTechProd_elecPassthru}, \code{L2233.GlobalPassThroughTech}, \code{L2233.GlobalTechEff_elecPassthru}, \code{L2233.GlobalTechShrwt_elecPassthru}, \code{L2233.GlobalIntTechCapital_elec}, \code{L2233.GlobalTechCapital_elecPassthru}, \code{L2233.GlobalIntTechOMfixed_elec}, \code{L2233.GlobalTechOMfixed_elecPassthru}, \code{L2233.GlobalIntTechOMvar_elec}, \code{L2233.GlobalTechOMvar_elecPassthru}, \code{L2233.PassThroughSector_elec_cool}, \code{`L2233.Supplysector_relative-cost-logit_elec_cool`}, \code{`L2233.Supplysector_absolute-cost-logit_elec_cool`},\code{L2233.Supplysector_elec_cool}, \code{L2233.ElecReserve_elec_cool}, \code{L2233.SubsectorShrwtFllt_elec_cool}, \code{`L2233.SubsectorLogit_relative-cost-logit_elec_cool`}, \code{`L2233.SubsectorLogit_absolute-cost-logit_elec_cool`}, \code{L2233.SubsectorLogit_elec_cool}, \code{L2233.StubTech_elec_cool}, \code{L2233.StubTechEff_elec_cool}, \code{L2233.StubTechProd_elec_cool}, \code{L2233.StubTechFixOut_hydro}, \code{L2233.StubTechShrwt_elec_cool}, \code{L2233.GlobalTechCapital_elec_cool}, \code{L2233.GlobalIntTechCapital_elec_cool}, \code{L2233.GlobalTechCoef_elec_cool}, \code{L2233.GlobalIntTechCoef_elec_cool}, \code{L2233.InputEmissCoeff_hist_elecPassthru}, \code{L2233.InputEmissCoeff_fut_elecPassthru, \code{L2233.AvgFossilEffKeyword_elec_cool}}. The corresponding file in the
#' original data system was \code{L2233.electricity_water.R} (water level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select first
#' @importFrom tidyr gather spread
#' @author ST June 2017
#' @export
module_water_L2233.electricity_water <- function(command, ...) {


  # Read in 40 L223 file names
  L223_fileNames <- c("AvgFossilEffKeyword_elec", "ElecReserve", "EQUIV_TABLE", "GlobalIntTechBackup_elec",
                      "GlobalIntTechCapital_elec", "GlobalIntTechEff_elec", "GlobalIntTechLifetime_elec",
                      "GlobalIntTechOMfixed_elec", "GlobalIntTechOMvar_elec", "GlobalIntTechShrwt_elec",
                      "GlobalTechCapture_elec", "GlobalTechCapital_elec", "GlobalTechEff_elec",
                      "GlobalTechInterp_elec", "GlobalTechLifetime_elec", "GlobalTechOMfixed_elec",
                      "GlobalTechOMvar_elec", "GlobalTechProfitShutdown_elec", "GlobalTechSCurve_elec",
                      "GlobalTechShrwt_elec", "PrimaryRenewKeyword_elec", "PrimaryRenewKeywordInt_elec",
                      "StubTech_elec", "StubTechCalInput_elec", "StubTechCapFactor_elec", "StubTechEff_elec",
                      "StubTechFixOut_elec", "StubTechFixOut_hydro", "StubTechProd_elec", "SubsectorInterp_elec",
                      "SubsectorInterpTo_elec", "SubsectorLogit_absolute-cost-logit", "SubsectorLogit_elec",
                      "SubsectorLogit_relative-cost-logit", "SubsectorShrwt_nuc", "SubsectorShrwt_renew",
                      "SubsectorShrwtFllt_elec", "Supplysector_absolute-cost-logit", "Supplysector_elec",
                      "Supplysector_relative-cost-logit")

  L223_fileNames_ <- paste0("temp-data-inject/L223.", L223_fileNames)
  names(L223_fileNames_) <- rep("FILE", 40)  # REMOVE ONCE OUTPUTS ARE READY!!

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
             "L1233.in_EJ_R_elec_F_tech_Yh_cool",
             "L1233.shrwt_R_elec_cool_Yf",
             FILE = "temp-data-inject/L223.StubTechEff_elec",
             FILE = "temp-data-inject/L201.en_bcoc_emissions",
             FILE = "temp-data-inject/L241.nonco2_tech_coeff",
             L223_fileNames_
    ))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2233.EQUIV_TABLE",
             "L2233.SectorNodeEquiv",
             "L2233.TechNodeEquiv",
             "L2233.StubTech_elecPassthru",
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
             "L2233.PassThroughSector_elec_cool",
             "`L2233.Supplysector_relative-cost-logit_elec_cool`",
             "`L2233.Supplysector_absolute-cost-logit_elec_cool`",
             "L2233.Supplysector_elec_cool",
             "L2233.ElecReserve_elec_cool",
             "L2233.SubsectorShrwtFllt_elec_cool",
             "`L2233.SubsectorLogit_relative-cost-logit_elec_cool`",
             "`L2233.SubsectorLogit_absolute-cost-logit_elec_cool`",
             "L2233.SubsectorLogit_elec_cool",
             "L2233.StubTech_elec_cool",
             "L2233.StubTechEff_elec_cool",
             "L2233.StubTechProd_elec_cool",
             "L2233.StubTechFixOut_hydro",
             "L2233.StubTechShrwt_elec_cool",
             "L2233.GlobalTechCapital_elec_cool",
             "L2233.GlobalIntTechCapital_elec_cool",
             "L2233.GlobalTechCoef_elec_cool",
             "L2233.GlobalIntTechCoef_elec_cool",
             "L2233.InputEmissCoeff_hist_elecPassthru",
             "L2233.InputEmissCoeff_fut_elecPassthru",
             "L2233.AvgFossilEffKeyword_elec_cool",
             "L2233.GlobalIntTechBackup_elec_cool",
             "L2233.GlobalIntTechEff_elec_cool",
             "L2233.GlobalIntTechLifetime_elec_cool",
             "L2233.GlobalIntTechShrwt_elec_cool",
             "L2233.GlobalTechCapture_elec_cool",
             "L2233.GlobalTechEff_elec_cool",
             "L2233.GlobalTechInterp_elec_cool",
             "L2233.GlobalTechLifetime_elec_cool",
             "L2233.GlobalTechProfitShutdown_elec_cool",
             "L2233.GlobalTechSCurve_elec_cool",
             "L2233.GlobalTechShrwt_elec_cool",
             "L2233.PrimaryRenewKeyword_elec_cool",
             "L2233.PrimaryRenewKeywordInt_elec_cool"))
  } else if(command == driver.MAKE) {


    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A23.globalinttech <- get_data(all_data, "energy/A23.globalinttech")
    A23.globaltech_shrwt <- get_data(all_data, "energy/A23.globaltech_shrwt")
    A23.sector <- get_data(all_data, "energy/A23.sector")
    elec_tech_water_map <- get_data(all_data, "water/elec_tech_water_map")
    A03.sector <- get_data(all_data, "water/A03.sector")
    A23.CoolingSystemCosts <- get_data(all_data, "water/A23.CoolingSystemCosts")
    Macknick_elec_water_m3MWh <- get_data(all_data, "water/Macknick_elec_water_m3MWh")
    L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh")
    L1233.out_EJ_R_elec_F_tech_Yh_cool <- get_data(all_data, "L1233.out_EJ_R_elec_F_tech_Yh_cool")
    L1233.in_EJ_R_elec_F_tech_Yh_cool <- get_data(all_data, "L1233.in_EJ_R_elec_F_tech_Yh_cool")
    L1233.shrwt_R_elec_cool_Yf <- get_data(all_data, "L1233.shrwt_R_elec_cool_Yf")
    L223.StubTechEff_elec <- get_data(all_data, "temp-data-inject/L223.StubTechEff_elec")
    L201.en_bcoc_emissions <- get_data(all_data, "temp-data-inject/L201.en_bcoc_emissions")
    L241.nonco2_tech_coeff <- get_data(all_data, "temp-data-inject/L241.nonco2_tech_coeff")

    # Use get_data_ function with sapply to read in all "L223." inputs at once
    L223_data <- sapply(paste0("temp-data-inject/L223.", L223_fileNames),
                        get_data_, all_data = all_data)
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


    ## PART 1: SUPPLY SECTOR AND SUB SECTOR INFORMATION IN THE EXISTING ELECTRICITY SECTOR
    ## INCLUDES SUPPLYSECTOR, SUBSECTOR, AND REGION-SPECIFIC STUB TECH CAPACITY FACTOR MODIFICATIONS

    # **Note from old ds: If capital costs are moved to the downstream sectors (the cooling system technologies)...
    # ... then the StubTechCapFactor table should not be here. Instead, it should be processed with...
    # ... the stub tech info in the new sectors**

    # Filter the L223 files list for those with "technology" within any column header ...
    # ... retaining StubTechCapFactor_elec, too.
    techFltr <- sapply(L223_data, function(x) !grepl("technology", paste(names(x), collapse = " ")) |
                         grepl("StubTechCapFactor_elec", attributes(x)$title))
    L2233.Elec_tables_copy <- L223_data[techFltr]

    # Reorder tables to the order in which they'll be written to xml
    # (without reordering the default fillout shareweights will over-write manually set share-weights,
    # ... and the supply sector info will be written after subsector information)

    elcTbNms <- names(L2233.Elec_tables_copy)
    L2333.reorder <- c(which(grepl("EQUIV_TABLE", elcTbNms)),
                       which(grepl("Supplysector.*logit", elcTbNms)),
                       which(grepl("Supplysector", elcTbNms)),
                       which(grepl("SubsectorShrwtFllt", elcTbNms)),
                       which(grepl("Subsector.*logit", elcTbNms)))
    L2333.reorder_ <- c(unique(L2333.reorder),
                        which(!(elcTbNms %in% elcTbNms[L2333.reorder])))
    L2233.Elec_tables_copy_ <- L2233.Elec_tables_copy[L2333.reorder_]

    ## ^^ WON'T BE REQUIRED IF OUTPUT ORDER IS SPECIFIED BELOW ^^


    wipeAttributes <- function(x){
      attr(x, ATTR_TITLE) <- NULL
      attr(x, ATTR_COMMENTS) <- NULL
      attr(x, "flags") <- NULL
      attr(x, "spec") <- NULL
      return(x)
    }
    L2233.Elec_tables_copy_ <- lapply(L2233.Elec_tables_copy, wipeAttributes)

    L2233.Elec_tables_copy_$EQUIV_TABLE %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_legacy_name("L2233.EQUIV_TABLE") %>%
      add_precursors("temp-data-inject/L223.EQUIV_TABLE") %>%
      add_flags() ->
      L2233.EQUIV_TABLE
    ## ABOVE NEEDS TO BE DONE FOR ALL 14 TIBBLES IN L2233.Elec_tables_copy_
    ## SMARTER TO DO THIS BY PASSING THE TITLE, UNITS ETC FROM THE INPUT...
    ## ... TO THE OUTPUT (RATHER THAN WIPING AND RE-ADDING)... COULD BE DONE BY SIMPLY ...
    ## ... WAITING FOR THE OUTPUTS TO BE READY, THEN REMOVING THE ADD_TITLE ETC!


    ## PART 2: PASS-THOUGH TECHNOLOGIES IN THE EXISTING ELECTRICITY SECTOR

    # Note: The technologies in the electricity sector all keep their own name;
    # the only parameters read here are shareweights and minicam-energy-inputs,
    # which are equal to the "to.supplysector" of the new generation technology.

    L223_data$StubTech_elec -> #%>%
      # add_title("descriptive title of data") %>%
      # add_units("units") %>%
      # add_comments("comments describing how data generated") %>%
      # add_comments("can be multiple lines") %>%
      # add_legacy_name("L2233.EQUIV_TABLE") %>%
      # add_precursors("L223.StubTech_elec") %>%
      # add_flags(FLAG_NO_XYEAR)
      L2233.StubTech_elecPassthru
    # ^^ THIS IS A STRAIGHT PASS FROM INPUT TO OUTPUT - FIX ONCE L223.StubTech_elec IS READY


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
      select(sector.name, subsector.name, technology) -> L2233.GlobalPassThroughTech # --OUTPUT--

    # Match in the global tech shareweights from the assumptions to the electric sector
    A23.globaltech_shrwt %>%
      gather(year, share.weight, -supplysector, -subsector, -technology) %>%
      mutate(year = as.integer(year)) %>%
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
                 by = c("year", "sector.name","subsector.name", "technology")) ->
      L2233.GlobalTechEffShrwt_elecPassthru
    # ^^ this tibble is subsetted to give the following two outputs...

    L2233.GlobalTechEffShrwt_elecPassthru %>%
      select(sector.name, subsector.name, technology, year, minicam.energy.input, efficiency) ->
      L2233.GlobalTechEff_elecPassthru # --OUTPUT--
    L2233.GlobalTechEffShrwt_elecPassthru %>%
      select(sector.name, subsector.name, technology, year, share.weight) ->
      L2233.GlobalTechShrwt_elecPassthru # --OUTPUT--

    L1231.out_EJ_R_elec_F_tech_Yh %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(calibrated_techs,
                                      - minicam.energy.input,
                                      -calibration, -secondary.output),
                               by = c("sector", "fuel", "technology")) %>%
      rename(stub.technology = technology, calOutputValue = value) %>%
      mutate(share.weight.year = year) -> StubTechProd_elecPassthru

    # the following replaces the "set_subsector_shrwt" function from the old data system...
    # we essentially aggregate output for subsectors with multiple technologies and then assign a
    # shareweight of 1 for any technology residing in a subsector that has aggregated output > 0.
    StubTechProd_elecPassthru %>%
      group_by(region, sector, subsector, year) %>%
      summarise(subs.share.weight = sum(calOutputValue)) %>% ungroup %>%
      left_join(StubTechProd_elecPassthru, by = c("region", "sector", "subsector", "year")) %>%
      mutate(subs.share.weight = if_else(subs.share.weight > 0, 1, 0)) %>%
      mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      filter(subsector != "hydro") %>%  # << hydro is fixed output (doesn't need calibrating here)
      select(-GCAM_region_ID, -sector, -fuel) %>%  # << remove unwanted columns
      select(region, supplysector, subsector, stub.technology,  # << better ordering of columns
             year,share.weight.year, subs.share.weight, tech.share.weight, calOutputValue) ->
      L2233.StubTechProd_elecPassthru # --OUTPUT--


    ## PART 3: SUPPLYSECTOR AND SUBSECTOR INFORMATION IN THE NEW ELEC SECTORS

    L2233.TechMap %>%
      select(to.supplysector) %>%
      filter(!(to.supplysector %in% L2233.StubTech_elecPassthru$supplysector)) %>%
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
      select(region, supplysector) %>%
      rename(pass.through.sector = supplysector) %>%
      mutate(marginal.revenue.sector = "electricity") %>%
      mutate(marginal.revenue.market = region) -> L2233.PassThroughSector_elec_cool # --OUTPUT--

    L2233.supplysector_info %>%
      select(region, supplysector, logit.type) %>%
      mutate(logit.type = "relative-cost-logit") -> supplysector_costLogit_elec_cool
    # ^^ replaces the "get_logit_fn_tables" function from the old data system...
    # ...and here we're simply setting all logit-types to "relative-cost-logit"

    `L2233.Supplysector_relative-cost-logit_elec_cool` <- supplysector_costLogit_elec_cool # --OUTPUT--
    `L2233.Supplysector_absolute-cost-logit_elec_cool` <-
      filter(supplysector_costLogit_elec_cool, logit.type == "absolute-cost-logit") # --OUTPUT--
    L2233.Supplysector_elec_cool <- select(L2233.supplysector_info, -logit.type) # --OUTPUT--

    L2233.TechMap %>%
      select(from.supplysector, from.subsector, from.technology, to.supplysector) %>%
      filter(from.supplysector %in% A23.globalinttech$supplysector &
               from.subsector %in% A23.globalinttech$subsector &
               from.technology %in% A23.globalinttech$technology) %>%
      select(to.supplysector) %>% unique -> L2233.elec_cool_Int_supplysectors

    L2233.supplysector_info %>%
      select(-logit.type) %>%
      filter(supplysector %in% L2233.elec_cool_Int_supplysectors$to.supplysector) %>%
      select(region, supplysector) %>%
      mutate(electricity.reserve.margin = unique(A23.sector$electricity.reserve.margin)) %>%
      mutate(average_grid.capacity.factor = unique(A23.sector$average.grid.capacity.factor)) ->
      # ^^ Margin and capacity factor assumed to be same as for electricity and elect_td_bld
      L2233.ElecReserve_elec_cool # --OUTPUT--



    elec_tech_water_map %>%
      select(to.supplysector, to.subsector) %>%
      unique %>%
      rename(supplysector = to.supplysector, subsector = to.subsector) ->
      supply_sub_elec_mapping
    # ^^ sets up simple mapping for supply to subsector for following section

    L2233.supplysector_info %>%
      select(region, supplysector) %>%
      left_join_error_no_match(supply_sub_elec_mapping, by = "supplysector") %>%
      mutate(year.fillout = first(BASE_YEARS)) %>%
      mutate(share.weight = 1) -> L2233.SubsectorShrwtFllt_elec_cool

    L2233.SubsectorShrwtFllt_elec_cool %>%
      select(region, supplysector, subsector) %>%
      mutate(logit.year.fillout = first(BASE_YEARS)) %>%
      mutate(logit.exponent = COOLING_SYSTEM_LOGIT) -> L2233.SubsectorLogit_elec_cool

    L2233.SubsectorLogit_elec_cool %>%
      select(region, supplysector, subsector) %>%
      mutate(logit.type = "relative-cost-logit") ->
      # ^^ replaces the "get_logit_fn_tables" function from the old data system...
      # ...and here we're simply setting all logit-types to "relative-cost-logit"
      `L2233.SubsectorLogit_relative-cost-logit_elec_cool`

    `L2233.SubsectorLogit_relative-cost-logit_elec_cool` %>%
      filter(logit.type == "absolute-cost-logit") -> `L2233.SubsectorLogit_absolute-cost-logit_elec_cool`

    #==========OUTPUT========================

    #========================================


    ## PART 4: GLOBAL TECHNOLOGY INFORMATION RE-ASSIGNED TO NEW STRUCTURE

    # Notes: (1) All global technology information from the prior electricity sector...
    #            ...is carried over to the new locations;
    #        (2) We use the string "sector.name" to identify tables with global technologies...
    #            ... since not all have "GlobalTech" in their name

    # Filter the L223 files list for those with "sector.name" as a column header
    sctrFltr <- sapply(L223_data, function(x) "sector.name" %in% attributes(x)$names)
    L2233.Elec_tables_globaltech <- L223_data[sctrFltr]

    # If the table has cost information, then we keep it in the passthrough technology...
    # ... because we don't want the whole-plant costs to be bundled with the cooling...
    # ... system costs. Keeping these together would mean that cooling system decisions...
    # ... would be made on < 1% cost differences, which would require very high logit...
    # ... to get any behavior at all.
    costFltr <- sapply(L2233.Elec_tables_globaltech, function(x)
      grepl("Capital", attributes(x)$title) |
        grepl("OM", attributes(x)$title))
    L2233.Elec_tables_globaltech_cost <- L2233.Elec_tables_globaltech[costFltr]
    # ^^ filters for tables with capital, fixed O&M and variable O&M

    # We reset the intermittent technology to standard technologies that remain in ...
    # ... the electricity sector. For an intermittent technology to be moved to a new ...
    # ... sector, the new passthur tech in the electricity sector is no longer an ...
    # ... intermittent technology (this would double count).

    # for all L2233.Elec_tables_globaltech_cost, change "intermittent.technology" to "technology"

    elec_tech_water_map %>%
      select(-plant_type, - cooling_system, - water_type,
             -sector, -fuel, -technology, -minicam.energy.input) %>%
      rename(sector.name = from.supplysector,
             subsector.name = from.subsector,
             technology = from.technology) %>%  unique -> elec_tech_water_map_
    # ^^ reduce and rename elec_tech_water_map for joining to all tables in L2233.Elec_tables_globaltech_cost

    resetTech <- function(x){
      names(x)[names(x) == "intermittent.technology"] <- "technology"
      x %>% left_join_keep_first_only(elec_tech_water_map_,by = c("sector.name", "subsector.name", "technology"))
    } # ^^ keep_first_only applied for to.technology column (maintaining arbitrary column from legacy code)
    # ^^ resetTech used to join tables in L2233.Elec_tables_globaltech_cost to elec_tech_water_map_, performed in next line:
    L2233.Elec_tables_glbTechCost_expanded <- sapply(L2233.Elec_tables_globaltech_cost, resetTech)

    L2233.Elec_tables_glbTechCost_expanded$GlobalTechCapital_elec %>%
      bind_rows(L2233.Elec_tables_glbTechCost_expanded$GlobalIntTechCapital_elec) ->
      GlobalTechCapital_elecPassthru
    L2233.Elec_tables_glbTechCost_expanded$GlobalTechOMfixed_elec %>%
      bind_rows(L2233.Elec_tables_glbTechCost_expanded$GlobalIntTechOMfixed_elec) ->
      GlobalTechOMfixed_elecPassthru
    L2233.Elec_tables_glbTechCost_expanded$GlobalTechOMvar_elec %>%
      bind_rows(L2233.Elec_tables_glbTechCost_expanded$GlobalIntTechOMvar_elec) ->
      GlobalTechOMvar_elecPassthru

    # The following section partitions the intermittend and standard technologies.
    # Intermittent technologies are not moved to different supplysector/subsector/technology

    # Capital costs of intermittent technologies applied in the electricity sector...
    GlobalTechCapital_elecPassthru %>%
      filter(sector.name == to.supplysector & sector.name %in% A23.globalinttech$supplysector) %>%
      filter(subsector.name == to.subsector & subsector.name %in% A23.globalinttech$subsector) %>%
      filter(technology == to.technology & technology %in% A23.globalinttech$technology) %>%
      select(-to.supplysector, -to.subsector, -to.technology) -> L2233.GlobalIntTechCapital_elec

    # Capital costs of passthru technologies in the electricity sector...
    # Note: generating costs applied here; cooling sys techs only have cooling sys costs
    GlobalTechCapital_elecPassthru %>%
      filter(!(sector.name %in% L2233.GlobalIntTechCapital_elec$sector.name &
                 subsector.name %in% L2233.GlobalIntTechCapital_elec$subsector.name &
                 technology %in% L2233.GlobalIntTechCapital_elec$technology)) %>%
      select(-to.supplysector, -to.subsector, -to.technology) -> L2233.GlobalTechCapital_elecPassthru

    # OMfixed costs of intermittent technologies applied in the electricity sector
    GlobalTechOMfixed_elecPassthru %>%
      filter(sector.name == to.supplysector & sector.name %in% A23.globalinttech$supplysector) %>%
      filter(subsector.name == to.subsector & subsector.name %in% A23.globalinttech$subsector) %>%
      filter(technology == to.technology & technology %in% A23.globalinttech$technology) %>%
      select(-to.supplysector, -to.subsector, -to.technology) -> L2233.GlobalIntTechOMfixed_elec

    # OMfixed costs of passthru technologies in the electricity sector
    GlobalTechOMfixed_elecPassthru %>%
      filter(!(sector.name %in% L2233.GlobalIntTechCapital_elec$sector.name &
                 subsector.name %in% L2233.GlobalIntTechCapital_elec$subsector.name &
                 technology %in% L2233.GlobalIntTechCapital_elec$technology)) %>%
      select(-to.supplysector, -to.subsector, -to.technology) -> L2233.GlobalTechOMfixed_elecPassthru

    # OMvar costs of intermittent technologies applied in the electricity sector
    GlobalTechOMvar_elecPassthru %>%
      filter(sector.name == to.supplysector & sector.name %in% A23.globalinttech$supplysector) %>%
      filter(subsector.name == to.subsector & subsector.name %in% A23.globalinttech$subsector) %>%
      filter(technology == to.technology & technology %in% A23.globalinttech$technology) %>%
      select(-to.supplysector, -to.subsector, -to.technology) -> L2233.GlobalIntTechOMvar_elec

    # OMvar costs of passthru technologies in the electricity sector
    GlobalTechOMvar_elecPassthru %>%
      filter(!(sector.name %in% L2233.GlobalIntTechCapital_elec$sector.name &
                 subsector.name %in% L2233.GlobalIntTechCapital_elec$subsector.name &
                 technology %in% L2233.GlobalIntTechCapital_elec$technology)) %>%
      select(-to.supplysector, -to.subsector, -to.technology) -> L2233.GlobalTechOMvar_elecPassthru

    # filter Elec_tables_globaltech for those *without* costs
    L2233.Elec_tables_globaltech_nocost <- L2233.Elec_tables_globaltech[!costFltr]

    # Note: The following function automates manipulation of all 14 tables contained...
    # ... in L2233.Elec_tables_globaltech_nocost. All tables contain fewer technologies...
    # ... than required for the number of cooling system options, so we work backwards...
    # ... from the tables with all possible global technology names and years available.
    prepGlobalTechNoCostOutputs <- function(elecTableName){
      tableName <- paste0("L2233.", elecTableName, "_cool")
      elecTable <- L2233.Elec_tables_globaltech_nocost[[which(names(L2233.Elec_tables_globaltech_nocost) == elecTableName)]]
      names(elecTable)[names(elecTable) == "intermittent.technology"] <- "technology"
      defCols <- names(elecTable) %in% c("sector.name","subsector.name", "technology", "year")
      nondataCols <- names(elecTable)[defCols]
      dataCols <- names(elecTable)[!defCols]
      if (!("year" %in% nondataCols)) elecTable %>% mutate(year = NA) -> elecTable
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
        select(one_of(c(nondataCols, dataCols))) %>%
        unique %>% na.omit -> newTable
      if ("efficiency" %in% names(newTable)){
        mutate(newTable,
               efficiency = if_else(grepl("dry", technology),
                                    efficiency * DRY_COOLING_EFF_ADJ, as.double(efficiency))) ->
          newTable
      }
      return(newTable %>%
               add_comments("Auto-generated by prepGlobalTechNoCostOutputs function in L2233.electricity_water") %>%
               add_legacy_name(tableName) %>%
               add_precursors(paste0("temp-data-inject/L223.", elecTableName),
                              "water/elec_tech_water_map"))
    }

    L2233.Elec_tables_globaltech_nocost_ <- sapply(names(L2233.Elec_tables_globaltech_nocost),
                                                   prepGlobalTechNoCostOutputs)

    # Capital costs of cooling systems only
    A23.CoolingSystemCosts %>%
      gather(year, value, -cooling_system, -input.capital) %>%
      mutate(year = as.integer(year)) %>%
      complete(nesting(cooling_system, input.capital),
               year = c(year, MODEL_YEARS)) %>%
      group_by(cooling_system, input.capital) %>%
      mutate(value = approx_fun(year, value)) %>% ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% rename(capital.overnight_USD2005 = value) %>%
      mutate(capital.overnight = round(capital.overnight_USD2005 * 0.3362, 0)) -> L2233.CoolingSystemCosts
    # conv_2005_1975_USD = 0.3362

    elec_tech_water_map %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(L2233.CoolingSystemCosts, by = c("cooling_system", "year")) %>%
      mutate(fixed.charge.rate = 0.15, capacity.factor = 0.6) %>%   ## cooling_system_FCR and cooling_system_capacity_factor
      select(-technology) %>%
      rename(sector.name = to.supplysector,
             subsector.name = to.subsector,
             technology = to.technology) %>%
      filter(capital.overnight > 0) ->
      L2233.GlobalTechCapital_elec_cool_all # includes both standard and intermittent!



    # L2233.GlobalTechCapital_elec_cool_all needs to be partitioned into standard into intermittent techs
    L2233.GlobalTechCapital_elec_cool_all %>%
      filter(from.supplysector %in% A23.globalinttech$supplysector &
               from.subsector %in% A23.globalinttech$subsector &
               from.technology %in% A23.globalinttech$technology) %>%
      select(sector.name, subsector.name, technology, year, input.capital,
             capital.overnight, fixed.charge.rate, capacity.factor) ->
      L2233.GlobalIntTechCapital_elec_cool
    L2233.GlobalTechCapital_elec_cool_all %>%
      filter(!(from.supplysector %in% A23.globalinttech$supplysector &
                 from.subsector %in% A23.globalinttech$subsector &
                 from.technology %in% A23.globalinttech$technology)) %>%
      select(sector.name, subsector.name, technology, year, input.capital,
             capital.overnight, fixed.charge.rate, capacity.factor) ->
      L2233.GlobalTechCapital_elec_cool

    # Water demand coefficients of electric technologies
    elec_tech_water_map %>%
      select(to.supplysector, to.subsector, to.technology, fuel, technology, cooling_system, water_type,
             from.supplysector,  from.subsector, from.technology) %>%
      filter(water_type != "none") %>%
      left_join_error_no_match(select(Macknick_elec_water_m3MWh, -sector),
                               by = c("fuel", "technology", "cooling_system", "water_type")) %>%
      mutate(water_withdrawals = round(water_withdrawals / CONV_MWH_GJ, 7)) %>%
      mutate(water_consumption = round(water_consumption / CONV_MWH_GJ, 7)) %>%
      gather(minicam.energy.input, coefficient,
             -to.supplysector, -to.subsector, -to.technology, -fuel, -technology, -cooling_system, -water_type,
             -from.supplysector, -from.subsector, -from.technology) %>%
      select(-technology) %>%
      # ^^ technology column dropped to prevent duplicate variable names...
      rename(sector.name = to.supplysector, subsector.name = to.subsector, technology = to.technology) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      #mutate(minicam.energy.input = sub("_", " ", minicam.energy.input)) %>%
      filter(!(water_type == "seawater" & minicam.energy.input == "water_consumption")) %>%
      # ^^ seawater is not relevant for consumption since since scarcity isn't an issue
      mutate(minicam.energy.input = if_else(water_type == "seawater", "seawater", minicam.energy.input)) %>%
      mutate(minicam.energy.input = replace(minicam.energy.input, minicam.energy.input == "water_withdrawals", "water_td_elec_W")) %>%
      mutate(minicam.energy.input = replace(minicam.energy.input, minicam.energy.input == "water_consumption", "water_td_elec_C")) %>%
      select(-fuel, -cooling_system, -water_type) -> L2233.GlobalTechCoef_elec_cool_all

    # L2233.GlobalTechCoef_elec_cool_all needs to be partitioned into standard and intermittent techs
    L2233.GlobalTechCoef_elec_cool_all %>%
      filter(!(from.supplysector %in% A23.globalinttech$supplysector &
               from.subsector %in% A23.globalinttech$subsector &
               from.technology %in% A23.globalinttech$technology)) %>%
      select(-from.supplysector, -from.subsector, -from.technology) ->
      L2233.GlobalTechCoef_elec_cool
    L2233.GlobalTechCoef_elec_cool_all %>%
      filter(from.supplysector %in% A23.globalinttech$supplysector &
                 from.subsector %in% A23.globalinttech$subsector &
                 from.technology %in% A23.globalinttech$technology) %>%
      select(-from.supplysector, -from.subsector, -from.technology) ->
      L2233.GlobalIntTechCoef_elec_cool

    # PART 5:











    # This needs to be partitioned between int-techs and standard techs
    #   L2233.GlobalIntTechCapital_elec_cool <- subset( L2233.GlobalTechCapital_elec_cool,
    #                                                   paste( from.supplysector, from.subsector, from.technology ) %in%
    #                                                     vecpaste( A23.globalinttech[ s_s_t ] ) )[ names_GlobalTechCapital ]
    # L2233.GlobalTechCapital_elec_cool <- subset( L2233.GlobalTechCapital_elec_cool,
    #                                              paste( sector.name, subsector.name, technology ) %!in%
    #                                                vecpaste( L2233.GlobalIntTechCapital_elec_cool[ c( "sector.name", "subsector.name", "technology" ) ] ) )[ names_GlobalTechCapital ]
    #



    ## (1) Left some comments blank... almost none of these outputs are generated from any calcs...
    ## It's pretty much all wrangling of intputs.

    ## (2) A lot of code is repeated across multiple tables stored inside lists...
    ## The legacy code does this with for loops, but I've replaced with apply functions,
    ## which I think are neater and run more smoothly.

    ## (3) There are so many outputs (34) that I found it much more convenient to
    ## write them as I went along (rather than dump all at the end).



    ## L2233.Elec_tables_globaltech_nocost_ outputs...

    L2233.Elec_tables_globaltech_nocost_$AvgFossilEffKeyword_elec %>%
      add_title("Average efficiencies of fossil fuels") %>%
      add_units("unitless") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.AvgFossilEffKeyword_elec_cool
    L2233.Elec_tables_globaltech_nocost_$GlobalIntTechBackup_elec %>%
      add_title("TBC") %>%
      add_units("TBC") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechBackup_elec_cool
    L2233.Elec_tables_globaltech_nocost_$GlobalIntTechEff_elec %>%
      add_title("Cooling efficiencies of intermittent electricity generating technologies") %>%
      add_units("Unitless") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechEff_elec_cool
    L2233.Elec_tables_globaltech_nocost_$GlobalIntTechLifetime_elec %>%
      add_title("Lifetimes of intermittent electricity generating technologies") %>%
      add_units("Years") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechLifetime_elec_cool
    L2233.Elec_tables_globaltech_nocost_$GlobalIntTechShrwt_elec %>%
      add_title("Shareweights of intermittent electricity generating technologies") %>%
      add_units("Unitless") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechShrwt_elec_cool
    L2233.Elec_tables_globaltech_nocost_$GlobalTechCapture_elec %>%
      add_title("Storage markets and remove fractions for standard electricity generating technolgies") %>%
      add_units("Unitless") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalTechCapture_elec_cool
    L2233.Elec_tables_globaltech_nocost_$GlobalTechCapture_elec %>%
      add_title("Storage markets and remove fractions for CCS tech by cooling type") %>%
      add_units("Unitless") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalTechCapture_elec_cool
    L2233.Elec_tables_globaltech_nocost_$GlobalTechEff_elec %>%
      add_title("Cooling efficiencies for electricity generating technologies") %>%
      add_units("Unitless") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalTechEff_elec_cool
    L2233.Elec_tables_globaltech_nocost_$GlobalTechInterp_elec %>%
      add_title("Table headers for temporal interpolation of shareweights") %>%
      add_units("NA") ->
      L2233.GlobalTechInterp_elec_cool
    L2233.Elec_tables_globaltech_nocost_$GlobalTechLifetime_elec %>%
      add_title("Lifetimes for standard electricity generating technologies") %>%
      add_units("Years") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalTechLifetime_elec_cool
    L2233.Elec_tables_globaltech_nocost_$GlobalTechProfitShutdown_elec %>%
      add_title("Shutdown points and profit shutdown steepness for standard electricity generating technologies") %>%
      add_units("Unitless") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalTechProfitShutdown_elec_cool
    L2233.Elec_tables_globaltech_nocost_$GlobalTechSCurve_elec %>%
      add_title("TBC") %>%
      add_units("Unitless") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalTechSCurve_elec_cool
    L2233.Elec_tables_globaltech_nocost_$GlobalTechShrwt_elec %>%
      add_title("TBC") %>%
      add_units("Unitless") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalTechShrwt_elec_cool
    L2233.Elec_tables_globaltech_nocost_$PrimaryRenewKeyword_elec %>%
      add_title("TBC") %>%
      add_units("NA") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.PrimaryRenewKeyword_elec_cool
    L2233.Elec_tables_globaltech_nocost_$PrimaryRenewKeywordInt_elec %>%
      add_title("TBC") %>%
      add_units("NA") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.PrimaryRenewKeywordInt_elec_cool







    # ===================================================

    # Produce outputs

    # --XX--
    L2233.GlobalPassThroughTech %>%
      add_title("Electricity sector, subsector, technology mapping") %>%
      add_units("NA") %>%
      add_comments("Generated by expansion of water/elec_tech_water_map for all years (plus filter and renaming of columns)") %>%
      add_legacy_name("L2233.GlobalPassThroughTech") %>%
      add_precursors("water/elec_tech_water_map") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalPassThroughTech

    # --XX--
    L2233.GlobalTechEff_elecPassthru %>%
      add_title("Input name and efficiency of pass-through technologies in the electricity sector") %>%
      add_units("Unitless") %>%
      add_comments("") %>%
      add_legacy_name("L2233.GlobalTechEff_elecPassthru") %>%
      add_precursors("energy/A23.globaltech_shrwt", "water/elec_tech_water_map") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechEff_elecPassthru

    # --XX--
    L2233.GlobalTechShrwt_elecPassthru %>%
      add_title("Share-weights of pass-through technologies in the electricity sector") %>%
      add_units("units") %>%
      add_comments("") %>%
      add_legacy_name("L2233.GlobalTechShrwt_elecPassthru") %>%
      add_precursors("energy/A23.globaltech_shrwt", "water/elec_tech_water_map")%>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechShrwt_elecPassthru

    # --XX--
    L2233.StubTechProd_elecPassthru %>%
      add_title("Calibrated electricity flow through the pass-through technologies") %>%
      add_units("EJ") %>%
      add_comments("") %>%
      add_legacy_name("L2233.StubTechProd_elecPassthru") %>%
      add_precursors("energy/calibrated_techs",
                     "L1231.out_EJ_R_elec_F_tech_Yh",
                     "common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTechProd_elecPassthru

    # --XX--
    L2233.PassThroughSector_elec_cool %>%
      add_title("Pass-through sectors with marginal revenue sector and markets") %>%
      add_units("NA") %>%
      add_comments("Marginal revenue set to electricity; market set to region") %>%
      add_legacy_name("L2233.PassThroughSector_elec_cool") %>%
      add_precursors("temp-data-inject/L223.Supplysector_elec",
                     "water/elec_tech_water_map",
                     "temp-data-inject/L223.StubTech_elec") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.PassThroughSector_elec_cool

    # --XX--
    `L2233.Supplysector_relative-cost-logit_elec_cool` %>%
      add_title("Supplysector (elec cooling) relative cost logit type table") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2233.Supplysector_relative-cost-logit_elec_cool") %>%
      add_precursors("temp-data-inject/L223.Supplysector_elec",
                     "water/elec_tech_water_map",
                     "temp-data-inject/L223.StubTech_elec") ->
      `L2233.Supplysector_relative-cost-logit_elec_cool`

    # --XX--
    `L2233.Supplysector_absolute-cost-logit_elec_cool` %>%
    add_title("Supplysector (elec cooling) absolute cost logit type table") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2233.Supplysector_absolute-cost-logit_elec_cool") %>%
      add_precursors("temp-data-inject/L223.Supplysector_elec",
                     "water/elec_tech_water_map",
                     "temp-data-inject/L223.StubTech_elec") ->
      `L2233.Supplysector_absolute-cost-logit_elec_cool`

    # --XX--
    L2233.Supplysector_elec_cool %>%
      add_title("Supplysector information for elec cooling") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2233.Supplysector_elec_cool") %>%
      add_precursors("temp-data-inject/L223.Supplysector_elec",
                     "water/elec_tech_water_map",
                     "temp-data-inject/L223.StubTech_elec") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.Supplysector_elec_cool

    # --XX--
    L2233.ElecReserve_elec_cool %>%
      add_title("Electricity reserve margin and average grid capacity factor") %>%
      add_units("Unitless") %>%
      add_comments("Factors assumed to be same as for electricity and elect_td_bld") %>%
      add_legacy_name("L2233.ElecReserve_elec_cool") %>%
      add_precursors("energy/A23.sector",
                     "temp-data-inject/L223.Supplysector_elec",
                     "water/elec_tech_water_map",
                     "temp-data-inject/L223.StubTech_elec") ->
      L2233.ElecReserve_elec_cool

    # --XX--
    L2233.SubsectorShrwtFllt_elec_cool %>%
      add_title("Subsector information for electricity cooling") %>%
      add_units("Unitless") %>%
      add_comments("Assumes that the subsectors are just pass-through") %>%
      add_comments("Assumes cooling system competition takes place at the technology level") %>%
      add_legacy_name("L2233.SubsectorShrwtFllt_elec_cool") %>%
      add_precursors("water/elec_tech_water_map",
                     "temp-data-inject/L223.Supplysector_elec",
                     "temp-data-inject/L223.StubTech_elec") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.SubsectorShrwtFllt_elec_cool

    # --XX--
    L2233.SubsectorLogit_elec_cool %>%
      add_title("Logit exponent for cooling system choice") %>%
      add_units("Unitless") %>%
      add_comments("Default cooling system logit applied to all sectors") %>%
      add_legacy_name("L2233.SubsectorLogit_elec_cool") %>%
      add_precursors("water/elec_tech_water_map",
                     "temp-data-inject/L223.Supplysector_elec",
                     "temp-data-inject/L223.StubTech_elec") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.SubsectorLogit_elec_cool

    # --XX--
    `L2233.SubsectorLogit_relative-cost-logit_elec_cool` %>%
      add_title("Subsector (elec cooling) relative cost logit type table") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2233.SubsectorLogit_relative-cost-logit_elec_cool") %>%
      add_precursors("water/elec_tech_water_map",
                     "temp-data-inject/L223.Supplysector_elec",
                     "temp-data-inject/L223.StubTech_elec") ->
      `L2233.SubsectorLogit_relative-cost-logit_elec_cool`

    # --XX--
    `L2233.SubsectorLogit_absolute-cost-logit_elec_cool` %>%
      add_title("Subsector (elec cooling) absolute cost logit type table") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L2233.SubsectorLogit_absolute-cost-logit_elec_cool") %>%
      add_precursors("water/elec_tech_water_map",
                     "temp-data-inject/L223.Supplysector_elec",
                     "temp-data-inject/L223.StubTech_elec") ->
      `L2233.SubsectorLogit_absolute-cost-logit_elec_cool`



    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.SectorNodeEquiv") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.SectorNodeEquiv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.TechNodeEquiv") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.TechNodeEquiv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTech_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTech_elecPassthru



    L2233.GlobalIntTechCapital_elec %>%
      add_title("Capital costs for intermittent electricity generating tech") %>%
      add_units("TBC") %>% ## think this is $1975/GJ for capital.overnight... not sure about fixed.charge.rate
      add_comments("") %>%
      add_legacy_name("L2233.GlobalIntTechCapital_elec") %>%
      add_precursors("water/elec_tech_water_map",
                     "energy/A23.globalinttech",
                     "temp-data-inject/L223.GlobalIntTechCapital_elec") ->
      L2233.GlobalIntTechCapital_elec


    L2233.GlobalTechCapital_elecPassthru %>%
      add_title("Capital costs for standard electricity generating tech") %>%
      add_units("TBC") %>%
      add_comments("") %>%
      add_legacy_name("L2233.GlobalTechCapital_elecPassthru") %>%
      add_precursors("water/elec_tech_water_map",
                     "energy/A23.globalinttech",
                     "temp-data-inject/L223.GlobalTechCapital_elec") ->
      L2233.GlobalTechCapital_elecPassthru

    L2233.GlobalIntTechOMfixed_elec %>%
      add_title("Operation and maintainance costs (fixed) for intermittent electricity generating tech") %>%
      add_units("TBC") %>%
      add_comments("") %>%
      add_legacy_name("L2233.GlobalIntTechOMfixed_elec") %>%
      add_precursors("water/elec_tech_water_map",
                     "energy/A23.globalinttech",
                     "temp-data-inject/L223.GlobalIntTechOMfixed_elec") ->
      L2233.GlobalIntTechOMfixed_elec

    L2233.GlobalTechOMfixed_elecPassthru %>%
      add_title("Operation and maintainance costs (fixed) for standard electricity generating tech") %>%
      add_units("TBC") %>%
      add_comments("") %>%
      add_legacy_name("L2233.GlobalTechOMfixed_elecPassthru") %>%
      add_precursors("water/elec_tech_water_map",
                     "energy/A23.globalinttech",
                     "temp-data-inject/L223.GlobalTechOMfixed_elec") ->
      L2233.GlobalTechOMfixed_elecPassthru

    L2233.GlobalIntTechOMvar_elec %>%
      add_title("Operation and maintainance costs (variable) for intermittent electricity generating tech") %>%
      add_units("TBC") %>%
      add_comments("") %>%
      add_legacy_name("L2233.GlobalIntTechOMvar_elec") %>%
      add_precursors("water/elec_tech_water_map",
                     "energy/A23.globalinttech",
                     "temp-data-inject/L223.GlobalIntTechOMvar_elec") ->
      L2233.GlobalIntTechOMvar_elec

    L2233.GlobalTechOMvar_elecPassthru %>%
      add_title("Operation and maintainance costs (variable) for standard electricity generating tech") %>%
      add_units("TBC") %>%
      add_comments("") %>%
      add_legacy_name("L2233.GlobalTechOMvar_elecPassthru") %>%
      add_precursors("water/elec_tech_water_map",
                     "energy/A23.globalinttech",
                     "temp-data-inject/L223.GlobalTechOMvar_elec") ->
      L2233.GlobalTechOMvar_elecPassthru


    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTech_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTech_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTechEff_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTechEff_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTechProd_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTechProd_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTechFixOut_hydro") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTechFixOut_hydro

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.StubTechShrwt_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTechShrwt_elec_cool

    L2233.GlobalTechCapital_elec_cool %>%
      add_title("Overnight capital, charge rates and capacity factors for standard cooling technologies") %>%
      add_units("1975USD") %>%
      add_comments("Capital costs converted from 2005USD") %>%
      add_comments("Fixed input assumptions for capacity factor and FCR") %>%
      add_legacy_name("L2233.GlobalTechCapital_elec_cool") %>%
      add_precursors("water/elec_tech_water_map",
                     "water/A23.CoolingSystemCosts") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalTechCapital_elec_cool

    L2233.GlobalIntTechCapital_elec_cool %>%
      add_title("Overnight capital, charge rates and capacity factors for standard cooling technologies") %>%
      add_units("1975USD") %>%
      add_comments("Capital costs converted from 2005USD") %>%
      add_comments("Fixed input assumptions for capacity factor and FCR") %>%
      add_legacy_name("L2233.GlobalIntTechCapital_elec_cool") %>%
      add_precursors("water/elec_tech_water_map",
                     "water/A23.CoolingSystemCosts") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechCapital_elec_cool

    L2233.GlobalTechCoef_elec_cool %>%
      add_title("Coefficients for water elec sector water withdrawal and consumption (standard techs)") %>%
      add_units("m^3/GJ") %>%
      add_comments("Water footprints mapped onto supplysector-subsector-technology tables") %>%
      add_comments("m^3/MWh converted to m^3/GJ") %>%
      add_legacy_name("L2233.GlobalTechCoef_elec_cool") %>%
      add_precursors("water/elec_tech_water_map",
                     "water/Macknick_elec_water_m3MWh") ->
      L2233.GlobalTechCoef_elec_cool

    L2233.GlobalIntTechCoef_elec_cool %>%
      add_title("Coefficients for water elec sector water withdrawal and consumption (intermittent techs)") %>%
      add_units("m^3/GJ") %>%
      add_comments("Water footprints mapped onto supplysector-subsector-technology tables") %>%
      add_comments("m^3/MWh converted to m^3/GJ") %>%
      add_legacy_name("L2233.GlobalTechCoef_elec_cool") %>%
      add_precursors("water/elec_tech_water_map",
                     "water/Macknick_elec_water_m3MWh") ->
      L2233.GlobalIntTechCoef_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.InputEmissCoeff_hist_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.InputEmissCoeff_hist_elecPassthru

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.InputEmissCoeff_fut_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.InputEmissCoeff_fut_elecPassthru

    return_data(L2233.EQUIV_TABLE,
                L2233.SectorNodeEquiv,
                L2233.TechNodeEquiv,
                L2233.StubTech_elecPassthru,
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
                L2233.PassThroughSector_elec_cool,
                `L2233.Supplysector_relative-cost-logit_elec_cool`,
                `L2233.Supplysector_absolute-cost-logit_elec_cool`,
                L2233.Supplysector_elec_cool,
                L2233.ElecReserve_elec_cool,
                L2233.SubsectorShrwtFllt_elec_cool,
                `L2233.SubsectorLogit_relative-cost-logit_elec_cool`,
                `L2233.SubsectorLogit_absolute-cost-logit_elec_cool`,
                L2233.SubsectorLogit_elec_cool,
                L2233.StubTech_elec_cool,
                L2233.StubTechEff_elec_cool,
                L2233.StubTechProd_elec_cool,
                L2233.StubTechFixOut_hydro,
                L2233.StubTechShrwt_elec_cool,
                L2233.GlobalTechCapital_elec_cool,
                L2233.GlobalIntTechCapital_elec_cool,
                L2233.GlobalTechCoef_elec_cool,
                L2233.GlobalIntTechCoef_elec_cool,
                L2233.InputEmissCoeff_hist_elecPassthru,
                L2233.InputEmissCoeff_fut_elecPassthru,
                L2233.AvgFossilEffKeyword_elec_cool,
                L2233.GlobalIntTechBackup_elec_cool,
                L2233.GlobalIntTechEff_elec_cool,
                L2233.GlobalIntTechLifetime_elec_cool,
                L2233.GlobalIntTechShrwt_elec_cool,
                L2233.GlobalTechCapture_elec_cool,
                L2233.GlobalTechEff_elec_cool,
                L2233.GlobalTechInterp_elec_cool,
                L2233.GlobalTechLifetime_elec_cool,
                L2233.GlobalTechProfitShutdown_elec_cool,
                L2233.GlobalTechSCurve_elec_cool,
                L2233.GlobalTechShrwt_elec_cool,
                L2233.PrimaryRenewKeyword_elec_cool,
                L2233.PrimaryRenewKeywordInt_elec_cool)
  } else {
    stop("Unknown command")
  }
}
