#' module_water_L2233.electricity_water
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{object}, \code{new_object}, \code{L2233.EQUIV_TABLE}, \code{L2233.SectorNodeEquiv}, \code{L2233.TechNodeEquiv}, \code{L2233.StubTech_elecPassthru}, \code{L2233.StubTechProd_elecPassthru}, \code{L2233.GlobalPassThroughTech}, \code{L2233.GlobalTechEff_elecPassthru}, \code{L2233.GlobalTechShrwt_elecPassthru}, \code{L2233.GlobalIntTechCapital_elec}, \code{L2233.GlobalTechCapital_elecPassthru}, \code{L2233.GlobalIntTechOMfixed_elec}, \code{L2233.GlobalTechOMfixed_elecPassthru}, \code{L2233.GlobalIntTechOMvar_elec}, \code{L2233.GlobalTechOMvar_elecPassthru}, \code{L2233.PassThroughSector_elec_cool}, \code{L2233.SectorLogitTables_elec_cool}, \code{L2233.Supplysector_elec_cool}, \code{L2233.ElecReserve_elec_cool}, \code{L2233.SubsectorShrwtFllt_elec_cool}, \code{L2233.SubsectorLogitTables_elec_cooling}, \code{L2233.SubsectorLogit_elec_cool}, \code{L2233.StubTech_elec_cool}, \code{L2233.StubTechEff_elec_cool}, \code{L2233.StubTechProd_elec_cool}, \code{L2233.StubTechFixOut_hydro}, \code{L2233.StubTechShrwt_elec_cool}, \code{L2233.GlobalTechCapital_elec_cool}, \code{L2233.GlobalIntTechCapital_elec_cool}, \code{L2233.GlobalTechCoef_elec_cool}, \code{L2233.GlobalIntTechCoef_elec_cool}, \code{L2233.InputEmissCoeff_hist_elecPassthru}, \code{L2233.InputEmissCoeff_fut_elecPassthru}. The corresponding file in the
#' original data system was \code{L2233.electricity_water.R} (water level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
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
    return(c("object",
             "new_object",             ## NEED TO ADD ALL THE OUTPUTS HERE!
             "L2233.EQUIV_TABLE",
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
             "L2233.SectorLogitTables_elec_cool",
             "L2233.Supplysector_elec_cool",
             "L2233.ElecReserve_elec_cool",
             "L2233.SubsectorShrwtFllt_elec_cool",
             "L2233.SubsectorLogitTables_elec_cooling",
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
             "L2233.InputEmissCoeff_fut_elecPassthru"))
  } else if(command == driver.MAKE) {

    curr_table <- NULL                  # silence package checks

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
      repeat_add_columns(tibble(year = MODEL_YEARS)) -> L2233.TechMapYr


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
      add_flags(FLAG_NO_XYEAR) ->
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

    #==========OUTPUT========================
    L2233.GlobalTechEffShrwt_elecPassthru %>%
      select(sector.name, subsector.name, technology) %>%
      add_title("Electricity sector, subsector, technology mapping") %>%
      add_units("NA") %>%
      add_comments("Generated by expansion of water/elec_tech_water_map for all years (plus filter and renaming of columns)") %>%
      add_legacy_name("L2233.GlobalPassThroughTech") %>%
      add_precursors("water/elec_tech_water_map") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L2233.GlobalPassThroughTech
    #========================================

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


    #==========OUTPUT========================
    L2233.GlobalTechEffShrwt_elecPassthru %>%
      select(sector.name, subsector.name, technology, year, minicam.energy.input, efficiency) %>%
      add_title("Input name and efficiency of pass-through technologies in the electricity sector") %>%
      add_units("Unitless") %>%
      add_comments("") %>%
      add_legacy_name("L2233.GlobalTechEff_elecPassthru") %>%
      add_precursors("energy/A23.globaltech_shrwt", "water/elec_tech_water_map") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechEff_elecPassthru
    #========================================


    #==========OUTPUT========================
    L2233.GlobalTechEffShrwt_elecPassthru %>%
      select(sector.name, subsector.name, technology, year, share.weight) %>%
      add_title("Share-weights of pass-through technologies in the electricity sector") %>%
      add_units("units") %>%
      add_comments("") %>%
      add_legacy_name("L2233.GlobalTechShrwt_elecPassthru") %>%
      add_precursors("energy/A23.globaltech_shrwt", "water/elec_tech_water_map")%>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechShrwt_elecPassthru
    #========================================

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
      StubTechProd_elecPassthru_


    #==========OUTPUT========================
    StubTechProd_elecPassthru_ %>%
      add_title("Calibrated electricity flow through the pass-through technologies") %>%
      add_units("EJ") %>%
      add_comments("") %>%
      add_legacy_name("L2233.StubTechProd_elecPassthru") %>%
      add_precursors("energy/calibrated_techs",
                     "L1231.out_EJ_R_elec_F_tech_Yh",
                     "common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.StubTechProd_elecPassthru
    #========================================


    ## PART 3: ... (start with test for chunks completed)















    # ===================================================

    # Produce outputs
    # OBVIOUSLY THIS (BELOW) NEEDS TO BE CHANGED
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("object") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      object

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("new_object") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      new_object

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

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalIntTechCapital_elec") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechCapital_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalTechCapital_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechCapital_elecPassthru

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalIntTechOMfixed_elec") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechOMfixed_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalTechOMfixed_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechOMfixed_elecPassthru

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalIntTechOMvar_elec") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechOMvar_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalTechOMvar_elecPassthru") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechOMvar_elecPassthru

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.PassThroughSector_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.PassThroughSector_elec_cool

    # OBVIOUSLY THIS (BELOW) NEEDS TO BE CHANGED
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.SectorLogitTables_elec_cool[[ curr_table ]]$data") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.SectorLogitTables_elec_cool#[[ curr_table ]]$data

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.Supplysector_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.Supplysector_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.ElecReserve_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.ElecReserve_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.SubsectorShrwtFllt_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.SubsectorShrwtFllt_elec_cool

    # OBVIOUSLY THIS (BELOW) NEEDS TO BE CHANGED
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.SubsectorLogitTables_elec_cooling[[ curr_table ]]$data") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.SubsectorLogitTables_elec_cooling#[[ curr_table ]]$data

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.SubsectorLogit_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.SubsectorLogit_elec_cool

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

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalTechCapital_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechCapital_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalIntTechCapital_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalIntTechCapital_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalTechCoef_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2233.GlobalTechCoef_elec_cool

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2233.GlobalIntTechCoef_elec_cool") %>%
      add_precursors("common/GCAM_region_names") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
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

    return_data(object,
                new_object,
                L2233.EQUIV_TABLE,
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
                L2233.SectorLogitTables_elec_cool, #[[ curr_table ]]$data,
                L2233.Supplysector_elec_cool,
                L2233.ElecReserve_elec_cool,
                L2233.SubsectorShrwtFllt_elec_cool,
                L2233.SubsectorLogitTables_elec_cooling,#[[ curr_table ]]$data,
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
                L2233.InputEmissCoeff_fut_elecPassthru)
  } else {
    stop("Unknown command")
  }
}
