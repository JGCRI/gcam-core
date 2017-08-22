#' module_energy_L242.building_agg
#'
#' Calculate supply sector, subsector, and technology information for the building sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L242.Supplysector_bld}, \code{L242.FinalEnergyKeyword_bld}, \code{L242.SubsectorLogit_bld}, \code{L242.SubsectorShrwtFllt_bld}, \code{L242.SubsectorInterp_bld}, \code{L242.StubTech_bld}, \code{L242.GlobalTechInterp_bld}, \code{L242.GlobalTechShrwt_bld}, \code{L242.GlobalTechEff_bld}, \code{L242.GlobalTechCost_bld}, \code{L242.StubTechCalInput_bld}, \code{L242.FuelPrefElast_bld}, \code{L242.PerCapitaBased_bld}, \code{L242.PriceElasticity_bld}, \code{L242.BaseService_bld}. The corresponding file in the
#' original data system was \code{L242.building_agg.R} (energy level2).
#' @details Calculate shareweights, cost, price elasticity, calibrated, and other data for the building sector
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AJS August 2017
module_energy_L242.building_agg <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs_bld_agg",
             FILE = "energy/A_regions",
             FILE = "energy/A42.sector",
             FILE = "energy/A42.subsector_interp",
             FILE = "energy/A42.subsector_logit",
             FILE = "energy/A42.subsector_shrwt",
             FILE = "energy/A42.globaltech_cost",
             FILE = "energy/A42.globaltech_eff",
             FILE = "energy/A42.globaltech_shrwt",
             FILE = "energy/A42.globaltech_interp",
             FILE = "energy/A42.fuelprefElasticity",
             FILE = "energy/A42.demand",
             "L142.in_EJ_R_bld_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L242.Supplysector_bld",
             "L242.FinalEnergyKeyword_bld",
             "L242.SubsectorLogit_bld",
             "L242.SubsectorShrwtFllt_bld",
             "L242.SubsectorInterp_bld",
             "L242.StubTech_bld",
             "L242.GlobalTechInterp_bld",
             "L242.GlobalTechShrwt_bld",
             "L242.GlobalTechEff_bld",
             "L242.GlobalTechCost_bld",
             "L242.StubTechCalInput_bld",
             "L242.FuelPrefElast_bld",
             "L242.PerCapitaBased_bld",
             "L242.PriceElasticity_bld",
             "L242.BaseService_bld"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs_bld_agg <- get_data(all_data, "energy/calibrated_techs_bld_agg")
    A_regions <- get_data(all_data, "energy/A_regions")
    A42.sector <- get_data(all_data, "energy/A42.sector")
    A42.subsector_interp <- get_data(all_data, "energy/A42.subsector_interp")
    A42.subsector_logit <- get_data(all_data, "energy/A42.subsector_logit")
    A42.subsector_shrwt <- get_data(all_data, "energy/A42.subsector_shrwt")
    A42.globaltech_cost <- get_data(all_data, "energy/A42.globaltech_cost")
    A42.globaltech_eff <- get_data(all_data, "energy/A42.globaltech_eff")
    A42.globaltech_shrwt <- get_data(all_data, "energy/A42.globaltech_shrwt")
    A42.globaltech_interp <- get_data(all_data, "energy/A42.globaltech_interp")
    A42.fuelprefElasticity <- get_data(all_data, "energy/A42.fuelprefElasticity")
    A42.demand <- get_data(all_data, "energy/A42.demand")
    L142.in_EJ_R_bld_F_Yh <- get_data(all_data, "L142.in_EJ_R_bld_F_Yh")

    # ===================================================

    # Silence package notes
    . <- GCAM_region_ID <- MgdFor_adj <- base.service <- technology <- to.value <-
    calibrated.value <- coefficient <- curr_table <- efficiency <- fuel <-
    has_district_heat <- input.cost <- logit.exponent <- minicam.energy.input <-
    minicam.non.energy.input <- output <- region <- region_subsector <- sector <-
    share.weight <- share.weight.year <- stub.technology <- subsector <- supplysector <-
    tradbio_region <- year <- year.fillout <- value <- NULL

    # PART A: PREP
    # Build tables to drop subsectors and technologies in regions where heat and traditional biomass are not
    # modeled as separate fuels
    A_regions %>%
      # 0 indicates district heat is not modeled
      filter(has_district_heat == 0) %>%
      select(GCAM_region_ID, region) ->
      regions_NoDistHeat

    A_regions %>%
      # 0 indicates traditional biomass is not modeled
      filter(tradbio_region == 0) %>%
      select(GCAM_region_ID, region) ->
      regions_NoTradBio

    # calibrated_techs_bld_agg reports mapping between intermediate sectors and fuels to the techs in aggregate buildings
    calibrated_techs_bld_agg %>%
      # Subset for buildings and heat
      filter(grepl("bld", sector) & fuel == "heat") %>%
      select(supplysector, subsector, technology) %>%
      unique() %>%
      repeat_add_columns(regions_NoDistHeat) ->
      L242.rm_heat_techs_R

    calibrated_techs_bld_agg %>%
      # Subset for buildings and traditional biomass
      filter(grepl("bld", sector) & fuel == "traditional biomass") %>%
      select(supplysector, subsector, technology) %>%
      unique() %>%
      repeat_add_columns(regions_NoTradBio) %>%
      # Combine with heat table
      bind_rows(L242.rm_heat_techs_R) %>%
      mutate(region_subsector = paste(region, subsector)) %>%
      .[["region_subsector"]] %>%
      unique() ->
      L242.rm_techs_R
      # Above list to be used to drop subsectors and technologies in regions where heat and traditional biomass are not
      # modeled as separate fuels


    # PART B: SUPPLYSECTOR INFORMATION
    # Expand supply sector information for building sector across all regions
    A42.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           GCAM_region_names = GCAM_region_names) ->
      L242.Supplysector_bld # OUTPUT

    # Expand supply sector keywords for building sector across all regions
    A42.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]],
                           GCAM_region_names = GCAM_region_names) ->
      L242.FinalEnergyKeyword_bld # OUTPUT

    # PART C: SUBSECTOR INFORMATION
    # Expand subsector logit exponents of building sector across all regions and remove
    # region/fuel combinations where heat and traditional biomass are not modeled as separate fuels.
    A42.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "logit.type"),
                           GCAM_region_names = GCAM_region_names) %>%
      mutate(region_subsector = paste(region, subsector)) %>%
      filter(!region_subsector %in% L242.rm_techs_R) %>%
      select(-region_subsector) ->
      L242.SubsectorLogit_bld # OUTPUT

    # Expand subsector shareweights of building sector across all regions and remove
    # region/fuel combinations where heat and traditional biomass are not modeled as separate fuels.
    A42.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]],
                           GCAM_region_names = GCAM_region_names) %>%
      # Remove non-existent heat subsectors from each region
      mutate(region_subsector = paste(region, subsector)) %>%
      filter(!region_subsector %in% L242.rm_techs_R) %>%
      select(one_of(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])) ->
      L242.SubsectorShrwtFllt_bld # OUTPUT

    # Expand subsector shareweight interpolation of building sector across all regions and remove
    # region/fuel combinations where heat and traditional biomass are not modeled as separate fuels.
    A42.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]],
                           GCAM_region_names = GCAM_region_names) %>%
      # Remove non-existent heat subsectors from each region
      mutate(region_subsector = paste(region, subsector)) %>%
      filter(!region_subsector %in% L242.rm_techs_R) %>%
      select(one_of(LEVEL2_DATA_NAMES[["SubsectorInterp"]])) ->
      L242.SubsectorInterp_bld # OUTPUT

    # PART D: TECHNOLOGY INFORMATION
    # Expand stub technologies of building sector across all regions and remove
    # region/fuel combinations where heat and traditional biomass are not modeled as separate fuels.
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A42.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]],
                           GCAM_region_names = GCAM_region_names) %>%
      # Drop heat as a stub-technology in regions where it is not modeled as a fuel
      mutate(region_subsector = paste(region, technology)) %>%
      filter(!region_subsector %in% L242.rm_techs_R) %>%
      select(region, supplysector, subsector, stub.technology = technology) ->
      L242.StubTech_bld # OUTPUT

    # Shareweights of global building sector technologies
    A42.globaltech_shrwt %>%
      gather(year, value, matches(YEAR_PATTERN)) %>% # Convert to long form
      mutate(year = as.integer(year)) %>% # Year needs to be integer or numeric to interpolate
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      select(sector.name = supplysector, subsector.name = subsector, technology, year, share.weight) ->
      L242.GlobalTechShrwt_bld # OUTPUT

    # Global technology shareweight interpolation of building sector
    A42.globaltech_interp %>%
      mutate(from.year = min(MODEL_YEARS),
             to.year = max(MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTechInterp"]])) -> # Drops to.value
      L242.GlobalTechInterp_bld # OUTPUT

    # Energy inputs and coefficients of global building energy use and feedstocks technologies
    DIGITS_EFFICIENCY = 3

    A42.globaltech_eff %>%
      gather(year, efficiency, matches(YEAR_PATTERN)) %>% # Convert to long form
      mutate(year = as.integer(year)) %>% # Year needs to be integer or numeric to interpolate
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.energy.input)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(efficiency = approx_fun(year, efficiency, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      mutate(efficiency = round(efficiency, digits = DIGITS_EFFICIENCY)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, minicam.energy.input, year, efficiency) ->
      L242.GlobalTechEff_bld # OUTPUT

    # Costs of global technologies
    # Capital costs of global building technologies
    DIGITS_COST <- 4

    A42.globaltech_cost %>%
      gather(year, input.cost, matches(YEAR_PATTERN)) %>% # Convert to long form
      mutate(year = as.integer(year)) %>% # Year needs to be integer or numeric to interpolate
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.non.energy.input)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(input.cost = approx_fun(year, input.cost, rule = 2),
             input.cost = round(input.cost, digits = DIGITS_COST)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, minicam.non.energy.input, year, input.cost) ->
      L242.GlobalTechCost_bld # OUTPUT

    # Calibration and region-specific data
    # Calibrated input of building energy use technologies (including cogen)
    L142.in_EJ_R_bld_F_Yh %>%
      # Subset table for model base years
      filter(year %in% BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Match in supplysector, subsector, technology
      left_join_error_no_match(calibrated_techs_bld_agg, by = c("sector", "fuel")) %>%
      # Aggregate as indicated in the supplysector/subsector/technology mapping (dropping fuel)
      group_by(region, supplysector, subsector, stub.technology = technology, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L242.in_EJ_R_bld_F_Yh

    DIGITS_CALOUTPUT <- 7

    L242.in_EJ_R_bld_F_Yh %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["StubTechYr"]], "value"))) %>%
      left_join_error_no_match(A42.globaltech_eff, by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      mutate(value = round(value, digits = DIGITS_CALOUTPUT),
             share.weight.year = year) %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input, share.weight.year, year) %>%
      summarise(calibrated.value = sum(value)) %>%
      ungroup() %>%
      mutate(subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechCalInput"]])) ->
      L242.StubTechCalInput_bld # OUTPUT

    # Fuel preference elasticities of building energy use
    FuelPrefElasticity <- c("region", "supplysector", "subsector", "year.fillout", "fuelprefElasticity")

    A42.fuelprefElasticity %>%
      mutate(year.fillout = min(FUTURE_YEARS)) %>%
      write_to_all_regions(FuelPrefElasticity,
                           GCAM_region_names = GCAM_region_names) %>%
      mutate(region_subsector = paste(region, subsector)) %>%
      filter(!region_subsector %in% L242.rm_techs_R) %>%
      select(one_of(FuelPrefElasticity)) ->
      L242.FuelPrefElast_bld # OUTPUT

    # Expand per-capita based flag for building final demand across all regions
    A42.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]],
                           GCAM_region_names = GCAM_region_names) ->
      L242.PerCapitaBased_bld # OUTPUT

    # Expand price elasticity of building final demand across all regions and future years.
    # Price elasticities are only applied to future periods. Application in base years will cause solution failure.
    A42.demand %>%
      repeat_add_columns(tibble(FUTURE_YEARS)) %>%
      rename(year = FUTURE_YEARS) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]],
                           GCAM_region_names = GCAM_region_names) ->
      L242.PriceElasticity_bld # OUTPUT

    # Base-year service output of building final demand
    # Base service is equal to the output of the building supplysector
    L242.StubTechCalInput_bld %>%
      left_join_error_no_match(L242.GlobalTechEff_bld, by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                                              "stub.technology" = "technology", "minicam.energy.input",
                                                              "share.weight.year" = "year")) %>%
      mutate(output = calibrated.value * efficiency) %>%
      group_by(region, supplysector, year) %>%
      summarise(output = sum(output)) %>%
      ungroup() %>%
      mutate(base.service = round(output, digits = DIGITS_CALOUTPUT)) %>%
      select(region, energy.final.demand = supplysector, year, base.service) ->
      L242.BaseService_bld # OUTPUT

    ## NOTE: income elasticities are GDP-dependent and are set in the socioeconomics module

    # ===================================================

    L242.Supplysector_bld %>%
      add_title("Supply sector information for building sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector information for building sector was written for all regions") %>%
      add_legacy_name("L242.Supplysector_bld") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.sector") ->
      L242.Supplysector_bld

    L242.FinalEnergyKeyword_bld %>%
      add_title("Supply sector keywords for building sector") %>%
      add_units("NA") %>%
      add_comments("Supply sector keywords for building sector was written for all regions") %>%
      add_legacy_name("L242.FinalEnergyKeyword_bld") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.sector") ->
      L242.FinalEnergyKeyword_bld

    L242.SubsectorLogit_bld %>%
      add_title("Subsector logit exponents of building sector") %>%
      add_units("Unitless") %>%
      add_comments("Subsector logit exponents of building sector were written for all regions") %>%
      add_comments("Region/fuel combinations where heat and traditional biomass are not modeled as separate fuels were removed") %>%
      add_legacy_name("L242.SubsectorLogit_bld") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs_bld_agg",
                     "energy/A_regions", "energy/A42.subsector_logit") ->
      L242.SubsectorLogit_bld

    L242.SubsectorShrwtFllt_bld %>%
      add_title("Subsector shareweights of building sector") %>%
      add_units("Unitless") %>%
      add_comments("Subsector shareweights of building sector were written for all regions") %>%
      add_comments("Region/fuel combinations where heat and traditional biomass are not modeled as separate fuels were removed") %>%
      add_legacy_name("L242.SubsectorShrwtFllt_bld") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs_bld_agg",
                     "energy/A_regions", "energy/A42.subsector_shrwt") ->
      L242.SubsectorShrwtFllt_bld

    L242.SubsectorInterp_bld %>%
      add_title("Subsector shareweight interpolation data of building sector") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweight interpolation data of building sector were written for all regions") %>%
      add_comments("Region/fuel combinations where heat and traditional biomass are not modeled as separate fuels were removed") %>%
      add_legacy_name("L242.SubsectorInterp_bld") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs_bld_agg",
                     "energy/A_regions", "energy/A42.subsector_interp") ->
      L242.SubsectorInterp_bld

    L242.StubTech_bld %>%
      add_title("Identification of stub technologies of building sector") %>%
      add_units("NA") %>%
      add_comments("Identification of stub technologies of building sector were written for all regions") %>%
      add_comments("Region/fuel combinations where heat and traditional biomass are not modeled as separate fuels were removed") %>%
      add_legacy_name("L242.StubTech_bld") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs_bld_agg",
                     "energy/A_regions", "energy/A42.globaltech_shrwt") ->
      L242.StubTech_bld

    L242.GlobalTechInterp_bld %>%
      add_title("Global technology shareweight interpolation of building sector") %>%
      add_units("NA") %>%
      add_comments("From/to years set to min/max of model years") %>%
      add_legacy_name("L242.GlobalTechInterp_bld") %>%
      add_precursors("energy/A42.globaltech_interp") ->
      L242.GlobalTechInterp_bld

    L242.GlobalTechShrwt_bld %>%
      add_title("Shareweights of global building sector technologies") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated shareweight data across model years") %>%
      add_legacy_name("L242.GlobalTechShrwt_bld") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs_bld_agg",
                     "energy/A_regions", "energy/A42.globaltech_shrwt") ->
      L242.GlobalTechShrwt_bld

    L242.GlobalTechEff_bld %>%
      add_title("Energy inputs and coefficients of global building energy use and feedstocks technologies") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated data across model years") %>%
      add_legacy_name("L242.GlobalTechEff_bld") %>%
      add_precursors("energy/A42.globaltech_eff") ->
      L242.GlobalTechEff_bld

    L242.GlobalTechCost_bld %>%
      add_title("Capital costs of global building technologies") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated data across model years") %>%
      add_legacy_name("L242.GlobalTechCost_bld") %>%
      add_precursors("energy/A42.globaltech_cost") ->
      L242.GlobalTechCost_bld

    L242.StubTechCalInput_bld %>%
      add_title("Calibrated input of building energy use technologies (including cogen)") %>%
      add_units("EJ") %>%
      add_comments("Data were aggregated (dropping fuel) and shareweights were determined from the calibrated value") %>%
      add_legacy_name("L242.StubTechCalInput_bld") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs_bld_agg",
                     "energy/A42.globaltech_eff", "L142.in_EJ_R_bld_F_Yh") ->
      L242.StubTechCalInput_bld

    L242.FuelPrefElast_bld %>%
      add_title("Fuel preference elasticities of building energy use") %>%
      add_units("Unitless") %>%
      add_comments("Data were written for all regions") %>%
      add_comments("Region/fuel combinations where heat and traditional biomass are not modeled as separate fuels were removed") %>%
      add_legacy_name("L242.FuelPrefElast_bld") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs_bld_agg",
                     "energy/A_regions", "energy/A42.fuelprefElasticity") ->
      L242.FuelPrefElast_bld

    L242.PerCapitaBased_bld %>%
      add_title("Per-capita based flag for building final demand") %>%
      add_units("Unitless") %>%
      add_comments("Data were written for all regions") %>%
      add_legacy_name("L242.PerCapitaBased_bld") %>%
      add_precursors("energy/A42.demand") ->
      L242.PerCapitaBased_bld

    L242.PriceElasticity_bld %>%
      add_title("Price elasticity of building final demand") %>%
      add_units("Unitless") %>%
      add_comments("Price elasticity data were written for all regions and only applied to future model years") %>%
      add_legacy_name("L242.PriceElasticity_bld") %>%
      add_precursors("energy/A42.demand") ->
      L242.PriceElasticity_bld

    L242.BaseService_bld %>%
      add_title("Base-year service output of building final demand") %>%
      add_units("EJ") %>%
      add_comments("Base service is equal to the output of the building supplysector") %>%
      add_legacy_name("L242.BaseService_bld") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs_bld_agg", "energy/A42.globaltech_eff",
                     "L142.in_EJ_R_bld_F_Yh", "energy/A42.globaltech_eff") ->
      L242.BaseService_bld

    return_data(L242.Supplysector_bld, L242.FinalEnergyKeyword_bld, L242.SubsectorLogit_bld, L242.SubsectorShrwtFllt_bld, L242.SubsectorInterp_bld, L242.StubTech_bld, L242.GlobalTechInterp_bld, L242.GlobalTechShrwt_bld, L242.GlobalTechEff_bld, L242.GlobalTechCost_bld, L242.StubTechCalInput_bld, L242.FuelPrefElast_bld, L242.PerCapitaBased_bld, L242.PriceElasticity_bld, L242.BaseService_bld)
  } else {
    stop("Unknown command")
  }
}
