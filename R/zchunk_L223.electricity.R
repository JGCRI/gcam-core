#' module_energy_L223.electricity
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L223.Supplysector_elec}, \code{L223.ElecReserve}, \code{L223.SubsectorLogit_elec},
#' \code{L223.SubsectorShrwt_elec}, \code{L223.SubsectorShrwtFllt_elec}, \code{L223.SubsectorShrwt_nuc},
#' \code{L223.SubsectorShrwt_renew}, \code{L223.SubsectorInterp_elec}, \code{L223.SubsectorInterpTo_elec},
#' \code{L223.StubTech_elec}, \code{L223.GlobalIntTechEff_elec}, \code{L223.GlobalTechEff_elec},
#' \code{L223.GlobalTechCapital_elec}, \code{L223.GlobalIntTechCapital_elec}, \code{L223.GlobalTechOMfixed_elec},
#' \code{L223.GlobalIntTechOMfixed_elec}, \code{L223.GlobalTechOMvar_elec}, \code{L223.GlobalIntTechOMvar_elec},
#' \code{L223.GlobalTechShrwt_elec}, \code{L223.GlobalTechInterp_elec}, \code{L223.GlobalIntTechShrwt_elec},
#' \code{L223.PrimaryRenewKeyword_elec}, \code{L223.PrimaryRenewKeywordInt_elec}, \code{L223.AvgFossilEffKeyword_elec},
#' \code{L223.GlobalTechCapture_elec}, \code{L223.GlobalIntTechBackup_elec}, \code{L223.StubTechCapFactor_elec},
#' \code{L223.GlobalTechShutdown_elec}, \code{L223.GlobalIntTechShutdown_elec}, \code{L223.GlobalTechSCurve_elec},
#' \code{L223.GlobalIntTechSCurve_elec}, \code{L223.GlobalTechLifetime_elec}, \code{L223.GlobalIntTechLifetime_elec},
#' \code{L223.GlobalTechProfitShutdown_elec}, \code{L223.GlobalIntTechProfitShutdown_elec},
#' \code{L223.StubTechCalInput_elec}, \code{L223.StubTechFixOut_elec}, \code{L223.StubTechFixOut_hydro},
#' \code{L223.StubTechProd_elec}, \code{L223.StubTechEff_elec}, \code{L223.GlobalTechCapital_sol_adv},
#' \code{L223.GlobalIntTechCapital_sol_adv}, \code{L223.GlobalTechCapital_wind_adv},
#' \code{L223.GlobalIntTechCapital_wind_adv}, \code{L223.GlobalTechCapital_geo_adv},
#' \code{L223.GlobalTechCapital_nuc_adv}, \code{L223.GlobalTechCapital_sol_low},
#' \code{L223.GlobalIntTechCapital_sol_low}, \code{L223.GlobalTechCapital_wind_low},
#' \code{L223.GlobalIntTechCapital_wind_low}, \code{L223.GlobalTechCapital_geo_low},
#' \code{L223.GlobalTechCapital_nuc_low}, \code{L223.GlobalTechCapital_bio_low}. The corresponding file in the
#' original data system was \code{L223.electricity.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL July 2017
#' @export
module_energy_L223.electricity <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "energy/fuel_energy_input",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A23.sector",
             FILE = "energy/A23.subsector_logit",
             FILE = "energy/A23.subsector_shrwt",
             FILE = "energy/A23.subsector_interp",
             FILE = "energy/A23.subsector_interp_R",
             FILE = "energy/A23.subsector_shrwt_nuc_R",
             FILE = "energy/A23.subsector_shrwt_renew_R",
             FILE = "energy/A23.globalinttech",
             FILE = "energy/A23.globaltech_shrwt",
             FILE = "energy/A23.globaltech_interp",
             FILE = "energy/A23.globaltech_keyword",
             FILE = "energy/A23.globaltech_eff",
             FILE = "energy/A23.globaltech_capital",
             FILE = "energy/A23.globaltech_capital_adv",
             FILE = "energy/A23.globaltech_capital_low",
             FILE = "energy/A23.globaltech_OMfixed",
             FILE = "energy/A23.globaltech_OMvar",
             FILE = "energy/A23.globaltech_retirement",
             FILE = "energy/A23.globaltech_co2capture",
             "L114.RsrcCurves_EJ_R_wind",
             "L118.out_EJ_R_elec_hydro_Yfut",
             FILE = "temp-data-inject/L119.Irradiance_rel_R",
             "L1231.in_EJ_R_elec_F_tech_Yh",
             "L1231.out_EJ_R_elec_F_tech_Yh",
             "L1231.eff_R_elec_F_tech_Yh",
             "L102.gdp_mil90usd_GCAM3_ctry_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L223.Supplysector_elec",
             "L223.ElecReserve",
             "L223.SubsectorLogit_elec",
             "L223.SubsectorShrwt_elec",
             "L223.SubsectorShrwtFllt_elec",
             "L223.SubsectorShrwt_nuc",
             "L223.SubsectorShrwt_renew",
             "L223.SubsectorInterp_elec",
             "L223.SubsectorInterpTo_elec",
             "L223.StubTech_elec",
             "L223.GlobalIntTechEff_elec",
             "L223.GlobalTechEff_elec",
             "L223.GlobalTechCapital_elec",
             "L223.GlobalIntTechCapital_elec",
             "L223.GlobalTechOMfixed_elec",
             "L223.GlobalIntTechOMfixed_elec",
             "L223.GlobalTechOMvar_elec",
             "L223.GlobalIntTechOMvar_elec",
             "L223.GlobalTechShrwt_elec",
             "L223.GlobalTechInterp_elec",
             "L223.GlobalIntTechShrwt_elec",
             "L223.PrimaryRenewKeyword_elec",
             "L223.PrimaryRenewKeywordInt_elec",
             "L223.AvgFossilEffKeyword_elec",
             "L223.GlobalTechCapture_elec",
             "L223.GlobalIntTechBackup_elec",
             "L223.StubTechCapFactor_elec",
             "L223.GlobalTechShutdown_elec",
             "L223.GlobalIntTechShutdown_elec",
             "L223.GlobalTechSCurve_elec",
             "L223.GlobalIntTechSCurve_elec",
             "L223.GlobalTechLifetime_elec",
             "L223.GlobalIntTechLifetime_elec",
             "L223.GlobalTechProfitShutdown_elec",
             "L223.GlobalIntTechProfitShutdown_elec",
             "L223.StubTechCalInput_elec",
             "L223.StubTechFixOut_elec",
             "L223.StubTechFixOut_hydro",
             "L223.StubTechProd_elec",
             "L223.StubTechEff_elec",
             "L223.GlobalTechCapital_sol_adv",
             "L223.GlobalIntTechCapital_sol_adv",
             "L223.GlobalTechCapital_wind_adv",
             "L223.GlobalIntTechCapital_wind_adv",
             "L223.GlobalTechCapital_geo_adv",
             "L223.GlobalTechCapital_nuc_adv",
             "L223.GlobalTechCapital_sol_low",
             "L223.GlobalIntTechCapital_sol_low",
             "L223.GlobalTechCapital_wind_low",
             "L223.GlobalIntTechCapital_wind_low",
             "L223.GlobalTechCapital_geo_low",
             "L223.GlobalTechCapital_nuc_low",
             "L223.GlobalTechCapital_bio_low"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    fuel_energy_input <- get_data(all_data, "energy/fuel_energy_input")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A23.sector <- get_data(all_data, "energy/A23.sector")
    A23.subsector_logit <- get_data(all_data, "energy/A23.subsector_logit")
    A23.subsector_shrwt <- get_data(all_data, "energy/A23.subsector_shrwt")
    A23.subsector_interp <- get_data(all_data, "energy/A23.subsector_interp")
    A23.subsector_interp_R <- get_data(all_data, "energy/A23.subsector_interp_R")
    A23.subsector_shrwt_nuc_R <- get_data(all_data, "energy/A23.subsector_shrwt_nuc_R")
    A23.subsector_shrwt_renew_R <- get_data(all_data, "energy/A23.subsector_shrwt_renew_R")
    A23.globalinttech <- get_data(all_data, "energy/A23.globalinttech")
    A23.globaltech_shrwt <- get_data(all_data, "energy/A23.globaltech_shrwt")
    A23.globaltech_interp <- get_data(all_data, "energy/A23.globaltech_interp")
    A23.globaltech_keyword <- get_data(all_data, "energy/A23.globaltech_keyword")
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    A23.globaltech_capital <- get_data(all_data, "energy/A23.globaltech_capital")
    A23.globaltech_capital_adv <- get_data(all_data, "energy/A23.globaltech_capital_adv")
    A23.globaltech_capital_low <- get_data(all_data, "energy/A23.globaltech_capital_low")
    A23.globaltech_OMfixed <- get_data(all_data, "energy/A23.globaltech_OMfixed")
    A23.globaltech_OMvar <- get_data(all_data, "energy/A23.globaltech_OMvar")
    A23.globaltech_retirement <- get_data(all_data, "energy/A23.globaltech_retirement")
    A23.globaltech_co2capture <- get_data(all_data, "energy/A23.globaltech_co2capture")
    L114.RsrcCurves_EJ_R_wind <- get_data(all_data, "L114.RsrcCurves_EJ_R_wind")
    L118.out_EJ_R_elec_hydro_Yfut <- get_data(all_data, "L118.out_EJ_R_elec_hydro_Yfut")
    L119.Irradiance_rel_R <- get_data(all_data, "temp-data-inject/L119.Irradiance_rel_R")
    L1231.in_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.in_EJ_R_elec_F_tech_Yh")
    L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh")
    L1231.eff_R_elec_F_tech_Yh <- get_data(all_data, "L1231.eff_R_elec_F_tech_Yh")
    L102.gdp_mil90usd_GCAM3_ctry_Y <- get_data(all_data, "L102.gdp_mil90usd_GCAM3_ctry_Y")

    browser()
    # Supplysector information (original file lines 61-68)
    # L223.Supplysector_elec: Supply sector information for electricity sector
    # L223.SectorLogitTables <- get_logit_fn_tables( A23.sector, names_SupplysectorLogitType,
    #                                                base.header="Supplysector_", include.equiv.table=T, write.all.regions=T )
    L223.Supplysector_elec <- write_to_all_regions(A23.sector, LEVEL2_DATA_NAMES[["Supplysector"]], GCAM_region_names)

    # L223.ElecReserve: Electricity reserve margin and average grid capacity factor
    L223.ElecReserve <- write_to_all_regions(A23.sector, LEVEL2_DATA_NAMES[["ElecReserve"]], GCAM_region_names)

    # L223.SubsectorLogit_elec: Subsector logit exponents of electricity sector (71-74)
    # L223.SubsectorLogitTables <- get_logit_fn_tables( A23.subsector_logit, names_SubsectorLogitType,
    #                                                   base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=T )
    L223.SubsectorLogit_elec <- write_to_all_regions(A23.subsector_logit, LEVEL2_DATA_NAMES[["SubsectorLogit"]], GCAM_region_names)

    # L223.SubsectorShrwt_elec and L223.SubsectorShrwtFllt_elec: Subsector shareweights of electricity sector (76-82)
    #L223.SubsectorShrwt_elec <- write_to_all_regions(filter(A23.subsector_shrwt, !is.na(year)), names_SubsectorShrwt, GCAM_region_names)
    L223.SubsectorShrwtFllt_elec <- write_to_all_regions(filter(A23.subsector_shrwt, !is.na(year.fillout)), LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names)

    # L223.SubsectorShrwt_nuc: Subsector shareweights of nuclear electricity (84-117)
    # Start out with the list of ISO matched to region_GCAM3
    iso_GCAM_regID %>%
      select(iso, region_GCAM3, GCAM_region_ID) %>%
      left_join_error_no_match(select(A23.subsector_shrwt_nuc_R, region_GCAM3, matches(YEAR_PATTERN)),
                               by = "region_GCAM3") ->
      L223.SubsectorShrwt_nuc_ctry

    # Where country-level shareweights are provided, use those
    L223.SubsectorShrwt_nuc_ctry %>%
      filter(iso %in% A23.subsector_shrwt_nuc_R$iso) %>%
      select(-matches(YEAR_PATTERN)) %>%
      left_join_error_no_match(select(A23.subsector_shrwt_nuc_R, iso, matches(YEAR_PATTERN)),
                               by = "iso") %>%
      bind_rows(filter(L223.SubsectorShrwt_nuc_ctry, !iso %in% A23.subsector_shrwt_nuc_R$iso)) %>%

      # Use GDP by country as a weighting factor in going from country-level shareweights to region-level shareweights
      left_join(L102.gdp_mil90usd_GCAM3_ctry_Y, by = "iso") %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      na.omit %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = weighted.mean(value, weight)) %>%
      # Interpolate to model time periods, and add columns specifying the final format
      complete(GCAM_region_ID, year = FUTURE_YEARS[FUTURE_YEARS >= min(year) & FUTURE_YEARS <= max(year)]) %>%
      arrange(GCAM_region_ID, year) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      mutate(supplysector = A23.subsector_shrwt_nuc_R$supplysector[1],
             subsector = A23.subsector_shrwt_nuc_R$subsector[1]) %>%
      # L223.SubsectorShrwt_nuc[ c( supp, subs ) ] <- A23.subsector_shrwt_nuc_R[ 1, c( supp, subs ) ]
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->   # TODO not sure this is right
      L223.SubsectorShrwt_nuc_R

    # L223.SubsectorShrwt_renew: Near term subsector shareweights of renewable technologies (119-138)
    # First, melt the table with near-term shareweights from GCAM 3.0 regions
    A23.subsector_shrwt_renew_R %>%
      gather(year, share.weight, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) ->
      L223.SubsectorShrwt_renew_GCAM3

    # Build a table with all combinations of GCAM regions, electricity technologies, and years
    expand.grid(GCAM_region_ID = GCAM_region_names$GCAM_region_ID,
                supplysector = unique(L223.SubsectorShrwt_renew_GCAM3$supplysector),
                subsector = unique(L223.SubsectorShrwt_renew_GCAM3$subsector),
                year = unique(L223.SubsectorShrwt_renew_GCAM3$year),
                stringsAsFactors = FALSE) %>%
      as_tibble %>%
      # use an approximate match between current regions and GCAM 3.0 regions
      left_join_keep_first_only(select(iso_GCAM_regID, GCAM_region_ID, region_GCAM3), by = "GCAM_region_ID") %>%
      left_join(select(L223.SubsectorShrwt_renew_GCAM3, region_GCAM3, subsector, year, share.weight),
                by = c("region_GCAM3", "subsector", "year")) %>%
      replace_na(list(share.weight = 0)) %>%
      #    L223.SubsectorShrwt_renew <- add_region_name( L223.SubsectorShrwt_renew )[ names_SubsectorShrwt ]
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->   # TODO not sure this is right
      L223.SubsectorShrwt_renew

    # L223.SubsectorInterp_elec and L223.SubsectorInterpTo_elec: Subsector shareweight interpolation of electricity sector (140-146)
    if(any(is.na(A23.subsector_interp$to.value))) {
      L223.SubsectorInterp_elec <- write_to_all_regions(A23.subsector_interp[ is.na( A23.subsector_interp$to.value),], LEVEL2_DATA_NAMES[["SubsectorInterp"]])
    }
    if( any( !is.na( A23.subsector_interp$to.value ) ) ){
      L223.SubsectorInterpTo_elec <- write_to_all_regions( A23.subsector_interp[ !is.na( A23.subsector_interp$to.value ), ], LEVEL2_DATA_NAMES[["SubsectorInterpTo"]])
    }

    # Adjust subsector interp rules regionally (148-159)
    # Any global interp rules that match by region + sector + subsector name will be replaced by
    # a regionally specific interp rule
    L223.SubsectorInterp_elec %>%
      filter(names_Subsector %in% A23.subsector_interp_R$names_Subsector) %>%
      bind_rows(A23.subsector_interp_R) %>%
      set_years() ->
      L223.SubsectorInterp_elec
    L223.SubsectorInterpTo_elec %>%
      filter(names_Subsector %in% A23.subsector_interp_R$names_Subsector) %>%
      bind_rows(A23.subsector_interp_R) %>%
      set_years() ->
      L223.SubsectorInterpTo_elec

    # 2c. Technology information
    # L223.StubTech_elec: Identification of stub technologies of electricity generation
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A23.globaltech_shrwt %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]]), GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L223.StubTech_elec

    # L223.GlobalTechEff_elec: Energy inputs and coefficients, efficiency, of global electricity generation technologies
    A23.globaltech_eff %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector, efficiency = value) %>%
      mutate(efficiency = round(efficiency, energy.DIGITS_EFFICIENCY)) ->
      L223.GlobalTechEff_elec_all
    # reorders columns to match expected model interface input
    L223.GlobalTechEff_elec_all <- L223.GlobalTechEff_elec_all[LEVEL2_DATA_NAMES[["GlobalTechEff"]]]

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.GlobalTechEff_elec_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechEff_elec
    #Hard code in type "Resource" for intermittent technology resource input only
    L223.GlobalIntTechEff_elec["type"] <- "Resource"

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.GlobalTechEff_elec_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechEff_elec

    # L223.GlobalTechCapital_elec: Capital costs of global electricity generation technologies
    A23.globaltech_capital %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector, capital.overnight = value, input.capital = `input-capital`) %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL)) ->
      L223.GlobalTechCapital_elec_all
    # reorders columns to match expected model interface input
    L223.GlobalTechCapital_elec_all <- L223.GlobalTechCapital_elec_all[LEVEL2_DATA_NAMES[["GlobalTechCapital"]]]

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.GlobalTechCapital_elec_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechCapital_elec

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.GlobalTechCapital_elec_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechCapital_elec

    # L223.GlobalTechCapital_elec: Capital costs of global electricity generation technologies - advanced case
    A23.globaltech_capital_adv %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector, capital.overnight = value, input.capital = `input-capital`) %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL)) ->
      L223.GlobalTechCapital_elec_adv_all
    # reorders columns to match expected model interface input
    L223.GlobalTechCapital_elec_adv_all <- L223.GlobalTechCapital_elec_adv_all[LEVEL2_DATA_NAMES[["GlobalTechCapital"]]]

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.GlobalTechCapital_elec_adv_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechCapital_elec_adv

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.GlobalTechCapital_elec_adv_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechCapital_elec_adv

    #Separate capital costs of global electricity technologies - advanced case (lines 195-203)
    L223.GlobalTechCapital_elec_adv %>%
      filter(subsector.name %in% c("solar", "rooftop_pv")) ->
      L223.GlobalTechCapital_sol_adv
    L223.GlobalIntTechCapital_elec_adv %>%
      filter(subsector.name %in% c("solar", "rooftop_pv")) ->
      L223.GlobalIntTechCapital_sol_adv

    L223.GlobalIntTechCapital_elec_adv %>%
      filter(subsector.name == "wind") ->
      L223.GlobalIntTechCapital_wind_adv
    L223.GlobalTechCapital_elec_adv %>%
      filter(subsector.name == "wind") ->
      L223.GlobalTechCapital_wind_adv

    L223.GlobalTechCapital_elec_adv %>%
      filter(subsector.name == "geothermal") ->
      L223.GlobalTechCapital_geo_adv

    L223.GlobalTechCapital_elec_adv %>%
      filter(subsector.name == "nuclear") ->
      L223.GlobalTechCapital_nuc_adv

    # L223.GlobalTechCapital_elec: Capital costs of global electricity generation technologies - low tech case
    A23.globaltech_capital_low %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector, capital.overnight = value, input.capital = `input-capital`) %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL)) ->
      L223.GlobalTechCapital_elec_low_all
    # reorders columns to match expected model interface input
    L223.GlobalTechCapital_elec_low_all <- L223.GlobalTechCapital_elec_low_all[LEVEL2_DATA_NAMES[["GlobalTechCapital"]]]

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.GlobalTechCapital_elec_low_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechCapital_elec_low

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.GlobalTechCapital_elec_low_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechCapital_elec_low

    #Separate capital costs of global electricity technologies - low case (lines 212-223)
    L223.GlobalIntTechCapital_elec_low %>%
      filter(subsector.name %in% c("solar", "rooftop_pv")) ->
      L223.GlobalIntTechCapital_sol_low
    L223.GlobalTechCapital_elec_low %>%
      filter(subsector.name %in% c("solar", "rooftop_pv")) ->
      L223.GlobalTechCapital_sol_low

    L223.GlobalIntTechCapital_elec_low %>%
      filter(subsector.name == "wind") ->
      L223.GlobalIntTechCapital_wind_low
    L223.GlobalTechCapital_elec_low %>%
      filter(subsector.name == "wind") ->
      L223.GlobalTechCapital_wind_low

    L223.GlobalTechCapital_elec_low %>%
      filter(subsector.name == "geothermal") ->
      L223.GlobalTechCapital_geo_low

    L223.GlobalTechCapital_elec_low %>%
      filter(subsector.name == "nuclear") ->
      L223.GlobalTechCapital_nuc_low

    L223.GlobalTechCapital_elec_low %>%
      filter(subsector.name == "biomass") ->
      L223.GlobalTechCapital_bio_low

    # L223.GlobalTechOMfixed_elec: Fixed O&M costs of global electricity generation technologies
    A23.globaltech_OMfixed %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector, OM.fixed = value) %>%
      mutate(OM.fixed = round(OM.fixed, energy.DIGITS_OM)) ->
      L223.globaltech_OMfixed_all
    # reorders columns to match expected model interface input
    L223.globaltech_OMfixed_all <- L223.globaltech_OMfixed_all[LEVEL2_DATA_NAMES[["GlobalTechOMfixed"]]]

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.globaltech_OMfixed_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechOMfixed_elec

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.globaltech_OMfixed_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechOMfixed_elec

    # L223.GlobalTechOMvar_elec: Variable O&M costs of global electricity generation technologies
    A23.globaltech_OMvar %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector, OM.var = value) %>%
      mutate(OM.var = round(OM.var, energy.DIGITS_OM)) ->
      L223.globaltech_OMvar_all
    # reorders columns to match expected model interface input
    L223.globaltech_OMvar_all <- L223.globaltech_OMvar_all[LEVEL2_DATA_NAMES[["GlobalTechOMvar"]]]

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.globaltech_OMvar_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechOMvar_elec

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.globaltech_OMvar_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechOMvar_elec

    # L223.GlobalTechShrwt_elec: Shareweights of global electricity generation technologies
    A23.globaltech_shrwt %>%
      gather(year, share.weight, matches(YEAR_PATTERN)) %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, BASE_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(as.numeric(year), share.weight, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L223.GlobalTechShrwt_elec_all
    # reorders columns to match expected model interface input
    L223.GlobalTechShrwt_elec_all <- L223.GlobalTechShrwt_elec_all[c(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight")]

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.GlobalTechShrwt_elec_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechShrwt_elec

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.GlobalTechShrwt_elec_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechShrwt_elec

    # Interpolation rules
    # TODO: we should subset_inttechs only at the moment there are none
    A23.globaltech_interp %>%
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = subsector) -> L223.GlobalTechInterp_elec

    # L223.PrimaryRenewKeyword_elec: Keywords of primary renewable electric generation technologies
    A23.globaltech_keyword %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L223.AllKeyword_elec

    # select only sector technology combinations with a value in primary renewable and match columns to expected model interface input
    L223.AllKeyword_elec %>%
      filter(!is.na(primary.renewable)) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "primary.renewable")) ->
      L223.PrimaryRenewKeyword_elec_all

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.PrimaryRenewKeyword_elec_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.PrimaryRenewKeywordInt_elec

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.PrimaryRenewKeyword_elec_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.PrimaryRenewKeyword_elec

    # L223.AvgFossilEffKeyword_elec: Keywords of fossil/bio electric generation technologies
    L223.AllKeyword_elec %>%
      filter(!is.na(average.fossil.efficiency)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "average.fossil.efficiency")) ->
      L223.AvgFossilEffKeyword_elec

    # L223.GlobalTechCapture_elec: CO2 capture fractions from global electricity generation technologies
    A23.globaltech_co2capture %>%
      gather(year, remove.fraction, matches(YEAR_PATTERN)) %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, BASE_YEARS, FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(as.numeric(year), remove.fraction, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% FUTURE_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
    # Rounds the fraction to two digits and adds the name of the carbon storage market
      mutate(remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION), storage.market = energy.CO2.STORAGE.MARKET) ->
      L223.GlobalTechCapture_elec
    # reorders columns to match expected model interface input
    L223.GlobalTechCapture_elec <- L223.GlobalTechCapture_elec[c(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction", "storage.market")]

    # L223.GlobalIntTechBackup_elec: Backup parameters for global electricity generation technologies
    A23.globalinttech %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L223.GlobalIntTechBackup_elec
    # reorders columns to match expected model interface input
    L223.GlobalIntTechBackup_elec <- L223.GlobalIntTechBackup_elec[c(LEVEL2_DATA_NAMES[["GlobalTechBackup"]])]

    A23.globaltech_retirement %>%
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L223.globaltech_retirement_base

    # Copies base year retirment information into all future years and appends back onto itself
    L223.globaltech_retirement_base %>%
      filter(year == max(BASE_YEARS)) %>%
      repeat_add_columns(tibble("year" = as.character(FUTURE_YEARS))) %>%
      select(-year.x) %>%
      rename(year = year.y) %>%
      bind_rows(L223.globaltech_retirement_base) ->
      L223.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L223.globaltech_retirement for each of these functions and creates a subset for each option then removes any subsets with 0 rows
    # All of these options have different headers, and all are allowed

    #Subsets the phased retirement function
    L223.globaltech_retirement %>%
      filter(!is.na(L223.globaltech_retirement$shutdown.rate)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTech"]], "lifetime", "shutdown.rate")) ->
      L223.GlobalTechShutdown_elec_all

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.GlobalTechShutdown_elec_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechShutdown_elec

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.GlobalTechShutdown_elec_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechShutdown_elec

    # Removes any empty data frames
    if (nrow(L223.GlobalIntTechShutdown_elec) == 0){
      rm(L223.GlobalIntTechShutdown_elec)
    }
    if (nrow(L223.GlobalTechShutdown_elec) == 0){
      rm(L223.GlobalTechShutdown_elec)
    }

    # Subsets the S-Curve retirement function
    L223.globaltech_retirement %>%
      filter(!is.na(L223.globaltech_retirement$half.life)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTech"]], "lifetime", "steepness", "half.life")) ->
      L223.GlobalTechSCurve_elec_all

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.GlobalTechSCurve_elec_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechSCurve_elec

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.GlobalTechSCurve_elec_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechSCurve_elec

    # Removes any empty data frames
    if (nrow(L223.GlobalIntTechSCurve_elec) == 0){
      rm(L223.GlobalIntTechSCurve_elec)
    }
    if (nrow(L223.GlobalTechSCurve_elec) == 0){
      rm(L223.GlobalTechSCurve_elec)
    }

    # Subsets the remaining with no retirement function
    # L223.GlobalTechLifetime_en: Global tech lifetime
    L223.globaltech_retirement %>%
      filter(is.na(L223.globaltech_retirement$shutdown.rate) & is.na(L223.globaltech_retirement$half.life)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTech"]], "lifetime")) ->
      L223.GlobalTechLifetime_elec_all

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.GlobalTechLifetime_elec_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechLifetime_elec

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.GlobalTechLifetime_elec_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechLifetime_elec

    # Removes any empty data frames
    if (nrow(L223.GlobalIntTechLifetime_elec) == 0){
      rm(L223.GlobalIntTechLifetime_elec)
    }
    if (nrow(L223.GlobalTechLifetime_elec) == 0){
      rm(L223.GlobalTechLifetime_elec)
    }

    # Subsets any technologies with a shutdown parameter based on profitability
    # L223.GlobalTechProfitShutdown_en: Global tech profit shutdown decider and parameters
    L223.globaltech_retirement %>%
      filter(!is.na(L223.globaltech_retirement$median.shutdown.point)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTech"]], "median.shutdown.point", "profit.shutdown.steepness")) ->
      L223.GlobalTechProfitShutdown_elec_all

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.GlobalTechProfitShutdown_elec_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechProfitShutdown_elec

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.GlobalTechProfitShutdown_elec_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechProfitShutdown_elec

    # Removes any empty data frames
    if (nrow(L223.GlobalIntTechProfitShutdown_elec) == 0){
      rm(L223.GlobalIntTechProfitShutdown_elec)
    }
    if (nrow(L223.GlobalTechProfitShutdown_elec) == 0){
      rm(L223.GlobalTechProfitShutdown_elec)
    }

    #2d. Calibration and region-specific data
    # generate base year calibrated inputs of electricity by interpolating from historical values
    L1231.in_EJ_R_elec_F_tech_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, BASE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(as.numeric(year), value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% BASE_YEARS) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L223.in_EJ_R_elec_F_tech_Yh_base

    # append matching calibrated technology sector/subsector/technology to calibrated inputs to electricity
    calibrated_techs %>%
      semi_join(L223.in_EJ_R_elec_F_tech_Yh_base, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology, calibration) %>%
      # left_join because the join changes the number of rows, multiple matches in electricity for every calibrated tech.
      left_join(L223.in_EJ_R_elec_F_tech_Yh_base, by = c("sector", "fuel", "technology")) %>%
      # filters out all calibrated techs whose calibration is not an input. With default techs, this is redundant with the above join and removes nothing.
      filter(calibration == "input") %>%
      select(-calibration) %>%
      rename(stub.technology = technology) ->
      L223.in_EJ_R_elec_F_tech_Yh

    # These steps calculate the shareweights and cleans up the format of the data frame for modelinterface
    # L223.StubTechCalInput_elec: calibrated input of electricity generation technologies
    # Note that there is no need to specify which stub technologies are intermittent
    L223.in_EJ_R_elec_F_tech_Yh %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT), share.weight.year = year) %>%
      set_subsector_shrwt() %>%
      mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      left_join(A23.globaltech_eff, by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      rename(calibrated.value = calOutputValue) ->
      L223.StubTechCalInput_elec
    L223.StubTechCalInput_elec <- L223.StubTechCalInput_elec[LEVEL2_DATA_NAMES[["StubTechCalInput"]]]

    # NOTE: Fixed output is assumed to apply in all historical years, regardless of final calibration year. CWR: This will only include different years if historical years are offset from base years.
    #  generate base year calibrated inputs of electricity by interpolating from historical values
    L1231.out_EJ_R_elec_F_tech_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, MODEL_YEARS[MODEL_YEARS %in% HISTORICAL_YEARS])) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(as.numeric(year), value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS[MODEL_YEARS %in% HISTORICAL_YEARS]) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L223.out_EJ_R_elec_F_tech_Yh_base

    # append matching calibrated technology sector/subsector/technology to calibrated outputs of electricity
    calibrated_techs %>%
      semi_join(L223.out_EJ_R_elec_F_tech_Yh_base, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology, calibration) %>%
      # left_join because the join changes the number of rows, multiple matches in electricity for every calibrated tech.
      left_join(L223.out_EJ_R_elec_F_tech_Yh_base, by = c("sector", "fuel", "technology")) %>%
      rename(stub.technology = technology) ->
      L223.out_EJ_R_elec_F_tech_Yh

    # L223.StubTechFixOut_elec: fixed output of electricity generation technologies
    # filters for all calibrated techs with a fixed output. By default, this is only hydropower.
    L223.out_EJ_R_elec_F_tech_Yh %>%
      filter(calibration == "fixed output") %>%
      select(-calibration) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT), share.weight.year = year, subsector.share.weight = 0, share.weight = 0) ->
      L223.StubTechFixOut_elec

    # filters for all calibrated techs with a fixed output. By default, this is nuclear, wind, solar, geothermal.
    L223.out_EJ_R_elec_F_tech_Yh %>%
      filter(calibration == "output" & year %in% BASE_YEARS) %>%
      select(-calibration) ->
      L223.calout_EJ_R_elec_F_tech_Yh

    # Adding in future hydropower generation here
    # L223.StubTechFixOut_hydro: fixed output of future hydropower
    L118.out_EJ_R_elec_hydro_Yfut %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, FUTURE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(as.numeric(year), value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS[MODEL_YEARS %in% HISTORICAL_YEARS]) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L223.StubTechFixOut_hydro

    calibrated_techs %>%
      semi_join(L223.StubTechFixOut_hydro, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology, calibration) %>%
      # left_join because the join changes the number of rows, multiple matches in electricity for every calibrated tech.
      left_join(L223.StubTechFixOut_hydro, by = c("sector", "fuel")) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT), share.weight.year = year, subs.share.weight = 0, tech.share.weight = 0) %>%
      rename(stub.technology = technology, fixedOutput = value) ->
      L223.StubTechFixOut_hydro
    L223.StubTechFixOut_hydro <- L223.StubTechFixOut_hydro[LEVEL2_DATA_NAMES[["StubTechFixOut"]]]

    # Cleaning up and setting shareweights for L223.StubTechProd_elec: calibrated output of electricity generation technologies
    L223.calout_EJ_R_elec_F_tech_Yh %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT), share.weight.year = year, share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      set_subsector_shrwt() ->
      L223.StubTechProd_elec
    L223.StubTechProd_elec <-L223.StubTechProd_elec[c(LEVEL2_DATA_NAMES[["StubTechYr"]], "calOutputValue","share.weight.year" , "subs.share.weight", "share.weight")]

    # L223.StubTechEff_elec: calibrated efficiencies of electricity generation technologies
    # NOTE: Electric sector efficiencies are assumed to apply for all historical years, regardless of final calibration year
    L1231.eff_R_elec_F_tech_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, MODEL_YEARS[MODEL_YEARS %in% HISTORICAL_YEARS])) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(as.numeric(year), value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS[MODEL_YEARS %in% HISTORICAL_YEARS]) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L223.eff_R_elec_F_tech_Yh

    calibrated_techs %>%
      semi_join(L223.eff_R_elec_F_tech_Yh, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology) %>%
      # left_join because the join changes the number of rows, multiple matches in electricity for every calibrated tech.
      left_join(L223.eff_R_elec_F_tech_Yh, by = c("sector", "fuel", "technology")) %>%
      left_join(A23.globaltech_eff, by = c("supplysector", "subsector", "technology")) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT), market.name = region) %>% # old data system rounds to caloutput. should we round to efficiency?
      rename(stub.technology = technology, efficiency = value) ->
      L223.StubTechEff_elec
    L223.StubTechEff_elec <- L223.StubTechEff_elec[LEVEL2_DATA_NAMES[["StubTechEff"]]]

    # L223.StubTechCapFactor_elec: regional adjustments to wind capacity factors
    # Regional adjustments to wind to include a "base price" for the wind resource supply
    # We will have these total levelized cost reconcile by adjusting the capacity factor
    L114.RsrcCurves_EJ_R_wind %>%
      select(GCAM_region_ID, base.price) %>%
      mutate(year = energy.WIND.BASE.COST.YEAR) ->
      L223.StubTechCapFactor_elec_base

    # filter to the base cost year and match the base capital, fixed OM, and variable OM costs to the base price that year for wind technology.
    L223.GlobalIntTechCapital_elec %>%
      filter(intermittent.technology == "wind" & year == energy.WIND.BASE.COST.YEAR) %>%
      select(LEVEL2_DATA_NAMES[["GlobalIntTechCapital"]], -capacity.factor) %>%
      left_join(L223.StubTechCapFactor_elec_base, by = "year") %>%
      left_join(L223.GlobalIntTechOMvar_elec, by = c("year", "sector.name", "subsector.name", "intermittent.technology")) %>%
      left_join(L223.GlobalIntTechOMfixed_elec, by = c("year", "sector.name", "subsector.name", "intermittent.technology")) %>%
      select(-input.OM.var, -capacity.factor, -year) %>%
    # Calculate a new capacity factor to match the regional base.price, append region names and duplicate over all model years.
    # Note that there is only one capacity factor but the same value needs to be read in for capital
    # and fixed O&M so the column is duplicated.
      mutate(capacity.factor.OM = round((capital.overnight * fixed.charge.rate +
                                OM.fixed) / (CONV_KWH_GJ * CONV_YEAR_HOURS) / (base.price - (OM.var / (1000 * CONV_KWH_GJ))), energy.DIGITS_CAPACITY_FACTOR)) %>%
      mutate(capacity.factor.capital = capacity.factor.OM) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(supplysector = sector.name, subsector = subsector.name)->
      L223.StubTechCapFactor_elec_nostor

     # duplicates rows and appends wind.storage as a stub technology to itself, so capacity factors apply also to wind technology with storage
    L223.StubTechCapFactor_elec_nostor %>%
      mutate(stub.technology = "wind.storage") %>%
      bind_rows(L223.StubTechCapFactor_elec_nostor) ->
      L223.StubTechCapFactor_elec
    L223.StubTechCapFactor_elec <- L223.StubTechCapFactor_elec[LEVEL2_DATA_NAMES[["StubTechCapFactor"]]]

    # Regional capacity factor adjustment for solar technologies. We will use relative total and direct irradiance
    # to scale the capacity factors for central PV and CSP respectively.
    # Clean up capital assumptions for solar technologies
    A23.globaltech_capital %>%
      filter(subsector %in% c("solar", "rooftop_pv")) %>%
      select(supplysector, subsector, technology, `input-capital`, capacity.factor) %>%
      rename(capacity.factor.capital = capacity.factor, input.capital = `input-capital`) ->
      L223.StubTechCapFactor_solar_cap

    # Filter electricity fixed OM cost assumptions for solar and rooftop PV and remove year columns
    A23.globaltech_OMfixed %>%
      filter(subsector %in% c("solar", "rooftop_pv")) %>%
      select(-matches(YEAR_PATTERN)) %>%
      rename(capacity.factor.OM = capacity.factor) %>%
      left_join_error_no_match(L223.StubTechCapFactor_solar_cap, by = c("supplysector", "subsector", "technology")) ->
      L223.StubTechCapFactor_solar_base

    # Add data on solar irradiance to base assumptions about solar electricity capacity factors.
    L119.Irradiance_rel_R %>%
      mutate(input.OM.fixed = "OM-fixed") %>%
      left_join(L223.StubTechCapFactor_solar_base, by = "input.OM.fixed") %>%
      rename(stub.technology = technology) ->
      L223.StubTechCapFactor_solar_full

    # Multiply capacity factor for CSP technologies by average direct normal irradiance
    L223.StubTechCapFactor_solar_full %>%
      filter(grepl("CSP", stub.technology)) %>%
      mutate(capacity.factor.capital = capacity.factor.capital * dni_avg_rel, capacity.factor.OM = capacity.factor.OM * dni_avg_rel) ->
      L223.StubTechCapFactor_solar_base3

    # Multiply capacity factor for PV technologies by average relative irradiance, re-bind solar and CSP into the same data frame, and change any capacity factors that exceed the maximum possible.
    L223.StubTechCapFactor_solar_full %>%
      filter(grepl("PV", stub.technology, ignore.case = TRUE)) %>%
      mutate(capacity.factor.capital = capacity.factor.capital * irradiance_avg_rel, capacity.factor.OM = capacity.factor.OM * irradiance_avg_rel) %>%
      bind_rows(L223.StubTechCapFactor_solar_base3) %>%
      mutate(capacity.factor.capital = ifelse(capacity.factor.capital > 0.85, 0.85, capacity.factor.capital),
             capacity.factor.OM = ifelse(capacity.factor.OM > 0.85, 0.85, capacity.factor.OM)) %>%
      # Replace region IDs with region names and propagate rows across each model year.
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L223.StubTechCapFactor_solar
    # Match expected model interface names in L223.StubTechCapFactor_elec and then bind solar capacity factors to rest of electricity sector.
    L223.StubTechCapFactor_solar <- L223.StubTechCapFactor_solar[LEVEL2_DATA_NAMES[["StubTechCapFactor"]]]
    L223.StubTechCapFactor_elec %>%
      bind_rows(L223.StubTechCapFactor_solar) ->
      L223.StubTechCapFactor_elec

    # ===================================================

    # Produce outputs

    L223.Supplysector_elec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Supplysector_elec") %>%
      add_precursors("common/GCAM_region_names", "energy/A23.sector") ->
      L223.Supplysector_elec

    L223.ElecReserve %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.ElecReserve") %>%
      add_precursors("common/GCAM_region_names", "energy/A23.sector") ->
      L223.ElecReserve

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorLogit_elec") %>%
      add_precursors("energy/A23.subsector_logit", "common/GCAM_region_names") ->
      L223.SubsectorLogit_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwt_elec") %>%
      add_precursors("energy/A23.subsector_shrwt", "common/GCAM_region_names") ->
      L223.SubsectorShrwt_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_elec") %>%
      add_precursors("energy/A23.subsector_shrwt", "common/GCAM_region_names") ->
      L223.SubsectorShrwtFllt_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwt_nuc") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A23.subsector_shrwt_nuc_R") ->
      L223.SubsectorShrwt_nuc

    L223.SubsectorShrwt_renew %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwt_renew") %>%
      add_precursors("common/iso_GCAM_regID", "A23.subsector_shrwt_renew_R", "common/GCAM_region_names") ->
      L223.SubsectorShrwt_renew

    L223.SubsectorInterp_elec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorInterp_elec") %>%
      add_precursors("energy/A23.subsector_interp", "energy/A23.subsector_interp_R") ->
      L223.SubsectorInterp_elec

    L223.SubsectorInterpTo_elec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorInterpTo_elec") %>%
      add_precursors("energy/A23.subsector_interp", "energy/A23.subsector_interp_R") ->
      L223.SubsectorInterpTo_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTech_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTech_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechEff_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechEff_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechEff_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechEff_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechCapital_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechCapital_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechCapital_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechOMfixed_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechOMfixed_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechOMfixed_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechOMfixed_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechOMvar_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechOMvar_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechOMvar_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechOMvar_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechShrwt_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechShrwt_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechInterp_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechInterp_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechShrwt_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechShrwt_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.PrimaryRenewKeyword_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.PrimaryRenewKeyword_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.PrimaryRenewKeywordInt_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.PrimaryRenewKeywordInt_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.AvgFossilEffKeyword_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.AvgFossilEffKeyword_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapture_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechCapture_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechBackup_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechBackup_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechCapFactor_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechCapFactor_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechShutdown_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechShutdown_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechShutdown_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechShutdown_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechSCurve_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechSCurve_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechSCurve_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechSCurve_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechLifetime_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechLifetime_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechLifetime_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechLifetime_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechProfitShutdown_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechProfitShutdown_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechProfitShutdown_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechProfitShutdown_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechCalInput_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechCalInput_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechFixOut_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechFixOut_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechFixOut_hydro") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechFixOut_hydro

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechProd_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechProd_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechEff_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.StubTechEff_elec

    L223.GlobalTechCapital_sol_adv %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_sol_adv") %>%
      add_precursors("energy/A23.globaltech_capital_adv") ->
      L223.GlobalTechCapital_sol_adv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechCapital_sol_adv") %>%
      add_precursors("energy/A23.globaltech_capital_adv") ->
      L223.GlobalIntTechCapital_sol_adv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_wind_adv") %>%
      add_precursors("energy/A23.globaltech_capital_adv") ->
      L223.GlobalTechCapital_wind_adv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechCapital_wind_adv") %>%
      add_precursors("energy/A23.globaltech_capital_adv") ->
      L223.GlobalIntTechCapital_wind_adv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_geo_adv") %>%
      add_precursors("energy/A23.globaltech_capital_adv") ->
      L223.GlobalTechCapital_geo_adv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_nuc_adv") %>%
      add_precursors("energy/A23.globaltech_capital_adv") ->
      L223.GlobalTechCapital_nuc_adv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_sol_low") %>%
      add_precursors("energy/A23.globaltech_capital_low") ->
      L223.GlobalTechCapital_sol_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechCapital_sol_low") %>%
      add_precursors("energy/A23.globaltech_capital_low") ->
      L223.GlobalIntTechCapital_sol_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_wind_low") %>%
      add_precursors("energy/A23.globaltech_capital_low") ->
      L223.GlobalTechCapital_wind_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechCapital_wind_low") %>%
      add_precursors("energy/A23.globaltech_capital_low") ->
      L223.GlobalIntTechCapital_wind_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_geo_low") %>%
      add_precursors("energy/A23.globaltech_capital_low") ->
      L223.GlobalTechCapital_geo_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_nuc_low") %>%
      add_precursors("energy/A23.globaltech_capital_low") ->
      L223.GlobalTechCapital_nuc_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_bio_low") %>%
      add_precursors("energy/A23.globaltech_capital_low") ->
      L223.GlobalTechCapital_bio_low

    return_data(L223.Supplysector_elec, L223.ElecReserve, L223.SubsectorLogit_elec, L223.SubsectorShrwt_elec, L223.SubsectorShrwtFllt_elec, L223.SubsectorShrwt_nuc, L223.SubsectorShrwt_renew, L223.SubsectorInterp_elec, L223.SubsectorInterpTo_elec, L223.StubTech_elec, L223.GlobalIntTechEff_elec, L223.GlobalTechEff_elec, L223.GlobalTechCapital_elec, L223.GlobalIntTechCapital_elec, L223.GlobalTechOMfixed_elec, L223.GlobalIntTechOMfixed_elec, L223.GlobalTechOMvar_elec, L223.GlobalIntTechOMvar_elec, L223.GlobalTechShrwt_elec, L223.GlobalTechInterp_elec, L223.GlobalIntTechShrwt_elec, L223.PrimaryRenewKeyword_elec, L223.PrimaryRenewKeywordInt_elec, L223.AvgFossilEffKeyword_elec, L223.GlobalTechCapture_elec, L223.GlobalIntTechBackup_elec, L223.StubTechCapFactor_elec, L223.GlobalTechShutdown_elec, L223.GlobalIntTechShutdown_elec, L223.GlobalTechSCurve_elec, L223.GlobalIntTechSCurve_elec, L223.GlobalTechLifetime_elec, L223.GlobalIntTechLifetime_elec, L223.GlobalTechProfitShutdown_elec, L223.GlobalIntTechProfitShutdown_elec, L223.StubTechCalInput_elec, L223.StubTechFixOut_elec, L223.StubTechFixOut_hydro, L223.StubTechProd_elec, L223.StubTechEff_elec, L223.GlobalTechCapital_sol_adv, L223.GlobalIntTechCapital_sol_adv, L223.GlobalTechCapital_wind_adv, L223.GlobalIntTechCapital_wind_adv, L223.GlobalTechCapital_geo_adv, L223.GlobalTechCapital_nuc_adv, L223.GlobalTechCapital_sol_low, L223.GlobalIntTechCapital_sol_low, L223.GlobalTechCapital_wind_low, L223.GlobalIntTechCapital_wind_low, L223.GlobalTechCapital_geo_low, L223.GlobalTechCapital_nuc_low, L223.GlobalTechCapital_bio_low)
  } else {
    stop("Unknown command")
  }
}
