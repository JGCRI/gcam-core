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
             FILE = "temp-data-inject/L102.gdp_mil90usd_GCAM3_ctry_Y"))
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
    get_data(all_data, "temp-data-inject/L102.gdp_mil90usd_GCAM3_ctry_Y") %>%
      # TEMPORARY
      gather(year, weight, -iso) %>% mutate(year = as.integer(substr(year, 2, 5))) %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      select(-year) ->
      L102.gdp_mil90usd_GCAM3_ctry_Y


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



    # Produce outputs

    L223.Supplysector_elec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.Supplysector_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.Supplysector_elec

    L223.ElecReserve %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.ElecReserve") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.ElecReserve

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorLogit_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorLogit_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwt_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorShrwt_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorShrwtFllt_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwt_nuc") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorShrwt_nuc

    L223.SubsectorShrwt_renew %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorShrwt_renew") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorShrwt_renew

    L223.SubsectorInterp_elec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorInterp_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.SubsectorInterp_elec

    L223.SubsectorInterpTo_elec %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.SubsectorInterpTo_elec") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
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

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_sol_adv") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechCapital_sol_adv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechCapital_sol_adv") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechCapital_sol_adv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_wind_adv") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechCapital_wind_adv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechCapital_wind_adv") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechCapital_wind_adv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_geo_adv") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechCapital_geo_adv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_nuc_adv") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechCapital_nuc_adv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_sol_low") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechCapital_sol_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechCapital_sol_low") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechCapital_sol_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_wind_low") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechCapital_wind_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalIntTechCapital_wind_low") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalIntTechCapital_wind_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_geo_low") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechCapital_geo_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_nuc_low") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechCapital_nuc_low

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_bio_low") %>%
      add_precursors("common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.GlobalTechCapital_bio_low

    return_data(L223.Supplysector_elec, L223.ElecReserve, L223.SubsectorLogit_elec, L223.SubsectorShrwt_elec, L223.SubsectorShrwtFllt_elec, L223.SubsectorShrwt_nuc, L223.SubsectorShrwt_renew, L223.SubsectorInterp_elec, L223.SubsectorInterpTo_elec, L223.StubTech_elec, L223.GlobalIntTechEff_elec, L223.GlobalTechEff_elec, L223.GlobalTechCapital_elec, L223.GlobalIntTechCapital_elec, L223.GlobalTechOMfixed_elec, L223.GlobalIntTechOMfixed_elec, L223.GlobalTechOMvar_elec, L223.GlobalIntTechOMvar_elec, L223.GlobalTechShrwt_elec, L223.GlobalTechInterp_elec, L223.GlobalIntTechShrwt_elec, L223.PrimaryRenewKeyword_elec, L223.PrimaryRenewKeywordInt_elec, L223.AvgFossilEffKeyword_elec, L223.GlobalTechCapture_elec, L223.GlobalIntTechBackup_elec, L223.StubTechCapFactor_elec, L223.GlobalTechShutdown_elec, L223.GlobalIntTechShutdown_elec, L223.GlobalTechSCurve_elec, L223.GlobalIntTechSCurve_elec, L223.GlobalTechLifetime_elec, L223.GlobalIntTechLifetime_elec, L223.GlobalTechProfitShutdown_elec, L223.GlobalIntTechProfitShutdown_elec, L223.StubTechCalInput_elec, L223.StubTechFixOut_elec, L223.StubTechFixOut_hydro, L223.StubTechProd_elec, L223.StubTechEff_elec, L223.GlobalTechCapital_sol_adv, L223.GlobalIntTechCapital_sol_adv, L223.GlobalTechCapital_wind_adv, L223.GlobalIntTechCapital_wind_adv, L223.GlobalTechCapital_geo_adv, L223.GlobalTechCapital_nuc_adv, L223.GlobalTechCapital_sol_low, L223.GlobalIntTechCapital_sol_low, L223.GlobalTechCapital_wind_low, L223.GlobalIntTechCapital_wind_low, L223.GlobalTechCapital_geo_low, L223.GlobalTechCapital_nuc_low, L223.GlobalTechCapital_bio_low)
  } else {
    stop("Unknown command")
  }
}
