# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L223.electricity
#'
#' Prepares assumptions and calibrated inputs and outputs for the electricity sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L223.Supplysector_elec}, \code{L223.ElecReserve}, \code{L223.SubsectorLogit_elec},
#' \code{L223.SubsectorShrwt_elec}, \code{L223.SubsectorShrwtFllt_elec},
#' \code{L223.SubsectorShrwt_coal}, \code{L223.SubsectorShrwt_nuc},
#' \code{L223.SubsectorShrwt_renew}, \code{L223.SubsectorInterp_elec}, \code{L223.SubsectorInterpTo_elec},
#' \code{L223.StubTech_elec}, \code{L223.GlobalIntTechEff_elec}, \code{L223.GlobalTechEff_elec},
#' \code{L223.GlobalTechCapFac_elec}, \code{L223.GlobalIntTechCapFac_elec}, \code{L223.GlobalTechCapital_elec},
#' \code{L223.GlobalIntTechCapital_elec}, \code{L223.GlobalTechOMfixed_elec}, \code{L223.GlobalIntTechOMfixed_elec},
#' \code{L223.GlobalTechOMvar_elec}, \code{L223.GlobalIntTechOMvar_elec}, \code{L223.GlobalTechShrwt_elec},
#' \code{L223.GlobalTechInterp_elec}, \code{L223.GlobalIntTechShrwt_elec}, \code{L223.PrimaryRenewKeyword_elec},
#' \code{L223.PrimaryRenewKeywordInt_elec}, \code{L223.AvgFossilEffKeyword_elec}, \code{L223.GlobalTechCapture_elec},
#' \code{L223.GlobalIntTechBackup_elec}, \code{L223.StubTechCapFactor_elec}, \code{L223.StubTechCost_offshore_wind},
#' \code{L223.GlobalTechShutdown_elec}, \code{L223.GlobalIntTechShutdown_elec}, \code{L223.GlobalTechSCurve_elec},
#' \code{L223.GlobalIntTechSCurve_elec}, \code{L223.GlobalTechLifetime_elec}, \code{L223.GlobalIntTechLifetime_elec},
#' \code{L223.GlobalTechProfitShutdown_elec}, \code{L223.GlobalIntTechProfitShutdown_elec},
#' \code{L223.StubTechCalInput_elec}, \code{L223.StubTechFixOut_elec}, \code{L223.StubTechFixOut_hydro},
#' \code{L223.StubTechProd_elec}, \code{L223.StubTechEff_elec}, \code{L223.StubTechSecOut_desal}, \code{L223.GlobalTechCapital_sol_adv},
#' \code{L223.GlobalIntTechCapital_sol_adv}, \code{L223.GlobalTechCapital_wind_adv},
#' \code{L223.GlobalIntTechCapital_wind_adv}, \code{L223.GlobalTechCapital_geo_adv},
#' \code{L223.GlobalTechCapital_nuc_adv}, \code{L223.GlobalTechCapital_sol_low},
#' \code{L223.GlobalIntTechCapital_sol_low}, \code{L223.GlobalTechCapital_wind_low},
#' \code{L223.GlobalIntTechCapital_wind_low}, \code{L223.GlobalTechCapital_geo_low},
#' \code{L223.GlobalTechCapital_nuc_low}, \code{L223.GlobalTechCapital_bio_low}. The corresponding file in the
#' original data system was \code{L223.electricity.R} (energy level2).
#' @details Includes all information for the global technology database, including capital and O&M costs, efficiencies, retirement rates, shareweights and interpolation rules.
#' Often uses interpolation functions to apply assumptions to all model periods.
#' Solar and wind capacity factor assumptions are scaled using data on irradiance and available wind resource. It also determines future fixed outputs of hydropower.
#' This also prepares alternate low- and high-tech capital costs, which are then saved to their own xmls and can be used to overwrite default capital costs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join arrange bind_rows filter if_else group_by left_join mutate select semi_join summarise rename
#' @importFrom tidyr complete nesting replace_na
#' @author CWR October 2017/BBL July 2017
module_energy_L223.electricity <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A23.sector",
             FILE = "energy/A23.subsector_logit",
             FILE = "energy/A23.subsector_shrwt",
             FILE = "energy/A23.subsector_interp",
             FILE = "energy/A23.subsector_interp_R",
             FILE = "energy/A23.subsector_shrwt_coal_R",
             FILE = "energy/A23.subsector_shrwt_nuc_R",
             FILE = "energy/A23.subsector_shrwt_renew_R",
             FILE = "energy/A23.globalinttech",
             FILE = "energy/A23.globaltech_shrwt",
             FILE = "energy/A23.globaltech_interp",
             FILE = "energy/A23.globaltech_keyword",
             FILE = "energy/A23.globaltech_capacity_factor",
             FILE = "energy/A23.globaltech_retirement",
             FILE = "energy/A23.globaltech_co2capture",
             FILE = "water/EFW_mapping",
             FILE = "energy/A23.globaltech_eff",
             "L113.globaltech_capital_ATB",
             "L113.globaltech_capital_ATB_adv",
             "L113.globaltech_capital_ATB_low",
             "L113.globaltech_OMfixed_ATB",
             "L113.globaltech_OMvar_ATB",
             "L114.RsrcCurves_EJ_R_wind",
             "L118.out_EJ_R_elec_hydro_Yfut",
             "L119.Irradiance_rel_R",
             "L1231.in_EJ_R_elec_F_tech_Yh",
             "L1231.out_EJ_R_elec_F_tech_Yh",
             "L1231.eff_R_elec_F_tech_Yh",
             "L120.GridCost_offshore_wind",
             "L120.RegCapFactor_offshore_wind",
             "L1232.desalsecout_R_elec_F_tech",
             "L102.gdp_mil90usd_GCAM3_ctry_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L223.Supplysector_elec",
             "L223.ElecReserve",
             "L223.SubsectorLogit_elec",
             "L223.SubsectorShrwt_elec",
             "L223.SubsectorShrwtFllt_elec",
             "L223.SubsectorShrwt_coal",
             "L223.SubsectorShrwt_nuc",
             "L223.SubsectorShrwt_renew",
             "L223.SubsectorInterp_elec",
             "L223.SubsectorInterpTo_elec",
             "L223.StubTech_elec",
             "L223.GlobalIntTechEff_elec",
             "L223.GlobalTechEff_elec",
             "L223.GlobalTechCapFac_elec",
             "L223.GlobalIntTechCapFac_elec",
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
             "L223.StubTechCost_offshore_wind",
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
             "L223.StubTechSecOut_desal",
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

    # Silence global package checks
    GCAM_region_ID <- OM.fixed <- OM.var <- average.fossil.efficiency <- base.price <-
      calOutputValue <- calibration <- capacity.factor <- capacity.factor.OM <-
      capacity.factor.capital <- capital.overnight <- dni_avg_rel <- efficiency <-
      fixed.charge.rate <- fuel <- `input-capital` <- input.OM.var <-
      intermittent.technology <- irradiance_avg_rel <- iso <-
      primary.renewable <- region <- region_GCAM3 <- remove.fraction <- sector <-
      sector.name <- share.weight <- stub.technology <- subsector <- subsector.name <-
      supplysector <- technology <- value <- weight <- year <- year.fillout <- year.x <- year.y <-
      CFmax <- grid.cost <- input.cost <- minicam.non.energy.input <- from.year <- to.year <-
      secondary.output <- output.ratio <- secout_coef <- NULL

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = TRUE)
    A23.sector <- get_data(all_data, "energy/A23.sector", strip_attributes = TRUE)
    A23.subsector_logit <- get_data(all_data, "energy/A23.subsector_logit", strip_attributes = TRUE)
    A23.subsector_shrwt <- get_data(all_data, "energy/A23.subsector_shrwt", strip_attributes = TRUE)
    A23.subsector_interp <- get_data(all_data, "energy/A23.subsector_interp", strip_attributes = TRUE)
    A23.subsector_interp_R <- get_data(all_data, "energy/A23.subsector_interp_R")
    A23.subsector_shrwt_coal_R <- get_data(all_data, "energy/A23.subsector_shrwt_coal_R")
    A23.subsector_shrwt_nuc_R <- get_data(all_data, "energy/A23.subsector_shrwt_nuc_R")
    A23.subsector_shrwt_renew_R <- get_data(all_data, "energy/A23.subsector_shrwt_renew_R")
    A23.globalinttech <- get_data(all_data, "energy/A23.globalinttech", strip_attributes = TRUE)
    A23.globaltech_shrwt <- get_data(all_data, "energy/A23.globaltech_shrwt", strip_attributes = TRUE)
    A23.globaltech_interp <- get_data(all_data, "energy/A23.globaltech_interp", strip_attributes = TRUE)
    A23.globaltech_keyword <- get_data(all_data, "energy/A23.globaltech_keyword", strip_attributes = TRUE)
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    A23.globaltech_capacity_factor <- get_data(all_data, "energy/A23.globaltech_capacity_factor")
    L113.globaltech_capital_ATB <- get_data(all_data, "L113.globaltech_capital_ATB", strip_attributes = TRUE)
    L113.globaltech_capital_ATB_adv <- get_data(all_data, "L113.globaltech_capital_ATB_adv", strip_attributes = TRUE)
    L113.globaltech_capital_ATB_low <- get_data(all_data, "L113.globaltech_capital_ATB_low", strip_attributes = TRUE)
    L113.globaltech_OMfixed_ATB <- get_data(all_data, "L113.globaltech_OMfixed_ATB")
    L113.globaltech_OMvar_ATB <- get_data(all_data, "L113.globaltech_OMvar_ATB")
    A23.globaltech_retirement <- get_data(all_data, "energy/A23.globaltech_retirement", strip_attributes = TRUE)
    A23.globaltech_co2capture <- get_data(all_data, "energy/A23.globaltech_co2capture")
    EFW_mapping <- get_data(all_data, "water/EFW_mapping")
    L114.RsrcCurves_EJ_R_wind <- get_data(all_data, "L114.RsrcCurves_EJ_R_wind")
    L118.out_EJ_R_elec_hydro_Yfut <- get_data(all_data, "L118.out_EJ_R_elec_hydro_Yfut")
    L119.Irradiance_rel_R <- get_data(all_data, "L119.Irradiance_rel_R")
    L120.GridCost_offshore_wind <- get_data(all_data, "L120.GridCost_offshore_wind")
    L120.RegCapFactor_offshore_wind <- get_data(all_data, "L120.RegCapFactor_offshore_wind")
    L1231.in_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.in_EJ_R_elec_F_tech_Yh")
    L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh")
    L1231.eff_R_elec_F_tech_Yh <- get_data(all_data, "L1231.eff_R_elec_F_tech_Yh")
    L1232.desalsecout_R_elec_F_tech <- get_data(all_data, "L1232.desalsecout_R_elec_F_tech", strip_attributes = TRUE)
    L102.gdp_mil90usd_GCAM3_ctry_Y <- get_data(all_data, "L102.gdp_mil90usd_GCAM3_ctry_Y")

    # ============================
    # 2a. Supplysector information
    # ============================

    # Write supply sector information for electricity sector to all regions in L223.Supplysector_elec
    L223.Supplysector_elec <- write_to_all_regions(A23.sector, c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names)

    # Write electricity reserve margin and average grid capacity factor assumptions to all regions in L223.ElecReserve
    L223.ElecReserve <- write_to_all_regions(A23.sector, LEVEL2_DATA_NAMES[["ElecReserve"]], GCAM_region_names)

    # =========================
    # 2b. Subsector Information
    # =========================

    # Write subsector logit exponents of electricity sector to all regions in L223.SubsectorLogit_elec
    L223.SubsectorLogit_elec <- write_to_all_regions(A23.subsector_logit, c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names)

    # Write subsector shareweights of electricity sector to all regions, separating those interpolating to a year in L223.SubsectorShrwt_elec:
    if(any(!is.na(A23.subsector_shrwt$year))) {
      A23.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwt"]], GCAM_region_names) ->
        L223.SubsectorShrwt_elec
    }

    # And those using a year.fillout in L223.SubsectorShrwtFllt_elec:
    if(any(!is.na(A23.subsector_shrwt$year.fillout))) {
      A23.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
        L223.SubsectorShrwtFllt_elec
    }

    # Assumed coal electricity subsector shareweights by region to generate L223.SubsectorShrwt_coal
    # This is intended to override default coal electricty subsector shareweights for specific regions and years.
    A23.subsector_shrwt_coal_R %>%
      gather_years(value_col = "share.weight") %>%
      # Interpolate to model time periods, and add columns specifying the final format
      complete(nesting(region, supplysector, subsector), year = MODEL_FUTURE_YEARS[MODEL_FUTURE_YEARS >= min(year) & MODEL_FUTURE_YEARS <= max(year)]) %>%
      filter(year %in% MODEL_FUTURE_YEARS[MODEL_FUTURE_YEARS >= min(year) & MODEL_FUTURE_YEARS <= max(year)]) %>%
      arrange(region,supplysector,subsector,year) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1),year = as.integer(year))->
      L223.SubsectorShrwt_coal
    L223.SubsectorShrwt_coal <- L223.SubsectorShrwt_coal[LEVEL2_DATA_NAMES[["SubsectorShrwt"]]]

    # Calculate subsector shareweights of nuclear electricity to generate L223.SubsectorShrwt_nuc
    # -------------------------------------------------------------------------------------------

    # Start out with the list of ISO matched to region_GCAM3
    iso_GCAM_regID %>%
      select(iso, region_GCAM3, GCAM_region_ID) %>%
      left_join_error_no_match(select(A23.subsector_shrwt_nuc_R, region_GCAM3, matches(YEAR_PATTERN)),
                               by = "region_GCAM3") ->
      L223.SubsectorShrwt_nuc_ctry

    # Filter to final year GDPs to prepare for weighting country-level shareweights
    L102.gdp_mil90usd_GCAM3_ctry_Y %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      select(iso, value) %>%
      rename(weight = value) ->
      L202.gdp_mil90usd_GCAM3_ctry_Y

    # Where country-level shareweights are provided, use those
    L223.SubsectorShrwt_nuc_ctry %>%
      filter(iso %in% A23.subsector_shrwt_nuc_R$iso) %>%
      select(-matches(YEAR_PATTERN)) %>%
      left_join_error_no_match(select(A23.subsector_shrwt_nuc_R, iso, matches(YEAR_PATTERN)),
                               by = "iso") %>%
      bind_rows(filter(L223.SubsectorShrwt_nuc_ctry, !iso %in% A23.subsector_shrwt_nuc_R$iso)) %>%

      # Use GDP by country as a weighting factor in going from country-level shareweights to region-level shareweights
      gather_years %>%
      left_join(L202.gdp_mil90usd_GCAM3_ctry_Y, by = "iso") %>%
      mutate(year = as.integer(year)) %>%
      na.omit %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = weighted.mean(value, weight)) %>%

      # Interpolate to model time periods, and add columns specifying the final format
      complete(GCAM_region_ID, year = MODEL_FUTURE_YEARS[MODEL_FUTURE_YEARS >= min(year) & MODEL_FUTURE_YEARS <= max(year)]) %>%
      filter(year %in% MODEL_FUTURE_YEARS[MODEL_FUTURE_YEARS >= min(year) & MODEL_FUTURE_YEARS <= max(year)]) %>%
      arrange(GCAM_region_ID, year) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      # applies consistent supplysector and subsector names (electricity, nuclear)
      mutate(supplysector = A23.subsector_shrwt_nuc_R$supplysector[1],
             subsector = A23.subsector_shrwt_nuc_R$subsector[1]) %>%
      ungroup() %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID) ->
      L223.SubsectorShrwt_nuc
    L223.SubsectorShrwt_nuc <- L223.SubsectorShrwt_nuc[LEVEL2_DATA_NAMES[["SubsectorShrwt"]]]

    # Calculate near term subsector shareweights of renewable technologies to generate L223.SubsectorShrwt_renew
    # ----------------------------------------------------------------------------------------------------------

    # First, melt the table with near-term shareweights from GCAM 3.0 regions
    A23.subsector_shrwt_renew_R %>%
      gather_years(value_col = "share.weight") ->
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
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L223.SubsectorShrwt_renew
    L223.SubsectorShrwt_renew <- L223.SubsectorShrwt_renew[LEVEL2_DATA_NAMES[["SubsectorShrwt"]]]

    # Calculate subsector shareweight interpolation rules within electricity sector for L223.SubsectorInterp_elec and L223.SubsectorInterpTo_elec
    # -------------------------------------------------------------------------------------------------------------------------------------------

    # First write global interpolation rules to all regions, then any global interp rules that match by region + sector + subsector name will be
    # replaced by a regionally specific interpolation rule by first removing those rules from L223.SubsectorInterp_elec and then replacing them
    if(any(is.na(A23.subsector_interp$to.value))) {
      L223.SubsectorInterp_elec <- write_to_all_regions(A23.subsector_interp[is.na(A23.subsector_interp$to.value),], LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names)

      L223.SubsectorInterp_elec %>%
        anti_join(A23.subsector_interp_R, by = c("region", "supplysector", "subsector")) %>%
        bind_rows(set_years(A23.subsector_interp_R[, names(L223.SubsectorInterp_elec)])) ->
        L223.SubsectorInterp_elec
    }

    # Same process for interpolation rules using a to.value
    if(any(!is.na(A23.subsector_interp$to.value))) {
      L223.SubsectorInterpTo_elec <- write_to_all_regions(A23.subsector_interp[!is.na(A23.subsector_interp$to.value),], LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], GCAM_region_names)

      L223.SubsectorInterpTo_elec %>%
        anti_join(A23.subsector_interp_R, by = c("region", "supplysector", "subsector")) %>%
        bind_rows(set_years(A23.subsector_interp_R[!is.na(A23.subsector_interp_R$to.value), names(L223.SubsectorInterpTo_elec)])) ->
        L223.SubsectorInterpTo_elec
    }

    # ==========================
    # 2c. Technology Information
    # ==========================

    # Identify stub technologies of electricity generation for all regions to generate L223.StubTech_elec
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A23.globaltech_shrwt %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]]), GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L223.StubTech_elec

    # Write energy inputs and coefficients, efficiency, of global electricity generation technologies in L223.GlobalTechEff_elec
    # --------------------------------------------------------------------------------------------------------------------------

    # Extrapolate efficiency values to all model years and round to appropriate number of digits
    ### A23.globaltech_eff ASSUMPTION FILE HAS TWO ADDITIONAL ROWS IN OLD DATA SYSTEM (backup electricity for CSP and PV)
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
    # Hard code in type "Resource" for intermittent technology resource input only
    L223.GlobalIntTechEff_elec["type"] <- "Resource"

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.GlobalTechEff_elec_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechEff_elec

    # Capacity factor of global technologies
    A23.globaltech_capacity_factor %>%
      gather_years(value_col = "capacity.factor") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(capacity.factor = approx_fun(year, capacity.factor, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L223.GlobalTechCapFac_elec_all

    # Subsets the intermittent technologies by checking it against the list in A23.globalinttech
    L223.GlobalTechCapFac_elec_all %>%
      semi_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) %>%
      rename(intermittent.technology = technology) ->
      L223.GlobalIntTechCapFac_elec

    # Subsets the non-intermittent technologies by checking against any not listed in A23.globalinttech
    L223.GlobalTechCapFac_elec_all %>%
      anti_join(A23.globalinttech, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology")) ->
      L223.GlobalTechCapFac_elec

    # Calculate base case capital costs of global electricity generation technologies in L223.GlobalTechCapital_elec
    # --------------------------------------------------------------------------------------------------------------

    L113.globaltech_capital_ATB %>%
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

    # Calculate advanced technology capital costs of global electricity generation technologies in L223.GlobalTechCapital_elec_adv
    # ----------------------------------------------------------------------------------------------------------------------------

    # Extrapolate capital cost assumptions to all future years and round them
    L113.globaltech_capital_ATB_adv %>%
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

    # Separate capital costs of global electricity technologies into separate files for each technology
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

    # Calculate capital costs of global electricity generation technologies - low tech case - for L223.GlobalTechCapital_elec_low
    # ----------------------------------------------------------------------------------------------------------

    # Extrapolate capital cost assumptions to all model years and then round to appropriate number of digits
    L113.globaltech_capital_ATB_low %>%
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

    # Separate low tech capital costs of global electricity technologies into separate files for each technology
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

    # Calculate fixed O&M costs of global electricity generation technologies for L223.GlobalTechOMfixed_elec
    # -------------------------------------------------------------------------------------------------------

    # Extrapolate fixed OM cost assumptions to all model years and then round to appropriate number of digits
    L113.globaltech_OMfixed_ATB %>%
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

    # Calculate variable O&M costs of global electricity generation technologies for L223.GlobalTechOMvar_elec
    # --------------------------------------------------------------------------------------------------------

    # Extrapolate variable OM cost assumptions to all model years and then round to appropriate number of digits
    L113.globaltech_OMvar_ATB %>%
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

    # Interpolate Shareweights of global electricity generation technologies for L223.GlobalTechShrwt_elec and L223.GlobalIntTechShrwt_elec
    # -------------------------------------------------------------------------------------------------------------------------------------

    # Interpolate shareweight assumptions to all base and future years.
    A23.globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1)) %>%
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

    # Interpolation rules for L223.GlobalTechInterp_elec
    # --------------------------------------------------

    # TODO: we should subset_inttechs only at the moment there are none
    A23.globaltech_interp %>%
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      # strips attributes from assumptions file
      mutate(sector.name = sector.name) ->
      L223.GlobalTechInterp_elec

    # Keywords of primary renewable electric generation technologies
    # --------------------------------------------------------------

    # Copy keywords to all model years
    A23.globaltech_keyword %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L223.AllKeyword_elec

    # Subset into L223.PrimaryRenewKeywordInt_elec, L223.PrimaryRenewKeyword_elec, and L223.AvgFossilEffKeyword_elec
    # select only sector technology combinations with a value in primary renewable and match columns to expected model interface input
    L223.AllKeyword_elec %>%
      filter(!is.na(primary.renewable)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "primary.renewable") ->
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
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "average.fossil.efficiency") ->
      L223.AvgFossilEffKeyword_elec

    # Write CO2 capture fractions from global electricity generation technologies for L223.GlobalTechCapture_elec
    # -----------------------------------------------------------------------------------------------------------

    # Interpolate fractions of CO2 captured to all future years
    A23.globaltech_co2capture %>%
      gather_years(value_col = "remove.fraction") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, remove.fraction, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      # Rounds the fraction to two digits and adds the name of the carbon storage market
      mutate(remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION), storage.market = energy.CO2.STORAGE.MARKET) ->
      L223.GlobalTechCapture_elec
    # reorders columns to match expected model interface input
    L223.GlobalTechCapture_elec <- L223.GlobalTechCapture_elec[c(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction", "storage.market")]

    # Set backup parameters for global electricity generation technologies for L223.GlobalIntTechBackup_elec
    # ------------------------------------------------------------------------------------------------------

    # Copy assumed parameters to all model years
    A23.globalinttech %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L223.GlobalIntTechBackup_elec
    # reorders columns to match expected model interface input
    L223.GlobalIntTechBackup_elec <- L223.GlobalIntTechBackup_elec[c(LEVEL2_DATA_NAMES[["GlobalTechBackup"]])]

    # Set global technology retirement information for all electricity sector technologies
    # ------------------------------------------------------------------------------------

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L223.globaltech_retirement for each of these functions and creates a subset for each option then removes any subsets with 0 rows
    # All of these options have different headers, and all are allowed.
    # Also, technologies that have an additional shutdown rate as a function of their profitability are also defined.

    # Replace years and prepare assumptions into correct format
    A23.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L223.globaltech_retirement_base

    # Copies base year retirement information into all future years and appends back onto itself
    L223.globaltech_retirement_base %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(filter(L223.globaltech_retirement_base, year == max(MODEL_BASE_YEARS))) ->
      L223.globaltech_retirement

    # PHASED RETIREMENT
    # Subsets the phased retirement function
    L223.globaltech_retirement %>%
      filter(!is.na(L223.globaltech_retirement$shutdown.rate)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "shutdown.rate") ->
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

    # S-CURVE RETIREMENT
    # Subsets the S-Curve retirement function
    L223.globaltech_retirement %>%
      filter(!is.na(L223.globaltech_retirement$half.life)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
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

    # NO RETIREMENT FUNCTION (FULL LIFETIME)
    # Subsets the remaining with no retirement function
    L223.globaltech_retirement %>%
      filter(is.na(L223.globaltech_retirement$shutdown.rate) & is.na(L223.globaltech_retirement$half.life)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime") ->
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


    # PROFIT-BASED SHUTDOWN PARAMETERS
    # Subsets any technologies with a shutdown parameter based on profitability
    L223.globaltech_retirement %>%
      filter(!is.na(L223.globaltech_retirement$median.shutdown.point)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
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

    # Removes any empty data frames (if above subsets return 0 rows)
    if(nrow(L223.GlobalIntTechShutdown_elec) == 0) {
      rm(L223.GlobalIntTechShutdown_elec)
    }
    if(nrow(L223.GlobalTechShutdown_elec) == 0) {
      rm(L223.GlobalTechShutdown_elec)
    }
    if(nrow(L223.GlobalIntTechLifetime_elec) == 0) {
      rm(L223.GlobalIntTechLifetime_elec)
    }
    if(nrow(L223.GlobalTechLifetime_elec) == 0) {
      rm(L223.GlobalTechLifetime_elec)
    }
    if(nrow(L223.GlobalIntTechSCurve_elec) == 0) {
      rm(L223.GlobalIntTechSCurve_elec)
    }
    if(nrow(L223.GlobalTechSCurve_elec) == 0) {
      rm(L223.GlobalTechSCurve_elec)
    }
    if(nrow(L223.GlobalIntTechProfitShutdown_elec) == 0) {
      rm(L223.GlobalIntTechProfitShutdown_elec)
    }
    if(nrow(L223.GlobalTechProfitShutdown_elec) == 0) {
      rm(L223.GlobalTechProfitShutdown_elec)
    }

    # ========================================
    # 2d. Calibration and region-specific data
    # ========================================

    # Calculate calibrated input values for electricity sector technologies
    # ---------------------------------------------------------------------

    # generate base year calibrated inputs of electricity by interpolating from historical values
    L1231.in_EJ_R_elec_F_tech_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel, technology), year = c(year, MODEL_BASE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel, technology) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
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


    # Generate base year calibrated fixed outputs of electricity for L223.StubTechFixOut_elec and L223.StubTechProd_elec
    # ------------------------------------------------------------------------------------------------------------------

    # NOTE: Fixed output is assumed to apply in all historical years, regardless of final calibration year.
    # NOTE: MODEL_BASE_YEARS /= (MODEL_YEARS %in% HISTORICAL YEARS) only if historical years are offset from base years.
    # Interpolate calibrated outputs to historical years from L1231 values
    L1231.out_EJ_R_elec_F_tech_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel, technology), year = c(year, MODEL_YEARS[MODEL_YEARS %in% HISTORICAL_YEARS])) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel, technology) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
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
      mutate(fixedOutput = round(value, energy.DIGITS_CALOUTPUT), share.weight.year = year, subsector.share.weight = 0, share.weight = 0) ->
      L223.StubTechFixOut_elec
    L223.StubTechFixOut_elec <- L223.StubTechFixOut_elec[c(LEVEL2_DATA_NAMES[["StubTechYr"]], "fixedOutput", "share.weight.year", "subsector.share.weight", "share.weight")]

    # filters for all other calibrated techs. By default, this is nuclear, wind, solar, geothermal.
    L223.out_EJ_R_elec_F_tech_Yh %>%
      filter(calibration == "output" & year %in% MODEL_BASE_YEARS) %>%
      select(-calibration) %>%
      # Cleaning up and setting shareweights for L223.StubTechProd_elec: calibrated output of electricity generation technologies
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT), share.weight.year = year, share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      set_subsector_shrwt() ->
      L223.StubTechProd_elec
    L223.StubTechProd_elec <-L223.StubTechProd_elec[c(LEVEL2_DATA_NAMES[["StubTechYr"]], "calOutputValue", "share.weight.year" , "subs.share.weight", "share.weight")]

    # Calculate fixed output of future hydropower generation for L223.StubTechFixOut_hydro
    # ------------------------------------------------------------------------------------

    # Interpolate values for fixed output of future hydropower
    L118.out_EJ_R_elec_hydro_Yfut %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS[!MODEL_FUTURE_YEARS %in% HISTORICAL_YEARS]) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L223.StubTechFixOut_hydro

    # Match in calibrated technology information, round digits, and set shareweights
    calibrated_techs %>%
      semi_join(L223.StubTechFixOut_hydro, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology, calibration) %>%
      # left_join because the join changes the number of rows, multiple matches in electricity for every calibrated tech.
      left_join(L223.StubTechFixOut_hydro, by = c("sector", "fuel")) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT), share.weight.year = year, subs.share.weight = 0, tech.share.weight = 0) %>%
      rename(stub.technology = technology, fixedOutput = value) ->
      L223.StubTechFixOut_hydro
    L223.StubTechFixOut_hydro <- L223.StubTechFixOut_hydro[LEVEL2_DATA_NAMES[["StubTechFixOut"]]]

    # Calculate calibrated efficiencies of electricity generation technologies for L223.StubTechEff_elec
    # --------------------------------------------------------------------------------------------------

    # NOTE: Electric sector efficiencies are assumed to apply for all historical years, regardless of final calibration year
    # Interpolate values to model years within historical years (by default, this equals MODEL_BASE_YEARS)
    L1231.eff_R_elec_F_tech_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel, technology), year = c(year, MODEL_YEARS[MODEL_YEARS %in% HISTORICAL_YEARS])) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel, technology) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
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

    # Make regional adjustments to wind capacity factors for L223.StubTechCapFactor_elec
    # ----------------------------------------------------------------------------------

    # Regional adjustments to wind to include a "base price" for the wind resource supply
    # We will have these total levelized cost reconcile by adjusting the capacity factor
    L114.RsrcCurves_EJ_R_wind %>%
      select(GCAM_region_ID, base.price) %>%
      mutate(year = energy.WIND.BASE.COST.YEAR) ->
      L223.StubTechCapFactor_elec_base

    # filter to the base cost year and match the base capital, fixed OM, and variable OM costs to the base price that year for wind technology.
    L223.GlobalIntTechCapital_elec %>%
      filter(intermittent.technology == "wind" & year == energy.WIND.BASE.COST.YEAR) %>%
      select(LEVEL2_DATA_NAMES[["GlobalIntTechCapital"]]) %>%
      left_join(L223.StubTechCapFactor_elec_base, by = "year") %>%
      left_join(L223.GlobalIntTechOMvar_elec, by = c("year", "sector.name", "subsector.name", "intermittent.technology")) %>%
      left_join(L223.GlobalIntTechOMfixed_elec, by = c("year", "sector.name", "subsector.name", "intermittent.technology")) %>%
      select(-input.OM.var, -year) %>%
      # Calculate a new capacity factor to match the regional base.price, append region names and duplicate over all model years.
      # This fixes the capacity factor for all future years and is inconsistent if future capacity factors are assumed to change.
      mutate(capacity.factor = round((capital.overnight * fixed.charge.rate +
                                        OM.fixed) / (CONV_KWH_GJ * CONV_YEAR_HOURS) / (base.price - (OM.var / (1000 * CONV_KWH_GJ))), energy.DIGITS_CAPACITY_FACTOR)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = intermittent.technology) ->
      L223.StubTechCapFactor_elec_nostor

    # duplicates rows and appends wind.storage as a stub technology to itself, so capacity factors apply also to wind technology with storage
    L223.StubTechCapFactor_elec_nostor %>%
      mutate(stub.technology = "wind_storage") %>%
      bind_rows(L223.StubTechCapFactor_elec_nostor) ->
      L223.StubTechCapFactor_elec
    L223.StubTechCapFactor_elec <- L223.StubTechCapFactor_elec[LEVEL2_DATA_NAMES[["StubTechCapFactor"]]]

    # Regional capacity factor adjustment for solar technologies. We will use relative total and direct irradiance
    # to scale the capacity factors for PV and CSP respectively.
    # ------------------------------------------------------------------------------------------------------------

    # Multiply capacity factor for CSP technologies by average direct normal irradiance
    L223.GlobalTechCapFac_elec_all %>%
      filter(grepl("CSP", technology)) %>%
      # reset CSP_storage capacity factors to same as CSP, as we want to apply the
      # DNI multiplier to the base capacity factor only, not the storage portion
      group_by(year) %>%
      mutate(capacity.factor = capacity.factor[technology=="CSP"]) %>%
      ungroup() %>%
      repeat_add_columns(L119.Irradiance_rel_R) %>%
      mutate(capacity.factor = capacity.factor * dni_avg_rel,
             # add back in the capacity factor boost for CSP_storage
             capacity.factor = if_else(grepl("storage", technology),
                                       capacity.factor + energy.CSP_STORAGE_CF_DIFF,
                                       capacity.factor)) ->
      L223.StubTechCapFactor_solar_csp

    # Multiply capacity factor for PV technologies by average relative irradiance,
    # re-bind solar and CSP into the same data frame, and change any capacity factors that exceed the maximum possible.
    L223.GlobalTechCapFac_elec_all %>%
      filter(grepl("PV", technology, ignore.case = TRUE)) %>%
      repeat_add_columns(L119.Irradiance_rel_R) %>%
      mutate(capacity.factor = capacity.factor * irradiance_avg_rel) %>%
      bind_rows(L223.StubTechCapFactor_solar_csp) %>%
      mutate(capacity.factor = if_else(capacity.factor > 0.85, 0.85, capacity.factor)) %>%
      # Replace region IDs with region names and propagate rows across each model year.
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) ->
      L223.StubTechCapFactor_solar
    # Match expected model interface names in L223.StubTechCapFactor_elec and then combine solar and wind capacity factors.
    L223.StubTechCapFactor_solar <- L223.StubTechCapFactor_solar[LEVEL2_DATA_NAMES[["StubTechCapFactor"]]]
    L223.StubTechCapFactor_elec %>%
      bind_rows(L223.StubTechCapFactor_solar) ->
      L223.StubTechCapFactor_elec

    # Adding offshore wind capacity factors
    # Adding regions to capacity factor first
    L120.RegCapFactor_offshore_wind %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") -> L120.RegCapFactor_offshore_wind

    # Adding capacity factors to the main electricity table
    L223.StubTechCapFactor_elec %>%
      filter(stub.technology == "wind") %>%
      mutate(stub.technology = "wind_offshore") %>%
      left_join_error_no_match(L120.RegCapFactor_offshore_wind ,
                               by = c("region")) %>%
      mutate(capacity.factor = round(CFmax, energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(region, supplysector, subsector, stub.technology, year, capacity.factor) -> L223.StubTechCapFactor_elec_offshore_wind

    L223.StubTechCapFactor_elec %>%
      bind_rows(L223.StubTechCapFactor_elec_offshore_wind) -> L223.StubTechCapFactor_elec

    # Regional non-energy cost adder for offshore wind grid connection cost
    gcam_regions <- unique(GCAM_region_names$region)

    L113.globaltech_capital_ATB %>%
      filter(technology == "wind_offshore") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(tibble(region = gcam_regions)) %>%
      mutate(minicam.non.energy.input = "regional price adjustment") %>%
      left_join_error_no_match(L120.GridCost_offshore_wind, by = c("region")) %>%
      rename(input.cost = grid.cost) %>%
      filter(!is.na(input.cost)) %>%
      mutate(input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      select(region, supplysector, subsector, stub.technology = technology,
             year, minicam.non.energy.input, input.cost) -> L223.StubTechCost_offshore_wind

    # L223.StubTechSecOut_desal: secondary output of desalinated seawater from electricity technologies
    # Note that this only applies in selected regions that have combined electric + desalination plants
    L223.StubTechSecOut_desal <- filter(L1232.desalsecout_R_elec_F_tech, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology),
                               by = c("sector", "fuel", "technology")) %>%
      rename(stub.technology = technology) %>%
      mutate(secondary.output = water.DESAL,
             output.ratio = round(secout_coef, energy.DIGITS_CALOUTPUT)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechSecOut"]])

    # ===================================================

    # Produce outputs

    L223.Supplysector_elec %>%
      add_title("Supply sector information for energy transformation sectors") %>%
      add_units("Unitless") %>%
      add_comments("Written to all regions from A23.sector") %>%
      add_legacy_name("L223.Supplysector_elec") %>%
      add_precursors("common/GCAM_region_names", "energy/A23.sector") ->
      L223.Supplysector_elec

    L223.ElecReserve %>%
      add_title("Electricity reserve margins and grid capacity factors by region") %>%
      add_units("unitless") %>%
      add_comments("Reserve margin: Average fraction of capacity reserved for backup") %>%
      add_comments("Grid Capacity: Conversion factor of grid capacity to output for entire grid") %>%
      add_legacy_name("L223.ElecReserve") %>%
      add_precursors("common/GCAM_region_names", "energy/A23.sector") ->
      L223.ElecReserve

    L223.SubsectorLogit_elec %>%
      add_title("Subsector logit exponents of energy transformation sectors") %>%
      add_units("Unitless") %>%
      add_comments("Written to all regions from A22.subsector_logit") %>%
      add_legacy_name("L223.SubsectorLogit_elec") %>%
      add_precursors("energy/A23.subsector_logit", "common/GCAM_region_names") ->
      L223.SubsectorLogit_elec

    if(exists("L223.SubsectorShrwt_elec")) {
      L223.SubsectorShrwt_elec %>%
        add_title("Subsector shareweights of energy transformation sectors") %>%
        add_units("Unitless") %>%
        add_comments("Conditionally created from the subset of A23.subsector_shrwt with values in column 'year'.") %>%
        add_comments("Does not exist by default") %>%
        add_legacy_name("L223.SubsectorShrwt_elec") %>%
        add_precursors("energy/A23.subsector_shrwt", "common/GCAM_region_names") ->
        L223.SubsectorShrwt_elec
    } else {
      missing_data() %>%
        add_legacy_name("L223.SubsectorShrwt_elec") ->
        L223.SubsectorShrwt_elec
    }

    if(exists("L223.SubsectorShrwtFllt_elec")) {
      L223.SubsectorShrwtFllt_elec %>%
        add_title("Subsector shareweights of energy transformation sectors") %>%
        add_units("Unitless") %>%
        add_comments("Conditionally created from the subset of A23.subsector_shrwt with values in column 'year.fillout'.") %>%
        add_legacy_name("L223.SubsectorShrwtFllt_elec") %>%
        add_precursors("energy/A23.subsector_shrwt", "common/GCAM_region_names") ->
        L223.SubsectorShrwtFllt_elec
    } else {
      missing_data() %>%
        add_legacy_name("L223.SubsectorShrwtFllt_elec") ->
        L223.SubsectorShrwtFllt_elec
    }

    L223.SubsectorShrwt_coal %>%
      add_title("Subsector Shareweights for regions for coal electricity technologies") %>%
      add_units("unitless") %>%
      add_comments("Assumptions in A23.subsector_shrwt_coal_R are used to override region shareweights") %>%
      add_precursors("energy/A23.subsector_shrwt_coal_R") ->
      L223.SubsectorShrwt_coal

    L223.SubsectorShrwt_nuc %>%
      add_title("Subsector Shareweights for all regions for nuclear electricity technologies") %>%
      add_units("unitless") %>%
      add_comments("Assumptions in A23.subsector_shrwt_nuc_R are used to generate country-level shareweights, which are then weighted by size of GDP") %>%
      add_comments("where country-level weights are not available. These are aggregated and interpolated for regional shareweights") %>%
      add_legacy_name("L223.SubsectorShrwt_nuc") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A23.subsector_shrwt_nuc_R", "L102.gdp_mil90usd_GCAM3_ctry_Y") ->
      L223.SubsectorShrwt_nuc

    L223.SubsectorShrwt_renew %>%
      add_title("Renewable electricity sector technology shareweights") %>%
      add_units("unitless") %>%
      add_comments("Incomplete GCAM3 assumptions approximately matched to a new table of region/supplysector/subsector/year") %>%
      add_legacy_name("L223.SubsectorShrwt_renew") %>%
      add_precursors("common/iso_GCAM_regID", "energy/A23.subsector_shrwt_renew_R", "common/GCAM_region_names") ->
      L223.SubsectorShrwt_renew

    L223.SubsectorInterp_elec %>%
      add_title("Regional interpolation rules using a to.year for electricity subsectors") %>%
      add_units("unitless") %>%
      add_comments("Global interpolation rules subset to those using a to.year, applied regionally, ") %>%
      add_comments("and then replaced by regional interpolation rules where found in A23.subsector.interp_R") %>%
      add_legacy_name("L223.SubsectorInterp_elec") %>%
      add_precursors("energy/A23.subsector_interp", "energy/A23.subsector_interp_R") ->
      L223.SubsectorInterp_elec

    L223.SubsectorInterpTo_elec %>%
      add_title("Regional interpolation rules using a to.value for electricity subsectors") %>%
      add_units("unitless") %>%
      add_comments("Global interpolation rules subset to those using a to.value, applied regionally and then replaced by regional interpolation rules where found in A23.subsector_interp_R") %>%
      add_comments("and then replaced by regional interpolation rules where found in A23.subsector.interp_R") %>%
      add_legacy_name("L223.SubsectorInterpTo_elec") %>%
      add_precursors("energy/A23.subsector_interp", "energy/A23.subsector_interp_R") ->
      L223.SubsectorInterpTo_elec

    L223.StubTech_elec %>%
      add_title("Stub technologies for electricity generation") %>%
      add_units("unitless") %>%
      add_comments("Generated from list in A23.globaltech_shrwt, which assumes these are comprehensive") %>%
      add_legacy_name("L223.StubTech_elec") %>%
      add_precursors("energy/A23.globaltech_shrwt") ->
      L223.StubTech_elec

    L223.GlobalIntTechEff_elec %>%
      add_title("Conversion efficiency by intermittent technology of energy into electricity") %>%
      add_units("unitless") %>%
      add_comments("Filtered for intermittent technologies and extrapolated using an exponential function") %>%
      add_legacy_name("L223.GlobalIntTechEff_elec") %>%
      add_precursors("energy/A23.globaltech_eff", "energy/A23.globalinttech") ->
      L223.GlobalIntTechEff_elec

    L223.GlobalTechEff_elec %>%
      add_title("Conversion efficiency by non-intermittent technology of energy into electricity") %>%
      add_units("unitless") %>%
      add_comments("Filtered for technologies not requiring intermittent backup and extrapolated using an exponential function") %>%
      add_legacy_name("L223.GlobalTechEff_elec") %>%
      add_precursors("energy/A23.globaltech_eff", "energy/A23.globalinttech") ->
      L223.GlobalTechEff_elec

    L223.GlobalTechCapFac_elec %>%
      add_title("Capacity factor for non-intermittent electricity sector technologies") %>%
      add_units("unitless") %>%
      add_comments("Non-intermittent technologies from A23.globaltech_capacity_factor and values interpolated from assumptions") %>%
      add_legacy_name("L223.GlobalTechCapFac_elec") %>%
      add_precursors("energy/A23.globaltech_capacity_factor", "energy/A23.globalinttech") ->
      L223.GlobalTechCapFac_elec

    L223.GlobalIntTechCapFac_elec %>%
      add_title("Capacity factor for intermittent electricity sector technologies") %>%
      add_units("unitless") %>%
      add_comments("Intermittent technologies from A23.globaltech_capacity_factor and values interpolated from assumptions") %>%
      add_legacy_name("L223.GlobalIntTechCapFac_elec") %>%
      add_precursors("energy/A23.globaltech_capacity_factor", "energy/A23.globalinttech") ->
      L223.GlobalIntTechCapFac_elec

    L223.GlobalTechCapital_elec %>%
      add_title("Overnight capital costs for non-intermittent electricity sector technologies") %>%
      add_units("1975$US/kw") %>%
      add_comments("Non-intermittent technologies from L113.globaltech_capital_ATB and values interpolated from assumptions") %>%
      add_comments("Values determined by an exponential function with terms for minimum achievable cost and pace of reduction") %>%
      add_legacy_name("L223.GlobalTechCapital_elec") %>%
      add_precursors("L113.globaltech_capital_ATB", "energy/A23.globalinttech") ->
      L223.GlobalTechCapital_elec

    L223.GlobalIntTechCapital_elec %>%
      add_title("Overnight capital costs for intermittent electricity sector technologies") %>%
      add_units("1975$US/kW") %>%
      add_comments("Intermittent technologies from L113.globaltech_capital_ATB and values interpolated from assumptions") %>%
      add_comments("Values determined by an exponential function with terms for minimum achievable cost and pace of reduction") %>%
      add_legacy_name("L223.GlobalIntTechCapital_elec") %>%
      add_precursors("L113.globaltech_capital_ATB", "energy/A23.globalinttech") ->
      L223.GlobalIntTechCapital_elec

    L223.GlobalTechOMfixed_elec %>%
      add_title("Fixed operation and maintenance costs of non-intermittent electricity sector technologies") %>%
      add_units("1975$US/kW/year") %>%
      add_comments("Values extrapolated from assumptions in L113.globaltech_OMfixed_ATB, filtering out any technologies requiring intermittent backup") %>%
      add_legacy_name("L223.GlobalTechOMfixed_elec") %>%
      add_precursors("L113.globaltech_OMfixed_ATB", "energy/A23.globalinttech") ->
      L223.GlobalTechOMfixed_elec

    L223.GlobalIntTechOMfixed_elec %>%
      add_title("Fixed operation and maintenance costs of intermittent electricity sector technologies") %>%
      add_units("1975$US/kW/yr") %>%
      add_comments("Values interpolated from assumptions in L113.globaltech_OMfixed_ATB for technologies requiring intermittent backup") %>%
      add_legacy_name("L223.GlobalIntTechOMfixed_elec") %>%
      add_precursors("L113.globaltech_OMfixed_ATB", "energy/A23.globalinttech") ->
      L223.GlobalIntTechOMfixed_elec

    L223.GlobalTechOMvar_elec %>%
      add_title("Variable operation and maintenance costs of non-intermittent electricity sector technologies") %>%
      add_units("1975$US/MWh") %>%
      add_comments("Values interpolated from assumptions in L113.globaltech_OMvar_ATB") %>%
      add_legacy_name("L223.GlobalTechOMvar_elec") %>%
      add_precursors("L113.globaltech_OMvar_ATB", "energy/A23.globalinttech") ->
      L223.GlobalTechOMvar_elec

    L223.GlobalIntTechOMvar_elec %>%
      add_title("Variable operation and maintenance costs of intermittent electricity sector technologies") %>%
      add_units("1975$US/MWh") %>%
      add_comments("Values interpolated from assumptions in L113.globaltech_OMvar_ATB") %>%
      add_legacy_name("L223.GlobalIntTechOMvar_elec") %>%
      add_precursors("L113.globaltech_OMvar_ATB", "energy/A23.globalinttech") ->
      L223.GlobalIntTechOMvar_elec

    L223.GlobalTechShrwt_elec %>%
      add_title("Global shareweights for non-intermittent technologies for the electricity sector") %>%
      add_units("unitless") %>%
      add_comments("Interpolated from model assumptions in A23.globaltech_shrwt") %>%
      add_legacy_name("L223.GlobalTechShrwt_elec") %>%
      add_precursors("energy/A23.globaltech_shrwt", "energy/A23.globalinttech") ->
      L223.GlobalTechShrwt_elec

    L223.GlobalTechInterp_elec %>%
      add_title("Interpolation rules for electricity technologies") %>%
      add_units("unitless") %>%
      add_comments("Model years applied to assumptions in A23.globaltech_interp") %>%
      add_legacy_name("L223.GlobalTechInterp_elec") %>%
      add_precursors("energy/A23.globaltech_interp") ->
      L223.GlobalTechInterp_elec

    L223.GlobalIntTechShrwt_elec %>%
      add_title("Global shareweights for intermittent technologies for the electricity sector") %>%
      add_units("unitless") %>%
      add_comments("Interpolated from model assumptions in A23.globaltech_shrwt") %>%
      add_legacy_name("L223.GlobalIntTechShrwt_elec") %>%
      add_precursors("energy/A23.globaltech_shrwt", "energy/A23.globalinttech") ->
      L223.GlobalIntTechShrwt_elec

    L223.PrimaryRenewKeyword_elec %>%
      add_title("keywords for non-intermittent renewable technologies for the electricity sector") %>%
      add_units("unitless") %>%
      add_comments("Subset from A23.globaltech_keyword") %>%
      add_legacy_name("L223.PrimaryRenewKeyword_elec") %>%
      add_precursors("energy/A23.globaltech_keyword") ->
      L223.PrimaryRenewKeyword_elec

    L223.PrimaryRenewKeywordInt_elec %>%
      add_title("keywords for intermittent renewable technologies") %>%
      add_units("unitless") %>%
      add_comments("Subset from A23.globaltech_keyword") %>%
      add_legacy_name("L223.PrimaryRenewKeywordInt_elec") %>%
      add_precursors("energy/A23.globaltech_keyword") ->
      L223.PrimaryRenewKeywordInt_elec

    L223.AvgFossilEffKeyword_elec %>%
      add_title("Average fossil fuel efficiency for electricity sector") %>%
      add_units("unitless") %>%
      add_comments("Subset from A23.globaltech_keyword") %>%
      add_legacy_name("L223.AvgFossilEffKeyword_elec") %>%
      add_precursors("energy/A23.globaltech_keyword") ->
      L223.AvgFossilEffKeyword_elec

    L223.GlobalTechCapture_elec %>%
      add_title("CO2 capture fractions from global technologies for electricity") %>%
      add_units("Unitless") %>%
      add_comments("Fraction of CO2 captured by global CCS tech in electricity interpolated from assumptions in A23.globaltech_co2capture") %>%
      add_legacy_name("L223.GlobalTechCapture_elec") %>%
      add_precursors("energy/A23.globaltech_co2capture") ->
      L223.GlobalTechCapture_elec

    L223.GlobalIntTechBackup_elec %>%
      add_title("Capital costs of backup technologies for intermittent techs") %>%
      add_units("1975 USD/kW/yr") %>%
      add_comments("Assumptions contained within A23.globalinttech") %>%
      add_legacy_name("L223.GlobalIntTechBackup_elec") %>%
      add_precursors("energy/A23.globalinttech") ->
      L223.GlobalIntTechBackup_elec

    L223.StubTechCapFactor_elec %>%
      add_title("Capacity factors of stub technologies including wind and solar") %>%
      add_units("unitless fraction") %>%
      add_comments("Average annual utilization of renewable technologies, calculated using regional solar irradiance data and wind resource curves to adjust default assumptions") %>%
      add_legacy_name("L223.StubTechCapFactor_elec") %>%
      add_precursors("common/GCAM_region_names", "L114.RsrcCurves_EJ_R_wind", "L119.Irradiance_rel_R", "L113.globaltech_capital_ATB", "L113.globaltech_OMfixed_ATB", "L113.globaltech_OMvar_ATB", "energy/A23.globalinttech") ->
      L223.StubTechCapFactor_elec

    L223.StubTechCost_offshore_wind %>%
      add_title("Cost of offshore wind") %>%
      add_units("unitless") %>%
      add_comments("Regional non-energy cost adder for offshore wind grid connection cost") %>%
      add_precursors("common/GCAM_region_names", "L114.RsrcCurves_EJ_R_wind", "L119.Irradiance_rel_R", "L113.globaltech_capital_ATB", "L113.globaltech_OMfixed_ATB", "L113.globaltech_OMvar_ATB", "energy/A23.globalinttech", "L223.StubTechCapFactor_elec", "L120.RegCapFactor_offshore_wind", "L120.GridCost_offshore_wind") ->
      L223.StubTechCost_offshore_wind

    if(exists("L223.GlobalTechShutdown_elec")) {
      L223.GlobalTechShutdown_elec %>%
        add_title("Global tech lifetime for techs with shutdown rate") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that uses a phased retirement function") %>%
        add_legacy_name("L223.GlobalTechShutdown_elec") %>%
        add_precursors("energy/A23.globaltech_retirement") ->
        L223.GlobalTechShutdown_elec
    } else {
      missing_data() %>%
        add_legacy_name("L223.GlobalTechShutdown_elec") ->
        L223.GlobalTechShutdown_elec
    }

    if(exists("L223.GlobalIntTechShutdown_elec")) {
      L223.GlobalIntTechShutdown_elec %>%
        add_title("Global tech lifetime for intermittent techs with shutdown rate") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that uses a phased retirement function") %>%
        add_legacy_name("L223.GlobalIntTechShutdown_elec") %>%
        add_precursors("energy/A23.globaltech_retirement") ->
        L223.GlobalIntTechShutdown_elec
    } else {
      missing_data() %>%
        add_legacy_name("L223.GlobalIntTechShutdown_elec") ->
        L223.GlobalIntTechShutdown_elec
    }

    if(exists("L223.GlobalTechSCurve_elec")) {
      L223.GlobalTechSCurve_elec %>%
        add_title("Global tech lifetime for techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_legacy_name("L223.GlobalTechSCurve_elec") %>%
        add_precursors("energy/A23.globaltech_retirement") ->
        L223.GlobalTechSCurve_elec
    } else {
      missing_data() %>%
        add_legacy_name("L223.GlobalTechSCurve_elec") ->
        L223.GlobalTechSCurve_elec
    }

    if(exists("L223.GlobalIntTechSCurve_elec")) {
      L223.GlobalIntTechSCurve_elec %>%
        add_title("Global tech lifetime for intermittent techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_legacy_name("L223.GlobalIntTechSCurve_elec") %>%
        add_precursors("energy/A23.globaltech_retirement") ->
        L223.GlobalIntTechSCurve_elec
    } else {
      missing_data() %>%
        add_legacy_name("L223.GlobalIntTechSCurve_elec") ->
        L223.GlobalIntTechSCurve_elec
    }

    if(exists("L223.GlobalTechLifetime_elec")) {
      L223.GlobalTechLifetime_elec %>%
        add_title("Global tech lifetime for any technology with no retirement function") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that has no phased or S-curve retirement function.") %>%
        add_legacy_name("L222.GlobalTechLifetime_elec") %>%
        add_precursors("energy/A23.globaltech_retirement") ->
        L223.GlobalTechLifetime_elec
    } else {
      missing_data() %>%
        add_legacy_name("L223.GlobalTechLifetime_elec") ->
        L223.GlobalTechLifetime_elec
    }

    if(exists("L223.GlobalTechLifetime_elec")) {
      L223.GlobalIntTechLifetime_elec %>%
        add_title("Global tech lifetime for any intermittent technology with no retirement function") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that has no phased or S-curve retirement function.") %>%
        add_legacy_name("L223.GlobalIntTechLifetime_elec") %>%
        add_precursors("energy/A23.globaltech_retirement") ->
        L223.GlobalIntTechLifetime_elec
    } else {
      missing_data() %>%
        add_legacy_name("L223.GlobalIntTechLifetime_elec") ->
        L223.GlobalIntTechLifetime_elec
    }

    if(exists("L223.GlobalTechProfitShutdown_elec")) {
      L223.GlobalTechProfitShutdown_elec %>%
        add_title("Global tech profit shutdown decider and parameters") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_legacy_name("L223.GlobalTechProfitShutdown_elec") %>%
        add_precursors("energy/A23.globaltech_retirement") ->
        L223.GlobalTechProfitShutdown_elec
    } else {
      missing_data() %>%
        add_legacy_name("L223.GlobalTechProfitShutdown_elec") ->
        L223.GlobalTechProfitShutdown_elec
    }

    if(exists("L223.GlobalIntTechProfitShutdown_elec")) {
      L223.GlobalIntTechProfitShutdown_elec %>%
        add_title("Global tech profit shutdown decider and parameters for intermittent technologies") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_legacy_name("L223.GlobalIntTechProfitShutdown_elec") %>%
        add_precursors("energy/A23.globaltech_retirement") ->
        L223.GlobalIntTechProfitShutdown_elec
    } else {
      missing_data() %>%
        add_legacy_name("L223.GlobalIntTechProfitShutdown_elec") ->
        L223.GlobalIntTechProfitShutdown_elec
    }

    L223.StubTechCalInput_elec %>%
      add_title("calibrated input values and shareweights for electricity sector by subsector and stub technology for base years") %>%
      add_units("Exajoules/year") %>%
      add_comments("Matched to list of calibrated techs and input values calculated from L1231.in_EJ_R_elec_F_tech_Yh") %>%
      add_legacy_name("L223.StubTechCalInput_elec") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs", "L1231.in_EJ_R_elec_F_tech_Yh") ->
      L223.StubTechCalInput_elec

    L223.StubTechFixOut_elec %>%
      add_title("Fixed output of electricity generation by region in all base years") %>%
      add_units("Exajoules/year") %>%
      add_comments("Interpolated from historical year calibrated outputs and filtered list to technologies with a fixed output. By default, this is hydropower.") %>%
      add_legacy_name("L223.StubTechFixOut_elec") %>%
      add_precursors("common/GCAM_region_names", "energy/calibrated_techs", "L1231.in_EJ_R_elec_F_tech_Yh") ->
      L223.StubTechFixOut_elec

    L223.StubTechFixOut_hydro %>%
      add_title("Fixed output of hydropower electricity generation by region in future years") %>%
      add_units("Exajoules (EJ)/year") %>%
      add_comments("Interpolated from projected fixed output values in L118.out_EJ_R_elec_hydro_Yfut and fixed shareweights applied") %>%
      add_legacy_name("L223.StubTechFixOut_hydro") %>%
      add_precursors("energy/calibrated_techs", "common/GCAM_region_names", "L118.out_EJ_R_elec_hydro_Yfut") ->
      L223.StubTechFixOut_hydro

    L223.StubTechProd_elec %>%
      add_title("Historical calibrated outputs and shareweights of electricity sector by subsector and technology") %>%
      add_units("Exajoules (EJ)/year") %>%
      add_comments("Calibrated historical outputs interpolated from historical values and shareweights applied (1 to non-zero outputs, else 0") %>%
      add_legacy_name("L223.StubTechProd_elec") %>%
      add_precursors("L1231.out_EJ_R_elec_F_tech_Yh", "energy/calibrated_techs", "common/GCAM_region_names") ->
      L223.StubTechProd_elec

    L223.StubTechEff_elec %>%
      add_title("conversion efficiency of fuel to energy in electricity sector") %>%
      add_units("unitless") %>%
      add_comments("") %>%
      add_legacy_name("L223.StubTechEff_elec") %>%
      add_precursors("energy/calibrated_techs", "common/GCAM_region_names", "L1231.eff_R_elec_F_tech_Yh", "energy/A23.globaltech_eff") ->
      L223.StubTechEff_elec

    L223.StubTechSecOut_desal %>%
      add_title("secondary output of desalinated water from the electricity sector") %>%
      add_units("m^3/GJ") %>%
      add_comments("This only applies in regions with combined electric + desalination plants") %>%
      add_precursors("energy/calibrated_techs", "common/GCAM_region_names", "water/EFW_mapping", "L1232.desalsecout_R_elec_F_tech") ->
      L223.StubTechSecOut_desal

    L223.GlobalTechCapital_sol_adv %>%
      add_title("high tech/low cost solar capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_legacy_name("L223.GlobalTechCapital_sol_adv") %>%
      add_precursors("L113.globaltech_capital_ATB_adv") ->
      L223.GlobalTechCapital_sol_adv

    L223.GlobalIntTechCapital_sol_adv %>%
      add_title("high tech/low cost intermittent solar capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_legacy_name("L223.GlobalIntTechCapital_sol_adv") %>%
      add_precursors("L113.globaltech_capital_ATB_adv") ->
      L223.GlobalIntTechCapital_sol_adv

    L223.GlobalTechCapital_wind_adv %>%
      add_title("high tech/low cost wind capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_legacy_name("L223.GlobalTechCapital_wind_adv") %>%
      add_precursors("L113.globaltech_capital_ATB_adv") ->
      L223.GlobalTechCapital_wind_adv

    L223.GlobalIntTechCapital_wind_adv %>%
      add_title("high tech/low cost intermittent wind capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_legacy_name("L223.GlobalIntTechCapital_wind_adv") %>%
      add_precursors("L113.globaltech_capital_ATB_adv") ->
      L223.GlobalIntTechCapital_wind_adv

    L223.GlobalTechCapital_geo_adv %>%
      add_title("high tech/low cost geothermal capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_legacy_name("L223.GlobalTechCapital_geo_adv") %>%
      add_precursors("L113.globaltech_capital_ATB_adv") ->
      L223.GlobalTechCapital_geo_adv

    L223.GlobalTechCapital_nuc_adv %>%
      add_title("high tech/low cost nuclear capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_legacy_name("L223.GlobalTechCapital_nuc_adv") %>%
      add_precursors("L113.globaltech_capital_ATB_adv") ->
      L223.GlobalTechCapital_nuc_adv

    L223.GlobalTechCapital_sol_low %>%
      add_title("Low tech/high cost solar capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_legacy_name("L223.GlobalTechCapital_sol_low") %>%
      add_precursors("L113.globaltech_capital_ATB_low") ->
      L223.GlobalTechCapital_sol_low

    L223.GlobalIntTechCapital_sol_low %>%
      add_title("Low tech/high cost intermittent solar capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_legacy_name("L223.GlobalIntTechCapital_sol_low") %>%
      add_precursors("L113.globaltech_capital_ATB_low") ->
      L223.GlobalIntTechCapital_sol_low

    L223.GlobalTechCapital_wind_low %>%
      add_title("Low tech/high cost wind capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_legacy_name("L223.GlobalTechCapital_wind_low") %>%
      add_precursors("L113.globaltech_capital_ATB_low") ->
      L223.GlobalTechCapital_wind_low

    L223.GlobalIntTechCapital_wind_low %>%
      add_title("Low tech/high cost intermittent wind capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_legacy_name("L223.GlobalIntTechCapital_wind_low") %>%
      add_precursors("L113.globaltech_capital_ATB_low") ->
      L223.GlobalIntTechCapital_wind_low

    L223.GlobalTechCapital_geo_low %>%
      add_title("Low tech/high cost geothermal capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_legacy_name("L223.GlobalTechCapital_geo_low") %>%
      add_precursors("L113.globaltech_capital_ATB_low") ->
      L223.GlobalTechCapital_geo_low

    L223.GlobalTechCapital_nuc_low %>%
      add_title("Low tech/high cost nuclear capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_legacy_name("L223.GlobalTechCapital_nuc_low") %>%
      add_precursors("L113.globaltech_capital_ATB_low") ->
      L223.GlobalTechCapital_nuc_low

    L223.GlobalTechCapital_bio_low %>%
      add_title("Low tech/high cost bioenergy capital costs for the electricity sector") %>%
      add_units("capital overnight - 1975USD/GJ, capacity factor - unitless, fixed.charge.rate - unitless") %>%
      add_comments("Capacity factor - average percent use of maximum rated output") %>%
      add_comments("Fixed charge rate - conversion from overnight capital cost to amortized annual payment") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.GlobalTechCapital_bio_low") %>%
      add_precursors("L113.globaltech_capital_ATB_low") ->
      L223.GlobalTechCapital_bio_low

    return_data(L223.Supplysector_elec, L223.ElecReserve, L223.SubsectorLogit_elec, L223.SubsectorShrwt_elec,
     L223.SubsectorShrwtFllt_elec, L223.SubsectorShrwt_coal, L223.SubsectorShrwt_nuc, L223.SubsectorShrwt_renew,
      L223.SubsectorInterp_elec, L223.SubsectorInterpTo_elec, L223.StubTech_elec,
      L223.GlobalIntTechEff_elec, L223.GlobalTechEff_elec, L223.GlobalTechCapFac_elec,
      L223.GlobalIntTechCapFac_elec, L223.GlobalTechCapital_elec, L223.GlobalIntTechCapital_elec,
      L223.GlobalTechOMfixed_elec, L223.GlobalIntTechOMfixed_elec, L223.GlobalTechOMvar_elec,
      L223.GlobalIntTechOMvar_elec, L223.GlobalTechShrwt_elec, L223.GlobalTechInterp_elec,
      L223.GlobalIntTechShrwt_elec, L223.PrimaryRenewKeyword_elec, L223.PrimaryRenewKeywordInt_elec,
       L223.AvgFossilEffKeyword_elec, L223.GlobalTechCapture_elec, L223.GlobalIntTechBackup_elec,
       L223.StubTechCapFactor_elec,L223.StubTechCost_offshore_wind, L223.GlobalTechShutdown_elec,
       L223.GlobalIntTechShutdown_elec, L223.GlobalTechSCurve_elec, L223.GlobalIntTechSCurve_elec,
       L223.GlobalTechLifetime_elec, L223.GlobalIntTechLifetime_elec, L223.GlobalTechProfitShutdown_elec,
       L223.GlobalIntTechProfitShutdown_elec, L223.StubTechCalInput_elec, L223.StubTechFixOut_elec,
       L223.StubTechFixOut_hydro, L223.StubTechProd_elec, L223.StubTechEff_elec, L223.StubTechSecOut_desal,
       L223.GlobalTechCapital_sol_adv, L223.GlobalIntTechCapital_sol_adv, L223.GlobalTechCapital_wind_adv,
        L223.GlobalIntTechCapital_wind_adv, L223.GlobalTechCapital_geo_adv, L223.GlobalTechCapital_nuc_adv,
        L223.GlobalTechCapital_sol_low, L223.GlobalIntTechCapital_sol_low, L223.GlobalTechCapital_wind_low,
        L223.GlobalIntTechCapital_wind_low, L223.GlobalTechCapital_geo_low, L223.GlobalTechCapital_nuc_low,
        L223.GlobalTechCapital_bio_low)
  } else {
    stop("Unknown command")
  }
}
