# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L270.limits
#'
#' Generate GCAM policy constraints which limit model behavior in some way.  In
#' particular limit the fraction of liquid feedstocks and inputs to electricity
#' generation which can come from sources other than crude oil.  Constrain the
#' total amount of subsidy as a fraction of GDP which an economy is will to give
#' to have net negative emissions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L270.CreditOutput}, \code{L270.CreditInput_elec}, \code{L270.CreditInput_feedstocks}, \code{L270.CreditMkt}, \code{L270.CTaxInput}, \code{L270.LandRootNegEmissMkt}, \code{L270.NegEmissBudgetMaxPrice}, \code{paste0("L270.NegEmissBudget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5))) )}. The corresponding file in the
#' original data system was \code{L270.limits.R} (energy level2).
#' @details Generate GCAM policy constraints which enforce limits to liquid feedstocks
#' and the amount of subsidies given for net negative emissions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by mutate select summarize
#' @importFrom magrittr %<>%
#' @author PLP March 2018
module_energy_L270.limits <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A23.globaltech_eff",
             FILE = "emissions/A_CDRU",
             FILE = "energy/A27.rsrc_info",
             FILE = "energy/A27.GrdRenewRsrcCurves",
             "L102.gdp_mil90usd_GCAM3_R_Y",
             "L102.gdp_mil90usd_Scen_R_Y",
             "L221.GlobalTechCoef_en",
             "L202.CarbonCoef",
             "L2012.AgYield_bio_ref"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L270.CreditOutput",
             "L270.CreditInput_elec",
             "L270.CreditInput_feedstocks",
             "L270.CreditMkt",
             "L270.CTaxInput",
             "L270.LandRootNegEmissMkt",
             "L270.NegEmissBudgetMaxPrice",
             # TODO: might just be easier to keep the scenarios in a single
             # table here and split when making XMLs but to match the old
             # data system we will split here
             paste0("L270.NegEmissBudget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)) ),
             "L270.RenewRsrc",
             "L270.RenewRsrcPrice",
             "L270.GrdRenewRsrcCurves",
             "L270.GrdRenewRsrcMax",
             "L270.ResTechShrwt",
             "L270.AgCoef_bioext"))
  } else if(command == driver.MAKE) {

    value <- subsector <- supplysector <- year <- GCAM_region_ID <- sector.name <-
      region <- scenario <- constraint <- . <- PrimaryFuelCO2Coef.name <-
      PrimaryFuelCO2Coef <- technology <- coefficient <- coef <- market <-
      resource_type <- resource <- `output-unit` <- `price-unit` <- subresource <-
      grade <- available <- extractioncost <- renewresource <- sub.renewable.resource <-
      AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <- NULL # silence package check notes

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    A_CDRU <- get_data(all_data, "emissions/A_CDRU")
    A27.rsrc_info <- get_data(all_data, "energy/A27.rsrc_info", strip_attributes = TRUE)
    A27.GrdRenewRsrcCurves <- get_data(all_data, "energy/A27.GrdRenewRsrcCurves", strip_attributes = TRUE)
    L102.gdp_mil90usd_GCAM3_R_Y <- get_data(all_data, "L102.gdp_mil90usd_GCAM3_R_Y", strip_attributes = TRUE)
    L102.gdp_mil90usd_Scen_R_Y <- get_data(all_data, "L102.gdp_mil90usd_Scen_R_Y")
    L102.gdp_mil90usd_Scen_R_Y <- get_data(all_data, "L102.gdp_mil90usd_Scen_R_Y")
    L221.GlobalTechCoef_en <- get_data(all_data, "L221.GlobalTechCoef_en", strip_attributes = TRUE)
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef")
    L2012.AgYield_bio_ref <- get_data(all_data, "L2012.AgYield_bio_ref", strip_attributes = TRUE)

    # Limit bioliquids for feedstocks and electricity
    # Note: we do this by requiring a certain fraction of inputs to those technologies to come from oil
    # L270.CreditOutput: Secondary output of oil credits
    tibble(sector.name = "refining",
           subsector.name = "oil refining",
           technology = "oil refining") %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(res.secondary.output = energy.OIL_CREDITS_MARKETNAME,
             output.ratio = 1.0) ->
      L270.CreditOutput

    # L270.CreditInput_elec: minicam-energy-input of oil credits for electricity techs
    A23.globaltech_eff %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      mutate(value = round(value, energy.DIGITS_EFFICIENCY)) %>%
      filter(subsector == "refined liquids") %>%
      mutate(minicam.energy.input = energy.OIL_CREDITS_MARKETNAME,
      # note we are converting the efficiency to a coefficient here
             coefficient = energy.OILFRACT_ELEC / value) %>%
      select(-value) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) ->
      L270.CreditInput_elec

    # L270.CreditInput_feedstocks: minicam-energy-input of oil credits for feedstock techs
    tibble(sector.name = "industrial feedstocks",
           subsector.name = "refined liquids",
           technology = "refined liquids") %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(minicam.energy.input = energy.OIL_CREDITS_MARKETNAME,
             coefficient = energy.OILFRACT_FEEDSTOCKS) ->
      L270.CreditInput_feedstocks

    # L270.CreditMkt: Market for oil credits
    tibble(region = GCAM_region_names$region,
           policy.portfolio.standard = energy.OIL_CREDITS_MARKETNAME,
           market = "global",
           policyType = "RES") %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(constraint = 1,
             price.unit = "1975$/GJ",
             output.unit = "EJ") ->
      L270.CreditMkt

    # Create the negative emissions GDP budget constraint limits

    # Create a usable GDP by region + scenario + year
    L102.gdp_mil90usd_GCAM3_R_Y %>%
      mutate(scenario = "GCAM3") %>%
      bind_rows(L102.gdp_mil90usd_Scen_R_Y) %>%
      left_join_error_no_match(GCAM_region_names, by = c("GCAM_region_ID")) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(-GCAM_region_ID) ->
      GDP_scenario

    # L270.CTaxInput: Create ctax-input for all biomass
    L221.GlobalTechCoef_en %>%
      filter(grepl("(biomass|ethanol)", sector.name)) %>%
      mutate(ctax.input = energy.NEG_EMISS_POLICY_NAME) %>%
      left_join_error_no_match(L202.CarbonCoef %>%
                                 distinct(PrimaryFuelCO2Coef.name, PrimaryFuelCO2Coef),
                               by = c("sector.name" = "PrimaryFuelCO2Coef.name")) %>%
      rename(fuel.C.coef = PrimaryFuelCO2Coef) ->
	  L270.CTaxInput

    L270.CTaxInput <- L270.CTaxInput[, c(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "ctax.input", "fuel.C.coef")]


    A_CDRU %>%
      gather_years("coef") -> A_CDRU

    #create a coefficient table for airCO2 only, analogous to L221.GlobalTechCoef_en (where we don't want airCO2 showing up), which will be appended to L270.CTaxInput
    A_CDRU %>%
      select(supplysector, subsector, technology) %>%
      distinct %>%
      # Interpolate to all years
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      left_join(A_CDRU, by = c("supplysector", "subsector", "technology", "year")) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(coefficient = approx_fun(year, value = coef, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, year, coefficient) -> L270.GlobalTechCoef_cdr

    L270.GlobalTechCoef_cdr %>%
      mutate(ctax.input = energy.NEG_EMISS_POLICY_NAME)  %>%
      rename(fuel.C.coef = coefficient)-> L270.GlobalTechCoef_cdr

    L270.CTaxInput %>%
      bind_rows(L270.GlobalTechCoef_cdr) -> L270.CTaxInput


    # L270.LandRootNegEmissMkt: set the negative emissions policy name into the LandAllocator root
    # so it can make it available to all land leaves under a UCT
    tibble(region = GCAM_region_names$region,
           LandAllocatorRoot = "root",
           negative.emiss.market = energy.NEG_EMISS_POLICY_NAME) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->
      L270.LandRootNegEmissMkt

    # L270.NegEmissBudget: Create the budget for paying for net negative emissions
    GDP_scenario %>%
      # no dollar year or unit conversions since emissions already match
      mutate(constraint = value * energy.NEG_EMISS_GDP_BUDGET_PCT,
             policy.portfolio.standard = energy.NEG_EMISS_POLICY_NAME,
             policyType = "tax",
             market = region,
             price.unit = "%",
             output.unit = "mil 1990$") %>%
      select(-value) %>%
      # constrain in only years which could include a carbon price
      filter(year >= 2020) ->
      L270.NegEmissBudget

    # Create a table for max price which gives a hint to the solver the price of
    # this market is 0 <= p <= 1
    tibble(region = GCAM_region_names$region,
           policy.portfolio.standard = energy.NEG_EMISS_POLICY_NAME,
           max.price = 1.0) ->
      L270.NegEmissBudgetMaxPrice

    if(energy.NEG_EMISS_MARKT_GLOBAL) {
      # when the negative emissions budget is global we need to aggregate
      # the constraint across regions
      L270.NegEmissBudget %>%
        group_by(scenario, year) %>%
        summarize(constraint = sum(constraint)) %>%
        ungroup() %>%
        left_join_error_no_match(select(L270.NegEmissBudget, -constraint), ., by = c("scenario", "year")) %>%
        mutate(market = "global") ->
        L270.NegEmissBudget
    }

    # create the biomass externality cost constraint tibbles which will tack on an additional
    # penalty for use of biomass given the total level of deployment

    # A. Output unit, price unit, market
    L270.rsrc_info <- A27.rsrc_info %>%
      # Repeat and add region to resource assumptions table
      repeat_add_columns(select(GCAM_region_names, region)) %>%
      # Reset regional markets to the names of the specific regions
      mutate(market = if_else(market == "regional", region, market))

    # L270.RenewRsrc: output unit, price unit, and market for renewable resources
    L270.RenewRsrc <- L270.rsrc_info %>%
      filter(resource_type == "renewresource") %>%
      select(region, renewresource = resource, output.unit = `output-unit`, price.unit = `price-unit`, market) %>%
      distinct()

    # L210.RenewRsrcPrice: initial prices for renewable resources
    L270.RenewRsrcPrice <- L270.rsrc_info %>%
      gather_years() %>%
      filter(resource_type == "renewresource",
             year %in% MODEL_BASE_YEARS) %>%
      select(region, renewresource = resource, year, price = value)

    L270.GrdRenewRsrcCurves <- A27.GrdRenewRsrcCurves %>%
      # Add region name
      mutate(region = gcam.USA_REGION) %>%
      select(region, renewresource = resource, sub.renewable.resource = subresource, grade, available, extractioncost)

    L270.GrdRenewRsrcCurves %>%
      select(region, renewresource, sub.renewable.resource) %>%
      distinct() %>%
      mutate(year.fillout = MODEL_BASE_YEARS[1],
             maxSubResource = 1.0) ->
      L270.GrdRenewRsrcMax

    L270.GrdRenewRsrcCurves %>%
      select(region, renewresource, sub.renewable.resource) %>%
      distinct() %>%
      mutate(technology = renewresource) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1.0) %>%
      rename(resource = renewresource,
             subresource = sub.renewable.resource) ->
      L270.ResTechShrwt

    L2012.AgYield_bio_ref %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology) %>%
      unique() %>%
      mutate(minicam.energy.input = unique(L270.rsrc_info$resource),
             coefficient = 1.0) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[['AgCoef']]) ->
      L270.AgCoef_bioext


    # Produce outputs
    L270.CreditOutput %>%
      add_title("Creates the supply of oil credits") %>%
      add_units("Unitless") %>%
      add_comments("Adds a secondary output to oil refining to generate oil-credits") %>%
      add_comments("that are consumed by electricity and feedstocks") %>%
      add_legacy_name("L270.CreditOutput") ->
      L270.CreditOutput

    L270.CreditInput_elec %>%
      add_title("Creates demand of oil credits in electricity") %>%
      add_units("Elec coef * constraint") %>%
      add_comments("Consumes oil-credits limiting the blend of refined") %>%
      add_comments("liquids that can be used generate electricity") %>%
      add_legacy_name("L270.CreditInput_elec") %>%
      add_precursors("energy/A23.globaltech_eff") ->
      L270.CreditInput_elec

    L270.CreditInput_feedstocks %>%
      add_title("Creates demand of oil credits in feedstocks") %>%
      add_units("% constraint") %>%
      add_comments("Consumes oil-credits limiting the blend of refined") %>%
      add_comments("liquids that can be used generate electricity") %>%
      add_legacy_name("L270.CreditInput_feedstocks") ->
      L270.CreditInput_feedstocks

    L270.CreditMkt %>%
      add_title("Sets up the oil-credits RES market") %>%
      add_units("NA") %>%
      add_comments("Boiler plate and units for creating the actual") %>%
      add_comments("market for balancing oil-credits") %>%
      add_legacy_name("L270.CreditMkt") %>%
      add_precursors("common/GCAM_region_names") ->
      L270.CreditMkt

    L270.CTaxInput %>%
      add_title("Creates the ctax-input limiting the amount of negative emissions") %>%
      add_units("kgC/GJ") %>%
      add_comments("Add ctax-input to all of the bio-energy supply sectors") %>%
      add_comments("by using L221.GlobalTechCoef_en filtered by biomass|ethanol") %>%
      add_comments("and joining carbon coefficients from L202.CarbonCoef") %>%
      add_comments("airCO2 is also appended for direct air capture (i.e. negative emissions not from biomass)") %>%
      add_legacy_name("L270.CTaxInput") %>%
      add_precursors("L221.GlobalTechCoef_en",
                     "L202.CarbonCoef",
                     "emissions/A_CDRU") ->
      L270.CTaxInput

    L270.LandRootNegEmissMkt %>%
      add_title("Sets the negative emissions policy name into the land system") %>%
      add_units("NA") %>%
      add_comments("Sets the policy name in the root which will handle passing it") %>%
      add_comments("down to all the leaves. This allows us to scale back the carbon") %>%
      add_comments("subsidy given in the land system if we have too many negative emissions") %>%
      add_precursors("common/GCAM_region_names") ->
      L270.LandRootNegEmissMkt

    L270.NegEmissBudgetMaxPrice %>%
      add_title("A hint for the solver for what the max price of this market is") %>%
      add_units("%") %>%
      add_comments("This value is just used to give the solver a hint of the") %>%
      add_comments("range of prices which are valid.  For the negative emissions") %>%
      add_comments("budget constraint the price is a fraction from 0 to 1") %>%
      add_legacy_name("L270.NegEmissBudgetMaxPrice") %>%
      add_precursors("common/GCAM_region_names") ->
      L270.NegEmissBudgetMaxPrice

    L270.RenewRsrc %>%
      add_title("The biomass externality cost resource") %>%
      add_units("NA") %>%
      add_comments("Creates the renewable resource which maps purpose grown biomass") %>%
      add_comments("deployment to an additional cost meant to reflect various externalities.") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A27.rsrc_info") ->
      L270.RenewRsrc

    L270.RenewRsrcPrice %>%
      add_title("Prices for the biomass externality cost resource") %>%
      add_units("1975$/GJ") %>%
      add_comments("Initial price guesses") %>%
      same_precursors_as(L270.RenewRsrc) ->
      L270.RenewRsrcPrice

    L270.GrdRenewRsrcCurves %>%
      add_title("Graded renewable supply curve for the biomass externality cost resource") %>%
      add_units("available: EJ; extractioncost: 1975$/GJ") %>%
      add_comments("Creates the renewable resource which maps purpose grown biomass") %>%
      add_comments("deployment to an additional cost meant to reflect various externalities.") %>%
      add_precursors("energy/A27.rsrc_info",
                     "energy/A27.GrdRenewRsrcCurves") ->
      L270.GrdRenewRsrcCurves

    L270.GrdRenewRsrcMax %>%
      add_title("Graded renewable max resource for the biomass externality cost resource") %>%
      add_units("NA") %>%
      add_comments("Note: the max resource is just set to one and available quantities are set") %>%
      add_comments("in L270.GrdRenewRsrcCurves") %>%
      add_precursors("energy/A27.GrdRenewRsrcCurves") ->
      L270.GrdRenewRsrcMax

    L270.ResTechShrwt %>%
      add_title("Technology for the biomass externality cost resource") %>%
      add_units("NA") %>%
      add_comments("Share weights won't matter for resource technologies as there") %>%
      add_comments("is no competetion between technologies.") %>%
      same_precursors_as(L270.GrdRenewRsrcMax) ->
      L270.ResTechShrwt

    L270.AgCoef_bioext %>%
      add_title("Inputs to add the cost for the biomass externality cost to purpose grown biomass production") %>%
      add_units("NA") %>%
      add_comments("coefficients are just set to 1 as the resource maps production to an addtional cost") %>%
      add_precursors("energy/A27.rsrc_info",
                     "L2012.AgYield_bio_ref") ->
      L270.AgCoef_bioext

    ret_data <- c("L270.CreditOutput", "L270.CreditInput_elec", "L270.CreditInput_feedstocks", "L270.CreditMkt", "L270.CTaxInput", "L270.LandRootNegEmissMkt", "L270.NegEmissBudgetMaxPrice",
                  "L270.RenewRsrc",
                  "L270.RenewRsrcPrice",
                  "L270.GrdRenewRsrcCurves",
                  "L270.GrdRenewRsrcMax",
                  "L270.ResTechShrwt",
                  "L270.AgCoef_bioext")
    # We will generate a bunch of tibbles for the negative emissions budgets for each scenario
    # and use assign() to save them to variables with names as L270.NegEmissBudget_[SCENARIO]
    # Note that since the call to assign() is in the for loop we must explicitly set the
    # environment to just outside of the loop:
    curr_env <- environment()
    for(scen in unique(L270.NegEmissBudget$scenario)) {
      curr_data_name <- paste0("L270.NegEmissBudget_", scen)
      L270.NegEmissBudget %>%
        filter(scenario == scen) %>%
        select(-scenario) %>%
        add_title(paste0("The negative emissions budget in scenario ", scen)) %>%
        add_units("mil 1990$") %>%
        add_comments("The budget a market is willing to subsidize negative emissions") %>%
        add_legacy_name(curr_data_name) %>%
        add_precursors(if_else(scen == "GCAM3", "L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y")) %>%
        assign(curr_data_name, ., envir = curr_env)

      ret_data <- c(ret_data, curr_data_name)
    }

    # Call return_data but we need to jump through some hoops since we generated some of the
    # tibbles from the scenarios so we will generate the call to return_data
    ret_data %>%
      paste(collapse = ", ") %>%
      paste0("return_data(", ., ")") %>%
      parse(text = .) %>%
      eval()
  } else {
    stop("Unknown command")
  }
}
