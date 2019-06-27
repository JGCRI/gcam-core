#' module_energy_L261.Cstorage
#'
#' Calculate carbon storage resource supply curves, shareweights, technology coefficients and costs, and other carbon storage information.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L261.Rsrc}, \code{L261.UnlimitRsrc}, \code{L261.RsrcCurves_C}, \code{L261.SectorLogitTables[[ curr_table ]]$data}, \code{L261.Supplysector_C}, \code{L261.SubsectorLogitTables[[ curr_table ]]$data}, \code{L261.SubsectorLogit_C}, \code{L261.SubsectorShrwtFllt_C}, \code{L261.StubTech_C}, \code{L261.GlobalTechCoef_C}, \code{L261.GlobalTechCost_C}, \code{L261.GlobalTechShrwt_C}, \code{L261.GlobalTechCost_C_High}, \code{L261.GlobalTechShrwt_C_nooffshore}, \code{L261.RsrcCurves_C_high}, \code{L261.RsrcCurves_C_low}, \code{L261.RsrcCurves_C_lowest}. The corresponding file in the
#' original data system was \code{L261.Cstorage.R} (energy level2).
#' @details The following tables pertaining to carbon storage properties are generated:
#' \itemize{
#'  \item{Carbon storage information}
#'  \item{Unlimited carbon storage information}
#'  \item{Supply curve of carbon storage resources}
#'  \item{High supply curve of onshore carbon storage resources}
#'  \item{Low supply curve of onshore carbon storage resources}
#'  \item{Lowest supply curve of onshore carbon storage resources}
#'  \item{Carbon storage sector information}
#'  \item{Subsector logit exponents of carbon storage sector}
#'  \item{Subsector shareweights of carbon storage sectors}
#'  \item{Identification of stub technologies of carbon storage}
#'  \item{Carbon storage global technology coefficients across base model years}
#'  \item{Carbon storage global technology costs across base model years}
#'  \item{Carbon storage global technology costs across base model years, high price scenario}
#'  \item{Shareweights of carbon storage technologies across base model years}
#'  \item{Shareweights of offshore carbon storage technologies}
#' }
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AJS August 2017
module_energy_L261.Cstorage <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A61.rsrc_info",
             FILE = "energy/A61.sector",
             FILE = "energy/A61.subsector_logit",
             FILE = "energy/A61.subsector_shrwt",
             FILE = "energy/A61.globaltech_coef",
             FILE = "energy/A61.globaltech_cost",
             FILE = "energy/A61.globaltech_shrwt",
             "L161.RsrcCurves_MtC_R"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L261.Rsrc",
             "L261.UnlimitRsrc",
             "L261.RsrcCurves_C",
             "L261.ResTechShrwt_C",
             "L261.Supplysector_C",
             "L261.SubsectorLogit_C",
             "L261.SubsectorShrwtFllt_C",
             "L261.StubTech_C",
             "L261.GlobalTechCoef_C",
             "L261.GlobalTechCost_C",
             "L261.GlobalTechShrwt_C",
             "L261.GlobalTechCost_C_High",
             "L261.GlobalTechShrwt_C_nooffshore",
             "L261.RsrcCurves_C_high",
             "L261.RsrcCurves_C_low",
             "L261.RsrcCurves_C_lowest"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A61.rsrc_info <- get_data(all_data, "energy/A61.rsrc_info")
    A61.sector <- get_data(all_data, "energy/A61.sector")
    A61.subsector_logit <- get_data(all_data, "energy/A61.subsector_logit")
    A61.subsector_shrwt <- get_data(all_data, "energy/A61.subsector_shrwt")
    A61.globaltech_coef <- get_data(all_data, "energy/A61.globaltech_coef")
    A61.globaltech_cost <- get_data(all_data, "energy/A61.globaltech_cost")
    A61.globaltech_shrwt <- get_data(all_data, "energy/A61.globaltech_shrwt")
    L161.RsrcCurves_MtC_R <- get_data(all_data, "L161.RsrcCurves_MtC_R")

    # ===================================================

    # Silence package notes
    . <- available <- capacity.factor <- curr_table <- extractioncost <-
      grade <- logit.type <- minicam.energy.input <- minicam.non.energy.input <-
      `output-unit` <- `price-unit` <- resource <- resource_type <- share.weight <-
      subresource <- subsector <- subsector.name <- supplysector <- technology <-
      value <- year <- region <- resource <- output.unit <- price.unit <-
      market <- logit.exponent <- coefficient <- input.cost <- NULL

    # A
    # Create tables for carbon storage resource information
    # A61.rsrc_info provides carbon storage resource info (output unit, price unit, capacity factor, market, etc)
    A61.rsrc_info %>%
      # Expand table to incorporate GCAM region names (use ID to ensure correct region ordering)
      # We will use these specific region names to replace the broad term, regional, in the market column.
      repeat_add_columns(GCAM_region_names) %>%
      # Reset regional markets to the names of the specific regions
      mutate(market = replace(market, market == "regional", region[market == "regional"]),
             capacity.factor = as.numeric(capacity.factor)) %>%
      rename(output.unit = `output-unit`, price.unit = `price-unit`) ->
      L261.rsrc_info

    # Split different types of resources into separate tables

    # Create table reporting carbon storage information for unlimited resources only
    L261.rsrc_info %>%
      filter(resource_type == "unlimited-resource") %>%
      select(region, unlimited.resource = resource, output.unit, price.unit, market, capacity.factor) ->
      L261.UnlimitRsrc # This is a final ouput table.

    # Create table reporting carbon storage information for depletable resources only
    L261.rsrc_info %>%
      filter(resource_type == "resource") %>%
      select(region, resource = resource, output.unit, price.unit, market) ->
      L261.Rsrc # This is a final ouput table.


    # B
    # Supply curves of carbon storage resources
    # First, define number of decimal places
    DIGITS_COST <- 1

    # L161.RsrcCurves_MtC_R reports carbon storage resource supply curves by GCAM region.
    L161.RsrcCurves_MtC_R %>%
      # Match in GCAM region names using region ID
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(available = round(available, DIGITS_COST)) %>%
      select(region, resource = resource, subresource, grade, available, extractioncost) ->
      L261.RsrcCurves_C # This is a final output table.

    L261.RsrcCurves_C %>%
      select(region, resource = resource, subresource) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L261.ResTechShrwt_C


    # Calculate three different supply curves of carbon storage resources: high, low, and lowest.
    # Multiply the extraction cost by its respective multiplier below.
    # Note that the multipliers were created for the SSPs and that high, low, and lowest is the level of CCS use and not cost.
    HI_CCS_COST_MULT <- 0.8

    LO_CCS_COST_MULT <- 3

    LOWEST_CCS_COST_MULT <- 10

    # Note that these will produce final output tables.
    # High supply curves of carbon storage resources
    L261.RsrcCurves_C_high <- mutate(L261.RsrcCurves_C, extractioncost = extractioncost * HI_CCS_COST_MULT)

    # Low supply curves of carbon storage resources
    L261.RsrcCurves_C_low <- mutate(L261.RsrcCurves_C, extractioncost = extractioncost * LO_CCS_COST_MULT)

    # Lowest supply curves of carbon storage resources
    L261.RsrcCurves_C_lowest <- mutate(L261.RsrcCurves_C, extractioncost = extractioncost * LOWEST_CCS_COST_MULT)


    # C
    # Carbon storage sector information
    A61.sector %>%
      mutate(logit.exponent = as.numeric(logit.exponent)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names = GCAM_region_names) ->
      L261.Supplysector_C # This is a final output table.


    # D
    # Subsector information

    # Subsector logit exponents of carbon storage sector
    A61.subsector_logit %>%
      mutate(logit.exponent = as.numeric(logit.exponent)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names = GCAM_region_names) ->
      L261.SubsectorLogit_C # This is a final output table.


    # Subsector shareweights of carbon storage sectors
    A61.subsector_shrwt %>%
      mutate(share.weight = as.numeric(share.weight)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names = GCAM_region_names) ->
      L261.SubsectorShrwtFllt_C # This is a final output table.


    # E
    # Technology information
    # Identification of stub technologies of carbon storage
    # A61.globaltech_shrwt reports carbon storage technology shareweights
    # Note: assuming that the technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A61.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]],
                           GCAM_region_names = GCAM_region_names) %>%
      select(region, supplysector, subsector, stub.technology = technology) ->
      L261.StubTech_C # This is a final output table.

    # Energy inputs and coefficients of global technologies for carbon storage
    # A61.globaltech_coef reports carbon storage global technology coefficients
    A61.globaltech_coef %>%
      gather_years %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.energy.input)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(coefficient = approx_fun(year, value, rule = 2)) %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, year, minicam.energy.input, coefficient) ->
      L261.GlobalTechCoef_C # This is a final output table.

    # Costs of global technologies
    # A61.globaltech_cost reports carbon storage offshore storage cost (1975$/tCO2)
    A61.globaltech_cost %>%
      gather_years %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology, minicam.non.energy.input)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(input.cost = approx_fun(year, value, rule = 2)) %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, year, minicam.non.energy.input, input.cost) ->
      L261.GlobalTechCost_C # This is a final output table.

    # High costs of global technologies for carbon storage -- this prices out CCS
    L261.GlobalTechCost_C %>%
      mutate(subsector.name = "onshore carbon-storage",
             technology = "onshore carbon-storage") %>%
      bind_rows(L261.GlobalTechCost_C) %>%
      # Price out CCS by using storage cost that is very high (i.e., $10,000/tCO2)
      mutate(input.cost = 10000) -> # 1975$/tCO2
      L261.GlobalTechCost_C_High # This is a final output table.

    # Shareweights of global technologies for energy transformation
    A61.globaltech_shrwt %>%
      gather_years %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years outside of min-max range are assigned values from closest data, as opposed to NAs
      mutate(share.weight = approx_fun(year, value, rule = 2)) %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(sector.name = supplysector, subsector.name = subsector, technology, year, share.weight) ->
      L261.GlobalTechShrwt_C # This is a final output table.

    # Use zero shareweights for offshore storage
    L261.GlobalTechShrwt_C %>%
      filter(subsector.name == "offshore carbon-storage") %>%
      mutate(share.weight = 0) ->
      L261.GlobalTechShrwt_C_nooffshore # This is a final output table.

    # ===================================================

    L261.Rsrc %>%
      add_title("Carbon storage information") %>%
      add_units("Output unit as listed (MtC), price unit as listed (1990$/tC)") %>%
      add_comments("Carbon storage resource information was expanded to include GCAM region names") %>%
      add_comments("and filtered for only depletable resources") %>%
      add_legacy_name("L261.Rsrc") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.rsrc_info") ->
      L261.Rsrc

    L261.UnlimitRsrc %>%
      add_title("Unlimited carbon storage information") %>%
      add_units("Output unit as listed (MtC), price unit as listed (1990$/tC), capacity factor is unitless") %>%
      add_comments("Carbon storage resource information was expanded to include GCAM region names") %>%
      add_comments("and filtered for only unlimited resources (i.e., offshore)") %>%
      add_legacy_name("L261.UnlimitRsrc") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.rsrc_info") ->
      L261.UnlimitRsrc

    L261.RsrcCurves_C %>%
      add_title("Supply curve of carbon storage resources") %>%
      add_units("Available in MtCO2, Extraction Cost in 1990$/tCO2") %>%
      add_comments("GCAM region names were added to the resource supply curves generated in level 1") %>%
      add_legacy_name("L261.RsrcCurves_C") %>%
      add_precursors("common/GCAM_region_names", "L161.RsrcCurves_MtC_R") ->
      L261.RsrcCurves_C

    L261.ResTechShrwt_C %>%
      add_title("Technology share-weights for the carbon storage resource") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      same_precursors_as(L261.RsrcCurves_C) ->
      L261.ResTechShrwt_C

    L261.RsrcCurves_C_high %>%
      add_title("High supply curve of onshore carbon storage resources") %>%
      add_units("Available in MtCO2, Extraction Cost in 1990$/tCO2") %>%
      add_comments("A multiplier (based on high level of CCS use) was applied to the extraction cost to generate a high supply curve") %>%
      add_legacy_name("L261.RsrcCurves_C_high") %>%
      add_precursors("common/GCAM_region_names", "L161.RsrcCurves_MtC_R") ->
      L261.RsrcCurves_C_high

    L261.RsrcCurves_C_low %>%
      add_title("Low supply curve of onshore carbon storage resources") %>%
      add_units("Available in MtCO2, Extraction Cost in 1990$/tCO2") %>%
      add_comments("A multiplier (based on low level of CCS use) was applied to the extraction cost to generate a low supply curve") %>%
      add_legacy_name("L261.RsrcCurves_C_low") %>%
      add_precursors("common/GCAM_region_names", "L161.RsrcCurves_MtC_R") ->
      L261.RsrcCurves_C_low

    L261.RsrcCurves_C_lowest %>%
      add_title("Lowest supply curve of onshore carbon storage resources") %>%
      add_units("Available in MtCO2, Extraction Cost in 1990$/tCO2") %>%
      add_comments("A multiplier (based on lowest level of CCS use) was applied to the extraction cost to generate a lowest supply curve") %>%
      add_legacy_name("L261.RsrcCurves_C_lowest") %>%
      add_precursors("common/GCAM_region_names", "L161.RsrcCurves_MtC_R") ->
      L261.RsrcCurves_C_lowest

    L261.Supplysector_C %>%
      add_title("Carbon storage sector information") %>%
      add_units("Output, input, and price units are as listed; exponent is unitless") %>%
      add_comments("Carbon storage sector information was expanded to include GCAM region names") %>%
      add_legacy_name("L261.Supplysector_C") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.sector") ->
      L261.Supplysector_C

    L261.SubsectorLogit_C %>%
      add_title("Subsector logit exponents of carbon storage sector") %>%
      add_units("Unitless") %>%
      add_comments("Table on subsector logit exponents was expanded to include GCAM region names") %>%
      add_legacy_name("L261.SubsectorLogit_C") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.subsector_logit") ->
      L261.SubsectorLogit_C

    L261.SubsectorShrwtFllt_C %>%
      add_title("Subsector shareweights of carbon storage sectors") %>%
      add_units("Unitless") %>%
      add_comments("Table on subsector shareweights was expanded to include GCAM region names") %>%
      add_legacy_name("L261.SubsectorShrwtFllt_C") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.subsector_shrwt") ->
      L261.SubsectorShrwtFllt_C

    L261.StubTech_C %>%
      add_title("Identification of stub technologies of carbon storage") %>%
      add_units("Not Applicable") %>%
      add_comments("Technology list in the global shareweight table for carbon storage was expanded to include GCAM regions") %>%
      add_legacy_name("L261.StubTech_C") %>%
      add_precursors("common/GCAM_region_names", "energy/A61.globaltech_shrwt") ->
      L261.StubTech_C

    L261.GlobalTechCoef_C %>%
      add_title("Carbon storage global technology coefficients across base model years") %>%
      add_units("Unitless") %>%
      add_comments("Global technology coefficients were interpolated across all base model years") %>%
      add_legacy_name("L261.GlobalTechCoef_C") %>%
      add_precursors("energy/A61.globaltech_coef") ->
      L261.GlobalTechCoef_C

    L261.GlobalTechCost_C %>%
      add_title("Carbon storage global technology costs across base model years") %>%
      add_units("1975$/tCO2") %>%
      add_comments("Global technology coefficients were interpolated across all base model years") %>%
      add_legacy_name("L261.GlobalTechCost_C") %>%
      add_precursors("energy/A61.globaltech_cost") ->
      L261.GlobalTechCost_C

    L261.GlobalTechCost_C_High %>%
      add_title("Carbon storage global technology costs across base model years (high price scenario)") %>%
      add_units("1975$/tCO2") %>%
      add_comments("Assigned onshore and offshore carbon storage technologies a high price to price out CCS") %>%
      add_legacy_name("L261.GlobalTechCost_C_High") %>%
      add_precursors("energy/A61.globaltech_cost") ->
      L261.GlobalTechCost_C_High

    L261.GlobalTechShrwt_C %>%
      add_title("Shareweights of carbon storage technologies across base model years") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights of global technologies for energy transformation were interpolated across all base model years") %>%
      add_legacy_name("L261.GlobalTechShrwt_C") %>%
      add_precursors("energy/A61.globaltech_shrwt") ->
      L261.GlobalTechShrwt_C

    L261.GlobalTechShrwt_C_nooffshore %>%
      add_title("Shareweights of offshore carbon storage technologies") %>%
      add_units("Unitless") %>%
      add_comments("Subset shareweight table for offshore only. Assigned them shareweights of zero") %>%
      add_legacy_name("L261.GlobalTechShrwt_C_nooffshore") %>%
      add_precursors("energy/A61.globaltech_shrwt") ->
      L261.GlobalTechShrwt_C_nooffshore


    return_data(L261.Rsrc, L261.UnlimitRsrc, L261.RsrcCurves_C, L261.ResTechShrwt_C, L261.Supplysector_C, L261.SubsectorLogit_C, L261.SubsectorShrwtFllt_C, L261.StubTech_C, L261.GlobalTechCoef_C, L261.GlobalTechCost_C, L261.GlobalTechShrwt_C, L261.GlobalTechCost_C_High, L261.GlobalTechShrwt_C_nooffshore, L261.RsrcCurves_C_high, L261.RsrcCurves_C_low, L261.RsrcCurves_C_lowest)
  } else {
    stop("Unknown command")
  }
}
