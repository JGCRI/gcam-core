# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_elec_tech_scenarios_xml
#'
#' Construct XML files to define electric sector technology levels (adv, low).
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{nuclear_adv_USA.xml}, \code{nuclear_low_USA.xml}.
module_gcamusa_elec_tech_scenarios_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "gcam-usa/A23.elecS_tech_mapping",
      FILE = "gcam-usa/A23.elecS_inttech_mapping",
      "L1233.globaltech_capital_ATB_adv",
      "L1233.globaltech_capital_ATB_low",
      "L1233.globaltech_OMfixed_ATB_adv",
      "L1233.globaltech_OMfixed_ATB_low",
      "L1233.globaltech_OMvar_ATB_adv",
      "L1233.globaltech_OMvar_ATB_low",
      "L2233.GlobalTechCapital_elecS_USA",
      "L2233.GlobalIntTechCapital_elecS_USA",
      "L2233.GlobalTechOMfixed_elecS_cool_USA",
      "L2233.GlobalIntTechOMfixed_elecS_cool_USA",
      "L2233.GlobalTechOMvar_elecS_cool_USA",
      "L2233.GlobalIntTechOMvar_elecS_cool_USA")

  MODULE_OUTPUTS <-
    c(XML = "geothermal_adv_USA.xml",
      XML = "nuclear_adv_USA.xml",
      XML = "solar_adv_USA.xml",
      XML = "wind_adv_USA.xml",
      XML = "geothermal_low_USA.xml",
      XML = "nuclear_low_USA.xml",
      XML = "solar_low_USA.xml",
      XML = "wind_low_USA.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence global package checks
    value <- year <- NULL

    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Rename some of the columns of the input data tables
    # Mapping tables: only use the columns needed, and match names used in the data tables
    A23.elecS_tech_mapping <- A23.elecS_tech_mapping %>%
      select(subsector.name = Electric.sector.technology,
             from.technology = technology)

    A23.elecS_inttech_mapping <- A23.elecS_inttech_mapping %>%
      select(subsector.name = Electric.sector.intermittent.technology,
             from.technology = intermittent.technology)

    # GlobalIntTech data tables don't have the tag names used in the global tech database
    L2233.GlobalIntTechCapital_elecS_USA <- L2233.GlobalIntTechCapital_elecS_USA %>%
      rename(sector.name = supplysector, subsector.name0 = subsector0, subsector.name = subsector)
    L2233.GlobalIntTechOMfixed_elecS_cool_USA <- L2233.GlobalIntTechOMfixed_elecS_cool_USA %>%
      rename(sector.name = supplysector, subsector.name0 = subsector0, subsector.name = subsector)
    L2233.GlobalIntTechOMvar_elecS_cool_USA <- L2233.GlobalIntTechOMvar_elecS_cool_USA %>%
      rename(sector.name = supplysector, subsector.name0 = subsector0, subsector.name = subsector)

    # Process data to final formats
    # Start with ref scenario assumptions (refdata), filter to future years, join in alt-scenario assumptions (replacement_data)
    assign_gcamusa_tech_cost <- function(refdata, replacement_data, costvar, mapping = A23.elecS_tech_mapping){
      refdata <- refdata[names(refdata) != costvar]

      replacement_data <- replacement_data[c("technology", "year", costvar)] %>%
        rename(from.technology = technology)

      # inner_join(mapping) drops the partitioned vintages of coal which aren't in A23.elecS_tech_mapping and which are not used
      # in differentiating technology scenarios
      # inner_join(replacement_data) drops the technologies in refdata that aren't in the replacement_data
      outdata <- refdata %>%
        inner_join(mapping, by = "subsector.name") %>%
        filter(year %in% MODEL_FUTURE_YEARS) %>%
        inner_join(replacement_data, by = c("from.technology", "year")) %>%
        select(-from.technology)

      return(outdata)
    }

    # Capital costs
    # - Standard technologies
    L2233.GlobalTechCapital_elecS_USA_adv <-
      assign_gcamusa_tech_cost(refdata = L2233.GlobalTechCapital_elecS_USA,
                               replacement_data = L1233.globaltech_capital_ATB_adv,
                               costvar = "capital.overnight") %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL))
    L2233.GlobalTechCapital_elecS_USA_low <-
      assign_gcamusa_tech_cost(refdata = L2233.GlobalTechCapital_elecS_USA,
                               replacement_data = L1233.globaltech_capital_ATB_low,
                               costvar = "capital.overnight") %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL))

    # - Intermittent technologies
    L2233.GlobalIntTechCapital_elecS_USA_adv <-
      assign_gcamusa_tech_cost(refdata = L2233.GlobalIntTechCapital_elecS_USA,
                               replacement_data = L1233.globaltech_capital_ATB_adv,
                               costvar = "capital.overnight",
                               mapping = A23.elecS_inttech_mapping) %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL))
    L2233.GlobalIntTechCapital_elecS_USA_low <-
      assign_gcamusa_tech_cost(refdata = L2233.GlobalIntTechCapital_elecS_USA,
                               replacement_data = L1233.globaltech_capital_ATB_low,
                               costvar = "capital.overnight",
                               mapping = A23.elecS_inttech_mapping) %>%
      mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL))

    # - Partition by technology
    # - note that rooftop_pv in the standard configuration is pre-levelized, so as to avoid re-counting the capital investment
    #   in each year a cohort operates. This is not done in GCAM-USA, as the macro model can not be run in GCAM-USA
    L2233.GlobalTechCapital_elecS_USA_geo_adv <- filter(L2233.GlobalTechCapital_elecS_USA_adv, subsector.name0 == "geothermal")
    L2233.GlobalTechCapital_elecS_USA_geo_low <- filter(L2233.GlobalTechCapital_elecS_USA_low, subsector.name0 == "geothermal")

    L2233.GlobalTechCapital_elecS_USA_nuc_adv <- filter(L2233.GlobalTechCapital_elecS_USA_adv, subsector.name0 == "nuclear", subsector.name != "nuc_base_Gen II")
    L2233.GlobalTechCapital_elecS_USA_nuc_low <- filter(L2233.GlobalTechCapital_elecS_USA_low, subsector.name0 == "nuclear", subsector.name != "nuc_base_Gen II")

    L2233.GlobalTechCapital_elecS_USA_sol_adv <- filter(L2233.GlobalTechCapital_elecS_USA_adv, subsector.name0 %in% c("solar", "rooftop_pv"))
    L2233.GlobalTechCapital_elecS_USA_sol_low <- filter(L2233.GlobalTechCapital_elecS_USA_low, subsector.name0 %in% c("solar", "rooftop_pv"))

    L2233.GlobalTechCapital_elecS_USA_wind_adv <- filter(L2233.GlobalTechCapital_elecS_USA_adv, subsector.name0 == "wind")
    L2233.GlobalTechCapital_elecS_USA_wind_low <- filter(L2233.GlobalTechCapital_elecS_USA_low, subsector.name0 == "wind")

    L2233.GlobalIntTechCapital_elecS_USA_sol_adv <- filter(L2233.GlobalIntTechCapital_elecS_USA_adv, subsector.name0 %in% c("solar", "rooftop_pv"))
    L2233.GlobalIntTechCapital_elecS_USA_sol_low <- filter(L2233.GlobalIntTechCapital_elecS_USA_low, subsector.name0 %in% c("solar", "rooftop_pv"))

    L2233.GlobalIntTechCapital_elecS_USA_wind_adv <- filter(L2233.GlobalIntTechCapital_elecS_USA_adv, subsector.name0 == "wind")
    L2233.GlobalIntTechCapital_elecS_USA_wind_low <- filter(L2233.GlobalIntTechCapital_elecS_USA_low, subsector.name0 == "wind")

    # Fixed O&M
    # - Standard technologies
    L2233.GlobalTechOMfixed_elecS_cool_USA_adv <-
      assign_gcamusa_tech_cost(refdata = L2233.GlobalTechOMfixed_elecS_cool_USA,
                               replacement_data = L1233.globaltech_OMfixed_ATB_adv,
                               costvar = "OM.fixed") %>%
      mutate(OM.fixed = round(OM.fixed, energy.DIGITS_OM))
    L2233.GlobalTechOMfixed_elecS_cool_USA_low <-
      assign_gcamusa_tech_cost(refdata = L2233.GlobalTechOMfixed_elecS_cool_USA,
                               replacement_data = L1233.globaltech_OMfixed_ATB_low,
                               costvar = "OM.fixed") %>%
      mutate(OM.fixed = round(OM.fixed, energy.DIGITS_OM))

    # - Intermittent technologies
    L2233.GlobalIntTechOMfixed_elecS_cool_USA_adv <-
      assign_gcamusa_tech_cost(refdata = L2233.GlobalIntTechOMfixed_elecS_cool_USA,
                               replacement_data = L1233.globaltech_OMfixed_ATB_adv,
                               costvar = "OM.fixed",
                               mapping = A23.elecS_inttech_mapping) %>%
      mutate(OM.fixed = round(OM.fixed, energy.DIGITS_OM))
    L2233.GlobalIntTechOMfixed_elecS_cool_USA_low <-
      assign_gcamusa_tech_cost(refdata = L2233.GlobalIntTechOMfixed_elecS_cool_USA,
                               replacement_data = L1233.globaltech_OMfixed_ATB_low,
                               costvar = "OM.fixed",
                               mapping = A23.elecS_inttech_mapping) %>%
      mutate(OM.fixed = round(OM.fixed, energy.DIGITS_OM))

    # - Partition by technology
    L2233.GlobalTechOMfixed_elecS_cool_USA_geo_adv <- filter(L2233.GlobalTechOMfixed_elecS_cool_USA_adv, subsector.name0 == "geothermal")
    L2233.GlobalTechOMfixed_elecS_cool_USA_geo_low <- filter(L2233.GlobalTechOMfixed_elecS_cool_USA_low, subsector.name0 == "geothermal")

    L2233.GlobalTechOMfixed_elecS_cool_USA_nuc_adv <- filter(L2233.GlobalTechOMfixed_elecS_cool_USA_adv, subsector.name0 == "nuclear", subsector.name != "nuc_base_Gen II")
    L2233.GlobalTechOMfixed_elecS_cool_USA_nuc_low <- filter(L2233.GlobalTechOMfixed_elecS_cool_USA_low, subsector.name0 == "nuclear", subsector.name != "nuc_base_Gen II")

    L2233.GlobalTechOMfixed_elecS_cool_USA_sol_adv <- filter(L2233.GlobalTechOMfixed_elecS_cool_USA_adv, subsector.name0 %in% c("solar", "rooftop_pv"))
    L2233.GlobalTechOMfixed_elecS_cool_USA_sol_low <- filter(L2233.GlobalTechOMfixed_elecS_cool_USA_low, subsector.name0 %in% c("solar", "rooftop_pv"))

    L2233.GlobalTechOMfixed_elecS_cool_USA_wind_adv <- filter(L2233.GlobalTechOMfixed_elecS_cool_USA_adv, subsector.name0 == "wind")
    L2233.GlobalTechOMfixed_elecS_cool_USA_wind_low <- filter(L2233.GlobalTechOMfixed_elecS_cool_USA_low, subsector.name0 == "wind")

    L2233.GlobalIntTechOMfixed_elecS_cool_USA_sol_adv <- filter(L2233.GlobalIntTechOMfixed_elecS_cool_USA_adv, subsector.name0 %in% c("solar", "rooftop_pv"))
    L2233.GlobalIntTechOMfixed_elecS_cool_USA_sol_low <- filter(L2233.GlobalIntTechOMfixed_elecS_cool_USA_low, subsector.name0 %in% c("solar", "rooftop_pv"))

    L2233.GlobalIntTechOMfixed_elecS_cool_USA_wind_adv <- filter(L2233.GlobalIntTechOMfixed_elecS_cool_USA_adv, subsector.name0 == "wind")
    L2233.GlobalIntTechOMfixed_elecS_cool_USA_wind_low <- filter(L2233.GlobalIntTechOMfixed_elecS_cool_USA_low, subsector.name0 == "wind")

    # Variable O&M
    # - Standard technologies
    L2233.GlobalTechOMvar_elecS_cool_USA_adv <-
      assign_gcamusa_tech_cost(refdata = L2233.GlobalTechOMvar_elecS_cool_USA,
                               replacement_data = L1233.globaltech_OMvar_ATB_adv,
                               costvar = "OM.var") %>%
      mutate(OM.var = round(OM.var, energy.DIGITS_OM))
    L2233.GlobalTechOMvar_elecS_cool_USA_low <-
      assign_gcamusa_tech_cost(refdata = L2233.GlobalTechOMvar_elecS_cool_USA,
                               replacement_data = L1233.globaltech_OMvar_ATB_low,
                               costvar = "OM.var") %>%
      mutate(OM.var = round(OM.var, energy.DIGITS_OM))

    # - Intermittent technologies
    L2233.GlobalIntTechOMvar_elecS_cool_USA_adv <-
      assign_gcamusa_tech_cost(refdata = L2233.GlobalIntTechOMvar_elecS_cool_USA,
                               replacement_data = L1233.globaltech_OMvar_ATB_adv,
                               costvar = "OM.var",
                               mapping = A23.elecS_inttech_mapping) %>%
      mutate(OM.var = round(OM.var, energy.DIGITS_OM))
    L2233.GlobalIntTechOMvar_elecS_cool_USA_low <-
      assign_gcamusa_tech_cost(refdata = L2233.GlobalIntTechOMvar_elecS_cool_USA,
                               replacement_data = L1233.globaltech_OMvar_ATB_low,
                               costvar = "OM.var",
                               mapping = A23.elecS_inttech_mapping) %>%
      mutate(OM.var = round(OM.var, energy.DIGITS_OM))

    # - Partition by technology
    L2233.GlobalTechOMvar_elecS_cool_USA_geo_adv <- filter(L2233.GlobalTechOMvar_elecS_cool_USA_adv, subsector.name0 == "geothermal")
    L2233.GlobalTechOMvar_elecS_cool_USA_geo_low <- filter(L2233.GlobalTechOMvar_elecS_cool_USA_low, subsector.name0 == "geothermal")

    L2233.GlobalTechOMvar_elecS_cool_USA_nuc_adv <- filter(L2233.GlobalTechOMvar_elecS_cool_USA_adv, subsector.name0 == "nuclear", subsector.name != "nuc_base_Gen II")
    L2233.GlobalTechOMvar_elecS_cool_USA_nuc_low <- filter(L2233.GlobalTechOMvar_elecS_cool_USA_low, subsector.name0 == "nuclear", subsector.name != "nuc_base_Gen II")

    L2233.GlobalTechOMvar_elecS_cool_USA_sol_adv <- filter(L2233.GlobalTechOMvar_elecS_cool_USA_adv, subsector.name0 %in% c("solar", "rooftop_pv"))
    L2233.GlobalTechOMvar_elecS_cool_USA_sol_low <- filter(L2233.GlobalTechOMvar_elecS_cool_USA_low, subsector.name0 %in% c("solar", "rooftop_pv"))

    L2233.GlobalTechOMvar_elecS_cool_USA_wind_adv <- filter(L2233.GlobalTechOMvar_elecS_cool_USA_adv, subsector.name0 == "wind")
    L2233.GlobalTechOMvar_elecS_cool_USA_wind_low <- filter(L2233.GlobalTechOMvar_elecS_cool_USA_low, subsector.name0 == "wind")

    L2233.GlobalIntTechOMvar_elecS_cool_USA_sol_adv <- filter(L2233.GlobalIntTechOMvar_elecS_cool_USA_adv, subsector.name0 %in% c("solar", "rooftop_pv"))
    L2233.GlobalIntTechOMvar_elecS_cool_USA_sol_low <- filter(L2233.GlobalIntTechOMvar_elecS_cool_USA_low, subsector.name0 %in% c("solar", "rooftop_pv"))

    L2233.GlobalIntTechOMvar_elecS_cool_USA_wind_adv <- filter(L2233.GlobalIntTechOMvar_elecS_cool_USA_adv, subsector.name0 == "wind")
    L2233.GlobalIntTechOMvar_elecS_cool_USA_wind_low <- filter(L2233.GlobalIntTechOMvar_elecS_cool_USA_low, subsector.name0 == "wind")

    # ===================================================

    # Produce outputs
    create_xml("geothermal_adv_USA.xml") %>%
      add_xml_data(L2233.GlobalTechCapital_elecS_USA_geo_adv, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecS_cool_USA_geo_adv, "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecS_cool_USA_geo_adv, "GlobalTechOMvar") %>%
      add_precursors("gcam-usa/A23.elecS_tech_mapping",
                     "L1233.globaltech_capital_ATB_adv",
                     "L1233.globaltech_OMfixed_ATB_adv",
                     "L1233.globaltech_OMvar_ATB_adv",
                     "L2233.GlobalTechOMvar_elecS_cool_USA",
                     "L2233.GlobalTechOMfixed_elecS_cool_USA",
                     "L2233.GlobalTechCapital_elecS_USA") ->
      geothermal_adv_USA.xml

    create_xml("nuclear_adv_USA.xml") %>%
      add_xml_data(L2233.GlobalTechCapital_elecS_USA_nuc_adv, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecS_cool_USA_nuc_adv, "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecS_cool_USA_nuc_adv, "GlobalTechOMvar") %>%
      same_precursors_as(geothermal_adv_USA.xml) ->
      nuclear_adv_USA.xml

    create_xml("solar_adv_USA.xml") %>%
      add_xml_data(L2233.GlobalTechCapital_elecS_USA_sol_adv, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecS_cool_USA_sol_adv, "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecS_cool_USA_sol_adv, "GlobalTechOMvar") %>%
      add_xml_data(L2233.GlobalIntTechCapital_elecS_USA_sol_adv, "GlobalIntTechCapital") %>%
      add_xml_data(L2233.GlobalIntTechOMfixed_elecS_cool_USA_sol_adv, "GlobalIntTechOMfixed") %>%
      add_xml_data(L2233.GlobalIntTechOMvar_elecS_cool_USA_sol_adv, "GlobalIntTechOMvar") %>%
      same_precursors_as(geothermal_adv_USA.xml) %>%
      add_precursors("gcam-usa/A23.elecS_inttech_mapping",
                     "L2233.GlobalIntTechCapital_elecS_USA",
                     "L2233.GlobalIntTechOMfixed_elecS_cool_USA",
                     "L2233.GlobalIntTechOMvar_elecS_cool_USA") ->
      solar_adv_USA.xml

    create_xml("wind_adv_USA.xml") %>%
      add_xml_data(L2233.GlobalTechCapital_elecS_USA_wind_adv, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecS_cool_USA_wind_adv, "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecS_cool_USA_wind_adv, "GlobalTechOMvar") %>%
      add_xml_data(L2233.GlobalIntTechCapital_elecS_USA_wind_adv, "GlobalIntTechCapital") %>%
      add_xml_data(L2233.GlobalIntTechOMfixed_elecS_cool_USA_wind_adv, "GlobalIntTechOMfixed") %>%
      add_xml_data(L2233.GlobalIntTechOMvar_elecS_cool_USA_wind_adv, "GlobalIntTechOMvar") %>%
      same_precursors_as(solar_adv_USA.xml) ->
      wind_adv_USA.xml

    create_xml("geothermal_low_USA.xml") %>%
      add_xml_data(L2233.GlobalTechCapital_elecS_USA_geo_low, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecS_cool_USA_geo_low, "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecS_cool_USA_geo_low, "GlobalTechOMvar") %>%
      add_precursors("L1233.globaltech_capital_ATB_low",
                     "L1233.globaltech_OMfixed_ATB_low",
                     "L1233.globaltech_OMvar_ATB_low",
                     "L2233.GlobalTechOMvar_elecS_cool_USA",
                     "L2233.GlobalTechOMfixed_elecS_cool_USA",
                     "L2233.GlobalTechCapital_elecS_USA") ->
      geothermal_low_USA.xml

    create_xml("nuclear_low_USA.xml") %>%
      add_xml_data(L2233.GlobalTechCapital_elecS_USA_nuc_low, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecS_cool_USA_nuc_low, "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecS_cool_USA_nuc_low, "GlobalTechOMvar") %>%
      same_precursors_as(geothermal_low_USA.xml) ->
      nuclear_low_USA.xml

    create_xml("solar_low_USA.xml") %>%
      add_xml_data(L2233.GlobalTechCapital_elecS_USA_sol_low, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecS_cool_USA_sol_low, "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecS_cool_USA_sol_low, "GlobalTechOMvar") %>%
      add_xml_data(L2233.GlobalIntTechCapital_elecS_USA_sol_low, "GlobalIntTechCapital") %>%
      add_xml_data(L2233.GlobalIntTechOMfixed_elecS_cool_USA_sol_low, "GlobalIntTechOMfixed") %>%
      add_xml_data(L2233.GlobalIntTechOMvar_elecS_cool_USA_sol_low, "GlobalIntTechOMvar") %>%
      same_precursors_as(geothermal_low_USA.xml) %>%
      add_precursors("L2233.GlobalIntTechCapital_elecS_USA",
                     "L2233.GlobalIntTechOMfixed_elecS_cool_USA",
                     "L2233.GlobalIntTechOMvar_elecS_cool_USA") ->
      solar_low_USA.xml

    create_xml("wind_low_USA.xml") %>%
      add_xml_data(L2233.GlobalTechCapital_elecS_USA_wind_low, "GlobalTechCapital") %>%
      add_xml_data(L2233.GlobalTechOMfixed_elecS_cool_USA_wind_low, "GlobalTechOMfixed") %>%
      add_xml_data(L2233.GlobalTechOMvar_elecS_cool_USA_wind_low, "GlobalTechOMvar") %>%
      add_xml_data(L2233.GlobalIntTechCapital_elecS_USA_wind_low, "GlobalIntTechCapital") %>%
      add_xml_data(L2233.GlobalIntTechOMfixed_elecS_cool_USA_wind_low, "GlobalIntTechOMfixed") %>%
      add_xml_data(L2233.GlobalIntTechOMvar_elecS_cool_USA_wind_low, "GlobalIntTechOMvar") %>%
      same_precursors_as(solar_low_USA.xml) ->
      wind_low_USA.xml

    return_data(geothermal_adv_USA.xml,
                nuclear_adv_USA.xml,
                solar_adv_USA.xml,
                wind_adv_USA.xml,
                geothermal_low_USA.xml,
                nuclear_low_USA.xml,
                solar_low_USA.xml,
                wind_low_USA.xml)
  } else {
    stop("Unknown command")
  }
}
