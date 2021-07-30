# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L203.water_td_USA
#'
#' Mapping of water consumption/withdrawal to sectoral demands at the state level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L203.DeleteSupplysector_USA}, \code{L203.Supplysector_USA}, \code{L203.SubsectorLogit_USA},
#' \code{L203.SubsectorShrwt_USA}, \code{L203.TechShrwt_USA}, \code{L203.TechCoef_USA}, \code{L203.TechPmult_USA},
#' \code{L203.TechDesalCoef_USA}, \code{L203.TechDesalShrwt_USA}, \code{L203.TechDesalCost_USA}.
#' The corresponding file in the original data system was \code{L203.water.mapping.R} (water level2).
#' @details Generates water mapping sector input files to group demands by sectors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author NTG May 2020
module_gcamusa_L203.water_td_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_to_country_mapping",
             FILE = "water/water_td_sectors",
             FILE = "water/A71.sector",
             FILE = "water/A72.sector",
             FILE = "water/A73.sector",
             FILE = "water/A74.sector",
             "L103.water_mapping_USA_R_LS_W_Ws_share",
             "L103.water_mapping_USA_R_PRI_W_Ws_share",
             "L103.water_mapping_USA_R_GLU_W_Ws_share",
             "L103.water_mapping_USA_R_B_W_Ws_share",
             FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/state_and_basin",
             FILE = "gcam-usa/usa_seawater_states_basins",
             FILE = "water/water_td_sectors",
             FILE = "water/A03.sector",
             "L201.RsrcTechCoef",
             "L203.Supplysector_desal_basin"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L203.DeleteSupplysector_USA",
             "L203.DeleteResTechInput",
             "L203.DeleteSubsector_USA",
             "L203.Supplysector_USA",
             "L203.SubsectorLogit_USA",
             "L203.SubsectorShrwt_USA",
             "L203.TechShrwt_USA",
             "L203.TechCoef_USA",
             "L203.TechPmult_USA",
             "L203.TechDesalCoef_USA",
             "L203.TechDesalShrwt_USA",
             "L203.TechDesalCost_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    water_td_sectors <- get_data(all_data, "water/water_td_sectors")
    A71.sector <- get_data(all_data, "water/A71.sector")
    A72.sector <- get_data(all_data, "water/A72.sector")
    A73.sector <- get_data(all_data, "water/A73.sector")
    A74.sector <- get_data(all_data, "water/A74.sector")
    L103.water_mapping_USA_R_LS_W_Ws_share <- get_data(all_data, "L103.water_mapping_USA_R_LS_W_Ws_share", strip_attributes = TRUE)
    L103.water_mapping_USA_R_PRI_W_Ws_share <- get_data(all_data, "L103.water_mapping_USA_R_PRI_W_Ws_share", strip_attributes = TRUE)
    L103.water_mapping_USA_R_GLU_W_Ws_share <- get_data(all_data,"L103.water_mapping_USA_R_GLU_W_Ws_share", strip_attributes = TRUE)
    L103.water_mapping_USA_R_B_W_Ws_share <- get_data(all_data,"L103.water_mapping_USA_R_B_W_Ws_share", strip_attributes = TRUE)
    GCAM_state_names <- get_data(all_data, "gcam-usa/states_subregions")
    state_and_basin <- get_data(all_data, "gcam-usa/state_and_basin")
    usa_seawater_states_basins <- get_data(all_data, "gcam-usa/usa_seawater_states_basins")
    water_td_sectors <- get_data(all_data, "water/water_td_sectors")
    A03.sector <- get_data(all_data, "water/A03.sector")
    L201.RsrcTechCoef <- get_data(all_data, "L201.RsrcTechCoef", strip_attributes = TRUE)
    L203.Supplysector_desal_basin <- get_data(all_data, "L203.Supplysector_desal_basin", strip_attributes = TRUE)

    GLU <- GLU_code <- GLU_name <- water.sector <-
      water_type <- supplysector <- field.eff <- conveyance.eff <-
      coefficient <- region <- state <- share <- basin_name <- Basin_name <-
      GCAM_basin_ID <- state_abbr <- water_sector <- year <- wt_short <- value <-
      state.to.country.share <- subsector <- technology <- share.weight <-
      price.unit <- input.unit <- output.unit <- logit.exponent <- logit.type <-
      logit.year.fillout <- resource <- minicam.energy.input <- subresource <- NULL  # silence package check notes

    # Define unique states and basins that have access to seawater that will
    # allow for seawate cooling

    seawater_states_basins <- unique(usa_seawater_states_basins$seawater_region)

    # Define in which states GCAM water basins exist by using data from R package created by Chris Vernon
    state_and_basin %>%
      left_join_error_no_match(basin_to_country_mapping, by = "GCAM_basin_ID") %>%
      select(GCAM_basin_ID, GLU_name, basin_name, state_abbr) %>%
      rename(region = state_abbr) ->
      state_and_basin_mapping

    # Create mappings for the sectors that have production at the state level already.
    # These sectors: Industrial, Municipal, and Electricity will not need to be shared
    # from the USA region to the states, and thus will not have separate market names by region
    L103.water_mapping_USA_R_B_W_Ws_share %>%
      mutate(water_sector = gsub("Domestic", "Municipal", water_sector)) %>%
      left_join_error_no_match(water_td_sectors, by = c("water_sector" = "water.sector")) %>%
      left_join_error_no_match(A03.sector, by = "supplysector", ignore_columns = c("logit.type")) %>%
      mutate(supplysector = set_water_input_name(water_sector, water_type, water_td_sectors)) ->
      L203.mapping_nonirr

    # Using irrigation shares, define water sector and add demand categories
    L103.water_mapping_USA_R_GLU_W_Ws_share %>%
      rename(state = region) %>%
      mutate(region = gcam.USA_REGION,
             water.sector = water.IRRIGATION) %>%
      left_join_error_no_match(water_td_sectors, by = "water.sector") %>%
      left_join_error_no_match(A03.sector, by = "supplysector", ignore_columns = "logit.type") %>%
      mutate(supplysector = set_water_input_name(water.sector, water_type, water_td_sectors, GLU_name)) ->
      L203.mapping_irr

    # Isolate the USA region which will share basin level demands in the USA region to
    # States which are defined as subsectors
    L203.mapping_irr %>%
      mutate(subsector = state,
             technology = supplysector,
             coefficient = if_else(water.sector == water.IRRIGATION & water_type == "water withdrawals",
                                   1 / gcamusa.CONVEYANCE_LOSSES, 1),
             ## ^^ conveyance losses for irrigation--applied to withdrawals only
             # Note: Conveyance losses are taken out of agriculture withdrawals and...
             # ... instead applied to water distribution sectors (water_td_irr). This means that to get total...
             # ... ag withdrawals for reporting (i.e., when querying GCAM results)...
             # ... it is necessary to include the conveyance loss.
             share.weight = share,
             market.name = state,
             share.weight.year = year,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-share, -state) %>%
      arrange(region) ->
      L203.mapping_irr_region

    # We must now set all subsectors in USA from gcam-core and water_mapping.xml to 0 so that we do not double count
    # demands
    L203.mapping_irr_region %>%
      bind_rows(L203.mapping_irr_region %>%
                  mutate(subsector = basin_name,
                         technology = basin_name,
                         share.weight = 0,
                         market.name = gcam.USA_REGION)) ->
      L203.mapping_irr_region

    # Isolate the states and define the basins which contribute water supplies to wach one.
    L203.mapping_irr %>%
      select(-region) %>%
      mutate(region = state,
             subsector = basin_name,
             technology = basin_name,
             coefficient = gcamusa.DEFAULT_COEFFICIENT,
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT,
             market.name = gcam.USA_REGION,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-share, -state) %>%
      arrange(region) ->
      L203.mapping_irr_state

    # Combine state and USA region irrigation mappings
    bind_rows(
      L203.mapping_irr_region %>%
        ## filter out basin name subsectors
        filter(subsector %in% gcamusa.STATES),
      L203.mapping_irr_state
    ) ->
      L203.mapping_irr


    # Livestock sector:
    # This done slightly different as production of livestock is not modeled at the state level.
    # Here we take the regional (i.e. USA) water demands of livestock and map them to the state level based on
    # the amount of water for livestock that each state requires compared to the USA as a whole, computed in
    # L103.water_mapping_USA
    L103.water_mapping_USA_R_LS_W_Ws_share %>%
      mutate(region=gcam.USA_REGION,
             water.sector = water.LIVESTOCK) %>%
      left_join_error_no_match(water_td_sectors, by = "water.sector") %>%
      left_join_error_no_match(A03.sector, by = "supplysector", ignore_columns = "logit.type") %>%
      mutate(wt_short = water.MAPPED_WATER_TYPES_SHORT[water_type],
             supplysector = paste(supplysector, wt_short, sep = "_"),
             coefficient = gcamusa.DEFAULT_COEFFICIENT,
             subsector = state,
             technology = supplysector,
             share.weight = value,
             market.name = state,
             share.weight.year = year,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-wt_short, -value, -state) %>%
      arrange(region) ->
      L203.mapping_livestock

    L203.mapping_livestock %>%
      bind_rows(L203.mapping_livestock %>%
                  # LJENM returns error because number of rows in data changes.
                  # The join is intended to duplicate rows because some states
                  # are mapped to multiple basisn.  Thus, left_join() is used.
                  left_join(state_and_basin_mapping, by = c("subsector" = "region")) %>%
                  mutate(share.weight = 0,
                         subsector = basin_name,
                         technology = basin_name,
                         market.name = gcam.USA_REGION) %>%
                  unique()
      ) ->
      L203.mapping_livestock

    # (d) primary energy sector
    # We use USGS withdrawal data for primary energy mining and ratios of fresh to saline water withdrawals to
    # map the demands from USA values to state level. This is done in 2 parts in order to specify differences in
    # subsectors at the state and national levels, as well as differences in share weights (i.e. mapping to states,
    # mapping of fresh to desal within a state)

    L103.water_mapping_USA_R_PRI_W_Ws_share %>%
      mutate(region = gcam.USA_REGION,
             water.sector = water.PRIMARY_ENERGY) %>%
      left_join_error_no_match(water_td_sectors, by = "water.sector") %>%
      left_join_error_no_match(A03.sector, by = "supplysector", ignore_columns = "logit.type") %>%
      mutate(wt_short = water.MAPPED_WATER_TYPES_SHORT[water_type],
             supplysector = paste(supplysector, wt_short, sep = "_"),
             coefficient = gcamusa.DEFAULT_COEFFICIENT,
             subsector = state,
             technology = supplysector,
             share.weight = state.to.country.share,
             market.name = state,
             share.weight.year = year,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-wt_short, -state.to.country.share, -state) %>%
      arrange(region) ->
      L203.mapping_primary_region

    L203.mapping_primary_region %>%
      bind_rows(L203.mapping_primary_region %>%
                  # LJENM returns error because number of rows in data changes.
                  # The join is intended to duplicate rows because some states
                  # are mapped to multiple basisn.  Thus, left_join() is used.
                  left_join(state_and_basin_mapping, by = c("subsector" = "region")) %>%
                  mutate(share.weight = 0,
                         subsector = basin_name,
                         technology = basin_name,
                         market.name = gcam.USA_REGION) %>%
                  unique()
      ) ->
      L203.mapping_primary_region

    # No values are present for DC, therefore NAs are created. These are replaced with
    # zero shareweights

    L203.mapping_primary_region %>%
      replace_na(list(share.weight = 0)) %>%
      replace_na(list(fresh.share = 0)) ->
      L203.mapping_primary


    # combine all sectors and add additional required columns. Long format is used for
    # subsector share weights, additional mapping is used for all other final outputs

    L203.mapping_nonirr %>%
      mutate(coefficient = gcamusa.DEFAULT_COEFFICIENT,
             subsector = basin_name,
             technology = basin_name,
             share.weight = share,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      arrange(region) %>%
      bind_rows(L203.mapping_nonirr %>%
                  filter(year == gcamusa.FINAL_MAPPING_YEAR) %>%
                  mutate(year=max(MODEL_BASE_YEARS),
                         coefficient = gcamusa.DEFAULT_COEFFICIENT,
                         subsector = basin_name,
                         technology = basin_name,
                         share.weight = share,
                         logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
                  arrange(region)) %>%
      complete(nesting(region, supplysector, subsector, technology, water.sector, basin_name, water_type, coefficient, share,
                       share.weight, price.unit, input.unit, output.unit, logit.exponent, logit.type, logit.year.fillout),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      dplyr::filter(!is.na(year)) %>%
      bind_rows(L203.mapping_livestock %>%
                  ## Filter out basin names in subsectors as these are deleted later
                  filter(subsector %in% gcamusa.STATES),
                L203.mapping_primary %>%
                  filter(subsector %in% gcamusa.STATES),
                L203.mapping_irr) %>%
      mutate(pMult = if_else(water.sector == water.IRRIGATION & water_type == "water withdrawals" & region != gcam.USA_REGION,
                             water.IRR_PRICE_SUBSIDY_MULT, water.MAPPING_PMULT)) ->
      L203.mapping_all

    L203.EFW_delete_supplysectors <- bind_rows(A71.sector, A72.sector, A73.sector, A74.sector) %>%
      pull(supplysector)
    L203.delete_desal_basin_sectors <- L203.Supplysector_desal_basin %>%
      filter(region == gcam.USA_REGION) %>%
      pull(supplysector)
    tibble(region = gcam.USA_REGION,
           supplysector = c(water.DELETE_DEMAND_TYPES,
                            L203.EFW_delete_supplysectors,
                            L203.delete_desal_basin_sectors)) ->
      L203.DeleteSupplysector_USA

    ## Also need to delete the "elect_td_ind" input to the groundwater grades in future periods
    L201.RsrcTechCoef %>%
      filter(region == gcam.USA_REGION) %>%
      select(region, resource, subresource, technology, year, minicam.energy.input) ->
      L203.DeleteResTechInput

    ## We delete the basin level subsectors in the USA region
    ## to eliminate double counting of irrigation, livestock,
    ## and primary energy. This overrides the mappings from
    ## water_mapping.XML and maps directly to the states.
    L203.mapping_irr_region %>%
      filter(!subsector %in% gcamusa.STATES) %>%
      select(region,supplysector,subsector) %>%
      bind_rows(
        L203.mapping_primary_region %>%
          filter(!subsector %in% gcamusa.STATES) %>%
          select(region,supplysector,subsector),
        L203.mapping_livestock%>%
          filter(!subsector %in% gcamusa.STATES) %>%
          select(region,supplysector,subsector)
      ) %>%
      unique()->
      L203.DeleteSubsector_USA


    # Sector information
    L203.mapping_all %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME) ->
      L203.Supplysector_USA

    # Subsector logit exponents for mapping sector
    L203.mapping_all %>%
      mutate(logit.exponent = if_else(region != gcam.USA_REGION, water.LOGIT_EXP, 0)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME) ->
      L203.SubsectorLogit_USA

    # Subsector share weights to 1 (no competition) in all states. Sharing happens at USA level. Water prices
    # will drive competition between the basins at the state level
    L203.mapping_all %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwt"]]) ->
      L203.SubsectorShrwt_USA

    # Technology share weights, defined by state and sector
    # Zero out technology shareweights in the USA region to make sure values are not counted multiple times
    L203.mapping_all %>%
      complete(nesting(region, supplysector, subsector, technology, water.sector, basin_name, water_type, coefficient),
               year = c(year, MODEL_BASE_YEARS,MODEL_FUTURE_YEARS)) %>%
      mutate(share.weight = if_else(region == gcam.USA_REGION & !(subsector %in% gcamusa.STATES) & !grepl("irr", supplysector), 0, 1)) %>%
      dplyr::filter(!is.na(year)) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]]) ->
      L203.TechShrwt_USA

    # Define market name and minicam energy input dependent upon whether the sector is
    # produced at the state level or is we map from USA region to state
    L203.mapping_all %>%
      complete(nesting(region, supplysector, subsector, technology, water.sector, basin_name, water_type, coefficient),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      mutate(minicam.energy.input = if_else((region == gcam.USA_REGION & grepl("water_td", technology)),
                                            supplysector,
                                            paste0(basin_name, "_", water_type)),
             market.name = if_else((region == gcam.USA_REGION & grepl("water_td", technology)), subsector, gcam.USA_REGION)) %>%
      dplyr::filter(!is.na(year)) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L203.TechCoef_USA

    # Pass-through technology water price adjust if there one
    L203.mapping_all %>%
      complete(nesting(region, supplysector, subsector, technology, water.sector, basin_name, water_type, coefficient),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      replace_na(list(pMult=1)) %>%
      select(LEVEL2_DATA_NAMES[["TechPmult"]]) ->
      L203.TechPmult_USA

    L203.TechCoef_USA %>%
      filter(region!=gcam.USA_REGION) %>%
      mutate(technology = "desalination",
             minicam.energy.input = gcamusa.WATER_TYPE_SEAWATER,
             market.name = gcam.USA_REGION) %>%
      dplyr::filter(!is.na(year))->
      L203.TechDesalCoef_USA

    # Set shareweight of desalination technologies to 0 in all non-coastal states
    # and basins that do not come in contact with the ocean. This removes the possibility
    # of having desalination required in Texas, but coming from the Rio Grande which does not
    # have access to seawater without inland transportation.
    #
    # Additionally, desalination is now allowed for all sectors, including irrigation.
    # Given the price subsidy on agricultural water, desalination should never come
    # for irrigated agriculture as the price required would exceed the limits defined in
    # water_supply_constrained.xml
    L203.TechShrwt_USA %>%
      filter(region != gcam.USA_REGION) %>%
      mutate(technology = "desalination",
             share.weight = if_else(!(region %in% seawater_states_basins), 0, 1))  %>%
      dplyr::filter(!is.na(year)) ->
      L203.TechDesalShrwt_USA

    L203.TechDesalShrwt_USA %>%
      rename(minicam.non.energy.input = share.weight) %>%
      mutate(minicam.non.energy.input = "final cost",
             input.cost = gcamusa.DESALINATION_PRICE) %>%
      dplyr::filter(!is.na(year)) ->
      L203.TechDesalCost_USA

    # ===================================================
    # Produce outputs
    L203.DeleteSupplysector_USA %>%
      add_title("Remove the water sectors from the USA region that are produced at the state level") %>%
      add_units("Unitless") %>%
      add_comments("Remove the USA electricity, municipal, and industrial water_td's") %>%
      add_comments("Also remove all energy-for-water (EFW) sectors") %>%
      add_precursors("L203.Supplysector_desal_basin",
                     "water/A71.sector",
                     "water/A72.sector",
                     "water/A73.sector",
                     "water/A74.sector") ->
      L203.DeleteSupplysector_USA

    L203.DeleteResTechInput %>%
      add_title("Remove the electricity inputs to groundwater supply curves") %>%
      add_units("Unitless") %>%
      add_comments("These would be pulling from a USA electricity market that does not exist in GCAM-USA") %>%
      add_precursors("L201.RsrcTechCoef") ->
      L203.DeleteResTechInput


    L203.DeleteSubsector_USA %>%
      add_title("Remove the three sectors that are produced at the state level") %>%
      add_units("Unitless") %>%
      add_comments("Remove the USA electricity, municipal, and industrial water_td's") %>%
      add_legacy_name("L2232.DeleteSubsector_USA") ->
      L203.DeleteSubsector_USA

    L203.Supplysector_USA %>%
      add_title("Water sector information") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector info expanded to USA and state regions for water demand sectors") %>%
      add_legacy_name("L203.Supplysector") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "L103.water_mapping_USA_R_GLU_W_Ws_share",
                     "L103.water_mapping_USA_R_B_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/water_td_sectors",
                     "water/A03.sector") ->
      L203.Supplysector_USA

    L203.SubsectorLogit_USA %>%
      add_title("Water subsector logit exponents for mapping sector") %>%
      add_units("Unitless") %>%
      add_comments("Subsector info expanded to USA and state regions for water demand sectors") %>%
      add_legacy_name("L203.SubsectorLogit") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "L103.water_mapping_USA_R_GLU_W_Ws_share",
                     "L103.water_mapping_USA_R_B_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/water_td_sectors",
                     "water/A03.sector") ->
      L203.SubsectorLogit_USA

    L203.SubsectorShrwt_USA %>%
      add_title("Water subsector share weights") %>%
      add_units("Unitless") %>%
      add_comments("Subsector shareweights expanded to USA and state regions for water demand sectors") %>%
      add_legacy_name("L203.SubsectorShrwtFllt") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_GLU_W_Ws_share",
                     "L103.water_mapping_USA_R_B_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/water_td_sectors",
                     "water/A03.sector") ->
      L203.SubsectorShrwt_USA

    L203.TechShrwt_USA %>%
      add_title("Water technology shareweights") %>%
      add_units("Unitless") %>%
      add_comments("Technology shareweights expanded to USA and state regions for water demand sectors") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.TechShrwt") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_GLU_W_Ws_share",
                     "L103.water_mapping_USA_R_B_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/water_td_sectors",
                     "water/A03.sector") ->
      L203.TechShrwt_USA

    L203.TechCoef_USA%>%
      add_title("Water technology coefficients") %>%
      add_units("Unitless") %>%
      add_comments("Technology coefficients expanded to USA and state regions for water demand sectors") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_GLU_W_Ws_share",
                     "L103.water_mapping_USA_R_B_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/water_td_sectors",
                     "water/A03.sector") ->
      L203.TechCoef_USA

    L203.TechDesalCoef_USA %>%
      add_title("Water technology desal coefficients") %>%
      add_units("Unitless") %>%
      add_comments("Desalination Coefficients for USA region and states. Available only for coastal states and basins") %>%
      add_legacy_name("L203.TechCoef") %>%
      same_precursors_as(L203.TechCoef_USA) ->
      L203.TechDesalCoef_USA

    L203.TechPmult_USA %>%
      add_title("Water technology price multipliers") %>%
      add_units("Unitless") %>%
      add_comments("Water price subsidy applied at USA and state level") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "L103.water_mapping_USA_R_B_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/water_td_sectors",
                     "water/A03.sector") ->
      L203.TechPmult_USA

    L203.TechDesalShrwt_USA %>%
      add_title("Water technology desal shareweights") %>%
      add_units("Unitless") %>%
      add_comments("Desalination Shareweights for USA region and states. Available only for coastal states and basins") %>%
      add_legacy_name("L203.TechCoef") %>%
      same_precursors_as(L203.TechShrwt_USA) %>%
      add_precursors("gcam-usa/usa_seawater_states_basins") ->
      L203.TechDesalShrwt_USA

    L203.TechDesalCost_USA %>%
      add_title("Water technology desal costs") %>%
      add_units("Unitless") %>%
      add_comments("Desalination fixed costs") %>%
      add_legacy_name("L203.TechCoef") %>%
      same_precursors_as(L203.TechShrwt_USA) ->
      L203.TechDesalCost_USA

    return_data(L203.DeleteSupplysector_USA,
                L203.DeleteResTechInput,
                L203.DeleteSubsector_USA,
                L203.Supplysector_USA,
                L203.SubsectorLogit_USA,
                L203.SubsectorShrwt_USA,
                L203.TechShrwt_USA,
                L203.TechCoef_USA,
                L203.TechDesalCoef_USA,
                L203.TechDesalShrwt_USA,
                L203.TechDesalCost_USA,
                L203.TechPmult_USA)
  } else {
    stop("Unknown command")
  }
}
