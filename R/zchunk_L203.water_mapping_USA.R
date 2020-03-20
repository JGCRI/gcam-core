#' module_gcamusa_L203.water_mapping_USA
#'
#' Mapping of water consumption/withdrawal to sectoral demands at the state level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L203.Supplysector}, \code{L203.SubsectorLogit}, \code{L203.SubsectorShrwtFllt}, \code{L203.TechShrwt}, \code{L203.TechCoef}. The corresponding file in the
#' original data system was \code{L203.water.mapping.R} (water level2).
#' @details Generates water mapping sector input files to group demands by sectors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author NTG Oct 2019
module_gcamusa_L203.water_mapping_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_ID",
             FILE = "water/basin_to_country_mapping",
             "L103.water_mapping_USA_R_LS_W_Ws_share",
             "L103.water_mapping_USA_R_PRI_W_Ws_share",
             "L103.water_mapping_USA_R_GLU_W_Ws_share",
             "L103.water_mapping_USA_R_B_W_Ws_share",
             FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/state_and_basin",
             FILE = "water/A03.sector"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L203.DeleteSupplysector_USA",
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
    basin_ID <- get_data(all_data, "water/basin_ID")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    L103.water_mapping_USA_R_LS_W_Ws_share <- get_data(all_data, "L103.water_mapping_USA_R_LS_W_Ws_share")
    L103.water_mapping_USA_R_PRI_W_Ws_share <- get_data(all_data, "L103.water_mapping_USA_R_PRI_W_Ws_share")
    L103.water_mapping_USA_R_GLU_W_Ws_share <- get_data(all_data,"L103.water_mapping_USA_R_GLU_W_Ws_share")
    L103.water_mapping_USA_R_B_W_Ws_share <- get_data(all_data,"L103.water_mapping_USA_R_B_W_Ws_share")
    GCAM_state_names <- get_data(all_data, "gcam-usa/states_subregions")
    state_and_basin <- get_data(all_data, "gcam-usa/state_and_basin")
    A03.sector <- get_data(all_data, "water/A03.sector")

    GLU <- GLU_code <- GLU_name <- water.sector <-
      water_type <- supplysector <- field.eff <- conveyance.eff <-
      coefficient <- region <- state <- share <- NULL  # silence package check notes

      # Define in which states GCAM water basins exist by using data from R package created by Chris Vernon
    state_and_basin %>%
      left_join_error_no_match(basin_to_country_mapping,by=c("basin_id"="GCAM_basin_ID")) %>%
      select(-basin_name, -Basin_name) %>%
      left_join_error_no_match(basin_ID,by=c("basin_id")) %>%
      ## Replaces incorrect basin naming to match resouces in unlimited_water_supply.xml
      select(basin_id,GLU_name,basin_name,state_abbr) %>% rename(region=state_abbr) ->
      state_and_basin_mapping

      # Create mappings for the sectors that have production at the state level already.
      # These sectors: Industrial, Municipal, and Electricity will not need to be shared
      # from the USA region to the states, and thus will not have separate market names by region
    L103.water_mapping_USA_R_B_W_Ws_share %>%
      mutate(water_sector = gsub("Domestic","Municipal",water_sector)) %>%
      left_join(A03.sector, by=c("water_sector" = "water.sector")) %>%
      mutate(supplysector = set_water_input_name(water_sector, water_type, A03.sector)) ->
      L203.mapping_nonirr

      # Using irrigation shares, define water sector and add demand categories
    L103.water_mapping_USA_R_GLU_W_Ws_share %>%
      rename(state=region) %>%
      mutate(region=gcam.USA_REGION) %>%
      repeat_add_columns(filter(A03.sector, water.sector %in% water.IRRIGATION)) %>%
      mutate(supplysector = set_water_input_name(water.sector, water_type, A03.sector, GLU_name)) ->
      L203.mapping_irr

      # Isolate the USA region which will share basin level demands in the USA region to
      # States which are defined as subsectors
    L203.mapping_irr %>%
      mutate(subsector = state,
             technology = supplysector,
             coefficient = if_else(water.sector == water.IRRIGATION & water_type == "water withdrawals",
                                   1/gcamusa.CONVEYANCE_LOSSES, 1),
      ## ^^ conveyance losses for irrigation--applied to withdrawals only
      # Note: Conveyance losses are taken out of agriculture withdrawals and...
      # ... instead applied to water distribution sectors (water_td_irr). This means that to get total...
      # ... ag withdrawals for reporting (i.e., when querying GCAM results)...
      # ... it is necessary to include the conveyance loss.
             share.weight = share,
             market.name = state,
             share.weight.year=year,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-share, -state) %>%
      arrange(region) ->
      L203.mapping_irr_region

    # We must now set all subsectors in USA from gcam-core and water_mapping.xml to 0 so that we do not double count
    # demands
    L203.mapping_irr_region %>%
      bind_rows(L203.mapping_irr_region %>%
                  mutate(subsector=basin_name,
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
             coefficient = 1,
             share.weight = 1,
             market.name = gcam.USA_REGION,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-share, -state) %>%
      arrange(region) ->
      L203.mapping_irr_state

      # Combine state and USA region irrigation mappings
    bind_rows(
      L203.mapping_irr_region,
      L203.mapping_irr_state
    ) ->
      L203.mapping_irr



    # Livestock sector:
    # This done slightly different as production of livestock is not modeled at the state level.
    # Here we take the regional (i.e. USA) water demands of livestock and map them to the state level based on
    # the amount of water for livestock that each state requires compared to the USA as a whole, computed in
    # L103.water_mapping_USA
    L103.water_mapping_USA_R_LS_W_Ws_share %>%
      mutate(region=gcam.USA_REGION) %>%
      repeat_add_columns(filter(A03.sector, (water.sector %in% water.LIVESTOCK))) %>%
      mutate(wt_short = if_else(water_type == "water consumption", "C", "W"),
             supplysector = paste(supplysector, wt_short, sep = "_"),
             coefficient = 1,
             subsector = state,
             technology = supplysector,
             share.weight = value,
             market.name=state,
             share.weight.year=year,
             logit.year.fillout=first(MODEL_BASE_YEARS)) %>%
      select(-wt_short, -value, -state) %>%
      arrange(region)  ->
      L203.mapping_livestock

    L203.mapping_livestock %>%
      bind_rows(L203.mapping_livestock %>%
        left_join(state_and_basin_mapping, by=c("subsector"="region")) %>%
        mutate(share.weight = 0,
             subsector = basin_name,
             technology = basin_name,
             market.name= gcam.USA_REGION) %>%
          unique()
        ) ->
      L203.mapping_livestock

    # (d) primary energy sector
    # We use USGS withdrawal data for primary energy mining and ratios of fresh to saline water withdrawals to
    # map the demands from USA values to state level. This is done in 2 parts in order to specify differences in
    # subsectors at the state and national levels, as well as differences in share weights (i.e. mapping to states,
    # mapping of fresh to desal within a state)



    L103.water_mapping_USA_R_PRI_W_Ws_share %>%
      mutate(region=gcam.USA_REGION) %>%
      repeat_add_columns(filter(A03.sector, (water.sector %in% water.PRIMARY_ENERGY))) %>%
      mutate(wt_short = if_else(water_type == "water consumption", "C", "W"),
             supplysector = paste(supplysector, wt_short, sep = "_"),
             coefficient = 1,
             subsector = state,
             technology = supplysector,
             share.weight = state.to.country.share,
             market.name=state,
             share.weight.year=year,
             logit.year.fillout=first(MODEL_BASE_YEARS)) %>%
      select(-wt_short, -state.to.country.share, -state) %>%
      arrange(region)  ->
      L203.mapping_primary_region

    L203.mapping_primary_region %>%
      bind_rows(L203.mapping_primary_region %>%
                  left_join(state_and_basin_mapping, by=c("subsector"="region")) %>%
                  mutate(share.weight = 0,
                         subsector = basin_name,
                         technology = basin_name,
                         market.name= gcam.USA_REGION) %>%
                  unique()
      ) ->
      L203.mapping_primary_region

    ## No values are present for DC, therefore NAs are created. These are replaced with
    ## zero shareweights

     L203.mapping_primary_region %>%
      replace_na(list(share.weight=0)) %>%
      replace_na(list(fresh.share=0))->
      L203.mapping_primary


    # combine all sectors and add additional required columns. Long format is used for
    # subsector share weights, additional mapping is used for all other final outputs

    L203.mapping_nonirr %>%
                  mutate(coefficient = 1,
                         subsector = basin_name,
                         technology = basin_name,
                         share.weight =share,
                         logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      arrange(region) %>%
      bind_rows(L203.mapping_livestock,L203.mapping_primary,L203.mapping_irr)%>%
      mutate(pMult = if_else(water.sector == water.IRRIGATION & water_type == "water withdrawals" & region!=gcam.USA_REGION,
                             water.IRR_PRICE_SUBSIDY_MULT, water.MAPPING_PMULT)) ->
      L203.mapping_all

    tibble(region = gcam.USA_REGION,
           supplysector = water.DELETE_DEMAND_TYPES) ->
      L203.DeleteSupplysector_USA

    # Sector information
    L203.mapping_all %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME) ->
      L203.Supplysector_USA

    # Subsector logit exponents for mapping sector
    L203.mapping_all %>%
      #mutate(logit.exponent = case_when(region!=gcam.USA_REGION&water_sector!=water.IRRIGATION ~ water.LOGIT_EXP,TRUE~0)) %>%
      mutate(logit.exponent = water.LOGIT_EXP) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME) ->
      L203.SubsectorLogit_USA

    # Subsector share weights to 1 (no competition) in all states. Sharing happens at USA level. Water prices
    # will drive competition between the basins at the state level
    L203.mapping_all %>%
      gather_years("share.weight") %>%
      complete(nesting(region, supplysector, subsector, technology,water.sector,basin_name,water_type,coefficient), year = c(year, MODEL_BASE_YEARS,MODEL_FUTURE_YEARS)) %>%
      dplyr::filter(!is.na(year))%>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwt"]]) ->
      L203.SubsectorShrwt_USA


    # Technology share weights, defined by state and sector
    L203.mapping_all %>%
      gather_years("share.weight") %>%
      complete(nesting(region, supplysector, subsector, technology,water.sector,basin_name,water_type,coefficient), year = c(year, MODEL_BASE_YEARS,MODEL_FUTURE_YEARS)) %>%
      mutate(share.weight=1) %>%
      dplyr::filter(!is.na(year))%>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]]) ->
      L203.TechShrwt_USA

    # Define market name and minicam energy input dependent upon whether the sector is
    # produced at the state level or is we map from USA region to state
    L203.mapping_all %>%
      gather_years("share.weight") %>%
      complete(nesting(region, supplysector, subsector, technology,water.sector,basin_name,water_type,coefficient), year = c(year, MODEL_BASE_YEARS,MODEL_FUTURE_YEARS)) %>%
      mutate(minicam.energy.input = case_when(region ==gcam.USA_REGION&grepl("water_td",technology) ~ supplysector, TRUE~paste0(basin_name,"_",water_type)),
             market.name = case_when( region ==gcam.USA_REGION&grepl("water_td",technology) ~ subsector,TRUE~gcam.USA_REGION)) %>%
      dplyr::filter(!is.na(year))%>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L203.TechCoef_USA

    # Pass-through technology water price adjust if there one
    L203.mapping_all %>%
      gather_years("share.weight") %>%
      complete(nesting(region, supplysector, subsector, technology,water.sector,basin_name,water_type,coefficient), year = c(year, MODEL_BASE_YEARS,MODEL_FUTURE_YEARS)) %>%
      replace_na(list(pMult=1)) %>%
      select(LEVEL2_DATA_NAMES[["TechPmult"]]) ->
      L203.TechPmult_USA

    L203.TechCoef_USA %>%
      #filter(!grepl("_irr_", supplysector)) %>%
      filter(region!=gcam.USA_REGION) %>%
      mutate(technology = "desalination",
             minicam.energy.input = "desalination",
             market.name=gcam.USA_REGION) %>%
      dplyr::filter(!is.na(year))->
      L203.TechDesalCoef_USA

    ## Set shareweight of desalination technologies to 0 in all non-coastal states
    L203.TechShrwt_USA %>%
      #filter(!grepl("_irr_", supplysector)) %>%
      filter(region!=gcam.USA_REGION) %>%
      mutate(technology = "desalination",
             share.weight = if_else((region %in% gcamusa.NO_SEAWATER_STATES)&grepl("Rio",subsector),0, 1))  %>%
      dplyr::filter(!is.na(year))->
      L203.TechDesalShrwt_USA

    L203.TechDesalShrwt_USA %>%
      rename(minicam.non.energy.input = share.weight) %>%
      mutate(minicam.non.energy.input = "final cost",
             input.cost = water.DESALINATION_PRICE) %>%
      dplyr::filter(!is.na(year))->
      L203.TechDesalCost_USA


   # ===================================================
   # Produce outputs
    L203.DeleteSupplysector_USA %>%
      add_title("Remove the three sectors that are produced at the state level") %>%
      add_units("Unitless") %>%
      add_comments("Remove the USA electricity, municipal, and industrial water_td's") %>%
      add_legacy_name("L2232.DeleteSubsector_USA") ->
      L203.DeleteSupplysector_USA

    L203.Supplysector_USA %>%
      add_title("Water sector information") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.Supplysector") %>%
      add_precursors("water/basin_ID",
                     "water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "L103.water_mapping_USA_R_GLU_W_Ws_share",
                     "L103.water_mapping_USA_R_B_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/A03.sector") ->
      L203.Supplysector_USA
    L203.SubsectorLogit_USA %>%
      add_title("Water subsector logit exponents for mapping sector") %>%
      add_units("Unitless") %>%
      add_comments("Subsector info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.SubsectorLogit") %>%
      add_precursors("water/basin_ID",
                     "water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "L103.water_mapping_USA_R_GLU_W_Ws_share",
                     "L103.water_mapping_USA_R_B_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/A03.sector") ->
      L203.SubsectorLogit_USA
    L203.SubsectorShrwt_USA %>%
      add_title("Water subsector share weights") %>%
      add_units("Unitless") %>%
      add_comments("Subsector shareweights expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.SubsectorShrwtFllt") %>%
      add_precursors("water/basin_ID",
                     "water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_GLU_W_Ws_share",
                     "L103.water_mapping_USA_R_B_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/A03.sector") ->
      L203.SubsectorShrwt_USA
    L203.TechShrwt_USA %>%
      add_title("Water technology shareweights") %>%
      add_units("Unitless") %>%
      add_comments("Technology shareweights expanded to GLU regions and water demand sectors") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.TechShrwt") %>%
      add_precursors("water/basin_ID",
                     "water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_GLU_W_Ws_share",
                     "L103.water_mapping_USA_R_B_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/A03.sector") ->
      L203.TechShrwt_USA
    L203.TechCoef_USA%>%
      add_title("Water technology coefficients") %>%
      add_units("Unitless") %>%
      add_comments("Technology info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_ID",
                     "water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_GLU_W_Ws_share",
                     "L103.water_mapping_USA_R_B_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/A03.sector") ->
      L203.TechCoef_USA

    L203.TechDesalCoef_USA %>%
      add_title("Water technology desal coefficients") %>%
      add_units("Unitless") %>%
      add_comments("filtered for non-irrigation") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_ID",
                     "water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/A03.sector") ->
      L203.TechDesalCoef_USA

    L203.TechPmult_USA %>%
      add_title("Water technology price multipliers") %>%
      add_units("Unitless") %>%
      add_comments("Technology info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_ID",
                     "water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "L103.water_mapping_USA_R_B_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/A03.sector") ->
      L203.TechPmult_USA

    L203.TechDesalShrwt_USA %>%
      add_title("Water technology desal shareweights") %>%
      add_units("Unitless") %>%
      add_comments("filtered for non-irrigation") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_ID",
                     "water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/A03.sector") ->
      L203.TechDesalShrwt_USA

    L203.TechDesalCost_USA %>%
      add_title("Water technology desal costs") %>%
      add_units("Unitless") %>%
      add_comments("filtered for non-irrigation") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_ID",
                     "water/basin_to_country_mapping",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "gcam-usa/state_and_basin",
                     "water/A03.sector") ->
      L203.TechDesalCost_USA

    return_data(L203.DeleteSupplysector_USA,  L203.Supplysector_USA, L203.SubsectorLogit_USA, L203.SubsectorShrwt_USA, L203.TechShrwt_USA, L203.TechCoef_USA,  L203.TechDesalCoef_USA, L203.TechDesalShrwt_USA, L203.TechDesalCost_USA, L203.TechPmult_USA)
  } else {
    stop("Unknown command")
  }
}
