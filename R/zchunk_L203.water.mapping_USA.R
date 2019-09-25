#' module_gcamusa_L203.water.mapping_USA
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
#' @author NTG August 2019
module_gcamusa_L203.water.mapping_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_to_country_mapping",
             #             "L125.LC_bm2_R_GLU",
             #             "L165.ag_IrrEff_R",
             "L103.water_mapping_USA_R_LS_W_Ws_share",
             "L103.water_mapping_USA_R_PRI_W_Ws_share",
             FILE = "gcam-usa/states_subregions",
             FILE = "water/A03.sector"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L203.Supplysector_USA",
             "L203.SubsectorLogit_USA",
             "L203.SubsectorShrwtFllt_USA",
             "L203.TechShrwt_USA",
             "L203.TechCoef_USA",
             "L203.DeleteSubsector_USA",
             "L203.TechDesalCoef_USA",
             "L203.TechDesalShrwt_USA",
             "L203.TechDesalCost_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    #    L125.LC_bm2_R_GLU <- get_data(all_data, "L125.LC_bm2_R_GLU")
    #    L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R")
    L103.water_mapping_USA_R_LS_W_Ws_share <- get_data(all_data, "L103.water_mapping_USA_R_LS_W_Ws_share")
    L103.water_mapping_USA_R_PRI_W_Ws_share <- get_data(all_data, "L103.water_mapping_USA_R_PRI_W_Ws_share")
    GCAM_state_names <- get_data(all_data, "gcam-usa/states_subregions")
    A03.sector <- get_data(all_data, "water/A03.sector")

    GLU <- GLU_code <- GLU_name <- water.sector <-
      water_type <- supplysector <- field.eff <- conveyance.eff <-
      coefficient <- region <- state <- share <- NULL  # silence package check notes

    # Create tibble with all possible mapping sectors...

    # (a) irrigation sectors
    #L125.LC_bm2_R_GLU %>% ungroup %>%
    #  select(GCAM_region_ID, GLU) %>%
    #  left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GLU_name), by = c("GLU" = "GLU_code")) %>%
    #  # ^^ join GLU names, which will replace GLU codes in this tibble
    #  select(-GLU) %>% rename(GLU = GLU_name) %>%
    #  left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
    #  repeat_add_columns(filter(A03.sector, water.sector %in% water.IRRIGATION)) %>%
    #  repeat_add_columns(tibble(water_type = c("water consumption", "water withdrawals"))) %>%
    #  mutate(supplysector = set_water_input_name(water.sector, water_type, A03.sector, GLU)) ->
    #  L203.mapping_irr

    # (b) non-irrigation
    # create supply sectors for each non-irrigation and non-primary energy water type (i.e. livestock, electricity, municipal, industry).
    # While individual sectors will be mapped from the USA region next, the demands must be mapped to the state level
    # in order to draw from the regional mapping created below.
    GCAM_state_names %>%
      select(state) %>%
      rename(region = state) %>%
      repeat_add_columns(filter(A03.sector, !(water.sector %in% water.IRRIGATION)&!(water.sector %in% water.PRIMARY_ENERGY))) %>%
      repeat_add_columns(tibble(water_type = water.MAPPED_WATER_TYPES)) %>%
      mutate(supplysector = set_water_input_name(water.sector, water_type, A03.sector)) ->
      L203.mapping_nonirr

    # (c) livestock sector
    # This done slightly different as production of livestock is not modeled at the state level.
    # Here we take the regional (i.e. USA) water demands of livestock and map them to the state level based on
    # the amount of water for livestock that each state requires compared to the USA as a whole, computed in
    # L103.water.basin.mapping_USA
    L103.water_mapping_USA_R_LS_W_Ws_share %>%
      mutate(region=gcam.USA_REGION) %>%
      repeat_add_columns(filter(A03.sector, (water.sector %in% water.LIVESTOCK))) %>%
      mutate(wt_short = if_else(water_type == "water consumption", "C", "W"),
             supplysector = paste(supplysector, wt_short, sep = "_"),
             coefficient = 1,
             subsector = state,
             technology = supplysector,
             share.weight = share,
             market.name=state,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-wt_short, -share, -state) %>%
      arrange(region)  ->
      L203.mapping_livestock

    # (d) primary energy sector
    # We use USGS withdrawal data for primary energy mining and ratios of fresh to saline water withdrawals to
    # map the demands from USA values to state level. This is done in 2 parts in order to specify differences in
    # subsectors at the state and national levels, as well as differences in share weights (i.e. mapping to states,
    # mapping of fresh to desal within a state)
    L103.water_mapping_USA_R_PRI_W_Ws_share %>%
      mutate(region=state) %>%
      repeat_add_columns(filter(A03.sector, (water.sector %in% water.PRIMARY_ENERGY))) %>%
      mutate(wt_short = if_else(water_type == "water consumption", "C", "W"),
             supplysector = paste(supplysector, wt_short, sep = "_"),
             coefficient = 1,
             subsector = supplysector,
             technology = supplysector,
             share.weight = fresh.share,
             market.name=state,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-wt_short, -state.to.country.share, -state) %>%
      arrange(region)  ->
      L203.mapping_primary_state

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
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-wt_short, -state.to.country.share, -state) %>%
      arrange(region)  ->
      L203.mapping_primary_region

    ## No values are present for DC, therefore NAs are created. These are replaced with
    ## zero shareweights
    bind_rows(L203.mapping_primary_state,
              L203.mapping_primary_region) %>%
      replace_na(list(share.weight=0))->
      L203.mapping_primary

    # isolate the fresh to saline and state-level shares for 1990 which will be used for 1975, and isolate 2015, which will be
    # held constant through the end of the century
    L203.mapping_primary %>%
        dplyr::filter(year==gcamusa.MIN_PRIM_ENERGY_YEAR) %>%
      rename(initial.share=share.weight) %>%
      select(region, supplysector, subsector, technology,water.sector,initial.share) ->
      initial.share

    L203.mapping_primary %>%
      dplyr::filter(year==min(MODEL_FUTURE_YEARS)) %>%
      rename(final.share=share.weight) %>%
      select(region, supplysector, subsector, technology,water.sector,final.share) ->
      final.share

    L203.mapping_primary %>%
      gather_years("share.weight") %>%
      complete(nesting(region, supplysector, subsector, technology,water.sector,market.name, logit.year.fillout), year = c(year, MODEL_BASE_YEARS,MODEL_FUTURE_YEARS)) %>%
      #repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join(final.share, by=c("region", "supplysector", "subsector", "technology","water.sector")) %>%
      left_join(initial.share, by=c("region", "supplysector", "subsector", "technology","water.sector")) %>%
      mutate(share.weight = if_else((water.sector == water.PRIMARY_ENERGY&region ==gcam.USA_REGION&year<=2015&year>=1990), share.weight,
                                    if_else((water.sector == water.PRIMARY_ENERGY&region ==gcam.USA_REGION&year>2015), final.share,
                                            if_else((water.sector == water.PRIMARY_ENERGY&region ==gcam.USA_REGION&year<1990), initial.share, 1))),
      coefficient = 1) ->
    L203.mapping_primary_long



    # (d) combine all sectors and add additional required columns
    #    L203.mapping_irr %>%
    #      bind_rows(L203.mapping_nonirr) %>%
    L203.mapping_nonirr %>%
      mutate(coefficient = 1,
             subsector = supplysector,
             technology = supplysector,
             share.weight =1,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      arrange(region) %>%
      bind_rows(L203.mapping_livestock,L203.mapping_primary_long) ->
      L203.mapping_all_long

     L203.mapping_nonirr %>%
      mutate(coefficient = 1,
             subsector = supplysector,
             technology = supplysector,
             share.weight =1,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      arrange(region) %>%
      bind_rows(L203.mapping_livestock,L203.mapping_primary) ->
      L203.mapping_all

    # Delete water_td_an_* for the USA region that is currently already mapped in the
    # original module_water_L203.water.mapping file. This ensures shares sum to 1 rather
    # than 2.
    tibble(region = gcam.USA_REGION,
           supplysector = water.DELETE_DEMAND_TYPES,
           subsector = water.DELETE_DEMAND_TYPES) ->
      L203.DeleteSubsector_USA

    # Sector information
    L203.mapping_all %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME) ->
      L203.Supplysector_USA

    # Subsector logit exponents for mapping sector
    L203.mapping_all %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME) ->
      L203.SubsectorLogit_USA

    # Subsector share weights to 1 (no competition)

    L203.mapping_all_long %>%
      rename(year.fillout=year)%>%
      mutate(year.fillout = if_else((region==gcam.USA_REGION&water.sector==water.PRIMARY_ENERGY), year.fillout , first(MODEL_YEARS))) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      L203.SubsectorShrwtFllt_USA

#    L203.mapping_all %>%
#      gather_years("share.weight") %>%
#      complete(nesting(region, supplysector, subsector, technology,water.sector), year = c(year, MODEL_BASE_YEARS,MODEL_FUTURE_YEARS)) %>%
#      #repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
#      left_join(final.share, by=c("region", "supplysector", "subsector", "technology","water.sector")) %>%
#      left_join(initial.share, by=c("region", "supplysector", "subsector", "technology","water.sector")) %>%
#      mutate(share.weight = if_else((water.sector == water.PRIMARY_ENERGY&region ==gcam.USA_REGION&year<=2015&year>=1990), share.weight,
#                                    if_else((water.sector == water.PRIMARY_ENERGY&region ==gcam.USA_REGION&year>2015), final.share,
#                                            if_else((water.sector == water.PRIMARY_ENERGY&region ==gcam.USA_REGION&year<1990), initial.share,
#                                                    if_else(water.sector==water.LIVESTOCK, share.weight, 1))))) %>%
#      dplyr::filter(!is.na(year))%>%
#      rename(year.fillout=year)%>%
#      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
#      L203.SubsectorShrwtFllt_USA

    # Pass-through technology to the water resource (no competition)
    L203.mapping_all %>%
      gather_years("share.weight") %>%
      complete(nesting(region, supplysector, subsector, technology,water.sector), year = c(year, MODEL_BASE_YEARS,MODEL_FUTURE_YEARS)) %>%
      #repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join(final.share, by=c("region", "supplysector", "subsector", "technology","water.sector")) %>%
      left_join(initial.share, by=c("region", "supplysector", "subsector", "technology","water.sector")) %>%
      mutate(share.weight = if_else((water.sector == water.PRIMARY_ENERGY&region !=gcam.USA_REGION&year<=2015&year>=1990), share.weight,
                                    if_else((water.sector == water.PRIMARY_ENERGY&region !=gcam.USA_REGION&year>2015), final.share,
                                            if_else((water.sector == water.PRIMARY_ENERGY&region !=gcam.USA_REGION&year<1990), initial.share, 1)))) %>%
      dplyr::filter(!is.na(year))%>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]]) ->
      L203.TechShrwt_USA

    # Pass-through technology to the water resource
    # Market name is already defined for livestock and remains the same as defined above.
    # In order for water to be mapped back to state level, water_td_an_* exists at the USA level,
    # but water consumption/withdrawal must exist at the state level therefore minicam.energy.input differs
    # in the USA region compared to each state
    L203.mapping_all %>%
      gather_years("share.weight") %>%
      complete(nesting(region, supplysector, subsector, technology,water.sector,water_type,market.name,coefficient), year = c(year, MODEL_BASE_YEARS,MODEL_FUTURE_YEARS)) %>%
      mutate(minicam.energy.input = case_when( ( water.sector == "Livestock" | water.sector == "Mining") & region =="USA" ~ supplysector, TRUE~water_type),
             market.name = case_when( ( water.sector == "Livestock" | water.sector == "Mining") & region =="USA" ~ market.name,TRUE~region)) %>%
      dplyr::filter(!is.na(year))%>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L203.TechCoef_USA

    L203.TechCoef_USA %>%
      filter(!grepl("_irr_", supplysector)) %>%
      mutate(technology = "desalination",
             minicam.energy.input = "desalination") %>%
    dplyr::filter(!is.na(year))->
      L203.TechDesalCoef_USA

    # Inititally set share weights at zero for the USA region in order for
    # all of water_td to be transfered to state level. If held at 1, water_td
    # values are evenly split to desal + water_td, and the state level supplysector
    # only calls for water_td, thus missing half of true demands
    L203.TechShrwt_USA %>%
      filter(!grepl("_irr_", supplysector)) %>%
      mutate(technology = "desalination",
             share.weight = if_else(region=="USA",0,(1-share.weight))) %>%
      dplyr::filter(!is.na(year))->
      L203.TechDesalShrwt_USA

    L203.TechDesalShrwt_USA %>%
      rename(minicam.non.energy.input = share.weight) %>%
      mutate(minicam.non.energy.input = "final cost",
             input.cost = water.DESALINATION_PRICE) %>%
      dplyr::filter(!is.na(year))->
      L203.TechDesalCost_USA

    # OUTPUTS
    # possible future amendment: consider creating water mapping files by demand type ...
    # ...(irr, dom, ind, ...) rather than by variable (see issue #663 on dsr gcamdata repo)

    L203.DeleteSubsector_USA %>%
      add_title("Remove the livestock and primary energy water withdrawal and consumption of the USA region") %>%
      add_units("Uniteless") %>%
      add_comments("Remove the USA livestock and primary energy water sectors, and replace with state level livestock sectors") %>%
      add_legacy_name("L2232.DeleteSubsector_USA") ->
      L203.DeleteSubsector_USA

    L203.Supplysector_USA %>%
      add_title("Water sector information") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.Supplysector") %>%
      add_precursors("water/basin_to_country_mapping",
                     #                     "L125.LC_bm2_R_GLU",
                     #                     "L165.ag_IrrEff_R",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "water/A03.sector") ->
      L203.Supplysector_USA
    L203.SubsectorLogit_USA %>%
      add_title("Water subsector logit exponents for mapping sector") %>%
      add_units("Unitless") %>%
      add_comments("Subsector info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.SubsectorLogit") %>%
      add_precursors("water/basin_to_country_mapping",
                     #                     "L125.LC_bm2_R_GLU",
                     #                     "L165.ag_IrrEff_R",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "water/A03.sector") ->
      L203.SubsectorLogit_USA
    L203.SubsectorShrwtFllt_USA %>%
      add_title("Water subsector share weights") %>%
      add_units("Unitless") %>%
      add_comments("Subsector shareweights expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.SubsectorShrwtFllt") %>%
      add_precursors("water/basin_to_country_mapping",
                     #                     "L125.LC_bm2_R_GLU",
                     #                     "L165.ag_IrrEff_R",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "water/A03.sector") ->
      L203.SubsectorShrwtFllt_USA
    L203.TechShrwt_USA %>%
      add_title("Water technology shareweights") %>%
      add_units("Unitless") %>%
      add_comments("Technology shareweights expanded to GLU regions and water demand sectors") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.TechShrwt") %>%
      add_precursors("water/basin_to_country_mapping",
                     #                     "L125.LC_bm2_R_GLU",
                     #                     "L165.ag_IrrEff_R",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "water/A03.sector") ->
      L203.TechShrwt_USA
    L203.TechCoef_USA%>%
      add_title("Water technology coefficients") %>%
      add_units("Unitless") %>%
      add_comments("Technology info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     #                     "L125.LC_bm2_R_GLU",
                     #                     "L165.ag_IrrEff_R",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "water/A03.sector") ->
      L203.TechCoef_USA

    L203.TechDesalCoef_USA %>%
      add_title("Water technology desal coefficients") %>%
      add_units("Unitless") %>%
      add_comments("filtered for non-irrigation") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
#                    "L165.ag_IrrEff_R",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "water/A03.sector") ->
      L203.TechDesalCoef_USA

    L203.TechDesalShrwt_USA %>%
      add_title("Water technology desal shareweights") %>%
      add_units("Unitless") %>%
      add_comments("filtered for non-irrigation") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     #                    "L165.ag_IrrEff_R",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "water/A03.sector") ->
      L203.TechDesalShrwt_USA

    L203.TechDesalCost_USA %>%
      add_title("Water technology desal costs") %>%
      add_units("Unitless") %>%
      add_comments("filtered for non-irrigation") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     #                    "L165.ag_IrrEff_R",
                     "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "L103.water_mapping_USA_R_PRI_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "water/A03.sector") ->
      L203.TechDesalCost_USA

    return_data(L203.Supplysector_USA, L203.SubsectorLogit_USA, L203.SubsectorShrwtFllt_USA, L203.TechShrwt_USA, L203.TechCoef_USA, L203.DeleteSubsector_USA, L203.TechDesalCoef_USA, L203.TechDesalShrwt_USA, L203.TechDesalCost_USA)
  } else {
    stop("Unknown command")
  }
}
