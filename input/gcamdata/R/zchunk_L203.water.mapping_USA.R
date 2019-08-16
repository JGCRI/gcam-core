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
             FILE = "gcam-usa/states_subregions",
             FILE = "water/A03.sector"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L203.Supplysector_USA",
             "L203.SubsectorLogit_USA",
             "L203.SubsectorShrwtFllt_USA",
             "L203.TechShrwt_USA",
             "L203.TechCoef_USA",
             "L203.DeleteSubsector_USAls"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
#    L125.LC_bm2_R_GLU <- get_data(all_data, "L125.LC_bm2_R_GLU")
#    L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R")
    L103.water_mapping_USA_R_LS_W_Ws_share <- get_data(all_data, "L103.water_mapping_USA_R_LS_W_Ws_share")
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
    GCAM_state_names %>%
      select(state) %>%
      rename(region = state) %>%
      repeat_add_columns(filter(A03.sector, !(water.sector %in% water.IRRIGATION))) %>%
      repeat_add_columns(tibble(water_type = water.MAPPED_WATER_TYPES)) %>%
      mutate(supplysector = set_water_input_name(water.sector, water_type, A03.sector)) ->
      L203.mapping_nonirr

    # (c) livestock sector
    # This done slightly different as production of livestock is not at the state level.
    # Here we take the regional (i.e. USA) water demands of livestock and map them to the state level based on
    # the amount of water for livestock that each state requires compared to the USA as a whole.
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

    tibble(region = gcam.USA_REGION,
           supplysector = c("water_td_an_W","water_td_an_C"),
           subsector = c("water_td_an_W","water_td_an_C")) ->
      L203.DeleteSubsector_USAls

    # (d) combine irrigation and non-irrigation sectors and add additional required columns
#    L203.mapping_irr %>%
#      bind_rows(L203.mapping_nonirr) %>%
    L203.mapping_nonirr %>%
      mutate(coefficient = 1,
             subsector = supplysector,
             technology = supplysector,
             share.weight =1,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      arrange(region) %>%
      bind_rows(L203.mapping_livestock) ->
      L203.mapping_all

    # Sector information
    L203.mapping_all %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME) ->
      L203.Supplysector

    # Subsector logit exponents for mapping sector
    L203.mapping_all %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME) ->
      L203.SubsectorLogit

    # Subsector share weights to 1 (no competition)
    L203.mapping_all %>%
      mutate(year.fillout = first(MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      L203.SubsectorShrwtFllt

    # Pass-through technology to the water resource (no competition)
    L203.mapping_all %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]]) ->
      L203.TechShrwt

    # Pass-through technology to the water resource
    # Market name is already defined for livestock and remain the same as defined above
    # In order for water to be mapped back to state level, water_td_an_* exists at the USA level,
    # but water consumption/withdrawal must exist at the state level
    L203.mapping_all %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(minicam.energy.input = case_when( water.sector == "Livestock" & region =="USA" ~ supplysector, TRUE~water_type),
             market.name = case_when( water.sector == "Livestock" & region =="USA" ~ market.name,TRUE~region)) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L203.TechCoef


    # OUTPUTS
    # possible future amendment: consider creating water mapping files by demand type ...
    # ...(irr, dom, ind, ...) rather than by variable (see issue #663 on dsr gcamdata repo)

    L203.DeleteSubsector_USAls %>%
      add_title("Remove the livestock water withdrawal and consumption of the USA region") %>%
      add_units("Uniteless") %>%
      add_comments("Remove the USA livestock water sectors, and replace with state level livestock sectors") %>%
      add_legacy_name("L2232.DeleteSubsector_USAls") ->
      L203.DeleteSubsector_USAls

    L203.Supplysector %>%
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
    L203.SubsectorLogit %>%
      add_title("Water subsector logit exponents for mapping sector") %>%
      add_units("Unitless") %>%
      add_comments("Subsector info expanded to GLU regions and water demand sectors") %>%
      add_legacy_name("L203.SubsectorLogit") %>%
      add_precursors("water/basin_to_country_mapping",
#                     "L125.LC_bm2_R_GLU",
#                     "L165.ag_IrrEff_R",
                      "L103.water_mapping_USA_R_LS_W_Ws_share",
                     "gcam-usa/states_subregions",
                     "water/A03.sector") ->
      L203.SubsectorLogit_USA
    L203.SubsectorShrwtFllt %>%
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
    L203.TechShrwt %>%
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
    L203.TechCoef %>%
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

    return_data(L203.Supplysector_USA, L203.SubsectorLogit_USA, L203.SubsectorShrwtFllt_USA, L203.TechShrwt_USA, L203.TechCoef_USA, L203.DeleteSubsector_USAls)
  } else {
    stop("Unknown command")
  }
}
