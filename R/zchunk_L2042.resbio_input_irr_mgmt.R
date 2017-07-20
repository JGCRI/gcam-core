#' module_aglu_L2042.resbio_input_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2042.AgResBio_For}, \code{L2042.AgResBioCurve_For}, \code{L2042.GlobalResBio_Mill}, \code{L2042.StubResBioCurve_Mill}, \code{L2042.AgResBio_ag_irr_mgmt}, \code{L2042.AgResBioCurve_ag_irr_mgmt}. The corresponding file in the
#' original data system was \code{L2042.resbio_input_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2042.resbio_input_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/A_agSupplySector",
             FILE = "aglu/A_demand_technology",
             FILE = "aglu/A_resbio_curves",
             FILE = "aglu/A_bio_frac_prod_R",
             "L111.ag_resbio_R_C",
             "L103.ag_Prod_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2042.AgResBio_For",
             "L2042.AgResBioCurve_For",
             "L2042.GlobalResBio_Mill",
             "L2042.StubResBioCurve_Mill",
             "L2042.AgResBio_ag_irr_mgmt",
             "L2042.AgResBioCurve_ag_irr_mgmt"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_agSupplySector <- get_data(all_data, "aglu/A_agSupplySector")
    A_demand_technology <- get_data(all_data, "aglu/A_demand_technology")
    A_resbio_curves <- get_data(all_data, "aglu/A_resbio_curves")
    A_bio_frac_prod_R <- get_data(all_data, "aglu/A_bio_frac_prod_R")
    L111.ag_resbio_R_C <- get_data(all_data, "L111.ag_resbio_R_C")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L123.For_Prod_bm3_R_Y_GLU <- get_data(all_data, "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU")

    # following lines temporary while L123 is temp-data-inject
    L123.For_Prod_bm3_R_Y_GLU %>%
      gather(year, prod, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L123.For_Prod_bm3_R_Y_GLU


    # following line temproary while L103 is output from zchunk_LA103.ag_R_C_Y_GLU.R as grouped
    L103.ag_Prod_Mt_R_C_Y_GLU %>% ungroup -> L103.ag_Prod_Mt_R_C_Y_GLU

    # the following lines convert basin identification from the current GLU### level 1 names to the
    # level 2 names.
    L103.ag_Prod_Mt_R_C_Y_GLU %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L103.ag_Prod_Mt_R_C_Y_GLU

    L123.For_Prod_bm3_R_Y_GLU %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L123.For_Prod_bm3_R_Y_GLU

    # add actual region names to table A_bio_frac_prod_R for joining later
    A_bio_frac_prod_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      A_bio_frac_prod_R


    # Forestry and Mill residue bio

    # The function, add_bio_res_params_For_Mill, takes a data frame and adds user specified parameters and
    # then repeats the resulting data frame for all MODEL_YEARS to form the  AgResBio_source
    # output table
    add_bio_res_params_For_Mill <- function(df, residueBiomassProduction, massConversion, harvestIndex, erosCtrl, massToEnergy, waterContent){
      df %>%
        mutate(residue.biomass.production = residueBiomassProduction,
               mass.conversion = massConversion,
               harvest.index = harvestIndex,
               eros.ctrl = erosCtrl,
               mass.to.energy = massToEnergy,
               water.content = waterContent) %>%
        repeat_add_columns(tibble::tibble(year = MODEL_YEARS))  ->
        df1
      df1
    } # end add_bio_res_params_For_Mill


    # 1. Form a table of Forest Residue Biomass Paramters by region-glu-year
    L123.For_Prod_bm3_R_Y_GLU %>%
      # Set up identifying information to fill in with parameters, incl 2.
      select(GCAM_region_ID, GCAM_commodity, GLU) %>%
      distinct %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = AgSupplySubsector) %>%
      add_bio_res_params_For_Mill("biomass", aglu.AVG_WOOD_DENSITY_KGM3, aglu.FOREST_HARVEST_INDEX,
                                  aglu.FOREST_EROSION_CTRL_KGM2, aglu.WOOD_ENERGY_CONTENT_GJKG,
                                  aglu.WOOD_WATER_CONTENT) %>%
      select(-GCAM_region_ID, -GCAM_commodity, -GLU) ->
      L204.AgResBio_For


    # 2. Form a table of global Mill Residue Biomass Paramters by year
    A_demand_technology %>%
      filter(supplysector == "NonFoodDemand_Forest") %>%
      select(supplysector, subsector, technology) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      add_bio_res_params_For_Mill("biomass", aglu.AVG_WOOD_DENSITY_KGM3, aglu.FOREST_HARVEST_INDEX,
                                  aglu.MILL_EROSION_CTRL_KGM2, aglu.WOOD_ENERGY_CONTENT_GJKG,
                                  aglu.WOOD_WATER_CONTENT) ->
      L204.GlobalResBio_Mill


    # 3. Forestry and Mill residue supply curves
    # First, the Forest and Mill information bound into a single data frame for processing.
    # Then build in a base residue biomass supply curve table, adding in the relevant residual bio forest vs price curve for
    # each Region - agsupply-year combo, and rename to reflect the fact that For = Fraction of  forest harvested for a
    # given price level. Both Forest and Mill get the forest vs price curve in the Old Data System. Assuming this is
    # an error, since a mill vs price curve is in A_resbio_curves. Therefore an OLD_DATA_SYSTEM_BEHAVIO flag is used
    # to code the two different behaviors.
    L204.AgResBio_For %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, residue.biomass.production) %>%
      mutate(colID = "For") ->
      For.tmp

    if(OLD_DATA_SYSTEM_BEHAVIOR){
      # Mill outputs are getting For base resbio curves from A_resbio_curves;
      # Likely a typo in the old DS, since there IS a Mill base curve in A_resbio_curves
      L204.GlobalResBio_Mill %>%
        # rename columns just for binding and working in a single pipeline
        rename(AgSupplySector = sector.name,
               AgSupplySubsector = subsector.name,
               AgProductionTechnology = technology) %>%
        select(AgSupplySector, AgSupplySubsector, AgProductionTechnology, residue.biomass.production) %>%
        write_to_all_regions(names = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "residue.biomass.production"),
                             GCAM_region_names) %>%
        filter(! region %in% aglu.NO_AGLU_REGIONS) %>%
        distinct %>%
        repeat_add_columns(bind_cols(tibble::tibble(year = MODEL_YEARS))) %>%
        mutate(colID = "Mill") %>%
        # bind to processed Forest data
        bind_rows(For.tmp) %>%
        # join in base resbio curves
        repeat_add_columns(select(A_resbio_curves, price, For)) %>%
        # assign appropriate fraction harvested based on colID
        rename(fract.harvested = For) ->
        # store in a temporary table for further processing
        For.Mill.tmp
    } else{
      # Mill gets its OWN base resbio_curve from A_resbio_curves
      # Correct one ; need first repeat_add_columns to just do For for both for Old DS behavior
      L204.GlobalResBio_Mill %>%
        # rename columns just for binding and working in a single pipeline
        rename(AgSupplySector = sector.name,
               AgSupplySubsector = subsector.name,
               AgProductionTechnology = technology) %>%
        select(AgSupplySector, AgSupplySubsector, AgProductionTechnology, residue.biomass.production) %>%
        write_to_all_regions(names = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "residue.biomass.production"),
                             GCAM_region_names) %>%
        filter(! region %in% aglu.NO_AGLU_REGIONS) %>%
        repeat_add_columns(bind_cols(tibble::tibble(year = MODEL_YEARS))) %>%
        mutate(colID = "Mill") %>%
        # bind to processed Forest data
        bind_rows(For.tmp) %>%
        # join in base resbio curves
        repeat_add_columns(select(A_resbio_curves, price, For, Mill)) %>%
        # assign appropriate fraction harvested based on colID
        mutate(fract.harvested = if_else(colID == "Mill", Mill, For)) %>%
        select(-For, -Mill) ->
        # store in a temporary table for further processing
        For.Mill.tmp
    }

    # In base years, replace the "fraction produced" at specified price aglu.PRICE_BIO_FRAC in the model BASE_YEAR,
    # in order to calibrate resbio production. The fract.harvested for these prices-years are replaced by
    # the GCAM_region's Base-year fraction of residue biomass produced by region for Forest/Mill as appropriate,
    # from assumption file A_bio_frac_prod_R
    For.Mill.tmp %>%
      filter(price == aglu.PRICE_BIO_FRAC, year %in% BASE_YEARS) %>%
      left_join_error_no_match(select(A_bio_frac_prod_R, region, For, Mill), by = "region") %>%
      select(-fract.harvested) %>%
      # assign appropriate fraction harvested based on colID
      mutate(fract.harvested = if_else(colID == "Mill", Mill, For)) %>%
      select(-For, -Mill) %>%
      bind_rows(filter(For.Mill.tmp, !( price == aglu.PRICE_BIO_FRAC & year %in% BASE_YEARS))) ->
      For.Mill.tmp2

    # Forest
    For.Mill.tmp2 %>%
      filter(colID == "For") %>%
      select(-colID) ->
      L204.AgResBioCurve_For

    # Mill
    For.Mill.tmp2 %>%
      filter(colID == "Mill") %>%
      select(-colID) %>%
      # return correct column names
      rename(supplysector = AgSupplySector,
             subsector = AgSupplySubsector,
             stub.technology = AgProductionTechnology) ->
      L204.StubResBioCurve_Mill


    # AGRICULTURE RESIDUE BIO
    # 4. Form a table of Agricultural Residue Biomass Paramters by region-glu-year
    L103.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GLU) %>%
      distinct %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # have to use left_join + na.omit to match  m e r g e  behavior in old DS
      left_join(L111.ag_resbio_R_C, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      na.omit %>%
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = AgSupplySubsector,
             residue.biomass.production = "biomass",
             # fill in parameters
             mass.conversion = aglu.AVG_AG_DENSITY,
             harvest.index = round(HarvestIndex, aglu.DIGITS_HARVEST_INDEX),
             eros.ctrl = round(ErosCtrl_tHa * CONV_THA_KGM2, aglu.DIGITS_EROS_CTRL),
             mass.to.energy = round(ResEnergy_GJt * CONV_KG_T, aglu.DIGITS_RES_ENERGY),
             water.content = round(WaterContent, aglu.DIGITS_WATER_CONTENT)) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, residue.biomass.production,
             mass.conversion, harvest.index, eros.ctrl, mass.to.energy, water.content) %>%
      repeat_add_columns(tibble::tibble(Irr_Rfd = c( "IRR", "RFD" ))) %>%
      repeat_add_columns(tibble::tibble(level = c( "lo", "hi" ))) %>%
      mutate(AgProductionTechnology = paste(paste(AgProductionTechnology, Irr_Rfd, sep = aglu.IRR_DELIMITER),
                                            level, sep = aglu.MGMT_DELIMITER)) %>%
      select(-Irr_Rfd, -level) ->
      L2042.AgResBio_ag_irr_mgmt

    # 5. Agricultural residue biomass supply curves
    # Much simpler than for Mill or forest, no replacing of base year values.
    L2042.AgResBio_ag_irr_mgmt %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, residue.biomass.production) %>%
      repeat_add_columns(select(A_resbio_curves, price, ag)) %>%
      rename(fract.harvested = ag) ->
      L2042.AgResBioCurve_ag_irr_mgmt


    # Produce outputs
    L204.AgResBio_For %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2042.AgResBio_For") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "aglu/A_demand_technology",
                     "aglu/A_resbio_curves",
                     "aglu/A_bio_frac_prod_R",
                     "L111.ag_resbio_R_C",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") ->
      L2042.AgResBio_For
    L204.AgResBioCurve_For %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2042.AgResBioCurve_For") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "aglu/A_demand_technology",
                     "aglu/A_resbio_curves",
                     "aglu/A_bio_frac_prod_R",
                     "L111.ag_resbio_R_C",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU")  %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2042.AgResBioCurve_For
    L204.GlobalResBio_Mill %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2042.GlobalResBio_Mill") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "aglu/A_demand_technology",
                     "aglu/A_resbio_curves",
                     "aglu/A_bio_frac_prod_R",
                     "L111.ag_resbio_R_C",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU")  ->
      L2042.GlobalResBio_Mill
    L204.StubResBioCurve_Mill %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2042.StubResBioCurve_Mill") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "aglu/A_demand_technology",
                     "aglu/A_resbio_curves",
                     "aglu/A_bio_frac_prod_R",
                     "L111.ag_resbio_R_C",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") ->
      L2042.StubResBioCurve_Mill
    L2042.AgResBio_ag_irr_mgmt %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2042.AgResBio_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "aglu/A_demand_technology",
                     "aglu/A_resbio_curves",
                     "aglu/A_bio_frac_prod_R",
                     "L111.ag_resbio_R_C",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") ->
      L2042.AgResBio_ag_irr_mgmt
    L2042.AgResBioCurve_ag_irr_mgmt %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2042.AgResBioCurve_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "aglu/A_demand_technology",
                     "aglu/A_resbio_curves",
                     "aglu/A_bio_frac_prod_R",
                     "L111.ag_resbio_R_C",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2042.AgResBioCurve_ag_irr_mgmt

    return_data(L2042.AgResBio_For, L2042.AgResBioCurve_For, L2042.GlobalResBio_Mill, L2042.StubResBioCurve_Mill, L2042.AgResBio_ag_irr_mgmt, L2042.AgResBioCurve_ag_irr_mgmt)
  } else {
    stop("Unknown command")
  }
}
