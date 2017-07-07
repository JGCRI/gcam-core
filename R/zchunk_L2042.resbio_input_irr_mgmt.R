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


    # Build Tables

    # add actual region names to table A_bio_frac_prod_R for joining later
    A_bio_frac_prod_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      A_bio_frac_prod_R


    # FORESTRY RESIDUE BIO

    # 1. Form a table of Forest Residue Biomass Paramters by region-glu-year
    L123.For_Prod_bm3_R_Y_GLU %>%
      # Set up identifying information to fill in with parameters, incl 2.
      select(GCAM_region_ID, GCAM_commodity, GLU) %>%
      distinct %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = AgSupplySubsector,
             ### from here down could be a function for Forestry and Mill - not Agriculture
             ### just add For/Mill identifier and gather a couple of the external tables
             residue.biomass.production = "biomass",
             # fill in parameters
             mass.conversion = aglu.AVG_WOOD_DENSITY_KGM3,
             harvest.index = aglu.FOREST_HARVEST_INDEX,
             eros.ctrl = aglu.FOREST_EROSION_CTRL_KGM2,
             mass.to.energy = aglu.WOOD_ENERGY_CONTENT_GJKG,
             water.content = aglu.WOOD_WATER_CONTENT) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) ->
      L204.AgResBio_For


    # 2. Forest Residue biomass supply curves

    # build in a base residue biomass supply curve table, adding in the relevant residual bio forest vs price curve for
    # each Region - agsupply-year combo, and rename to reflect the fact that For = Fraction of  forest harvested for a
    # given price level.
    L204.AgResBio_For %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, residue.biomass.production) %>%
      repeat_add_columns(select(A_resbio_curves, price, For)) %>%
      rename(fract.harvested = For) ->
      # store for rejoining after further manipulation
      L204.AgResBioCurve_For_tmp

    # In base years, replace the "fraction produced" at specified price aglu.PRICE_BIO_FRAC in the model BASE_YEAR,
    # in order to calibrate resbio production. The fract.harvested for these prices X years are replaced by
    # the GCAM_region's Base-year fraction of residue biomass produced by region for Forest, from assumption file
    # A_bio_frac_prod_R
    L204.AgResBioCurve_For_tmp %>%
      filter(price == aglu.PRICE_BIO_FRAC, year %in% BASE_YEARS) %>%
      left_join(select(A_bio_frac_prod_R, region, For), by = "region") %>%
      select(-fract.harvested) %>%
      rename(fract.harvested = For) %>%
      # join back to full table of AgResBioCurve_For
      bind_rows(filter(L204.AgResBioCurve_For_tmp, !( price == aglu.PRICE_BIO_FRAC & year %in% BASE_YEARS))) ->
      L204.AgResBioCurve_For


    # MILL RESIDUE BIO - this IS a technology in the global tech database

    # 3. Form a table of Mill Residue Biomass Paramters by region-glu-year
    A_demand_technology %>%
      filter(supplysector == "NonFoodDemand_Forest") %>%
      select(supplysector, subsector, technology) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector)%>%
      # add int he parameters, could be combined/made a function for this and Forest
      mutate(residue.biomass.production = "biomass",
             mass.conversion = aglu.AVG_WOOD_DENSITY_KGM3,
             harvest.index = aglu.FOREST_HARVEST_INDEX,
             eros.ctrl = aglu.MILL_EROSION_CTRL_KGM2,
             mass.to.energy = aglu.WOOD_ENERGY_CONTENT_GJKG,
             water.content = aglu.WOOD_WATER_CONTENT) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS))->
      L204.GlobalResBio_Mill



    # 4. Mill Residue biomass supply curves
      A_demand_technology %>%
      filter(supplysector == "NonFoodDemand_Forest") %>%
      select(supplysector, subsector, technology) %>%
      write_to_all_regions(names = c("region", "supplysector", "subsector", "technology"),
                           GCAM_region_names) %>%
      filter(! region %in% aglu.NO_AGLU_REGIONS) ->
      L204.R_Mill_tech





    # AGRICULTURE



    # Produce outputs
    tibble() %>%
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
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2042.AgResBio_For
    tibble() %>%
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
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2042.AgResBioCurve_For
    tibble() %>%
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
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2042.GlobalResBio_Mill
    tibble() %>%
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
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2042.StubResBioCurve_Mill
    tibble() %>%
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
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2042.AgResBio_ag_irr_mgmt
    tibble() %>%
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
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2042.AgResBioCurve_ag_irr_mgmt

    return_data(L2042.AgResBio_For, L2042.AgResBioCurve_For, L2042.GlobalResBio_Mill, L2042.StubResBioCurve_Mill, L2042.AgResBio_ag_irr_mgmt, L2042.AgResBioCurve_ag_irr_mgmt)
  } else {
    stop("Unknown command")
  }
}
