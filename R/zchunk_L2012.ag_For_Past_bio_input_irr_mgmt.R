#' module_aglu_L2012.ag_For_Past_bio_input_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2012.AgSupplySector}, \code{L2012.AgSupplySubsector}, \code{L2012.AgProduction_ag_irr_mgmt}, \code{L2012.AgProduction_For}, \code{L2012.AgProduction_Past}, \code{L2012.AgHAtoCL_irr_mgmt}, \code{L2012.AgYield_bio_ref}. The corresponding file in the
#' original data system was \code{L2012.ag_For_Past_bio_input_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2012.ag_For_Past_bio_input_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/A_agSupplySector",
             FILE = "aglu/A_agSupplySubsector",
             "L103.ag_Prod_Mt_R_C_Y_GLU",
             "L113.ag_bioYield_GJm2_R_GLU",
             "L122.ag_HA_to_CropLand_R_Y_GLU",
             FILE = "temp-data-inject/L123.ag_Prod_Mt_R_Past_Y_GLU",
             FILE = "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU",
             "L132.ag_an_For_Prices",
             FILE = "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU",
             "L163.ag_irrBioYield_GJm2_R_GLU",
             "L163.ag_rfdBioYield_GJm2_R_GLU",
             "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level",
             "L181.YieldMult_R_bio_GLU_irr"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2012.AgSupplySector",
             "L2012.AgSupplySubsector",
             "L2012.AgProduction_ag_irr_mgmt",
             "L2012.AgProduction_For",
             "L2012.AgProduction_Past",
             "L2012.AgHAtoCL_irr_mgmt",
             "L2012.AgYield_bio_ref"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_AgSupplySector <- get_data(all_data, "aglu/A_agSupplySector")
    A_AgSupplySubsector <- get_data(all_data, "aglu/A_agSupplySubsector")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L113.ag_bioYield_GJm2_R_GLU <- get_data(all_data, "L113.ag_bioYield_GJm2_R_GLU")
    L122.ag_HA_to_CropLand_R_Y_GLU <- get_data(all_data, "L122.ag_HA_to_CropLand_R_Y_GLU")
    L123.ag_Prod_Mt_R_Past_Y_GLU <- get_data(all_data, "temp-data-inject/L123.ag_Prod_Mt_R_Past_Y_GLU") %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%  # temporary
      mutate(year = as.integer(substr(year, 2, 5)))
    L123.For_Prod_bm3_R_Y_GLU <- get_data(all_data, "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%  # temporary
      mutate(year = as.integer(substr(year, 2, 5)))
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices" )
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%  # temporary
      mutate(year = as.integer(substr(year, 2, 5)))
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%  # temporary
      mutate(year = as.integer(substr(year, 2, 5)))
    L163.ag_irrBioYield_GJm2_R_GLU <- get_data(all_data, "L163.ag_irrBioYield_GJm2_R_GLU")
    L163.ag_rfdBioYield_GJm2_R_GLU <- get_data(all_data, "L163.ag_rfdBioYield_GJm2_R_GLU")
    L181.ag_Prod_Mt_R_C_Y_GLU_irr_level <- get_data(all_data, "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level")
    L181.YieldMult_R_bio_GLU_irr <- get_data(all_data, "L181.YieldMult_R_bio_GLU_irr")

    # 2. Build tables
    # Make template table of available region x commodity x glu
    # Biograss: available anywhere that has any crop production at all
    L103.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GLU) %>%
      unique %>%
      mutate(GCAM_commodity = "biomass_grass") ->
      L201.R_C_GLU_biograss
    # Biotree: available anywhere that has any forest production at all
    L123.For_Prod_bm3_R_Y_GLU %>%
      select(GCAM_region_ID, GLU) %>%
      unique %>%
      mutate(GCAM_commodity = "biomass_tree") ->
      L201.R_C_GLU_biotree

    # Bind Ag, forest, pasture and biomass all together
    L103.ag_Prod_Mt_R_C_Y_GLU %>%
      bind_rows(L123.For_Prod_bm3_R_Y_GLU, L123.ag_Prod_Mt_R_Past_Y_GLU) %>%
      select(GCAM_region_ID, GCAM_commodity, GLU) %>%
      unique %>%
      bind_rows(L201.R_C_GLU_biograss, L201.R_C_GLU_biotree) %>%
      arrange(GCAM_region_ID, GLU, GCAM_commodity) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L201.R_C_GLU

    # L201.AgSupplySector: Generic AgSupplySector characteristics (units, calprice, market, logit)
    # At the supplysector (market) level, all regions get all supplysectors
    A_AgSupplySector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["AgSupplySector"]], "logit.type"), GCAM_region_names = GCAM_region_names) %>%
      select(-calPrice) %>%
      # Missing value for biomass, use left_join
      left_join(L132.ag_an_For_Prices, by = c("AgSupplySector" = "GCAM_commodity")) %>%
      mutate(calPrice = replace(calPrice, AgSupplySector == "biomass", 1), # value irrelevant
             market = replace(market, market == "regional", region[market == "regional"])) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgSupplySector"]])) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) -> # Remove any regions for which agriculture and land use are not modeled
      L2012.AgSupplySector

    # L201.AgSupplySubsector: Generic AgSupplySubsector characteristics (none specified as competition is in the land allocator)
    # At the subsector (production) level, only region x GLU combinations that actually exist are created
    L201.R_C_GLU %>%
      left_join_error_no_match(A_AgSupplySubsector, by = c("GCAM_commodity" = "AgSupplySubsector")) %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Subsector isn't just the supplysector & GLU for biomass crops, as this is where the grass/tree split is done
      mutate(AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             # We do not actually care about the logit here but we need a value to avoid errors
             logit.year.fillout = min(BASE_YEARS), logit.exponent = -3) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgSupplySubsector"]])) ->
      L2012.AgSupplySubsector

    # L2011.AgProduction_ag_irr: subsector and technology shareweights
    L161.ag_irrProd_Mt_R_C_Y_GLU %>%
      mutate(IRR_RFD = "IRR") %>%
      bind_rows(mutate(L161.ag_rfdProd_Mt_R_C_Y_GLU, IRR_RFD = "RFD")) %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digit = aglu.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Set technology shareweights (subsector requires the year as well)
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) ->
      L2011.AgProduction_ag_irr

    # Set subsector shareweights at the aggregate level
    L2011.AgProduction_ag_irr %>%
      group_by(region, GCAM_commodity, GLU_name, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      mutate(subs.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      ungroup %>%
      select(-calOutputValue) %>%
      right_join(L2011.AgProduction_ag_irr, by = c("region", "GCAM_commodity", "GLU_name", "year")) %>%
      # Copy to high and low management levels
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo"))) %>%
      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, IRR_RFD, MGMT, sep = "_")) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgProduction"]])) ->
      L2011.AgProduction_ag_irr

    # L2012.AgProduction_ag_irr_mgm: agricultural product calibrated output, with irr/rfd and high/low management disaggregated
    # Match in the values from the management-partitioned data
    L181.ag_Prod_Mt_R_C_Y_GLU_irr_level %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digit = aglu.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Add sector, subsector, technology names
      mutate(AgProductionTechnology = paste(GCAM_commodity, GLU_name, toupper(Irr_Rfd), level, sep = "_")) %>%
      right_join(select(L2011.AgProduction_ag_irr, -calOutputValue), by = c("region", "AgProductionTechnology", "year")) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgProduction"]]), level, Irr_Rfd, GLU = GLU_name) ->
      L2012.AgProduction_ag_irr_mgmt

    # L2012.AgProduction_For: Forest product calibration (output)
    # L2012.AgProduction_Past: Pasture product calibration (output)
    L123.For_Prod_bm3_R_Y_GLU %>%
      bind_rows(L123.ag_Prod_Mt_R_Past_Y_GLU) %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digits = aglu.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      mutate(AgProductionTechnology = paste(GCAM_commodity, GLU_name, sep = "_")) ->
      L201.For_Past_Prod_R_Y_GLU

    # Subset only forest and pasture products from main table and paste in calibrated production, rounded
    L2012.AgSupplySubsector %>%
      mutate(AgProductionTechnology = AgSupplySubsector) %>%
      # Copy to all base years
      repeat_add_columns(tibble::tibble(year = BASE_YEARS)) %>%
      semi_join(L201.For_Past_Prod_R_Y_GLU, by = c("AgSupplySector" = "GCAM_commodity")) %>%
      left_join_error_no_match(L201.For_Past_Prod_R_Y_GLU, by = c("region", "AgProductionTechnology", "year")) %>%
      # Subsector and technology shareweights (subsector requires the year as well)
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgProduction"]])) ->
      L2012.AgProduction_For_Past

    # L2012.AgHAtoCL_irr_mgmt: Harvests per year, repeat by irr/rfd and mgmt techs
    # Harvested-area-to-cropland table
    L122.ag_HA_to_CropLand_R_Y_GLU %>%
      select(GCAM_region_ID, GLU) %>%
      unique %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      full_join(L122.ag_HA_to_CropLand_R_Y_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      mutate(year = as.numeric(year), value = as.numeric(value)) %>%
      group_by(GCAM_region_ID, GLU) %>%
      arrange(GCAM_region_ID, GLU, year) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(harvests.per.year = round(value, digits = aglu.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) ->
      L201.ag_HA_to_CropLand_R_Y_GLU

    # Paste in HA:CL to ag crops only
    L2012.AgSupplySubsector %>%
      mutate(AgProductionTechnology = AgSupplySubsector) %>%
      semi_join(L103.ag_Prod_Mt_R_C_Y_GLU, by = c("AgSupplySector" = "GCAM_commodity")) %>%
      # Copy to all model years
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      # Separate the AgProductionTechnology variable to get GLU names for matching in the harvest data
      mutate(AgProductionTechnology = sub("Root_Tuber", "RootTuber", AgProductionTechnology)) %>%
      separate(AgProductionTechnology, c("GCAM_commodity", "GLU_name"), sep = "_") %>%
      left_join(L201.ag_HA_to_CropLand_R_Y_GLU, by = c("region", "GLU_name", "year")) %>%
      # Copy to both irrigated and rainfed technologies
      repeat_add_columns(tibble::tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      # Copy to high and low management levels
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo"))) %>%
      # Revise technology names to add all technologies
      mutate(AgProductionTechnology = paste(AgSupplySubsector, IRR_RFD, MGMT, sep = "_")) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgHAtoCL"]])) ->
      L2012.AgHAtoCL_irr_mgmt

    # L2011.AgYield_bio_grass_irr: yields of bioenergy grass crops, with irrigated and rainfed split
    L163.ag_irrBioYield_GJm2_R_GLU %>%
      mutate(IRR_RFD = "IRR") %>%
      bind_rows(mutate(L163.ag_rfdBioYield_GJm2_R_GLU, IRR_RFD = "RFD")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      mutate(AgSupplySubsector = paste("biomass_grass", GLU_name, sep = "_")) ->
      L2011.AgYield_bio_grass_irr

    L2012.AgSupplySubsector %>%
      filter(grepl("biomass_grass", AgSupplySubsector)) %>%
      # Copy to all base years
      repeat_add_columns(tibble::tibble(year = BASE_YEARS)) %>%
      # Copy to both irrigated and rainfed technologies
      repeat_add_columns(tibble::tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      # mutate(AgProductionTechnology = paste(AgSupplySubsector, IRR_RFD, sep = "_")) %>%
      left_join(L2011.AgYield_bio_grass_irr,
                by = c("region", "AgSupplySubsector", "IRR_RFD")) %>%
      mutate(yield = Yield_GJm2,
             AgProductionTechnology = paste(AgSupplySubsector, IRR_RFD, sep = "_")) %>%
      # Places with no irrigated crop production will return missing values here.
      # May as well set these yields to 0 to ensure that they get no irrigated bioenergy production
      # in the future periods (most are tropical areas with no need for irrigation).
      replace_na(list(yield = 0)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgYield"]])) ->
      L2011.AgYield_bio_grass_irr

    # L201.AgYield_bio_tree: Base year biomass yields, tree bioenergy crops
    # Just use the grass yields where available.
    L113.ag_bioYield_GJm2_R_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      mutate(yield = round(Yield_GJm2, digit = aglu.DIGITS_CALOUTPUT)) %>%
      select(-GCAM_region_ID, -GLU, -Yield_GJm2) ->
      L201.AgYield_bio_grass

    # Where not available (i.e., where there is forest but not cropped land),
    # use a default minimum as these are likely remote / unpopulated lands
    L2012.AgSupplySubsector %>%
      filter(grepl("biomass_tree", AgSupplySubsector)) %>%
      mutate(AgProductionTechnology = AgSupplySubsector) %>%
      # Copy to all base years
      repeat_add_columns(tibble::tibble(year = BASE_YEARS)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgTechYr"]])) %>%
      mutate(GLU_name = AgSupplySubsector,
             GLU_name = sub("biomass_tree_", "", GLU_name)) %>%
      left_join(L201.AgYield_bio_grass, by = c("region", "GLU_name")) %>%
      replace_na(list(yield = min(L201.AgYield_bio_grass$yield))) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgYield"]])) ->
      L201.AgYield_bio_tree

    # L2011.AgYield_bio_tree_irr: yield of bioenergy tree crops (use the irr:rfd yield ratios from grass crops)
    # This method essentially copies prior versions of GCAM with irrigation, albeit in a different sequence.
    # Before, we used the generic bioenergy crop yields by irr/rfd, multiplied by an assumed tree:grass yield conversion factor.
    # Here, we start with the generic tree bioenergy crop yields in each land use region,
    # and multiply by generic:irr and generic:rfd conversion factors.

    # Compile the generic:irr and generic:rfd conversion factors
    L201.AgYield_bio_grass %>%
      mutate(AgSupplySubsector = paste("biomass_grass", GLU_name, sep = "_")) %>%
      rename(generic.yield = yield) %>%
      right_join(L2011.AgYield_bio_grass_irr, by = c("region", "AgSupplySubsector")) %>%
      mutate(factor = yield / generic.yield,
             AgSupplySubsector = sub("biomass_grass", "biomass_tree", AgSupplySubsector),
             AgProductionTechnology = sub("biomass_grass", "biomass_tree", AgProductionTechnology)) %>%
      select(-GLU_name, -yield, -generic.yield) ->
      L2011.irr_rfd_factors

    # Multiply generic tree crop yields by the conversion factors
    # Not all regions that have tree crops also have grass crops (e.g., regions w forest but no ag production)
    # Simply use the defaults for these, which are likely minor ag regions anyway
    # Copy to both irrigated and rainfed technologies
    L201.AgYield_bio_tree %>%
      repeat_add_columns(tibble::tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      mutate(AgProductionTechnology = paste(AgProductionTechnology, IRR_RFD, sep = "_")) %>%
      left_join(L2011.irr_rfd_factors, by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%
      mutate(yield_irr = yield * factor,
             yield = replace(yield, !is.na(yield_irr), yield_irr[!is.na(yield_irr)]),
             yield = round(yield, digit = aglu.DIGITS_CALOUTPUT)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgYield"]])) ->
      L2011.AgYield_bio_tree_irr

    # Bioenergy yields: Repeat by mgmt techs and replace values
    # First, prepare a yield multiplier table, with the level as a column ID
    L181.YieldMult_R_bio_GLU_irr %>%
      gather(level, yieldmult, -GCAM_region_ID, -GLU, -Irr_Rfd) %>%
      mutate(level = sub("yieldmult_", "", level),
             Irr_Rfd = toupper(Irr_Rfd)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) ->
      L2012.YieldMult_R_bio_GLU_irr

    # Next, rbind the tree and grass tables, repeat by mgmt level, and match in the yield multipliers
    # by region, GLU, irr, and mgmt level
    L2011.AgYield_bio_grass_irr %>%
      bind_rows(L2011.AgYield_bio_tree_irr) %>%
      # Copy to high and low management levels
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo"))) %>%
      separate(AgProductionTechnology, c("biomass", "type", "GLU_name", "IRR_RFD")) %>%
      left_join(L2012.YieldMult_R_bio_GLU_irr, by = c("region", "GLU_name", "IRR_RFD" = "Irr_Rfd", "MGMT" = "level")) %>%
      # For minor region/GLUs that are missing from the ag data, set the multipliers to 1 (effectively fixing yields in all periods)
      replace_na(list(yieldmult = 1)) %>%
      mutate(yield = yield * yieldmult,
             AgProductionTechnology = paste(AgSupplySubsector, IRR_RFD, MGMT, sep = "_")) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgYield"]])) ->
      L2012.AgYield_bio_ref

    # Produce outputs
    L2012.AgSupplySector %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgSupplySector") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "L132.ag_an_For_Prices") ->
      L2012.AgSupplySector

   L2012.AgSupplySubsector %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgSupplySubsector") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySubsector",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "temp-data-inject/L123.ag_Prod_Mt_R_Past_Y_GLU",
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") ->
      L2012.AgSupplySubsector

    L2012.AgProduction_ag_irr_mgmt %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgProduction_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU",
                     "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level") ->
      L2012.AgProduction_ag_irr_mgmt

    L2012.AgProduction_For_Past %>%
      filter(AgSupplySector == "Forest") %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgProduction_For") %>%
      same_precursors_as("L2012.AgSupplySubsector") ->
      L2012.AgProduction_For

    L2012.AgProduction_For_Past %>%
      filter(AgSupplySector == "Pasture") %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgProduction_Past") %>%
      same_precursors_as("L2012.AgSupplySubsector") ->
      L2012.AgProduction_Past

    L2012.AgHAtoCL_irr_mgmt %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgHAtoCL_irr_mgmt") %>%
      same_precursors_as("L2012.AgProduction_ag_irr_mgmt") %>%
      add_precursors("L122.ag_HA_to_CropLand_R_Y_GLU") ->
      L2012.AgHAtoCL_irr_mgmt

    L2012.AgYield_bio_ref %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgYield_bio_ref") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "aglu/A_agSupplySubsector",
                     "L113.ag_bioYield_GJm2_R_GLU",
                     "L163.ag_irrBioYield_GJm2_R_GLU",
                     "L163.ag_rfdBioYield_GJm2_R_GLU",
                     "L181.YieldMult_R_bio_GLU_irr") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2012.AgYield_bio_ref

    return_data(L2012.AgSupplySector, L2012.AgSupplySubsector, L2012.AgProduction_ag_irr_mgmt, L2012.AgProduction_For, L2012.AgProduction_Past, L2012.AgHAtoCL_irr_mgmt, L2012.AgYield_bio_ref)
  } else {
    stop("Unknown command")
  }
}
