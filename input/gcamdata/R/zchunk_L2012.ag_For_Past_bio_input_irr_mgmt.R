# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2012.ag_For_Past_bio_input_irr_mgmt
#'
#' Build agriculture, forest, pasture and biomass production inputs for all technologies.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2012.AgSupplySector}, \code{L2012.AgSupplySubsector}, \code{L2012.AgProduction_ag_irr_mgmt}, \code{L2012.AgProduction_For}, \code{L2012.AgProduction_Past}, \code{L2012.AgHAtoCL_irr_mgmt}, \code{L2012.AgYield_bio_ref}. The corresponding file in the
#' original data system was \code{L2012.ag_For_Past_bio_input_irr_mgmt.R} (aglu level2).
#' @details This chunk specifies the input tables for agriculture, forest, pasture and biomass supply sectors and subsectors,
#' agricultural commodity production and harvest area to cropland by technologies, forest and pasture production,
#' and biomass grass and tree crops yield by technologies.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter full_join group_by if_else left_join mutate right_join select semi_join summarise
#' @importFrom tidyr gather replace_na separate
#' @author RC August 2017
module_aglu_L2012.ag_For_Past_bio_input_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/A_agSupplySector",
             FILE = "aglu/A_agSupplySubsector",
             "L101.ag_Prod_Mt_R_C_Y_GLU",
             "L113.ag_bioYield_GJm2_R_GLU",
             "L122.ag_HA_to_CropLand_R_Y_GLU",
             "L123.ag_Prod_Mt_R_Past_Y_GLU",
             "L123.For_Prod_bm3_R_Y_GLU",
             "L132.ag_an_For_Prices",
             "L1321.ag_prP_R_C_75USDkg",
             "L1321.expP_R_F_75USDm3",
             "L161.ag_irrProd_Mt_R_C_Y_GLU",
             "L161.ag_rfdProd_Mt_R_C_Y_GLU",
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
             "L2012.AgYield_bio_ref",
             "L201.AgYield_bio_grass",
             "L201.AgYield_bio_tree"))
  } else if(command == driver.MAKE) {

    GCAM_commodity <- GCAM_region_ID <- region <- value <- year <- GLU <- GLU_name <- GLU_code <-
      AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <- share.weight.year <- subs.share.weight <-
      tech.share.weight <- logit.year.fillout <- logit.exponent <- calPrice <- calOutputValue <-
      market <- IRR_RFD <- Irr_Rfd <- MGMT <- level <- yield <- generic.yield <- yield_irr <-
      yieldmult <- Yield_GJm2 <- reg_calPrice <- NULL   # silence package check notes

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_AgSupplySector <- get_data(all_data, "aglu/A_agSupplySector", strip_attributes = TRUE)
    A_AgSupplySubsector <- get_data(all_data, "aglu/A_agSupplySubsector", strip_attributes = TRUE)
    L101.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L101.ag_Prod_Mt_R_C_Y_GLU", strip_attributes = TRUE)
    L113.ag_bioYield_GJm2_R_GLU <- get_data(all_data, "L113.ag_bioYield_GJm2_R_GLU", strip_attributes = TRUE)
    L122.ag_HA_to_CropLand_R_Y_GLU <- get_data(all_data, "L122.ag_HA_to_CropLand_R_Y_GLU", strip_attributes = TRUE)
    L123.ag_Prod_Mt_R_Past_Y_GLU <- get_data(all_data, "L123.ag_Prod_Mt_R_Past_Y_GLU", strip_attributes = TRUE)
    L123.For_Prod_bm3_R_Y_GLU <- get_data(all_data, "L123.For_Prod_bm3_R_Y_GLU", strip_attributes = TRUE)
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices", strip_attributes = TRUE)
    L1321.ag_prP_R_C_75USDkg <- get_data(all_data, "L1321.ag_prP_R_C_75USDkg", strip_attributes = TRUE)
    L1321.expP_R_F_75USDm3 <- get_data(all_data, "L1321.expP_R_F_75USDm3", strip_attributes = TRUE)
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrProd_Mt_R_C_Y_GLU", strip_attributes = TRUE)
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_rfdProd_Mt_R_C_Y_GLU", strip_attributes = TRUE)
    L163.ag_irrBioYield_GJm2_R_GLU <- get_data(all_data, "L163.ag_irrBioYield_GJm2_R_GLU", strip_attributes = TRUE)
    L163.ag_rfdBioYield_GJm2_R_GLU <- get_data(all_data, "L163.ag_rfdBioYield_GJm2_R_GLU", strip_attributes = TRUE)
    L181.ag_Prod_Mt_R_C_Y_GLU_irr_level <- get_data(all_data, "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level", strip_attributes = TRUE)
    L181.YieldMult_R_bio_GLU_irr <- get_data(all_data, "L181.YieldMult_R_bio_GLU_irr", strip_attributes = TRUE)

    # L2012.AgSupplySector: Generic AgSupplySector characteristics (units, calprice, market, logit)
    # Set up the regional price data to be joined in to the ag supplysector table
    L2012.P_R_C <- left_join_error_no_match(L1321.ag_prP_R_C_75USDkg%>%
                                                bind_rows(L1321.expP_R_F_75USDm3),
                                              GCAM_region_names,
                                              by = "GCAM_region_ID") %>%
      mutate(reg_calPrice = round(value, aglu.DIGITS_CALPRICE)) %>%
      select(region, GCAM_commodity, reg_calPrice)

    A_AgSupplySector %>%
      # At the supplysector (market) level, all regions get all supplysectors
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["AgSupplySector"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names = GCAM_region_names) %>%
      select(-calPrice) %>%
      # Join calibration price data, there are missing value for biomass, use left_join instead
      left_join(L132.ag_an_For_Prices, by = c("AgSupplySector" = "GCAM_commodity")) %>%
      left_join(L2012.P_R_C, by = c("region", AgSupplySector = "GCAM_commodity")) %>%
      mutate(calPrice = replace(calPrice, AgSupplySector == "biomass", 1), # value irrelevant
             calPrice = if_else(is.na(reg_calPrice), calPrice, reg_calPrice),
             # For regional commodities, specify market names with region names
             market = replace(market, market == "regional", region[market == "regional"])) %>%
      select(LEVEL2_DATA_NAMES[["AgSupplySector"]], LOGIT_TYPE_COLNAME) %>%
      # Remove any regions for which agriculture and land use are not modeled
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->
      L2012.AgSupplySector

    # L2012.AgSupplySubsector: Generic AgSupplySubsector characteristics (none specified as competition is in the land allocator)
    # At the subsector (production) level, only region x GLU combinations that actually exist are created.
    # So start with template production tables of available region x commodity x glu for all commodities.
    # First, biograss: available anywhere that has any crop production at all
    L101.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GLU) %>%
      unique %>%
      mutate(GCAM_commodity = "biomassGrass") ->
      L201.R_C_GLU_biograss
    # Second, biotree: available anywhere that has any forest production at all
    L123.For_Prod_bm3_R_Y_GLU %>%
      select(GCAM_region_ID, GLU) %>%
      unique %>%
      mutate(GCAM_commodity = "biomassTree") ->
      L201.R_C_GLU_biotree
    # Third, bind Ag commodties, forest, pasture and biomass all together
    L101.ag_Prod_Mt_R_C_Y_GLU %>%
      bind_rows(L123.For_Prod_bm3_R_Y_GLU, L123.ag_Prod_Mt_R_Past_Y_GLU) %>%
      select(GCAM_region_ID, GCAM_commodity, GLU) %>%
      unique %>%
      bind_rows(L201.R_C_GLU_biograss, L201.R_C_GLU_biotree) %>%
      arrange(GCAM_region_ID, GLU, GCAM_commodity) %>%
      left_join_error_no_match(A_AgSupplySubsector, by = c("GCAM_commodity" = "AgSupplySubsector")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GLU_name), by = c("GLU" = "GLU_code")) %>%
      # Subsector isn't just the supplysector & GLU for biomass crops, as this is where the grass/tree split is done
      mutate(AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             # We do not actually care about the logit here but we need a value to avoid errors
             logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = -3,
             logit.type = NA) %>%
      select(LEVEL2_DATA_NAMES[["AgSupplySubsector"]], LOGIT_TYPE_COLNAME) ->
      L2012.AgSupplySubsector

    # L2012.AgProduction_ag_irr_mgm: Agricultural commodities production by all technoligies
    # Start with L2011.AgProduction_ag_irr to get subsector and technology shareweights
    L161.ag_irrProd_Mt_R_C_Y_GLU %>%
      mutate(IRR_RFD = "IRR") %>%
      bind_rows(mutate(L161.ag_rfdProd_Mt_R_C_Y_GLU, IRR_RFD = "RFD")) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digits = aglu.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GLU_name), by = c("GLU" = "GLU_code")) %>%
      # Set subsector year and technology shareweights, subsector shareweights are set at the aggregate level later
      mutate(share.weight.year = year,
             tech.share.weight = 0,
             tech.share.weight = replace(tech.share.weight, calOutputValue > 0, 1)) ->
      L2011.AgProduction_ag_irr

    # Set subsector shareweights at the aggregate level across irrigated and rainfed production
    L2011.AgProduction_ag_irr %>%
      group_by(region, GCAM_commodity, GLU_name, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup %>%
      mutate(subs.share.weight = 0,
             subs.share.weight = replace(subs.share.weight, calOutputValue > 0, 1)) %>%
      select(-calOutputValue) %>%
      # Combine with subsector year and technology shareweights
      right_join(L2011.AgProduction_ag_irr, by = c("region", "GCAM_commodity", "GLU_name", "year")) %>%
      # Copy to high and low management levels
      repeat_add_columns(tibble(MGMT = c("hi", "lo"))) %>%
      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, IRR_RFD, MGMT, sep = "_")) %>%
      select(LEVEL2_DATA_NAMES[["AgProduction"]]) ->
      L2011.AgProduction_ag_irr

    # For agricultural product calibrated output, use the specific management-partitioned data
    L181.ag_Prod_Mt_R_C_Y_GLU_irr_level %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digits = aglu.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GLU_name), by = c("GLU" = "GLU_code")) %>%
      # Add sector, subsector, technology names
      mutate(AgProductionTechnology = paste(GCAM_commodity, GLU_name, toupper(Irr_Rfd), level, sep = "_")) %>%
      # Combine with subsector and technology shareweights
      right_join(select(L2011.AgProduction_ag_irr, -calOutputValue),
                 by = c("region", "AgProductionTechnology", "year")) %>%
      # recheck technology share weight after splitting technology levels and rounding
      mutate(tech.share.weight = 0,
             tech.share.weight = replace(tech.share.weight, calOutputValue > 0, 1)) %>%
      select(LEVEL2_DATA_NAMES[["AgProduction"]]) ->
      L2012.AgProduction_ag_irr_mgmt

    # L2012.AgProduction_For and L2012.AgProduction_Past: Forest and pasture product calibration (output)
    L123.For_Prod_bm3_R_Y_GLU %>%
      # Combine forest and pasture production by region x GLU
      bind_rows(L123.ag_Prod_Mt_R_Past_Y_GLU) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digits = aglu.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GLU_name), by = c("GLU" = "GLU_code")) %>%
      mutate(AgProductionTechnology = paste(GCAM_commodity, GLU_name, sep = "_")) ->
      L201.For_Past_Prod_R_Y_GLU

    # Subset only forest and pasture products from the main subsector table and paste in calibrated production, rounded
    L2012.AgSupplySubsector %>%
      # Use semi_join to filter region x GLU that have forest and pasture production
      semi_join(L201.For_Past_Prod_R_Y_GLU, by = c("AgSupplySector" = "GCAM_commodity")) %>%
      # No disaggregation of technologies
      mutate(AgProductionTechnology = AgSupplySubsector) %>%
      # Copy to all base years
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L201.For_Past_Prod_R_Y_GLU, by = c("region", "AgProductionTechnology", "year")) %>%
      # Subsector and technology shareweights (subsector requires the year as well)
      mutate(share.weight.year = year,
             subs.share.weight = 0,
             subs.share.weight = replace(subs.share.weight, calOutputValue > 0, 1),
             tech.share.weight = 0,
             tech.share.weight = replace(tech.share.weight, calOutputValue > 0, 1)) %>%
      select(LEVEL2_DATA_NAMES[["AgProduction"]]) ->
      L2012.AgProduction_For_Past

    # L2012.AgHAtoCL_irr_mgmt: Harvests area to cropland ratio per year, by technologies
    # Start with the harvested-area-to-cropland table, extrapolate base year values to all model years
    L122.ag_HA_to_CropLand_R_Y_GLU %>%
      # Build a template table with all existing region x GLU combinations and by all model years
      select(GCAM_region_ID, GLU) %>%
      unique %>%
      # Add all model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Full_join the original data to keep all model years for extrapolation
      full_join(L122.ag_HA_to_CropLand_R_Y_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      mutate(year = as.numeric(year), value = as.numeric(value)) %>%
      group_by(GCAM_region_ID, GLU) %>%
      # Copy the last base year value to all model years
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup %>%
      # Drop other historical years that are not in model base years
      filter(year %in% MODEL_YEARS) %>%
      mutate(harvests.per.year = round(value, digits = aglu.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GLU_name), by = c("GLU" = "GLU_code")) ->
      L201.ag_HA_to_CropLand_R_Y_GLU

    # Paste in harvested-area-to-cropland only for agriculture commodities, repeat by irr/rfd and mgmt techs
    L2012.AgSupplySubsector %>%
      semi_join(L101.ag_Prod_Mt_R_C_Y_GLU, by = c("AgSupplySector" = "GCAM_commodity")) %>%
      # Copy to all model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Separate the AgSupplySubsector variable to get GLU names for matching in the harvest data
      separate(AgSupplySubsector, c("GCAM_commodity", "GLU_name"), sep = "_") %>%
      left_join(L201.ag_HA_to_CropLand_R_Y_GLU, by = c("region", "GLU_name", "year")) %>%
      # Copy to both irrigated and rainfed technologies
      repeat_add_columns(tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      # Copy to high and low management levels
      repeat_add_columns(tibble(MGMT = c("hi", "lo"))) %>%
      # Add subsector and technology names
      mutate(AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, IRR_RFD, MGMT, sep = "_")) %>%
      select(LEVEL2_DATA_NAMES[["AgHAtoCL"]]) ->
      L2012.AgHAtoCL_irr_mgmt

    # L2012.AgYield_bio_ref: bioenergy yields.
    # For bio grass crops, start with data of irrigated and rainfed split;
    # For bio tree crops, use the no-tech-split data of bio grass crops,
    #  whenever it is not available, use a minimum default yield;
    #  then split to irrigated and rainfed using irr:rfd yield ratios from grass crops;
    # And last for both crops, apply different yield multipliers for high and low managements

    # L2011.AgYield_bio_grass_irr: yields of bioenergy grass crops, with irrigated and rainfed split
    L163.ag_irrBioYield_GJm2_R_GLU %>%
      mutate(IRR_RFD = "IRR") %>%
      # Combine irrigated and rainfet yield data
      bind_rows(mutate(L163.ag_rfdBioYield_GJm2_R_GLU, IRR_RFD = "RFD")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GLU_name), by = c("GLU" = "GLU_code")) %>%
      mutate(AgSupplySubsector = paste("biomassGrass", GLU_name, sep = "_")) ->
      L2011.AgYield_bio_grass_irr

    # From the subsector generic table, filter bio grass crops
    L2012.AgSupplySubsector %>%
      filter(grepl("biomassGrass", AgSupplySubsector)) %>%
      # Copy to all base years
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      # Copy to both irrigated and rainfed technologies
      repeat_add_columns(tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      # Match in yield data, use left_join instead because of NAs
      left_join(L2011.AgYield_bio_grass_irr, by = c("region", "AgSupplySubsector", "IRR_RFD")) %>%
      # Round yield data
      mutate(yield = round(Yield_GJm2, digits = aglu.DIGITS_CALOUTPUT)) ->
      L2011.AgYield_bio_grass_irr

    L2011.AgYield_bio_grass_irr %>%
      # Places with no irrigated crop production will return missing values here.
      # May as well set these yields to 0 to ensure that they get no irrigated bioenergy production
      # in the future periods (most are tropical areas with no need for irrigation).
      replace_na(list(yield = 0)) %>%
      mutate(AgProductionTechnology = paste(AgSupplySubsector, IRR_RFD, sep = "_")) %>%
      select(LEVEL2_DATA_NAMES[["AgYield"]]) ->
      L2011.AgYield_bio_grass_irr

    # L201.AgYield_bio_tree: base year biomass yields, tree bioenergy crops
    # Start with the grass crop yields with no tech split where available
    L113.ag_bioYield_GJm2_R_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GLU_name), by = c("GLU" = "GLU_code")) %>%
      mutate(yield = round(Yield_GJm2, digits = aglu.DIGITS_CALOUTPUT)) %>%
      select(-GCAM_region_ID, -GLU, -Yield_GJm2) ->
      L201.AgYield_bio_grass

    # From the subsector table, filter bio tree crops
    L2012.AgSupplySubsector %>%
      filter(grepl("biomassTree", AgSupplySubsector)) %>%
      # No tech split yet
      mutate(AgProductionTechnology = AgSupplySubsector) %>%
      # Copy to all base years
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["AgTechYr"]]) %>%
      mutate(GLU_name = AgSupplySubsector,
             GLU_name = sub("biomassTree_", "", GLU_name)) %>%
      # Match in grass crop yields where available, use left_join instead because of NAs
      left_join(L201.AgYield_bio_grass, by = c("region", "GLU_name")) %>%
      # Where not available (i.e., where there is forest but not cropped land),
      # use a default minimum as these are likely remote / unpopulated lands
      replace_na(list(yield = min(L201.AgYield_bio_grass$yield))) %>%
      select(LEVEL2_DATA_NAMES[["AgYield"]]) ->
      L201.AgYield_bio_tree

    # L2011.AgYield_bio_tree_irr: yield of bioenergy tree crops (use the irr:rfd yield ratios from grass crops)
    # This method essentially copies prior versions of GCAM with irrigation, albeit in a different sequence.
    # Before, we used the generic bioenergy crop yields by irr/rfd, multiplied by an assumed tree:grass yield conversion factor.
    # Here, we start with the generic tree bioenergy crop yields in each land use region,
    # and multiply by generic:irr and generic:rfd conversion factors.

    # Compile the generic:irr and generic:rfd conversion factors
    L201.AgYield_bio_grass %>%
      mutate(AgSupplySubsector = paste("biomassGrass", GLU_name, sep = "_")) %>%
      # Generic bio grass crop yield w/o tech split
      rename(generic.yield = yield) %>%
      # Match in irrigated and rainfed bio grass yields
      right_join(L2011.AgYield_bio_grass_irr, by = c("region", "AgSupplySubsector")) %>%
      # Compute conversion factor as irr/generic, and rfd/generic
      mutate(factor = yield / generic.yield,
             # Prepare for bio tree crops
             AgSupplySubsector = sub("biomassGrass", "biomassTree", AgSupplySubsector),
             AgProductionTechnology = sub("biomassGrass", "biomassTree", AgProductionTechnology)) %>%
      select(-GLU_name, -yield, -generic.yield) ->
      L2011.irr_rfd_factors

    # Multiply generic tree crop yields by the conversion factors
    # For regions that do not have grass crops but only tree crops (e.g., regions w forest but no ag production),
    # simply use the generic defaults, which are likely minor ag regions anyway
    L201.AgYield_bio_tree %>%
      # Copy to both irrigated and rainfed technologies
      repeat_add_columns(tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      mutate(AgProductionTechnology = paste(AgProductionTechnology, IRR_RFD, sep = "_")) %>%
      # Match in conversion factors
      left_join(L2011.irr_rfd_factors,
                by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%
      # Calculate the irrigated and rainfed yields using the conversion factors from bio grass crops
      mutate(yield_irr = yield * factor,
             # When grass crops are not available, use the generic yields
             yield = replace(yield, !is.na(yield_irr), yield_irr[!is.na(yield_irr)]),
             yield = round(yield, digits = aglu.DIGITS_CALOUTPUT)) %>%
      select(LEVEL2_DATA_NAMES[["AgYield"]]) ->
      L2011.AgYield_bio_tree_irr

    # Last step, apply different yield multipliers for high and low mgmt techs
    # Prepare a yield multiplier table, with the mgmt level as a column ID
    L181.YieldMult_R_bio_GLU_irr %>%
      gather(level, yieldmult, -GCAM_region_ID, -GLU, -Irr_Rfd) %>%
      mutate(level = sub("yieldmult_", "", level),
             Irr_Rfd = toupper(Irr_Rfd)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(basin_to_country_mapping, GLU_code, GLU_name), by = c("GLU" = "GLU_code")) ->
      L2012.YieldMult_R_bio_GLU_irr

    # Bind the tree and grass tables, repeat by mgmt level, and match in the yield multipliers
    L2011.AgYield_bio_grass_irr %>%
      bind_rows(L2011.AgYield_bio_tree_irr) %>%
      # Copy to high and low management levels
      repeat_add_columns(tibble(MGMT = c("hi", "lo"))) %>%
      # Separate technology to match in the multipliers
      separate(AgProductionTechnology, c("biomass", "GLU_name", "IRR_RFD")) %>%
      # Match in multipliers, use left_join instead because of NAs
      left_join(L2012.YieldMult_R_bio_GLU_irr, by = c("region", "GLU_name", "IRR_RFD" = "Irr_Rfd", "MGMT" = "level")) %>%
      # For minor region/GLUs that are missing from the ag data, set the multipliers to 1 (effectively fixing yields in all periods)
      replace_na(list(yieldmult = 1)) %>%
      mutate(yield = yield * yieldmult,
             # Add technology back
             AgProductionTechnology = paste(AgSupplySubsector, IRR_RFD, MGMT, sep = "_")) %>%
      select(LEVEL2_DATA_NAMES[["AgYield"]]) ->
      L2012.AgYield_bio_ref

    # Add sector, subsector, and technology information to L201.AgYield_bio_grass
    L201.AgYield_bio_grass %>%
      mutate(AgSupplySector = "biomass",
             AgSupplySubsector = paste0("biomassGrass_", GLU_name),
             AgProductionTechnology = AgSupplySubsector) %>%
      repeat_add_columns((tibble(year = MODEL_BASE_YEARS))) %>%
      select(-GLU_name) ->
      L201.AgYield_bio_grass

    # Produce outputs
    L2012.AgSupplySector %>%
      add_title("Generic information for agriculture supply sectors") %>%
      add_units("Unitless") %>%
      add_comments("Specify generic supply sector characteristics (units, calprice, market, logit)") %>%
      add_comments("At the supplysector (market) level, all regions get all supplysectors") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L2012.AgSupplySector") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "L132.ag_an_For_Prices",
                     "L1321.ag_prP_R_C_75USDkg",
                     "L1321.expP_R_F_75USDm3") ->
      L2012.AgSupplySector

   L2012.AgSupplySubsector %>%
      add_title("Generic information for agriculture supply subsectors") %>%
      add_units("Unitless") %>%
      add_comments("Specify generic supply subsector characteristics") %>%
      add_comments("At the subsector (production) level, only region x GLU combinations that actually exist are created") %>%
      add_legacy_name("L2012.AgSupplySubsector") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySubsector",
                     "L101.ag_Prod_Mt_R_C_Y_GLU",
                     "L123.ag_Prod_Mt_R_Past_Y_GLU",
                     "L123.For_Prod_bm3_R_Y_GLU") ->
      L2012.AgSupplySubsector

    L2012.AgProduction_ag_irr_mgmt %>%
      add_title("Input table for agriculture commodity production by all technologies") %>%
      add_units("Mt") %>%
      add_comments("Calibrated outputs are specified by each technology") %>%
      add_comments("Subsector shareweights are set at the aggregated region x GLU level, same for all tech") %>%
      add_comments("Technology shareweights are set by irrigated vs. rainfed, same for high and low mgmt") %>%
      add_legacy_name("L2012.AgProduction_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "L161.ag_rfdProd_Mt_R_C_Y_GLU",
                     "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level") ->
      L2012.AgProduction_ag_irr_mgmt

    L2012.AgProduction_For_Past %>%
      filter(AgSupplySector == "Forest") %>%
      add_title("Input table for forest production") %>%
      add_units("bm3") %>%
      add_comments("Calibrated ouputs or shareweights are not specify by technology") %>%
      add_legacy_name("L2012.AgProduction_For") %>%
      same_precursors_as("L2012.AgSupplySubsector") ->
      L2012.AgProduction_For

    L2012.AgProduction_For_Past %>%
      filter(AgSupplySector == "Pasture") %>%
      add_title("Input table for pasture production") %>%
      add_units("Mt") %>%
      add_comments("Calibrated ouputs or shareweights are not specify by technology") %>%
      add_legacy_name("L2012.AgProduction_Past") %>%
      same_precursors_as("L2012.AgSupplySubsector") ->
      L2012.AgProduction_Past

    L2012.AgHAtoCL_irr_mgmt %>%
      add_title("Harvest area to cropland value of agricultural commodities by year and technology") %>%
      add_units("Unitless") %>%
      add_comments("Copy the same value to all technologies") %>%
      add_comments("Exclude forest and pasture") %>%
      add_legacy_name("L2012.AgHAtoCL_irr_mgmt") %>%
      same_precursors_as("L2012.AgProduction_ag_irr_mgmt") %>%
      add_precursors("L122.ag_HA_to_CropLand_R_Y_GLU") ->
      L2012.AgHAtoCL_irr_mgmt

    L2012.AgYield_bio_ref %>%
      add_title("Bioenergy grass and tree crops yields by all technologies") %>%
      add_units("GJ/m2") %>%
      add_comments("Grass crops yields are used for tree crops where available") %>%
      add_comments("Apply the irr:generic and rfd:generic conversion factors from grass crops") %>%
      add_comments("Use default minimum value where grass crops are not available") %>%
      add_comments("Apply different multipliers to high and low management for both grass and tree crops") %>%
      add_legacy_name("L2012.AgYield_bio_ref") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "aglu/A_agSupplySubsector",
                     "L113.ag_bioYield_GJm2_R_GLU",
                     "L163.ag_irrBioYield_GJm2_R_GLU",
                     "L163.ag_rfdBioYield_GJm2_R_GLU",
                     "L181.YieldMult_R_bio_GLU_irr") ->
      L2012.AgYield_bio_ref

    L201.AgYield_bio_grass %>%
      add_title("Bioenergy grass crops yields for aggregate tech") %>%
      add_units("GJ/m2") %>%
      add_comments("Append region and basin names to L113.ag_bioYield_GJm2_R_GLU") %>%
      add_legacy_name("L201.AgYield_bio_grass") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L113.ag_bioYield_GJm2_R_GLU") ->
      L201.AgYield_bio_grass

    L201.AgYield_bio_tree %>%
      add_title("Bioenergy tree crops yields for aggregate tech") %>%
      add_units("GJ/m2") %>%
      add_comments("Set bioenergy tree crop yield to the same as the bioenergy grass yield") %>%
      add_comments("If data is missing for a region/basin, use the minimum yield") %>%
      add_legacy_name("L201.AgYield_bio_tree") %>%
      same_precursors_as("L201.AgYield_bio_grass") %>%
      same_precursors_as("L2012.AgSupplySubsector") ->
      L201.AgYield_bio_tree

    return_data(L2012.AgSupplySector, L2012.AgSupplySubsector, L2012.AgProduction_ag_irr_mgmt, L2012.AgProduction_For, L2012.AgProduction_Past, L2012.AgHAtoCL_irr_mgmt, L2012.AgYield_bio_ref, L201.AgYield_bio_grass, L201.AgYield_bio_tree)
  } else {
    stop("Unknown command")
  }
}
