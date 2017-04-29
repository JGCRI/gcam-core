#' module_aglu_LB165.ag_water_R_C_Y_GLU_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L165.BlueIrr_m3kg_R_C_GLU}, \code{L165.TotIrr_m3kg_R_C_GLU}, \code{L165.GreenRfd_m3kg_R_C_GLU}, \code{L165.ag_IrrEff_R}. The corresponding file in the
#' original data system was \code{LB165.ag_water_R_C_Y_GLU_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL April 2017
#' @export
module_aglu_LB165.ag_water_R_C_Y_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO_ag_items_PRODSTAT",
             "L100.Water_footprint_m3",
             "L100.LDS_ag_prod_t",
             FILE = "aglu/Mekonnen_Hoekstra_Rep47_A2",
             FILE = "aglu/Rohwer_2007_IrrigationEff",
             "L151.ag_irrProd_t_ctry_crop",
             "L151.ag_rfdProd_t_ctry_crop",
             "L151.ag_irrHA_ha_ctry_crop"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L165.BlueIrr_m3kg_R_C_GLU",
             "L165.TotIrr_m3kg_R_C_GLU",
             "L165.GreenRfd_m3kg_R_C_GLU",
             "L165.ag_IrrEff_R"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    L100.Water_footprint_m3 <- get_data(all_data, "L100.Water_footprint_m3")
    Mekonnen_Hoekstra_Rep47_A2 <- get_data(all_data, "aglu/Mekonnen_Hoekstra_Rep47_A2")
    Rohwer_2007_IrrigationEff <- get_data(all_data, "aglu/Rohwer_2007_IrrigationEff")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L151.ag_irrProd_t_ctry_crop <- get_data(all_data, "L151.ag_irrProd_t_ctry_crop")
    L151.ag_rfdProd_t_ctry_crop <- get_data(all_data, "L151.ag_rfdProd_t_ctry_crop")
    L151.ag_irrHA_ha_ctry_crop <- get_data(all_data, "L151.ag_irrHA_ha_ctry_crop")

    if(0) {

      # Perform computations

      # The method here is simple in principle (only)--we are taking inventory
      # estimates of either water demand coefficients by country and crop, or aggregated
      # gridded volumes of water use by country, GLU, and crop, and using these estimates to
      # calculate average water consumption coefficients by GCAM region, crop, GLU, and
      # irrigation level.
      #
      # All data represent a statistical year around 2000, and the GTAP/Monfreda data were
      # used in constructing the gridded and national inventory data, so this is the appropriate
      # production data to use.
      #
      # The major methodological decisions relate to how to estimate the coefficients by GLU for
      # crops that are not available in the gridded inventory, how to extrapolate from the given
      # crops to the fodder crops, and how to assign priority for the crops included in both the
      # gridded and national inventories. These decisions are documented below.

      # The inventory data disaggregate green, blue, and gray water. Gray water indicates
      # downstream pollution; this is not considered in GCAM and has not been included in the
      # gridded data processing. Blue water is assigned to only irrigated production, but green
      # water applies to both irrigated and rainfed production, as the two management technologies
      # are not disaggregated in the inventory data. For rainfed crops, total biophysical = green,
      # and for irrigated crops, total biophysical = blue + green.

      # Initial data cleaning (original lines 55-57)
      L100.Water_footprint_m3 %>%
        rename(MH_crop = GTAP_crop) %>%
        left_join_error_no_match(select(FAO_ag_items_PRODSTAT, MH_crop, GTAP_crop), by = "MH_crop") ->
        L165.Water_footprint_m3

      # Re-cast nation-level data so that blue and green are represented as data columns (original lines 59-64)
      Mekonnen_Hoekstra_Rep47_A2 %>%
        gather(iso, coef_m3t, -FAO_crop, -water_type) %>%
        mutate(coef_m3kg = coef_m3t / CONV_T_KG) %>%
        select(iso, FAO_crop, water_type, coef_m3kg) %>%
        # Set all missing values to zero -- this may be re-visited at another point
        # We may want to substitute default values from the MH2011 table
        mutate(coef_m3kg = if_else(is.na(coef_m3kg), 0, coef_m3kg)) %>%
        spread(water_type, coef_m3kg) ->
        L165.Mekonnen_Hoekstra_Rep47_A2

      # Building inventory of water use by country, crop, and GLU for the 18 gridded crops (lines 66-74 in original)
      # We give precedence to the aggregated gridded data for the 18 crops that are mapped,
      # and fill in nation-level inventory data for the remainder
      L165.ag_Prod_t_ctry_crop_GLU <- rename(L100.LDS_ag_prod_t, Prod_t = value)

      L165.Water_footprint_m3 %>%
        filter(water_type %in% c("green", "blue")) %>%
        # TODO: this following join has NAs but shouldn't change # of rows (currently doesn't) - use left_join_restricted
        left_join(L165.ag_Prod_t_ctry_crop_GLU, by = c("iso", "GTAP_crop", "GLU")) %>%
        # M+H have some country/crop/GLU combinations that Monfreda doesn't
        # Set missing values to 0
        mutate(Prod_t = if_else(is.na(Prod_t), 0L, Prod_t)) %>%
        # For the 18 M+H gridded crops, calculate the coefs by country, GLU, and crop (original lines 79-82)
        # NOTE: dropping all country / GLU / crops with zero production; these won't have any water anyway
        filter(Prod_t > 0) %>%
        mutate(coef_m3kg = value / (Prod_t * CONV_T_KG)) ->
        L165.ag_Water_ctry_MHcrop_GLU

      # Clip extremely high values which are due to statistical discrepancies (original lines 84-106)
      # The assumed maximum values are crop specific and based on analysis of the 2011 inventory data
      # In this step, we're adding 2 m3/kg to every maximum observed value across all national averages.
      # We want to use something a bit higher than the maximum of the national averages, because the
      # sub-national regions' values will have a greater range.
      #
      # Still, this sort of cap is necessary because many of the water quantities, particularly in
      # small regions, are clearly inconsistent with the underlying production data and produce
      # extremely high water demand coefficients. In the case of Barley in Iraq it is clearly an
      # error in the M+H gridded inventory, but for others it could be a simple mis-match in
      # estimates of crop production by region, and share of irrigated production by region.
      ctry_GLU_max_adder <- 2
      L165.Mekonnen_Hoekstra_Rep47_A2 %>%
        group_by(FAO_crop) %>%
        summarise(Blue = max(Blue + ctry_GLU_max_adder),
                  Green = max(Green + ctry_GLU_max_adder)) %>%
        left_join(select(FAO_ag_items_PRODSTAT, item, MH_crop), by = c("FAO_crop" = "item")) ->
        L165.MaxWaterCoefs_m3kg

      # Change L165.ag_Water_ctry_MHcrop_GLU$coef_m3kg to be the minimum of itself and
      # the corresponding (by crop and water_type) value in L165.MaxWaterCoefs_m3kg
      L165.MaxWaterCoefs_m3kg %>%
        gather(water_type, value_other, Blue, Green) %>%
        mutate(water_type = tolower(water_type)) %>%
        right_join(L165.ag_Water_ctry_MHcrop_GLU, by = c("MH_crop", "water_type")) %>%
        mutate(coef_m3kg = pmin(coef_m3kg, value_other)) %>%
        select(-value_other, -FAO_crop) ->
        L165.ag_Water_ctry_MHcrop_GLU

      # Work through calculation sequence to determine blue and green water coefficients for irrigated
      # and rainfed production for gridded crops (original lines 108-122). The sequence has 5 steps:
      #	(1) Calculate the total biophysical (green + blue) water quantities and coefficients for each country, GLU, and crop
      #	(2) Match in the irrigated production and calculate the blue water coef for irrigated as min(blue water quantity / irr prod, total biophysical coef)
      #	(3) Calculate the green water coef for irrigated crops as the total biophysical coef minus the blue water coef
      #	(4) Calculate the total volume of green water on rainfed crops, as the total green water minus green water used by irrigated crops
      #	(5) Calculate the green water coef for rainfed crops as rainfed green water volume divided by rainfed production
      L165.ag_Water_ctry_MHcrop_GLU %>%
        ungroup %>%
        select(-MH_crop, -value) %>%
        mutate(water_type = paste0(water_type, "_m3kg")) %>%
        spread(water_type, coef_m3kg, fill = 0.0) %>%
        mutate(total_m3kg = blue_m3kg + green_m3kg,
               blue_thousm3 = Prod_t * blue_m3kg,
               green_thousm3 = Prod_t * green_m3kg) ->
        L165.ag_Water_ctry_MHcrop_GLU

      # Match in the irrigated production to calculate the blue water coef for irrigated crops (original lines 124-128)
      L165.ag_Water_ctry_MHcrop_GLU %>%
        left_join_error_no_match(L151.ag_irrProd_t_ctry_crop, by = c("iso", "GTAP_crop", "GLU")) %>%
        mutate(BlueIrr_m3kg = blue_thousm3 / irrProd_t)

    }

    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L165.BlueIrr_m3kg_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L100.Water_footprint_m3",
                     "L100.LDS_ag_prod_t",
                     "aglu/Mekonnen_Hoekstra_Rep47_A2",
                     "aglu/Rohwer_2007_IrrigationEff",
                     "L151.ag_irrProd_t_ctry_crop",
                     "L151.ag_rfdProd_t_ctry_crop",
                     "L151.ag_irrHA_ha_ctry_crop") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L165.BlueIrr_m3kg_R_C_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L165.TotIrr_m3kg_R_C_GLU") %>%
      same_precursors_as(L165.BlueIrr_m3kg_R_C_GLU) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L165.TotIrr_m3kg_R_C_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L165.GreenRfd_m3kg_R_C_GLU") %>%
      same_precursors_as(L165.BlueIrr_m3kg_R_C_GLU) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L165.GreenRfd_m3kg_R_C_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L165.ag_IrrEff_R") %>%
      same_precursors_as(L165.BlueIrr_m3kg_R_C_GLU) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L165.ag_IrrEff_R

    return_data(L165.BlueIrr_m3kg_R_C_GLU, L165.TotIrr_m3kg_R_C_GLU, L165.GreenRfd_m3kg_R_C_GLU, L165.ag_IrrEff_R)
  } else {
    stop("Unknown command")
  }
}
