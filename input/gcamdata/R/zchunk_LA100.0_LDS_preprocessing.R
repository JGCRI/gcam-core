# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LA100.0_LDS_preprocessing
#'
#' Read in and process LDS (Land Data System) files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{object}. The corresponding file in the
#' original data system was \code{LA100.0_LDS_preprocessing.R} (aglu level1).
#' @details Read in the various LDS datasets; regularize their column names and
#' GLU (Geographic Land Unit) data; change Taiwan ISO to that of mainland China; make LDS_ag_HA_ha and
#' LDS_ag_prod_t tables consistent. See Di Vittorio, A., P. Kyle, and W. Collins. 2016.
#' What are the effects of Agro-Ecological Zones and land use region boundaries on
#' land resource projection using the Global Change Assessment Model? Environmental
#' Modelling & Software 85, 246-265. http://dx.doi.org/10.1016/j.envsoft.2016.08.016.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate semi_join summarise
#' @author BBL March 2017
module_aglu_LA100.0_LDS_preprocessing <- function(command, ...) {

  namelist <- c("Land_type_area_ha",
                "LDS_ag_HA_ha",
                "LDS_ag_prod_t",
                "LDS_value_milUSD",
                "MIRCA_irrHA_ha",
                "MIRCA_rfdHA_ha",
                "Mueller_yield_levels",
                "Ref_veg_carbon_Mg_per_ha",
                "Water_footprint_m3")
  dirname <- "aglu/LDS/"

  if(command == driver.DECLARE_INPUTS) {
    x <- paste0(dirname, namelist)
    names(x) <- rep("FILE", length(x))
    return(x)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(paste0("L100.", namelist))
  } else if(command == driver.MAKE) {

    . <- value <- iso <- GTAP_crop <- GLU <- MIRCA_crop <- NULL             # silence package check.
    L100.Land_type_area_ha <- L100.LDS_value_milUSD <- L100.MIRCA_irrHA_ha <-
        L100.MIRCA_rfdHA_ha <- L100.Mueller_yield_levels <-
        L100.Ref_veg_carbon_Mg_per_ha <- L100.Water_footprint_m3 <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs ----
    LDSfiles <- list()
    for(nm in namelist) {
      LDSfiles[[nm]] <- get_data(all_data, paste0(dirname, nm))
    }

    # Go through all data frames and...
    for(nm in namelist) {

      # Regularize data frame names
      names(LDSfiles[[nm]]) %>%
        sub("ctry_iso", "iso", .) %>%
        sub("reglr_iso", "GTAP_region", .) %>%
        sub("glu_code", aglu.GLU, .) %>%
        sub("land_type", "land_code", .) %>%
        sub("SAGE_crop", "GTAP_crop", .) %>%
        sub("mirca_crop", "MIRCA_crop", .) %>%
        sub("use_sector", "GTAP_use", .) ->
        names(LDSfiles[[nm]])

      # Replace numerical GLU code with a concatenation of "GLU" and the
      # three-digit code (padded with zeroes as necessary)
      if(aglu.GLU %in% names(LDSfiles[[nm]])) {
        LDSfiles[[nm]][[aglu.GLU]] <- paste(aglu.GLU, sprintf("%03d", LDSfiles[[nm]][[aglu.GLU]]), sep = aglu.GLU_NAME_DELIMITER)
      }

      # From GPK 3/31/17: We don't have Taiwan as an aglu region, because (a) Taiwan was excluded
      # from SAGE/HYDE in the aggregation that Yuyu did, and (b) it was excluded from FAOSTAT
      # when I queried the data four years ago. Alan Di Vittorio has addressed (a); it's now
      # included in the land cover data that's the input to the data system. And, FAOSTAT now
      # includes Taiwan in their various databases. However, the FAOSTAT data currently in the
      # data system was queried in about 2012, and Taiwan was added in 2014, so it's not actually
      # in our level0 data files. As such, we're not yet in a position to add Taiwan as an aglu
      # region. We would need to update our FAOSTAT queries, which will come along with more fun
      # because they've certainly changed country names and quantities (if not the available data
      # altogether). The steps performed here basically re-map the ISO code of Taiwan back to China,
      # and aggregate anything that needs aggregation (e.g., quantity variables like land cover
      # totals, but not characteristic variables like vegetative carbon densities).

      # Re-set Taiwan to mainland China, as the current version of AgLU
      # (and pre-2015 versions of FAOSTAT) doesn't disaggregate Taiwan
      if("iso" %in% names(LDSfiles[[nm]])) {
        d <- LDSfiles[[nm]]
        if(nm != "Ref_veg_carbon_Mg_per_ha") {
          at <- attributes(d)
          d %>%
            # group by everything EXCEPT for value and sum up
            dplyr::group_by_at(dplyr::vars(-value)) %>%
            summarise(value = sum(value)) %>%
            ungroup() %>%
            # summarise() produces a new tibble, but we don't want to lose file info
            same_attributes_as(d) %>%
            add_comments("Since 2015 BY update, data available for Taiwan as an agricultural region.")->
            LDSfiles[[nm]]
        }
        # Drop Taiwan from the carbon contents
        if(nm == "Ref_veg_carbon_Mg_per_ha") {
          d %>%
            add_comments("Since 2015 BY update, data available for Taiwan as an agricultural region.") ->
            LDSfiles[[nm]]
        }
      }
    }

    # Add necessary legacy and precursor information and assign to environment
    for(nm in namelist) {
      legacy_name <- paste0("L100.", nm)
      LDSfiles[[nm]] %>%
        add_legacy_name(legacy_name) %>%
        add_precursors(paste0(dirname, nm)) ->
        df
      assign(legacy_name, df)
    }

    # The production and harvested area tables have values <1 clipped, resulting
    # in some country/glu/crops present in one but not the other. For now these will
    # simply be dropped; in the future, we may want to add a digit of rounding in the lds
    L100.LDS_ag_HA_ha %>%
      semi_join(L100.LDS_ag_prod_t, by = c("iso", aglu.GLU, "GTAP_crop")) ->
      L100.LDS_ag_HA_ha
    L100.LDS_ag_prod_t %>%
      semi_join(L100.LDS_ag_HA_ha, by = c("iso", aglu.GLU, "GTAP_crop")) ->
      L100.LDS_ag_prod_t

    ## 9/30/2019 modification (gpk, kbn)
    # Taiwan is now included but has some discrepancies between FAOSTAT and Monfreda that cause issues in one of the
    # land use regions. Specifically, Flax Fiber and Tow has about 1000 ha in FAOSTAT, and 1 in Monfreda, which is assigned
    # to the smaller land use region. Also, wheat harvested area has a significant dip in the years around 2000; the FAOSTAT
    # estimate in the trough years is ~50 whereas otherwise it is about 1000. Monfreda allocates 100% of this area to the smaller
    # land use region. The steps below insert quantities for harvested area and production...
    #  - Flax fiber and tow: assign a value to GLU103 of similar magnitude to the data in FAOSTAT
    #  - Wheat: re-assign the production from GLU078 to GLU103


    #1. Adjustment for wheat ----
    #a. Get Taiwan's data for GLU078 for wheat for production, harvested area and MIRCA
    GLUDataforWheat = subset(L100.LDS_ag_HA_ha,GTAP_crop=='Wheat'& GLU=='GLU078'& iso=="twn")
    Value = GLUDataforWheat$value

    GLUProdDataforWheat = subset(L100.LDS_ag_prod_t,GTAP_crop=='Wheat'& GLU=='GLU078'& iso=="twn")
    ProdValue=GLUProdDataforWheat$value

    GLUIRRDataforWheat=subset(L100.MIRCA_rfdHA_ha,MIRCA_crop==1 & GLU=='GLU078'& iso=="twn")
    irrValue=GLUIRRDataforWheat$value
    #b. Transfer data into GLU103, assign a small seed value to GLU078 to avoid null values.MIRCA also bifurcates arable land by irrigated and rainfed.
    #We have adjusted both below.
    if (Value > 0){

      L100.LDS_ag_HA_ha<-add_row(L100.LDS_ag_HA_ha,iso="twn",GLU="GLU103",GTAP_crop='Wheat',value=Value)
      L100.LDS_ag_HA_ha<-L100.LDS_ag_HA_ha[!(L100.LDS_ag_HA_ha$iso =="twn" & L100.LDS_ag_HA_ha$GLU=="GLU078" & L100.LDS_ag_HA_ha$GTAP_crop=="Wheat"),]
      L100.LDS_ag_HA_ha<-add_row(L100.LDS_ag_HA_ha,iso="twn",GLU="GLU078",GTAP_crop='Wheat',value=1)

      L100.LDS_ag_prod_t<-add_row(L100.LDS_ag_prod_t,iso="twn",GLU="GLU103",GTAP_crop='Wheat',value=Value)
      L100.LDS_ag_prod_t<-L100.LDS_ag_prod_t[!(L100.LDS_ag_prod_t$iso =="twn" & L100.LDS_ag_prod_t$GLU=="GLU078" & L100.LDS_ag_prod_t$GTAP_crop=="Wheat"),]
      L100.LDS_ag_prod_t<-add_row(L100.LDS_ag_prod_t,iso="twn",GLU="GLU078",GTAP_crop='Wheat',value=1)

      L100.MIRCA_rfdHA_ha<-add_row(L100.MIRCA_rfdHA_ha,iso="twn",GLU="GLU103",MIRCA_crop=1,value=Value)
      L100.MIRCA_rfdHA_ha<-L100.MIRCA_rfdHA_ha[!(L100.MIRCA_rfdHA_ha$iso =="twn" & L100.MIRCA_rfdHA_ha$GLU=="GLU078" & L100.MIRCA_rfdHA_ha$MIRCA_crop==1),]
      L100.MIRCA_rfdHA_ha<-add_row(L100.MIRCA_rfdHA_ha,iso="twn",GLU="GLU078",MIRCA_crop=1,value=1)

      L100.MIRCA_irrHA_ha<-add_row(L100.MIRCA_irrHA_ha,iso="twn",GLU="GLU078",MIRCA_crop=1,value=0)

    }

    #2. Adjustment for Flax ----
    #Add rows for production and harvested area for GLU103 with values that are commensurate with FAOSTAT.
    GLUDataforFlax = subset(L100.LDS_ag_HA_ha,GTAP_crop=='FlaxFibr_Tow'& GLU=='GLU078'& iso=="twn")
    Value = GLUDataforFlax$value

    if (Value > 0){

      L100.LDS_ag_HA_ha<-add_row(L100.LDS_ag_HA_ha,iso="twn",GLU="GLU103",GTAP_crop='FlaxFibr_Tow',value=2000)
      L100.LDS_ag_prod_t<-add_row(L100.LDS_ag_prod_t,iso="twn",GLU="GLU103",GTAP_crop='FlaxFibr_Tow',value=1000)
    }

    #3. Adjustment for Rapeseed and Barley ----
    # Move all harvested area and production from GLU078 to GLU103
    L100.LDS_ag_HA_ha$GLU[L100.LDS_ag_HA_ha$iso == "twn" &
                            L100.LDS_ag_HA_ha$GTAP_crop == "Rapeseed"] <- "GLU103"
    L100.LDS_ag_prod_t$GLU[L100.LDS_ag_prod_t$iso == "twn" &
                             L100.LDS_ag_prod_t$GTAP_crop == "Rapeseed"] <- "GLU103"

    L100.LDS_ag_HA_ha$GLU[L100.LDS_ag_HA_ha$iso == "twn" &
                            L100.LDS_ag_HA_ha$GTAP_crop == "Barley"] <- "GLU103"
    L100.LDS_ag_prod_t$GLU[L100.LDS_ag_prod_t$iso == "twn" &
                             L100.LDS_ag_prod_t$GTAP_crop == "Barley"] <- "GLU103"

    #4. Adjustment for Soybean (production in the 1970's was >100x the production in ~2000; using the 2000-era GLU shares leads to too much land required in GLU078) ----
    # Soybean: move nearly all harvested area and production from GLU078 to GLU103, by setting the production and harvested area in GLU078 to a nominal value.
    L100.LDS_ag_HA_ha$value[L100.LDS_ag_HA_ha$iso == "twn" &
                          L100.LDS_ag_HA_ha$GTAP_crop == "Soybeans" &
                          L100.LDS_ag_HA_ha$GLU == "GLU078"] <- 1
    L100.LDS_ag_prod_t$value[L100.LDS_ag_prod_t$iso == "twn" &
                           L100.LDS_ag_prod_t$GTAP_crop == "Soybeans" &
                           L100.LDS_ag_HA_ha$GLU == "GLU078"] <- 1

    #5. Adjustment for Sweet potatoes (production in the 1970's was >20x the production in ~2000. GLU-wise allocation from ~2000 causes issues in GLU078 ----
    L100.LDS_ag_HA_ha$value[L100.LDS_ag_HA_ha$iso == "twn" &
                              L100.LDS_ag_HA_ha$GTAP_crop == "SweetPotato" &
                              L100.LDS_ag_HA_ha$GLU == "GLU078"] <- 1
    L100.LDS_ag_prod_t$value[L100.LDS_ag_prod_t$iso == "twn" &
                               L100.LDS_ag_prod_t$GTAP_crop == "SweetPotato" &
                               L100.LDS_ag_HA_ha$GLU == "GLU078"] <- 1

    #6. Adjustment for GrndntWShll
    L100.LDS_ag_HA_ha$value[L100.LDS_ag_HA_ha$iso == "twn" &
                              L100.LDS_ag_HA_ha$GTAP_crop == "GrndntWShll" &
                              L100.LDS_ag_HA_ha$GLU == "GLU078"] <- 1
    L100.LDS_ag_prod_t$value[L100.LDS_ag_prod_t$iso == "twn" &
                               L100.LDS_ag_prod_t$GTAP_crop == "GrndntWShll" &
                               L100.LDS_ag_HA_ha$GLU == "GLU078"] <- 1
    #6. Adjustment for VgtbFrshNES
    L100.LDS_ag_HA_ha$value[L100.LDS_ag_HA_ha$iso == "twn" &
                              L100.LDS_ag_HA_ha$GTAP_crop == "VgtbFrshNES" &
                              L100.LDS_ag_HA_ha$GLU == "GLU078"] <- 1
    L100.LDS_ag_prod_t$value[L100.LDS_ag_prod_t$iso == "twn" &
                               L100.LDS_ag_prod_t$GTAP_crop == "VgtbFrshNES" &
                               L100.LDS_ag_HA_ha$GLU == "GLU078"] <- 1


    #6. Adjustment for small yield crops in small region ----
    # yield too small: GTAP_crop == "FrgProdNES", GLU %in% c("GLU049", "GLU021"), iso == "pol"
    # adjust using average yield in the region doesn't help
    # simply filter out both land and prod

    L100.LDS_ag_prod_t %>% filter(!(iso == "pol" & GTAP_crop == "FrgProdNES" & GLU %in% c("GLU049", "GLU021"))) ->
      L100.LDS_ag_prod_t
    L100.LDS_ag_HA_ha %>% filter(!(iso == "pol" & GTAP_crop == "FrgProdNES" & GLU %in% c("GLU049", "GLU021"))) ->
      L100.LDS_ag_HA_ha


    # And we're done
    return_data(L100.Land_type_area_ha,
                L100.LDS_ag_HA_ha,
                L100.LDS_ag_prod_t,
                L100.LDS_value_milUSD,
                L100.MIRCA_irrHA_ha,
                L100.MIRCA_rfdHA_ha,
                L100.Mueller_yield_levels,
                L100.Ref_veg_carbon_Mg_per_ha,
                L100.Water_footprint_m3)
  } else {
    stop("Unknown command")
  }
}
