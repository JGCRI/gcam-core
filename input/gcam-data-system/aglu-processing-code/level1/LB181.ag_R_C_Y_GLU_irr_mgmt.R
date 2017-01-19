# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu processing scripts, please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "LB181.ag_R_C_Y_GLU_irr_mgmt.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Agricultural production and cropland area by region, GLU, crop, year, irrigation, and management technology" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
Mueller_yield_levels <- readdata( "AGLU_LDS_DATA", "Mueller_yield_levels", na.strings = "NA" )
Muller_crops <- readdata( "AGLU_MAPPINGS", "Muller_crops" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
L151.ag_irrHA_ha_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_irrHA_ha_ctry_crop" )
L151.ag_rfdHA_ha_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_rfdHA_ha_ctry_crop" )
L151.ag_irrProd_t_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_irrProd_t_ctry_crop" )
L151.ag_rfdProd_t_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_rfdProd_t_ctry_crop" )
L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU" )
L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU" )
L171.ag_irrEcYield_kgm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L171.ag_irrEcYield_kgm2_R_C_Y_GLU" )
L171.ag_rfdEcYield_kgm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Merging irrigated and rainfed files" )
L181.ag_HA_ha_ctry_crop_irr <- merge( L151.ag_irrHA_ha_ctry_crop, L151.ag_rfdHA_ha_ctry_crop )
L181.ag_HA_ha_ctry_crop_irr <- melt( L181.ag_HA_ha_ctry_crop_irr,
                                     id.vars = c( "iso", "GTAP_crop", GLU ),
                                     variable.name = irr, value.name = "HA_ha" )
L181.ag_HA_ha_ctry_crop_irr[[irr]] <- sub( "HA", "", L181.ag_HA_ha_ctry_crop_irr[[irr]] )

L181.ag_Prod_t_ctry_crop_irr <- merge( L151.ag_irrProd_t_ctry_crop, L151.ag_rfdProd_t_ctry_crop )
L181.ag_Prod_t_ctry_crop_irr <- melt( L181.ag_Prod_t_ctry_crop_irr,
                                      id.vars = c( "iso", "GTAP_crop", GLU ),
                                      variable.name = irr, value.name = "Prod_t" )
L181.ag_Prod_t_ctry_crop_irr[[irr]] <- sub( "Prod", "", L181.ag_Prod_t_ctry_crop_irr[[irr]] )

L181.ag_Yield_tha_ctry_crop_irr <- merge( L181.ag_HA_ha_ctry_crop_irr, L181.ag_Prod_t_ctry_crop_irr )
L181.ag_Yield_tha_ctry_crop_irr$yield_tha <- with( L181.ag_Yield_tha_ctry_crop_irr, Prod_t / HA_ha )

#Match in Mueller's yields. First prepare the data for matching in
printlog( "Matching in Mueller database low and high yields for each available crop and region" )
L181.Mueller_yield_levels <- na.omit( Mueller_yield_levels )
names( L181.Mueller_yield_levels )[ names( L181.Mueller_yield_levels ) == "Country" ] <- "iso"

#Replace Romania (from rou to rom), Serbia (from srb to scg; dropping mne), Taiwan (re-set to China)
L181.Mueller_yield_levels <- subset( L181.Mueller_yield_levels, !iso == "mne" )
L181.Mueller_yield_levels$iso <- sub( "srb", "scg", L181.Mueller_yield_levels$iso )
L181.Mueller_yield_levels$iso[ L181.Mueller_yield_levels$iso == "twn"] <- "chn"

L181.Mueller_yield_levels[[GLU]] <- paste0("GLU", sprintf( "%03d", L181.Mueller_yield_levels$Basin ) )
L181.Mueller_yield_levels$GTAP_crop <- Muller_crops$GTAP_crop[ match( L181.Mueller_yield_levels$crop, Muller_crops$crop ) ]
L181.Mueller_yield_levels_lo <- subset( L181.Mueller_yield_levels, yield_level == "_02ndpercentileyield" )
L181.Mueller_yield_levels_hi <- subset( L181.Mueller_yield_levels, yield_level %in% c( "_95thpercentileyield", "_rainfedyieldceilings" ) )
L181.Mueller_yield_levels_hi$Irr_Rfd <- "irr"
L181.Mueller_yield_levels_hi$Irr_Rfd[ L181.Mueller_yield_levels_hi$yield_level == "_rainfedyieldceilings" ] <- "rfd"

#Subset only the crops, countries, and GLUs from the GTAP database that are represented in the Muller data
match.fields <- c( "iso", "GTAP_crop", GLU )
L181.Mueller_ag_Yield_tha_irr <- L181.ag_Yield_tha_ctry_crop_irr[
      vecpaste( L181.ag_Yield_tha_ctry_crop_irr[ match.fields ] ) %in%
      vecpaste( L181.Mueller_yield_levels[ match.fields ] ), ]

#Only use data where production (and harvested area) is non-zero
L181.Mueller_ag_Yield_tha_irr <- subset( L181.Mueller_ag_Yield_tha_irr, HA_ha > 0 )

#Match in lo yields
min_yield_adj <- 0.05
#Use Mueller's 2nd percentile average to estimate the "lower" yielding technology
L181.Mueller_ag_Yield_tha_irr$lo <- L181.Mueller_yield_levels_lo$average[
      match( vecpaste( L181.Mueller_ag_Yield_tha_irr[ match.fields ] ),
             vecpaste( L181.Mueller_yield_levels_lo[ match.fields ] ) ) ]
printlog( "Adjusting lo and hi yields where the observed yields were not within the bounds" )
#Where Muller's 2nd percentile averages are higher than observed yields, use observed times an adjustment fraction
L181.Mueller_ag_Yield_tha_irr$lo[ L181.Mueller_ag_Yield_tha_irr$lo > L181.Mueller_ag_Yield_tha_irr[[ "yield_tha" ]] * ( 1 - min_yield_adj ) ] <- 
      L181.Mueller_ag_Yield_tha_irr[[ "yield_tha" ]][ L181.Mueller_ag_Yield_tha_irr$lo > L181.Mueller_ag_Yield_tha_irr[[ "yield_tha" ]] * ( 1 - min_yield_adj ) ] *
      ( 1 - min_yield_adj )

#Match in hi yields
L181.Mueller_ag_Yield_tha_irr$hi <- L181.Mueller_yield_levels_hi$average[
      match( vecpaste( L181.Mueller_ag_Yield_tha_irr[ c( match.fields, irr ) ] ),
             vecpaste( L181.Mueller_yield_levels_hi[ c( match.fields, irr ) ] ) ) ]

#Because many of the crops have no rainfed yield ceiling information available, use the 95th percentile de-rated by some fraction
# Calculate the fraction to use, specific to each land use region
printlog( "Calculating an irrigated:rainfed maximum yield derating factor, to use for ctry/GLU/crops where rainfed yield ceilings were not reported" )
L181.RfdDerating_ctry_crop_GLU <- subset( L181.Mueller_ag_Yield_tha_irr, Irr_Rfd == "irr")
L181.RfdDerating_ctry_crop_GLU[[irr]] <- "rfd"
names(L181.RfdDerating_ctry_crop_GLU)[ names( L181.RfdDerating_ctry_crop_GLU ) == "hi" ] <- "hi_irr"
L181.RfdDerating_ctry_crop_GLU$hi_rfd <- L181.Mueller_yield_levels_hi$average[
  match( vecpaste( L181.RfdDerating_ctry_crop_GLU[ c( match.fields, irr )]),
         vecpaste( L181.Mueller_yield_levels_hi[ c( match.fields, irr )]))]
L181.RfdDerating_ctry_crop_GLU$wt_derating <- with( L181.RfdDerating_ctry_crop_GLU, HA_ha * hi_rfd / hi_irr)
L181.RfdDerating_ctry_crop_GLU <- na.omit( L181.RfdDerating_ctry_crop_GLU )
L181.RfdDerating_ctry_GLU<-aggregate( L181.RfdDerating_ctry_crop_GLU[ c( "HA_ha", "wt_derating" ) ],
                 by = L181.RfdDerating_ctry_crop_GLU[ c( "iso", "GLU" ) ], sum, na.rm = T )
L181.RfdDerating_ctry_GLU$derating <- with( L181.RfdDerating_ctry_GLU, wt_derating / HA_ha )

#Match this into the dataset
L181.Mueller_ag_Yield_tha_irr$derating <- L181.RfdDerating_ctry_GLU$derating[
  match( vecpaste( L181.Mueller_ag_Yield_tha_irr[ c( "iso", "GLU" ) ] ),
         vecpaste( L181.RfdDerating_ctry_GLU[ c( "iso", "GLU" ) ] ) ) ]
#Re-set derating factor to 1 for irrigated crops, just to make sure that none of these get derated
L181.Mueller_ag_Yield_tha_irr$derating[ L181.Mueller_ag_Yield_tha_irr[[irr ]] == "irr" ] <- 1

#Set missing values to zero. These will be over-written later
L181.Mueller_ag_Yield_tha_irr$derating[ is.na( L181.Mueller_ag_Yield_tha_irr$derating ) ] <- 0
L181.Mueller_ag_Yield_tha_irr$hi[ is.na( L181.Mueller_ag_Yield_tha_irr$hi ) ] <- L181.Mueller_yield_levels_hi$average[
      match( vecpaste( L181.Mueller_ag_Yield_tha_irr[ is.na( L181.Mueller_ag_Yield_tha_irr$hi ), match.fields ] ),
             vecpaste( L181.Mueller_yield_levels_hi[ match.fields ] ) ) ] *
  L181.Mueller_ag_Yield_tha_irr$derating[ is.na( L181.Mueller_ag_Yield_tha_irr$hi ) ]
L181.Mueller_ag_Yield_tha_irr$derating <- NULL

#Where Muller's 95th percentile / rainfed ceiling averages are less than observed yields, use observed times (1+adjustment fraction)
L181.Mueller_ag_Yield_tha_irr$hi[ L181.Mueller_ag_Yield_tha_irr$hi < L181.Mueller_ag_Yield_tha_irr[[ "yield_tha" ]] * ( 1 + min_yield_adj ) ] <- 
      L181.Mueller_ag_Yield_tha_irr[[ "yield_tha" ]][ L181.Mueller_ag_Yield_tha_irr$hi < L181.Mueller_ag_Yield_tha_irr[[ "yield_tha" ]] * ( 1 + min_yield_adj ) ] *
      ( 1 + min_yield_adj )

printlog( "Calculating lo and hi yields of crops and regions in the GTAP database that aren't represented in the Mueller database" )
# Apply a generic functional form in order to get "hi" yields of crops not covered by Mueller
# Separately subset the crops that are not in the Muller data. Yield multipliers and land shares will be assigned from the above calc
L181.noMueller_ag_Yield_tha_irr <- L181.ag_Yield_tha_ctry_crop_irr[
  vecpaste( L181.ag_Yield_tha_ctry_crop_irr[ match.fields ] ) %!in%
    vecpaste( L181.Mueller_yield_levels[ match.fields ] ), ]
L181.noMueller_ag_Yield_tha_irr <- subset( L181.noMueller_ag_Yield_tha_irr, HA_ha > 0 )

# First step is to figure out, for the 17 available crops, how far each land use region's 95th percentile yields
# are from the global maximum 95th percentile yields. This gives a climate-based index of yields that can be
# applied to the global 95th percentile yields of each of the non-covered crops
L181.Mueller_YieldIndex_irr <- L181.Mueller_ag_Yield_tha_irr
L181.Mueller_YieldMax_irr <- aggregate( L181.Mueller_YieldIndex_irr[ "hi" ],
                                        by = L181.Mueller_YieldIndex_irr[ c( "GTAP_crop", irr ) ],
                                        max )
L181.Mueller_YieldIndex_irr$maxYield <- L181.Mueller_YieldMax_irr$hi[
  match( vecpaste( L181.Mueller_YieldIndex_irr[ c( "GTAP_crop", irr ) ] ),
         vecpaste( L181.Mueller_YieldMax_irr[ c( "GTAP_crop", irr ) ] ) ) ]
L181.Mueller_YieldIndex_irr$wt_YieldIndex <- with( L181.Mueller_YieldIndex_irr, hi * HA_ha / maxYield )
L181.YieldIndex_ctry_GLU_irr <- aggregate( L181.Mueller_YieldIndex_irr[ c( "HA_ha", "wt_YieldIndex" ) ],
                                         by = L181.Mueller_YieldIndex_irr[ c( "iso", GLU, irr ) ],
                                         sum )
L181.YieldIndex_ctry_GLU_irr$YieldIndex <- with( L181.YieldIndex_ctry_GLU_irr, wt_YieldIndex / HA_ha )

#Next, figure out the 95th percentile observed yield across all regions and agricultural regions (intersection of country and GLU)
# Using 95th percentile because many of the crops have absurdly high maximum values (e.g., cucumbers with >500t/ha)
# Final "hi" yields will be max of( ( average of 95th percentile and observed ) and ( observed plus min-yield-adj ) )
L181.noMueller_maxYield_crop <- aggregate( L181.noMueller_ag_Yield_tha_irr[ "yield_tha" ],
                                          by = L181.noMueller_ag_Yield_tha_irr[ "GTAP_crop" ],
                                          FUN = quantile, probs = 0.95 )
L181.noMueller_ag_Yield_tha_irr$lo <- L181.noMueller_ag_Yield_tha_irr$yield_tha / 2
L181.noMueller_ag_Yield_tha_irr$maxYield_C <- L181.noMueller_maxYield_crop$yield_tha[
  match( L181.noMueller_ag_Yield_tha_irr$GTAP_crop, L181.noMueller_maxYield_crop$GTAP_crop ) ]
L181.noMueller_ag_Yield_tha_irr$YieldIndex <- L181.YieldIndex_ctry_GLU_irr$YieldIndex[
  match( vecpaste( L181.noMueller_ag_Yield_tha_irr[ c( "iso", GLU, irr ) ] ),
         vecpaste( L181.YieldIndex_ctry_GLU_irr[ c( "iso", GLU, irr ) ] ) ) ]

# Not all countries, basins, and irrigation levels are necessarily represented in the yield index data
# Just set them to zero, and the yields will be re-set to the observed plus the min adjustment factor
L181.noMueller_ag_Yield_tha_irr$YieldIndex[ is.na( L181.noMueller_ag_Yield_tha_irr$YieldIndex ) ] <- 0
L181.noMueller_ag_Yield_tha_irr$hi <- with( L181.noMueller_ag_Yield_tha_irr,
                                           pmax( ( maxYield_C * YieldIndex ),
                                                 yield_tha * ( 1 + min_yield_adj ) ) )
L181.noMueller_ag_Yield_tha_irr <- L181.noMueller_ag_Yield_tha_irr[ !names( L181.noMueller_ag_Yield_tha_irr ) %in% c( "maxYield_C", "YieldIndex" ) ]

#Bind this back in with the dataset that only has Muller crops
L181.ag_Yield_tha_ctry_crop_irr_mgmt <- rbind( L181.Mueller_ag_Yield_tha_irr, L181.noMueller_ag_Yield_tha_irr )
# In order to calculate weighted yield levels for aggregation, we don't want to be using the raw yields, as our
# GCAM commodities may include a blend of heterogeneous yielding commodities. For example, cucumber yields are in
# excess of 400 tonnes/hectare in some places, whereas pulses tend to be about 2. In a non-indexed aggregation,
# the cucumbers would be the only crop that matters for the final yields, and the yield of the "high" technology
# would not be representative of a biophysically attainable yield for the commodity class as a whole. Instead, 
# each crop's low and high yields are simply indexed to the observed yield, and these multipliers are weighted
# by harvested area and aggregated.
printlog( "Calculating multipliers from observed to lo and to hi yields, in order to aggregate by GCAM regions and commodities" )
L181.ag_Yield_tha_ctry_crop_irr_mgmt[ c( "yieldmult_lo", "yieldmult_hi" ) ] <-
  L181.ag_Yield_tha_ctry_crop_irr_mgmt[ c( "lo", "hi" ) ] /
  L181.ag_Yield_tha_ctry_crop_irr_mgmt[[ "yield_tha" ]]
L181.ag_Yield_tha_ctry_crop_irr_mgmt[ c( "wt_yieldmult_lo", "wt_yieldmult_hi" ) ] <-
  L181.ag_Yield_tha_ctry_crop_irr_mgmt[ c( "yieldmult_lo", "yieldmult_hi" ) ] *
  L181.ag_Yield_tha_ctry_crop_irr_mgmt[[ "HA_ha" ]]

#This is now ready to have GCAM commodities and regions matched in, and to then be aggregated, weighted by production (observed, lo, and hi)
L181.ag_Yield_tha_ctry_crop_irr_mgmt[[R]] <- iso_GCAM_regID[[R]][
  match( L181.ag_Yield_tha_ctry_crop_irr_mgmt[["iso"]], iso_GCAM_regID[["iso"]])]
L181.ag_Yield_tha_ctry_crop_irr_mgmt[[C]] <- FAO_ag_items_PRODSTAT[[C]][
  match( L181.ag_Yield_tha_ctry_crop_irr_mgmt[["GTAP_crop"]], FAO_ag_items_PRODSTAT[["GTAP_crop"]])]
L181.YieldLevels_R_C_GLU_irr <- aggregate( L181.ag_Yield_tha_ctry_crop_irr_mgmt[ c( "HA_ha", "wt_yieldmult_lo", "wt_yieldmult_hi" ) ],
                                           by = L181.ag_Yield_tha_ctry_crop_irr_mgmt[ R_C_GLU_irr ],
                                           sum )
L181.YieldLevels_R_C_GLU_irr[ c( "yieldmult_lo", "yieldmult_hi" ) ] <-
  L181.YieldLevels_R_C_GLU_irr[ c( "wt_yieldmult_lo", "wt_yieldmult_hi" ) ] /
  L181.YieldLevels_R_C_GLU_irr[[ "HA_ha" ]]

printlog( "Applying yield multipliers to the baseline historical economic yields" )
#Multipliers are applied to economic yields (kg/m2/yr, not kg/m2/harvest), and shares are applied to land areas.
# Production is calculated as land area times yield
# First, calculate the new EcYields as the former yields times the yield mults, for hi and lo
#  EcYields are done first because a feasibility check will re-write some of the multipliers
L171.ag_rfdEcYield_kgm2_R_C_Y_GLU[[irr]] <- "rfd"
L171.ag_irrEcYield_kgm2_R_C_Y_GLU[[irr]] <- "irr"
L181.ag_EcYield_kgm2_R_C_Y_GLU_irr <- rbind( L171.ag_irrEcYield_kgm2_R_C_Y_GLU, L171.ag_rfdEcYield_kgm2_R_C_Y_GLU )
L181.ag_EcYield_kgm2_R_C_Y_GLU_irr.melt <- melt( L181.ag_EcYield_kgm2_R_C_Y_GLU_irr,
                                                 measure.vars = X_historical_years,
                                                 variable.name = Y )

#Match in the multipliers for the commodities that are matched
L181.ag_EcYield_kgm2_R_C_Y_GLU_irr.melt[ c( "yieldmult_lo", "yieldmult_hi" ) ] <-
  L181.YieldLevels_R_C_GLU_irr[
     match( vecpaste( L181.ag_EcYield_kgm2_R_C_Y_GLU_irr.melt[ R_C_GLU_irr ] ),
            vecpaste( L181.YieldLevels_R_C_GLU_irr[ R_C_GLU_irr ] ) ),
     c( "yieldmult_lo", "yieldmult_hi" ) ]

#Where land allocation and therefore yields are zero, set the multipliers to zero as well
L181.ag_EcYield_kgm2_R_C_Y_GLU_irr.melt[ L181.ag_EcYield_kgm2_R_C_Y_GLU_irr.melt$value == 0, c( "yieldmult_lo", "yieldmult_hi" ) ] <- 0

# Any remaining missing values would be minor combinations of region / crop / GLU / irrigation.
# These would be included in Monfreda/LDS/FAO/MIRCA, for commodities considered by Mueller, but not in the final Mueller aggregation
# Set the multipliers to 1
L181.ag_EcYield_kgm2_R_C_Y_GLU_irr.melt[ is.na( L181.ag_EcYield_kgm2_R_C_Y_GLU_irr.melt$yieldmult_lo ), c( "yieldmult_lo", "yieldmult_hi" ) ] <- 1

# Hi and lo yields are now calculated as the observed yield times the multipliers
L181.ag_EcYield_kgm2_R_C_Y_GLU_irr.melt[ c( "EcYield_kgm2_lo", "EcYield_kgm2_hi" ) ] <-
  L181.ag_EcYield_kgm2_R_C_Y_GLU_irr.melt$value *
  L181.ag_EcYield_kgm2_R_C_Y_GLU_irr.melt[ c( "yieldmult_lo", "yieldmult_hi" ) ]

printlog( "Calculating land shares to each technology in to return correct average yields" )
#Calculate the land shares to allocate to lo and hi
L181.YieldLevels_R_C_GLU_irr$landshare_lo <- with( L181.YieldLevels_R_C_GLU_irr, ( 1 - yieldmult_hi ) / (yieldmult_lo - yieldmult_hi ) )
L181.YieldLevels_R_C_GLU_irr$landshare_hi <- 1 - L181.YieldLevels_R_C_GLU_irr$landshare_lo

# Applying land shares to disaggregate lo- and hi-input land
L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU[[irr]] <- "rfd"
L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU[[irr]] <- "irr"
L181.LC_bm2_R_C_Yh_GLU_irr <- rbind( L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU, L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU )
L181.LC_bm2_R_C_Yh_GLU_irr.melt <- melt( L181.LC_bm2_R_C_Yh_GLU_irr,
                                         measure.vars = X_land_cover_years,
                                         variable.name = Y )

#Match in the shares for the commodities that are matched
L181.LC_bm2_R_C_Yh_GLU_irr.melt[ c( "landshare_lo", "landshare_hi" ) ] <-
  L181.YieldLevels_R_C_GLU_irr[
      match( vecpaste( L181.LC_bm2_R_C_Yh_GLU_irr.melt[ R_C_GLU_irr ] ),
             vecpaste( L181.YieldLevels_R_C_GLU_irr[ R_C_GLU_irr ] ) ),
      c( "landshare_lo", "landshare_hi" ) ]

#Where land allocation is zero, set the shares to zero as well
L181.LC_bm2_R_C_Yh_GLU_irr.melt[ L181.LC_bm2_R_C_Yh_GLU_irr.melt$value == 0, c( "landshare_lo", "landshare_hi" ) ] <- 0

#As above, any remaining missing values would be minor combinations of region / crop / GLU / irrigation.
# Leaving this step here in case observations are included in Monfreda/LDS/FAO/MIRCA crop data, for commodities considered by Mueller,
# but not reported in the Mueller aggregation
# Set the shares to 0.5 hi/lo, as no further information is available
L181.LC_bm2_R_C_Yh_GLU_irr.melt[ is.na( L181.LC_bm2_R_C_Yh_GLU_irr.melt$landshare_lo ), c( "landshare_lo", "landshare_hi" ) ] <- 0.5
L181.LC_bm2_R_C_Yh_GLU_irr.melt[ c( "LC_bm2_lo", "LC_bm2_hi" ) ] <-
  L181.LC_bm2_R_C_Yh_GLU_irr.melt$value *
      L181.LC_bm2_R_C_Yh_GLU_irr.melt[ c( "landshare_lo", "landshare_hi" ) ]

#Land: cast into two dataframes and bind together to be written out
L181.LC_bm2_R_C_Yh_GLU_irr_hi <- dcast( L181.LC_bm2_R_C_Yh_GLU_irr.melt,
                                        GCAM_region_ID + GCAM_commodity + GLU + Irr_Rfd ~ year, value.var = "LC_bm2_hi" )
L181.LC_bm2_R_C_Yh_GLU_irr_hi[[lvl]] <- "hi"
L181.LC_bm2_R_C_Yh_GLU_irr_lo <- dcast( L181.LC_bm2_R_C_Yh_GLU_irr.melt,
                                        GCAM_region_ID + GCAM_commodity + GLU + Irr_Rfd ~ year, value.var = "LC_bm2_lo" )
L181.LC_bm2_R_C_Yh_GLU_irr_lo[[lvl]] <- "lo"
L181.LC_bm2_R_C_Yh_GLU_irr_level <- rbind(
  L181.LC_bm2_R_C_Yh_GLU_irr_hi,
  L181.LC_bm2_R_C_Yh_GLU_irr_lo )[ c( R_C_GLU_irr_lvl, X_land_cover_years ) ]

#Yields: cast into two dataframes and bind together to be written out
L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_hi <- dcast( L181.ag_EcYield_kgm2_R_C_Y_GLU_irr.melt,
                                                GCAM_region_ID + GCAM_commodity + GLU + Irr_Rfd ~ year, value.var = "EcYield_kgm2_hi" )
L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_hi[[lvl]] <- "hi"
L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_lo <- dcast( L181.ag_EcYield_kgm2_R_C_Y_GLU_irr.melt,
                                                GCAM_region_ID + GCAM_commodity + GLU + Irr_Rfd ~ year, value.var = "EcYield_kgm2_lo" )
L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_lo[[lvl]] <- "lo"
L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level <- rbind(
  L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_hi,
  L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_lo )[ c( R_C_GLU_irr_lvl, X_historical_years ) ]

#Calculate production: economic yield times land area
L181.ag_Prod_Mt_R_C_Y_GLU_irr_level <- L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level
L181.ag_Prod_Mt_R_C_Y_GLU_irr_level[ X_historical_years ] <- L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level[ X_historical_years ] *
  L181.LC_bm2_R_C_Yh_GLU_irr_level[ match(
          vecpaste( L181.ag_Prod_Mt_R_C_Y_GLU_irr_level[ c( R_C_GLU_irr_lvl ) ] ),
          vecpaste( L181.LC_bm2_R_C_Yh_GLU_irr_level[ c( R_C_GLU_irr_lvl ) ] ) ),
      X_historical_years ]

printlog( "Calculating bioenergy yield levels" )
#For bioenergy yields, calculate a generic weighted lo, observed, and hi yield across all crops
L181.YieldLevels_R_GLU_irr <- aggregate( L181.YieldLevels_R_C_GLU_irr[ c( "HA_ha", "wt_yieldmult_lo", "wt_yieldmult_hi" ) ],
       by = L181.YieldLevels_R_C_GLU_irr[ R_GLU_irr ], sum )
L181.YieldLevels_R_GLU_irr[ c( "yieldmult_lo", "yieldmult_hi" ) ] <-
       L181.YieldLevels_R_GLU_irr[ c( "wt_yieldmult_lo", "wt_yieldmult_hi" ) ] /
       L181.YieldLevels_R_GLU_irr[[ "HA_ha" ]]
#don't let the bioenergy "hi" yield multipliers exceed some exogenous threshold
max_bio_mult_hi <- 3
L181.YieldLevels_R_GLU_irr$yieldmult_hi <- pmin( L181.YieldLevels_R_GLU_irr$yieldmult_hi, max_bio_mult_hi )
L181.YieldMult_R_bio_GLU_irr <- L181.YieldLevels_R_GLU_irr[ c( R_GLU_irr, "yieldmult_lo", "yieldmult_hi" ) ]

printlog( "Calculating bioenergy land shares" )
#For bioenergy ghost shares, write out the table of land shares
L181.YieldLevels_R_GLU_irr$landshare_lo <- with( L181.YieldLevels_R_GLU_irr, ( 1 - yieldmult_hi ) / (yieldmult_lo - yieldmult_hi ) )
L181.YieldLevels_R_GLU_irr$landshare_hi <- 1 - L181.YieldLevels_R_GLU_irr$landshare_lo
L181.LandShare_R_bio_GLU_irr <- L181.YieldLevels_R_GLU_irr[ c( R_GLU_irr, "landshare_lo", "landshare_hi" ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L181.LC_bm2_R_C_Yh_GLU_irr_level <- c( "Cropland cover by GCAM region / commodity / year / GLU / irrigation / mgmt level","Unit = bm2" )
comments.L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level <- c( "Economic yield by GCAM region / commodity / year / GLU / irrigation / mgmt level","Unit = kg/m2" )
comments.L181.ag_Prod_Mt_R_C_Y_GLU_irr_level <- c( "Agricultural production by GCAM region / commodity / year / GLU / irrigation / mgmt level","Unit = Mt" )
comments.L181.YieldMult_R_bio_GLU_irr <- c( "Yield multipliers for bioenergy by region / GLU / irrigation / mgmt level","Unitless" )
comments.L181.LandShare_R_bio_GLU_irr <- c( "Ghost land shares for bioenergy by region / GLU / irrigation / mgmt level","Unitless" )

writedata( L181.LC_bm2_R_C_Yh_GLU_irr_level, domain="AGLU_LEVEL1_DATA", fn="L181.LC_bm2_R_C_Yh_GLU_irr_level", comments=comments.L181.LC_bm2_R_C_Yh_GLU_irr_level )
writedata( L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level, domain="AGLU_LEVEL1_DATA", fn="L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level", comments=comments.L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level )
writedata( L181.ag_Prod_Mt_R_C_Y_GLU_irr_level, domain="AGLU_LEVEL1_DATA", fn="L181.ag_Prod_Mt_R_C_Y_GLU_irr_level", comments=comments.L181.ag_Prod_Mt_R_C_Y_GLU_irr_level )
writedata( L181.YieldMult_R_bio_GLU_irr, domain="AGLU_LEVEL1_DATA", fn="L181.YieldMult_R_bio_GLU_irr", comments=comments.L181.YieldMult_R_bio_GLU_irr )
writedata( L181.LandShare_R_bio_GLU_irr, domain="AGLU_LEVEL1_DATA", fn="L181.LandShare_R_bio_GLU_irr", comments=comments.L181.LandShare_R_bio_GLU_irr )

# Every script should finish with this line
logstop()
