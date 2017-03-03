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
logstart( "LB163.bio_Yield_R_GLU_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Biomass yield by region / GLU, base year" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
L100.LDS_ag_HA_ha <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_HA_ha" )
L100.LDS_ag_prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_prod_t" )
L151.ag_irrHA_ha_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_irrHA_ha_ctry_crop" )
L151.ag_irrProd_t_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_irrProd_t_ctry_crop" )
L151.ag_rfdHA_ha_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_rfdHA_ha_ctry_crop" )
L151.ag_rfdProd_t_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_rfdProd_t_ctry_crop" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# This method follows the same method as LB113, with the exception that the yield indices are computed separately for rainfed/irrigated,
# but again against the global average for each crop, across both irrigated and rainfed. This method should roughly preserve the global
# average bioenergy yields; what we want to avoid here is increasing the global average yields just by separating irr/rfd
printlog( "Calculating global average yields for each FAO crop in the base year" )
L163.ag_HA_ha_glbl_crop <- aggregate( L100.LDS_ag_HA_ha[ "value" ],
                                      by = L100.LDS_ag_HA_ha[ "GTAP_crop" ], sum )
L163.ag_prod_t_glbl_crop <- aggregate( L100.LDS_ag_prod_t[ "value" ],
                                       by = L100.LDS_ag_prod_t[ "GTAP_crop" ], sum )
L163.ag_prod_t_glbl_crop$HA_ha <- L163.ag_HA_ha_glbl_crop$value[
  match( L163.ag_prod_t_glbl_crop$GTAP_crop, L163.ag_HA_ha_glbl_crop$GTAP_crop ) ]
L163.ag_prod_t_glbl_crop$Yield_avg <- L163.ag_prod_t_glbl_crop$value / L163.ag_prod_t_glbl_crop$HA_ha

printlog( "Calculating each region / zone / crop / irr's comparative yield, to the global average" )
# First, add a column indicating irr/rfd, and re-name the value columns for combining the data frames
L151.ag_irrHA_ha_ctry_crop[[irr]] <- "IRR"
names( L151.ag_irrHA_ha_ctry_crop ) <- sub( "irrHA", "HA", names( L151.ag_irrHA_ha_ctry_crop ) )
L151.ag_irrProd_t_ctry_crop[[irr]] <- "IRR"
names( L151.ag_irrProd_t_ctry_crop ) <- sub( "irrProd", "Prod", names( L151.ag_irrProd_t_ctry_crop ) )
L151.ag_rfdHA_ha_ctry_crop[[irr]] <- "RFD"
names( L151.ag_rfdHA_ha_ctry_crop ) <- sub( "rfdHA", "HA", names( L151.ag_rfdHA_ha_ctry_crop ) )
L151.ag_rfdProd_t_ctry_crop[[irr]] <- "RFD"
names( L151.ag_rfdProd_t_ctry_crop ) <- sub( "rfdProd", "Prod", names( L151.ag_rfdProd_t_ctry_crop ) )

L163.ag_HA_ha_ctry_crop_irr <- rbind( L151.ag_irrHA_ha_ctry_crop, L151.ag_rfdHA_ha_ctry_crop )
L163.ag_Prod_t_ctry_crop_irr <- rbind( L151.ag_irrProd_t_ctry_crop, L151.ag_rfdProd_t_ctry_crop )

#Then build the yield dataframe
L163.ag_Yield_tha_ctry_crop_irr <- L163.ag_HA_ha_ctry_crop_irr
L163.ag_Yield_tha_ctry_crop_irr$Prod <- L163.ag_Prod_t_ctry_crop_irr$Prod[
  match( vecpaste( L163.ag_Yield_tha_ctry_crop_irr[ c( "iso", GLU, "GTAP_crop", irr ) ] ),
         vecpaste( L163.ag_Prod_t_ctry_crop_irr[ c( "iso", GLU, "GTAP_crop", irr ) ] ) ) ]
L163.ag_Yield_tha_ctry_crop_irr$Yield <- with( L163.ag_Yield_tha_ctry_crop_irr, Prod / HA )

#Drop the missing values, where HA values were below the reporting threshold
L163.ag_Yield_tha_ctry_crop_irr <- na.omit (L163.ag_Yield_tha_ctry_crop_irr )

#Match in the global avg yield for each crop, and compute the ratio from that yield
L163.ag_Yield_tha_ctry_crop_irr$average <- L163.ag_prod_t_glbl_crop$Yield_avg[
  match( L163.ag_Yield_tha_ctry_crop_irr$GTAP_crop, L163.ag_prod_t_glbl_crop$GTAP_crop ) ]
L163.ag_Yield_tha_ctry_crop_irr$Ratio <- with( L163.ag_Yield_tha_ctry_crop_irr, Yield / average )
L163.ag_Yield_tha_ctry_crop_irr$Ratio_weight <- with( L163.ag_Yield_tha_ctry_crop_irr, Ratio * HA )
L163.ag_Yield_tha_ctry_crop_irr[[R]] <- iso_GCAM_regID[[R]][ match( L163.ag_Yield_tha_ctry_crop_irr$iso, iso_GCAM_regID$iso ) ]

L163.YieldIndex_R_GLU_irr <- aggregate( L163.ag_Yield_tha_ctry_crop_irr[ c( "HA", "Ratio_weight" ) ],
                                    by = L163.ag_Yield_tha_ctry_crop_irr[ R_GLU_irr ], sum )
L163.YieldIndex_R_GLU_irr$YieldIndex <- with( L163.YieldIndex_R_GLU_irr, Ratio_weight / HA )

#Bioenergy yields are equal to this region/zone-specific index multiplied by a base yield
# The base yield is taken to be the maximum of the yields in the USA region, or the region containing the USA,
# because the Wullschleger paper from which the yield estimate was derived was for the USA.
USAreg <- iso_GCAM_regID[[R]][ iso_GCAM_regID$iso == "usa" ][1]
L163.base_bio_yield_tha <- Max_bio_yield_tha / max( L163.YieldIndex_R_GLU_irr$YieldIndex[
  L163.YieldIndex_R_GLU_irr[[R]] == USAreg ] )
L163.base_bio_yield_GJm2 <- L163.base_bio_yield_tha * bio_GJt / conv_Ha_m2
L163.ag_bioYield_GJm2_R_GLU_irr <- data.frame(
  L163.YieldIndex_R_GLU_irr[ R_GLU_irr ],
  Yield_GJm2 = L163.YieldIndex_R_GLU_irr$YieldIndex * L163.base_bio_yield_GJm2 )

# Split rainfed and irrigated into separate tables for the write-out (to be consistent with other files)
L163.ag_irrBioYield_GJm2_R_GLU <- L163.ag_bioYield_GJm2_R_GLU_irr[ L163.ag_bioYield_GJm2_R_GLU_irr[[irr]] == "IRR", c( R_GLU, "Yield_GJm2") ]
L163.ag_rfdBioYield_GJm2_R_GLU <- L163.ag_bioYield_GJm2_R_GLU_irr[ L163.ag_bioYield_GJm2_R_GLU_irr[[irr]] == "RFD", c( R_GLU, "Yield_GJm2") ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L163.ag_irrBioYield_GJm2_R_GLU <- c( "Reference base year bioenergy yields for irrigated crops by GCAM region / GLU","Unit = GJ.m2" )
comments.L163.ag_rfdBioYield_GJm2_R_GLU <- c( "Reference base year bioenergy yields for rainfed crops by GCAM region / GLU","Unit = GJ.m2" )

writedata( L163.ag_irrBioYield_GJm2_R_GLU, domain="AGLU_LEVEL1_DATA", fn="L163.ag_irrBioYield_GJm2_R_GLU", comments=comments.L163.ag_irrBioYield_GJm2_R_GLU )
writedata( L163.ag_rfdBioYield_GJm2_R_GLU, domain="AGLU_LEVEL1_DATA", fn="L163.ag_rfdBioYield_GJm2_R_GLU", comments=comments.L163.ag_rfdBioYield_GJm2_R_GLU )

# Every script should finish with this line
logstop()
