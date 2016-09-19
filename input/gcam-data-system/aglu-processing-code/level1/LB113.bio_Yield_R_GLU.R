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
logstart( "LB113.bio_Yield_R_GLU.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Biomass yield by region / zone, base year" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
L100.LDS_ag_HA_ha <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_HA_ha" )
L100.LDS_ag_prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_prod_t" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Calculating global average yields for each FAO crop in the base year" )
L113.ag_HA_ha_glbl_crop <- aggregate( L100.LDS_ag_HA_ha[ "value" ],
                                      by = L100.LDS_ag_HA_ha[ "GTAP_crop" ], sum )
L113.ag_prod_t_glbl_crop <- aggregate( L100.LDS_ag_prod_t[ "value" ],
                                      by = L100.LDS_ag_prod_t[ "GTAP_crop" ], sum )
L113.ag_prod_t_glbl_crop$HA_ha <- L113.ag_HA_ha_glbl_crop$value[
  match( L113.ag_prod_t_glbl_crop$GTAP_crop, L113.ag_HA_ha_glbl_crop$GTAP_crop ) ]
L113.ag_prod_t_glbl_crop$Yield_avg <- L113.ag_prod_t_glbl_crop$value / L113.ag_prod_t_glbl_crop$HA_ha

printlog( "Calculating each region / zone / crop's comparative yield, to the global average" )
LDS_ag_Yield_tha <- L100.LDS_ag_HA_ha
names( LDS_ag_Yield_tha )[ names( LDS_ag_Yield_tha ) == "value" ] <- "HA"
LDS_ag_Yield_tha$prod <- L100.LDS_ag_prod_t$value[
  match( vecpaste( L100.LDS_ag_HA_ha[ c( "iso", GLU, "GTAP_crop" ) ] ),
         vecpaste( L100.LDS_ag_prod_t[ c( "iso", GLU, "GTAP_crop" ) ] ) ) ]
LDS_ag_Yield_tha$Yield <- LDS_ag_Yield_tha$prod / LDS_ag_Yield_tha$HA

#Drop the missing values, where the harvested area was above the min threshold but production was not
LDS_ag_Yield_tha <- na.omit (LDS_ag_Yield_tha )

#Match in the global avg yield for each crop, and compute the ratio from that yield
LDS_ag_Yield_tha$average <- L113.ag_prod_t_glbl_crop$Yield_avg[
  match( LDS_ag_Yield_tha$GTAP_crop, L113.ag_prod_t_glbl_crop$GTAP_crop ) ]
LDS_ag_Yield_tha$Ratio <- LDS_ag_Yield_tha$Yield / LDS_ag_Yield_tha$average
LDS_ag_Yield_tha$Ratio_weight <- LDS_ag_Yield_tha$Ratio * LDS_ag_Yield_tha$HA
LDS_ag_Yield_tha[[R]] <- iso_GCAM_regID[[R]][ match( LDS_ag_Yield_tha$iso, iso_GCAM_regID$iso ) ]

L113.YieldIndex_R_GLU <- aggregate( LDS_ag_Yield_tha[ c( "HA", "Ratio_weight" ) ],
                                  by = LDS_ag_Yield_tha[ R_GLU ], sum )
L113.YieldIndex_R_GLU$YieldIndex <- L113.YieldIndex_R_GLU$Ratio_weight / L113.YieldIndex_R_GLU$HA

#Bioenergy yields are equal to this region/zone-specific index multiplied by a base yield
L113.base_bio_yield_tha <- Max_bio_yield_tha / max( L113.YieldIndex_R_GLU$YieldIndex )
L113.base_bio_yield_GJm2 <- L113.base_bio_yield_tha * bio_GJt / conv_Ha_m2
L113.ag_bioYield_GJm2_R_GLU <- data.frame(
  L113.YieldIndex_R_GLU[ R_GLU ],
  Yield_GJm2 = L113.YieldIndex_R_GLU$YieldIndex * L113.base_bio_yield_GJm2 )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L113.ag_bioYield_GJm2_R_GLU <- c( "Base year bioenergy yields by GCAM region / GLU","Unit = GJ.m2" )

writedata( L113.ag_bioYield_GJm2_R_GLU, domain="AGLU_LEVEL1_DATA", fn="L113.ag_bioYield_GJm2_R_GLU", comments=comments.L113.ag_bioYield_GJm2_R_GLU )

# Every script should finish with this line
logstop()
