# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu data system. Please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "LA102.ag_GTAP_R_C_AEZ.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Primary agricultural good data from GTAP, assigned to GCAM region / commodity / AEZ" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
GTAP_ag_HA_ha <- readdata( "AGLU_LEVEL0_DATA", "GTAP_ag_HA_ha" )
GTAP_ag_Prod_t <- readdata( "AGLU_LEVEL0_DATA", "GTAP_ag_Prod_t" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#add lookup vectors to each of the tables
printlog( "Adding region and crop lookup vectors to GTAP tables" )
GTAP_ag_HA_ha$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( GTAP_ag_HA_ha$ctry, iso_GCAM_regID$iso ) ]
GTAP_ag_Prod_t$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( GTAP_ag_Prod_t$ctry, iso_GCAM_regID$iso ) ]

GTAP_ag_HA_ha$GCAM_commodity <- FAO_ag_items_PRODSTAT$GCAM_commodity[ match( GTAP_ag_HA_ha$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
GTAP_ag_Prod_t$GCAM_commodity <- FAO_ag_items_PRODSTAT$GCAM_commodity[ match( GTAP_ag_Prod_t$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

#build tables collapsed by GCAM regions and crop names
printlog( "Collapsing GTAP ag commodity data into GCAM regions and commodities")
L102.ag_HA_Ha_R_C_AEZ <- aggregate( GTAP_ag_HA_ha[ AEZs ], by=as.list( GTAP_ag_HA_ha[ R_C ] ), sum )
L102.ag_Prod_t_R_C_AEZ <- aggregate( GTAP_ag_Prod_t[ AEZs ], by=as.list( GTAP_ag_Prod_t[ R_C ] ), sum )

#convert to desired units (Mt and bm2)
printlog( "Converting GTAP mass to Mt and area to thousand km2 (billion m2, or bm2)")
L102.ag_HA_bm2_R_C_AEZ <- L102.ag_HA_Ha_R_C_AEZ
L102.ag_HA_bm2_R_C_AEZ[ AEZs ] <- L102.ag_HA_Ha_R_C_AEZ[ AEZs ] * conv_Ha_bm2

L102.ag_Prod_Mt_R_C_AEZ <- L102.ag_Prod_t_R_C_AEZ
L102.ag_Prod_Mt_R_C_AEZ[ AEZs ] <- L102.ag_Prod_t_R_C_AEZ[ AEZs ] * conv_t_Mt

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L102.ag_HA_bm2_R_C_AEZ <- c( "Harvested area by GCAM region / commodity / AEZ","Unit = bm2" )
comments.L102.ag_Prod_Mt_R_C_AEZ <- c( "Crop production by GCAM region / commodity / AEZ","Unit = Mt" )

#export final tables as CSV files
writedata( L102.ag_HA_bm2_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L102.ag_HA_bm2_R_C_AEZ", comments=comments.L102.ag_HA_bm2_R_C_AEZ )
writedata( L102.ag_Prod_Mt_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L102.ag_Prod_Mt_R_C_AEZ", comments=comments.L102.ag_Prod_Mt_R_C_AEZ )

# Every script should finish with this line
logstop()
