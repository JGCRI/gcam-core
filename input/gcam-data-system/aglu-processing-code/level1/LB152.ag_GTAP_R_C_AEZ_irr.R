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
logstart( "LB152.ag_GTAP_R_C_AEZ_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Agricultural good data from GTAP, split into irrigated and rainfed from MIRCA, assigned to GCAM region / commodity / AEZ" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
L151.GTAP_ag_irrHA_ha <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_irrHA_ha" )
L151.GTAP_ag_rfdHA_ha <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_rfdHA_ha" )
L151.GTAP_ag_irrProd_t <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_irrProd_t" )
L151.GTAP_ag_rfdProd_t <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_rfdProd_t" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#add lookup vectors to each of the tables
printlog( "Adding region and crop lookup vectors to GTAP tables" )
with( iso_GCAM_regID, {
	L151.GTAP_ag_irrHA_ha$GCAM_region_ID <<- GCAM_region_ID[ match ( L151.GTAP_ag_irrHA_ha$ctry, iso ) ]
	L151.GTAP_ag_rfdHA_ha$GCAM_region_ID <<- GCAM_region_ID[ match ( L151.GTAP_ag_rfdHA_ha$ctry, iso ) ]
	L151.GTAP_ag_irrProd_t$GCAM_region_ID <<- GCAM_region_ID[ match( L151.GTAP_ag_irrProd_t$ctry, iso ) ]
	L151.GTAP_ag_rfdProd_t$GCAM_region_ID <<- GCAM_region_ID[ match( L151.GTAP_ag_rfdProd_t$ctry, iso ) ]
} )

with( FAO_ag_items_PRODSTAT, {
	L151.GTAP_ag_irrHA_ha$GCAM_commodity <<- GCAM_commodity[ match ( L151.GTAP_ag_irrHA_ha$GTAP_crop, GTAP_crop ) ]
	L151.GTAP_ag_rfdHA_ha$GCAM_commodity <<- GCAM_commodity[ match ( L151.GTAP_ag_rfdHA_ha$GTAP_crop, GTAP_crop ) ]
	L151.GTAP_ag_irrProd_t$GCAM_commodity <<- GCAM_commodity[ match( L151.GTAP_ag_irrProd_t$GTAP_crop, GTAP_crop ) ]
	L151.GTAP_ag_rfdProd_t$GCAM_commodity <<- GCAM_commodity[ match( L151.GTAP_ag_rfdProd_t$GTAP_crop, GTAP_crop ) ]
} )

#build tables collapsed by GCAM regions and crop names
printlog( "Collapsing GTAP ag commodity data into GCAM regions and commodities")
L152.ag_irrHA_Ha_R_C_AEZ <- aggregate( L151.GTAP_ag_irrHA_ha[ AEZs ], by=as.list( L151.GTAP_ag_irrHA_ha[ R_C ] ), sum )
L152.ag_rfdHA_Ha_R_C_AEZ <- aggregate( L151.GTAP_ag_rfdHA_ha[ AEZs ], by=as.list( L151.GTAP_ag_rfdHA_ha[ R_C ] ), sum )

L152.ag_irrProd_t_R_C_AEZ <- aggregate( L151.GTAP_ag_irrProd_t[ AEZs ], by=as.list( L151.GTAP_ag_irrProd_t[ R_C ] ), sum )
L152.ag_rfdProd_t_R_C_AEZ <- aggregate( L151.GTAP_ag_rfdProd_t[ AEZs ], by=as.list( L151.GTAP_ag_rfdProd_t[ R_C ] ), sum )

#convert to desired units (Mt and bm2)
printlog( "Converting mass to Mt and area to thousand km2 (billion m2, or bm2)")
L152.ag_irrHA_bm2_R_C_AEZ <- L152.ag_irrHA_Ha_R_C_AEZ
L152.ag_irrHA_bm2_R_C_AEZ[ AEZs ] <- L152.ag_irrHA_Ha_R_C_AEZ[ AEZs ] * conv_Ha_bm2
L152.ag_rfdHA_bm2_R_C_AEZ <- L152.ag_rfdHA_Ha_R_C_AEZ
L152.ag_rfdHA_bm2_R_C_AEZ[ AEZs ] <- L152.ag_rfdHA_Ha_R_C_AEZ[ AEZs ] * conv_Ha_bm2

L152.ag_irrProd_Mt_R_C_AEZ <- L152.ag_irrProd_t_R_C_AEZ
L152.ag_irrProd_Mt_R_C_AEZ[ AEZs ] <- L152.ag_irrProd_t_R_C_AEZ[ AEZs ] * conv_t_Mt
L152.ag_rfdProd_Mt_R_C_AEZ <- L152.ag_rfdProd_t_R_C_AEZ
L152.ag_rfdProd_Mt_R_C_AEZ[ AEZs ] <- L152.ag_rfdProd_t_R_C_AEZ[ AEZs ] * conv_t_Mt

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L152.ag_irrHA_bm2_R_C_AEZ <- c( "Irrigated harvested area by GCAM region / commodity / AEZ","Unit = bm2" )
comments.L152.ag_rfdHA_bm2_R_C_AEZ <- c( "Rainfed harvested area by GCAM region / commodity / AEZ","Unit = bm2" )
comments.L152.ag_irrProd_Mt_R_C_AEZ <- c( "Irrigated crop production by GCAM region / commodity / AEZ","Unit = Mt" )
comments.L152.ag_rfdProd_Mt_R_C_AEZ <- c( "Rainfed crop production by GCAM region / commodity / AEZ","Unit = Mt" )

#export final tables as CSV files
writedata( L152.ag_irrHA_bm2_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L152.ag_irrHA_bm2_R_C_AEZ", comments=comments.L152.ag_irrHA_bm2_R_C_AEZ )
writedata( L152.ag_rfdHA_bm2_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L152.ag_rfdHA_bm2_R_C_AEZ", comments=comments.L152.ag_rfdHA_bm2_R_C_AEZ )
writedata( L152.ag_irrProd_Mt_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L152.ag_irrProd_Mt_R_C_AEZ", comments=comments.L152.ag_irrProd_Mt_R_C_AEZ )
writedata( L152.ag_rfdProd_Mt_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L152.ag_rfdProd_Mt_R_C_AEZ", comments=comments.L152.ag_rfdProd_Mt_R_C_AEZ )

# Every script should finish with this line
logstop()
