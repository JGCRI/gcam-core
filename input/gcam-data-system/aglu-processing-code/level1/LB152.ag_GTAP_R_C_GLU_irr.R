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
logstart( "LB152.ag_GTAP_R_C_GLU_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Agricultural good data from LDS/GTAP, split into irrigated and rainfed from MIRCA, assigned to GCAM region / commodity / GLU" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
L151.ag_irrHA_ha_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_irrHA_ha_ctry_crop" )
L151.ag_rfdHA_ha_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_rfdHA_ha_ctry_crop" )
L151.ag_irrProd_t_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_irrProd_t_ctry_crop" )
L151.ag_rfdProd_t_ctry_crop <- readdata( "AGLU_LEVEL1_DATA", "L151.ag_rfdProd_t_ctry_crop" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#add lookup vectors to each of the tables
printlog( "Adding region and crop lookup vectors to GTAP tables" )
with( iso_GCAM_regID, {
	L151.ag_irrHA_ha_ctry_crop[[R]] <<- GCAM_region_ID[ match ( L151.ag_irrHA_ha_ctry_crop$iso, iso ) ]
	L151.ag_rfdHA_ha_ctry_crop[[R]] <<- GCAM_region_ID[ match ( L151.ag_rfdHA_ha_ctry_crop$iso, iso ) ]
	L151.ag_irrProd_t_ctry_crop[[R]] <<- GCAM_region_ID[ match( L151.ag_irrProd_t_ctry_crop$iso, iso ) ]
	L151.ag_rfdProd_t_ctry_crop[[R]] <<- GCAM_region_ID[ match( L151.ag_rfdProd_t_ctry_crop$iso, iso ) ]
} )

with( FAO_ag_items_PRODSTAT, {
	L151.ag_irrHA_ha_ctry_crop[[C]] <<- GCAM_commodity[ match ( L151.ag_irrHA_ha_ctry_crop$GTAP_crop, GTAP_crop ) ]
	L151.ag_rfdHA_ha_ctry_crop[[C]] <<- GCAM_commodity[ match ( L151.ag_rfdHA_ha_ctry_crop$GTAP_crop, GTAP_crop ) ]
	L151.ag_irrProd_t_ctry_crop[[C]] <<- GCAM_commodity[ match( L151.ag_irrProd_t_ctry_crop$GTAP_crop, GTAP_crop ) ]
	L151.ag_rfdProd_t_ctry_crop[[C]] <<- GCAM_commodity[ match( L151.ag_rfdProd_t_ctry_crop$GTAP_crop, GTAP_crop ) ]
} )

#build tables collapsed by GCAM regions and crop names
printlog( "Collapsing ag commodity data into GCAM regions and commodities, and converting to appropriate units (bm2 and Mt)" )
L152.ag_irrHA_bm2_R_C_GLU <- aggregate( L151.ag_irrHA_ha_ctry_crop[ "irrHA" ] * conv_Ha_bm2,
                                       by = L151.ag_irrHA_ha_ctry_crop[ R_C_GLU ], sum )
L152.ag_rfdHA_bm2_R_C_GLU <- aggregate( L151.ag_rfdHA_ha_ctry_crop[ "rfdHA" ] * conv_Ha_bm2,
                                       by = L151.ag_rfdHA_ha_ctry_crop[ R_C_GLU ], sum )

L152.ag_irrProd_Mt_R_C_GLU <- aggregate( L151.ag_irrProd_t_ctry_crop[ "irrProd" ] * conv_t_Mt,
                                        by = L151.ag_irrProd_t_ctry_crop[ R_C_GLU ], sum )
L152.ag_rfdProd_Mt_R_C_GLU <- aggregate( L151.ag_rfdProd_t_ctry_crop[ "rfdProd" ] * conv_t_Mt,
                                        by = L151.ag_rfdProd_t_ctry_crop[ R_C_GLU ], sum )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L152.ag_irrHA_bm2_R_C_GLU <- c( "Irrigated harvested area by GCAM region / commodity / GLU","Unit = bm2" )
comments.L152.ag_rfdHA_bm2_R_C_GLU <- c( "Rainfed harvested area by GCAM region / commodity / GLU","Unit = bm2" )
comments.L152.ag_irrProd_Mt_R_C_GLU <- c( "Irrigated crop production by GCAM region / commodity / GLU","Unit = Mt" )
comments.L152.ag_rfdProd_Mt_R_C_GLU <- c( "Rainfed crop production by GCAM region / commodity / GLU","Unit = Mt" )

#export final tables as CSV files
writedata( L152.ag_irrHA_bm2_R_C_GLU, domain="AGLU_LEVEL1_DATA",fn="L152.ag_irrHA_bm2_R_C_GLU", comments=comments.L152.ag_irrHA_bm2_R_C_GLU )
writedata( L152.ag_rfdHA_bm2_R_C_GLU, domain="AGLU_LEVEL1_DATA",fn="L152.ag_rfdHA_bm2_R_C_GLU", comments=comments.L152.ag_rfdHA_bm2_R_C_GLU )
writedata( L152.ag_irrProd_Mt_R_C_GLU, domain="AGLU_LEVEL1_DATA",fn="L152.ag_irrProd_Mt_R_C_GLU", comments=comments.L152.ag_irrProd_Mt_R_C_GLU )
writedata( L152.ag_rfdProd_Mt_R_C_GLU, domain="AGLU_LEVEL1_DATA",fn="L152.ag_rfdProd_Mt_R_C_GLU", comments=comments.L152.ag_rfdProd_Mt_R_C_GLU )

# Every script should finish with this line
logstop()
