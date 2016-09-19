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
logstart( "LA102.ag_LDS_R_C_GLU.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Primary agricultural good data from LDS, assigned to GCAM region / commodity / GLU" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
L100.LDS_ag_HA_ha <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_HA_ha" )
L100.LDS_ag_prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_prod_t" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Adding GCAM region and commodity info to LDS tables" )
L100.LDS_ag_HA_ha[[R]] <- iso_GCAM_regID[[R]][ match( L100.LDS_ag_HA_ha$iso, iso_GCAM_regID$iso ) ]
L100.LDS_ag_prod_t[[R]] <- iso_GCAM_regID[[R]][ match( L100.LDS_ag_prod_t$iso, iso_GCAM_regID$iso ) ]

L100.LDS_ag_HA_ha[[C]] <- FAO_ag_items_PRODSTAT[[C]][ match( L100.LDS_ag_HA_ha$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L100.LDS_ag_prod_t[[C]] <- FAO_ag_items_PRODSTAT[[C]][ match( L100.LDS_ag_prod_t$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

printlog( "Collapsing production and harvested area data to GCAM regions and commodities")
L102.ag_HA_Ha_R_C_GLU <- aggregate( L100.LDS_ag_HA_ha[ "value" ], by = L100.LDS_ag_HA_ha[ R_C_GLU ], sum )
L102.ag_Prod_t_R_C_GLU <- aggregate( L100.LDS_ag_prod_t[ "value" ], by = L100.LDS_ag_prod_t[ R_C_GLU ], sum )

#convert to desired units (Mt and bm2)
printlog( "Converting GTAP mass to Mt and area to thousand km2 (billion m2, or bm2)")
L102.ag_HA_bm2_R_C_GLU <- L102.ag_HA_Ha_R_C_GLU
L102.ag_HA_bm2_R_C_GLU[["value"]] <- L102.ag_HA_Ha_R_C_GLU[["value"]] * conv_Ha_bm2

L102.ag_Prod_Mt_R_C_GLU <- L102.ag_Prod_t_R_C_GLU
L102.ag_Prod_Mt_R_C_GLU[["value"]] <- L102.ag_Prod_t_R_C_GLU[["value"]] * conv_t_Mt

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L102.ag_HA_bm2_R_C_GLU <- c( "Harvested area by GCAM region / commodity / GLU","Unit = bm2" )
comments.L102.ag_Prod_Mt_R_C_GLU <- c( "Crop production by GCAM region / commodity / GLU","Unit = Mt" )

#export final tables as CSV files
writedata( L102.ag_HA_bm2_R_C_GLU, domain="AGLU_LEVEL1_DATA",fn="L102.ag_HA_bm2_R_C_GLU", comments=comments.L102.ag_HA_bm2_R_C_GLU )
writedata( L102.ag_Prod_Mt_R_C_GLU, domain="AGLU_LEVEL1_DATA",fn="L102.ag_Prod_Mt_R_C_GLU", comments=comments.L102.ag_Prod_Mt_R_C_GLU )

# Every script should finish with this line
logstop()
