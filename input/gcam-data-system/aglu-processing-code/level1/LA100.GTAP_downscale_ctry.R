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
logstart( "LA100.GTAP_downscale_ctry.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Downscaling of GTAP economic output by region, commodity, and GLU to the country level" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
L100.LDS_value_milUSD <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_value_milUSD" )
L100.LDS_ag_prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_prod_t" )

# -----------------------------------------------------------------------------
# 2. Perform computations
GTAP_uses <- sort( unique( FAO_ag_items_PRODSTAT$GTAP_use ) )
all_iso_GLU <- unique( L100.LDS_ag_prod_t[ c( "iso", GLU ) ] )

#Downscale GTAP region-level data to all countries
L100.GTAP_LV_milUSD <- repeat_and_add_vector( all_iso_GLU, "GTAP_use", GTAP_uses)
L100.GTAP_LV_milUSD$GTAP_region <- AGLU_ctry$GTAP_region[ match( L100.GTAP_LV_milUSD$iso, AGLU_ctry$iso ) ]

#Match in the land value for the entire GTAP region. These will be multiplied by country shares
L100.GTAP_LV_milUSD$value <- L100.LDS_value_milUSD$value[ 
     match( vecpaste( L100.GTAP_LV_milUSD[ c( "GTAP_region", GLU, "GTAP_use" ) ] ),
            vecpaste( L100.LDS_value_milUSD[ c( "GTAP_region", GLU, "GTAP_use" ) ] ) ) ]
L100.GTAP_LV_milUSD$value[ is.na( L100.GTAP_LV_milUSD$value ) ] <- 0

#Compute the country-within-GTAP region shares for each of the commodity classes
L100.LDS_ag_prod_t$GTAP_use <- FAO_ag_items_PRODSTAT$GTAP_use[ match( L100.LDS_ag_prod_t$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L100.ag_Prod_t_ctry_Cgtap_GLU <- aggregate( L100.LDS_ag_prod_t[ "value" ], by=as.list( L100.LDS_ag_prod_t[ c( "iso", GLU, "GTAP_use" ) ] ), sum )
L100.ag_Prod_t_ctry_Cgtap_GLU$GTAP_region <- AGLU_ctry$GTAP_region[ match( L100.ag_Prod_t_ctry_Cgtap_GLU$iso, AGLU_ctry$iso ) ]
L100.ag_Prod_t_Rgtap_Cgtap_GLU <- aggregate( L100.ag_Prod_t_ctry_Cgtap_GLU[ "value" ],
      by=as.list( L100.ag_Prod_t_ctry_Cgtap_GLU[ c( "GTAP_region", GLU, "GTAP_use" ) ] ), sum )

#Share = production by country and GTAP use / production by GTAP region and GTAP use
L100.ag_Share_ctry_Cgtap_GLU <- L100.ag_Prod_t_ctry_Cgtap_GLU
L100.ag_Share_ctry_Cgtap_GLU$share <- L100.ag_Prod_t_ctry_Cgtap_GLU$value / L100.ag_Prod_t_Rgtap_Cgtap_GLU$value[
      match( vecpaste( L100.ag_Prod_t_ctry_Cgtap_GLU[ c( "GTAP_region", GLU, "GTAP_use" ) ] ),
             vecpaste( L100.ag_Prod_t_Rgtap_Cgtap_GLU[ c( "GTAP_region", GLU, "GTAP_use" ) ] ) ) ]
L100.ag_Share_ctry_Cgtap_GLU[ is.na( L100.ag_Share_ctry_Cgtap_GLU ) ] <- 0

#Multiply the land values by the shares
L100.GTAP_LV_milUSD$value <- L100.GTAP_LV_milUSD$value * L100.ag_Share_ctry_Cgtap_GLU$share[
      match( vecpaste( L100.GTAP_LV_milUSD[ c( "iso", GLU, "GTAP_use" ) ] ),
             vecpaste( L100.ag_Share_ctry_Cgtap_GLU[ c( "iso", GLU, "GTAP_use" ) ] ) ) ]
L100.GTAP_LV_milUSD$value[ is.na( L100.GTAP_LV_milUSD$value ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L100.GTAP_LV_milUSD <- c( "Land value by country / GLU / GTAP commodity class","Million US Dollars" )

#write tables as CSV files
writedata( L100.GTAP_LV_milUSD, domain="AGLU_LEVEL1_DATA", fn="L100.GTAP_LV_milUSD", comments=comments.L100.GTAP_LV_milUSD )

# Every script should finish with this line
logstop()
