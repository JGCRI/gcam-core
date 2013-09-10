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
logstart( "L100.GTAP_downscale_ctry.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Downscaling of GTAP economic output by region, commodity, and AEZ to the country level" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
GTAP_value_milUSD <- readdata( "AGLU_LEVEL0_DATA", "GTAP_value_milUSD" )
GTAP_ag_Prod_t <- readdata( "AGLU_LEVEL0_DATA", "GTAP_ag_Prod_t" )

# -----------------------------------------------------------------------------
# 2. Perform computations
GTAP_uses <- sort( unique( FAO_ag_items_PRODSTAT$GTAP_use ) )
all_iso <- sort( unique( GTAP_ag_Prod_t$ctry ) )

#Downscale GTAP region-level data to all countries
L100.GTAP_LV_milUSD <- data.frame(
      iso = rep( all_iso, times = length( GTAP_uses ) ),
      GTAP_use = sort( rep( GTAP_uses, times = length( all_iso ) ) ) )
L100.GTAP_LV_milUSD$GTAP_region <- AGLU_ctry$GTAP_region[ match( L100.GTAP_LV_milUSD$iso, AGLU_ctry$iso ) ]

#Match in the land value for the entire GTAP region. These will be multiplied by country shares
L100.GTAP_LV_milUSD[ AEZs ] <- GTAP_value_milUSD[ 
     match( vecpaste( L100.GTAP_LV_milUSD[ c( "GTAP_region", "GTAP_use" ) ] ),
            vecpaste( GTAP_value_milUSD[ c( "ctry87", "GTAP_use" ) ] ) ),
     AEZs ]

#Compute the country-within-GTAP region shares for each of the commodity classes
GTAP_ag_Prod_t$GTAP_use <- FAO_ag_items_PRODSTAT$GTAP_use[ match( GTAP_ag_Prod_t$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L100.ag_Prod_t_ctry_Cgtap_AEZ <- aggregate( GTAP_ag_Prod_t[ AEZs ], by=as.list( GTAP_ag_Prod_t[ c( "ctry", "GTAP_use" ) ] ), sum )
L100.ag_Prod_t_ctry_Cgtap_AEZ$GTAP_region <- AGLU_ctry$GTAP_region[ match( L100.ag_Prod_t_ctry_Cgtap_AEZ$ctry, AGLU_ctry$iso ) ]
L100.ag_Prod_t_Rgtap_Cgtap_AEZ <- aggregate( L100.ag_Prod_t_ctry_Cgtap_AEZ[ AEZs ],
      by=as.list( L100.ag_Prod_t_ctry_Cgtap_AEZ[ c( "GTAP_region", "GTAP_use" ) ] ), sum )

#Share = production by country and GTAP use / production by GTAP region and GTAP use
L100.ag_Share_ctry_Cgtap_AEZ <- L100.ag_Prod_t_ctry_Cgtap_AEZ
L100.ag_Share_ctry_Cgtap_AEZ[ AEZs ] <- L100.ag_Prod_t_ctry_Cgtap_AEZ[ AEZs ] / L100.ag_Prod_t_Rgtap_Cgtap_AEZ[
      match( vecpaste( L100.ag_Prod_t_ctry_Cgtap_AEZ[ c( "GTAP_region", "GTAP_use" ) ] ),
             vecpaste( L100.ag_Prod_t_Rgtap_Cgtap_AEZ[ c( "GTAP_region", "GTAP_use" ) ] ) ),
      AEZs ]
L100.ag_Share_ctry_Cgtap_AEZ[ is.na( L100.ag_Share_ctry_Cgtap_AEZ ) ] <- 0

#Multiply the land values by the shares
L100.GTAP_LV_milUSD[ AEZs ] <- L100.GTAP_LV_milUSD[ AEZs ] * L100.ag_Share_ctry_Cgtap_AEZ[
      match( vecpaste( L100.GTAP_LV_milUSD[ c( "iso", "GTAP_use" ) ] ),
             vecpaste( L100.ag_Share_ctry_Cgtap_AEZ[ c( "ctry", "GTAP_use" ) ] ) ),
      AEZs ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L100.GTAP_LV_milUSD <- c( "Land value by country / GTAP commodity class","Million US Dollars" )

#write tables as CSV files
writedata( L100.GTAP_LV_milUSD, domain="AGLU_LEVEL1_DATA", fn="L100.GTAP_LV_milUSD", comments=comments.L100.GTAP_LV_milUSD )

# Every script should finish with this line
logstop()
