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
logstart( "L111.ag_resbio_R_C.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Residue biomass parameters for primary agricultural goods, by GCAM region / commodity" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
L100.FAO_ag_Prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_ag_Prod_t" )
Various_ag_resbio_data <- readdata( "AGLU_LEVEL0_DATA", "Various_ag_resbio_data" )

# -----------------------------------------------------------------------------
# 2. Perform computations

#Compute weighted averages of each parameter ( HarvestIndex, ErosionControl, and ResidueEnergyContent ) for each crop type in each GCAM region
printlog( "Calculating production-weighted averages of residue biomass parameters" )
printlog( "NOTE: Using 2005 production data as weighting factor" )
L111.ag_resbio_Prod <- L100.FAO_ag_Prod_t[ c( "iso", "item", "X2005" ) ]
resbio_params <- names( Various_ag_resbio_data )[ !names( Various_ag_resbio_data ) %in% names( L100.FAO_ag_Prod_t ) ]
L111.ag_resbio_Prod[ resbio_params ] <- Various_ag_resbio_data[ match( L111.ag_resbio_Prod$item, Various_ag_resbio_data$item ), resbio_params ]

#Drop rows with NA values ( dropping commodities that are not in the resbio dataset )
L111.ag_resbio_Prod <- na.omit( L111.ag_resbio_Prod )

#Multiply by production to get weights
printlog( "Multiplying residue biomass parameters by production" )
L111.ag_resbio_Prod[ resbio_params ] <- L111.ag_resbio_Prod$X2005 * L111.ag_resbio_Prod[ resbio_params ]

#Add vectors for GCAM regions and commodities, collapse, and divide by production to get residue biomass values
printlog( "Aggregating by GCAM region and commodity" )
L111.ag_resbio_Prod$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L111.ag_resbio_Prod$iso, iso_GCAM_regID$iso ) ]
L111.ag_resbio_Prod$GCAM_commodity <- FAO_ag_items_PRODSTAT$GCAM_commodity[ match( L111.ag_resbio_Prod$item, FAO_ag_items_PRODSTAT$item ) ]
L111.ag_resbio_Prod_R_C <- aggregate( L111.ag_resbio_Prod[ c( "X2005", resbio_params ) ], by=as.list( L111.ag_resbio_Prod[ R_C ] ), sum )

printlog( "Dividing by production to get weighted average residue biomass parameters by region and crop" )
L111.ag_resbio_R_C <- L111.ag_resbio_Prod_R_C[ R_C ]
L111.ag_resbio_R_C[ resbio_params ] <- L111.ag_resbio_Prod_R_C[ resbio_params ] / L111.ag_resbio_Prod_R_C$X2005
L111.ag_resbio_R_C[ is.na( L111.ag_resbio_R_C ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L111.ag_resbio_R_C <- c( "Weighted average residue biomass parameters by GCAM region / commodity","Units: varied" )

#write tables as CSV files
writedata( L111.ag_resbio_R_C, domain="AGLU_LEVEL1_DATA",fn="L111.ag_resbio_R_C", comments=comments.L111.ag_resbio_R_C )

# Every script should finish with this line
logstop()
