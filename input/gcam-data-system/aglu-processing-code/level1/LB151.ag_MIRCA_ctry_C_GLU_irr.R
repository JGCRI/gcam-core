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
logstart( "LB151.ag_MIRCA_ctry_C_GLU_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Agricultural production and yield by country / GLU / irrigation" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
FAO_ag_CROSIT <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_CROSIT" )
L100.LDS_ag_HA_ha <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_HA_ha" )
L100.LDS_ag_prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.LDS_ag_prod_t" )
L100.MIRCA_irrHA_ha <- readdata( "AGLU_LEVEL1_DATA", "L100.MIRCA_irrHA_ha" )
L100.MIRCA_rfdHA_ha <- readdata( "AGLU_LEVEL1_DATA", "L100.MIRCA_rfdHA_ha" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Adding crop string to the MIRCA crop identifiers")
L100.MIRCA_irrHA_ha$MIRCA_crop <- paste0( "Crop", L100.MIRCA_irrHA_ha$MIRCA_crop )
L100.MIRCA_rfdHA_ha$MIRCA_crop <- paste0( "Crop", L100.MIRCA_rfdHA_ha$MIRCA_crop )

printlog( "Compute the share of harvested area that is irrigated by country, MIRCA crop, and GLU")
# First, merge the datasets so that all combinations of ctry/GLU/crop in either database are included
MIRCA_IDs <- c( "iso", GLU, "MIRCA_crop" )
names( L100.MIRCA_irrHA_ha )[ names( L100.MIRCA_irrHA_ha ) == "value" ] <- "HA_irr"
names( L100.MIRCA_rfdHA_ha )[ names( L100.MIRCA_rfdHA_ha ) == "value" ] <- "HA_rfd"
L151.ag_irrshareHA_ctry_Cmir_GLU <- merge( L100.MIRCA_irrHA_ha, L100.MIRCA_rfdHA_ha, all = T )

#Replace missing values with 0 (zeroes are not written out by LDS), and compute the shares
L151.ag_irrshareHA_ctry_Cmir_GLU[ is.na( L151.ag_irrshareHA_ctry_Cmir_GLU ) ] <- 0
L151.ag_irrshareHA_ctry_Cmir_GLU$irrshareHA <- with( L151.ag_irrshareHA_ctry_Cmir_GLU, HA_irr / ( HA_irr + HA_rfd ) )

printlog( "Compute harvested area for irrigated and rainfed land, by country, GTAP crop, and GLU")
#Match irrigated shares into GTAP table of harvested area by country, crop, and GLU
L151.ag_HA_ha_ctry_crop <- L100.LDS_ag_HA_ha
names( L151.ag_HA_ha_ctry_crop )[ names( L151.ag_HA_ha_ctry_crop ) == "value" ] <- "totHA"
L151.ag_HA_ha_ctry_crop$MIRCA_crop <- FAO_ag_items_PRODSTAT$MIRCA_crop[ 
      match( L151.ag_HA_ha_ctry_crop$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L151.ag_HA_ha_ctry_crop$irrshareHA <- L151.ag_irrshareHA_ctry_Cmir_GLU$irrshareHA[
      match( vecpaste( L151.ag_HA_ha_ctry_crop[ MIRCA_IDs ] ),
             vecpaste( L151.ag_irrshareHA_ctry_Cmir_GLU[ MIRCA_IDs ] ) ) ]

# At this point, missing values indicate crop x ag-region instances in the Monfreda-based data but not in the MIRCA aggregation.
# While we could just assume that all production is rainfed in such places, there's no way to guarantee that this is actually
# possible. Instead, as a first-order approximation, calculate a default share for each country and basin that is represented in MIRCA
L151.ag_irrshareHA_ctry_GLU <- aggregate( L151.ag_irrshareHA_ctry_Cmir_GLU[ c( "HA_irr", "HA_rfd" ) ],
                                          by = L151.ag_irrshareHA_ctry_Cmir_GLU[ c( "iso", GLU ) ], sum )
L151.ag_irrshareHA_ctry_GLU$irrshareHA <- with( L151.ag_irrshareHA_ctry_GLU, HA_irr / ( HA_irr + HA_rfd ) )

L151.ag_HA_ha_ctry_crop$irrshareHA[ is.na( L151.ag_HA_ha_ctry_crop$irrshareHA ) ] <- L151.ag_irrshareHA_ctry_GLU$irrshareHA[
  match( vecpaste( L151.ag_HA_ha_ctry_crop[ is.na( L151.ag_HA_ha_ctry_crop$irrshareHA ), c( "iso", GLU ) ] ),
         vecpaste( L151.ag_irrshareHA_ctry_GLU[ c( "iso", GLU ) ] ) ) ]

#Any remaining missing values can be set to 0. Current version (3/21/16) doesn't have any.
L151.ag_HA_ha_ctry_crop[ is.na( L151.ag_HA_ha_ctry_crop ) ] <- 0
L151.ag_HA_ha_ctry_crop$irrHA <- with( L151.ag_HA_ha_ctry_crop, totHA * irrshareHA )

# GPK 3/21/16 - removed a series of steps for scaling MIRCA's harvested area quantities to FAO, related to
# an initial aggregation of MIRCA that has since been corrected.
#Prepare for the write-out
L151.ag_irrHA_ha_ctry_crop <- L151.ag_HA_ha_ctry_crop[ c( "iso", "GTAP_crop", GLU, "irrHA" ) ]

#Subtract to get rainfed area
L151.ag_HA_ha_ctry_crop$rfdHA <- with( L151.ag_HA_ha_ctry_crop, totHA - irrHA )
L151.ag_rfdHA_ha_ctry_crop <- L151.ag_HA_ha_ctry_crop[ c( "iso", "GTAP_crop", GLU, "rfdHA" ) ]

printlog( "Calculate production on irrigated and rainfed land, by country, GTAP crop, and GLU")
#Calculate the yield ratio for each country and crop, based on the CROSIT database
L151.ag_irr_rfd_yieldratio <- L100.LDS_ag_HA_ha[ c( "iso", "GTAP_crop", GLU ) ]
L151.ag_irr_rfd_yieldratio$CROSIT_ctry <- AGLU_ctry$CROSIT_country_ID[ match( L151.ag_irr_rfd_yieldratio$iso, AGLU_ctry$iso ) ]
L151.ag_irr_rfd_yieldratio$CROSIT_cropID <- FAO_ag_items_PRODSTAT$CROSIT_cropID[
      match( L151.ag_irr_rfd_yieldratio$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L151.ag_irr_rfd_yieldratio$Yirr <- FAO_ag_CROSIT$Yield_kgHa_irrigated[
      match( vecpaste( L151.ag_irr_rfd_yieldratio[ c( "CROSIT_ctry", "CROSIT_cropID" ) ] ),
             vecpaste( FAO_ag_CROSIT[ c( "country_ID", "crop_ID" ) ] ) ) ]
L151.ag_irr_rfd_yieldratio$Yrfd <- FAO_ag_CROSIT$Yield_kgHa_rainfed[
      match( vecpaste( L151.ag_irr_rfd_yieldratio[ c( "CROSIT_ctry", "CROSIT_cropID" ) ] ),
             vecpaste( FAO_ag_CROSIT[ c( "country_ID", "crop_ID" ) ] ) ) ]
L151.ag_irr_rfd_yieldratio$yieldratio <- L151.ag_irr_rfd_yieldratio$Yirr / L151.ag_irr_rfd_yieldratio$Yrfd

#Reset NA values to 1 (default is no difference assumed between rainfed and irrigated yields)
L151.ag_irr_rfd_yieldratio$yieldratio[ is.na( L151.ag_irr_rfd_yieldratio$yieldratio ) ] <- 1
L151.ag_irr_rfd_yieldratio$yieldratio[ L151.ag_irr_rfd_yieldratio$yieldratio == Inf ] <- 1
L151.ag_irr_rfd_yieldratio$yieldratio[ L151.ag_irr_rfd_yieldratio$yieldratio == 0 ] <- 1

#Use the yield ratio to solve for the production shares (irrigated versus rainfed)
L151.ag_HA_ha_ctry_crop$yieldratio <- L151.ag_irr_rfd_yieldratio$yieldratio[
      match( vecpaste( L151.ag_HA_ha_ctry_crop[ c( "iso", "GTAP_crop" ) ] ),
             vecpaste( L151.ag_irr_rfd_yieldratio[ c( "iso", "GTAP_crop" ) ] ) ) ]
L151.ag_HA_ha_ctry_crop$irrshareProd <- with( L151.ag_HA_ha_ctry_crop,
                                              ( irrshareHA * yieldratio ) / 
                                              ( ( irrshareHA * yieldratio ) + ( 1 - irrshareHA ) ) )

#Multiply through to calculate rainfed and irrigated production
L151.ag_Prod_t_ctry_crop <- L100.LDS_ag_prod_t
names( L151.ag_Prod_t_ctry_crop )[ names( L151.ag_Prod_t_ctry_crop ) == "value" ] <- "totProd"
L151.ag_Prod_t_ctry_crop$irrshareProd <- L151.ag_HA_ha_ctry_crop$irrshareProd[
  match( vecpaste( L151.ag_Prod_t_ctry_crop[ c( "iso", GLU, "GTAP_crop" ) ] ),
         vecpaste( L151.ag_HA_ha_ctry_crop[ c( "iso", GLU, "GTAP_crop" ) ] ) ) ]
L151.ag_Prod_t_ctry_crop$irrProd <- with( L151.ag_Prod_t_ctry_crop, totProd * irrshareProd )

#Subtract to get rainfed production
L151.ag_Prod_t_ctry_crop$rfdProd <- with( L151.ag_Prod_t_ctry_crop, totProd - irrProd )

#Prepare for the final write-out
L151.ag_irrProd_t_ctry_crop <- L151.ag_Prod_t_ctry_crop[ c( "iso", "GTAP_crop", GLU, "irrProd" ) ]
L151.ag_rfdProd_t_ctry_crop <- L151.ag_Prod_t_ctry_crop[ c( "iso", "GTAP_crop", GLU, "rfdProd" ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L151.ag_irrHA_ha_ctry_crop <- c( "Irrigated harvested area by country / GTAP crop / GLU","Unit = ha" )
comments.L151.ag_rfdHA_ha_ctry_crop <- c( "Rainfed harvested area by country / GTAP crop / GLU","Unit = ha" )
comments.L151.ag_irrProd_t_ctry_crop <- c( "Irrigated production by country / GTAP crop / GLU","Unit = t" )
comments.L151.ag_rfdProd_t_ctry_crop <- c( "Rainfed production by country / GTAP crop / GLU","Unit = t" )

#export final tables as CSV files
writedata( L151.ag_irrHA_ha_ctry_crop, domain="AGLU_LEVEL1_DATA",fn="L151.ag_irrHA_ha_ctry_crop", comments=comments.L151.ag_irrHA_ha_ctry_crop )
writedata( L151.ag_rfdHA_ha_ctry_crop, domain="AGLU_LEVEL1_DATA",fn="L151.ag_rfdHA_ha_ctry_crop", comments=comments.L151.ag_rfdHA_ha_ctry_crop )
writedata( L151.ag_irrProd_t_ctry_crop, domain="AGLU_LEVEL1_DATA",fn="L151.ag_irrProd_t_ctry_crop", comments=comments.L151.ag_irrProd_t_ctry_crop )
writedata( L151.ag_rfdProd_t_ctry_crop, domain="AGLU_LEVEL1_DATA",fn="L151.ag_rfdProd_t_ctry_crop", comments=comments.L151.ag_rfdProd_t_ctry_crop )

# Every script should finish with this line
logstop()
