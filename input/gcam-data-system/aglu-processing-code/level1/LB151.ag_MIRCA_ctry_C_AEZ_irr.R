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
logstart( "LB151.ag_MIRCA_ctry_C_AEZ_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Agricultural production and yield by country / AEZ / irrigation" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
FAO_ag_CROSIT <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_CROSIT" )
GTAP_ag_HA_ha <- readdata( "AGLU_LEVEL0_DATA", "GTAP_ag_HA_ha" )
GTAP_ag_Prod_t <- readdata( "AGLU_LEVEL0_DATA", "GTAP_ag_Prod_t" )
MIRCA_irrHA <- readdata( "AGLU_LEVEL0_DATA", "MIRCA_irrHA" )
MIRCA_rfdHA <- readdata( "AGLU_LEVEL0_DATA", "MIRCA_rfdHA" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Prepare GTAP and MIRCA data for computation")
# Sort the GTAP tables alphabetically to facilitate matching
L151.GTAP_ag_HA_ha <- GTAP_ag_HA_ha[ order( GTAP_ag_HA_ha$ctry, GTAP_ag_HA_ha$GTAP_crop ), ]
L151.GTAP_ag_Prod_t <- GTAP_ag_Prod_t[ order( GTAP_ag_Prod_t$ctry, GTAP_ag_Prod_t$GTAP_crop ), ]

# 2a. Preparation of MIRCA data for matching with GTAP databases
#Indicate the AEZs with the full reporting codes and specify the iso of each country
MIRCA_irrHA$AEZ <- paste( "AEZ", sprintf( "%02d", MIRCA_irrHA$AEZ ), sep = "" )
MIRCA_rfdHA$AEZ <- paste( "AEZ", sprintf( "%02d", MIRCA_rfdHA$AEZ ), sep = "" )
MIRCA_irrHA$iso <- AGLU_ctry$iso[ match( MIRCA_irrHA$Country_name, AGLU_ctry$MIRCA_country)]
MIRCA_rfdHA$iso <- AGLU_ctry$iso[ match( MIRCA_rfdHA$Country_name, AGLU_ctry$MIRCA_country)]

#Drop the missing values
MIRCA_irrHA <- na.omit( MIRCA_irrHA )
MIRCA_rfdHA <- na.omit( MIRCA_rfdHA )

# NOTE: there are several countries where a single iso was applied to multiple mirca countries.
## West Bank and Gaza Strip; Serbia and Montenegro
## At present, both Gaza Strip and Montenegro are all na's, so not bothering to do an aggregation step
## Also Western Sahara and Vatican City are not assigned ISOs in the GTAP country list, but all values are missing anyway

#Melt the dataframes, using only the relevant variables
MIRCA_IDs <- c( "iso", AEZ )
MIRCA_crops <- paste( "Crop", 1:26, sep = "" )

L151.ag_HA_ctry_Cmir_AEZ_irr <- melt( MIRCA_irrHA[ c( MIRCA_IDs, MIRCA_crops ) ],
      id.vars = MIRCA_IDs, variable.name = "MIRCA_crop", value.name = "HA_irr" )
L151.ag_HA_ctry_Cmir_AEZ_rfd <- melt( MIRCA_rfdHA[ c( MIRCA_IDs, MIRCA_crops ) ],
      id.vars = MIRCA_IDs, variable.name = "MIRCA_crop", value.name = "HA_rfd" )

#Create a vector of all possible country x aez combinations in each of these datasets
L151.ctry_AEZs <- unique( c( paste( MIRCA_irrHA$iso, MIRCA_irrHA$AEZ, sep = "|" ), paste( MIRCA_rfdHA$iso, MIRCA_rfdHA$AEZ, sep = "|" ) ) )

printlog( "Compute the share of harvested area that is irrigated by country, MIRCA crop, and AEZ")
#Build a dataframe for the computation of irrigated and rainfed HA shares, with one one row of each combination of ctry and aez that exists in the MIRCA data
L151.ag_irrshareHA_ctry_Cmir_AEZ <- data.frame(
      iso = substr( L151.ctry_AEZs, 1, 3 ),
      AEZ  = substr( L151.ctry_AEZs, 5, 9 ) )

#Repeat and add vector for MIRCA crops, and match in the quantities for computation of shares
L151.ag_irrshareHA_ctry_Cmir_AEZ <- repeat_and_add_vector( L151.ag_irrshareHA_ctry_Cmir_AEZ, "MIRCA_crop", MIRCA_crops )
L151.ag_irrshareHA_ctry_Cmir_AEZ$HA_irr <- L151.ag_HA_ctry_Cmir_AEZ_irr$HA_irr[
      match( vecpaste( L151.ag_irrshareHA_ctry_Cmir_AEZ[ c( MIRCA_IDs, "MIRCA_crop" ) ] ),
             vecpaste( L151.ag_HA_ctry_Cmir_AEZ_irr[ c( MIRCA_IDs, "MIRCA_crop" ) ] ) ) ]
L151.ag_irrshareHA_ctry_Cmir_AEZ$HA_rfd <- L151.ag_HA_ctry_Cmir_AEZ_rfd$HA_rfd[
      match( vecpaste( L151.ag_irrshareHA_ctry_Cmir_AEZ[ c( MIRCA_IDs, "MIRCA_crop" ) ] ),
             vecpaste( L151.ag_HA_ctry_Cmir_AEZ_rfd[ c( MIRCA_IDs, "MIRCA_crop" ) ] ) ) ]

#Replace NAs with 0 at this step. Calculate shares, and then replace NAs with 0 again (for non-existent combinations, assume 100% rainfed)
L151.ag_irrshareHA_ctry_Cmir_AEZ[ is.na( L151.ag_irrshareHA_ctry_Cmir_AEZ ) ] <- 0
L151.ag_irrshareHA_ctry_Cmir_AEZ$irrshareHA <- L151.ag_irrshareHA_ctry_Cmir_AEZ$HA_irr /
      ( L151.ag_irrshareHA_ctry_Cmir_AEZ$HA_irr + L151.ag_irrshareHA_ctry_Cmir_AEZ$HA_rfd )
L151.ag_irrshareHA_ctry_Cmir_AEZ[ is.na( L151.ag_irrshareHA_ctry_Cmir_AEZ ) ] <- 0

printlog( "Compute harvested area for irrigated and rainfed land, by country, GTAP crop, and AEZ")
#Match irrigated shares into GTAP table of harvested area by country, crop, and AEZ
L151.GTAP_ag_HA_ha.melt <- melt( L151.GTAP_ag_HA_ha, id.vars = c( "ctry", "GTAP_crop" ), variable.name = AEZ, value.name = "totHA" )
L151.GTAP_ag_HA_ha.melt$MIRCA_crop <- FAO_ag_items_PRODSTAT$MIRCA_crop[ match( L151.GTAP_ag_HA_ha.melt$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L151.GTAP_ag_HA_ha.melt$irrshareHA <- L151.ag_irrshareHA_ctry_Cmir_AEZ$irrshareHA[
      match( vecpaste( L151.GTAP_ag_HA_ha.melt[ c( "ctry", AEZ, "MIRCA_crop" ) ] ),
             vecpaste( L151.ag_irrshareHA_ctry_Cmir_AEZ[ c( "iso", AEZ, "MIRCA_crop" ) ] ) ) ]
#The MIRCA data only has existent AEZ x ctry combinations written out. Set missing values to 0
L151.GTAP_ag_HA_ha.melt[ is.na( L151.GTAP_ag_HA_ha.melt ) ] <- 0
L151.GTAP_ag_HA_ha.melt$irrHA <- L151.GTAP_ag_HA_ha.melt$totHA * L151.GTAP_ag_HA_ha.melt$irrshareHA

printlog( "Compute ratio of FAO irrigated area to MIRCA irrigated area by country and GTAP crop" )
#First compute the share of harvested area that is irrigated by country and FAO crop, using FAO data
L151.ag_irrshareHA_ctry_Cfao <- FAO_ag_CROSIT
L151.ag_irrshareHA_ctry_Cfao$ctry <- AGLU_ctry$iso[ match( L151.ag_irrshareHA_ctry_Cfao$country_ID, AGLU_ctry$CROSIT_country_ID ) ]
L151.ag_irrshareHA_ctry_Cfao$irrshareHA <- L151.ag_irrshareHA_ctry_Cfao$HA_kha_irrigated / L151.ag_irrshareHA_ctry_Cfao$HA_kha
L151.ag_irrshareHA_ctry_Cfao <- subset( L151.ag_irrshareHA_ctry_Cfao, year == "2005" )
L151.ag_FAOirrshareHA_ctry_CGTAP <- L151.GTAP_ag_HA_ha.melt
L151.ag_FAOirrshareHA_ctry_CGTAP$CROSIT_crop_ID <- FAO_ag_items_PRODSTAT$CROSIT_cropID[
      match( L151.ag_FAOirrshareHA_ctry_CGTAP$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L151.ag_FAOirrshareHA_ctry_CGTAP$irrshareHA <- L151.ag_irrshareHA_ctry_Cfao$irrshareHA[
      match( vecpaste( L151.ag_FAOirrshareHA_ctry_CGTAP[ c( "ctry", "CROSIT_crop_ID" )]),
             vecpaste( L151.ag_irrshareHA_ctry_Cfao[ c( "ctry", "crop_ID")]) ) ]
L151.ag_FAOirrshareHA_ctry_CGTAP$irrHA <- L151.ag_FAOirrshareHA_ctry_CGTAP$irrshareHA * L151.ag_FAOirrshareHA_ctry_CGTAP$totHA

#Next, compute the share of irrigated area in the FAO data by country and GTAP crop
L151.ag_FAOirrshareHA_ctry_CGTAP <- aggregate( L151.ag_FAOirrshareHA_ctry_CGTAP[ c( "totHA", "irrHA" ) ],
      by=as.list( L151.ag_FAOirrshareHA_ctry_CGTAP[ c( "ctry", "GTAP_crop" ) ] ), sum )
L151.ag_FAOirrshareHA_ctry_CGTAP$irrshareHA <- L151.ag_FAOirrshareHA_ctry_CGTAP$irrHA / L151.ag_FAOirrshareHA_ctry_CGTAP$totHA

#Next, compute the share of irrigated area in the MIRCA data by country and GTAP crop
L151.GTAP_ag_HA_ha_MIRCA <- aggregate( L151.GTAP_ag_HA_ha.melt[ c( "totHA", "irrHA" ) ],
      by=as.list( L151.GTAP_ag_HA_ha.melt[ c( "ctry", "GTAP_crop" ) ] ), sum )
L151.GTAP_ag_HA_ha_MIRCA$irrshareHA <- L151.GTAP_ag_HA_ha_MIRCA$irrHA / L151.GTAP_ag_HA_ha_MIRCA$totHA

#Then, compute scaler
L151.GTAP_ag_MIRCAtoFAO <- L151.GTAP_ag_HA_ha_MIRCA[ names( L151.GTAP_ag_HA_ha_MIRCA ) %in% c( "ctry", "GTAP_crop", "irrshareHA" )]
names( L151.GTAP_ag_MIRCAtoFAO )[ names( L151.GTAP_ag_MIRCAtoFAO ) == "irrshareHA" ] <- "MIRCA_share"
L151.GTAP_ag_MIRCAtoFAO$FAO_share <- L151.ag_FAOirrshareHA_ctry_CGTAP$irrshareHA[
      match( vecpaste( L151.GTAP_ag_MIRCAtoFAO[ c( "ctry", "GTAP_crop" ) ] ),
             vecpaste( L151.ag_FAOirrshareHA_ctry_CGTAP[ c( "ctry", "GTAP_crop" )]) )]
L151.GTAP_ag_MIRCAtoFAO$scaler <- L151.GTAP_ag_MIRCAtoFAO$FAO_share / L151.GTAP_ag_MIRCAtoFAO$MIRCA_share
L151.GTAP_ag_MIRCAtoFAO$scaler[ L151.GTAP_ag_MIRCAtoFAO$scaler == "Inf" ] <- 1
L151.GTAP_ag_MIRCAtoFAO$scaler[ L151.GTAP_ag_MIRCAtoFAO$scaler == "NaN" ] <- 1
L151.GTAP_ag_MIRCAtoFAO$scaler[ is.na( L151.GTAP_ag_MIRCAtoFAO$scaler ) ] <- 1

printlog( "Compute scaled harvested area for irrigated and rainfed land, by country, GTAP crop, and AEZ")
#Match scaler into GTAP data
L151.GTAP_ag_HA_ha.melt$scaler <- L151.GTAP_ag_MIRCAtoFAO$scaler[
      match( vecpaste(L151.GTAP_ag_HA_ha.melt[ c( "ctry", "GTAP_crop" ) ] ),
             vecpaste( L151.GTAP_ag_MIRCAtoFAO[ c("ctry", "GTAP_crop") ] ) ) ]
L151.GTAP_ag_HA_ha.melt$adj_irrHA <- L151.GTAP_ag_HA_ha.melt$irrHA * L151.GTAP_ag_HA_ha.melt$scaler
L151.GTAP_ag_HA_ha.melt$adj_irrHA[ L151.GTAP_ag_HA_ha.melt$adj_irrHA > L151.GTAP_ag_HA_ha.melt$totHA ] <-
      L151.GTAP_ag_HA_ha.melt$totHA[ L151.GTAP_ag_HA_ha.melt$adj_irrHA > L151.GTAP_ag_HA_ha.melt$totHA ]
L151.GTAP_ag_HA_ha.melt$adj_irrshareHA <- L151.GTAP_ag_HA_ha.melt$adj_irrHA / L151.GTAP_ag_HA_ha.melt$totHA
L151.GTAP_ag_HA_ha.melt[ is.na( L151.GTAP_ag_HA_ha.melt ) ] <- 0

#Cast by AEZ
L151.GTAP_ag_irrHA_ha <- dcast( L151.GTAP_ag_HA_ha.melt, ctry + GTAP_crop ~ AEZ, value.var = "adj_irrHA" )

#Subtract to get rainfed area
L151.GTAP_ag_rfdHA_ha <- data.frame(
      L151.GTAP_ag_HA_ha[ c( "ctry", "GTAP_crop" ) ],
      L151.GTAP_ag_HA_ha[ AEZs ] - L151.GTAP_ag_irrHA_ha[ AEZs ] )

printlog( "Calculate production on irrigated and rainfed land, by country, GTAP crop, and AEZ")
#Calculate the yield ratio for each country and crop, based on the CROSIT database
L151.ag_irr_rfd_yieldratio <- L151.GTAP_ag_HA_ha[ c( "ctry", "GTAP_crop" ) ]
L151.ag_irr_rfd_yieldratio$CROSIT_ctry <- AGLU_ctry$CROSIT_country_ID[ match( L151.ag_irr_rfd_yieldratio$ctry, AGLU_ctry$iso ) ]
L151.ag_irr_rfd_yieldratio$CROSIT_cropID <- FAO_ag_items_PRODSTAT$CROSIT_cropID[ match( L151.ag_irr_rfd_yieldratio$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
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
L151.GTAP_ag_HA_ha.melt$yieldratio <- L151.ag_irr_rfd_yieldratio$yieldratio[
      match( vecpaste( L151.GTAP_ag_HA_ha.melt[ c( "ctry", "GTAP_crop" ) ] ),
             vecpaste( L151.ag_irr_rfd_yieldratio[ c( "ctry", "GTAP_crop" ) ] ) ) ]
L151.GTAP_ag_HA_ha.melt$irrshareProd <- L151.GTAP_ag_HA_ha.melt$adj_irrshareHA * L151.GTAP_ag_HA_ha.melt$yieldratio / 
      ( ( L151.GTAP_ag_HA_ha.melt$adj_irrshareHA * L151.GTAP_ag_HA_ha.melt$yieldratio ) + ( 1 - L151.GTAP_ag_HA_ha.melt$adj_irrshareHA ) )

#Melt the production table to calculate rainfed and irrigated production
L151.GTAP_ag_Prod_t.melt <- melt( L151.GTAP_ag_Prod_t,
       id.vars = c( "ctry", "GTAP_crop" ), variable.name = "AEZ", value.name = "totProd" )
L151.GTAP_ag_Prod_t.melt$irrshareProd <- L151.GTAP_ag_HA_ha.melt$irrshareProd
L151.GTAP_ag_Prod_t.melt$irrProd <- L151.GTAP_ag_Prod_t.melt$totProd * L151.GTAP_ag_Prod_t.melt$irrshareProd

#Cast by AEZ
L151.GTAP_ag_irrProd_t <- dcast( L151.GTAP_ag_Prod_t.melt, ctry + GTAP_crop ~ AEZ, value.var = "irrProd" )

#Subtract to get rainfed production
L151.GTAP_ag_rfdProd_t <- data.frame(
      L151.GTAP_ag_Prod_t[ c( "ctry", "GTAP_crop" ) ],
      L151.GTAP_ag_Prod_t[ AEZs ] - L151.GTAP_ag_irrProd_t[ AEZs ] )

printlog( "Compute yield for irrigated and rainfed land, by country, MIRCA crop, and AEZ")
# Aggregate production and harvested area by MIRCA crop for yield calc
L151.GTAP_ag_HA_ha.melt$rfdHA <- L151.GTAP_ag_HA_ha.melt$totHA - L151.GTAP_ag_HA_ha.melt$irrHA
L151.GTAP_ag_HA_ha.melt$MIRCA_country <- AGLU_ctry$MIRCA_country[ match( L151.GTAP_ag_HA_ha.melt$ctry, AGLU_ctry$iso ) ]
L151.MIRCA_ag_HA_ha <- aggregate( L151.GTAP_ag_HA_ha.melt[ c( "irrHA", "rfdHA" ) ],
      by=as.list( L151.GTAP_ag_HA_ha.melt[ c( "MIRCA_country", "MIRCA_crop", "AEZ" ) ] ), sum )

L151.GTAP_ag_Prod_t.melt$rfdProd <- L151.GTAP_ag_Prod_t.melt$totProd - L151.GTAP_ag_Prod_t.melt$irrProd
L151.GTAP_ag_Prod_t.melt$MIRCA_country <- AGLU_ctry$MIRCA_country[ match( L151.GTAP_ag_Prod_t.melt$ctry, AGLU_ctry$iso ) ]
L151.GTAP_ag_Prod_t.melt$MIRCA_crop <- FAO_ag_items_PRODSTAT$MIRCA_crop[ match( L151.GTAP_ag_Prod_t.melt$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L151.MIRCA_ag_Prod_t <- aggregate( L151.GTAP_ag_Prod_t.melt[ c( "irrProd", "rfdProd" ) ],
      by=as.list( L151.GTAP_ag_Prod_t.melt[ c( "MIRCA_country", "MIRCA_crop", "AEZ" ) ] ), sum )
L151.MIRCA_ag_Yield_tha <- L151.MIRCA_ag_Prod_t[ c( "MIRCA_country", "MIRCA_crop", AEZ ) ]
L151.MIRCA_ag_Yield_tha[ c( "irrYield", "rfdYield" ) ] <- L151.MIRCA_ag_Prod_t[ c( "irrProd", "rfdProd" ) ] / 
      L151.MIRCA_ag_HA_ha[ match(
          vecpaste( L151.MIRCA_ag_Yield_tha[ c( "MIRCA_country", "MIRCA_crop", "AEZ" ) ] ),
          vecpaste( L151.MIRCA_ag_HA_ha[ c( "MIRCA_country", "MIRCA_crop", "AEZ" ) ] ) ),
      c( "irrHA", "rfdHA" ) ]
L151.MIRCA_ag_irrYield_tha <- dcast( L151.MIRCA_ag_Yield_tha, MIRCA_country + AEZ ~ MIRCA_crop, value.var = "irrYield" )
L151.MIRCA_ag_rfdYield_tha <- dcast( L151.MIRCA_ag_Yield_tha, MIRCA_country + AEZ ~ MIRCA_crop, value.var = "rfdYield" )
L151.MIRCA_ag_irrProd_t <- dcast( L151.MIRCA_ag_Prod_t, MIRCA_country + AEZ ~ MIRCA_crop, value.var = "irrProd" )
L151.MIRCA_ag_rfdProd_t <- dcast( L151.MIRCA_ag_Prod_t, MIRCA_country + AEZ ~ MIRCA_crop, value.var = "rfdProd" )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L151.GTAP_ag_irrHA_ha <- c( "Irrigated harvested area by country / GTAP crop / AEZ","Unit = ha" )
comments.L151.GTAP_ag_rfdHA_ha <- c( "Rainfed harvested area by country / GTAP crop / AEZ","Unit = ha" )
comments.L151.GTAP_ag_irrProd_t <- c( "Irrigated production by country / GTAP crop / AEZ","Unit = t" )
comments.L151.GTAP_ag_rfdProd_t <- c( "Rainfed production by country / GTAP crop / AEZ","Unit = t" )
comments.L151.MIRCA_ag_irrYield_tha <- c( "Irrigated yield by MIRCA country / MIRCA crop / AEZ","Unit = t/ha" )
comments.L151.MIRCA_ag_rfdYield_tha <- c( "Rainfed yield by MIRCA country / MIRCA crop / AEZ","Unit = t/ha" )
comments.L151.MIRCA_ag_irrProd_t <- c( "Irrigated production by MIRCA country / MIRCA crop / AEZ","Unit = t" )
comments.L151.MIRCA_ag_rfdProd_t <- c( "Rainfed production by MIRCA country / MIRCA crop / AEZ","Unit = t" )

#export final tables as CSV files
writedata( L151.GTAP_ag_irrHA_ha, domain="AGLU_LEVEL1_DATA",fn="L151.GTAP_ag_irrHA_ha", comments=comments.L151.GTAP_ag_irrHA_ha )
writedata( L151.GTAP_ag_rfdHA_ha, domain="AGLU_LEVEL1_DATA",fn="L151.GTAP_ag_rfdHA_ha", comments=comments.L151.GTAP_ag_rfdHA_ha )
writedata( L151.GTAP_ag_irrProd_t, domain="AGLU_LEVEL1_DATA",fn="L151.GTAP_ag_irrProd_t", comments=comments.L151.GTAP_ag_irrProd_t )
writedata( L151.GTAP_ag_rfdProd_t, domain="AGLU_LEVEL1_DATA",fn="L151.GTAP_ag_rfdProd_t", comments=comments.L151.GTAP_ag_rfdProd_t )
writedata( L151.MIRCA_ag_irrYield_tha, domain="AGLU_LEVEL1_DATA",fn="L151.MIRCA_ag_irrYield_tha", comments=comments.L151.MIRCA_ag_irrYield_tha )
writedata( L151.MIRCA_ag_rfdYield_tha, domain="AGLU_LEVEL1_DATA",fn="L151.MIRCA_ag_rfdYield_tha", comments=comments.L151.MIRCA_ag_rfdYield_tha )
writedata( L151.MIRCA_ag_irrProd_t, domain="AGLU_LEVEL1_DATA",fn="L151.MIRCA_ag_irrProd_t", comments=comments.L151.MIRCA_ag_irrProd_t )
writedata( L151.MIRCA_ag_rfdProd_t, domain="AGLU_LEVEL1_DATA",fn="L151.MIRCA_ag_rfdProd_t", comments=comments.L151.MIRCA_ag_rfdProd_t )

# Every script should finish with this line
logstop()
