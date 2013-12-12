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
logstart( "LB115.ag_CCI_ISIMIP.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Climate change impacts on yields from ISIMIP project" )

# -------------
# 0. Pre-processing to return the filenames of files to import. Do this in a command prompt (not R)
#Set the capital letters to lowercase and specify which climate model was used and which crop model was used
# Make sure to update the climate and crop models in the step below prior to performing it. can only do one set at at time
#ls *.csv | awk '{print("mv "$1" "$1)}' | sed 's/original_RCP/noresm_lpjml_rcp/2' | /bin/sh

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
GIS_ctry_AEZ <- readdata( "AGLU_MAPPINGS", "GIS_ctry_AEZ" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
GTAP_ag_HA_ha <- readdata( "AGLU_LEVEL0_DATA", "GTAP_ag_HA_ha" )

# 1b. reading ISIMIP files in a loop
ISIMIPfilepath <- readdomainpathmap()["AGLU_ISIMIP_DATA"][[1]]
ISIMIPfiles.list <- list.files( ISIMIPfilepath )
ISIMIPfiles.list <- sub( ".csv", "", ISIMIPfiles.list )
ISIMIPfiles <- list()
for( i in ISIMIPfiles.list ){
  index <- which( ISIMIPfiles.list == i )
  ISIMIPfiles[[ index ]] <- readdata( "AGLU_ISIMIP_DATA", i)
}
names( ISIMIPfiles ) <- ISIMIPfiles.list
ISIMIP_data <- do.call( rbind, ISIMIPfiles )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Years for which crop model output is available
ISIMIP_years <- 1971:2100
X_ISIMIP_years <- paste0( "X", ISIMIP_years )

#Add 2100 to the ISIMIP data
ISIMIP_data$X2100 <- ISIMIP_data$X2099

#Make some user-configurable assumptions here
#Upper limit on positive climate impacts (multiplier)
max_CCImult <- 2.5
# Lower limit on negative climate impacts (multiplier)
min_CCImult <- 0.1
# Floor on area weights, in hectares. Below this climate impacts will be ignored. These are more likely than others to be problematic
weight_floor_ha <- 1
# Set the number of years to define the range for rolling averages. range = 2 times this plus 1
rolling_avg_years <- 4
#Indicate the years for which to apply rolling averages
X_ISIMIP_future_years <- paste0( "X", ISIMIP_years[ ISIMIP_years > max( historical_years ) ] )
X_ISIMIP_base_years <- paste0( "X", ISIMIP_years[ ISIMIP_years <= max( historical_years ) ] )
X_CCI_years <- c( "base", X_future_years )

#Create the ID vectors from the rownames of the isimip data
printlog( "Generating ID vectors for emissions scenario, climate model, crop model, crop, and irrigation from file names of isimip data" )
printlog( "NOTE: assuming naming convention is gcm_cropmodel_scenario_irrigation_crop" )
ISIMIP_data$rcp <- substr( row.names( ISIMIP_data ), regexpr("rcp",row.names(ISIMIP_data)), regexpr("rcp",row.names(ISIMIP_data)) + 4 )
ISIMIP_data$gcm <- substr( row.names( ISIMIP_data ), 1, regexpr("_",row.names(ISIMIP_data)) - 1 )
ISIMIP_data$cropmodel <- substr( row.names( ISIMIP_data ), regexpr("_",row.names(ISIMIP_data)) + 1, regexpr("rcp",row.names(ISIMIP_data)) - 2 )
ISIMIP_data$crop <- substr( row.names( ISIMIP_data ), regexpr("irr",row.names(ISIMIP_data)) + 4, regexpr("irr",row.names(ISIMIP_data)) + 6 ) 
ISIMIP_data$irr <- substr( row.names( ISIMIP_data ), regexpr("irr",row.names(ISIMIP_data)) - 2, regexpr("irr",row.names(ISIMIP_data)) + 2 )
names( ISIMIP_data )[ names( ISIMIP_data ) == "area_ha" ] <- "weight"
row.names( ISIMIP_data ) <- 1:nrow( ISIMIP_data )

#Drop inland water and any rows where the area weight is less than the exogenous floor
L115.Yield_tha_ISIMIP_all <- subset( ISIMIP_data, ID != 0 & weight > weight_floor_ha )

#Create data frames with the appropriate averages
# For the base years, calculate the average
# For the future years, calculate rolling averages
L115.Yield_tha_ISIMIP_avg <- L115.Yield_tha_ISIMIP_all
L115.Yield_tha_ISIMIP_avg$base <- rowMeans( L115.Yield_tha_ISIMIP_avg[ X_ISIMIP_base_years ] )
for( i in 1:length( X_ISIMIP_future_years ) ){
	left <- max( 1, i - rolling_avg_years )
	right <- min( length( X_ISIMIP_future_years ), i + rolling_avg_years )
	L115.Yield_tha_ISIMIP_avg[ X_ISIMIP_future_years ][i] <- rowMeans( L115.Yield_tha_ISIMIP_all[ X_ISIMIP_future_years ][ left:right ] )
}

#Dropping irrelevant columns
L115.Yield_tha_ISIMIP_avg <- L115.Yield_tha_ISIMIP_avg[ c( rcp_gcm_cm, "ID", "crop", "irr", "weight", X_CCI_years ) ]

printlog( "Multiplying base areas by yields to get the production" )
L115.Prod_t_ISIMIP_avg <- L115.Yield_tha_ISIMIP_avg
L115.Prod_t_ISIMIP_avg[ X_CCI_years ] <- L115.Yield_tha_ISIMIP_avg[ X_CCI_years ] * L115.Yield_tha_ISIMIP_avg$weight

printlog( "Aggregating production and weights in order to drop irrigation status" )
L115.Prod_t_rcp_gcm_cm_ctry_Cisi_Y <- aggregate( L115.Prod_t_ISIMIP_avg[ c( X_CCI_years, "weight" ) ],
      by=as.list( L115.Prod_t_ISIMIP_avg[ rcp_gcm_cm_aezid_crop ] ), sum )

L115.Yield_tha_rcp_gcm_cm_ctry_Cisi_Y <- data.frame(
      L115.Prod_t_rcp_gcm_cm_ctry_Cisi_Y[ rcp_gcm_cm_aezid_crop ],
      L115.Prod_t_rcp_gcm_cm_ctry_Cisi_Y[ X_CCI_years ] / L115.Prod_t_rcp_gcm_cm_ctry_Cisi_Y$weight,
      L115.Prod_t_rcp_gcm_cm_ctry_Cisi_Y[ "weight" ] )

L115.CCI_rcp_gcm_cm_R_Cisi_Y <- L115.Yield_tha_rcp_gcm_cm_ctry_Cisi_Y[ c( rcp_gcm_cm_aezid_crop, "weight" ) ]
L115.CCI_rcp_gcm_cm_R_Cisi_Y[ X_CCI_years ] <- L115.Yield_tha_rcp_gcm_cm_ctry_Cisi_Y[ X_CCI_years ] /
      L115.Yield_tha_rcp_gcm_cm_ctry_Cisi_Y$base

#2b. Calculate the C3 average in ISIMIP and append this to the table with ISIMIP crops differentiated
printlog( "Calculating average C3 crop impacts to be applied to C3 crops not included in the ISIMIP set")
printlog( "NOTE: Using area-weighted impacts to avoid bias of computing averages from crops with different base yields" )
# NOTE: This step works for LPJmL's crop list. not sure how to modify for crop models with different crop lists
L115.CCI_rcp_gcm_cm_R_C3avg_Y <- subset( L115.CCI_rcp_gcm_cm_R_Cisi_Y, crop %in%
      FAO_ag_items_PRODSTAT$LPJmL_crop[ FAO_ag_items_PRODSTAT$C3avg_include == 1 ] )

#Aggregate crops
L115.wtCCI_rcp_gcm_cm_R_C3avg_Y <- L115.CCI_rcp_gcm_cm_R_C3avg_Y
L115.wtCCI_rcp_gcm_cm_R_C3avg_Y[ X_CCI_years ] <- L115.CCI_rcp_gcm_cm_R_C3avg_Y[ X_CCI_years ] * L115.CCI_rcp_gcm_cm_R_C3avg_Y$weight
L115.CCI_rcp_gcm_cm_R_C3avg_Y <- aggregate( L115.wtCCI_rcp_gcm_cm_R_C3avg_Y[ c( X_CCI_years, "weight" ) ],
      by=as.list( L115.wtCCI_rcp_gcm_cm_R_C3avg_Y[ rcp_gcm_cm_aezid ] ), sum )
L115.CCI_rcp_gcm_cm_R_C3avg_Y[ X_CCI_years ] <- L115.CCI_rcp_gcm_cm_R_C3avg_Y[ X_CCI_years ] / L115.CCI_rcp_gcm_cm_R_C3avg_Y$weight
L115.CCI_rcp_gcm_cm_R_C3avg_Y$crop <- "C3avg"

#Append C3 average to the table of impacts
L115.CCI_rcp_gcm_cm_R_Cisi_Y <- rbind( L115.CCI_rcp_gcm_cm_R_Cisi_Y, L115.CCI_rcp_gcm_cm_R_C3avg_Y )

#Match in the iso and AEZ for the region ID vector
L115.CCI_rcp_gcm_cm_R_Cisi_Y[ c( "iso", AEZ ) ] <- GIS_ctry_AEZ[ match( L115.CCI_rcp_gcm_cm_R_Cisi_Y$ID, GIS_ctry_AEZ$AEZ_ID ), c( "iso", AEZ ) ]

#2c. Use the base-year GTAP database to downscale from crops in the crop models to the full set
printlog( "Matching yield impacts from ISIMIP into the GTAP database of production")
L115.GTAP_ag_HA_ha <- GTAP_ag_HA_ha
L115.GTAP_ag_HA_ha$iso <- L115.GTAP_ag_HA_ha$ctry
L115.GTAP_ag_HA_ha$LPJmL_crop <- FAO_ag_items_PRODSTAT$LPJmL_crop[
      match( L115.GTAP_ag_HA_ha$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L115.GTAP_ag_HA_ha <- na.omit( L115.GTAP_ag_HA_ha )
L115.GTAP_ag_HA_ha.melt <- melt( L115.GTAP_ag_HA_ha, variable_name = AEZ, measure.vars = AEZs )

# Drop the zeroes. This will make the next steps much easier
L115.GTAP_ag_HA_ha.melt <- subset( L115.GTAP_ag_HA_ha.melt, value != 0 )

#Repeat by number of scenarios being considered for climate impacts. Match in the timestep
L115.all_rcp_gcm_cm <- unique( L115.CCI_rcp_gcm_cm_R_Cisi_Y[ rcp_gcm_cm ] )
L115.all_rcp_gcm_cm$scenID <- 1:nrow( L115.all_rcp_gcm_cm )
L115.GTAP_ag_HA_ha_rcp_gcm_cm <- repeat_and_add_vector( L115.GTAP_ag_HA_ha.melt, "scenID", L115.all_rcp_gcm_cm$scenID )
L115.GTAP_ag_HA_ha_rcp_gcm_cm[ rcp_gcm_cm ] <- L115.all_rcp_gcm_cm[
      match( L115.GTAP_ag_HA_ha_rcp_gcm_cm$scenID, L115.all_rcp_gcm_cm$scenID ), rcp_gcm_cm]

#Match in the climate impacts for each row, in a separate column.
L115.GTAP_ag_HA_ha_rcp_gcm_cm[ X_CCI_years ] <- L115.CCI_rcp_gcm_cm_R_Cisi_Y[
      match( vecpaste( L115.GTAP_ag_HA_ha_rcp_gcm_cm[ c( rcp_gcm_cm, "iso", AEZ, "LPJmL_crop" ) ] ),
             vecpaste( L115.CCI_rcp_gcm_cm_R_Cisi_Y[ c( rcp_gcm_cm, "iso", AEZ, "crop" ) ] ) ),
      X_CCI_years ]

#Drop missing values (where ISIMIP's base year weight was 0 but GTAP had production)
L115.GTAP_ag_HA_ha_rcp_gcm_cm <- na.omit( L115.GTAP_ag_HA_ha_rcp_gcm_cm )

#Multiply the yield delta by the base year weight
printlog( "Aggregating yield impacts by GCAM regions and commodities")
L115.GTAP_ag_wtCCI_rcp_gcm_cm <- L115.GTAP_ag_HA_ha_rcp_gcm_cm
L115.GTAP_ag_wtCCI_rcp_gcm_cm[ X_CCI_years ] <- L115.GTAP_ag_HA_ha_rcp_gcm_cm[ X_CCI_years ] * L115.GTAP_ag_HA_ha_rcp_gcm_cm$value

#Match GCAM regions and commodities into the tables of weighted impacts and the base-year weights
L115.GTAP_ag_wtCCI_rcp_gcm_cm[[R]]<- iso_GCAM_regID[[R]][ match( L115.GTAP_ag_wtCCI_rcp_gcm_cm$iso, iso_GCAM_regID$iso ) ]
L115.GTAP_ag_wtCCI_rcp_gcm_cm[[C]] <- FAO_ag_items_PRODSTAT[[C]][
      match( L115.GTAP_ag_wtCCI_rcp_gcm_cm$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

#Aggregate weighted impacts and base year weights by GCAM region and commodity
L115.ag_wtCCI_rcp_gcm_cm_R_C_AEZ <- aggregate( L115.GTAP_ag_wtCCI_rcp_gcm_cm[ X_CCI_years ],
      by=as.list( L115.GTAP_ag_wtCCI_rcp_gcm_cm[ c( rcp_gcm_cm, R_C_AEZ ) ] ), sum )

#Divide weighted impacts by the base year weights to get weighted average impacts by GCAM regions and commodities
L115.ag_CCI_rcp_gcm_cm_R_C_AEZ <- L115.ag_wtCCI_rcp_gcm_cm_R_C_AEZ
L115.ag_CCI_rcp_gcm_cm_R_C_AEZ[ X_CCI_years ] <- L115.ag_wtCCI_rcp_gcm_cm_R_C_AEZ[ X_CCI_years ] / L115.ag_wtCCI_rcp_gcm_cm_R_C_AEZ$base
L115.ag_CCI_rcp_gcm_cm_R_C_AEZ[ X_CCI_years ][ L115.ag_CCI_rcp_gcm_cm_R_C_AEZ[ X_CCI_years ] > max_CCImult ] <- max_CCImult
L115.ag_CCI_rcp_gcm_cm_R_C_AEZ[ X_CCI_years ][ L115.ag_CCI_rcp_gcm_cm_R_C_AEZ[ X_CCI_years ] < min_CCImult ] <- min_CCImult

#Bioenergy climate change impacts: use the median of the other crops
L115.bio_CCI_rcp_gcm_cm_R_AEZ <- aggregate( L115.ag_CCI_rcp_gcm_cm_R_C_AEZ[ X_CCI_years ],
      by=as.list( L115.ag_CCI_rcp_gcm_cm_R_C_AEZ[ c( rcp_gcm_cm, R_AEZ ) ] ), median )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L115.ag_CCI_rcp_gcm_cm_R_C_AEZ <- c( "Climate impacts on agricultural productivity by RCP / GCM / crop model / region / commodity / year / AEZ",
      "Unitless multiplier" )
comments.L115.bio_CCI_rcp_gcm_cm_R_AEZ <- c( "Climate impacts on bioenergy productivity by RCP / GCM / crop model / region / year / AEZ",
      "Unitless multiplier" )

#write tables as CSV files
writedata( L115.ag_CCI_rcp_gcm_cm_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L115.ag_CCI_rcp_gcm_cm_R_C_AEZ", comments=comments.L115.ag_CCI_rcp_gcm_cm_R_C_AEZ )
writedata( L115.bio_CCI_rcp_gcm_cm_R_AEZ, domain="AGLU_LEVEL1_DATA",fn="L115.bio_CCI_rcp_gcm_cm_R_AEZ", comments=comments.L115.bio_CCI_rcp_gcm_cm_R_AEZ )

# Every script should finish with this line
logstop()
