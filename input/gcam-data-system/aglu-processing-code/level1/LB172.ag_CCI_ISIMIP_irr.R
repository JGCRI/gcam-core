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
logstart( "LB172.ag_CCI_ISIMIP_irr.R" )
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
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
GIS_ctry_AEZ <- readdata( "AGLU_MAPPINGS", "GIS_ctry_AEZ" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
L151.GTAP_ag_irrHA_ha <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_irrHA_ha" )
L151.GTAP_ag_rfdHA_ha <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_rfdHA_ha" )

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

#For uncertainty study in the USA, add some extra rows with yields increased or decreased in the future years
#uncertainty_bound <- 0.2
#USA_AEZIDs <- 15107:15116
#ISIMIP_data_USA <- subset( ISIMIP_data, ID %in% USA_AEZIDs & cropmodel == "gepic" )
#ISIMIP_data_USA_hi <- ISIMIP_data_USA
#ISIMIP_data_USA_hi[ X_ISIMIP_future_years ] <- ISIMIP_data_USA[ X_ISIMIP_future_years ] * ( 1 + uncertainty_bound )
#ISIMIP_data_USA_hi$cropmodel <- "gepicUSAhi"
#ISIMIP_data_USA_lo <- ISIMIP_data_USA
#ISIMIP_data_USA_lo[ X_ISIMIP_future_years ] <- ISIMIP_data_USA[ X_ISIMIP_future_years ] * ( 1 - uncertainty_bound )
#ISIMIP_data_USA_lo$cropmodel <- "gepicUSAlo"

# Include this with the full dataset
#ISIMIP_data <- rbind( ISIMIP_data, ISIMIP_data_USA_hi, ISIMIP_data_USA_lo )

#Drop inland water and any rows where the area weight is less than the exogenous floor
L172.Yield_tha_ISIMIP_all <- subset( ISIMIP_data, ID != 0 & weight > weight_floor_ha )

#Create data frames with the appropriate averages
# For the base years, calculate the average
# For the future years, calculate rolling averages
L172.Yield_tha_ISIMIP_avg <- L172.Yield_tha_ISIMIP_all
L172.Yield_tha_ISIMIP_avg$base <- rowMeans( L172.Yield_tha_ISIMIP_avg[ X_ISIMIP_base_years ] )
for( i in 1:length( X_ISIMIP_future_years ) ){
	left <- max( 1, i - rolling_avg_years )
	right <- min( length( X_ISIMIP_future_years ), i + rolling_avg_years )
	L172.Yield_tha_ISIMIP_avg[ X_ISIMIP_future_years ][i] <- rowMeans( L172.Yield_tha_ISIMIP_all[ X_ISIMIP_future_years ][ left:right ] )
}

#Dropping irrelevant columns and calculating climate impacts from the base year
L172.Yield_tha_ISIMIP_avg <- L172.Yield_tha_ISIMIP_avg[ c( rcp_gcm_cm, "ID", "crop", "irr", "weight", X_CCI_years ) ]

L172.CCI_rcp_gcm_cm_R_Cisi_Y <- L172.Yield_tha_ISIMIP_avg[ c( rcp_gcm_cm_aezid_crop_irr, "weight" ) ]
L172.CCI_rcp_gcm_cm_R_Cisi_Y[ X_CCI_years ] <- L172.Yield_tha_ISIMIP_avg[ X_CCI_years ] /
      L172.Yield_tha_ISIMIP_avg$base

#2b. Calculate the C3 average in ISIMIP and append this to the table with ISIMIP crops differentiated
printlog( "Calculating average C3 crop impacts to be applied to C3 crops not included in the ISIMIP set")
printlog( "NOTE: Using area-weighted impacts to avoid bias of computing averages from crops with different base yields" )
# NOTE: This step works for LPJmL's crop list. not sure how to handle other crop models with smaller numbers of crops
L172.CCI_rcp_gcm_cm_R_C3avg_Y <- subset( L172.CCI_rcp_gcm_cm_R_Cisi_Y, crop %in%
      FAO_ag_items_PRODSTAT$LPJmL_crop[ FAO_ag_items_PRODSTAT$C3avg_include == 1 ] )

#Aggregate crops
L172.wtCCI_rcp_gcm_cm_R_C3avg_Y <- L172.CCI_rcp_gcm_cm_R_C3avg_Y
L172.wtCCI_rcp_gcm_cm_R_C3avg_Y[ X_CCI_years ] <- L172.CCI_rcp_gcm_cm_R_C3avg_Y[ X_CCI_years ] * L172.CCI_rcp_gcm_cm_R_C3avg_Y$weight
L172.CCI_rcp_gcm_cm_R_C3avg_Y <- aggregate( L172.wtCCI_rcp_gcm_cm_R_C3avg_Y[ c( X_CCI_years, "weight" ) ],
      by=as.list( L172.wtCCI_rcp_gcm_cm_R_C3avg_Y[ rcp_gcm_cm_aezid_irr ] ), sum )
L172.CCI_rcp_gcm_cm_R_C3avg_Y[ X_CCI_years ] <- L172.CCI_rcp_gcm_cm_R_C3avg_Y[ X_CCI_years ] / L172.CCI_rcp_gcm_cm_R_C3avg_Y$weight
L172.CCI_rcp_gcm_cm_R_C3avg_Y$crop <- "C3avg"

#Append C3 average to the table of impacts
L172.CCI_rcp_gcm_cm_R_Cisi_Y <- rbind( L172.CCI_rcp_gcm_cm_R_Cisi_Y, L172.CCI_rcp_gcm_cm_R_C3avg_Y )

#Match in the iso and AEZ for the region ID vector
L172.CCI_rcp_gcm_cm_R_Cisi_Y[ c( "iso", "AEZ" ) ] <- GIS_ctry_AEZ[ match( L172.CCI_rcp_gcm_cm_R_Cisi_Y$ID, GIS_ctry_AEZ$AEZ_ID ), c( "iso", "AEZ" ) ]

#2c. Use the base-year GTAP database to downscale from crops in the crop models to the full set
printlog( "Matching yield impacts from ISIMIP into the GTAP database of production")
L151.GTAP_ag_irrHA_ha$irr <- "_firr"
L151.GTAP_ag_rfdHA_ha$irr <- "noirr"
L172.GTAP_ag_HA_ha <- rbind( L151.GTAP_ag_irrHA_ha, L151.GTAP_ag_rfdHA_ha )
L172.GTAP_ag_HA_ha$iso <- L172.GTAP_ag_HA_ha$ctry
L172.GTAP_ag_HA_ha$LPJmL_crop <- FAO_ag_items_PRODSTAT$LPJmL_crop[
      match( L172.GTAP_ag_HA_ha$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L172.GTAP_ag_HA_ha$GEPIC_crop <- FAO_ag_items_PRODSTAT$GEPIC_crop[
      match( L172.GTAP_ag_HA_ha$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L172.GTAP_ag_HA_ha <- na.omit( L172.GTAP_ag_HA_ha )
L172.GTAP_ag_HA_ha.melt <- melt( L172.GTAP_ag_HA_ha, variable.name = "AEZ", measure.vars = AEZs )

# Drop the zeroes. This will make the next steps much easier
L172.GTAP_ag_HA_ha.melt <- subset( L172.GTAP_ag_HA_ha.melt, value != 0 )

#Repeat by number of scenarios being considered for climate impacts. Match in the timestep
L172.all_rcp_gcm_cm <- unique( L172.CCI_rcp_gcm_cm_R_Cisi_Y[ rcp_gcm_cm ] )
L172.all_rcp_gcm_cm$scenID <- 1:nrow( L172.all_rcp_gcm_cm )
L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr <- repeat_and_add_vector( L172.GTAP_ag_HA_ha.melt, "scenID", L172.all_rcp_gcm_cm$scenID )
L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr[ rcp_gcm_cm ] <- L172.all_rcp_gcm_cm[
      match( L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr$scenID, L172.all_rcp_gcm_cm$scenID ), rcp_gcm_cm]
L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr$cm_crop[ L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr$cropmodel == "lpjml" ] <-
     L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr$LPJmL_crop[ L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr$cropmodel == "lpjml" ]
L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr$cm_crop[ grepl( "gepic", L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr$cropmodel ) ] <-
     L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr$GEPIC_crop[ grepl( "gepic", L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr$cropmodel ) ]

#Match in the climate impacts for each row, in a separate column.
L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr[ X_CCI_years ] <- L172.CCI_rcp_gcm_cm_R_Cisi_Y[
      match( vecpaste( L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr[ c( rcp_gcm_cm, "iso", "AEZ", "cm_crop", "irr" ) ] ),
             vecpaste( L172.CCI_rcp_gcm_cm_R_Cisi_Y[ c( rcp_gcm_cm, "iso", "AEZ", "crop", "irr" ) ] ) ),
      X_CCI_years ]

#Drop missing values (where ISIMIP's base year weight was 0 but GTAP had production)
L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr <- na.omit( L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr )

#Multiply the yield delta by the base year weight
printlog( "Aggregating yield impacts by GCAM regions and commodities")
L172.GTAP_ag_wtCCI_rcp_gcm_cm_irr <- L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr
L172.GTAP_ag_wtCCI_rcp_gcm_cm_irr[ X_CCI_years ] <- L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr[ X_CCI_years ] * L172.GTAP_ag_HA_ha_rcp_gcm_cm_irr$value

#Match GCAM regions and commodities into the tables of weighted impacts and the base-year weights
L172.GTAP_ag_wtCCI_rcp_gcm_cm_irr$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L172.GTAP_ag_wtCCI_rcp_gcm_cm_irr$iso, iso_GCAM_regID$iso ) ]
L172.GTAP_ag_wtCCI_rcp_gcm_cm_irr$GCAM_commodity <- FAO_ag_items_PRODSTAT$GCAM_commodity[
      match( L172.GTAP_ag_wtCCI_rcp_gcm_cm_irr$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

#Aggregate weighted impacts and base year weights by GCAM region and commodity
L172.ag_wtCCI_rcp_gcm_cm_R_C_AEZ_irr <- aggregate( L172.GTAP_ag_wtCCI_rcp_gcm_cm_irr[ X_CCI_years ],
      by=as.list( L172.GTAP_ag_wtCCI_rcp_gcm_cm_irr[ c( rcp_gcm_cm, R_C, "AEZ", "irr" ) ] ), sum )

#Divide weighted impacts by the base year weights to get weighted average impacts by GCAM regions and commodities
L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr <- L172.ag_wtCCI_rcp_gcm_cm_R_C_AEZ_irr
L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[ X_CCI_years ] <- L172.ag_wtCCI_rcp_gcm_cm_R_C_AEZ_irr[ X_CCI_years ] / L172.ag_wtCCI_rcp_gcm_cm_R_C_AEZ_irr$base
L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[ X_CCI_years ][ L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[ X_CCI_years ] > max_CCImult ] <- max_CCImult
L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[ X_CCI_years ][ L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[ X_CCI_years ] < min_CCImult ] <- min_CCImult

#Re-set the irrigation names to match what will be used later on
names( L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr )[ names( L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr ) == "irr" ] <- irr
L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[[ irr ]] <- sub( "_firr", "IRR", L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[[ irr ]] )
L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[[ irr ]] <- sub( "noirr", "RFD", L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[[ irr ]] )

#Bioenergy climate change impacts: use the median of the other crops
L172.bio_CCI_rcp_gcm_cm_R_AEZ_irr <- aggregate( L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[ X_CCI_years ],
      by=as.list( L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[ c( rcp_gcm_cm, R_AEZ_irr ) ] ), median )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr <- c( "Climate impacts on agricultural productivity by RCP / GCM / crop model / region / commodity / year / AEZ",
      "Unitless multiplier" )
comments.L172.bio_CCI_rcp_gcm_cm_R_AEZ_irr <- c( "Climate impacts on bioenergy productivity by RCP / GCM / crop model / region / year / AEZ",
      "Unitless multiplier" )

#write tables as CSV files
writedata( L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr, domain="AGLU_LEVEL1_DATA",fn="L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr",
      comments=comments.L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr )
writedata( L172.bio_CCI_rcp_gcm_cm_R_AEZ_irr, domain="AGLU_LEVEL1_DATA",fn="L172.bio_CCI_rcp_gcm_cm_R_AEZ_irr", comments=comments.L172.bio_CCI_rcp_gcm_cm_R_AEZ_irr )

# Every script should finish with this line
logstop()
