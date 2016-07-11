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
logstart( "LB165.ag_water_R_C_Y_AEZ_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Water demand by GCAM region, commodity, AEZ" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_ag_CROSIT <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_CROSIT" )
UNESCO_green_irr <- readdata( "AGLU_LEVEL0_DATA", "UNESCO_green_irr" )
UNESCO_green_rfd <- readdata( "AGLU_LEVEL0_DATA", "UNESCO_green_rfd" )
UNESCO_blue <- readdata( "AGLU_LEVEL0_DATA", "UNESCO_blue" )
MIRCA_green_2426 <- readdata( "AGLU_LEVEL0_DATA", "MIRCA_greenVWC_crop24and26" )
MIRCA_blue_2426 <- readdata( "AGLU_LEVEL0_DATA", "MIRCA_blueVWC_crop24and26" )
fodder_VWC <- readdata( "AGLU_LEVEL0_DATA", "Fodder_VWC" )
MIRCA_irrHA <- readdata( "AGLU_LEVEL0_DATA", "MIRCA_irrHA" )
MIRCA_rfdHA <- readdata( "AGLU_LEVEL0_DATA", "MIRCA_rfdHA" )
GTAP_ag_irrProd_t <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_irrProd_t" )
GTAP_ag_rfdProd_t <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_rfdProd_t" )
L151.MIRCA_ag_irrProd_t <- readdata( "AGLU_LEVEL1_DATA", "L151.MIRCA_ag_irrProd_t" )
L151.MIRCA_ag_rfdProd_t <- readdata( "AGLU_LEVEL1_DATA", "L151.MIRCA_ag_rfdProd_t" )
L151.GTAP_ag_irrProd_t <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_irrProd_t" )
L151.GTAP_ag_rfdProd_t <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_rfdProd_t" )
L161.ag_irrProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_AEZ" )
L161.ag_rfdProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_AEZ" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Combine MIRCA & GTAP datasets, calculating consumption of blue water and biophysical water
printlog( "Prepare UNESCO and MIRCA data for processing" )
#Indicate the AEZs with the full reporting codes and specify the iso of each country
UNESCO_green_irr$AEZ <- paste( "AEZ", sprintf( "%02d", UNESCO_green_irr$AEZ ), sep = "" )
UNESCO_green_rfd$AEZ <- paste( "AEZ", sprintf( "%02d", UNESCO_green_rfd$AEZ ), sep = "" )
UNESCO_blue$AEZ <- paste( "AEZ", sprintf( "%02d", UNESCO_blue$AEZ ), sep = "" )
UNESCO_green_irr$ctry <- AGLU_ctry$iso[ match( UNESCO_green_irr$Country, AGLU_ctry$MIRCA_country)]
UNESCO_green_rfd$ctry <- AGLU_ctry$iso[ match( UNESCO_green_rfd$Country, AGLU_ctry$MIRCA_country)]
UNESCO_blue$ctry <- AGLU_ctry$iso[ match( UNESCO_blue$Country, AGLU_ctry$MIRCA_country)]
MIRCA_irrHA$AEZ <- paste( "AEZ", sprintf( "%02d", MIRCA_irrHA$AEZ ), sep = "" )
MIRCA_rfdHA$AEZ <- paste( "AEZ", sprintf( "%02d", MIRCA_rfdHA$AEZ ), sep = "" )
MIRCA_irrHA$ctry <- AGLU_ctry$iso[ match( MIRCA_irrHA$Country_name, AGLU_ctry$MIRCA_country)]
MIRCA_rfdHA$ctry <- AGLU_ctry$iso[ match( MIRCA_rfdHA$Country_name, AGLU_ctry$MIRCA_country)]
L151.MIRCA_ag_irrProd_t$ctry <- AGLU_ctry$iso[ match( L151.MIRCA_ag_irrProd_t$MIRCA_country, AGLU_ctry$MIRCA_country)]
L151.MIRCA_ag_rfdProd_t$ctry <- AGLU_ctry$iso[ match( L151.MIRCA_ag_rfdProd_t$MIRCA_country, AGLU_ctry$MIRCA_country)]
MIRCA_green_2426$ctry <- AGLU_ctry$iso[ match( MIRCA_green_2426$Country_name, AGLU_ctry$MIRCA_country)]
MIRCA_blue_2426$ctry <- AGLU_ctry$iso[ match( MIRCA_blue_2426$Country_name, AGLU_ctry$MIRCA_country)]
fodder_VWC$ctry <- AGLU_ctry$iso[ match( fodder_VWC$Country_name, AGLU_ctry$FAO_country)]

#Melt the dataframes, using only the relevant variables
MIRCA_IDs <- c( "ctry", "AEZ" )
MIRCA_crops <- paste( "Crop", 1:26, sep = "" )

L165.UNESCO_blue.melt <- melt( UNESCO_blue[ names( UNESCO_blue ) %!in% c( "FID", "Country", "PY_ID", "Total.water..km.3.year.", "GCAM_region" )], id.vars = c( MIRCA_IDs, "Province" ) )
L165.UNESCO_green_irr.melt <- melt( UNESCO_green_irr[ names( UNESCO_green_irr ) %!in% c( "FID", "Country", "PY_ID", "Total.water..km.3.year.", "GCAM_region" )], id.vars = c( MIRCA_IDs, "Province" ) )
L165.UNESCO_green_rfd.melt <- melt( UNESCO_green_rfd[ names( UNESCO_green_rfd ) %!in% c( "FID", "Country", "PY_ID", "Total.water..km.3.year.", "GCAM_region" )], id.vars = c( MIRCA_IDs, "Province" ) )
L165.MIRCA_blue_2426.melt <- melt( MIRCA_blue_2426[ names( MIRCA_blue_2426) %!in% c( "Country_name" )], id.vars = c( "ctry" ) )
L165.MIRCA_green_2426.melt <- melt( MIRCA_green_2426[ names( MIRCA_green_2426) %!in% c( "Country_name" )], id.vars = c( "ctry" ) )
L151.MIRCA_ag_irrProd_t.melt <- melt( L151.MIRCA_ag_irrProd_t[ names( L151.MIRCA_ag_irrProd_t ) %!in% c( "MIRCA_country" )], id.vars = c( "ctry", "AEZ" ) )
L151.MIRCA_ag_rfdProd_t.melt <- melt( L151.MIRCA_ag_rfdProd_t[ names( L151.MIRCA_ag_rfdProd_t ) %!in% c( "MIRCA_country" )], id.vars = c( "ctry", "AEZ" ) )

L165.MIRCA_irrHA.melt <- melt( MIRCA_irrHA[ names( MIRCA_irrHA ) %!in% c( "FID", "Country_name", "X" )], id.vars = c( MIRCA_IDs, "ID" ) )
L165.MIRCA_rfdHA.melt <- melt( MIRCA_rfdHA[ names( MIRCA_rfdHA ) %!in% c( "FID", "Country_name", "X8" )], id.vars = c( MIRCA_IDs, "ID" ) )

#Match GTAP_crop name
L165.MIRCA_blue_2426.melt$GTAP_crop <- FAO_ag_items_PRODSTAT$GTAP_crop[ match( L165.MIRCA_blue_2426.melt$variable, FAO_ag_items_PRODSTAT$MIRCA_Crop24and26)]
L165.MIRCA_green_2426.melt$GTAP_crop <- FAO_ag_items_PRODSTAT$GTAP_crop[ match( L165.MIRCA_green_2426.melt$variable, FAO_ag_items_PRODSTAT$MIRCA_Crop24and26)]

printlog( "Compute total water" )
#Calculate total biophysical water for irrigated crops
L165.UNESCO_tot_irr.melt <- L165.UNESCO_blue.melt
L165.UNESCO_tot_irr.melt$value <- L165.UNESCO_tot_irr.melt$value + 
  L165.UNESCO_green_irr.melt$value[ match( vecpaste( L165.UNESCO_tot_irr.melt[ c( "ctry", "AEZ", "Province", "variable" ) ] ), 
                                           vecpaste( L165.UNESCO_green_irr.melt[ c( "ctry", "AEZ", "Province", "variable" ) ] ) ) ]
L165.UNESCO_tot_irr.melt <- aggregate( L165.UNESCO_tot_irr.melt$value, by=as.list( L165.UNESCO_tot_irr.melt[ c( "ctry", "AEZ", "variable")]), sum )
names( L165.UNESCO_tot_irr.melt )[ names( L165.UNESCO_tot_irr.melt ) == "x" ] <- "value"

#Calculate total blue water for irrigated crops
L165.UNESCO_blue.melt <- aggregate( L165.UNESCO_blue.melt$value, by=as.list( L165.UNESCO_blue.melt[ c( "ctry", "AEZ", "variable")]), sum )
names( L165.UNESCO_blue.melt )[ names( L165.UNESCO_blue.melt ) == "x" ] <- "value"

#Calculate total green water for rainfed crops
L165.UNESCO_green_rfd.melt <- aggregate( L165.UNESCO_green_rfd.melt$value, by=as.list( L165.UNESCO_green_rfd.melt[ c( "ctry", "AEZ", "variable")]), sum )
names( L165.UNESCO_green_rfd.melt )[ names( L165.UNESCO_green_rfd.melt ) == "x" ] <- "value"

#Calculate water for crops 24 & 26 - for now we aren't distinguishing between irrigated & rainfed green
# L165.MIRCA_tot_2426.melt <- L165.MIRCA_blue_2426.melt
# L165.MIRCA_tot_2426.melt$value <- L165.MIRCA_blue_2426.melt$value[
#   match( vecpaste( L165.MIRCA_tot_2426.melt[ c( "ctry", "GTAP_crop" ) ] ),
#          vecpaste( L165.MIRCA_blue_2426.melt[ c( "ctry", "GTAP_crop" ) ] ) ) ] + L165.MIRCA_green_2426.melt$value[
#            match( vecpaste( L165.MIRCA_tot_2426.melt[ c( "ctry", "GTAP_crop" ) ] ),
#                   vecpaste( L165.MIRCA_green_2426.melt[ c( "ctry", "GTAP_crop" ) ] ) ) ]          

printlog( "Compute water coefficients from UNESCO data" )
#Match in ha - do this to compute weighted average coefficient per AEZ
#Note: this will map the first instance in the MIRCA dataframe. I think this is okay because each ID seems to repeat the same value.
#TODO: perhaps take the average of the MIRCA???
L165.UNESCO_tot_irr.melt$prod <- L151.MIRCA_ag_irrProd_t.melt$value[ match( vecpaste( L165.UNESCO_tot_irr.melt[ c( "ctry", "AEZ", "variable" ) ] ), 
                                                                   vecpaste( L151.MIRCA_ag_irrProd_t.melt[ c( "ctry", "AEZ", "variable" ) ] ) ) ]        
L165.UNESCO_blue.melt$prod <- L151.MIRCA_ag_irrProd_t.melt$value[ match( vecpaste( L165.UNESCO_blue.melt[ c( "ctry", "AEZ", "variable" ) ] ), 
                                                                   vecpaste( L151.MIRCA_ag_irrProd_t.melt[ c( "ctry", "AEZ", "variable" ) ] ) ) ]        
L165.UNESCO_green_rfd.melt$prod <- L151.MIRCA_ag_rfdProd_t.melt$value[ match( vecpaste( L165.UNESCO_green_rfd.melt[ c( "ctry", "AEZ", "variable" ) ] ), 
                                                                   vecpaste( L151.MIRCA_ag_rfdProd_t.melt[ c( "ctry", "AEZ", "variable" ) ] ) ) ]  

L165.UNESCO_tot_irr.melt$coeff <- L165.UNESCO_tot_irr.melt$value / L165.UNESCO_tot_irr.melt$prod
L165.UNESCO_blue.melt$coeff <- L165.UNESCO_blue.melt$value  / L165.UNESCO_blue.melt$prod
L165.UNESCO_green_rfd.melt$coeff <- L165.UNESCO_green_rfd.melt$value / L165.UNESCO_green_rfd.melt$prod

L165.UNESCO_tot_irr.melt <- na.omit( L165.UNESCO_tot_irr.melt )
L165.UNESCO_blue.melt <- na.omit( L165.UNESCO_blue.melt )
L165.UNESCO_green_rfd.melt <- na.omit( L165.UNESCO_green_rfd.melt )

printlog( "Aggregate water coefficients to GTAP crops" )
#Match coefficients into GTAP table of production by country, crop, and AEZ
# Biophysical water for irrigated crops
L151.GTAP_ag_irrProd_t.melt <- melt( L151.GTAP_ag_irrProd_t, id.vars=c( "ctry", "GTAP_crop" ))
L165.GTAP_ag_tot_m3.melt <- L151.GTAP_ag_irrProd_t.melt
names( L165.GTAP_ag_tot_m3.melt )[ names( L165.GTAP_ag_tot_m3.melt ) == "value" ] <- "irrProd"
names( L165.GTAP_ag_tot_m3.melt )[ names( L165.GTAP_ag_tot_m3.melt ) == "variable" ] <- "AEZ"
L165.GTAP_ag_tot_m3.melt$MIRCA_crop <- FAO_ag_items_PRODSTAT$MIRCA_crop[ match( L165.GTAP_ag_tot_m3.melt$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L165.GTAP_ag_tot_m3.melt$cons_coeff <- L165.UNESCO_tot_irr.melt$coeff[
      match( vecpaste( L165.GTAP_ag_tot_m3.melt[ c( "ctry", "AEZ", "MIRCA_crop" )] ),
             vecpaste( L165.UNESCO_tot_irr.melt[ c( "ctry", "AEZ", "variable" )] ) ) ]
L165.GTAP_ag_tot_m3.melt$cons_coeff[ is.na( L165.GTAP_ag_tot_m3.melt$cons_coeff ) ] <- 0
L165.GTAP_ag_tot_m3.melt$cons_coeff[ L165.GTAP_ag_tot_m3.melt$cons_coeff == "Inf" ] <- 0
L165.GTAP_ag_tot_m3.melt$water <- L165.GTAP_ag_tot_m3.melt$irrProd * L165.GTAP_ag_tot_m3.melt$cons_coeff

# Blue water for irrigated crops
L151.GTAP_ag_irrProd_t.melt <- melt( L151.GTAP_ag_irrProd_t, id.vars=c( "ctry", "GTAP_crop" ))
L165.GTAP_ag_blue_m3.melt <- L151.GTAP_ag_irrProd_t.melt
names( L165.GTAP_ag_blue_m3.melt )[ names( L165.GTAP_ag_blue_m3.melt ) == "value" ] <- "irrProd"
names( L165.GTAP_ag_blue_m3.melt )[ names( L165.GTAP_ag_blue_m3.melt ) == "variable" ] <- "AEZ"
L165.GTAP_ag_blue_m3.melt$MIRCA_crop <- FAO_ag_items_PRODSTAT$MIRCA_crop[ match( L165.GTAP_ag_blue_m3.melt$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L165.GTAP_ag_blue_m3.melt$cons_coeff <- L165.UNESCO_blue.melt$coeff[
  match( vecpaste( L165.GTAP_ag_blue_m3.melt[ c( "ctry", "AEZ", "MIRCA_crop" )] ),
         vecpaste( L165.UNESCO_blue.melt[ c( "ctry", "AEZ", "variable" )] ) ) ]
L165.GTAP_ag_blue_m3.melt$cons_coeff[ is.na( L165.GTAP_ag_blue_m3.melt$cons_coeff ) ] <- 0
L165.GTAP_ag_blue_m3.melt$cons_coeff[ L165.GTAP_ag_blue_m3.melt$cons_coeff == "Inf" ] <- 0
L165.GTAP_ag_blue_m3.melt$water <- L165.GTAP_ag_blue_m3.melt$irrProd * L165.GTAP_ag_blue_m3.melt$cons_coeff

# Green water for rainfed crops
L151.GTAP_ag_rfdProd_t.melt <- melt( L151.GTAP_ag_rfdProd_t, id.vars=c( "ctry", "GTAP_crop" ))
L165.GTAP_ag_green_m3.melt <- L151.GTAP_ag_rfdProd_t.melt
names( L165.GTAP_ag_green_m3.melt )[ names( L165.GTAP_ag_green_m3.melt ) == "value" ] <- "rfdProd"
names( L165.GTAP_ag_green_m3.melt )[ names( L165.GTAP_ag_green_m3.melt ) == "variable" ] <- "AEZ"
L165.GTAP_ag_green_m3.melt$MIRCA_crop <- FAO_ag_items_PRODSTAT$MIRCA_crop[ match( L165.GTAP_ag_green_m3.melt$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L165.GTAP_ag_green_m3.melt$cons_coeff <- L165.UNESCO_green_rfd.melt$coeff[
  match( vecpaste( L165.GTAP_ag_green_m3.melt[ c( "ctry", "AEZ", "MIRCA_crop" )] ),
         vecpaste( L165.UNESCO_green_rfd.melt[ c( "ctry", "AEZ", "variable" )] ) ) ]
L165.GTAP_ag_green_m3.melt$cons_coeff[ is.na( L165.GTAP_ag_green_m3.melt$cons_coeff ) ] <- 0
L165.GTAP_ag_green_m3.melt$cons_coeff[ L165.GTAP_ag_green_m3.melt$cons_coeff == "Inf" ] <- 0
L165.GTAP_ag_green_m3.melt$water <- L165.GTAP_ag_green_m3.melt$rfdProd * L165.GTAP_ag_green_m3.melt$cons_coeff

#Cast by AEZ
L165.GTAP_ag_tot_m3 <- dcast( L165.GTAP_ag_tot_m3.melt, ctry + GTAP_crop ~ AEZ, value.var = "water" )
L165.GTAP_ag_blue_m3 <- dcast( L165.GTAP_ag_blue_m3.melt, ctry + GTAP_crop ~ AEZ, value.var = "water" )
L165.GTAP_ag_green_m3 <- dcast( L165.GTAP_ag_green_m3.melt, ctry + GTAP_crop ~ AEZ, value.var = "water" )

#2b Aggregate to GCAM regions and commodities
#add lookup vectors to each of the tables
printlog( "Adding region and crop lookup vectors to GTAP tables" )
with( iso_GCAM_regID, {
  L165.GTAP_ag_tot_m3$GCAM_region_ID <<- GCAM_region_ID[ match ( L165.GTAP_ag_tot_m3$ctry, iso ) ]
  L165.GTAP_ag_blue_m3$GCAM_region_ID <<- GCAM_region_ID[ match ( L165.GTAP_ag_blue_m3$ctry, iso ) ]
  L165.GTAP_ag_green_m3$GCAM_region_ID <<- GCAM_region_ID[ match ( L165.GTAP_ag_green_m3$ctry, iso ) ]
	GTAP_ag_irrProd_t$GCAM_region_ID <<- GCAM_region_ID[ match ( GTAP_ag_irrProd_t$ctry, iso ) ]
	GTAP_ag_rfdProd_t$GCAM_region_ID <<- GCAM_region_ID[ match ( GTAP_ag_rfdProd_t$ctry, iso ) ]
} )

with( FAO_ag_items_PRODSTAT, {
  L165.GTAP_ag_tot_m3$GCAM_commodity <<- GCAM_commodity[ match ( L165.GTAP_ag_tot_m3$GTAP_crop, GTAP_crop ) ]
  L165.GTAP_ag_blue_m3$GCAM_commodity <<- GCAM_commodity[ match ( L165.GTAP_ag_blue_m3$GTAP_crop, GTAP_crop ) ]
  L165.GTAP_ag_green_m3$GCAM_commodity <<- GCAM_commodity[ match ( L165.GTAP_ag_green_m3$GTAP_crop, GTAP_crop ) ]
  GTAP_ag_irrProd_t$GCAM_commodity <<- GCAM_commodity[ match ( GTAP_ag_irrProd_t$GTAP_crop, GTAP_crop ) ]
	GTAP_ag_rfdProd_t$GCAM_commodity <<- GCAM_commodity[ match ( GTAP_ag_rfdProd_t$GTAP_crop, GTAP_crop ) ]

} )

#build tables collapsed by GCAM regions and crop names
printlog( "Collapsing GTAP ag commodity data into GCAM regions and commodities")
L165.ag_tot_m3_R_C_AEZ <- aggregate( L165.GTAP_ag_tot_m3[ AEZs ],
   by=list( GCAM_region_ID= L165.GTAP_ag_tot_m3$GCAM_region_ID,
            GCAM_commodity= L165.GTAP_ag_tot_m3$GCAM_commodity ), sum )
L165.ag_blue_m3_R_C_AEZ <- aggregate( L165.GTAP_ag_blue_m3[ AEZs ],
   by=list( GCAM_region_ID= L165.GTAP_ag_blue_m3$GCAM_region_ID,
            GCAM_commodity= L165.GTAP_ag_blue_m3$GCAM_commodity ), sum )
L165.ag_green_m3_R_C_AEZ <- aggregate( L165.GTAP_ag_green_m3[ AEZs ],
   by=list( GCAM_region_ID= L165.GTAP_ag_green_m3$GCAM_region_ID,
            GCAM_commodity= L165.GTAP_ag_green_m3$GCAM_commodity ), sum )
L165.GTAP_ag_irrProd_t <- aggregate( GTAP_ag_irrProd_t[ AEZs ],
   by=list( GCAM_region_ID= GTAP_ag_irrProd_t$GCAM_region_ID,
            GCAM_commodity= GTAP_ag_irrProd_t$GCAM_commodity ), sum )
L165.GTAP_ag_rfdProd_t <- aggregate( GTAP_ag_rfdProd_t[ AEZs ],
   by=list( GCAM_region_ID= GTAP_ag_rfdProd_t$GCAM_region_ID,
            GCAM_commodity= GTAP_ag_rfdProd_t$GCAM_commodity ), sum )
       
#convert to desired units (Mt and km3)
printlog( "Converting volume to km3 and mass to Mt")
L165.ag_tot_km3_R_C_AEZ <- L165.ag_tot_m3_R_C_AEZ
L165.ag_tot_km3_R_C_AEZ[ AEZs ] <- L165.ag_tot_km3_R_C_AEZ[ AEZs ] * conv_m3_bm3
L165.ag_blue_km3_R_C_AEZ <- L165.ag_blue_m3_R_C_AEZ
L165.ag_blue_km3_R_C_AEZ[ AEZs ] <- L165.ag_blue_km3_R_C_AEZ[ AEZs ] * conv_m3_bm3
L165.ag_green_km3_R_C_AEZ <- L165.ag_green_m3_R_C_AEZ
L165.ag_green_km3_R_C_AEZ[ AEZs ] <- L165.ag_green_km3_R_C_AEZ[ AEZs ] * conv_m3_bm3
L165.GTAP_ag_irrProd_Mt <- L165.GTAP_ag_irrProd_t
L165.GTAP_ag_irrProd_Mt[ AEZs ] <- L165.GTAP_ag_irrProd_Mt[ AEZs ] * conv_t_Mt
L165.GTAP_ag_rfdProd_Mt <- L165.GTAP_ag_rfdProd_t
L165.GTAP_ag_rfdProd_Mt[ AEZs ] <- L165.GTAP_ag_rfdProd_Mt[ AEZs ] * conv_t_Mt

#2c Compute coefficients
L165.tot_km3Mt_R_C_AEZ <- L165.GTAP_ag_irrProd_Mt
L165.tot_km3Mt_R_C_AEZ[ AEZs ] <- L165.ag_tot_km3_R_C_AEZ[ match( vecpaste( L165.tot_km3Mt_R_C_AEZ[ R_C ] ), vecpaste( L165.ag_tot_km3_R_C_AEZ[ R_C ] ) ), AEZs ] / L165.GTAP_ag_irrProd_Mt[ AEZs ]
L165.blue_km3Mt_R_C_AEZ <- L165.GTAP_ag_irrProd_Mt
L165.blue_km3Mt_R_C_AEZ[ AEZs ] <- L165.ag_blue_km3_R_C_AEZ[ match( vecpaste( L165.blue_km3Mt_R_C_AEZ[ R_C ] ), vecpaste( L165.ag_blue_km3_R_C_AEZ[ R_C ] ) ), AEZs ] / L165.GTAP_ag_irrProd_Mt[ AEZs ]
L165.green_km3Mt_R_C_AEZ <- L165.GTAP_ag_rfdProd_Mt
L165.green_km3Mt_R_C_AEZ[ AEZs ] <- L165.ag_green_km3_R_C_AEZ[ match( vecpaste( L165.green_km3Mt_R_C_AEZ[ R_C ] ), vecpaste( L165.ag_green_km3_R_C_AEZ[ R_C ] ) ), AEZs ] / L165.GTAP_ag_rfdProd_Mt[ AEZs ]

#Compute Fodder coefficients
L165.ag_irrProd_Mt_R_C_AEZ <- L161.ag_irrProd_Mt_R_C_Y_AEZ[ L161.ag_irrProd_Mt_R_C_Y_AEZ$year == "2005", ]
L165.ag_rfdProd_Mt_R_C_AEZ <- L161.ag_rfdProd_Mt_R_C_Y_AEZ[ L161.ag_rfdProd_Mt_R_C_Y_AEZ$year == "2005", ]
L165.ag_irrProd_Mt_R_C_AEZ <- L165.ag_irrProd_Mt_R_C_AEZ[ names( L165.ag_irrProd_Mt_R_C_AEZ ) != "year" ]
L165.ag_rfdProd_Mt_R_C_AEZ <- L165.ag_rfdProd_Mt_R_C_AEZ[ names( L165.ag_rfdProd_Mt_R_C_AEZ ) != "year" ]
L165.ag_totProd_Mt_R_C_AEZ <- L165.ag_irrProd_Mt_R_C_AEZ
L165.ag_totProd_Mt_R_C_AEZ[ AEZs ] <- L165.ag_irrProd_Mt_R_C_AEZ[ AEZs ] + L165.ag_rfdProd_Mt_R_C_AEZ[ AEZs ]

L165.Fodder_VWC_Mm3 <- fodder_VWC
with( iso_GCAM_regID, {
	L165.Fodder_VWC_Mm3$GCAM_region_ID <<- GCAM_region_ID[ match ( L165.Fodder_VWC_Mm3$ctry, iso ) ]
} )
L165.Fodder_VWC_R_Mm3 <- aggregate( L165.Fodder_VWC_Mm3[ c( "Green_rfd", "Green_irr", "Green_tot", "Blue" ) ],
   by=list( GCAM_region_ID= L165.Fodder_VWC_Mm3$GCAM_region_ID ), sum )
L165.Fodder_VWC_R_Mm3$Tot_bio <- L165.Fodder_VWC_R_Mm3$Green_tot + L165.Fodder_VWC_R_Mm3$Blue
L165.Fodder_VWC_R_km3 <- L165.Fodder_VWC_R_Mm3
L165.Fodder_VWC_R_km3[ names( L165.Fodder_VWC_R_km3 ) != "GCAM_region_ID" ] <- L165.Fodder_VWC_R_km3[ names( L165.Fodder_VWC_R_km3 ) != "GCAM_region_ID" ] * conv_Mm3_km3
   
L165.Fodder_irrProd_R_A_Mt <- L165.ag_irrProd_Mt_R_C_AEZ[ L165.ag_irrProd_Mt_R_C_AEZ$GCAM_commodity %in% c( "FodderGrass", "FodderHerb"),  ]
L165.Fodder_irrProd_R_A_Mt <- aggregate( L165.Fodder_irrProd_R_A_Mt[ AEZs ],
   by=list( GCAM_region_ID= L165.Fodder_irrProd_R_A_Mt$GCAM_region_ID ), sum )
L165.Fodder_irrProd_R_Mt <- L165.Fodder_irrProd_R_A_Mt[ names( L165.Fodder_irrProd_R_A_Mt ) == "GCAM_region_ID" ]
L165.Fodder_irrProd_R_Mt$irrProd <- rowSums( L165.Fodder_irrProd_R_A_Mt[ AEZs ] )

L165.Fodder_totProd_R_A_Mt <- L165.ag_totProd_Mt_R_C_AEZ[ L165.ag_totProd_Mt_R_C_AEZ$GCAM_commodity %in% c( "FodderGrass", "FodderHerb"),  ]
L165.Fodder_totProd_R_A_Mt <- aggregate( L165.Fodder_totProd_R_A_Mt[ AEZs ],
   by=list( GCAM_region_ID= L165.Fodder_totProd_R_A_Mt$GCAM_region_ID ), sum )
L165.Fodder_totProd_R_Mt <- L165.Fodder_totProd_R_A_Mt[ names( L165.Fodder_totProd_R_A_Mt ) == "GCAM_region_ID" ]
L165.Fodder_totProd_R_Mt$totProd <- rowSums( L165.Fodder_totProd_R_A_Mt[ AEZs ] )

L165.blue_km3Mt_R_C_AEZ[ L165.blue_km3Mt_R_C_AEZ$GCAM_commodity %in% c( "FodderGrass", "FodderHerb"), AEZs ] <-
      L165.Fodder_VWC_R_km3$Blue[ match( L165.blue_km3Mt_R_C_AEZ$GCAM_region_ID, L165.Fodder_VWC_R_km3$GCAM_region_ID ) ] /
      L165.Fodder_irrProd_R_Mt$irrProd[ match( L165.blue_km3Mt_R_C_AEZ$GCAM_region_ID, L165.Fodder_irrProd_R_Mt$GCAM_region_ID ) ] 
L165.tot_km3Mt_R_C_AEZ[ L165.tot_km3Mt_R_C_AEZ$GCAM_commodity %in% c( "FodderGrass", "FodderHerb"), AEZs ] <-
      L165.Fodder_VWC_R_km3$Tot_bio[ match( L165.tot_km3Mt_R_C_AEZ$GCAM_region_ID, L165.Fodder_VWC_R_km3$GCAM_region_ID ) ] /
      L165.Fodder_totProd_R_Mt$totProd[ match( L165.tot_km3Mt_R_C_AEZ$GCAM_region_ID, L165.Fodder_totProd_R_Mt$GCAM_region_ID ) ] 
L165.green_km3Mt_R_C_AEZ[ L165.green_km3Mt_R_C_AEZ$GCAM_commodity %in% c( "FodderGrass", "FodderHerb"), AEZs ] <-
      L165.Fodder_VWC_R_km3$Tot_bio[ match( L165.green_km3Mt_R_C_AEZ$GCAM_region_ID, L165.Fodder_VWC_R_km3$GCAM_region_ID ) ] /
      L165.Fodder_totProd_R_Mt$totProd[ match( L165.green_km3Mt_R_C_AEZ$GCAM_region_ID, L165.Fodder_totProd_R_Mt$GCAM_region_ID ) ] 

#Convert all missing values to 0 to avoid errors in later processing
L165.blue_km3Mt_R_C_AEZ[ is.na( L165.blue_km3Mt_R_C_AEZ ) ] <- 0
L165.tot_km3Mt_R_C_AEZ[ is.na( L165.tot_km3Mt_R_C_AEZ ) ] <- 0
L165.green_km3Mt_R_C_AEZ[ is.na( L165.green_km3Mt_R_C_AEZ ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L165.blue_km3Mt_R_C_AEZ <- c( "Irrigation water consumption coefficients by GCAM region / commodity / AEZ","Unit = km3 / Mt" )
comments.L165.tot_km3Mt_R_C_AEZ <- c( "Biophysical water consumption coefficients for irrigated crops by GCAM region / commodity / AEZ","Unit = km3 / Mt" )
comments.L165.green_km3Mt_R_C_AEZ <- c( "Biophysical water consumption coefficients for rainfed crops by GCAM region / commodity / AEZ","Unit = km3 / Mt" )

#export final tables as CSV files
writedata( L165.blue_km3Mt_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L165.blue_km3Mt_R_C_AEZ", comments=comments.L165.blue_km3Mt_R_C_AEZ )
writedata( L165.tot_km3Mt_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L165.tot_km3Mt_R_C_AEZ", comments=comments.L165.tot_km3Mt_R_C_AEZ )
writedata( L165.green_km3Mt_R_C_AEZ, domain="AGLU_LEVEL1_DATA",fn="L165.green_km3Mt_R_C_AEZ", comments=comments.L165.green_km3Mt_R_C_AEZ )

# Every script should finish with this line
logstop()
            
