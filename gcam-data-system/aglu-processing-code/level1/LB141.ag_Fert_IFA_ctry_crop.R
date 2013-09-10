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
logstart( "L141.ag_Fert_IFA_ctry_crop.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Downscaling of IFA fertilizer demands to FAO countries and crops" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
GTAP_ag_HA_ha <- readdata( "AGLU_LEVEL0_DATA", "GTAP_ag_HA_ha" )
IFA2002_Fert_ktN <- readdata( "AGLU_LEVEL0_DATA", "IFA2002_Fert_ktN" )
IFA_Fert_ktN <- readdata( "AGLU_LEVEL0_DATA", "IFA_Fert_ktN" )

# -----------------------------------------------------------------------------
# 2. Perform calculations
Rifa_Cifa <- c( "IFA_region", "IFA_commodity" )

printlog( "Step 1: Modify IFA global fertilizer consumption by commodity and region to more detailed inventory with partial coverage" )
#Aggregate IFA fertilizer consumption by its 24 regions and compute scalers to match the FAO totals
L141.IFA_Fert_ktN.melt <- melt( subset( IFA_Fert_ktN, Country != "World Total" ), id.vars = "Country" )
L141.IFA_Fert_ktN.melt$Fert_MtN <- L141.IFA_Fert_ktN.melt$value * conv_kt_Mt
names( L141.IFA_Fert_ktN.melt )[ names( L141.IFA_Fert_ktN.melt ) %in% c( "Country", "variable" ) ] <- Rifa_Cifa

#Compile data from IFA on fertilizer consumption by IFA region and crop
L141.IFA2002_Fert_ktN <- IFA2002_Fert_ktN
L141.IFA2002_Fert_ktN$N_tha <- L141.IFA2002_Fert_ktN$N_kt / L141.IFA2002_Fert_ktN$AREA_thousHa

#Drop missing values, and then take the maximum of available application rates, when multiple years are present for the same country/crop
printlog( "NOTE: Using application rates (not quantities) from detailed IFA inventory, and using maximum of all years where multiple years are available")
L141.IFA2002_Fert_ktN <- na.omit( L141.IFA2002_Fert_ktN )
L141.IFA2002_Fert_ktN <- aggregate( L141.IFA2002_Fert_ktN[ c( "AREA_thousHa", "N_kt", "N_tha" ) ],
      by=list( IFA2002_country = L141.IFA2002_Fert_ktN$COUNTRY, IFA2002_crop = L141.IFA2002_Fert_ktN$CROP ), max )

#Multiply harvested area by application rates to get a bottom-up estimate of fertilizer demands
printlog( "Compiling specific fertilizer demand coefficients for the 87 countries / 106 crops where available" )
L141.IFA_Fert_Cons_MtN_ctry_crop <- data.frame( GTAP_ag_HA_ha[ c( "ctry", "GTAP_crop" ) ],
      HA_ha = apply( GTAP_ag_HA_ha[ AEZs ], 1, sum ) )
#Drop rows with 0 harvested area (these wouldn't have any fertilizer consumption anyway)
L141.IFA_Fert_Cons_MtN_ctry_crop <- subset( L141.IFA_Fert_Cons_MtN_ctry_crop, HA_ha !=0 )
L141.IFA_Fert_Cons_MtN_ctry_crop$IFA2002_crop <- FAO_ag_items_PRODSTAT$IFA2002_crop[ match( L141.IFA_Fert_Cons_MtN_ctry_crop$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
#In Ethiopia, replace unspecified cereals with teff
L141.IFA_Fert_Cons_MtN_ctry_crop$IFA2002_crop[ L141.IFA_Fert_Cons_MtN_ctry_crop$ctry == "eth" & L141.IFA_Fert_Cons_MtN_ctry_crop$GTAP_crop == "cerealsnes" ] <- "Teff"
L141.IFA_Fert_Cons_MtN_ctry_crop$IFA2002_country <- AGLU_ctry$IFA2002_country[ match( L141.IFA_Fert_Cons_MtN_ctry_crop$ctry, AGLU_ctry$iso ) ]

#Match in the application rate
L141.IFA_Fert_Cons_MtN_ctry_crop$IFA2002_N_tha <- L141.IFA2002_Fert_ktN$N_tha[
      match( vecpaste( L141.IFA_Fert_Cons_MtN_ctry_crop[ c( "IFA2002_crop", "IFA2002_country" ) ] ),
             vecpaste( L141.IFA2002_Fert_ktN[ c( "IFA2002_crop", "IFA2002_country" ) ] ) ) ]

printlog( "Using default coefficients for remaining countries and crops")
#Match in the IFA regions and crop commodity classes (drop ones that aren't mapped to any commodities), and aggregate harvested area
L141.IFA_Fert_Cons_MtN_ctry_crop$IFA_commodity <- FAO_ag_items_PRODSTAT$IFA_commodity[
      match( L141.IFA_Fert_Cons_MtN_ctry_crop$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L141.IFA_Fert_Cons_MtN_ctry_crop <- L141.IFA_Fert_Cons_MtN_ctry_crop[ !is.na( L141.IFA_Fert_Cons_MtN_ctry_crop$IFA_commodity ), ]
L141.IFA_Fert_Cons_MtN_ctry_crop$IFA_region <- AGLU_ctry$IFA_region[
      match( L141.IFA_Fert_Cons_MtN_ctry_crop$ctry, AGLU_ctry$iso ) ]
L141.HA_ha_Rifa_Cifa <- aggregate( L141.IFA_Fert_Cons_MtN_ctry_crop[ c( "HA_ha" ) ],
      by=as.list( L141.IFA_Fert_Cons_MtN_ctry_crop[ Rifa_Cifa ] ), sum, na.rm = T )

#Match in the fertilizer demands by these regions and commodities
L141.HA_ha_Rifa_Cifa$IFA_N_Mt <- L141.IFA_Fert_ktN.melt$Fert_MtN[
      match( vecpaste( L141.HA_ha_Rifa_Cifa[ Rifa_Cifa ] ),
             vecpaste( L141.IFA_Fert_ktN.melt[ Rifa_Cifa ] ) ) ]
L141.HA_ha_Rifa_Cifa$IFA_N_tha_default <- L141.HA_ha_Rifa_Cifa$IFA_N_Mt / L141.HA_ha_Rifa_Cifa$HA_ha / conv_t_Mt

printlog( "Using specific coefficients where available, and defaults where not, to calculate bottom-up estimates of fertilizer by country and crop")
L141.IFA_Fert_Cons_MtN_ctry_crop$IFA_N_tha_default <- L141.HA_ha_Rifa_Cifa$IFA_N_tha_default[
      match( vecpaste( L141.IFA_Fert_Cons_MtN_ctry_crop[ Rifa_Cifa ] ),
             vecpaste( L141.HA_ha_Rifa_Cifa[ Rifa_Cifa ] ) ) ]
L141.IFA_Fert_Cons_MtN_ctry_crop$IFA_N_Mt_unscaled <- ifelse( is.na( L141.IFA_Fert_Cons_MtN_ctry_crop$IFA2002_N_tha ),
      L141.IFA_Fert_Cons_MtN_ctry_crop$HA_ha * L141.IFA_Fert_Cons_MtN_ctry_crop$IFA_N_tha_default * conv_t_Mt,
      L141.IFA_Fert_Cons_MtN_ctry_crop$HA_ha * L141.IFA_Fert_Cons_MtN_ctry_crop$IFA2002_N_tha * conv_t_Mt )

printlog( "Step 2: Re-scale fertilizer consumption estimates so that totals match the IFA top-down inventory" )
L141.IFA_Fert_Cons_Rifa_Cifa <- aggregate( L141.IFA_Fert_Cons_MtN_ctry_crop[ "IFA_N_Mt_unscaled" ],
      by=as.list( L141.IFA_Fert_Cons_MtN_ctry_crop[ Rifa_Cifa ] ), sum )
L141.IFA_Fert_Cons_Rifa_Cifa$IFA_N_Mt <- L141.IFA_Fert_ktN.melt$Fert_MtN[
      match( vecpaste( L141.IFA_Fert_Cons_Rifa_Cifa[ Rifa_Cifa ] ),
             vecpaste( L141.IFA_Fert_ktN.melt[ Rifa_Cifa ] ) ) ]
L141.IFA_Fert_Cons_Rifa_Cifa$scaler <- L141.IFA_Fert_Cons_Rifa_Cifa$IFA_N_Mt / L141.IFA_Fert_Cons_Rifa_Cifa$IFA_N_Mt_unscaled
L141.IFA_Fert_Cons_Rifa_Cifa$scaler[ is.na( L141.IFA_Fert_Cons_Rifa_Cifa$scaler ) ] <- 1

#Apply this scaler to calculate final scaled fertilizer demands for IFA base year time period
L141.IFA_Fert_Cons_MtN_ctry_crop$scaler <- L141.IFA_Fert_Cons_Rifa_Cifa$scaler[
      match( vecpaste( L141.IFA_Fert_Cons_MtN_ctry_crop[ Rifa_Cifa ] ),
             vecpaste( L141.IFA_Fert_Cons_Rifa_Cifa[ Rifa_Cifa ] ) ) ]
L141.IFA_Fert_Cons_MtN_ctry_crop$Fert_Cons_MtN <- L141.IFA_Fert_Cons_MtN_ctry_crop$IFA_N_Mt_unscaled * L141.IFA_Fert_Cons_MtN_ctry_crop$scaler

#Build final table for write-out
L141.ag_Fert_Cons_MtN_ctry_crop <- L141.IFA_Fert_Cons_MtN_ctry_crop[ c( "ctry", "GTAP_crop", "Fert_Cons_MtN" ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L141.ag_Fert_Cons_MtN_ctry_crop <- c( "Fertilizer consumption by GTAP country / crop ","Unit = MtN" )

#write tables as CSV files
writedata( L141.ag_Fert_Cons_MtN_ctry_crop, domain="AGLU_LEVEL1_DATA", fn="L141.ag_Fert_Cons_MtN_ctry_crop", comments=comments.L141.ag_Fert_Cons_MtN_ctry_crop )

# Every script should finish with this line
logstop()
