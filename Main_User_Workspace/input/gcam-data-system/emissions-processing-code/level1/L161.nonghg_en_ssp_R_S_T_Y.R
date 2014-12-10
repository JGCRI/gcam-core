# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "EMISSPROC_DIR" ) ){
    if( Sys.getenv( "EMISSIONSPROC" ) != "" ){
        EMISSPROC_DIR <- Sys.getenv( "EMISSIONSPROC" )
    } else {
        stop("Could not determine location of emissions data system. Please set the R var EMISSPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
logstart( "L161.nonghg_en_ssp_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Emissions factors by GCAM technology for the SSPs. Computed using GAINS data." )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
A_region <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
GAINS_sector <- readdata( "EMISSIONS_MAPPINGS", "gains_to_gcam_sector" )
GAINS_activities <- readdata( "EMISSIONS_LEVEL0_DATA", "GAINS_activities" )
GAINS_emissions <- readdata( "EMISSIONS_LEVEL0_DATA", "GAINS_emissions" )
L102.pcgdp_thous90USD_SSP_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_SSP_R_Y")
L111.nonghg_tgej_R_en_S_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L111.nonghg_tgej_R_en_S_F_Yh" )
L114.bcoc_tgej_R_en_S_F_2000 <- readdata( "EMISSIONS_LEVEL1_DATA", "L114.bcoc_tgej_R_en_S_F_2000")

# -----------------------------------------------------------------------------
# 2. Perform computations
# Prepare data for use
GAINS_emissions$POLL <- gsub( "NOX", "NOx", GAINS_emissions$POLL )
GAINS_emissions$POLL <- gsub( "VOC", "NMVOC", GAINS_emissions$POLL )

printlog( "Compute the scalers of emissions factors using GAINS data for GAINS regions" )
L161.GAINS_emissions_agg <- GAINS_emissions
L161.GAINS_emissions_agg$agg_sector <- GAINS_sector$GCAM_tag[ match( L161.GAINS_emissions_agg$TIMER_SECTOR, GAINS_sector$IIASA_Sector )]
L161.GAINS_emissions_agg <- aggregate( L161.GAINS_emissions_agg[ c( "CLE", "MFR", "SLE" ) ], by=as.list( L161.GAINS_emissions_agg[ c( "TIMER_REGION", "agg_sector", "POLL", "IDYEARS" ) ] ), sum )
L161.GAINS_emissions_agg <- L161.GAINS_emissions_agg[ L161.GAINS_emissions_agg$agg_sector != "", ]
L161.GAINS_emissions_agg <- na.omit( L161.GAINS_emissions_agg )

L161.GAINS_activities_agg <- GAINS_activities
L161.GAINS_activities_agg$agg_sector <- GAINS_sector$GCAM_tag[ match( L161.GAINS_activities_agg$TIMER_SECTOR, GAINS_sector$IIASA_Sector )]
L161.GAINS_activities_agg <- aggregate( L161.GAINS_activities_agg[ c( "ACT" ) ], by=as.list( L161.GAINS_activities_agg[ c( "TIMER_REGION", "agg_sector", "IDYEARS" ) ] ), sum )
L161.GAINS_activities_agg <- L161.GAINS_activities_agg[ L161.GAINS_activities_agg$agg_sector != "", ]
L161.GAINS_activities_agg <- na.omit( L161.GAINS_activities_agg )

L161.GAINS_emfact <- L161.GAINS_emissions_agg
L161.GAINS_emfact$activity <- L161.GAINS_activities_agg$ACT[ match( vecpaste( L161.GAINS_emfact[ c( "TIMER_REGION", "agg_sector", "IDYEARS" ) ] ), vecpaste( L161.GAINS_activities_agg[ c( "TIMER_REGION", "agg_sector", "IDYEARS" ) ] ) ) ]
L161.GAINS_emfact$CLE_emfact <- L161.GAINS_emfact$CLE / L161.GAINS_emfact$activity
L161.GAINS_emfact$MFR_emfact <- L161.GAINS_emfact$MFR / L161.GAINS_emfact$activity
L161.GAINS_emfact$SLE_emfact <- L161.GAINS_emfact$SLE / L161.GAINS_emfact$activity
L161.GAINS_emfact <- L161.GAINS_emfact[ names( L161.GAINS_emfact ) %!in% c( "CLE", "MFR", "SLE", "activity" )  ]

#Replace SLE & MFR 2000 emissions factors with CLE emissions factors. They don't all start from the same value.
L161.GAINS_emfact$SLE_emfact[ L161.GAINS_emfact$IDYEARS == GAINS_base_year ] <- L161.GAINS_emfact$CLE_emfact[ L161.GAINS_emfact$IDYEARS == GAINS_base_year ]
L161.GAINS_emfact$MFR_emfact[ L161.GAINS_emfact$IDYEARS == GAINS_base_year ] <- L161.GAINS_emfact$CLE_emfact[ L161.GAINS_emfact$IDYEARS == GAINS_base_year ]

#Compute emissions factor scaler. These scalers are relative to the previous decade's numbers.
L161.GAINS_emfact_scaler <- subset( L161.GAINS_emfact, L161.GAINS_emfact$IDYEARS == GAINS_base_year )
L161.GAINS_emfact_scaler$CLE_scaler <- 1 
L161.GAINS_emfact_scaler$SLE_scaler <- 1 
L161.GAINS_emfact_scaler$MFR_scaler <- 1 
prev_y <- GAINS_base_year
  
for ( y in GAINS_years ) {
  temp <- subset( L161.GAINS_emfact, L161.GAINS_emfact$IDYEARS == y )
  prev_temp <- subset( L161.GAINS_emfact, L161.GAINS_emfact$IDYEARS == prev_y )
  
  temp$prev_CLE <- prev_temp$CLE_emfact[ match( vecpaste( temp[ c( "TIMER_REGION", "agg_sector", "POLL") ] ), vecpaste( prev_temp[ c( "TIMER_REGION", "agg_sector", "POLL") ]) ) ]
  temp$prev_SLE <- prev_temp$SLE_emfact[ match( vecpaste( temp[ c( "TIMER_REGION", "agg_sector", "POLL") ] ), vecpaste( prev_temp[ c( "TIMER_REGION", "agg_sector", "POLL") ]) ) ]
  temp$prev_MFR <- prev_temp$MFR_emfact[ match( vecpaste( temp[ c( "TIMER_REGION", "agg_sector", "POLL") ] ), vecpaste( prev_temp[ c( "TIMER_REGION", "agg_sector", "POLL") ]) ) ]
  
  temp$CLE_scaler <- temp$CLE_emfact / temp$prev_CLE
  temp$SLE_scaler <- temp$SLE_emfact / temp$prev_SLE
  temp$MFR_scaler <- temp$MFR_emfact / temp$prev_MFR 
  
  # Scalers should never be greater than 1
  temp$CLE_scaler[ temp$CLE_scaler > 1 ] <- 1  
  temp$SLE_scaler[ temp$SLE_scaler > 1 ] <- 1  
  temp$MFR_scaler[ temp$MFR_scaler > 1 ] <- 1  
  
  temp <- temp[ names( temp ) %!in% c( "prev_CLE", "prev_SLE", "prev_MFR" ) ]
  
  prev_y <- y
  L161.GAINS_emfact_scaler <- rbind( L161.GAINS_emfact_scaler, temp )
}

printlog( "Compute future emissions factors for GAINS scenarios" )
names( L114.bcoc_tgej_R_en_S_F_2000 )[ names( L114.bcoc_tgej_R_en_S_F_2000 ) == "X2000" ] <- "X2005"
L161.nonghg_tgej_R_en_S_F_2005 <- L111.nonghg_tgej_R_en_S_F_Yh[ c( "GCAM_region_ID", "Non.CO2", "supplysector", "subsector", "stub.technology", "X2005")]
L161.nonghg_tgej_R_en_S_F_2005 <- rbind( L161.nonghg_tgej_R_en_S_F_2005, L114.bcoc_tgej_R_en_S_F_2000 )
L161.nonghg_tgej_R_en_S_F_2005$TIMER_REGION <- A_region$GAINS_region[ match( L161.nonghg_tgej_R_en_S_F_2005$GCAM_region_ID, A_region$GCAM_region_ID)]
L161.nonghg_tgej_R_en_S_F_2005$agg_sector <- GCAM_sector_tech$IIASA_sector[ match( vecpaste(L161.nonghg_tgej_R_en_S_F_2005[ c( "supplysector", "subsector", "stub.technology" )] ), vecpaste( GCAM_sector_tech[ c( "supplysector", "subsector", "stub.technology" )] ))]
L161.nonghg_tgej_R_en_S_F_2005 <- subset( L161.nonghg_tgej_R_en_S_F_2005, !is.na( L161.nonghg_tgej_R_en_S_F_2005$agg_sector  ))

L161.em_fact_2010 <- L161.nonghg_tgej_R_en_S_F_2005
L161.em_fact_2010$year <- 2010
L161.em_fact_2010$CLE_scaler <- L161.GAINS_emfact_scaler$CLE_scaler[ match( vecpaste( L161.em_fact_2010[ c( "TIMER_REGION", "agg_sector", "Non.CO2", "year" )] ), vecpaste( L161.GAINS_emfact_scaler[ c( "TIMER_REGION", "agg_sector", "POLL", "IDYEARS")]) )]
L161.em_fact_2010$SLE_scaler <- L161.GAINS_emfact_scaler$SLE_scaler[ match( vecpaste( L161.em_fact_2010[ c( "TIMER_REGION", "agg_sector", "Non.CO2", "year" )] ), vecpaste( L161.GAINS_emfact_scaler[ c( "TIMER_REGION", "agg_sector", "POLL", "IDYEARS")]) )]
L161.em_fact_2010$MFR_scaler <- L161.GAINS_emfact_scaler$MFR_scaler[ match( vecpaste( L161.em_fact_2010[ c( "TIMER_REGION", "agg_sector", "Non.CO2", "year" )] ), vecpaste( L161.GAINS_emfact_scaler[ c( "TIMER_REGION", "agg_sector", "POLL", "IDYEARS")]) )]
L161.em_fact_2010$CLE_em_fact <- L161.em_fact_2010$X2005 * L161.em_fact_2010$CLE_scaler
L161.em_fact_2010$SLE_em_fact <- L161.em_fact_2010$X2005 * L161.em_fact_2010$SLE_scaler
L161.em_fact_2010$MFR_em_fact <- L161.em_fact_2010$X2005 * L161.em_fact_2010$MFR_scaler
L161.em_fact_2010 <- L161.em_fact_2010[ names( L161.em_fact_2010 ) %!in% c( "X2005", "CLE_scaler", "SLE_scaler", "MFR_scaler" ) ]

L161.em_fact_2020 <- L161.em_fact_2010
L161.em_fact_2020$year <- 2020
L161.em_fact_2020$CLE_scaler <- L161.GAINS_emfact_scaler$CLE_scaler[ match( vecpaste( L161.em_fact_2020[ c( "TIMER_REGION", "agg_sector", "Non.CO2", "year" )] ), vecpaste( L161.GAINS_emfact_scaler[ c( "TIMER_REGION", "agg_sector", "POLL", "IDYEARS")]) )]
L161.em_fact_2020$SLE_scaler <- L161.GAINS_emfact_scaler$SLE_scaler[ match( vecpaste( L161.em_fact_2020[ c( "TIMER_REGION", "agg_sector", "Non.CO2", "year" )] ), vecpaste( L161.GAINS_emfact_scaler[ c( "TIMER_REGION", "agg_sector", "POLL", "IDYEARS")]) )]
L161.em_fact_2020$MFR_scaler <- L161.GAINS_emfact_scaler$MFR_scaler[ match( vecpaste( L161.em_fact_2020[ c( "TIMER_REGION", "agg_sector", "Non.CO2", "year" )] ), vecpaste( L161.GAINS_emfact_scaler[ c( "TIMER_REGION", "agg_sector", "POLL", "IDYEARS")]) )]
L161.em_fact_2020$CLE_em_fact <- L161.em_fact_2020$CLE_em_fact * L161.em_fact_2020$CLE_scaler
L161.em_fact_2020$SLE_em_fact <- L161.em_fact_2020$SLE_em_fact * L161.em_fact_2020$SLE_scaler
L161.em_fact_2020$MFR_em_fact <- L161.em_fact_2020$MFR_em_fact * L161.em_fact_2020$MFR_scaler
L161.em_fact_2020 <- L161.em_fact_2020[ names( L161.em_fact_2020 ) %!in% c( "CLE_scaler", "SLE_scaler", "MFR_scaler" )]

L161.em_fact_2030 <- L161.em_fact_2020
L161.em_fact_2030$year <- 2030
L161.em_fact_2030$CLE_scaler <- L161.GAINS_emfact_scaler$CLE_scaler[ match( vecpaste( L161.em_fact_2030[ c( "TIMER_REGION", "agg_sector", "Non.CO2", "year" )] ), vecpaste( L161.GAINS_emfact_scaler[ c( "TIMER_REGION", "agg_sector", "POLL", "IDYEARS")]) )]
L161.em_fact_2030$SLE_scaler <- L161.GAINS_emfact_scaler$SLE_scaler[ match( vecpaste( L161.em_fact_2030[ c( "TIMER_REGION", "agg_sector", "Non.CO2", "year" )] ), vecpaste( L161.GAINS_emfact_scaler[ c( "TIMER_REGION", "agg_sector", "POLL", "IDYEARS")]) )]
L161.em_fact_2030$MFR_scaler <- L161.GAINS_emfact_scaler$MFR_scaler[ match( vecpaste( L161.em_fact_2030[ c( "TIMER_REGION", "agg_sector", "Non.CO2", "year" )] ), vecpaste( L161.GAINS_emfact_scaler[ c( "TIMER_REGION", "agg_sector", "POLL", "IDYEARS")]) )]
L161.em_fact_2030$CLE_em_fact <- L161.em_fact_2030$CLE_em_fact * L161.em_fact_2030$CLE_scaler
L161.em_fact_2030$SLE_em_fact <- L161.em_fact_2030$SLE_em_fact * L161.em_fact_2030$SLE_scaler
L161.em_fact_2030$MFR_em_fact <- L161.em_fact_2030$MFR_em_fact * L161.em_fact_2030$MFR_scaler
L161.em_fact_2030 <- L161.em_fact_2030[ names( L161.em_fact_2030 ) %!in% c( "CLE_scaler", "SLE_scaler", "MFR_scaler" )]

printlog( "Determine region groupings" )
L161.pcgdp_2010 <- subset( L102.pcgdp_thous90USD_SSP_R_Y, L102.pcgdp_thous90USD_SSP_R_Y$scenario == "SSP4" )
L161.pcgdp_2010 <- L161.pcgdp_2010[ names( L161.pcgdp_2010 ) %in% c( "GCAM_region_ID", "X2010" ) ]
L161.pcgdp_2010$X2010 <- L161.pcgdp_2010$X2010 * conv_1990_2010_USD
L161.highmed_reg <- L161.pcgdp_2010$GCAM_region_ID[ L161.pcgdp_2010$X2010 >= lo_pcgdp ]
L161.low_reg <- L161.pcgdp_2010$GCAM_region_ID[ L161.pcgdp_2010$X2010 < lo_pcgdp ]

L161.coal_so2 <- subset( L161.em_fact_2030, L161.em_fact_2030$agg_sector == "elec_coal" & L161.em_fact_2030$Non.CO2 == "SO2" & L161.em_fact_2030$GCAM_region_ID %in% L161.highmed_reg )
L161.highmed_strong_reg <- L161.coal_so2$GCAM_region_ID[ L161.coal_so2$CLE_em_fact <= coal_so2_thresshold ]
L161.highmed_weak_reg <- L161.coal_so2$GCAM_region_ID[ L161.coal_so2$CLE_em_fact > coal_so2_thresshold ]

printlog( "SSPs 1 and 5: High and Medium Income Countries")
#High-Med Income Countries. 2030 = 0.75*CLE2030; 2050 = SLE2030; 2100 = MFR
L161.SSP15_HM_EF <- subset( L161.em_fact_2010, L161.em_fact_2010$GCAM_region_ID %in% L161.highmed_reg )
L161.SSP15_HM_EF <- na.omit( L161.SSP15_HM_EF )
L161.SSP15_HM_EF <- L161.SSP15_HM_EF[ names( L161.SSP15_HM_EF ) %!in% c( "TIMER_REGION", "year", "SLE_em_fact", "MFR_em_fact" )]
names( L161.SSP15_HM_EF )[ names( L161.SSP15_HM_EF ) == "CLE_em_fact" ] <- "X2010"

# Map 2030 information. Then, ensure that it is not bigger than 2010 ( emissions factors can only decline )
L161.SSP15_HM_EF$X2030 <- 0.75 * L161.em_fact_2030$CLE_em_fact[ match( vecpaste( L161.SSP15_HM_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP15_HM_EF$X2030[ is.na( L161.SSP15_HM_EF$X2030 ) ] <- L161.SSP15_HM_EF$X2010[ is.na( L161.SSP15_HM_EF$X2030 ) ]
L161.SSP15_HM_EF$X2030[ L161.SSP15_HM_EF$X2030 > L161.SSP15_HM_EF$X2010 ] <- L161.SSP15_HM_EF$X2010[ L161.SSP15_HM_EF$X2030 > L161.SSP15_HM_EF$X2010 ]

# Map 2050 information. Then, ensure that it is not bigger than 2030 ( emissions factors can only decline )
L161.SSP15_HM_EF$X2050 <- L161.em_fact_2030$SLE_em_fact[ match( vecpaste( L161.SSP15_HM_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP15_HM_EF$X2050[ is.na( L161.SSP15_HM_EF$X2050 ) ] <- L161.SSP15_HM_EF$X2030[ is.na( L161.SSP15_HM_EF$X2050 ) ]
L161.SSP15_HM_EF$X2050[ L161.SSP15_HM_EF$X2050 > L161.SSP15_HM_EF$X2030 ] <- L161.SSP15_HM_EF$X2030[ L161.SSP15_HM_EF$X2050 > L161.SSP15_HM_EF$X2030 ]

# Map 2095 information. Then, ensure that it is not bigger than 2050 ( emissions factors can only decline )
L161.SSP15_HM_EF$X2100 <- L161.em_fact_2030$MFR_em_fact[ match( vecpaste( L161.SSP15_HM_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP15_HM_EF$X2100[ is.na( L161.SSP15_HM_EF$X2100 ) ] <- L161.SSP15_HM_EF$X2050[ is.na( L161.SSP15_HM_EF$X2100 ) ]
L161.SSP15_HM_EF$X2100[ L161.SSP15_HM_EF$X2100 > L161.SSP15_HM_EF$X2050 ] <- L161.SSP15_HM_EF$X2050[ L161.SSP15_HM_EF$X2100 > L161.SSP15_HM_EF$X2050 ]

printlog( "SSPs 1 and 5: Low Income Countries")
#Low Income Countries. 2030 = CLE2030; 2050 = W.Eur CLE2030; 2100 = SLE2030
L161.SSP15_L_EF <- subset( L161.em_fact_2010, L161.em_fact_2010$GCAM_region_ID %in% L161.low_reg )
L161.SSP15_L_EF <- na.omit( L161.SSP15_L_EF )
L161.SSP15_L_EF <- L161.SSP15_L_EF[ names( L161.SSP15_L_EF ) %!in% c( "TIMER_REGION", "year", "SLE_em_fact", "MFR_em_fact" )]
names( L161.SSP15_L_EF )[ names( L161.SSP15_L_EF ) == "CLE_em_fact" ] <- "X2010"

# Map 2030 information. Then, ensure that it is not bigger than 2010 ( emissions factors can only decline )
L161.SSP15_L_EF$X2030 <- L161.em_fact_2030$CLE_em_fact[ match( vecpaste( L161.SSP15_L_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP15_L_EF$X2030[ is.na( L161.SSP15_L_EF$X2030 ) ] <- L161.SSP15_L_EF$X2010[ is.na( L161.SSP15_L_EF$X2030 ) ]
L161.SSP15_L_EF$X2030[ L161.SSP15_L_EF$X2030 > L161.SSP15_L_EF$X2010 ] <- L161.SSP15_L_EF$X2010[ L161.SSP15_L_EF$X2030 > L161.SSP15_L_EF$X2010 ]

# Map 2050 information. Then, ensure that it is not bigger than 2030 ( emissions factors can only decline )
L161.SSP15_L_EF$X2050 <- L161.em_fact_2030$CLE_em_fact[ match( paste( ssp_marker_region, vecpaste( L161.SSP15_L_EF[ c( "agg_sector", "Non.CO2" )]), sep=" "), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP15_L_EF$X2050[ is.na( L161.SSP15_L_EF$X2050 ) ] <- L161.SSP15_L_EF$X2030[ is.na( L161.SSP15_L_EF$X2050 ) ]
L161.SSP15_L_EF$X2050[ L161.SSP15_L_EF$X2050 > L161.SSP15_L_EF$X2030 ] <- L161.SSP15_L_EF$X2030[ L161.SSP15_L_EF$X2050 > L161.SSP15_L_EF$X2030 ]

# Map 2095 information. Then, ensure that it is not bigger than 2050 ( emissions factors can only decline )
L161.SSP15_L_EF$X2100 <- L161.em_fact_2030$SLE_em_fact[ match( vecpaste( L161.SSP15_L_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP15_L_EF$X2100[ is.na( L161.SSP15_L_EF$X2100 ) ] <- L161.SSP15_L_EF$X2050[ is.na( L161.SSP15_L_EF$X2100 ) ]
L161.SSP15_L_EF$X2100[ L161.SSP15_L_EF$X2100 > L161.SSP15_L_EF$X2050 ] <- L161.SSP15_L_EF$X2050[ L161.SSP15_L_EF$X2100 > L161.SSP15_L_EF$X2050 ]

#Combine dataframes
L161.SSP15_EF <- rbind( L161.SSP15_HM_EF, L161.SSP15_L_EF )

printlog( "SSPs 2: High and Medium Income Countries w/ Strong Pollution Policies")
#High-Med Income Countries. 2030 = CLE2030; 2050 = SLE2030; 2100 = min( SLE2030 )
L161.SSP2_HM_EF <- subset( L161.em_fact_2010, L161.em_fact_2010$GCAM_region_ID %in% L161.highmed_strong_reg )
L161.SSP2_HM_EF <- na.omit( L161.SSP2_HM_EF )
L161.SSP2_HM_EF <- L161.SSP2_HM_EF[ names( L161.SSP2_HM_EF ) %!in% c( "TIMER_REGION", "year", "SLE_em_fact", "MFR_em_fact" )]
names( L161.SSP2_HM_EF )[ names( L161.SSP2_HM_EF ) == "CLE_em_fact" ] <- "X2010"

# Map 2030 information. Then, ensure that it is not bigger than 2010 ( emissions factors can only decline )
L161.SSP2_HM_EF$X2030 <- L161.em_fact_2030$CLE_em_fact[ match( vecpaste( L161.SSP2_HM_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP2_HM_EF$X2030[ is.na( L161.SSP2_HM_EF$X2030 ) ] <- L161.SSP2_HM_EF$X2010[ is.na( L161.SSP2_HM_EF$X2030 ) ]
L161.SSP2_HM_EF$X2030[ L161.SSP2_HM_EF$X2030 > L161.SSP2_HM_EF$X2010 ] <- L161.SSP2_HM_EF$X2010[ L161.SSP2_HM_EF$X2030 > L161.SSP2_HM_EF$X2010 ]

# Map 2050 information. Then, ensure that it is not bigger than 2030 ( emissions factors can only decline )
L161.SSP2_HM_EF$X2050 <- L161.em_fact_2030$SLE_em_fact[ match( vecpaste( L161.SSP2_HM_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP2_HM_EF$X2050[ is.na( L161.SSP2_HM_EF$X2050 ) ] <- L161.SSP2_HM_EF$X2030[ is.na( L161.SSP2_HM_EF$X2050 ) ]
L161.SSP2_HM_EF$X2050[ L161.SSP2_HM_EF$X2050 > L161.SSP2_HM_EF$X2030 ] <- L161.SSP2_HM_EF$X2030[ L161.SSP2_HM_EF$X2050 > L161.SSP2_HM_EF$X2030 ]

# Map 2095 information. Then, ensure that it is not bigger than 2050 ( emissions factors can only decline )
L161.SSP2_HM_EF$X2100 <- L161.em_fact_2030$SLE_em_fact[ match( vecpaste( L161.SSP2_HM_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP2_HM_EF$X2100[ is.na( L161.SSP2_HM_EF$X2100 ) ] <- L161.SSP2_HM_EF$X2050[ is.na( L161.SSP2_HM_EF$X2100 ) ]
L161.SSP2_HM_EF$X2100[ L161.SSP2_HM_EF$X2100 > L161.SSP2_HM_EF$X2050 ] <- L161.SSP2_HM_EF$X2050[ L161.SSP2_HM_EF$X2100 > L161.SSP2_HM_EF$X2050 ]

printlog( "SSPs 2: High and Medium Income Countries w/o Strong Pollution Policies")
#High-Med Income Countries. 2030 = CLE2030; 2050 = min( CLE2030 ); 2100 = WEU SLE2030
L161.SSP2_HM2_EF <- subset( L161.em_fact_2010, L161.em_fact_2010$GCAM_region_ID %in% L161.highmed_weak_reg )
L161.SSP2_HM2_EF <- na.omit( L161.SSP2_HM2_EF )
L161.SSP2_HM2_EF <- L161.SSP2_HM2_EF[ names( L161.SSP2_HM2_EF ) %!in% c( "TIMER_REGION", "year", "SLE_em_fact", "MFR_em_fact" )]
names( L161.SSP2_HM2_EF )[ names( L161.SSP2_HM2_EF ) == "CLE_em_fact" ] <- "X2010"

# Map 2030 information. Then, ensure that it is not bigger than 2010 ( emissions factors can only decline )
L161.SSP2_HM2_EF$X2030 <- L161.em_fact_2030$CLE_em_fact[ match( vecpaste( L161.SSP2_HM2_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP2_HM2_EF$X2030[ is.na( L161.SSP2_HM2_EF$X2030 ) ] <- L161.SSP2_HM2_EF$X2010[ is.na( L161.SSP2_HM2_EF$X2030 ) ]
L161.SSP2_HM2_EF$X2030[ L161.SSP2_HM2_EF$X2030 > L161.SSP2_HM2_EF$X2010 ] <- L161.SSP2_HM2_EF$X2010[ L161.SSP2_HM2_EF$X2030 > L161.SSP2_HM2_EF$X2010 ]

# Map 2050 information. Then, ensure that it is not bigger than 2030 ( emissions factors can only decline )
L161.SSP2_HM2_EF$X2050 <- L161.em_fact_2030$CLE_em_fact[ match( vecpaste( L161.SSP2_HM2_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP2_HM2_EF$X2050[ is.na( L161.SSP2_HM2_EF$X2050 ) ] <- L161.SSP2_HM2_EF$X2030[ is.na( L161.SSP2_HM2_EF$X2050 ) ]
L161.SSP2_HM2_EF$X2050[ L161.SSP2_HM2_EF$X2050 > L161.SSP2_HM2_EF$X2030 ] <- L161.SSP2_HM2_EF$X2030[ L161.SSP2_HM2_EF$X2050 > L161.SSP2_HM2_EF$X2030 ]

# Map 2095 information. Then, ensure that it is not bigger than 2050 ( emissions factors can only decline )
L161.SSP2_HM2_EF$X2100 <- L161.em_fact_2030$SLE_em_fact[ match( paste( ssp_marker_region, vecpaste( L161.SSP2_HM2_EF[ c( "agg_sector", "Non.CO2" )]), sep=" "), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP2_HM2_EF$X2100[ is.na( L161.SSP2_HM2_EF$X2100 ) ] <- L161.SSP2_HM2_EF$X2050[ is.na( L161.SSP2_HM2_EF$X2100 ) ]
L161.SSP2_HM2_EF$X2100[ L161.SSP2_HM2_EF$X2100 > L161.SSP2_HM2_EF$X2050 ] <- L161.SSP2_HM2_EF$X2050[ L161.SSP2_HM2_EF$X2100 > L161.SSP2_HM2_EF$X2050 ]

printlog( "SSPs 2: Low Income Countries")
#Low Income Countries. 2030 = CLE2030; 2050 = W.Eur CLE2030; 2100 = SLE2030
L161.SSP2_L_EF <- subset( L161.em_fact_2010, L161.em_fact_2010$GCAM_region_ID %in% L161.low_reg )
L161.SSP2_L_EF <- na.omit( L161.SSP2_L_EF )
L161.SSP2_L_EF <- L161.SSP2_L_EF[ names( L161.SSP2_L_EF ) %!in% c( "TIMER_REGION", "year", "SLE_em_fact", "MFR_em_fact" )]
names( L161.SSP2_L_EF )[ names( L161.SSP2_L_EF ) == "CLE_em_fact" ] <- "X2010"

# Map 2030 information. Then, ensure that it is not bigger than 2010 ( emissions factors can only decline )
L161.SSP2_L_EF$X2030 <- L161.em_fact_2020$CLE_em_fact[ match( vecpaste( L161.SSP2_L_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2020[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP2_L_EF$X2030[ is.na( L161.SSP2_L_EF$X2030 ) ] <- L161.SSP2_L_EF$X2010[ is.na( L161.SSP2_L_EF$X2030 ) ]
L161.SSP2_L_EF$X2030[ L161.SSP2_L_EF$X2030 > L161.SSP2_L_EF$X2010 ] <- L161.SSP2_L_EF$X2010[ L161.SSP2_L_EF$X2030 > L161.SSP2_L_EF$X2010 ]

# Map 2050 information. Then, ensure that it is not bigger than 2030 ( emissions factors can only decline )
L161.SSP2_L_EF$X2050 <- L161.em_fact_2030$CLE_em_fact[ match( vecpaste( L161.SSP2_L_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP2_L_EF$X2050[ is.na( L161.SSP2_L_EF$X2050 ) ] <- L161.SSP2_L_EF$X2030[ is.na( L161.SSP2_L_EF$X2050 ) ]
L161.SSP2_L_EF$X2050[ L161.SSP2_L_EF$X2050 > L161.SSP2_L_EF$X2030 ] <- L161.SSP2_L_EF$X2030[ L161.SSP2_L_EF$X2050 > L161.SSP2_L_EF$X2030 ]

# Map 2095 information. Then, ensure that it is not bigger than 2050 ( emissions factors can only decline )
L161.SSP2_L_EF$X2100 <- L161.em_fact_2030$CLE_em_fact[ match( paste( ssp_marker_region, vecpaste( L161.SSP2_L_EF[ c( "agg_sector", "Non.CO2" )]), sep=" "), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP2_L_EF$X2100[ is.na( L161.SSP2_L_EF$X2100 ) ] <- L161.SSP2_L_EF$X2050[ is.na( L161.SSP2_L_EF$X2100 ) ]
L161.SSP2_L_EF$X2100[ L161.SSP2_L_EF$X2100 > L161.SSP2_L_EF$X2050 ] <- L161.SSP2_L_EF$X2050[ L161.SSP2_L_EF$X2100 > L161.SSP2_L_EF$X2050 ]

#Combine dataframes
L161.SSP2_EF <- rbind( L161.SSP2_HM_EF, L161.SSP2_HM2_EF, L161.SSP2_L_EF )

printlog( "SSPs 3 and 4: High and Medium Income Countries")
#High-Med Income Countries. 2030 = CLE2020; 2050 = CLE2030; 2100 = SLE2030
L161.SSP34_HM_EF <- subset( L161.em_fact_2010, L161.em_fact_2010$GCAM_region_ID %in% L161.highmed_reg )
L161.SSP34_HM_EF <- na.omit( L161.SSP34_HM_EF )
L161.SSP34_HM_EF <- L161.SSP34_HM_EF[ names( L161.SSP34_HM_EF ) %!in% c( "TIMER_REGION", "year", "SLE_em_fact", "MFR_em_fact" )]
names( L161.SSP34_HM_EF )[ names( L161.SSP34_HM_EF ) == "CLE_em_fact" ] <- "X2010"

# Map 2030 information. Then, ensure that it is not bigger than 2010 ( emissions factors can only decline )
L161.SSP34_HM_EF$X2030 <- L161.em_fact_2020$CLE_em_fact[ match( vecpaste( L161.SSP34_HM_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2020[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP34_HM_EF$X2030[ is.na( L161.SSP34_HM_EF$X2030 ) ] <- L161.SSP34_HM_EF$X2010[ is.na( L161.SSP34_HM_EF$X2030 ) ]
L161.SSP34_HM_EF$X2030[ L161.SSP34_HM_EF$X2030 > L161.SSP34_HM_EF$X2010 ] <- L161.SSP34_HM_EF$X2010[ L161.SSP34_HM_EF$X2030 > L161.SSP34_HM_EF$X2010 ]

# Map 2050 information. Then, ensure that it is not bigger than 2030 ( emissions factors can only decline )
L161.SSP34_HM_EF$X2050 <- L161.em_fact_2030$CLE_em_fact[ match( vecpaste( L161.SSP34_HM_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP34_HM_EF$X2050[ is.na( L161.SSP34_HM_EF$X2050 ) ] <- L161.SSP34_HM_EF$X2030[ is.na( L161.SSP34_HM_EF$X2050 ) ]
L161.SSP34_HM_EF$X2050[ L161.SSP34_HM_EF$X2050 > L161.SSP34_HM_EF$X2030 ] <- L161.SSP34_HM_EF$X2030[ L161.SSP34_HM_EF$X2050 > L161.SSP34_HM_EF$X2030 ]

# Map 2095 information. Then, ensure that it is not bigger than 2050 ( emissions factors can only decline )
L161.SSP34_HM_EF$X2100 <- L161.em_fact_2030$SLE_em_fact[ match( vecpaste( L161.SSP34_HM_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP34_HM_EF$X2100[ is.na( L161.SSP34_HM_EF$X2100 ) ] <- L161.SSP34_HM_EF$X2050[ is.na( L161.SSP34_HM_EF$X2100 ) ]
L161.SSP34_HM_EF$X2100[ L161.SSP34_HM_EF$X2100 > L161.SSP34_HM_EF$X2050 ] <- L161.SSP34_HM_EF$X2050[ L161.SSP34_HM_EF$X2100 > L161.SSP34_HM_EF$X2050 ]

printlog( "SSPs 3 and 4: Low Income Countries")
#Low Income Countries. 2030 = CLE2030; 2050 = W.Eur CLE2030; 2100 = SLE2030
L161.SSP34_L_EF <- subset( L161.em_fact_2010, L161.em_fact_2010$GCAM_region_ID %!in% L161.highmed_reg )
L161.SSP34_L_EF <- na.omit( L161.SSP34_L_EF )
L161.SSP34_L_EF <- L161.SSP34_L_EF[ names( L161.SSP34_L_EF ) %!in% c( "TIMER_REGION", "year", "SLE_em_fact", "MFR_em_fact" )]
names( L161.SSP34_L_EF )[ names( L161.SSP34_L_EF ) == "CLE_em_fact" ] <- "X2010"

# Map 2030 information. Then, ensure that it is not bigger than 2010 ( emissions factors can only decline )
L161.SSP34_L_EF$X2030 <- L161.SSP34_L_EF$X2010

# Map 2050 information. Then, ensure that it is not bigger than 2030 ( emissions factors can only decline )
L161.SSP34_L_EF$X2050 <- L161.em_fact_2030$CLE_em_fact[ match( vecpaste( L161.SSP34_L_EF[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )]), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP34_L_EF$X2050[ is.na( L161.SSP34_L_EF$X2050 ) ] <- L161.SSP34_L_EF$X2030[ is.na( L161.SSP34_L_EF$X2050 ) ]
L161.SSP34_L_EF$X2050[ L161.SSP34_L_EF$X2050 > L161.SSP34_L_EF$X2030 ] <- L161.SSP34_L_EF$X2030[ L161.SSP34_L_EF$X2050 > L161.SSP34_L_EF$X2030 ]

# Map 2095 information. Then, ensure that it is not bigger than 2050 ( emissions factors can only decline )
L161.SSP34_L_EF$X2100 <- L161.em_fact_2030$CLE_em_fact[ match( paste( ssp_marker_region, vecpaste( L161.SSP34_L_EF[ c( "agg_sector", "Non.CO2" )]), sep=" "), vecpaste( L161.em_fact_2030[ c( "GCAM_region_ID", "agg_sector", "Non.CO2" )] ) )]
L161.SSP34_L_EF$X2100[ is.na( L161.SSP34_L_EF$X2100 ) ] <- L161.SSP34_L_EF$X2050[ is.na( L161.SSP34_L_EF$X2100 ) ]
L161.SSP34_L_EF$X2100[ L161.SSP34_L_EF$X2100 > L161.SSP34_L_EF$X2050 ] <- L161.SSP34_L_EF$X2050[ L161.SSP34_L_EF$X2100 > L161.SSP34_L_EF$X2050 ]

#Combine dataframes
L161.SSP34_EF <- rbind( L161.SSP34_HM_EF, L161.SSP34_L_EF )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L161.SSP15_EF <- c( "Emissions factors for SSP1 & 5 by GCAM region / sector / technology / future year", "Unit = Tg / EJ" )
comments.L161.SSP2_EF <- c( "Emissions factors for SSP2 by GCAM region / sector / technology / future year", "Unit = Tg / EJ" )
comments.L161.SSP34_EF <- c( "Emissions factors for SSP3 & 4 by GCAM region / sector / technology / future year", "Unit = Tg / EJ" )

#write tables as CSV files
writedata( L161.SSP15_EF, domain="EMISSIONS_LEVEL1_DATA", fn="L161.SSP15_EF", comments=comments.L161.SSP15_EF )
writedata( L161.SSP2_EF, domain="EMISSIONS_LEVEL1_DATA", fn="L161.SSP2_EF", comments=comments.L161.SSP2_EF )
writedata( L161.SSP34_EF, domain="EMISSIONS_LEVEL1_DATA", fn="L161.SSP34_EF", comments=comments.L161.SSP34_EF )

# Every script should finish with this line
logstop()
