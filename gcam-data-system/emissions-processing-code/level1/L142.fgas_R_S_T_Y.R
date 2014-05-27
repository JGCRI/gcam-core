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
logstart( "L142.fgas_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical F-gas emissions by GCAM technology, computed from EDGAR emissions data and EPA emissions shares" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
GWP <- readdata( "EMISSIONS_ASSUMPTIONS", "A41.GWP" )
EDGAR_PFC <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_PFC" )
EDGAR_HFC <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_HFC" )
L141.fgas_pct_USA_S_2005 <- readdata( "EMISSIONS_LEVEL1_DATA", "L141.fgas_pct_USA_S_2005" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Map EDGAR HFC emissions to GCAM technologies" )
#First, set up data frame
L142.hfc_tg_R_S_Yh.melt <- L141.fgas_pct_USA_S_2005
L142.hfc_tg_R_S_Yh.melt <- subset( L142.hfc_tg_R_S_Yh.melt, L142.hfc_tg_R_S_Yh.melt$HFC_PFC == "HFC" )
L142.hfc_tg_R_S_Yh.melt <- repeat_and_add_vector( L142.hfc_tg_R_S_Yh.melt, "GCAM_region_ID", GCAM_region_names$GCAM_region_ID )
L142.hfc_tg_R_S_Yh.melt <- repeat_and_add_vector( L142.hfc_tg_R_S_Yh.melt, "xyear", X_EDGAR_historical_years )

#Then, prepare EDGAR data for use
L142.EDGAR_HFC <- EDGAR_HFC 
L142.EDGAR_HFC$EDGAR_agg_sector <- EDGAR_sector$agg_sector[ match( L142.EDGAR_HFC$IPCC_description, EDGAR_sector$IPCC_description )]
L142.EDGAR_HFC$iso <- EDGAR_nation$iso[ match( L142.EDGAR_HFC$ISO_A3, EDGAR_nation$ISO_A3 )]
L142.EDGAR_HFC$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L142.EDGAR_HFC$iso, iso_GCAM_regID$iso )]   
L142.EDGAR_HFC <- L142.EDGAR_HFC[ c( "GCAM_region_ID", "EDGAR_agg_sector", X_EDGAR_historical_years) ]
L142.EDGAR_HFC.melt <- melt( L142.EDGAR_HFC, id.vars=c( "GCAM_region_ID", "EDGAR_agg_sector" ) )
L142.EDGAR_HFC.melt <- aggregate( L142.EDGAR_HFC.melt$value, by=as.list( L142.EDGAR_HFC.melt[ c( "GCAM_region_ID", "EDGAR_agg_sector", "variable" ) ] ), sum )
names( L142.EDGAR_HFC.melt )[ names( L142.EDGAR_HFC.melt ) == "x" ] <- "EDGAR_emissions"

#Now, map in EDGAR emissions & compute tech/gas specific emissions
L142.hfc_tg_R_S_Yh.melt$EDGAR_emissions <- L142.EDGAR_HFC.melt$EDGAR_emissions[ match( vecpaste( L142.hfc_tg_R_S_Yh.melt[ c( "GCAM_region_ID", "EDGAR_agg_sector", "xyear")]), vecpaste( L142.EDGAR_HFC.melt[ c( "GCAM_region_ID", "EDGAR_agg_sector", "variable" )] ))]
L142.hfc_tg_R_S_Yh.melt$EDGAR_emissions[ is.na( L142.hfc_tg_R_S_Yh.melt$EDGAR_emissions )] <- 0
L142.hfc_tg_R_S_Yh.melt$emissions <- L142.hfc_tg_R_S_Yh.melt$EDGAR_emissions * L142.hfc_tg_R_S_Yh.melt$pct

printlog( "Map EDGAR PFC emissions to GCAM technologies" )
#First, set up data frame
L142.pfc_tg_R_S_Yh.melt <- L141.fgas_pct_USA_S_2005
L142.pfc_tg_R_S_Yh.melt <- subset( L142.pfc_tg_R_S_Yh.melt, L142.pfc_tg_R_S_Yh.melt$HFC_PFC == "PFC" )
L142.pfc_tg_R_S_Yh.melt <- repeat_and_add_vector( L142.pfc_tg_R_S_Yh.melt, "GCAM_region_ID", GCAM_region_names$GCAM_region_ID )
L142.pfc_tg_R_S_Yh.melt <- repeat_and_add_vector( L142.pfc_tg_R_S_Yh.melt, "xyear", X_EDGAR_historical_years )

#Then, prepare EDGAR data for use
L142.EDGAR_PFC <- EDGAR_PFC 
L142.EDGAR_PFC$EDGAR_agg_sector <- EDGAR_sector$agg_sector[ match( L142.EDGAR_PFC$IPCC_description, EDGAR_sector$IPCC_description )]
L142.EDGAR_PFC$iso <- EDGAR_nation$iso[ match( L142.EDGAR_PFC$ISO_A3, EDGAR_nation$ISO_A3 )]
L142.EDGAR_PFC$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L142.EDGAR_PFC$iso, iso_GCAM_regID$iso )]   
L142.EDGAR_PFC <- L142.EDGAR_PFC[ c( "GCAM_region_ID", "EDGAR_agg_sector", X_EDGAR_historical_years) ]
L142.EDGAR_PFC.melt <- melt( L142.EDGAR_PFC, id.vars=c( "GCAM_region_ID", "EDGAR_agg_sector" ) )
L142.EDGAR_PFC.melt <- aggregate( L142.EDGAR_PFC.melt$value, by=as.list( L142.EDGAR_PFC.melt[ c( "GCAM_region_ID", "EDGAR_agg_sector", "variable" ) ] ), sum )
names( L142.EDGAR_PFC.melt )[ names( L142.EDGAR_PFC.melt ) == "x" ] <- "EDGAR_emissions"

#Now, map in emissions
L142.pfc_tg_R_S_Yh.melt$EDGAR_emissions <- L142.EDGAR_PFC.melt$EDGAR_emissions[ match( vecpaste( L142.pfc_tg_R_S_Yh.melt[ c( "GCAM_region_ID", "EDGAR_agg_sector", "xyear")]), vecpaste( L142.EDGAR_PFC.melt[ c( "GCAM_region_ID", "EDGAR_agg_sector", "variable" )] ))]
L142.pfc_tg_R_S_Yh.melt$EDGAR_emissions[ is.na( L142.pfc_tg_R_S_Yh.melt$EDGAR_emissions )] <- 0
L142.pfc_tg_R_S_Yh.melt$emissions <- L142.pfc_tg_R_S_Yh.melt$EDGAR_emissions * L142.pfc_tg_R_S_Yh.melt$pct

#Combine dataframes, convert to Tg of gas, & reshape
L142.fgas_tg_R_S_Yh.melt <- rbind( L142.hfc_tg_R_S_Yh.melt, L142.pfc_tg_R_S_Yh.melt )
L142.fgas_tg_R_S_Yh.melt <- na.omit( L142.fgas_tg_R_S_Yh.melt )
L142.fgas_tg_R_S_Yh.melt$GWP <- GWP$GWP[ match( L142.fgas_tg_R_S_Yh.melt$Non.CO2, GWP$Gas )]
L142.fgas_tg_R_S_Yh.melt$emissions <- L142.fgas_tg_R_S_Yh.melt$emissions / L142.fgas_tg_R_S_Yh.melt$GWP
L142.fgas_tg_R_S_Yh.melt <- aggregate( L142.fgas_tg_R_S_Yh.melt$emissions, by=as.list( L142.fgas_tg_R_S_Yh.melt[ c( "GCAM_region_ID", "supplysector", "subsector", "stub.technology", "Non.CO2", "xyear" ) ]), sum)
L142.fgas_tg_R_S_Yh <- dcast( L142.fgas_tg_R_S_Yh.melt, GCAM_region_ID + supplysector + subsector + stub.technology + Non.CO2 ~ xyear, value.var=c( "x" ))
L142.fgas_tg_R_S_Yh[ is.na( L142.fgas_tg_R_S_Yh ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L142.fgas_tg_R_S_Yh <- c( "F-gas emissions by region / sector / gas / historical year", "Unit = Gg" )

#write tables as CSV files
writedata( L142.fgas_tg_R_S_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L142.fgas_tg_R_S_Yh", comments=comments.L142.fgas_tg_R_S_Yh )

# Every script should finish with this line
logstop()
