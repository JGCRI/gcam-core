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
logstart( "L142.pfc_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical HFC emissions by GCAM technology, computed from EDGAR emissions data" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
GCAM_tech <- readdata( "EMISSIONS_MAPPINGS", "gcam_fgas_tech" )
Other_F <- readdata( "EMISSIONS_MAPPINGS", "other_f_gases" )
L144.in_EJ_R_bld_serv_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L144.in_EJ_R_bld_serv_F_Yh" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector_fgas" )
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
GWP <- readdata( "EMISSIONS_ASSUMPTIONS", "A41.GWP" )
EDGAR_SF6 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_SF6" )
EDGAR_C2F6 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_C2F6" )
EDGAR_CF4 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CF4" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Map EDGAR HFC emissions to GCAM technologies" )
#First, bind all gases together
EDGAR_SF6$Non.CO2 <- "SF6"
EDGAR_C2F6$Non.CO2 <- "C2F6"
EDGAR_CF4$Non.CO2 <- "CF4"
L142.EDGAR_PFC <- rbind( EDGAR_SF6, EDGAR_C2F6, EDGAR_CF4 )  

#Then, prepare EDGAR data for use
L142.EDGAR_PFC$EDGAR_agg_sector <- EDGAR_sector$agg_sector[ match( L142.EDGAR_PFC$IPCC_description, EDGAR_sector$IPCC_description )]
L142.EDGAR_PFC$iso <- EDGAR_nation$iso[ match( L142.EDGAR_PFC$ISO_A3, EDGAR_nation$ISO_A3 )]
L142.EDGAR_PFC$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L142.EDGAR_PFC$iso, iso_GCAM_regID$iso )]   
L142.EDGAR_PFC <- L142.EDGAR_PFC[ c( "GCAM_region_ID", "EDGAR_agg_sector", "Non.CO2", X_EDGAR_historical_years) ]
L142.EDGAR_PFC_R_S_T_Yh.melt <- melt( L142.EDGAR_PFC, id.vars=c( "GCAM_region_ID", "EDGAR_agg_sector", "Non.CO2" ) )
L142.EDGAR_PFC_R_S_T_Yh.melt <- aggregate( L142.EDGAR_PFC_R_S_T_Yh.melt$value, by=as.list( L142.EDGAR_PFC_R_S_T_Yh.melt[ c( "GCAM_region_ID", "EDGAR_agg_sector", "Non.CO2", "variable" ) ] ), sum )
names( L142.EDGAR_PFC_R_S_T_Yh.melt )[ names( L142.EDGAR_PFC_R_S_T_Yh.melt ) == "x" ] <- "EDGAR_emissions"

#Map in other f-gas sector, which varies by gas
L142.EDGAR_PFC_R_S_T_Yh_rest <- subset( L142.EDGAR_PFC_R_S_T_Yh.melt, L142.EDGAR_PFC_R_S_T_Yh.melt$EDGAR_agg_sector != "other_f_gases")
L142.EDGAR_PFC_R_S_T_Yh_other <- subset( L142.EDGAR_PFC_R_S_T_Yh.melt, L142.EDGAR_PFC_R_S_T_Yh.melt$EDGAR_agg_sector == "other_f_gases")
L142.EDGAR_PFC_R_S_T_Yh_other$EDGAR_agg_sector <- Other_F$Sector[ match( L142.EDGAR_PFC_R_S_T_Yh_other$Non.CO2, Other_F$Gas )]
L142.EDGAR_PFC_R_S_T_Yh.melt <- rbind( L142.EDGAR_PFC_R_S_T_Yh_rest, L142.EDGAR_PFC_R_S_T_Yh_other )

#Map to GCAM technologies
L142.pfc_R_S_T_Yh.melt <- GCAM_tech
L142.pfc_R_S_T_Yh.melt <- repeat_and_add_vector( L142.pfc_R_S_T_Yh.melt, "GCAM_region_ID", GCAM_region_names$GCAM_region_ID )
L142.pfc_R_S_T_Yh.melt <- repeat_and_add_vector( L142.pfc_R_S_T_Yh.melt, "xyear", X_EDGAR_historical_years )
L142.pfc_R_S_T_Yh.melt <- repeat_and_add_vector( L142.pfc_R_S_T_Yh.melt, "Non.CO2", PFCs )
L142.pfc_R_S_T_Yh.melt$emissions <- L142.EDGAR_PFC_R_S_T_Yh.melt$EDGAR_emissions[ match( vecpaste(L142.pfc_R_S_T_Yh.melt[ c( "GCAM_region_ID", "EDGAR_agg_sector", "Non.CO2", "xyear" ) ] ), vecpaste( L142.EDGAR_PFC_R_S_T_Yh.melt[ c( "GCAM_region_ID", "EDGAR_agg_sector", "Non.CO2", "variable" )] ))]
L142.pfc_R_S_T_Yh.melt$emissions[ is.na( L142.pfc_R_S_T_Yh.melt$emissions )] <- 0

#Disaggregate cooling emissions to residential and commercial sectors
L142.R_cooling_T_Yh <- subset(L144.in_EJ_R_bld_serv_F_Yh, L144.in_EJ_R_bld_serv_F_Yh$service %in% c( "comm cooling", "resid cooling" ))
L142.R_cooling_T_Yh <- subset( L142.R_cooling_T_Yh, L142.R_cooling_T_Yh$fuel == "electricity" )
L142.R_cooling_T_Yh.melt <- melt( L142.R_cooling_T_Yh, id.vars=c( "GCAM_region_ID", "sector", "fuel", "service"))
L142.R_cooling_Yh <- aggregate( L142.R_cooling_T_Yh.melt$value, by=as.list( L142.R_cooling_T_Yh.melt[ c( "GCAM_region_ID", "variable")]), sum )
L142.R_cooling_T_Yh.melt$total <- L142.R_cooling_Yh$x[ match( vecpaste( L142.R_cooling_T_Yh.melt[ c( "GCAM_region_ID", "variable" ) ]), vecpaste( L142.R_cooling_Yh[ c( "GCAM_region_ID", "variable" )]))]
L142.R_cooling_T_Yh.melt$share <- L142.R_cooling_T_Yh.melt$value / L142.R_cooling_T_Yh.melt$total 
L142.pfc_R_S_T_Yh.melt$share <- L142.R_cooling_T_Yh.melt$share[ match( vecpaste( L142.pfc_R_S_T_Yh.melt[ c( "GCAM_region_ID", "xyear", "supplysector" ) ]), vecpaste(L142.R_cooling_T_Yh.melt[ c( "GCAM_region_ID", "variable", "service" ) ] ))]
L142.pfc_R_S_T_Yh.melt$share[ is.na( L142.pfc_R_S_T_Yh.melt$share ) ] <- 1
L142.pfc_R_S_T_Yh.melt$emissions <- L142.pfc_R_S_T_Yh.melt$emissions * L142.pfc_R_S_T_Yh.melt$share

#Reshape
L142.pfc_R_S_T_Yh.melt <- aggregate( L142.pfc_R_S_T_Yh.melt$emissions, by=as.list( L142.pfc_R_S_T_Yh.melt[ c( "GCAM_region_ID", "supplysector", "subsector", "stub.technology", "Non.CO2", "xyear" ) ]), sum)
L142.pfc_R_S_T_Yh <- dcast( L142.pfc_R_S_T_Yh.melt, GCAM_region_ID + supplysector + subsector + stub.technology + Non.CO2 ~ xyear, value.var=c( "x" ))
L142.pfc_R_S_T_Yh[ is.na( L142.pfc_R_S_T_Yh ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L142.pfc_R_S_T_Yh <- c( "HFC emissions by region / sector / technology / gas / historical year", "Unit = Gg" )

#write tables as CSV files
writedata( L142.pfc_R_S_T_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L142.pfc_R_S_T_Yh", comments=comments.L142.pfc_R_S_T_Yh )

# Every script should finish with this line
logstop()
