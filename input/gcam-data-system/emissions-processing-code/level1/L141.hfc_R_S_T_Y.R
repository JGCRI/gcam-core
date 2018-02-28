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
logstart( "L141.hfc_R_S_T_Y.R" )
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
EDGAR_HFC125 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_HFC125" )
EDGAR_HFC134a <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_HFC134a" )
EDGAR_HFC143a <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_HFC143a" )
EDGAR_HFC152a <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_HFC152a" )
EDGAR_HFC227ea <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_HFC227ea" )
EDGAR_HFC23 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_HFC23" )
EDGAR_HFC236fa <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_HFC236fa" )
EDGAR_HFC245fa <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_HFC245fa" )
EDGAR_HFC32 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_HFC32" )
EDGAR_HFC365mfc <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_HFC365mfc" )
EDGAR_HFC43 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_HFC43" )
Guus_Data <- readdata( "EMISSIONS_LEVEL0_DATA", "HFC_Inventory_GV" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Map EDGAR HFC emissions to GCAM technologies" )
#First, bind all gases together
EDGAR_HFC125$Non.CO2 <- "HFC125"
EDGAR_HFC134a$Non.CO2 <- "HFC134a"
EDGAR_HFC143a$Non.CO2 <- "HFC143a"
EDGAR_HFC152a$Non.CO2 <- "HFC152a"
EDGAR_HFC227ea$Non.CO2 <- "HFC227ea"
EDGAR_HFC23$Non.CO2 <- "HFC23"
EDGAR_HFC236fa$Non.CO2 <- "HFC236fa"
EDGAR_HFC245fa$Non.CO2 <- "HFC245fa"
EDGAR_HFC32$Non.CO2 <- "HFC32"
EDGAR_HFC365mfc$Non.CO2 <- "HFC365mfc"
EDGAR_HFC43$Non.CO2 <- "HFC43"
L141.EDGAR_HFC <- rbind( EDGAR_HFC125, EDGAR_HFC134a, EDGAR_HFC143a, EDGAR_HFC152a, EDGAR_HFC227ea, EDGAR_HFC23,
                         EDGAR_HFC236fa, EDGAR_HFC245fa, EDGAR_HFC32, EDGAR_HFC365mfc, EDGAR_HFC43  )  

#Then, prepare EDGAR data for use
L141.EDGAR_HFC$EDGAR_agg_sector <- EDGAR_sector$agg_sector[ match( L141.EDGAR_HFC$IPCC_description, EDGAR_sector$IPCC_description )]
L141.EDGAR_HFC$iso <- EDGAR_nation$iso[ match( L141.EDGAR_HFC$ISO_A3, EDGAR_nation$ISO_A3 )]
L141.EDGAR_HFC$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L141.EDGAR_HFC$iso, iso_GCAM_regID$iso )]   
L141.EDGAR_HFC <- L141.EDGAR_HFC[ c( "GCAM_region_ID", "EDGAR_agg_sector", "Non.CO2", X_EDGAR_historical_years) ]
L141.EDGAR_hfc_R_S_T_Yh.melt <- melt( L141.EDGAR_HFC, id.vars=c( "GCAM_region_ID", "EDGAR_agg_sector", "Non.CO2" ),
                                      variable.name = Y, value.name = "EDGAR_emissions" )
L141.EDGAR_hfc_R_S_T_Yh.melt <- aggregate( L141.EDGAR_hfc_R_S_T_Yh.melt[ "EDGAR_emissions" ],
                                           by = L141.EDGAR_hfc_R_S_T_Yh.melt[ c( R, "EDGAR_agg_sector", "Non.CO2", Y ) ], sum )

#Map in other f-gas sector, which varies by gas
L141.EDGAR_hfc_R_S_T_Yh_rest <- subset( L141.EDGAR_hfc_R_S_T_Yh.melt, L141.EDGAR_hfc_R_S_T_Yh.melt$EDGAR_agg_sector != "other_f_gases")
L141.EDGAR_hfc_R_S_T_Yh_other <- subset( L141.EDGAR_hfc_R_S_T_Yh.melt, L141.EDGAR_hfc_R_S_T_Yh.melt$EDGAR_agg_sector == "other_f_gases")
L141.EDGAR_hfc_R_S_T_Yh_other$EDGAR_agg_sector <- Other_F$Sector[ match( L141.EDGAR_hfc_R_S_T_Yh_other$Non.CO2, Other_F$Gas )]
L141.EDGAR_hfc_R_S_T_Yh.melt <- rbind( L141.EDGAR_hfc_R_S_T_Yh_rest, L141.EDGAR_hfc_R_S_T_Yh_other )

#Map to GCAM technologies
L141.hfc_R_S_T_Yh.melt <- GCAM_tech
L141.hfc_R_S_T_Yh.melt <- repeat_and_add_vector( L141.hfc_R_S_T_Yh.melt, R, GCAM_region_names[[R]] )
L141.hfc_R_S_T_Yh.melt <- repeat_and_add_vector( L141.hfc_R_S_T_Yh.melt, Y, X_EDGAR_historical_years )
L141.hfc_R_S_T_Yh.melt <- repeat_and_add_vector( L141.hfc_R_S_T_Yh.melt, "Non.CO2", HFCs )
L141.hfc_R_S_T_Yh.melt$emissions <- L141.EDGAR_hfc_R_S_T_Yh.melt$EDGAR_emissions[
  match( vecpaste(L141.hfc_R_S_T_Yh.melt[ c( R, "EDGAR_agg_sector", "Non.CO2", Y ) ] ),
         vecpaste( L141.EDGAR_hfc_R_S_T_Yh.melt[ c( R, "EDGAR_agg_sector", "Non.CO2", Y )] ))]
# gpk - why not just drop the na's instead?
L141.hfc_R_S_T_Yh.melt$emissions[ is.na( L141.hfc_R_S_T_Yh.melt$emissions )] <- 0

#Disaggregate cooling emissions to residential and commercial sectors
L141.R_cooling_T_Yh <- subset(L144.in_EJ_R_bld_serv_F_Yh,
                              service %in% c( "comm cooling", "resid cooling" ) & fuel == "electricity" )
L141.R_cooling_T_Yh.melt <- melt( L141.R_cooling_T_Yh, id.vars=c( R, "sector", "fuel", "service"), measure.vars = X_historical_years,
                                  variable.name = Y )
L141.R_cooling_Yh <- aggregate( L141.R_cooling_T_Yh.melt$value,
                                by =  L141.R_cooling_T_Yh.melt[ R_Y ], sum )
L141.R_cooling_T_Yh.melt$total <- L141.R_cooling_Yh$x[
  match( vecpaste( L141.R_cooling_T_Yh.melt[ R_Y ]),
         vecpaste( L141.R_cooling_Yh[ R_Y ] ) ) ]
L141.R_cooling_T_Yh.melt$share <- L141.R_cooling_T_Yh.melt$value / L141.R_cooling_T_Yh.melt$total 
L141.hfc_R_S_T_Yh.melt$share <- L141.R_cooling_T_Yh.melt$share[
  match( vecpaste( L141.hfc_R_S_T_Yh.melt[ c( R_Y, "supplysector" ) ]),
         vecpaste(L141.R_cooling_T_Yh.melt[ c( R_Y, "service" ) ] ) ) ]
L141.hfc_R_S_T_Yh.melt$share[ is.na( L141.hfc_R_S_T_Yh.melt$share ) ] <- 1
L141.hfc_R_S_T_Yh.melt$emissions <- L141.hfc_R_S_T_Yh.melt$emissions * L141.hfc_R_S_T_Yh.melt$share

#Add 2008 since Guus has data
TEMP <- subset( L141.hfc_R_S_T_Yh.melt, year == "X2008" )
TEMP$year <- "X2010"
L141.hfc_R_S_T_Yh.melt <- rbind( L141.hfc_R_S_T_Yh.melt, TEMP )

#Scale to match global totals from Guus Velders
L141.hfc_scaler <- aggregate( L141.hfc_R_S_T_Yh.melt$emissions, by = L141.hfc_R_S_T_Yh.melt[ c( Y, "Non.CO2" ) ], sum )
names( L141.hfc_scaler )[ names( L141.hfc_scaler ) == "x" ] <- "EDGAR_tot"
Guus_Data$Species <- gsub( "-", "", Guus_Data$Species )
Guus_Data$Species <- gsub( "4310mee", "43", Guus_Data$Species )
Guus_Data$year <- paste( "X", Guus_Data$Year, sep="" )
L141.hfc_scaler$Guus_tot <- Guus_Data$Emissions[ match( vecpaste( L141.hfc_scaler[ c( Y, "Non.CO2")]),
                                                        vecpaste( Guus_Data[ c( Y, "Species" )]))]
L141.hfc_scaler$scaler <- L141.hfc_scaler$Guus_tot / L141.hfc_scaler$EDGAR_tot
L141.hfc_scaler$scaler[ is.na( L141.hfc_scaler$scaler ) ] <- 1

L141.hfc_R_S_T_Yh.melt$scaler <- L141.hfc_scaler$scaler[ match( vecpaste( L141.hfc_R_S_T_Yh.melt[ c( Y, "Non.CO2" )]),
                                                                vecpaste( L141.hfc_scaler[ c( Y, "Non.CO2" )]))]
L141.hfc_R_S_T_Yh.melt$adj_emissions <- L141.hfc_R_S_T_Yh.melt$emissions * L141.hfc_R_S_T_Yh.melt$scaler

#Reshape
L141.hfc_R_S_T_Yh.melt <- aggregate( L141.hfc_R_S_T_Yh.melt[ "adj_emissions" ],
                                     by = L141.hfc_R_S_T_Yh.melt[ c( R, "supplysector", "subsector", "stub.technology", "Non.CO2", Y ) ], sum)
L141.hfc_R_S_T_Yh <- dcast( L141.hfc_R_S_T_Yh.melt, GCAM_region_ID + supplysector + subsector + stub.technology + Non.CO2 ~ year,
                            value.var = "adj_emissions" )
L141.hfc_R_S_T_Yh[ is.na( L141.hfc_R_S_T_Yh ) ] <- 0

#Compute cooling emissions factors
L141.hfc_R_cooling_T_Yh.melt <- subset( L141.hfc_R_S_T_Yh.melt, supplysector %in% c( "comm cooling", "resid cooling" ) )
L141.hfc_R_cooling_T_Yh.melt$energy <- L141.R_cooling_T_Yh.melt$value[
  match( vecpaste( L141.hfc_R_cooling_T_Yh.melt[ c( R_Y, "supplysector" ) ] ),
         vecpaste( L141.R_cooling_T_Yh.melt[ c( R_Y, "service" ) ] ) ) ] 
L141.hfc_R_cooling_T_Yh.melt$em_fact <- with( L141.hfc_R_cooling_T_Yh.melt, adj_emissions / energy )
L141.hfc_ef_R_cooling_Yh <- dcast( L141.hfc_R_cooling_T_Yh.melt,
                                   GCAM_region_ID + supplysector + subsector + stub.technology + Non.CO2 ~ year, value.var = "em_fact" )
L141.hfc_ef_R_cooling_Yh[ is.na( L141.hfc_ef_R_cooling_Yh ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L141.hfc_R_S_T_Yh <- c( "HFC emissions by region / sector / technology / gas / historical year", "Unit = Gg" )
comments.L141.hfc_ef_R_cooling_Yh <- c( "HFC emissions factors for cooling by region / sector / technology / gas / historical year", "Unit = Gg / EJ" )

#write tables as CSV files
writedata( L141.hfc_R_S_T_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L141.hfc_R_S_T_Yh", comments=comments.L141.hfc_R_S_T_Yh )
writedata( L141.hfc_ef_R_cooling_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L141.hfc_ef_R_cooling_Yh", comments=comments.L141.hfc_ef_R_cooling_Yh )

# Every script should finish with this line
logstop()
