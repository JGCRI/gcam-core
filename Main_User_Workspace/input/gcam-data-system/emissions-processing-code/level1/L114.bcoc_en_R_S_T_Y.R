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
logstart( "L114.bcoc_en_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical BC/OC emissions in the energy system by GCAM technology, computed from RCP emissions data and Smith emissions factors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
bc.oc_tech <- readdata( "EMISSIONS_MAPPINGS", "EPA_ghg_tech" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
L101.in_EJ_R_en_Si_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L101.in_EJ_R_en_Si_F_Yh" )
L104.bcoc_tgej_USA_en_T_1990 <- readdata( "EMISSIONS_LEVEL1_DATA", "L104.bcoc_tgej_USA_en_T_1990" )
RCP_BC_2000 <- readdata( "EMISSIONS_LEVEL0_DATA", "RCP_BC_2000" )
RCP_OC_2000 <- readdata( "EMISSIONS_LEVEL0_DATA", "RCP_OC_2000" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# Compute emissions using EPA emissions factors and IEA fuel consumption
printlog( "Computing unscaled emissions by country and technology" )
L114.bcoc_tg_R_en_Si_F_Yh.melt <- melt( L101.in_EJ_R_en_Si_F_Yh, id.vars=c( "GCAM_region_ID", "sector", "fuel", "technology" ) ) 
names( L114.bcoc_tg_R_en_Si_F_Yh.melt )[ names( L114.bcoc_tg_R_en_Si_F_Yh.melt ) == "variable" ] <- "xyear"
names( L114.bcoc_tg_R_en_Si_F_Yh.melt )[ names( L114.bcoc_tg_R_en_Si_F_Yh.melt ) == "value" ] <- "energy"
L114.bcoc_tg_R_en_Si_F_Yh.melt$BCOC_agg_sector <- GCAM_sector_tech$BCOC_agg_sector[ match( vecpaste( L114.bcoc_tg_R_en_Si_F_Yh.melt[ c( "sector", "technology" )]), vecpaste( GCAM_sector_tech[ c( "sector", "technology" )]) )]
L114.bcoc_tg_R_en_Si_F_Yh.melt$BCOC_agg_fuel <- GCAM_sector_tech$BCOC_agg_fuel[ match( vecpaste( L114.bcoc_tg_R_en_Si_F_Yh.melt[ c( "sector", "technology" )]), vecpaste( GCAM_sector_tech[ c( "sector", "technology" )]) )]

#Subset for 2000, which is the only year with data
L114.bcoc_tg_R_en_Si_F_2000.melt <- subset( L114.bcoc_tg_R_en_Si_F_Yh.melt, L114.bcoc_tg_R_en_Si_F_Yh.melt$xyear == "X2000" )

#Duplicate for all gases
L114.bcoc_tg_R_en_Si_F_2000.melt <- repeat_and_add_vector( L114.bcoc_tg_R_en_Si_F_2000.melt, "Non.CO2", c( "BC", "OC" ) )

#Match in emissions factors
names( L104.bcoc_tgej_USA_en_T_1990 )[ names( L104.bcoc_tgej_USA_en_T_1990 ) == "bc_em_factor" ] <- "BC"
names( L104.bcoc_tgej_USA_en_T_1990 )[ names( L104.bcoc_tgej_USA_en_T_1990) == "oc_em_factor" ] <- "OC"
L104.bcoc_tgej_USA_en_T_1990.melt <- melt( L104.bcoc_tgej_USA_en_T_1990, id.vars = c( "sector", "technology" ))
L114.bcoc_tg_R_en_Si_F_2000.melt$emfact <- L104.bcoc_tgej_USA_en_T_1990.melt$value[ match( vecpaste( L114.bcoc_tg_R_en_Si_F_2000.melt[ c( "Non.CO2", "BCOC_agg_sector", "BCOC_agg_fuel" )]), vecpaste( L104.bcoc_tgej_USA_en_T_1990.melt[ c( "variable", "sector", "technology" ) ] ) )]

#Compute unscaled emissions
L114.bcoc_tg_R_en_Si_F_2000.melt$unscaled_emissions <- L114.bcoc_tg_R_en_Si_F_2000.melt$energy * L114.bcoc_tg_R_en_Si_F_2000.melt$emfact
  
#Aggregate by sector and region
L114.bcoc_tg_R_en_Si_F_2000.melt <- na.omit( L114.bcoc_tg_R_en_Si_F_2000.melt)
L114.bcoc_tg_R_en_Si_F_2000.melt$RCP_agg_sector <- GCAM_sector_tech$RCP_agg_sector[ match( vecpaste( L114.bcoc_tg_R_en_Si_F_2000.melt[ c( "sector", "technology" )] ), vecpaste( GCAM_sector_tech[ c( "sector", "technology" )] ))]
L114.bcoc_tg_R_en_Srcp_2000.melt <- aggregate( L114.bcoc_tg_R_en_Si_F_2000.melt$unscaled_emissions, by=as.list( L114.bcoc_tg_R_en_Si_F_2000.melt[ c( "GCAM_region_ID", "Non.CO2", "RCP_agg_sector", "xyear" ) ] ), sum )
names( L114.bcoc_tg_R_en_Srcp_2000.melt )[ names( L114.bcoc_tg_R_en_Srcp_2000.melt ) == "x" ] <- "unscaled_emissions"

printlog( "Compute RCP emissions by region and sector" )
RCP_BC_2000$Non.CO2 <- "BC"
RCP_OC_2000$Non.CO2 <- "OC"
L114.RCP <- rbind( RCP_BC_2000, RCP_OC_2000 )
L114.RCP <- melt( L114.RCP, id.vars=c( "Country", "iso", "Non.CO2" ) )
names( L114.RCP )[ names( L114.RCP ) == "variable" ] <- "RCP_agg_sector" 
L114.RCP$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L114.RCP$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, and aggregate by region
L114.RCP <- L114.RCP[ names( L114.RCP ) %!in% c( "Country", "iso" ) ]
L114.RCP <- na.omit( L114.RCP )
L114.RCP <- aggregate( L114.RCP$value, by=as.list( L114.RCP[ c( "GCAM_region_ID", "Non.CO2", "RCP_agg_sector" ) ]), sum )
names( L114.RCP )[ names( L114.RCP ) == "x" ] <- "RCP_emissions"

printlog( "Scale emissions by tech to match RCP totals")
#First compute scalers
L114.emiss_scaler <- L114.bcoc_tg_R_en_Srcp_2000.melt
L114.emiss_scaler$RCP_emissions <- L114.RCP$RCP_emissions[ match( vecpaste( L114.emiss_scaler[ c( "GCAM_region_ID", "RCP_agg_sector" )]), vecpaste( L114.RCP[ c( "GCAM_region_ID", "RCP_agg_sector" ) ]))]
L114.emiss_scaler$scaler <- L114.emiss_scaler$RCP_emissions / L114.emiss_scaler$unscaled_emissions * kg_to_tg 

#Now, scale EPA emissions
L114.bcoc_tg_R_en_Si_F_2000.melt$scaler <- L114.emiss_scaler$scaler[ match( vecpaste( L114.bcoc_tg_R_en_Si_F_2000.melt[ c( "GCAM_region_ID", "RCP_agg_sector" ) ]), vecpaste( L114.emiss_scaler[ c( "GCAM_region_ID", "RCP_agg_sector" ) ]) ) ]
L114.bcoc_tg_R_en_Si_F_2000.melt$emissions <- L114.bcoc_tg_R_en_Si_F_2000.melt$unscaled_emissions * L114.bcoc_tg_R_en_Si_F_2000.melt$scaler
L114.bcoc_tg_R_en_Si_F_2000.melt[ is.na( L114.bcoc_tg_R_en_Si_F_2000.melt ) ] <- 0

printlog( "Map in GCAM sector, technology, and driver type")
L114.bcoc_tg_R_en_S_F_2000.melt <- L114.bcoc_tg_R_en_Si_F_2000.melt
L114.bcoc_tg_R_en_S_F_2000.melt$supplysector <- GCAM_sector_tech$supplysector[ match( vecpaste( L114.bcoc_tg_R_en_S_F_2000.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L114.bcoc_tg_R_en_S_F_2000.melt$subsector <- GCAM_sector_tech$subsector[ match( vecpaste( L114.bcoc_tg_R_en_S_F_2000.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L114.bcoc_tg_R_en_S_F_2000.melt$stub.technology <- GCAM_sector_tech$stub.technology[ match( vecpaste( L114.bcoc_tg_R_en_S_F_2000.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L114.bcoc_tg_R_en_S_F_2000.melt <- aggregate( L114.bcoc_tg_R_en_S_F_2000.melt$emissions, by=as.list( L114.bcoc_tg_R_en_S_F_2000.melt[ R_G_StubTechYr ] ), sum)
names( L114.bcoc_tg_R_en_S_F_2000.melt )[ names( L114.bcoc_tg_R_en_S_F_2000.melt ) == "x" ] <- "input.emissions"

printlog( "Compute emissions factor by GCAM sector, technology, and driver type" )
#First compute energy by sector
L114.in_EJ_R_en_S_F_Yh.melt <- melt( L101.in_EJ_R_en_Si_F_Yh, id.vars=c( "GCAM_region_ID", "sector", "fuel", "technology" ) )
L114.in_EJ_R_en_S_F_Yh.melt$supplysector <- GCAM_sector_tech$supplysector[ match( vecpaste( L114.in_EJ_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L114.in_EJ_R_en_S_F_Yh.melt$subsector <- GCAM_sector_tech$subsector[ match( vecpaste( L114.in_EJ_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L114.in_EJ_R_en_S_F_Yh.melt$stub.technology <- GCAM_sector_tech$stub.technology[ match( vecpaste( L114.in_EJ_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
names( L114.in_EJ_R_en_S_F_Yh.melt )[ names( L114.in_EJ_R_en_S_F_Yh.melt ) == "variable" ] <- "xyear"
L114.in_EJ_R_en_S_F_Yh.melt <- aggregate( L114.in_EJ_R_en_S_F_Yh.melt$value, by=as.list( L114.in_EJ_R_en_S_F_Yh.melt[ R_StubTechYr ]), sum)
names( L114.in_EJ_R_en_S_F_Yh.melt )[ names( L114.in_EJ_R_en_S_F_Yh.melt ) == "x" ] <- "energy"

L114.bcoc_tg_R_en_S_F_2000.melt$energy <- L114.in_EJ_R_en_S_F_Yh.melt$energy[ match( vecpaste( L114.bcoc_tg_R_en_S_F_2000.melt[ R_StubTechYr ] ), vecpaste( L114.in_EJ_R_en_S_F_Yh.melt[ R_StubTechYr ] ) )] 
L114.bcoc_tg_R_en_S_F_2000.melt$emfact <- L114.bcoc_tg_R_en_S_F_2000.melt$input.emissions / L114.bcoc_tg_R_en_S_F_2000.melt$energy
L114.bcoc_tg_R_en_S_F_2000.melt <- na.omit( L114.bcoc_tg_R_en_S_F_2000.melt )

#Reshape
L114.bcoc_tgej_R_en_S_F_2000 <- dcast( L114.bcoc_tg_R_en_S_F_2000.melt, GCAM_region_ID + Non.CO2 + supplysector + subsector + stub.technology ~ xyear, value = c( "emfact" ) )


# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L114.bcoc_tgej_R_en_S_F_2000 <- c( "BC / OC emissions factors for energy technologies by GCAM region / sector / technology / 2000", "Unit = Tg / EJ" )

#write tables as CSV files
writedata( L114.bcoc_tgej_R_en_S_F_2000, domain="EMISSIONS_LEVEL1_DATA", fn="L114.bcoc_tgej_R_en_S_F_2000", comments=comments.L114.bcoc_tgej_R_en_S_F_2000 )

# Every script should finish with this line
logstop()
