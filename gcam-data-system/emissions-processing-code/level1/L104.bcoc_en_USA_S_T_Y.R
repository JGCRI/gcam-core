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
logstart( "L104.bcoc_en_USA_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical BC/OC emissions factors for energy by GCAM technology, computed from EPA emissions data and IEA energy balances" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
IEA_flow_sector <- readdata( "ENERGY_MAPPINGS", "IEA_flow_sector" )
IEA_product_fuel <- readdata( "ENERGY_MAPPINGS", "IEA_product_fuel" )
bc.oc_tech <- readdata( "EMISSIONS_MAPPINGS", "bc.oc_tech" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
L101.in_EJ_R_en_Si_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L101.in_EJ_R_en_Si_F_Yh" )
Smith_BC_1990 <- readdata( "EMISSIONS_LEVEL0_DATA", "Smith_BC_1990" )
Smith_OC_1990 <- readdata( "EMISSIONS_LEVEL0_DATA", "Smith_OC_1990" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# First subset for USA, then combine BC & OC data frames
L104.bcoc_tg_USA_en_T_1990 <- Smith_BC_1990[ names( Smith_BC_1990 ) %in% c( "Technology", "USA" ) ]
names( L104.bcoc_tg_USA_en_T_1990 )[ names( L104.bcoc_tg_USA_en_T_1990 ) == "USA" ] <- "BC"
L104.bcoc_tg_USA_en_T_1990$OC <- Smith_OC_1990$USA

printlog( "Convert BC/OC emissions inventory to Tg and aggregate by sector and technology" )
L104.bcoc_tg_USA_en_T_1990$sector <- bc.oc_tech$sector[ match( L104.bcoc_tg_USA_en_T_1990$Technology , bc.oc_tech$BCOC.Technology )]
L104.bcoc_tg_USA_en_T_1990$technology <- bc.oc_tech$technology[ match( L104.bcoc_tg_USA_en_T_1990$Technology , bc.oc_tech$BCOC.Technology )]
L104.bcoc_tg_USA_en_T_1990 <- aggregate( L104.bcoc_tg_USA_en_T_1990[ c( "BC", "OC") ], by=as.list( L104.bcoc_tg_USA_en_T_1990[ c( "sector", "technology" ) ] ), sum )

#Drop missing values
L104.bcoc_tg_USA_en_T_1990  <- na.omit( L104.bcoc_tg_USA_en_T_1990  )

#Convert to Tg
L104.bcoc_tg_USA_en_T_1990[ c( "BC", "OC" ) ] <- L104.bcoc_tg_USA_en_T_1990[ c( "BC", "OC" ) ] * gg_to_tg  

printlog( "Compute BC/OC emissions factors by dividing Smith inventory by IEA energy balances" )
#Subset for USA only in 1990 and aggregate to BC/OC categories
L104.in_EJ_USA_en_Si_F_Yh <- subset( L101.in_EJ_R_en_Si_F_Yh, L101.in_EJ_R_en_Si_F_Yh$GCAM_region_ID == "1" )
L104.in_EJ_USA_en_Si_F_Yh.melt <- melt( L104.in_EJ_USA_en_Si_F_Yh, id.vars = c( "GCAM_region_ID", "sector", "fuel", "technology" ) )
L104.in_EJ_USA_en_Si_F_Yh.melt$BCOC_agg_sector <- GCAM_sector_tech$BCOC_agg_sector[ match( vecpaste( L104.in_EJ_USA_en_Si_F_Yh.melt[ c( "sector", "technology" )] ), vecpaste( GCAM_sector_tech[ c( "sector", "technology" )]) )]
L104.in_EJ_USA_en_Si_F_Yh.melt$BCOC_agg_fuel <- GCAM_sector_tech$BCOC_agg_fuel[ match( vecpaste( L104.in_EJ_USA_en_Si_F_Yh.melt[ c( "sector", "technology" )] ), vecpaste( GCAM_sector_tech[ c( "sector", "technology" )]) )]
L104.in_EJ_USA_en_T_Yh.melt <- aggregate( L104.in_EJ_USA_en_Si_F_Yh.melt$value, by=as.list( L104.in_EJ_USA_en_Si_F_Yh.melt[ c( "BCOC_agg_sector", "BCOC_agg_fuel", "variable" )]), sum )
names( L104.in_EJ_USA_en_T_Yh.melt )[ names( L104.in_EJ_USA_en_T_Yh.melt ) == "x" ] <- "energy"
L104.in_EJ_USA_en_T_1990.melt <- subset( L104.in_EJ_USA_en_T_Yh.melt, L104.in_EJ_USA_en_T_Yh.melt$variable == "X1990" )

L104.bcoc_tgej_USA_en_T_1990 <- L104.bcoc_tg_USA_en_T_1990
L104.bcoc_tgej_USA_en_T_1990$energy <- L104.in_EJ_USA_en_T_1990.melt$energy[ match( vecpaste( L104.bcoc_tgej_USA_en_T_1990[ c( "sector", "technology" )]), vecpaste( L104.in_EJ_USA_en_T_1990.melt[ c( "BCOC_agg_sector", "BCOC_agg_fuel" ) ] )  )]
L104.bcoc_tgej_USA_en_T_1990$bc_em_factor <- L104.bcoc_tgej_USA_en_T_1990$BC / L104.bcoc_tgej_USA_en_T_1990$energy
L104.bcoc_tgej_USA_en_T_1990$oc_em_factor <- L104.bcoc_tgej_USA_en_T_1990$OC / L104.bcoc_tgej_USA_en_T_1990$energy
L104.bcoc_tgej_USA_en_T_1990$bc_em_factor[ L104.bcoc_tgej_USA_en_T_1990$bc_em_factor == "Inf" ] <- 0
L104.bcoc_tgej_USA_en_T_1990$oc_em_factor[ L104.bcoc_tgej_USA_en_T_1990$oc_em_factor == "Inf" ] <- 0
L104.bcoc_tgej_USA_en_T_1990$bc_em_factor[ is.na( L104.bcoc_tgej_USA_en_T_1990$bc_em_factor ) ] <- 0
L104.bcoc_tgej_USA_en_T_1990$oc_em_factor[ is.na( L104.bcoc_tgej_USA_en_T_1990$oc_em_factor ) ] <- 0

#Drop unnecessary columns
L104.bcoc_tgej_USA_en_T_1990 <- L104.bcoc_tgej_USA_en_T_1990[ names( L104.bcoc_tgej_USA_en_T_1990 ) %!in% c( "BC", "OC", "energy" )]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L104.bcoc_tgej_USA_en_T_1990 <- c( "BC/OC emissions factors for energy technologies in the USA by sector / technology / 1990", "Unit = Tg / EJ" )

#write tables as CSV files
writedata( L104.bcoc_tgej_USA_en_T_1990, domain="EMISSIONS_LEVEL1_DATA", fn="L104.bcoc_tgej_USA_en_T_1990", comments=comments.L104.bcoc_tgej_USA_en_T_1990 )

# Every script should finish with this line
logstop()
