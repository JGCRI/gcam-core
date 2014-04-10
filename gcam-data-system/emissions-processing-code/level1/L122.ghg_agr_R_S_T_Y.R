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
logstart( "L122.ghg_agr_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions by GCAM technology, computed from EDGAR emissions data and EPA emissions factors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
L104.ag_Prod_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L104.ag_Prod_Mt_R_C_Y_AEZ" )
L142.ag_Fert_IO_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L142.ag_Fert_IO_R_C_Y_AEZ" )
EDGAR_CH4 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CH4" )
EDGAR_N2O <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_N2O" )
EDGAR_NH3 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NH3" )
EDGAR_NOx <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NOx" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compute share of regional production by technology" )
L122.ag_Prod_Mt_R_Y <- aggregate( L104.ag_Prod_Mt_R_C_Y_AEZ[ X_historical_years ], by=as.list( L104.ag_Prod_Mt_R_C_Y_AEZ[ c( "GCAM_region_ID" ) ] ), sum)
L122.ag_Prod_Mt_R_Y.melt <- melt( L122.ag_Prod_Mt_R_Y, id.vars=c( "GCAM_region_ID" ) )
L122.ag_pct_R_C_Y_AEZ.melt <- melt( L104.ag_Prod_Mt_R_C_Y_AEZ, id.vars=c( "GCAM_region_ID", "GCAM_commodity", "AEZ" ))
L122.ag_pct_R_C_Y_AEZ.melt$total_prod <- L122.ag_Prod_Mt_R_Y.melt$value[ match( vecpaste( L122.ag_pct_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "variable" )]), vecpaste( L122.ag_Prod_Mt_R_Y.melt[ c( "GCAM_region_ID", "variable" )]))]
L122.ag_pct_R_C_Y_AEZ.melt$prod_share <- L122.ag_pct_R_C_Y_AEZ.melt$value / L122.ag_pct_R_C_Y_AEZ.melt$total_prod

printlog( "Compute EDGAR emissions by region" )
EDGAR_CH4$Non.CO2 <- "CH4_AGR"
EDGAR_N2O$Non.CO2 <- "N2O_AGR"
EDGAR_NH3$Non.CO2 <- "NH3_AGR"
EDGAR_NOx$Non.CO2 <- "NOx_AGR"
L122.EDGAR <- rbind( EDGAR_CH4, EDGAR_N2O, EDGAR_NH3, EDGAR_NOx )
L122.EDGAR$sector <- EDGAR_sector$agg_sector[ match( L122.EDGAR$IPCC, EDGAR_sector$IPCC ) ]
L122.EDGAR$iso <- EDGAR_nation$iso[ match( L122.EDGAR$ISO_A3, EDGAR_nation$ISO_A3 )]
L122.EDGAR$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L122.EDGAR$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L122.EDGAR <- L122.EDGAR[ names( L122.EDGAR ) %!in% c( "IPCC.Annex", "World.Region", "ISO_A3", "iso", "Name", "IPCC", "IPCC_description" ) ]
L122.EDGAR <- na.omit( L122.EDGAR )
L122.EDGAR <- aggregate( L122.EDGAR[ X_EDGAR_historical_years ], by=as.list( L122.EDGAR[ c( "GCAM_region_ID", "Non.CO2", "sector")]), sum)
L122.EDGAR.melt <- melt( L122.EDGAR, id.vars = c( "GCAM_region_ID", "Non.CO2", "sector" ))

#Convert to Tg
L122.EDGAR.melt$value <- L122.EDGAR.melt$value / 1000.0 

#Subset for AGR
L122.EDGAR_agr.melt <- subset( L122.EDGAR.melt, L122.EDGAR.melt$sector %in% agr_sectors )

printlog( "Compute emissions from rice by GCAM region, commodity, and AEZ" )
#Subset for rice
L122.EDGAR_rice.melt <- subset( L122.EDGAR_agr.melt, L122.EDGAR_agr.melt$sector == "rice" )
L122.ag_Prod_Mt_R_rice_Y_AEZ <- subset( L104.ag_Prod_Mt_R_C_Y_AEZ, L104.ag_Prod_Mt_R_C_Y_AEZ$GCAM_commodity == "Rice" )

#Compute share of rice production by AEZ in each region / year
L122.ag_Prod_Mt_R_rice_Y <- aggregate( L122.ag_Prod_Mt_R_rice_Y_AEZ[ X_historical_years ], by=as.list( L122.ag_Prod_Mt_R_rice_Y_AEZ[ c( "GCAM_region_ID" ) ] ), sum)
L122.ag_Prod_Mt_R_rice_Y.melt <- melt( L122.ag_Prod_Mt_R_rice_Y, id.vars=c( "GCAM_region_ID" ))
L122.ag_pct_R_rice_Y_AEZ.melt <- melt( L122.ag_Prod_Mt_R_rice_Y_AEZ, id.vars=c( "GCAM_region_ID", "GCAM_commodity", "AEZ" ))
L122.ag_pct_R_rice_Y_AEZ.melt$total_prod <- L122.ag_Prod_Mt_R_rice_Y.melt$value[ match( vecpaste( L122.ag_pct_R_rice_Y_AEZ.melt[ c( "GCAM_region_ID", "variable" )]), vecpaste( L122.ag_Prod_Mt_R_rice_Y.melt[ c( "GCAM_region_ID", "variable" )]))]
L122.ag_pct_R_rice_Y_AEZ.melt$prod_share <- L122.ag_pct_R_rice_Y_AEZ.melt$value / L122.ag_pct_R_rice_Y_AEZ.melt$total_prod

L122.ghg_tg_R_rice_Y_AEZ.melt <- L122.ag_pct_R_rice_Y_AEZ.melt[ names( L122.ag_pct_R_rice_Y_AEZ.melt ) %in% c( R_C_AEZ, "variable", "prod_share" )]
L122.ghg_tg_R_rice_Y_AEZ.melt <- repeat_and_add_vector( L122.ghg_tg_R_rice_Y_AEZ.melt, "Non.CO2", agr_gases )
L122.ghg_tg_R_rice_Y_AEZ.melt$total_emiss <- L122.EDGAR_rice.melt$value[ match( vecpaste( L122.ghg_tg_R_rice_Y_AEZ.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )]), vecpaste( L122.EDGAR_rice.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )] ) )]
L122.ghg_tg_R_rice_Y_AEZ.melt$emissions <- L122.ghg_tg_R_rice_Y_AEZ.melt$total_emiss * L122.ghg_tg_R_rice_Y_AEZ.melt$prod_share
L122.ghg_tg_R_rice_Y_AEZ.melt$type <- "Rice"

printlog( "Compute emissions from soils by GCAM region, commodity, and AEZ" )
#Subset for soil
L122.EDGAR_soil.melt <- subset( L122.EDGAR_agr.melt, L122.EDGAR_agr.melt$sector == "soil" )

L122.ghgsoil_tg_R_C_Y_AEZ.melt <- L122.ag_pct_R_C_Y_AEZ.melt[ names( L122.ag_pct_R_C_Y_AEZ.melt ) %in% c( R_C_AEZ, "variable", "prod_share" )]
L122.ghgsoil_tg_R_C_Y_AEZ.melt <- repeat_and_add_vector( L122.ghgsoil_tg_R_C_Y_AEZ.melt, "Non.CO2", agr_gases )
L122.ghgsoil_tg_R_C_Y_AEZ.melt$total_emiss <- L122.EDGAR_soil.melt$value[ match( vecpaste( L122.ghgsoil_tg_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )]), vecpaste( L122.EDGAR_soil.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )] ) )]
L122.ghgsoil_tg_R_C_Y_AEZ.melt$emissions <- L122.ghgsoil_tg_R_C_Y_AEZ.melt$total_emiss * L122.ghgsoil_tg_R_C_Y_AEZ.melt$prod_share
L122.ghgsoil_tg_R_C_Y_AEZ.melt$type <- "Soil"

printlog( "Compute emissions from fertilizer by GCAM region, commodity, and AEZ" )
#Subset for fertizizer
L122.EDGAR_fert.melt <- subset( L122.EDGAR_agr.melt, L122.EDGAR_agr.melt$sector == "fertilizer" )

#Compute fertilizer by crop
L122.fert_IO_R_C_Y_AEZ.melt <- melt( L142.ag_Fert_IO_R_C_Y_AEZ, id.vars=c( R_C_AEZ ) )

L122.fert_Mt_R_C_Y_AEZ.melt <- melt( L104.ag_Prod_Mt_R_C_Y_AEZ, id.vars=c( R_C_AEZ ) )
names( L122.fert_Mt_R_C_Y_AEZ.melt )[ names( L122.fert_Mt_R_C_Y_AEZ.melt ) == "value" ] <- "ag_production"
L122.fert_Mt_R_C_Y_AEZ.melt$IO <- L122.fert_IO_R_C_Y_AEZ.melt$value[ match( vecpaste(L122.fert_Mt_R_C_Y_AEZ.melt[ c( R_C_AEZ, "variable" )]), vecpaste( L122.fert_IO_R_C_Y_AEZ.melt[ c( R_C_AEZ, "variable" )]))]
L122.fert_Mt_R_C_Y_AEZ.melt$fertilizer <- L122.fert_Mt_R_C_Y_AEZ.melt$ag_production * L122.fert_Mt_R_C_Y_AEZ.melt$IO

L122.fert_Mt_R_Y.melt <- aggregate( L122.fert_Mt_R_C_Y_AEZ.melt$fertilizer, by=as.list( L122.fert_Mt_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "variable" )]), sum)
L122.fert_pct_R_C_Y_AEZ.melt <- L122.fert_Mt_R_C_Y_AEZ.melt[ c( R_C_AEZ, "variable", "fertilizer" )]
L122.fert_pct_R_C_Y_AEZ.melt$tot_fert <- L122.fert_Mt_R_Y.melt$x[ match( vecpaste(L122.fert_pct_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "variable" )]), vecpaste( L122.fert_Mt_R_Y.melt[ c( "GCAM_region_ID", "variable" )]))]
L122.fert_pct_R_C_Y_AEZ.melt$fert_share <- L122.fert_pct_R_C_Y_AEZ.melt$fertilizer / L122.fert_pct_R_C_Y_AEZ.melt$tot_fert

L122.ghgfert_tg_R_C_Y_AEZ.melt <- L122.fert_pct_R_C_Y_AEZ.melt[ names( L122.fert_pct_R_C_Y_AEZ.melt ) %in% c( R_C_AEZ, "variable", "fert_share" )]
L122.ghgfert_tg_R_C_Y_AEZ.melt <- repeat_and_add_vector( L122.ghgfert_tg_R_C_Y_AEZ.melt, "Non.CO2", agr_gases )
L122.ghgfert_tg_R_C_Y_AEZ.melt$total_emiss <- L122.EDGAR_fert.melt$value[ match( vecpaste( L122.ghgfert_tg_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )]), vecpaste(L122.EDGAR_fert.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )] ) )]
L122.ghgfert_tg_R_C_Y_AEZ.melt$emissions <- L122.ghgfert_tg_R_C_Y_AEZ.melt$total_emiss * L122.ghgfert_tg_R_C_Y_AEZ.melt$fert_share
L122.ghgfert_tg_R_C_Y_AEZ.melt$type <- "Fertilizer"

printlog( "Sum all AGR emissions by GCAM region, commodity, and AEZ" )
#Drop unnecessary columns
L122.ghg_tg_R_rice_Y_AEZ.melt <- L122.ghg_tg_R_rice_Y_AEZ.melt[ names( L122.ghg_tg_R_rice_Y_AEZ.melt ) %!in% c( "prod_share", "total_emiss" )]
L122.ghgsoil_tg_R_C_Y_AEZ.melt <- L122.ghgsoil_tg_R_C_Y_AEZ.melt[ names( L122.ghgsoil_tg_R_C_Y_AEZ.melt ) %!in% c( "prod_share", "total_emiss" )]
L122.ghgfert_tg_R_C_Y_AEZ.melt <- L122.ghgfert_tg_R_C_Y_AEZ.melt[ names( L122.ghgfert_tg_R_C_Y_AEZ.melt ) %!in% c( "fert_share", "total_emiss" )]

#Bind together dataframes & aggregate
L122.ghg_tg_R_agr_C_Y_AEZ.melt <- rbind( L122.ghg_tg_R_rice_Y_AEZ.melt, L122.ghgsoil_tg_R_C_Y_AEZ.melt, L122.ghgfert_tg_R_C_Y_AEZ.melt )
L122.ghg_tg_R_agr_C_Y_AEZ.melt <- na.omit( L122.ghg_tg_R_agr_C_Y_AEZ.melt )
L122.ghg_tg_R_agr_C_Y_AEZ.melt <- aggregate( L122.ghg_tg_R_agr_C_Y_AEZ.melt$emissions, by=as.list( L122.ghg_tg_R_agr_C_Y_AEZ.melt[ c( R_C_AEZ, "variable", "Non.CO2") ]), sum )


#Reshape
L122.ghg_tg_R_agr_C_Y_AEZ <- dcast( L122.ghg_tg_R_agr_C_Y_AEZ.melt, GCAM_region_ID + Non.CO2 + GCAM_commodity + AEZ ~ variable, value = c( "x" ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L122.ghg_tg_R_agr_C_Y_AEZ <- c( "Agriculture emissions by GCAM region / commodity / AEZ / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L122.ghg_tg_R_agr_C_Y_AEZ, domain="EMISSIONS_LEVEL1_DATA", fn="L122.ghg_tg_R_agr_C_Y_AEZ", comments=comments.L122.ghg_tg_R_agr_C_Y_AEZ )

# Every script should finish with this line
logstop()
