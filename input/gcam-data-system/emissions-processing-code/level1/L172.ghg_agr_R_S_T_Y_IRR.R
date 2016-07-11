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
logstart( "L172.ghg_agr_R_S_T_Y_IRR.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical AGR GHG emissions by GCAM technology, computed from EDGAR emissions data" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
L161.ag_irrProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_AEZ" )
L161.ag_rfdProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_AEZ" )
L142.ag_Fert_IO_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L142.ag_Fert_IO_R_C_Y_AEZ" )
EDGAR_CH4 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CH4" )
EDGAR_N2O <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_N2O" )
EDGAR_NH3 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NH3" )
EDGAR_NOx <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NOx" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compute share of regional production by technology" )
L161.ag_irrProd_Mt_R_C_Y_AEZ$irr <- "IRR"
L161.ag_rfdProd_Mt_R_C_Y_AEZ$irr <- "RFD" 
L172.ag_Prod_Mt_R_C_Y_AEZ <- rbind( L161.ag_irrProd_Mt_R_C_Y_AEZ, L161.ag_rfdProd_Mt_R_C_Y_AEZ)
L172.ag_Prod_Mt_R_C_Y_AEZ <- subset( L172.ag_Prod_Mt_R_C_Y_AEZ, L172.ag_Prod_Mt_R_C_Y_AEZ$GCAM_commodity %!in% c( "Pasture", "Forest" ))
L172.ag_Prod_Mt_R_C_Y_AEZ.melt <- melt( L172.ag_Prod_Mt_R_C_Y_AEZ, id.vars=c("GCAM_region_ID", "GCAM_commodity", "irr", "year" ))
L172.ag_Prod_Mt_R_Y <- aggregate( L172.ag_Prod_Mt_R_C_Y_AEZ.melt$value, by=as.list( L172.ag_Prod_Mt_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "year" ) ] ), sum )
L172.ag_pct_R_C_Y_AEZ.melt <- L172.ag_Prod_Mt_R_C_Y_AEZ.melt
L172.ag_pct_R_C_Y_AEZ.melt$total_prod <- L172.ag_Prod_Mt_R_Y$x[ match( vecpaste( L172.ag_pct_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "year" )]), vecpaste( L172.ag_Prod_Mt_R_Y[ c( "GCAM_region_ID", "year" )]))]
L172.ag_pct_R_C_Y_AEZ.melt$prod_share <- L172.ag_pct_R_C_Y_AEZ.melt$value / L172.ag_pct_R_C_Y_AEZ.melt$total_prod
names( L172.ag_pct_R_C_Y_AEZ.melt )[ names( L172.ag_pct_R_C_Y_AEZ.melt ) == "variable" ] <- "AEZ"
L172.ag_pct_R_C_Y_AEZ.melt$variable <- paste( "X", L172.ag_pct_R_C_Y_AEZ.melt$year, sep="")
L172.ag_pct_R_C_Y_AEZ.melt$value[ is.na( L172.ag_pct_R_C_Y_AEZ.melt$value ) ] <- 0

printlog( "Compute EDGAR emissions by region" )
EDGAR_CH4$Non.CO2 <- "CH4_AGR"
EDGAR_N2O$Non.CO2 <- "N2O_AGR"
EDGAR_NH3$Non.CO2 <- "NH3_AGR"
EDGAR_NOx$Non.CO2 <- "NOx_AGR"
L172.EDGAR <- rbind( EDGAR_CH4, EDGAR_N2O, EDGAR_NH3, EDGAR_NOx )
L172.EDGAR$sector <- EDGAR_sector$agg_sector[ match( L172.EDGAR$IPCC, EDGAR_sector$IPCC ) ]
L172.EDGAR$iso <- EDGAR_nation$iso[ match( L172.EDGAR$ISO_A3, EDGAR_nation$ISO_A3 )]
L172.EDGAR$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L172.EDGAR$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L172.EDGAR <- L172.EDGAR[ names( L172.EDGAR ) %!in% c( "IPCC.Annex", "World.Region", "ISO_A3", "iso", "Name", "IPCC", "IPCC_description" ) ]
L172.EDGAR <- na.omit( L172.EDGAR )
L172.EDGAR <- aggregate( L172.EDGAR[ X_EDGAR_historical_years ], by=as.list( L172.EDGAR[ c( "GCAM_region_ID", "Non.CO2", "sector")]), sum)
L172.EDGAR.melt <- melt( L172.EDGAR, id.vars = c( "GCAM_region_ID", "Non.CO2", "sector" ))

#Convert to Tg
L172.EDGAR.melt$value <- L172.EDGAR.melt$value / 1000.0 

#Subset for AGR
L172.EDGAR_agr.melt <- subset( L172.EDGAR.melt, L172.EDGAR.melt$sector %in% agr_sectors )

printlog( "Compute emissions from rice by GCAM region, commodity, and AEZ" )
#Subset for rice
L172.EDGAR_rice.melt <- subset( L172.EDGAR_agr.melt, L172.EDGAR_agr.melt$sector == "rice" )
L172.ag_Prod_Mt_R_rice_Y_AEZ <- subset( L172.ag_Prod_Mt_R_C_Y_AEZ.melt, L172.ag_Prod_Mt_R_C_Y_AEZ.melt$GCAM_commodity == "Rice" )

#Compute share of rice production by AEZ in each region / year
L172.ag_Prod_Mt_R_rice_Y <- aggregate( L172.ag_Prod_Mt_R_rice_Y_AEZ$value, by=as.list( L172.ag_Prod_Mt_R_rice_Y_AEZ[ c( "GCAM_region_ID", "year" ) ] ), sum)
L172.ag_pct_R_rice_Y_AEZ.melt <- L172.ag_Prod_Mt_R_rice_Y_AEZ
L172.ag_pct_R_rice_Y_AEZ.melt$total_prod <- L172.ag_Prod_Mt_R_rice_Y$x[ match( vecpaste( L172.ag_pct_R_rice_Y_AEZ.melt[ c( "GCAM_region_ID", "year" )]), vecpaste( L172.ag_Prod_Mt_R_rice_Y[ c( "GCAM_region_ID", "year" )]))]
L172.ag_pct_R_rice_Y_AEZ.melt$prod_share <- L172.ag_pct_R_rice_Y_AEZ.melt$value / L172.ag_pct_R_rice_Y_AEZ.melt$total_prod
names( L172.ag_pct_R_rice_Y_AEZ.melt )[ names( L172.ag_pct_R_rice_Y_AEZ.melt ) == "variable" ] <- "AEZ"
L172.ag_pct_R_rice_Y_AEZ.melt$variable <- paste( "X", L172.ag_pct_R_rice_Y_AEZ.melt$year, sep="")

L172.ghg_tg_R_rice_Y_AEZ.melt <- L172.ag_pct_R_rice_Y_AEZ.melt[ names( L172.ag_pct_R_rice_Y_AEZ.melt ) %in% c( R_C_AEZ, "irr", "variable", "prod_share" )]
L172.ghg_tg_R_rice_Y_AEZ.melt <- repeat_and_add_vector( L172.ghg_tg_R_rice_Y_AEZ.melt, "Non.CO2", agr_gases )
L172.ghg_tg_R_rice_Y_AEZ.melt$total_emiss <- L172.EDGAR_rice.melt$value[ match( vecpaste( L172.ghg_tg_R_rice_Y_AEZ.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )]), vecpaste( L172.EDGAR_rice.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )] ) )]
L172.ghg_tg_R_rice_Y_AEZ.melt$emissions <- L172.ghg_tg_R_rice_Y_AEZ.melt$total_emiss * L172.ghg_tg_R_rice_Y_AEZ.melt$prod_share
L172.ghg_tg_R_rice_Y_AEZ.melt$type <- "Rice"

printlog( "Compute emissions from soils by GCAM region, commodity, and AEZ" )
#Subset for soil
L172.EDGAR_soil.melt <- subset( L172.EDGAR_agr.melt, L172.EDGAR_agr.melt$sector == "soil" )

L172.ghgsoil_tg_R_C_Y_AEZ.melt <- L172.ag_pct_R_C_Y_AEZ.melt[ names( L172.ag_pct_R_C_Y_AEZ.melt ) %in% c( R_C_AEZ, "irr", "variable", "prod_share" )]
L172.ghgsoil_tg_R_C_Y_AEZ.melt <- repeat_and_add_vector( L172.ghgsoil_tg_R_C_Y_AEZ.melt, "Non.CO2", agr_gases )
L172.ghgsoil_tg_R_C_Y_AEZ.melt$total_emiss <- L172.EDGAR_soil.melt$value[ match( vecpaste( L172.ghgsoil_tg_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )]), vecpaste( L172.EDGAR_soil.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )] ) )]
L172.ghgsoil_tg_R_C_Y_AEZ.melt$emissions <- L172.ghgsoil_tg_R_C_Y_AEZ.melt$total_emiss * L172.ghgsoil_tg_R_C_Y_AEZ.melt$prod_share
L172.ghgsoil_tg_R_C_Y_AEZ.melt$type <- "Soil"

printlog( "Compute emissions from fertilizer by GCAM region, commodity, and AEZ" )
#Subset for fertizizer
L172.EDGAR_fert.melt <- subset( L172.EDGAR_agr.melt, L172.EDGAR_agr.melt$sector == "fertilizer" )

#Compute fertilizer by crop
L172.fert_IO_R_C_Y_AEZ.melt <- melt( L142.ag_Fert_IO_R_C_Y_AEZ, id.vars=c( R_C_AEZ ) )

L172.fert_Mt_R_C_Y_AEZ.melt <- L172.ag_Prod_Mt_R_C_Y_AEZ.melt
names( L172.fert_Mt_R_C_Y_AEZ.melt )[ names( L172.fert_Mt_R_C_Y_AEZ.melt ) == "variable" ] <- "AEZ"
names( L172.fert_Mt_R_C_Y_AEZ.melt )[ names( L172.fert_Mt_R_C_Y_AEZ.melt ) == "value" ] <- "ag_production"
L172.fert_Mt_R_C_Y_AEZ.melt$variable <- paste( "X", L172.fert_Mt_R_C_Y_AEZ.melt$year, sep="")
L172.fert_Mt_R_C_Y_AEZ.melt$IO <- L172.fert_IO_R_C_Y_AEZ.melt$value[ match( vecpaste(L172.fert_Mt_R_C_Y_AEZ.melt[ c( R_C_AEZ, "variable" )]), vecpaste( L172.fert_IO_R_C_Y_AEZ.melt[ c( R_C_AEZ, "variable" )]))]
L172.fert_Mt_R_C_Y_AEZ.melt$fertilizer <- L172.fert_Mt_R_C_Y_AEZ.melt$ag_production * L172.fert_Mt_R_C_Y_AEZ.melt$IO

L172.fert_Mt_R_Y.melt <- aggregate( L172.fert_Mt_R_C_Y_AEZ.melt$fertilizer, by=as.list( L172.fert_Mt_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "variable" )]), sum)
L172.fert_pct_R_C_Y_AEZ.melt <- L172.fert_Mt_R_C_Y_AEZ.melt[ c( R_C_AEZ, "irr", "variable", "fertilizer" )]
L172.fert_pct_R_C_Y_AEZ.melt$tot_fert <- L172.fert_Mt_R_Y.melt$x[ match( vecpaste(L172.fert_pct_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "variable" )]), vecpaste( L172.fert_Mt_R_Y.melt[ c( "GCAM_region_ID", "variable" )]))]
L172.fert_pct_R_C_Y_AEZ.melt$fert_share <- L172.fert_pct_R_C_Y_AEZ.melt$fertilizer / L172.fert_pct_R_C_Y_AEZ.melt$tot_fert

L172.ghgfert_tg_R_C_Y_AEZ.melt <- L172.fert_pct_R_C_Y_AEZ.melt[ names( L172.fert_pct_R_C_Y_AEZ.melt ) %in% c( R_C_AEZ, "irr", "variable", "fert_share" )]
L172.ghgfert_tg_R_C_Y_AEZ.melt <- repeat_and_add_vector( L172.ghgfert_tg_R_C_Y_AEZ.melt, "Non.CO2", agr_gases )
L172.ghgfert_tg_R_C_Y_AEZ.melt$total_emiss <- L172.EDGAR_fert.melt$value[ match( vecpaste( L172.ghgfert_tg_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )]), vecpaste(L172.EDGAR_fert.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )] ) )]
L172.ghgfert_tg_R_C_Y_AEZ.melt$emissions <- L172.ghgfert_tg_R_C_Y_AEZ.melt$total_emiss * L172.ghgfert_tg_R_C_Y_AEZ.melt$fert_share
L172.ghgfert_tg_R_C_Y_AEZ.melt$type <- "Fertilizer"

printlog( "Sum all AGR emissions by GCAM region, commodity, and AEZ" )
#Drop unnecessary columns
L172.ghg_tg_R_rice_Y_AEZ.melt <- L172.ghg_tg_R_rice_Y_AEZ.melt[ names( L172.ghg_tg_R_rice_Y_AEZ.melt ) %!in% c( "prod_share", "total_emiss" )]
L172.ghgsoil_tg_R_C_Y_AEZ.melt <- L172.ghgsoil_tg_R_C_Y_AEZ.melt[ names( L172.ghgsoil_tg_R_C_Y_AEZ.melt ) %!in% c( "prod_share", "total_emiss" )]
L172.ghgfert_tg_R_C_Y_AEZ.melt <- L172.ghgfert_tg_R_C_Y_AEZ.melt[ names( L172.ghgfert_tg_R_C_Y_AEZ.melt ) %!in% c( "fert_share", "total_emiss" )]

#Bind together dataframes & aggregate
L172.ghg_tg_R_agr_C_Y_AEZ.melt <- rbind( L172.ghg_tg_R_rice_Y_AEZ.melt, L172.ghgsoil_tg_R_C_Y_AEZ.melt, L172.ghgfert_tg_R_C_Y_AEZ.melt )
L172.ghg_tg_R_agr_C_Y_AEZ.melt <- na.omit( L172.ghg_tg_R_agr_C_Y_AEZ.melt )
L172.ghg_tg_R_agr_C_Y_AEZ.melt <- aggregate( L172.ghg_tg_R_agr_C_Y_AEZ.melt$emissions, by=as.list( L172.ghg_tg_R_agr_C_Y_AEZ.melt[ c( R_C_AEZ, "irr", "variable", "Non.CO2") ]), sum )

#Reshape
L172.ghg_tg_R_agr_C_Y_AEZ_IRR <- dcast( L172.ghg_tg_R_agr_C_Y_AEZ.melt, GCAM_region_ID + Non.CO2 + GCAM_commodity + irr + AEZ ~ variable, value.var = c( "x" ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L172.ghg_tg_R_agr_C_Y_AEZ_IRR <- c( "Agriculture emissions by GCAM region / commodity / AEZ / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L172.ghg_tg_R_agr_C_Y_AEZ_IRR, domain="EMISSIONS_LEVEL1_DATA", fn="L172.ghg_tg_R_agr_C_Y_AEZ_IRR", comments=comments.L172.ghg_tg_R_agr_C_Y_AEZ_IRR )

# Every script should finish with this line
logstop()
