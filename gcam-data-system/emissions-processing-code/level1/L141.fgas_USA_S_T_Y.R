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
logstart( "L141.fgas_USA_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions factors by GCAM technology, computed from EPA emissions data and IEA energy balances" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_tech <- readdata( "EMISSIONS_MAPPINGS", "gcam_fgas_tech" )
EPA_fgas_tech <- readdata( "EMISSIONS_MAPPINGS", "EPA_fgas_tech" )
A41.GWP <- readdata( "EMISSIONS_ASSUMPTIONS", "A41.GWP" )
EPA_Fgas <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_Fgas" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Map EPA F-gas emissions to GCAM sectors, technologies, and gases" )
#First, set up data frame
L141.fgas_tg_USA_S_2005.melt <- GCAM_tech
L141.fgas_tg_USA_S_2005.melt <- repeat_and_add_vector( L141.fgas_tg_USA_S_2005.melt, "Non.CO2", F_Gases )

#Then, prepare EPA data for use
L141.EPA_fgas <- EPA_Fgas 
L141.EPA_fgas$EPA_agg_sector <- EPA_fgas_tech$sector[ match( L141.EPA_fgas$Source_Category, EPA_fgas_tech$EPA_Category )]
L141.EPA_fgas <- L141.EPA_fgas[ names( L141.EPA_fgas ) != "Source_Category" ]
L141.EPA_fgas.melt <- melt( L141.EPA_fgas, id.vars=c( "EPA_agg_sector" ) )
L141.EPA_fgas.melt <- aggregate( L141.EPA_fgas.melt$value, by=as.list( L141.EPA_fgas.melt[ c( "EPA_agg_sector", "variable" ) ] ), sum )
names( L141.EPA_fgas.melt )[ names( L141.EPA_fgas.melt ) == "variable" ] <- "Non.CO2"
names( L141.EPA_fgas.melt )[ names( L141.EPA_fgas.melt ) == "x" ] <- "emissions"

#Replace the "." in the EPA gas names
L141.EPA_fgas.melt$Non.CO2 <- gsub( "\\.", "", L141.EPA_fgas.melt$Non.CO2 )

#Now, map in emissions
L141.fgas_tg_USA_S_2005.melt$emissions <- L141.EPA_fgas.melt$emissions[ match( vecpaste( L141.fgas_tg_USA_S_2005.melt[ c( "EPA_sector", "Non.CO2")]), vecpaste( L141.EPA_fgas.melt[ c( "EPA_agg_sector", "Non.CO2" )] ))]

printlog( "Compute the % EDGAR emissions by each gas & technology" )
L141.fgas_tg_USA_S_2005.melt$HFC_PFC <- A41.GWP$HFC_PFC[ match( L141.fgas_tg_USA_S_2005.melt$Non.CO2, A41.GWP$Gas )]
L141.fgas_tg_USA_S_2005.melt$emissions[ is.na( L141.fgas_tg_USA_S_2005.melt$emissions ) ] <- 0
L141.hfc.pfc_tg_USA_Sedgar_2005.melt <- aggregate( L141.fgas_tg_USA_S_2005.melt$emissions, by=as.list( L141.fgas_tg_USA_S_2005.melt[ c( "EDGAR_agg_sector", "HFC_PFC")]), sum )
L141.hfc.pfc_ct_USA_Sedgar_2005.melt <- aggregate( L141.fgas_tg_USA_S_2005.melt$emissions, by=as.list( L141.fgas_tg_USA_S_2005.melt[ c( "EDGAR_agg_sector", "HFC_PFC")]), length )
L141.fgas_tg_USA_S_2005.melt$sector_total <- L141.hfc.pfc_tg_USA_Sedgar_2005.melt$x[ match( vecpaste( L141.fgas_tg_USA_S_2005.melt[ c( "EDGAR_agg_sector", "HFC_PFC" )]), vecpaste( L141.hfc.pfc_tg_USA_Sedgar_2005.melt[ c( "EDGAR_agg_sector", "HFC_PFC")] ))]
L141.fgas_tg_USA_S_2005.melt$sector_count <- L141.hfc.pfc_ct_USA_Sedgar_2005.melt$x[ match( vecpaste( L141.fgas_tg_USA_S_2005.melt[ c( "EDGAR_agg_sector", "HFC_PFC" )]), vecpaste( L141.hfc.pfc_tg_USA_Sedgar_2005.melt[ c( "EDGAR_agg_sector", "HFC_PFC")] ))]
L141.fgas_tg_USA_S_2005.melt$pct <- L141.fgas_tg_USA_S_2005.melt$emissions / L141.fgas_tg_USA_S_2005.melt$sector_total 
L141.fgas_tg_USA_S_2005.melt$pct[ L141.fgas_tg_USA_S_2005.melt$pct == "NaN" ] <- 1 / L141.fgas_tg_USA_S_2005.melt$sector_count[ L141.fgas_tg_USA_S_2005.melt$pct == "NaN" ] 
L141.fgas_tg_USA_S_2005.melt <- na.omit( L141.fgas_tg_USA_S_2005.melt )

#Drop unnecessary columns
L141.fgas_pct_USA_S_2005 <- L141.fgas_tg_USA_S_2005.melt[ names( L141.fgas_tg_USA_S_2005.melt ) %!in% c( "sector_total", "emissions", "EPA_sector" ) ]
L141.fgas_pct_USA_S_2005[ is.na( L141.fgas_pct_USA_S_2005 ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L141.fgas_pct_USA_S_2005 <- c( "% of 2005 USA HFC & PFC emissions by sector / gas", "Unit = %" )

#write tables as CSV files
writedata( L141.fgas_pct_USA_S_2005, domain="EMISSIONS_LEVEL1_DATA", fn="L141.fgas_pct_USA_S_2005", comments=comments.L141.fgas_pct_USA_S_2005 )

# Every script should finish with this line
logstop()
