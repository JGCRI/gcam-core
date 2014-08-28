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
logstart( "L103.ghg_an_USA_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions factors for animals by GCAM technology, computed from EPA emissions data and FAO animal data" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EPA_tech <- readdata( "EMISSIONS_MAPPINGS", "EPA_ghg_tech" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
L107.an_Prod_Mt_R_C_Sys_Fd_Y <- readdata( "AGLU_LEVEL1_DATA", "L107.an_Prod_Mt_R_C_Sys_Fd_Y" )
EPA_FCCC_GHG_2005 <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_FCCC_AG_2005" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Convert EPA GHG emissions inventory to Tg and aggregate by sector and technology" )
L103.ghg_tg_USA_an_Sepa_F_2005 <- EPA_FCCC_GHG_2005
L103.ghg_tg_USA_an_Sepa_F_2005$sector <- EPA_tech$sector[ match( L103.ghg_tg_USA_an_Sepa_F_2005$Source_Category , EPA_tech$Source_Category )]
L103.ghg_tg_USA_an_Sepa_F_2005$fuel <- EPA_tech$fuel[ match( L103.ghg_tg_USA_an_Sepa_F_2005$Source_Category , EPA_tech$Source_Category )]
L103.ghg_tg_USA_an_Sepa_F_2005 <- aggregate( L103.ghg_tg_USA_an_Sepa_F_2005[ GHG_names ], by=as.list( L103.ghg_tg_USA_an_Sepa_F_2005[ c( "sector", "fuel" ) ] ), sum )

#Drop missing values
L103.ghg_tg_USA_an_Sepa_F_2005[ is.na( L103.ghg_tg_USA_an_Sepa_F_2005 ) ] <- 0

#Convert to Tg
L103.ghg_tg_USA_an_Sepa_F_2005[ GHG_names ] <- L103.ghg_tg_USA_an_Sepa_F_2005[ GHG_names ] * gg_to_tg  

printlog( "Compute GHG emissions factors by dividing EPA inventory by FAO animal production" )
#Subset for USA only in 2005 and aggregate to EPA categories"
L103.out_Mt_USA_an_C_Sys_Fd_Yh <- subset( L107.an_Prod_Mt_R_C_Sys_Fd_Y, L107.an_Prod_Mt_R_C_Sys_Fd_Y$GCAM_region_ID == "1" )
L103.out_Mt_USA_an_C_Sys_Fd_Yh.melt <- melt( L103.out_Mt_USA_an_C_Sys_Fd_Yh, id.vars = c( "GCAM_region_ID", "GCAM_commodity", "system", "feed" ) )
L103.out_Mt_USA_an_C_Sys_Fd_Yh.melt$EPA_agg_sector <- GCAM_sector_tech$EPA_agg_sector[ match( vecpaste( L103.out_Mt_USA_an_C_Sys_Fd_Yh.melt[ c( "GCAM_commodity", "system", "feed" )] ), vecpaste( GCAM_sector_tech[ c( "sector", "fuel", "technology" )]) )]
L103.out_Mt_USA_an_C_Sys_Fd_Yh.melt <- aggregate( L103.out_Mt_USA_an_C_Sys_Fd_Yh.melt$value, by=as.list( L103.out_Mt_USA_an_C_Sys_Fd_Yh.melt[ c( "EPA_agg_sector", "variable" )]), sum )
names( L103.out_Mt_USA_an_C_Sys_Fd_Yh.melt )[ names( L103.out_Mt_USA_an_C_Sys_Fd_Yh.melt ) == "x" ] <- "production"
L103.out_Mt_USA_an_C_Sys_Fd_2005.melt <- subset( L103.out_Mt_USA_an_C_Sys_Fd_Yh.melt, L103.out_Mt_USA_an_C_Sys_Fd_Yh.melt$variable == "X2005" )

L103.ghg_tgmt_USA_an_Sepa_F_2005 <- L103.ghg_tg_USA_an_Sepa_F_2005
L103.ghg_tgmt_USA_an_Sepa_F_2005$production <- L103.out_Mt_USA_an_C_Sys_Fd_2005.melt$production[ match( vecpaste( L103.ghg_tgmt_USA_an_Sepa_F_2005[ c( "sector" )]), vecpaste( L103.out_Mt_USA_an_C_Sys_Fd_2005.melt[ c( "EPA_agg_sector" ) ] )  )]
L103.ghg_tgmt_USA_an_Sepa_F_2005$ch4_em_factor <- L103.ghg_tgmt_USA_an_Sepa_F_2005$CH4 / L103.ghg_tgmt_USA_an_Sepa_F_2005$production
L103.ghg_tgmt_USA_an_Sepa_F_2005$ch4_em_factor[ L103.ghg_tgmt_USA_an_Sepa_F_2005$ch4_em_factor == "Inf" ] <- 0
L103.ghg_tgmt_USA_an_Sepa_F_2005$ch4_em_factor[ is.na( L103.ghg_tgmt_USA_an_Sepa_F_2005$ch4_em_factor ) ] <- 0

#Drop unnecessary columns
L103.ghg_tgmt_USA_an_Sepa_F_2005<- L103.ghg_tgmt_USA_an_Sepa_F_2005[ names( L103.ghg_tgmt_USA_an_Sepa_F_2005 ) %!in% c( "CH4", "CO2", "N2O", "production" )]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L103.ghg_tgmt_USA_an_Sepa_F_2005 <- c( "GHG emissions factors for animals by sector / technology / 2005", "Unit = Tg / Mt" )

#write tables as CSV files
writedata( L103.ghg_tgmt_USA_an_Sepa_F_2005, domain="EMISSIONS_LEVEL1_DATA", fn="L103.ghg_tgmt_USA_an_Sepa_F_2005", comments=comments.L103.ghg_tgmt_USA_an_Sepa_F_2005 )

# Every script should finish with this line
logstop()
