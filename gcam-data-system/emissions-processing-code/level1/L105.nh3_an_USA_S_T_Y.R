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
logstart( "L105.nh3_an_USA_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions factors by GCAM technology, computed from EPA emissions data and IEA energy balances" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EPA_tech <- readdata( "EMISSIONS_MAPPINGS", "EPA_tech" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
L107.an_Prod_Mt_R_C_Sys_Fd_Y <- readdata( "AGLU_LEVEL1_DATA", "L107.an_Prod_Mt_R_C_Sys_Fd_Y" )
EPA_NH3 <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_NH3" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# Combine all energy driver data into a single dataframe
printlog( "Convert EPA GHG emissions inventory to Tg and aggregate by sector and technology" )
L105.nh3_tg_USA_an_Sepa_F_Yh <- EPA_NH3
L105.nh3_tg_USA_an_Sepa_F_Yh$sector <- EPA_tech$sector[ match( L105.nh3_tg_USA_an_Sepa_F_Yh$Source_Category , EPA_tech$EPA_Category )]
L105.nh3_tg_USA_an_Sepa_F_Yh$fuel <- EPA_tech$fuel[ match( L105.nh3_tg_USA_an_Sepa_F_Yh$Source_Category , EPA_tech$EPA_Category )]
L105.nh3_tg_USA_an_Sepa_F_Yh <- aggregate( L105.nh3_tg_USA_an_Sepa_F_Yh[ EPA_NH3_historical_years ], by=as.list( L105.nh3_tg_USA_an_Sepa_F_Yh[ c( "sector", "fuel" ) ] ), sum )

#Drop missing values and subset for animals
L105.nh3_tg_USA_an_Sepa_F_Yh[ is.na( L105.nh3_tg_USA_an_Sepa_F_Yh ) ] <- 0
L105.nh3_tg_USA_an_Sepa_F_Yh <- subset( L105.nh3_tg_USA_an_Sepa_F_Yh, L105.nh3_tg_USA_an_Sepa_F_Yh$sector == "Animals" )

#Convert to Tg
L105.nh3_tg_USA_an_Sepa_F_Yh[ EPA_NH3_historical_years ] <- L105.nh3_tg_USA_an_Sepa_F_Yh[ EPA_NH3_historical_years ] * tst_to_tg  

printlog( "Compute GHG emissions factors by dividing EPA inventory by FAO animal production" )
#Subset for USA only in 2005 and aggregate to EPA categories"
L105.out_Mt_USA_an_C_Sys_Fd_Yh <- subset( L107.an_Prod_Mt_R_C_Sys_Fd_Y, L107.an_Prod_Mt_R_C_Sys_Fd_Y$GCAM_region_ID == "1" )
L105.out_Mt_USA_an_C_Sys_Fd_Yh.melt <- melt( L105.out_Mt_USA_an_C_Sys_Fd_Yh, id.vars = c( "GCAM_region_ID", "GCAM_commodity", "system", "feed" ) )
L105.out_Mt_USA_an_Yh.melt <- aggregate( L105.out_Mt_USA_an_C_Sys_Fd_Yh.melt$value, by=as.list( L105.out_Mt_USA_an_C_Sys_Fd_Yh.melt[ c( "variable" )]), sum )
names( L105.out_Mt_USA_an_Yh.melt )[ names( L105.out_Mt_USA_an_Yh.melt ) == "x" ] <- "production"
L105.out_Mt_USA_an_Yh.melt <- subset( L105.out_Mt_USA_an_Yh.melt, L105.out_Mt_USA_an_Yh.melt$variable %in% EPA_NH3_historical_years )

L105.nh3_tgmt_USA_an_Yh.melt <- melt( L105.nh3_tg_USA_an_Sepa_F_Yh, id.vars=c( "sector", "fuel" ) )
L105.nh3_tgmt_USA_an_Yh.melt$production <- L105.out_Mt_USA_an_Yh.melt$production[ match( L105.nh3_tgmt_USA_an_Yh.melt$variable, L105.out_Mt_USA_an_Yh.melt$variable   )]
L105.nh3_tgmt_USA_an_Yh.melt$nh3_em_factor <- L105.nh3_tgmt_USA_an_Yh.melt$value / L105.nh3_tgmt_USA_an_Yh.melt$production
L105.nh3_tgmt_USA_an_Yh.melt$nh3_em_factor[ L105.nh3_tgmt_USA_an_Yh.melt$nh3_em_factor == "Inf" ] <- 0
L105.nh3_tgmt_USA_an_Yh.melt$nh3_em_factor[ is.na( L105.nh3_tgmt_USA_an_Yh.melt$nh3_em_factor ) ] <- 0

#Drop unnecessary columns
L105.nh3_tgmt_USA_an_Yh.melt <- L105.nh3_tgmt_USA_an_Yh.melt[ names( L105.nh3_tgmt_USA_an_Yh.melt ) %!in% c( "value", "production" )]

#Add extra years
for ( y in X_NH3_extra_years ) {
  temp <- subset( L105.nh3_tgmt_USA_an_Yh.melt, L105.nh3_tgmt_USA_an_Yh.melt$variable == "X1990" )
  temp$variable <- y
  L105.nh3_tgmt_USA_an_Yh.melt <- rbind( L105.nh3_tgmt_USA_an_Yh.melt, temp )
}

#Reshape
L105.nh3_tgmt_USA_an_Yh <- dcast( L105.nh3_tgmt_USA_an_Yh.melt, sector + fuel ~ variable, value = c( "nh3_em_factor" ))

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L105.nh3_tgmt_USA_an_Yh <- c( "NH3 emissions factors by sector / technology / year", "Unit = Tg / Mt" )

#write tables as CSV files
writedata( L105.nh3_tgmt_USA_an_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L105.nh3_tgmt_USA_an_Yh", comments=comments.L105.nh3_tgmt_USA_an_Yh )

# Every script should finish with this line
logstop()
