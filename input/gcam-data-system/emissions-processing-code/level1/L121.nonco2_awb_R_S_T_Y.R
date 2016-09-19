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
logstart( "L121.nonco2_awb_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical AWB emissions by GCAM technology, computed from EDGAR emissions data" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
L103.ag_Prod_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_Prod_Mt_R_C_Y_GLU" )
L111.ag_resbio_R_C <- readdata( "AGLU_LEVEL1_DATA", "L111.ag_resbio_R_C" )
EDGAR_SO2 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_SO2" )
EDGAR_CO <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CO" )
EDGAR_NOx <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NOx" )
EDGAR_VOC <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NMVOC" )
EDGAR_CH4 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CH4" )
EDGAR_N2O <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_N2O" )
EDGAR_NH3 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NH3" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Agricultural waste burning emissions by region are assigned to crops and land use regions on the basis of excess dry biomass..." )
printlog( "estimated from production, harvest index, and water content" )
L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU <- L103.ag_Prod_Mt_R_C_Y_GLU
L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU[ c( "HarvestIndex", "WaterContent" ) ] <- L111.ag_resbio_R_C[
  match( vecpaste( L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU[ R_C ] ),
         vecpaste( L111.ag_resbio_R_C[ R_C ] ) ),
  c( "HarvestIndex", "WaterContent" ) ]

#For fiber and fodder crops, default to harvest index of 1 and water content of 0.15
L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU$HarvestIndex[ is.na( L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU$HarvestIndex ) ] <- 1
L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU$WaterContent[ is.na( L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU$WaterContent ) ] <- 0.15

# Burnable excess biomass is equal to ( ( Production / HarvestIndex ) - Production ) * ( 1 - WaterContent )
# For root crops, the calculation could be done differently, if the root mass is excluded from the denominator of the reported harvest index.
# This doesn't seem to be the case in the literature--while for other crops, root mass is excluded from the harvest index, it is included for potatoes.
# If excluded, then the harvest index could be greater than 1 (if the tubers weigh more than the above-ground shoots), and the above calculation would
# return a negative number. None of the crops in the underlying harvested index database have values greater than 1 so this isn't currently an issue.
L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU[ X_historical_years ] <- with( L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU,
      ( L103.ag_Prod_Mt_R_C_Y_GLU[ X_historical_years ]  / HarvestIndex - L103.ag_Prod_Mt_R_C_Y_GLU[ X_historical_years ] ) *
      ( 1 - WaterContent ) )

L121.ag_ExcessDryBiomass_Mt_R_Y <- aggregate( L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU[ X_historical_years ],
                                  by = L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU[ R ], sum )
L121.ag_ExcessDryBiomass_Mt_R_Y.melt <- melt( L121.ag_ExcessDryBiomass_Mt_R_Y,
                                           id.vars = R, measure.vars = X_historical_years, variable.name = Y )

# Make table of ag waste burning share of emissions, for downscaling regional emissions to region/GLU/crop
L121.AWBshare_R_C_Y_GLU <- melt( L121.ag_ExcessDryBiomass_Mt_R_C_Y_GLU,
                                      id.vars = R_C_GLU, measure.vars = X_historical_years, variable.name = Y )
L121.AWBshare_R_C_Y_GLU$total_excess_bio <- L121.ag_ExcessDryBiomass_Mt_R_Y.melt$value[
  match( vecpaste( L121.AWBshare_R_C_Y_GLU[ c( R_Y )]),
         vecpaste( L121.ag_ExcessDryBiomass_Mt_R_Y.melt[ c( R_Y ) ] ) ) ]
L121.AWBshare_R_C_Y_GLU$AWB_emiss_share <- L121.AWBshare_R_C_Y_GLU$value / L121.AWBshare_R_C_Y_GLU$total_excess_bio
L121.AWBshare_R_C_Y_GLU <- L121.AWBshare_R_C_Y_GLU[ c( R_C_Y_GLU, "AWB_emiss_share" ) ]

printlog( "Compute EDGAR emissions by region" )
EDGAR_SO2$Non.CO2 <- "SO2_AWB"
EDGAR_CO$Non.CO2 <- "CO_AWB"
EDGAR_NOx$Non.CO2 <- "NOx_AWB"
EDGAR_VOC$Non.CO2 <- "NMVOC_AWB"
EDGAR_CH4$Non.CO2 <- "CH4_AWB"
EDGAR_N2O$Non.CO2 <- "N2O_AWB"
EDGAR_NH3$Non.CO2 <- "NH3_AWB"
EDGAR_VOC <- EDGAR_VOC[ names( EDGAR_VOC ) %!in% c( "X2009", "X2010" )]
L121.EDGAR <- rbind( EDGAR_SO2, EDGAR_CO, EDGAR_NOx, EDGAR_VOC, EDGAR_CH4, EDGAR_N2O, EDGAR_NH3 )
L121.EDGAR$sector <- EDGAR_sector$agg_sector[ match( L121.EDGAR$IPCC, EDGAR_sector$IPCC ) ]
L121.EDGAR$iso <- EDGAR_nation$iso[ match( L121.EDGAR$ISO_A3, EDGAR_nation$ISO_A3 )]
L121.EDGAR$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L121.EDGAR$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L121.EDGAR <- L121.EDGAR[ names( L121.EDGAR ) %!in% c( "IPCC.Annex", "World.Region", "ISO_A3", "iso", "Name", "IPCC", "IPCC_description" ) ]
L121.EDGAR <- na.omit( L121.EDGAR )
L121.EDGAR <- aggregate( L121.EDGAR[ X_EDGAR_historical_years ], by=as.list( L121.EDGAR[ c( "GCAM_region_ID", "Non.CO2", "sector")]), sum)
L121.EDGAR.melt <- melt( L121.EDGAR, id.vars = c( "GCAM_region_ID", "Non.CO2", "sector" ), variable.name = Y )

#Convert to Tg
L121.EDGAR.melt$value <- L121.EDGAR.melt$value * conv_Gg_Tg

#Subset for AWB
L121.EDGAR_awb.melt <- subset( L121.EDGAR.melt, L121.EDGAR.melt$sector == "ag_waste_burning" )

printlog( "Compute emissions by GCAM region, commodity, and GLU" )
L121.nonco2_tg_R_awb_C_Y_GLU.melt <- repeat_and_add_vector( L121.AWBshare_R_C_Y_GLU, "Non.CO2", awb_gases )
L121.nonco2_tg_R_awb_C_Y_GLU.melt$total_emiss <- L121.EDGAR_awb.melt$value[
  match( vecpaste( L121.nonco2_tg_R_awb_C_Y_GLU.melt[ c( R_Y, "Non.CO2" ) ] ),
         vecpaste( L121.EDGAR_awb.melt[ c( R_Y, "Non.CO2" ) ] ) ) ]
L121.nonco2_tg_R_awb_C_Y_GLU.melt$emissions <- with( L121.nonco2_tg_R_awb_C_Y_GLU.melt, total_emiss * AWB_emiss_share )

#Subset only the historical years in EDGAR, and reshape for write-out
L121.nonco2_tg_R_awb_C_Y_GLU.melt <- L121.nonco2_tg_R_awb_C_Y_GLU.melt[ L121.nonco2_tg_R_awb_C_Y_GLU.melt[[Y]] %in% X_EDGAR_historical_years, ]
L121.nonco2_tg_R_awb_C_Y_GLU <- dcast( L121.nonco2_tg_R_awb_C_Y_GLU.melt, GCAM_region_ID + Non.CO2 + GCAM_commodity + GLU ~ year,
                                       value.var = c( "emissions" ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L121.AWBshare_R_C_Y_GLU <- c( "Ag waste burning share of emissions by GCAM region / commodity / GLU / historical year", "unitless share" )
comments.L121.nonco2_tg_R_awb_C_Y_GLU <- c( "Ag waste burning emissions by GCAM region / commodity / GLU / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L121.AWBshare_R_C_Y_GLU, domain="EMISSIONS_LEVEL1_DATA", fn="L121.AWBshare_R_C_Y_GLU", comments=comments.L121.AWBshare_R_C_Y_GLU )
writedata( L121.nonco2_tg_R_awb_C_Y_GLU, domain="EMISSIONS_LEVEL1_DATA", fn="L121.nonco2_tg_R_awb_C_Y_GLU", comments=comments.L121.nonco2_tg_R_awb_C_Y_GLU )

# Every script should finish with this line
logstop()
