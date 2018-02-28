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
printlog( "Historical AGR GHG emissions by GCAM technology, computed from EDGAR emissions data" )

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
L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU" )
L142.ag_Fert_IO_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L142.ag_Fert_IO_R_C_Y_GLU" )
EDGAR_CH4 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CH4" )
EDGAR_N2O <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_N2O" )
EDGAR_NH3 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NH3" )
EDGAR_NOx <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NOx" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compute shares of regional cropland allocation by crop type, and regional production of each crop by each GLU" )
# In the downscaling from (geopolitical) region to crop and GLU, we use land area to go from region to region/crop, and
# production to go from region/crop to region/GLU/crop
#Land area shares (region/crop within region)
L122.LC_bm2_R_HarvCropLand_C_Y_GLU.melt <- melt( L122.LC_bm2_R_HarvCropLand_C_Yh_GLU,
                                                 id.vars = R_C_GLU, measure.vars = X_historical_years, variable.name = Y )
L122.LC_bm2_R_HarvCropLand_C_Y.melt <- aggregate( L122.LC_bm2_R_HarvCropLand_C_Y_GLU.melt[ "value" ],
                                                  by = L122.LC_bm2_R_HarvCropLand_C_Y_GLU.melt[ R_C_Y ], sum )
L122.LC_bm2_R_HarvCropLand_Y.melt <- aggregate( L122.LC_bm2_R_HarvCropLand_C_Y_GLU.melt[ "value" ],
                                                by = L122.LC_bm2_R_HarvCropLand_C_Y_GLU.melt[ R_Y ], sum )
L122.CropAreaShare_R_C_Y <- L122.LC_bm2_R_HarvCropLand_C_Y.melt
L122.CropAreaShare_R_C_Y$crop_area_total <- L122.LC_bm2_R_HarvCropLand_Y.melt$value[
  match( vecpaste( L122.CropAreaShare_R_C_Y[ R_Y ] ),
         vecpaste( L122.LC_bm2_R_HarvCropLand_Y.melt[ R_Y ] ) ) ]
L122.CropAreaShare_R_C_Y$crop_area_share <- with( L122.CropAreaShare_R_C_Y, value / crop_area_total )

#Production shares (region/GLU/crop within region/crop)
L122.ag_Prod_Mt_R_C_Y <- aggregate( L103.ag_Prod_Mt_R_C_Y_GLU[ X_historical_years ],
                                  by = L103.ag_Prod_Mt_R_C_Y_GLU[ R_C ], sum)
L122.ag_Prod_Mt_R_C_Y.melt <- melt( L122.ag_Prod_Mt_R_C_Y, id.vars = R_C, variable.name = Y )
L122.ProdGLUshare_R_C_Y_GLU.melt <- melt( L103.ag_Prod_Mt_R_C_Y_GLU, id.vars = R_C_GLU, variable.name = Y )
L122.ProdGLUshare_R_C_Y_GLU.melt$Prod_R_C <- L122.ag_Prod_Mt_R_C_Y.melt$value[
  match( vecpaste( L122.ProdGLUshare_R_C_Y_GLU.melt[ c( R_C_Y )]),
         vecpaste( L122.ag_Prod_Mt_R_C_Y.melt[ c( R_C_Y ) ] ) ) ]
L122.ProdGLUshare_R_C_Y_GLU.melt$prod_share_GLU <- with( L122.ProdGLUshare_R_C_Y_GLU.melt, value / Prod_R_C )
L122.ProdGLUshare_R_C_Y_GLU.melt$prod_share_GLU[ is.na( L122.ProdGLUshare_R_C_Y_GLU.melt$prod_share_GLU ) ] <- 0

# Emissions shares: product of region/crop shares and region/crop/glu shares
L122.EmissShare_R_C_Y_GLU <- L122.ProdGLUshare_R_C_Y_GLU.melt[ c( R_C_Y_GLU, "prod_share_GLU" ) ]
L122.EmissShare_R_C_Y_GLU$crop_area_share <- L122.CropAreaShare_R_C_Y$crop_area_share[
  match( vecpaste( L122.EmissShare_R_C_Y_GLU[ R_C_Y ]),
         vecpaste( L122.CropAreaShare_R_C_Y[ R_C_Y ] ) ) ]
L122.EmissShare_R_C_Y_GLU$emiss_share <- with( L122.EmissShare_R_C_Y_GLU, prod_share_GLU * crop_area_share )
L122.EmissShare_R_C_Y_GLU <- L122.EmissShare_R_C_Y_GLU[ c( R_C_Y_GLU, "emiss_share" ) ]

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
L122.EDGAR <- aggregate( L122.EDGAR[ X_EDGAR_historical_years ],
                         by = L122.EDGAR[ c( "GCAM_region_ID", "Non.CO2", "sector")], sum)
L122.EDGAR.melt <- melt( L122.EDGAR, id.vars = c( "GCAM_region_ID", "Non.CO2", "sector" ), variable.name = Y)

#Convert to Tg
L122.EDGAR.melt$value <- L122.EDGAR.melt$value * conv_Gg_Tg

#Subset for AGR
L122.EDGAR_agr.melt <- subset( L122.EDGAR.melt, L122.EDGAR.melt$sector %in% agr_sectors )

printlog( "Compute emissions from rice by GCAM region, commodity, and GLU" )
#Subset for rice
L122.EDGAR_rice.melt <- subset( L122.EDGAR_agr.melt, L122.EDGAR_agr.melt$sector == "rice" )
L122.ag_Prod_Mt_R_rice_Y_GLU <- subset( L103.ag_Prod_Mt_R_C_Y_GLU, GCAM_commodity == "Rice" )

#Compute share of rice production by GLU in each region / year
L122.ag_Prod_Mt_R_rice_Y <- aggregate( L122.ag_Prod_Mt_R_rice_Y_GLU[ X_historical_years ],
                                       by = L122.ag_Prod_Mt_R_rice_Y_GLU[ R_C ], sum)
L122.ag_Prod_Mt_R_rice_Y.melt <- melt( L122.ag_Prod_Mt_R_rice_Y, id.vars = R_C, variable.name = Y )
L122.ag_pct_R_rice_Y_GLU.melt <- melt( L122.ag_Prod_Mt_R_rice_Y_GLU, id.vars = R_C_GLU, variable.name = Y )
L122.ag_pct_R_rice_Y_GLU.melt$total_prod <- L122.ag_Prod_Mt_R_rice_Y.melt$value[
  match( vecpaste( L122.ag_pct_R_rice_Y_GLU.melt[ R_C_Y ] ),
         vecpaste( L122.ag_Prod_Mt_R_rice_Y.melt[ R_C_Y ] ) ) ]
L122.ag_pct_R_rice_Y_GLU.melt$prod_share <- L122.ag_pct_R_rice_Y_GLU.melt$value / L122.ag_pct_R_rice_Y_GLU.melt$total_prod

L122.ghg_tg_R_rice_Y_GLU.melt <- L122.ag_pct_R_rice_Y_GLU.melt[ c( R_C_Y_GLU, "prod_share" )]
L122.ghg_tg_R_rice_Y_GLU.melt <- repeat_and_add_vector( L122.ghg_tg_R_rice_Y_GLU.melt, "Non.CO2", agr_gases )
L122.ghg_tg_R_rice_Y_GLU.melt$total_emiss <- L122.EDGAR_rice.melt$value[
  match( vecpaste( L122.ghg_tg_R_rice_Y_GLU.melt[ c( R_Y, "Non.CO2" )]),
         vecpaste( L122.EDGAR_rice.melt[ c( R_Y, "Non.CO2" ) ] ) ) ]
L122.ghg_tg_R_rice_Y_GLU.melt$emissions <- L122.ghg_tg_R_rice_Y_GLU.melt$total_emiss * L122.ghg_tg_R_rice_Y_GLU.melt$prod_share
L122.ghg_tg_R_rice_Y_GLU.melt$type <- "Rice"

printlog( "Compute emissions from soils by GCAM region, commodity, and GLU" )
#Subset for soil
L122.EDGAR_soil.melt <- subset( L122.EDGAR_agr.melt, L122.EDGAR_agr.melt$sector == "soil" )

L122.ghgsoil_tg_R_C_Y_GLU.melt <- repeat_and_add_vector( L122.EmissShare_R_C_Y_GLU, "Non.CO2", unique( L122.EDGAR_soil.melt$Non.CO2) )
L122.ghgsoil_tg_R_C_Y_GLU.melt$total_emiss <- L122.EDGAR_soil.melt$value[
  match( vecpaste( L122.ghgsoil_tg_R_C_Y_GLU.melt[ c( R_Y, "Non.CO2" )]),
         vecpaste( L122.EDGAR_soil.melt[ c( R_Y, "Non.CO2" ) ] ) ) ]
L122.ghgsoil_tg_R_C_Y_GLU.melt$emissions <- L122.ghgsoil_tg_R_C_Y_GLU.melt$total_emiss * L122.ghgsoil_tg_R_C_Y_GLU.melt$emiss_share
L122.ghgsoil_tg_R_C_Y_GLU.melt$type <- "Soil"

printlog( "Compute emissions from fertilizer by GCAM region, commodity, and GLU" )
#Subset for fertizizer
L122.EDGAR_fert.melt <- subset( L122.EDGAR_agr.melt, L122.EDGAR_agr.melt$sector == "fertilizer" )

#Compute fertilizer by crop
L122.fert_IO_R_C_Y_GLU.melt <- melt( L142.ag_Fert_IO_R_C_Y_GLU, id.vars=c( R_C_GLU ), variable.name = Y )

L122.fert_Mt_R_C_Y_GLU.melt <- melt( L103.ag_Prod_Mt_R_C_Y_GLU, id.vars=c( R_C_GLU ), variable.name = Y, value.name = "ag_production" )
L122.fert_Mt_R_C_Y_GLU.melt$IO <- L122.fert_IO_R_C_Y_GLU.melt$value[
  match( vecpaste(L122.fert_Mt_R_C_Y_GLU.melt[ R_C_Y_GLU ] ),
         vecpaste( L122.fert_IO_R_C_Y_GLU.melt[ R_C_Y_GLU ] ) ) ]
L122.fert_Mt_R_C_Y_GLU.melt$fertilizer <- with( L122.fert_Mt_R_C_Y_GLU.melt, ag_production * IO )

L122.fert_Mt_R_Y.melt <- aggregate( L122.fert_Mt_R_C_Y_GLU.melt[ "fertilizer" ],
                                    by = L122.fert_Mt_R_C_Y_GLU.melt[ R_Y ], sum)
L122.fert_pct_R_C_Y_GLU.melt <- L122.fert_Mt_R_C_Y_GLU.melt[ c(R_C_Y_GLU, "fertilizer" )]
L122.fert_pct_R_C_Y_GLU.melt$tot_fert <- L122.fert_Mt_R_Y.melt$fertilizer[
  match( vecpaste(L122.fert_pct_R_C_Y_GLU.melt[ R_Y ] ),
         vecpaste( L122.fert_Mt_R_Y.melt[ R_Y ] ) ) ]
L122.fert_pct_R_C_Y_GLU.melt$fert_share <- with( L122.fert_pct_R_C_Y_GLU.melt, fertilizer / tot_fert )

L122.ghgfert_tg_R_C_Y_GLU.melt <- L122.fert_pct_R_C_Y_GLU.melt[ c( R_C_Y_GLU, "fert_share" )]
L122.ghgfert_tg_R_C_Y_GLU.melt <- repeat_and_add_vector( L122.ghgfert_tg_R_C_Y_GLU.melt, "Non.CO2", unique( L122.EDGAR_fert.melt$Non.CO2 ) )
L122.ghgfert_tg_R_C_Y_GLU.melt$total_emiss <- L122.EDGAR_fert.melt$value[
  match( vecpaste( L122.ghgfert_tg_R_C_Y_GLU.melt[ c( R_Y, "Non.CO2" ) ] ),
         vecpaste(L122.EDGAR_fert.melt[ c( R_Y, "Non.CO2" ) ] ) )]
L122.ghgfert_tg_R_C_Y_GLU.melt$emissions <- with( L122.ghgfert_tg_R_C_Y_GLU.melt, total_emiss * fert_share )
L122.ghgfert_tg_R_C_Y_GLU.melt$type <- "Fertilizer"

printlog( "Sum all AGR emissions by GCAM region, commodity, and GLU" )
#Drop unnecessary columns
names_GHGtable <- c( R_C_Y_GLU, "Non.CO2", "emissions", "type" )
L122.ghg_tg_R_rice_Y_GLU.melt <- L122.ghg_tg_R_rice_Y_GLU.melt[ names_GHGtable ]
L122.ghgsoil_tg_R_C_Y_GLU.melt <- L122.ghgsoil_tg_R_C_Y_GLU.melt[ names_GHGtable ]
L122.ghgfert_tg_R_C_Y_GLU.melt <- L122.ghgfert_tg_R_C_Y_GLU.melt[ names_GHGtable ]

#Bind together dataframes & aggregate
L122.ghg_tg_R_agr_C_Y_GLU.melt <- rbind( L122.ghg_tg_R_rice_Y_GLU.melt, L122.ghgsoil_tg_R_C_Y_GLU.melt, L122.ghgfert_tg_R_C_Y_GLU.melt )
L122.ghg_tg_R_agr_C_Y_GLU.melt <- na.omit( L122.ghg_tg_R_agr_C_Y_GLU.melt )
L122.ghg_tg_R_agr_C_Y_GLU.melt <- aggregate( L122.ghg_tg_R_agr_C_Y_GLU.melt[ "emissions" ],
                                             by = L122.ghg_tg_R_agr_C_Y_GLU.melt[ c( R_C_Y_GLU, "Non.CO2") ], sum )

#Reshape
L122.ghg_tg_R_agr_C_Y_GLU <- dcast( L122.ghg_tg_R_agr_C_Y_GLU.melt, GCAM_region_ID + Non.CO2 + GCAM_commodity + GLU ~ year, value.var = "emissions" )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L122.EmissShare_R_C_Y_GLU <- c( "Agriculture emissions shares by GCAM region / commodity / GLU / historical year", "unitless share" )
comments.L122.ghg_tg_R_agr_C_Y_GLU <- c( "Agriculture emissions by GCAM region / commodity / GLU / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L122.EmissShare_R_C_Y_GLU, domain="EMISSIONS_LEVEL1_DATA", fn="L122.EmissShare_R_C_Y_GLU", comments=comments.L122.EmissShare_R_C_Y_GLU )
writedata( L122.ghg_tg_R_agr_C_Y_GLU, domain="EMISSIONS_LEVEL1_DATA", fn="L122.ghg_tg_R_agr_C_Y_GLU", comments=comments.L122.ghg_tg_R_agr_C_Y_GLU )

# Every script should finish with this line
logstop()
