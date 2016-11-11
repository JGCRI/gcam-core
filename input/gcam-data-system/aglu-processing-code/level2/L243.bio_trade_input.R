# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu processing scripts, please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "L243.bio_trade_input.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model biomass trade" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_bio_supplysector <- readdata( "AGLU_ASSUMPTIONS", "A_bio_supplysector" )
A_bio_subsector_logit <- readdata( "AGLU_ASSUMPTIONS", "A_bio_subsector_logit" )
A_bio_subsector <- readdata( "AGLU_ASSUMPTIONS", "A_bio_subsector" )
L120.LC_bm2_R_LT_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_R_LT_Yh_GLU" )
L102.pcgdp_thous90USD_Scen_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_Scen_R_Y" )

# -----------------------------------------------------------------------------
# 2. Build tables
# First, delete existing regional biomass input
L243.DeleteInput_RegBio <- data.frame( region = GCAM_region_names$region )
L243.DeleteInput_RegBio$supplysector <- "regional biomass"
L243.DeleteInput_RegBio$subsector <- "regional biomass"
L243.DeleteInput_RegBio$technology <- "regional biomass"
L243.DeleteInput_RegBio <- repeat_and_add_vector( L243.DeleteInput_RegBio, "year", model_years )
L243.DeleteInput_RegBio$minicam.energy.input <- "biomass"

# Now, create new regional biomass input
L243.TechCoef_RegBio <- L243.DeleteInput_RegBio
L243.TechCoef_RegBio$minicam.energy.input <- "total biomass"
L243.TechCoef_RegBio$coefficient <- 1
L243.TechCoef_RegBio$market.name <- L243.TechCoef_RegBio$region

# Next, set up global trade. Markets now default to regional and we use logits to allow trade.
# L243.Supplysector_Bio -- supplysector for the biomass sectors
L243.SectorLogitTables <- get_logit_fn_tables( A_bio_supplysector, names_SupplysectorLogitType, base.header="Supplysector_",
    include.equiv.table=T, write.all.regions=T, has.traded=T )
L243.Supplysector_Bio <- write_to_all_regions( A_bio_supplysector, names_Supplysector, has.traded=T )

# The traded markets tend to be a good candidate to solve explicitly since they tie together
# many solved markets.
L243.SectorUseTrialMarket_Bio <- write_to_all_regions( subset( A_bio_supplysector, traded == T ), c( reg, supp ), has.traded=T )
L243.SectorUseTrialMarket_Bio$use.trial.market <- 1

# L243.SubsectorLogit_Bio -- Subsector logit definitions
L243.SubsectorLogitTables <- get_logit_fn_tables( A_bio_subsector_logit, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=T, has.traded=T )
L243.SubsectorLogit_Bio <- write_to_all_regions( A_bio_subsector_logit, names_SubsectorLogit, has.traded=T )

# L243.GlobalTechCoef_TotBio -- coefficients for the regional biomass technologies
L243.GlobalTechCoef_TotBio <- data.frame( subsector.name = c( "domestic biomass", "imported biomass" ),
                                           technology = c( "domestic biomass", "imported biomass" ),
                                           minicam.energy.input = c( "biomass", "traded biomass" ) )
L243.GlobalTechCoef_TotBio$sector.name <- "total biomass"
L243.GlobalTechCoef_TotBio$coefficient <- 1
L243.GlobalTechCoef_TotBio <- repeat_and_add_vector( L243.GlobalTechCoef_TotBio, "year", model_years )
L243.GlobalTechCoef_TotBio <- L243.GlobalTechCoef_TotBio[ names_GlobalTechCoef ]

# L243.GlobalTechShrwt_TotBio -- share weights for the regional biomass technologies
L243.GlobalTechShrwt_TotBio <- L243.GlobalTechCoef_TotBio[ names_GlobalTechYr ]
L243.GlobalTechShrwt_TotBio$share.weight <- A_bio_subsector$share.weight[ match( L243.GlobalTechShrwt_TotBio$subsector ,
                                                                                A_bio_subsector$subsector )]

# L243.StubTech_TotBio -- stub technologies for the regional biomass technologies
L243.StubTech_TotBio <- data.frame( subsector = c( "domestic biomass", "imported biomass" ),
                                    stub.technology = c( "domestic biomass", "imported biomass" ) )
L243.StubTech_TotBio$supplysector <- "total biomass"
L243.StubTech_TotBio <- repeat_and_add_vector( L243.StubTech_TotBio, "region", GCAM_region_names$region )
L243.StubTech_TotBio <- L243.StubTech_TotBio[ names_StubTech ]

# L243.StubTechShrwt_TotBio -- share weights for the regional biomass technologies
L243.StubTechShrwt_TotBio <- L243.StubTech_TotBio
L243.StubTechShrwt_TotBio <- repeat_and_add_vector( L243.StubTechShrwt_TotBio, "year", model_years )
L243.StubTechShrwt_TotBio$share.weight <- 1

# L243.StubTechCoef_ImportedBio -- stub technologies for the regional biomass technologies
L243.StubTechCoef_ImportedBio <- subset( L243.StubTech_TotBio, subsector == "imported biomass" )
L243.StubTechCoef_ImportedBio <- repeat_and_add_vector( L243.StubTechCoef_ImportedBio, "year", model_years )
L243.StubTechCoef_ImportedBio$minicam.energy.input <- "traded biomass"
L243.StubTechCoef_ImportedBio$coefficient <- 1
L243.StubTechCoef_ImportedBio$market <- GCAM_region_names$region[1]

# L243.StubTechCoef_DomesticBio -- stub technologies for the regional biomass technologies
L243.StubTechCoef_DomesticBio <- subset( L243.StubTech_TotBio, subsector == "domestic biomass" )
L243.StubTechCoef_DomesticBio <- repeat_and_add_vector( L243.StubTechCoef_DomesticBio, "year", model_years )
L243.StubTechCoef_DomesticBio$minicam.energy.input <- "biomass"
L243.StubTechCoef_DomesticBio$coefficient <- 1
L243.StubTechCoef_DomesticBio$market <- L243.StubTechCoef_DomesticBio$region

# L243.StubTechShrwt_TotBio -- share weights for the regional biomass subsectors
L243.SubsectorShrwtFllt_TotBio <- L243.StubTech_TotBio[ names_Subsector ]
L243.SubsectorShrwtFllt_TotBio$year.fillout <- min( model_years )
L243.SubsectorShrwtFllt_TotBio$share.weight <- A_bio_subsector$share.weight[ match( L243.SubsectorShrwtFllt_TotBio$subsector,
                                                                                   A_bio_subsector$subsector )]

# L243.TechCoef_TradedBio -- input name, market, coeff for traded biomass
L243.TechCoef_TradedBio <- data.frame( region = GCAM_region_names$region[ GCAM_region_names[[R]] == 1 ],
                                       supplysector = "traded biomass" )
L243.TechCoef_TradedBio$minicam.energy.input <- "biomass"
L243.TechCoef_TradedBio$coefficient <- 1
L243.TechCoef_TradedBio <- repeat_and_add_vector( L243.TechCoef_TradedBio, "market.name", GCAM_region_names$region )
L243.TechCoef_TradedBio <- repeat_and_add_vector( L243.TechCoef_TradedBio, "year", model_years )
L243.TechCoef_TradedBio$subsector <- paste( L243.TechCoef_TradedBio$market.name, "traded biomass", sep=" " )
L243.TechCoef_TradedBio$technology <- L243.TechCoef_TradedBio$subsector
L243.TechCoef_TradedBio <- L243.TechCoef_TradedBio[ names_TechCoef ]

# Compute share weights based on cropland area. Largest region gets shareweight of 1
L243.TechShrwt <- subset( L120.LC_bm2_R_LT_Yh_GLU, Land_Type == "Cropland" )
L243.TechShrwt <- aggregate( L243.TechShrwt[[ X_final_historical_year]], by = L243.TechShrwt[ R], sum )
names( L243.TechShrwt )[ names( L243.TechShrwt ) == "x" ] <- "Cropland"
MAX_CROP <- max( L243.TechShrwt$Cropland )
L243.TechShrwt$Max_Cropland <- MAX_CROP
L243.TechShrwt$Shrwt <- round( L243.TechShrwt$Cropland / L243.TechShrwt$Max_Cropland, digits_land_use )
L243.TechShrwt <- add_region_name( L243.TechShrwt )
L243.TechShrwt$subsector <- paste( L243.TechShrwt$region, "traded biomass", sep=" ")

# L243.TechShrwt_TradedBio -- share weight for traded biomass technologies
# setting to 1 since the competition happens in the subsector
L243.TechShrwt_TradedBio <- L243.TechCoef_TradedBio[ names_TechYr ]
L243.TechShrwt_TradedBio$share.weight <- 1

# L243.SubsectorShrwtFllt_TradedBio -- share weight for traded biomass subsectors
L243.SubsectorShrwtFllt_TradedBio <- unique( L243.TechCoef_TradedBio[ names_Subsector ] )
L243.SubsectorShrwtFllt_TradedBio$year.fillout <- min( model_years )
L243.SubsectorShrwtFllt_TradedBio$share.weight <- L243.TechShrwt$Shrwt[ match( L243.SubsectorShrwtFllt_TradedBio$subsector , 
                                                                   L243.TechShrwt$subsector )]

# Re-set NAs to zero. NA's come from no-aglu regions that aren't written to land cover table
L243.SubsectorShrwtFllt_TradedBio$share.weight[ is.na( L243.SubsectorShrwtFllt_TradedBio$share.weight ) ] <- 0

# First, determine which regions are in which groupings.
L243.pcgdp_2010 <- subset( L102.pcgdp_thous90USD_Scen_R_Y, L102.pcgdp_thous90USD_Scen_R_Y$scenario == "SSP4" )
L243.pcgdp_2010 <- L243.pcgdp_2010[ names( L243.pcgdp_2010) %in% c( "GCAM_region_ID", "X2010" ) ]
L243.pcgdp_2010 <- add_region_name( L243.pcgdp_2010 )
L243.pcgdp_2010$X2010 <- L243.pcgdp_2010$X2010 * conv_1990_2010_USD
L243.low_reg <- L243.pcgdp_2010$region[ L243.pcgdp_2010$X2010 < lo_growth_pcgdp ]

# Next, we want to limit imports & exports of biomass into low income regions for SSP4. Do this with share-weights
L243.SubsectorShrwtFllt_TotBio_SSP4_lo <- subset( L243.SubsectorShrwtFllt_TotBio, region %in% L243.low_reg & subsector == "imported biomass" )
L243.SubsectorShrwtFllt_TotBio_SSP4_lo$year.fillout <- 2025
L243.SubsectorShrwtFllt_TotBio_SSP4_lo$share.weight <- 0.1

L243.SubsectorShrwtFllt_TotBio_SSP4_hi <- subset( L243.SubsectorShrwtFllt_TotBio, region %!in% L243.low_reg & subsector == "imported biomass" )
L243.SubsectorShrwtFllt_TotBio_SSP4_hi$year.fillout <- 2025
L243.SubsectorShrwtFllt_TotBio_SSP4_hi$share.weight <- 0.5
L243.SubsectorShrwtFllt_TotBio_SSP4 <- rbind( L243.SubsectorShrwtFllt_TotBio_SSP4_lo, L243.SubsectorShrwtFllt_TotBio_SSP4_hi )

L243.low_reg_tech <- paste( L243.low_reg, "traded biomass", sep=" " )
L243.SubsectorShrwtFllt_TradedBio_SSP4 <- subset( L243.SubsectorShrwtFllt_TradedBio, subsector %in% L243.low_reg_tech )
L243.SubsectorShrwtFllt_TradedBio_SSP4$year.fillout <- 2025
L243.SubsectorShrwtFllt_TradedBio_SSP4$share.weight <- 0.1

L243.TechShrwt_TradedBio_SSP4 <- subset( L243.TechShrwt_TradedBio, technology %in% L243.low_reg_tech & year > 2020 )
L243.TechShrwt_TradedBio_SSP4$share.weight <- 0.1

L243.StubTechShrwt_TotBio_SSP4_lo <- subset( L243.StubTechShrwt_TotBio, region %in% L243.low_reg 
                                                                     & subsector == "imported biomass" 
                                                                     & year > 2020 )
L243.StubTechShrwt_TotBio_SSP4_lo$share.weight <- 0.1

L243.StubTechShrwt_TotBio_SSP4_hi <- subset( L243.StubTechShrwt_TotBio, region %!in% L243.low_reg 
                                          & subsector == "imported biomass" 
                                          & year > 2020 )
L243.StubTechShrwt_TotBio_SSP4_hi$share.weight <- 0.25
L243.StubTechShrwt_TotBio_SSP4 <- rbind( L243.StubTechShrwt_TotBio_SSP4_lo, L243.StubTechShrwt_TotBio_SSP4_hi )

# For SSP3, we want to limit trade of biomass in all regions
L243.SubsectorShrwtFllt_TotBio_SSP3 <- subset( L243.SubsectorShrwtFllt_TotBio, subsector == "imported biomass" )
L243.SubsectorShrwtFllt_TotBio_SSP3$year.fillout <- 2025
L243.SubsectorShrwtFllt_TotBio_SSP3$share.weight <- 0.1

L243.StubTechShrwt_TotBio_SSP3 <- subset( L243.StubTechShrwt_TotBio, subsector == "imported biomass" 
                                          & year > 2020 )
L243.StubTechShrwt_TotBio_SSP3$share.weight <- 0.1

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L243.DeleteInput_RegBio, "DeleteInput", "AGLU_LEVEL2_DATA", "L243.DeleteInput_RegBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
for( curr_table in names ( L243.SectorLogitTables) ) {
  write_mi_data( L243.SectorLogitTables[[ curr_table ]]$data, L243.SectorLogitTables[[ curr_table ]]$header,
                 "AGLU_LEVEL2_DATA", paste0("L243.", L243.SectorLogitTables[[ curr_table ]]$header ), "AGLU_XML_BATCH",
                 "batch_bio_trade.xml" )
}
write_mi_data( L243.TechCoef_RegBio, "TechCoef", "AGLU_LEVEL2_DATA", "L243.TechCoef_RegBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.Supplysector_Bio, "Supplysector", "AGLU_LEVEL2_DATA", "L243.Supplysector_Bio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.SectorUseTrialMarket_Bio, "SectorUseTrialMarket", "AGLU_LEVEL2_DATA", "L243.SectorUseTrialMarket_Bio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
for( curr_table in names ( L243.SubsectorLogitTables ) ) {
  write_mi_data( L243.SubsectorLogitTables[[ curr_table ]]$data, L243.SubsectorLogitTables[[ curr_table ]]$header,
                 "AGLU_LEVEL2_DATA", paste0("L243.", L243.SubsectorLogitTables[[ curr_table ]]$header ), "AGLU_XML_BATCH",
                 "batch_bio_trade.xml" )
}

write_mi_data( L243.SubsectorLogit_Bio, "SubsectorLogit", "AGLU_LEVEL2_DATA", "L243.SubsectorLogit_Bio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.SubsectorShrwtFllt_TotBio, "SubsectorShrwtFllt", "AGLU_LEVEL2_DATA", "L243.SubsectorShrwtFllt_TotBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.SubsectorShrwtFllt_TradedBio, "SubsectorShrwtFllt", "AGLU_LEVEL2_DATA", "L243.SubsectorShrwtFllt_TradedBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.GlobalTechCoef_TotBio, "GlobalTechCoef", "AGLU_LEVEL2_DATA", "L243.GlobalTechCoef_TotBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.GlobalTechShrwt_TotBio, "GlobalTechShrwt", "AGLU_LEVEL2_DATA", "L243.GlobalTechShrwt_TotBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.StubTech_TotBio, "StubTech", "AGLU_LEVEL2_DATA", "L243.StubTech_TotBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.StubTechShrwt_TotBio, "StubTechShrwt", "AGLU_LEVEL2_DATA", "L243.StubTechShrwt_TotBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.StubTechCoef_ImportedBio, "StubTechCoef", "AGLU_LEVEL2_DATA", "L243.StubTechCoef_ImportedBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.StubTechCoef_DomesticBio, "StubTechCoef", "AGLU_LEVEL2_DATA", "L243.StubTechCoef_DomesticBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.TechCoef_TradedBio, "TechCoef", "AGLU_LEVEL2_DATA", "L243.TechCoef_TradedBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.TechShrwt_TradedBio, "TechShrwt", "AGLU_LEVEL2_DATA", "L243.TechShrwt_TradedBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_bio_trade.xml", "AGLU_XML_FINAL", "bio_trade.xml", "", xml_tag="outFile" )

write_mi_data( L243.SubsectorShrwtFllt_TotBio_SSP4, "SubsectorShrwtFllt", "AGLU_LEVEL2_DATA", "L243.SubsectorShrwtFllt_TotBio_SSP4", "AGLU_XML_BATCH", "batch_ssp4_bio_trade.xml" )
write_mi_data( L243.SubsectorShrwtFllt_TradedBio_SSP4, "SubsectorShrwtFllt", "AGLU_LEVEL2_DATA", "L243.SubsectorShrwtFllt_TradedBio_SSP4", "AGLU_XML_BATCH", "batch_ssp4_bio_trade.xml" )
write_mi_data( L243.TechShrwt_TradedBio_SSP4, "TechShrwt", "AGLU_LEVEL2_DATA", "L243.TechShrwt_TradedBio_SSP4", "AGLU_XML_BATCH", "batch_ssp4_bio_trade.xml" )
write_mi_data( L243.StubTechShrwt_TotBio_SSP4, "StubTechShrwt", "AGLU_LEVEL2_DATA", "L243.StubTechShrwt_TotBio_SSP4", "AGLU_XML_BATCH", "batch_ssp4_bio_trade.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ssp4_bio_trade.xml", "AGLU_XML_FINAL", "ssp4_bio_trade.xml", "", xml_tag="outFile" )

write_mi_data( L243.SubsectorShrwtFllt_TotBio_SSP3, "SubsectorShrwtFllt", "AGLU_LEVEL2_DATA", "L243.SubsectorShrwtFllt_TotBio_SSP3", "AGLU_XML_BATCH", "batch_ssp3_bio_trade.xml" )
write_mi_data( L243.StubTechShrwt_TotBio_SSP3, "StubTechShrwt", "AGLU_LEVEL2_DATA", "L243.StubTechShrwt_TotBio_SSP3", "AGLU_XML_BATCH", "batch_ssp3_bio_trade.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ssp3_bio_trade.xml", "AGLU_XML_FINAL", "ssp3_bio_trade.xml", "", xml_tag="outFile" )

logstop()
