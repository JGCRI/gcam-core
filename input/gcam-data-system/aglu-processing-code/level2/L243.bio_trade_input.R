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
A_biotrade_supplysector <- readdata( "AGLU_ASSUMPTIONS", "A_biotrade_supplysector" )
A_bio_subsector <- readdata( "AGLU_ASSUMPTIONS", "A_bio_subsector" )
L120.LC_bm2_R_LT_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_R_LT_Yh_AEZ" )
L102.pcgdp_thous90USD_SSP_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_SSP_R_Y" )

# -----------------------------------------------------------------------------
# 2. Build tables
# First, delete existing regional biomass input
L243.Delete_RegBioInput <- data.frame( region = GCAM_region_names$region )
L243.Delete_RegBioInput$supplysector <- "regional biomass"
L243.Delete_RegBioInput$subsector <- "regional biomass"
L243.Delete_RegBioInput$technology <- "regional biomass"
L243.Delete_RegBioInput <- repeat_and_add_vector( L243.Delete_RegBioInput, "year", model_years )
L243.Delete_RegBioInput$minicam.energy.input <- "biomass"

# Now, create new regional biomass input
L243.RegBioInput <- L243.Delete_RegBioInput
L243.RegBioInput$minicam.energy.input <- "total biomass"
L243.RegBioInput$coefficient <- 1
L243.RegBioInput$market.name <- L243.RegBioInput$region

# Next, set up global trade. Markets now default to regional and we use logits to allow trade.
# L243.Supplysector_TotBio -- supplysector for the regional biomass
L243.Supplysector_TotBio <- A_bio_supplysector
L243.Supplysector_TotBio$logit.year.fillout <- min( model_years )
L243.Supplysector_TotBio <- repeat_and_add_vector( L243.Supplysector_TotBio, "region", GCAM_region_names$region )
L243.Supplysector_TotBio <- L243.Supplysector_TotBio[ names_Supplysector ]

# L243.GlobalTechCoef_TotBio -- coefficients for the regional biomass technologies
L243.GlobalTechCoeff_TotBio <- data.frame( subsector.name = c( "domestic biomass", "imported biomass" ),
                                           technology = c( "domestic biomass", "imported biomass" ),
                                           minicam.energy.input = c( "biomass", "traded biomass" ) )
L243.GlobalTechCoeff_TotBio$sector.name <- "total biomass"
L243.GlobalTechCoeff_TotBio$coefficient <- 1
L243.GlobalTechCoeff_TotBio <- repeat_and_add_vector( L243.GlobalTechCoeff_TotBio, "year", model_years )
L243.GlobalTechCoeff_TotBio <- L243.GlobalTechCoeff_TotBio[ names_GlobalTechCoef ]

# L243.GlobalTechShWt_TotBio -- share weights for the regional biomass technologies
L243.GlobalTechShWt_TotBio <- L243.GlobalTechCoeff_TotBio[ names_GlobalTechYr ]
L243.GlobalTechShWt_TotBio$share.weight <- A_bio_subsector$share.weight[ match( L243.GlobalTechShWt_TotBio$subsector ,
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
L243.StubTechCoef_ImportedBio$market <- A_biotrade_supplysector$region

# L243.StubTechCoef_DomesticBio -- stub technologies for the regional biomass technologies
L243.StubTechCoef_DomesticBio <- subset( L243.StubTech_TotBio, subsector == "domestic biomass" )
L243.StubTechCoef_DomesticBio <- repeat_and_add_vector( L243.StubTechCoef_DomesticBio, "year", model_years )
L243.StubTechCoef_DomesticBio$minicam.energy.input <- "biomass"
L243.StubTechCoef_DomesticBio$coefficient <- 1
L243.StubTechCoef_DomesticBio$market <- L243.StubTechCoef_DomesticBio$region

# L243.StubTechLogit_TotBio -- logit exponents for the regional biomass subsectors
L243.SubsLogit_TotBio <- L243.StubTech_TotBio[ names_Subsector ]
L243.SubsLogit_TotBio$logit.year.fillout <- min( model_years )
L243.SubsLogit_TotBio$logit.exponent <- A_bio_subsector$logit.exponent[ match( L243.SubsLogit_TotBio$subsector,
                                                                                   A_bio_subsector$subsector )]

# L243.StubTechShWt_TotBio -- share weights for the regional biomass subsectors
L243.SubsShWt_TotBio <- L243.StubTech_TotBio[ names_Subsector ]
L243.SubsShWt_TotBio$year.fillout <- min( model_years )
L243.SubsShWt_TotBio$share.weight <- A_bio_subsector$share.weight[ match( L243.SubsShWt_TotBio$subsector,
                                                                                   A_bio_subsector$subsector )]

# L243.Supplysector_TradedBio -- supplysector for traded biomass
A_bio_supplysector_reg <- repeat_and_add_vector( A_bio_supplysector, "region", GCAM_region_names$region )
ALL_BIO_SUPPLY_LOGIT <- rbind( A_bio_supplysector_reg, A_biotrade_supplysector )
L243.SectorLogitTables <- get_logit_fn_tables( ALL_BIO_SUPPLY_LOGIT, names_SupplysectorLogitType,
                                               base.header="Supplysector_", include.equiv.table=T, write.all.regions=F )
L243.Supplysector_TradedBio <- A_biotrade_supplysector
L243.Supplysector_TradedBio$logit.year.fillout <- min( model_years )
L243.Supplysector_TradedBio <- L243.Supplysector_TradedBio[ names_Supplysector ]

# L243.TechCoef_TradedBio -- input name, market, coeff for traded biomass
L243.TechCoef_TradedBio <- L243.Supplysector_TradedBio[ c( "region", "supplysector" )]
L243.TechCoef_TradedBio$minicam.energy.input <- "biomass"
L243.TechCoef_TradedBio$coefficient <- 1
L243.TechCoef_TradedBio <- repeat_and_add_vector( L243.TechCoef_TradedBio, "market.name", GCAM_region_names$region )
L243.TechCoef_TradedBio <- repeat_and_add_vector( L243.TechCoef_TradedBio, "year", model_years )
L243.TechCoef_TradedBio$subsector <- paste( L243.TechCoef_TradedBio$market.name, "traded biomass", sep=" " )
L243.TechCoef_TradedBio$technology <- L243.TechCoef_TradedBio$subsector
L243.TechCoef_TradedBio <- L243.TechCoef_TradedBio[ names_TechCoef ]

# Compute share weights based on cropland area. Largest region gets shareweight of 1
L243.TechShWt <- subset( L120.LC_bm2_R_LT_Yh_AEZ, Land_Type == "Cropland" )
L243.TechShWt <- aggregate( L243.TechShWt$X2010, by=as.list( L243.TechShWt[ c( "GCAM_region_ID" )]), sum )
names( L243.TechShWt )[ names( L243.TechShWt ) == "x" ] <- "Cropland"
MAX_CROP <- max( L243.TechShWt$Cropland )
L243.TechShWt$Max_Cropland <- MAX_CROP
L243.TechShWt$ShWt <- L243.TechShWt$Cropland / L243.TechShWt$Max_Cropland
L243.TechShWt$region <- GCAM_region_names$region[ match( L243.TechShWt$GCAM_region_ID, GCAM_region_names$GCAM_region_ID )]
L243.TechShWt$subsector <- paste( L243.TechShWt$region, "traded biomass", sep=" ")

# L243.TechShWt_TradedBio -- share weight for traded biomass technologies
L243.TechShWt_TradedBio <- L243.TechCoef_TradedBio[ names_TechYr ]
L243.TechShWt_TradedBio$share.weight <- L243.TechShWt$ShWt[ match( L243.TechShWt_TradedBio$subsector , 
                                                                   L243.TechShWt$subsector )]

# L243.SubsShWt_TradedBio -- share weight for traded biomass subsectors
L243.SubsShWt_TradedBio <- L243.TechCoef_TradedBio[ names_Subsector ]
L243.SubsShWt_TradedBio$year.fillout <- min( model_years )
L243.SubsShWt_TradedBio$share.weight <- L243.TechShWt$ShWt[ match( L243.SubsShWt_TradedBio$subsector , 
                                                                   L243.TechShWt$subsector )]

# L243.SubsLogit_TradedBio -- logit for traded biomass subsectors
L243.SubsLogit_TradedBio <- L243.SubsShWt_TradedBio
L243.SubsLogit_TradedBio$logit.exponent <- -3
L243.SubsLogit_TradedBio$share.weight <- NULL

# Set subsector logit types
A_bio_subsector_reg <- repeat_and_add_vector( A_bio_subsector, "region", GCAM_region_names$region )
A_bio_subsector_reg <- A_bio_subsector_reg[ names( A_bio_subsector_reg )  %in% c( "region", "supplysector", "subsector" ) ]
A_biotrade_subsector <- L243.SubsShWt_TradedBio[ names( L243.SubsShWt_TradedBio ) %in% c( "region", "supplysector", "subsector" )]
ALL_BIO_SUB_LOGIT <- rbind( A_bio_subsector_reg, A_biotrade_subsector )
ALL_BIO_SUB_LOGIT$logit.type <- NA
L243.SubsectorLogitTablesTotal <- get_logit_fn_tables( ALL_BIO_SUB_LOGIT, names_SubsectorLogitType,
                                                       base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )

# First, determine which regions are in which groupings.
L243.pcgdp_2010 <- subset( L102.pcgdp_thous90USD_SSP_R_Y, L102.pcgdp_thous90USD_SSP_R_Y$scenario == "SSP4" )
L243.pcgdp_2010 <- L243.pcgdp_2010[ names( L243.pcgdp_2010) %in% c( "GCAM_region_ID", "X2010" ) ]
L243.pcgdp_2010 <- add_region_name( L243.pcgdp_2010 )
L243.pcgdp_2010$X2010 <- L243.pcgdp_2010$X2010 * conv_1990_2010_USD
L243.low_reg <- L243.pcgdp_2010$region[ L243.pcgdp_2010$X2010 < lo_growth_pcgdp ]

# Next, we want to limit imports & exports of biomass into low income regions for SSP4. Do this with share-weights
L243.SSP4_SubsShWt_TotBio <- subset( L243.SubsShWt_TotBio, region %in% L243.low_reg & subsector == "imported biomass" )
L243.SSP4_SubsShWt_TotBio$year.fillout <- 2025
L243.SSP4_SubsShWt_TotBio$share.weight <- 0.1

L243.SSP4_SubsShWt_TotBio2 <- subset( L243.SubsShWt_TotBio, region %!in% L243.low_reg & subsector == "imported biomass" )
L243.SSP4_SubsShWt_TotBio2$year.fillout <- 2025
L243.SSP4_SubsShWt_TotBio2$share.weight <- 0.5

L243.low_reg_tech <- paste( L243.low_reg, "traded biomass", sep=" " )
L243.SSP4_SubsShWt_TradedBio <- subset( L243.SubsShWt_TradedBio, subsector %in% L243.low_reg_tech )
L243.SSP4_SubsShWt_TradedBio$year.fillout <- 2025
L243.SSP4_SubsShWt_TradedBio$share.weight <- 0.1

L243.SSP4_TechShWt_TradedBio <- subset( L243.TechShWt_TradedBio, technology %in% L243.low_reg_tech & year > 2020 )
L243.SSP4_TechShWt_TradedBio$share.weight <- 0.1

L243.SSP4_StubTechShrwt_TotBio <- subset( L243.StubTechShrwt_TotBio, region %in% L243.low_reg 
                                                                     & subsector == "imported biomass" 
                                                                     & year > 2020 )
L243.SSP4_StubTechShrwt_TotBio$share.weight <- 0.1

L243.SSP4_StubTechShrwt_TotBio2 <- subset( L243.StubTechShrwt_TotBio, region %!in% L243.low_reg 
                                          & subsector == "imported biomass" 
                                          & year > 2020 )
L243.SSP4_StubTechShrwt_TotBio2$share.weight <- 0.25

# For SSP3, we want to limit trade of biomass in all regions
L243.SSP3_SubsShWt_TotBio <- subset( L243.SubsShWt_TotBio, subsector == "imported biomass" )
L243.SSP3_SubsShWt_TotBio$year.fillout <- 2025
L243.SSP3_SubsShWt_TotBio$share.weight <- 0.1

L243.SSP3_StubTechShrwt_TotBio <- subset( L243.StubTechShrwt_TotBio, subsector == "imported biomass" 
                                          & year > 2020 )
L243.SSP3_StubTechShrwt_TotBio$share.weight <- 0.1

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L243.Delete_RegBioInput, "DeleteInput", "AGLU_LEVEL2_DATA", "L243.Delete_RegBioInput", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
for( curr_table in names ( L243.SectorLogitTables) ) {
  write_mi_data( L243.SectorLogitTables[[ curr_table ]]$data, L243.SectorLogitTables[[ curr_table ]]$header,
                 "AGLU_LEVEL2_DATA", paste0("L243.", L243.SectorLogitTables[[ curr_table ]]$header ), "AGLU_XML_BATCH",
                 "batch_bio_trade.xml" )
}
write_mi_data( L243.RegBioInput, "TechCoef", "AGLU_LEVEL2_DATA", "L243.RegBioInput", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.Supplysector_TotBio, "Supplysector", "AGLU_LEVEL2_DATA", "L243.Supplysector_TotBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.Supplysector_TradedBio, "Supplysector", "AGLU_LEVEL2_DATA", "L243.Supplysector_TradedBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
for( curr_table in names ( L243.SubsectorLogitTablesTotal ) ) {
  write_mi_data( L243.SubsectorLogitTablesTotal[[ curr_table ]]$data, L243.SubsectorLogitTablesTotal[[ curr_table ]]$header,
                 "AGLU_LEVEL2_DATA", paste0("L243.", L243.SubsectorLogitTablesTotal[[ curr_table ]]$header ), "AGLU_XML_BATCH",
                 "batch_bio_trade.xml" )
}

write_mi_data( L243.SubsLogit_TotBio, "SubsectorLogit", "AGLU_LEVEL2_DATA", "L243.SubsLogit_TotBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.SubsShWt_TotBio, "SubsectorShrwtFllt", "AGLU_LEVEL2_DATA", "L243.SubsShWt_TotBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.SubsLogit_TradedBio, "SubsectorLogit", "AGLU_LEVEL2_DATA", "L243.SubsLogit_TradedBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.SubsShWt_TradedBio, "SubsectorShrwtFllt", "AGLU_LEVEL2_DATA", "L243.SubsShWt_TradedBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.GlobalTechCoeff_TotBio, "GlobalTechCoef", "AGLU_LEVEL2_DATA", "L243.GlobalTechCoeff_TotBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.GlobalTechShWt_TotBio, "GlobalTechShrwt", "AGLU_LEVEL2_DATA", "L243.GlobalTechShWt_TotBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.StubTech_TotBio, "StubTech", "AGLU_LEVEL2_DATA", "L243.StubTech_TotBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.StubTechShrwt_TotBio, "StubTechShrwt", "AGLU_LEVEL2_DATA", "L243.StubTechShrwt_TotBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.StubTechCoef_ImportedBio, "StubTechCoef", "AGLU_LEVEL2_DATA", "L243.StubTechCoef_ImportedBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.StubTechCoef_DomesticBio, "StubTechCoef", "AGLU_LEVEL2_DATA", "L243.StubTechCoef_DomesticBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.TechCoef_TradedBio, "TechCoef", "AGLU_LEVEL2_DATA", "L243.TechCoef_TradedBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )
write_mi_data( L243.TechShWt_TradedBio, "TechShrwt", "AGLU_LEVEL2_DATA", "L243.TechShWt_TradedBio", "AGLU_XML_BATCH", "batch_bio_trade.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_bio_trade.xml", "AGLU_XML_FINAL", "bio_trade.xml", "", xml_tag="outFile" )

write_mi_data( L243.SSP4_SubsShWt_TotBio, "SubsectorShrwtFllt", "AGLU_LEVEL2_DATA", "L243.SSP4_SubsShWt_TotBio", "AGLU_XML_BATCH", "batch_ssp4_bio_trade.xml" )
write_mi_data( L243.SSP4_SubsShWt_TradedBio, "SubsectorShrwtFllt", "AGLU_LEVEL2_DATA", "L243.SSP4_SubsShWt_TradedBio", "AGLU_XML_BATCH", "batch_ssp4_bio_trade.xml" )
write_mi_data( L243.SSP4_TechShWt_TradedBio, "TechShrwt", "AGLU_LEVEL2_DATA", "L243.SSP4_TechShWt_TradedBio", "AGLU_XML_BATCH", "batch_ssp4_bio_trade.xml" )
write_mi_data( L243.SSP4_StubTechShrwt_TotBio, "StubTechShrwt", "AGLU_LEVEL2_DATA", "L243.SSP4_StubTechShrwt_TotBio", "AGLU_XML_BATCH", "batch_ssp4_bio_trade.xml" )
write_mi_data( L243.SSP4_SubsShWt_TotBio2, "SubsectorShrwtFllt", "AGLU_LEVEL2_DATA", "L243.SSP4_SubsShWt_TotBio2", "AGLU_XML_BATCH", "batch_ssp4_bio_trade.xml" )
write_mi_data( L243.SSP4_StubTechShrwt_TotBio2, "StubTechShrwt", "AGLU_LEVEL2_DATA", "L243.SSP4_StubTechShrwt_TotBio2", "AGLU_XML_BATCH", "batch_ssp4_bio_trade.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ssp4_bio_trade.xml", "AGLU_XML_FINAL", "ssp4_bio_trade.xml", "", xml_tag="outFile" )

write_mi_data( L243.SSP3_SubsShWt_TotBio, "SubsectorShrwtFllt", "AGLU_LEVEL2_DATA", "L243.SSP3_SubsShWt_TotBio", "AGLU_XML_BATCH", "batch_ssp3_bio_trade.xml" )
write_mi_data( L243.SSP3_StubTechShrwt_TotBio, "StubTechShrwt", "AGLU_LEVEL2_DATA", "L243.SSP3_StubTechShrwt_TotBio", "AGLU_XML_BATCH", "batch_ssp3_bio_trade.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ssp3_bio_trade.xml", "AGLU_XML_FINAL", "ssp3_bio_trade.xml", "", xml_tag="outFile" )

logstop()
