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
logstart( "L224.resbio_input_IRR.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for residue biomass production from agriculture / forestry / milling" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_AgSupplySector <- readdata( "AGLU_ASSUMPTIONS", "A_agSupplySector" )
A_biocrops_R_AEZ_irr <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ_irr" )
A_demand_technology <- readdata( "AGLU_ASSUMPTIONS", "A_demand_technology" )
A_resbio_curves <- readdata( "AGLU_ASSUMPTIONS", "A_resbio_curves" )
A_bio_frac_prod_R <- readdata( "AGLU_ASSUMPTIONS", "A_bio_frac_prod_R" )
L111.ag_resbio_R_C <- readdata( "AGLU_LEVEL1_DATA", "L111.ag_resbio_R_C" )
L123.For_Prod_bm3_R_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L123.For_Prod_bm3_R_Y_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "Building base tables of combinations of regions / sectors / technologies" )
#Add region names to bio frac table
A_bio_frac_prod_R <- add_region_name( A_bio_frac_prod_R )

#FORESTRY
#Melt forest production table to get Forest name by region and AEZ
L224.For_Prod_bm3_R_Y_AEZ.melt <- melt( L123.For_Prod_bm3_R_Y_AEZ, id.vars = R_C_AEZ, variable.name = "year" )
L224.For_Prod_bm3_R_Y_AEZ.melt$year <- sub( "X", "", L224.For_Prod_bm3_R_Y_AEZ.melt$year )
L224.For_Prod_bm3_R_Y_AEZ.melt<- add_region_name( L224.For_Prod_bm3_R_Y_AEZ.melt )
L224.For_Prod_bm3_R_AEZ.melt <- subset( L224.For_Prod_bm3_R_Y_AEZ.melt, year == max( year ) )
L224.R_For_tech <- data.frame(
      region = L224.For_Prod_bm3_R_AEZ.melt[[reg]],
      AgSupplySector = L224.For_Prod_bm3_R_AEZ.melt[[C]],
      AgSupplySubsector = paste( L224.For_Prod_bm3_R_AEZ.melt[[C]], L224.For_Prod_bm3_R_AEZ.melt[[AEZ]], sep = AEZ_delimiter ),
      AgProductionTechnology = paste( L224.For_Prod_bm3_R_AEZ.melt[[C]], L224.For_Prod_bm3_R_AEZ.melt[[AEZ]], sep = AEZ_delimiter ) )

printlog( "L224.AgResBio_For: Forest residue biomass parameters" )
L224.AgResBio_For <- repeat_and_add_vector( L224.R_For_tech, Y, c( model_base_years, model_future_years ) )
L224.AgResBio_For$residue.biomass.production <- "biomass"
L224.AgResBio_For$mass.conversion <- AvgWoodDensity_kgm3
L224.AgResBio_For$harvest.index <- ForestHarvestIndex
L224.AgResBio_For$eros.ctrl <- ForestErosCtrl_kgm2
L224.AgResBio_For$mass.to.energy <- WoodEnergyContent_GJkg
L224.AgResBio_For$water.content <- WoodWaterContent

printlog( "L224.AgResBioCurve_For: Forest residue biomass supply curves" )
L224.R_For_tech_year <- repeat_and_add_vector( L224.R_For_tech, "year", c( model_base_years, model_future_years ) )
L224.R_For_tech_year$residue.biomass.production <- "biomass"
L224.AgResBioCurve_For <- repeat_and_add_vector( L224.R_For_tech_year, "price", unique (A_resbio_curves$price) )
L224.AgResBioCurve_For$fract.harvested <- A_resbio_curves$For[ match( L224.AgResBioCurve_For$price, A_resbio_curves$price ) ]

#In base years, replace the "fraction produced" at specified prices in order to calibrate resbio production
L224.AgResBioCurve_For$fract.harvested[
      L224.AgResBioCurve_For$price == Price_bio_frac & L224.AgResBioCurve_For$year %in% model_base_years ] <-
      A_bio_frac_prod_R$For[ match( L224.AgResBioCurve_For$region[
           L224.AgResBioCurve_For$price == Price_bio_frac & L224.AgResBioCurve_For$year %in% model_base_years ],
      A_bio_frac_prod_R$region ) ]

#MILL -- NOTE THAT THIS IS A TECHNOLOGY IN THE GLOBAL TECH DATABASE
L224.Mill_tech <- A_demand_technology[ A_demand_technology$supplysector == "NonFoodDemand_Forest", c( supp, subs, tech ) ]
L224.Mill_tech[ c( "sector.name", "subsector.name" ) ] <- L224.Mill_tech[ c( "supplysector", "subsector" ) ]
L224.Mill_globaltech <- L224.Mill_tech[ c( "sector.name", "subsector.name", "technology" ) ]
L224.R_Mill_tech <- data.frame(
      region = GCAM_region_names$region,
      L224.Mill_tech[ rep( 1:nrow( L224.Mill_tech ), times = length( GCAM_region_names$region ) ), ] )
L224.R_Mill_tech <- L224.R_Mill_tech[ !names( L224.R_Mill_tech ) %in% c( "sector.name", "subsector.name" ) ]

printlog( "L224.GlobalResBio_Mill: Mill residue biomass parameters" )
L224.GlobalResBio_Mill <- repeat_and_add_vector( L224.Mill_globaltech, Y, c( model_base_years, model_future_years ) )
L224.GlobalResBio_Mill$residue.biomass.production <- "biomass"
L224.GlobalResBio_Mill$mass.conversion <- AvgWoodDensity_kgm3
L224.GlobalResBio_Mill$harvest.index <- ForestHarvestIndex
L224.GlobalResBio_Mill$eros.ctrl <- 0
L224.GlobalResBio_Mill$mass.to.energy <- WoodEnergyContent_GJkg
L224.GlobalResBio_Mill$water.content <- WoodWaterContent

printlog( "L224.StubResBioCurve_Mill: Mill residue biomass supply curves" )
L224.R_Mill_tech_year <- repeat_and_add_vector( L224.R_Mill_tech, "year", c( model_base_years, model_future_years ) )
L224.R_Mill_tech_year$residue.biomass.production <- "biomass"
L224.StubResBioCurve_Mill <- repeat_and_add_vector( L224.R_Mill_tech_year, "price", unique( A_resbio_curves$price) )
L224.StubResBioCurve_Mill$fract.harvested <- A_resbio_curves$For[ match( L224.StubResBioCurve_Mill$price, A_resbio_curves$price ) ]
names( L224.StubResBioCurve_Mill )[ names( L224.StubResBioCurve_Mill ) == "technology" ] <- "stub.technology"

#In base years, replace the "fraction produced" at specified prices in order to calibrate resbio production
L224.StubResBioCurve_Mill$fract.harvested[
      L224.StubResBioCurve_Mill$price == Price_bio_frac & L224.StubResBioCurve_Mill$year %in% model_base_years ] <-
         A_bio_frac_prod_R$Mill[ match( L224.StubResBioCurve_Mill$region[
             L224.StubResBioCurve_Mill$price == Price_bio_frac & L224.StubResBioCurve_Mill$year %in% model_base_years ],
      A_bio_frac_prod_R$region ) ]

#AGRICULTURE
L224.ag_resbio_R_C <- add_region_name( L111.ag_resbio_R_C )
L224.R_Ag_supplysector <- data.frame(
      region = rep( GCAM_region_names$region, times = length( unique( L111.ag_resbio_R_C$GCAM_commodity ) ) ),
      AgSupplySector = sort( rep( unique( L111.ag_resbio_R_C$GCAM_commodity ), times = length( GCAM_region_names$region ) ) ) )
L224.R_Ag_technology <- repeat_and_add_vector( L224.R_Ag_supplysector, "AEZ", AEZs )
L224.R_Ag_technology$AgSupplySubsector = paste( L224.R_Ag_technology$AgSupplySector, L224.R_Ag_technology$AEZ, sep = "" )
L224.R_Ag_technology <- repeat_and_add_vector( L224.R_Ag_technology, "Irr_Rfd", c( "IRR", "RFD" ) )
L224.R_Ag_technology$AgProductionTechnology <- paste( L224.R_Ag_technology$AgSupplySubsector, L224.R_Ag_technology$Irr_Rfd, sep = "" )
L224.R_Ag_technology <- L224.R_Ag_technology[ names( L224.R_Ag_technology ) != "AEZ" ]
L224.R_Ag_technology <- L224.R_Ag_technology[ names( L224.R_Ag_technology ) != "Irr_Rfd" ]
L224.R_Ag_technology_year <- repeat_and_add_vector( L224.R_Ag_technology, "year", c( model_base_years, model_future_years ) )
L224.R_Ag_technology_year$residue.biomass.production <- "biomass"

printlog( "L224.AgResBio_ag: Agricultural residue biomass parameters" )
L224.AgResBio_ag <- L224.R_Ag_technology_year
L224.AgResBio_ag$mass.conversion <- 1
L224.AgResBio_ag$harvest.index <- round( L224.ag_resbio_R_C$HarvestIndex[
      match( paste( L224.AgResBio_ag$region, L224.AgResBio_ag$AgSupplySector ),
             paste( L224.ag_resbio_R_C$region, L224.ag_resbio_R_C$GCAM_commodity ) ) ],
      digits_harvest_index )
L224.AgResBio_ag$eros.ctrl <- round( L224.ag_resbio_R_C$ErosCtrl_tHa[
      match( paste( L224.AgResBio_ag$region, L224.AgResBio_ag$AgSupplySector ),
             paste( L224.ag_resbio_R_C$region, L224.ag_resbio_R_C$GCAM_commodity ) ) ] * conv_tha_kgm2,
      digits_eros_ctrl )
L224.AgResBio_ag$mass.to.energy <- round( L224.ag_resbio_R_C$ResEnergy_GJt[
      match( paste( L224.AgResBio_ag$region, L224.AgResBio_ag$AgSupplySector ),
             paste( L224.ag_resbio_R_C$region, L224.ag_resbio_R_C$GCAM_commodity ) ) ] * conv_kg_t,
      digits_res_energy )
L224.AgResBio_ag$water.content <- round( L224.ag_resbio_R_C$WaterContent[
      match( paste( L224.AgResBio_ag$region, L224.AgResBio_ag$AgSupplySector ),
             paste( L224.ag_resbio_R_C$region, L224.ag_resbio_R_C$GCAM_commodity ) ) ],
      digits_water_content )

printlog( "L224.AgResBioCurve_ag: Agricultural residue biomass supply curves" )
L224.AgResBioCurve_ag <- repeat_and_add_vector( L224.R_Ag_technology_year, "price", unique (A_resbio_curves$price) )
L224.AgResBioCurve_ag$fract.harvested <- A_resbio_curves$ag[ match( L224.AgResBioCurve_ag$price, A_resbio_curves$price ) ]

#JATROPHA
printlog( "L224.Jatr_R_Y_AEZ: Jatropha residue biomass parameters" )
L224.A_biocrops_R_AEZ_irr <- A_biocrops_R_AEZ_irr
L224.A_biocrops_R_AEZ_irr$crop <- substr( A_biocrops_R_AEZ_irr$AgSupplySubsector,
      1,
      nchar( as.character( A_biocrops_R_AEZ_irr$AgSupplySubsector ) ) - 5 )

#Build base table for jatropha res bio input tables
L224.Jatr_R_AEZ <- L224.A_biocrops_R_AEZ_irr[ L224.A_biocrops_R_AEZ_irr$crop == "Jatropha",
      c( "region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" ) ]
L224.Jatr_R_AEZ <- repeat_and_add_vector( L224.Jatr_R_AEZ, "Irr_Rfd", c( "IRR", "RFD" ) )
L224.Jatr_R_AEZ$AgProductionTechnology <- paste( L224.Jatr_R_AEZ$AgSupplySubsector, L224.Jatr_R_AEZ$Irr_Rfd, sep="" )
L224.Jatr_R_AEZ <- L224.Jatr_R_AEZ[ names(L224.Jatr_R_AEZ) != "Irr_Rfd" ]
L224.Jatr_R_Y_AEZ <- repeat_and_add_vector( L224.Jatr_R_AEZ, "year", c( model_base_years, model_future_years ) )
L224.Jatr_R_Y_AEZ$residue.biomass.production <- "biomass"

L224.AgResBio_Jatr <- L224.Jatr_R_Y_AEZ
L224.AgResBio_Jatr$mass.conversion <- 1
L224.AgResBio_Jatr$harvest.index <- JatrophaHarvestIndex
L224.AgResBio_Jatr$eros.ctrl <- JatrophaErosCtrl_kgm2
L224.AgResBio_Jatr$mass.to.energy <- JatrophaMassEnergy
L224.AgResBio_Jatr$water.content <- JatrophaWaterContent

printlog( "L224.AgResBioCurve_Jatr: Agricultural residue biomass supply curves" )
L224.AgResBioCurve_Jatr <- repeat_and_add_vector( L224.Jatr_R_Y_AEZ, "price", unique( A_resbio_curves$price) )
L224.AgResBioCurve_Jatr$fract.harvested <- A_resbio_curves$Jatr[ match( L224.AgResBioCurve_Jatr$price, A_resbio_curves$price ) ]

printlog( "Removing non-existent region x AEZs from all tables" )
L224.StubResBioCurve_Mill <- subset( L224.StubResBioCurve_Mill, !region %in% no_aglu_regions )
L224.AgResBio_For <- remove_AEZ_nonexist( L224.AgResBio_For )
L224.AgResBioCurve_For <- remove_AEZ_nonexist( L224.AgResBioCurve_For )
L224.AgResBio_ag <- remove_AEZ_nonexist( L224.AgResBio_ag )
L224.AgResBioCurve_ag <- remove_AEZ_nonexist( L224.AgResBioCurve_ag )
L224.AgResBio_Jatr <- remove_AEZ_nonexist( L224.AgResBio_Jatr )
L224.AgResBioCurve_Jatr <- remove_AEZ_nonexist( L224.AgResBioCurve_Jatr )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L224.AgResBio_For, IDstring="AgResBio", domain="AGLU_LEVEL2_DATA", fn="L224.AgResBio_For",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_resbio_input_IRR.xml" )
write_mi_data( L224.GlobalResBio_Mill, "GlobalResBio", "AGLU_LEVEL2_DATA", "L224.GlobalResBio_Mill", "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml" )
write_mi_data( L224.AgResBio_ag, "AgResBio", "AGLU_LEVEL2_DATA", "L224.AgResBio_ag", "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml" )
write_mi_data( L224.AgResBio_Jatr, "AgResBio", "AGLU_LEVEL2_DATA", "L224.AgResBio_Jatr", "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml" )
write_mi_data( L224.AgResBioCurve_For, "AgResBioCurve", "AGLU_LEVEL2_DATA", "L224.AgResBioCurve_For", "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml" )
write_mi_data( L224.StubResBioCurve_Mill, "StubResBioCurve", "AGLU_LEVEL2_DATA", "L224.StubResBioCurve_Mill", "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml" )
write_mi_data( L224.AgResBioCurve_ag, "AgResBioCurve", "AGLU_LEVEL2_DATA", "L224.AgResBioCurve_ag", "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml" )
write_mi_data( L224.AgResBioCurve_Jatr, "AgResBioCurve", "AGLU_LEVEL2_DATA", "L224.AgResBioCurve_Jatr", "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml", "AGLU_XML_FINAL", "resbio_input_IRR.xml", "", xml_tag="outFile" )

logstop()
