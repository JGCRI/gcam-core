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
logstart( "L204.resbio_input.R" )
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
A_biocrops_R_AEZ <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ" )
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
L204.For_Prod_bm3_R_Y_AEZ.melt <- melt( L123.For_Prod_bm3_R_Y_AEZ, id.vars = R_C_AEZ, variable_name = "year" )
L204.For_Prod_bm3_R_Y_AEZ.melt$year <- sub( "X", "", L204.For_Prod_bm3_R_Y_AEZ.melt$year )
L204.For_Prod_bm3_R_Y_AEZ.melt<- add_region_name( L204.For_Prod_bm3_R_Y_AEZ.melt )
L204.For_Prod_bm3_R_AEZ.melt <- subset( L204.For_Prod_bm3_R_Y_AEZ.melt, year == max( year ) )
L204.R_For_tech <- data.frame(
      region = L204.For_Prod_bm3_R_AEZ.melt[[reg]],
      AgSupplySector = L204.For_Prod_bm3_R_AEZ.melt[[C]],
      AgSupplySubsector = paste( L204.For_Prod_bm3_R_AEZ.melt[[C]], L204.For_Prod_bm3_R_AEZ.melt[[AEZ]], sep = AEZ_delimiter ),
      AgProductionTechnology = paste( L204.For_Prod_bm3_R_AEZ.melt[[C]], L204.For_Prod_bm3_R_AEZ.melt[[AEZ]], sep = AEZ_delimiter ) )

printlog( "L204.AgResBio_For: Forest residue biomass parameters" )
L204.AgResBio_For <- repeat_and_add_vector( L204.R_For_tech, Y, c( model_base_years, model_future_years ) )
L204.AgResBio_For$residue.biomass.production <- "biomass"
L204.AgResBio_For$mass.conversion <- AvgWoodDensity_kgm3
L204.AgResBio_For$harvest.index <- ForestHarvestIndex
L204.AgResBio_For$eros.ctrl <- ForestErosCtrl_kgm2
L204.AgResBio_For$mass.to.energy <- WoodEnergyContent_GJkg
L204.AgResBio_For$water.content <- WoodWaterContent

printlog( "L204.AgResBioCurve_For: Forest residue biomass supply curves" )
L204.R_For_tech_year <- repeat_and_add_vector( L204.R_For_tech, "year", c( model_base_years, model_future_years ) )
L204.R_For_tech_year$residue.biomass.production <- "biomass"
L204.AgResBioCurve_For <- repeat_and_add_vector( L204.R_For_tech_year, "price", unique (A_resbio_curves$price) )
L204.AgResBioCurve_For$fract.harvested <- A_resbio_curves$For[ match( L204.AgResBioCurve_For$price, A_resbio_curves$price ) ]

#In base years, replace the "fraction produced" at specified prices in order to calibrate resbio production
L204.AgResBioCurve_For$fract.harvested[
      L204.AgResBioCurve_For$price == Price_bio_frac & L204.AgResBioCurve_For$year %in% model_base_years ] <-
      A_bio_frac_prod_R$For[ match( L204.AgResBioCurve_For$region[
           L204.AgResBioCurve_For$price == Price_bio_frac & L204.AgResBioCurve_For$year %in% model_base_years ],
      A_bio_frac_prod_R$region ) ]

#MILL
L204.Mill_tech <- A_demand_technology[ A_demand_technology$supplysector == "NonFoodDemand_Forest", c( supp, subs, tech ) ]
L204.R_Mill_tech <- data.frame(
      region = GCAM_region_names$region,
      L204.Mill_tech[ rep( 1:nrow( L204.Mill_tech ), times = length( GCAM_region_names$region ) ), ] )

printlog( "L204.ResBio_Mill: Mill residue biomass parameters" )
L204.ResBio_Mill <- repeat_and_add_vector( L204.R_Mill_tech, Y, c( model_base_years, model_future_years ) )
L204.ResBio_Mill$residue.biomass.production <- "biomass"
L204.ResBio_Mill$mass.conversion <- AvgWoodDensity_kgm3
L204.ResBio_Mill$harvest.index <- ForestHarvestIndex
L204.ResBio_Mill$eros.ctrl <- 0
L204.ResBio_Mill$mass.to.energy <- WoodEnergyContent_GJkg
L204.ResBio_Mill$water.content <- WoodWaterContent

printlog( "L204.ResBioCurve_Mill: Mill residue biomass supply curves" )
L204.R_Mill_tech_year <- repeat_and_add_vector( L204.R_Mill_tech, "year", c( model_base_years, model_future_years ) )
L204.R_Mill_tech_year$residue.biomass.production <- "biomass"
L204.ResBioCurve_Mill <- repeat_and_add_vector( L204.R_Mill_tech_year, "price", unique( A_resbio_curves$price) )
L204.ResBioCurve_Mill$fract.harvested <- A_resbio_curves$For[ match( L204.ResBioCurve_Mill$price, A_resbio_curves$price ) ]

#In base years, replace the "fraction produced" at specified prices in order to calibrate resbio production
L204.ResBioCurve_Mill$fract.harvested[
      L204.ResBioCurve_Mill$price == Price_bio_frac & L204.ResBioCurve_Mill$year %in% model_base_years ] <-
         A_bio_frac_prod_R$Mill[ match( L204.ResBioCurve_Mill$region[
             L204.ResBioCurve_Mill$price == Price_bio_frac & L204.ResBioCurve_Mill$year %in% model_base_years ],
      A_bio_frac_prod_R$region ) ]

#AGRICULTURE
L204.ag_resbio_R_C <- add_region_name( L111.ag_resbio_R_C )
L204.R_Ag_supplysector <- data.frame(
      region = rep( GCAM_region_names$region, times = length( unique( L111.ag_resbio_R_C$GCAM_commodity ) ) ),
      AgSupplySector = sort( rep( unique( L111.ag_resbio_R_C$GCAM_commodity ), times = length( GCAM_region_names$region ) ) ) )
L204.R_Ag_technology <- repeat_and_add_vector( L204.R_Ag_supplysector, AEZ, AEZs )
L204.R_Ag_technology$AgSupplySubsector = paste( L204.R_Ag_technology$AgSupplySector, L204.R_Ag_technology$AEZ, sep = AEZ_delimiter )
L204.R_Ag_technology$AgProductionTechnology <- L204.R_Ag_technology$AgSupplySubsector
L204.R_Ag_technology <- L204.R_Ag_technology[ names( L204.R_Ag_technology ) != "AEZ" ]
L204.R_Ag_technology_year <- repeat_and_add_vector( L204.R_Ag_technology, "year", c( model_base_years, model_future_years ) )
L204.R_Ag_technology_year$residue.biomass.production <- "biomass"

printlog( "L204.AgResBio_ag: Agricultural residue biomass parameters" )
L204.AgResBio_ag <- L204.R_Ag_technology_year
L204.AgResBio_ag$mass.conversion <- 1
L204.AgResBio_ag$harvest.index <- round( L204.ag_resbio_R_C$HarvestIndex[
      match( vecpaste( L204.AgResBio_ag[ c( reg, agsupp ) ] ), vecpaste( L204.ag_resbio_R_C[ c( reg, C ) ] ) ) ],
      digits_harvest_index )
L204.AgResBio_ag$eros.ctrl <- round( L204.ag_resbio_R_C$ErosCtrl_tHa[
      match( vecpaste( L204.AgResBio_ag[ c( reg, agsupp ) ] ), vecpaste( L204.ag_resbio_R_C[ c( reg, C ) ] ) ) ] * conv_tha_kgm2,
      digits_eros_ctrl )
L204.AgResBio_ag$mass.to.energy <- round( L204.ag_resbio_R_C$ResEnergy_GJt[
      match( vecpaste( L204.AgResBio_ag[ c( reg, agsupp ) ] ), vecpaste( L204.ag_resbio_R_C[ c( reg, C ) ] ) ) ] * conv_kg_t,
      digits_res_energy )
L204.AgResBio_ag$water.content <- round( L204.ag_resbio_R_C$WaterContent[
      match( vecpaste( L204.AgResBio_ag[ c( reg, agsupp ) ] ), vecpaste( L204.ag_resbio_R_C[ c( reg, C ) ] ) ) ],
      digits_water_content )

printlog( "L204.AgResBioCurve_ag: Agricultural residue biomass supply curves" )
L204.AgResBioCurve_ag <- repeat_and_add_vector( L204.R_Ag_technology_year, "price", unique (A_resbio_curves$price) )
L204.AgResBioCurve_ag$fract.harvested <- A_resbio_curves$ag[ match( L204.AgResBioCurve_ag$price, A_resbio_curves$price ) ]

#JATROPHA
printlog( "L204.Jatr_R_Y_AEZ: Jatropha residue biomass parameters" )
L204.A_biocrops_R_AEZ <- A_biocrops_R_AEZ
L204.A_biocrops_R_AEZ$crop <- substr( A_biocrops_R_AEZ$AgSupplySubsector,
      1,
      nchar( as.character( A_biocrops_R_AEZ$AgSupplySubsector ) ) - 5 )

#Build base table for jatropha res bio input tables
L204.Jatr_R_AEZ <- L204.A_biocrops_R_AEZ[ L204.A_biocrops_R_AEZ$crop == "Jatropha", c( reg, agsupp, agsubs, agtech ) ]
L204.Jatr_R_Y_AEZ <- repeat_and_add_vector( L204.Jatr_R_AEZ, Y, c( model_base_years, model_future_years ) )
L204.Jatr_R_Y_AEZ$residue.biomass.production <- "biomass"

L204.AgResBio_Jatr <- L204.Jatr_R_Y_AEZ
L204.AgResBio_Jatr$mass.conversion <- 1
L204.AgResBio_Jatr$harvest.index <- JatrophaHarvestIndex
L204.AgResBio_Jatr$eros.ctrl <- JatrophaErosCtrl_kgm2
L204.AgResBio_Jatr$mass.to.energy <- JatrophaMassEnergy
L204.AgResBio_Jatr$water.content <- JatrophaWaterContent

printlog( "L204.AgResBioCurve_Jatr: Agricultural residue biomass supply curves" )
L204.AgResBioCurve_Jatr <- repeat_and_add_vector( L204.Jatr_R_Y_AEZ, "price", unique( A_resbio_curves$price) )
L204.AgResBioCurve_Jatr$fract.harvested <- A_resbio_curves$Jatr[ match( L204.AgResBioCurve_Jatr$price, A_resbio_curves$price ) ]

printlog( "Removing non-existent region x AEZs from all tables" )
L204.AgResBio_For <- remove_AEZ_nonexist( L204.AgResBio_For )
L204.AgResBioCurve_For <- remove_AEZ_nonexist( L204.AgResBioCurve_For )
L204.AgResBio_ag <- remove_AEZ_nonexist( L204.AgResBio_ag )
L204.AgResBioCurve_ag <- remove_AEZ_nonexist( L204.AgResBioCurve_ag )
L204.AgResBio_Jatr <- remove_AEZ_nonexist( L204.AgResBio_Jatr )
L204.AgResBioCurve_Jatr <- remove_AEZ_nonexist( L204.AgResBioCurve_Jatr )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L204.AgResBio_For, IDstring="AgResBio", domain="AGLU_LEVEL2_DATA", fn="L204.AgResBio_For",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_resbio_input.xml" )
write_mi_data( L204.ResBio_Mill, "ResBio", "AGLU_LEVEL2_DATA", "L204.ResBio_Mill", "AGLU_XML_BATCH", "batch_resbio_input.xml" )
write_mi_data( L204.AgResBio_ag, "AgResBio", "AGLU_LEVEL2_DATA", "L204.AgResBio_ag", "AGLU_XML_BATCH", "batch_resbio_input.xml" )
write_mi_data( L204.AgResBio_Jatr, "AgResBio", "AGLU_LEVEL2_DATA", "L204.AgResBio_Jatr", "AGLU_XML_BATCH", "batch_resbio_input.xml" )
write_mi_data( L204.AgResBioCurve_For, "AgResBioCurve", "AGLU_LEVEL2_DATA", "L204.AgResBioCurve_For", "AGLU_XML_BATCH", "batch_resbio_input.xml" )
write_mi_data( L204.ResBioCurve_Mill, "ResBioCurve", "AGLU_LEVEL2_DATA", "L204.ResBioCurve_Mill", "AGLU_XML_BATCH", "batch_resbio_input.xml" )
write_mi_data( L204.AgResBioCurve_ag, "AgResBioCurve", "AGLU_LEVEL2_DATA", "L204.AgResBioCurve_ag", "AGLU_XML_BATCH", "batch_resbio_input.xml" )
write_mi_data( L204.AgResBioCurve_Jatr, "AgResBioCurve", "AGLU_LEVEL2_DATA", "L204.AgResBioCurve_Jatr", "AGLU_XML_BATCH", "batch_resbio_input.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_resbio_input.xml", "AGLU_XML_FINAL", "resbio_input.xml", "", xml_tag="outFile" )

logstop()
