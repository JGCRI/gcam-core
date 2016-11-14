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
A_demand_technology <- readdata( "AGLU_ASSUMPTIONS", "A_demand_technology" )
A_resbio_curves <- readdata( "AGLU_ASSUMPTIONS", "A_resbio_curves" )
A_bio_frac_prod_R <- readdata( "AGLU_ASSUMPTIONS", "A_bio_frac_prod_R" )
L111.ag_resbio_R_C <- readdata( "AGLU_LEVEL1_DATA", "L111.ag_resbio_R_C" )
L103.ag_Prod_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_Prod_Mt_R_C_Y_GLU" )
L123.For_Prod_bm3_R_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L123.For_Prod_bm3_R_Y_GLU" )

# -----------------------------------------------------------------------------
# 2. Build tables
#Add region names to bio frac table
A_bio_frac_prod_R <- add_region_name( A_bio_frac_prod_R )

#FORESTRY
#Melt forest production table to get Forest name by region and GLU
L204.R_C_GLU_For <- add_region_name( L123.For_Prod_bm3_R_Y_GLU[ R_C_GLU ] )
L204.R_C_GLU_For[[agsupp]] <- L204.R_C_GLU_For[[C]]
L204.R_C_GLU_For[[agsubs]] <- paste( L204.R_C_GLU_For[[C]], L204.R_C_GLU_For[[GLU]], sep = crop_GLU_delimiter )
L204.R_C_GLU_For[[agtech]] <- L204.R_C_GLU_For[[agsubs]]

printlog( "L204.AgResBio_For: Forest residue biomass parameters" )
L204.AgResBio_For <- repeat_and_add_vector( L204.R_C_GLU_For, Y, model_years )
L204.AgResBio_For$residue.biomass.production <- "biomass"
L204.AgResBio_For$mass.conversion <- AvgWoodDensity_kgm3
L204.AgResBio_For$harvest.index <- ForestHarvestIndex
L204.AgResBio_For$eros.ctrl <- ForestErosCtrl_kgm2
L204.AgResBio_For$mass.to.energy <- WoodEnergyContent_GJkg
L204.AgResBio_For$water.content <- WoodWaterContent
L204.AgResBio_For <- L204.AgResBio_For[ names_AgResBio ]

printlog( "L204.AgResBioCurve_For: Forest residue biomass supply curves" )
L204.AgResBioCurve_For <- repeat_and_add_vector( L204.AgResBio_For[ c( names_AgTechYr, "residue.biomass.production" ) ],
                                                 "price", unique (A_resbio_curves$price) )
L204.AgResBioCurve_For$fract.harvested <- A_resbio_curves$For[ match( L204.AgResBioCurve_For$price, A_resbio_curves$price ) ]

#In base years, replace the "fraction produced" at specified prices in order to calibrate resbio production
L204.AgResBioCurve_For$fract.harvested[
      L204.AgResBioCurve_For$price == Price_bio_frac & L204.AgResBioCurve_For$year %in% model_base_years ] <-
      A_bio_frac_prod_R$For[ match( L204.AgResBioCurve_For$region[
           L204.AgResBioCurve_For$price == Price_bio_frac & L204.AgResBioCurve_For$year %in% model_base_years ],
      A_bio_frac_prod_R$region ) ]

#MILL -- NOTE THAT THIS IS A TECHNOLOGY IN THE GLOBAL TECH DATABASE
L204.Mill_tech <- A_demand_technology[ A_demand_technology$supplysector == "NonFoodDemand_Forest", c( supp, subs, tech ) ]
L204.Mill_tech[ c( "sector.name", "subsector.name" ) ] <- L204.Mill_tech[ c( "supplysector", "subsector" ) ]
L204.Mill_globaltech <- L204.Mill_tech[ c( "sector.name", "subsector.name", "technology" ) ]
L204.R_Mill_tech <- write_to_all_regions( L204.Mill_tech, names = c( reg, supp, subs, tech) )
L204.R_Mill_tech <- subset( L204.R_Mill_tech, !region %in% no_aglu_regions )

printlog( "L204.GlobalResBio_Mill: Mill residue biomass parameters" )
L204.GlobalResBio_Mill <- repeat_and_add_vector( L204.Mill_globaltech, Y, model_years )
L204.GlobalResBio_Mill$residue.biomass.production <- "biomass"
L204.GlobalResBio_Mill$mass.conversion <- AvgWoodDensity_kgm3
L204.GlobalResBio_Mill$harvest.index <- ForestHarvestIndex
L204.GlobalResBio_Mill$eros.ctrl <- 0
L204.GlobalResBio_Mill$mass.to.energy <- WoodEnergyContent_GJkg
L204.GlobalResBio_Mill$water.content <- WoodWaterContent

printlog( "L204.StubResBioCurve_Mill: Mill residue biomass supply curves" )
L204.R_Mill_tech_year <- repeat_and_add_vector( L204.R_Mill_tech, Y, model_years )
L204.R_Mill_tech_year$residue.biomass.production <- "biomass"
L204.StubResBioCurve_Mill <- repeat_and_add_vector( L204.R_Mill_tech_year, "price", unique( A_resbio_curves$price) )
L204.StubResBioCurve_Mill$fract.harvested <- A_resbio_curves$For[ match( L204.StubResBioCurve_Mill$price, A_resbio_curves$price ) ]
names( L204.StubResBioCurve_Mill )[ names( L204.StubResBioCurve_Mill ) == "technology" ] <- "stub.technology"

#In base years, replace the "fraction produced" at specified prices in order to calibrate resbio production
L204.StubResBioCurve_Mill$fract.harvested[
      L204.StubResBioCurve_Mill$price == Price_bio_frac & L204.StubResBioCurve_Mill$year %in% model_base_years ] <-
         A_bio_frac_prod_R$Mill[ match( L204.StubResBioCurve_Mill$region[
             L204.StubResBioCurve_Mill$price == Price_bio_frac & L204.StubResBioCurve_Mill$year %in% model_base_years ],
      A_bio_frac_prod_R$region ) ]

#AGRICULTURE
L204.ag_resbio_R_C <- add_region_name( L111.ag_resbio_R_C )
L204.ag_R_C_GLU <- add_region_name( L103.ag_Prod_Mt_R_C_Y_GLU[ R_C_GLU ] )

printlog( "L204.AgResBio_ag: Agricultural residue biomass parameters" )
L204.AgResBio_ag <- merge( L204.ag_R_C_GLU, L204.ag_resbio_R_C )
L204.AgResBio_ag[[agsupp]] <- L204.AgResBio_ag[[C]]
L204.AgResBio_ag[[agsubs]] <- paste( L204.AgResBio_ag[[C]], L204.AgResBio_ag[[GLU]], sep = crop_GLU_delimiter )
L204.AgResBio_ag[[agtech]] <- L204.AgResBio_ag[[agsubs]]
L204.AgResBio_ag <- repeat_and_add_vector( L204.AgResBio_ag, Y, model_years )
L204.AgResBio_ag$residue.biomass.production <- "biomass"
L204.AgResBio_ag$mass.conversion <- 1
L204.AgResBio_ag$harvest.index <- round( L204.AgResBio_ag$HarvestIndex, digits_harvest_index )
L204.AgResBio_ag$eros.ctrl <- round( L204.AgResBio_ag$ErosCtrl_tHa * conv_tha_kgm2, digits_eros_ctrl )
L204.AgResBio_ag$mass.to.energy <- round( L204.AgResBio_ag$ResEnergy_GJt * conv_kg_t, digits_res_energy )
L204.AgResBio_ag$water.content <- round( L204.AgResBio_ag$WaterContent, digits_water_content )
L204.AgResBio_ag <- L204.AgResBio_ag[ names_AgResBio ]

printlog( "L204.AgResBioCurve_ag: Agricultural residue biomass supply curves" )
L204.AgResBioCurve_ag <- repeat_and_add_vector( L204.AgResBio_ag[ c( names_AgTechYr, "residue.biomass.production" ) ], "price", unique (A_resbio_curves$price) )
L204.AgResBioCurve_ag$fract.harvested <- A_resbio_curves$ag[ match( L204.AgResBioCurve_ag$price, A_resbio_curves$price ) ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L204.AgResBio_For, IDstring="AgResBio", domain="AGLU_LEVEL2_DATA", fn="L204.AgResBio_For",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_resbio_input.xml" )
write_mi_data( L204.GlobalResBio_Mill, "GlobalResBio", "AGLU_LEVEL2_DATA", "L204.GlobalResBio_Mill", "AGLU_XML_BATCH", "batch_resbio_input.xml" )
write_mi_data( L204.AgResBio_ag, "AgResBio", "AGLU_LEVEL2_DATA", "L204.AgResBio_ag", "AGLU_XML_BATCH", "batch_resbio_input.xml" )
write_mi_data( L204.AgResBioCurve_For, "AgResBioCurve", "AGLU_LEVEL2_DATA", "L204.AgResBioCurve_For", "AGLU_XML_BATCH", "batch_resbio_input.xml" )
write_mi_data( L204.StubResBioCurve_Mill, "StubResBioCurve", "AGLU_LEVEL2_DATA", "L204.StubResBioCurve_Mill", "AGLU_XML_BATCH", "batch_resbio_input.xml" )
write_mi_data( L204.AgResBioCurve_ag, "AgResBioCurve", "AGLU_LEVEL2_DATA", "L204.AgResBioCurve_ag", "AGLU_XML_BATCH", "batch_resbio_input.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_resbio_input.xml", "AGLU_XML_FINAL", "resbio_input.xml", "", xml_tag="outFile" )

logstop()
