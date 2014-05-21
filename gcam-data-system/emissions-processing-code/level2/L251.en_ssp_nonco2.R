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
logstart( "L251.en_ssp_nonco2.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Emissions factors for the SSP future years in the energy system" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_region <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
L161.SSP2_EF <- readdata( "EMISSIONS_LEVEL1_DATA", "L161.SSP2_EF")
L161.SSP15_EF <- readdata( "EMISSIONS_LEVEL1_DATA", "L161.SSP15_EF")
L161.SSP34_EF <- readdata( "EMISSIONS_LEVEL1_DATA", "L161.SSP34_EF")

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#SSP1/5 Emissions coefficients
L251.ssp15 <- interpolate_and_melt( L161.SSP15_EF, ssp_model_years )
L251.ssp15 <- add_region_name( L251.ssp15 )

#Format for csv file
L251.ssp15_ef <- L251.ssp15[ c( names_StubTechYr, "Non.CO2" ) ]
L251.ssp15_ef$emiss.coeff <- round( L251.ssp15$value, digits_emissions )

#SSP2 Emissions coefficients
L251.ssp2 <- interpolate_and_melt( L161.SSP2_EF, ssp_model_years )
L251.ssp2 <- add_region_name( L251.ssp2 )

#Format for csv file
L251.ssp2_ef <- L251.ssp2[ c( names_StubTechYr, "Non.CO2" ) ]
L251.ssp2_ef$emiss.coeff <- round( L251.ssp2$value, digits_emissions )

#SSP3/4 Emissions coefficients
L251.ssp34 <- interpolate_and_melt( L161.SSP34_EF, ssp_model_years )
L251.ssp34 <- add_region_name( L251.ssp34 )

#Format for csv file
L251.ssp34_ef <- L251.ssp34[ c( names_StubTechYr, "Non.CO2" ) ]
L251.ssp34_ef$emiss.coeff <- round( L251.ssp34$value, digits_emissions )

#Delete GDP controls
L251.ctrl.delete <- L251.ssp2_ef[ c( names_StubTechYr, "Non.CO2" ) ]
L251.ctrl.delete$year <- ctrl_base_year
L251.ctrl.delete$ctrl.name <- "GDP_control"

printlog( "Rename to regional SO2" )
L251.ctrl.delete <- rename_SO2( L251.ctrl.delete, A_region, FALSE )
L251.ssp15_ef <- rename_SO2( L251.ssp15_ef, A_region, FALSE )
L251.ssp2_ef <- rename_SO2( L251.ssp2_ef, A_region, FALSE )
L251.ssp34_ef <- rename_SO2( L251.ssp34_ef, A_region, FALSE )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L251.ctrl.delete, "DelEmCtrl", "EMISSIONS_LEVEL2_DATA", "L251.ctrl.delete", "EMISSIONS_XML_BATCH", "batch_delete_gdp_control.xml" ) 
write_mi_data( L251.ssp15_ef, "InputEmissCoeff", "EMISSIONS_LEVEL2_DATA", "L251.ssp15_ef", "EMISSIONS_XML_BATCH", "batch_ssp15_emissions_factors.xml" ) 
write_mi_data( L251.ssp2_ef, "InputEmissCoeff", "EMISSIONS_LEVEL2_DATA", "L251.ssp2_ef", "EMISSIONS_XML_BATCH", "batch_ssp2_emissions_factors.xml" ) 
write_mi_data( L251.ssp34_ef, "InputEmissCoeff", "EMISSIONS_LEVEL2_DATA", "L251.ssp34_ef", "EMISSIONS_XML_BATCH", "batch_ssp34_emissions_factors.xml" ) 

insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_delete_gdp_control.xml", "EMISSIONS_XML_FINAL", "delete_gdp_control.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_ssp15_emissions_factors.xml", "EMISSIONS_XML_FINAL", "ssp15_emissions_factors.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_ssp2_emissions_factors.xml", "EMISSIONS_XML_FINAL", "ssp2_emissions_factors.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_ssp34_emissions_factors.xml", "EMISSIONS_XML_FINAL", "ssp34_emissions_factors.xml", "", xml_tag="outFile" )

logstop()
