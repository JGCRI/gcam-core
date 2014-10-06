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
logstart( "L241.fgas.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions for F-gases" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_region <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
L141.hfc_R_S_T_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L141.hfc_R_S_T_Yh" )
L142.pfc_R_S_T_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L142.pfc_R_S_T_Yh" )
L141.hfc_ef_R_cooling_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L141.hfc_ef_R_cooling_Yh" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#HFC emissions
printlog( "L241.hfc: F-gas emissions for technologies in all regions" )
#Interpolate and add region name
L241.hfc <- L141.hfc_R_S_T_Yh
L241.hfc <- melt( L241.hfc, id.vars = grep( "X[0-9]{4}", names( L241.hfc ), invert = T ) )
L241.hfc$year <- as.numeric( substr( L241.hfc$variable, 2, 5 ) )
L241.hfc <- subset( L241.hfc, L241.hfc$year %in% emiss_model_base_years )
L241.hfc <- add_region_name( L241.hfc )

#Format for csv file
L241.hfc_all <- L241.hfc[ c( names_StubTechYr, "Non.CO2" ) ]
L241.hfc_all$input.emissions <- round( L241.hfc$value, digits_emissions )

printlog( "L241.pfc: F-gas emissions for technologies in all regions" )
#Interpolate and add region name
L241.pfc <- L142.pfc_R_S_T_Yh
L241.pfc <- melt( L241.pfc, id.vars = grep( "X[0-9]{4}", names( L241.pfc ), invert = T ) )
L241.pfc$year <- as.numeric( substr( L241.pfc$variable, 2, 5 ) )
L241.pfc <- subset( L241.pfc, L241.pfc$year %in% emiss_model_base_years )
L241.pfc <- add_region_name( L241.pfc )

#Format for csv file
L241.pfc_all <- L241.pfc[ c( names_StubTechYr, "Non.CO2" ) ]
L241.pfc_all$input.emissions <- round( L241.pfc$value, digits_emissions )

printlog( "L241.hfc_future: F-gas emissions factors for cooling in developing regions" )
#Interpolate and add region name
L241.hfc_ef <- L141.hfc_ef_R_cooling_Yh
L241.hfc_ef <- melt( L241.hfc_ef, id.vars = grep( "X[0-9]{4}", names( L241.hfc_ef ), invert = T ) )
L241.hfc_ef$year <- as.numeric( substr( L241.hfc_ef$variable, 2, 5 ) )
L241.hfc_ef_2005 <- subset( L241.hfc_ef, L241.hfc_ef$year == max(emiss_model_base_years) )
L241.hfc_ef_2005 <- add_region_name( L241.hfc_ef_2005 )

#Determine which regions need updated emissions factors
L241.hfc_ef_2005$USA_factor <- L241.hfc_ef_2005$value[ match( paste( vecpaste( L241.hfc_ef_2005[ c( "supplysector", "Non.CO2", "year" )]), "USA"),
                                                              vecpaste( L241.hfc_ef_2005[ c( "supplysector", "Non.CO2", "year", "region" )]) )]
L241.hfc_ef_update <- subset( L241.hfc_ef_2005, L241.hfc_ef_2005$USA_factor > L241.hfc_ef_2005$value )
names( L241.hfc_ef_update )[ names( L241.hfc_ef_update) == "value" ] <- "X2005"
names( L241.hfc_ef_update )[ names( L241.hfc_ef_update) == "USA_factor" ] <- "X2030"
L241.hfc_ef_update <- L241.hfc_ef_update[ names( L241.hfc_ef_update ) %!in% c( "year", "variable" )]
L241.hfc_ef_update$X2015 <- L241.hfc_ef_update$X2005 + (10 / 25 )*( L241.hfc_ef_update$X2030 - L241.hfc_ef_update$X2005 )
L241.hfc_ef_update$X2020 <- L241.hfc_ef_update$X2005 + (15 / 25 )*( L241.hfc_ef_update$X2030 - L241.hfc_ef_update$X2005 )
L241.hfc_ef_update$X2025 <- L241.hfc_ef_update$X2005 + (20 / 25 )*( L241.hfc_ef_update$X2030 - L241.hfc_ef_update$X2005 )
L241.hfc_ef_update.melt <- melt( L241.hfc_ef_update, id.vars = grep( "X[0-9]{4}", names( L241.hfc_ef_update ), invert = T ) )
L241.hfc_ef_update.melt$year <- as.numeric( substr( L241.hfc_ef_update.melt$variable, 2, 5 ) )
L241.hfc_ef_update.melt <- subset( L241.hfc_ef_update.melt, L241.hfc_ef_update.melt$year %!in% emiss_model_base_years )

#Format for csv file
L241.hfc_future <- L241.hfc_ef_update.melt[ c( names_StubTechYr, "Non.CO2" ) ]
L241.hfc_future$emiss.coeff <- round( L241.hfc_ef_update.melt$value, digits_emissions )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L241.hfc_all, "StbTechOutputEmissions", "EMISSIONS_LEVEL2_DATA", "L241.hfc_all", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 
write_mi_data( L241.pfc_all, "StbTechOutputEmissions", "EMISSIONS_LEVEL2_DATA", "L241.pfc_all", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 
write_mi_data( L241.hfc_future, "OutputEmissCoeff", "EMISSIONS_LEVEL2_DATA", "L241.hfc_future", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 

insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml", "EMISSIONS_XML_FINAL", "all_fgas_emissions.xml", "", xml_tag="outFile" )

logstop()
