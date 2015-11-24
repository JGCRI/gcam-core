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
printlog( "Input files for F-gases" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_region <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
FUT_EMISS_GV <- readdata( "EMISSIONS_LEVEL0_DATA", "FUT_EMISS_GV")
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
L241.hfc <- subset( L241.hfc, L241.hfc$year %in% hfc_model_base_years )
L241.hfc <- add_region_name( L241.hfc )

#Format for csv file
L241.hfc_all <- L241.hfc[ names_StubTechNonCO2 ]
L241.hfc_all$input.emissions <- round( L241.hfc$value, digits_emissions )

printlog( "L241.pfc: F-gas emissions for technologies in all regions" )
#Interpolate and add region name
#Because no future coefs are read in for any techs, anything that's zero in all base years can be dropped
L241.pfc <- L142.pfc_R_S_T_Yh[ rowSums( L142.pfc_R_S_T_Yh[ names( L142.pfc_R_S_T_Yh ) %in% X_historical_years ] ) > 0, ]
L241.pfc <- melt( L241.pfc, id.vars = grep( "X[0-9]{4}", names( L241.pfc ), invert = T ) )
L241.pfc$year <- as.numeric( substr( L241.pfc$variable, 2, 5 ) )
L241.pfc <- subset( L241.pfc, L241.pfc$year %in% emiss_model_base_years )
L241.pfc <- add_region_name( L241.pfc )

#Format for csv file
L241.pfc_all <- L241.pfc[ names_StubTechNonCO2 ]
L241.pfc_all$input.emissions <- round( L241.pfc$value, digits_emissions )

printlog( "L241.hfc_future: F-gas emissions factors for future years" )
#First, prepare 2010 ef for cooling
L241.hfc_cool_ef <- L141.hfc_ef_R_cooling_Yh
L241.hfc_cool_ef <- melt( L241.hfc_cool_ef, id.vars = grep( "X[0-9]{4}", names( L241.hfc_cool_ef ), invert = T ) )
L241.hfc_cool_ef$year <- as.numeric( substr( L241.hfc_cool_ef$variable, 2, 5 ) )
L241.hfc_cool_ef_2010 <- subset( L241.hfc_cool_ef, L241.hfc_cool_ef$year == max(hfc_model_base_years) )
L241.hfc_cool_ef_2010 <- add_region_name( L241.hfc_cool_ef_2010 )

#Determine which regions need updated emissions factors
#Assume that USA is in region 1
Index_region <- GCAM_region_names$region[ GCAM_region_names$GCAM_region_ID == 1 ]
L241.hfc_cool_ef_2010$USA_factor <- L241.hfc_cool_ef_2010$value[ match( paste( vecpaste( L241.hfc_cool_ef_2010[ c( "supplysector", "Non.CO2", "year" )]), Index_region),
                                                                        vecpaste( L241.hfc_cool_ef_2010[ c( "supplysector", "Non.CO2", "year", "region" )]) )]
L241.hfc_cool_ef_2010$USA_factor[ L241.hfc_cool_ef_2010$Non.CO2 == "HFC134a" ] <- L241.hfc_cool_ef_2010$USA_factor[ L241.hfc_cool_ef_2010$Non.CO2 == "HFC134a" ] / 3 # Don't let HFC134a grow as fast. It is less commonly used now in USA
L241.hfc_cool_ef_update <- subset( L241.hfc_cool_ef_2010, L241.hfc_cool_ef_2010$USA_factor > L241.hfc_cool_ef_2010$value )
names( L241.hfc_cool_ef_update )[ names( L241.hfc_cool_ef_update) == "value" ] <- "X2010"
names( L241.hfc_cool_ef_update )[ names( L241.hfc_cool_ef_update) == "USA_factor" ] <- "X2030"
L241.hfc_cool_ef_update <- L241.hfc_cool_ef_update[ names( L241.hfc_cool_ef_update ) %!in% c( "year", "variable" )]
L241.hfc_cool_ef_update$X2015 <- L241.hfc_cool_ef_update$X2010 + ( 5 / 20 )*( L241.hfc_cool_ef_update$X2030 - L241.hfc_cool_ef_update$X2010 )
L241.hfc_cool_ef_update$X2020 <- L241.hfc_cool_ef_update$X2010 + (10 / 20 )*( L241.hfc_cool_ef_update$X2030 - L241.hfc_cool_ef_update$X2010 )
L241.hfc_cool_ef_update$X2025 <- L241.hfc_cool_ef_update$X2010 + (15 / 20 )*( L241.hfc_cool_ef_update$X2030 - L241.hfc_cool_ef_update$X2010 )
L241.hfc_cool_ef_update.melt <- melt( L241.hfc_cool_ef_update, id.vars = grep( "X[0-9]{4}", names( L241.hfc_cool_ef_update ), invert = T ) )
L241.hfc_cool_ef_update.melt$year <- as.numeric( substr( L241.hfc_cool_ef_update.melt$variable, 2, 5 ) )
L241.hfc_cool_ef_update.melt <- subset( L241.hfc_cool_ef_update.melt, L241.hfc_cool_ef_update.melt$year %!in% hfc_model_base_years )

#Then, prepare 2010 ef for non-cooling
L241.hfc_ef <- subset( L141.hfc_R_S_T_Yh, supplysector %!in% c( "resid cooling", "comm cooling" ))
L241.hfc_ef <- melt( L241.hfc_ef, id.vars = grep( "X[0-9]{4}", names( L241.hfc_ef ), invert = T ) )
L241.hfc_ef$year <- as.numeric( substr( L241.hfc_ef$variable, 2, 5 ) )
L241.hfc_ef$value <- L241.hfc_ef$value * 1000 # EF is 1000 x emissions for non-cooling sectors
L241.hfc_ef_2010 <- subset( L241.hfc_ef, L241.hfc_ef$year == max(hfc_model_base_years) )
L241.hfc_ef_2010 <- add_region_name( L241.hfc_ef_2010 )
L241.hfc_ef_2010 <- subset( L241.hfc_ef_2010, value > 0 )

#Use Data from Guus Velders to Update EF in the near term for non-cooling
L241.FUT_EMISS_GV <- dcast( FUT_EMISS_GV, Species + Scenario ~ Year, value.var=c( "EF" ) )
names( L241.FUT_EMISS_GV )[ names( L241.FUT_EMISS_GV ) == 2010 ] <- "X2010"
names( L241.FUT_EMISS_GV )[ names( L241.FUT_EMISS_GV ) == 2020 ] <- "X2020"
names( L241.FUT_EMISS_GV )[ names( L241.FUT_EMISS_GV ) == 2030 ] <- "X2030"
L241.FUT_EMISS_GV <- L241.FUT_EMISS_GV[ names( L241.FUT_EMISS_GV ) %in% c( "Species", "Scenario", "X2010", "X2020", "X2030" ) ]
L241.FUT_EMISS_GV$Ratio_2020 <- L241.FUT_EMISS_GV$X2020 / L241.FUT_EMISS_GV$X2010
L241.FUT_EMISS_GV$Ratio_2030 <- L241.FUT_EMISS_GV$X2030 / L241.FUT_EMISS_GV$X2010
L241.FUT_EMISS_GV$Species <- gsub( "-", "", L241.FUT_EMISS_GV$Species )
L241.hfc_ef_2010$X2020 <- L241.hfc_ef_2010$value * L241.FUT_EMISS_GV$Ratio_2020[ match( L241.hfc_ef_2010$Non.CO2, L241.FUT_EMISS_GV$Species )]
L241.hfc_ef_2010$X2030 <- L241.hfc_ef_2010$value * L241.FUT_EMISS_GV$Ratio_2030[ match( L241.hfc_ef_2010$Non.CO2, L241.FUT_EMISS_GV$Species )]
L241.hfc_ef_2010 <- na.omit( L241.hfc_ef_2010 )
L241.hfc_ef_update <- L241.hfc_ef_2010[ names( L241.hfc_ef_2010 ) %!in% c( "variable", "value", "year" ) ]
L241.hfc_ef_update.melt <- melt( L241.hfc_ef_update, id.vars = grep( "X[0-9]{4}", names( L241.hfc_ef_update ), invert = T ) )
L241.hfc_ef_update.melt$year <- as.numeric( substr( L241.hfc_ef_update.melt$variable, 2, 5 ) )
L241.hfc_ef_update.melt <- subset( L241.hfc_ef_update.melt, L241.hfc_ef_update.melt$year %!in% hfc_model_base_years )

#Bind data together
L241.hfc_ef_update.melt <- rbind( L241.hfc_cool_ef_update.melt, L241.hfc_ef_update.melt )

#Format for csv file
L241.hfc_future <- L241.hfc_ef_update.melt[ names_StubTechNonCO2 ]
L241.hfc_future$emiss.coeff <- round( L241.hfc_ef_update.melt$value, digits_emissions )

#Subset only the relevant technologies and gases (i.e., drop ones whose values would be zero in all years)
L241.hfc_delete <- aggregate( L241.hfc_all[ "input.emissions" ], by=as.list( L241.hfc_all[ c( names_StubTech, "Non.CO2" ) ] ), max )
L241.hfc_delete <- L241.hfc_delete[ L241.hfc_delete$input.emissions == 0 &
      vecpaste( L241.hfc_delete[ c( names_StubTech, "Non.CO2" ) ] ) %!in% vecpaste( L241.hfc_all[ c( names_StubTech, "Non.CO2" ) ] ), ]
L241.hfc_all <- L241.hfc_all[ vecpaste( L241.hfc_all[ c( names_StubTech, "Non.CO2" ) ] ) %!in% vecpaste( L241.hfc_delete[ c( names_StubTech, "Non.CO2" ) ] ), ]

# Set the units string for all fgases
L241.fgas_all_units <- rbind( L241.pfc_all[, names_StubTechNonCO2 ], L241.hfc_all[, names_StubTechNonCO2 ],
                              L241.hfc_future[, names_StubTechNonCO2 ] )
L241.fgas_all_units <- unique( L241.fgas_all_units )
L241.fgas_all_units$emissions.unit <- F_Gas_Units

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L241.hfc_all, "StbTechOutputEmissions", "EMISSIONS_LEVEL2_DATA", "L241.hfc_all", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 
write_mi_data( L241.pfc_all, "StbTechOutputEmissions", "EMISSIONS_LEVEL2_DATA", "L241.pfc_all", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 
write_mi_data( L241.hfc_future, "OutputEmissCoeff", "EMISSIONS_LEVEL2_DATA", "L241.hfc_future", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 
write_mi_data( L241.fgas_all_units, "StubTechEmissUnits", "EMISSIONS_LEVEL2_DATA", "L241.fgas_all_units", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 

insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml", "EMISSIONS_XML_FINAL", "all_fgas_emissions.xml", "", xml_tag="outFile" )

logstop()
