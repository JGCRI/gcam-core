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
logstart( "L201.en_nonco2.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions in the energy system" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_region <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
L111.nonghg_tg_R_en_S_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L111.nonghg_tg_R_en_S_F_Yh" )
L111.nonghg_tgej_R_en_S_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L111.nonghg_tgej_R_en_S_F_Yh" )
L112.ghg_tg_R_en_S_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L112.ghg_tg_R_en_S_F_Yh" )
L112.ghg_tgej_R_en_S_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L112.ghg_tgej_R_en_S_F_Yh" )
L114.bcoc_tgej_R_en_S_F_2000 <- readdata( "EMISSIONS_LEVEL1_DATA", "L114.bcoc_tgej_R_en_S_F_2000" )
A51.max_reduction <- readdata( "EMISSIONS_ASSUMPTIONS", "A51.max_reduction" )
A51.steepness <- readdata( "EMISSIONS_ASSUMPTIONS", "A51.steepness" )


# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "L201.nonghg_en: Pollutant emissions for energy technologies in all regions" )
#Interpolate and add region name
L201.nonghg <- subset( L111.nonghg_tg_R_en_S_F_Yh, L111.nonghg_tg_R_en_S_F_Yh$supplysector != "out_resources" )
L201.nonghg <- interpolate_and_melt( L201.nonghg, emiss_model_base_years )
L201.nonghg <- add_region_name( L201.nonghg )

#Format for csv file
L201.nonghg_en <- L201.nonghg[ c( names_StubTechYr, "Non.CO2" ) ]
L201.nonghg_en$input.emissions <- round( L201.nonghg$value, digits_emissions )

printlog( "L201.ghg_en: GHG emissions for energy technologies in all regions" )
#Interpolate and add region name
L201.GHG <- subset( L112.ghg_tg_R_en_S_F_Yh, L112.ghg_tg_R_en_S_F_Yh$supplysector != "out_resources" )
L201.GHG <- interpolate_and_melt( L201.GHG, emiss_model_base_years )
L201.GHG <- add_region_name( L201.GHG )

#Format for csv file
L201.ghg_en <- L201.GHG[ c( names_StubTechYr, "Non.CO2" ) ]
L201.ghg_en$input.emissions <- round( L201.GHG$value, digits_emissions )

printlog( "L201.bcoc_en: BC/OC emissions factors for energy technologies in all regions" )
#Interpolate and add region name
L201.BCOC <- subset( L114.bcoc_tgej_R_en_S_F_2000, L114.bcoc_tgej_R_en_S_F_2000$supplysector != "out_resources" )
L201.BCOC <- add_region_name( L201.BCOC )
L201.BCOC <- repeat_and_add_vector( L201.BCOC, "year", model_base_years )

#Format for csv file
L201.bcoc_en <- L201.BCOC[ c( names_StubTechYr, "Non.CO2" ) ]
L201.bcoc_en$emiss.coef <- round( L201.BCOC$X2000, digits_emissions )

printlog( "L201.nonghg_max_reduction: maximum reduction for energy technologies in all regions" )
L201.max_reduction <- melt( A51.max_reduction, id.vars=c( "supplysector", "subsector", "stub.technology" ))
L201.max_reduction <- repeat_and_add_vector( L201.max_reduction, "region", GCAM_region_names$region )
L201.nonghg_max_reduction <- rbind( L201.bcoc_en[ c( names_StubTech, "Non.CO2" ) ], L201.nonghg_en[ c( names_StubTech, "Non.CO2" ) ] )
L201.nonghg_max_reduction$year <- max( model_base_years )
L201.nonghg_max_reduction$ctrl.name <- "GDP_control"
L201.nonghg_max_reduction$max.reduction <- L201.max_reduction$value[ match( vecpaste( L201.nonghg_max_reduction[ c( names_StubTech, "Non.CO2" ) ]), vecpaste( L201.max_reduction[ c( names_StubTech, "variable" ) ]) )]
L201.nonghg_max_reduction <- na.omit( L201.nonghg_max_reduction )
L201.nonghg_max_reduction <- L201.nonghg_max_reduction[ c( names_StubTechYr, "Non.CO2", "ctrl.name", "max.reduction" )]

printlog( "L201.nonghg_steepness: steepness of reduction for energy technologies in all regions" )
L201.steepness <- melt( A51.steepness, id.vars=c( "supplysector", "subsector", "stub.technology" ))
L201.steepness <- repeat_and_add_vector( L201.steepness, "region", GCAM_region_names$region )
L201.nonghg_steepness <- L201.nonghg_max_reduction[ c( names_StubTech, "Non.CO2" ) ]
L201.nonghg_steepness$year <- max( model_base_years )
L201.nonghg_steepness$ctrl.name <- "GDP_control"
L201.nonghg_steepness$steepness <- L201.steepness$value[ match( vecpaste( L201.nonghg_steepness[ c( names_StubTech, "Non.CO2" ) ]), vecpaste( L201.steepness[ c( names_StubTech, "variable" ) ]) )]
L201.nonghg_steepness <- na.omit( L201.nonghg_steepness )
L201.nonghg_steepness <- L201.nonghg_steepness[ c( names_StubTechYr, "Non.CO2", "ctrl.name", "steepness" )]

printlog( "L201.nonghg_res: Pollutant emissions for energy resources in all regions" )
#Interpolate and add region name
L201.nonghg_coef <- subset( L111.nonghg_tgej_R_en_S_F_Yh, L111.nonghg_tgej_R_en_S_F_Yh$supplysector == "out_resources" )
L201.nonghg_coef <- interpolate_and_melt( L201.nonghg_coef, emiss_model_base_years )
L201.nonghg_coef <- subset( L201.nonghg_coef, L201.nonghg_coef$year == final_emiss_year )
L201.nonghg_coef <- add_region_name( L201.nonghg_coef )

#Format for csv file
L201.nonghg_res <- L201.nonghg_coef[ c( "region", "subsector", "Non.CO2" ) ]
L201.nonghg_res$emiss.coef <- round( L201.nonghg_coef$value, digits_emissions )

printlog( "L201.ghg_en: GHG emissions for energy technologies in all regions" )
#Interpolate and add region name
L201.GHG_coef <- subset( L112.ghg_tgej_R_en_S_F_Yh, L112.ghg_tgej_R_en_S_F_Yh$supplysector == "out_resources" )
L201.GHG_coef <- interpolate_and_melt( L201.GHG_coef, emiss_model_base_years )
L201.GHG_coef <- subset( L201.GHG_coef, L201.GHG_coef$year == final_emiss_year )
L201.GHG_coef <- add_region_name( L201.GHG_coef )

#Format for csv file
L201.ghg_res <- L201.GHG_coef[ c( "region", "subsector", "Non.CO2" ) ]
L201.ghg_res$input.emissions <- round( L201.GHG_coef$value, digits_emissions )

printlog( "Rename to regional SO2" )
L201.nonghg_en <- rename_SO2( L201.nonghg_en, A_region, FALSE )
L201.nonghg_max_reduction <- rename_SO2( L201.nonghg_max_reduction, A_region, FALSE )
L201.nonghg_steepness <- rename_SO2( L201.nonghg_steepness, A_region, FALSE )
L201.nonghg_res <- rename_SO2( L201.nonghg_res, A_region, FALSE )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L201.nonghg_en, "InputEmissions", "EMISSIONS_LEVEL2_DATA", "L201.en_pol_emissions", "EMISSIONS_XML_BATCH", "batch_en_emissions.xml" ) 
write_mi_data( L201.ghg_en, "InputEmissions", "EMISSIONS_LEVEL2_DATA", "L201.en_ghg_emissions", "EMISSIONS_XML_BATCH", "batch_en_emissions.xml" ) 
write_mi_data( L201.bcoc_en, "InputEmissCoeff", "EMISSIONS_LEVEL2_DATA", "L201.en_bcoc_emissions", "EMISSIONS_XML_BATCH", "batch_en_emissions.xml" ) 
write_mi_data( L201.nonghg_max_reduction, "GDPCtrlMax", "EMISSIONS_LEVEL2_DATA", "L201.nonco2_max_reduction", "EMISSIONS_XML_BATCH", "batch_en_emissions.xml" ) 
write_mi_data( L201.nonghg_steepness, "GDPCtrlSteep", "EMISSIONS_LEVEL2_DATA", "L201.nonco2_steepness", "EMISSIONS_XML_BATCH", "batch_en_emissions.xml" ) 
write_mi_data( L201.nonghg_res, "ResEmissCoef", "EMISSIONS_LEVEL2_DATA", "L201.nonghg_res", "EMISSIONS_XML_BATCH", "batch_en_emissions.xml" ) 
write_mi_data( L201.ghg_res, "ResEmissCoef", "EMISSIONS_LEVEL2_DATA", "L201.ghg_res", "EMISSIONS_XML_BATCH", "batch_en_emissions.xml" ) 

logstop()
