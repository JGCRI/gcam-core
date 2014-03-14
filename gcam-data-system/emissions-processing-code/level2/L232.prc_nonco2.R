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
logstart( "L232.prc_nonco2.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions for industrial & urban processes" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_region <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
L131.nonco2_tg_R_prc_S_S_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L131.nonco2_tg_R_prc_S_S_Yh" )
A32.max_reduction <- readdata( "EMISSIONS_ASSUMPTIONS", "A32.max_reduction" )
A32.steepness <- readdata( "EMISSIONS_ASSUMPTIONS", "A32.steepness" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#Sulfur emissions
printlog( "L232.nonco2_prc: Pollutant emissions for energy technologies in all regions" )
#Interpolate and add region name
L232.nonco2 <- L131.nonco2_tg_R_prc_S_S_Yh
L232.nonco2 <- melt( L232.nonco2, id.vars = grep( "X[0-9]{4}", names( L232.nonco2 ), invert = T ) )
L232.nonco2$year <- as.numeric( substr( L232.nonco2$variable, 2, 5 ) )
L232.nonco2 <- subset( L232.nonco2, L232.nonco2$year %in% emiss_model_base_years )
L232.nonco2 <- add_region_name( L232.nonco2 )

#Format for csv file
L232.nonco2_prc <- L232.nonco2[ c( names_StubTechYr, "Non.CO2" ) ]
L232.nonco2_prc$input.emissions <- round( L232.nonco2$value, digits_emissions )

printlog( "L232.nonco2_max_reduction: maximum reduction for energy technologies in all regions" )
L232.max_reduction <- melt( A32.max_reduction, id.vars=c( "supplysector", "subsector", "stub.technology" ))
L232.max_reduction <- repeat_and_add_vector( L232.max_reduction, "region", GCAM_region_names$region )
L232.nonco2_max_reduction <- L232.nonco2_prc[ c( names_StubTech, "Non.CO2" ) ] 
L232.nonco2_max_reduction <- repeat_and_add_vector( L232.nonco2_max_reduction, "year", model_base_years )
L232.nonco2_max_reduction$ctrl.name <- "GDP_control"
L232.nonco2_max_reduction$max.reduction <- L232.max_reduction$value[ match( vecpaste( L232.nonco2_max_reduction[ c( names_StubTech, "Non.CO2" ) ]), vecpaste( L232.max_reduction[ c( names_StubTech, "variable" ) ]) )]
L232.nonco2_max_reduction <- na.omit( L232.nonco2_max_reduction )
L232.nonco2_max_reduction <- L232.nonco2_max_reduction[ c( names_StubTechYr, "Non.CO2", "ctrl.name", "max.reduction" )]

printlog( "L232.nonco2_steepness: steepness of reduction for energy technologies in all regions" )
L232.steepness <- melt( A32.steepness, id.vars=c( "supplysector", "subsector", "stub.technology" ))
L232.steepness <- repeat_and_add_vector( L232.steepness, "region", GCAM_region_names$region )
L232.nonco2_steepness <- L232.nonco2_max_reduction[ c( names_StubTech, "Non.CO2" ) ]
L232.nonco2_steepness <- repeat_and_add_vector( L232.nonco2_steepness, "year", model_base_years )
L232.nonco2_steepness$ctrl.name <- "GDP_control"
L232.nonco2_steepness$steepness <- L232.steepness$value[ match( vecpaste( L232.nonco2_steepness[ c( names_StubTech, "Non.CO2" ) ]), vecpaste( L232.steepness[ c( names_StubTech, "variable" ) ]) )]
L232.nonco2_steepness <- na.omit( L232.nonco2_steepness )
L232.nonco2_steepness <- L232.nonco2_steepness[ c( names_StubTechYr, "Non.CO2", "ctrl.name", "steepness" )]

printlog( "Rename to regional SO2" )
L232.nonco2_prc <- rename_SO2( L232.nonco2_prc, A_region, FALSE )
L232.nonco2_max_reduction <- rename_SO2( L232.nonco2_max_reduction, A_region, FALSE )
L232.nonco2_steepness <- rename_SO2( L232.nonco2_steepness, A_region, FALSE )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L232.nonco2_prc, "StbTechOutputEmissions", "EMISSIONS_LEVEL2_DATA", "L232.nonco2_prc", "EMISSIONS_XML_BATCH", "batch_en_emissions.xml" ) 
write_mi_data( L232.nonco2_max_reduction, "GDPCtrlMax", "EMISSIONS_LEVEL2_DATA", "L232.nonco2_max_reduction", "EMISSIONS_XML_BATCH", "batch_en_emissions.xml" ) 
write_mi_data( L232.nonco2_steepness, "GDPCtrlSteep", "EMISSIONS_LEVEL2_DATA", "L232.nonco2_steepness", "EMISSIONS_XML_BATCH", "batch_en_emissions.xml" ) 

logstop()
