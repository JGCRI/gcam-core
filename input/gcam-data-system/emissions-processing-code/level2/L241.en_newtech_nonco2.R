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
logstart( "L241.en_newtech_nonco2.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Emissions factors for new technologies in the energy system" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_regions <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
A_regions.en <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
A41.tech_coeff <- readdata( "EMISSIONS_ASSUMPTIONS", "A41.tech_coeff" )
A51.max_reduction <- readdata( "EMISSIONS_ASSUMPTIONS", "A51.max_reduction" )
A51.steepness <- readdata( "EMISSIONS_ASSUMPTIONS", "A51.steepness" )
L111.nonghg_tgej_R_en_S_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L111.nonghg_tgej_R_en_S_F_Yh" )
L112.ghg_tgej_R_en_S_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L112.ghg_tgej_R_en_S_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#Sulfur emissions
printlog( "L241.nonco2_tech_coeff: emissions factors for new energy technologies in all regions" )
L241.nonco2_tech_coeff <- melt( A41.tech_coeff, id.vars=c( "supplysector", "subsector", "stub.technology", "exception", "exception_tech", "may.be.historic" ))
names( L241.nonco2_tech_coeff )[ names( L241.nonco2_tech_coeff ) == "variable" ] <- "Non.CO2"
names( L241.nonco2_tech_coeff )[ names( L241.nonco2_tech_coeff ) == "value" ] <- "emiss.coeff"
L241.nonco2_tech_coeff <- repeat_and_add_vector( L241.nonco2_tech_coeff, "region", GCAM_region_names$region )

#Compute CO coefficients for technologies with exceptions
L241.nonghg_tgej_R_en_S_F_Yh.melt <- melt( L111.nonghg_tgej_R_en_S_F_Yh, id.vars=c( "GCAM_region_ID", "Non.CO2", "supplysector", "subsector", "stub.technology" ) )
L241.nonghg_tgej_R_en_S_F_Yh.melt <- add_region_name( L241.nonghg_tgej_R_en_S_F_Yh.melt )
L241.nonghg_tgej_R_en_S_F_Yh.melt$year <- as.numeric( substr( L241.nonghg_tgej_R_en_S_F_Yh.melt$variable, 2, 5 ) )
max_year <- max( L241.nonghg_tgej_R_en_S_F_Yh.melt$year )
L241.co_tgej_R_en_S_F_fy.melt <- subset( L241.nonghg_tgej_R_en_S_F_Yh.melt, L241.nonghg_tgej_R_en_S_F_Yh.melt$year == max_year & L241.nonghg_tgej_R_en_S_F_Yh.melt$Non.CO2 == "CO" )

L241.co_tech_coeff_except <- subset( L241.nonco2_tech_coeff, L241.nonco2_tech_coeff$exception == "CO" & L241.nonco2_tech_coeff$Non.CO2 == "CO" )
L241.co_tech_coeff_except$new_coeff <- L241.co_tgej_R_en_S_F_fy.melt$value[ match( vecpaste(L241.co_tech_coeff_except[ c( "region", "Non.CO2", "supplysector", "exception_tech" ) ] ), vecpaste( L241.co_tgej_R_en_S_F_fy.melt[ c( "region", "Non.CO2", "supplysector", "stub.technology" ) ] ) )]
L241.co_tech_coeff_except$emiss.coeff <- L241.co_tech_coeff_except$new_coeff
L241.co_tech_coeff_except <- L241.co_tech_coeff_except[ names( L241.co_tech_coeff_except ) != "new_coeff" ]

#Compute CH4 coefficients for technologies with exceptions
L241.ghg_tgej_R_en_S_F_Yh.melt <- melt( L112.ghg_tgej_R_en_S_F_Yh, id.vars=c( "GCAM_region_ID", "Non.CO2", "supplysector", "subsector", "stub.technology" ) )
L241.ghg_tgej_R_en_S_F_Yh.melt <- add_region_name( L241.ghg_tgej_R_en_S_F_Yh.melt )
L241.ghg_tgej_R_en_S_F_Yh.melt$year <- as.numeric( substr( L241.ghg_tgej_R_en_S_F_Yh.melt$variable, 2, 5 ) )
max_year <- max( L241.ghg_tgej_R_en_S_F_Yh.melt$year )
L241.ch4_tgej_R_en_S_F_fy.melt <- subset( L241.ghg_tgej_R_en_S_F_Yh.melt, L241.ghg_tgej_R_en_S_F_Yh.melt$year == max_year & L241.ghg_tgej_R_en_S_F_Yh.melt$Non.CO2 == "CH4" )

L241.ch4_tech_coeff_except <- subset( L241.nonco2_tech_coeff, L241.nonco2_tech_coeff$exception == "CH4" & L241.nonco2_tech_coeff$Non.CO2 == "CH4" )
L241.ch4_tech_coeff_except$new_coeff <- L241.ch4_tgej_R_en_S_F_fy.melt$value[ match( vecpaste(L241.ch4_tech_coeff_except[ c( "region", "Non.CO2", "exception_tech" ) ] ), vecpaste( L241.ch4_tgej_R_en_S_F_fy.melt[ c( "region", "Non.CO2", "stub.technology" ) ] ) )]
L241.ch4_tech_coeff_except$emiss.coeff <- L241.ch4_tech_coeff_except$new_coeff
L241.ch4_tech_coeff_except <- L241.ch4_tech_coeff_except[ names( L241.ch4_tech_coeff_except ) != "new_coeff" ]

#Replace coefficients
L241.nonco2_tech_coeff_noexcept <- subset( L241.nonco2_tech_coeff, L241.nonco2_tech_coeff$emiss.coeff != "NA" )
L241.nonco2_tech_coeff <- rbind( L241.nonco2_tech_coeff_noexcept, L241.co_tech_coeff_except, L241.ch4_tech_coeff_except )
L241.nonco2_tech_coeff$year <- min( future_years )
L241.nonco2_tech_coeff <- L241.nonco2_tech_coeff[ c( names_StubTechYr, "Non.CO2", "emiss.coeff" ) ]

printlog( "L241.nonghg_max_reduction: maximum reduction for energy technologies in all regions" )
L241.max_reduction <- melt( A51.max_reduction, id.vars=c( "supplysector", "subsector", "stub.technology" ))
L241.max_reduction <- repeat_and_add_vector( L241.max_reduction, "region", GCAM_region_names$region )
L241.nonghg_max_reduction <- L241.nonco2_tech_coeff[ c( names_StubTech, "Non.CO2" ) ]
L241.nonghg_max_reduction$year <- min( future_years )
L241.nonghg_max_reduction$ctrl.name <- "GDP_control"
L241.nonghg_max_reduction$max.reduction <- L241.max_reduction$value[ match( vecpaste( L241.nonghg_max_reduction[ c( names_StubTech, "Non.CO2" ) ]), vecpaste( L241.max_reduction[ c( names_StubTech, "variable" ) ]) )]
L241.nonghg_max_reduction <- na.omit( L241.nonghg_max_reduction )
L241.nonghg_max_reduction <- L241.nonghg_max_reduction[ c( names_StubTechYr, "Non.CO2", "ctrl.name", "max.reduction" )]

printlog( "L241.nonghg_steepness: steepness of reduction for energy technologies in all regions" )
L241.steepness <- melt( A51.steepness, id.vars=c( "supplysector", "subsector", "stub.technology" ))
L241.steepness <- repeat_and_add_vector( L241.steepness, "region", GCAM_region_names$region )
L241.nonghg_steepness <- L241.nonghg_max_reduction[ c( names_StubTech, "Non.CO2" ) ]
L241.nonghg_steepness$year <- min( future_years )
L241.nonghg_steepness$ctrl.name <- "GDP_control"
L241.nonghg_steepness$steepness <- L241.steepness$value[ match( vecpaste( L241.nonghg_steepness[ c( names_StubTech, "Non.CO2" ) ]), vecpaste( L241.steepness[ c( names_StubTech, "variable" ) ]) )]
L241.nonghg_steepness <- na.omit( L241.nonghg_steepness )
L241.nonghg_steepness <- L241.nonghg_steepness[ c( names_StubTechYr, "Non.CO2", "ctrl.name", "steepness" )]

printlog( "Rename to regional SO2" )
L241.nonco2_tech_coeff <- rename_SO2( L241.nonco2_tech_coeff, A_regions, FALSE )
L241.nonghg_max_reduction <- rename_SO2( L241.nonghg_max_reduction, A_regions, FALSE )
L241.nonghg_steepness <- rename_SO2( L241.nonghg_steepness, A_regions, FALSE )

# TODO: better way to handle this, probably these technologies should pull from historical data
printlog( "Allow some future techs that may be in the historical years" )
L241.names_StubTechNoRegion <- names_StubTech[ names_StubTech != "region" ]
L241.maybe_historic <- na.omit( A41.tech_coeff[, c( L241.names_StubTechNoRegion, "may.be.historic" ) ] )
L241.nonco2_tech_coeff[ vecpaste( L241.nonco2_tech_coeff[, L241.names_StubTechNoRegion ] ) %in%
    vecpaste( L241.maybe_historic[, L241.names_StubTechNoRegion ] ), "year" ] <- model_base_years[1]
L241.nonghg_max_reduction[ vecpaste( L241.nonghg_max_reduction[, L241.names_StubTechNoRegion ] ) %in%
    vecpaste( L241.maybe_historic[, L241.names_StubTechNoRegion ] ), "year" ] <- model_base_years[1]
L241.nonghg_steepness[ vecpaste( L241.nonghg_steepness[, L241.names_StubTechNoRegion ] ) %in%
    vecpaste( L241.maybe_historic[, L241.names_StubTechNoRegion ] ), "year" ] <- model_base_years[1]

printlog( "Ensure only regions that have first gen biofuels get read in" )
L241.firstgenbio_techs <- c( "corn ethanol", "sugarbeet ethanol", "sugar cane ethanol", "biodiesel" )
L241.nonco2_tech_coeff <- subset( L241.nonco2_tech_coeff, stub.technology %!in% L241.firstgenbio_techs | 
      paste( region, stub.technology ) %in%
      c( paste( A_regions.en$region, A_regions.en$ethanol ),
         paste( A_regions.en$region, A_regions.en$biodiesel ) ) )
L241.nonghg_max_reduction <- subset( L241.nonghg_max_reduction, stub.technology %!in% L241.firstgenbio_techs | 
      paste( region, stub.technology ) %in%
      c( paste( A_regions.en$region, A_regions.en$ethanol ),
         paste( A_regions.en$region, A_regions.en$biodiesel ) ) )
L241.nonghg_steepness <- subset( L241.nonghg_steepness, stub.technology %!in% L241.firstgenbio_techs | 
      paste( region, stub.technology ) %in%
      c( paste( A_regions.en$region, A_regions.en$ethanol ),
         paste( A_regions.en$region, A_regions.en$biodiesel ) ) )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L241.nonco2_tech_coeff, "InputEmissCoeff", "EMISSIONS_LEVEL2_DATA", "L241.nonco2_tech_coeff", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L241.nonghg_max_reduction, "GDPCtrlMax", "EMISSIONS_LEVEL2_DATA", "L241.nonco2_max_reduction", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L241.nonghg_steepness, "GDPCtrlSteep", "EMISSIONS_LEVEL2_DATA", "L241.nonco2_steepness", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 

insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml", "EMISSIONS_XML_FINAL", "all_energy_emissions.xml", "", xml_tag="outFile" )

logstop()
