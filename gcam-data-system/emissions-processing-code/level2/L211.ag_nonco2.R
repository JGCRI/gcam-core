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
logstart( "L211.ag_nonco2.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions in the aglu system" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_regions <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
A_agSupplySector <- readdata( "AGLU_ASSUMPTIONS", "A_agSupplySector" )
A_biocrops_R_AEZ <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L121.nonco2_tg_R_awb_C_Y_AEZ <- readdata( "EMISSIONS_LEVEL1_DATA", "L121.nonco2_tg_R_awb_C_Y_AEZ" )
L122.ghg_tg_R_agr_C_Y_AEZ <- readdata( "EMISSIONS_LEVEL1_DATA", "L122.ghg_tg_R_agr_C_Y_AEZ" )
L113.ghg_tg_R_an_C_Sys_Fd_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L113.ghg_tg_R_an_C_Sys_Fd_Yh" )
L123.bcoc_tgmt_R_awb_2000 <- readdata( "EMISSIONS_LEVEL1_DATA", "L123.bcoc_tgmt_R_awb_2000" )
L115.nh3_tg_R_an_C_Sys_Fd_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L115.nh3_tg_R_an_C_Sys_Fd_Yh" )
A11.max_reduction <- readdata( "EMISSIONS_ASSUMPTIONS", "A11.max_reduction" )
A11.steepness <- readdata( "EMISSIONS_ASSUMPTIONS", "A11.steepness" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#Sulfur emissions
printlog( "L211.AWBEmissions: AWB emissions in all regions" )
#Interpolate and add region name
L211.AWB <- L121.nonco2_tg_R_awb_C_Y_AEZ[ names( L121.nonco2_tg_R_awb_C_Y_AEZ ) %!in% c( "X2009", "X2010") ]

#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L211.AWB <- melt( L211.AWB, id.vars = grep( "X[0-9]{4}", names( L211.AWB ), invert = T ) )
L211.AWB <- na.omit( L211.AWB )
L211.AWB$year <- as.numeric( substr( L211.AWB$variable, 2, 5 ) )
L211.AWB <- subset( L211.AWB, L211.AWB$year %in% emiss_model_base_years )
L211.AWB <- add_region_name( L211.AWB )

#Add subsector and tech names
L211.AWB$AgSupplySector <- L211.AWB$GCAM_commodity
L211.AWB$AgSupplySubsector <- paste( L211.AWB$GCAM_commodity, L211.AWB$AEZ, sep="" )
L211.AWB$AgProductionTechnology <- paste( L211.AWB$GCAM_commodity, L211.AWB$AEZ, sep="" )

#Format for csv file
L211.AWBEmissions <- L211.AWB[ c( names_AgTech, "year", "Non.CO2" ) ]
L211.AWBEmissions$input.emissions <- L211.AWB$value

printlog( "L211.AGREmissions: ag AGR emissions in all regions" )
#Interpolate and add region name
L211.AGR <- L122.ghg_tg_R_agr_C_Y_AEZ[ names( L122.ghg_tg_R_agr_C_Y_AEZ ) %!in% c( "X2009", "X2010") ]

#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L211.AGR <- melt( L211.AGR, id.vars = grep( "X[0-9]{4}", names( L211.AGR ), invert = T ) )
L211.AGR$year <- as.numeric( substr( L211.AGR$variable, 2, 5 ) )
L211.AGR <- subset( L211.AGR, L211.AGR$year %in% emiss_model_base_years )
L211.AGR <- add_region_name( L211.AGR )

#Add subsector and tech names
L211.AGR$AgSupplySector <- L211.AGR$GCAM_commodity
L211.AGR$AgSupplySubsector <- paste( L211.AGR$GCAM_commodity, L211.AGR$AEZ, sep="" )
L211.AGR$AgProductionTechnology <- paste( L211.AGR$GCAM_commodity, L211.AGR$AEZ, sep="" )

#Format for csv file
L211.AGREmissions <- L211.AGR[ c( names_AgTech, "year", "Non.CO2" ) ]
L211.AGREmissions$input.emissions <- round( L211.AGR$value, digits_emissions )

printlog( "L211.AGRBioEmissions: bio AGR emissions in all regions" )
#Map in coefficients from assumption file
L211.AGRBio <- A_regions[ names( A_regions ) %in% c( "region", "bio_N2O_coef" )]

#Add sector, subsector, technology, year, gas information
L211.AGRBio$AgSupplySector <- "biomass"
L211.AGRBio <- repeat_and_add_vector( L211.AGRBio, "AEZ", AEZs )
L211.AGRBio$AgSupplySubsector <- paste( L211.AGRBio$AgSupplySector, L211.AGRBio$AEZ, sep="" )
L211.AGRBio$AgProductionTechnology <- L211.AGRBio$AgSupplySubsector
L211.AGRBio$Non.CO2 <- "N2O_AGR"
L211.AGRBio$year <- final_emiss_year

#Format for csv file
L211.AGRBio <- L211.AGRBio[ c( names_AgTech, "year", "Non.CO2", "bio_N2O_coef" ) ]

L211.AGRBio <- rename_biocrops( L211.AGRBio, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
                                          lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )

printlog( "L211.AnAGREmissions: an AGR emissions in all regions" )
#Interpolate and add region name
L211.AN <- L113.ghg_tg_R_an_C_Sys_Fd_Yh[ names( L113.ghg_tg_R_an_C_Sys_Fd_Yh ) %!in% c( "X2009", "X2010") ]

#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L211.AN <- melt( L211.AN, id.vars = grep( "X[0-9]{4}", names( L211.AN ), invert = T ) )
L211.AN$year <- as.numeric( substr( L211.AN$variable, 2, 5 ) )
L211.AN <- subset( L211.AN, L211.AN$year %in% emiss_model_base_years )
L211.AN <- add_region_name( L211.AN )

#Format for csv file
L211.AnEmissions <- L211.AN[ c( names_StubTechYr, "Non.CO2" ) ]
L211.AnEmissions$input.emissions <- round( L211.AN$value, digits_emissions )

printlog( "L211.AnNH3Emissions: an NH3 emissions in all regions" )
#Interpolate and add region name
L211.AN_NH3 <- L115.nh3_tg_R_an_C_Sys_Fd_Yh[ names( L115.nh3_tg_R_an_C_Sys_Fd_Yh ) %!in% c( "X2009", "X2010") ]

#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L211.AN_NH3 <- melt( L211.AN_NH3, id.vars = grep( "X[0-9]{4}", names( L211.AN ), invert = T ) )
L211.AN_NH3$year <- as.numeric( substr( L211.AN_NH3$variable, 2, 5 ) )
L211.AN_NH3 <- subset( L211.AN_NH3, L211.AN_NH3$year %in% emiss_model_base_years )
L211.AN_NH3 <- add_region_name( L211.AN_NH3 )

#Format for csv file
L211.AnNH3Emissions <- L211.AN_NH3[ c( names_StubTechYr, "Non.CO2" ) ]
L211.AnNH3Emissions$input.emissions <- round( L211.AN_NH3$value, digits_emissions )

printlog( "L211.AWB_BCOC_Emissions: BC / OC AWB emissions in all regions" )
#Add region name & replicate for all commodities & base years
L211.AWB_BCOC <- L123.bcoc_tgmt_R_awb_2000
L211.AWB_BCOC <- add_region_name( L211.AWB_BCOC )
L211.AWB_BCOC <- repeat_and_add_vector( L211.AWB_BCOC, "AgSupplySector", A_agSupplySector$AgSupplySector )
L211.AWB_BCOC <- subset( L211.AWB_BCOC,  L211.AWB_BCOC$AgSupplySector %!in% c( "Pasture", "Forest" ) )
L211.AWB_BCOC <- repeat_and_add_vector( L211.AWB_BCOC, "AEZ", AEZs )
L211.AWB_BCOC$AgSupplySubsector <- paste( L211.AWB_BCOC$AgSupplySector, L211.AWB_BCOC$AEZ, sep="" )
L211.AWB_BCOC$AgProductionTechnology <- paste( L211.AWB_BCOC$AgSupplySector, L211.AWB_BCOC$AEZ, sep="" )
L211.AWB_BCOC <- repeat_and_add_vector( L211.AWB_BCOC, "year", model_base_years )

#Format for csv file
L211.AWB_BCOC_Emissions <- L211.AWB_BCOC[ c( names_AgTech, "year", "Non.CO2" ) ]
L211.AWB_BCOC_Emissions$emiss.coef <- round( L211.AWB_BCOC$emfact, digits_emissions )

printlog( "L211.nonghg_max_reduction: maximum reduction for energy technologies in all regions" )
L211.max_reduction <- melt( A11.max_reduction, id.vars=c( "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" ))
L211.max_reduction <- repeat_and_add_vector( L211.max_reduction, "region", GCAM_region_names$region )
L211.nonghg_max_reduction <- rbind( L211.AWB_BCOC[ c( names_AgTech, "Non.CO2" ) ], L211.AWB[ c( names_AgTech, "Non.CO2" ) ] )
L211.nonghg_max_reduction$year <- ctrl_base_year
L211.nonghg_max_reduction$ctrl.name <- "GDP_control"
L211.nonghg_max_reduction$max.reduction <- L211.max_reduction$value[ match( vecpaste( L211.nonghg_max_reduction[ c( names_AgTech, "Non.CO2" ) ]), vecpaste( L211.max_reduction[ c( names_AgTech, "variable" ) ]) )]
L211.nonghg_max_reduction <- na.omit( L211.nonghg_max_reduction )
L211.nonghg_max_reduction <- L211.nonghg_max_reduction[ c( names_AgTechYr, "Non.CO2", "ctrl.name", "max.reduction" )]

printlog( "L211.nonghg_steepness: steepness of reduction for energy technologies in all regions" )
L211.steepness <- melt( A11.steepness, id.vars=c( "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" ))
L211.steepness <- repeat_and_add_vector( L211.steepness, "region", GCAM_region_names$region )
L211.nonghg_steepness <- L211.nonghg_max_reduction[ c( names_AgTech, "Non.CO2" ) ]
L211.nonghg_steepness$year <- ctrl_base_year
L211.nonghg_steepness$ctrl.name <- "GDP_control"
L211.nonghg_steepness$steepness <- L211.steepness$value[ match( vecpaste( L211.nonghg_steepness[ c( names_AgTech, "Non.CO2" ) ]), vecpaste( L211.steepness[ c( names_AgTech, "variable" ) ]) )]
L211.nonghg_steepness <- na.omit( L211.nonghg_steepness )
L211.nonghg_steepness <- L211.nonghg_steepness[ c( names_AgTechYr, "Non.CO2", "ctrl.name", "steepness" )]

printlog( "Removing non-existent regions and AEZs from all tables")
L211.AnEmissions <- subset( L211.AnEmissions, !region %in% no_aglu_regions )
L211.AnNH3Emissions <- subset( L211.AnNH3Emissions, !region %in% no_aglu_regions )
L211.AWBEmissions <- remove_AEZ_nonexist( L211.AWBEmissions )
L211.AGREmissions <- remove_AEZ_nonexist( L211.AGREmissions )
L211.AGRBio <- remove_AEZ_nonexist( L211.AGRBio )
L211.AWB_BCOC_Emissions <- remove_AEZ_nonexist( L211.AWB_BCOC_Emissions )
L211.nonghg_max_reduction <- remove_AEZ_nonexist( L211.nonghg_max_reduction )
L211.nonghg_steepness <- remove_AEZ_nonexist( L211.nonghg_steepness )

printlog( "Rename to regional SO2" )
L211.AWBEmissions <- rename_SO2( L211.AWBEmissions, A_regions, TRUE )
L211.nonghg_max_reduction <- rename_SO2( L211.nonghg_max_reduction, A_regions, TRUE )
L211.nonghg_steepness <- rename_SO2( L211.nonghg_steepness, A_regions, TRUE )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L211.AWBEmissions, "OutputEmissionsAg", "EMISSIONS_LEVEL2_DATA", "L211.awb_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.AGREmissions, "OutputEmissionsAg", "EMISSIONS_LEVEL2_DATA", "L211.agr_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.AnEmissions, "OutputEmissions", "EMISSIONS_LEVEL2_DATA", "L211.an_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.AnNH3Emissions, "OutputEmissions", "EMISSIONS_LEVEL2_DATA", "L211.an_nh3_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.AGRBio, "OutputEmissCoeffAg", "EMISSIONS_LEVEL2_DATA", "L211.bio_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.AWB_BCOC_Emissions, "OutputEmissCoeffAg", "EMISSIONS_LEVEL2_DATA", "L211.awb_bcoc_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.nonghg_max_reduction, "AgGDPCtrlMax", "EMISSIONS_LEVEL2_DATA", "L211.nonco2_max_reduction", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.nonghg_steepness, "AgGDPCtrlSteep", "EMISSIONS_LEVEL2_DATA", "L211.nonco2_steepness", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 

logstop()
