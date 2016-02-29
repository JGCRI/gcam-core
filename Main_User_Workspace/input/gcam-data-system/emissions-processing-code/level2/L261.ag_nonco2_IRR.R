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
logstart( "L261.ag_nonco2_IRR.R" )
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
A_biocrops_R_AEZ <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ_IRR" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L171.nonco2_tg_R_awb_C_Y_AEZ_IRR <- readdata( "EMISSIONS_LEVEL1_DATA", "L171.nonco2_tg_R_awb_C_Y_AEZ_IRR" )
L172.ghg_tg_R_agr_C_Y_AEZ_IRR <- readdata( "EMISSIONS_LEVEL1_DATA", "L172.ghg_tg_R_agr_C_Y_AEZ_IRR" )
L113.ghg_tg_R_an_C_Sys_Fd_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L113.ghg_tg_R_an_C_Sys_Fd_Yh" )
L123.bcoc_tgmt_R_awb_2000 <- readdata( "EMISSIONS_LEVEL1_DATA", "L123.bcoc_tgmt_R_awb_2000" )
L115.nh3_tg_R_an_C_Sys_Fd_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L115.nh3_tg_R_an_C_Sys_Fd_Yh" )
A11.max_reduction <- readdata( "EMISSIONS_ASSUMPTIONS", "A11.max_reduction" )
A11.steepness <- readdata( "EMISSIONS_ASSUMPTIONS", "A11.steepness" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#Sulfur emissions
printlog( "L261.AWBEmissions: AWB emissions in all regions" )
#Interpolate and add region name
L261.AWB <- L171.nonco2_tg_R_awb_C_Y_AEZ_IRR[ names( L171.nonco2_tg_R_awb_C_Y_AEZ_IRR ) %!in% c( "X2009", "X2010") ]

#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L261.AWB <- melt( L261.AWB, id.vars = grep( "X[0-9]{4}", names( L261.AWB ), invert = T ) )
L261.AWB <- na.omit( L261.AWB )
L261.AWB$year <- as.numeric( substr( L261.AWB$variable, 2, 5 ) )
L261.AWB <- subset( L261.AWB, L261.AWB$year %in% emiss_model_base_years )
L261.AWB <- add_region_name( L261.AWB )

#Add subsector and tech names
L261.AWB$AgSupplySector <- L261.AWB$GCAM_commodity
L261.AWB$AgSupplySubsector <- paste( L261.AWB$GCAM_commodity, L261.AWB$AEZ, sep="" )
L261.AWB$AgProductionTechnology <- paste( L261.AWB$GCAM_commodity, L261.AWB$AEZ, L261.AWB$irr, sep="" )

#Format for csv file
L261.AWBEmissions <- L261.AWB[ c( names_AgTech, "year", "Non.CO2" ) ]
L261.AWBEmissions$input.emissions <- L261.AWB$value

printlog( "L261.AGREmissions: ag AGR emissions in all regions" )
#Interpolate and add region name
L261.AGR <- L172.ghg_tg_R_agr_C_Y_AEZ_IRR[ names( L172.ghg_tg_R_agr_C_Y_AEZ_IRR ) %!in% c( "X2009", "X2010") ]

#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L261.AGR <- melt( L261.AGR, id.vars = grep( "X[0-9]{4}", names( L261.AGR ), invert = T ) )
L261.AGR$year <- as.numeric( substr( L261.AGR$variable, 2, 5 ) )
L261.AGR <- subset( L261.AGR, L261.AGR$year %in% emiss_model_base_years )
L261.AGR <- add_region_name( L261.AGR )

#Add subsector and tech names
L261.AGR$AgSupplySector <- L261.AGR$GCAM_commodity
L261.AGR$AgSupplySubsector <- paste( L261.AGR$GCAM_commodity, L261.AGR$AEZ, sep="" )
L261.AGR$AgProductionTechnology <- paste( L261.AGR$GCAM_commodity, L261.AGR$AEZ, L261.AGR$irr, sep="" )

#Format for csv file
L261.AGREmissions <- L261.AGR[ c( names_AgTech, "year", "Non.CO2" ) ]
L261.AGREmissions$input.emissions <- round( L261.AGR$value, digits_emissions )

printlog( "L261.AGRBioEmissions: bio AGR emissions in all regions" )
#Map in coefficients from assumption file
L261.AGRBio <- A_regions[ names( A_regions ) %in% c( "region", "bio_N2O_coef" )]

#Add sector, subsector, technology, year, gas information
L261.AGRBio$AgSupplySector <- "biomass"
L261.AGRBio <- repeat_and_add_vector( L261.AGRBio, "AEZ", AEZs )
L261.AGRBio <- repeat_and_add_vector( L261.AGRBio, "irr", c( "IRR", "RFD" ) )
L261.AGRBio$AgSupplySubsector <- paste( L261.AGRBio$AgSupplySector, L261.AGRBio$AEZ, sep="" )
L261.AGRBio$AgProductionTechnology <- paste( L261.AGRBio$AgSupplySubsector, L261.AGRBio$irr, sep="" )
L261.AGRBio$Non.CO2 <- "N2O_AGR"
L261.AGRBio$year <- ctrl_base_year

#Format for csv file
L261.AGRBio <- L261.AGRBio[ c( names_AgTech, "year", "Non.CO2", "bio_N2O_coef" ) ]

printlog( "L261.AnAGREmissions: an AGR emissions in all regions" )
#Interpolate and add region name
L261.AN <- L113.ghg_tg_R_an_C_Sys_Fd_Yh[ names( L113.ghg_tg_R_an_C_Sys_Fd_Yh ) %!in% c( "X2009", "X2010") ]

#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L261.AN <- melt( L261.AN, id.vars = grep( "X[0-9]{4}", names( L261.AN ), invert = T ) )
L261.AN$year <- as.numeric( substr( L261.AN$variable, 2, 5 ) )
L261.AN <- subset( L261.AN, L261.AN$year %in% emiss_model_base_years )
L261.AN <- add_region_name( L261.AN )

#Format for csv file
L261.AnEmissions <- L261.AN[ c( names_StubTechYr, "Non.CO2" ) ]
L261.AnEmissions$input.emissions <- round( L261.AN$value, digits_emissions )

printlog( "L261.AnNH3Emissions: an NH3 emissions in all regions" )
#Interpolate and add region name
L261.AN_NH3 <- L115.nh3_tg_R_an_C_Sys_Fd_Yh[ names( L115.nh3_tg_R_an_C_Sys_Fd_Yh ) %!in% c( "X2009", "X2010") ]

#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L261.AN_NH3 <- melt( L261.AN_NH3, id.vars = grep( "X[0-9]{4}", names( L261.AN ), invert = T ) )
L261.AN_NH3$year <- as.numeric( substr( L261.AN_NH3$variable, 2, 5 ) )
L261.AN_NH3 <- subset( L261.AN_NH3, L261.AN_NH3$year %in% emiss_model_base_years )
L261.AN_NH3 <- add_region_name( L261.AN_NH3 )

#Format for csv file
L261.AnNH3Emissions <- L261.AN_NH3[ c( names_StubTechYr, "Non.CO2" ) ]
L261.AnNH3Emissions$input.emissions <- round( L261.AN_NH3$value, digits_emissions )

printlog( "L261.AWB_BCOC_Emissions: BC / OC AWB emissions in all regions" )
#Add region name & replicate for all commodities & base years
L261.AWB_BCOC <- L123.bcoc_tgmt_R_awb_2000
L261.AWB_BCOC <- add_region_name( L261.AWB_BCOC )
L261.AWB_BCOC <- repeat_and_add_vector( L261.AWB_BCOC, "AgSupplySector", A_agSupplySector$AgSupplySector )
L261.AWB_BCOC <- subset( L261.AWB_BCOC,  L261.AWB_BCOC$AgSupplySector %!in% c( "Pasture", "Forest" ) )
L261.AWB_BCOC <- repeat_and_add_vector( L261.AWB_BCOC, "AEZ", AEZs )
L261.AWB_BCOC <- repeat_and_add_vector( L261.AWB_BCOC, "irr", c( "IRR", "RFD" ) )
L261.AWB_BCOC$AgSupplySubsector <- paste( L261.AWB_BCOC$AgSupplySector, L261.AWB_BCOC$AEZ, sep="" )
L261.AWB_BCOC$AgProductionTechnology <- paste( L261.AWB_BCOC$AgSupplySector, L261.AWB_BCOC$AEZ, L261.AWB_BCOC$irr, sep="" )
L261.AWB_BCOC <- repeat_and_add_vector( L261.AWB_BCOC, "year", model_base_years )

#Format for csv file
L261.AWB_BCOC_Emissions <- L261.AWB_BCOC[ c( names_AgTech, "year", "Non.CO2" ) ]
L261.AWB_BCOC_Emissions$emiss.coef <- round( L261.AWB_BCOC$emfact, digits_emissions )

printlog( "L261.nonghg_max_reduction: maximum reduction for energy technologies in all regions" )
L261.max_reduction <- melt( A11.max_reduction, id.vars=c( "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" ))
L261.max_reduction <- repeat_and_add_vector( L261.max_reduction, "region", GCAM_region_names$region )
L261.max_reduction <- repeat_and_add_vector( L261.max_reduction, "irr", c( "IRR", "RFD" ))
L261.max_reduction$AgProductionTechnology <- paste( L261.max_reduction$AgProductionTechnology, L261.max_reduction$irr, sep="")
L261.max_reduction <- L261.max_reduction[ names( L261.max_reduction ) != "irr" ]
L261.nonghg_max_reduction <- rbind( L261.AWB_BCOC[ c( names_AgTech, "Non.CO2" ) ], L261.AWB[ c( names_AgTech, "Non.CO2" ) ] )
L261.nonghg_max_reduction$year <- ctrl_base_year
L261.nonghg_max_reduction$ctrl.name <- "GDP_control"
L261.nonghg_max_reduction$max.reduction <- L261.max_reduction$value[ match( vecpaste( L261.nonghg_max_reduction[ c( names_AgTech, "Non.CO2" ) ]), vecpaste( L261.max_reduction[ c( names_AgTech, "variable" ) ]) )]
L261.nonghg_max_reduction <- na.omit( L261.nonghg_max_reduction )
L261.nonghg_max_reduction <- L261.nonghg_max_reduction[ c( names_AgTechYr, "Non.CO2", "ctrl.name", "max.reduction" )]

printlog( "L261.nonghg_steepness: steepness of reduction for energy technologies in all regions" )
L261.steepness <- melt( A11.steepness, id.vars=c( "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" ))
L261.steepness <- repeat_and_add_vector( L261.steepness, "region", GCAM_region_names$region )
L261.steepness <- repeat_and_add_vector( L261.steepness, "irr", c( "IRR", "RFD" ))
L261.steepness$AgProductionTechnology <- paste( L261.steepness$AgProductionTechnology, L261.steepness$irr, sep="")
L261.steepness <- L261.steepness[ names( L261.steepness ) != "irr" ]
L261.nonghg_steepness <- L261.nonghg_max_reduction[ c( names_AgTech, "Non.CO2" ) ]
L261.nonghg_steepness$year <- ctrl_base_year
L261.nonghg_steepness$ctrl.name <- "GDP_control"
L261.nonghg_steepness$steepness <- L261.steepness$value[ match( vecpaste( L261.nonghg_steepness[ c( names_AgTech, "Non.CO2" ) ]), vecpaste( L261.steepness[ c( names_AgTech, "variable" ) ]) )]
L261.nonghg_steepness <- na.omit( L261.nonghg_steepness )
L261.nonghg_steepness <- L261.nonghg_steepness[ c( names_AgTechYr, "Non.CO2", "ctrl.name", "steepness" )]

printlog( "Rename bio-energy crops in some region/AEZs" )
L261.AGRBio <- rename_biocrops( L261.AGRBio, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
  lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
L261.AWB_BCOC_Emissions <- rename_biocrops( L261.AWB_BCOC_Emissions, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
  lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
L261.nonghg_max_reduction <- rename_biocrops( L261.nonghg_max_reduction, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
  lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
L261.nonghg_steepness <- rename_biocrops( L261.nonghg_steepness, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
  lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )


printlog( "Removing non-existent regions and AEZs from all tables")
L261.AnEmissions <- subset( L261.AnEmissions, !region %in% no_aglu_regions )
L261.AnNH3Emissions <- subset( L261.AnNH3Emissions, !region %in% no_aglu_regions )
L261.AWBEmissions <- remove_AEZ_nonexist( L261.AWBEmissions )
L261.AGREmissions <- remove_AEZ_nonexist( L261.AGREmissions )
L261.AGRBio <- remove_AEZ_nonexist( L261.AGRBio )
L261.AWB_BCOC_Emissions <- remove_AEZ_nonexist( L261.AWB_BCOC_Emissions )
L261.nonghg_max_reduction <- remove_AEZ_nonexist( L261.nonghg_max_reduction )
L261.nonghg_steepness <- remove_AEZ_nonexist( L261.nonghg_steepness )

printlog( "Rename to regional SO2" )
L261.AWBEmissions <- rename_SO2( L261.AWBEmissions, A_regions, TRUE )
L261.nonghg_max_reduction <- rename_SO2( L261.nonghg_max_reduction, A_regions, TRUE )
L261.nonghg_steepness <- rename_SO2( L261.nonghg_steepness, A_regions, TRUE )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L261.AWBEmissions, "OutputEmissionsAg", "EMISSIONS_LEVEL2_DATA", "L261.awb_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L261.AGREmissions, "OutputEmissionsAg", "EMISSIONS_LEVEL2_DATA", "L261.agr_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L261.AnEmissions, "StbTechOutputEmissions", "EMISSIONS_LEVEL2_DATA", "L261.an_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L261.AnNH3Emissions, "StbTechOutputEmissions", "EMISSIONS_LEVEL2_DATA", "L261.an_nh3_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L261.AGRBio, "OutputEmissCoeffAg", "EMISSIONS_LEVEL2_DATA", "L261.bio_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L261.AWB_BCOC_Emissions, "OutputEmissCoeffAg", "EMISSIONS_LEVEL2_DATA", "L261.awb_bcoc_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L261.nonghg_max_reduction, "AgGDPCtrlMax", "EMISSIONS_LEVEL2_DATA", "L261.nonco2_max_reduction", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L261.nonghg_steepness, "AgGDPCtrlSteep", "EMISSIONS_LEVEL2_DATA", "L261.nonco2_steepness", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 

logstop()
