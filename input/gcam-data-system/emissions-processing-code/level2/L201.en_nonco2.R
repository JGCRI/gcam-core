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
printlog( "NonCO2 emissions in the energy system" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_regions <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
A_regions.en <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
L111.nonghg_tg_R_en_S_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L111.nonghg_tg_R_en_S_F_Yh" )
L111.nonghg_tgej_R_en_S_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L111.nonghg_tgej_R_en_S_F_Yh" )
L112.ghg_tg_R_en_S_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L112.ghg_tg_R_en_S_F_Yh" )
L112.ghg_tgej_R_en_S_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L112.ghg_tgej_R_en_S_F_Yh" )
L114.bcoc_tgej_R_en_S_F_2000 <- readdata( "EMISSIONS_LEVEL1_DATA", "L114.bcoc_tgej_R_en_S_F_2000" )
L151.nonghg_ctrl_R_en_S_T <- readdata( "EMISSIONS_LEVEL1_DATA", "L151.nonghg_ctrl_R_en_S_T" )
A51.steepness <- readdata( "EMISSIONS_ASSUMPTIONS", "A51.steepness" )
EN.L244.DeleteThermalService <- readdata( "ENERGY_LEVEL2_DATA", "L244.DeleteThermalService", must.exist=FALSE, skip=4 )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "L201.en_pol_emissions: Pollutant emissions for energy technologies in all regions" )
#Interpolate and add region name
L201.nonghg <- subset( L111.nonghg_tg_R_en_S_F_Yh, L111.nonghg_tg_R_en_S_F_Yh$supplysector != "out_resources" )
L201.nonghg <- interpolate_and_melt( L201.nonghg, emiss_model_base_years )
L201.nonghg <- add_region_name( L201.nonghg )

#Format for csv file
L201.en_pol_emissions <- L201.nonghg[ c( names_StubTechYr, "Non.CO2" ) ]
L201.en_pol_emissions$input.emissions <- round( L201.nonghg$value, digits_emissions )

printlog( "L201.en_ghg_emissions: GHG emissions for energy technologies in all regions" )
#Interpolate and add region name
L201.GHG <- subset( L112.ghg_tg_R_en_S_F_Yh, L112.ghg_tg_R_en_S_F_Yh$supplysector != "out_resources" )
L201.GHG <- interpolate_and_melt( L201.GHG, emiss_model_base_years )
L201.GHG <- add_region_name( L201.GHG )

#Format for csv file
L201.en_ghg_emissions <- L201.GHG[ c( names_StubTechYr, "Non.CO2" ) ]
L201.en_ghg_emissions$input.emissions <- round( L201.GHG$value, digits_emissions )

printlog( "L201.en_bcoc_emissions: BC/OC emissions factors for energy technologies in all regions" )
#Interpolate and add region name
L201.BCOC <- subset( L114.bcoc_tgej_R_en_S_F_2000, L114.bcoc_tgej_R_en_S_F_2000$supplysector != "out_resources" )
L201.BCOC <- add_region_name( L201.BCOC )
L201.BCOC <- repeat_and_add_vector( L201.BCOC, "year", model_base_years )

#Format for csv file
L201.en_bcoc_emissions <- L201.BCOC[ c( names_StubTechYr, "Non.CO2" ) ]
L201.en_bcoc_emissions$emiss.coef <- round( L201.BCOC$X2000, digits_emissions )

printlog( "L201.nonghg_max_reduction: maximum reduction for energy technologies in all regions" )
L201.max_reduction <- subset( L151.nonghg_ctrl_R_en_S_T, L151.nonghg_ctrl_R_en_S_T$supplysector != "out_resources" )
L201.max_reduction <- add_region_name( L201.max_reduction )

L201.nonghg_max_reduction <- L201.max_reduction[ c( names_StubTech, "Non.CO2" ) ]
L201.nonghg_max_reduction$year <- ctrl_base_year
L201.nonghg_max_reduction$ctrl.name <- "GDP_control"
L201.nonghg_max_reduction$max.reduction <- L201.max_reduction$max_reduction[ match( vecpaste( L201.nonghg_max_reduction[ c( names_StubTech, "Non.CO2" ) ]), vecpaste( L201.max_reduction[ c( names_StubTech, "Non.CO2" ) ]) )]
L201.nonghg_max_reduction <- na.omit( L201.nonghg_max_reduction )
L201.nonghg_max_reduction <- L201.nonghg_max_reduction[ c( names_StubTechYr, "Non.CO2", "ctrl.name", "max.reduction" )]

printlog( "L201.nonghg_steepness: steepness of reduction for energy technologies in all regions" )
L201.steepness <- melt( A51.steepness, id.vars=c( "supplysector", "subsector", "stub.technology" ))
L201.steepness <- subset( L201.steepness, L201.steepness$supplysector != "out_resources" )
L201.steepness <- repeat_and_add_vector( L201.steepness, "region", GCAM_region_names$region )

L201.nonghg_steepness <- L201.nonghg_max_reduction[ c( names_StubTech, "Non.CO2" ) ]
L201.nonghg_steepness$year <- ctrl_base_year
L201.nonghg_steepness$ctrl.name <- "GDP_control"
L201.nonghg_steepness$steepness <- L201.steepness$value[ match( vecpaste( L201.nonghg_steepness[ c( names_StubTech, "Non.CO2" ) ]), vecpaste( L201.steepness[ c( names_StubTech, "variable" ) ]) )]
L201.nonghg_steepness <- na.omit( L201.nonghg_steepness )
L201.nonghg_steepness <- L201.nonghg_steepness[ c( names_StubTechYr, "Non.CO2", "ctrl.name", "steepness" )]

# Remove rows where we only have a value for one of max.reduction or steepness
# TODO: is this what we want or should we raise an error?
L201.nonghg_gdp_control <- merge( L201.nonghg_max_reduction, L201.nonghg_steepness, all=TRUE )
L201.nonghg_gdp_control <- na.omit( L201.nonghg_gdp_control )
# No need to include a GDP control when the max.reduction is zero
L201.nonghg_gdp_control <- L201.nonghg_gdp_control[ L201.nonghg_gdp_control$max.reduction > 0, ]

L201.nonghg_max_reduction <- L201.nonghg_gdp_control[, names( L201.nonghg_gdp_control ) != "steepness" ]
L201.nonghg_steepness <- L201.nonghg_gdp_control[, names( L201.nonghg_gdp_control ) != "max.reduction" ]

printlog( "L201.nonghg_res: Pollutant emissions for energy resources in all regions" )
#Interpolate and add region name
L201.nonghg_coef <- subset( L111.nonghg_tgej_R_en_S_F_Yh, L111.nonghg_tgej_R_en_S_F_Yh$supplysector == "out_resources" )
L201.nonghg_coef <- interpolate_and_melt( L201.nonghg_coef, emiss_model_base_years )
L201.nonghg_coef <- subset( L201.nonghg_coef, L201.nonghg_coef$year == final_emiss_year )
L201.nonghg_coef <- add_region_name( L201.nonghg_coef )
names( L201.nonghg_coef )[ names( L201.nonghg_coef ) == "subsector" ] <- "depresource"

#Format for csv file
L201.nonghg_res <- L201.nonghg_coef[ c( "region", "depresource", "Non.CO2" ) ]
names( L201.nonghg_res )[ names( L201.nonghg_res ) == "subsector" ] <- "depresource"
L201.nonghg_res$emiss.coef <- round( L201.nonghg_coef$value, digits_emissions )

printlog( "L201.ghg_res: GHG emissions from resource production in all regions" )
#Interpolate and add region name
L201.GHG_coef <- subset( L112.ghg_tgej_R_en_S_F_Yh, L112.ghg_tgej_R_en_S_F_Yh$supplysector == "out_resources" )
L201.GHG_coef <- interpolate_and_melt( L201.GHG_coef, emiss_model_base_years )
L201.GHG_coef <- subset( L201.GHG_coef, L201.GHG_coef$year == final_emiss_year )
L201.GHG_coef <- add_region_name( L201.GHG_coef )
names( L201.GHG_coef )[ names( L201.GHG_coef ) == "subsector" ] <- "depresource"

#Format for csv file
L201.ghg_res <- L201.GHG_coef[ c( "region", "depresource", "Non.CO2" ) ]
names( L201.ghg_res )[ names( L201.ghg_res ) == "subsector" ] <- "depresource"
L201.ghg_res$emiss.coef <- round( L201.GHG_coef$value, digits_emissions )

printlog( "L201.nonghg_max_reduction_res: maximum reduction for resources in all regions" )
L201.max_reduction_res <- subset( L151.nonghg_ctrl_R_en_S_T, L151.nonghg_ctrl_R_en_S_T$supplysector == "out_resources" )
L201.max_reduction_res <- add_region_name( L201.max_reduction_res )
names( L201.max_reduction_res )[ names( L201.max_reduction_res ) == "subsector" ] <- "depresource"

L201.nonghg_max_reduction_res <- L201.max_reduction_res[ c( "region", "depresource", "Non.CO2" ) ]
L201.nonghg_max_reduction_res$ctrl.name <- "GDP_control"
L201.nonghg_max_reduction_res$max.reduction <- L201.max_reduction_res$max_reduction[ match( vecpaste( L201.nonghg_max_reduction_res[ c( "region", "depresource", "Non.CO2" ) ]), 
                                                                                            vecpaste( L201.max_reduction_res[ c( "region", "depresource", "Non.CO2" ) ]) )]
L201.nonghg_max_reduction_res <- na.omit( L201.nonghg_max_reduction_res )
L201.nonghg_max_reduction_res <- L201.nonghg_max_reduction_res[ c( "region", "depresource", "Non.CO2", "ctrl.name", "max.reduction" )]

printlog( "L201.nonghg_steepness_res: steepness of reduction for resources in all regions" )
L201.steepness_res <- melt( A51.steepness, id.vars=c( "supplysector", "subsector", "stub.technology" ))
L201.steepness_res <- subset( L201.steepness_res, L201.steepness_res$supplysector == "out_resources" )
L201.steepness_res <- repeat_and_add_vector( L201.steepness_res, "region", GCAM_region_names$region )
names( L201.steepness_res )[ names( L201.steepness_res ) == "subsector" ] <- "depresource"
names( L201.steepness_res )[ names( L201.steepness_res ) == "variable" ] <- "Non.CO2"

L201.nonghg_steepness_res <- L201.steepness_res[ c( "region", "depresource", "Non.CO2" ) ]
L201.nonghg_steepness_res$ctrl.name <- "GDP_control"
L201.nonghg_steepness_res$steepness <- L201.steepness_res$value[ match( vecpaste( L201.nonghg_steepness_res[ c( "region", "depresource", "Non.CO2" ) ]), 
                                                                        vecpaste( L201.steepness_res[ c( "region", "depresource", "Non.CO2" ) ]) )]
L201.nonghg_steepness_res <- na.omit( L201.nonghg_steepness_res )
L201.nonghg_steepness_res <- L201.nonghg_steepness_res[ c( "region", "depresource", "Non.CO2", "ctrl.name", "steepness" )]

# Remove rows where we only have a value for one of max.reduction or steepness
# TODO: is this what we want or should we raise an error?
L201.nonghg_gdp_control_res <- merge( L201.nonghg_max_reduction_res, L201.nonghg_steepness_res, all=TRUE )
L201.nonghg_gdp_control_res <- na.omit( L201.nonghg_gdp_control_res )
# No need to include a GDP control when the max.reduction is zero
L201.nonghg_gdp_control_res <- L201.nonghg_gdp_control_res[ L201.nonghg_gdp_control_res$max.reduction > 0, ]

L201.nonghg_max_reduction_res <- L201.nonghg_gdp_control_res[, names( L201.nonghg_gdp_control_res ) != "steepness" ]
L201.nonghg_steepness_res <- L201.nonghg_gdp_control_res[, names( L201.nonghg_gdp_control_res ) != "max.reduction" ]

printlog( "Rename to regional SO2" )
L201.en_pol_emissions <- rename_SO2( L201.en_pol_emissions, A_regions, FALSE )
L201.nonghg_max_reduction <- rename_SO2( L201.nonghg_max_reduction, A_regions, FALSE )
L201.nonghg_steepness <- rename_SO2( L201.nonghg_steepness, A_regions, FALSE )
L201.nonghg_res <- rename_SO2( L201.nonghg_res, A_regions, FALSE )
L201.nonghg_steepness_res <- rename_SO2( L201.nonghg_steepness_res, A_regions, FALSE )
L201.nonghg_max_reduction_res <- rename_SO2( L201.nonghg_max_reduction_res, A_regions, FALSE )

printlog( "Remove district heat from regions that do have have it" )
L201.distheat.regions <- A_regions.en[ A_regions.en$heat == 1, "region" ]
L201.en_pol_emissions <- subset( L201.en_pol_emissions, supplysector != "district heat" | region %in% L201.distheat.regions )
L201.en_ghg_emissions <- subset( L201.en_ghg_emissions, supplysector != "district heat" | region %in% L201.distheat.regions )
L201.en_bcoc_emissions <- subset( L201.en_bcoc_emissions, supplysector != "district heat" | region %in% L201.distheat.regions )
L201.nonghg_max_reduction <- subset( L201.nonghg_max_reduction, supplysector != "district heat" | region %in% L201.distheat.regions )
L201.nonghg_steepness <- subset( L201.nonghg_steepness, supplysector != "district heat" | region %in% L201.distheat.regions )

# It may be the case with certain regional aggregations that regions exist that have no
# heating or cooling sectors.  We should delete those here.
if( !is.null( EN.L244.DeleteThermalService ) ) {
printlog( "Delete sectors that do not exist due to zero heating/cooling degree days" )
L201.delete.sectors <- paste0( EN.L244.DeleteThermalService$region, EN.L244.DeleteThermalService$supplysector )
L201.en_pol_emissions <- subset( L201.en_pol_emissions, paste0( region, supplysector ) %!in% L201.delete.sectors )
L201.en_ghg_emissions <- subset( L201.en_ghg_emissions, paste0( region, supplysector ) %!in% L201.delete.sectors )
L201.en_bcoc_emissions <- subset( L201.en_bcoc_emissions, paste0( region, supplysector ) %!in% L201.delete.sectors )
L201.nonghg_max_reduction <- subset( L201.nonghg_max_reduction, paste0( region, supplysector ) %!in% L201.delete.sectors )
L201.nonghg_steepness <- subset( L201.nonghg_steepness, paste0( region, supplysector ) %!in% L201.delete.sectors )
}


# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L201.en_pol_emissions, "InputEmissions", "EMISSIONS_LEVEL2_DATA", "L201.en_pol_emissions", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L201.en_ghg_emissions, "InputEmissions", "EMISSIONS_LEVEL2_DATA", "L201.en_ghg_emissions", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L201.en_bcoc_emissions, "InputEmissCoeff", "EMISSIONS_LEVEL2_DATA", "L201.en_bcoc_emissions", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L201.nonghg_max_reduction, "GDPCtrlMax", "EMISSIONS_LEVEL2_DATA", "L201.nonghg_max_reduction", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L201.nonghg_steepness, "GDPCtrlSteep", "EMISSIONS_LEVEL2_DATA", "L201.nonghg_steepness", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L201.nonghg_max_reduction_res, "GDPCtrlMaxRes", "EMISSIONS_LEVEL2_DATA", "L201.nonghg_max_reduction_res", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L201.nonghg_steepness_res, "GDPCtrlSteepRes", "EMISSIONS_LEVEL2_DATA", "L201.nonghg_steepness_res", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L201.nonghg_res, "ResEmissCoef", "EMISSIONS_LEVEL2_DATA", "L201.nonghg_res", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L201.ghg_res, "ResEmissCoef", "EMISSIONS_LEVEL2_DATA", "L201.ghg_res", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 

logstop()
