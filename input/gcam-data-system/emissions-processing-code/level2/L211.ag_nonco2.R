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
L103.ag_Prod_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_Prod_Mt_R_C_Y_GLU" )
L205.AgCost_bio <- readdata( "AGLU_LEVEL2_DATA", "L205.AgCost_bio", skip = 4)
L113.ghg_tg_R_an_C_Sys_Fd_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L113.ghg_tg_R_an_C_Sys_Fd_Yh" )
L115.nh3_tg_R_an_C_Sys_Fd_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L115.nh3_tg_R_an_C_Sys_Fd_Yh" )
L121.nonco2_tg_R_awb_C_Y_GLU <- readdata( "EMISSIONS_LEVEL1_DATA", "L121.nonco2_tg_R_awb_C_Y_GLU" )
L122.ghg_tg_R_agr_C_Y_GLU <- readdata( "EMISSIONS_LEVEL1_DATA", "L122.ghg_tg_R_agr_C_Y_GLU" )
L123.bcoc_tgmt_R_awb_2000 <- readdata( "EMISSIONS_LEVEL1_DATA", "L123.bcoc_tgmt_R_awb_2000" )
A11.max_reduction <- readdata( "EMISSIONS_ASSUMPTIONS", "A11.max_reduction" )
A11.steepness <- readdata( "EMISSIONS_ASSUMPTIONS", "A11.steepness" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#Sulfur emissions
printlog( "L211.AWBEmissions: AWB emissions in all regions" )
#Interpolate and add region name
#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L211.AWB <- melt( L121.nonco2_tg_R_awb_C_Y_GLU, id.vars = c( R_C_GLU, "Non.CO2" ), measure.vars = X_emiss_model_base_years,
                  variable.name = "xyear", value.name = "input.emissions" )
L211.AWB$year <- as.numeric( substr( L211.AWB$xyear, 2, 5 ) )

#Add region, supplysector, subsector and tech names
L211.AWB <- add_region_name( L211.AWB )
L211.AWB$AgSupplySector <- L211.AWB$GCAM_commodity
L211.AWB$AgSupplySubsector <- paste( L211.AWB$GCAM_commodity, L211.AWB$GLU, sep=crop_GLU_delimiter )
L211.AWB$AgProductionTechnology <- L211.AWB$AgSupplySubsector

#Format for csv file
L211.AWBEmissions <- L211.AWB[ c( names_AgTechYr, "Non.CO2", "input.emissions" ) ]
L211.AWBEmissions$input.emissions <- round( L211.AWBEmissions$input.emissions, digits_emissions )

printlog( "L211.AGREmissions: ag AGR emissions in all regions" )
L211.AGR <- melt( L122.ghg_tg_R_agr_C_Y_GLU, id.vars = c( R_C_GLU, "Non.CO2" ), measure.vars = X_emiss_model_base_years,
                  variable.name = "xyear", value.name = "input.emissions" )
L211.AGR$year <- as.numeric( substr( L211.AGR$xyear, 2, 5 ) )

#Add region, supplysector, subsector and tech names
L211.AGR <- add_region_name( L211.AGR )
L211.AGR$AgSupplySector <- L211.AGR$GCAM_commodity
L211.AGR$AgSupplySubsector <- paste( L211.AGR$GCAM_commodity, L211.AGR$GLU, sep=crop_GLU_delimiter )
L211.AGR$AgProductionTechnology <- L211.AGR$AgSupplySubsector

#Format for csv file
L211.AGREmissions <- L211.AGR[ c( names_AgTechYr, "Non.CO2", "input.emissions" ) ]
L211.AGREmissions$input.emissions <- round( L211.AGREmissions$input.emissions, digits_emissions )

printlog( "L211.AGRBioEmissions: bio AGR emissions in all regions" )
#Map in coefficients from assumption file
L211.AGRBio <- L205.AgCost_bio[ L205.AgCost_bio[[Y]] == ctrl_base_year, names_AgTechYr ]
L211.AGRBio$Non.CO2 <- "N2O_AGR"
L211.AGRBio$bio_N2O_coef <- A_regions$bio_N2O_coef[ match( L211.AGRBio$region, A_regions$region ) ]

printlog( "L211.AnAGREmissions: animal AGR emissions in all regions" )
#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L211.AN <- melt( L113.ghg_tg_R_an_C_Sys_Fd_Yh, id.vars = c( R, S_S_T, "Non.CO2" ), measure.vars = X_emiss_model_base_years,
                 variable.name = "xyear", value.name = "input.emissions" )
L211.AN$year <- as.numeric( substr( L211.AN$xyear, 2, 5 ) )

#Format for csv file
L211.AN <- add_region_name( L211.AN )
L211.AnEmissions <- L211.AN[ c( names_StubTechYr, "Non.CO2" ) ]
L211.AnEmissions$input.emissions <- round( L211.AN$input.emissions, digits_emissions )

printlog( "L211.AnNH3Emissions: animal NH3 emissions in all regions" )
#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L211.AN_NH3 <- melt( L115.nh3_tg_R_an_C_Sys_Fd_Yh, id.vars = c( R, S_S_T, "Non.CO2" ),
                     measure.vars = names( L115.nh3_tg_R_an_C_Sys_Fd_Yh )[ names( L115.nh3_tg_R_an_C_Sys_Fd_Yh ) %in% X_emiss_model_base_years ],
                     variable.name = "xyear", value.name = "input.emissions" )
L211.AN_NH3$year <- as.numeric( substr( L211.AN_NH3$xyear, 2, 5 ) )

#Format for csv file
L211.AN_NH3 <- add_region_name( L211.AN_NH3 )
L211.AnNH3Emissions <- L211.AN_NH3[ c( names_StubTechYr, "Non.CO2" ) ]
L211.AnNH3Emissions$input.emissions <- round( L211.AN_NH3$input.emissions, digits_emissions )

printlog( "L211.AWB_BCOC_EmissCoeff: BC / OC AWB emissions coefficients in all regions" )
#Add region name & replicate for all commodities & base years
L211.AWB_BCOC <- add_region_name( L123.bcoc_tgmt_R_awb_2000 )
L211.AWB_BCOC$AgSupplySector <- L211.AWB_BCOC[[C]]
L211.AWB_BCOC$AgSupplySubsector <- paste( L211.AWB_BCOC$AgSupplySector, L211.AWB_BCOC$GLU, sep = crop_GLU_delimiter )
L211.AWB_BCOC$AgProductionTechnology <- L211.AWB_BCOC$AgSupplySubsector
L211.AWB_BCOC <- repeat_and_add_vector( L211.AWB_BCOC, "year", model_base_years )

#Format for csv file
L211.AWB_BCOC_EmissCoeff <- L211.AWB_BCOC[ c( names_AgTechYr, "Non.CO2" ) ]
L211.AWB_BCOC_EmissCoeff$emiss.coef <- round( L211.AWB_BCOC$emfact, digits_emissions )

printlog( "L211.nonghg_max_reduction: maximum emissions coefficient reduction for ag technologies in all regions" )
L211.nonghg_max_reduction <- rbind( L211.AWB_BCOC[ L211.AWB_BCOC[[Y]] == ctrl_base_year, c( names_AgTechYr, "Non.CO2" ) ],
                                    L211.AWB[ L211.AWB[[Y]] == ctrl_base_year, c( names_AgTechYr, "Non.CO2" ) ] )
L211.nonghg_max_reduction$ctrl.name <- "GDP_control"
L211.nonghg_max_reduction$max.reduction <- A11.max_reduction$max.reduction[
  match( L211.nonghg_max_reduction$AgSupplySector, A11.max_reduction$AgSupplySector ) ]

printlog( "L211.nonghg_steepness: steepness of reduction for energy technologies in all regions" )
L211.nonghg_steepness <- L211.nonghg_max_reduction[ c( names_AgTechYr, "Non.CO2" ) ]
L211.nonghg_steepness$ctrl.name <- "GDP_control"
L211.nonghg_steepness$steepness <- A11.steepness$steepness[
  match( L211.nonghg_steepness$AgSupplySector, A11.steepness$AgSupplySector ) ]

L211.AnEmissions <- subset( L211.AnEmissions, !region %in% no_aglu_regions )
L211.AnNH3Emissions <- subset( L211.AnNH3Emissions, !region %in% no_aglu_regions )

printlog( "Rename to regional SO2" )
L211.AWBEmissions <- rename_SO2( L211.AWBEmissions, A_regions, TRUE )
L211.nonghg_max_reduction <- rename_SO2( L211.nonghg_max_reduction, A_regions, TRUE )
L211.nonghg_steepness <- rename_SO2( L211.nonghg_steepness, A_regions, TRUE )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L211.AWBEmissions, "OutputEmissionsAg", "EMISSIONS_LEVEL2_DATA", "L211.AWBEmissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.AGREmissions, "OutputEmissionsAg", "EMISSIONS_LEVEL2_DATA", "L211.AGREmissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.AnEmissions, "StbTechOutputEmissions", "EMISSIONS_LEVEL2_DATA", "L211.AnEmissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.AnNH3Emissions, "StbTechOutputEmissions", "EMISSIONS_LEVEL2_DATA", "L211.AnNH3Emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.AGRBio, "OutputEmissCoeffAg", "EMISSIONS_LEVEL2_DATA", "L211.AGRBio", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.AWB_BCOC_EmissCoeff, "OutputEmissCoeffAg", "EMISSIONS_LEVEL2_DATA", "L211.AWB_BCOC_EmissCoeff", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.nonghg_max_reduction, "AgGDPCtrlMax", "EMISSIONS_LEVEL2_DATA", "L211.nonghg_max_reduction", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L211.nonghg_steepness, "AgGDPCtrlSteep", "EMISSIONS_LEVEL2_DATA", "L211.nonghg_steepness", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 

logstop()
