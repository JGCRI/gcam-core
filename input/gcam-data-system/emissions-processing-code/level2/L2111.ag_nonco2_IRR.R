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
logstart( "L2111.ag_nonco2_IRR.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions with irrigated and rainfed production separated" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_regions <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
L1211.nonco2_tg_R_awb_C_Y_GLU_IRR <- readdata( "EMISSIONS_LEVEL1_DATA", "L1211.nonco2_tg_R_awb_C_Y_GLU_IRR" )
L1221.ghg_tg_R_agr_C_Y_GLU_IRR <- readdata( "EMISSIONS_LEVEL1_DATA", "L1221.ghg_tg_R_agr_C_Y_GLU_IRR" )
L211.AnEmissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L211.AnEmissions", skip = 4 )
L211.AnNH3Emissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L211.AnNH3Emissions", skip = 4 )
L211.AGRBio <- readdata( "EMISSIONS_LEVEL2_DATA", "L211.AGRBio", skip = 4 )
L211.AWB_BCOC_EmissCoeff <- readdata( "EMISSIONS_LEVEL2_DATA", "L211.AWB_BCOC_EmissCoeff", skip = 4 )
L211.nonghg_max_reduction <- readdata( "EMISSIONS_LEVEL2_DATA", "L211.nonghg_max_reduction", skip = 4 )
L211.nonghg_steepness <- readdata( "EMISSIONS_LEVEL2_DATA", "L211.nonghg_steepness", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "L2111.AWBEmissions: AWB emissions in all regions" )
#Interpolate and add region name
#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L2111.AWB <- melt( L1211.nonco2_tg_R_awb_C_Y_GLU_IRR, id.vars = c( R_C_GLU_irr, "Non.CO2" ), measure.vars = X_emiss_model_base_years,
                  variable.name = "xyear", value.name = "input.emissions" )
L2111.AWB$year <- as.numeric( substr( L2111.AWB$xyear, 2, 5 ) )

#Add region, supplysector, subsector and tech names
L2111.AWB <- add_region_name( L2111.AWB )
L2111.AWB$AgSupplySector <- L2111.AWB$GCAM_commodity
L2111.AWB$AgSupplySubsector <- paste( L2111.AWB$GCAM_commodity, L2111.AWB$GLU, sep=crop_GLU_delimiter )
L2111.AWB$AgProductionTechnology <- paste( L2111.AWB$AgSupplySubsector, L2111.AWB[[irr]], sep = irr_delimiter )

#Format for csv file
L2111.AWBEmissions <- L2111.AWB[ c( names_AgTechYr, "Non.CO2", "input.emissions" ) ]
L2111.AWBEmissions$input.emissions <- round( L2111.AWBEmissions$input.emissions, digits_emissions )

printlog( "Rename to regional SO2" )
L2111.AWBEmissions <- rename_SO2( L2111.AWBEmissions, A_regions, TRUE )

printlog( "L2111.AGREmissions: ag AGR emissions in all regions" )
L2111.AGR <- melt( L1221.ghg_tg_R_agr_C_Y_GLU_IRR, id.vars = c( R_C_GLU_irr, "Non.CO2" ), measure.vars = X_emiss_model_base_years,
                  variable.name = "xyear", value.name = "input.emissions" )
L2111.AGR$year <- as.numeric( substr( L2111.AGR$xyear, 2, 5 ) )

#Add region, supplysector, subsector and tech names
L2111.AGR <- add_region_name( L2111.AGR )
L2111.AGR$AgSupplySector <- L2111.AGR$GCAM_commodity
L2111.AGR$AgSupplySubsector <- paste( L2111.AGR$GCAM_commodity, L2111.AGR$GLU, sep=crop_GLU_delimiter )
L2111.AGR$AgProductionTechnology <- paste( L2111.AGR$AgSupplySubsector, L2111.AGR[[irr]], sep = irr_delimiter )

#Format for csv file
L2111.AGREmissions <- L2111.AGR[ c( names_AgTechYr, "Non.CO2", "input.emissions" ) ]
L2111.AGREmissions$input.emissions <- round( L2111.AGREmissions$input.emissions, digits_emissions )

printlog( "L2111.AnEmissions and L2111.AnNH3Emissions: Copy tables exactly (irr/rfd irrelevant) ")
L2111.AnEmissions <- L211.AnEmissions
L2111.AnNH3Emissions <- L211.AnNH3Emissions

printlog( "L2111.AGRBio, L2111.AWB_BCOC_EmissCoeff, L2111.nonghg_max_reduction, L2111.nonghg_steepness: repeat by irr/rfd and copy" )
L2111.AGRBio <- repeat_and_add_vector( L211.AGRBio, irr, c( "IRR", "RFD" ) )
L2111.AGRBio[[agtech]] <- paste( L2111.AGRBio[[agsubs]], L2111.AGRBio[[irr]], sep = irr_delimiter )
L2111.AGRBio[[irr]] <- NULL

L2111.AWB_BCOC_EmissCoeff <- repeat_and_add_vector( L211.AWB_BCOC_EmissCoeff, irr, c( "IRR", "RFD" ) )
L2111.AWB_BCOC_EmissCoeff[[agtech]] <- paste( L2111.AWB_BCOC_EmissCoeff[[agsubs]], L2111.AWB_BCOC_EmissCoeff[[irr]], sep = irr_delimiter )
L2111.AWB_BCOC_EmissCoeff[[irr]] <- NULL

L2111.nonghg_max_reduction <- repeat_and_add_vector( L211.nonghg_max_reduction, irr, c( "IRR", "RFD" ) )
L2111.nonghg_max_reduction[[agtech]] <- paste( L2111.nonghg_max_reduction[[agsubs]], L2111.nonghg_max_reduction[[irr]], sep = irr_delimiter )
L2111.nonghg_max_reduction[[irr]] <- NULL

L2111.nonghg_steepness <- repeat_and_add_vector( L211.nonghg_steepness, irr, c( "IRR", "RFD" ) )
L2111.nonghg_steepness[[agtech]] <- paste( L2111.nonghg_steepness[[agsubs]], L2111.nonghg_steepness[[irr]], sep = irr_delimiter )
L2111.nonghg_steepness[[irr]] <- NULL

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2111.AWBEmissions, "OutputEmissionsAg", "EMISSIONS_LEVEL2_DATA", "L2111.AWBEmissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L2111.AGREmissions, "OutputEmissionsAg", "EMISSIONS_LEVEL2_DATA", "L2111.AGREmissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L2111.AnEmissions, "StbTechOutputEmissions", "EMISSIONS_LEVEL2_DATA", "L2111.AnEmissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L2111.AnNH3Emissions, "StbTechOutputEmissions", "EMISSIONS_LEVEL2_DATA", "L2111.AnNH3Emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L2111.AGRBio, "OutputEmissCoeffAg", "EMISSIONS_LEVEL2_DATA", "L2111.AGRBio", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L2111.AWB_BCOC_EmissCoeff, "OutputEmissCoeffAg", "EMISSIONS_LEVEL2_DATA", "L2111.AWB_BCOC_EmissCoeff", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L2111.nonghg_max_reduction, "AgGDPCtrlMax", "EMISSIONS_LEVEL2_DATA", "L2111.nonghg_max_reduction", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L2111.nonghg_steepness, "AgGDPCtrlSteep", "EMISSIONS_LEVEL2_DATA", "L2111.nonghg_steepness", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 

logstop()
