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
logstart( "L2112.ag_nonco2_IRR_MGMT.R" )
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
L2111.AWBEmissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L2111.AWBEmissions", skip = 4 )
L2111.AGREmissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L2111.AGREmissions", skip = 4 )
L2111.AGRBio <- readdata( "EMISSIONS_LEVEL2_DATA", "L2111.AGRBio", skip = 4 )
L2111.AWB_BCOC_EmissCoeff <- readdata( "EMISSIONS_LEVEL2_DATA", "L2111.AWB_BCOC_EmissCoeff", skip = 4 )
L2111.nonghg_max_reduction <- readdata( "EMISSIONS_LEVEL2_DATA", "L2111.nonghg_max_reduction", skip = 4 )
L2111.nonghg_steepness <- readdata( "EMISSIONS_LEVEL2_DATA", "L2111.nonghg_steepness", skip = 4 )
L2012.AgProduction_ag_irr_mgmt <- readdata( "AGLU_LEVEL2_DATA", "L2012.AgProduction_ag_irr_mgmt", skip = 4 )
L2111.AnEmissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L2111.AnEmissions", skip = 4 )
L2111.AnNH3Emissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L2111.AnNH3Emissions", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#Pass through the animal tables
L2112.AnEmissions <- L2111.AnEmissions
L2112.AnNH3Emissions <- L2111.AnNH3Emissions

L2112.tables_nochanges <- list( L2111.AGRBio, L2111.AWB_BCOC_EmissCoeff, L2111.nonghg_max_reduction, L2111.nonghg_steepness )
names( L2112.tables_nochanges ) <- c( "L2112.AGRBio", "L2112.AWB_BCOC_EmissCoeff", "L2112.nonghg_max_reduction", "L2112.nonghg_steepness" )
L2112.tables_nochanges_IDs <- c( "OutputEmissCoeffAg", "OutputEmissCoeffAg", "AgGDPCtrlMax", "AgGDPCtrlSteep" )
for( i in 1:length( L2112.tables_nochanges ) ){
	L2112.tables_nochanges[[i]] <- repeat_and_add_vector( L2112.tables_nochanges[[i]], lvl, c( "lo", "hi" ) )
	L2112.tables_nochanges[[i]][[agtech]] <- paste( L2112.tables_nochanges[[i]][[agtech]], L2112.tables_nochanges[[i]][[lvl]], sep = mgmt_delimiter )
	L2112.tables_nochanges[[i]][[lvl]] <- NULL
}

#For the tables whose emissions are read as quantities rather than rates, disaggregate emissions on the basis of production
L2112.AgProduction_ag_irr_nomgmt <- subset( L2012.AgProduction_ag_irr_mgmt, year == max( model_base_years ) )
L2112.AgProduction_ag_irr_nomgmt[[agtech]] <- substr(
  L2112.AgProduction_ag_irr_nomgmt[[agtech]],
  1, nchar( L2112.AgProduction_ag_irr_nomgmt[[agtech]]) - 3 )
L2112.AgProduction_ag_irr_nomgmt <- aggregate( L2112.AgProduction_ag_irr_nomgmt[ "calOutputValue" ],
      by = L2112.AgProduction_ag_irr_nomgmt[ c( reg, agsupp, agsubs, agtech, Y ) ], sum )
L2112.AgProduction_ag <- L2012.AgProduction_ag_irr_mgmt[ L2012.AgProduction_ag_irr_mgmt[[Y]] == max( model_base_years ),
                                                         c( reg, agsupp, agsubs, agtech, Y, "calOutputValue" ) ]
L2112.AgProduction_ag$match_tech <- substr( L2112.AgProduction_ag[[agtech]], 1, nchar( L2112.AgProduction_ag[[agtech]] ) - 3 )
L2112.AgProduction_ag$total <- L2112.AgProduction_ag_irr_nomgmt$calOutputValue[
      match( vecpaste( L2112.AgProduction_ag[ c( reg, agsupp, agsubs, "match_tech" ) ] ),
             vecpaste( L2112.AgProduction_ag_irr_nomgmt[ c( reg, agsupp, agsubs, agtech ) ] ) ) ]
L2112.AgProduction_ag$share_tech <- with( L2112.AgProduction_ag, calOutputValue / total )

#These shares can now be matched in to the emissions quantities, and multiplied through
L2112.awb_agr_emissions <- repeat_and_add_vector( rbind( L2111.AWBEmissions, L2111.AGREmissions ), lvl, c( "lo", "hi" ) )
L2112.awb_agr_emissions[[agtech]] <- paste( L2112.awb_agr_emissions[[agtech]], L2112.awb_agr_emissions[[lvl]], sep = mgmt_delimiter )
L2112.awb_agr_emissions$share_tech <- L2112.AgProduction_ag$share_tech[
      match( vecpaste( L2112.awb_agr_emissions[ c( reg, agsupp, agtech ) ] ),
             vecpaste( L2112.AgProduction_ag[ c( reg, agsupp, agtech ) ] ) ) ]

#Where shares allocated to lo/hi are NA but emissions are positive, split it 50/50 between the techs. For all others, set share to zero
L2112.awb_agr_emissions$share_tech[ is.na( L2112.awb_agr_emissions$share_tech ) & L2112.awb_agr_emissions$input.emissions > 1e-6 ] <- 0.5
L2112.awb_agr_emissions$share_tech[ is.na( L2112.awb_agr_emissions$share_tech ) ] <- 0
L2112.awb_agr_emissions$input.emissions <- with( L2112.awb_agr_emissions, input.emissions * share_tech )
L2112.awb_agr_emissions$share_tech <- NULL
L2112.AWBEmissions <- subset( L2112.awb_agr_emissions, grepl( "AWB", Non.CO2 ) )
L2112.AGREmissions <- subset( L2112.awb_agr_emissions, !grepl( "AWB", Non.CO2 ) )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( i in 1:length( L2112.tables_nochanges ) ){
	objectname <- names( L2112.tables_nochanges[i] )
	object <- L2112.tables_nochanges[[i]]
	assign( objectname, object )
	write_mi_data( object, L2112.tables_nochanges_IDs[i], "EMISSIONS_LEVEL2_DATA", objectname, "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR_MGMT.xml" )
}
write_mi_data( L2112.AnEmissions, "OutputEmissions", "EMISSIONS_LEVEL2_DATA", "L2112.an_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR_MGMT.xml" ) 
write_mi_data( L2112.AnNH3Emissions, "OutputEmissions", "EMISSIONS_LEVEL2_DATA", "L2112.an_nh3_emissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR_MGMT.xml" ) 

write_mi_data( L2112.AWBEmissions, "OutputEmissionsAg", "EMISSIONS_LEVEL2_DATA", "L2112.AWBEmissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR_MGMT.xml" ) 
write_mi_data( L2112.AGREmissions, "OutputEmissionsAg", "EMISSIONS_LEVEL2_DATA", "L2112.AGREmissions", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR_MGMT.xml" ) 

logstop()
