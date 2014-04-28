# L250_RGCAM_dom_water.R_
# Generates state level domestic water use input file
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L250_RGCAM_dom_water.R" )
printlog( "RGCAM domestic water use input file" )

# -----------------------------------------------------------------------------
# 1. Read files

A_dom_Delete <- readdata( "A_dom_Delete" )
A_dom_sector <- readdata( "A_dom_sector" )
A_dom_subs <- readdata( "A_dom_subs" )
A_dom_subs_interp <- readdata( "A_dom_subs_interp" )
A_dom_tech_input <- readdata( "A_dom_tech_input" )
A_dom_finaldemand <- readdata( "A_dom_finaldemand" )

USGS_dom_water <- readdata( "USGS_dom_water" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files
L250_subregions <- unique( USGS_dom_water$region )

printlog( "L250_Delete: DELETE EXISTING US DOM SECTOR" )
L250_DeleteSector <- A_dom_Delete[ A_dom_Delete$type == "supplysector", c( "region", "sector" ) ]
L250_DeleteFD <- A_dom_Delete[ A_dom_Delete$type == "energy-final-demand", c( "region", "sector" ) ]

printlog( "L250_Sector: SECTOR LEVEL PARAMS" )
# The sector table will include units strings as well as the logit exponent by region
L250_Sector <- data.frame( A_dom_sector, keyword="domestic" )
L250_Sector <- L250_Sector[ sort( rep( 1:nrow( L250_Sector ), times = length( L250_subregions ) ) ), ]
L250_Sector <- data.frame( region=L250_subregions, L250_Sector )

# Create a table for subsector nest share-weights and logits
printlog( "L250_Subsector: SUBSECTOR LEVEL PARAMS" )
L250_Subsector <- A_dom_subs
L250_Subsector <- L250_Subsector[ sort( rep( 1:nrow( L250_Subsector ), times = length( L250_subregions ) ) ), ]
L250_Subsector <- data.frame( region=L250_subregions, L250_Subsector )
# The header expects the logit to be the last column
L250_Subsector$logit_exp <- L250_Subsector$logit
L250_Subsector$logit <- NULL

# Create a table for subsector interpolation rules
printlog( "L250_SubsInterpRule: SUBSECTOR LEVEL INTERP RULES" )
L250_SubsInterpRule <- data.frame( A_dom_subs_interp, apply_to="share-weight" )
L250_SubsInterpRule <- L250_SubsInterpRule[ sort( rep( 1:nrow( L250_SubsInterpRule ), times = length( L250_subregions ) ) ), ]
L250_SubsInterpRule <- data.frame( region=L250_subregions, L250_SubsInterpRule )

# Create a for technology parameters including costs, efficiencies, inputs, etc
printlog( "L250_GlobalDBTechInput: TECH INPUT" )
L250_GlobalDBTechInput <- A_dom_tech_input[, !grepl( "_with", names( A_dom_tech_input ) ) ]
# Reorganize and add columns to match header
L250_GlobalDBTechInput$lifetime <- -1 
L250_GlobalDBTechInput <- L250_GlobalDBTechInput[, c( "supplysector", "subsector", "technology", "year",
    "share_weight", "input_cost", "lifetime", "efficiency", "minicam_energy_input" ) ]
L250_GlobalDBTechInput$market <- " "
# To make sure the market column does not get removed
L250_GlobalDBTechInput$extra <- "Junk"

# Create stubs to pulll in techs from the global tech db
printlog( "L250_TechStubs: TECH STUBS" )
L250_TechStubs <- unique( L250_GlobalDBTechInput[, c( "supplysector", "subsector", "technology" ) ] )
L250_TechStubs <- L250_TechStubs[ sort( rep( 1:nrow( L250_TechStubs), times = length( L250_subregions ) ) ), ]
L250_TechStubs <- data.frame( region=L250_subregions, L250_TechStubs )

# Create final demand input tables
printlog( "L250_FDBaseService" )
L250_FDBaseService <- melt( USGS_dom_water, c( "region" ), variable_name="year" )
L250_FDBaseService$year <- sub( 'X', '', L250_FDBaseService$year )
L250_FDBaseService$energy_final_demand <- A_dom_finaldemand$energy_final_demand
L250_FDBaseService <- L250_FDBaseService[, c( "region", "energy_final_demand", "year", "value" ) ]
printlog( "L250_FDElasticities" )
L250_FDElasticities <- A_dom_finaldemand[ sort( rep( 1:nrow( A_dom_finaldemand ), times=length( GCAM_future_years ) ) ),
    names( A_dom_finaldemand ) %!in% c( "perCapitaBased" ) ]
L250_FDElasticities$price_year <- GCAM_future_years
L250_FDElasticities$income_year <- GCAM_future_years
L250_FDElasticities$aeei_year <- GCAM_future_years
L250_FDElasticities <- L250_FDElasticities[ sort( rep( 1:nrow( L250_FDElasticities ), times = length( L250_subregions ) ) ), ]
L250_FDElasticities <- data.frame( region=L250_subregions, L250_FDElasticities )
L250_FDElasticities <- L250_FDElasticities[, c( "region", "energy_final_demand", "price_year", "price_elast",
    "income_year", "income_elast", "aeei_year", "aeei" ) ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L250_DeleteSector, "SectorDelete", "L250_DeleteSector", "batch_rgcam_dom_water_input.xml" )
write_mi_data( L250_DeleteFD, "FinalDemandDelete", "L250_DeleteFD", "batch_rgcam_dom_water_input.xml" )
write_mi_data( L250_Sector, "SectorKeyword", "L250_Sector", "batch_rgcam_dom_water_input.xml" )
write_mi_data( L250_Subsector, "ElecSubsector", "L250_Subsector", "batch_rgcam_dom_water_input.xml" )
write_mi_data( L250_SubsInterpRule, "ElecSubsInterpRule", "L250_SubsInterpRule", "batch_rgcam_dom_water_input.xml" )
write_mi_data( L250_GlobalDBTechInput, "GeothermalInput", "L250_GlobalDBTechInput", "batch_rgcam_dom_water_input.xml" )
write_mi_data( L250_TechStubs, "TechStubs", "L250_TechStubs", "batch_rgcam_dom_water_input.xml" )
write_mi_data( L250_FDBaseService, "FDBaseSerivce", "L250_FDBaseService", "batch_rgcam_dom_water_input.xml" )
write_mi_data( L250_FDElasticities, "FDElasticitiesAEEI", "L250_FDElasticities", "batch_rgcam_dom_water_input.xml" )

insert_file_into_batchxml( "batch_rgcam_dom_water_input.xml", "rgcam_dom_water_input.xml", "", xml_tag="outFile" )

logstop()
