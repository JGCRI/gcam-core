# L241_RGCAM_rooftop_PV.R
# Generates state level rooftop PV resources and technologies
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L242_RGCAM_ccs_supply.R" )
printlog( "RGCAM ccs supply input file" )

# -----------------------------------------------------------------------------
# 1. Read files

states_subregions <- readdata( "states_subregions" )

nerc_ccs_supply_curves <- readdata( "nerc_ccs_supply_curves" )

A_ccs_delete <- readdata( "A_ccs_delete" )
A_ccs_names <- readdata( "A_ccs_names" )

A_ccs_sector <- readdata( "A_ccs_sector" )
A_ccs_subsector <- readdata( "A_ccs_subsector" )
A_ccs_subs_interp <- readdata( "A_ccs_subs_interp" )
A_ccs_tech_input <- readdata( "A_ccs_tech_input" )

# For CCS tech names
A_elecS_tech_CCS <- readdata( "A_elecS_tech_CCS" )
A_refliq_tech_CCS <-readdata( "A_refliq_tech_CCS" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files

L242_nerc_regions <- unique( nerc_ccs_supply_curves$region )

# First create the resource supply curves for each region.
printlog( "L242_DeleteResource: DELETE US LEVEL RESOURCES" )
L242_DeleteResource <- subset( A_ccs_delete, type == "resource", select= c( "region", "name" ) )

# Create table for the resource level parameters
printlog( "L242_Resource: RESOURCE CHARACTORISTICS" )
L242_Resource <- A_ccs_names
L242_Resource <- data.frame( region=L242_nerc_regions, L242_Resource[ rep( 1:nrow( L242_Resource ), times=
    nrow( L242_Resource ) ), ], market=L242_nerc_regions )
#L241_PVResource <- data.frame( L241_PVResource, market=L241_PVResource$region,
    #pri_fuel_name=L241_PVResource$resource, PrimaryFuelCO2Coef=0 )

printlog( "L242_Grades: CCS graded supply curve" )
L242_Grades <- merge( L242_Resource[, c( "region", "resource" ) ], nerc_ccs_supply_curves )
L242_Grades$subresource <- L242_Grades$resource
# The supply curve costs are provided in 2005$/tCO2, the model uses 1990$/tC
L242_Grades$cost <- L242_Grades$cost * conv_2005_1990_USD / conv_CO2_C
# The supply curve storage capacity is provided a MTCO2, the model uses MTC
L242_Grades$capacity <- L242_Grades$capacity * conv_CO2_C

# We add a high cost final grade to give a steep slope for exhaustion of the
# final grade
L242_MaxGrade <- subset( L242_Grades, grade == max( L242_Grades$grade ) )
L242_MaxGrade$grade <- max( L242_Grades$grade ) + 1
L242_MaxGrade$capacity <- 0
L242_MaxGrade$cost <- 10000
L242_Grades <- rbind( L242_Grades, L242_MaxGrade )

# Adjust the bottom of the supply curve to give the solution algorithm some
# room to play when the low cost is zero
L242_Grades$cost <- pmax( L242_Grades$cost, 0.001 )

L242_Grades <- L242_Grades[, c( "region", "resource", "subresource", "grade", "cost", "capacity" ) ]

# Delete old carbon storage sectors
printlog( "L242_DeleteSector: DELETE SECTOR" )
L242_DeleteSector <- subset( A_ccs_delete, type == "sector", select=c( "region", "name" ) )

# Create carbon storage sectors in the states
printlog( "L242_Sector: SECTOR LEVEL PARAMS" )
# The sector table will include units strings as well as the logit exponent by region
L242_Sector <- A_ccs_sector[ rep( 1:nrow( A_ccs_sector ), times = length( states ) ), ]
L242_Sector <- data.frame( region=states, L242_Sector )

# Create a table for subsector nest share-weights and logits
printlog( "L242_Subsector: SUBSECTOR LEVEL PARAMS" )
L242_Subsector <- A_ccs_subsector[ sort( rep( 1:nrow( A_ccs_subsector ), times=length( states ) ) ), ]
L242_Subsector <- data.frame( region=states, L242_Subsector )

# Create a table for subsector interpolation rules
printlog( "L241_ElecSubsInterpRule: SUBSECTOR LEVEL INTERP RULES" )
L242_SubsInterpRule <- data.frame( A_ccs_subs_interp, apply_to="share-weight" )
L242_SubsInterpRule <- L242_SubsInterpRule[ sort( rep( 1:nrow( L242_SubsInterpRule ), times = length( states ) ) ), ]
L242_SubsInterpRule <- data.frame( region=states, L242_SubsInterpRule )

# Create a for technology parameters including costs, efficiencies, inputs, etc
printlog( "L242_TechInput: TECH INPUT" )
L242_TechInput <- A_ccs_tech_input[ rep( 1:nrow( A_ccs_tech_input ), times=length( states ) ), ]
L242_TechInput <- data.frame( region=states, L242_TechInput )
L242_TechInput$market <- states_subregions[ match( L242_TechInput$region, states_subregions$state ),
    c( "subregion_FERC" ) ]
# add column for calibration value and reorder columns so that we can utilize an already existing
# header for this table
L242_TechInput$calibrated_value <- -1
L242_TechInput <- L242_TechInput[, c( "region", "supplysector", "subsector", "technology", "period",
    "share_weight", "minicam_energy_input", "calibrated_value", "coefficient", "market" ) ]

# Change storage markets for CCS technologies in the global database such that they will
# just use the one in it's own market, i.e. the default
# TODO: no need apparently but how did it work before?

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
# TODO: the deletes are not actually wanted since H2 production in the USA region still
# relies on it.
#write_mi_data( L242_DeleteResource, "DeleteResource", "L242_DeleteResource", "batch_rgcam_ccs_input.xml" )
write_mi_data( L242_Resource, "DepResource", "L242_Resource", "batch_rgcam_ccs_input.xml" )
write_mi_data( L242_Grades, "DepResourceGrades", "L242_Grades", "batch_rgcam_ccs_input.xml" )

#write_mi_data( L242_DeleteSector, "SectorDelete", "L242_DeleteSector", "batch_rgcam_ccs_input.xml" )
write_mi_data( L242_Sector, "Sector", "L242_Sector", "batch_rgcam_ccs_input.xml" )
write_mi_data( L242_Subsector, "ElecSubsector", "L242_Subsector", "batch_rgcam_ccs_input.xml" )
write_mi_data( L242_SubsInterpRule, "ElecSubsInterpRule", "L242_SubsInterpRule", "batch_rgcam_ccs_input.xml" )
write_mi_data( L242_TechInput, "ElecSubgregionalCal", "L242_TechInput", "batch_rgcam_ccs_input.xml" )
#write_mi_data( L241_ChangeMarket, "ChangeInputMarket", "L241_ChangeMarket", "batch_rgcam_ccs_input.xml" )

insert_file_into_batchxml( "batch_rgcam_ccs_input.xml", "rgcam_ccs_input.xml", "", xml_tag="outFile" )

logstop()
