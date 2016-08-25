# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "WATERPROC_DIR" ) ){
    if( Sys.getenv( "WATERPROC" ) != "" ){
        WATERPROC_DIR <- Sys.getenv( "WATERPROC" )
    } else {
        stop("Could not determine location of water data system. Please set the R var WATERPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(WATERPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(WATERPROC_DIR,"/../_common/headers/WATER_header.R",sep=""))
logstart( "L2233.electricity_water.R" )
printlog( "Model input for electricity sector with cooling system types disaggregated" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
A23.globalinttech <- readdata( "ENERGY_ASSUMPTIONS", "A23.globalinttech" )
A23.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_shrwt" )
A23.sector <- readdata( "ENERGY_ASSUMPTIONS", "A23.sector" )
elec_tech_water_map <- readdata( "WATER_MAPPINGS", "elec_tech_water_map" )
A03.sector <- readdata( "WATER_ASSUMPTIONS", "A03.sector" )
A23.CoolingSystemCosts <- readdata( "WATER_ASSUMPTIONS", "A23.CoolingSystemCosts" )
Macknick_elec_water_m3MWh <- readdata( "WATER_LEVEL0_DATA", "Macknick_elec_water_m3MWh" )
L1231.out_EJ_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.out_EJ_R_elec_F_tech_Yh" )
L1233.out_EJ_R_elec_F_tech_Yh_cool <- readdata( "WATER_LEVEL1_DATA", "L1233.out_EJ_R_elec_F_tech_Yh_cool" )
L1233.in_EJ_R_elec_F_tech_Yh_cool <- readdata( "WATER_LEVEL1_DATA", "L1233.in_EJ_R_elec_F_tech_Yh_cool" )
L1233.shrwt_R_elec_cool_Yf <- readdata( "WATER_LEVEL1_DATA", "L1233.shrwt_R_elec_cool_Yf" )
L223.StubTechEff_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.StubTechEff_elec", skip = 4 )

# Read in the existing electric sector files in a for loop, so that we can efficiently make changes to all files
Elec_files <- list.files( readdomainpathmap()["ENERGY_LEVEL2_DATA"][[1]] )[
     grepl( "L223\\.", list.files( readdomainpathmap()["ENERGY_LEVEL2_DATA"][[1]] ) ) ]
Elec_files <- sub( ".csv", "", Elec_files )
Elec_files.list <- list()
for( i in Elec_files ){
  index <- which( Elec_files == i )
  Elec_files.list[[index]] <- readdata( "ENERGY_LEVEL2_DATA", i, skip=4 )
}
names(Elec_files.list) <- Elec_files

# NonCO2 files
L201.en_bcoc_emissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L201.en_bcoc_emissions", skip = 4 )
L241.nonco2_tech_coeff <- readdata( "EMISSIONS_LEVEL2_DATA", "L241.nonco2_tech_coeff", skip = 4 )

# -------------------------------------------------------------------------------

#2. Build tables
# Make a couple of tables with all of the possible technologies from the new and old structures
L2233.TechMap <- elec_tech_water_map[
      grepl( "from\\.", names( elec_tech_water_map ) ) |
      grepl( "to\\.", names( elec_tech_water_map ) ) ]
L2233.TechMapYr <- repeat_and_add_vector( L2233.TechMap, Y, model_years )

printlog( "Not building different XML files for permutations of technology levels" )
printlog( "Removing technology scenario files from the set" )
Elec_files.list <- Elec_files.list[ !grepl( "_adv", names( Elec_files.list ) ) ]
Elec_files.list <- Elec_files.list[ !grepl( "_low", names( Elec_files.list ) ) ]

printlog( "PART 1: SUPPLYSECTOR AND SUBSECTOR INFORMATION IN THE EXISTING ELECTRICITY SECTOR" )
printlog( "Includes supplysector, subsector, and region-specific stub tech capacity factor modifications" )
# NOTE: if capital costs are moved to the downstream sectors (the cooling system techs), then the StubTechCapFactor
# table should not be here, and instead should be processed with the stub tech info in the new sectors
L2233.Elec_tables_copy <- list()
for( i in Elec_files ){
	table_names <- paste( names( Elec_files.list[[i]] ), collapse = " " )
	L2233.Elec_tables_copy[[i]] <- if( !grepl( "technology", table_names ) | i == "L223.StubTechCapFactor_elec" ) Elec_files.list[[i]]
}

# In this list of tables, the sorting is alphabetical, not the order in which the files are written to the XML batch file
# Without any modifications, default fillout shareweights will over-write manually set share-weights, and the supplysector
# information will be after the subsector information, where it should be before it
L2333.reorder_table_names <- c( grep( "EQUIV_TABLE", names( L2233.Elec_tables_copy ), value=T ) )
L2333.reorder_table_names <- c( L2333.reorder_table_names, grep( "Supplysector.*logit", names( L2233.Elec_tables_copy )[ names( L2233.Elec_tables_copy ) %!in% L2333.reorder_table_names], value=T ) )
L2333.reorder_table_names <- c( L2333.reorder_table_names, grep( "Supplysector", names( L2233.Elec_tables_copy )[ names( L2233.Elec_tables_copy ) %!in% L2333.reorder_table_names], value=T ) )
L2333.reorder_table_names <- c( L2333.reorder_table_names, grep( "SubsectorShrwtFllt", names( L2233.Elec_tables_copy )[ names( L2233.Elec_tables_copy ) %!in% L2333.reorder_table_names], value=T ) )
L2333.reorder_table_names <- c( L2333.reorder_table_names, grep( "Subsector.*logit", names( L2233.Elec_tables_copy )[ names( L2233.Elec_tables_copy ) %!in% L2333.reorder_table_names], value=T ) )
L2333.reorder_table_names <- c( L2333.reorder_table_names, names( L2233.Elec_tables_copy )[ names( L2233.Elec_tables_copy ) %!in% L2333.reorder_table_names] )
L2233.Elec_tables_copy <- L2233.Elec_tables_copy[ L2333.reorder_table_names ]

for( i in 1:length( L2233.Elec_tables_copy ) ){
	objectname <- sub( "L223.", "L2233.", names( L2233.Elec_tables_copy[i] ) )
	object <- L2233.Elec_tables_copy[[i]]
	assign( objectname, object )
    IDstringendpoint <- if( grepl( "_", objectname ) & !grepl( 'EQUIV_TABLE', objectname ) & !grepl( '-logit$', objectname ) ) {
        regexpr( "_", objectname, fixed = T ) - 1
    } else {
        nchar( objectname )
    }
    IDstring <- substr( objectname, 7, IDstringendpoint )
	write_mi_data( object, IDstring, "WATER_LEVEL2_DATA", objectname, "WATER_XML_BATCH", "batch_electricity_water.xml")
}

printlog( "PART 2: PASSTHROUGH TECHNOLOGIES IN THE EXISTING ELECTRICITY SECTOR" )
# The technologies in the electric sector all keep their own name; the only parameters read here are shareweights (so that the
# technologies exist) and minicam-energy-inputs, which are equal to the "to.supplysector" of the new generation technology.
printlog( "L2233.StubTech_elecPassthru: Stub technology tags for the pass-through technologies in the electricity sector" )
# These technology names do not change; their energy-inputs, shareweights, and efficiencies will be read to the global technology database
L2233.StubTech_elecPassthru <- Elec_files.list[[ "L223.StubTech_elec" ]]

printlog( "L2233.GlobalTechEff_elecPassthru: Input name and efficiency of pass-through technologies in the electricity sector" )
printlog( "L2233.GlobalTechShrwt_elecPassthru: Share-weights of pass-through technologies in the electricity sector" )
# First, subset only the technologies that are actually moving to a different sector
# Note that the designation of intermittent technologies does not need to be made here; pass-through technologies are not intermittent
L2233.GlobalTechEffShrwt_elecPassthru <- subset( L2233.TechMapYr, from.supplysector != to.supplysector )
L2233.GlobalTechEffShrwt_elecPassthru[[input]] <- L2233.GlobalTechEffShrwt_elecPassthru$to.supplysector
L2233.GlobalTechEffShrwt_elecPassthru[ c( "sector.name", "subsector.name", "technology" ) ] <-
      L2233.GlobalTechEffShrwt_elecPassthru[ c( "from.supplysector", "from.subsector", "from.technology" ) ]
L2233.GlobalTechEffShrwt_elecPassthru$efficiency <- 1

L2233.GlobalPassThroughTech <- L2233.GlobalTechEffShrwt_elecPassthru[, names_GlobalTech ]

#Match in the global tech shareweights from the assumptions to the electric sector
# Note: would be somewhat difficult to use the level2 output here because some are intermittent technologies
L223.globaltech_shrwt_passthru <- interpolate_and_melt( A23.globaltech_shrwt, model_years, value.name = "share.weight", digits = digits_shrwt )
L2233.GlobalTechEffShrwt_elecPassthru$share.weight <- L223.globaltech_shrwt_passthru$share.weight[
      match( vecpaste( L2233.GlobalTechEffShrwt_elecPassthru[ c( "sector.name", "subsector.name", "technology", Y ) ] ),
             vecpaste( L223.globaltech_shrwt_passthru[ c( s_s_t, Y ) ] ) ) ]

L2233.GlobalTechEff_elecPassthru <- L2233.GlobalTechEffShrwt_elecPassthru[ names_GlobalTechEff ]
L2233.GlobalTechShrwt_elecPassthru <- L2233.GlobalTechEffShrwt_elecPassthru[ c( names_GlobalTechYr, "share.weight" ) ]

printlog( "L2233.StubTechProd_elecPassthru: calibrated electricity flow through the pass-through technologies" )
L2233.StubTechProd_elecPassthru <- interpolate_and_melt( L1231.out_EJ_R_elec_F_tech_Yh, model_base_years, "calOutputValue", digits_calOutput )
L2233.StubTechProd_elecPassthru <- add_region_name( L2233.StubTechProd_elecPassthru )
L2233.StubTechProd_elecPassthru[ c( supp, subs, "stub.technology" ) ] <- calibrated_techs[
      match( vecpaste( L2233.StubTechProd_elecPassthru[ S_F_tech ] ),
             vecpaste( calibrated_techs[ S_F_tech ] ) ),
      s_s_t ]
L2233.StubTechProd_elecPassthru$share.weight.year <- L2233.StubTechProd_elecPassthru$year
L2233.StubTechProd_elecPassthru <- set_subsector_shrwt( L2233.StubTechProd_elecPassthru )
L2233.StubTechProd_elecPassthru$tech.share.weight <- ifelse( L2233.StubTechProd_elecPassthru$calOutputValue > 0, 1, 0 )

printlog( "Note: dropping hydro calibration from the passthru tech because it is a fixed output tech and will be calibrated elsewhere" )
#NOTE: this step assumes that hydro is remaining in the electric sector. Not ideal but hydro is going to be a pain no matter what.
# If hydro were moved to its own sector, then the output at that level could not be "fixed" because there would be no way to compute a price
L2233.StubTechProd_elecPassthru <- subset( L2233.StubTechProd_elecPassthru[ names_StubTechProd ], subsector != "hydro" )

printlog( "PART 3: SUPPLYSECTOR AND SUBSECTOR INFORMATION IN THE NEW ELECTRICITY SECTORS" )
printlog( "L2233.Supplysector_elec_cool: supplysectors" )
#Note that the value of the logit exponent doesn't matter b/c there's no subsector competition here. Just inheriting all values from the electric sector table
L2233.elec_cool_supplysectors <- unique( L2233.TechMap$to.supplysector )[ unique( L2233.TechMap$to.supplysector ) %!in% L2233.StubTech_elecPassthru$supplysector ]
L2233.supplysector_info <- Elec_files.list[[ "L223.Supplysector_elec"]][
      Elec_files.list[["L223.Supplysector_elec"]][["supplysector"]] == "electricity", ]
L2233.supplysector_info <- repeat_and_add_vector(
      L2233.supplysector_info, supp, L2233.elec_cool_supplysectors )
L2233.supplysector_info$logit.type <- NA

L2233.PassThroughSector_elec_cool <- L2233.supplysector_info[, c( reg, supp ) ]
names(L2233.PassThroughSector_elec_cool )[ names( L2233.PassThroughSector_elec_cool ) == supp ] <- "pass.through.sector"
L2233.PassThroughSector_elec_cool$marginal.revenue.sector <- "electricity"
L2233.PassThroughSector_elec_cool$marginal.revenue.market <- L2233.PassThroughSector_elec_cool$region
L2233.SectorLogitTables_elec_cool <- get_logit_fn_tables( L2233.supplysector_info, names_SupplysectorLogitType, base.header="Supplysector_",
    include.equiv.table=F, write.all.regions=F )
L2233.Supplysector_elec_cool <- L2233.supplysector_info[, names_Supplysector ]

printlog( "L2233.ElecReserve_elec_cool: electricity reserve margin and average grid capacity factor" )
L2233.elec_cool_Int_supplysectors <- unique( L2233.TechMap$to.supplysector[
      vecpaste( L2233.TechMap[ c( "from.supplysector", "from.subsector", "from.technology" ) ] ) %in%
      vecpaste( A23.globalinttech[ s_s_t ] ) ] )
L2233.ElecReserve_elec_cool <- subset( L2233.Supplysector_elec_cool, supplysector %in% L2233.elec_cool_Int_supplysectors )[ c( reg, supp ) ]
L2233.ElecReserve_elec_cool[ c( "electricity.reserve.margin", "average_grid.capacity.factor" ) ] <-
      unique( A23.sector[ c( "electricity.reserve.margin", "average.grid.capacity.factor" ) ] )

printlog( "L2233.SubsectorShrwtFllt_elec_cool: subsector info" )
printlog( "NOTE: Assuming that the subsectors are just pass-through, and the cooling system competition takes place at the technology level" )
L2233.SubsectorShrwtFllt_elec_cool <- L2233.Supplysector_elec_cool[ c( "region", "supplysector" ) ]
L2233.SubsectorShrwtFllt_elec_cool$subsector <- elec_tech_water_map$to.subsector[
      match( L2233.SubsectorShrwtFllt_elec_cool$supplysector, elec_tech_water_map$to.supplysector ) ]
L2233.SubsectorShrwtFllt_elec_cool$year.fillout <- min( model_base_years )
L2233.SubsectorShrwtFllt_elec_cool$share.weight <- 1

printlog( "L2233.SubsectorLogit_elec_cool: logit exponent for cooling system choice" )
L2233.SubsectorLogit_elec_cool <- L2233.SubsectorShrwtFllt_elec_cool[ names_Subsector ]
L2233.SubsectorLogit_elec_cool$logit.year.fillout <- min( model_base_years )
L2233.SubsectorLogit_elec_cool$logit.exponent <- cooling_system_logit
L2233.SubsectorLogit_elec_cool$logit.type <- cooling_system_logit.type
L2233.SubsectorLogitTables_elec_cooling <- get_logit_fn_tables( L2233.SubsectorLogit_elec_cool, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )
L2233.SubsectorLogit_elec_cool <- L2233.SubsectorLogit_elec_cool[, names_SubsectorLogit ]

printlog( "PART 4: GLOBAL TECHNOLOGY INFORMATION RE-ASSIGNED TO NEW STRUCTURE" )
#All global technology information from the prior electricity sector is carried over to the new locations
# Note: using the string "sector.name" to identify tables with global technologies (not all have "GlobalTech" in their name)
L2233.Elec_tables_globaltech <- list()
for( i in Elec_files ){
	table_names <- paste( names( Elec_files.list[[i]] ), collapse = " " )
	L2233.Elec_tables_globaltech[[i]] <- if( grepl( "sector.name", table_names ) ) Elec_files.list[[i]]
}

printlog( "Capital, fixed O&M, and variable O&M are assigned to the standard electricity sector" )
# If the table has cost information, then we keep it in the passthrough technology, because we don't want the whole-plant
# costs to be bundled with the cooling system costs. Keeping these together would mean that cooling system
# decisions would be made on <1% cost differences, which would require a very high logit to get any behavior at all.
L2233.Elec_tables_globaltech_cost <- L2233.Elec_tables_globaltech[
      names( L2233.Elec_tables_globaltech )[
             grepl( "Capital", names( L2233.Elec_tables_globaltech ) ) |
             grepl( "OM", names( L2233.Elec_tables_globaltech ) ) ] ]

#Re-set the intermittent technology to standard technology
# Also add in the "to-technology" ID vectors, which are used in separating out the intermittent technologies that remain in the "electricity" sector
# The part about this that isn't straightforward is that for an intermittent technology that is being moved to a new sector, the new passthru
# tech in the electricity sector is no longer an intermittent technology (which would involve double-counting)
to.techIDs <- c( "to.supplysector", "to.subsector", "to.technology" )
for( i in 1:length( L2233.Elec_tables_globaltech_cost ) ){
	names( L2233.Elec_tables_globaltech_cost[[i]] )[ names( L2233.Elec_tables_globaltech_cost[[i]] ) == "intermittent.technology" ] <- "technology"
	L2233.Elec_tables_globaltech_cost[[i]][ to.techIDs ] <- elec_tech_water_map[
	      match( vecpaste( L2233.Elec_tables_globaltech_cost[[i]][ c( "sector.name", "subsector.name", "technology" ) ] ),
	             vecpaste( elec_tech_water_map[ c( "from.supplysector", "from.subsector", "from.technology" ) ] ) ),
	      to.techIDs ]
}

L2233.GlobalTechCapital_elecPassthru <- rbind(
      L2233.Elec_tables_globaltech_cost[[ "L223.GlobalTechCapital_elec" ]],
      L2233.Elec_tables_globaltech_cost[[ "L223.GlobalIntTechCapital_elec" ]] )
L2233.GlobalTechOMfixed_elecPassthru <- rbind(
      L2233.Elec_tables_globaltech_cost[[ "L223.GlobalTechOMfixed_elec" ]],
      L2233.Elec_tables_globaltech_cost[[ "L223.GlobalIntTechOMfixed_elec" ]] )
L2233.GlobalTechOMvar_elecPassthru <- rbind(
      L2233.Elec_tables_globaltech_cost[[ "L223.GlobalTechOMvar_elec" ]],
      L2233.Elec_tables_globaltech_cost[[ "L223.GlobalIntTechOMvar_elec" ]] )

# Partition the intermittent and standard technologies. Intermittent techs are in the table of intermittent technologies
# AND are not being moved to a different supplysector/subsector/technology
printlog( "L2233.GlobalIntTechCapital_elec: Capital costs of intermittent technologies applied in the electricity sector" )
L2233.GlobalIntTechCapital_elec <- subset( L2233.GlobalTechCapital_elecPassthru,
      paste( sector.name, subsector.name, technology ) == paste( to.supplysector, to.subsector, to.technology ) &
      paste( sector.name, subsector.name, technology ) %in%
      paste( A23.globalinttech$supplysector, A23.globalinttech$subsector, A23.globalinttech$technology ) )
L2233.GlobalIntTechCapital_elec <- L2233.GlobalIntTechCapital_elec[ names( L2233.GlobalIntTechCapital_elec ) %!in% to.techIDs ]

printlog( "L2233.GlobalTechCapital_elecPassthru: Capital costs of passthru technologies in the electricity sector" )
printlog( "NOTE: generation costs are applied here; the cooling system techs only have cooling system costs")
L2233.GlobalTechCapital_elecPassthru <- subset( L2233.GlobalTechCapital_elecPassthru,
      paste( sector.name, subsector.name, technology ) %!in%
      vecpaste( L2233.GlobalIntTechCapital_elec[ c( "sector.name", "subsector.name", "technology" ) ] ) )
L2233.GlobalTechCapital_elecPassthru <- L2233.GlobalTechCapital_elecPassthru[ names( L2233.GlobalTechCapital_elecPassthru ) %!in% to.techIDs ]

printlog( "L2233.GlobalIntTechOMfixed_elec: OMfixed costs of intermittent technologies applied in the electricity sector" )
L2233.GlobalIntTechOMfixed_elec <- subset( L2233.GlobalTechOMfixed_elecPassthru,
      paste( sector.name, subsector.name, technology ) == paste( to.supplysector, to.subsector, to.technology ) &
      paste( sector.name, subsector.name, technology ) %in%
      paste( A23.globalinttech$supplysector, A23.globalinttech$subsector, A23.globalinttech$technology ) )
L2233.GlobalIntTechOMfixed_elec <- L2233.GlobalIntTechOMfixed_elec[ names( L2233.GlobalIntTechOMfixed_elec ) %!in% to.techIDs ]

printlog( "L2233.GlobalTechOMfixed_elecPassthru: OMfixed costs of passthru technologies in the electricity sector" )
printlog( "NOTE: generation costs are applied here; the cooling system techs only have cooling system costs")
L2233.GlobalTechOMfixed_elecPassthru <- subset( L2233.GlobalTechOMfixed_elecPassthru,
      paste( sector.name, subsector.name, technology ) %!in%
      vecpaste( L2233.GlobalIntTechOMfixed_elec[ c( "sector.name", "subsector.name", "technology" ) ] ) )
L2233.GlobalTechOMfixed_elecPassthru <- L2233.GlobalTechOMfixed_elecPassthru[ names( L2233.GlobalTechOMfixed_elecPassthru ) %!in% to.techIDs ]

printlog( "L2233.GlobalIntTechOMvar_elec: OMvar costs of intermittent technologies applied in the electricity sector" )
L2233.GlobalIntTechOMvar_elec <- subset( L2233.GlobalTechOMvar_elecPassthru,
      paste( sector.name, subsector.name, technology ) == paste( to.supplysector, to.subsector, to.technology ) &
      paste( sector.name, subsector.name, technology ) %in%
      paste( A23.globalinttech$supplysector, A23.globalinttech$subsector, A23.globalinttech$technology ) )
L2233.GlobalIntTechOMvar_elec <- L2233.GlobalIntTechOMvar_elec[ names( L2233.GlobalIntTechOMvar_elec ) %!in% to.techIDs ]

printlog( "L2233.GlobalTechOMvar_elecPassthru: OMvar costs of passthru technologies in the electricity sector" )
printlog( "NOTE: generation costs are applied here; the cooling system techs only have cooling system costs")
L2233.GlobalTechOMvar_elecPassthru <- subset( L2233.GlobalTechOMvar_elecPassthru,
      paste( sector.name, subsector.name, technology ) %!in%
      vecpaste( L2233.GlobalIntTechOMvar_elec[ c( "sector.name", "subsector.name", "technology" ) ] ) )
L2233.GlobalTechOMvar_elecPassthru <- L2233.GlobalTechOMvar_elecPassthru[ names( L2233.GlobalTechOMvar_elecPassthru ) %!in% to.techIDs ]

L2233.Elec_tables_globaltech_nocost <- L2233.Elec_tables_globaltech[
      names( L2233.Elec_tables_globaltech ) %!in% names( L2233.Elec_tables_globaltech_cost ) ]

for( i in 1:length( L2233.Elec_tables_globaltech_nocost ) ){
		objectname <- paste0( sub( "L223.", "L2233.", names( L2233.Elec_tables_globaltech_nocost[i] ) ), "_cool" )
		object <- L2233.Elec_tables_globaltech_nocost[[i]]
        if("year" %in% names( object ) ) {
            non.data.names <- names_GlobalTechYr
        } else {
            non.data.names <- names_GlobalTech
        }
		data.names <- names( object )[ names( object ) %!in% c( non.data.names, "intermittent.technology" ) ]
		# Re-set intermittent technology name to technology. Intermittency will still be captured in the "electricity" sector.
		if( "intermittent.technology" %in% names( L2233.Elec_tables_globaltech_nocost[[i]] ) ){
			names( object )[ names( object ) == "intermittent.technology" ] <- "technology"
		} 
		# The existing "object" contains fewer technologies than we want, as we're expanding this by the number of cooling systems,
		# and each generation tech can have any number of cooling system options. For this reason, we'll work backwards from the
		# tables with all possible global technology names and years available
		new_object <- subset( L2233.TechMapYr, paste( from.supplysector, from.subsector, from.technology, year ) %in%
		                      paste( object$sector.name, object$subsector.name, object$technology, object$year ) )
		new_object[ data.names ] <- object[
		     match( vecpaste( new_object[ c( "from.supplysector", "from.subsector", "from.technology", Y ) ] ),
		            vecpaste( object[ non.data.names ] ) ),
		     data.names ]
		#Re-set the column names to the ones we want, and drop the remaining columns
		names( new_object )[ names( new_object ) %in% c( "to.supplysector", "to.subsector", "to.technology" ) ] <- names_GlobalTech
		new_object <- new_object[ c( non.data.names, data.names ) ]
		
		#If it's a table with efficiency information, reduce the efficiencies of the dry cooling techs
		if( "efficiency" %in% names( new_object ) ){
			new_object$efficiency[ grepl( "dry", new_object$technology ) ] <-
			new_object$efficiency[ grepl( "dry", new_object$technology ) ] * dry_cooling_eff_adj
		}
	assign( objectname, object )
	IDstring <- substr( objectname, regexpr( ".", objectname, fixed = T ) + 1, regexpr( "_", objectname, fixed = T ) - 1 )
	write_mi_data( new_object, IDstring, "WATER_LEVEL2_DATA", objectname, "WATER_XML_BATCH", "batch_electricity_water.xml")
}

printlog( "L2233.GlobalTechCapital_elec_cool: Capital costs of cooling systems only" )
L2233.CoolingSystemCosts <- interpolate_and_melt( A23.CoolingSystemCosts, model_years, "capital.overnight.2005USD" )
L2233.CoolingSystemCosts$capital.overnight <- round( L2233.CoolingSystemCosts$capital.overnight.2005USD * conv_2005_1975_USD, digits_capital )
L2233.GlobalTechCapital_elec_cool <- repeat_and_add_vector( elec_tech_water_map, Y, model_years )
L2233.GlobalTechCapital_elec_cool[ c( "input.capital", "capital.overnight" ) ] <- L2233.CoolingSystemCosts[
      match( L2233.GlobalTechCapital_elec_cool$cooling_system, L2233.CoolingSystemCosts$cooling_system ),
      c( "input.capital", "capital.overnight" ) ]
L2233.GlobalTechCapital_elec_cool$fixed.charge.rate <- cooling_system_FCR
L2233.GlobalTechCapital_elec_cool$capacity.factor <- cooling_system_capacity_factor
L2233.GlobalTechCapital_elec_cool[ c( "sector.name", "subsector.name", "technology" ) ] <-
      L2233.GlobalTechCapital_elec_cool[ c( "to.supplysector", "to.subsector", "to.technology" ) ]
L2233.GlobalTechCapital_elec_cool <- subset( L2233.GlobalTechCapital_elec_cool, capital.overnight > 0 )

# This needs to be partitioned between int-techs and standard techs
L2233.GlobalIntTechCapital_elec_cool <- subset( L2233.GlobalTechCapital_elec_cool,
      paste( from.supplysector, from.subsector, from.technology ) %in%
      vecpaste( A23.globalinttech[ s_s_t ] ) )[ names_GlobalTechCapital ]
L2233.GlobalTechCapital_elec_cool <- subset( L2233.GlobalTechCapital_elec_cool,
      paste( sector.name, subsector.name, technology ) %!in%
      vecpaste( L2233.GlobalIntTechCapital_elec_cool[ c( "sector.name", "subsector.name", "technology" ) ] ) )[ names_GlobalTechCapital ]

printlog( "L2233.GlobalTechCoef_elec_cool: Water demand coefficients of electric technologies" )
L2233.GlobalTechCoef_elec_cool <- elec_tech_water_map[
      c( "to.supplysector", "to.subsector", "to.technology", "fuel", "technology", "cooling_system", "water_type" ) ]
L2233.GlobalTechCoef_elec_cool <- subset( L2233.GlobalTechCoef_elec_cool, water_type != "none" )
L2233.GlobalTechCoef_elec_cool[ c( "water_withdrawals", "water_consumption" ) ] <- round(
      Macknick_elec_water_m3MWh[
         match( vecpaste( L2233.GlobalTechCoef_elec_cool[ c( "fuel", "technology", "cooling_system", "water_type" ) ] ),
                vecpaste( Macknick_elec_water_m3MWh[ c( "fuel", "technology", "cooling_system", "water_type" ) ] ) ),
         c( "water_withdrawals", "water_consumption" ) ] / conv_MWh_GJ,
      digits_coefficient )
L2233.GlobalTechCoef_elec_cool <- melt( L2233.GlobalTechCoef_elec_cool,
      id.vars = c( "to.supplysector", "to.subsector", "to.technology" ),
      measure.vars = c( "water_consumption", "water_withdrawals" ),
      variable.name = input,
      value.name = "coefficient" )
names( L2233.GlobalTechCoef_elec_cool )[ names( L2233.GlobalTechCoef_elec_cool ) %in% c( "to.supplysector", "to.subsector", "to.technology" ) ] <-
      c( "sector.name", "subsector.name", "technology" )
L2233.GlobalTechCoef_elec_cool <- repeat_and_add_vector( L2233.GlobalTechCoef_elec_cool, Y, model_years )[ names_GlobalTechCoef ]
L2233.GlobalTechCoef_elec_cool[[input]] <- sub( "_", " ", L2233.GlobalTechCoef_elec_cool[[input]])
#Seawater is only relevant for water withdrawals, not consumption (values reflect temperature rises in inland water bodies, and we don't care about seawater scarcity)
L2233.GlobalTechCoef_elec_cool <- subset( L2233.GlobalTechCoef_elec_cool, paste( technology, minicam.energy.input ) != "seawater water consumption" )
L2233.GlobalTechCoef_elec_cool[[input]][ L2233.GlobalTechCoef_elec_cool[[tech]] == "seawater" ] <- "seawater"

#TODO This needs to be partitioned between int-techs and standard techs
L2233.GlobalTechCoef_elec_cool[ c( "from.supplysector", "from.subsector", "from.technology" ) ] <- elec_tech_water_map[
      match( vecpaste( L2233.GlobalTechCoef_elec_cool[ c( "sector.name", "subsector.name", "technology" ) ] ),
             vecpaste( elec_tech_water_map[ c( "to.supplysector", "to.subsector", "to.technology" ) ] ) ),
      c( "from.supplysector", "from.subsector", "from.technology" ) ]
L2233.GlobalIntTechCoef_elec_cool <- subset( L2233.GlobalTechCoef_elec_cool,
      paste( from.supplysector, from.subsector, from.technology ) %in%
      vecpaste( A23.globalinttech[ s_s_t ] ) )[ names_GlobalTechCoef ]
L2233.GlobalTechCoef_elec_cool <- subset( L2233.GlobalTechCoef_elec_cool,
      paste( sector.name, subsector.name, technology ) %!in%
      vecpaste( L2233.GlobalIntTechCoef_elec_cool[ c( "sector.name", "subsector.name", "technology" ) ] ) )[ names_GlobalTechCoef ]
# We must re-map water inputs as some of the water_types should go through a mapping sector
L2233.GlobalTechCoef_elec_cool[[water_sector]] <- "Electricity"
L2233.GlobalTechCoef_elec_cool$minicam.energy.input <- get_water_inputs_for_mapping( L2233.GlobalTechCoef_elec_cool, A03.sector, water_type.col="minicam.energy.input" )
L2233.GlobalTechCoef_elec_cool[[water_sector]] <- NULL
L2233.GlobalIntTechCoef_elec_cool[[water_sector]] <- "Electricity"
L2233.GlobalIntTechCoef_elec_cool$minicam.energy.input <- get_water_inputs_for_mapping( L2233.GlobalIntTechCoef_elec_cool, A03.sector, water_type.col="minicam.energy.input" )
L2233.GlobalIntTechCoef_elec_cool[[water_sector]] <- NULL

printlog( "PART 5: STUB TECHNOLOGY INFORMATION IN THE NEW SECTOR STRUCTURE")
printlog( "L2233.StubTech_elec_cool: stub technologies of the cooling system options" )
L2233.StubTech_elec_cool <- repeat_and_add_vector( L2233.TechMap, R, GCAM_region_names$GCAM_region_ID )
L2233.StubTech_elec_cool <- add_region_name( L2233.StubTech_elec_cool )
L2233.StubTech_elec_cool[ c( supp, subs, "stub.technology" ) ] <-
      L2233.StubTech_elec_cool[ c( "to.supplysector", "to.subsector", "to.technology" ) ]
L2233.StubTech_elec_cool <- L2233.StubTech_elec_cool[ names_StubTech ]

printlog( "L2233.StubTechShrwt_elec_cool: stub technology shareweights for cooling system options" )
L2233.shrwt_R_elec_cool_Yf <- interpolate_and_melt( L1233.shrwt_R_elec_cool_Yf, model_future_years, value.name = "share.weight", digits = digits_shrwt )
L2233.shrwt_R_elec_cool_Yf <- add_region_name( L2233.shrwt_R_elec_cool_Yf )
L2233.shrwt_R_elec_cool_Yf[ c( supp, subs, "stub.technology" ) ] <- elec_tech_water_map[
      match( vecpaste( L2233.shrwt_R_elec_cool_Yf[ S_F_tech_cool ] ),
             vecpaste( elec_tech_water_map[ S_F_tech_cool ] ) ),
      c( "to.supplysector", "to.subsector", "to.technology" ) ]
L2233.StubTechShrwt_elec_cool <- repeat_and_add_vector( L2233.StubTech_elec_cool, Y, model_future_years )
L2233.StubTechShrwt_elec_cool$share.weight <- L2233.shrwt_R_elec_cool_Yf$share.weight[
      match( vecpaste( L2233.StubTechShrwt_elec_cool[ c( reg, supp, subs, "stub.technology", Y ) ] ),
             vecpaste( L2233.shrwt_R_elec_cool_Yf[ c( reg, supp, subs, "stub.technology", Y ) ] ) ) ]

#Dropping the NA's is important here--don't just set to zero. The NA's refer to techs without cooling system options,
# which in many cases already have stub-technology share-weights
L2233.StubTechShrwt_elec_cool <- na.omit( L2233.StubTechShrwt_elec_cool )

printlog( "L2233.StubTechEff_elec_cool: calibrated efficiencies of the cooling system options" )
L2233.RegTechMapYr <- repeat_and_add_vector( L2233.TechMapYr, R, GCAM_region_names[[R]] )
L2233.RegTechMapYr <- add_region_name( L2233.RegTechMapYr )
L2233.StubTechEff_elec_cool <- subset( L2233.RegTechMapYr, paste( from.supplysector, from.subsector, from.technology, year ) %in%
		                      vecpaste( L223.StubTechEff_elec[ c( supp, subs, "stub.technology", Y ) ] ) )
L2233.StubTechEff_elec_cool[ c( input, "efficiency", "market.name" ) ] <- L223.StubTechEff_elec[
      match( vecpaste( L2233.StubTechEff_elec_cool[ c( reg, "from.supplysector", "from.subsector", "from.technology", Y ) ] ),
             vecpaste( L223.StubTechEff_elec[ c( reg, supp, subs, "stub.technology", Y ) ] ) ),
      c( input, "efficiency", "market.name" ) ]
names( L2233.StubTechEff_elec_cool ) <- sub( "to\\.", "", names( L2233.StubTechEff_elec_cool ) )
names( L2233.StubTechEff_elec_cool )[ names( L2233.StubTechEff_elec_cool ) == "technology" ] <- "stub.technology"
L2233.StubTechEff_elec_cool <- L2233.StubTechEff_elec_cool[ names_StubTechEff ]

# Electric technology calibration: Not bothering to split input/output/fixed output anymore (there is no point)
L2233.out_EJ_R_elec_F_tech_Yh_cool <- interpolate_and_melt( L1233.out_EJ_R_elec_F_tech_Yh_cool, model_base_years, "calOutputValue", digits_calOutput )
L2233.out_EJ_R_elec_F_tech_Yh_cool <- add_region_name( L2233.out_EJ_R_elec_F_tech_Yh_cool )
L2233.out_EJ_R_elec_F_tech_Yh_cool[ c( supp, subs, "stub.technology" ) ] <- elec_tech_water_map[
      match( vecpaste( L2233.out_EJ_R_elec_F_tech_Yh_cool[ S_F_tech_cool ] ),
             vecpaste( elec_tech_water_map[ S_F_tech_cool ] ) ),
      c( "to.supplysector", "to.subsector", "to.technology" ) ]
L2233.out_EJ_R_elec_F_tech_Yh_cool$share.weight.year <- L2233.out_EJ_R_elec_F_tech_Yh_cool$year
L2233.out_EJ_R_elec_F_tech_Yh_cool <- set_subsector_shrwt( L2233.out_EJ_R_elec_F_tech_Yh_cool )
L2233.out_EJ_R_elec_F_tech_Yh_cool$tech.share.weight <- ifelse( L2233.out_EJ_R_elec_F_tech_Yh_cool$calOutputValue > 0, 1, 0 )

printlog( "L2233.StubTechProd_elec_cool: calibrated output of the cooling system options" )
L2233.StubTechProd_elec_cool <- L2233.out_EJ_R_elec_F_tech_Yh_cool[ names_StubTechProd ]

printlog( "L2233.StubTechFixOut_hydro: Hydropower fixed output in future periods" )
printlog( "NOTE: the historical calibration for hydro should be included here")
L2233.StubTechProd_elec_cool_hydro <- subset( L2233.StubTechProd_elec_cool, subsector == "hydro" )
L2233.StubTechProd_elec_cool <- subset( L2233.StubTechProd_elec_cool, subsector != "hydro" )
L2233.StubTechProd_elec_cool_hydro$fixedOutput <- L2233.StubTechProd_elec_cool_hydro$calOutputValue
L2233.StubTechProd_elec_cool_hydro[ c( "subs.share.weight", "tech.share.weight" ) ] <- 0

#Future years are mapped from the prior input files; assuming that hydro is not being disaggregated to multiple technologies
L2233.StubTechFixOut_hydro_fut <- Elec_files.list[[ "L223.StubTechFixOut_hydro" ]]
L2233.StubTechFixOut_hydro_fut[ c( supp, subs, "stub.technology" ) ] <- elec_tech_water_map[
      match( vecpaste( L2233.StubTechFixOut_hydro_fut[ c( supp, subs, "stub.technology" ) ] ),
             vecpaste( elec_tech_water_map[ c( "from.supplysector", "from.subsector", "from.technology" ) ] ) ),
      c( "to.supplysector", "to.subsector", "to.technology" ) ]
L2233.StubTechFixOut_hydro <- rbind(
      L2233.StubTechProd_elec_cool_hydro[ names_StubTechFixOut ],
      L2233.StubTechFixOut_hydro_fut )

printlog( "L2233.SectorNodeEquiv: Sets up equivalent sector tag names to avoid having to partition input tables" )
# TODO: Move this to some common file like level2_data_names? If so we would want a complete list.
L2233.SectorNodeEquiv <- data.frame( t( c( "SectorXMLTags", "supplysector", "pass-through-sector" ) ) )

printlog( "L2233.TechNodeEquiv: Sets up equivalent technology tag names to avoid having to partition input tables" )
# TODO: Move this to some common file like level2_data_names? If so we would want a complete list.
L2233.TechNodeEquiv <- data.frame( t( c( "TechnologyXMLTags", "technology", "intermittent-technology", "pass-through-technology" ) ) )

printlog( "NonCO2 emissions: Need to divide all emissions coefficients by elec tech efficiencies in order to get emissions correct" )
# The emissions coefficients use "input-driver" and the input is assumed to be the fuel, but here we have switched the input of these technologies
# to the power output. To keep the same emissions levels while the "input" decreased, the coefficient should be increased
printlog( "L2233.InputEmissCoeff_hist_elecPassthru: BC and OC emissions coefficients from electric power plants" )
L2233.InputEmissCoeff_hist_elecPassthru <- subset( L201.en_bcoc_emissions, supplysector == "electricity" )
L2233.InputEmissCoeff_hist_elecPassthru$efficiency <- L223.StubTechEff_elec$efficiency[
      match( vecpaste( L2233.InputEmissCoeff_hist_elecPassthru[ c( reg, supp, subs, "stub.technology", Y ) ] ),
             vecpaste( L223.StubTechEff_elec[ c( reg, supp, subs, "stub.technology", Y ) ] ) ) ]
L2233.InputEmissCoeff_hist_elecPassthru$emiss.coef <- round(
      L2233.InputEmissCoeff_hist_elecPassthru$emiss.coef / L2233.InputEmissCoeff_hist_elecPassthru$efficiency,
      digits_coefficient )

printlog( "L2233.InputEmissCoeff_fut_elecPassthru: " )
L223.GlobalTechEff_elec <- Elec_files.list[[ "L223.GlobalTechEff_elec" ]]
# make the spelling of the parameter consistent with the name in the XML
names( L241.nonco2_tech_coeff )[ names( L241.nonco2_tech_coeff ) == "emiss.coeff" ] <- "emiss.coef"
L2233.InputEmissCoeff_fut_elecPassthru <- subset( L241.nonco2_tech_coeff, supplysector == "electricity" )
L2233.InputEmissCoeff_fut_elecPassthru$efficiency <- L223.GlobalTechEff_elec$efficiency[
      match( vecpaste( L2233.InputEmissCoeff_fut_elecPassthru[ c( supp, subs, "stub.technology", Y ) ] ),
             vecpaste( L223.GlobalTechEff_elec[ c( names_GlobalTechYr ) ] ) ) ]
L2233.InputEmissCoeff_fut_elecPassthru$emiss.coef <- round(
      L2233.InputEmissCoeff_fut_elecPassthru$emiss.coef / L2233.InputEmissCoeff_fut_elecPassthru$efficiency,
      digits_coefficient )
      
# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
#Passthrough sectors
write_mi_data( L2233.SectorNodeEquiv, "EQUIV_TABLE", "WATER_LEVEL2_DATA", "L2233.SectorNodeEquiv", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.TechNodeEquiv, "EQUIV_TABLE", "WATER_LEVEL2_DATA", "L2233.TechNodeEquiv", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.StubTech_elecPassthru, "StubTech", "WATER_LEVEL2_DATA", "L2233.StubTech_elecPassthru", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.StubTechProd_elecPassthru, "StubTechProd", "WATER_LEVEL2_DATA", "L2233.StubTechProd_elecPassthru", "WATER_XML_BATCH", "batch_electricity_water.xml")
# Note this CSV file will set the pass-through-technology as the appropriate XML tag, any other tables
# that read data into these technologies should be written after this one.
write_mi_data( L2233.GlobalPassThroughTech, "GlobalPassThroughTech", "WATER_LEVEL2_DATA", "L2233.GlobalPassThroughTech", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.GlobalTechEff_elecPassthru, "GlobalTechEff", "WATER_LEVEL2_DATA", "L2233.GlobalTechEff_elecPassthru", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.GlobalTechShrwt_elecPassthru, "GlobalTechShrwt", "WATER_LEVEL2_DATA", "L2233.GlobalTechShrwt_elecPassthru", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.GlobalIntTechCapital_elec, "GlobalIntTechCapital", "WATER_LEVEL2_DATA", "L2233.GlobalIntTechCapital_elec", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.GlobalTechCapital_elecPassthru, "GlobalTechCapital", "WATER_LEVEL2_DATA", "L2233.GlobalTechCapital_elecPassthru", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.GlobalIntTechOMfixed_elec, "GlobalIntTechOMfixed", "WATER_LEVEL2_DATA", "L2233.GlobalIntTechOMfixed_elec", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.GlobalTechOMfixed_elecPassthru, "GlobalTechOMfixed", "WATER_LEVEL2_DATA", "L2233.GlobalTechOMfixed_elecPassthru", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.GlobalIntTechOMvar_elec, "GlobalIntTechOMvar", "WATER_LEVEL2_DATA", "L2233.GlobalIntTechOMvar_elec", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.GlobalTechOMvar_elecPassthru, "GlobalTechOMvar", "WATER_LEVEL2_DATA", "L2233.GlobalTechOMvar_elecPassthru", "WATER_XML_BATCH", "batch_electricity_water.xml")

#Cooling system sectors
# Note this CSV file will set the pass-through-sector as the appropriate XML tag, any other tables
# that read data into these sectors should be written after this one.
write_mi_data( L2233.PassThroughSector_elec_cool, "PassThroughSector", "WATER_LEVEL2_DATA", "L2233.PassThroughSector_elec_cool", "WATER_XML_BATCH", "batch_electricity_water.xml")
for( curr_table in names ( L2233.SectorLogitTables_elec_cool ) ) {
write_mi_data( L2233.SectorLogitTables_elec_cool[[ curr_table ]]$data, L2233.SectorLogitTables_elec_cool[[ curr_table ]]$header,
    "WATER_LEVEL2_DATA", paste0( "L2233.", L2233.SectorLogitTables_elec_cool[[ curr_table ]]$header, "_elec_cool" ), "WATER_XML_BATCH",
    "batch_electricity_water.xml" )
}
write_mi_data( L2233.Supplysector_elec_cool, "Supplysector", "WATER_LEVEL2_DATA", "L2233.Supplysector_elec_cool", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.ElecReserve_elec_cool, "ElecReserve", "WATER_LEVEL2_DATA", "L2233.ElecReserve_elec_cool", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.SubsectorShrwtFllt_elec_cool, "SubsectorShrwtFllt", "WATER_LEVEL2_DATA", "L2233.SubsectorShrwtFllt_elec_cool", "WATER_XML_BATCH", "batch_electricity_water.xml")
for( curr_table in names ( L2233.SubsectorLogitTables_elec_cooling ) ) {
write_mi_data( L2233.SubsectorLogitTables_elec_cooling[[ curr_table ]]$data, L2233.SubsectorLogitTables_elec_cooling[[ curr_table ]]$header,
    "WATER_LEVEL2_DATA", paste0( "L2233.", L2233.SubsectorLogitTables_elec_cooling[[ curr_table ]]$header, "_elec_cool" ), "WATER_XML_BATCH",
    "batch_electricity_water.xml" )
}
write_mi_data( L2233.SubsectorLogit_elec_cool, "SubsectorLogit", "WATER_LEVEL2_DATA", "L2233.SubsectorLogit_elec_cool", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.StubTech_elec_cool, "StubTech", "WATER_LEVEL2_DATA", "L2233.StubTech_elec_cool", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.StubTechEff_elec_cool, "StubTechEff", "WATER_LEVEL2_DATA", "L2233.StubTechEff_elec_cool", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.StubTechProd_elec_cool, "StubTechProd", "WATER_LEVEL2_DATA", "L2233.StubTechProd_elec_cool", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.StubTechFixOut_hydro, "StubTechFixOut", "WATER_LEVEL2_DATA", "L2233.StubTechFixOut_hydro", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.StubTechShrwt_elec_cool, "StubTechShrwt", "WATER_LEVEL2_DATA", "L2233.StubTechShrwt_elec_cool", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.GlobalTechCapital_elec_cool, "GlobalTechCapital", "WATER_LEVEL2_DATA", "L2233.GlobalTechCapital_elec_cool", "WATER_XML_BATCH", "batch_electricity_water.xml")
write_mi_data( L2233.GlobalIntTechCapital_elec_cool, "GlobalIntTechCapital", "WATER_LEVEL2_DATA", "L2233.GlobalIntTechCapital_elec_cool", "WATER_XML_BATCH", "batch_electricity_water.xml")

#Write the coefficients to a different file, so that markets can be turned on and off
write_mi_data( L2233.GlobalTechCoef_elec_cool, "GlobalTechCoef", "WATER_LEVEL2_DATA", "L2233.GlobalTechCoef_elec_cool", "WATER_XML_BATCH", "batch_electricity_water_coefs.xml")
write_mi_data( L2233.GlobalIntTechCoef_elec_cool, "GlobalIntTechCoef", "WATER_LEVEL2_DATA", "L2233.GlobalIntTechCoef_elec_cool", "WATER_XML_BATCH", "batch_electricity_water_coefs.xml")

write_mi_data( L2233.InputEmissCoeff_hist_elecPassthru, "InputEmissCoeff", "WATER_LEVEL2_DATA", "L2233.InputEmissCoeff_hist_elecPassthru", "WATER_XML_BATCH", "batch_water_elec_emissions.xml")
write_mi_data( L2233.InputEmissCoeff_fut_elecPassthru, "InputEmissCoeff", "WATER_LEVEL2_DATA", "L2233.InputEmissCoeff_fut_elecPassthru", "WATER_XML_BATCH", "batch_water_elec_emissions.xml")

insert_file_into_batchxml( "WATER_XML_BATCH", "batch_electricity_water.xml", "WATER_XML_FINAL", "electricity_water.xml", "", "outFile" )
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_electricity_water_coefs.xml", "WATER_XML_FINAL", "electricity_water_coefs.xml", "", "outFile" )
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_elec_emissions.xml", "WATER_XML_FINAL", "water_elec_emissions.xml", "", "outFile" )

logstop()
