if( !exists( "GCAMUSAPROC_DIR" ) ){
    if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
        GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "L223.electricity_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for GCAM USA electricity sectors with demand resolved nationally (1 region)" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
NREL_us_re_technical_potential <- readdata( "GCAMUSA_LEVEL0_DATA", "NREL_us_re_technical_potential" )
A23.globaltech_eff <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_eff" )
L114.CapacityFactor_wind_state <- readdata( "GCAMUSA_LEVEL1_DATA", "L114.CapacityFactor_wind_state" )
L119.CapFacScaler_PV_state <- readdata( "GCAMUSA_LEVEL1_DATA", "L119.CapFacScaler_PV_state" )
L119.CapFacScaler_CSP_state <- readdata( "GCAMUSA_LEVEL1_DATA", "L119.CapFacScaler_CSP_state" )
L223.Supplysector_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.Supplysector_elec", skip = 4 )
L223.ElecReserve <- readdata( "ENERGY_LEVEL2_DATA", "L223.ElecReserve", skip = 4 )
L223.SubsectorLogit_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.SubsectorLogit_elec", skip = 4 )
L223.SubsectorShrwt_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.SubsectorShrwt_elec", skip = 4, must.exist = F )
L223.SubsectorShrwtFllt_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.SubsectorShrwtFllt_elec", skip = 4, must.exist = F )
L223.SubsectorShrwt_nuc <- readdata( "ENERGY_LEVEL2_DATA", "L223.SubsectorShrwt_nuc", skip = 4, must.exist = F )
L223.SubsectorShrwt_renew <- readdata( "ENERGY_LEVEL2_DATA", "L223.SubsectorShrwt_renew", skip = 4, must.exist = F )
L223.SubsectorInterp_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.SubsectorInterp_elec", skip = 4, must.exist = F )
L223.SubsectorInterpTo_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.SubsectorInterpTo_elec", skip = 4, must.exist = F )
L223.StubTech_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.StubTech_elec", skip = 4 )
L223.StubTechEff_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.StubTechEff_elec", skip = 4 )
L223.StubTechCapFactor_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.StubTechCapFactor_elec", skip = 4 )
L223.GlobalIntTechBackup_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechBackup_elec", skip = 4 )
L1231.in_EJ_state_elec_F_tech <- readdata( "GCAMUSA_LEVEL1_DATA", "L1231.in_EJ_state_elec_F_tech" )
L1231.out_EJ_state_elec_F_tech <- readdata( "GCAMUSA_LEVEL1_DATA", "L1231.out_EJ_state_elec_F_tech" )
L1232.out_EJ_sR_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L1232.out_EJ_sR_elec" )
L126.out_EJ_state_td_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L126.out_EJ_state_td_elec" )

# -----------------------------------------------------------------------------
# 2. Perform computations

#Set up equivalent sector and technology tag names. 

printlog( "L223.SectorNodeEquiv: Sets up equivalent sector tag names to avoid having to partition input tables" )
L223.SectorNodeEquiv <- data.frame( t( c( "SectorXMLTags", "supplysector", "pass-through-sector" ) ) )

printlog( "L223.TechNodeEquiv: Sets up equivalent technology tag names to avoid having to partition input tables" )
L223.TechNodeEquiv <- data.frame( t( c( "TechnologyXMLTags", "technology", "intermittent-technology", "pass-through-technology" ) ) )

grid_regions <- sort( unique( states_subregions$grid_region ) )
elec_gen_names <- "electricity"

#Indicate states where geothermal electric technologies will not be created
NREL_us_re_technical_potential$state <- states_subregions$state[ match( NREL_us_re_technical_potential$State, states_subregions$state_name ) ]
geo_states_noresource <- paste( states[ states %in% NREL_us_re_technical_potential$state[ NREL_us_re_technical_potential$Geothermal_Hydrothermal_GWh == 0 ] ], "geothermal" )

if( !use_regional_elec_markets ){	
	printlog( "PART 1: THE USA REGION" )
	# Define the sector(s) that will be used in this code file. Can be one or multiple sectors
	printlog( "The subsectors of the existing USA electricity sector are deleted. Keeping the supplysector info, incl. reserve margin" )
	printlog( "NOTE: This also removes the rooftop PV subsector of the USA elect_td_bld sector")
	L223.DeleteSubsector_USAelec <- subset( L223.SubsectorLogit_elec[ names_Subsector ], region == "USA" )
	write_mi_data( L223.DeleteSubsector_USAelec, "DeleteSubsector", "GCAMUSA_LEVEL2_DATA", "L223.DeleteSubsector_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )

	printlog( "L223.Supplysector_USAelec: supplysector for electricity sector in the USA region, including logit exponent between grid regions" )
	#All of the supplysector information is the same as before, except the logit exponent
	L223.Supplysector_USAelec <- data.frame(
      region = "USA", supplysector = elec_gen_names, output.unit = "EJ", input.unit = "EJ", price.unit = "1975$/GJ",
      logit.year.fillout = min( model_base_years ), logit.exponent = grid_region_logit, logit.type=grid_region_logit_type )
    L223.SectorLogitTables_USAelec <- get_logit_fn_tables( L223.Supplysector_USAelec, names_SupplysectorLogitType,
        base.header="Supplysector_", include.equiv.table=F, write.all.regions=F )
    L223.Supplysector_USAelec <- L223.Supplysector_USAelec[, names_Supplysector ]

	#No need to read in subsector logit exponents, which are applied to the technology competition
	printlog( "L223.SubsectorShrwtFllt_USAelec: subsector (grid region) shareweights in USA electricity" )
	L223.SubsectorShrwtFllt_USAelec <- data.frame(
      region = "USA",
      supplysector = rep( elec_gen_names, times = length( grid_regions ) ),
      subsector = paste( sort( rep( grid_regions, times = length( elec_gen_names ) ) ), elec_gen_names, sep = " " ),
      year.fillout = min( model_base_years ), share.weight = 1,
      stringsAsFactors = F )

	printlog( "L223.SubsectorInterp_USAelec: subsector (grid region) shareweights in USA electricity" )
	L223.SubsectorInterp_USAelec <- data.frame(
      L223.SubsectorShrwtFllt_USAelec[ names_Subsector ],
      apply.to = "share-weight", from.year = max( model_base_years ), to.year = max( model_years ),
      interpolation.function = "fixed" )

    # NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
    L223.SubsectorLogit_USAelec <- data.frame(
          L223.SubsectorShrwtFllt_USAelec[ names_Subsector ],
          logit.year.fillout = min( model_base_years ), logit.exponent = grid_region_logit, logit.type=grid_region_logit_type )
    L223.SubsectorLogitTables_USAelec <- get_logit_fn_tables( L223.SubsectorLogit_USAelec, names_SubsectorLogitType,
        base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )
    L223.SubsectorLogit_USAelec <- L223.SubsectorLogit_USAelec[, names_SubsectorLogit ]

	printlog( "L223.TechShrwt_USAelec: technology shareweights, USA region" )
	L223.TechShrwt_USAelec <- repeat_and_add_vector(
      data.frame( L223.SubsectorShrwtFllt_USAelec[ names_Subsector ],
                  technology = L223.SubsectorShrwtFllt_USAelec$subsector, stringsAsFactors = F ),
      Y, model_years )
	L223.TechShrwt_USAelec$share.weight <- 1

	printlog( "L223.TechCoef_USAelec: technology coefficients and market names, USA region")
	L223.TechCoef_USAelec <- L223.TechShrwt_USAelec[ names_TechYr ]
	L223.TechCoef_USAelec[[input]] <- L223.TechCoef_USAelec[[supp]]
	L223.TechCoef_USAelec$coefficient <- 1
	L223.TechCoef_USAelec$market.name <- substr( L223.TechCoef_USAelec[[tech]], 1, nchar( L223.TechCoef_USAelec[[subs]] ) - nchar( L223.TechCoef_USAelec[[supp]]) - 1 )

	printlog( "L223.Production_USAelec: calibrated electricity production in USA (consuming output of grid subregions)" )
	L223.out_EJ_sR_elec.melt <- interpolate_and_melt( L1232.out_EJ_sR_elec, model_base_years, value.name = "calOutputValue", digits = digits_calOutput )
	L223.out_EJ_sR_elec.melt[[supp]] <- calibrated_techs[[supp]][
      match( L223.out_EJ_sR_elec.melt$sector, calibrated_techs$sector ) ]
	L223.out_EJ_sR_elec.melt[[subs]] <- paste( L223.out_EJ_sR_elec.melt$grid_region, L223.out_EJ_sR_elec.melt[[supp]], sep = " " )

	L223.Production_USAelec <- subset( L223.TechCoef_USAelec[ names_TechYr ], year %in% model_base_years )
	L223.Production_USAelec$calOutputValue <- L223.out_EJ_sR_elec.melt$calOutputValue[
      match( vecpaste( L223.Production_USAelec[ c( supp, subs, Y ) ] ),
             vecpaste( L223.out_EJ_sR_elec.melt[ c( supp, subs, Y ) ] ) ) ]
	L223.Production_USAelec$share.weight.year <- L223.Production_USAelec$year
	L223.Production_USAelec <- set_subsector_shrwt( L223.Production_USAelec )
	L223.Production_USAelec$tech.share.weight <- ifelse( L223.Production_USAelec$calOutputValue == 0, 0, 1 )
}

printlog( "PART 2: THE FERC REGIONS" )
printlog( "NOTE: FERC regions function in similar fashion to the USA region: competing electricity from subregions" )
printlog( "L223.Supplysector_elec_FERC: supplysector for electricity sector in the USA region, including logit exponent between grid regions" )
printlog( "NOTE: using the same logit exponent for states within FERC region as for FERC regions within the USA" )
L223.Supplysector_elec_FERC <- data.frame(
      region = sort( rep( grid_regions, times = length( elec_gen_names ) ) ),
      supplysector = rep( elec_gen_names, times = length( grid_regions ) ),
      output.unit = "EJ", input.unit = "EJ", price.unit = "1975$/GJ",
      logit.year.fillout = min( model_base_years ), logit.exponent = grid_region_logit, logit.type=grid_region_logit_type )
L223.SectorLogitTables_elec_FERC <- get_logit_fn_tables( L223.Supplysector_elec_FERC, names_SupplysectorLogitType,
    base.header="Supplysector_", include.equiv.table=F, write.all.regions=F )
L223.Supplysector_elec_FERC <- L223.Supplysector_elec_FERC[, names_Supplysector ]

printlog( "L223.SubsectorShrwtFllt_elec_FERC: subsector (grid region) shareweights in USA electricity" )
L223.SubsectorShrwtFllt_elec_FERC <- data.frame(
      region = rep( states_subregions$grid_region, times = length( elec_gen_names ) ),
      supplysector = sort( rep( elec_gen_names, times = nrow( states_subregions ) ) ),
      stringsAsFactors = F )
L223.SubsectorShrwtFllt_elec_FERC[[subs]] <- paste(
      rep( states_subregions$state, times = length( elec_gen_names ) ),
      L223.SubsectorShrwtFllt_elec_FERC[[supp]], sep = " " )
L223.SubsectorShrwtFllt_elec_FERC$year.fillout <- min( model_base_years )
L223.SubsectorShrwtFllt_elec_FERC$share.weight <- 1
L223.SubsectorShrwtFllt_elec_FERC <- L223.SubsectorShrwtFllt_elec_FERC[ order( L223.SubsectorShrwtFllt_elec_FERC[[reg]] ), ]

printlog( "L223.SubsectorInterp_elec_FERC: subsector (grid region) shareweights in USA electricity" )
L223.SubsectorInterp_elec_FERC <- data.frame(
      L223.SubsectorShrwtFllt_elec_FERC[ names_Subsector ],
      apply.to = "share-weight", from.year = max( model_base_years ), to.year = max( model_years ),
      interpolation.function = "fixed" )

# NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
L223.SubsectorLogit_elec_FERC <- data.frame(
      L223.SubsectorShrwtFllt_elec_FERC[ names_Subsector ],
      logit.year.fillout = min( model_base_years ), logit.exponent = grid_region_logit, logit.type=grid_region_logit_type )
L223.SubsectorLogitTables_elec_FERC <- get_logit_fn_tables( L223.SubsectorLogit_elec_FERC, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )
L223.SubsectorLogit_elec_FERC <- L223.SubsectorLogit_elec_FERC[, names_SubsectorLogit ]

printlog( "L223.TechShrwt_elec_FERC: technology shareweights, USA region" )
L223.TechShrwt_elec_FERC <- repeat_and_add_vector(
      data.frame( L223.SubsectorShrwtFllt_elec_FERC[ names_Subsector ],
                  technology = L223.SubsectorShrwtFllt_elec_FERC$subsector, stringsAsFactors = F ),
      Y, model_years )
L223.TechShrwt_elec_FERC$share.weight <- 1

printlog( "L223.TechCoef_elec_FERC: technology coefficients and market names, USA region")
L223.TechCoef_elec_FERC <- L223.TechShrwt_elec_FERC[ names_TechYr ]
L223.TechCoef_elec_FERC[[input]] <- L223.TechCoef_elec_FERC[[supp]]
L223.TechCoef_elec_FERC$coefficient <- 1
L223.TechCoef_elec_FERC$market.name <- substr( L223.TechCoef_elec_FERC[[tech]], 1, nchar( L223.TechCoef_elec_FERC[[subs]] ) - nchar( L223.TechCoef_elec_FERC[[supp]]) - 1 )

printlog( "Create a L223.PassThroughSector_elec dataframe" )
#Create a L223.PassThroughSector_elec dataframe (to be converted into a csv table later). The marginal revenue sector is the 
#region's electricity sector whereas the marginal revenue market is the grid region.
L223.PassThroughSector_elec <- data.frame(region=states_subregions$state, passthrough.sector="electricity",marginal.revenue.sector = "electricity", marginal.revenue.market = states_subregions$grid_region)

printlog( "Create a L223.PassThroughTech_elec_FERC dataframe" )
#Create a L223.PassThroughTech_elec_FERC dataframe (to be converted into a csv table later). This one should contain 
#region, supplysector, subsector, technology for the grid regions to which electricity produced in states is passed through. 
#Note that the "technology" in this data-frame will be called "passthrough technology"
L223.PassThroughTech_elec_FERC <- data.frame(region=L223.TechShrwt_elec_FERC$region, 
                                             supplysector = L223.TechShrwt_elec_FERC$supplysector, 
                                             subsector = L223.TechShrwt_elec_FERC$subsector, 
                                             technology = L223.TechShrwt_elec_FERC$technology)

printlog( "L223.Production_elec_FERC: calibrated electricity production in USA (consuming output of grid subregions)" )
L1231.out_EJ_state_elec_F_tech.melt <- interpolate_and_melt( L1231.out_EJ_state_elec_F_tech, model_base_years, value.name = "calOutputValue", digits = digits_calOutput )
L1231.out_EJ_state_elec_F_tech.melt[[supp]] <- calibrated_techs[[supp]][
      match( L1231.out_EJ_state_elec_F_tech.melt$sector, calibrated_techs$sector ) ]
L1231.out_EJ_state_elec_F_tech.melt[[subs]] <- paste( L1231.out_EJ_state_elec_F_tech.melt$state, L1231.out_EJ_state_elec_F_tech.melt[[supp]], sep = " " )

#This needs to be aggregated to the supplysector level
L223.out_EJ_state_elec.melt <- aggregate( L1231.out_EJ_state_elec_F_tech.melt[ "calOutputValue" ],
      by=as.list( L1231.out_EJ_state_elec_F_tech.melt[ c( supp, subs, Y ) ] ), sum )
L223.Production_elec_FERC <- subset( L223.TechCoef_elec_FERC[ names_TechYr ], year %in% model_base_years )
L223.Production_elec_FERC$calOutputValue <- L223.out_EJ_state_elec.melt$calOutputValue[
      match( vecpaste( L223.Production_elec_FERC[ c( supp, subs, Y ) ] ),
             vecpaste( L223.out_EJ_state_elec.melt[ c( supp, subs, Y ) ] ) ) ]
L223.Production_elec_FERC$share.weight.year <- L223.Production_elec_FERC$year
L223.Production_elec_FERC <- set_subsector_shrwt( L223.Production_elec_FERC )
L223.Production_elec_FERC$tech.share.weight <- ifelse( L223.Production_elec_FERC$calOutputValue == 0, 0, 1 )

printlog( "Socioeconomic information in the electricity grid regions (required for GCAM to run with these regions)" )
printlog( "L223.InterestRate_FERC: Interest rates in the FERC grid regions" )
L223.InterestRate_FERC <- data.frame( region = grid_regions, interest.rate = default_interest.rate )

printlog( "L223.Pop_FERC: Population" )
L223.Pop_FERC <- data.frame(
      region = rep( grid_regions, times = length( model_years ) ),
      year = sort( rep( model_years, times = length( grid_regions ) ) ),
      totalPop = 1 )

printlog( "L223.BaseGDP_FERC: Base GDP in FERC grid regions" )
L223.BaseGDP_FERC <- data.frame( region = grid_regions, baseGDP = 1 )

printlog( "L223.LaborForceFillout_FERC: labor force in the grid regions" )
L223.LaborForceFillout_FERC <- data.frame(
      region = grid_regions,
      year.fillout = min( model_base_years ),
      laborforce = default_laborforce )

printlog( "PART 3: THE STATES" )
printlog( "All tables for which processing is identical are done in a for loop")
printlog( "This applies to the supplysectors, subsectors, and stub tech characteristics of the states")

printlog("Writing Passthrough Sector files first")
#Writing Passthrough Sector files first. Equivalent tag names specified earlier should take care of consistency across xml files
#as long as the L223.SectorNodeEquiv and L223TechNodeEquiv files are read in first in the xml batch file (batch_electricity_USA.xml)

write_mi_data( L223.SectorNodeEquiv, "EQUIV_TABLE", "GCAMUSA_LEVEL2_DATA", "L223.SectorNodeEquiv", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.TechNodeEquiv, "EQUIV_TABLE", "GCAMUSA_LEVEL2_DATA", "L223.TechNodeEquiv", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.PassThroughSector_elec, "PassThroughSector", "GCAMUSA_LEVEL2_DATA", "L223.PassthroughSector_elec_USA", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.PassThroughTech_elec_FERC, "PassThroughTech", "GCAMUSA_LEVEL2_DATA", "L223.PassthroughTech_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )

printlog( "NOTE: writing out the tables in this step as well")
L223.tables <- list( L223.Supplysector_elec = L223.Supplysector_elec,
                     L223.ElecReserve = L223.ElecReserve,
                     L223.SubsectorLogit_elec = L223.SubsectorLogit_elec,
                     L223.SubsectorShrwt_elec = L223.SubsectorShrwt_elec,
                     L223.SubsectorShrwtFllt_elec = L223.SubsectorShrwtFllt_elec,
                     L223.SubsectorShrwt_nuc = L223.SubsectorShrwt_nuc,
                     L223.SubsectorShrwt_renew = L223.SubsectorShrwt_renew,
                     L223.SubsectorInterp_elec = L223.SubsectorInterp_elec,
                     L223.SubsectorInterpTo_elec = L223.SubsectorInterpTo_elec,
                     L223.StubTech_elec = L223.StubTech_elec,
                     L223.StubTechEff_elec = L223.StubTechEff_elec,
                     L223.StubTechCapFactor_elec = L223.StubTechCapFactor_elec )

# The logit functions should be processed before any other table that needs to read logit exponents
L223.tables <- c( read_logit_fn_tables( "ENERGY_LEVEL2_DATA", "L223.Supplysector_", skip=4, include.equiv.table=T ),
                  read_logit_fn_tables( "ENERGY_LEVEL2_DATA", "L223.SubsectorLogit_", skip=4, include.equiv.table=F ),
                  L223.tables )




for( i in 1:length( L223.tables ) ){
  if( !is.null( L223.tables[[i]] ) ){
	objectname <- paste0( names( L223.tables[i] ), "_USA" )
    if( substr( objectname, 6, 16 ) == "EQUIV_TABLE" || nrow( subset( L223.tables[[i]], region == "USA" ) ) == 0 ) {
        # Just use the object as is
        object <- L223.tables[[i]]
    } else {
        object <- write_to_all_states( subset( L223.tables[[i]], region == "USA" ), names( L223.tables[[i]] ) )
    }
	if( subs %in% names( object ) ) object <- subset( object, !paste( region, subsector ) %in% geo_states_noresource )
#Re-set markets from USA to regional markets, if called for in the GCAM-USA assumptions
	if( use_regional_fuel_markets & "market.name" %in% names( object ) ){
		object$market.name[ object[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
		      match( object$region[ object[[input]] %in% regional_fuel_markets], states_subregions$state ) ]
	}
	assign( objectname, object )
    curr_table_name <- names( L223.tables )[i]
    IDstringendpoint <- if( grepl( "_", curr_table_name ) & !grepl( 'EQUIV_TABLE', curr_table_name ) & !grepl( '-logit$', curr_table_name ) ) {
        regexpr( "_", curr_table_name, fixed = T ) - 1
    } else {
        nchar( curr_table_name )
    }
    IDstring <- substr( curr_table_name, 6, IDstringendpoint )
	write_mi_data( object, IDstring, "GCAMUSA_LEVEL2_DATA", objectname, "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
	  }
  }

printlog( "NOTE: Modifying the shareweight path for nuclear to include state preferences" )
L223.out_EJ_state_elec_nuc <- L1231.out_EJ_state_elec_F_tech[ L1231.out_EJ_state_elec_F_tech$fuel == "nuclear", c( state, X_final_historical_year ) ]
L223.out_EJ_state_elec <- aggregate( L1231.out_EJ_state_elec_F_tech[ X_final_historical_year ],
      by=as.list( L1231.out_EJ_state_elec_F_tech[ state ] ), sum )
L223.share_USA_elec <- sum( L223.out_EJ_state_elec_nuc[[X_final_historical_year]] ) / sum( L223.out_EJ_state_elec[[X_final_historical_year]] )
L223.share_state_elec_nuc <- data.frame( state = L223.out_EJ_state_elec_nuc$state, share = L223.out_EJ_state_elec_nuc[[X_final_historical_year ]] /
      L223.out_EJ_state_elec[[X_final_historical_year ]][ match( L223.out_EJ_state_elec_nuc$state, L223.out_EJ_state_elec$state ) ] )
L223.state_nuc_pref <- data.frame(
      state = L223.share_state_elec_nuc$state,
      pref = L223.share_state_elec_nuc[["share"]] / L223.share_USA_elec )
#Just set some bounds on the share weight multiplier
L223.state_nuc_pref$share.weight.mult <- pmax( pmin( L223.state_nuc_pref$pref, 2 ), 0.1 )

#Set VT to zero because they have already shut down their only nuclear plant which used to account for ~70% of generation
L223.state_nuc_pref$share.weight.mult[ L223.state_nuc_pref$state == "VT" ] <- 0
L223.SubsectorShrwt_nuc_USA$share.weight <- round( L223.SubsectorShrwt_nuc_USA$share.weight *
      L223.state_nuc_pref$share.weight.mult[ match( L223.SubsectorShrwt_nuc_USA$region, L223.state_nuc_pref$state ) ],
      digits_cost )

printlog( "Stub technology information for state electricity generation" )
# calibration
L223.in_EJ_state_elec_F_tech <- interpolate_and_melt( L1231.in_EJ_state_elec_F_tech, model_base_years, value.name = "calibrated.value", digits = digits_calOutput )
L223.in_EJ_state_elec_F_tech$region <- L223.in_EJ_state_elec_F_tech$state
L223.in_EJ_state_elec_F_tech[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( paste( L223.in_EJ_state_elec_F_tech$sector, L223.in_EJ_state_elec_F_tech$fuel, L223.in_EJ_state_elec_F_tech$technology ),
             paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ),
      c( "supplysector", "subsector", "technology" ) ]
L223.in_EJ_state_elec_F_tech <- subset( L223.in_EJ_state_elec_F_tech, calibrated_techs$calibration[
      match( paste( sector, fuel, technology ), paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ) ] == "input" )

printlog( "NOTE: Fixed output is assumed to apply in all historical years, regardless of final calibration year")
L1231.out_EJ_state_elec_F_tech <- interpolate_and_melt( L1231.out_EJ_state_elec_F_tech, model_years[ model_years %in% historical_years ],
      value.name = "calOutputValue", digits = digits_calOutput )
L1231.out_EJ_state_elec_F_tech$region <- L1231.out_EJ_state_elec_F_tech$state
L1231.out_EJ_state_elec_F_tech[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( paste( L1231.out_EJ_state_elec_F_tech$sector, L1231.out_EJ_state_elec_F_tech$fuel, L1231.out_EJ_state_elec_F_tech$technology ),
             paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ),
      c( "supplysector", "subsector", "technology" ) ]
L223.fixout_EJ_state_elec_F_tech <- subset( L1231.out_EJ_state_elec_F_tech, calibrated_techs$calibration[
      match( paste( sector, fuel, technology ), paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ) ] == "fixed output" )
L223.calout_EJ_state_elec_F_tech <- subset( L1231.out_EJ_state_elec_F_tech,
      calibrated_techs$calibration[
          match( paste( sector, fuel, technology ), paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ) ] != "fixed output" &
          year %in% model_base_years )

##NOTE: the model seems to calibrate better from the output rather than the input (less chance of aggregated rounding errors in tiny states)
#printlog( "L223.StubTechCalInput_elec_USA: calibrated input of electricity generation technologies")
#Note that there is no need to specify which stub technologies are intermittent
#L223.StubTechCalInput_elec_USA <- L223.in_EJ_state_elec_F_tech[ names_StubTechYr ]
#L223.StubTechCalInput_elec_USA[[input]] <- calibrated_techs[[input]][ 
#      match( vecpaste( L223.StubTechCalInput_elec_USA[ c( supp, subs, "stub.technology" ) ] ),
#             vecpaste( calibrated_techs[ c( supp, subs, tech ) ] ) ) ]
#L223.StubTechCalInput_elec_USA$calibrated.value <- L223.in_EJ_state_elec_F_tech$calibrated.value
#L223.StubTechCalInput_elec_USA$share.weight.year <- L223.StubTechCalInput_elec_USA$year
#L223.StubTechCalInput_elec_USA <- set_subsector_shrwt( L223.StubTechCalInput_elec_USA, value.name = "calibrated.value" )
#L223.StubTechCalInput_elec_USA$tech.share.weight <- ifelse( L223.StubTechCalInput_elec_USA$calibrated.value > 0, 1, 0 )
#L223.StubTechCalInput_elec_USA <- subset( L223.StubTechCalInput_elec_USA, !paste( region, subsector ) %in% geo_states_noresource )

printlog( "L223.StubTechFixOut_elec_USA: fixed output of electricity generation technologies")
L223.StubTechFixOut_elec_USA <- L223.fixout_EJ_state_elec_F_tech[ names_StubTechYr ]
L223.StubTechFixOut_elec_USA$fixedOutput <- round( L223.fixout_EJ_state_elec_F_tech$calOutputValue, digits_calOutput )
L223.StubTechFixOut_elec_USA$share.weight.year <- L223.StubTechFixOut_elec_USA$year
L223.StubTechFixOut_elec_USA$subs.share.weight <- 0
L223.StubTechFixOut_elec_USA$tech.share.weight <- 0

#Adding in future hydropower generation here
printlog( "L223.StubTechFixOut_hydro_USA: fixed output of future hydropower")
#TO DO: This just holds it constant for now; at some point, should downscale of the (almost completely flat) nation-level projection
L223.StubTechFixOut_hydro_USA <- repeat_and_add_vector(
      subset( L223.StubTechFixOut_elec_USA, grepl( "hydro", stub.technology ) & year == max( historical_years ) ),
      Y, model_future_years )

printlog( "L223.StubTechProd_elec_USA: calibrated output of electricity generation technologies" )
L223.StubTechProd_elec_USA <- L223.calout_EJ_state_elec_F_tech[ c( names_StubTechYr, "calOutputValue" ) ]
L223.StubTechProd_elec_USA$share.weight.year <- L223.StubTechProd_elec_USA$year
L223.StubTechProd_elec_USA <- set_subsector_shrwt( L223.StubTechProd_elec_USA, value.name="calOutputValue" )
L223.StubTechProd_elec_USA$share.weight <- ifelse( L223.StubTechProd_elec_USA$calOutputValue > 0, 1, 0 )
L223.StubTechProd_elec_USA <- subset( L223.StubTechProd_elec_USA, !paste( region, subsector ) %in% geo_states_noresource )

printlog( "L223.StubTechMarket_elec_USA: market names of inputs to state electricity sectors" )
L223.StubTechMarket_elec_USA <- repeat_and_add_vector( L223.StubTech_elec_USA, Y, model_years )
L223.StubTechMarket_elec_USA[[input]] <- A23.globaltech_eff[[input]][
      match( vecpaste( L223.StubTechMarket_elec_USA[ c( supp, subs, "stub.technology" ) ] ),
             vecpaste( A23.globaltech_eff[ c( supp, subs, tech ) ] ) ) ]
#Remove NA rows for hydro
L223.StubTechMarket_elec_USA <- subset( L223.StubTechMarket_elec_USA, complete.cases( L223.StubTechMarket_elec_USA ) )
L223.StubTechMarket_elec_USA$market.name <- "USA"
L223.StubTechMarket_elec_USA$market.name[ L223.StubTechMarket_elec_USA[[input]] %in% c( state_renewable_resources, state_unlimited_resources ) ] <-
      L223.StubTechMarket_elec_USA$region[ L223.StubTechMarket_elec_USA[[input]] %in% c( state_renewable_resources, state_unlimited_resources ) ]
L223.StubTechMarket_elec_USA <- subset( L223.StubTechMarket_elec_USA, !paste( region, subsector ) %in% geo_states_noresource )
if( use_regional_fuel_markets ){
	L223.StubTechMarket_elec_USA$market.name[ L223.StubTechMarket_elec_USA[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
	      match( L223.StubTechMarket_elec_USA$region[ L223.StubTechMarket_elec_USA[[input]] %in% regional_fuel_markets ],
	             states_subregions$state ) ]
}

printlog( "L223.StubTechMarket_backup_USA: market names of backup inputs to state electricity sectors" )
L223.GlobalIntTechBackup_elec[ c( supp, subs ) ] <- L223.GlobalIntTechBackup_elec[ c( "sector.name", "subsector.name" ) ]
L223.StubTechMarket_backup_USA <- repeat_and_add_vector( L223.GlobalIntTechBackup_elec[ c( s_s_t_i, Y ) ], reg, states )
L223.StubTechMarket_backup_USA$market.name <- "USA"
L223.StubTechMarket_backup_USA$stub.technology <- L223.StubTechMarket_backup_USA$technology
L223.StubTechMarket_backup_USA <- L223.StubTechMarket_backup_USA[ names_StubTechMarket ]

#The backup electric market is only set here if regional electricity markets are not used (i.e., one national grid)
if( !use_regional_elec_markets ){
	printlog( "L223.StubTechElecMarket_backup_USA: market name of electricity sector for backup calculations" )
	L223.StubTechElecMarket_backup_USA <- L223.StubTechMarket_backup_USA[ names_StubTechYr ]
	L223.StubTechElecMarket_backup_USA$electric.sector.market <- "USA"
}

printlog( "L223.StubTechCapFactor_elec_wind_USA: capacity factors for wind electricity in the states" )
#Just use the subsector for matching - technologies include storage technologies as well
L114.CapacityFactor_wind_state[ c( supp, subs ) ] <- calibrated_techs[
      match( vecpaste( L114.CapacityFactor_wind_state[ S_F ] ),
             vecpaste( calibrated_techs[ S_F ] ) ),
      c( supp, subs ) ]
L223.StubTechCapFactor_elec_wind_USA <- repeat_and_add_vector(
      subset( L223.StubTechCapFactor_elec, region == "USA" &
              paste( supplysector, subsector ) %in% vecpaste( L114.CapacityFactor_wind_state[ c( supp, subs ) ] ) ),
      reg, states )
L223.StubTechCapFactor_elec_wind_USA$capacity.factor.capital <- round(
      L114.CapacityFactor_wind_state$capacity.factor[
         match( vecpaste( L223.StubTechCapFactor_elec_wind_USA[ c( reg, supp, subs ) ] ),
                vecpaste( L114.CapacityFactor_wind_state[ c( state, supp, subs ) ] ) ) ],
      digits_capacity_factor )
L223.StubTechCapFactor_elec_wind_USA$capacity.factor.OM <- L223.StubTechCapFactor_elec_wind_USA$capacity.factor.capital
L223.StubTechCapFactor_elec_wind_USA <- L223.StubTechCapFactor_elec_wind_USA[ names_StubTechCapFactor ]

printlog( "L223.StubTechCapFactor_elec_solar_USA: capacity factors by state and solar electric technology" )
L223.CapFacScaler_solar_state <- rbind( L119.CapFacScaler_PV_state, L119.CapFacScaler_CSP_state )
L223.CapFacScaler_solar_state[ s_s_t ] <- calibrated_techs[
      match( vecpaste( L223.CapFacScaler_solar_state[ S_F ] ),
             vecpaste( calibrated_techs[ S_F ] ) ),
      s_s_t ]
#Just use the subsector for matching - technologies include storage technologies as well
L223.StubTechCapFactor_elec_solar_USA <- repeat_and_add_vector(
      subset( L223.StubTechCapFactor_elec, region == "USA" &
              paste( supplysector, subsector ) %in% vecpaste( L223.CapFacScaler_solar_state[ c( supp, subs ) ] ) ),
      reg, states )
#For matching capacity factors to technologies, need to have a name that matches what's in the capacity factor table (which doesn't include storage techs)
L223.StubTechCapFactor_elec_solar_USA$match_tech <- sub( "_storage", "", L223.StubTechCapFactor_elec_solar_USA$stub.technology )
L223.StubTechCapFactor_elec_solar_USA$capacity.factor.capital <- round(
      L223.StubTechCapFactor_elec_solar_USA$capacity.factor.capital * L223.CapFacScaler_solar_state$scaler[
         match( vecpaste( L223.StubTechCapFactor_elec_solar_USA[ c( reg, supp, subs, "match_tech" ) ] ),
                vecpaste( L223.CapFacScaler_solar_state[ c( state, s_s_t ) ] ) ) ],
      digits_cost )
L223.StubTechCapFactor_elec_solar_USA$capacity.factor.OM <- L223.StubTechCapFactor_elec_solar_USA$capacity.factor.capital
L223.StubTechCapFactor_elec_solar_USA <- L223.StubTechCapFactor_elec_solar_USA[ names_StubTechCapFactor ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
#The USA region tables are only relevant when demands are resolved only at the level of the whole USA
if( !use_regional_elec_markets ){	
for( curr_table in names ( L223.SectorLogitTables_USAelec ) ) {
write_mi_data( L223.SectorLogitTables_USAelec[[ curr_table ]]$data, L223.SectorLogitTables_USAelec[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SectorLogitTables_USAelec[[ curr_table ]]$header, "_USA" ), "GCAMUSA_XML_BATCH",
    "batch_electricity_USA.xml" )
}
write_mi_data( L223.Supplysector_USAelec, "Supplysector", "GCAMUSA_LEVEL2_DATA", "L223.Supplysector_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.SubsectorShrwtFllt_USAelec, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L223.SubsectorShrwtFllt_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.SubsectorInterp_USAelec, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L223.SubsectorInterp_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
for( curr_table in names ( L223.SubsectorLogitTables_USAelec ) ) {
write_mi_data( L223.SubsectorLogitTables_USAelec[[ curr_table ]]$data, L223.SubsectorLogitTables_USAelec[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SubsectorLogitTables_USAelec[[ curr_table ]]$header, "_USA" ), "GCAMUSA_XML_BATCH",
    "batch_electricity_USA.xml" )
}
write_mi_data( L223.SubsectorLogit_USAelec, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L223.SubsectorLogit_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.TechShrwt_USAelec, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L223.TechShrwt_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.TechCoef_USAelec, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L223.TechCoef_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.Production_USAelec, "Production", "GCAMUSA_LEVEL2_DATA", "L223.Production_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
}

for( curr_table in names ( L223.SectorLogitTables_elec_FERC ) ) {
write_mi_data( L223.SectorLogitTables_elec_FERC[[ curr_table ]]$data, L223.SectorLogitTables_elec_FERC[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SectorLogitTables_elec_FERC[[ curr_table ]]$header, "_FERC" ), "GCAMUSA_XML_BATCH",
    "batch_electricity_USA.xml" )
}
write_mi_data( L223.Supplysector_elec_FERC, "Supplysector", "GCAMUSA_LEVEL2_DATA", "L223.Supplysector_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.SubsectorShrwtFllt_elec_FERC, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L223.SubsectorShrwtFllt_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.SubsectorInterp_elec_FERC, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L223.SubsectorInterp_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
for( curr_table in names ( L223.SubsectorLogitTables_elec_FERC ) ) {
write_mi_data( L223.SubsectorLogitTables_elec_FERC[[ curr_table ]]$data, L223.SubsectorLogitTables_elec_FERC[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SubsectorLogitTables_elec_FERC[[ curr_table ]]$header, "_FERC" ), "GCAMUSA_XML_BATCH",
    "batch_electricity_USA.xml" )
}
write_mi_data( L223.SubsectorLogit_elec_FERC, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L223.SubsectorLogit_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.TechShrwt_elec_FERC, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L223.TechShrwt_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.TechCoef_elec_FERC, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L223.TechCoef_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.Production_elec_FERC, "Production", "GCAMUSA_LEVEL2_DATA", "L223.Production_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )

write_mi_data( L223.InterestRate_FERC, "InterestRate", "GCAMUSA_LEVEL2_DATA", "L223.InterestRate_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.Pop_FERC, "Pop", "GCAMUSA_LEVEL2_DATA", "L223.Pop_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.BaseGDP_FERC, "BaseGDP", "GCAMUSA_LEVEL2_DATA", "L223.BaseGDP_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.LaborForceFillout_FERC, "LaborForceFillout", "GCAMUSA_LEVEL2_DATA", "L223.LaborForceFillout_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )

#write_mi_data( L223.StubTechCalInput_elec_USA, "StubTechCalInput", "GCAMUSA_LEVEL2_DATA", "L223.StubTechCalInput_elec_USA", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.SubsectorShrwt_nuc_USA, "SubsectorShrwt", "GCAMUSA_LEVEL2_DATA", "L223.SubsectorShrwt_nuc_USA", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.StubTechFixOut_elec_USA, "StubTechFixOut", "GCAMUSA_LEVEL2_DATA", "L223.StubTechFixOut_elec_USA", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.StubTechFixOut_hydro_USA, "StubTechFixOut", "GCAMUSA_LEVEL2_DATA", "L223.StubTechFixOut_hydro_USA", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.StubTechProd_elec_USA, "StubTechProd", "GCAMUSA_LEVEL2_DATA", "L223.StubTechProd_elec_USA", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.StubTechMarket_elec_USA, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L223.StubTechMarket_elec_USA", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.StubTechMarket_backup_USA, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L223.StubTechMarket_backup_USA", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
if( !use_regional_elec_markets ){	
	write_mi_data( L223.StubTechElecMarket_backup_USA, "StubTechElecMarket", "GCAMUSA_LEVEL2_DATA", "L223.StubTechElecMarket_backup_USA", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
}
write_mi_data( L223.StubTechCapFactor_elec_wind_USA, "StubTechCapFactor", "GCAMUSA_LEVEL2_DATA", "L223.StubTechCapFactor_elec_wind_USA", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L223.StubTechCapFactor_elec_solar_USA, "StubTechCapFactor", "GCAMUSA_LEVEL2_DATA", "L223.StubTechCapFactor_elec_solar_USA", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml", "GCAMUSA_XML_FINAL", "electricity_USA.xml", "", xml_tag="outFile" )

logstop()
