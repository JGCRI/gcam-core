# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
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
logstart( "L226.en_distribution_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Primary energy prices in the states, and electricity T&D sectors" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_gcamusa_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions")
A21.sector <- readdata( "ENERGY_ASSUMPTIONS", "A21.sector" )
A26.sector <- readdata( "ENERGY_ASSUMPTIONS", "A26.sector" )
EIA_state_energy_prices <- readdata( "GCAMUSA_LEVEL0_DATA", "EIA_state_energy_prices" )
L202.CarbonCoef <- readdata( "ENERGY_LEVEL2_DATA", "L202.CarbonCoef", skip = 4 )

#Electricity T&D files
L226.Supplysector_en <- readdata( "ENERGY_LEVEL2_DATA", "L226.Supplysector_en", skip = 4 )
L226.SubsectorLogit_en <- readdata( "ENERGY_LEVEL2_DATA", "L226.SubsectorLogit_en", skip = 4 )
L226.SubsectorShrwt_en <- readdata( "ENERGY_LEVEL2_DATA", "L226.SubsectorShrwt_en", skip = 4, must.exist = FALSE )
L226.SubsectorShrwtFllt_en <- readdata( "ENERGY_LEVEL2_DATA", "L226.SubsectorShrwtFllt_en", skip = 4, must.exist = FALSE )
L226.SubsectorInterp_en <- readdata( "ENERGY_LEVEL2_DATA", "L226.SubsectorInterp_en", skip = 4, must.exist = FALSE )
L226.SubsectorInterpTo_en <- readdata( "ENERGY_LEVEL2_DATA", "L226.SubsectorInterpTo_en", skip = 4, must.exist = FALSE )
L226.GlobalTechCost_en <- readdata( "ENERGY_LEVEL2_DATA", "L226.GlobalTechCost_en", skip = 4 )
L226.GlobalTechShrwt_en <- readdata( "ENERGY_LEVEL2_DATA", "L226.GlobalTechShrwt_en", skip = 4 )
L226.StubTechCoef_electd <- readdata( "ENERGY_LEVEL2_DATA", "L226.StubTechCoef_electd", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Supplysector information
printlog( "PART 1: FUEL HANDLING AND DELIVERY SECTORS")
printlog( "L226.Supplysector_en_USA: Supply sector information for energy handling and delivery sectors" )
printlog( "NOTE: Currently using FERC regions as a proxy for regional energy markets")
energy_market_regions <- sort( unique( states_subregions$grid_region ) )
L226.Supplysector_en_USA <- repeat_and_add_vector(
      rbind( subset( A21.sector[ names( A21.sector ) %in% names_Supplysector ], supplysector %in% regional_fuel_markets ),
             subset( A26.sector[ names( A26.sector ) %in% names_Supplysector ], supplysector %in% regional_fuel_markets ) ),
      reg, energy_market_regions )
L226.Supplysector_en_USA$logit.year.fillout <- min( model_base_years )
L226.Supplysector_en_USA <- L226.Supplysector_en_USA[ names_Supplysector ]

printlog( "L226.SubsectorShrwtFllt_en_USA: subsector shareweights of energy handling and delivery" )
L226.SubsectorShrwtFllt_en_USA <- L226.Supplysector_en_USA
L226.SubsectorShrwtFllt_en_USA$subsector <- L226.SubsectorShrwtFllt_en_USA$supplysector
L226.SubsectorShrwtFllt_en_USA$year.fillout <- min( model_base_years )
L226.SubsectorShrwtFllt_en_USA$share.weight <- 1
L226.SubsectorShrwtFllt_en_USA <- L226.SubsectorShrwtFllt_en_USA[ names_SubsectorShrwtFllt ]

printlog( "NOTE: can't use stub technologies because these would inherit the wrong energy-inputs" )
printlog( "L226.TechShrwt_en_USA: technology shareweights of energy handling and delivery" )
L226.TechShrwt_en_USA <- repeat_and_add_vector( L226.SubsectorShrwtFllt_en_USA[ names_Subsector ], Y, model_years )
L226.TechShrwt_en_USA[[tech]] <- L226.TechShrwt_en_USA[[subs]]
L226.TechShrwt_en_USA$share.weight <- 1
L226.TechShrwt_en_USA <- L226.TechShrwt_en_USA[ c( names_TechYr, "share.weight" ) ]

printlog( "L226.TechCoef_en_USA: technology coefficients and market names of energy handling and delivery" )
L226.TechCoef_en_USA <- L226.TechShrwt_en_USA[ names_TechYr ]
L226.TechCoef_en_USA[[input]] <- L226.TechCoef_en_USA[[supp]]
L226.TechCoef_en_USA$coefficient <- 1
L226.TechCoef_en_USA$market.name <- "USA"

printlog( "Calculating grid region specific cost adders" )
printlog( "NOTE: the average national costs are already accounted in the corresponding sectors of the USA; this table implements a price adjustment factor" )
EIA_state_energy_prices$grid_region <- states_subregions$grid_region[ match( EIA_state_energy_prices$State, states_subregions$state_name ) ]
EIA_state_energy_prices$coal_adj <- ( EIA_state_energy_prices$Coal - EIA_state_energy_prices$Coal[ EIA_state_energy_prices$State == "United States" ] ) *
      conv_2009_1975_USD * conv_btu_kJ
EIA_state_energy_prices$gas_adj <- ( EIA_state_energy_prices$Natural.gas - EIA_state_energy_prices$Natural.gas[ EIA_state_energy_prices$State == "United States" ] ) *
      conv_2009_1975_USD * conv_btu_kJ
printlog( "NOTE: using distillate fuel oil as proxy for liquid fuels to avoid composition bias in the petroleum total" )
# in other words, states with a lot of residual fuel would end up having lower apparent liquid fuel prices
EIA_state_energy_prices$liq_adj <- ( EIA_state_energy_prices$Distillate.fuel.oil - EIA_state_energy_prices$Distillate.fuel.oil[ EIA_state_energy_prices$State == "United States" ] ) *
      conv_2009_1975_USD * conv_btu_kJ

#In states with missing values for coal, assign the maximum price
EIA_state_energy_prices$coal_adj[ is.na( EIA_state_energy_prices$coal_adj ) ] <- max( EIA_state_energy_prices$coal_adj[ !is.na( EIA_state_energy_prices$coal_adj ) ] )

#For gas, the value in Hawaii is extremely high; just cap it at a max threshold
EIA_state_energy_prices$gas_adj[ EIA_state_energy_prices$State == "Hawaii" ] <- 5
L226.CostAdj_75USDGJ_FERC_F <- aggregate( EIA_state_energy_prices[ c( "coal_adj", "gas_adj", "liq_adj" )  ],
      by=as.list( EIA_state_energy_prices[ "grid_region" ] ), median )

printlog( "L226.TechCost_en_USA: cost adders" )
L226.TechCost_en_USA <- L226.TechShrwt_en_USA[ names_TechYr ]
L226.TechCost_en_USA$minicam.non.energy.input <- "regional price adjustment"
L226.TechCost_en_USA$input.cost <- NA
L226.TechCost_en_USA$input.cost[ grepl( "coal", L226.TechCost_en_USA[[supp]] ) ] <- L226.CostAdj_75USDGJ_FERC_F$coal_adj[
      match( L226.TechCost_en_USA$region[ grepl( "coal", L226.TechCost_en_USA[[supp]] ) ],
             L226.CostAdj_75USDGJ_FERC_F$grid_region ) ]
L226.TechCost_en_USA$input.cost[ grepl( "gas", L226.TechCost_en_USA[[supp]] ) ] <- L226.CostAdj_75USDGJ_FERC_F$gas_adj[
      match( L226.TechCost_en_USA$region[ grepl( "gas", L226.TechCost_en_USA[[supp]] ) ],
             L226.CostAdj_75USDGJ_FERC_F$grid_region ) ]
L226.TechCost_en_USA$input.cost[ grepl( "liquid", L226.TechCost_en_USA[[supp]] ) ] <- L226.CostAdj_75USDGJ_FERC_F$liq_adj[
      match( L226.TechCost_en_USA$region[ grepl( "liquid", L226.TechCost_en_USA[[supp]] ) ],
             L226.CostAdj_75USDGJ_FERC_F$grid_region ) ]
L226.TechCost_en_USA$input.cost <- round( L226.TechCost_en_USA$input.cost, digits_cost )

printlog( "L226.Ccoef: carbon coef for cost adder sectors." )
L226.Ccoef.USA <- subset( L202.CarbonCoef, region == "USA" )
L226.Ccoef <- unique( L226.TechCost_en_USA[, c( "region", "supplysector" ) ] )
L226.Ccoef$PrimaryFuelCO2Coef <- L226.Ccoef.USA$PrimaryFuelCO2Coef[
    match( L226.Ccoef$supplysector,
           L226.Ccoef.USA$PrimaryFuelCO2Coef.name ) ]

printlog( "PART 2: ELECTRICITY TRANSMISSION AND DISTRIBUTION" )
printlog( "L226.DeleteSupplysector_USAelec: Removing the electricity T&D sectors of the USA region" )
L226.DeleteSupplysector_USAelec <- data.frame( region = "USA", supplysector = elect_td_sectors )
write_mi_data( L226.DeleteSupplysector_USAelec, "DeleteSupplysector", "GCAMUSA_LEVEL2_DATA", "L226.DeleteSupplysector_USAelec", "GCAMUSA_XML_BATCH", "batch_electd_USA.xml" )

printlog( "All tables for which processing is identical are done in a for loop")
printlog( "This applies to the supplysectors, subsectors, and stub tech characteristics of the states")
printlog( "NOTE: writing out the tables in this step as well")
L226.tables <- list( L226.Supplysector_electd = L226.Supplysector_en,
                     L226.SubsectorLogit_electd = L226.SubsectorLogit_en,
                     L226.SubsectorShrwt_electd = L226.SubsectorShrwt_en,
                     L226.SubsectorShrwtFllt_electd = L226.SubsectorShrwtFllt_en,
                     L226.SubsectorInterp_electd = L226.SubsectorInterp_en,
                     L226.SubsectorInterpTo_electd = L226.SubsectorInterpTo_en )

for( i in 1:length( L226.tables ) ){
  if( !is.null( L226.tables[[i]] ) ){
	objectname <- paste0( names( L226.tables[i] ), "_USA" )
	object <- write_to_all_states( subset( L226.tables[[i]], region == "USA" & supplysector %in% elect_td_sectors ), names( L226.tables[[i]] ) )
	assign( objectname, object )
	IDstringendpoint <- if( grepl( "_", names( L226.tables )[i] ) ) { regexpr( "_", names( L226.tables )[i], fixed = T ) - 1
	                       } else nchar( names( L226.tables )[i] )
	IDstring <- substr( names( L226.tables )[i], 6, IDstringendpoint )
	write_mi_data( object, IDstring, "GCAMUSA_LEVEL2_DATA", objectname, "GCAMUSA_XML_BATCH", "batch_electd_USA.xml" )
	  }
  }

if( !use_regional_elec_markets ){
	printlog( "Using national elec markets. State elect_td sectors are treated as stub technologies" )
	L226.StubTechCoef_electd_USA <- write_to_all_states( subset( L226.StubTechCoef_electd, region == "USA" ), names_StubTechCoef )
	write_mi_data( L226.StubTechCoef_electd_USA, "StubTechCoef", "GCAMUSA_LEVEL2_DATA", "L226.StubTechCoef_electd_USA", "GCAMUSA_XML_BATCH", "batch_electd_USA.xml" )

}

if( use_regional_elec_markets ){
	printlog( "NOTE: using regional elec markets. The elect_td sectors can not use the global tech database as their input is different. Remaking" )
	L226.TechShrwt_electd_USA <- write_to_all_states( subset( L226.GlobalTechShrwt_en, sector.name %in% elect_td_sectors ), c( reg, names_GlobalTechYr, "share.weight" ) )
	names( L226.TechShrwt_electd_USA )[ names( L226.TechShrwt_electd_USA ) %in% c( "sector.name", "subsector.name" ) ] <- c( supp, subs ) 

	L226.TechCost_electd_USA <- write_to_all_states( subset( L226.GlobalTechCost_en, sector.name %in% elect_td_sectors ), c( reg, names_GlobalTechCost ) )
	names( L226.TechCost_electd_USA )[ names( L226.TechCost_electd_USA ) %in% c( "sector.name", "subsector.name" ) ] <- c( supp, subs ) 

	L226.TechCoef_electd_USA <- write_to_all_states( subset( L226.StubTechCoef_electd, supplysector %in% elect_td_sectors & region == "USA" ), names_StubTechCoef )
	names( L226.TechCoef_electd_USA )[ names( L226.TechCoef_electd_USA ) == "stub.technology" ] <- tech
	L226.TechCoef_electd_USA[[input]] <- "electricity domestic supply"
	L226.TechCoef_electd_USA$market.name <- states_subregions$grid_region[
      match( L226.TechCoef_electd_USA$region, states_subregions$state ) ]

	write_mi_data( L226.TechShrwt_electd_USA, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L226.TechShrwt_electd_USA", "GCAMUSA_XML_BATCH", "batch_electd_USA.xml" )
	write_mi_data( L226.TechCost_electd_USA, "TechCost", "GCAMUSA_LEVEL2_DATA", "L226.TechCost_electd_USA", "GCAMUSA_XML_BATCH", "batch_electd_USA.xml" )
	write_mi_data( L226.TechCoef_electd_USA, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L226.TechCoef_electd_USA", "GCAMUSA_XML_BATCH", "batch_electd_USA.xml" )
}

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_electd_USA.xml", "GCAMUSA_XML_FINAL", "electd_USA.xml", "", xml_tag="outFile" )

## if the regional fuel market are not being used, then don't even bother writing these out
if( use_regional_fuel_markets ){
	write_mi_data( L226.Supplysector_en_USA, IDstring="Supplysector", domain="GCAMUSA_LEVEL2_DATA", fn="L226.Supplysector_en_USA",
               batch_XML_domain="GCAMUSA_XML_BATCH", batch_XML_file="batch_en_prices_USA.xml" ) 
	write_mi_data( L226.SubsectorShrwtFllt_en_USA, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L226.SubsectorShrwtFllt_en_USA", "GCAMUSA_XML_BATCH", "batch_en_prices_USA.xml" ) 
	write_mi_data( L226.TechShrwt_en_USA, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L226.TechShrwt_en_USA", "GCAMUSA_XML_BATCH", "batch_en_prices_USA.xml" ) 
	write_mi_data( L226.TechCoef_en_USA, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L226.TechCoef_en_USA", "GCAMUSA_XML_BATCH", "batch_en_prices_USA.xml" ) 
	write_mi_data( L226.TechCost_en_USA, "TechCost", "GCAMUSA_LEVEL2_DATA", "L226.TechCost_en_USA", "GCAMUSA_XML_BATCH", "batch_en_prices_USA.xml" ) 
	write_mi_data( L226.Ccoef, "CarbonCoef", "GCAMUSA_LEVEL2_DATA", "L226.Ccoef", "GCAMUSA_XML_BATCH", "batch_en_prices_USA.xml" ) 

	insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_en_prices_USA.xml", "GCAMUSA_XML_FINAL", "en_prices_USA.xml", "", xml_tag="outFile" )
}

logstop()
