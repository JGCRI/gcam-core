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
logstart( "L222.en_transformation_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for GCAM USA energy transformation sectors (gas processing and refining)" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
L222.Supplysector_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.Supplysector_en", skip = 4 )
L222.SubsectorLogit_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.SubsectorLogit_en", skip = 4 )
L222.StubTech_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.StubTech_en", skip = 4 )
L222.StubTechCoef_refining <- readdata( "ENERGY_LEVEL2_DATA", "L222.StubTechCoef_refining", skip = 4 )
printlog( "NOTE: the refining techs within the states are assigned a different supplysector,
           so the global technology database needs to be expanded to include the different sector-names" )
L222.GlobalTechInterp_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.GlobalTechInterp_en", skip = 4 )
L222.GlobalTechCoef_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.GlobalTechCoef_en", skip = 4 )
L222.GlobalTechCost_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.GlobalTechCost_en", skip = 4 )
L222.GlobalTechShrwt_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.GlobalTechShrwt_en", skip = 4 )
L222.GlobalTechCapture_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.GlobalTechCapture_en", skip = 4 )
L222.GlobalTechShutdownProfit_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.GlobalTechShutdownProfit_en", skip = 4, must.exist = F )
L222.GlobalTechShutdown_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.GlobalTechShutdown_en", skip = 4, must.exist = F )
L222.GlobalTechSCurveProfit_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.GlobalTechSCurveProfit_en", skip = 4, must.exist = F )
L222.GlobalTechSCurve_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.GlobalTechSCurve_en", skip = 4, must.exist = F )
L222.GlobalTechLifetimeProfit_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.GlobalTechLifetimeProfit_en", skip = 4, must.exist = F )
L222.GlobalTechLifetime_en <- readdata( "ENERGY_LEVEL2_DATA", "L222.GlobalTechLifetime_en", skip = 4, must.exist = F )
L122.out_EJ_state_refining_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L122.out_EJ_state_refining_F" )
L202.CarbonCoef <- readdata( "ENERGY_LEVEL2_DATA", "L202.CarbonCoef", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Perform computations
# Define the sector(s) that will be used in this code file. Can be one or multiple sectors
en_names <- "refining"
printlog( "The supplysector and subsector structure in the USA refining sector is retained")
printlog( "L222.DeleteStubTech_USAen: remove existing stub technologies in the USA region" )
L222.DeleteStubTech_USAen <- subset( L222.StubTech_en,
      region == "USA" & supplysector %in% en_names )
write_mi_data( L222.DeleteStubTech_USAen, "DeleteStubTech", "GCAMUSA_LEVEL2_DATA", "L222.DeleteStubTech_USAen", "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml" )

printlog( "NOTE: Oil refining sectors are only created in states where the production is >0 in the historical period. Other techs are available everywhere" )
oil_refining_states <- unique( L122.out_EJ_state_refining_F$state[
      rowSums( L122.out_EJ_state_refining_F[ L122.out_EJ_state_refining_F$sector == "oil refining", X_historical_years ] ) > 0 ] )

printlog( "L222.TechInterp_USAen: technology shareweights, USA region")
L222.Tech_USAen <- repeat_and_add_vector(
      subset( L222.SubsectorLogit_en[ c( reg, supp, subs ) ], region == "USA" & supplysector %in% en_names ),
      state, states )
L222.Tech_USAen <- subset( L222.Tech_USAen, subsector == "oil refining" & state %in% oil_refining_states | subsector != "oil refining" )
L222.Tech_USAen[[tech]] <- paste( L222.Tech_USAen[[state]], L222.Tech_USAen[[subs]], sep = " " )
L222.Tech_USAen <- L222.Tech_USAen[ names_Tech ]

#Technology interpolation only applies to calibrated technologies
L222.TechInterp_USAen <- subset( L222.Tech_USAen, subsector %in% c( "oil refining", "biomass liquids" ) )
L222.TechInterp_USAen$apply.to <- "share-weight"
L222.TechInterp_USAen$from.year <- max( model_base_years )
L222.TechInterp_USAen$to.year <- 2100
L222.TechInterp_USAen$interpolation.function <- "fixed"
#For biomass liquids, allow state shares to shift over time (future techs are different than present techs)
L222.TechInterp_USAen$interpolation.function[ L222.TechInterp_USAen$subsector == "biomass liquids" ] <- "s-curve"

printlog( "L222.TechShrwt_USAen: technology shareweights, USA region")
L222.TechShrwt_USAen <- repeat_and_add_vector( L222.Tech_USAen, Y, model_years )
#Default the base year shareweights to 0. This will be over-ridden in calibration
L222.TechShrwt_USAen$share.weight[ L222.TechShrwt_USAen$year %in% model_base_years ] <- 0
L222.TechShrwt_USAen$share.weight[ L222.TechShrwt_USAen$year %in% model_future_years ] <- 1

printlog( "L222.TechCoef_USAen: technology coefficients and market names, USA region")
L222.TechCoef_USAen <- L222.TechShrwt_USAen[ names_TechYr ]
L222.TechCoef_USAen[[input]] <- L222.TechCoef_USAen[[subs]]
L222.TechCoef_USAen$coefficient <- 1
L222.TechCoef_USAen$market.name <- substr( L222.TechCoef_USAen[[tech]], 1, nchar( L222.TechCoef_USAen[[tech]] ) - nchar( L222.TechCoef_USAen[[subs]]) - 1 )

printlog( "L222.Production_USArefining: calibrated refinery production in USA (consuming output of states)" )
L222.Production_USArefining <- interpolate_and_melt( L122.out_EJ_state_refining_F, model_base_years, value.name = "calOutputValue", digits = digits_calOutput )
L222.Production_USArefining[[reg]] <- "USA"
L222.Production_USArefining[ c( supp, subs ) ] <- calibrated_techs[
      match( L222.Production_USArefining$sector, calibrated_techs$sector ),
      c( supp, subs ) ]
L222.Production_USArefining[[tech]] <- paste( L222.Production_USArefining$state, L222.Production_USArefining[[subs]], sep = " " )
L222.Production_USArefining[[input]] <- L222.Production_USArefining[[ subs ]]
L222.Production_USArefining <- subset( L222.Production_USArefining, subsector == "oil refining" & state %in% oil_refining_states | subsector != "oil refining" )

#This needs to be aggregated to the supplysector/subsector/technology level
L222.Production_USArefining <- aggregate( L222.Production_USArefining[ "calOutputValue" ],
      by=as.list( L222.Production_USArefining[ c( reg, s_s_t_i, Y ) ] ), sum )
L222.Production_USArefining$share.weight.year <- L222.Production_USArefining$year
L222.Production_USArefining <- set_subsector_shrwt( L222.Production_USArefining )
L222.Production_USArefining$tech.share.weight <- ifelse( L222.Production_USArefining$calOutputValue == 0, 0, 1 )
L222.Production_USArefining <- L222.Production_USArefining[ names_Production ]

printlog( "All tables for which processing is identical are done in a for loop")
printlog( "NOTE: writing out the tables in this step as well")
L222.tables <- list( L222.SubsectorLogit_en = L222.SubsectorLogit_en,
                     L222.StubTech_en = L222.StubTech_en,
                     L222.StubTechCoef_refining = L222.StubTechCoef_refining,
                     L222.GlobalTechInterp_en = L222.GlobalTechInterp_en,
                     L222.GlobalTechCoef_en = L222.GlobalTechCoef_en,
                     L222.GlobalTechCost_en = L222.GlobalTechCost_en,
                     L222.GlobalTechShrwt_en = L222.GlobalTechShrwt_en,
                     L222.GlobalTechCapture_en = L222.GlobalTechCapture_en,
                     L222.GlobalTechShutdownProfit_en = L222.GlobalTechShutdownProfit_en,
                     L222.GlobalTechShutdown_en = L222.GlobalTechShutdown_en,
                     L222.GlobalTechSCurveProfit_en = L222.GlobalTechSCurveProfit_en,
                     L222.GlobalTechSCurve_en = L222.GlobalTechSCurve_en,
                     L222.GlobalTechLifetimeProfit_en = L222.GlobalTechLifetimeProfit_en,
                     L222.GlobalTechLifetime_en = L222.GlobalTechLifetime_en )

for( i in 1:length( L222.tables ) ){
  if( !is.null( L222.tables[[i]] ) ){
  	  objectname <- paste0( names( L222.tables[i] ), "_USA" )
#Global technology database processing is simple: just set the sector name to the subsector name
	if( substr( objectname, 6, 15 ) == "GlobalTech" ){
		object <- subset( L222.tables[[i]], sector.name %in% en_names )
		object$sector.name <- object$subsector.name
	}
	if( substr( objectname, 6, 15 ) != "GlobalTech" ) {
		object <- write_to_all_states( subset( L222.tables[[i]], region == "USA" & supplysector %in% en_names ), names( L222.tables[[i]] ) )
# oil refining sectors are not created in states where no refineries currently exist
		object <- subset( object, subsector == "oil refining" & region %in% oil_refining_states | subsector != "oil refining" )
# the subsector will be the supplysector at the state level
		object[[supp]] <- object[[subs]]
	}
	  assign( objectname, object )
	  IDstringendpoint <- if( grepl( "_", names( L222.tables )[i] ) ) { regexpr( "_", names( L222.tables )[i], fixed = T ) - 1
	                       } else nchar( names( L222.tables )[i] )
	  IDstring <- substr( names( L222.tables )[i], 6, IDstringendpoint )
	  write_mi_data( object, IDstring, "GCAMUSA_LEVEL2_DATA", objectname, "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml" )
	  }
  }

#Supplysector information: need to replace name of supplysector with the subsector names
L222.Supplysector_en_USA <- L222.SubsectorLogit_en_USA[ names_Subsector ]
L222.Supplysector_en_USA$old_supplysector <- L222.SubsectorLogit_en$supplysector[
      match( L222.Supplysector_en_USA$subsector, L222.SubsectorLogit_en$subsector ) ]
L222.Supplysector_en_USA[ names_Supplysector[ !names_Supplysector %in% names( L222.Supplysector_en_USA ) ] ] <- L222.Supplysector_en[
      match( L222.Supplysector_en_USA$old_supplysector, L222.Supplysector_en$supplysector ),
      names_Supplysector[ !names_Supplysector %in% names( L222.Supplysector_en_USA ) ] ]
L222.Supplysector_en_USA <- L222.Supplysector_en_USA[ names_Supplysector ]

#Subsector shareweights: there is no competition here, so just fill out with 1s (will be over-ridden by base year calibration where necessary)
printlog( "L222.SubsectorShrwtFllt_en_USA: filled out subsector shareweights of state refining sectors (no subsec competition)")
L222.SubsectorShrwtFllt_en_USA <- L222.SubsectorLogit_en_USA[ names_Subsector ]
L222.SubsectorShrwtFllt_en_USA$year <- min( model_years )
L222.SubsectorShrwtFllt_en_USA$share.weight <- 1

printlog( "L222.StubTechProd_refining_USA: calibrated fuel production by state" )
L222.StubTechProd_refining_USA <- interpolate_and_melt( L122.out_EJ_state_refining_F, model_base_years, value.name = "calOutputValue", digits_calOutput )
L222.StubTechProd_refining_USA$region <- L222.StubTechProd_refining_USA$state
L222.StubTechProd_refining_USA[ s_s_t ] <- calibrated_techs[
      match( paste( L222.StubTechProd_refining_USA$sector, "output" ),  #Only take the tech IDs where the calibration is identified as output
             paste( calibrated_techs$sector, calibrated_techs$calibration ) ), s_s_t ]
#The supplysector is the same as the subsector within the states
L222.StubTechProd_refining_USA[[ supp ]] <- L222.StubTechProd_refining_USA[[ subs ]]
L222.StubTechProd_refining_USA$stub.technology <- L222.StubTechProd_refining_USA$technology
L222.StubTechProd_refining_USA$share.weight.year <- L222.StubTechProd_refining_USA[[Y]]
L222.StubTechProd_refining_USA <- set_subsector_shrwt( L222.StubTechProd_refining_USA )
L222.StubTechProd_refining_USA$tech.share.weight <- ifelse( L222.StubTechProd_refining_USA$calOutputValue > 0, 1, 0 )
L222.StubTechProd_refining_USA <- L222.StubTechProd_refining_USA[ names_StubTechProd ]

printlog( "L222.StubTechMarket_en_USA: market names of inputs to state refining sectors" )
L222.StubTechMarket_en_USA <- repeat_and_add_vector( L222.GlobalTechCoef_en_USA[ names_GlobalTechInput ], reg, states )
names( L222.StubTechMarket_en_USA )[ names( L222.StubTechMarket_en_USA ) %in% c( "sector.name", "subsector.name", tech ) ] <- c( supp, subs, "stub.technology" )
L222.StubTechMarket_en_USA$market.name <- "USA"
#If designated, switch fuel market names to the regional markets
if( use_regional_fuel_markets ){
	L222.StubTechMarket_en_USA$market.name[ L222.StubTechMarket_en_USA[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
	    match( L222.StubTechMarket_en_USA$region[ L222.StubTechMarket_en_USA[[input]] %in% regional_fuel_markets ], states_subregions$state ) ]
}

#Set electricity to the state markets
printlog( "NOTE: electricity is consumed from local markets")
L222.StubTechMarket_en_USA$market.name[ L222.StubTechMarket_en_USA[[input]] %in% elect_td_sectors ] <-
      L222.StubTechMarket_en_USA$region[ L222.StubTechMarket_en_USA[[input]] %in% elect_td_sectors ]
L222.StubTechMarket_en_USA <- L222.StubTechMarket_en_USA[ names_StubTechMarket ]
L222.StubTechMarket_en_USA <- subset( L222.StubTechMarket_en_USA, paste( supplysector, subsector, stub.technology ) %in%
      paste( L222.StubTech_en_USA$supplysector, L222.StubTech_en_USA$subsector, L222.StubTech_en_USA$stub.technology ) )

L222.CarbonCoef_en_USA <- L222.Supplysector_en_USA[ c( reg, supp ) ]
L222.CarbonCoef_en_USA$PrimaryFuelCO2Coef.name <- L222.CarbonCoef_en_USA[[ supp ]]
L222.CarbonCoef_en_USA$match_name <- L222.TechShrwt_USAen[[supp]][
      match( L222.CarbonCoef_en_USA[[supp]], L222.TechShrwt_USAen[[subs]] ) ]
L222.CarbonCoef_en_USA$PrimaryFuelCO2Coef <- L202.CarbonCoef$PrimaryFuelCO2Coef[
      match( paste( "USA", L222.CarbonCoef_en_USA$match_name ),
             paste( L202.CarbonCoef$region, L202.CarbonCoef$PrimaryFuelCO2Coef.name ) ) ]
L222.CarbonCoef_en_USA <- L222.CarbonCoef_en_USA[ names_PrimaryFuelCO2Coef ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L222.TechShrwt_USAen, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L222.TechShrwt_USAen", "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml" )
write_mi_data( L222.TechInterp_USAen, "TechInterp", "GCAMUSA_LEVEL2_DATA", "L222.TechInterp_USAen", "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml" )
write_mi_data( L222.TechShrwt_USAen, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L222.TechShrwt_USAen", "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml" )
write_mi_data( L222.TechCoef_USAen, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L222.TechCoef_USAen", "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml" )
write_mi_data( L222.Production_USArefining, "Production", "GCAMUSA_LEVEL2_DATA", "L222.Production_USArefining", "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml" )

write_mi_data( L222.Supplysector_en_USA, "Supplysector", "GCAMUSA_LEVEL2_DATA", "L222.Supplysector_en_USA", "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml" )
write_mi_data( L222.SubsectorShrwtFllt_en_USA, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L222.SubsectorShrwtFllt_en_USA", "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml" )
write_mi_data( L222.StubTechProd_refining_USA, "StubTechProd", "GCAMUSA_LEVEL2_DATA", "L222.StubTechProd_refining_USA", "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml" )
write_mi_data( L222.StubTechMarket_en_USA, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L222.StubTechMarket_en_USA", "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml" )
write_mi_data( L222.CarbonCoef_en_USA, "CarbonCoef", "GCAMUSA_LEVEL2_DATA", "L222.CarbonCoef_en_USA", "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_en_transformation_USA.xml", "GCAMUSA_XML_FINAL", "en_transformation_USA.xml", "", xml_tag="outFile" )

logstop()
