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
logstart( "L2321.cement_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for GCAM USA cement sectors" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
A321.demand <- readdata( "ENERGY_ASSUMPTIONS", "A321.demand" )
A321.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A321.globaltech_coef" )
L2321.Supplysector_cement <- readdata( "ENERGY_LEVEL2_DATA", "L2321.Supplysector_cement", skip = 4 )
L2321.Supplysector_cement <- readdata( "ENERGY_LEVEL2_DATA", "L2321.Supplysector_cement", skip = 4 )
L2321.FinalEnergyKeyword_cement <- readdata( "ENERGY_LEVEL2_DATA", "L2321.FinalEnergyKeyword_cement", skip = 4 )
L2321.SubsectorLogit_cement <- readdata( "ENERGY_LEVEL2_DATA", "L2321.SubsectorLogit_cement", skip = 4 )
L2321.SubsectorShrwt_cement <- readdata( "ENERGY_LEVEL2_DATA", "L2321.SubsectorShrwt_cement", skip = 4, must.exist = F )
L2321.SubsectorShrwtFllt_cement <- readdata( "ENERGY_LEVEL2_DATA", "L2321.SubsectorShrwtFllt_cement", skip = 4, must.exist = F )
L2321.SubsectorInterp_cement <- readdata( "ENERGY_LEVEL2_DATA", "L2321.SubsectorInterp_cement", skip = 4, must.exist = F )
L2321.SubsectorInterpTo_cement <- readdata( "ENERGY_LEVEL2_DATA", "L2321.SubsectorInterpTo_cement", skip = 4, must.exist = F )
L2321.StubTech_cement <- readdata( "ENERGY_LEVEL2_DATA", "L2321.StubTech_cement", skip = 4 )
L2321.PerCapitaBased_cement <- readdata( "ENERGY_LEVEL2_DATA", "L2321.PerCapitaBased_cement", skip = 4 )
L2321.PriceElasticity_cement <- readdata( "ENERGY_LEVEL2_DATA", "L2321.PriceElasticity_cement", skip = 4 )
L2321.IncomeElasticity_cement_gcam3 <- readdata( "ENERGY_LEVEL2_DATA", "L2321.IncomeElasticity_cement_gcam3", skip = 4 )
L1321.in_EJ_state_cement_F_Y <- readdata( "GCAMUSA_LEVEL1_DATA", "L1321.in_EJ_state_cement_F_Y" )
L1321.IO_GJkg_state_cement_F_Yh <- readdata( "GCAMUSA_LEVEL1_DATA", "L1321.IO_GJkg_state_cement_F_Yh" )
L1321.out_Mt_state_cement_Yh <- readdata( "GCAMUSA_LEVEL1_DATA", "L1321.out_Mt_state_cement_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "NOTE: cement sectors are only created in states where the Census data indicate production" )
cement_states <- unique( L1321.out_Mt_state_cement_Yh$state )

# Need to delete the cement sector in the USA region (energy-final-demands and supplysectors)
printlog( "Deleting cement sectors in the full USA region")
L2321.DeleteSupplysector_USAcement <- L2321.Supplysector_cement[
      L2321.Supplysector_cement$region == "USA",
      c( reg, supp ) ]
write_mi_data( L2321.DeleteSupplysector_USAcement, "DeleteSupplysector", "GCAMUSA_LEVEL2_DATA", "L2321.DeleteSupplysector_USAcement", "GCAMUSA_XML_BATCH", "batch_cement_USA.xml" ) 

printlog( "Deleting energy final demand sectors in the full USA region" )
L2321.DeleteFinalDemand_USAcement <- L2321.PerCapitaBased_cement[
      L2321.PerCapitaBased_cement$region == "USA", names_EnergyFinalDemand ]
write_mi_data( L2321.DeleteFinalDemand_USAcement, "DeleteFinalDemand", "GCAMUSA_LEVEL2_DATA", "L2321.DeleteFinalDemand_USAcement", "GCAMUSA_XML_BATCH", "batch_cement_USA.xml" ) 

printlog( "All tables for which processing is identical are done in a for loop")
printlog( "NOTE: writing out the tables in this step as well")
L2321.tables <- list( L2321.Supplysector_cement = L2321.Supplysector_cement,
                      L2321.FinalEnergyKeyword_cement = L2321.FinalEnergyKeyword_cement,
                      L2321.SubsectorLogit_cement = L2321.SubsectorLogit_cement,
                      L2321.SubsectorShrwt_cement = if( !is.null( L2321.SubsectorShrwt_cement ) ) L2321.SubsectorShrwt_cement,
                      L2321.SubsectorShrwtFllt_cement = if( !is.null( L2321.SubsectorShrwtFllt_cement ) ) L2321.SubsectorShrwtFllt_cement,
                      L2321.SubsectorInterp_cement = if( !is.null( L2321.SubsectorInterp_cement ) ) L2321.SubsectorInterp_cement,
                      L2321.SubsectorInterpTo_cement = if( !is.null( L2321.SubsectorInterpTo_cement ) ) L2321.SubsectorInterpTo_cement,
                      L2321.StubTech_cement = L2321.StubTech_cement,
                      L2321.PerCapitaBased_cement = L2321.PerCapitaBased_cement,
                      L2321.PriceElasticity_cement = L2321.PriceElasticity_cement,
                      L2321.IncomeElasticity_cement_gcam3 = L2321.IncomeElasticity_cement_gcam3 )

for( i in 1:length( L2321.tables ) ){
  if( !is.null( L2321.tables[[i]] ) ){
  	  objectname <- paste0( names( L2321.tables[i] ), "_USA" )
	  object <- write_to_all_states( subset( L2321.tables[[i]], region == "USA" ), names( L2321.tables[[i]] ) )
# Note that the cement sector is not created in states where production is omitted from the census data. subset only the states where it exists
	  object <- subset( object, region %in% cement_states )
	  assign( objectname, object )
	  IDstringendpoint <- if( grepl( "_", names( L2321.tables )[i] ) ) { regexpr( "_", names( L2321.tables )[i], fixed = T ) - 1
	                       } else nchar( names( L2321.tables )[i] )
	  IDstring <- substr( names( L2321.tables )[i], 7, IDstringendpoint )
	  write_mi_data( object, IDstring, "GCAMUSA_LEVEL2_DATA", objectname, "GCAMUSA_XML_BATCH", "batch_cement_USA.xml" )
	  }
  }

printlog( "L2321.StubTechProd_cement_USA: calibrated cement production" )
L2321.StubTechProd_cement_USA <- interpolate_and_melt( L1321.out_Mt_state_cement_Yh, model_base_years, value.name = "calOutputValue", digits_calOutput )
L2321.StubTechProd_cement_USA$region <- L2321.StubTechProd_cement_USA$state
L2321.StubTechProd_cement_USA[ s_s_t ] <- calibrated_techs[
      match( paste( L2321.StubTechProd_cement_USA$sector, "output" ),  #Only take the tech IDs where the calibration is identified as output
             paste( calibrated_techs$sector, calibrated_techs$calibration ) ), s_s_t ]
L2321.StubTechProd_cement_USA$stub.technology <- L2321.StubTechProd_cement_USA$technology
L2321.StubTechProd_cement_USA$share.weight.year <- L2321.StubTechProd_cement_USA[[Y]]
L2321.StubTechProd_cement_USA$subs.share.weight <- ifelse( L2321.StubTechProd_cement_USA$calOutputValue > 0, 1, 0 )
L2321.StubTechProd_cement_USA$tech.share.weight <- L2321.StubTechProd_cement_USA$subs.share.weight
L2321.StubTechProd_cement_USA <- L2321.StubTechProd_cement_USA[ names_StubTechProd ]

printlog( "L2321.StubTechCoef_cement_USA: coefficients of cement production technologies" )
#Write this out to all periods; don't want to inherit global tech characteristics here, as the market name is set in this table
L1321.IO_GJkg_state_cement_F_Yh[ s_s_t_i ] <- calibrated_techs[ match( vecpaste( L1321.IO_GJkg_state_cement_F_Yh[ S_F ] ), vecpaste( calibrated_techs[ S_F ] ) ), s_s_t_i ]
L2321.globaltech_coef <- gcam_interp( A321.globaltech_coef, model_future_years )
L1321.IO_GJkg_state_cement_F_Yh[ X_future_years ] <- L2321.globaltech_coef[
      match( vecpaste( L1321.IO_GJkg_state_cement_F_Yh[ s_s_t_i ] ),
             vecpaste( L2321.globaltech_coef[ s_s_t_i ] ) ),
      X_future_years ]
L2321.IO_GJkg_state_cement_F_Yh.melt <- interpolate_and_melt( L1321.IO_GJkg_state_cement_F_Yh, model_years, value.name = "coefficient", digits_coefficient )

#Note that this table omits the non-calibrated "cement ccs" technology. The following method reads this, assuming the same coefs on the technologies. 
L2321.StubTechCoef_cement_USA <- subset( L2321.StubTech_cement_USA, supplysector %in% L2321.IO_GJkg_state_cement_F_Yh.melt$supplysector )
L2321.StubTechCoef_cement_USA <- repeat_and_add_vector( L2321.StubTechCoef_cement_USA, Y, model_years )
L2321.StubTechCoef_cement_USA <- repeat_and_add_vector( L2321.StubTechCoef_cement_USA, input, unique( L2321.IO_GJkg_state_cement_F_Yh.melt[[input]] ) )
L2321.StubTechCoef_cement_USA$coefficient <- L2321.IO_GJkg_state_cement_F_Yh.melt$coefficient[
      match( vecpaste( L2321.StubTechCoef_cement_USA[ c( reg, supp, input, Y ) ] ),
             vecpaste( L2321.IO_GJkg_state_cement_F_Yh.melt[ c( state, supp, input, Y ) ] ) ) ]

#The market is tricky - limestone and process heat are from state markets, and electricity from the USA
L2321.StubTechCoef_cement_USA$market.name <- L2321.StubTechCoef_cement_USA$region
L2321.StubTechCoef_cement_USA$market.name[ grepl( "elec", L2321.StubTechCoef_cement_USA[[input]] ) ] <- "USA"

if( use_regional_fuel_markets ){
	L2321.StubTechCoef_cement_USA$market.name[ L2321.StubTechCoef_cement_USA[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
	      match( L2321.StubTechCoef_cement_USA$region[ L2321.StubTechCoef_cement_USA[[input]] %in% regional_fuel_markets ],
	             states_subregions$state ) ]
}
printlog( "NOTE: electricity is consumed from state markets" )
L2321.StubTechCoef_cement_USA$market.name[ L2321.StubTechCoef_cement_USA[[input]] %in% elect_td_sectors ] <-
      L2321.StubTechCoef_cement_USA$region[ L2321.StubTechCoef_cement_USA[[input]] %in% elect_td_sectors ]

printlog( "L2321.StubTechCalInput_cement_heat_USA: calibrated input of fuel consumption for producing heat" )
L2321.StubTechCalInput_cement_heat_USA <- interpolate_and_melt( L1321.in_EJ_state_cement_F_Y, model_base_years, value.name = "calibrated.value", digits = digits_calOutput )
L2321.StubTechCalInput_cement_heat_USA$region <- L2321.StubTechCalInput_cement_heat_USA$state
L2321.StubTechCalInput_cement_heat_USA[ s_s_t_i ] <- calibrated_techs[
      match( vecpaste( L2321.StubTechCalInput_cement_heat_USA[ S_F] ),
             vecpaste( calibrated_techs[ S_F ] ) ), s_s_t_i ]

#This table should only be the technologies for producing heat - drop the electricity inputs to the cement production technology
L2321.StubTechCalInput_cement_heat_USA <- subset( L2321.StubTechCalInput_cement_heat_USA, supplysector %!in% L2321.StubTechCoef_cement_USA$supplysector )
L2321.StubTechCalInput_cement_heat_USA$stub.technology <- L2321.StubTechCalInput_cement_heat_USA$technology
L2321.StubTechCalInput_cement_heat_USA$share.weight.year <- L2321.StubTechCalInput_cement_heat_USA[[Y]]
L2321.StubTechCalInput_cement_heat_USA$subs.share.weight <- ifelse( L2321.StubTechCalInput_cement_heat_USA$calibrated.value > 0, 1, 0 )
L2321.StubTechCalInput_cement_heat_USA$tech.share.weight <- L2321.StubTechCalInput_cement_heat_USA$subs.share.weight
L2321.StubTechCalInput_cement_heat_USA <- L2321.StubTechCalInput_cement_heat_USA[ names_StubTechCalInput ]

printlog( "L2321.StubTechMarket_cement_USA: Name the markets for the fuels consumed for heat by the state cement sectors" )
L2321.StubTechMarket_cement_USA <- repeat_and_add_vector( L2321.StubTech_cement_USA, Y, model_years )
#Remove the cement supplysector, leaving only the process heat supplysector
L2321.StubTechMarket_cement_USA <- subset( L2321.StubTechMarket_cement_USA, supplysector %!in% L2321.StubTechCoef_cement_USA$supplysector )
L2321.StubTechMarket_cement_USA[[input]] <- calibrated_techs[[input]][
      match( vecpaste( L2321.StubTechMarket_cement_USA[ c( supp, subs, "stub.technology" ) ] ),
             vecpaste( calibrated_techs[ s_s_t ] ) ) ]
#Fuels are from the USA markets
L2321.StubTechMarket_cement_USA$market.name <- "USA"
L2321.StubTechMarket_cement_USA <- L2321.StubTechMarket_cement_USA[ names_StubTechMarket ]

if( use_regional_fuel_markets ){
	L2321.StubTechMarket_cement_USA$market.name[ L2321.StubTechMarket_cement_USA[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
	      match( L2321.StubTechMarket_cement_USA$region[ L2321.StubTechMarket_cement_USA[[input]] %in% regional_fuel_markets ],
	             states_subregions$state ) ]
}

printlog( "L2321.BaseService_cement_USA: base-year service output of cement final demand" )
#Base service is equal to the output of the cement supplysector
L2321.BaseService_cement_USA <- data.frame(
      region = L2321.StubTechProd_cement_USA$region,
      energy.final.demand = A321.demand$energy.final.demand,
      year = L2321.StubTechProd_cement_USA$year,
      base.service = L2321.StubTechProd_cement_USA$calOutputValue )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2321.StubTechProd_cement_USA, "StubTechProd", "GCAMUSA_LEVEL2_DATA", "L2321.StubTechProd_cement_USA", "GCAMUSA_XML_BATCH", "batch_cement_USA.xml" )
write_mi_data( L2321.StubTechCoef_cement_USA, "StubTechCoef", "GCAMUSA_LEVEL2_DATA", "L2321.StubTechCoef_cement_USA", "GCAMUSA_XML_BATCH", "batch_cement_USA.xml" )
write_mi_data( L2321.StubTechCalInput_cement_heat_USA, "StubTechCalInput", "GCAMUSA_LEVEL2_DATA", "L2321.StubTechCalInput_cement_heat_USA", "GCAMUSA_XML_BATCH", "batch_cement_USA.xml" )
write_mi_data( L2321.StubTechMarket_cement_USA, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L2321.StubTechMarket_cement_USA", "GCAMUSA_XML_BATCH", "batch_cement_USA.xml" )
write_mi_data( L2321.BaseService_cement_USA, "BaseService", "GCAMUSA_LEVEL2_DATA", "L2321.BaseService_cement_USA", "GCAMUSA_XML_BATCH", "batch_cement_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_cement_USA.xml", "GCAMUSA_XML_FINAL", "cement_USA.xml", "", xml_tag="outFile" )

logstop()
