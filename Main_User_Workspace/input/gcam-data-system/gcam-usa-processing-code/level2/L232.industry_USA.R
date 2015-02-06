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
logstart( "L232.industry_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for aggregate industrial sectors" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_ind_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
A32.demand <- readdata( "ENERGY_ASSUMPTIONS", "A32.demand" )
A32.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A32.globaltech_coef" )
A32.globaltech_eff <- readdata( "ENERGY_ASSUMPTIONS", "A32.globaltech_eff" )
A32.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A32.globaltech_shrwt" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
L232.Supplysector_ind <- readdata( "ENERGY_LEVEL2_DATA", "L232.Supplysector_ind", skip = 4 )
L232.FinalEnergyKeyword_ind <- readdata( "ENERGY_LEVEL2_DATA", "L232.FinalEnergyKeyword_ind", skip = 4 )
L232.SubsectorLogit_ind <- readdata( "ENERGY_LEVEL2_DATA", "L232.SubsectorLogit_ind", skip = 4 )
L232.SubsectorShrwt_ind <- readdata( "ENERGY_LEVEL2_DATA", "L232.SubsectorShrwt_ind", skip = 4, must.exist = F )
L232.SubsectorShrwtFllt_ind <- readdata( "ENERGY_LEVEL2_DATA", "L232.SubsectorShrwtFllt_ind", skip = 4, must.exist = F )
L232.SubsectorInterp_ind <- readdata( "ENERGY_LEVEL2_DATA", "L232.SubsectorInterp_ind", skip = 4, must.exist = F )
L232.SubsectorInterpTo_ind <- readdata( "ENERGY_LEVEL2_DATA", "L232.SubsectorInterpTo_ind", skip = 4, must.exist = F )
L232.FuelPrefElast_indenergy <- readdata( "ENERGY_LEVEL2_DATA", "L232.FuelPrefElast_indenergy", skip = 4 )
L232.StubTech_ind <- readdata( "ENERGY_LEVEL2_DATA", "L232.StubTech_ind", skip = 4 )
L232.StubTechInterp_ind <- readdata( "ENERGY_LEVEL2_DATA", "L232.StubTechInterp_ind", skip = 4 )
L232.PerCapitaBased_ind <- readdata( "ENERGY_LEVEL2_DATA", "L232.PerCapitaBased_ind", skip = 4 )
L232.PriceElasticity_ind <- readdata( "ENERGY_LEVEL2_DATA", "L232.PriceElasticity_ind", skip = 4 )
L232.IncomeElasticity_ind_gcam3 <- readdata( "ENERGY_LEVEL2_DATA", "L232.IncomeElasticity_ind_gcam3", skip = 4 )
L132.in_EJ_state_indnochp_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L132.in_EJ_state_indnochp_F", skip = 4 )
L132.in_EJ_state_indfeed_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L132.in_EJ_state_indfeed_F", skip = 4 )
L132.in_EJ_state_indchp_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L132.in_EJ_state_indchp_F", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Perform computations
# Need to delete the industry sectors in the USA region (energy-final-demands and supplysectors)
printlog( "Deleting industrial sectors in the full USA region")
L232.DeleteSupplysector_USAind <- L232.Supplysector_ind[
      L232.Supplysector_ind$region == "USA",
      c( reg, supp ) ]
write_mi_data( L232.DeleteSupplysector_USAind, "DeleteSupplysector", "GCAMUSA_LEVEL2_DATA", "L232.DeleteSupplysector_USAind", "GCAMUSA_XML_BATCH", "batch_industry_USA.xml" ) 

printlog( "Deleting energy final demand sectors in the full USA region" )
L232.DeleteFinalDemand_USAind <- L232.PerCapitaBased_ind[
      L232.PerCapitaBased_ind$region == "USA", names_EnergyFinalDemand ]
write_mi_data( L232.DeleteFinalDemand_USAind, "DeleteFinalDemand", "GCAMUSA_LEVEL2_DATA", "L232.DeleteFinalDemand_USAind", "GCAMUSA_XML_BATCH", "batch_industry_USA.xml" ) 

printlog( "All tables for which processing is identical are done in a for loop")
printlog( "NOTE: writing out the tables in this step as well")

# Fuel preference elasticities may not be used in the USA; remove this table if that is the case
if( !"USA" %in% L232.FuelPrefElast_indenergy$region ) L232.FuelPrefElast_indenergy <- NULL

L232.tables <- list( L232.Supplysector_ind = L232.Supplysector_ind,
                     L232.FinalEnergyKeyword_ind = L232.FinalEnergyKeyword_ind,
                     L232.SubsectorLogit_ind = L232.SubsectorLogit_ind,
                     L232.SubsectorShrwt_ind = if( !is.null( L232.SubsectorShrwt_ind ) ) L232.SubsectorShrwt_ind,
                     L232.SubsectorShrwtFllt_ind = if( !is.null( L232.SubsectorShrwtFllt_ind ) ) L232.SubsectorShrwtFllt_ind,
                     L232.SubsectorInterp_ind = if( !is.null( L232.SubsectorInterp_ind ) ) L232.SubsectorInterp_ind,
                     L232.SubsectorInterpTo_ind = if( !is.null( L232.SubsectorInterpTo_ind ) ) L232.SubsectorInterpTo_ind,
                     L232.FuelPrefElast_indenergy = if( !is.null( L232.FuelPrefElast_indenergy ) ) L232.FuelPrefElast_indenergy,
                     L232.StubTech_ind = L232.StubTech_ind,
                     L232.StubTechInterp_ind = L232.StubTechInterp_ind,
                     L232.PerCapitaBased_ind = L232.PerCapitaBased_ind,
                     L232.PriceElasticity_ind = L232.PriceElasticity_ind,
                     L232.IncomeElasticity_ind_gcam3 = L232.IncomeElasticity_ind_gcam3 )

for( i in 1:length( L232.tables ) ){
  if( !is.null( L232.tables[[i]] ) ){
  	  objectname <- paste0( names( L232.tables[i] ), "_USA" )
	  object <- write_to_all_states( subset( L232.tables[[i]], region == "USA" ), names( L232.tables[[i]] ) )
	  assign( objectname, object )
	  IDstringendpoint <- if( grepl( "_", names( L232.tables )[i] ) ) { regexpr( "_", names( L232.tables )[i], fixed = T ) - 1
	                       } else nchar( names( L232.tables )[i] )
	  IDstring <- substr( names( L232.tables )[i], 6, IDstringendpoint )
	  write_mi_data( object, IDstring, "GCAMUSA_LEVEL2_DATA", objectname, "GCAMUSA_XML_BATCH", "batch_industry_USA.xml" )
	  }
  }

printlog( "L232.StubTechCalInput_indenergy_USA_USA: calibrated input of industrial energy use technologies (including cogen)")
L232.in_EJ_state_indenergy_F_Yh <- interpolate_and_melt( rbind( L132.in_EJ_state_indnochp_F, L132.in_EJ_state_indchp_F ), model_base_years )
L232.in_EJ_state_indenergy_F_Yh$region <- L232.in_EJ_state_indenergy_F_Yh$state
L232.in_EJ_state_indenergy_F_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( vecpaste( L232.in_EJ_state_indenergy_F_Yh[ S_F ] ),
             vecpaste( calibrated_techs[ S_F ] ) ),
      c( "supplysector", "subsector", "technology" ) ]

L232.StubTechCalInput_indenergy_USA <- L232.in_EJ_state_indenergy_F_Yh[ names_StubTechYr ]
L232.StubTechCalInput_indenergy_USA$minicam.energy.input <- A32.globaltech_eff$minicam.energy.input[ 
      match( vecpaste( L232.StubTechCalInput_indenergy_USA[ c( "subsector", "stub.technology" ) ] ),
             vecpaste( A32.globaltech_eff[ c( "subsector", "technology" ) ] ) ) ]
L232.StubTechCalInput_indenergy_USA$calibrated.value <- round( L232.in_EJ_state_indenergy_F_Yh$value, digits_calOutput )
L232.StubTechCalInput_indenergy_USA$share.weight.year <- L232.StubTechCalInput_indenergy_USA$year
L232.StubTechCalInput_indenergy_USA <- set_subsector_shrwt( L232.StubTechCalInput_indenergy_USA, value.name = "calibrated.value" )
L232.StubTechCalInput_indenergy_USA$tech.share.weight <- ifelse( L232.StubTechCalInput_indenergy_USA$calibrated.value > 0, 1, 0 )
L232.StubTechCalInput_indenergy_USA <- L232.StubTechCalInput_indenergy_USA[ names_StubTechCalInput ]

printlog( "L232.StubTechCalInput_indfeed_USA: calibrated input of industrial feedstock technologies")
L232.in_EJ_state_indfeed_F_Yh <- interpolate_and_melt( L132.in_EJ_state_indfeed_F, model_base_years )
L232.in_EJ_state_indfeed_F_Yh$region <- L232.in_EJ_state_indfeed_F_Yh$state
L232.in_EJ_state_indfeed_F_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( vecpaste( L232.in_EJ_state_indfeed_F_Yh[ S_F ] ),
             vecpaste( calibrated_techs[ S_F ] ) ),
      c( "supplysector", "subsector", "technology" ) ]

L232.StubTechCalInput_indfeed_USA <- L232.in_EJ_state_indfeed_F_Yh[ names_StubTechYr ]
L232.StubTechCalInput_indfeed_USA$minicam.energy.input <- A32.globaltech_eff$minicam.energy.input[ 
      match( vecpaste( L232.StubTechCalInput_indfeed_USA[ c( "subsector", "stub.technology" ) ] ),
             vecpaste( A32.globaltech_eff[ c( "subsector", "technology" ) ] ) ) ]
L232.StubTechCalInput_indfeed_USA$calibrated.value <- round( L232.in_EJ_state_indfeed_F_Yh$value, digits_calOutput )
L232.StubTechCalInput_indfeed_USA$share.weight.year <- L232.StubTechCalInput_indfeed_USA$year
L232.StubTechCalInput_indfeed_USA <- set_subsector_shrwt( L232.StubTechCalInput_indfeed_USA, value.name = "calibrated.value" )
L232.StubTechCalInput_indfeed_USA$tech.share.weight <- ifelse( L232.StubTechCalInput_indfeed_USA$calibrated.value > 0, 1, 0 )

printlog( "L232.StubTechProd_industry_USA: calibrated output of industrial sector" )
#First, calculate service output by technology, for energy-use and feedstocks
L232.globaltech_eff.melt <- interpolate_and_melt( A32.globaltech_eff, model_base_years, value.name="efficiency", digits = digits_efficiency )
L232.out_EJ_state_ind_serv_F_Yh <- rbind( L232.in_EJ_state_indenergy_F_Yh, L232.in_EJ_state_indfeed_F_Yh )
L232.out_EJ_state_ind_serv_F_Yh$efficiency <- L232.globaltech_eff.melt$efficiency[
      match( vecpaste( L232.out_EJ_state_ind_serv_F_Yh[ c( supp, subs, "stub.technology", Y ) ] ),
             vecpaste( L232.globaltech_eff.melt[ c( s_s_t, Y ) ] ) ) ]
L232.out_EJ_state_ind_serv_F_Yh$calOutputValue <- round( L232.out_EJ_state_ind_serv_F_Yh$value * L232.out_EJ_state_ind_serv_F_Yh$efficiency, digits_calOutput )

#Aggregate service output by region. This is the output of the industrial sector in each region.
L232.StubTechProd_industry_USA <- aggregate( L232.out_EJ_state_ind_serv_F_Yh[ "calOutputValue" ],
      by=as.list( L232.out_EJ_state_ind_serv_F_Yh[ c( "region", Y ) ] ), sum )
L232.industry_names <- A32.globaltech_shrwt[ A32.globaltech_shrwt[[supp]] == "industry", s_s_t ]
L232.StubTechProd_industry_USA[ c( supp, subs, "stub.technology" ) ] <- L232.industry_names[ rep( 1, times = nrow( L232.StubTechProd_industry_USA ) ), ]
L232.StubTechProd_industry_USA$share.weight.year <- L232.StubTechProd_industry_USA[[Y]]
L232.StubTechProd_industry_USA$subs.share.weight <- ifelse( L232.StubTechProd_industry_USA$calOutputValue > 0, 1, 0 )
L232.StubTechProd_industry_USA$tech.share.weight <- L232.StubTechProd_industry_USA$subs.share.weight
L232.StubTechProd_industry_USA <- L232.StubTechProd_industry_USA[ names_StubTechProd ]

printlog( "L232.StubTechCoef_industry_USA: calibrated output of industrial sector" )
#Next, aggregate service output by sector to calculate the portion of each input
L232.StubTechCoef_industry_USA_base <- aggregate( L232.out_EJ_state_ind_serv_F_Yh[ "calOutputValue" ],
      by=as.list( L232.out_EJ_state_ind_serv_F_Yh[ c( "region", supp, Y ) ] ), sum )
L232.StubTechCoef_industry_USA_base$output_tot <- L232.StubTechProd_industry_USA$calOutputValue[
      match( vecpaste( L232.StubTechCoef_industry_USA_base[ c( "region", Y ) ] ),
             vecpaste( L232.StubTechProd_industry_USA[ c( "region", Y ) ] ) ) ]
L232.StubTechCoef_industry_USA_base$coefficient <- round( L232.StubTechCoef_industry_USA_base$calOutputValue / L232.StubTechCoef_industry_USA_base$output_tot, digits_coefficient )
names( L232.StubTechCoef_industry_USA_base )[ names( L232.StubTechCoef_industry_USA_base ) == supp ] <- input
L232.StubTechCoef_industry_USA_base[ c( supp, subs, "stub.technology" ) ] <- L232.industry_names[ rep( 1, times = nrow( L232.StubTechCoef_industry_USA_base ) ), ]
L232.StubTechCoef_industry_USA_base$market.name <- L232.StubTechCoef_industry_USA_base$region

#This set of coefficients covers only the base years; the first "future" period will default to the global tech coefficient
# Instead, copy the base year values forward
# NOTE: This is different from the assumptions for the model as a whole, as no energy:feedstock evolution in the industrial sector is expected for the USA
L232.StubTechCoef_industry_USA_fut <- repeat_and_add_vector( subset( L232.StubTechCoef_industry_USA_base, year == max( model_base_years ) ), "year", model_future_years )
L232.StubTechCoef_industry_USA <- rbind( L232.StubTechCoef_industry_USA_base[ names_StubTechCoef ], L232.StubTechCoef_industry_USA_fut[ names_StubTechCoef ] )

printlog( "L232.StubTechMarket_ind_USA: Name the markets for the fuels consumed by the state industrial sectors" )
L232.StubTechMarket_ind_USA <- repeat_and_add_vector( L232.StubTech_ind_USA, Y, model_years )
L232.StubTechMarket_ind_USA[[input]] <- A32.globaltech_eff[[input]][
      match( vecpaste( L232.StubTechMarket_ind_USA[ c( supp, subs, "stub.technology" ) ] ),
             vecpaste( A32.globaltech_eff[ s_s_t ] ) ) ]

#The table of all stub technologies includes the generic industrial technology, which doesn't apply here. Only setting markets here for the ones that consume fuels.
L232.StubTechMarket_ind_USA <- L232.StubTechMarket_ind_USA[ !is.na( L232.StubTechMarket_ind_USA[[input]] ), ]
L232.StubTechMarket_ind_USA$market.name <- "USA"
L232.StubTechMarket_ind_USA <- L232.StubTechMarket_ind_USA[ names_StubTechMarket ]

if( use_regional_fuel_markets ){
	L232.StubTechMarket_ind_USA$market.name[ L232.StubTechMarket_ind_USA[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
	      match( L232.StubTechMarket_ind_USA$region[ L232.StubTechMarket_ind_USA[[input]] %in% regional_fuel_markets ],
	             states_subregions$state ) ]
}
printlog( "NOTE: electricity is consumed from state markets" )
L232.StubTechMarket_ind_USA$market.name[ L232.StubTechMarket_ind_USA[[input]] %in% elect_td_sectors ] <-
      L232.StubTechMarket_ind_USA$region[ L232.StubTechMarket_ind_USA[[input]] %in% elect_td_sectors ]

printlog( "L232.StubTechSecMarket_ind_USA: Name the markets for the cogenerated electricity (secondary output)" )
L232.chp_techs <- subset( A32.globaltech_eff, !is.na( secondary.output ) )
L232.StubTechSecMarket_ind_USA <- L232.StubTechMarket_ind_USA[
      vecpaste( L232.StubTechMarket_ind_USA[ c( supp, subs, "stub.technology" ) ] ) %in%
      vecpaste( L232.chp_techs[ s_s_t ] ), ]
L232.StubTechSecMarket_ind_USA$secondary.output <- "electricity"
L232.StubTechSecMarket_ind_USA <- L232.StubTechSecMarket_ind_USA[ c( names_StubTechYr, "secondary.output", "market.name" ) ]
#If regional fuel markets are being used, make sure to over-ride that here
L232.StubTechSecMarket_ind_USA$market.name <- "USA"
#Cogen should output to the regional electricity markets, if these are used
if( use_regional_elec_markets ){
	L232.StubTechSecMarket_ind_USA$market.name <- states_subregions$grid_region[
	      match( L232.StubTechSecMarket_ind_USA$region, states_subregions$state ) ]
}

printlog( "L232.BaseService_ind_USA: base-year service output of industry final demand" )
#Base service is equal to the output of the industry supplysector
L232.BaseService_ind_USA <- data.frame(
      region = L232.StubTechProd_industry_USA$region,
      energy.final.demand = A32.demand$energy.final.demand,
      year = L232.StubTechProd_industry_USA$year,
      base.service = L232.StubTechProd_industry_USA$calOutputValue )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L232.StubTechCalInput_indenergy_USA, "StubTechCalInput", "GCAMUSA_LEVEL2_DATA", "L232.StubTechCalInput_indenergy_USA", "GCAMUSA_XML_BATCH", "batch_industry_USA.xml" )
write_mi_data( L232.StubTechCalInput_indfeed_USA, "StubTechCalInput", "GCAMUSA_LEVEL2_DATA", "L232.StubTechCalInput_indfeed_USA", "GCAMUSA_XML_BATCH", "batch_industry_USA.xml" )
write_mi_data( L232.StubTechProd_industry_USA, "StubTechProd", "GCAMUSA_LEVEL2_DATA", "L232.StubTechProd_industry_USA", "GCAMUSA_XML_BATCH", "batch_industry_USA.xml" )
write_mi_data( L232.StubTechCoef_industry_USA, "StubTechCoef", "GCAMUSA_LEVEL2_DATA", "L232.StubTechCoef_industry_USA", "GCAMUSA_XML_BATCH", "batch_industry_USA.xml" )
write_mi_data( L232.StubTechMarket_ind_USA, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L232.StubTechMarket_ind_USA", "GCAMUSA_XML_BATCH", "batch_industry_USA.xml" )
write_mi_data( L232.StubTechSecMarket_ind_USA, "StubTechSecMarket", "GCAMUSA_LEVEL2_DATA", "L232.StubTechSecMarket_ind_USA", "GCAMUSA_XML_BATCH", "batch_industry_USA.xml" )
write_mi_data( L232.BaseService_ind_USA, "BaseService", "GCAMUSA_LEVEL2_DATA", "L232.BaseService_ind_USA", "GCAMUSA_XML_BATCH", "batch_industry_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_industry_USA.xml", "GCAMUSA_XML_FINAL", "industry_USA.xml", "", xml_tag="outFile" )

logstop()
