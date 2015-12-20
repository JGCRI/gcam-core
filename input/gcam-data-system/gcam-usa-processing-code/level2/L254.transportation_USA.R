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
logstart( "L254.transportation_USA.R" )
printlog( "GCAM-USA transportation sector model inputs" )

#Note that transportation is a bit different from buildings, where GCAM-USA has a more detailed representation built up from
# different data sources and processed in different ways than the buildings sector in the USA region of core GCAM. In this case,
# we want to copy the data from the USA transportation sector to each of the states, only changing the calibration data as
# written out in the GCAM-USA level1 processing. For this reason, we will read in level2 data files, 

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_trn_data", extension = ".R" )
UCD_techs <- readdata( "ENERGY_MAPPINGS", "UCD_techs" )
A54.globaltech_nonmotor <- readdata( "ENERGY_ASSUMPTIONS", "A54.globaltech_nonmotor" )
A54.globaltech_passthru <- readdata( "ENERGY_ASSUMPTIONS", "A54.globaltech_passthru" )
A54.sector <- readdata( "ENERGY_ASSUMPTIONS", "A54.sector" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
L254.Supplysector_trn <- readdata( "ENERGY_LEVEL2_DATA", "L254.Supplysector_trn", skip = 4 )
L254.FinalEnergyKeyword_trn <- readdata( "ENERGY_LEVEL2_DATA", "L254.FinalEnergyKeyword_trn", skip = 4 )
L254.tranSubsectorLogit <- readdata( "ENERGY_LEVEL2_DATA", "L254.tranSubsectorLogit", skip = 4 )
L254.tranSubsectorShrwt <- readdata( "ENERGY_LEVEL2_DATA", "L254.tranSubsectorShrwt", skip = 4, must.exist = F )
L254.tranSubsectorShrwtFllt <- readdata( "ENERGY_LEVEL2_DATA", "L254.tranSubsectorShrwtFllt", skip = 4, must.exist = F )
L254.tranSubsectorInterp <- readdata( "ENERGY_LEVEL2_DATA", "L254.tranSubsectorInterp", skip = 4, must.exist = F )
L254.tranSubsectorInterpTo <- readdata( "ENERGY_LEVEL2_DATA", "L254.tranSubsectorInterpTo", skip = 4, must.exist = F )
L254.tranSubsectorSpeed <- readdata( "ENERGY_LEVEL2_DATA", "L254.tranSubsectorSpeed", skip = 4 )
L254.tranSubsectorSpeed_passthru <- readdata( "ENERGY_LEVEL2_DATA", "L254.tranSubsectorSpeed_passthru", skip = 4 )
L254.tranSubsectorSpeed_noVOTT <- readdata( "ENERGY_LEVEL2_DATA", "L254.tranSubsectorSpeed_noVOTT", skip = 4 )
L254.tranSubsectorSpeed_nonmotor <- readdata( "ENERGY_LEVEL2_DATA", "L254.tranSubsectorSpeed_nonmotor", skip = 4 )
L254.tranSubsectorVOTT <- readdata( "ENERGY_LEVEL2_DATA", "L254.tranSubsectorVOTT", skip = 4 )
L254.tranSubsectorFuelPref <- readdata( "ENERGY_LEVEL2_DATA", "L254.tranSubsectorFuelPref", skip = 4 )
L254.StubTranTech <- readdata( "ENERGY_LEVEL2_DATA", "L254.StubTranTech", skip = 4 )
L254.StubTech_passthru <- readdata( "ENERGY_LEVEL2_DATA", "L254.StubTech_passthru", skip = 4 )
L254.StubTech_nonmotor <- readdata( "ENERGY_LEVEL2_DATA", "L254.StubTech_nonmotor", skip = 4 )
L254.StubTranTechLoadFactor <- readdata( "ENERGY_LEVEL2_DATA", "L254.StubTranTechLoadFactor", skip = 4 )
L254.StubTranTechCost <- readdata( "ENERGY_LEVEL2_DATA", "L254.StubTranTechCost", skip = 4 )
L254.StubTranTechCoef <- readdata( "ENERGY_LEVEL2_DATA", "L254.StubTranTechCoef", skip = 4 )
L254.PerCapitaBased_trn <- readdata( "ENERGY_LEVEL2_DATA", "L254.PerCapitaBased_trn", skip = 4 )
L254.PriceElasticity_trn <- readdata( "ENERGY_LEVEL2_DATA", "L254.PriceElasticity_trn", skip = 4 )
L254.IncomeElasticity_trn <- readdata( "ENERGY_LEVEL2_DATA", "L254.IncomeElasticity_trn", skip = 4 )
L154.in_EJ_state_trn_m_sz_tech_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L154.in_EJ_state_trn_m_sz_tech_F" )
L154.out_mpkm_state_trn_nonmotor_Yh <- readdata( "GCAMUSA_LEVEL1_DATA", "L154.out_mpkm_state_trn_nonmotor_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# Need to delete the transportation sector in the USA region (energy-final-demands and supplysectors)
printlog( "Deleting transportation sectors in the full USA region")
L254.DeleteSupplysector_USAtrn <- L254.Supplysector_trn[
      L254.Supplysector_trn$region == "USA",
      c( reg, supp ) ]
write_mi_data( L254.DeleteSupplysector_USAtrn, "DeleteSupplysector", "GCAMUSA_LEVEL2_DATA", "L254.DeleteSupplysector_USAtrn", "GCAMUSA_XML_BATCH", "batch_transportation_USA.xml" ) 

printlog( "Deleting energy final demand sectors in the full USA region" )
L254.DeleteFinalDemand_USAtrn <- L254.PerCapitaBased_trn[
      L254.PerCapitaBased_trn$region == "USA", names_EnergyFinalDemand ]
write_mi_data( L254.DeleteFinalDemand_USAtrn, "DeleteFinalDemand", "GCAMUSA_LEVEL2_DATA", "L254.DeleteFinalDemand_USAtrn", "GCAMUSA_XML_BATCH", "batch_transportation_USA.xml" ) 

printlog( "All tables for which processing is identical are done in a for loop")
printlog( "NOTE: writing out the tables in this step as well")
##Some of the tables don't have the ID string embedded in the object name.
# TODO: This should be fixed in the level2 code file
# For now, just re-name the objects here
L254.StubTranTech_passthru <- L254.StubTech_passthru
L254.StubTranTech_nonmotor <- L254.StubTech_nonmotor

#Reduce the rounding on the coefficients
L254.StubTranTechCoef$coefficient <- round( L254.StubTranTechCoef$coefficient, digits_trnUSA_default )

L254.tables <- list( L254.Supplysector_trn = L254.Supplysector_trn,
                     L254.FinalEnergyKeyword_trn = L254.FinalEnergyKeyword_trn,
                     L254.tranSubsectorLogit = L254.tranSubsectorLogit,
                     L254.tranSubsectorShrwt = if( !is.null( L254.tranSubsectorShrwt ) ) L254.tranSubsectorShrwt,
                     L254.tranSubsectorShrwtFllt = if( !is.null( L254.tranSubsectorShrwtFllt ) ) L254.tranSubsectorShrwtFllt,
                     L254.tranSubsectorInterp = if( !is.null( L254.tranSubsectorInterp ) ) L254.tranSubsectorInterp,
                     L254.tranSubsectorInterpTo = if( !is.null( L254.tranSubsectorInterpTo ) ) L254.tranSubsectorInterpTo,
                     L254.tranSubsectorSpeed = L254.tranSubsectorSpeed,
                     L254.tranSubsectorSpeed_passthru = L254.tranSubsectorSpeed_passthru,
                     L254.tranSubsectorSpeed_noVOTT = L254.tranSubsectorSpeed_noVOTT,
                     L254.tranSubsectorSpeed_nonmotor = L254.tranSubsectorSpeed_nonmotor,
                     L254.tranSubsectorVOTT = L254.tranSubsectorVOTT,
                     L254.tranSubsectorFuelPref = L254.tranSubsectorFuelPref,
                     L254.StubTranTech = L254.StubTranTech,
                     L254.tranSubsectorFuelPref = L254.tranSubsectorFuelPref,
                     L254.StubTranTech = L254.StubTranTech,
                     L254.StubTranTech_passthru = L254.StubTranTech_passthru,
                     L254.StubTranTech_nonmotor = L254.StubTranTech_nonmotor,
                     L254.StubTranTechLoadFactor = L254.StubTranTechLoadFactor,
                     L254.StubTranTechCost = L254.StubTranTechCost,
                     L254.StubTranTechCoef = L254.StubTranTechCoef,
                     L254.PerCapitaBased_trn = L254.PerCapitaBased_trn,
                     L254.PriceElasticity_trn = L254.PriceElasticity_trn,
                     L254.IncomeElasticity_trn = L254.IncomeElasticity_trn )

# The logit functions should be processed before any other table that needs to read logit exponents
L254.tables <- c( read_logit_fn_tables( "ENERGY_LEVEL2_DATA", "L254.Supplysector_", skip=4, include.equiv.table=T ),
                  read_logit_fn_tables( "ENERGY_LEVEL2_DATA", "L254.tranSubsector_", skip=4, include.equiv.table=F ),
                  L254.tables )

for( i in 1:length( L254.tables ) ){
  if( !is.null( L254.tables[[i]] ) ){
  	  objectname <- paste0( names( L254.tables[i] ), "_USA" )
      if( substr( objectname, 6, 16 ) == "EQUIV_TABLE" || nrow( subset( L254.tables[[i]], region == "USA" ) ) == 0 ) {
          # Just use the object as is
          object <- L254.tables[[i]]
      } else {
          object <- write_to_all_states( subset( L254.tables[[i]], region == "USA" ), names( L254.tables[[i]] ) )
        if( use_regional_fuel_markets & "market.name" %in% names( object ) ){
            object$market.name[ object[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
                  match( object$region[ object[[input]] %in% regional_fuel_markets], states_subregions$state ) ]
        }
        #NOTE: electricity is always consumed from state markets
        if( "market.name" %in% names( object ) ){
            object$market.name[ object[[input]] %in% elect_td_sectors ] <- object$region[ object[[input]] %in% elect_td_sectors ]
        }
      }
	  assign( objectname, object )
      curr_table_name <- names( L254.tables )[i]
      IDstringendpoint <- if( grepl( "_", curr_table_name ) & !grepl( 'EQUIV_TABLE', curr_table_name ) & !grepl( '-logit$', curr_table_name ) ) {
          regexpr( "_", curr_table_name, fixed = T ) - 1
      } else {
          nchar( curr_table_name )
      }
	  IDstring <- substr( names( L254.tables )[i], 6, IDstringendpoint )
	  write_mi_data( object, IDstring, "GCAMUSA_LEVEL2_DATA", objectname, "GCAMUSA_XML_BATCH", "batch_transportation_USA.xml" )
	  }
  }

#Calibration
printlog( "L254.StubTranTechCalInput_USA: calibrated energy consumption by all technologies" )
##NOTE: NEED TO WRITE THIS OUT FOR ALL TECHNOLOGIES, NOT JUST THOSE THAT EXIST IN SOME BASE YEARS. Model may make up cal values otherwise.
L254.StubTranTechCalInput_USA.tmp <- interpolate_and_melt(
      L154.in_EJ_state_trn_m_sz_tech_F, model_base_years, value.name = "calibrated.value", digits = digits_calOutput )
L254.StubTranTechCalInput_USA.tmp$region <- L254.StubTranTechCalInput_USA.tmp$state
L254.StubTranTechCalInput_USA.tmp[ c( "supplysector", "tranSubsector", "stub.technology", "minicam.energy.input" ) ] <- UCD_techs[
      match( vecpaste( L254.StubTranTechCalInput_USA.tmp[ UCD_techID ] ),
             vecpaste( UCD_techs[ UCD_techID ] ) ),
      c( s_tS_tT, input ) ]

L254.StubTranTechCalInput_USA <- subset( L254.StubTranTechCoef_USA, year %in% model_base_years )[ names( L254.StubTranTechCoef_USA ) %in% names_StubTranTechCalInput ]
L254.StubTranTechCalInput_USA$calibrated.value <- L254.StubTranTechCalInput_USA.tmp$calibrated.value[
      match( vecpaste( L254.StubTranTechCalInput_USA[ c( names_StubTranTechYr, input ) ] ),
             vecpaste( L254.StubTranTechCalInput_USA.tmp[ c( names_StubTranTechYr, input ) ] ) ) ]
L254.StubTranTechCalInput_USA$calibrated.value[ is.na( L254.StubTranTechCalInput_USA$calibrated.value ) ] <- 0
L254.StubTranTechCalInput_USA$share.weight.year <- L254.StubTranTechCalInput_USA$year
L254.StubTranTechCalInput_USA <- set_subsector_shrwt( L254.StubTranTechCalInput_USA, subsector.name = "tranSubsector", value.name = "calibrated.value" )
L254.StubTranTechCalInput_USA$tech.share.weight <- ifelse( L254.StubTranTechCalInput_USA$calibrated.value > 0, 1, 0 )
L254.StubTranTechCalInput_USA <- L254.StubTranTechCalInput_USA[ names_StubTranTechCalInput ]

#Non-motorized technologies
printlog( "L254.StubTranTechProd_nonmotor_USA: service output of non-motorized transportation technologies" )
L254.StubTranTechProd_nonmotor_USA <- interpolate_and_melt(
      L154.out_mpkm_state_trn_nonmotor_Yh, model_base_years, value.name = "calOutputValue", digits = digits_mpkm )
L254.StubTranTechProd_nonmotor_USA$region <- L254.StubTranTechProd_nonmotor_USA$state
L254.StubTranTechProd_nonmotor_USA[ s_tS_t ] <- A54.globaltech_nonmotor[
      match( L254.StubTranTechProd_nonmotor_USA$mode,
             A54.globaltech_nonmotor$tranSubsector ),
      s_tS_t ]
L254.StubTranTechProd_nonmotor_USA$stub.technology <- L254.StubTranTechProd_nonmotor_USA$technology
#There is no need to match shareweights to the calOutputValue because no region should ever have a 0 here
L254.StubTranTechProd_nonmotor_USA <- L254.StubTranTechProd_nonmotor_USA[ c( names_StubTranTechYr, "calOutputValue" ) ]

printlog( "L254.StubTranTechCalInput_passthru_USA: calibrated input of passthrough technologies" )
#First, need to calculate the service output for all tranTechnologies (= calInput * loadFactor * unit_conversion / (coef * unit conversion ) )
L254.StubTranTechOutput_USA <- L254.StubTranTechCalInput_USA[ !grepl( "share", names( L254.StubTranTechCalInput_USA ) ) ]
L254.StubTranTechOutput_USA$loadFactor <- L254.StubTranTechLoadFactor_USA$loadFactor[
      match( vecpaste( L254.StubTranTechOutput_USA[ c( "region", supp, "tranSubsector", "stub.technology", Y ) ] ),
             vecpaste( L254.StubTranTechLoadFactor_USA[ c( "region", supp, "tranSubsector", "stub.technology", Y ) ] ) ) ]
L254.StubTranTechOutput_USA$coefficient <- L254.StubTranTechCoef_USA$coefficient[
      match( vecpaste( L254.StubTranTechOutput_USA[ c( "region", supp, "tranSubsector", "stub.technology", Y ) ] ),
             vecpaste( L254.StubTranTechCoef_USA[ c( "region", supp, "tranSubsector", "stub.technology", Y ) ] ) ) ]
L254.StubTranTechOutput_USA <- within( L254.StubTranTechOutput_USA,
      output <- round( calibrated.value * loadFactor * conv_EJ_GJ / ( coefficient * conv_btu_kJ ), digits_trnUSA_default ) )

#The next step is to bind rows with all pass-through technologies on to this table
#Write all possible pass-through technologies to all regions
L254.StubTranTechCalInput_passthru_USA <- repeat_and_add_vector(
      A54.globaltech_passthru, Y, model_base_years )
L254.StubTranTechCalInput_passthru_USA <- write_to_all_states(
      L254.StubTranTechCalInput_passthru_USA, c( names_tranSubsector, tech, Y, input ) )
names( L254.StubTranTechCalInput_passthru_USA )[ names( L254.StubTranTechCalInput_passthru_USA ) == "technology" ] <- "stub.technology"

#Subset only the passthrough technologies that are applicable in each region
L254.StubTranTechCalInput_passthru_USA <- L254.StubTranTechCalInput_passthru_USA[
      vecpaste( L254.StubTranTechCalInput_passthru_USA[ names_StubTranTech ] ) %in%
      vecpaste( L254.StubTranTech_passthru_USA[ names_StubTranTech ] ), ]

#Start with a 0 value for output, and bind this to the table of output by tranTechnology (using only columns whose names match)
L254.StubTranTechCalInput_passthru_USA$output <- 0
L254.StubTranTechCalInput_passthru_USA <- rbind(
      L254.StubTranTechOutput_USA[ names( L254.StubTranTechCalInput_passthru_USA ) ],
      L254.StubTranTechCalInput_passthru_USA )

for( i in 1:nrow( L254.StubTranTechCalInput_passthru_USA ) ){
	L254.StubTranTechCalInput_passthru_USA[i, "output"] <- aggregate_passthroughs( L254.StubTranTechCalInput_passthru_USA, i )
}

#Then, remove the technologies that are not pass-through sectors, and rename "output" to "calibrated.value"
L254.StubTranTechCalInput_passthru_USA <- L254.StubTranTechCalInput_passthru_USA[
      vecpaste( L254.StubTranTechCalInput_passthru_USA[ names_StubTranTech ] ) %in%
      vecpaste( L254.StubTranTech_passthru_USA[ names_StubTranTech ] ), ]
names( L254.StubTranTechCalInput_passthru_USA )[ names( L254.StubTranTechCalInput_passthru_USA ) == "output" ] <- "calibrated.value"
L254.StubTranTechCalInput_passthru_USA$share.weight.year <- L254.StubTranTechCalInput_passthru_USA$year
L254.StubTranTechCalInput_passthru_USA$subs.share.weight <- ifelse( L254.StubTranTechCalInput_passthru_USA$calibrated.value > 0, 1, 0 )
L254.StubTranTechCalInput_passthru_USA$tech.share.weight <- ifelse( L254.StubTranTechCalInput_passthru_USA$calibrated.value > 0, 1, 0 )

printlog( "L254.BaseService_trn_USA: base-year service output of transportation final demand" )
L254.AllTechOutput_USA <- rbind(
      L254.StubTranTechOutput_USA[ names_StubTranTechYr ],
      L254.StubTranTechProd_nonmotor_USA[ names_StubTranTechYr ] )
L254.AllTechOutput_USA$base.service <- c(
      L254.StubTranTechOutput_USA$output,
      L254.StubTranTechProd_nonmotor_USA$calOutputValue )
L254.AllTechOutput_USA$energy.final.demand <- A54.sector$energy.final.demand[
      match( L254.AllTechOutput_USA$supplysector, A54.sector$supplysector ) ]
L254.BaseService_trn_USA <- aggregate( L254.AllTechOutput_USA[ "base.service" ],
      by=as.list( L254.AllTechOutput_USA[ c( names_EnergyFinalDemand, "year" ) ] ), sum )

# -----------------------------------------------------------------------------
##WRITE OUT THE XML FILES THAT WEREN'T WRITTEN OUT IN THE LOOP ABOVE

write_mi_data( L254.StubTranTechCalInput_USA, "StubTranTechCalInput", "GCAMUSA_LEVEL2_DATA", "L254.StubTranTechCalInput_USA", "GCAMUSA_XML_BATCH", "batch_transportation_USA.xml" ) 
write_mi_data( L254.StubTranTechProd_nonmotor_USA, "StubTranTechProd", "GCAMUSA_LEVEL2_DATA", "L254.StubTranTechProd_nonmotor_USA", "GCAMUSA_XML_BATCH", "batch_transportation_USA.xml" ) 
write_mi_data( L254.StubTranTechCalInput_passthru_USA, "StubTranTechCalInput", "GCAMUSA_LEVEL2_DATA", "L254.StubTranTechCalInput_passthru_USA", "GCAMUSA_XML_BATCH", "batch_transportation_USA.xml" )
write_mi_data( L254.BaseService_trn_USA, "BaseService", "GCAMUSA_LEVEL2_DATA", "L254.BaseService_trn_USA", "GCAMUSA_XML_BATCH", "batch_transportation_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_transportation_USA.xml", "GCAMUSA_XML_FINAL", "transportation_USA.xml", "", xml_tag="outFile" )

logstop()
