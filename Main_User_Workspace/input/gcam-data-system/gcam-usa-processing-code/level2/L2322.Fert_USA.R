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
logstart( "L2322.Fert_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for GCAM USA N fertilizer sector" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_ccs_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
A322.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A322.globaltech_coef" )
L2322.Supplysector_Fert <- readdata( "ENERGY_LEVEL2_DATA", "L2322.Supplysector_Fert", skip = 4 )
L2322.FinalEnergyKeyword_Fert <- readdata( "ENERGY_LEVEL2_DATA", "L2322.FinalEnergyKeyword_Fert", skip = 4 )
L2322.SubsectorLogit_Fert <- readdata( "ENERGY_LEVEL2_DATA", "L2322.SubsectorLogit_Fert", skip = 4 )
L2322.SubsectorShrwt_Fert <- readdata( "ENERGY_LEVEL2_DATA", "L2322.SubsectorShrwt_Fert", skip = 4, must.exist = F )
L2322.SubsectorShrwtFllt_Fert <- readdata( "ENERGY_LEVEL2_DATA", "L2322.SubsectorShrwtFllt_Fert", skip = 4, must.exist = F )
L2322.SubsectorInterp_Fert <- readdata( "ENERGY_LEVEL2_DATA", "L2322.SubsectorInterp_Fert", skip = 4, must.exist = F )
L2322.SubsectorInterpTo_Fert <- readdata( "ENERGY_LEVEL2_DATA", "L2322.SubsectorInterpTo_Fert", skip = 4, must.exist = F )
L2322.StubTech_Fert <- readdata( "ENERGY_LEVEL2_DATA", "L2322.StubTech_Fert", skip = 4 )
L1322.IO_GJkg_state_Fert_F_Yh <- readdata( "GCAMUSA_LEVEL1_DATA", "L1322.IO_GJkg_state_Fert_F_Yh" )
L1322.out_Mt_state_Fert_Yh <- readdata( "GCAMUSA_LEVEL1_DATA", "L1322.out_Mt_state_Fert_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# For fertilizer, we keep the USA sector because its output is consumed by the techs in the aglu module
printlog( "N fertilizer is retained as a sector in the USA region, as is the Imports subsector.
          The fuel subsectors are deleted, replaced with state subsectors")
printlog( "L2322.DeleteSubsector_USAFert: delete fuel subsectors in USA N fertilizer sector" )
L2322.DeleteSubsector_USAFert <- subset( L2322.SubsectorLogit_Fert,
      region == "USA" & supplysector == Fert_name & subsector != "Imports",
      select = names_Subsector )
write_mi_data( L2322.DeleteSubsector_USAFert, "DeleteSubsector", "GCAMUSA_LEVEL2_DATA", "L2322.DeleteSubsector_USAFert", "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml" )

#Remove the keyword
L2322.FinalEnergyKeyword_USAFert <- subset( L2322.FinalEnergyKeyword_Fert, region == "USA" )
L2322.FinalEnergyKeyword_USAFert$final.energy <- "none"

printlog( "NOTE: N fertilizer sectors are only created in states where the Census data indicate production" )
Fert_states <- unique( L1322.out_Mt_state_Fert_Yh$state )

# The USA N fertilizer sector is logited among the states that produce this commodity
# Write out each state's fertilizer sector as a subsector in the USA's fertilizer sector
printlog( "L2322.SubsectorLogit_USAFert: subsector logit exponents, USA region" )
L2322.SubsectorLogit_USAFert <- repeat_and_add_vector( subset( L2322.Supplysector_Fert, region == "USA" & supplysector == Fert_name, select = c( reg, supp ) ), state, Fert_states )
L2322.SubsectorLogit_USAFert[[subs]] <- paste( L2322.SubsectorLogit_USAFert$state, Fert_name, sep = " " )
L2322.SubsectorLogit_USAFert$logit.year.fillout <- min( model_base_years )
L2322.SubsectorLogit_USAFert$logit.exponent <- default_subsector_logit
L2322.SubsectorLogit_USAFert <- L2322.SubsectorLogit_USAFert[ names_SubsectorLogit ]

#Subsector shareweights
printlog( "L2322.SubsectorShrwtFllt_USAFert: subsector default shareweights, USA region" )
L2322.SubsectorShrwtFllt_USAFert <- L2322.SubsectorLogit_USAFert[ c( reg, supp, subs ) ]
L2322.SubsectorShrwtFllt_USAFert$year.fillout <- min( model_base_years )
L2322.SubsectorShrwtFllt_USAFert$share.weight <- 1

printlog( "L2322.SubsectorInterp_USAFert: subsector shareweight interpolation, USA region" )
L2322.SubsectorInterp_USAFert <- L2322.SubsectorLogit_USAFert[ c( reg, supp, subs ) ]
L2322.SubsectorInterp_USAFert$apply.to <- "share-weight"
L2322.SubsectorInterp_USAFert$from.year <- max( model_base_years )
L2322.SubsectorInterp_USAFert$to.year <- max( model_years )
L2322.SubsectorInterp_USAFert$interpolation.function <- "fixed"

printlog( "L2322.TechShrwt_USAFert: technology shareweights, USA region")
L2322.TechShrwt_USAFert <- L2322.SubsectorLogit_USAFert[ c( reg, supp, subs ) ]
L2322.TechShrwt_USAFert[[tech]] <- L2322.TechShrwt_USAFert[[subs]]
L2322.TechShrwt_USAFert <- repeat_and_add_vector( L2322.TechShrwt_USAFert, Y, model_years )
L2322.TechShrwt_USAFert$share.weight <- 1

printlog( "L2322.Production_USAFert: calibrated production in USA region fertilizer sector (consuming output of states)" )
L2322.Production_USAFert <- interpolate_and_melt( L1322.out_Mt_state_Fert_Yh, model_base_years, value.name = "calOutputValue", digits = digits_calOutput )
L2322.Production_USAFert[[reg]] <- "USA"
L2322.Production_USAFert[[supp]] <- Fert_name
L2322.Production_USAFert[[subs]] <- paste( L2322.Production_USAFert$state, Fert_name, sep = " " )
L2322.Production_USAFert[[tech]] <- L2322.Production_USAFert[[subs]]
L2322.Production_USAFert[[input]] <- Fert_name
L2322.Production_USAFert$share.weight.year <- L2322.Production_USAFert$year
L2322.Production_USAFert$subs.share.weight <- ifelse( L2322.Production_USAFert$calOutputValue == 0, 0, 1 )
L2322.Production_USAFert$tech.share.weight <- L2322.Production_USAFert$subs.share.weight
L2322.Production_USAFert <- L2322.Production_USAFert[ names_Production ]

printlog( "L2322.TechCoef_USAFert: coefficients of USA region fertilizer" )
L2322.TechCoef_USAFert <- L2322.TechShrwt_USAFert
L2322.TechCoef_USAFert[[input]] <- Fert_name
L2322.TechCoef_USAFert$coefficient <- 1
L2322.TechCoef_USAFert$market.name <- substr( L2322.TechCoef_USAFert[[subs]], 1, nchar( L2322.TechCoef_USAFert[[subs]] ) - nchar( Fert_name ) - 1 )
L2322.TechCoef_USAFert <- L2322.TechCoef_USAFert[ names_TechCoef ]

printlog( "All tables for which processing is identical are done in a for loop")
printlog( "NOTE: writing out the tables in this step as well")
L2322.tables <- list( L2322.FinalEnergyKeyword_Fert = L2322.FinalEnergyKeyword_Fert,
                      L2322.SubsectorLogit_Fert = L2322.SubsectorLogit_Fert,
                      L2322.SubsectorShrwt_Fert = if( !is.null( L2322.SubsectorShrwt_Fert ) ) L2322.SubsectorShrwt_Fert,
                      L2322.SubsectorShrwtFllt_Fert = if( !is.null( L2322.SubsectorShrwtFllt_Fert ) ) L2322.SubsectorShrwtFllt_Fert,
                      L2322.SubsectorInterp_Fert = if( !is.null( L2322.SubsectorInterp_Fert ) ) L2322.SubsectorInterp_Fert,
                      L2322.SubsectorInterpTo_Fert = if( !is.null( L2322.SubsectorInterpTo_Fert ) ) L2322.SubsectorInterpTo_Fert,
                      L2322.StubTech_Fert = L2322.StubTech_Fert )

for( i in 1:length( L2322.tables ) ){
  if( !is.null( L2322.tables[[i]] ) ){
  	  objectname <- paste0( names( L2322.tables[i] ), "_USA" )
# state-level Exports_fertilizer sector should be excluded
	  object <- write_to_all_states( subset( L2322.tables[[i]], region == "USA" & supplysector == Fert_name ), names( L2322.tables[[i]] ) )
# N fertilizer sector is not created in states where production is omitted from the census data. subset only the states where it exists
	  object <- subset( object, region %in% Fert_states )
# state-level N fertilizer should not include the Imports subsector. no need for the alternative fuels either; just gas
	  if( "subsector" %in% names( object ) ) object <- subset( object, subsector == "gas" )
	  assign( objectname, object )
	  IDstringendpoint <- if( grepl( "_", names( L2322.tables )[i] ) ) { regexpr( "_", names( L2322.tables )[i], fixed = T ) - 1
	                       } else nchar( names( L2322.tables )[i] )
	  IDstring <- substr( names( L2322.tables )[i], 7, IDstringendpoint )
	  write_mi_data( object, IDstring, "GCAMUSA_LEVEL2_DATA", objectname, "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml" )
	  }
  }

printlog( "L2322.StubTechProd_Fert_USA: calibrated fertilizer production by state" )
L2322.StubTechProd_Fert_USA <- interpolate_and_melt( L1322.out_Mt_state_Fert_Yh, model_base_years, value.name = "calOutputValue", digits_calOutput )
L2322.StubTechProd_Fert_USA$region <- L2322.StubTechProd_Fert_USA$state
L2322.StubTechProd_Fert_USA[ s_s_t ] <- calibrated_techs[
      match( paste( L2322.StubTechProd_Fert_USA$sector, "output" ),  #Only take the tech IDs where the calibration is identified as output
             paste( calibrated_techs$sector, calibrated_techs$calibration ) ), s_s_t ]
L2322.StubTechProd_Fert_USA$stub.technology <- L2322.StubTechProd_Fert_USA$technology
L2322.StubTechProd_Fert_USA$share.weight.year <- L2322.StubTechProd_Fert_USA[[Y]]
L2322.StubTechProd_Fert_USA$subs.share.weight <- ifelse( L2322.StubTechProd_Fert_USA$calOutputValue > 0, 1, 0 )
L2322.StubTechProd_Fert_USA$tech.share.weight <- L2322.StubTechProd_Fert_USA$subs.share.weight
L2322.StubTechProd_Fert_USA <- L2322.StubTechProd_Fert_USA[ names_StubTechProd ]

printlog( "L2322.StubTechCoef_Fert_USA: coefficients of fertilizer production technologies" )
#Only historical periods. Need another table to write out the market in the future years
L2322.StubTechCoef_Fert_USA <- interpolate_and_melt( L1322.IO_GJkg_state_Fert_F_Yh, model_base_years, value.name = "coefficient", digits_coefficient )
L2322.StubTechCoef_Fert_USA$region <- L2322.StubTechCoef_Fert_USA$state
L2322.StubTechCoef_Fert_USA[ s_s_t_i ] <- calibrated_techs[ match( vecpaste( L2322.StubTechCoef_Fert_USA[ S_F ] ), vecpaste( calibrated_techs[ S_F ] ) ), s_s_t_i ]
L2322.StubTechCoef_Fert_USA$stub.technology <- L2322.StubTechCoef_Fert_USA$technology
L2322.StubTechCoef_Fert_USA$market.name <- "USA"
L2322.StubTechCoef_Fert_USA <- L2322.StubTechCoef_Fert_USA[ names_StubTechCoef ]

if( use_regional_fuel_markets ){
	L2322.StubTechCoef_Fert_USA$market.name[ L2322.StubTechCoef_Fert_USA[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
	      match( L2322.StubTechCoef_Fert_USA$region[ L2322.StubTechCoef_Fert_USA[[input]] %in% regional_fuel_markets ],
	             states_subregions$state ) ]
}

printlog( "NOTE: electricity is consumed from state markets" )
L2322.StubTechCoef_Fert_USA$market.name[ L2322.StubTechCoef_Fert_USA[[input]] %in% elect_td_sectors ] <-
      L2322.StubTechCoef_Fert_USA$region[ L2322.StubTechCoef_Fert_USA[[input]] %in% elect_td_sectors ]

printlog( "L2322.StubTechMarket_Fert_USA: market for the fuel inputs to the state fertilizer sectors" )
L2322.StubTechMarket_Fert_USA <- repeat_and_add_vector( L2322.StubTech_Fert_USA, Y, model_years )
L2322.StubTechMarket_Fert_USA[[input]] <- A322.globaltech_coef[[input]][
      match( vecpaste( L2322.StubTechMarket_Fert_USA[ c( supp, subs, "stub.technology") ] ),
             vecpaste( A322.globaltech_coef[ s_s_t ] ) ) ]
L2322.StubTechMarket_Fert_USA$market.name <- "USA"

if( use_regional_fuel_markets ){
	L2322.StubTechMarket_Fert_USA$market.name[ L2322.StubTechMarket_Fert_USA[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
	      match( L2322.StubTechMarket_Fert_USA$region[ L2322.StubTechMarket_Fert_USA[[input]] %in% regional_fuel_markets ],
	             states_subregions$state ) ]
}

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2322.FinalEnergyKeyword_USAFert, "FinalEnergyKeyword", "GCAMUSA_LEVEL2_DATA", "L2322.FinalEnergyKeyword_USAFert", "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml" )
write_mi_data( L2322.SubsectorLogit_USAFert, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L2322.SubsectorLogit_USAFert", "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml" )
write_mi_data( L2322.SubsectorShrwtFllt_USAFert, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L2322.SubsectorShrwtFllt_USAFert", "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml" )
write_mi_data( L2322.SubsectorInterp_USAFert, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L2322.SubsectorInterp_USAFert", "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml" )
write_mi_data( L2322.TechShrwt_USAFert, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L2322.TechShrwt_USAFert", "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml" )
write_mi_data( L2322.Production_USAFert, "Production", "GCAMUSA_LEVEL2_DATA", "L2322.Production_USAFert", "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml" )
write_mi_data( L2322.TechCoef_USAFert, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L2322.TechCoef_USAFert", "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml" )
write_mi_data( L2322.StubTechProd_Fert_USA, "StubTechProd", "GCAMUSA_LEVEL2_DATA", "L2322.StubTechProd_Fert_USA", "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml" )
write_mi_data( L2322.StubTechCoef_Fert_USA, "StubTechCoef", "GCAMUSA_LEVEL2_DATA", "L2322.StubTechCoef_Fert_USA", "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml" )
write_mi_data( L2322.StubTechMarket_Fert_USA, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L2322.StubTechMarket_Fert_USA", "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_Fert_USA.xml", "GCAMUSA_XML_FINAL", "Fert_USA.xml", "", xml_tag="outFile" )

logstop()
