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
logstart( "L201.socioeconomics_USA.R" )
printlog( "GCAM-USA population and GDP" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
L100.pcGDP_thous90usd_state <- readdata( "GCAMUSA_LEVEL1_DATA", "L100.pcGDP_thous90usd_state" )
L100.Pop_thous_state <- readdata( "GCAMUSA_LEVEL1_DATA", "L100.Pop_thous_state" )
L100.GDP_mil90usd_state <- readdata("GCAMUSA_LEVEL1_DATA","L100.GDP_mil90usd_state")
L102.pcgdp_thous90USD_GCAM3_ctry_Y <- readdata( "SOCIO_LEVEL1_DATA" , "L102.pcgdp_thous90USD_GCAM3_ctry_Y" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "L201.InterestRate: Interest rates by region" )
L201.InterestRate <- data.frame( region = states_subregions$state, interest.rate = default_interest.rate )

printlog( "L201.Pop_GCAMUSA: Population by region from the GCAM 3.0 core scenario" )
L201.Pop_GCAMUSA <- interpolate_and_melt( L100.Pop_thous_state,
      c( model_base_years, model_future_years ), value.name = "totalPop", digits = digits_Pop )
L201.Pop_GCAMUSA$region <- L201.Pop_GCAMUSA$state
L201.Pop_GCAMUSA <- L201.Pop_GCAMUSA[ names_Pop ]

printlog( "L201.BaseGDP_GCAMUSA: Base GDP for GCAM-USA scenario")
L201.BaseGDP_GCAMUSA <- interpolate_and_melt( L100.GDP_mil90usd_state,
      min( model_base_years ), value.name = "baseGDP", digits = digits_GDP )
L201.BaseGDP_GCAMUSA$region <- L201.BaseGDP_GCAMUSA$state
L201.BaseGDP_GCAMUSA <- L201.BaseGDP_GCAMUSA[ names_baseGDP ]

printlog( "L201.LaborForceFillout: Labor force participation and productivity for all scenarios")
printlog( "NOTE: No model of labor force used; labor force participation set to a constant" )
L201.LaborForceFillout <- data.frame(
      region = states_subregions$state, year.fillout = min( model_base_years ), laborforce = default_laborforce )

#LABOR PRODUCTIVITY GROWTH RATE CALCULATION
#Labor productivity growth is calculated from the change in per-capita GDP ratio in each time period
#Calculate the growth rate in per-capita GDP
L201.pcgdp_thous90USD_usa <- subset( L102.pcgdp_thous90USD_GCAM3_ctry_Y, iso == "usa" )
L201.pcgdpRatio_GCAMUSA_Y <- L201.pcgdp_thous90USD_usa[ c( "iso", X_model_years[ 2:length( X_model_years ) ] ) ]
L201.pcgdpRatio_GCAMUSA_Y[ X_model_years[ 2:length( X_model_years ) ] ] <-
      L201.pcgdp_thous90USD_usa[ X_model_years[ 2:length( X_model_years ) ] ] /
      L201.pcgdp_thous90USD_usa[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]

#Build a table with timesteps to derive annual growth rates from the ratios
timesteps <- model_years[ 2:length( model_years ) ] - model_years[ 1:( length( model_years ) - 1 ) ]

#Annualize the ratios to return annual growth rates
L201.pcgdpGrowth_GCAMUSA_Y <- L201.pcgdpRatio_GCAMUSA_Y
L201.pcgdpGrowth_GCAMUSA_Y[ X_model_years[ 2:length( X_model_years ) ] ] <-
      round( L201.pcgdpRatio_GCAMUSA_Y[ X_model_years[ 2:length( X_model_years ) ] ] ^ ( 1 / timesteps ) - 1,
             digits_LaborProductivity )

printlog( "L201.LaborProductivity_GCAM3: Labor force productivity growth rate for GCAMUSA scenario")
printlog( "NOTE: applying the USA average to all states equally" )
L201.LaborProductivity_GCAMUSA <- interpolate_and_melt( L201.pcgdpGrowth_GCAMUSA_Y, model_years[ 2:length( model_years ) ], value.name = "laborproductivity" )
L201.LaborProductivity_GCAMUSA <- write_to_all_states( L201.LaborProductivity_GCAMUSA, names_LaborProductivity )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L201.InterestRate, "InterestRate", "GCAMUSA_LEVEL2_DATA", "L201.InterestRate", "GCAMUSA_XML_BATCH", "batch_interest_rate_USA.xml" ) 
write_mi_data( L201.Pop_GCAMUSA, "Pop", "GCAMUSA_LEVEL2_DATA", "L201.Pop_GCAMUSA", "GCAMUSA_XML_BATCH", "batch_socioeconomics_USA.xml" ) 
write_mi_data( L201.BaseGDP_GCAMUSA, "BaseGDP", "GCAMUSA_LEVEL2_DATA", "L201.BaseGDP_GCAMUSA", "GCAMUSA_XML_BATCH", "batch_socioeconomics_USA.xml" ) 
write_mi_data( L201.LaborForceFillout, "LaborForceFillout", "GCAMUSA_LEVEL2_DATA", "L201.LaborForceFillout", "GCAMUSA_XML_BATCH", "batch_socioeconomics_USA.xml" ) 
write_mi_data( L201.LaborProductivity_GCAMUSA, "LaborProductivity", "GCAMUSA_LEVEL2_DATA", "L201.LaborProductivity_GCAMUSA", "GCAMUSA_XML_BATCH", "batch_socioeconomics_USA.xml" ) 

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_interest_rate_USA.xml", "GCAMUSA_XML_FINAL", "interest_rate_USA.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_socioeconomics_USA.xml", "GCAMUSA_XML_FINAL", "socioeconomics_USA.xml", "", xml_tag="outFile" )

logstop()
