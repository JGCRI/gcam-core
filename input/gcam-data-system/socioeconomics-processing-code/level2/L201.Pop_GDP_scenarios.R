# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "SOCIOPROC_DIR" ) ){
    if( Sys.getenv( "SOCIOPROC" ) != "" ){
        SOCIOPROC_DIR <- Sys.getenv( "SOCIOPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var SOCIOPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
logstart( "L201.Pop_GDP_scenarios.R" )
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
printlog( "Population and GDP scenarios by GCAM region" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
L101.Pop_thous_GCAM3_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_GCAM3_R_Y" )
L101.Pop_thous_R_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_R_Yh" )
L101.Pop_thous_Scen_R_Yfut <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_Scen_R_Yfut" )
L102.gdp_mil90usd_GCAM3_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.gdp_mil90usd_GCAM3_R_Y" )
L102.gdp_mil90usd_Scen_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.gdp_mil90usd_Scen_R_Y" )
L102.pcgdp_thous90USD_Scen_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_Scen_R_Y" )
L102.PPP_MER_R <- readdata( "SOCIO_LEVEL1_DATA", "L102.PPP_MER_R" )
# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "L201.InterestRate: Interest rates by region" )
L201.InterestRate <- data.frame( region = GCAM_region_names$region, interest.rate = default_interest.rate )

printlog( "L201.Pop_GCAM3: Population by region from the GCAM 3.0 core scenario" )
L201.Pop_GCAM3 <- interpolate_and_melt( L101.Pop_thous_GCAM3_R_Y,
      c( model_base_years, model_future_years ), value.name = "totalPop", digits = digits_Pop )
L201.Pop_GCAM3 <- add_region_name( L201.Pop_GCAM3 )
L201.Pop_GCAM3 <- L201.Pop_GCAM3[ names_Pop ]

#Population in the gSSP SSP scenarios
#Merge the gSSP SSP population table (future) with the historical estimates
L101.Pop_thous_Scen_R_Y <- merge( L101.Pop_thous_R_Yh, L101.Pop_thous_Scen_R_Yfut, all.y = T )
L101.Pop_thous_Scen_R_Y.melt <- interpolate_and_melt( L101.Pop_thous_Scen_R_Y,
      c( model_base_years, model_future_years ), value.name = "totalPop", digits = digits_Pop )
L101.Pop_thous_Scen_R_Y.melt <- add_region_name( L101.Pop_thous_Scen_R_Y.melt )

#Calculation of parameters required for GDP scenario
printlog( "L201.BaseGDP_GCAM3: Base GDP for GCAM 3.0 core scenario")
L201.BaseGDP_GCAM3 <- add_region_name( L102.gdp_mil90usd_GCAM3_R_Y )
L201.BaseGDP_GCAM3$baseGDP <- round( L201.BaseGDP_GCAM3[[ X_model_base_years[1] ]], digits_GDP )
L201.BaseGDP_GCAM3 <- L201.BaseGDP_GCAM3[ names_baseGDP ]

printlog( "L201.BaseGDP_Scen: Base GDP for all Scen scenarios")
L201.BaseGDP_Scen <- add_region_name( L102.gdp_mil90usd_Scen_R_Y[ L102.gdp_mil90usd_Scen_R_Y[[Scen]] == base_gdp_scen, ] )
L201.BaseGDP_Scen$baseGDP <- round( L201.BaseGDP_Scen[[ X_model_base_years[1] ]], digits_GDP )
L201.BaseGDP_Scen <- L201.BaseGDP_Scen[ names_baseGDP ]

printlog( "L201.LaborForceFillout: Labor force participation and productivity for all scenarios")
printlog( "NOTE: No model of labor force used; labor force participation set to a constant" )
L201.LaborForceFillout <- data.frame(
      region = GCAM_region_names$region, year.fillout = min( model_base_years ), laborforce = default_laborforce )

#LABOR PRODUCTIVITY GROWTH RATE CALCULATION
#Labor productivity growth is calculated from the change in per-capita GDP ratio in each time period
#For the GCAM 3.0 scenario, calculate the per-capita GDP
L201.Pop_thous_GCAM3_R_Y <- add_region_name( L101.Pop_thous_GCAM3_R_Y )
L201.gdp_mil90usd_GCAM3_R_Y <- add_region_name( L102.gdp_mil90usd_GCAM3_R_Y )
L201.pcgdp_GCAM3_R_Y <- data.frame( L201.gdp_mil90usd_GCAM3_R_Y[ c( "region", X_model_years ) ] )
L201.pcgdp_GCAM3_R_Y[ X_model_years ] <- L201.gdp_mil90usd_GCAM3_R_Y[ X_model_years ] / L201.Pop_thous_GCAM3_R_Y[
      match( L201.gdp_mil90usd_GCAM3_R_Y$region, L201.Pop_thous_GCAM3_R_Y$region ),
      X_model_years ]

#Calculate the growth rate in per-capita GDP
#GCAM 3.0
L201.pcgdpRatio_GCAM3_R_Y <- L201.pcgdp_GCAM3_R_Y[ c( "region", X_model_years[ 2:length( X_model_years ) ] ) ]
L201.pcgdpRatio_GCAM3_R_Y[ X_model_years[ 2:length( X_model_years ) ] ] <-
      L201.pcgdp_GCAM3_R_Y[ X_model_years[ 2:length( X_model_years ) ] ] /
      L201.pcgdp_GCAM3_R_Y[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]

#gSSPs and SSPs
L201.pcgdp_thous90USD_Scen_R_Y <- add_region_name( L102.pcgdp_thous90USD_Scen_R_Y )
L201.pcgdpRatio_Scen_R_Y <- L201.pcgdp_thous90USD_Scen_R_Y[ c( Scen, "region", X_model_years[ 2:length( X_model_years ) ] ) ]
L201.pcgdpRatio_Scen_R_Y[ X_model_years[ 2:length( X_model_years ) ] ] <-
      L201.pcgdp_thous90USD_Scen_R_Y[ X_model_years[ 2:length( X_model_years ) ] ] /
      L201.pcgdp_thous90USD_Scen_R_Y[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]

#Build a table with timesteps to derive annual growth rates from the ratios
timesteps <- model_years[ 2:length( model_years ) ] - model_years[ 1:( length( model_years ) - 1 ) ]
timesteps_repR <- t( data.frame( timesteps ) )[ rep( 1, times = nrow( L201.pcgdpRatio_GCAM3_R_Y ) ), ]
timesteps_repR_Scen <- t( data.frame( timesteps ) )[ rep( 1, times = nrow( L201.pcgdpRatio_Scen_R_Y ) ), ]

#Annualize the ratios to return annual growth rates
#GCAM 3.0
L201.pcgdpGrowth_GCAM3_R_Y <- L201.pcgdpRatio_GCAM3_R_Y
L201.pcgdpGrowth_GCAM3_R_Y[ X_model_years[ 2:length( X_model_years ) ] ] <-
      round( L201.pcgdpRatio_GCAM3_R_Y[ X_model_years[ 2:length( X_model_years ) ] ] ^ ( 1 / timesteps_repR ) - 1,
             digits_LaborProductivity )

#gSSPs and SSPs
L201.pcgdpGrowth_Scen_R_Y <- L201.pcgdpRatio_Scen_R_Y
L201.pcgdpGrowth_Scen_R_Y[ X_model_years[ 2:length( X_model_years ) ] ] <-
      round( L201.pcgdpRatio_Scen_R_Y[ X_model_years[ 2:length( X_model_years ) ] ] ^ ( 1 / timesteps_repR_Scen ) - 1,
             digits_LaborProductivity )

printlog( "L201.LaborProductivity_GCAM3: Labor force participation and productivity for GCAM 3.0 core scenario")
L201.LaborProductivity_GCAM3 <- interpolate_and_melt( L201.pcgdpGrowth_GCAM3_R_Y, model_years[ 2:length( model_years ) ], value.name = "laborproductivity" )
L201.LaborProductivity_GCAM3 <- L201.LaborProductivity_GCAM3[ names_LaborProductivity ]

# Write out the PPP:MER conversion factors (purely used for reporting)
L201.PPPConvert <- add_region_name( L102.PPP_MER_R )
L201.PPPConvert <- data.frame( region = L201.PPPConvert$region,
      constRatio = 1,
      PPPConvert = round( L201.PPPConvert$PPP_MER, digits_LaborProductivity ) )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L201.InterestRate, "InterestRate", "SOCIO_LEVEL2_DATA", "L201.InterestRate", "SOCIO_XML_BATCH", "batch_interest_rate.xml" ) 

write_mi_data( L201.Pop_GCAM3, "Pop", "SOCIO_LEVEL2_DATA", "L201.Pop_GCAM3", "SOCIO_XML_BATCH", "batch_socioeconomics_GCAM3.xml" ) 
write_mi_data( L201.BaseGDP_GCAM3, "BaseGDP", "SOCIO_LEVEL2_DATA", "L201.BaseGDP_GCAM3", "SOCIO_XML_BATCH", "batch_socioeconomics_GCAM3.xml" ) 
write_mi_data( L201.LaborForceFillout, "LaborForceFillout", "SOCIO_LEVEL2_DATA", "L201.LaborForceFillout", "SOCIO_XML_BATCH", "batch_socioeconomics_GCAM3.xml" ) 
write_mi_data( L201.LaborProductivity_GCAM3, "LaborProductivity", "SOCIO_LEVEL2_DATA", "L201.LaborProductivity_GCAM3", "SOCIO_XML_BATCH", "batch_socioeconomics_GCAM3.xml" ) 
write_mi_data( L201.PPPConvert, "PPPConvert", "SOCIO_LEVEL2_DATA", "L201.PPPConvert", "SOCIO_XML_BATCH", "batch_socioeconomics_GCAM3.xml" ) 

insert_file_into_batchxml( "SOCIO_XML_BATCH", "batch_interest_rate.xml", "SOCIO_XML_FINAL", "interest_rate.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "SOCIO_XML_BATCH", "batch_socioeconomics_GCAM3.xml", "SOCIO_XML_FINAL", "socioeconomics_GCAM3.xml", "", xml_tag="outFile" )

#Split into separate gSSP tables to be written out
printlog( "Split into separate SSP gSSP tables to be written out" )
Scens <- sort( unique( L101.Pop_thous_Scen_R_Y.melt[[Scen]] ) )
for( i in Scens ){
  objectname <- paste0( "L201.Pop_", i )
  object <- L101.Pop_thous_Scen_R_Y.melt[ L101.Pop_thous_Scen_R_Y.melt[[Scen]] == i, names_Pop ]
  batchXMLstring <- paste0( "batch_socioeconomics_", i, ".xml" )
  XMLstring <- sub( "batch_", "", batchXMLstring )

  write_mi_data( object, "Pop", "SOCIO_LEVEL2_DATA", objectname, "SOCIO_XML_BATCH", batchXMLstring )
  write_mi_data( L201.BaseGDP_Scen, "BaseGDP", "SOCIO_LEVEL2_DATA", "L201.BaseGDP_Scen", "SOCIO_XML_BATCH", batchXMLstring ) 
  write_mi_data( L201.LaborForceFillout, "LaborForceFillout", "SOCIO_LEVEL2_DATA", "L201.LaborForceFillout", "SOCIO_XML_BATCH", batchXMLstring ) 
  
  objectname2 <- paste0( "L201.LaborProductivity_", i )
  object2 <- interpolate_and_melt( L201.pcgdpGrowth_Scen_R_Y[
  L201.pcgdpGrowth_Scen_R_Y[[Scen]] == i, ], model_years[ 2:length( model_years ) ], value.name = "laborproductivity" )
  object2 <- object2[ names_LaborProductivity ]
  
  write_mi_data( object2, "LaborProductivity", "SOCIO_LEVEL2_DATA", objectname2, "SOCIO_XML_BATCH", batchXMLstring ) 
  write_mi_data( L201.PPPConvert, "PPPConvert", "SOCIO_LEVEL2_DATA", "L201.PPPConvert", "SOCIO_XML_BATCH", batchXMLstring ) 
  
  insert_file_into_batchxml( "SOCIO_XML_BATCH", batchXMLstring, "SOCIO_XML_FINAL", XMLstring, "", xml_tag="outFile" )
}

logstop()
