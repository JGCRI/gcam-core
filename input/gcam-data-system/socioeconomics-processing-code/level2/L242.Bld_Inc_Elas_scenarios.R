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
logstart( "L242.Bld_Inc_Elas_scenarios.R" )
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
printlog( "Building sector income elasticities by socioeconomic scenario and GCAM region" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A42.demand <- readdata( "ENERGY_ASSUMPTIONS", "A42.demand" )
A42.inc_elas <- readdata( "SOCIO_ASSUMPTIONS", "A42.inc_elas" )
L101.Pop_thous_GCAM3_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_GCAM3_R_Y" )
L102.gdp_mil90usd_GCAM3_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.gdp_mil90usd_GCAM3_R_Y" )
L102.pcgdp_thous90USD_Scen_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_Scen_R_Y" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "L242.IncomeElasticity_bld_GCAM3: building sector income elasticity for GCAM 3.0 socioeconomics" )
#For the GCAM 3.0 scenario, calculate the per-capita GDP
L242.Pop_thous_GCAM3_R_Y <- add_region_name( L101.Pop_thous_GCAM3_R_Y )
L242.gdp_mil90usd_GCAM3_R_Y <- add_region_name( L102.gdp_mil90usd_GCAM3_R_Y )
L242.pcgdp_GCAM3_R_Y <- data.frame( L242.gdp_mil90usd_GCAM3_R_Y[ c( "region", X_model_years ) ] )
L242.pcgdp_GCAM3_R_Y[ X_model_years ] <- L242.gdp_mil90usd_GCAM3_R_Y[ X_model_years ] / L242.Pop_thous_GCAM3_R_Y[
      match( L242.gdp_mil90usd_GCAM3_R_Y$region, L242.Pop_thous_GCAM3_R_Y$region ),
      X_model_years ]
L242.IncomeElasticity_bld_GCAM3 <- interpolate_and_melt( L242.pcgdp_GCAM3_R_Y, model_future_years, value = "pcgdp_90thousUSD" )

#Match in the income elasticity for each GDP point
L242.IncomeElasticity_bld_GCAM3$income.elasticity <- round(
      approx( A42.inc_elas$pcgdp_90thousUSD, A42.inc_elas$inc_elas, xout = L242.IncomeElasticity_bld_GCAM3$pcgdp_90thousUSD, rule = 2 )$y,
      digits_IncElas_bld )
L242.IncomeElasticity_bld_GCAM3$energy.final.demand <- A42.demand$energy.final.demand
L242.IncomeElasticity_bld_GCAM3 <- L242.IncomeElasticity_bld_GCAM3[ names_IncomeElasticity]

#Scens GSPs
L242.pcgdp_thous90USD_Scen_R_Y <- add_region_name( L102.pcgdp_thous90USD_Scen_R_Y )
L242.pcgdp_thous90USD_Scen_R_Y.melt <- interpolate_and_melt( L242.pcgdp_thous90USD_Scen_R_Y, model_future_years, value.name = "pcgdp_90thousUSD" )
L242.pcgdp_thous90USD_Scen_R_Y.melt$income.elasticity <- round(
      approx( A42.inc_elas$pcgdp_90thousUSD, A42.inc_elas$inc_elas, xout = L242.pcgdp_thous90USD_Scen_R_Y.melt$pcgdp_90thousUSD, rule = 2 )$y,
      digits_IncElas_bld )
L242.pcgdp_thous90USD_Scen_R_Y.melt$energy.final.demand <- A42.demand$energy.final.demand

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L242.IncomeElasticity_bld_GCAM3, "IncomeElasticity", "SOCIO_LEVEL2_DATA", "L242.IncomeElasticity_bld_GCAM3", "SOCIO_XML_BATCH", "batch_bld_agg_GCAM3.xml" ) 

insert_file_into_batchxml( "SOCIO_XML_BATCH", "batch_bld_agg_GCAM3.xml", "SOCIO_XML_FINAL", "bld_agg_GCAM3.xml", "", xml_tag="outFile" )

printlog( "Writing out Scen  GSP files in a for loop" )
Scens <- sort( unique( L242.pcgdp_thous90USD_Scen_R_Y.melt[[Scen]] ) )
for( i in Scens ){
	objectname <- paste0( "L242.IncomeElasticity_bld_", i )
	object <- L242.pcgdp_thous90USD_Scen_R_Y.melt[ L242.pcgdp_thous90USD_Scen_R_Y.melt[[Scen]] == i, names_IncomeElasticity ]
	batchXMLstring <- paste0( "batch_bld_agg_", i, ".xml" )
	write_mi_data( object, "IncomeElasticity", "SOCIO_LEVEL2_DATA", objectname, "SOCIO_XML_BATCH", batchXMLstring )
	XMLstring <- sub( "batch_", "", batchXMLstring )
	insert_file_into_batchxml( "SOCIO_XML_BATCH", batchXMLstring, "SOCIO_XML_FINAL", XMLstring, "", xml_tag="outFile" )
}

logstop()
