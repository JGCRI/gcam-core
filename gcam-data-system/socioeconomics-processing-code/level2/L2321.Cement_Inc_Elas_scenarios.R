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
logstart( "L2321.Cement_Inc_Elas_scenarios.R" )
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
printlog( "Cement income elasticities by socioeconomic scenario and GCAM region" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_ind_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A321.demand <- readdata( "ENERGY_ASSUMPTIONS", "A321.demand" )
A321.inc_elas <- readdata( "SOCIO_ASSUMPTIONS", "A321.inc_elas" )
L101.Pop_thous_GCAM3_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_GCAM3_R_Y" )
L102.gdp_mil90usd_GCAM3_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.gdp_mil90usd_GCAM3_R_Y" )
L102.pcgdp_thous90USD_SSP_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_SSP_R_Y" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "L2321.IncomeElasticity_cement_GCAM3: cement sector income elasticity for GCAM 3.0 socioeconomics" )
#For the GCAM 3.0 scenario, calculate the per-capita GDP
L2321.Pop_thous_GCAM3_R_Y <- add_region_name( L101.Pop_thous_GCAM3_R_Y )
L2321.gdp_mil90usd_GCAM3_R_Y <- add_region_name( L102.gdp_mil90usd_GCAM3_R_Y )
L2321.pcgdp_GCAM3_R_Y <- data.frame( L2321.gdp_mil90usd_GCAM3_R_Y[ c( "region", X_model_years ) ] )
L2321.pcgdp_GCAM3_R_Y[ X_model_years ] <- L2321.gdp_mil90usd_GCAM3_R_Y[ X_model_years ] / L2321.Pop_thous_GCAM3_R_Y[
      match( L2321.gdp_mil90usd_GCAM3_R_Y$region, L2321.Pop_thous_GCAM3_R_Y$region ),
      X_model_years ]
L2321.IncomeElasticity_cement_GCAM3 <- interpolate_and_melt( L2321.pcgdp_GCAM3_R_Y, model_future_years, value = "pcgdp_90thousUSD" )

#Match in the income elasticity for each GDP point
L2321.IncomeElasticity_cement_GCAM3$income.elasticity <- round(
      approx( A321.inc_elas$pcgdp_90thousUSD, A321.inc_elas$inc_elas, xout = L2321.IncomeElasticity_cement_GCAM3$pcgdp_90thousUSD, rule = 2 )$y,
      digits_IncElas_ind )
L2321.IncomeElasticity_cement_GCAM3$energy.final.demand <- A321.demand$energy.final.demand
L2321.IncomeElasticity_cement_GCAM3 <- L2321.IncomeElasticity_cement_GCAM3[ names_IncomeElasticity]

#SSPs
L2321.pcgdp_thous90USD_SSP_R_Y <- add_region_name( L102.pcgdp_thous90USD_SSP_R_Y )
L2321.pcgdp_thous90USD_SSP_R_Y.melt <- interpolate_and_melt( L2321.pcgdp_thous90USD_SSP_R_Y, model_future_years, value.name = "pcgdp_90thousUSD" )
L2321.pcgdp_thous90USD_SSP_R_Y.melt$income.elasticity <- round(
      approx( A321.inc_elas$pcgdp_90thousUSD, A321.inc_elas$inc_elas, xout = L2321.pcgdp_thous90USD_SSP_R_Y.melt$pcgdp_90thousUSD, rule = 2 )$y,
      digits_IncElas_ind )
L2321.pcgdp_thous90USD_SSP_R_Y.melt$energy.final.demand <- A321.demand$energy.final.demand

printlog( "L2321.IncomeElasticity_cement_SSP1")
L2321.IncomeElasticity_cement_SSP1 <- L2321.pcgdp_thous90USD_SSP_R_Y.melt[ L2321.pcgdp_thous90USD_SSP_R_Y.melt[[Scen]] == "SSP1", names_IncomeElasticity ]

printlog( "L2321.IncomeElasticity_cement_SSP2")
L2321.IncomeElasticity_cement_SSP2 <- L2321.pcgdp_thous90USD_SSP_R_Y.melt[ L2321.pcgdp_thous90USD_SSP_R_Y.melt[[Scen]] == "SSP2", names_IncomeElasticity ]

printlog( "L2321.IncomeElasticity_cement_SSP3")
L2321.IncomeElasticity_cement_SSP3 <- L2321.pcgdp_thous90USD_SSP_R_Y.melt[ L2321.pcgdp_thous90USD_SSP_R_Y.melt[[Scen]] == "SSP3", names_IncomeElasticity ]

printlog( "L2321.IncomeElasticity_cement_SSP4")
L2321.IncomeElasticity_cement_SSP4 <- L2321.pcgdp_thous90USD_SSP_R_Y.melt[ L2321.pcgdp_thous90USD_SSP_R_Y.melt[[Scen]] == "SSP4", names_IncomeElasticity ]

printlog( "L2321.IncomeElasticity_cement_SSP5")
L2321.IncomeElasticity_cement_SSP5 <- L2321.pcgdp_thous90USD_SSP_R_Y.melt[ L2321.pcgdp_thous90USD_SSP_R_Y.melt[[Scen]] == "SSP5", names_IncomeElasticity ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2321.IncomeElasticity_cement_GCAM3, "IncomeElasticity", "SOCIO_LEVEL2_DATA", "L2321.IncomeElasticity_cement_GCAM3", "SOCIO_XML_BATCH", "batch_cement_GCAM3.xml" ) 
write_mi_data( L2321.IncomeElasticity_cement_SSP1, "IncomeElasticity", "SOCIO_LEVEL2_DATA", "L2321.IncomeElasticity_cement_SSP1", "SOCIO_XML_BATCH", "batch_cement_SSP1.xml" ) 
write_mi_data( L2321.IncomeElasticity_cement_SSP2, "IncomeElasticity", "SOCIO_LEVEL2_DATA", "L2321.IncomeElasticity_cement_SSP2", "SOCIO_XML_BATCH", "batch_cement_SSP2.xml" ) 
write_mi_data( L2321.IncomeElasticity_cement_SSP3, "IncomeElasticity", "SOCIO_LEVEL2_DATA", "L2321.IncomeElasticity_cement_SSP3", "SOCIO_XML_BATCH", "batch_cement_SSP3.xml" ) 
write_mi_data( L2321.IncomeElasticity_cement_SSP4, "IncomeElasticity", "SOCIO_LEVEL2_DATA", "L2321.IncomeElasticity_cement_SSP4", "SOCIO_XML_BATCH", "batch_cement_SSP4.xml" ) 
write_mi_data( L2321.IncomeElasticity_cement_SSP5, "IncomeElasticity", "SOCIO_LEVEL2_DATA", "L2321.IncomeElasticity_cement_SSP5", "SOCIO_XML_BATCH", "batch_cement_SSP5.xml" ) 

insert_file_into_batchxml( "SOCIO_XML_BATCH", "batch_cement_GCAM3.xml", "SOCIO_XML_FINAL", "cement_GCAM3.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "SOCIO_XML_BATCH", "batch_cement_SSP1.xml", "SOCIO_XML_FINAL", "cement_SSP1.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "SOCIO_XML_BATCH", "batch_cement_SSP2.xml", "SOCIO_XML_FINAL", "cement_SSP2.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "SOCIO_XML_BATCH", "batch_cement_SSP3.xml", "SOCIO_XML_FINAL", "cement_SSP3.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "SOCIO_XML_BATCH", "batch_cement_SSP4.xml", "SOCIO_XML_FINAL", "cement_SSP4.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "SOCIO_XML_BATCH", "batch_cement_SSP5.xml", "SOCIO_XML_FINAL", "cement_SSP5.xml", "", xml_tag="outFile" )

logstop()
