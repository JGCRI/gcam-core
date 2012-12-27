#------------------------------------------------------------------------------------
#MODEL PERIODS FOR ANALYSIS
base_years <- c( 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2008 )
X_base_years <- paste( "X", base_years, sep = "" )
RECS_years <- c( 1979, 1990, 1993, 1997, 2001, 2005 )
X_RECS_years <- paste( "X", RECS_years, sep = "" )
GCAM_years <- seq( 1990, 2095, by = 15 )
X_GCAM_years <- paste( "X", GCAM_years, sep = "" )
GCAM_base_years <- c( 1990, 2005 )
X_GCAM_base_years <- paste( "X", GCAM_base_years, sep = "" )
GCAM_future_years <- seq( 2020, 2095, by = 15 )
X_GCAM_future_years <- paste( "X", GCAM_future_years, sep = "" )
GCAM_base_shareweights <- c( "share_weight_1990", "share_weight_2005" )
GCAM_model_period0 <- 1975
X_GCAM_model_period0 <- "X1975"
final_cal_year <- 2005
X_final_cal_year <- "X2005"
GCAM_out_years <- c( 1990, seq( 2005, 2095, by = 5 ) )
X_GCAM_out_years <- paste( "X", GCAM_out_years, sep = "" )

#Identifier columns
state_S_F <- c( "state", "GCAM_sector", "GCAM_fuel" )
sup_sub_tech <- c( "supplysector", "subsector", "technology" )
sup_sub_input <- c( "supplysector", "subsector", "minicam_energy_input" )
sup_sub_tech_input <- c( "supplysector", "subsector", "technology", "minicam_energy_input" )
R_sup_sub_tech_input <- c( "region", "supplysector", "subsector", "technology", "minicam_energy_input" )
R_sup_sub_stubtech_input <- c( "region", "supplysector", "subsector", "stub_technology", "minicam_energy_input" )
globaltech_ID <- c( "sector_name", "subsector_name", "technology" )

#list the states
states <- c( "AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA",
             "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", 
             "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY" )

#Conversions
conv_milft2_bm2 <- 0.0929/1e3     #million square feet to billion square meters
conv_ft2_bm2 <- 0.0929/1e9        #square feet to billion square meters

conv_Tbtu_EJ <- 0.0010551         #trillion btu to EJ
conv_kbtu_EJ <- 1.0551e-12        #thousand btu to EJ

conv_TWh_EJ <- 0.0036

conv_CO2_C <- 12/44

# Dollar year conversions
conv_1996_1975_USD <- 0.4049
conv_1997_1975_USD <- 0.3983
conv_1998_1975_USD <- 0.3939
conv_1999_1975_USD <- 0.3883
conv_2000_1975_USD <- 0.380
conv_2001_1975_USD <- 0.3711
conv_2002_1975_USD <- 0.3647
conv_2003_1975_USD <- 0.3571
conv_2004_1975_USD <- 0.3472
conv_2005_1975_USD <- 0.3362
conv_2006_1975_USD <- 0.3257
conv_2007_1975_USD <- 0.317
conv_2008_1975_USD <- 0.3104
conv_2009_1975_USD <- 0.3104


conv_2004_1990_USD <- 0.7454
conv_2005_1990_USD <- 0.7218

#Shorthand names for residential buildings services in the RECS databases
RECS_services_elec <- c( "sph", "col", "wth", "lgt", "apl", "othapl", "oth" )
RECS_services_gas <- c( "sph", "wth", "apl", "oth" )
RECS_services_oil <- c( "sph", "wth", "apl", "oth" )
GCAM_res_otherU <- c( "resid lighting", "resid appliances", "resid other appliances", "resid other" )

#services in the CBECS databases
CBECS_services_elec <- c( "sph", "col", "wth", "vnt", "ckg", "lgt", "rfg", "ofc", "othint" )
CBECS_services_gas <- c( "sph", "col", "wth", "ckg", "othint" )
CBECS_services_oil <- c( "sph", "wth", "othint" )

#select the number of decimal points to round
CalInput_digits <- 6
flsp_digits <- 6
flsp_sat_digits <- 9
HDDCDD_digits <- 0
cost_digits <- 3
intgains_digits <- 3
retirement_fn_digits <- 3
BEND_efficiency_digits <- 4
BEND_kbtu_digits <- 0
capacity_factor_digits <- 2

P_elas_flsp <- -1         #price elasticity of floorspace

#Set the satiation level of floorspace, compared with base year per-capita floorspace demands
satiation_flsp_mult <- 1.118

#Buildings node structure
bld_node_names <- c( "gcam_consumer", "nodeInput", "building_node_input" )

#buildings service names
thermal_services <- c( "resid heating", "resid cooling", "comm heating", "comm cooling" )
heating_services <- c( "resid heating", "comm heating" )
cooling_services <- c( "resid cooling", "comm cooling" )

floor_to_surface <- 5.5

coal_phaseout_year <- 2020   #the year at which coal use in buildings is assumed to go to 0

BEND_techID <- c( "BEND_sector", "BEND_service", "BEND_fuel", "BEND_technology" )




