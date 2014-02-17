
#At present the CO2 emissions inventory from CDIAC stops at 2009
CO2_historical_years <- historical_years[ historical_years < 2010 ]
X_CO2_historical_years <- paste( "X", CO2_historical_years, sep = "" )

#Select whether to use GCAM3 fuel carbon coefficients
use_GCAM3_Ccoefs <- 0
#Select whether to use global average carbon coefficients on fuels, or region-specific carbon coefficients
use_global_Ccoefs <- 1
#Select year from which to calculate fuel emissions coefficients (2009 is currently the most recent)
inventory_match_year <- 2009

#Indicate the default coefficients to write out in regions with zero consumption of a particular fuel
default_gas_Ccoef <- 14.2
default_coal_Ccoef <- 27.3
default_liquids_Ccoef <- 19.6

digits_CO2coef <- 1

#Level2 data names
#Primary fuel carbon coefficients
names_PrimaryFuelCO2Coef <- c( "region", "PrimaryFuelCO2Coef.name", "PrimaryFuelCO2Coef")

