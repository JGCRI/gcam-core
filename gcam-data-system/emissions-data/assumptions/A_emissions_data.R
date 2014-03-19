#Lookup
C_S_F <- c( "iso", "sector", "fuel")

#------------------------------------------------------------------------------------
#Historical years for data write-out
EPA_historical_years <- 1971:2002
X_EPA_historical_years <- paste( "X", EPA_historical_years, sep = "" )
NH3_historical_years <- 1990:2002
X_NH3_historical_years <- paste( "X", NH3_historical_years, sep = "" )
EDGAR_historical_years <- 1971:2008
X_EDGAR_historical_years <- paste( "X", EDGAR_historical_years, sep = "" )
NH3_extra_years <- 1971:1989
X_NH3_extra_years <- paste( "X", NH3_extra_years, sep = "" )

#At present the CO2 emissions inventory from CDIAC stops at 2009
CO2_historical_years <- historical_years[ historical_years < 2010 ]
X_CO2_historical_years <- paste( "X", CO2_historical_years, sep = "" )

#Select whether to use GCAM3 fuel carbon coefficients
use_GCAM3_Ccoefs <- 1
#Select whether to use global average carbon coefficients on fuels, or region-specific carbon coefficients
use_global_Ccoefs <- 1
#Select year from which to calculate fuel emissions coefficients (2009 is currently the most recent)
inventory_match_year <- 2009

names_PrimaryFuelCO2Coef <- c( "region", "PrimaryFuelCO2Coef.name", "PrimaryFuelCO2Coef")

#Indicate the default coefficients to write out in regions with zero consumption of a particular fuel
default_gas_Ccoef <- 14.2
default_coal_Ccoef <- 27.3
default_liquids_Ccoef <- 19.6

#Conversion from thousand short tons to Tg
tst_to_tg <- 0.000907

#Metric conversions
gg_to_tg <- 0.001
kg_to_tg <- 0.000000001

#Significant digits
digits_emissions <- 7
digits_CO2coef <- 1

#Base years for emissions data
emiss_model_base_years <- c( "1975", "1990", "2005" )

#Gases
nonghg_gases <- c( "SO2", "NOx", "CO", "NMVOC", "NH3" )
awb_gases <- c( "SO2_AWB", "NOx_AWB", "CO_AWB", "NMVOC_AWB", "CH4_AWB", "N2O_AWB", "NH3_AWB" )
agr_gases <- c( "CH4_AGR", "N2O_AGR", "NH3_AGR" )
GHG_names <- c( "CH4", "N2O" )
F_Gases <- c( "HFC23", "HFC32", "HFC125", "HFC134a", "HFC143a", "HFC236fa", "CF4", "C2F6", "SF6" )

#Sectors
awb_sectors <- "ag_waste_burning"
agr_sectors <- c( "rice", "fertilizer", "soil" )
prc_sectors <- c( "industry_processes", "chemicals", "landfills", "wastewater", "aerosols", "metals", "foams", "solvents", "semiconductors" )

#Lookup vectors
R_G_Sedgar <- c( "GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector")
R_G_StubTechYr <- c( "GCAM_region_ID", "Non.CO2", "supplysector", "subsector", "stub.technology", "xyear" )
R_StubTechYr <- c( "GCAM_region_ID", "supplysector", "subsector", "stub.technology", "xyear" )

#Taxes to use in MAC curve
MAC_taxes <- c( -30, -20, -15, -10, 0, 15, 20, 30, 50, 60, 100, 120, 165, 200 )

#Final calibration year where we have emissions data
final_emiss_year <- 2005

#Regional coal CH4 emission coefficient - some of the calculated values are astronomical. Overwriting this for now
coal_CH4_coef <- 0.38