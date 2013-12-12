# Level 1 column headers
Xyr <- "Xyear"
R_S <- c( "GCAM_region_ID", "sector" )
R_S_F <- c( "GCAM_region_ID", "sector", "fuel")
R_S_F_tech <- c( R_S_F, "technology" )
R_F <- c( "GCAM_region_ID", "fuel" )
R_F_Y <- c( "GCAM_region_ID", "fuel", "year" )
R_F_Xyr <- c( "GCAM_region_ID", "fuel", "Xyear" )
R_Y <- c( "GCAM_region_ID", "year" )
R_Xyr <- c( "GCAM_region_ID", "Xyear" )
S_F <- c( "sector", "fuel" )
RG3_subrsrc_grd <- c( "region_GCAM3", "subresource", "grade" )
RG3_F <- c( "region_GCAM3", "fuel" )
R_subrsrc <- c( "GCAM_region_ID", "subresource" )
names_RsrcCurve <- c( "GCAM_region_ID", "resource", "subresource", "grade", "extractioncost", "available" )
s_s_t <- c( "supplysector", "subsector", "technology" )
s_s_t_i <- c( "supplysector", "subsector", "technology", "minicam.energy.input" )

#Resource assumptions
rsrc_fuels <- c( "coal", "gas", "refined liquids" )
calproduction_fuels <- c( "coal", "natural gas", "crude oil" )

#Electricity generation fuels whose calibrated quantities in the IEA energy balances are used
electricity_input_fuels <- c( "biomass", "coal", "gas", "refined liquids" )

#Assumed base year heat price, used for calculating adjustment to non-energy costs of electricity technologies with secondary output of heat
heat_price <- 2.8

digits_CO2coef <- 1
digits_calproduction <- 7
digits_calOutput <- 7
digits_depresource <- 1
digits_maxSubResource <- 5
digits_mid.price <- 3
digits_curve.exponent <- 3
digits_coefficient <- 7
digits_cost <- 4
digits_efficiency <- 3
digits_capital <- 0
digits_OM <- 2
digits_remove.fraction <- 2
digits_floorspace <- 3
digits_hddcdd <- 0

default_energy_unit <- "EJ"
default_energy_price_unit <- "1975$/GJ"

default_subsector_logit <- -3
default_technology_logit <- -6

CO2.storage.market <- "carbon-storage"

default_electric_efficiency <- 0.3     #Used in region x fuel combinations where the IEA data has fuel input but not output

min_in_EJ_ind <- 1e-3
NH3_H_frac <- 3/17
H_energy_GJtH2 <- 120

#Set a year to which to converge the feedstock:energy ratios in generic industrial production function coefficients
indcoef_conv_year <- 2150
X_indcoef_conv_year <- paste0( "X", indcoef_conv_year )

cement_CCS_cost_2000USDtCO2 <- 50   #Starting point of supply curve in Mahasenan et al 2003
CO2_storage_cost_1990USDtC <- 42    #From GCAM 1.0 inputs

discount_rate_bld <- 0.1

#Set which years to use to calculate climate normals (HDD and CDD) in each region
climate_normal_years <- 1981:2000
X_climate_normal_years <- paste0( "X", climate_normal_years )
InternalGainsScalar_USA_h <- -930
InternalGainsScalar_USA_c <- 350

floor.to.surface.ratio <- 5.5





