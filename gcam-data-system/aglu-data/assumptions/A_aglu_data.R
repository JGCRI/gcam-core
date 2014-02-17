#------------------------------------------------------------------------------------
AGLU_historical_years <- 1971:2010
X_AGLU_historical_years <- paste( "X", AGLU_historical_years, sep = "" )

FAO_historical_years <- 1961:2011
X_FAO_historical_years <- paste( "X", FAO_historical_years, sep = "" )

land_history_years <- c( 1700, 1750, 1800, 1850, 1900, 1950, 1975 )

land_cover_years <- sort( unique(  c( land_history_years, AGLU_historical_years ) ) )
X_land_cover_years <- paste( "X", land_cover_years, sep = "" )

preAGLU_years <- land_history_years[ !land_history_years %in% AGLU_historical_years ]
X_preAGLU_years <- paste( "X", preAGLU_years, sep = "" )

# Diet years
diet_years <- c( max( historical_years ), future_years[ future_years <= 2050 ] )
X_diet_years <- paste( "X", diet_years, sep="" )
diet_gdpScen <- "SSP2" #Scenario to match with FAO projections in deriving income elasticities
diet_convergence_year <- 2200
X_diet_convergence_year <- paste0("X", diet_convergence_year )
convergence_kcald_crops <- 2500
convergence_kcald_meat <- 1000

# Specified ag productivity years
spec_ag_prod_years <- seq( 2010, 2050, 5 )
X_spec_ag_prod_years <- paste( "X", spec_ag_prod_years, sep="" )

#Year from which to base bioenergy crop yields
bio_yield_year <- max( AGLU_historical_years )
X_bio_yield_year <- paste( "X", bio_yield_year, sep = "" )

# Years used for calculating base year prices
model_price_years <- 2001:2005
X_model_price_years <- paste( "X", model_price_years, sep="" )

# Years used for calculating base year costs
model_cost_years <- 2001:2005
X_model_cost_years <- paste( "X", model_cost_years, sep="" )
X_final_cost_year <- X_model_cost_years[ length( X_model_cost_years ) ]

base_year_IFA <- 2006
X_base_year_IFA <- paste0( "X", base_year_IFA )

#---------------------------------------------------------------------------------
#AEZs
AEZs <- c( paste( "AEZ0", 1:9, sep="" ), paste( "AEZ", 10:18, sep="" ) )

#-----------------------------------------------------
# Identifier columns
LT <- "Land_Type"
AEZ <- "AEZ"
C <- "GCAM_commodity"
Sys <- "system"
Fd <- "feed"

R_LT <- c( R, LT )
R_LT_Y <- c( R, LT, Y ) 
R_LT_AEZ <- c( R, LT, AEZ ) 
R_AEZ <- c( R, AEZ ) 
R_LT_Y_AEZ <- c( R, LT, Y, AEZ )

# Identifier columns for datasets with commodities
R_C <- c( R, C )
R_C_AEZ <- c( R, C, AEZ )
R_C_Y <- c( R, C, Y ) 
R_C_Y_AEZ <- c( R, C, Y, AEZ ) 
C_Y <- c( C, Y ) 
C_AEZ <- c( C, AEZ )
R_C_Sys <- c( R, C, Sys )
R_C_Sys_Fd <- c( R, C, Sys, Fd )
R_C_Sys_Fd_Y <- c( R, C, Sys, Fd, Y )
C_Sys <- c( C, Sys )
C_Sys_Fd <- c( C, Sys, Fd )
R_Fd <- c( R, Fd )
rcp_gcm_cm <- c( "rcp", "gcm", "cropmodel" )
rcp_gcm_cm_aezid <- c( rcp_gcm_cm, "ID" )
rcp_gcm_cm_aezid_irr <- c( rcp_gcm_cm, "ID", "irr" )
rcp_gcm_cm_aezid_crop <- c( rcp_gcm_cm, "ID", "crop" )
rcp_gcm_cm_aezid_crop_irr <- c( rcp_gcm_cm, "ID", "crop", "irr" )


#-----------------------------------------------------------------
#NUMBERS OF DIGITS FOR MODEL INPUT DATA
digits_calPrice <- 4 #prices and costs
digits_calOutput <- 7 #production
digits_feed_IO <- 3 #feed mass per animal product mass
digits_Fert_IO <- 8 #units energy per mass of fertilizer
digits_C_density <- 1 #vegetative and soil carbon density
digits_MatureAge <- 0 #number of years for mature age
digits_C_density_crop <- 3 #cropland vegetative soil carbon content
digits_IncElas <- 4 #income elasticities

#Residue biomass characteristics
digits_harvest_index <- 2
digits_eros_ctrl <- 2
digits_res_energy <- 4
digits_water_content <- 2

#Land cover data
digits_land_total <- 2
digits_land_use <- 5
digits_land_value <- 0

digits_AgProdChange <- 4 #rate of change in yield
digits_MAC <- 4 #portion of abatement at specified cost

#-----------------------------------------------------------
#ASSUMPTIONS
# Fraction of land to protect
protect_land_fract <- 0.9

# Threshold for regional production fractions in "small AEZs" whose production can be adjusted
small_AEZ_prodfrac <- 0.015

#Maximum Feed input-output coefs (for non-existent technologies)
max_FeedIO <- 100

# Minimum and maximum harvested:cropped ratios
min_HA_to_Cropland <- 1
max_HA_to_Cropland <- 3

# Maximum portion of any AEZs pastures that can be in production
max_MgdPast_frac <- 0.85
max_MgdFor_frac <- 1

# Average density of wood, in kg C per m3
AvgWoodDensity_kgCm3 <- 288 # ASMP: 
# In kg per m3
# TODO: This should be consistent with carbon content above
AvgWoodDensity_kgm3 <- 500

# Energy content of biomass
bio_GJt <- 17.5

# Other parameters for wood residue biomass
WoodEnergyContent_GJkg <- 0.0189
WoodWaterContent <- 0.065
ForestHarvestIndex <- 0.8
ForestErosCtrl_kgm2 <- 0.2
ForestRootShoot <- 0.2485

#Jatropha residue biomass characteristics
JatrophaHarvestIndex <- 0.5
JatrophaErosCtrl_kgm2 <- 0
JatrophaMassEnergy <- 2.2
JatrophaWaterContent <- 0.1

#Price at which base year bio frac produced is used
Price_bio_frac <- 1.2

# Tolerance threshold on inter-annual changes in land cover for any region/AEZ
LandTolerance <- 0.001

# Meat price elasticity in the USA
food_meat_P_elas_USA <- -0.09

#Default income elasticity for convergence (post projection period)
default_IncElas <- 0

#Price conversion from alfalfa to grass hay
priceratio_grass_alfalfa <- 0.7

# Biomass yield multiplier to convert from EPIC switchgrass yields to recent literature switchgrass yields
bio_yield_mult <- 2.02

# AEZ for indexing of bioenergy yields
Index_AEZ <- "AEZ12"

# AEZ classes
AEZs_trop <- c( paste( "AEZ0", 1:6, sep="" ) ) 
AEZs_temp <- c( paste( "AEZ0", 7:9, sep="" ), paste( "AEZ", 10:12, sep="" ) )
AEZs_pol <- c( paste( "AEZ", 13:18, sep="" ) )

# Dry AEZs that get a different bioenergy base yield
AEZs_arid <- c( paste( "AEZ0", 1:2, sep="" ), paste( "AEZ0", 7:8, sep="" ), paste( "AEZ", 13:14, sep="" ) )

# Dry AEZs that get a different logit exponent on pasture / non-pasture
AEZs_most_arid <- c( paste( "AEZ0", c( 1, 7 ), sep="" ), paste( "AEZ", 13, sep="" ) )

#AEZs where bioenergy is costed out
AEZs_hi_bio_cost <- c( paste( "AEZ0", 1, sep="" ), paste( "AEZ0", 7, sep="" ), paste( "AEZ", 13:18, sep="" ) )

# Specify crops on which to base region- and AEZ-based adjustments to EPIC switchgrass yields
firstgenbio_crops <- c( "Corn", "OilCrop", "PalmFruit", "SugarCrop" )
cellulosic_crops <- c( "Wheat", "OtherGrain" )

#Maximum biomass yield improvement rates, reference and advanced
bio_maxYieldRate_ref <- 0.005
bio_maxYieldRate_adv <- 0.01

#Minimum soil and vegetation carbon densities
min.veg.carbon.density <- 0
min.soil.carbon.density <- 0

#Default share on land uses
default_share_2020 <- 0.02
default_share_2025 <- 0.1
default_share <- 0.25

# Minimum non-input costs of animal production technologies, in $/kg
min_an_noninput_cost <- 0.05

#Cost index crop, for crops with no cost data
Cost_index_crop <- "Corn"

#Forestry cost (1975$/GJ)
cost_For_75USDm3 <- 29.59

#Start year for purpose-grown biomass
Bio_start_year <- 2020

#Carbon content adjustments from unmanaged to managed
Cveg_Mult_UnmgdFor_MgdFor <- 0.5
Csoil_Mult_UnmgdFor_MgdFor <- 0.87
Cveg_Mult_UnmgdPast_MgdPast <- 0.5
Csoil_Mult_UnmgdPast_MgdPast <- 0.75

#Carbon content of all cellulose
Ccontent_cellulose <- 0.45

#Conversion from peak biomass to average biomass integrated over the course of the year
Cconv_peak_avg <- 0.5

#Fertilizer application rate for biomass
swgr_Fert_IO_gNm2 <- 5.6
swgr_Yield_kgCm2 <- 0.34
popl_Fert_IO_gNm2 <- 3.36
popl_Yield_kgCm2 <- 0.345

#Water characteristics
Irr_Cons_name <- "water consumption"
Irr_W_name <- "water withdrawals"
Bio_Cons_name <- "biophysical water consumption"
swgr_Bio_IO_km3EJ <- 25  # From Vaibhav's paper
misc_Bio_IO_km3EJ <- 20
Jat_Bio_IO_km3EJ <- 240  # Roughly double the coefficient of soybeans
wdy_Bio_IO_km3EJ <- 25

#Woody bioenergy crop names
woody_biocrops <- c( "eucalyptus", "willow" )

AEZ_delimiter <- ""       #delimiter between the appended sector name and AEZ name


