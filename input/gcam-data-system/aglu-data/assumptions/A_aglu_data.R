#------------------------------------------------------------------------------------
AGLU_historical_years <- 1971:2010
X_AGLU_historical_years <- paste( "X", AGLU_historical_years, sep = "" )

FAO_historical_years <- 1961:2011
X_FAO_historical_years <- paste( "X", FAO_historical_years, sep = "" )

land_history_years <- c( 1700, 1750, 1800, 1850, 1900, 1950, 1975 )
X_land_history_years <- paste( "X", land_history_years, sep = "" )

land_cover_years <- sort( unique(  c( land_history_years, AGLU_historical_years ) ) )
X_land_cover_years <- paste( "X", land_cover_years, sep = "" )

preAGLU_years <- land_history_years[ !land_history_years %in% AGLU_historical_years ]
X_preAGLU_years <- paste( "X", preAGLU_years, sep = "" )

# Diet years
diet_years <- c( max( historical_years ), future_years[ future_years <= 2050 ] )
X_diet_years <- paste( "X", diet_years, sep="" )
diet_gdpScen <- "SSP2" #Scenario to match with FAO projections in deriving income elasticities
diet_convergence_year <- 9999
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

#-----------------------------------------------------
# Identifier columns
LT <- "Land_Type"
GLU <- "GLU"
C <- "GCAM_commodity"
Sys <- "system"
Fd <- "feed"

R_LT <- c( R, LT )
R_LT_Y <- c( R, LT, Y ) 
R_LT_GLU <- c( R, LT, GLU ) 
R_GLU <- c( R, GLU ) 
R_Y_GLU <- c( R, Y, GLU ) 
R_LT_Y_GLU <- c( R, LT, Y, GLU )

# Identifier columns for datasets with commodities
R_C <- c( R, C )
R_C_GLU <- c( R, C, GLU )
R_C_Y <- c( R, C, Y ) 
R_C_Y_GLU <- c( R_C_Y, GLU )
R_C_Y_GLU <- c( R, C, Y, GLU ) 
C_Y <- c( C, Y ) 
C_GLU <- c( C, GLU )
R_C_Sys <- c( R, C, Sys )
R_C_Sys_Fd <- c( R, C, Sys, Fd )
R_C_Sys_Fd_Y <- c( R, C, Sys, Fd, Y )
C_Sys <- c( C, Sys )
C_Sys_Fd <- c( C, Sys, Fd )
R_Fd <- c( R, Fd )
rcp_gcm_cm <- c( "rcp", "gcm", "cropmodel" )
rcp_gcm_cm_id <- c( rcp_gcm_cm, "ID" )
rcp_gcm_cm_id_irr <- c( rcp_gcm_cm, "ID", "irr" )
rcp_gcm_cm_id_crop <- c( rcp_gcm_cm, "ID", "crop" )
rcp_gcm_cm_id_crop_irr <- c( rcp_gcm_cm, "ID", "crop", "irr" )
irr <- "Irr_Rfd"
lvl <- "level"
R_C_irr <- c( R_C, irr )
R_C_Y_irr <- c( R_C_Y, irr )
R_C_Y_GLU_irr <- c( R_C_Y_GLU, irr )
R_C_GLU_irr <- c( R_C_GLU, irr )
R_GLU_irr <- c( R, GLU, irr )
R_C_GLU_irr_lvl <- c( R_C_GLU_irr, lvl )

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
digits_land_use <- 7
digits_land_value <- 0

digits_AgProdChange <- 4 #rate of change in yield
digits_MAC <- 4 #portion of abatement at specified cost

#-----------------------------------------------------------
#ASSUMPTIONS
# Fraction of land to protect
protect_land_fract <- 0.9

#Maximum Feed input-output coefs (for non-existent technologies)
max_FeedIO <- 100

# Minimum and maximum harvested:cropped ratios
min_HA_to_Cropland <- 1
max_HA_to_Cropland <- 3

# Maximum portion of any land use region's pastures that can be in production
max_MgdPast_frac <- 0.95
max_MgdFor_frac <- 1

# Average density of wood
AvgWoodDensity_kgm3 <- 500 # In kg per m3
AvgWoodDensity_kgCm3 <- 250 # In kg C per m3

# Energy content of biomass
bio_GJt <- 17.5

#Maximum bioenergy (switchgrass) yield allowable, in tonnes per hectare
Max_bio_yield_tha <- 20

# Other parameters for wood residue biomass
WoodEnergyContent_GJkg <- 0.0189
WoodWaterContent <- 0.065
ForestHarvestIndex <- 0.8
ForestErosCtrl_kgm2 <- 0.2
ForestRootShoot <- 0.2485

#Price at which base year bio frac produced is used
Price_bio_frac <- 1.2

# Tolerance threshold on inter-annual changes in land cover for any land use region
LandTolerance <- 0.001

# Meat price elasticity in the USA
food_meat_P_elas_USA <- -0.09

#Default income elasticity for convergence (post projection period)
default_IncElas <- 0

#Price conversion from alfalfa to grass hay
priceratio_grass_alfalfa <- 0.7

#Maximum biomass yield improvement rates, reference and advanced
bio_maxYieldRate_ref <- 0.005
bio_maxYieldRate_adv <- 0.01

#Minimum soil and vegetation carbon densities
min.veg.carbon.density <- 0
min.soil.carbon.density <- 0

# Minimum non-input costs of animal production technologies, in $/kg
min_an_noninput_cost <- 0.05

#Forestry cost (1975$/GJ)
cost_For_75USDm3 <- 29.59
cost_Past_75USDkg <- 0

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

#Biomass names
bio_grass_name <- "biomass_grass"
bio_tree_name <- "biomass_tree"

#Production costs of biomass (from Patrick Luckow's work)
bio_grass_Cost_75USD_GJ <- 0.75
bio_tree_Cost_75USD_GJ <- 0.67

#Fertilizer application rate for biomass, and carbon yields. Values from Adler et al. 2007
bio_grass_Fert_IO_gNm2 <- 5.6
bio_grass_Yield_kgCm2 <- 0.34
bio_tree_Fert_IO_gNm2 <- 3.36
bio_tree_Yield_kgCm2 <- 0.345

#Water characteristics
Irr_Cons_name <- "water consumption"
Irr_W_name <- "water withdrawals"
Bio_Cons_name <- "biophysical water consumption"
bio_grass_Water_IO_km3EJ <- 25  # From Vaibhav's paper
bio_tree_Water_IO_km3EJ <- 25

GLU_ndigits <- 3    #number of digits in the geographic land unit identifier codes
GLU_name_delimiter <- ""       # delimiter between the GLU name and number
crop_GLU_delimiter <- "_"       # delimiter between the crop name and GLU name
LT_GLU_delimiter <- crop_GLU_delimiter       # delimiter between the land use type name and GLU name. should be the same as the crop-glu delimiter
irr_delimiter <- "_"       # delimiter between the appended crop x GLU and irrigation level
mgmt_delimiter <- "_"     #delimiter between appended tech name and management level

#Multipliers for high & low ag prod growth scenarios
hi_ag_prod_growth_mult <- 1.5
low_ag_prod_growth_mult <- 0.5

#Ghost share on land uses
irrig_ghost_share_mult <- 0.25

#Fraction of food that should be produced locally
local_food_fract <- 0.8

#set a minimum allowable profit margin (in percent terms)
min_profit_margin <- 0.15

#SSP diet parameters
max.mult.ssp1 <- 1.2
max.mult.ssp2 <- 1.25
max.mult.ssp3 <- 1.3
max.mult.ssp4 <- 1.25
max.mult.ssp5 <- 1.3

#GDP per capita thressholds for SSP4 region groupings
hi_growth_pcgdp <- 12.275
lo_growth_pcgdp <- 2.75

