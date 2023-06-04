# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

# General behavior constants ======================================================================

OUTPUTS_DIR              <- "outputs/"
XML_DIR                  <- "xml/"
COMMENT_CHAR             <- "#"
UNDER_TIMESHIFT          <- FALSE
YEAR_PATTERN             <- "^(1|2)[0-9]{3}$"   # a 1 or 2 followed by three digits, and nothing else
LOGIT_TYPE_COLNAME       <- "logit.type"        # will be removed by test code before old-new comparison
DISABLED_MODULES         <- "NONE"


# Flags ======================================================================

FLAG_INPUT_DATA      <- "FLAG_INPUT_DATA"       # input data, don't output
FLAG_NO_OUTPUT       <- "FLAG_NO_OUTPUT"        # don't output
FLAG_NO_TEST         <- "FLAG_NO_TEST"          # don't test
FLAG_XML             <- "FLAG_XML"              # xml data


# Time constants ======================================================================

# Historical years for level 1 data processing. All chunks that produce historical data
# for model calibration are required to produce annual data covering this entire span.
HISTORICAL_YEARS        <- 1971:2015
# Future years for level 1 data processing, for the few chunks that
# produce future data (e.g., population projections)
FUTURE_YEARS            <- 2016:2100
# Calibrated periods in the model. Only level 2 chunks should reference these
MODEL_BASE_YEARS        <- c(1975, 1990, 2005, 2010, 2015)
# Future (not calibrated) model periods. Only level 2 chunks should reference these
MODEL_FUTURE_YEARS      <- seq(2020, 2100, 5)
MODEL_YEARS             <- c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)
MODEL_FINAL_BASE_YEAR   <- 2015


# GCAM constants ======================================================================

gcam.USA_CODE            <- 1
gcam.USA_REGION          <- "USA"
gcam.WESTERN_EUROPE_CODE <- 13
gcam.LOGIT_TYPES         <- c("relative-cost-logit", "absolute-cost-logit")
gcam.EQUIV_TABLE         <- "EQUIV_TABLE"
gcam.IND_ENERGY_USE      <- c("biomass", "coal", "gas", "refined liquids")  # GCAM industrial energy use fuels
GCAM_REGION_ID      <- "GCAM_region_ID"
# The default market price GCAM will use to start solving from if it has no other info
# If users do not have an estimate for a starting price this is a safe one to set
gcam.DEFAULT_PRICE <- 1.0
gcam.DEFAULT_SUBSECTOR_LOGIT  <- -3
gcam.DEFAULT_TECH_LOGIT       <- -6
gcam.REGION_NUMBER            <- 32    # Use for assertion in data processing to ensure all region has data
gcam.REAL_PRICE_BASE_YEAR     <- 1975  # This is only used in AgLU prices now.

# Driver constants ======================================================================

driver.MAKE            <- "MAKE"
driver.DECLARE_OUTPUTS <- "DECLARE_OUTPUTS"
driver.DECLARE_INPUTS  <- "DECLARE_INPUTS"
driver.DECLARE_MODIFY  <- "DECLARE_MODIFY"

# Data and utility constants ======================================================================

data.SEPARATOR <- "; "
data.PRECURSOR <- "Precursor"
data.DEPENDENT <- "Dependent"
data.USER_MOD_POSTFIX <- "__0"


# Modeltime constants ======================================================================
# The number of years encompased in the first model period, currently hard coded in the C++
# Note, this is different than the number of years between period 0 and period 1
# The value typically does not matter but does come up for calculating resource depletion
modeltime.PERIOD0_TIMESTEP <- 15

# MAGICC model assumptions
modeltime.MAGICC_LAST_HISTORICAL_YEAR <- 2005
modeltime.MAGICC_BC_UNIT_FORCING      <- 0
modeltime.MAGICC_DEFAULT_EMISS_FILE   <- "../input/magicc/Historical Emissions/Default Emissions Module/Hist_to_2008_Annual.csv"
modeltime.MAGICC_C_START_YEAR         <- 1705

# Hector model assumptions
modeltime.HECTOR_END_YEAR        <- 2300
modeltime.HECTOR_EMISSIONS_YEAR  <- 2005
modeltime.HECTOR_INI_FILE        <- "../input/climate/hector-gcam.ini"


# Conversion constants ======================================================================
# The naming convention is CONV_(FROM-UNIT)_(TO-UNIT).

# Numeric (unitless)
CONV_BIL_MIL    <- 1000
CONV_MIL_BIL    <- 1 / CONV_BIL_MIL
CONV_BIL_THOUS  <- 1e6
CONV_THOUS_BIL  <- 1 / CONV_BIL_THOUS
CONV_MIL_THOUS  <- 1000
CONV_ONES_THOUS <- 0.001

# Mass
CONV_TON_MEGATON <- 1e-6
CONV_T_KG <- 1e3
CONV_KG_T <- 1 / CONV_T_KG
CONV_T_METRIC_SHORT <- 1000 / 908  # Ratio between metric ton and short ton
CONV_HA_BM2 <- 1e-5
CONV_HA_M2 <- 10000
CONV_THA_KGM2 <- 0.1   # tons C/ha -> kg C/m2
CONV_G_TG <- 1e-12
CONV_GG_TG <- 0.001 # gigagrams to tegagrams
CONV_TST_TG <- 0.000907 # thousand short tons to Tg
CONV_KG_TO_TG <- 1e-9
CONV_KT_MT <- 0.001 # kt to Mt
CONV_T_MT <- 1e-6 # t to Mt
CONV_G_KG <- 1e-3 # kilograms to grams
CONV_NH3_N <- 14/17 # Nitrogen to Ammonia
CONV_KBBL_BBL <- 1000 # thousand barrels to barrels
CONV_BBL_TONNE_RFO <- 1 / 6.66 # barrels to tons residual fuel oil
CONV_TONNE_GJ_RFO <- 40.87 # tons to GJ residual fuel oil
CONV_BBL_TONNE_DISTILLATE <- 1 / 7.46 # barrels to tons distillate
CONV_BBL_TONNE_RFO  <- 1 / 6.66       # barrels to tons residual fuel oil
CONV_G_KG           <- 1e-3           # kilograms to grams
CONV_GG_TG          <- 0.001          # gigagrams to teragrams
CONV_HA_BM2         <- 1e-5
CONV_HA_M2          <- 10000
CONV_KBBL_BBL       <- 1000           # thousand barrels to barrels
CONV_KG_TO_TG       <- 1e-9
CONV_KT_MT          <- 0.001          # kt to Mt
CONV_NH3_N          <- 14/17          # Nitrogen to Ammonia
CONV_T_KG           <- 1e3
CONV_KG_T           <- 1 / CONV_T_KG
CONV_T_METRIC_SHORT <- 1000 / 908     # Ratio between metric ton and short ton
CONV_T_MT           <- 1e-6           # t to Mt
CONV_THA_KGM2       <- 0.1            # tons C/ha -> kg C/m2
CONV_TON_MEGATON    <- 1e-6
CONV_TONNE_GJ_DISTILLATE  <- 42.91    # tons to GJ distillate
CONV_TONNE_GJ_RFO   <- 40.87          # tons to GJ residual fuel oil

# Time
CONV_YEAR_HOURS <- 24 * 365.25
CONV_DAYS_YEAR <- 1 / 365.25
CONV_DAY_HOURS <- 24

# Energy
CONV_MWH_GJ <- 3.6 # Megawatt hours to Gigajoules
CONV_MWH_EJ <- 3.6e-9 # Megawatt hours to Exajoules
CONV_GWH_EJ <- 3.6e-6
CONV_TWH_EJ <- 3.6e-3
CONV_KWH_GJ <- 3.6e-3
CONV_GJ_EJ <- 1e-9
CONV_MJ_EJ <- 1e-12
CONV_EJ_GJ <- 1 / CONV_GJ_EJ
CONV_MBLD_EJYR <- 6.119 * 365.25 * 1e-3 # million barrels a day to EJ per year
CONV_KBTU_EJ <- 1.0551e-12 # KiloBTU to EJ
CONV_TBTU_EJ <- 0.0010551 # TeraBTU to EJ
CONV_MJ_BTU <- 947.777
CONV_BTU_KJ <- 1.0551
CONV_MMBTU_KGH2 <- 0.113939965425114 # MMBTU/kg H2 - LHV Source: H2 CCTP Workbook.xls (Used for older GCAM assumptions)
CONV_GJ_KGH2 <- 0.12021 #GJ/kg H2 - LHV

# Distance
CONV_MILE_KM <- 1.60934 # Mile to km
CONV_NMILE_KM <- 1.852 # Nautical mile to km

# Other
CONV_MCAL_PCAL <- 1e-9
CONV_M3_BM3 <- 1e-09 # Cubic meters (m3) to billion cubic meters (bm3)
CONV_MILLION_M3_KM3 <- 1e-03
CONV_M2_ACR <- 0.0002471058
CONV_HA_M2 <- 1e4 # ha to m2
CONV_BM2_M2 <- 1e9
CONV_MILFT2_M2 <- 92900 # Million square feet to square meters
CONV_FT2_M2 <- 0.0929 # Square feet to square meters
CONV_GAL_M3 <- 0.00378541 #gallons to m3
CONV_MI_KM <- 1.60934
CONV_PERS_MILPERS <- 1000000 #Person to million-passengers

# SO2 related conversion factors
RESID_BTU_PER_BBL <- 6.29 # Source EIA (Note HHV)
RESID_BBLS_PER_TONNE <- 6.66 # Source EIA (Note HHV)
RESID_ENERGY_DENSITY_BTU <- RESID_BTU_PER_BBL * RESID_BBLS_PER_TONNE * 0.95 # Btu/tonne net
RESID_ENERGY_DENSITY_JOULES <- RESID_ENERGY_DENSITY_BTU * 1055 # TJ/Tg
RESID_ENERGY_CONTENT <- RESID_ENERGY_DENSITY_JOULES/1E6 # Tg/EJ
SO2_SHIP_LIMIT_POLICY_MULTIPLIER <- 0.001 * 2

# AgLU constants ======================================================================

# Time
aglu.MODEL_MEAN_PERIOD_LENGTH <- 5       # AgLU data use a moving average over this period length in LA.100
aglu.MODEL_PRICE_YEARS      <- 2013:2017 # consistent with aglu.MODEL_SUA_MEAN_PERIODS
aglu.MODEL_MACRONUTRIENT_YEARS <- 2013:2017   # consistent with aglu.MODEL_SUA_MEAN_PERIODS; FAO only has data for after 2010
aglu.MODEL_COST_YEARS       <- 2008:2016
aglu.DEFLATOR_BASE_YEAR     <- 2015      # year used as the basis for computing regional price deflators
aglu.FALLOW_YEARS           <- 2013:2017 # Years used for calculating the % of fallow land
aglu.AGLU_HISTORICAL_YEARS  <- 1973:2015
aglu.BASE_YEAR_IFA          <- 2006      # Base year of International Fertilizer Industry Association (IFA) fertilizer application data
aglu.BIO_START_YEAR         <- 2025      # Also set in aglu/A_bio_ghost_share
aglu.CROSIT_HISTORICAL_YEAR <- 2005      # Historical year from the CROSIT data
aglu.FAO_LDS_YEARS          <- 1998:2002  # Years for which FAO harvested area data is averaged over for use in the land data system (LDS)
aglu.GTAP_HISTORICAL_YEAR   <- 2000      # Is the year that the GTAP data is based on.
aglu.LAND_HISTORY_YEARS     <- c(1700, 1750, 1800, 1850, 1900, 1950, 1975)
aglu.LAND_COVER_YEARS       <- sort(unique(c(aglu.LAND_HISTORY_YEARS, aglu.AGLU_HISTORICAL_YEARS)))
aglu.PREAGLU_YEARS          <- c(1700, 1750,1800, 1850, 1900, 1950)          # Cropland cover years prior to first aglu historical year to use in climate model component
aglu.SPEC_AG_PROD_YEARS     <- seq(max(aglu.AGLU_HISTORICAL_YEARS), 2050, by = 5) # Specified ag productivity years, KD i think this might need a better comment
aglu.SSP_DEMAND_YEARS       <- seq(2015, 2100, 5) # food demand in the SSPs is calculated at 5-yr intervals
# aglu.TRADED_* regional market commodities
aglu.TRADED_CROPS           <- c("Corn", "FiberCrop", "Fruits", "Legumes", "MiscCrop", "NutsSeeds", "OilCrop", "OtherGrain", "OilPalm", "Rice", "RootTuber", "Soybean", "SugarCrop", "Vegetables", "Wheat")
aglu.BIO_TRADE_SSP4_YEAR_FILLOUT       <- 2025 # year.fillout for SSP4 in L243.bio_trade_input
aglu.BIO_TRADE_SSP3_YEAR_FILLOUT       <- 2020 # year.fillout for SSP4 in L243.bio_trade_input
aglu.TRADED_MEATS           <- c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")
aglu.TRADED_FORESTS         <- c("Forest")
# Integrated world market and non-trade commodities
aglu.IWM_TRADED_COMM        <- c("FodderHerb", "OtherMeat_Fish") # Integrated World Market (IWM)commodities
aglu.NONTRADED_COMM         <- c("DDGS and feedcakes", "FodderGrass", "Pasture", "Residue", "Scavenging_Other") # non-traded commodities; "Pasture" is modeled as a crop produced from pasture land.

aglu.LAND_TOLERANCE    <- 0.005
aglu.MIN_PROFIT_MARGIN <- 0.15  # Unitless and is used to ensure that Agricultural Costs (units 1975USD/kg) don't lead to profits below a minimum profit margin.
aglu.MAX_FAO_LDS_SCALER <- 5   # Unitless max multiplier in reconciling LDS harvested area with FAO harvested area by country and crop. Useful for preventing bad allocations of N fert in AFG, TWN, several others
aglu.TREECROP_MATURE_AGE <- 10 # Number of years for vegetation carbon to reach peak, for tree crops

aglu.Min_Share_PastureFeed_in_PastureFodderGrass <- 0.1 # minimum share of pasture in Pasture_FodderGrass for feed uses to avoid negative or zero (not including Japan now); USA has ~30%
aglu.Zero_Min_PastureFeed_Share_iso <- c("jpn")         # mapped to GCAM_region_ID of Japan; Japan has zero unmanaged and protected pasture

# GLU (Geographic Land Unit) settings - see module_aglu_LA100.0_LDS_preprocessing
aglu.GLU <- "GLU"
aglu.GLU_NAME_DELIMITER <- ""  # delimiter between the GLU name and number


# Ratio of alfalfa price to grass hay used in the price conversion from alfalfa to grass hay.
# Alfalfa price source: USDA. 2011. Prices Received for Alfalfa Hay, Baled, Washington. National Agricultural Statistics Service, U.S. Department of Agriculture.
# Grass price source: Baker, A., and H. Lutman. 2008. Feed Year in Review (Domestic): Record Demand Drives U.S. Feed Grain Prices Higher in 2007/2008.
# FDS-2008-01, Economic Research Service, United States Department of Agriculture. Available at http://usda.mannlib.cornell.edu/usda/ers/FDS-yearbook/2000s/2008/FDS-yearbook-05-23-2008_Special_Report.pdf
aglu.PRICERATIO_GRASS_ALFALFA <- 0.7

# Pasture (forage) prices are equal to the hay (foddergrass) price times this exogenous multiplier. Equal to the hay price minus mowing, bundling, and transport.
aglu.PRICERATIO_PASTURE_HAY <- 0.5

# Carbon content of all cellulose
aglu.CCONTENT_CELLULOSE    <- 0.45

# Conversion from peak biomass to average biomass integrated over the course of the year
aglu.CCONV_PEAK_AVG <- 0.5

# Constraints for the minimum and maximum harvested:cropped ratios
# Source: Dalrymple, D.G. 1971, Survey of Multiple Cropping in Less Developed Nations, Foreign Econ. Dev. Serv., U.S. Dep. of Agricul., Washington, D.C.
# Cited in: Monfreda et al. 2008, Farming the Planet: 2., Global Biogeochemical Cycles 22, GB1022, http://dx.doi.org/10.1029/2007GB002947
aglu.MIN_HA_TO_CROPLAND <- 1  # minimum harvested:cropped ratios
aglu.MAX_HA_TO_CROPLAND <- 3  # maximum harvested:cropped ratios


# Production constraints
aglu.MAX_MGDPAST_FRAC <- 0.95 # Maximum percentage of any region/GLUs pasture that is allowed to be in managed production.
aglu.MAX_MGDFOR_FRAC  <- 1    # Maximum percentage of any region/GLUs forest that is allowed to be in managed production.

# GDP constraints
aglu.HIGH_GROWTH_PCGDP <- 12.275   # GDP per capita high threshold for SSP4 region groupings, thousand 2010$ per person
aglu.LOW_GROWTH_PCGDP  <- 2.75     # GDP per capita low threshold for SSP4 region groupings, thousand 2010$ per person
aglu.PCGDP_YEAR <- 2010            # Year to compare to PCGDP thresholds

# AgLu mulitpliers
aglu.MGMT_YIELD_ADJ <- 0.2       # Yield multiplier that goes from the observed yield to the "high" and "low" yields: observed plus or minus observed times this number.
aglu.HI_PROD_GROWTH_MULT <- 1.5  # Multipliers for high ag prod growth scenarios
aglu.LOW_PROD_GROWTH_MULT <- 0.5 # Multipliers for low ag prod growth scenarios

# AgLU cost constants
aglu.BIO_GRASS_COST_75USD_GJ <- 0.75   # Production costs of biomass (from Patrick Luckow's work)
aglu.BIO_TREE_COST_75USD_GJ  <- 0.67   # Production costs of biomass (from Patrick Luckow's work)
aglu.FERT_PRICE              <- 596    # Price of fertilizer, 2010$ per ton NH3
aglu.FERT_PRICE_YEAR         <- 2010   # Year corresponding to the above price/cost
aglu.FOR_COST_SHARE          <- 0.59   # Non-land forestry cost share (from 2011 GTAP data base)

# Price at which base year bio frac produced is used.
# The share of residue biomass production in each region,
# defined as the energy produced divided by the total
# waste biomass produced, is read in by A_bio_frac_prod_R.csv.
# This price, in 1975$/GJ, indicates the biomass price at
# the given shares. It should be close to the model's actual
# (endogenous) biomass prices in the final calibration year.
aglu.PRICE_BIO_FRAC <- 1.2

# Fertilizer application rate for biomass, and carbon yields. Values from Adler et al. 2007 (doi:10.1890/05-2018)
aglu.BIO_GRASS_FERT_IO_GNM2 <- 5.6
aglu.BIO_GRASS_YIELD_KGCM2  <- 0.34
aglu.BIO_TREE_FERT_IO_GNM2  <- 3.36
aglu.BIO_TREE_YIELD_KGCM2   <- 0.345

# Water characteristics for biomass
# Reference: Chaturvedi et al. 2015, Climate mitigation policy implications for global irrigation water demand, Mitig Adapt Strateg Glob Change (2015) 20:389-407. DOI 10.1007/s11027-013-9497-4
aglu.BIO_GRASS_WATER_IO_KM3EJ <- 25
aglu.BIO_TREE_WATER_IO_KM3EJ  <- 25

# Maximum bioenergy (switchgrass) yield allowable, in tons per hectare from Wullschleger doi:10.2134/agronj2010.0087
aglu.MAX_BIO_YIELD_THA <- 20

# Energy content of biomass, GJ/ton
aglu.BIO_ENERGY_CONTENT_GJT <- 17.5

# Regions in which agriculture and land use are not modeled
# kbn 2019/09/25 Took taiwan out from below since we have data for Taiwan now.
aglu.NO_AGLU_REGIONS <- ""

# Define GCAM category name of fertilizer
aglu.FERT_NAME <- "N fertilizer"

# Average Wood Density kg/m^3 for mass conversion
# Source: http://www.engineeringtoolbox.com/wood-density-d_40.html
# To Page's knowledge, nobody's ever done a weighted average wood density
# across all tree species that are commercially logged;
# 500 was was chosen to be towards the middle of the species that are produced.
aglu.AVG_WOOD_DENSITY_KGM3 <- 500 # In kg per m3
# Carbon content of wood is about 50 percent across species
aglu.AVG_WOOD_DENSITY_KGCM3 <- 250 # In kg carbon per m3

# Carbon content adjustments from unmanaged to managed
# conversion factor from unmanaged forest to managed forest, where the former is
# understood to be forest not in logging rotation, and the latter is forest in
# logging rotation. The average vegetation biomass of the logged forest is assumed
# to be 50% of that of the unlogged forest (integrated over the rotation period).
# Using 50% under the assumption that the veg biomass of the logged forest over the
# rotation period can be approximated by a triangle.
aglu.CVEG_MULT_UNMGDFOR_MGDFOR <- 0.5
aglu.CSOIL_MULT_UNMGDFOR_MGDFOR <- 0.87      #source: Guo and Gifford 2002; https://doi.org/10.1046/j.1354-1013.2002.00486.x
aglu.CVEG_MULT_UNMGDPAST_MGDPAST <- 0.5
aglu.CSOIL_MULT_UNMGDPAST_MGDPAST <- 0.8     # stay conservative here b/c no data source

# Average Agriculture Density kg/m^3 for mass conversion
# Source: http://www.engineeringtoolbox.com/wood-density-d_40.html
aglu.AVG_AG_DENSITY <- 1

# Forest Harvest Index
aglu.FOREST_HARVEST_INDEX <- 0.8

# Forest Erosion Control in kg/m^2
aglu.FOREST_EROSION_CTRL_KGM2 <- 0.2

# Mill Erosion Control in kg/m^2
aglu.MILL_EROSION_CTRL_KGM2 <- 0

# Wood energy content in GJ/kg
aglu.WOOD_ENERGY_CONTENT_GJKG <- 0.0189

# wood water content
# Unitless (mass of water / total wood mass)
aglu.WOOD_WATER_CONTENT <- 0.065

# Min veg and soil carbon densities
# kg C per m2
aglu.MIN_VEG_CARBON_DENSITY  <- 0
aglu.MIN_SOIL_CARBON_DENSITY <- 0

#This is the model carbon year. Carbon outputs are scaled to this year
MODEL_CARBON_YEAR <- 2010

# These are the default values of carbon densities from Houghton (in MgC/ha) by land type. moirai only outputs carbon for unmanaged land. Therefore, we need default values for other land types.
#Moreover we do not have data on carbon for Polar deserts and Tundra. So we use default values for those as well.
aglu.DEFAULT_SOIL_CARBON_PASTURE <- 13
aglu.DEFAULT_VEG_CARBON_PASTURE <- 0.7
aglu.DEFAULT_SOIL_CARBON_CROPLAND <- 9
aglu.DEFAULT_VEG_CARBON_CROPLAND <- 0.3
aglu.DEFAULT_SOIL_CARBON_URBANLAND <- 5.8
aglu.DEFAULT_VEG_CARBON_URBANLAND <- 0.3
aglu.DEFAULT_SOIL_CARBON_TUNDRA <- 22
aglu.DEFAULT_VEG_CARBON_TUNDRA <- 0.9

# This is the default maturity age from Houghton.
aglu.DEFAULT_MATURITY_AGE_PASTURE <- 10
aglu.DEFAULT_TUNDRA_AGE <- 50

# We may not have maturity age for some land types. If this arises, set it to the below (mean of all LT)
aglu.DEFAULT_MATURITY_AGE_ALL_LAND <- 35

# Define top-level (zero) land nest logit exponent and logit type
aglu.N0_LOGIT_EXP  <- 0
aglu.N0_LOGIT_TYPE <- NA



#Set the below constant to TRUE to de-activate the protected areas differentiated by land type and region in GCAM. Setting it to TRUE will use the default protection fraction defined in aglu.PROTECT_DEFAULT
aglu.PROTECTION_DATA_SOURCE_DEFAULT <- FALSE
#Un-Protected area status- This constant can be used to make more land types from the protection categories available for expansion.
# The available options for land types are - Unknown, UnsuitableUnprotected, SuitableUnprotected, SuitableHighProtectionIntact, SuitbaleHighProtectionDeforested, SuitableLow Protection, UnsuitableHighProtection, UnsuitableLowProtection
aglu.NONPROTECT_LAND_STATUS <- c("SuitableUnprotected","Unknown")

# Default fraction for protected land. This is used if the aglu.PROTECTION_DATA_SOURCE is set to TRUE or if protection data is unavailable.
aglu.PROTECT_DEFAULT<- 0.9

#Set the constants below to select the data source for carbon initialization. Currently set to `houghton`. Alternatively, this can be set to 'moirai'
aglu.CARBON_DATA_SOURCE <- "moirai"

# Available options for aglu.CARBON_STATE are median_value (median of all available grid cells), min_value (minimum of all available grid cells), max_value (maximum of all available grid cells),
# weighted_average (weighted average of all available grid cells using the land area as a weight), q1_value (first quartile of all available grid cells) and q3_value (3rd quartile of all available grid cells).
# Default recommended for GCAM is the q3_value. Note that these states can be selected only when using moirai as the carbon data source.
aglu.CARBON_STATE <- c("q3_value")


# Multiplier on the ghost share for irrigated land
aglu.IRR_GHOST_SHARE_MULT <- 0.25

# unManaged Land Value
# 1975$/thou km2 ??
aglu.UNMANAGED_LAND_VALUE <- 1

# default protected, unmanaged land LN1 logit info
aglu.LN1_PROTUNMGD_LOGIT_EXP  <- 0
aglu.LN1_PROTUNMGD_LOGIT_TYPE <- NA

# default logit exponent and type for LN5, the competition betweein high and lo management
aglu.MGMT_LOGIT_EXP  <- 2.5
aglu.MGMT_LOGIT_TYPE <- "absolute-cost-logit"

# Statistical differences reconciliation: China's Vegetable production estimates are inconsistent between the PRODSTAT
# ("Production") and SUA ("Commodity Balances"). Because the latter dataset is used for estimating food consumption in
# GCAM, and because these SUA food consumption estimates are derived from production data that is about 20% higher than
# PRODSTAT, this discrepancy causes very high negative "non-food" demands in this nation, which are large enough to
# result in negative non-food demands globally.
aglu.CHN_VEG_FOOD_MULT <- 0.8

# XML-related constants
aglu.CROP_GLU_DELIMITER   <- "_"  # delimiter between the crop name and GLU name
aglu.GLU_NDIGITS          <- 3    # number of digits in the geographic land unit identifier codes
aglu.IRR_DELIMITER        <- "_"  # delimiter between the appended crop x GLU and irrigation level
aglu.LT_GLU_DELIMITER     <-      # delimiter between the land use type name and GLU name. should be the same as the crop-glu delimiter
aglu.MGMT_DELIMITER       <- "_"  # delimiter between appended tech name and management level

# AgLU digits constants to control the number of digits for rounding going into XMLs.
aglu.DIGITS_AGPRODCHANGE  <- 4 # rate of change in yield values
aglu.DIGITS_C_DENSITY     <- 1
aglu.DIGITS_C_DENSITY_CROP <- 3 # cropland vegetative soil carbon content
aglu.DIGITS_CALOUTPUT     <- 7 # for production values
aglu.DIGITS_CALPRICE      <- 4 # prices and costs values
aglu.DIGITS_EROS_CTRL     <- 2
aglu.DIGITS_GHOSTSHARE    <- 3
aglu.DIGITS_HARVEST_INDEX <- 2
aglu.DIGITS_INCELAS       <- 4 # food demand income elasticity values
aglu.DIGITS_LAND_TOTAL    <- 2
aglu.DIGITS_LAND_USE      <- 7
aglu.DIGITS_LAND_VALUE    <- 0
aglu.DIGITS_MATUREAGE     <- 0
aglu.DIGITS_RES_ENERGY    <- 4
aglu.DIGITS_WATER_CONTENT <- 2


#Land leaf names used in the data system for different land types
aglu.PASTURE_NODE_NAMES <- "Pasture"
aglu.FOREST_NODE_NAMES <- "Forest"
aglu.GRASSLAND_NODE_NAMES <- "Grassland"


# Energy constants ======================================================================

# Time
energy.CDIAC_CO2_HISTORICAL_YEARS <- HISTORICAL_YEARS[HISTORICAL_YEARS < 2010] # At present the CO2 emissions inventory from CDIAC stops at 2009
energy.CLIMATE_NORMAL_YEARS       <- 1981:2000
energy.SATIATION_YEAR             <- max(MODEL_BASE_YEARS) # Needs to be the last model base year to avoid the risk of the model crashing
energy.UCD_EN_YEAR                <- 2005        # UCD transportation year to use to compute shares for allocation of energy to mode/technology/fuel within category/fuel
energy.WIND.BASE.COST.YEAR        <- 2005        # Base cost year for wind, used in capacity factor calculations

energy.REG_NG_MARKET <- "regional natural gas" #Name of the regional natural gas market

energy.MIN_WEIGHT_EJ <- 1e-08

# Transportation fixed charge rate information
energy.DISCOUNT_RATE_VEH <- 0.1   # Consumer discount rate for vehicle purchases
energy.NPER_AMORT_VEH    <- 10    # Number of periods (years) over which vehicle capital payments are amortized

energy.DEFAULT_ELECTRIC_EFFICIENCY <- 0.33

energy.ELECTRICITY_INPUT_FUELS <- c("biomass", "coal", "gas", "refined liquids")
energy.RSRC_FUELS              <- c("coal", "gas", "refined liquids")

# Assumed base year heat price, used for calculating adjustment to non-energy costs of electricity
# technologies with secondary output of heat in units of 1975$/EJ
energy.HEAT_PRICE <- 3.2
energy.GAS_PRICE  <- 2
energy.GAS_PIPELINE_COST_ADDER_75USDGJ  <- 0.1  # estimated cost mark-up from "regional natural gas" to "wholesale gas" (1975$/GJ)

energy.CO2.STORAGE.MARKET <- "carbon-storage"

# Indicate the ceiling on direct air capture in the USA region
energy.DAC_LIMIT_USA_MTC <- 2000

# the year for the ratio of industrial energy:feedstocks convergence in all regions
# in the old data system this was intended to be 2150 but was actually 2100
energy.INDCOEF_CONVERGENCE_YR <- 2100

energy.CEMENT_CCS_COST_2000USDTCO2 <- 50 # Starting point of supply curve in Mahasenan et al 2003; come from ENERGY_ASSUMPTIONS/A_ccs_data.R
energy.CO2_STORAGE_COST_1990_USDTC <- 42 # From GCAM 1.0 inputs; come from ENERGY_ASSUMPTIONS/A_ccs_data.R

energy.FLOOR_TO_SURFACE_RATIO <- 5.5
energy.GDP_MID_SATIATION      <- 10.5

energy.INTERNAL_GAINS_SCALAR_USA_H <- -930
energy.INTERNAL_GAINS_SCALAR_USA_C <- 350

# Used to avoid negative/zero energy when disaggregating detailed industries (cement, fertilizer)
energy.MIN_IN_EJ_IND <- 1e-3

# Sets maximum for electricity IO coefficient used in cement sector
energy.MAX_IOELEC <- 4

# Solar related constants
energy.HOURS_PER_YEAR          <- 24 * 365
energy.PV_COMM_INSTALLED_COST  <- 7290     # 2005USD per kw
energy.PV_COMM_OM              <- 40       # 2005USD per kw per year
energy.PV_DERATING_FACTOR      <- 0.77     # Incorporates various factors: inverters, transformers, mismatch, soiling, and others
energy.PV_DISCOUNT_RATE        <- 0.1      # year^-1
energy.PV_LIFETIME             <- 30       # years
energy.PV_RESID_INSTALLED_COST <- 9500     # 2005USD per kw
energy.PV_RESID_OM             <- 100      # 2005USD per kw per year
energy.CSP_STORAGE_CF_DIFF     <- 0.25     # capacity factor difference between CSP_storage (0.5) and CSP (0.25)
energy.SOLAR_ELECTROLYSIS_KGH2_D <- 50000    # kg of h2 produced per day at a solar-electrolysis plant
energy.ELECTROLYZER_RENEWABLE_CAPACITY_RATIO <- 0.618  # unitless capacity ratio of electrolyzers to renewable-electric equipment

# Wind related constants
energy.WIND_CURVE_MIDPOINT <- 0.5
energy.WIND_MIN_POTENTIAL <- 0.001
energy.WIND_ELECTROLYSIS_KGH2_D <- 50000    # kg of h2 produced per day at a wind-electrolysis plant

# Digits for rounding into XMLs
energy.DIGITS_CALOUTPUT        <- 7
energy.DIGITS_CALPRODUCTION    <- 7
energy.DIGITS_CAPACITY_FACTOR  <- 2
energy.DIGITS_CAPITAL          <- 0
energy.DIGITS_COEFFICIENT      <- 7
energy.DIGITS_COST             <- 4
energy.DIGITS_CURVE_EXPONENT   <- 3
energy.DIGITS_RESOURCE      <- 2
energy.DIGITS_EFFICIENCY       <- 3
energy.DIGITS_FLOORSPACE       <- 6
energy.DIGITS_GDP_SUPPLY_ELAST <- 3
energy.DIGITS_HDDCDD           <- 0
energy.DIGITS_INCELAS_IND      <- 3
energy.DIGITS_INCELAS_TRN      <- 3
energy.DIGITS_LOADFACTOR       <- 2
energy.DIGITS_MAX_SUB_RESOURCE <- 5
energy.DIGITS_MID_PRICE        <- 3
energy.DIGITS_MPKM             <- 0
energy.DIGITS_OM               <- 2
energy.DIGITS_REMOVE.FRACTION  <- 2
energy.DIGITS_SATIATION_ADDER  <- 9
energy.DIGITS_SHRWT            <- 4
energy.DIGITS_SPEED            <- 1
energy.DIGITS_TECHCHANGE       <- 4

#jf 2021-10-21
#defines fraction of liquid fuel consumption used for off-road vehicles in agriculture, mining, and construction energy use.
#the remainder is allocated to stationary equipment (e.g., generators)
energy.LIQUID_FUEL_MOBILE_FRAC <- 0.8

# Policy assumptions for module_energy_L270.limits
energy.NEG_EMISS_POLICY_NAME    <- "negative_emiss_budget"
energy.NEG_EMISS_TARGET_GAS     <- "CO2_LTG" # the name of the gas to target in the negative emiss budget
energy.NEG_EMISS_GDP_BUDGET_PCT <- 0.01 # Max fraction of GDP which may be given to subsidize net negative emissions
energy.NEG_EMISS_MARKT_GLOBAL   <- TRUE # If the negative emissions budget is global (TRUE) or regional (FALSE)
energy.OIL_CREDITS_MARKETNAME   <- "oil-credits"
energy.OILFRACT_ELEC            <- 1.0 # Fraction of liquids for feedstocks that must come from oil
energy.OILFRACT_FEEDSTOCKS      <- 0.8 # Fraction of liquids for oil electricity that must come from oil

# kbn 2019-10-11 Adding constant for transportation type. Set this to 'rev.mode' to use revised mode classes, 'rev_size.class' to use revised size classes.
# To use the old modes and size classes, use 'mode' and 'size.class' for the constants. The default for GCAM are the new modes and size classes.

energy.TRAN_UCD_MODE<-'rev.mode'
energy.TRAN_UCD_SIZE_CLASS<-'rev_size.class'

# Constants related to ATB power sector technology costs
energy.ATB_2017_YEARS <- c(2015:2016)
energy.ATB_BASE_YEAR <- 2015
energy.ATB_MID_YEAR <- 2035
energy.ATB_TARGET_YEAR <- 2035
gcamusa.STORAGE_TECH <- "battery"
energy.COSTS_MID_CASE <- "central"
energy.COSTS_ADV_CASE <- "adv tech"
energy.COSTS_LOW_CASE <- "low tech"
energy.CAPITAL_INPUT <- "capital"
energy.OM_FIXED_INPUT <- "OM-fixed"
energy.OM_VAR_INPUT <- "OM-var"

# Constants for the residential sector: Parameters for USA (estimated offline) and unadjusted saturation values:
energy.OBS_UNADJ_SAT <- 100
gcamusa.OBS_UNADJ_SAT <- 150

gcamusa.LAND_DENSITY_PARAM <- 0
gcamusa.B_PARAM <- 3.49026
gcamusa.INCOME_PARAM <- 0.4875

# Constants for global detailed industry
energy.OFF_ROAD.BIOMASS_GROWTH <- c("Africa_Eastern","Africa_Southern","Africa_Western") #limit fast growth of biomass in agriculture energy use
energy.IRON_STEEL.DEFAULT_COEF <- c("Biomass-based","scrap","H2 wholesale delivery") #assign iron & steel global technology coefficients
energy.IRON_STEEL.RESOURCES <- c("Other semi-finished iron and steel products","Rolled iron and steel",
                                 "Iron and steel bars","Intermediate iron and steel making products",
                                 "Iron and steel wire","Iron and steel sections") #finished and semi-finished iron and steel resources
energy.IRON_STEEL.DOMESTIC_SW <- c("Africa_Southern","Indonesia","Africa_Northern","Africa_Eastern","Africa_Western","South Asia","Southeast Asia")
energy.IRON_STEEL.TRADED_SW <- c("Africa_Southern traded iron and steel","Indonesia traded iron and steel","Africa_Northern traded iron and steel","Africa_Eastern traded iron and steel","Africa_Western traded iron and steel","South Asia traded iron and steel","Southeast Asia traded iron and steel")

# Socioeconomics constants ======================================================================

# Population years - note that these sequences shouldn't have any overlap,
# and should contain all historical years used by other modules
socioeconomics.MADDISON_HISTORICAL_YEARS <- seq(1700, 1900, 50) # Years for which to use Maddison data
socioeconomics.UN_HISTORICAL_YEARS       <- c(1950, 1971:2015)  # Years for which to use UN data
socioeconomics.PWT_CONSTANT_CURRENCY_YEAR <- 2011 # Currency base year in Penn World Table data

# Final historical year, we use this because it's also the first year of the SSP database.
# Using a different year if the final historical year in the UN historical years changes, this would result in
# different SSP projections. (Because the SSP scenarios begin to diverge in 2015, so we'd have to reconsider how
# we do the SSP scenarios if we update to UN 2015 population.)
socioeconomics.FINAL_HIST_YEAR <- 2015

# Sets the years during which the IMF projections are used over-riding the default (generally SSP) assumptions.
socioeconomics.IMF_GDP_YEARS <- 2015:2024
# There will be an imblance of trade by region historically which is implicitly balanced by
# capital flows.  We can phase this out by the year assumed below (linearly).  Note, setting a value
# beyond the final model year will hold the final historical net capital flows constant.  Either
# approach could be reasonable
socioeconomics.TRADE_BALANCE_YEAR <- 2035

# CES elasticity of substitution parameter governing sharing behavior between energy
# and "value added" (the capital-labor nest)
socioeconomics.CES_RHO <- 0.5
# CES elasticity of substitution parameter governing sharing behavior between capital and labor
socioeconomics.CES_GAMMA <- -0.3

socioeconomics.BASE_POP_SCEN         <- "SSP2"
socioeconomics.BASE_GDP_SCENARIO     <- "SSP2"
socioeconomics.DEFAULT_MEDIAN_HOURS_WORKED <- 1944

# Asumptions related to tracking capital investments
socioeconomics.DEFAULT_INTEREST_RATE <- 0.10
socioeconomics.RESOURCE_CAPITAL_RATIO <- 0.6 # loosley based on GTAP capital shares of total cost
socioeconomics.REFINING_CAPITAL_RATIO <- 0.6 # loosley based on GTAP capital shares of total cost
socioeconomics.REFINING_CAP_PAYMENTS <- 30
socioeconomics.H2_CAPITAL_RATIO <- 0.8
socioeconomics.H2_CAP_PAYMENTS <- 30
socioeconomics.INDUSTRY_CAPITAL_RATIO <- 0.9
socioeconomics.INDUSTRY_CAP_PAYMENTS <- 30
socioeconomics.BUILDINGS_CAPITAL_RATIO <- 1.0
socioeconomics.BUILDINGS_CAP_PAYMENTS <- 1
socioeconomics.BUILDINGS_DEPRECIATION_RATE <- 1/15
socioeconomics.TRANSPORT_LDV_DEPRECIATION_RATE <- 1/15
socioeconomics.TRANSPORT_DEPRECIATION_RATE <- 1/30


# Digits for rounding into XMLs
socioeconomics.DEFAULT_LABORFORCE        <- 0.5
socioeconomics.GDP_DIGITS                <- 0
socioeconomics.LABOR_PRODUCTIVITY_DIGITS <- 5
socioeconomics.POP_DIGITS                <- 0

# Names for GDP energy accounting
socioeconomics.EN_SERVICE_NAME <- "energy service"
socioeconomics.EN_TRADE_NAME <- "energy net export"
socioeconomics.EN_CAPITAL_MARKET_NAME <- "capital"
socioeconomics.EN_DURABLE_MARKET_NAME <- "consumer durable"
# list of final energy sectors which will serve as "energy service" used in the macro model
socioeconomics.FINAL_DEMAND_SECTORS <- c("other industrial energy use",
                                         "process heat cement",
                                         "agricultural energy use",
                                         "construction energy use",
                                         "mining energy use",
                                         "chemical energy use",
                                         "alumina",
                                         "iron and steel",
                                         "resid cooling",
                                         "resid heating",
                                         "resid others",
                                         "comm cooling",
                                         "comm heating",
                                         "comm others",
                                         "trn_freight",
                                         "trn_freight_road",
                                         "trn_shipping_intl",
                                         "trn_aviation_intl",
                                         "trn_pass",
                                         "trn_pass_road",
                                         "trn_pass_road_LDV",
                                         "trn_pass_road_LDV_4W")

# for filling missing socioeconomic data
socioeconomics.TAIWAN_REGION_ID <- 30


# Water constants ======================================================================

water.ALL_WATER_TYPES                     <- c("water consumption",
                                               "water withdrawals",
                                               "seawater",
                                               "biophysical water consumption")
water.AG_ONLY_WATER_TYPES                 <- "biophysical water consumption"
water.COOLING_SYSTEM_CAPACITY_FACTOR      <- 0.6   # Cooling system capacity factor (Unitless)
water.COOLING_SYSTEM_FCR                  <- 0.15  # Cooling system fixed charge rate (Unitless)
water.COOLING_SYSTEM_LOGIT 				        <- -5    # Cooling system logit (Unitless)
water.DEFAULT_IRR_WATER_PRICE             <- 0.001 # 1975$/m3. This excludes water abstraction costs explicitly modeled.
water.DEFAULT_UNLIMITED_WATER_PRICE       <- 0
water.DEFAULT_UNLIMITED_WITHD_WATER_PRICE <- 0.00001 # 1975$/m3. This excludes water abstraction costs explicitly modeled.
water.DEFAULT_BASEYEAR_WATER_PRICE        <- 0.001
water.GRADE_HIST_UPPER_BOUND              <- 0.0062  # This value is derived from the Superwell cost bins, and indicates the upper bound production cost of nonrenewable groundwater during the historical period
water.IRR_PRICE_SUBSIDY_MULT              <- 0.05  # Multiplier for irrigation water price (OECD 2009 Managing Water for All; aiming for 1% of muni water price)
water.DRY_COOLING_EFF_ADJ 				        <- 0.95  # Dry cooling efficiency adjustment (Unitless)
water.IRRIGATION                          <- "Irrigation"
water.MANUFACTURING                       <- "Manufacturing"
water.MUNICIPAL                           <- "Municipal"
water.MAPPED_WATER_TYPES                  <- c("water consumption", "water withdrawals")
water.MAPPED_WATER_TYPES_SHORT            <- c("C", "W")
names(water.MAPPED_WATER_TYPES_SHORT)     <- water.MAPPED_WATER_TYPES
water.DESAL                               <- "desalinated water"
water.LIVESTOCK                           <- "Livestock"
water.PRIMARY_ENERGY                      <- "Mining"
water.LIVESTOCK_TYPES                     <- c("Beef","Dairy","Pork","Poultry","SheepGoat")
water.DELETE_DEMAND_TYPES                 <- c("water_td_elec_W", "water_td_elec_C", "water_td_muni_W", "water_td_muni_C", "water_td_ind_W", "water_td_ind_C")

water.WATER_UNITS_PRICE                   <- "1975$/m^3"
water.WATER_UNITS_QUANTITY                <- "km^3"
water.DIGITS_MUNI_WATER                   <- 4
water.IRR_SHARE                           <- 1    # Irrigation demands are compiled by basin, so no separate basin-within-region sharing is applied
water.MAPPING_COEF                        <- 1    # The default assumption is that water withdrawals by sector refer to withdrawals from a source (i.e. upstream of any losses). Modified for irrigation.
water.MAPPING_PMULT                       <- 1    # The default assumption is that estimated water production costs are paid by the consumers. Again this is modified for irrigation water consumers.
water.NONIRRIGATION_SECTORS               <- c("Municipal", "Electricity", "Livestock", "Manufacturing", "Mining")
water.LOGIT_EXP                           <- -6
water.GW_HIST_MULTIPLIER                  <- 1.01   # Multiplier for the "available" in the "grade hist" grade of groundwater supply curves. Needed to prevent the model from pulling from higher grades in historical periods due to solution tolerance
water.DIGITS_TD_FLOWS                     <- 9     # Need a large number of digits here due to a large number of tiny numbers

# GCAM intermediate sectors for which Vassolo + Doll assessed manufacturing water demands. In the paper, they indicate
# chemicals, pulp and paper, pig iron, sugar, beer, cloth, cement, and crude steel. some industrial mfg does take place
# in energy transformation (charcoal, pig iron), so we'll leave that one in.
water.GCAM_MFG_SECTORS_VASSOLO <- c("in_industry_general", "net_industry_energy transformation")

# GCAM intermediate fuels used for extrapolating manufacturing water use from one base year to all base years.
water.GCAM_MFG_FUELS_EFW <- c("electricity")

# the maximum portion of aquastat industrial (mfg + elec) water withdrawals that is allowed to be assigned to
# manufacturing. Used to set a cap on derived manufacturing water withdrawals
water.MAX_MFG_FRAC_OF_IND <- 0.85

# Groundwater may be calibrated using either the "watergap" or "gleeson" historical groundwater depletion estimates.
water.GROUNDWATER_CALIBRATION <- "watergap"  # "gleeson"
water.GROUNDWATER_SCENARIO <- "25pct" # may be "05pct", "25pct", or "40pct" (i.e., 5, 25, 40 % of groundwater)

# Groundwater depletable resource curve parameters (see Kim et al., 2016)
water.GROUNDWATER_MAX_PRICE_INC <- 10000
water.GROUNDWATER_UNIFORM_GRADES <- 10
water.GROUNDWATER_BETA <- 1.0
water.DIGITS_GROUND_WATER <- 6 # Digits for rounding estimates of historical groundwater depletion
water.DIGITS_GROUND_WATER_RSC <- 5 # Digits for rounding groundwater supply curves (available, extractioncost)
water.DIGITS_RENEW_WATER <- 4 # Digits for rounding historical averages of runoff and access fraction
water.DIGITS_RENEW_WATER_RSC <- 7 # Digits for rounding renewable resource supply curves (available, extractioncost)
water.GW_DEPLETION_HISTORICAL <- c(2005, 2010, 2015) # Historical years for groundwater depletion
water.GW_DEPLETION_BASE_YEAR <- 1990 # Historical year for groundwater depletion calibration
water.RUNOFF_HISTORICAL <- c(1990, 2005, 2010, 2015) # Historical years for freshwater runoff
water.RENEW.COST.GRADE1 <- 0.00001 # Renewable water grade1 cost
water.RENEW.COST.GRADE2 <- 0.001 # Renewable water grade2 cost
water.RENEW.COST.GRADE3 <- 10 # Renewable water grade3 cost
water.DEMAND_FRAC_THRESHOLD <- 1e-4 # Demand fraction of total runoff below which we use a 3-point supply curve to help model solution

# Energy-for-water constants ======================================================================

efw.GW_ABSTRACTION_EFW                    <- 0.000666  # GJ of electricity per cubic meter of groundwater produced
efw.SW_ABSTRACTION_EFW                    <- 0.000284  # GJ of electricity per cubic meter of surface water produced
efw.MAX_COMMIND_ENERGY_DESAL              <- 0.15      # maximum share of commercial + industrial sector energy consumption that can be re-assigned to desalination
efw.MAX_AGCOMM_ENERGY_IRR                 <- 0.5       # maximum share of ag + commercial energy that can be re-assigned to irrigation
efw.MAX_AG_ENERGY_IRR                     <- 1         # maximum share of reported agricultural energy that can be re-assigned to irrigation
efw.MAX_IND_ENERGY_EFW                    <- 0.05      # maximum share of industrial sector energy consumption that can be re-assigned to industrial manufacturing energy-for-water
efw.MAX_COMMIND_ENERGY_MUNI_EFW           <- 0.2       # maximum share of commercial + industrial sector energy consumption that can be re-assigned to municipal water systems
efw.MAX_COMM_ENERGY_MUNI_EFW              <- 0.3       # maximum share of commercial sector energy consumption that can be re-assigned to municipal water systems
efw.MAX_WWTRT_FRAC                        <- 0.85      # maximum share of municipal/industrial water use that can be treated as wastewater (considering consumptive uses)
efw.WWTRT_STEEPNESS                       <- 10        # steepness assumption relating per-capita GDP to the portion of municipal/industrial wastewater treated
efw.DEFAULT_IRR_ELEC_PRICE_75USDGJ        <- 15        # default assumed price paid for the electricity used for irrigation water abstraction

efw.WWTRT_GDP_SCEN                        <- "SSP2"    # scenario to use for increasing the fraction of wastewater treated as a function of GDP in post-2010 years
efw.COUNTRIES_ELEC_DESAL                  <- c("are", "qat", "sau")     # countries whose desalination-related energy is in the power sector (combined electric + desal plants)
efw.COUNTRIES_NO_DESAL                    <- c("mng", "pry", "mhl", "tjk", "tkm")   # landlocked or tiny countries whose reported desalinated water production is not included in GCAM
efw.DESAL_ENERGY_SECTORS                  <- c("in_bld_comm", "in_industry_general")    # intermediate mapping sectors where desalination-related energy is initially assigned (from the energy balances)
efw.ELEC_DESAL_TECHS                      <- c("gas (steam/CT)", "refined liquids (steam/CT)")    # Electric generation technologies that can produce secondary output of desalinated seawater




# Emissions constants ======================================================================

# scaling CH4 and N2O emissions to EPA 2019 mitigation report BAU emission trajectory
emissions.NONCO2.EPA.SCALING <- FALSE
emissions.EPA.SCALING.THRESHOLD <- 50 # EPA emissions/ CEDS emission, used to check scaling outliers in L112 chunk
emissions.EPA.SCALING.THRESHOLD.COMBUSTION <- 20 # check scaling outliers in L112 chunk for combustion sector

# default unconventional oil fugitive emfacts (Tg/EJ) for regions without historical unconventional oil. Based on emissions factors from the 2019 Refinement to the 2006
# IPCC Guidelines for National GHG Inventories; calculated as the weighted average of the oil sands emissions factor and average non-oil-sands emissions factor.
# Weights come from the share of oil sands and non-oil-sands global recoverable unconventional oil resources (Wang 2016 https://doi.org/10.1016/S1876-3804(16)30111-2)
# Emfacts for regions with historical unconventional oil are specified in emissions/IPCC_unconventional_oil_fug_emfacts.csv
emissions.UNCONVENTIONAL.OIL.FUG.CO2.EMFACT <- 0.994
emissions.UNCONVENTIONAL.OIL.FUG.CH4.EMFACT <- 0.0882
emissions.UNCONVENTIONAL.OIL.FUG.N2O.EMFACT <- 0.000000939

# Time
emissions.CEDS_YEARS              <- 1970:2019           # Year coverage for CEDS inventory.
emissions.CTRL_BASE_YEAR          <- 1975                # Year to read in pollution controls
emissions.DEFOREST_COEF_YEARS     <- c(2000, 2005)
emissions.EDGAR_YEARS             <- 1971:2008
emissions.EPA_HISTORICAL_YEARS    <- 1971:2002
emissions.EPA_MACC_YEAR           <- seq(2015, 2050, 5)        # based on 2019 EPA nonCO2 report
emissions.EPA_MACC_FUTURE_YEAR    <- seq(2055, 2100, 5)        # EPA report only covers till 2050
emissions.EPA_TC_TIMESTEP         <- 5   # currently calculate EPA MAC-based technological change based on every 5 years
emissions.EPA_BAU_HIST_YEAR       <- c(1990, 1995, 2000, 2005, 2010, 2015) # based on 2019 EPA nonCO2 report
emissions.FINAL_EMISS_YEAR        <- min(max(MODEL_BASE_YEARS), 2005)
emissions.GAINS_BASE_YEAR         <- 2005
emissions.GAINS_YEARS             <- c(2010, 2020, 2030)
emissions.GHG_CONTROL_READIN_YEAR <- 1975
emissions.HFC_MODEL_BASE_YEARS    <- MODEL_YEARS[ MODEL_YEARS <= 2010] # We don't want this to change in timeshift
emissions.INVENTORY_MATCH_YEAR    <- 2009                # Select year from which to calculate fuel emissions coefficients (2009 is currently the most recent)
emissions.MODEL_BASE_YEARS        <- MODEL_BASE_YEARS
emissions.NH3_EXTRA_YEARS         <- 1971:1989
emissions.NH3_HISTORICAL_YEARS    <- 1990:2002
emissions.SSP_FUTURE_YEARS        <- MODEL_YEARS[MODEL_YEARS %in% 2015:2100]
emissions.HFC_FUT_YEAR            <- 2030            # max year for emissions factors in L241.fgas
emissions.GV_YEARS                <- c(2020, 2030)   # years to fill in from Guus Velders data

# Other emissions constants
emissions.CONV_C_CO2    <- 44 / 12 # Convert Carbon to CO2
emissions.F_GAS_UNITS   <- "Gg"
emissions.TST_TO_TG     <- 0.000907 # Thousand short tons to Tg
emissions.ZERO_EM_TECH  <- c("electricity", "Electric", "BEV","FCEV","district heat","NG","LA-BEV")  # These technologies get filtered out and no emissions are generated for them. Note that NG emissions for vehicles are added directly from GAINS and not calculated.
emissions.HIGH_EM_FACTOR_THRESHOLD <- 1000  # All emission factors above this threshold are replaced with the global median of emission factors.
emissions.GFED_NODATA <- c("ala","bes","blm","ggy","jey","maf","xad","xko","xnc")  # GFED LULC dataset does not contaian data for these isos. These get filtered out so we can use the left_join_error_no_match.
emissions.UNMGD_LAND_AVG_YRS <- 30 # Years for climatological average for the GFED LULC data.
emissions.CEDS_SCALE    <- "usa" # iso's that will be scaled to CEDS emissions
emissions.CH4.GWP.AR4 <- 25 # used for EPA non-CO2 scaling, the 2019 EPA non-CO2 report uses AR4 GWPs
emissions.N2O.GWP.AR4 <- 298 # used for EPA non-CO2 scaling, the 2019 EPA non-CO2 report uses AR4 GWPs

emissions.COAL_SO2_THRESHOLD <- 0.1   # Tg/EJ (here referring to Tg SO2 per EJ of coal electricity)
emissions.LOW_PCGDP          <- 2.75  # thousand 1990 USD
emissions.MAC_TAXES          <- c(0, 2, 4, 6, 13, 27, 53, 100, 200, 450, 850, 2000, 3000, 5000) # Range of MAC curve costs to keep to read into GCAM; they are in EPA's units (2010USD_tCO2e)
emissions.MAC_MARKET         <- "CO2" # Default market that MAC curves will look for
emissions.MAC_HIGHESTREDUCTION <- 0.95 # a high MAC reduction used to replace calculated values there are greater than 1

emissions.AGR_SECTORS        <- c("rice", "fertilizer", "soil")
emissions.AGR_GASES          <- c("CH4_AGR", "N2O_AGR", "NH3_AGR", "NOx_AGR")
emissions.AG_MACC_GHG_NAMES  <- c("CH4_AGR", "N2O_AGR")
emissions.GHG_NAMES          <- c("CH4", "N2O")
emissions.NONGHG_PROC_SECTORS <- c("SO2_1", "SO2_2", "SO2_3", "SO2_4", "NOx", "CO", "NMVOC", "PM2.5", "PM10")
emissions.REFGHG_GASES		   <- c("NOx","SO2","PM2.5","CO","NH3")
emissions.NONGHG_GASES       <- c("SO2", "NOx", "CO", "NMVOC", "NH3")
emissions.PFCS               <- c("CF4", "C2F6", "SF6")
emissions.TRN_INTL_SECTORS   <- c("trn_intl_ship", "trn_intl_air")

emissions.USE_GCAM3_CCOEFS     <- 1 # Select whether to use GCAM3 fuel carbon coefficients
emissions.USE_GLOBAL_CCOEFS    <- 1 # Select whether to use global average carbon coefficients on fuels, or region-specific carbon coefficients
emissions.UNMGD_LAND_INPUT_NAME <- "land-input"

emissions.FUGITIVE_FOSSIL_CO2_NAME <- "CO2_FUG" # name for fugitive co2 emissions from fossil production
emissions.FOSSIL_EMFACT_THRESHOLD_PERCENTILE <- 0.95 # energy production percentile used in determination of fossil emfact upper thresholds
emissions.FOSSIL_EMFACT_THRESHOLD_TOP_PRODUCERS <- 0.9975 # percent of production defining the "top producers" used in determination of fossil emfact upper thresholds


# Digits for rounding into XMLs
emissions.DIGITS_CO2COEF       <- 1
emissions.DIGITS_EMISS_COEF    <- 7
emissions.DIGITS_EMISSIONS     <- 10
emissions.DIGITS_MACC          <- 3

# IND URB PROCESSES CONSTANTS (copied to all regions in world in zchunk_L231.proc_sector.R (hard coded - may want to change to these)
# using these same constants for all states in GCAM-USA chunk L231.proc_sector_USA.R)
# would be good to know where these values come from
emissions.FINAL_DEMAND <- "urban processes"
emissions.FINAL_DEMAND_PCB <- 1 # per capita based
emissions.FINAL_DEMAND_INCELAS <- 0 # income elasticity
emissions.FINAL_DEMAND_BASE_SERVICE <- 0.004
emissions.FINAL_DEMAND_AEEI <- 0 # autonomous energy efficiency improvement
emissions.REG_TECH_CAL_VALUE_MINICAM_ENERGY_INPUT <- "misc emissions sources"
emissions.REG_TECH_CAL_VALUE <- 0.001
emissions.IND_PROC_INPUT <- 0.008
emissions.IND_PROC_MINICAM_ENERGY_INPUT <- "industrial processes"
emissions.DIGITS_MACC_TC       <- 4 # tech.change rounding
emissions.DIGITS_GFED          <- 12

# Parameters for the urban processing sector
emissions.URBAN_PROCESS_PERCAPITABASED <- 1 # service as function of population?
emissions.URBAN_PROCESS_INCOME_ELASTICITY <- 0
emissions.URBAN_PROCESS_BASE_SERVICE <- 0.004 # base service (per capita)
emissions.URBAN_PROCESS_AEEI <- 0 # No energy efficiency improvements
# Calibrated value for misc emissions from industrial and urban processes
emissions.INDURB_PROCESS_MISCEMISSIONS_CALVAL <- 0.001

# GCAM-USA constants ======================================================================

# GCAM-USA time
gcamusa.SEDS_DATA_YEARS <- 1971:2017 # years for which we'll use EIA SEDS data in module_gcamusa_LA101.EIA_SEDS
gcamusa.WIND_BASE_COST_YEAR <- 2005
gcamusa.HYDRO_HIST_YEAR <- 2015
gcamusa.HYDRO_FINAL_AEO_YEAR <- 2050

gcamusa.SE_HIST_YEAR <- 2015  # year to which historical socioeconomic data (pop & GDP) are used in GCAM-USA
gcamusa.SE_NEAR_TERM_YEAR <- 2030  # year after which projected growth rates from various socio-economic data sources are used as-is
# (until this year, growth rates are interpolated from 2015 historical values to prevent spikey near-term behavior)
gcamusa.AEO_SE_YEAR <- 2050   # year to which AEO 2019 socioeconomic assumptions run

# Assumptions related to coal

# Vintage groups built before 2015 will retire based on an S-curve.
# Assumed lifetime and S-curve parametetrs for coal units:
gcamusa.AVG_COAL_PLANT_LIFETIME <- 80
gcamusa.AVG_COAL_PLANT_HALFLIFE <- 70
gcamusa.COAL_RETIRE_STEEPNESS <- 0.3

# Profit shutdown parameters
gcamusa.MEDIAN_SHUTDOWN_POINT <- -0.1
gcamusa.PROFIT_SHUTDOWN_STEEPNESS <- 6

# Define vintage bins and categories
# These categories chosen for lifetime assumptions are such that capacity in each category is roughly same.
# This is done to get a somewhat smooth behavior for coal retirements.
gcamusa.COAL_VINTAGE_BREAKS <- c(0, seq(1950, 2015, 5))
gcamusa.COAL_VINTAGE_LABELS <- c("before 1950", "1951-1955", "1956-1960", "1961-1965", "1966-1970", "1971-1975", "1976-1980",
                                 "1981-1985", "1986-1990", "1991-1995", "1996-2000", "2001-2005", "2006-2010", "2011-2015")

gcamusa.FIRST_NEW_COAL_YEAR <- 2035

# GCAM-USA states
gcamusa.STATES <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA",
                    "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR",
                    "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# USA states with ocean coastline, which can be reasonably assigned offshore carbon storage
gcamusa.COASTAL_STATES <- c("AK", "AL", "CA", "CT", "DE", "FL", "GA", "HI", "LA", "MA", "MD", "ME", "MS",
                            "NC", "NH", "NJ", "NY", "OR", "RI", "SC", "TX", "VA", "WA")


# GCAM-USA grid regions
gcamusa.GRID_REGIONS <- c("Alaska grid", "California grid", "Central East grid", "Central Northeast grid",  "Central Northwest grid",
                          "Central Southwest grid", "Florida grid", "Hawaii grid", "Mid-Atlantic grid", "New England grid",
                          "New York grid", "Northwest grid", "Southeast grid", "Southwest grid", "Texas grid")

# GCAM-USA default constants
gcamusa.DEFAULT_COEFFICIENT <- 1
gcamusa.DEFAULT_LOGIT_TYPE  <- NA  # default logit type
gcamusa.DEFAULT_LOGITEXP    <- -3
gcamusa.DEFAULT_MARKET      <- gcam.USA_REGION
gcamusa.DEFAULT_SHAREWEIGHT <- 1
gcamusa.FIXED_SHAREWEIGHT <- "fixed"
gcamusa.LINEAR_SHAREWEIGHT <- "linear"
gcamusa.INTERP_APPLY_TO <- "share-weight"

# Water related constants for GCAM-USA
gcamusa.DISALLOWED_COOLING_TECH <- "once through"
gcamusa.MUNICIPAL_SECTOR <- "Municipal"
gcamusa.UCS_WATER_TYPE_OCEAN <- "Ocean"
gcamusa.UCS_WATER_TYPE_SURFACE <- "Surface Water"
gcamusa.WATER_TYPE_FRESH <- "fresh"
gcamusa.WATER_TYPE_SEAWATER <- "seawater"
gcamusa.ELEC_COOLING_SYSTEM_NONE <- "none"
gcamusa.ELEC_COOLING_SYSTEM_BINARY <- "binary"
gcamusa.ELEC_TECHS_NO_COOLING <- c("hydro", "PV", "wind")
gcamusa.ELEC_TECHS_NO_COOLING_FRESH <- c("hydro", "PV") # electricity technologies with no cooling system, still mapped to "fresh" water type
gcamusa.UCS_WATER_FIRST_YEAR <- 2000 # we calculate cooling shares based on power plants built this century

# Logit exponent regulating competition between different grid regions in USA electricity market
# (single market approach only)
gcamusa.GRID_REGION_LOGIT      <- -6

gcamusa.GRID_REGION_LOGIT_TYPE <- "relative-cost-logit"

gcamusa.GEOTHERMAL_DEFAULT_EFFICIENCY <- 0.1

gcamusa.ELECT_TD_SECTORS  <- c("elect_td_bld", "elect_td_ind", "elect_td_trn")

#Fuels whose markets will be represented with state-specific prices
gcamusa.STATE_FUEL_MARKETS <- c(gcamusa.ELECT_TD_SECTORS, "H2 industrial", "H2 retail delivery", "H2 retail dispensing",
                                "H2 wholesale delivery", "H2 wholesale dispensing","H2 central production","H2 pipeline","H2 liquid truck")

# Indicate whether to use regional ?cost adders? to differentiate
# fuel prices by grid region in GCAM-USA (FALSE = same prices in all states)
gcamusa.USE_REGIONAL_FUEL_MARKETS  <- TRUE

# GCAM-USA fertlizer constants
gcamusa.FERT_LOGIT_EXP  <- -3             # Define default logit expoent used in the fertlizer subsector
gcamusa.FERT_LOGIT_TYPE <- NA
gcamusa.FERT_NAME       <- "N fertilizer" # Define GCAM-USA category name of fertilizer

# Fuels whose markets will be modeled at the level of the FERC regions, with prices calibrated
gcamusa.REGIONAL_FUEL_MARKETS <- c("regional coal", "delivered coal", "wholesale gas", "delivered gas",
                                   "refined liquids industrial", "refined liquids enduse")


# Resources that will be modeled at the state level
gcamusa.STATE_RENEWABLE_RESOURCES <- c("distributed_solar", "geothermal", "onshore wind resource", "offshore wind resource")
gcamusa.STATE_UNLIMITED_RESOURCES <- c("global solar resource", "limestone", "scrap")

# Define sector(s) used in L222.en_transformation_USA
# The supplysector and subsector structure in these sectors are retained
gcamusa.SECTOR_EN_NAMES <- "refining"

# Define intermittent technologies
gcamusa.INT_TECH_LIST <- c("CSP", "PV", "wind")

# Define storage technologies
gcamusa.STORAGE_TECH_LIST <- c("CSP_storage", "PV_storage", "wind_storage")


# Degree day norms
gcamusa.BASE_HDD_USA <- 4524 # https://www.eia.gov/totalenergy/data/annual/showtext.php?t=ptb0107
gcamusa.BASE_CDD_USA <- 1215 # https://www.eia.gov/totalenergy/data/annual/showtext.php?t=ptb010
gcamusa.AEO_DD_YEARS <- seq(2010, 2040, 5)


# Years to be adjusted for RECS
gcamusa.RECS_YEARS <- c(2009,2015)

gcamusa.GAS_ADJ_THRESH      <- 5

# Some xml delimiter
gcamusa.STATE_SUBSECTOR_DELIMITER <- " "

# Number of digits for model input data
gcamusa.DIGITS_CALOUTPUT          <- 7    # production
gcamusa.EFFICIENCY_PARTITION_YEAR <- 2010
gcamusa.DIGITS_TRNUSA_DEFAULT     <- 1    # Reduce rounding in detailed USA transport for compatability with model
gcamusa.DIGITS_EMISSIONS          <- 5

# Electricity load segments
gcamusa.LOAD_SEG_CAL_YEARS <- c(2015,2010, 2005, 1990)       # Years for which electricity load segments are calibrated
gcamusa.ELEC_SEGMENT_BASE <- "base load generation"
gcamusa.ELEC_SEGMENT_INT <- "intermediate generation"
gcamusa.ELEC_SEGMENT_SUBPEAK <- "subpeak generation"
gcamusa.ELEC_SEGMENT_PEAK <- "peak generation"

# Water mapping assumptions
gcamusa.FINAL_MAPPING_YEAR <- 2010 # Water mappings are conducted from the Huang et al. (2018) dataset which are through 2010, not the final historical year
gcamusa.WATER_MAPPING_YEAR <- 2005
gcamusa.USA_REGION_NUMBER <- 1
gcamusa.ZERO_WATER_COEF <- 0
gcamusa.CONVEYANCE_LOSSES <- 0.829937455747218 ## From file: L165.ag_IrrEff_R
water.MAPPED_PRI_WATER_TYPES                  <- c("water consumption", "water withdrawals","desalination")
gcamusa.MIN_PRIM_ENERGY_YEAR <- 1990

# GCAM-USA does not have energy-for-water so desalination is an exogenous, unlimited resource with a fixed price
gcamusa.DESALINATION_PRICE                  <- 0.214  # 1975$/m3

# GCAM-USA transportation emissions vehicle classes
gcamusa.MOVES_BASE_YEAR_CLASSES <- c(2005,2010,2015) # Year classes of cars to be used to get base year vintaged emissions
gcamusa.MOVES_MIN_VINTAGE_YEAR <- 1990 # Earliest vintage year used
gcamusa.MOVES_MAX_AGE<- 25 # Maximum age used
gcamusa.MARKAL_DEGRADE_YEARS <- 15 # Number of years we have EF degradation values for
gcamusa.MOTO_VINTAGES <- c(seq(gcamusa.MOVES_MIN_VINTAGE_YEAR, max(MODEL_FUTURE_YEARS), 5)) # Vintages to select from MOVES motorcycle data

# GCAM-USA transportation emissions fuels to filter out because there is no intensity data
gcamusa.MARKAL_LDV_FILTER_OUT_FUELS <- c("B20","PH10G","PH10E")
gcamusa.MARKAL_MINICAR_FILTER_OUT_FUELS <- c("DSL", "E10", "E15")

# GCAM-USA Transportation sectors
gcamusa.TRANSPORT_SECTORS <- c("trn_freight", "trn_pass","trn_shipping_intl", "trn_aviation_intl")
gcamusa.LDV_SUPPLYSECTORS <- c("trn_pass_road_LDV_4W", "trn_pass_road_LDV")
gcamusa.HDV_SUPPLYSECTORS <- c("trn_pass_road","trn_freight_road")
gcamusa.GCAM_TRANSPORT_SECTORS <- c("Road")

# GCAM-USA transportation EFs to change to be based on NEI emissions/own service demand for that vehicle class directly
gcamusa.TRANSUBSECTOR_CHANGE_EF <- c("Heavy truck")
gcamusa.YEAR_CHANGE_EF <- c("2010")
gcamusa.STUB_TECH_CHANGE_EF <- c("Liquids")
gcamusa.NONCO2_CHANGE_EF <- c("NOx", "SO2", "PM2.5", "PM10", "NH3", "CO", "NMVOC", "BC", "OC")

# GCAM-USA transportation base years and future years (different than model base years and future years)
# Calibrated periods in the model. Only level 2 chunks should reference these
gcamusa.TRAN_MODEL_BASE_YEARS <- c(1975, 1990, 2005, 2010, 2015)
# Future (not calibrated) model periods. Only level 2 chunks should reference these
gcamusa.TRAN_MODEL_FUTURE_YEARS <- seq(2020, 2100, 5)

# defined for MARKAL EF years in LA171 gcam-usa chunk
gcamusa.TRN_MARKAL_EMISSION_YEARS <- seq(2005,2050, 5)

# defined for EF years in L271 gcam-usa chunk
gcamusa.TRN_EMISSION_YEARS <- seq(2005,2100, 5)

# emission factor timestep
gcamusa.TRN_EF_TIMESTEP <- 5

# GCAM-USA StubTranTech missing lifetime
gcamusa.STUBTRANTECH_LIFETIME_2045V <- 25 # lifetime for missing vehicles vintages 2045 and earlier
gcamusa.STUBTRANTECH_LIFETIME_2050V <- 20 # lifetime for missing vehicles vintages 2050 and later

# GCAM-USA groups to filter out of degrades table (to not predict degradation of EF over time)
gcamusa.DEGRADES_FILTER_OUT_MARKAL_CLASS <- "Small SUV"
gcamusa.DEGRADES_FILTER_OUT_MARKAL_FUEL <- "ELC"
gcamusa.DEGRADES_FILTER_OUT_NONCO2 <-c("PM2.5", "PM10")

# GCAM-USA other transportation
gcamusa.INTL_SHIP_PM_RATIO <- 0.92 # this is the ratio of PM2.5 to PM10 for international shipping emissions. Value calculated from the EPA US inventory modelling platform 2016v2 20aug2021

# GCAM-USA process emissions
gcamusa.IND_PROC_EM_NEI_GCAM_SECTORS <- c("industry_processes", "solvents")
gcamusa.URB_PROC_EM_NEI_GCAM_SECTORS <- c("landfills", "wastewater", "waste_incineration")
gcamusa.CEMENT_NEI_GCAM_SECTORS <- c("cement")
gcamusa.NONGHG_PROC_SECTORS.MISSING_POLLUTANTS <- c("PM2.5", "PM10", "NH3")
gcamusa.NONGHG_PROC_SECTORS.MISSING_SUBSECTORS <- c("wastewater")
gcamusa.NONGHG_PROC_SECTORS.GDP_MAX_REDUCTION <- 30
gcamusa.NONGHG_PROC_SECTORS.GDP_STEEPNESS <- 3.5

gcamusa.PROC_DEFAULT_SECTOR <- "industrial processes"
gcamusa.PROC_DEFAULT_S_T <- "other industrial processes"

gcamusa.CEMENT_TECHS <- c("cement", "cement CCS")

# GCAM-USA industry / industrial energy
# Define sector(s) used in L275.indenergy_nonghg_USA and L231.proc_sector_USA
gcamusa.IND_SECTOR_NAME <- "other industry"
gcamusa.IND_EN_SECTOR_NAME <- "other industrial energy use"
gcamusa.IND_FDSTCK_SECTOR_NAME <- "other industrial feedstocks"

# Number of digits for model input data
gcamusa.DIGITS_TRN_EF_DEGRADE     <- 15

# GCAM-USA petroleum fuel conversion factors
gcamusa.CONVERSIONFACTOR_NATURALGAS_GJ_PER_T_NET <- 48.0 # Natural Gas GJ/t. (Divide TJ by net heating value (LHV) to get kt) 2006 IPCC guidelines for National GHG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2
gcamusa.CONVERSIONFACTOR_MOTORGASOLINE_GJ_PER_T_NET <- 44.75 # Motor gasoline(3) GJ/t, IEA energy statistics manual
gcamusa.CONVERSIONFACTOR_DIESEL_GJ_PER_T_NET <- 43.38 # Gas/diesel oil GJ/t, IEA energy statistics manual
gcamusa.CONVERSIONFACTOR_RESIDUALOIL_GJ_PER_T_NET <- 41.57 # Fuel oil GJ/t, high-sulphur, IEA energy statistics manual

# Non-CO2 BC - OC - PM conversion factors
gcamusa.OC_TO_OM <- 1.3
gcamusa.PM1_TO_PM2.5 <- 1.1

# Non-CO2 BC and OC onroad scaling factors
gcamusa.BC_1990_ONROAD_SCALING_FACTOR <- 1/1.4
gcamusa.OC_1990_ONROAD_SCALING_FACTOR <- 1/1.4

# If this variable is FALSE, onroad dust emissions (CEDS 1A3b_Road-noncomb) will not be included
# If it is TRUE, onroad dust emissions will be included
gcamusa.DUST <- TRUE

# Time shift conditions ======================================================================
# Uncomment these lines to run under 'timeshift' conditions
# # HISTORICAL_YEARS <- 1971:2005       # normally 1971:2010
# MODEL_FUTURE_YEARS <- seq(2010, 2100, 5)  # normally seq(2015, 2100, 5)
# MODEL_BASE_YEARS <- c(1975, 1990, 2005)   # normally (1975, 1990, 2005, 2010)
# MODEL_YEARS <- c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)
