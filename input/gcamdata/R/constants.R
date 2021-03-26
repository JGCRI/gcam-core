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
FLAG_SUM_TEST        <- "FLAG_SUM_TEST"         # use less-restrictive sum test
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


# Driver constants ======================================================================

driver.MAKE            <- "MAKE"
driver.DECLARE_OUTPUTS <- "DECLARE_OUTPUTS"
driver.DECLARE_INPUTS  <- "DECLARE_INPUTS"

# Data and utility constants ======================================================================

data.SEPARATOR <- "; "
data.PRECURSOR <- "Precursor"
data.DEPENDENT <- "Dependent"


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
CONV_TST_TG         <- 0.000907       # thousand short tons to Tg

# Time
CONV_YEAR_HOURS <- 24 * 365.25
CONV_DAYS_YEAR <- 1 / 365.25

# Energy
CONV_MWH_GJ <- 3.6 # Megawatt hours to Gigajoules
CONV_MWH_EJ <- 3.6e-9 # Megawatt hours to Exajoules
CONV_GWH_EJ <- 3.6e-6
CONV_TWH_EJ <- 3.6e-3
CONV_KWH_GJ <- 3.6e-3
CONV_GJ_EJ <- 1e-9
CONV_EJ_GJ <- 1 / CONV_GJ_EJ
CONV_MBLD_EJYR <- 6.119 * 365.25 * 1e-3 # million barrels a day to EJ per year
CONV_KBTU_EJ <- 1.0551e-12 # KiloBTU to EJ
CONV_TBTU_EJ <- 0.0010551 # TeraBTU to EJ
CONV_MJ_BTU <- 947.777
CONV_BTU_KJ <- 1.0551

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

# AgLU constants ======================================================================

# Time
aglu.AGLU_HISTORICAL_YEARS  <- 1971:2015
aglu.BASE_YEAR_IFA          <- 2006      # Base year of International Fertilizer Industry Association (IFA) fertilizer application data KD does this belong here???
aglu.BIO_START_YEAR         <- 2020
aglu.CROSIT_HISTORICAL_YEAR <- 2005      # Historical year from the CROSIT data
aglu.DIET_YEARS             <- seq(max(aglu.AGLU_HISTORICAL_YEARS), 2050, by = 5)
aglu.FAO_HISTORICAL_YEARS   <- 1961:2015
aglu.FAO_LDS_YEARS          <- 1998:2002  # Years for which FAO harvested area data is averaged over for use in the land data system (LDS)
aglu.GTAP_HISTORICAL_YEAR   <- 2000      # Is the year that the GTAP data is based on.
aglu.LAND_HISTORY_YEARS     <- c(1700, 1750, 1800, 1850, 1900, 1950, 1975)
aglu.LAND_COVER_YEARS       <- sort(unique(c(aglu.LAND_HISTORY_YEARS, aglu.AGLU_HISTORICAL_YEARS)))
aglu.MODEL_COST_YEARS       <- 2008:2016
aglu.MODEL_PRICE_YEARS      <- 2008:2016
aglu.PREAGLU_YEARS          <- c(1700, 1750,1800, 1850, 1900, 1950)          # Cropland cover years prior to first aglu historical year to use in climate model component
aglu.DEFLATOR_BASE_YEAR     <- 2015                                          # year used as the basis for computing regional price deflators
aglu.SPEC_AG_PROD_YEARS     <- seq(max(aglu.AGLU_HISTORICAL_YEARS), 2050, by = 5) # Specified ag productivity years, KD i think this might need a better comment
aglu.SSP_DEMAND_YEARS       <- seq(2015, 2100, 5) # food demand in the SSPs is calculated at 5-yr intervals
aglu.TRADE_CAL_YEARS        <- 2008:2012 # Years used for calculating base year gross trade. Should ideally include the final base year, but note that the trade data starts in 1986.
aglu.TRADE_FINAL_BASE_YEAR  <- max(MODEL_BASE_YEARS) # The base year to which gross trade volumes are assigned. Should be within the aglu.TRADE_CAL_YEARS and equal to the final model calibration year
aglu.FALLOW_YEARS           <- 2008:2012 # Years used for calculating the % of fallow land
aglu.TRADED_CROPS           <- c("Corn", "FiberCrop", "MiscCrop", "OilCrop", "OtherGrain", "PalmFruit", "Rice", "RootTuber", "SugarCrop", "Wheat")
aglu.TRADED_MEATS           <- c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")
aglu.TRADED_AG_AN           <- c(aglu.TRADED_CROPS, aglu.TRADED_MEATS)
aglu.LAND_TOLERANCE    <- 0.005
aglu.MIN_PROFIT_MARGIN <- 0.15  # Unitless and is used to ensure that Agricultural Costs (units 1975USD/kg) don't lead to profits below a minimum profit margin.
aglu.MAX_FAO_LDS_SCALER <- 5   # Unitless max multiplier in reconciling LDS harvested area with FAO harvested area by country and crop. Useful for preventing bad allocations of N fert in AFG, TWN, several others

# GLU (Geographic Land Unit) settings - see module_aglu_LA100.0_LDS_preprocessing
aglu.GLU <- "GLU"
aglu.GLU_NAME_DELIMITER <- ""  # delimiter between the GLU name and number

# FAO PRICESTAT database disaggregates "cottonseed" and "cotton lint" as different commodities.
# This is the weight used to calculate the weighted average producer price for "seed cotton",
# based on that FAO total production volume of "seed cotton" is about 40% cotton lint and 60% cotton seeds.
# Source: http://www.fao.org/es/faodef/fdef06e.htm
aglu.WEIGHT_COTTON_LINT <- 0.4

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

# Minimum non-input costs of animal production technologies, in $/kg
aglu.MIN_AN_NONINPUT_COST <- 0.05

# Production constraints
aglu.MAX_MGDPAST_FRAC <- 0.95 # Maximum percentage of any region/GLUs pasture that is allowed to be in managed production.
aglu.MAX_MGDFOR_FRAC  <- 1    # Maximum percentage of any region/GLUs forest that is allowed to be in managed production.

# GDP constraints
aglu.HIGH_GROWTH_PCGDP <- 12.275   # GDP per capita high threshold for SSP4 region groupings, thousand 2010$ per person
aglu.LOW_GROWTH_PCGDP  <- 2.75     # GDP per capita low threshold for SSP4 region groupings, thousand 2010$ per person

# AgLu mulitpliers
aglu.MGMT_YIELD_ADJ <- 0.2       # Yield multiplier that goes from the observed yield to the "high" and "low" yields: observed plus or minus observed times this number.
aglu.HI_PROD_GROWTH_MULT <- 1.5  # Multipliers for high ag prod growth scenarios
aglu.LOW_PROD_GROWTH_MULT <- 0.5 # Multipliers for low ag prod growth scenarios

# AgLU cost constants
aglu.BIO_GRASS_COST_75USD_GJ <- 0.75   # Production costs of biomass (from Patrick Luckow's work)
aglu.BIO_TREE_COST_75USD_GJ  <- 0.67   # Production costs of biomass (from Patrick Luckow's work)
aglu.FERT_PRICE              <- 596    # Price of fertilizer, 2010$ per ton NH3
aglu.FERT_PRICE_YEAR         <- 2010    # Year corresponding to the above price/cost
aglu.FOR_COST_75USDM3        <- 29.59  # Forestry cost (1975$/GJ)

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
#kbn 2019/09/25 Took taiwan out from below since we have data for Taiwan now.
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
aglu.CSOIL_MULT_UNMGDPAST_MGDPAST <- 0.9     # stay conservative here b/c no data source

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

# Define top-level (zero) land nest logit exponent and logit type
aglu.N0_LOGIT_EXP  <- 0
aglu.N0_LOGIT_TYPE <- NA

# Fraction of land that is protected
aglu.PROTECT_LAND_FRACT <- 0.9

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


# Energy constants ======================================================================

# Time
energy.CDIAC_CO2_HISTORICAL_YEARS <- HISTORICAL_YEARS[HISTORICAL_YEARS < 2010] # At present the CO2 emissions inventory from CDIAC stops at 2009
energy.CLIMATE_NORMAL_YEARS       <- 1981:2000
energy.SATIATION_YEAR             <- max(MODEL_BASE_YEARS) # Needs to be the last model base year to avoid the risk of the model crashing
energy.UCD_EN_YEAR                <- 2005        # UCD transportation year to use to compute shares for allocation of energy to mode/technology/fuel within category/fuel
energy.WIND.BASE.COST.YEAR        <- 2005        # Base cost year for wind, used in capacity factor calculations


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

# PV related constants
energy.HOURS_PER_YEAR          <- 24 * 365
energy.PV_COMM_INSTALLED_COST  <- 7290     # 2005USD per kw
energy.PV_COMM_OM              <- 40       # 2005USD per kw per year
energy.PV_DERATING_FACTOR      <- 0.77     # Incorporates various factors: inverters, transformers, mismatch, soiling, and others
energy.PV_DISCOUNT_RATE        <- 0.1      # year^-1
energy.PV_LIFETIME             <- 30       # years
energy.PV_RESID_INSTALLED_COST <- 9500     # 2005USD per kw
energy.PV_RESID_OM             <- 100      # 2005USD per kw per year

# Wind related constants
energy.WIND_CURVE_MIDPOINT <- 0.5
energy.WIND_MIN_POTENTIAL <- 0.001

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
energy.DIGITS_FLOORSPACE       <- 3
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

# Policy assumptions for module_energy_L270.limits
energy.NEG_EMISS_POLICY_NAME    <- "negative_emiss_budget"
energy.NEG_EMISS_TARGET_GAS     <- "CO2_LTG" # the name of the gas to target in the negative emiss budget
energy.NEG_EMISS_GDP_BUDGET_PCT <- 0.01 # Max fraction of GDP which may be given to subsidize net negative emissions
energy.NEG_EMISS_MARKT_GLOBAL   <- TRUE # If the negative emissions budget is global (TRUE) or regional (FALSE)
energy.OIL_CREDITS_MARKETNAME   <- "oil-credits"
energy.OILFRACT_ELEC            <- 1.0 # Fraction of liquids for feedstocks that must come from oil
energy.OILFRACT_FEEDSTOCKS      <- 0.8 # Fraction of liquids for oil electricity that must come from oil

#kbn 2019-10-11 Adding constant for transportation type. Set this to rev.mode to use revised mode classes, rev_size.class to use revised size classes.
#To use the old modes and size classes, use mode and size.class for the constants. The default for GCAM are the new modes and size classes.

energy.TRAN_UCD_MODE<-'rev.mode'
energy.TRAN_UCD_SIZE_CLASS<-'rev_size.class'


# Socioeconomics constants ======================================================================

# Population years - note that these sequences shouldn't have any overlap,
# and should contain all historical years used by other modules
socioeconomics.MADDISON_HISTORICAL_YEARS <- seq(1700, 1900, 50) # Years for which to use Maddison data
socioeconomics.UN_HISTORICAL_YEARS       <- c(1950, 1971:2015)  # Years for which to use UN data

# Final historical year, we use this because it's also the first year of the SSP database.
# Using a different year if the final historical year in the UN historical years changes, this would result in
# different SSP projections. (Because the SSP scenarios begin to diverge in 2015, so we'd have to reconsider how
# we do the SSP scenarios if we update to UN 2015 population.)
socioeconomics.FINAL_HIST_YEAR <- 2015

# Sets the years during which the IMF projections are used over-riding the default (generally SSP) assumptions.
socioeconomics.IMF_GDP_YEARS <- 2015:2024

socioeconomics.BASE_POP_SCEN         <- "SSP2"
socioeconomics.BASE_GDP_SCENARIO     <- "SSP2"
socioeconomics.DEFAULT_INTEREST_RATE <- 0.05

# Digits for rounding into XMLs
socioeconomics.DEFAULT_LABORFORCE        <- 0.5
socioeconomics.GDP_DIGITS                <- 0
socioeconomics.LABOR_PRODUCTIVITY_DIGITS <- 5
socioeconomics.POP_DIGITS                <- 0


# Water constants ======================================================================

water.ALL_WATER_TYPES                     <- c("water consumption",

"water withdrawals",
"seawater",
"biophysical water consumption",
"desalination")

water.AG_ONLY_WATER_TYPES                 <- "biophysical water consumption"
water.COOLING_SYSTEM_CAPACITY_FACTOR      <- 0.6   # Cooling system capacity factor (Unitless)
water.COOLING_SYSTEM_FCR                  <- 0.15  # Cooling system fixed charge rate (Unitless)
water.COOLING_SYSTEM_LOGIT 				        <- -5    # Cooling system logit (Unitless)
water.DEFAULT_IRR_WATER_PRICE             <- 0.00175 # (Units: 1975$/m3)
water.DEFAULT_UNLIMITED_WATER_PRICE       <- 0
water.DEFAULT_UNLIMITED_WITHD_WATER_PRICE <- 0.001
water.DEFAULT_BASEYEAR_WATER_PRICE        <- 0.001
water.IRR_PRICE_SUBSIDY_MULT              <- 0.05  # Multiplier for irrigation water price (OECD 2009 Managing Water for All; aiming for 1% of muni water price)
water.DRY_COOLING_EFF_ADJ 				        <- 0.95  # Dry cooling efficiency adjustment (Unitless)
water.IRRIGATION                          <- "Irrigation"
water.MAPPED_WATER_TYPES                  <- c("water consumption", "water withdrawals")
water.WATER_UNITS_PRICE                   <- "1975$/m^3"
water.WATER_UNITS_QUANTITY                <- "km^3"
water.DIGITS_MUNI_WATER                   <- 4
water.DESALINATION_PRICE                  <- 0.214  # 1975$/m3
water.IRR_SHARE                           <- 1
water.MAPPING_COEF                        <- 1
water.MAPPING_PMULT                       <- 1
water.NONIRRIGATION_SECTORS               <- c("Municipal", "Electricity", "Livestock", "Manufacturing", "Mining")
water.LOGIT_EXP                           <- -6
water.GW_HIST_MULTIPLIER                  <- 1.01   # Multiplier for the "available" in the "grade hist" grade of groundwater supply curves. Needed to prevent the model from pulling from higher grades in historical periods due to solution tolerance




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
water.DIGITS_GROUND_WATER <- 6 #Digits for rounding
water.DIGITS_GROUND_WATER_RSC <- 5 #Digits for rounding
water.DIGITS_RENEW_WATER <- 3 #Digits for rounding
water.GW_DEPLETION_HISTORICAL <- c(2005, 2010, 2015) # Historical years for groundwater depletion
water.GW_DEPLETION_BASE_YEAR <- 1990 # Historical year for groundwater depletion calibration
water.RUNOFF_HISTORICAL <- c(1990, 2005, 2010, 2015) # Historical years for freshwater runoff
water.RENEW.COST.GRADE1 <- 0.00001 #Renewable water grade1 cost
water.RENEW.COST.GRADE2 <- 0.001 #Renewable water grade2 cost
water.RENEW.COST.GRADE3 <- 10 #Renewable water grade3 cost




# Emissions constants ======================================================================

# scaling CH4 and N2O emissions to EPA 2019 mitigation report BAU emission trajectory
emissions.nonCO2.EPA.scaling <- FALSE
emissions.EPA.scaling.threshold <- 50 # EPA emissions/ CEDS emission, used to check scaling outliers in L112 chunk
emissions.EPA.scaling.threshold.combustion <- 20 # check scaling outliers in L112 chunk for combustion sector

# Time
emissions.CEDS_YEARS              <- 1971:2019           # Year coverage for CEDS inventory.
emissions.CTRL_BASE_YEAR          <- 1975                # Year to read in pollution controls
emissions.DEFOREST_COEF_YEARS     <- c(2000, 2005)
emissions.EDGAR_YEARS             <- 1971:2008
emissions.EPA_HISTORICAL_YEARS    <- 1971:2002
emissions.EPA_MACC_YEAR           <- seq(2015, 2050, 5)        # based on 2019 EPA nonCO2 report
emissions.EPA_MACC_FUTURE_YEAR    <- seq(2055, 2100, 5)        # EPA report only covers till 2050
emissions.EPA_TC_TimeStep         <- 5   # currently calculate EPA MAC-based technological change based on every 5 years
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

# Other emissions constants
emissions.CONV_C_CO2    <- 44 / 12 # Convert Carbon to CO2
emissions.F_GAS_UNITS   <- "Gg"
emissions.TST_TO_TG     <- 0.000907 # Thousand short tons to Tg
emissions.ZERO_EM_TECH  <- c("electricity", "Electric", "BEV","FCEV","district heat","NG","LA-BEV")  #These technologies get filtered out and no emissions are generated for them. Note that NG emissions for vehicles are added directly from GAINS and not calculated.
emissions.HIGH_EM_FACTOR_THRESHOLD <- 1000  #All emission factors above this threshold are replaced with the global median of emission factors.
emissions.GFED_NODATA <- c("ala","bes","blm","ggy","jey","maf","xad","xko","xnc")  #GFED LULC dataset does not contaian data for these isos. These get filtered out so we can use the left_join_error_no_match.
emissions.UNMGD_LAND_AVG_YRS <- 30 #Years for climatological average for the GFED LULC data.
emissions.CH4.GWP.AR4 <- 25 # used for EPA non-CO2 scaling, the 2019 EPA non-CO2 report uses AR4 GWPs
emissions.N2O.GWP.AR4 <- 298 # used for EPA non-CO2 scaling, the 2019 EPA non-CO2 report uses AR4 GWPs

emissions.COAL_SO2_THRESHOLD <- 0.1   # Tg/EJ (here referring to Tg SO2 per EJ of coal electricity)
emissions.LOW_PCGDP          <- 2.75  # thousand 1990 USD
emissions.MAC_TAXES          <- c(0, 2, 4, 6, 13, 27, 53, 100, 200, 450, 850, 2000, 3000, 5000) # Range of MAC curve costs to keep to read into GCAM; they are in EPA's units (2010USD_tCO2e)
emissions.MAC_MARKET         <- "CO2" # Default market that MAC curves will look for
emissions.MAC_highestReduction <- 0.95 # a high MAC reduction used to replace calculated values there are greater than 1

emissions.AGR_SECTORS        <- c("rice", "fertilizer", "soil")
emissions.AGR_GASES          <- c("CH4_AGR", "N2O_AGR", "NH3_AGR", "NOx_AGR")
emissions.AG_MACC_GHG_NAMES  <- c("CH4_AGR", "N2O_AGR")
emissions.GHG_NAMES          <- c("CH4", "N2O")
emissions.NONGHG_GASES       <- c("SO2", "NOx", "CO", "NMVOC", "NH3")
emissions.PFCS               <- c("CF4", "C2F6", "SF6")
emissions.TRN_INTL_SECTORS   <- c("trn_intl_ship", "trn_intl_air")

emissions.USE_GCAM3_CCOEFS     <- 1 # Select whether to use GCAM3 fuel carbon coefficients
emissions.USE_GLOBAL_CCOEFS    <- 1 # Select whether to use global average carbon coefficients on fuels, or region-specific carbon coefficients
emissions.UNMGD_LAND_INPUT_NAME <- "land-input"

# Digits for rounding into XMLs
emissions.DIGITS_CO2COEF       <- 1
emissions.DIGITS_EMISS_COEF    <- 7
emissions.DIGITS_EMISSIONS     <- 10
emissions.DIGITS_MACC          <- 3
emissions.DIGITS_MACC_TC       <- 4 # tech.change rounding

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
gcamusa.STATE_UNLIMITED_RESOURCES <- c("global solar resource", "limestone")

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
gcamusa.FINAL_MAPPING_YEAR <- 2010 #    Water mappings are conducted from the Huang et al. (2018) dataset which are through 2010, not the final historical year
gcamusa.WATER_MAPPING_YEAR <- 2005
gcamusa.USA_REGION_NUMBER <- 1
gcamusa.ZERO_WATER_COEF <- 0
gcamusa.CONVEYANCE_LOSSES <- 0.829937455747218 ## From file: L165.ag_IrrEff_R
water.MAPPED_PRI_WATER_TYPES                  <- c("water consumption", "water withdrawals","desalination")
gcamusa.MIN_PRIM_ENERGY_YEAR <- 1990

water.LIVESTOCK                           <- "Livestock"
water.PRIMARY_ENERGY                      <- "Mining"
water.LIVESTOCK_TYPES                     <- c("Beef","Dairy","Pork","Poultry","SheepGoat")
water.DELETE_DEMAND_TYPES              <- c("water_td_elec_W","water_td_elec_C","water_td_dom_W","water_td_dom_C", "water_td_ind_W","water_td_ind_C")
water.MAPPED_WATER_TYPES_SHORT            <- c("C", "W")
names(water.MAPPED_WATER_TYPES_SHORT)     <- water.MAPPED_WATER_TYPES

# Time shift conditions ======================================================================
# Uncomment these lines to run under 'timeshift' conditions
# # HISTORICAL_YEARS <- 1971:2005       # normally 1971:2010
# MODEL_FUTURE_YEARS <- seq(2010, 2100, 5)  # normally seq(2015, 2100, 5)
# MODEL_BASE_YEARS <- c(1975, 1990, 2005)   # normally (1975, 1990, 2005, 2010)
# MODEL_YEARS <- c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)
