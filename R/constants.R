# General behavior constants ======================================================================

OUTPUTS_DIR  <- "outputs/"
XML_DIR      <- "xml/"
COMMENT_CHAR <- "#"
OLD_DATA_SYSTEM_BEHAVIOR <- TRUE
UNDER_TIMESHIFT <- FALSE
YEAR_PATTERN <- "^(1|2)[0-9]{3}$"   # a 1 or 2 followed by three digits, and nothing else
LOGIT_COLUMN_NAME <- "logit.type"   # will be removed by test code before old-new comparison


# Flags ======================================================================
# Flags used by chunks
FLAG_INPUT_DATA      <- "FLAG_INPUT_DATA"       # input data, don't output
FLAG_LONG_YEAR_FORM  <- "FLAG_LONG_YEAR_FORM"   # 'year' column but original data are wide
FLAG_NO_OUTPUT       <- "FLAG_NO_OUTPUT"        # don't output
FLAG_NO_XYEAR        <- "FLAG_NO_XYEAR"         # year names don't have X's in front
FLAG_NO_TEST         <- "FLAG_NO_TEST"          # don't test
FLAG_SUM_TEST        <- "FLAG_SUM_TEST"         # use less-restrictive sum test
FLAG_PROTECT_FLOAT   <- "FLAG_PROTECT_FLOAT"    # protect float columns from readr bug
FLAG_XML             <- "FLAG_XML"              # xml data
FLAG_YEAR_COL_XYEARS <- "FLAG_YEAR_COL_XYEARS"  # 'year' column without X's in front


# Time constants======================================================================

HISTORICAL_YEARS <- 1971:2010
IMF_GDP_YEARS <- 2010:2020
FUTURE_YEARS <- seq(2015, 2100, 5)
BASE_YEARS <- c(1975, 1990, 2005, 2010)
MODEL_YEARS <- c(BASE_YEARS, FUTURE_YEARS)
SSP_FUTURE_YEARS <- c(2010, FUTURE_YEARS)
GHG_CONTROL_READIN_YEAR <- 1975
BASE_YEAR_IFA <- 2006


# GCAM constants ======================================================================

gcam.USA_CODE <- 1
gcam.WESTERN_EUROPE_CODE <- 13
gcam.LOGIT_TYPES <- c("relative-cost-logit", "absolute-cost-logit")
gcam.EQUIV_TABLE <- "EQUIV_TABLE"

GCAM_REGION_ID <- "GCAM_region_ID"


# AgLU constants ======================================================================

AGLU_HISTORICAL_YEARS <- 1971:2010
FAO_HISTORICAL_YEARS <- 1961:2011
FAO_LDS_YEARS <- 1998:2002
MODEL_PRICE_YEARS <- 2001:2005
MODEL_COST_YEARS <- 2001:2005
LAND_HISTORY_YEARS <- c(1700, 1750, 1800, 1850, 1900, 1950, 1975)
PREAGLU_YEARS <- c(1700, 1750,1800, 1850, 1900, 1950)
aglu.LAND_COVER_YEARS <- sort(unique(c(LAND_HISTORY_YEARS, AGLU_HISTORICAL_YEARS)))
GTAP_HISTORICAL_YEAR <- 2000
CROSIT_HISTORICAL_YEAR <- 2005
aglu.BIO_START_YEAR <- 2020
SPEC_AG_PROD_YEARS <- seq(max(AGLU_HISTORICAL_YEARS), 2050, by = 5) # Specified ag productivity years
aglu.DIET_YEARS <- seq(max(AGLU_HISTORICAL_YEARS), 2050, by = 5)
MIN_PROFIT_MARGIN <- 0.15
LAND_TOLERANCE <- 0.005
DIGITS_LAND_TOTAL <- 2
DIGITS_LAND_USE <- 7

# GLU (Geographic Land Unit) settings - see module_aglu_LA100.0_LDS_preprocessing
aglu.GLU <- "GLU"
aglu.GLU_NAME_DELIMITER <- ""  # delimiter between the GLU name and number

# FAO PRICESTAT database disaggregates "cottonseed" and "cotton lint" as different commodities.
# This is the weight used to calculate the weighted average producer price for "seed cotton",
# based on that FAO total production volume of "seed cotton" is about 40% cotton lint and 60% cotton seeds.
# Source: http://www.fao.org/es/faodef/fdef06e.htm
WEIGHT_COTTON_LINT <- 0.4

# Price conversion from alfalfa to grass hay
# Sources:
# Alfalfa price: USDA. 2011. Prices Received for Alfalfa Hay, Baled, Washington. National Agricultural Statistics Service, U.S. Department of Agriculture.
# Grass price: Baker, A., and H. Lutman. 2008. Feed Year in Review (Domestic): Record Demand Drives U.S. Feed Grain Prices Higher in 2007/2008.
# FDS-2008-01, Economic Research Service, United States Department of Agriculture. Available at http://usda.mannlib.cornell.edu/usda/ers/FDS-yearbook/2000s/2008/FDS-yearbook-05-23-2008_Special_Report.pdf
PRICERATIO_GRASS_ALFALFA <- 0.7

# NUMBERS OF DIGITS FOR MODEL INPUT DATA
aglu.DIGITS_CALPRICE <- 4 # prices and costs
aglu.DIGITS_CALOUTPUT <- 7 # production
aglu.DIGITS_INCELAS <- 4 # food demand income elasticity
aglu.DIGITS_AGPRODCHANGE <- 4 # rate of change in yield

# Carbon content of all cellulose
aglu.CCONTENT_CELLULOSE <- 0.45

# Minimum and maximum harvested:cropped ratios
MIN_HA_TO_CROPLAND <- 1
# Source: Dalrymple, D.G. 1971, Survey of Multiple Cropping in Less Developed Nations, Foreign Econ. Dev. Serv., U.S. Dep. of Agricul., Washington, D.C.
# Cited in: Monfreda et al. 2008, Farming the Planet: 2., Global Biogeochemical Cycles 22, GB1022, http://dx.doi.org/10.1029/2007GB002947
MAX_HA_TO_CROPLAND <- 3

# Maximum portion of any land use region's pastures that can be in production
MAX_MGDPAST_FRAC <- 0.95
MAX_MGDFOR_FRAC <- 1

# Yield multiplier that goes from the observed yield to the "high" and "low" yields: observed plus or minus observed times this number
MGMT_YIELD_ADJ <- 0.1

# Meat price elasticity in the USA
aglu.FOOD_MEAT_P_ELAS_USA <- -0.09

# Multipliers for high & low ag prod growth scenarios
aglu.HI_PROD_GROWTH_MULT <- 1.5
aglu.LOW_PROD_GROWTH_MULT <- 0.5

# Forestry cost (1975$/GJ)
aglu.FOR_COST_75USDM3 <- 29.59

# Production costs of biomass (from Patrick Luckow's work)
aglu.BIO_GRASS_COST_75USD_GJ <- 0.75
aglu.BIO_TREE_COST_75USD_GJ <- 0.67

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
aglu.BIO_GRASS_YIELD_KGCM2 <- 0.34
aglu.BIO_TREE_FERT_IO_GNM2 <- 3.36
aglu.BIO_TREE_YIELD_KGCM2 <- 0.345

# Water characteristics for biomass
# Reference: Chaturvedi et al. 2015, Climate mitigation policy implications for global irrigation water demand, Mitig Adapt Strateg Glob Change (2015) 20:389-407. DOI 10.1007/s11027-013-9497-4
aglu.BIO_GRASS_WATER_IO_KM3EJ <- 25
aglu.BIO_TREE_WATER_IO_KM3EJ <- 25

# Cost of Fertilizer
aglu.FERT_COST <- 363 # 2007$ per ton NH3

# Minimum non-input costs of animal production technologies, in $/kg
aglu.MIN_AN_NONINPUT_COST <- 0.05

# Maximum bioenergy (switchgrass) yield allowable, in tons per hectare
# Source: Wullschleger doi:10.2134/agronj2010.0087
aglu.MAX_BIO_YIELD_THA <- 20

aglu.BIO_ENERGY_CONTENT_GJT <- 17.5  # Energy content of biomass, GJ/ton

# GDP per capita thresholds for SSP4 region groupings
aglu.HIGH_GROWTH_PCGDP <- 12.275   # thousand 2010$ per person
aglu.LOW_GROWTH_PCGDP  <- 2.75     # thousand 2010$ per person

# Regions in which agriculture and land use are not modeled
aglu.NO_AGLU_REGIONS <- "Taiwan"

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

# define top-level (zero) land nest logit exponent and logit type
aglu.N0_LOGIT_EXP <- 0
aglu.N0_LOGIT_TYPE <- NA

# fraction of land that is protected
aglu.PROTECT_LAND_FRACT <- 0.9

# unManaged Land Value
# 1975$/thou km2 ??
aglu.UNMANAGED_LAND_VALUE <- 1

# default protected, unmanaged land LN1 logit info
aglu.LN1_PROTUNMGD_LOGIT_EXP <- 0
aglu.LN1_PROTUNMGD_LOGIT_TYPE <- NA

# default logit exponent and type for LN5, the competition betweein high and lo management
aglu.MGMT_LOGIT_EXP <- 0.5
aglu.MGMT_LOGIT_TYPE <- "absolute-cost-logit"

# XML-related constants
aglu.GLU_NDIGITS          <- 3    # number of digits in the geographic land unit identifier codes
aglu.LT_GLU_DELIMITER     <-      # delimiter between the land use type name and GLU name. should be the same as the crop-glu delimiter
aglu.CROP_GLU_DELIMITER   <- "_"  # delimiter between the crop name and GLU name
aglu.IRR_DELIMITER        <- "_"  # delimiter between the appended crop x GLU and irrigation level
aglu.MGMT_DELIMITER       <- "_"  # delimiter between appended tech name and management level
aglu.CROP_DELIMITER       <- "_"  # delimiter between (some) crop names such as Root_Tuber, biomass_grass, biomass_tree

# some more digits for rounding going into XMLs
aglu.DIGITS_HARVEST_INDEX <- 2
aglu.DIGITS_EROS_CTRL     <- 2
aglu.DIGITS_RES_ENERGY    <- 4
aglu.DIGITS_WATER_CONTENT <- 2
aglu.DIGITS_LAND_VALUE    <- 0
aglu.DIGITS_LAND_USE      <- 7
aglu.DIGITS_C_DENSITY     <- 1
aglu.DIGITS_MATUREAGE     <- 0

# Energy constants ======================================================================

# At present the CO2 emissions inventory from CDIAC stops at 2009
energy.CDIAC_CO2_HISTORICAL_YEARS <- HISTORICAL_YEARS[HISTORICAL_YEARS < 2010]

# Constant to select SSP database to use for transportation UCD
energy.TRN_SSP <- "CORE"

# UCD transportation year to use to compute shares for allocation of energy to mode/technology/fuel within category/fuel
energy.UCD_EN_YEAR <- 2005
energy.MIN_WEIGHT_EJ <- 1e-08

# Transportation fixed charge rate information
energy.DISCOUNT_RATE_VEH <- 0.1   # Consumer discount rate for vehicle purchases
energy.NPER_AMORT_VEH <- 10    # Number of periods (years) over which vehicle capital payments are amortized

DEFAULT_ELECTRIC_EFFICIENCY <- 0.33

ELECTRICITY_INPUT_FUELS<- c("biomass", "coal", "gas", "refined liquids")

energy.CLIMATE_NORMAL_YEARS <- 1981:2000
energy.RSRC_FUELS <- c("coal", "gas", "refined liquids")

# Assumed base year heat price, used for calculating adjustment to non-energy costs of electricity technologies with secondary output of heat
# in units of 1975$/EJ
energy.HEAT_PRICE <- 3.2
energy.GAS_PRICE <- 2

# below come from ENERGY_ASSUMPTIONS/A_ccs_data.R
energy.DIGITS_EFFICIENCY <- 3
energy.DIGITS_COST <- 4
energy.DIGITS_REMOVE.FRACTION <- 2
energy.CO2.STORAGE.MARKET <- "carbon-storage"

# Digits for rounding into XMLs
energy.DIGITS_CALOUTPUT <- 7
energy.DIGITS_COEFFICIENT <- 7
energy.DIGITS_COST <- 4
energy.DIGITS_EFFICIENCY <- 3
energy.DIGITS_SHRWT <- 4
# Conversion constants ======================================================================
# The naming convention is CONV_(FROM-UNIT)_(TO-UNIT).

# Numeric (unitless)
CONV_BIL_MIL <- 1000
CONV_MIL_BIL <- 1 / CONV_BIL_MIL
CONV_BIL_THOUS <- 1e6
CONV_THOUS_BIL <- 1 / CONV_BIL_THOUS
CONV_MIL_THOUS <- 1000
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
CONV_TONNE_GJ_DISTILLATE <- 42.91 # tons to GJ distillate

# Time
CONV_YEAR_HOURS <- 24 * 365.25
CONV_DAYS_YEAR <- 1 / 365.25

# Energy
CONV_MWH_GJ <- 3.6 # Megawatt hours to Gigajoules
CONV_GWH_EJ <- 3.6e-6
CONV_KWH_GJ <- 3.6e-3
CONV_GJ_EJ <- 1e-9
CONV_BBLD_EJYR <- 6.119 * 365.25 * 1e-3 # billion barrels a day to EJ per year
CONV_KBTU_EJ <- 1.0551e-12
CONV_TBTU_EJ <- 0.0010551

# Other
CONV_MCAL_PCAL <- 1e-9
CONV_M3_BM3 <- 1e-09 # Cubic meters (m3) to billion cubic meters (bm3)
CONV_MILLION_M3_KM3 <- 1e-03
CONV_M2_ACR <- 0.0002471058
CONV_HA_M2 <- 1e4 # ha to m2
CONV_BM2_M2 <- 1e9
CONV_MILFT2_M2 <- 92900
CONV_FT2_M2 <- 0.0929

# Driver constants ======================================================================

driver.MAKE <- "MAKE"
driver.DECLARE_OUTPUTS <- "DECLARE_OUTPUTS"
driver.DECLARE_INPUTS <- "DECLARE_INPUTS"


# Modeltime constants ======================================================================

# MAGICC model assumptions
modeltime.MAGICC_LAST_HISTORICAL_YEAR <- 2005
modeltime.MAGICC_BC_UNIT_FORCING <- 0
modeltime.MAGICC_DEFAULT_EMISS_FILE <- "../input/magicc/Historical Emissions/Default Emissions Module/Hist_to_2008_Annual.csv"
modeltime.MAGICC_C_START_YEAR <- 1705

# Hector model assumptions
modeltime.HECTOR_END_YEAR <- 2100
modeltime.HECTOR_EMISSIONS_YEAR <- 2005
modeltime.HECTOR_INI_FILE <- "../input/climate/hector-gcam.ini"


# Socioeconomics constants ======================================================================

# Population years - note that these sequences shouldn't have any overlap,
# and should contain all historical years used by other modules
socioeconomics.MADDISON_HISTORICAL_YEARS <- seq(1700, 1900, 50) # Years for which to use Maddison data
socioeconomics.UN_HISTORICAL_YEARS <- c(1950, 1971:2010) # Years for which to use UN data
socioeconomics.FINAL_HIST_YEAR <- 2010 # Final historical year,
# NOTE that we use this because it's also the first year of the SSP database.
# Using a different year if the final historical year in the UN historical years changes, this would result in
# different SSP projections. (Because the SSP scenarios begin to diverge in 2015, so we'd have to reconsider how
# we do the SSP scenarios if we update to UN 2015 population.)
socioeconomics.BASE_POP_SCEN <- "SSP2" # These are both being used in the data system by different files.
BASE_POP_SCENARIO <- "SSP2" # These are both being used in the data system by different files.
BASE_GDP_SCENARIO <- "SSP2"

socioeconomics.DEFAULT_INTEREST_RATE <- 0.05
socioeconomics.GDP_DIGITS <- 0
socioeconomics.POP_DIGITS <- 0
socioeconomics.DEFAULT_LABORFORCE <- 0.5
socioeconomics.LABOR_PRODUCTIVITY_DIGITS <- 5


# Water constants ======================================================================

IRRIGATION                          <- "Irrigation"
MAPPED_WATER_TYPES                  <- c("water consumption", "water withdrawals")
MAPPED_WATER_TYPES_SHORT            <- c("C", "W")
names(MAPPED_WATER_TYPES_SHORT)     <- MAPPED_WATER_TYPES
DEFAULT_UNLIMITED_WATER_PRICE       <- 0
DEFAULT_UNLIMITED_WITHD_WATER_PRICE <- 0.001
DEFAULT_UNLIMITED_IRR_WATER_PRICE   <- 0.001 # (Units: 1975$/m3)
WATER_UNITS_QUANTITY                <- "km^3"
WATER_UNITS_PRICE                   <- "1975$/m^3"
AG_ONLY_WATER_TYPES                 <- "biophysical water consumption"
COOLING_SYSTEM_LOGIT 				        <- -5    # Cooling system logit (Unitless)
DRY_COOLING_EFF_ADJ 				        <- 0.95  # Dry cooling efficiency adjustment (Unitless)
COOLING_SYSTEM_FCR                  <- 0.15  # Cooling system fixed charge rate (Unitless)
COOLING_SYSTEM_CAPACITY_FACTOR      <- 0.6   # Cooling system capacity factor (Unitless)

# Emissions constants ======================================================================

emissions.DIGITS_EMISSIONS <- 10
emissions.CTRL_BASE_YEAR   <- 1975  # Year to read in pollution controls
emissions.MODEL_BASE_YEARS <- BASE_YEARS[BASE_YEARS < 2008]
emissions.FINAL_EMISS_YEAR <- min(max(BASE_YEARS), 2005)

emissions.EPA_HISTORICAL_YEARS <- 1971:2002
emissions.TST_TO_TG            <- 0.000907 # Thousand short tons to Tg
emissions.NH3_HISTORICAL_YEARS <- 1990:2002
emissions.NH3_EXTRA_YEARS <- 1971:1989
emissions.EDGAR_YEARS <- 1971:2008
emissions.EDGAR_HISTORICAL <- 1971:2008
emissions.EDGAR_F_GASSES_YEARS <- 1970:2000
emissions.EPA_MACC_YEAR <- 2030  # Must be either 2020 or 2030
emissions.MAC_TAXES <- c(0, 5, 10, 15, 32, 66, 129, 243, 486, 1093) # Range of costs in 1990 USD
emissions.CONV_C_CO2 <- 44 / 12 # Convert Carbon to CO2
emissions.DEFOREST_COEF_YEARS <- c(2000, 2005)
emissions.PFCS <- c("CF4", "C2F6", "SF6")
emissions.HFC_MODEL_BASE_YEARS <- c(1975, 1990, 2005, 2010)
emissions.F_GAS_UNITS <- "Gg"
emissions.GAINS_BASE_YEAR <- 2005
emissions.GAINS_YEARS <- c(2010, 2020, 2030)
emissions.LOW_PCGDP <- 2.75  # thousand 1990 USD
emissions.COAL_SO2_THRESHOLD <- 0.1 # Tg/EJ (here referring to Tg SO2 per EJ of coal electricity)

emissions.EPA_MACC_YEAR        <- 2030  # Must be either 2020 or 2030
emissions.MAC_TAXES            <- c(0, 5, 10, 15, 32, 66, 129, 243, 486, 1093) # Range of costs in 1990 USD
emissions.CONV_C_CO2           <- 44 / 12 # Convert Carbon to CO2
emissions.DEFOREST_COEF_YEARS  <- c(2000, 2005)
emissions.AGR_SECTORS          <- c("rice", "fertilizer", "soil")
emissions.AGR_GASES            <- c("CH4_AGR", "N2O_AGR", "NH3_AGR", "NOx_AGR")
emissions.AG_MACC_GHG_NAMES    <- c("CH4_AGR", "N2O_AGR")
emissions.GHG_NAMES            <- c("CH4", "N2O")
emissions.USE_GV_MAC           <- 1
emissions.NONGHG_GASES         <- c("SO2", "NOx", "CO", "NMVOC", "NH3")
emissions.EDGAR_YEARS_PLUS     <- 1970:2008

# GCAM-USA constants ======================================================================

gcamusa.STATES <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA",
                    "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR",
                    "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# Uncomment these lines to run under 'timeshift' conditions
# HISTORICAL_YEARS <- 1971:2005       # normally 1971:2010
# FUTURE_YEARS <- seq(2010, 2100, 5)  # normally seq(2015, 2100, 5)
# BASE_YEARS <- c(1975, 1990, 2005)   # normally (1975, 1990, 2005, 2010)
# MODEL_YEARS <- c(BASE_YEARS, FUTURE_YEARS)
