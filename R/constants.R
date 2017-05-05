
# ======================================================================
# General behavior constants
OUTPUTS_DIR  <- "outputs/"
COMMENT_CHAR <- "#"
OLD_DATA_SYSTEM_BEHAVIOR <- TRUE

# ======================================================================
# Flags used by chunks
FLAG_INPUT_DATA <- "INPUT_DATA"
FLAG_LONG_YEAR_FORM  <- "FLAG_LONG_YEAR_FORM"
FLAG_NO_OUTPUT  <- "NO_OUTPUT"
FLAG_NO_XYEAR   <- "NO_XYEAR"
FLAG_NO_TEST    <- "NO_TEST"
FLAG_SUM_TEST   <- "FLAG_SUM_TEST"
FLAG_PROTECT_FLOAT <- "FLAG_PROTECT_FLOAT"

# ======================================================================
# Time constants
HISTORICAL_YEARS <- 1971:2010
FUTURE_YEARS <- seq(2015, 2100, 5)
IMF_GDP_YEARS <- 2010:2020

# ======================================================================
# GCAM constants
gcam.USA_CODE <- 1

# ======================================================================
# aglu constants
AGLU_HISTORICAL_YEARS <- 1971:2010
FAO_HISTORICAL_YEARS <- 1961:2011
MODEL_PRICE_YEARS <- 2001:2005
LAND_HISTORY_YEARS <- c(1700, 1750, 1800, 1850, 1900, 1950, 1975)
PREAGLU_YEARS <- c(1700, 1750,1800, 1850, 1900, 1950)
aglu.LAND_COVER_YEARS <- sort(unique(c(LAND_HISTORY_YEARS, AGLU_HISTORICAL_YEARS)))
GTAP_HISTORICAL_YEAR <- 2000
CROSIT_HISTORICAL_YEAR <- 2005
SPEC_AG_PROD_YEARS <- seq(2010, 2050, 5) # Specified ag productivity years

# ======================================================================
# socioeconomics constants
BASE_POP_SCENARIO <- "SSP2"
BASE_GDP_SCENARIO <- "SSP2"

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

# Carbon content of all cellulose
aglu.CCONTENT_CELLULOSE <- 0.45

# Minimum and maximum harvested:cropped ratios
MIN_HA_TO_CROPLAND <- 1
# Source: Dalrymple, D.G. 1971, Survey of Multiple Cropping in Less Developed Nations, Foreign Econ. Dev. Serv., U.S. Dep. of Agricul., Washington, D.C.
# Cited in: Monfreda et al. 2008, Farming the Planet: 2., Global Biogeochemical Cycles 22, GB1022, http://dx.doi.org/10.1029/2007GB002947
MAX_HA_TO_CROPLAND <- 3

# ======================================================================
# energy constants

# At present the CO2 emissions inventory from CDIAC stops at 2009
energy.CDIAC_CO2_HISTORICAL_YEARS <- HISTORICAL_YEARS[HISTORICAL_YEARS < 2010]


## ======================================================================
## Conversion constants.  The naming convention is CONV_(FROM-UNIT)_(TO-UNIT).
CONV_BIL_MIL <- 1000
CONV_MIL_BIL <- 0.001
CONV_MIL_THOUS <- 1000
CONV_ONES_THOUS <- 0.001
CONV_TON_MEGATON <- 1e-6
CONV_T_KG <- 1e3
CONV_T_METRIC_SHORT <- 1000/908  # Ratio between metric ton and short ton
CONV_MCAL_PCAL <- 1e-9
CONV_HA_BM2 <- 1e-5
CONV_THA_KGM2 <- 0.1   # tons C/ha -> kg C/m2
CONV_GG_TG <- 0.001
CONV_TST_TG <- 0.000907 # thousand short tons to Tg

# Cubic meters (m3) to billion cubic meters (bm3)
CONV_M3_BM3 <- 1e-09
CONV_MILLION_M3_KM3 <- 1e-03


# ======================================================================
# Driver constants
driver.MAKE <- "MAKE"
driver.DECLARE_OUTPUTS <- "DECLARE_OUTPUTS"
driver.DECLARE_INPUTS <- "DECLARE_INPUTS"

# ======================================================================
# Column names (?)
GCAM_REGION_ID <- "GCAM_region_ID"

# ======================================================================
# Modeltime constants

# Model base and future years
modeltime.BASE_YEARS <- c(1975, 1990, 2005, 2010)
modeltime.FUTURE_YEARS <- seq(2015, 2100, by = 5)
modeltime.YEARS <- c(modeltime.BASE_YEARS, modeltime.FUTURE_YEARS)

# MAGICC model assumptions
modeltime.MAGICC_LAST_HISTORICAL_YEAR <- 2005
modeltime.MAGICC_BC_UNIT_FORCING <- 0
modeltime.MAGICC_C_START_YEAR <- 1705

# Hector model assumptions
modeltime.HECTOR_END_YEAR <- 2100
modeltime.HECTOR_EMISSIONS_YEAR <- 2005
modeltime.HECTOR_INI_FILE <- "../input/climate/hector-gcam.ini"

# ======================================================================
#Set a default electric efficiency
DEFAULT_ELECTRIC_EFFICIENCY <- 0.33

# ======================================================================
#Set a default electric efficiency
ELECTRICITY_INPUT_FUELS<- c( "biomass", "coal", "gas", "refined liquids" )

# ======================================================================
#Create X_HISTORICAL_YEARS
#X_HISTORICAL_YEARS <- paste( "X", HISTORICAL_YEARS, sep = "" )

# socioeconomics constants

# Population years - note that these sequences shouldn't have any overlap,
# and should contain all historical years used by other modules
socioeconomics.MADDISON_HISTORICAL_YEARS <- seq(1700, 1900, 50) # Years for which to use Maddison data
socioeconomics.UN_HISTORICAL_YEARS <- c(1950, 1971:2010) # Years for which to use UN data

socioeconomics.BASE_POP_SCEN <- "SSP2"

# ======================================================================
# water constants

IRRIGATION <- "Irrigation"
MAPPED_WATER_TYPES <- c("water consumption", "water withdrawals")
MAPPED_WATER_TYPES_SHORT <- c("C", "W")
names(MAPPED_WATER_TYPES_SHORT) <- MAPPED_WATER_TYPES
DEFAULT_UNLIMITED_WATER_PRICE <- 0
DEFAULT_UNLIMITED_WITHD_WATER_PRICE <- 0.001

# ======================================================================
# emissions constants

emissions.EPA_HISTORICAL_YEARS <- 1971:2002
emissions.TST_TO_TG <- 0.000907 # Conversion from thousand short tons to Tg
emissions.NH3_HISTORICAL_YEARS <- 1990:2002
emissions.NH3_EXTRA_YEARS <- 1971:1989
emissions.EDGAR_YEARS <- 1971:2008
emissions.EPA_MACC_YEAR <- 2030  # Must be either 2020 or 2030
emissions.MAC_TAXES <- c( 0, 5, 10, 15, 32, 66, 129, 243, 486, 1093 ) # Range of costs in 1990 USD
emissions.CONV_C_CO2 <- 44/12 # Convert Carbon to CO2
