
# ======================================================================
# Behavior constants
OUTPUTS_DIR <- "outputs/"
COMMENT_CHAR <- "#"
FLAG_INPUT_DATA <- "INPUT_DATA"
FLAG_LONG_NO_X_FORM <- "LONG_NO_X_FORM"
FLAG_NO_OUTPUT <- "NO_OUTPUT"
FLAG_NO_X_FORM <- "NO_X_FORM"

# ======================================================================
# Time constants
HISTORICAL_YEARS <- 1971:2010
FUTURE_YEARS <- seq( 2015, 2100, 5 )

# ======================================================================
# aglu constants
AGLU_HISTORICAL_YEARS <- 1971:2010
FAO_HISTORICAL_YEARS <- 1961:2011

# ======================================================================
# Conversion constants
CONV_BIL_MIL <- 1000
CONV_MIL_THOUS <- 1000
CONV_ONES_THOUS <- 0.001
CONV_1990_2005_USD <- 1.383

# ======================================================================
# Driver constants
driver.MAKE <- "MAKE"
driver.DECLARE_OUTPUTS <- "DECLARE_OUTPUTS"
driver.DECLARE_INPUTS <- "DECLARE_INPUTS"


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
