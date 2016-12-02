
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
