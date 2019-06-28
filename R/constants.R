# General behavior constants ======================================================================

OUTPUTS_DIR              <- "outputs/"
XML_DIR                  <- "xml/"
COMMENT_CHAR             <- "#"
UNDER_TIMESHIFT          <- FALSE
YEAR_PATTERN             <- "^(1|2)[0-9]{3}$"   # a 1 or 2 followed by three digits, and nothing else
LOGIT_TYPE_COLNAME        <- "logit.type"        # will be removed by test code before old-new comparison


# Flags ======================================================================

FLAG_INPUT_DATA      <- "FLAG_INPUT_DATA"       # input data, don't output
FLAG_NO_OUTPUT       <- "FLAG_NO_OUTPUT"        # don't output
FLAG_NO_TEST         <- "FLAG_NO_TEST"          # don't test
FLAG_SUM_TEST        <- "FLAG_SUM_TEST"         # use less-restrictive sum test
FLAG_XML             <- "FLAG_XML"              # xml data


# Time constants ======================================================================

# Historical years for level 1 data processing. All chunks that produce historical data
# for model calibration are required to produce annual data covering this entire span.
HISTORICAL_YEARS        <- 1971:2010
# Future years for level 1 data processing, for the few chunks that
# produce future data (e.g., population projections)
FUTURE_YEARS            <- 2011:2100
# Calibrated periods in the model. Only level 2 chunks should reference these
MODEL_BASE_YEARS        <- c(1975, 1990, 2005, 2010)
# Future (not calibrated) model periods. Only level 2 chunks should reference these
MODEL_FUTURE_YEARS      <- seq(2015, 2100, 5)
MODEL_YEARS             <- c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)


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
CONV_BIL_THOUS  <- 1e6
CONV_MIL_BIL    <- 1 / CONV_BIL_MIL
CONV_MIL_THOUS  <- 1000
CONV_ONES_THOUS <- 0.001
CONV_THOUS_BIL  <- 1 / CONV_BIL_THOUS

# Mass
CONV_BBL_TONNE_DISTILLATE <- 1 / 7.46 # barrels to tons distillate
CONV_BBL_TONNE_RFO  <- 1 / 6.66       # barrels to tons residual fuel oil
CONV_G_KG           <- 1e-3           # kilograms to grams
CONV_GG_TG          <- 0.001          # gigagrams to tegagrams
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
CONV_DAYS_YEAR  <- 1 / 365.25
CONV_YEAR_HOURS <- 24 * 365.25

# Energy
CONV_BTU_KJ    <- 1.0551
CONV_GJ_EJ     <- 1e-9
CONV_EJ_GJ     <- 1 / CONV_GJ_EJ
CONV_GWH_EJ    <- 3.6e-6
CONV_KBTU_EJ   <- 1.0551e-12            # KiloBTU to EJ
CONV_KWH_GJ    <- 3.6e-3
CONV_MBLD_EJYR <- 6.119 * 365.25 * 1e-3 # million barrels a day to EJ per year
CONV_MJ_BTU    <- 947.777
CONV_MWH_GJ    <- 3.6                   # Megawatt hours to Gigajoules
CONV_TBTU_EJ   <- 0.0010551             # TeraBTU to EJ
CONV_TWH_EJ    <- 3.6e-3

# Other
CONV_BM2_M2         <- 1e9
CONV_FT2_M2         <- 0.0929        # Square feet to square meters
CONV_HA_M2          <- 1e4           # ha to m2
CONV_M2_ACR         <- 0.0002471058
CONV_M3_BM3         <- 1e-09         # Cubic meters (m3) to billion cubic meters (bm3)
CONV_MCAL_PCAL      <- 1e-9
CONV_MILFT2_M2      <- 92900         # Million square feet to square meters
CONV_MILLION_M3_KM3 <- 1e-03


# AgLU constants ======================================================================

# Time
aglu.AGLU_HISTORICAL_YEARS  <- 1971:2010
aglu.BASE_YEAR_IFA          <- 2006      # Base year of International Fertilizer Industry Association (IFA) fertilizer application data KD does this belong here???
aglu.BIO_START_YEAR         <- 2020
aglu.CROSIT_HISTORICAL_YEAR <- 2005      # Historical year from the CROSIT data
aglu.DIET_YEARS             <- seq(max(aglu.AGLU_HISTORICAL_YEARS), 2050, by = 5)
aglu.FAO_HISTORICAL_YEARS   <- 1961:2011
aglu.FAO_LDS_YEARS          <- 1998:2002  # Years for which FAO harvested area data is averaged over for use in the land data system (LDS)
aglu.GTAP_HISTORICAL_YEAR   <- 2000      # Is the year that the GTAP data is based on.
aglu.LAND_HISTORY_YEARS     <- c(1700, 1750, 1800, 1850, 1900, 1950, 1975)
aglu.LAND_COVER_YEARS       <- sort(unique(c(aglu.LAND_HISTORY_YEARS, aglu.AGLU_HISTORICAL_YEARS)))
aglu.MODEL_COST_YEARS       <- 2008:2011
aglu.MODEL_PRICE_YEARS      <- 2008:2011
aglu.PREAGLU_YEARS          <- c(1700, 1750,1800, 1850, 1900, 1950)          # Cropland cover years prior to first aglu historical year to use in climate model component
aglu.SPEC_AG_PROD_YEARS     <- seq(max(aglu.AGLU_HISTORICAL_YEARS), 2050, by = 5) # Specified ag productivity years, KD i think this might need a better comment
aglu.SSP_DEMAND_YEARS       <- seq(2010, 2100, 5) # food demand in the SSPs is calculated at 5-yr intervals

aglu.LAND_TOLERANCE    <- 0.005
aglu.MIN_PROFIT_MARGIN <- 0.15  # Unitless and is used to ensure that Agricultural Costs (units 1975USD/kg) don't lead to profits below a minimum profit margin.

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

# Carbon content of all cellulose
aglu.CCONTENT_CELLULOSE    <- 0.45

# Conversion from peak biomass to average biomass integrated over the course of the year
aglu.CCONV_PEAK_AVG <- 0.5

# Meat price elasticity in the USA
aglu.FOOD_MEAT_P_ELAS_USA <- -0.09

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
aglu.MGMT_YIELD_ADJ <- 0.1       # Yield multiplier that goes from the observed yield to the "high" and "low" yields: observed plus or minus observed times this number.
aglu.HI_PROD_GROWTH_MULT <- 1.5  # Multipliers for high ag prod growth scenarios
aglu.LOW_PROD_GROWTH_MULT <- 0.5 # Multipliers for low ag prod growth scenarios

# AgLU cost constants
aglu.BIO_GRASS_COST_75USD_GJ <- 0.75   # Production costs of biomass (from Patrick Luckow's work)
aglu.BIO_TREE_COST_75USD_GJ  <- 0.67   # Production costs of biomass (from Patrick Luckow's work)
aglu.FERT_COST               <- 363    # Cost of fertlizer, 2007$ per ton NH3
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
aglu.MGMT_LOGIT_EXP  <- 0.5
aglu.MGMT_LOGIT_TYPE <- "absolute-cost-logit"

# XML-related constants
aglu.CROP_DELIMITER       <- "_"  # delimiter between (some) crop names such as Root_Tuber, biomass_grass, biomass_tree
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
energy.UCD_EN_YEAR                <- 2005        # UCD transporctation year to use to compute shares for allocation of energy to mode/technology/fuel within category/fuel
energy.WIND.BASE.COST.YEAR        <- 2005        # Base cost year for wind, used in capacity factor calculations

# Constant to select SSP database to use for transportation UCD
energy.TRN_SSP <- "CORE"

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

# Digits for rounding into XMLs
energy.DIGITS_CALOUTPUT        <- 7
energy.DIGITS_CALPRODUCTION    <- 7
energy.DIGITS_CAPACITY_FACTOR  <- 2
energy.DIGITS_CAPITAL          <- 0
energy.DIGITS_COEFFICIENT      <- 7
energy.DIGITS_COST             <- 4
energy.DIGITS_CURVE_EXPONENT   <- 3
energy.DIGITS_DEPRESOURCE      <- 1
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

# Policy assumptions for module_energy_L270.limits
energy.NEG_EMISS_POLICY_NAME    <- "negative_emiss_budget"
energy.NEG_EMISS_GDP_BUDGET_PCT <- 0.01 # Max fraction of GDP which may be given to subsidize net negative emissions
energy.NEG_EMISS_MARKT_GLOBAL   <- TRUE # If the negative emissions budget is global (TRUE) or regional (FALSE)
energy.OILFRACT_ELEC            <- 1.0 # Fraction of liquids for feedstocks that must come from oil
energy.OILFRACT_FEEDSTOCKS      <- 0.8 # Fraction of liquids for oil electricity that must come from oil


# Socioeconomics constants ======================================================================

# Population years - note that these sequences shouldn't have any overlap,
# and should contain all historical years used by other modules
socioeconomics.MADDISON_HISTORICAL_YEARS <- seq(1700, 1900, 50) # Years for which to use Maddison data
socioeconomics.UN_HISTORICAL_YEARS       <- c(1950, 1971:2010)  # Years for which to use UN data

# Final historical year, we use this because it's also the first year of the SSP database.
# Using a different year if the final historical year in the UN historical years changes, this would result in
# different SSP projections. (Because the SSP scenarios begin to diverge in 2015, so we'd have to reconsider how
# we do the SSP scenarios if we update to UN 2015 population.)
socioeconomics.FINAL_HIST_YEAR <- 2010

# Sets the years during which the IMF projections are used over-riding the default (generally SSP) assumptions.
socioeconomics.IMF_GDP_YEARS <- 2010:2020

socioeconomics.BASE_POP_SCEN         <- "SSP2"
socioeconomics.BASE_GDP_SCENARIO     <- "SSP2"
socioeconomics.DEFAULT_INTEREST_RATE <- 0.05

# Digits for rounding into XMLs
socioeconomics.DEFAULT_LABORFORCE        <- 0.5
socioeconomics.GDP_DIGITS                <- 0
socioeconomics.LABOR_PRODUCTIVITY_DIGITS <- 5
socioeconomics.POP_DIGITS                <- 0


# Water constants ======================================================================

water.AG_ONLY_WATER_TYPES                 <- "biophysical water consumption"
water.COOLING_SYSTEM_CAPACITY_FACTOR      <- 0.6   # Cooling system capacity factor (Unitless)
water.COOLING_SYSTEM_FCR                  <- 0.15  # Cooling system fixed charge rate (Unitless)
water.COOLING_SYSTEM_LOGIT 				        <- -5    # Cooling system logit (Unitless)
water.DEFAULT_UNLIMITED_IRR_WATER_PRICE   <- 0.001 # (Units: 1975$/m3)
water.DEFAULT_UNLIMITED_WATER_PRICE       <- 0
water.DEFAULT_UNLIMITED_WITHD_WATER_PRICE <- 0.001
water.DRY_COOLING_EFF_ADJ 				        <- 0.95  # Dry cooling efficiency adjustment (Unitless)
water.IRRIGATION                          <- "Irrigation"
water.MAPPED_WATER_TYPES                  <- c("water consumption", "water withdrawals")
water.MAPPED_WATER_TYPES_SHORT            <- c("C", "W")
names(water.MAPPED_WATER_TYPES_SHORT)     <- water.MAPPED_WATER_TYPES
water.WATER_UNITS_PRICE                   <- "1975$/m^3"
water.WATER_UNITS_QUANTITY                <- "km^3"
water.DIGITS_MUNI_WATER                   <- 4

# GCAM intermediate sectors for which Vassolo + Doll assessed manufacturing water demands. In the paper, they indicate
# chemicals, pulp and paper, pig iron, sugar, beer, cloth, cement, and crude steel. some industrial mfg does take place
# in energy transformation (charcoal, pig iron), so we'll leave that one in.
water.GCAM_MFG_SECTORS_VASSOLO <- c("in_industry_general", "net_industry_energy transformation")

# GCAM intermediate fuels used for extrapolating manufacturing water use from one base year to all base years.
water.GCAM_MFG_FUELS_EFW <- c("electricity")

# the maximum portion of aquastat industrial (mfg + elec) water withdrawals that is allowed to be assigned to
# manufacturing. Used to set a cap on derived manufacturing water withdrawals
water.MAX_MFG_FRAC_OF_IND <- 0.85

# Emissions constants ======================================================================

# Time
emissions.CTRL_BASE_YEAR          <- 1975                # Year to read in pollution controls
emissions.DEFOREST_COEF_YEARS     <- c(2000, 2005)
emissions.EDGAR_HISTORICAL        <- 1971:2008
emissions.EDGAR_YEARS             <- 1971:2008
emissions.EDGAR_YEARS_PLUS        <- 1970:2008
emissions.EPA_HISTORICAL_YEARS    <- 1971:2002
emissions.EPA_MACC_YEAR           <- 2030                # Must be either 2020 or 2030
emissions.FINAL_EMISS_YEAR        <- min(max(MODEL_BASE_YEARS), 2005)
emissions.GAINS_BASE_YEAR         <- 2005
emissions.GAINS_YEARS             <- c(2010, 2020, 2030)
emissions.GHG_CONTROL_READIN_YEAR <- 1975
emissions.HFC_MODEL_BASE_YEARS    <- MODEL_YEARS[ MODEL_YEARS <= 2010] # We don't want this to change in timeshift
emissions.INVENTORY_MATCH_YEAR    <- 2009                # Select year from which to calculate fuel emissions coefficients (2009 is currently the most recent)
emissions.MODEL_BASE_YEARS        <- MODEL_BASE_YEARS[MODEL_BASE_YEARS < 2008]
emissions.NH3_EXTRA_YEARS         <- 1971:1989
emissions.NH3_HISTORICAL_YEARS    <- 1990:2002
emissions.SSP_FUTURE_YEARS        <- MODEL_YEARS[MODEL_YEARS %in% 2010:2100]

# Other emissions constants
emissions.CONV_C_CO2    <- 44 / 12 # Convert Carbon to CO2
emissions.F_GAS_UNITS   <- "Gg"
emissions.TST_TO_TG     <- 0.000907 # Thousand short tons to Tg

emissions.COAL_SO2_THRESHOLD <- 0.1   # Tg/EJ (here referring to Tg SO2 per EJ of coal electricity)
emissions.LOW_PCGDP          <- 2.75  # thousand 1990 USD
emissions.MAC_TAXES          <- c(0, 2, 4, 6, 13, 27, 53, 100, 200, 450, 850, 2000, 3000, 5000) # Range of MAC curve costs to keep to read into GCAM; they are in EPA's units (2010USD_tCO2e)
emissions.MAC_MARKET         <- "CO2" # Default market that MAC curves will look for

emissions.AGR_SECTORS        <- c("rice", "fertilizer", "soil")
emissions.AGR_GASES          <- c("CH4_AGR", "N2O_AGR", "NH3_AGR", "NOx_AGR")
emissions.AG_MACC_GHG_NAMES  <- c("CH4_AGR", "N2O_AGR")
emissions.GHG_NAMES          <- c("CH4", "N2O")
emissions.NONGHG_GASES       <- c("SO2", "NOx", "CO", "NMVOC", "NH3")
emissions.PFCS               <- c("CF4", "C2F6", "SF6")
emissions.TRN_INTL_SECTORS   <- c("trn_intl_ship", "trn_intl_air")

emissions.USE_GV_MAC           <- 1
emissions.USE_GCAM3_CCOEFS     <- 1 # Select whether to use GCAM3 fuel carbon coefficients
emissions.USE_GLOBAL_CCOEFS    <- 1 # Select whether to use global average carbon coefficients on fuels, or region-specific carbon coefficients

# Digits for rounding into XMLs
emissions.DIGITS_CO2COEF   <- 1
emissions.DIGITS_EMISSIONS <- 10


# GCAM-USA constants ======================================================================

# GCAM-USA time
gcamusa.WIND_BASE_COST_YEAR <- 2005

# GCAM-USA states
gcamusa.STATES <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA",
                    "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR",
                    "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")


# GCAM-USA default constants
gcamusa.DEFAULT_COEFFICIENT <- 1
gcamusa.DEFAULT_LOGIT_TYPE  <- NA  # default logit type
gcamusa.DEFAULT_LOGITEXP    <- -3
gcamusa.DEFAULT_MARKET      <- gcam.USA_REGION
gcamusa.DEFAULT_SHAREWEIGHT <- 1

# Logit exponent regulating competition between different grid regions in USA electricity market
# (single market approach only)
gcamusa.GRID_REGION_LOGIT      <- -6

gcamusa.GRID_REGION_LOGIT_TYPE <- "relative-cost-logit"

gcamusa.GEOTHERMAL_DEFAULT_EFFICIENCY <- 0.1

gcamusa.ELECT_TD_SECTORS  <- c("elect_td_bld", "elect_td_ind", "elect_td_trn")

# Indicate whether to resolve electricity demands at the level of the nation or the grid regions
gcamusa.USE_REGIONAL_ELEC_MARKETS <- TRUE

# Indicate whether to use regional as opposed to national fuel markets (FALSE = national markets)
gcamusa.USE_REGIONAL_FUEL_MARKETS  <- TRUE

# GCAM-USA fertlizer constants
gcamusa.FERT_LOGIT_EXP  <- -3             # Define default logit expoent used in the fertlizer subsector
gcamusa.FERT_LOGIT_TYPE <- NA
gcamusa.FERT_NAME       <- "N fertilizer" # Define GCAM-USA category name of fertilizer

# Fuels whose markets will be modeled at the level of the FERC regions, with prices calibrated
gcamusa.REGIONAL_FUEL_MARKETS <- c("regional coal", "delivered coal", "wholesale gas", "delivered gas",
                                   "refined liquids industrial", "refined liquids enduse")


# Resources that will be modeled at the state level
gcamusa.STATE_RENEWABLE_RESOURCES <- c("distributed_solar", "geothermal", "onshore wind resource")
gcamusa.STATE_UNLIMITED_RESOURCES <- c("global solar resource", "limestone")

# Define sector(s) used in L222.en_transformation_USA
# The supplysector and subsector structure in these sectors are retained
gcamusa.SECTOR_EN_NAMES <- "refining"

# Degree day norms
gcamusa.BASE_HDD_USA <- 4524 # https://www.eia.gov/totalenergy/data/annual/showtext.php?t=ptb0107
gcamusa.BASE_CDD_USA <- 1215 # https://www.eia.gov/totalenergy/data/annual/showtext.php?t=ptb010

gcamusa.GAS_ADJ_THRESH      <- 5

# Some xml delimiter
gcamusa.STATE_SUBSECTOR_DELIMITER <- " "

# Number of digits for model input data
gcamusa.DIGITS_CALOUTPUT          <- 7    # production
gcamusa.DIGITS_COST               <- 4
gcamuse.DIGITS_DEPRESOURCE        <- 1
gcamusa.EFFICIENCY_PARTITION_YEAR <- 2005
gcamusa.DIGITS_TRNUSA_DEFAULT     <- 1    # Reduce rounding in detailed USA transport for compatability with model


# Time shift conditions ======================================================================
# Uncomment these lines to run under 'timeshift' conditions
# HISTORICAL_YEARS <- 1971:2005       # normally 1971:2010
# MODEL_FUTURE_YEARS <- seq(2010, 2100, 5)  # normally seq(2015, 2100, 5)
# MODEL_BASE_YEARS <- c(1975, 1990, 2005)   # normally (1975, 1990, 2005, 2010)
# MODEL_YEARS <- c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)
