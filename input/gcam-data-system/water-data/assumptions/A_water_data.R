# The units for water prices and quantities as input into the model
water_units_price <- "1975$/m^3"
water_units_quantity <- "km^3"

# Names of the water types
water_type <- "water_type"
all_water_types <- c( "water consumption", "water withdrawals", "seawater", "biophysical water consumption" )
mapped_water_types <- c( "water consumption", "water withdrawals" )
mapped_water_types_short <- c( "C", "W" )
water_C <- all_water_types[1]
water_W <- all_water_types[2]

# Water sector names
water_sector <- "water.sector"
all_water_sectors <- c( "Irrigation", "Municipal", "Electricity", "Livestock", "Manufacturing", "Mining" )
irr_water_sector <- all_water_sectors[1]
nonirr_water_sectors <- all_water_sectors[ 2:length(all_water_sectors) ]

S_F_tech_cool <- c( "sector", "fuel", "technology", "cooling_system", water_type )

# Default unlimited water price for basins/water_type with out an explicit assumption (Units: 1975$/m3)
DEFAULT_UNLIMITED_WATER_PRICE <- 0
DEFAULT_UNLIMITED_IRR_WATER_PRICE <- 0.001

#Fixed charge rate and capacity factor to use on the cooling systems' capital costs
cooling_system_FCR <- 0.15
cooling_system_capacity_factor <- 0.60

cooling_system_logit <- -5
cooling_system_logit.type <- NA

dry_cooling_eff_adj <- 0.95
