
digits_floorspace <- 3 # unit = billion square meters
digits_hddcdd <- 0 # unit = degree F days
digits_satiation_adder <- 9 #unit = million square meters per person, so the rounding has to be high
discount_rate_bld <- 0.1
bld_frac_of_income <- 0.2 # portion of income spent on housing. not currently used.

#Set which years to use to calculate climate normals (HDD and CDD) in each region
climate_normal_years <- 1981:2000
X_climate_normal_years <- paste0( "X", climate_normal_years )
InternalGainsScalar_USA_h <- -930
InternalGainsScalar_USA_c <- 350

floor.to.surface.ratio <- 5.5

#BS parameter - "impedance". used to derive "satiation adders".
impedance_level <- 10.5
satiation_year <- 2005
X_satiation_year <- paste0( "X", satiation_year )

## Level2 data names for the model interface headers
#Consumers
names_BldConsumers <- c( "region", "gcam.consumer" )
names_BldNodes <- c( "region", "gcam.consumer", "nodeInput", "building.node.input" )
names_PriceExp_IntGains <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "price.exp.year.fillout", "price.exponent",
                             "internal.gains.market.name", "internal.gains.unit")
names_Floorspace <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "year", "base.building.size" )
names_DemandFunction_flsp <- c("region", "gcam.consumer", "nodeInput", "prodDmdFnType" )
names_DemandFunction_serv <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "prodDmdFnType" )
names_Satiation_flsp <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "satiation.level" )
names_SatiationAdder <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "satiation.adder" )
names_ShellConductance <- c( names_BldNodes, "year", "shell.conductance", "shell.year", "floor.to.surface.ratio" )
names_GenericServiceSatiation <- c( names_BldNodes, "building.service.input", "satiation.level" )
names_ThermalServiceSatiation <- c( names_BldNodes, "thermal.building.service.input", "satiation.level" )
names_HDDCDD <- c( names_BldNodes, "thermal.building.service.input", "year", "degree.days" )
names_Intgains_scalar <- c( names_BldNodes, "thermal.building.service.input", "internal.gains.scalar")					

