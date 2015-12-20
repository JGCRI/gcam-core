
#Electricity generation fuels whose calibrated quantities in the IEA energy balances are used
electricity_input_fuels <- c( "biomass", "coal", "gas", "refined liquids" )

#Assumed base year heat price, used for calculating adjustment to non-energy costs of electricity technologies with secondary output of heat
heat_price <- 3.2
gas_price <- 2

digits_capital <- 0
digits_OM <- 2
digits_capacity_factor <- 2

#Set a default electric efficiency, (a) for regions where the IEA has input but no output, and
#  (b) for setting a floor below which heat plant costs are modified
default_electric_efficiency <- 0.33

#Level2 data names specific to the electric sector
#Note - level2 data names at the technology level are held in the generic level2_data_names folder
names_ElecReserve <- c( "region", "supplysector", "electricity.reserve.margin", "average.grid.capacity.factor" )

wind_base_cost_year <- 2005

