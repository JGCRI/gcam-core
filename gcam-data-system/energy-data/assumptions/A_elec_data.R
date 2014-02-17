
#Electricity generation fuels whose calibrated quantities in the IEA energy balances are used
electricity_input_fuels <- c( "biomass", "coal", "gas", "refined liquids" )

#Assumed base year heat price, used for calculating adjustment to non-energy costs of electricity technologies with secondary output of heat
heat_price <- 2.8

digits_capital <- 0
digits_OM <- 2

default_electric_efficiency <- 0.3     #Used in region x fuel combinations where the IEA data has fuel input but not output

#Level2 data names specific to the electric sector
#Note - level2 data names at the technology level are held in the generic level2_data_names folder
names_ElecReserve <- c( "region", "supplysector", "electricity.reserve.margin", "average.grid.capacity.factor" )



