
#Resource assumptions
rsrc_fuels <- c( "coal", "gas", "refined liquids" )
digits_calproduction <- 7
digits_depresource <- 1
digits_maxSubResource <- 5
digits_mid.price <- 3
digits_curve.exponent <- 3
digits_gdpSupplyElast <- 3
digits_capacity.factor <- 2

#Level2 data names

#Multipliers on extraction cost for SSP scenarios - note "hi" indicates high ccs not high cost
hi_ccs_cost_mult <- 0.8
lo_ccs_cost_mult <- 3
lowest_ccs_cost_mult <- 10

#GDP per capita thressholds for SSP4 region groupings
hi_growth_pcgdp <- 12.275
lo_growth_pcgdp <- 2.75