# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of energy data system, please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "LB1322.Fert.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Fertilizer production by region / fuel and fertilizer costs by technology" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_ind_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
IEA_ctry <- readdata( "ENERGY_MAPPINGS", "IEA_ctry" )
IEA_Fert_fuel_data <- readdata( "ENERGY_LEVEL0_DATA", "IEA_Fert_fuel_data" )
H2A_Prod_Tech <- readdata( "ENERGY_LEVEL0_DATA", "H2A_Prod_Tech" )
L142.ag_Fert_Prod_MtN_ctry_Y <- readdata( "AGLU_LEVEL1_DATA", "L142.ag_Fert_Prod_MtN_ctry_Y" )
A10.rsrc_info <- readdata( "ENERGY_ASSUMPTIONS", "A10.rsrc_info" )
A21.globaltech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A21.globaltech_cost" )
A22.globaltech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A22.globaltech_cost" )
L1321.in_EJ_R_indenergy_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1321.in_EJ_R_indenergy_F_Yh" )
L132.in_EJ_R_indfeed_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L132.in_EJ_R_indfeed_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations

#Compute fertilizer production and energy inputs by technology
printlog( "Disaggregating fertilizer production by country / year to production technologies (gas, coal, oil)" )
IEA_Fert_fuel_data.melt <- melt( IEA_Fert_fuel_data, id.vars = "IEA_Fert_reg" )
L1322.IEA_fert_fuel_shares <- subset( IEA_Fert_fuel_data.melt, grepl( "share", variable ) )
L1322.IEA_fert_fuel_shares$fuel <- sub( "share_", "", as.character( L1322.IEA_fert_fuel_shares$variable ) )
#Reset oil to refined liquids
L1322.IEA_fert_fuel_shares$fuel <- sub( "oil", "refined liquids", as.character( L1322.IEA_fert_fuel_shares$fuel ) )
L1322.IEA_fert_fuel_intensities <- subset( IEA_Fert_fuel_data.melt, grepl( "GJtNH3", variable ) )
L1322.IEA_fert_fuel_intensities$fuel <- sub( "_GJtNH3", "", as.character( L1322.IEA_fert_fuel_intensities$variable ) )
L1322.IEA_fert_fuel_intensities$fuel <- sub( "oil", "refined liquids", as.character( L1322.IEA_fert_fuel_intensities$fuel ) )
L1322.IEA_fert_fuel_intensities$intensity_GJkgN <- L1322.IEA_fert_fuel_intensities$value / conv_t_kg / conv_NH3_N

#Melt fertilizer production table to allow fuel shares to be modified over time
L1322.Fert_Prod_MtN_ctry_Y.melt <- melt( L142.ag_Fert_Prod_MtN_ctry_Y, id.vars = "iso", variable.name = Xyr )

#Repeat fertilizer production table by number of fuels and match in the IEA region, fuel share, and intensity for each country
L1322.Fert_Prod_MtN_ctry_F_Y.melt <- repeat_and_add_vector( L1322.Fert_Prod_MtN_ctry_Y.melt, "fuel", c( "coal", "gas", "refined liquids" ) )
L1322.Fert_Prod_MtN_ctry_F_Y.melt$IEA_Fert_reg <- IEA_ctry$IEA_Fert_reg[ match( L1322.Fert_Prod_MtN_ctry_F_Y.melt$iso, IEA_ctry$iso ) ]
L1322.Fert_Prod_MtN_ctry_F_Y.melt$share <- L1322.IEA_fert_fuel_shares$value[
      match( vecpaste( L1322.Fert_Prod_MtN_ctry_F_Y.melt[ c( "IEA_Fert_reg", "fuel" ) ] ),
             vecpaste( L1322.IEA_fert_fuel_shares[ c( "IEA_Fert_reg", "fuel" ) ] ) ) ]

#Switch individual countries' fuel shares according to literature or to make energy balances work at the regional level.
#North Korea: coal in all years (http://www9.ocn.ne.jp/~aslan/dprkeng0409.pdf)
L1322.Fert_Prod_MtN_ctry_F_Y.melt$share[ L1322.Fert_Prod_MtN_ctry_F_Y.melt$iso == "prk"  & L1322.Fert_Prod_MtN_ctry_F_Y.melt$fuel == "coal" ] <- 1
L1322.Fert_Prod_MtN_ctry_F_Y.melt$share[ L1322.Fert_Prod_MtN_ctry_F_Y.melt$iso == "prk"  & L1322.Fert_Prod_MtN_ctry_F_Y.melt$fuel != "coal" ] <- 0

#Multiply total production by fuel-specific shares to calculate fertilizer production by fuel type
L1322.Fert_Prod_MtN_ctry_F_Y.melt$Fert_Prod_MtN <- with( L1322.Fert_Prod_MtN_ctry_F_Y.melt, value * share )

printlog( "Calculating energy inputs to fertilizer production by country and fuel type" )
#Match in the energy intensity by fuel type and IEA fertilizer region
L1322.Fert_Prod_MtN_ctry_F_Y.melt$intensity_GJkgN <- L1322.IEA_fert_fuel_intensities$intensity_GJkgN[
      match( vecpaste( L1322.Fert_Prod_MtN_ctry_F_Y.melt[ c( "IEA_Fert_reg", "fuel" ) ] ),
             vecpaste( L1322.IEA_fert_fuel_intensities[ c( "IEA_Fert_reg", "fuel" ) ] ) ) ]
L1322.Fert_Prod_MtN_ctry_F_Y.melt$in_Fert <- with( L1322.Fert_Prod_MtN_ctry_F_Y.melt, Fert_Prod_MtN * intensity_GJkgN )

printlog( "Checking total energy inputs to fertilizer against industrial energy use for each region")
#First, aggregate by region
L1322.Fert_Prod_MtN_ctry_F_Y.melt[[R]] <- iso_GCAM_regID[[R]][ match( L1322.Fert_Prod_MtN_ctry_F_Y.melt$iso, iso_GCAM_regID$iso ) ]
L1322.Fert_ALL_MtN_R_F_Y <- aggregate( L1322.Fert_Prod_MtN_ctry_F_Y.melt[ "in_Fert" ],
      by=as.list( L1322.Fert_Prod_MtN_ctry_F_Y.melt[ R_F_Xyr ] ), sum )

#Expand this table to all regions x fuels x years in case there are regions that aren't countries in the aglu databases
L1322.Fert_ALL_MtN_R_F_Y <- translate_to_full_table( L1322.Fert_ALL_MtN_R_F_Y,
      R, unique( iso_GCAM_regID[[R]] ),
      "fuel", unique( L1322.Fert_ALL_MtN_R_F_Y$fuel ),
      "Xyear", X_historical_years,
      datacols = "in_Fert" )

#Melt other energy tables (ind energy, ind feedstocks) and match in values
L1322.in_EJ_R_indenergy_F_Yh.melt <- melt( L1321.in_EJ_R_indenergy_F_Yh, id.vars = R_S_F, variable.name = Xyr )
L1322.in_EJ_R_indfeed_F_Yh.melt <- melt( L132.in_EJ_R_indfeed_F_Yh, id.vars = R_S_F, variable.name = Xyr )

L1322.Fert_ALL_MtN_R_F_Y$in_indenergy <- L1322.in_EJ_R_indenergy_F_Yh.melt$value[
      match( vecpaste( L1322.Fert_ALL_MtN_R_F_Y[ R_F_Xyr ] ),
             vecpaste( L1322.in_EJ_R_indenergy_F_Yh.melt[ R_F_Xyr ] ) ) ]
L1322.Fert_ALL_MtN_R_F_Y$in_indfeed <- L1322.in_EJ_R_indfeed_F_Yh.melt$value[
      match( vecpaste( L1322.Fert_ALL_MtN_R_F_Y[ R_F_Xyr ] ),
             vecpaste( L1322.in_EJ_R_indfeed_F_Yh.melt[ R_F_Xyr ] ) ) ]

#Check whether the remaining available industrial energy use is not negative (ind. energy + ind. feedstocks - fert)
L1322.Fert_ALL_MtN_R_F_Y$check <- with( L1322.Fert_ALL_MtN_R_F_Y, in_indenergy + in_indfeed - in_Fert )

printlog( "Modifying fertilizer production shares so that industrial energy/feedstock use does not go negative for any fuels")
#Modify the fuel shares in the country-level fertilizer production tables, and re-compute fertilizer production quantities
L1322.Fert_modify <- subset( L1322.Fert_ALL_MtN_R_F_Y, check < 0 )
L1322.Fert_modify$mult <- 1 - (-1 * L1322.Fert_modify$check / L1322.Fert_modify$in_Fert )

#For matched region/fuel/year, multiply the original shares by the multipliers from above
L1322.Fert_Prod_MtN_ctry_F_Y.melt$mult <- L1322.Fert_modify$mult[
      match( vecpaste( L1322.Fert_Prod_MtN_ctry_F_Y.melt[ R_F_Xyr ] ), vecpaste( L1322.Fert_modify[ R_F_Xyr ] ) ) ]
L1322.Fert_Prod_MtN_ctry_F_Y.melt$mult[ is.na( L1322.Fert_Prod_MtN_ctry_F_Y.melt$mult ) ] <- 1
L1322.Fert_Prod_MtN_ctry_F_Y.melt$share_adj <- with( L1322.Fert_Prod_MtN_ctry_F_Y.melt, share * mult )

#For regions whose fuel shares changed, re-assign the production and energy inputs to the oil-based technology
L1322.Fert_Prod_MtN_ctry_F_Y.melt$share_adj[ L1322.Fert_Prod_MtN_ctry_F_Y.melt$fuel == "refined liquids" ] <-
      1 - L1322.Fert_Prod_MtN_ctry_F_Y.melt$share_adj[ L1322.Fert_Prod_MtN_ctry_F_Y.melt$fuel == "coal" ] - 
          L1322.Fert_Prod_MtN_ctry_F_Y.melt$share_adj[ L1322.Fert_Prod_MtN_ctry_F_Y.melt$fuel == "gas" ]

#Re-compute fertilizer production and energy requirements using these new shares
L1322.Fert_Prod_MtN_ctry_F_Y.melt$Fert_Prod_MtN_adj <- with( L1322.Fert_Prod_MtN_ctry_F_Y.melt, value * share_adj )
L1322.Fert_Prod_MtN_ctry_F_Y.melt$in_Fert_adj <- with( L1322.Fert_Prod_MtN_ctry_F_Y.melt, Fert_Prod_MtN_adj * intensity_GJkgN )

printlog( "Re-checking total energy inputs to fertilizer against industrial energy use for each region")
L1322.Fert_ALL_MtN_R_F_Y_adj <- aggregate( L1322.Fert_Prod_MtN_ctry_F_Y.melt[ c( "Fert_Prod_MtN_adj", "in_Fert_adj" ) ],
      by=as.list( L1322.Fert_Prod_MtN_ctry_F_Y.melt[ R_F_Xyr ] ), sum )

#Again need to write out to all region x fuel x year combinations
L1322.Fert_ALL_MtN_R_F_Y_adj <- translate_to_full_table( L1322.Fert_ALL_MtN_R_F_Y_adj,
      R, unique( iso_GCAM_regID[[R]] ),
      "fuel", unique( L1322.Fert_ALL_MtN_R_F_Y_adj$fuel ),
      "Xyear", X_historical_years,
      datacols = c( "Fert_Prod_MtN_adj", "in_Fert_adj" ) )

L1322.Fert_ALL_MtN_R_F_Y_adj[ c( "in_indenergy", "in_indfeed" ) ] <-
      L1322.Fert_ALL_MtN_R_F_Y[ c( "in_indenergy", "in_indfeed" ) ]
L1322.Fert_ALL_MtN_R_F_Y_adj$check <- with( L1322.Fert_ALL_MtN_R_F_Y_adj, in_indenergy + in_indfeed - in_Fert_adj )
L1322.Fert_ALL_MtN_R_F_Y_adj$check[ abs( L1322.Fert_ALL_MtN_R_F_Y_adj$check ) < 1e-6 ] <- 0
if( any( L1322.Fert_ALL_MtN_R_F_Y_adj$check < 0 ) ){
	printlog( "ERROR: the following regions have negative industrial energy consumption after adjustment")
	print( subset( L1322.Fert_ALL_MtN_R_F_Y_adj, check < 0 ) )
}

printlog( "Disaggregating input energy to fertilizer production into energy use (combustion) and feedstocks" )
#The "feedstock" requirement is equal to the energy content of the hydrogen in NH3
# see http://www.iea.org/papers/2009/chemical_petrochemical_sector.pdf
NH3_energy_GJtNH3 <- H_energy_GJtH2 * NH3_H_frac

# Convert from GJ per t NH3 to GJ per kg N
NH3_energy_GJkgN <- NH3_energy_GJtNH3  / conv_t_kg / conv_NH3_N

L1322.Fert_ALL_MtN_R_F_Y_adj$intensity_GJkgN <- with( L1322.Fert_ALL_MtN_R_F_Y_adj, in_Fert_adj / Fert_Prod_MtN_adj )
L1322.Fert_ALL_MtN_R_F_Y_adj$intensity_GJkgN[ is.na( L1322.Fert_ALL_MtN_R_F_Y_adj$intensity_GJkgN ) ] <- 0
L1322.Fert_ALL_MtN_R_F_Y_adj$feedstock_GJkgN <- NH3_energy_GJkgN
L1322.Fert_ALL_MtN_R_F_Y_adj$energy_GJkgN <- L1322.Fert_ALL_MtN_R_F_Y_adj$intensity_GJkgN - NH3_energy_GJkgN

#Calculate first-order estimate of energy and feedstock quantities in the industrial sector
L1322.Fert_ALL_MtN_R_F_Y_adj$indfeed_to_Fert <- with( L1322.Fert_ALL_MtN_R_F_Y_adj, Fert_Prod_MtN_adj * feedstock_GJkgN )
L1322.Fert_ALL_MtN_R_F_Y_adj$indenergy_to_Fert <- with( L1322.Fert_ALL_MtN_R_F_Y_adj, Fert_Prod_MtN_adj * energy_GJkgN )

#Calculate remaining industrial energy and feedstock consumption re-allocating to avoid negative values
L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert <- with( L1322.Fert_ALL_MtN_R_F_Y_adj, in_indenergy - indenergy_to_Fert )
L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert <- with( L1322.Fert_ALL_MtN_R_F_Y_adj, in_indfeed - indfeed_to_Fert )

#This is complicated. This last step pertains to the available energy and feedstock quantities for the industrial sector, in regions
# where either went negative as a result of including the fertilizer industry (using the IEA's shares of production by fuel type, 
# modified so that no regions went negative, and following the IEA's conventions on disaggregation of feedstocks and energy-use.
# We are assuming that for the purposes of re-allocation of energy from the (general) industrial sector to the fertilizer sector, all energy
# is available, whether initially classified as feedstocks or energy-use. This may zero out both energy and feedstocks or both in the
# general industrial sector, causing all industrial demands of natural gas to be for the ammonia industry.
# Note that the quantities are added because one is negative.
L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert < 0 ] <-
      L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert < 0 ] +
      L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert < 0 ]
L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert < 0 ] <- 0

L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert < 0 ] <-
      L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert < 0 ] +
      L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert < 0 ]
L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert < 0 ] <- 0

printlog( "Building tables of fertilizer production by technology, IO coefs, and energy/feedstock inputs to rest of industry")
L1322.Fert_ALL_MtN_R_F_Y_adj$sector <- Fert_name
L1322.Fert_Prod_MtN_R_F_Y <- dcast( L1322.Fert_ALL_MtN_R_F_Y_adj, GCAM_region_ID + sector + fuel ~ Xyear, value.var = "Fert_Prod_MtN_adj" )
L1322.IO_R_Fert_F_Yh <- dcast( L1322.Fert_ALL_MtN_R_F_Y_adj, GCAM_region_ID + sector + fuel ~ Xyear, value.var = "intensity_GJkgN" )
L1322.in_EJ_R_indenergy_Ffert_Yh <- dcast( L1322.Fert_ALL_MtN_R_F_Y_adj, GCAM_region_ID  + fuel ~ Xyear, value.var = "in_indenergy_netFert" )
L1322.in_EJ_R_indfeed_Ffert_Yh <- dcast( L1322.Fert_ALL_MtN_R_F_Y_adj, GCAM_region_ID + fuel ~ Xyear, value.var = "in_indfeed_netFert" )

L1322.in_EJ_R_indenergy_F_Yh <- L1321.in_EJ_R_indenergy_F_Yh
#Need to match this in due to different sorting in the two tables
L1322.in_EJ_R_indenergy_F_Yh[
      vecpaste( L1322.in_EJ_R_indenergy_F_Yh[ R_F ] ) %in% vecpaste( L1322.in_EJ_R_indenergy_Ffert_Yh[ R_F ] ), X_historical_years ] <-
      L1322.in_EJ_R_indenergy_Ffert_Yh[ 
          match( vecpaste( L1322.in_EJ_R_indenergy_F_Yh[
                     vecpaste( L1322.in_EJ_R_indenergy_F_Yh[ R_F ] ) %in% vecpaste( L1322.in_EJ_R_indenergy_Ffert_Yh[ R_F ] ), R_F ] ),
                 vecpaste( L1322.in_EJ_R_indenergy_Ffert_Yh[ R_F ] ) ),
      X_historical_years ]

L1322.in_EJ_R_indfeed_F_Yh <- L132.in_EJ_R_indfeed_F_Yh
L1322.in_EJ_R_indfeed_F_Yh[
      vecpaste( L1322.in_EJ_R_indfeed_F_Yh[ R_F ] ) %in% vecpaste( L1322.in_EJ_R_indfeed_Ffert_Yh[ R_F ] ), X_historical_years ] <-
      L1322.in_EJ_R_indfeed_Ffert_Yh[
          match( vecpaste( L1322.in_EJ_R_indfeed_F_Yh[
                     vecpaste( L1322.in_EJ_R_indfeed_F_Yh[ R_F ] ) %in% vecpaste( L1322.in_EJ_R_indfeed_Ffert_Yh[ R_F ] ), R_F ] ),
                 vecpaste( L1322.in_EJ_R_indfeed_Ffert_Yh[ R_F ] ) ),      
      X_historical_years ]

#Base year costs of fertilizer manufacturing
#Calculate gas cost per kg N
printlog( "Calculating non-energy costs for gas technology as USA market fertilizer price minus GCAM fuel costs" )
#First, calculate the gas price as the sum of resource costs plus intermediate sectoral mark-ups
#2005 is used as the fertilizer base price. Interpolate cost tables to make sure this year is included
A10.rsrc_info <- gcam_interp( A10.rsrc_info, 2005 )
A21.globaltech_cost <- gcam_interp( A21.globaltech_cost, 2005 )
A22.globaltech_cost <- gcam_interp( A22.globaltech_cost, 2005 )

L1322.P_gas_75USDGJ <- sum( A10.rsrc_info$X2005[ A10.rsrc_info$resource == "natural gas" ],
                       A21.globaltech_cost$X2005[ A21.globaltech_cost$technology == "regional natural gas" ],
                       A22.globaltech_cost$X2005[ A22.globaltech_cost$technology == "natural gas" ] )

L1322.IO_GJkgN_Fert_gas <- L1322.IO_R_Fert_F_Yh$X2005[
      L1322.IO_R_Fert_F_Yh$GCAM_region_ID == 1 &
      L1322.IO_R_Fert_F_Yh$fuel == "gas" ]
L1322.Fert_Fuelcost_75USDGJ_gas <- L1322.P_gas_75USDGJ * L1322.IO_GJkgN_Fert_gas

#Convert total NH3 cost (2007$/tNH3) to N cost (1975$/kgN)
Fert_Cost_75USDkgN <- Fert_Cost_07USDtNH3 * conv_2007_1975_USD * conv_kg_t / conv_NH3_N

#Calculate non-fuel cost of natural gas steam reforming (includes delivery charges)
L1322.Fert_NEcost_75USDkgN_gas <- Fert_Cost_75USDkgN - L1322.Fert_Fuelcost_75USDGJ_gas

#Use H2A technology characteristics to derive characteristics of other technologies
printlog( "Deriving costs for gas with CCS, coal, and coal with CCS using H2A model inputs" )
#NOTE: Because our NGSR NEcosts were calculated as a residual from mkt prices, and include delivery costs,
#not using a ratio of costs, but rather an arithmetic adder. H2A costs are in $/kgH; convert to N-equivalent
printlog( "NOTE: Using arithmetic cost adders based on costs of natural gas steam reforming" )
#Gas with CCS
#First, calculate costs in 1975USD per kg N
H2A_Prod_Tech$NEcost_75USDkgN <- H2A_Prod_Tech$NEcost * conv_2005_1975_USD * NH3_H_frac / conv_NH3_N

#Derive costs as the cost of NGSR plus the specified cost adder
L1322.Fert_NEcost_75USDkgN_gasCCS <- L1322.Fert_NEcost_75USDkgN_gas +
      H2A_Prod_Tech$NEcost_75USDkgN[ H2A_Prod_Tech$Technology == "Central Natural Gas Sequestration" ] -
      H2A_Prod_Tech$NEcost_75USDkgN[ H2A_Prod_Tech$Technology == "Central Natural Gas" ]

#Coal
L1322.Fert_NEcost_75USDkgN_coal <- L1322.Fert_NEcost_75USDkgN_gas +
      H2A_Prod_Tech$NEcost_75USDkgN[ H2A_Prod_Tech$Technology == "Central Coal" ] -
      H2A_Prod_Tech$NEcost_75USDkgN[ H2A_Prod_Tech$Technology == "Central Natural Gas" ]

#Coal CCS
L1322.Fert_NEcost_75USDkgN_coalCCS <- L1322.Fert_NEcost_75USDkgN_gas +
      H2A_Prod_Tech$NEcost_75USDkgN[ H2A_Prod_Tech$Technology == "Central Coal Sequestration" ] -
      H2A_Prod_Tech$NEcost_75USDkgN[ H2A_Prod_Tech$Technology == "Central Natural Gas" ]

#Oil
#For oil, the lack of differentiation in oil-derived products means that the fuel costs are too high
# Fertilizer is made from relatively low-cost by-products of oil refining
#Also, the technology is being phased out where it is currently used (primarily India)
# To minimize price distortions from this phase-out, set the NE cost to 0
printlog( "Setting NE costs for oil technology to 0" )
L1322.Fert_NEcost_75USDkgN_oil <- 0

# Build output table with NE costs by technology
L1322.Fert_NEcost_75USDkgN_F <- data.frame(
      fuel = c( "gas", "gas CCS", "coal", "coal CCS", "refined liquids" ),
      NEcost_75USDkgN = c( L1322.Fert_NEcost_75USDkgN_gas, L1322.Fert_NEcost_75USDkgN_gasCCS,
                           L1322.Fert_NEcost_75USDkgN_coal, L1322.Fert_NEcost_75USDkgN_coalCCS,
                           L1322.Fert_NEcost_75USDkgN_oil ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L1322.Fert_Prod_MtN_R_F_Y <- c( "Fertilizer production by GCAM region / fuel / year","MtN" )
comments.L1322.IO_R_Fert_F_Yh <- c( "Fertilizer input-output coefs by GCAM region / fuel (base year techs only) / year","GJ per kg N" )
comments.L1322.in_EJ_R_indenergy_F_Yh <- c( "Adjusted industrial energy use by GCAM region / fuel / year","EJ" )
comments.L1322.in_EJ_R_indfeed_F_Yh <- c( "Adjusted industrial feedstock use by GCAM region / fuel / year","EJ" )
comments.L1322.Fert_NEcost_75USDkgN_F <- c( "Fertilizer non-energy costs by technology","75USD/kgN" )

#write tables as CSV files
writedata( L1322.Fert_Prod_MtN_R_F_Y, domain="ENERGY_LEVEL1_DATA", fn="L1322.Fert_Prod_MtN_R_F_Y", comments=comments.L1322.Fert_Prod_MtN_R_F_Y )
writedata( L1322.IO_R_Fert_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1322.IO_R_Fert_F_Yh", comments=comments.L1322.IO_R_Fert_F_Yh )
writedata( L1322.in_EJ_R_indenergy_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1322.in_EJ_R_indenergy_F_Yh", comments=comments.L1322.in_EJ_R_indenergy_F_Yh )
writedata( L1322.in_EJ_R_indfeed_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1322.in_EJ_R_indfeed_F_Yh", comments=comments.L1322.in_EJ_R_indfeed_F_Yh )
writedata( L1322.Fert_NEcost_75USDkgN_F, domain="ENERGY_LEVEL1_DATA", fn="L1322.Fert_NEcost_75USDkgN_F", comments=comments.L1322.Fert_NEcost_75USDkgN_F )

# Every script should finish with this line
logstop()
