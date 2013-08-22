# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of aglu processing scripts, please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "L1322.Fert.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Fertilizer production by region / fuel and fertilizer costs by technology" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
IEA_ctry <- readdata( "ENERGY_MAPPINGS", "IEA_ctry" )
IEA_Fert_fuel_data <- readdata( "ENERGY_LEVEL0_DATA", "IEA_Fert_fuel_data" )
H2A_Prod_Tech <- readdata( "ENERGY_LEVEL0_DATA", "H2A_Prod_Tech" )
L142.ag_Fert_Prod_MtN_ctry_Y <- readdata( "AGLU_LEVEL1_DATA", "L142.ag_Fert_Prod_MtN_ctry_Y" )
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
L1322.Fert_Prod_MtN_ctry_Y.melt <- melt( L142.ag_Fert_Prod_MtN_ctry_Y, id.vars = "iso", variable_name = Xyr )

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

#Melt other energy tables (ind energy, ind feedstocks) and match in values
L1322.in_EJ_R_indenergy_F_Yh.melt <- melt( L1321.in_EJ_R_indenergy_F_Yh, id.vars = R_S_F, variable_name = Xyr )
L1322.in_EJ_R_indfeed_F_Yh.melt <- melt( L132.in_EJ_R_indfeed_F_Yh, id.vars = R_S_F, variable_name = Xyr )

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

L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert < 0 ] <-
      L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert < 0 ] -
      L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert < 0 ]
L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert < 0 ] <- 0

L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert < 0 ] <-
      L1322.Fert_ALL_MtN_R_F_Y_adj$in_indfeed_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert < 0 ] -
      L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert < 0 ]
L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert[ L1322.Fert_ALL_MtN_R_F_Y_adj$in_indenergy_netFert < 0 ] <- 0

printlog( "Building tables of fertilizer production by technology, IO coefs, and energy/feedstock inputs to rest of industry")
L1322.Fert_ALL_MtN_R_F_Y_adj$sector <- Fert_name
L1322.Fert_Prod_MtN_R_F_Y <- cast( L1322.Fert_ALL_MtN_R_F_Y_adj, GCAM_region_ID + sector + fuel ~ Xyear, value = "Fert_Prod_MtN_adj" )
L1322.IO_R_Fert_F_Yh <- cast( L1322.Fert_ALL_MtN_R_F_Y_adj, GCAM_region_ID + sector + fuel ~ Xyear, value = "intensity_GJkgN" )
L1322.in_EJ_R_indenergy_Ffert_Yh <- cast( L1322.Fert_ALL_MtN_R_F_Y_adj, GCAM_region_ID  + fuel ~ Xyear, value = "in_indenergy_netFert" )
L1322.in_EJ_R_indfeed_Ffert_Yh <- cast( L1322.Fert_ALL_MtN_R_F_Y_adj, GCAM_region_ID + fuel ~ Xyear, value = "in_indfeed_netFert" )

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

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L1322.Fert_Prod_MtN_R_F_Y <- c( "Fertilizer production by GCAM region / fuel / year","MtN" )
comments.L1322.IO_R_Fert_F_Yh <- c( "Fertilizer input-output coefs by GCAM region / fuel (base year techs only) / year","GJ per kg N" )
comments.L1322.in_EJ_R_indenergy_F_Yh <- c( "Adjusted industrial energy use by GCAM region / fuel / year","EJ" )
comments.L1322.in_EJ_R_indfeed_F_Yh <- c( "Adjusted industrial feedstock use by GCAM region / fuel / year","EJ" )

#write tables as CSV files
writedata( L1322.Fert_Prod_MtN_R_F_Y, domain="ENERGY_LEVEL1_DATA", fn="L1322.Fert_Prod_MtN_R_F_Y", comments=comments.L1322.Fert_Prod_MtN_R_F_Y )
writedata( L1322.IO_R_Fert_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1322.IO_R_Fert_F_Yh", comments=comments.L1322.IO_R_Fert_F_Yh )
writedata( L1322.in_EJ_R_indenergy_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1322.in_EJ_R_indenergy_F_Yh", comments=comments.L1322.in_EJ_R_indenergy_F_Yh )
writedata( L1322.in_EJ_R_indfeed_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1322.in_EJ_R_indfeed_F_Yh", comments=comments.L1322.in_EJ_R_indfeed_F_Yh )

# Every script should finish with this line
logstop()
