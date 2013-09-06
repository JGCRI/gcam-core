
if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "L132.industry.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical industrial sector energy consumption (general energy use and feedstocks, not including cogen)" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
enduse_fuel_aggregation <- readdata( "ENERGY_MAPPINGS", "enduse_fuel_aggregation" )
enduse_sector_aggregation <- readdata( "ENERGY_MAPPINGS", "enduse_sector_aggregation" )
L121.in_EJ_R_unoil_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L121.in_EJ_R_unoil_F_Yh" )
L122.in_EJ_R_refining_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L122.in_EJ_R_refining_F_Yh" )
L122.in_EJ_R_gasproc_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L122.in_EJ_R_gasproc_F_Yh" )
L122.out_EJ_R_gasproc_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L122.out_EJ_R_gasproc_F_Yh" )
L123.in_EJ_R_indchp_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.in_EJ_R_indchp_F_Yh" )
L124.in_EJ_R_heat_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L124.in_EJ_R_heat_F_Yh" )
L131.in_EJ_R_Senduse_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L131.in_EJ_R_Senduse_F_Yh" )
L131.share_R_Senduse_heat_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L131.share_R_Senduse_heat_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Calculation of industrial energy consumption
L132.in_EJ_R_ind_F_Yh <- subset( L131.in_EJ_R_Senduse_F_Yh, grepl( "industry", sector ) )
L132.in_EJ_R_ind_F_Yh$sector <- enduse_sector_aggregation$sector_agg[ match( L132.in_EJ_R_ind_F_Yh$sector, enduse_sector_aggregation$sector ) ]
L132.in_EJ_R_ind_F_Yh$fuel <- enduse_fuel_aggregation$industry[ match( L132.in_EJ_R_ind_F_Yh$fuel, enduse_fuel_aggregation$fuel ) ]
L132.in_EJ_R_ind_F_Yh <- aggregate( L132.in_EJ_R_ind_F_Yh[ X_historical_years ], by=as.list( L132.in_EJ_R_ind_F_Yh[ R_S_F ] ), sum )

#Split dataframe into energy and feedstocks for adjustments (feedstocks do not get adjusted)
L132.in_EJ_R_indfeed_F_Yh <- subset( L132.in_EJ_R_ind_F_Yh, grepl( "feedstocks", sector ) )
L132.in_EJ_R_indfeed_F_Yh$sector <- sub( "in_", "", L132.in_EJ_R_indfeed_F_Yh$sector )
L132.in_EJ_R_indenergy_F_Yh <- subset( L132.in_EJ_R_ind_F_Yh, grepl( "energy", sector ) )
L132.in_EJ_R_indenergy_F_Yh$sector <- sub( "in_", "", L132.in_EJ_R_indenergy_F_Yh$sector )

#Compile the net energy use by unconventional oil production, gas processing, refining, and CHP that were derived elsewhere
# This energy will need to be deducted from industrial energy use
## Unconventional oil: the only relevant fuel is gas, as electricity (if any) was taken off prior to scaling for end-use sectors
L132.in_EJ_R_indunoil_F_Yh <- subset( L121.in_EJ_R_unoil_F_Yh, fuel == "gas" )

## Gas processing: This is complicated. Coal is not deducted, as inputs to coal gasification were mapped directly
# from the IEA energy balances, not derived from known fuel outputs multiplied by assumed IO coefs. The reason for doing this is that
# coal is one of several possible inputs to "gas works", and there is only one output. So no way to disaggregate fuel inputs if calculating from the output.
# Biogas is treated as primary energy in the IEA energy balances, so not relevant here.
## Natural gas processing net energy use is the only one that may need to be calculated (09/2013: It is currently 0 as the IO coef is 1).
L132.in_EJ_R_indgasproc_F_Yh <- subset( L122.in_EJ_R_gasproc_F_Yh, fuel == "gas" )
L132.in_EJ_R_indgasproc_F_Yh[ L132.in_EJ_R_indgasproc_F_Yh$fuel == "gas", X_historical_years ] <-
      L122.in_EJ_R_gasproc_F_Yh[ L122.in_EJ_R_gasproc_F_Yh$fuel == "gas", X_historical_years ] - 
      L122.out_EJ_R_gasproc_F_Yh[ L122.out_EJ_R_gasproc_F_Yh$fuel == "gas", X_historical_years ]

## Refining: crude oil refining energy consumption is not derived based on the output and assumed IO coefs so it doesn't apply here
## Refining: Electricity was taken off prior to scaling for end-use sectors, and in the IEA energy balances, biofuels are treated as primary energy
L132.in_EJ_R_indrefining_F_Yh <- subset( L122.in_EJ_R_refining_F_Yh, !grepl( "oil refining", sector ) & fuel %in% c( "gas", "coal" ) )

#CHP: no adjustments necessary
L132.in_EJ_R_indchp_F_Yh <- L123.in_EJ_R_indchp_F_Yh

#Combine all of the deduction tables and multiply by -1 to indicate that these are deductions
L132.in_EJ_R_inddeductions_F_Yh <- rbind( L132.in_EJ_R_indunoil_F_Yh, L132.in_EJ_R_indrefining_F_Yh, L132.in_EJ_R_indgasproc_F_Yh, L132.in_EJ_R_indchp_F_Yh )
L132.in_EJ_R_inddeductions_F_Yh[ X_historical_years ] <- -1 * L132.in_EJ_R_inddeductions_F_Yh[ X_historical_years ]

##Heat: fuel inputs to heat need to be added to industrial energy use, in regions where heat is not modeled as a final fuel
#Calculate the share of heat consumed by the industrial sector, in regions where heat is not modeled as a separate fuel
L132.share_R_indenergy_heat_Yh <- subset( L131.share_R_Senduse_heat_Yh, grepl( "industry", sector ) )
L132.share_R_indenergy_heat_Yh <- aggregate( L132.share_R_indenergy_heat_Yh[ X_historical_years ],
      by=as.list( L132.share_R_indenergy_heat_Yh[ R_F ] ), sum )

#Multiply these shares by the energy inputs to heat
L132.in_EJ_R_indheat_F_Yh <- subset( L124.in_EJ_R_heat_F_Yh, GCAM_region_ID %in% A_regions[[R]][ A_regions$heat == 0 ] )
L132.in_EJ_R_indheat_F_Yh[ X_historical_years ] <- L132.in_EJ_R_indheat_F_Yh[ X_historical_years ] * L132.share_R_indenergy_heat_Yh[
      match( L132.in_EJ_R_indheat_F_Yh[[R]], L132.share_R_indenergy_heat_Yh[[R]] ),
      X_historical_years ]

#Re-calculate industrial energy as original estimate minus fuel inputs to unconventional oil production, gas processing, and refining, and plus inputs to heat
L132.in_EJ_R_Sindenergy_F_Yh <- rbind( L132.in_EJ_R_indenergy_F_Yh, L132.in_EJ_R_inddeductions_F_Yh, L132.in_EJ_R_indheat_F_Yh )
L132.in_EJ_R_Sindenergy_F_Yh$sector <- unique( L132.in_EJ_R_indenergy_F_Yh$sector )

#Drop heat in regions where this fuel is backed out to its fuel inputs
L132.in_EJ_R_Sindenergy_F_Yh <- subset( L132.in_EJ_R_Sindenergy_F_Yh, paste( GCAM_region_ID, fuel ) %!in% paste( A_regions[[R]][ A_regions$heat == 0 ], "heat" ) )
L132.in_EJ_R_indenergy_F_Yh <- aggregate( L132.in_EJ_R_Sindenergy_F_Yh[ X_historical_years ], by=as.list( L132.in_EJ_R_Sindenergy_F_Yh[R_S_F ] ), sum )

#Where unit conversions have produced slightly negative numbers that should be 0, re-set to 0
L132.in_EJ_R_indenergy_F_Yh[ X_historical_years ][
      L132.in_EJ_R_indenergy_F_Yh[ X_historical_years ] < 0 & L132.in_EJ_R_indenergy_F_Yh[ X_historical_years ] > -1e-6] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L132.in_EJ_R_indenergy_F_Yh <- c( "Industrial energy consumption (not including CHP) by GCAM region / fuel / historical year","Unit = EJ" )
comments.L132.in_EJ_R_indfeed_F_Yh <- c( "Industrial feedstock consumption by GCAM region / fuel / historical year","Unit = EJ" )

#write tables as CSV files
writedata( L132.in_EJ_R_indenergy_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L132.in_EJ_R_indenergy_F_Yh", comments=comments.L132.in_EJ_R_indenergy_F_Yh )
writedata( L132.in_EJ_R_indfeed_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L132.in_EJ_R_indfeed_F_Yh", comments=comments.L132.in_EJ_R_indfeed_F_Yh )

# Every script should finish with this line
logstop()
