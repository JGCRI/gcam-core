# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
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
logstart( "LA101.en_bal_IEA.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical energy balances from IEA, aggregated to GCAM regions, intermediate sectors, and intermediate fuels" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
IEA_flow_sector <- readdata( "ENERGY_MAPPINGS", "IEA_flow_sector" )
IEA_product_fuel <- readdata( "ENERGY_MAPPINGS", "IEA_product_fuel" )
IEA_sector_fuel_modifications <- readdata( "ENERGY_MAPPINGS", "IEA_sector_fuel_modifications" )
enduse_fuel_aggregation <- readdata( "ENERGY_MAPPINGS", "enduse_fuel_aggregation" )
L100.IEA_en_bal_ctry_hist <- readdata( "ENERGY_LEVEL1_DATA", "L100.IEA_en_bal_ctry_hist" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# subset only the relevant years and combine OECD with non-OECD
printlog( "Matching GCAM region names, intermediate sector names, and intermediate fuel names into IEA energy balances" )
L101.IEA_en_bal_ctry_hist <- L100.IEA_en_bal_ctry_hist
L101.IEA_en_bal_ctry_hist$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L101.IEA_en_bal_ctry_hist$iso, iso_GCAM_regID$iso ) ]

#Add in the fuel names and sector names
L101.IEA_en_bal_ctry_hist$sector <- IEA_flow_sector$sector[ match( L101.IEA_en_bal_ctry_hist$FLOW, IEA_flow_sector$flow_code ) ]
L101.IEA_en_bal_ctry_hist$fuel <- IEA_product_fuel$fuel[ match( L101.IEA_en_bal_ctry_hist$PRODUCT, IEA_product_fuel$product ) ]
L101.IEA_en_bal_ctry_hist$conversion <- IEA_flow_sector$conversion[ match( L101.IEA_en_bal_ctry_hist$FLOW, IEA_flow_sector$flow_code ) ]

#Drop missing values (many of the products and flows are not used)
L101.IEA_en_bal_ctry_hist <- na.omit( L101.IEA_en_bal_ctry_hist )

#Re-name sectors for selected sector/fuel combinations
#Most of this re-mapping takes place in the "IEA_sector_fuel_modifications" mapping file; however, some are region-specific and are done here
#Specify traditional biomass in regions where indicated; otherwise, label as biomass
printlog( "Setting biomass_tradbio (wood) to biomass in regions/sectors where wood is not labeled traditional biomass" )
L101.IEA_en_bal_ctry_hist$fuel[ { L101.IEA_en_bal_ctry_hist$fuel == "biomass_tradbio" & L101.IEA_en_bal_ctry_hist$sector != "in_bld_resid" } |
      { L101.IEA_en_bal_ctry_hist$fuel == "biomass_tradbio" & A_regions$tradbio_region[
      match( L101.IEA_en_bal_ctry_hist$GCAM_region_ID, A_regions$GCAM_region_ID ) ] == 0 } ] <- "biomass"

#In some countries, "gas works gas" is produced from coal. This is calibrated (coal gasification), so re-name the relevant sectors
printlog( "Specifying coal gasification in selected regions")
#Rename the sector and the fuel: where the sector is gas works and the fuel is coal, this is the input to gas processing
L101.IEA_en_bal_ctry_hist$sector[
      L101.IEA_en_bal_ctry_hist$sector == "net_gas works" &
      L101.IEA_en_bal_ctry_hist$fuel == "coal" ] <- "in_gas processing"
#Where the sector is gas works and the fuel is not coal, this is industry/energy transformation
L101.IEA_en_bal_ctry_hist$sector[
      L101.IEA_en_bal_ctry_hist$sector == "net_gas works" &
      L101.IEA_en_bal_ctry_hist$fuel != "coal" ] <- "net_industry_energy transformation"
         
printlog( "Re-setting sector-fuel combinations as specified in", file_fqn( "ENERGY_MAPPINGS", "IEA_sector_fuel_modifications" ) )
sector_fuel_init <- vecpaste( L101.IEA_en_bal_ctry_hist[ S_F ] )
sector_fuel_tochange <- vecpaste( IEA_sector_fuel_modifications[ c( "sector_initial", "fuel_initial" ) ] )
L101.IEA_en_bal_ctry_hist[ sector_fuel_init %in% sector_fuel_tochange, c( S_F, "conversion" ) ] <-
      IEA_sector_fuel_modifications[ match( sector_fuel_init[ sector_fuel_init %in% sector_fuel_tochange ], sector_fuel_tochange ),
      c( S_F, "conversion" ) ]

#Drop some sector-fuel combinations that are not relevant
#Electricity-only fuels in sectors other than electricity generation
L101.IEA_en_bal_ctry_hist$sector[ grepl( "elec_", L101.IEA_en_bal_ctry_hist$fuel) & !grepl( "electricity generation", L101.IEA_en_bal_ctry_hist$sector ) ] <- NA
L101.IEA_en_bal_ctry_hist$sector[ L101.IEA_en_bal_ctry_hist$fuel %in% c( "biomass", "heat" ) & grepl( "trn_", L101.IEA_en_bal_ctry_hist$sector ) ] <- NA

#Subset the table minus the missing values for further processing
L101.IEA_en_bal_ctry_hist_clean <- na.omit( L101.IEA_en_bal_ctry_hist )

#Aggregate by relevant categories, multiplying through by conversion factors (to EJ)
printlog( "Converting energy balance data to EJ and aggregating by GCAM region, intermediate sector, and intermediate fuel")
L101.en_bal_EJ_R_Si_Fi_Yh <- aggregate( L101.IEA_en_bal_ctry_hist_clean[ X_historical_years ] * L101.IEA_en_bal_ctry_hist_clean$conversion,
      by=as.list( L101.IEA_en_bal_ctry_hist_clean[ R_S_F ] ), sum )
               
printlog( "Setting to zero net fuel production from energy transformation sectors modeled under the industrial sector" )
for( i in 1:ncol( L101.en_bal_EJ_R_Si_Fi_Yh[ X_historical_years ] ) ){
	L101.en_bal_EJ_R_Si_Fi_Yh[ X_historical_years ][i][
	    L101.en_bal_EJ_R_Si_Fi_Yh[ X_historical_years ][i] < 0 &
	    grepl( "industry", L101.en_bal_EJ_R_Si_Fi_Yh$sector ),] <- 0
}

printlog( "Setting to zero net production of fuels classified as coal at gas works (gas coke)" )
for( i in 1:ncol( L101.en_bal_EJ_R_Si_Fi_Yh[ X_historical_years ] ) ){
	L101.en_bal_EJ_R_Si_Fi_Yh[ X_historical_years ][i][
	    L101.en_bal_EJ_R_Si_Fi_Yh[ X_historical_years ][i] < 0 &
	    L101.en_bal_EJ_R_Si_Fi_Yh$sector == "in_gas processing",] <- 0
}

#Create a template table with all applicable combinations of sector and fuel found in any region
# First, define the available combinations of sector and fuel
sector_fuel_final <- sort( unique( paste( L101.en_bal_EJ_R_Si_Fi_Yh$sector, L101.en_bal_EJ_R_Si_Fi_Yh$fuel, sep = "|") ) )

# Then build the table
L101.en_bal_EJ_R_Si_Fi_Yh_full <- data.frame(
      GCAM_region_ID = sort( rep( A_regions$GCAM_region_ID, times = length( sector_fuel_final ) ) ),
      sector = substr( sector_fuel_final, 1, regexpr( "|", sector_fuel_final, fixed = TRUE ) - 1 ),
      fuel = substr( sector_fuel_final, regexpr( "|", sector_fuel_final, fixed = TRUE ) + 1, nchar( sector_fuel_final ) ) )
L101.en_bal_EJ_R_Si_Fi_Yh_full[ X_historical_years ] <- L101.en_bal_EJ_R_Si_Fi_Yh[
      match( vecpaste( L101.en_bal_EJ_R_Si_Fi_Yh_full[ R_S_F ] ),
             vecpaste( L101.en_bal_EJ_R_Si_Fi_Yh[ R_S_F ] ) ),
      X_historical_years ]
L101.en_bal_EJ_R_Si_Fi_Yh_full[ is.na( L101.en_bal_EJ_R_Si_Fi_Yh_full ) ] <- 0

#Calculate the total primary energy supply (TPES) in each region as the sum of all flows that are inputs
##This guarantees that our TPES will be consistent with the tracked forms of consumption (i.e. no statdiffs, stock changes, transfers)
printlog( "Calculating total primary energy supply for each region / intermediate fuel and appending to the energy balance table" )
L101.in_EJ_R_Si_Fi_Yh <- subset( L101.en_bal_EJ_R_Si_Fi_Yh_full, grepl( "in_", substr( sector, 1, 3 ) ) | grepl( "net_", substr( sector, 1, 4 ) ) )
L101.in_EJ_R_Si_Fi_Yh$sector <- "TPES"
L101.in_EJ_R_TPES_Fi_Yh <- aggregate( L101.in_EJ_R_Si_Fi_Yh[ X_historical_years ],
      by = as.list( L101.in_EJ_R_Si_Fi_Yh[ R_S_F ] ), sum )
                 
#Append (rbind) TPES onto the end of the energy balances
L101.en_bal_EJ_R_Si_Fi_Yh_full <- rbind( L101.en_bal_EJ_R_Si_Fi_Yh_full, L101.in_EJ_R_TPES_Fi_Yh )

#For downscaling of buildings and transportation energy, aggregate by fuel and country
L101.in_ktoe_ctry_trn_Fiea <- subset( L101.IEA_en_bal_ctry_hist_clean, grepl( "trn", sector ) )
L101.in_ktoe_ctry_trn_Fiea$fuel <- enduse_fuel_aggregation$trn[ match( L101.in_ktoe_ctry_trn_Fiea$fuel, enduse_fuel_aggregation$fuel ) ]
L101.in_EJ_ctry_trn_Fi_Yh <- aggregate( L101.in_ktoe_ctry_trn_Fiea[ X_historical_years ] * L101.in_ktoe_ctry_trn_Fiea$conversion,
      by=as.list( L101.in_ktoe_ctry_trn_Fiea[ c( "iso", S_F ) ] ), sum )

L101.in_ktoe_ctry_bld_Fiea <- subset( L101.IEA_en_bal_ctry_hist_clean, grepl( "bld", sector ) )
L101.in_ktoe_ctry_bld_Fiea$fuel <- enduse_fuel_aggregation$bld[ match( L101.in_ktoe_ctry_bld_Fiea$fuel, enduse_fuel_aggregation$fuel ) ]
L101.in_EJ_ctry_bld_Fi_Yh <- aggregate( L101.in_ktoe_ctry_bld_Fiea[ X_historical_years ] * L101.in_ktoe_ctry_bld_Fiea$conversion,
      by=as.list( L101.in_ktoe_ctry_bld_Fiea[ c( "iso", S_F ) ] ), sum )

printlog( "For country-level comparisons, keep the iso and aggregate all sectors and fuels")
L101.en_bal_EJ_ctry_Si_Fi_Yh <- aggregate( L101.IEA_en_bal_ctry_hist_clean[ X_historical_years ] * L101.IEA_en_bal_ctry_hist_clean$conversion,
      by=as.list( L101.IEA_en_bal_ctry_hist_clean[ c( "iso", R_S_F ) ] ), sum )
L101.in_EJ_ctry_Si_Fi_Yh <- subset( L101.en_bal_EJ_ctry_Si_Fi_Yh, grepl( "in_", substr( sector, 1, 3 ) ) | grepl( "net_", substr( sector, 1, 4 ) ) )
L101.in_EJ_ctry_Si_Fi_Yh$sector <- "TPES"
L101.in_EJ_ctry_TPES_Fi_Yh <- aggregate( L101.in_EJ_ctry_Si_Fi_Yh[ X_historical_years ],
      by = as.list( L101.in_EJ_ctry_Si_Fi_Yh[ c( "iso", R_S_F ) ] ), sum )
                 
#Append (rbind) TPES onto the end of the energy balances
L101.en_bal_EJ_ctry_Si_Fi_Yh_full <- rbind( L101.en_bal_EJ_ctry_Si_Fi_Yh, L101.in_EJ_ctry_TPES_Fi_Yh )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L101.en_bal_EJ_R_Si_Fi_Yh_full <- c( "Energy balances by GCAM region / intermediate sector / intermediate fuel / historical year","Unit = EJ" )
comments.L101.en_bal_EJ_ctry_Si_Fi_Yh_full <- c( "Energy balances by country / GCAM region / intermediate sector / intermediate fuel / historical year","Unit = EJ" )
comments.L101.in_EJ_ctry_trn_Fi_Yh <- c( "Transportation sector energy consumption by country / IEA mode / fuel / historical year","Unit = EJ" )
comments.L101.in_EJ_ctry_bld_Fi_Yh <- c( "Building energy consumption by country / IEA sector / fuel / historical year","Unit = EJ" )

#write tables as CSV files
writedata( L101.en_bal_EJ_R_Si_Fi_Yh_full, domain="ENERGY_LEVEL1_DATA", fn="L101.en_bal_EJ_R_Si_Fi_Yh_full", comments=comments.L101.en_bal_EJ_R_Si_Fi_Yh_full )
writedata( L101.en_bal_EJ_ctry_Si_Fi_Yh_full, domain="ENERGY_LEVEL1_DATA", fn="L101.en_bal_EJ_ctry_Si_Fi_Yh_full", comments=comments.L101.en_bal_EJ_ctry_Si_Fi_Yh_full )
writedata( L101.in_EJ_ctry_trn_Fi_Yh, domain="ENERGY_LEVEL1_DATA", fn="L101.in_EJ_ctry_trn_Fi_Yh", comments=comments.L101.in_EJ_ctry_trn_Fi_Yh )
writedata( L101.in_EJ_ctry_bld_Fi_Yh, domain="ENERGY_LEVEL1_DATA", fn="L101.in_EJ_ctry_bld_Fi_Yh", comments=comments.L101.in_EJ_ctry_bld_Fi_Yh )

# Every script should finish with this line
logstop()
