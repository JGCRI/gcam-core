# L111.rsrc_fos_Prod.R

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
logstart( "L111.rsrc_fos_Prod.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical fossil energy production, supply curves, and price adjustments by GCAM region and fuel" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
IEA_product_rsrc <- readdata( "ENERGY_MAPPINGS", "IEA_product_rsrc" )
rsrc_unconv_oil_prod_bbld <- readdata( "ENERGY_LEVEL0_DATA", "rsrc_unconv_oil_prod_bbld")
A11.fos_curves <- readdata( "ENERGY_ASSUMPTIONS", "A11.fos_curves" )
L100.IEA_en_bal_ctry_hist <- readdata( "ENERGY_LEVEL1_DATA", "L100.IEA_en_bal_ctry_hist" )
L1011.en_bal_EJ_R_Si_Fi_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1011.en_bal_EJ_R_Si_Fi_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Historical fossil energy production
printlog( "NOTE: Regional production is derived for each fuel as global TPES times regional share of global production" )
printlog( "Determining global total primary energy supply (TPES) for each fuel" )
L111.TPES_EJ_R_F_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "TPES" & fuel %in% rsrc_fuels )
L111.TPES_EJ_F_Yh <- aggregate( L111.TPES_EJ_R_F_Yh[ X_historical_years ], by=as.list( L111.TPES_EJ_R_F_Yh[ S_F] ), sum )

printlog( "Determining regional shares of production for each primary fuel" )
L111.Prod_EJ_R_F_Yh_IEA <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "out_resources" & fuel %in% rsrc_fuels )
L111.Prod_EJ_F_Yh <- aggregate( L111.Prod_EJ_R_F_Yh_IEA[ X_historical_years ], by=as.list( L111.Prod_EJ_R_F_Yh_IEA[ S_F ] ), sum )
L111.Prod_share_R_F_Yh <- L111.Prod_EJ_R_F_Yh_IEA
L111.Prod_share_R_F_Yh[ X_historical_years ] <- L111.Prod_EJ_R_F_Yh_IEA[ X_historical_years ] /
      L111.Prod_EJ_F_Yh[ match( L111.Prod_EJ_R_F_Yh_IEA$fuel, L111.Prod_EJ_F_Yh$fuel ), X_historical_years ]

printlog( "Multiplying through to calculate production by fuel")
L111.Prod_EJ_R_F_Yh_IEA_adj <- L111.Prod_EJ_R_F_Yh_IEA
L111.Prod_EJ_R_F_Yh_IEA_adj[ X_historical_years ] <- L111.Prod_share_R_F_Yh[ X_historical_years ] *
      L111.TPES_EJ_F_Yh[ match( L111.Prod_EJ_R_F_Yh_IEA_adj$fuel, L111.TPES_EJ_F_Yh$fuel ),
      X_historical_years ]

printlog( "Determining unconventional oil production")
#Interpolate production to all historical years
L111.unconv_oil_prod_bbld <- gcam_interp( rsrc_unconv_oil_prod_bbld, historical_years )
L111.unconv_oil_prod_bbld$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L111.unconv_oil_prod_bbld$iso, iso_GCAM_regID$iso ) ]

#Convert to EJ/yr
L111.Prod_EJ_ctry_unconvOil_Yh <- aggregate( L111.unconv_oil_prod_bbld [X_historical_years ] * conv_bbld_EJyr,
      by=as.list( L111.unconv_oil_prod_bbld[ "GCAM_region_ID" ] ), sum )
L111.Prod_EJ_ctry_unconvOil_Yh$sector <- "unconventional oil production"
L111.Prod_EJ_ctry_unconvOil_Yh$fuel <- "unconventional oil"

#Subtract the unconventional oil, append the unconventional oil to the table, and write it out
printlog( "Deducting unconventional oil from total oil and including unconventional oil in calibrated production table")
L111.Prod_EJ_R_F_Yh <- L111.Prod_EJ_R_F_Yh_IEA_adj
L111.Prod_EJ_R_F_Yh[ L111.Prod_EJ_R_F_Yh$GCAM_region_ID %in% L111.Prod_EJ_ctry_unconvOil_Yh$GCAM_region_ID &
                     L111.Prod_EJ_R_F_Yh$fuel == "refined liquids", X_historical_years ] <-
      L111.Prod_EJ_R_F_Yh_IEA_adj[ L111.Prod_EJ_R_F_Yh_IEA_adj$GCAM_region_ID %in% L111.Prod_EJ_ctry_unconvOil_Yh$GCAM_region_ID &
                                   L111.Prod_EJ_R_F_Yh_IEA_adj$fuel == "refined liquids", X_historical_years ] -
      L111.Prod_EJ_ctry_unconvOil_Yh[ X_historical_years ]

#Switch the names from final to primary
L111.Prod_EJ_R_F_Yh$fuel[ L111.Prod_EJ_R_F_Yh$fuel == "refined liquids" ] <- "crude oil"
L111.Prod_EJ_R_F_Yh$fuel[ L111.Prod_EJ_R_F_Yh$fuel == "gas" ] <- "natural gas"
L111.Prod_EJ_R_F_Yh <- rbind( L111.Prod_EJ_R_F_Yh, L111.Prod_EJ_ctry_unconvOil_Yh[ c( R_S_F, X_historical_years ) ] )

#2b. Fossil resource supply curves
printlog( "Fossil resource supply curves" )
# Using supply curves from GCAM 3.0 (same as MiniCAM)
# These need to be downscaled to the country level and then aggregated by the new GCAM regions
printlog( "NOTE: Downscaling fossil resource supply curves from GCAM 3.0 to countries on the basis of resource production" )
printlog( "NOTE: Downscaling method requires that all regions have the same price points")
L111.Prod_ktoe_ctry_F_Yh <- subset( L100.IEA_en_bal_ctry_hist, FLOW == "INDPROD" & PRODUCT %in% IEA_product_rsrc$PRODUCT )
L111.Prod_ktoe_ctry_F_Yh$CumulSum <- rowSums( L111.Prod_ktoe_ctry_F_Yh[ X_historical_years ] )
L111.Prod_ktoe_ctry_F_Yh$resource <- IEA_product_rsrc$resource[ match( L111.Prod_ktoe_ctry_F_Yh$PRODUCT, IEA_product_rsrc$PRODUCT ) ]
L111.Prod_ktoe_ctry_F_Yh <- aggregate( L111.Prod_ktoe_ctry_F_Yh[ "CumulSum" ],
      by=as.list( L111.Prod_ktoe_ctry_F_Yh[ c( "iso", "resource" ) ] ), sum )

printlog( "Calculating production shares of country within GCAM 3.0 region")
L111.Prod_ktoe_ctry_F_Yh$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L111.Prod_ktoe_ctry_F_Yh$iso, iso_GCAM_regID$iso ) ]
L111.Prod_ktoe_RG3_F_Yh <- aggregate( L111.Prod_ktoe_ctry_F_Yh[ "CumulSum" ],
      by=as.list( L111.Prod_ktoe_ctry_F_Yh[ c( "region_GCAM3", "resource" ) ] ), sum )
L111.Prod_share_ctry_F_Yh <- L111.Prod_ktoe_ctry_F_Yh[ c( "iso", "resource" ) ]
L111.Prod_share_ctry_F_Yh$share <- L111.Prod_ktoe_ctry_F_Yh$CumulSum / L111.Prod_ktoe_RG3_F_Yh$CumulSum[
      match( vecpaste( L111.Prod_ktoe_ctry_F_Yh[ c( "region_GCAM3", "resource" ) ] ),
             vecpaste( L111.Prod_ktoe_RG3_F_Yh[ c( "region_GCAM3", "resource" ) ] ) ) ]

printlog( "Downscaling available resources by GCAM 3.0 region to countries")
L111.RsrcCurves_EJ_ctry_Ffos <- data.frame( iso = rep( sort( unique( L111.Prod_share_ctry_F_Yh$iso ) ), times = length( unique( A11.fos_curves$subresource ) ) ),
      subresource = sort( rep( unique( A11.fos_curves$subresource ), times = length( unique( L111.Prod_share_ctry_F_Yh$iso ) ) ) ) )
L111.RsrcCurves_EJ_ctry_Ffos$resource <- A11.fos_curves$resource[ match( L111.RsrcCurves_EJ_ctry_Ffos$subresource, A11.fos_curves$subresource) ]
L111.RsrcCurves_EJ_ctry_Ffos <- repeat_and_add_vector( L111.RsrcCurves_EJ_ctry_Ffos, "grade", unique( A11.fos_curves$grade ) )

#Remove non-existent grades
L111.RsrcCurves_EJ_ctry_Ffos <- subset( L111.RsrcCurves_EJ_ctry_Ffos, paste( subresource, grade ) %in% paste( A11.fos_curves$subresource, A11.fos_curves$grade ) )

#Match in GCAM3 region, along with available resources
L111.RsrcCurves_EJ_ctry_Ffos$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L111.RsrcCurves_EJ_ctry_Ffos$iso, iso_GCAM_regID$iso ) ]
L111.RsrcCurves_EJ_ctry_Ffos$available_region_GCAM3 <- A11.fos_curves$available[
      match( vecpaste( L111.RsrcCurves_EJ_ctry_Ffos[ RG3_subrsrc_grd ] ), vecpaste( A11.fos_curves[ RG3_subrsrc_grd ] ) ) ]

#Match in the share of resource available by each country within GCAM 3.0 region
L111.RsrcCurves_EJ_ctry_Ffos$share <- L111.Prod_share_ctry_F_Yh$share[
      match( vecpaste( L111.RsrcCurves_EJ_ctry_Ffos[ c( "iso", "resource" ) ] ),
             vecpaste( L111.Prod_share_ctry_F_Yh[ c( "iso", "resource" ) ] ) ) ]

#Use crude oil production shares as a proxy for unconventional oil resources
L111.RsrcCurves_EJ_ctry_Ffos$share[ L111.RsrcCurves_EJ_ctry_Ffos$resource == "unconventional oil" ] <- L111.Prod_share_ctry_F_Yh$share[
      match( paste( L111.RsrcCurves_EJ_ctry_Ffos$iso[ L111.RsrcCurves_EJ_ctry_Ffos$resource == "unconventional oil" ], "crude oil" ),
             paste( L111.Prod_share_ctry_F_Yh$iso, L111.Prod_share_ctry_F_Yh$resource ) ) ]

#Set all other missing values to 0 (these are small countries)
L111.RsrcCurves_EJ_ctry_Ffos$share[ is.na( L111.RsrcCurves_EJ_ctry_Ffos$share ) ] <- 0
L111.RsrcCurves_EJ_ctry_Ffos$available <- L111.RsrcCurves_EJ_ctry_Ffos$available_region_GCAM3 * L111.RsrcCurves_EJ_ctry_Ffos$share

printlog( "Aggregating by GCAM regions" )
L111.RsrcCurves_EJ_ctry_Ffos[[R]] <- iso_GCAM_regID[[R]][ match( L111.RsrcCurves_EJ_ctry_Ffos$iso, iso_GCAM_regID$iso ) ]
L111.RsrcCurves_EJ_R_Ffos <- aggregate( L111.RsrcCurves_EJ_ctry_Ffos[ "available" ],
      by=as.list( L111.RsrcCurves_EJ_ctry_Ffos[ c( R, "resource", "subresource", "grade" ) ] ), sum )
L111.RsrcCurves_EJ_R_Ffos$extractioncost <- A11.fos_curves$extractioncost[
      match( vecpaste( L111.RsrcCurves_EJ_R_Ffos[ c( "resource", "subresource", "grade" ) ] ),
             vecpaste( A11.fos_curves[ c( "resource", "subresource", "grade" ) ] ) ) ]

#2c. Resource prices
# Fossil resource historical prices are currently assumed. No level1 processing is needed.

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L111.Prod_EJ_R_F_Yh <- c( "Calibrated fossil energy production by GCAM region / resource / historical year","Unit = EJ" )
comments.L111.RsrcCurves_EJ_R_Ffos <- c( "Fossil energy resource curves by GCAM region / resource","Unit = EJ" )

#write tables as CSV files
writedata( L111.Prod_EJ_R_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L111.Prod_EJ_R_F_Yh", comments=comments.L111.Prod_EJ_R_F_Yh )
writedata( L111.RsrcCurves_EJ_R_Ffos, domain="ENERGY_LEVEL1_DATA", fn="L111.RsrcCurves_EJ_R_Ffos", comments=comments.L111.RsrcCurves_EJ_R_Ffos )

# Every script should finish with this line
logstop()
