
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
logstart( "L121.oil.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical consumption of conventional and unconventional oil" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
IEA_product_rsrc <- readdata( "ENERGY_MAPPINGS", "IEA_product_rsrc" )
A21.unoil_demandshares <- readdata( "ENERGY_ASSUMPTIONS", "A21.unoil_demandshares")
A21.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A21.globaltech_coef")
L100.IEA_en_bal_ctry_hist <- readdata( "ENERGY_LEVEL1_DATA", "L100.IEA_en_bal_ctry_hist" )
L1011.en_bal_EJ_R_Si_Fi_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1011.en_bal_EJ_R_Si_Fi_Yh" )
L111.Prod_EJ_R_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L111.Prod_EJ_R_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Calculating energy inputs (gas) to unconventional oil production in the historical years" )
L121.globaltech_coef <- A21.globaltech_coef[ vecpaste( A21.globaltech_coef[ s_s_t_i ] ) %in% vecpaste( calibrated_techs[ s_s_t_i ] ), ]
L121.globaltech_coef_interp <- gcam_interp( L121.globaltech_coef, historical_years )
L121.globaltech_coef_interp[ S_F ] <- calibrated_techs[
      match( vecpaste( L121.globaltech_coef_interp[ s_s_t_i ] ), vecpaste( calibrated_techs[ s_s_t_i ] ) ),
      S_F ]

#Energy inputs = production times fuel IO coef
L121.in_EJ_R_unoil_F_Yh <- L111.Prod_EJ_R_F_Yh[ vecpaste( L111.Prod_EJ_R_F_Yh[ S_F ] ) %in% vecpaste( L121.globaltech_coef_interp[ S_F ] ), ]
L121.in_EJ_R_unoil_F_Yh <- repeat_and_add_vector( L121.in_EJ_R_unoil_F_Yh, "fuel", L121.globaltech_coef_interp$fuel )
L121.in_EJ_R_unoil_F_Yh[ X_historical_years ] <- L121.in_EJ_R_unoil_F_Yh[ X_historical_years ] * L121.globaltech_coef_interp[
      match( vecpaste( L121.in_EJ_R_unoil_F_Yh[ S_F ] ), vecpaste( L121.globaltech_coef_interp[ S_F ] ) ),
      X_historical_years ]

printlog( "Downscaling unconventional oil consumption shares by GCAM 3.0 region to countries" )
L121.TPES_ktoe_ctry_oil_Yf <- L100.IEA_en_bal_ctry_hist[ L100.IEA_en_bal_ctry_hist$FLOW == "TPES" &
      L100.IEA_en_bal_ctry_hist$PRODUCT %in% IEA_product_rsrc$PRODUCT[ IEA_product_rsrc$resource == "crude oil" ],
      c( "iso", X_final_historical_year ) ]
L121.TPES_ktoe_ctry_oil_Yf <- aggregate( L121.TPES_ktoe_ctry_oil_Yf[ X_final_historical_year ],
      by = as.list( L121.TPES_ktoe_ctry_oil_Yf[ "iso" ] ), sum )
L121.TPES_ktoe_ctry_oil_Yf$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L121.TPES_ktoe_ctry_oil_Yf$iso, iso_GCAM_regID$iso ) ]
L121.TPES_ktoe_RG3_oil_Yf <- aggregate( L121.TPES_ktoe_ctry_oil_Yf[ X_final_historical_year ],
      by=as.list( L121.TPES_ktoe_ctry_oil_Yf[ "region_GCAM3" ] ), sum )

#Calculate the shares of country-within-GCAM3-region, match in shares of GCAM3-region-within-world, and multiply
L121.TPES_ktoe_ctry_oil_Yf$share_ctry_RG3 <- L121.TPES_ktoe_ctry_oil_Yf[[X_final_historical_year]] / L121.TPES_ktoe_RG3_oil_Yf[[X_final_historical_year]][
      match( L121.TPES_ktoe_ctry_oil_Yf$region_GCAM3, L121.TPES_ktoe_RG3_oil_Yf$region_GCAM3 ) ]
L121.TPES_ktoe_ctry_oil_Yf$share_RG3_world <- A21.unoil_demandshares$share[
      match( L121.TPES_ktoe_ctry_oil_Yf$region_GCAM3, A21.unoil_demandshares$region_GCAM3 ) ]
L121.TPES_ktoe_ctry_oil_Yf$share <- L121.TPES_ktoe_ctry_oil_Yf$share_ctry_RG3 * L121.TPES_ktoe_ctry_oil_Yf$share_RG3_world

printlog( "Aggregating country shares of unconventional oil demand to GCAM regions" )
L121.TPES_ktoe_ctry_oil_Yf[[R]] <- iso_GCAM_regID[[R]][ match( L121.TPES_ktoe_ctry_oil_Yf$iso, iso_GCAM_regID$iso ) ]
L121.share_R_TPES_unoil_Yf <- aggregate( L121.TPES_ktoe_ctry_oil_Yf[ "share" ], by=as.list( L121.TPES_ktoe_ctry_oil_Yf[ R ] ), sum )

printlog( "Calculating unconventional oil demand by region and historical year" )
L121.Prod_EJ_R_unoil_Yh <- subset( L111.Prod_EJ_R_F_Yh, fuel == "unconventional oil" )
L121.Prod_EJ_unoil_Yh <- aggregate( L121.Prod_EJ_R_unoil_Yh[ X_historical_years ], by = as.list( L121.Prod_EJ_R_unoil_Yh[ "fuel" ] ), sum )
L121.in_EJ_R_TPES_unoil_Yh <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID[[R]] ) ),
      sector = "TPES",
      fuel = "unconventional oil" )
L121.in_EJ_R_TPES_unoil_Yh[ X_historical_years ] <- L121.share_R_TPES_unoil_Yf$share * L121.Prod_EJ_unoil_Yh[
      rep( 1, times = nrow( L121.in_EJ_R_TPES_unoil_Yh ) ), X_historical_years ]

#Conventional (crude) oil: calculate as liquids TPES - unconventional oil
L121.in_EJ_R_TPES_liq_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "TPES" & fuel == "refined liquids" )
L121.in_EJ_R_TPES_crude_Yh <- L121.in_EJ_R_TPES_liq_Yh[ c( R_S_F, X_historical_years ) ]
L121.in_EJ_R_TPES_crude_Yh$fuel <- "crude oil"
L121.in_EJ_R_TPES_crude_Yh[ X_historical_years ] <-
      L121.in_EJ_R_TPES_liq_Yh[ X_historical_years ] -
      L121.in_EJ_R_TPES_unoil_Yh[ X_historical_years ]
      
# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L121.in_EJ_R_unoil_F_Yh <- c( "Energy inputs to unconventional oil production by GCAM region / fuel / historical year","Unit = EJ" )
comments.L121.in_EJ_R_TPES_unoil_Yh <- c( "Unconventional oil total primary energy supply by GCAM region / historical year","Unit = EJ" )
comments.L121.in_EJ_R_TPES_crude_Yh <- c( "Crude oil total primary energy supply by GCAM region / historical year","Unit = EJ" )
#write tables as CSV files
writedata( L121.in_EJ_R_unoil_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L121.in_EJ_R_unoil_F_Yh", comments=comments.L121.in_EJ_R_unoil_F_Yh )
writedata( L121.in_EJ_R_TPES_crude_Yh, domain="ENERGY_LEVEL1_DATA", fn="L121.in_EJ_R_TPES_crude_Yh", comments=comments.L121.in_EJ_R_TPES_crude_Yh )
writedata( L121.in_EJ_R_TPES_unoil_Yh, domain="ENERGY_LEVEL1_DATA", fn="L121.in_EJ_R_TPES_unoil_Yh", comments=comments.L121.in_EJ_R_TPES_unoil_Yh )

# Every script should finish with this line
logstop()
