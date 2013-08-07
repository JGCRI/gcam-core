
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
logstart( "L122.gasproc_refining.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical inputs, outputs, and IO coefficients of gas processing and refining" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
A21.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A21.globaltech_coef")
A22.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A22.globaltech_coef" )
L1011.en_bal_EJ_R_Si_Fi_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1011.en_bal_EJ_R_Si_Fi_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. GAS PROCESSING
printlog( "Gas processing input-output coefficients are exogenous" )
# Interpolate gas processing IO coefs to all historical years and match in the fuel name
L122.gasproc_coef <- gcam_interp( subset( A22.globaltech_coef, supplysector == "gas processing" ), historical_years )
L122.gasproc_coef[ S_F ] <- calibrated_techs[ match( vecpaste( L122.gasproc_coef[ s_s_t ] ), vecpaste( calibrated_techs[ s_s_t ] ) ), S_F ]

printlog( "Gas processing output: biogas and natural gas equal to regional TPES" )
L122.out_EJ_R_gasproc_F_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh,
      sector == "TPES" & fuel == "gasified biomass" |
      sector == "TPES" & fuel == "gas" )
L122.out_EJ_R_gasproc_F_Yh$sector <- "gas processing"
L122.out_EJ_R_gasproc_F_Yh$fuel[ L122.out_EJ_R_gasproc_F_Yh$fuel == "gasified biomass" ] <- "biomass"

printlog( "Gas processing output from coal gasification is calculated from the input of coal")
L122.in_EJ_R_gasproc_coal_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "in_gas processing" & fuel == "coal" )
L122.in_EJ_R_gasproc_coal_Yh$sector <- "gas processing"
L122.out_EJ_R_gasproc_coal_Yh <- L122.in_EJ_R_gasproc_coal_Yh
L122.out_EJ_R_gasproc_coal_Yh[ X_historical_years ] <- L122.in_EJ_R_gasproc_coal_Yh[ X_historical_years ] / L122.gasproc_coef[
      match( vecpaste( L122.in_EJ_R_gasproc_coal_Yh[ S_F ] ), vecpaste( L122.gasproc_coef[ S_F ] ) ),
      X_historical_years ]
      
#Combine (rbind) coal and the other fuels
L122.out_EJ_R_gasproc_F_Yh <- rbind( L122.out_EJ_R_gasproc_F_Yh, L122.out_EJ_R_gasproc_coal_Yh )      

printlog( "Calculate the inputs to gas processing")
L122.in_EJ_R_gasproc_F_Yh <- L122.out_EJ_R_gasproc_F_Yh
L122.in_EJ_R_gasproc_F_Yh[ X_historical_years ] <- L122.out_EJ_R_gasproc_F_Yh[ X_historical_years ] * L122.gasproc_coef[
      match( vecpaste( L122.in_EJ_R_gasproc_F_Yh[ S_F ] ), vecpaste( L122.gasproc_coef[ S_F ] ) ),
      X_historical_years ]

# 2b. REFINING
##For most technologies, inputs are derived from outputs based on exogenous IO coefficients. Subset the relevant IO coefs
L122.globaltech_coef <- subset( A22.globaltech_coef, paste( supplysector, subsector, technology ) %in% vecpaste( calibrated_techs[ s_s_t ] ) )
L122.globaltech_coef[ S_F ] <- calibrated_techs[
      match( vecpaste( L122.globaltech_coef[ s_s_t_i ] ), vecpaste( calibrated_techs[ s_s_t_i ] ) ),
      S_F ]
L122.globaltech_coef <- gcam_interp( L122.globaltech_coef, historical_years )

## BIOMASS LIQUIDS
printlog( "Ethanol and biodiesel output: equal to regional TPES")
L122.out_EJ_R_biofuel_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "TPES" & fuel %in% c( "refined biofuels_ethanol", "refined biofuels_FT" ) )
L122.out_EJ_R_biofuel_Yh$sector[ L122.out_EJ_R_biofuel_Yh$fuel == "refined biofuels_ethanol" ] <- A_regions$ethanol[
      match( L122.out_EJ_R_biofuel_Yh$GCAM_region_ID[ L122.out_EJ_R_biofuel_Yh$fuel == "refined biofuels_ethanol" ],
             A_regions$GCAM_region_ID ) ]
L122.out_EJ_R_biofuel_Yh$sector[ L122.out_EJ_R_biofuel_Yh$fuel == "refined biofuels_FT" ] <- A_regions$biodiesel[
      match( L122.out_EJ_R_biofuel_Yh$GCAM_region_ID[ L122.out_EJ_R_biofuel_Yh$fuel == "refined biofuels_FT" ],
             A_regions$GCAM_region_ID ) ]
L122.out_EJ_R_biofuel_Yh$fuel <- calibrated_techs$fuel[ match( L122.out_EJ_R_biofuel_Yh$sector, calibrated_techs$sector ) ]

#Inputs to biofuel production are region-specific
printlog( "Inputs to ethanol and biodiesel: equal to output times exogenous input-output coefficients" )
L122.biofuel_coef <- subset( L122.globaltech_coef, sector %in% L122.out_EJ_R_biofuel_Yh$sector )

#Because some have multiple inputs, repeat coefficient table by number of regions and then subset only the applicable combinations
L122.biofuel_coef_repR <- repeat_and_add_vector( L122.biofuel_coef, "GCAM_region_ID", A_regions$GCAM_region_ID )
L122.biofuel_coef_R <- subset( L122.biofuel_coef_repR, paste( GCAM_region_ID, sector ) %in%
      c( vecpaste( A_regions[ c( "GCAM_region_ID", "ethanol" ) ] ),
         vecpaste( A_regions[ c( "GCAM_region_ID", "biodiesel" ) ] ) ) )

#Build table of inputs to biofuel production (IO coefs times output)
L122.in_EJ_R_biofuel_F_Yh <- L122.biofuel_coef_R[ c( R_S_F ) ]
L122.in_EJ_R_biofuel_F_Yh[ X_historical_years ] <- L122.biofuel_coef_R[ X_historical_years ] * L122.out_EJ_R_biofuel_Yh[
      match( vecpaste( L122.in_EJ_R_biofuel_F_Yh[ R_S ] ), vecpaste( L122.out_EJ_R_biofuel_Yh[ R_S ] ) ),
      X_historical_years ]

## GAS AND COAL TO LIQUIDS
printlog( "Output of GTL and CTL: subset from energy balance table")
L122.out_EJ_R_gtlctl_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector %in% c( "out_gtl", "out_ctl" ) )
L122.out_EJ_R_gtlctl_Yh$sector <- sub( "out_", "", L122.out_EJ_R_gtlctl_Yh$sector )

printlog( "GTL and CTL inputs: derived as output times exogenous input-output coefficients" )
# Interpolate gas processing IO coefs to all historical years and match in the fuel name
L122.gtlctl_coef <- subset( L122.globaltech_coef, paste( sector, fuel ) %in% vecpaste( L122.out_EJ_R_gtlctl_Yh[ S_F ] ) )

#Assuming only one input per technology
L122.in_EJ_R_gtlctl_F_Yh <- L122.out_EJ_R_gtlctl_Yh
L122.in_EJ_R_gtlctl_F_Yh[ X_historical_years ] <- L122.out_EJ_R_gtlctl_Yh[ X_historical_years ] * L122.gtlctl_coef[
      match( vecpaste( L122.in_EJ_R_gtlctl_F_Yh[ S_F ] ), vecpaste( L122.gtlctl_coef[ S_F ] ) ),
      X_historical_years ]
      
##CRUDE OIL REFINING
printlog( "Oil refining: output is equal to TPES minus net refinery energy use" )
L122.out_EJ_R_oilrefining_Yh <- data.frame( GCAM_region_ID = GCAM_region_names$GCAM_region_ID, sector = "oil refining", fuel = "oil" )
L122.out_EJ_R_oilrefining_Yh[ X_historical_years ] <-
      subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "TPES" & fuel == "refined liquids", select = X_historical_years ) - 
      subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "net_oil refining" & fuel == "refined liquids", select = X_historical_years )

printlog( "Oil refining: input of oil is equal to TPES, and input of other fuels is from net refinery energy use")
L122.in_EJ_R_oilrefining_F_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "net_oil refining" )
L122.in_EJ_R_oilrefining_F_Yh[ L122.in_EJ_R_oilrefining_F_Yh$fuel == "refined liquids", X_historical_years ] <-
      subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "TPES" & fuel == "refined liquids", select = X_historical_years )
L122.in_EJ_R_oilrefining_F_Yh$sector <- sub( "net_", "", L122.in_EJ_R_oilrefining_F_Yh$sector )
L122.in_EJ_R_oilrefining_F_Yh$fuel[ L122.in_EJ_R_oilrefining_F_Yh$fuel == "refined liquids" ] <- "oil"      
      
printlog( "Calculating region- and fuel-specific coefficients of crude oil refining" )
L122.IO_R_oilrefining_F_Yh <- L122.in_EJ_R_oilrefining_F_Yh
L122.IO_R_oilrefining_F_Yh[ X_historical_years ] <- L122.in_EJ_R_oilrefining_F_Yh[ X_historical_years ] / L122.out_EJ_R_oilrefining_Yh[
      match( L122.IO_R_oilrefining_F_Yh$GCAM_region_ID, L122.out_EJ_R_oilrefining_Yh$GCAM_region_ID ),
      X_historical_years ]

#COMBINE (RBIND) ALL CALIBRATED REFINERY INPUT AND OUTPUT TABLES
L122.out_EJ_R_refining_F_Yh <- rbind( L122.out_EJ_R_oilrefining_Yh, L122.out_EJ_R_gtlctl_Yh, L122.out_EJ_R_biofuel_Yh )
L122.in_EJ_R_refining_F_Yh <- rbind( L122.in_EJ_R_oilrefining_F_Yh, L122.in_EJ_R_gtlctl_F_Yh, L122.in_EJ_R_biofuel_F_Yh )

#Ancillary step: calculate and write out the derived crop inputs to the various first-generation biofuel technologies, for the AGLU processing
L122.in_EJ_R_1stgenbio_F_Yh <- L122.in_EJ_R_biofuel_F_Yh
L122.in_EJ_R_1stgenbio_F_Yh$passthrough.sector <- calibrated_techs$minicam.energy.input[
      match( vecpaste( L122.in_EJ_R_1stgenbio_F_Yh[ S_F ] ), vecpaste( calibrated_techs[ S_F ] ) ) ]
L122.in_EJ_R_1stgenbio_F_Yh <- subset( L122.in_EJ_R_1stgenbio_F_Yh, passthrough.sector %in% A21.globaltech_coef$supplysector )
L122.in_EJ_R_1stgenbio_F_Yh$GCAM_commodity <- A21.globaltech_coef$minicam.energy.input[
      match( L122.in_EJ_R_1stgenbio_F_Yh$passthrough.sector, A21.globaltech_coef$supplysector ) ]

#crop inputs to biodiesel are region-specific
L122.in_EJ_R_1stgenbio_F_Yh$GCAM_commodity[ L122.in_EJ_R_1stgenbio_F_Yh$GCAM_commodity == "biomassOil" ] <- A_regions$biomassOil_tech[
      match( L122.in_EJ_R_1stgenbio_F_Yh$GCAM_region_ID[ L122.in_EJ_R_1stgenbio_F_Yh$GCAM_commodity == "biomassOil" ], A_regions$GCAM_region_ID ) ]
L122.in_EJ_R_1stgenbio_F_Yh <- subset( L122.in_EJ_R_1stgenbio_F_Yh, GCAM_commodity %in% FAO_ag_items_PRODSTAT$GCAM_commodity )

#Interpolate coefs to all historical periods, and multiply by the input quantities
L121.globaltech_coef <- gcam_interp( A21.globaltech_coef, historical_years )
L122.in_Mt_R_C_Yh <- L122.in_EJ_R_1stgenbio_F_Yh[ c( "GCAM_region_ID", "GCAM_commodity") ]
L122.in_Mt_R_C_Yh[ X_historical_years ] <- L122.in_EJ_R_1stgenbio_F_Yh[ X_historical_years ] * L121.globaltech_coef[
      match( L122.in_Mt_R_C_Yh$GCAM_commodity, L121.globaltech_coef$minicam.energy.input ),
      X_historical_years ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L122.out_EJ_R_gasproc_F_Yh <- c( "Outputs of gas processing by GCAM region / fuel / historical year","Unit = EJ" )
comments.L122.in_EJ_R_gasproc_F_Yh <- c( "Inputs to gas processing by GCAM region / fuel / historical year","Unit = EJ" )
comments.L122.IO_R_oilrefining_F_Yh <- c( "Oil refining input-output coefficients by GCAM region / fuel / historical year","Unitless IO" )
comments.L122.out_EJ_R_refining_F_Yh <- c( "Outputs of refining by GCAM region / fuel / historical year","Unit = EJ" )
comments.L122.in_EJ_R_refining_F_Yh <- c( "Inputs to refining by GCAM region / fuel / historical year","Unit = EJ" )
comments.L122.in_Mt_R_C_Yh <- c( "Crop inputs to first-generation biofuel production by GCAM region / commodity / historical year","Unit = Mt" )

#write tables as CSV files
writedata( L122.out_EJ_R_gasproc_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L122.out_EJ_R_gasproc_F_Yh", comments=comments.L122.out_EJ_R_gasproc_F_Yh )
writedata( L122.in_EJ_R_gasproc_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L122.in_EJ_R_gasproc_F_Yh", comments=comments.L122.in_EJ_R_gasproc_F_Yh )
writedata( L122.IO_R_oilrefining_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L122.IO_R_oilrefining_F_Yh", comments=comments.L122.IO_R_oilrefining_F_Yh )
writedata( L122.out_EJ_R_refining_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L122.out_EJ_R_refining_F_Yh", comments=comments.L122.out_EJ_R_refining_F_Yh )
writedata( L122.in_EJ_R_refining_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L122.in_EJ_R_refining_F_Yh", comments=comments.L122.in_EJ_R_refining_F_Yh )
writedata( L122.in_Mt_R_C_Yh, domain="ENERGY_LEVEL1_DATA", fn="L122.in_Mt_R_C_Yh", comments=comments.L122.in_Mt_R_C_Yh )

# Every script should finish with this line
logstop()
