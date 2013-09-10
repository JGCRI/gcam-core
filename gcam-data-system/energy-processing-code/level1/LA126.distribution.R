
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
logstart( "LA126.distribution.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical IO coefficients of energy distribution sectors" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
L1011.en_bal_EJ_R_Si_Fi_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1011.en_bal_EJ_R_Si_Fi_Yh" )
L122.out_EJ_R_gasproc_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L122.out_EJ_R_gasproc_F_Yh" )
L123.out_EJ_R_elec_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.out_EJ_R_elec_F_Yh" )
L123.out_EJ_R_indchp_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.out_EJ_R_indchp_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Electricity net ownuse: deduct electricity consumed on-site prior to any transmission and distribution losses
printlog( "Inputs, outputs, and IO coefs of electricity ownuse sector" )
L126.net_EJ_R_elecownuse_F_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "net_electricity ownuse" )
L126.net_EJ_R_elecownuse_F_Yh$sector <- sub( "net_", "", L126.net_EJ_R_elecownuse_F_Yh$sector )

##Input of elec_net_ownuse = sum of all electricity generation (incl CHP secondary output)
L126.out_EJ_R_electot_F_Yh <- rbind( L123.out_EJ_R_elec_F_Yh, L123.out_EJ_R_indchp_F_Yh )
L126.in_EJ_R_elecownuse_F_Yh <- derive_passthrough_inputs( net.data.frame = L126.net_EJ_R_elecownuse_F_Yh, input.data.frame = L126.out_EJ_R_electot_F_Yh )
#Output of elec_net_ownuse= input - net
L126.out_EJ_R_elecownuse_F_Yh <- data.frame(
      L126.in_EJ_R_elecownuse_F_Yh[ R_S_F ], L126.in_EJ_R_elecownuse_F_Yh[ X_historical_years ] - L126.net_EJ_R_elecownuse_F_Yh[ X_historical_years ] )
L126.IO_R_elecownuse_F_Yh <- data.frame(
      L126.in_EJ_R_elecownuse_F_Yh[ R_S_F ], L126.in_EJ_R_elecownuse_F_Yh[ X_historical_years ] / L126.out_EJ_R_elecownuse_F_Yh[ X_historical_years ] )

# 2b. Electricity transmission and distribution
printlog( "Inputs, outputs, and IO coefs of electricity transmission and distribution" )
L126.net_EJ_R_electd_F_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "net_electricity distribution" )
L126.net_EJ_R_electd_F_Yh$sector <- sub( "net_", "", L126.net_EJ_R_electd_F_Yh$sector )

##Input of electd = output of elec_net_ownuse
L126.in_EJ_R_electd_F_Yh <- derive_passthrough_inputs( net.data.frame = L126.net_EJ_R_electd_F_Yh, input.data.frame = L126.out_EJ_R_elecownuse_F_Yh )
L126.out_EJ_R_electd_F_Yh <- data.frame(
      L126.in_EJ_R_electd_F_Yh[ R_S_F ], L126.in_EJ_R_electd_F_Yh[ X_historical_years ] - L126.net_EJ_R_electd_F_Yh[ X_historical_years ] )
L126.IO_R_electd_F_Yh <- data.frame(
      L126.in_EJ_R_electd_F_Yh[ R_S_F ], L126.in_EJ_R_electd_F_Yh[ X_historical_years ] / L126.out_EJ_R_electd_F_Yh[ X_historical_years ] )

# 2c. Gas pipeline
printlog( "Inputs, outputs, and IO coefs of gas pipelines" )
L126.net_EJ_R_gaspipe_F_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "net_gas pipeline" )
L126.net_EJ_R_gaspipe_F_Yh$sector <- sub( "net_", "", L126.net_EJ_R_gaspipe_F_Yh$sector )

##Input of gas pipeline = output of gas processing
L126.in_EJ_R_gaspipe_F_Yh <- derive_passthrough_inputs( net.data.frame = L126.net_EJ_R_gaspipe_F_Yh, input.data.frame = L122.out_EJ_R_gasproc_F_Yh )
L126.out_EJ_R_gaspipe_F_Yh <- data.frame(
      L126.in_EJ_R_gaspipe_F_Yh[ R_S_F ], L126.in_EJ_R_gaspipe_F_Yh[ X_historical_years ] - L126.net_EJ_R_gaspipe_F_Yh[ X_historical_years ] )
L126.IO_R_gaspipe_F_Yh <- data.frame(
      L126.in_EJ_R_gaspipe_F_Yh[ R_S_F ], L126.in_EJ_R_gaspipe_F_Yh[ X_historical_years ] / L126.out_EJ_R_gaspipe_F_Yh[ X_historical_years ] )
#Some countries have zero gas in some of the base years. reset their IO coefs to 1
L126.IO_R_gaspipe_F_Yh[ is.na( L126.IO_R_gaspipe_F_Yh ) ] <- 1

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L126.in_EJ_R_elecownuse_F_Yh <- c( "Inputs to electricity ownuse sector by GCAM region / historical year","Unit = EJ" )
comments.L126.out_EJ_R_elecownuse_F_Yh <- c( "Outputs of electricity ownuse sector by GCAM region / historical year","Unit = EJ" )
comments.L126.IO_R_elecownuse_F_Yh <- c( "IO coefficients of electricity ownuse sector by GCAM region / historical year","Unitless IO" )
comments.L126.in_EJ_R_electd_F_Yh <- c( "Inputs to electricity T&D by GCAM region / historical year","Unit = EJ" )
comments.L126.out_EJ_R_electd_F_Yh <- c( "Outputs of electricity T&D by GCAM region / historical year","Unit = EJ" )
comments.L126.IO_R_electd_F_Yh <- c( "IO coefficients of electricity T&D by GCAM region / historical year","Unitless IO" )
comments.L126.in_EJ_R_gaspipe_F_Yh <- c( "Inputs to gas pipeline sector by GCAM region / historical year","Unit = EJ" )
comments.L126.out_EJ_R_gaspipe_F_Yh <- c( "Outputs of gas pipeline sector by GCAM region / historical year","Unit = EJ" )
comments.L126.IO_R_gaspipe_F_Yh <- c( "IO coefficients of gas pipeline sector by GCAM region / historical year","Unitless IO" )

#write tables as CSV files
writedata( L126.in_EJ_R_elecownuse_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L126.in_EJ_R_elecownuse_F_Yh", comments=comments.L126.in_EJ_R_elecownuse_F_Yh )
writedata( L126.out_EJ_R_elecownuse_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L126.out_EJ_R_elecownuse_F_Yh", comments=comments.L126.out_EJ_R_elecownuse_F_Yh )
writedata( L126.IO_R_elecownuse_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L126.IO_R_elecownuse_F_Yh", comments=comments.L126.IO_R_elecownuse_F_Yh )
writedata( L126.in_EJ_R_electd_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L126.in_EJ_R_electd_F_Yh", comments=comments.L126.in_EJ_R_electd_F_Yh )
writedata( L126.out_EJ_R_electd_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L126.out_EJ_R_electd_F_Yh", comments=comments.L126.out_EJ_R_electd_F_Yh )
writedata( L126.IO_R_electd_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L126.IO_R_electd_F_Yh", comments=comments.L126.IO_R_electd_F_Yh )
writedata( L126.in_EJ_R_gaspipe_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L126.in_EJ_R_gaspipe_F_Yh", comments=comments.L126.in_EJ_R_gaspipe_F_Yh )
writedata( L126.out_EJ_R_gaspipe_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L126.out_EJ_R_gaspipe_F_Yh", comments=comments.L126.out_EJ_R_gaspipe_F_Yh )
writedata( L126.IO_R_gaspipe_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L126.IO_R_gaspipe_F_Yh", comments=comments.L126.IO_R_gaspipe_F_Yh )

# Every script should finish with this line
logstop()
