
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
logstart( "L223.electricity.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Electricity sector" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
fuel_energy_input <- readdata( "ENERGY_MAPPINGS", "fuel_energy_input" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
A23.sector <- readdata( "ENERGY_ASSUMPTIONS", "A23.sector" )
A23.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A23.subsector_logit" )
A23.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A23.subsector_shrwt" )
A23.subsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A23.subsector_interp" )
A23.subsector_shrwt_nuc_R <- readdata( "ENERGY_ASSUMPTIONS", "A23.subsector_shrwt_nuc_R" )
A23.subsector_shrwt_renew_R <- readdata( "ENERGY_ASSUMPTIONS", "A23.subsector_shrwt_renew_R" )
A23.globalinttech <- readdata( "ENERGY_ASSUMPTIONS", "A23.globalinttech" )
A23.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_shrwt" )
A23.globaltech_eff <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_eff" )
A23.globaltech_capital <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_capital" )
A23.globaltech_OMfixed <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_OMfixed" )
A23.globaltech_OMvar <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_OMvar" )
A23.globaltech_retirement <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_retirement" )
A23.globaltech_co2capture <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_co2capture" )
L1231.in_EJ_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.in_EJ_R_elec_F_tech_Yh" )
L1231.out_EJ_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.out_EJ_R_elec_F_tech_Yh" )
L1231.eff_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.eff_R_elec_F_tech_Yh" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Supplysector information
printlog( "L223.Supplysector_elec: Supply sector information for electricity sector" )
L223.Supplysector_elec <- write_to_all_regions( A23.sector, names_Supplysector )

printlog( "L223.ElecReserve: Electricity reserve margin and average grid capacity factor" )
L223.ElecReserve <- write_to_all_regions( A23.sector, names_ElecReserve )

# 2b. Subsector information
printlog( "L223.SubsectorLogit_elec: Subsector logit exponents of electricity sector" )
L223.SubsectorLogit_elec <- write_to_all_regions( A23.subsector_logit, names_SubsectorLogit )

printlog( "L223.SubsectorShrwt_elec and L223.SubsectorShrwtFllt_elec: Subsector shareweights of electricity sector" )
if( any( !is.na( A23.subsector_shrwt$year ) ) ){
	L223.SubsectorShrwt_elec <- write_to_all_regions( A23.subsector_shrwt[ !is.na( A23.subsector_shrwt$year ), ], names_SubsectorShrwt )
	}
if( any( !is.na( A23.subsector_shrwt$year.fillout ) ) ){
	L223.SubsectorShrwtFllt_elec <- write_to_all_regions( A23.subsector_shrwt[ !is.na( A23.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
	}

printlog( "L223.SubsectorShrwt_nuc: Subsector shareweights of nuclear electricity" )
L223.SubsectorShrwt_nuc_GCAM3 <- interpolate_and_melt(
      A23.subsector_shrwt_nuc_R, model_future_years[model_future_years >= 2020 & model_future_years <= 2065 ], value.name = "share.weight" )
L223.SubsectorShrwt_nuc <- repeat_and_add_vector( subset( L223.SubsectorShrwt_nuc_GCAM3, region_GCAM3 == region_GCAM3[1] ), R, GCAM_region_names[[R]] )
L223.SubsectorShrwt_nuc$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L223.SubsectorShrwt_nuc[[R]], iso_GCAM_regID[[R]] ) ]
L223.SubsectorShrwt_nuc$share.weight <- L223.SubsectorShrwt_nuc_GCAM3$share.weight[
      match( vecpaste( L223.SubsectorShrwt_nuc[ c( "region_GCAM3", "subsector", "year" ) ] ),
             vecpaste( L223.SubsectorShrwt_nuc_GCAM3[ c( "region_GCAM3", "subsector", "year" ) ] ) ) ]
L223.SubsectorShrwt_nuc <- add_region_name( L223.SubsectorShrwt_nuc )
L223.SubsectorShrwt_nuc <- na.omit( L223.SubsectorShrwt_nuc[ names_SubsectorShrwt ] )

printlog( "L223.SubsectorShrwt_renew: Near term subsector shareweights of renewable technologies" )
L223.SubsectorShrwt_renew_GCAM3 <- melt( A23.subsector_shrwt_renew_R, id.vars = grep( "X[0-9]{4}", names( A23.subsector_shrwt_renew_R ), invert = T ) )
L223.SubsectorShrwt_renew_GCAM3$year <- substring( L223.SubsectorShrwt_renew_GCAM3$variable, 2, 5 )
names( L223.SubsectorShrwt_renew_GCAM3 )[ names( L223.SubsectorShrwt_renew_GCAM3 ) == "value" ] <- "share.weight"
L223.SubsectorShrwt_renew <- repeat_and_add_vector( subset( L223.SubsectorShrwt_renew_GCAM3, region_GCAM3 == region_GCAM3[1] ), R, GCAM_region_names[[R]] )
L223.SubsectorShrwt_renew$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L223.SubsectorShrwt_renew[[R]], iso_GCAM_regID[[R]] ) ]
L223.SubsectorShrwt_renew$share.weight <- L223.SubsectorShrwt_renew_GCAM3$share.weight[
      match( vecpaste( L223.SubsectorShrwt_renew[ c( "region_GCAM3", "subsector", "year" ) ] ),
             vecpaste( L223.SubsectorShrwt_renew_GCAM3[ c( "region_GCAM3", "subsector", "year" ) ] ) ) ]
L223.SubsectorShrwt_renew <- add_region_name( L223.SubsectorShrwt_renew )
L223.SubsectorShrwt_renew <- na.omit( L223.SubsectorShrwt_renew[ names_SubsectorShrwt ] )

printlog( "L223.SubsectorInterp_elec and L223.SubsectorInterpTo_elec: Subsector shareweight interpolation of electricity sector" )
if( any( is.na( A23.subsector_interp$to.value ) ) ){
	L223.SubsectorInterp_elec <- write_to_all_regions( A23.subsector_interp[ is.na( A23.subsector_interp$to.value ), ], names_SubsectorInterp )
	}
if( any( !is.na( A23.subsector_interp$to.value ) ) ){
	L223.SubsectorInterpTo_elec <- write_to_all_regions( A23.subsector_interp[ !is.na( A23.subsector_interp$to.value ), ], names_SubsectorInterpTo )
	}

# 2c. Technology information
printlog( "L223.StubTech_elec: Identification of stub technologies of electricity generation" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L223.StubTech_elec <- write_to_all_regions( A23.globaltech_shrwt, names_Tech )
names( L223.StubTech_elec ) <- names_StubTech

#Efficiencies of global technologies
printlog( "L223.GlobalTechEff_elec: Energy inputs and coefficients of global electricity generation technologies" )
L223.globaltech_eff.melt <- interpolate_and_melt( A23.globaltech_eff, c( model_base_years, model_future_years ), value.name="efficiency" )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L223.globaltech_eff.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_eff.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechEff_elec <- L223.globaltech_eff.melt[ names_GlobalTechEff ]
L223.GlobalTechEff_elec$efficiency <- round( L223.GlobalTechEff_elec$efficiency, digits_efficiency )

L223.GlobalIntTechEff_elec <- subset_inttechs( L223.GlobalTechEff_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechEff_elec <- subset_techs( L223.GlobalTechEff_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

#Costs of global technologies
printlog( "L223.GlobalTechCapital_elec: Capital costs of global electricity generation technologies" )
L223.globaltech_capital.melt <- interpolate_and_melt( A23.globaltech_capital, c( model_base_years, model_future_years ), value.name="capital.overnight" )
L223.globaltech_capital.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_capital.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechCapital_elec <- L223.globaltech_capital.melt[ names_GlobalTechCapital ]
L223.GlobalTechCapital_elec$capital.overnight <- round( L223.GlobalTechCapital_elec$capital.overnight, digits_capital )
L223.GlobalIntTechCapital_elec <- subset_inttechs( L223.GlobalTechCapital_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechCapital_elec <- subset_techs( L223.GlobalTechCapital_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

printlog( "L223.GlobalTechOMfixed_elec: Fixed O&M costs of global electricity generation technologies" )
L223.globaltech_OMfixed.melt <- interpolate_and_melt( A23.globaltech_OMfixed, c( model_base_years, model_future_years ), value.name="OM.fixed" )
L223.globaltech_OMfixed.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_OMfixed.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechOMfixed_elec <- L223.globaltech_OMfixed.melt[ names_GlobalTechOMfixed ]
L223.GlobalTechOMfixed_elec$OM.fixed <- round( L223.GlobalTechOMfixed_elec$OM.fixed, digits_OM )
L223.GlobalIntTechOMfixed_elec <- subset_inttechs( L223.GlobalTechOMfixed_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechOMfixed_elec <- subset_techs( L223.GlobalTechOMfixed_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

printlog( "L223.GlobalTechOMvar_elec: Variable O&M costs of global electricity generation technologies" )
L223.globaltech_OMvar.melt <- interpolate_and_melt( A23.globaltech_OMvar, c( model_base_years, model_future_years ), value.name="OM.var" )
L223.globaltech_OMvar.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_OMvar.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechOMvar_elec <- L223.globaltech_OMvar.melt[ names_GlobalTechOMvar ]
L223.GlobalTechOMvar_elec$OM.var <- round( L223.GlobalTechOMvar_elec$OM.var, digits_OM )
L223.GlobalIntTechOMvar_elec <- subset_inttechs( L223.GlobalTechOMvar_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechOMvar_elec <- subset_techs( L223.GlobalTechOMvar_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

#Shareweights of global technologies
printlog( "L223.GlobalTechShrwt_elec: Shareweights of global electricity generation technologies" )
L223.globaltech_shrwt.melt <- interpolate_and_melt( A23.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L223.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechShrwt_elec <- L223.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]
L223.GlobalIntTechShrwt_elec <- subset_inttechs( L223.GlobalTechShrwt_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechShrwt_elec <- subset_techs( L223.GlobalTechShrwt_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

#CO2 capture rates of global technologies
printlog( "L223.GlobalTechCapture_elec: CO2 capture fractions from global electricity generation technologies" )
## No need to consider historical periods or intermittent technologies here
L223.globaltech_co2capture.melt <- interpolate_and_melt( A23.globaltech_co2capture, model_future_years, value.name="remove.fraction" )
L223.globaltech_co2capture.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_co2capture.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechCapture_elec <- data.frame(
      L223.globaltech_co2capture.melt[ names_GlobalTechYr ],
      remove.fraction = round( L223.globaltech_co2capture.melt$remove.fraction, digits = digits_remove.fraction ) )
L223.GlobalTechCapture_elec$storage.market <- CO2.storage.market

#Backup parameters for intermittent technologies
printlog( "L223.GlobalIntTechBackup_elec: Backup-related information for global electricity generation technologies" )
L223.globalinttech_repY <- repeat_and_add_vector( A23.globalinttech, "year", c( model_base_years, model_future_years ) )
L223.globalinttech_repY[ c( "sector.name", "subsector.name" ) ] <- L223.globalinttech_repY[ c( "supplysector", "subsector" ) ]
L223.GlobalIntTechBackup_elec <- L223.globalinttech_repY[ names_GlobalTechBackup ]

#Retirement information
L223.globaltech_retirement <- set_years ( A23.globaltech_retirement )
L223.globaltech_retirement[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_retirement[ c( "supplysector", "subsector" ) ]
#Copy the data in the first future period through to the end year
L223.globaltech_retirement <- rbind(
      subset( L223.globaltech_retirement, year == max( model_base_years ) ),
      repeat_and_add_vector( subset( L223.globaltech_retirement, year == min(model_future_years ) ), "year", model_future_years ) )

#Retirement may consist of any of three types of retirement function (phased, s-curve, or none), with and without profit shutdown decider
# All of these options have different headers, and all are allowed
if( any( !is.na( L223.globaltech_retirement$shutdown.rate ) & L223.globaltech_retirement$profit.shutdown == 1 ) ){
	printlog( "L223.GlobalTechShutdownProfit_elec: Global tech lifetime and shutdown rate, including profit shutdown decider" )
	L223.GlobalTechShutdownProfit_elec <- L223.globaltech_retirement[
	     !is.na( L223.globaltech_retirement$shutdown.rate ) & L223.globaltech_retirement$profit.shutdown == 1,
	      c( names_GlobalTechYr, "lifetime", "shutdown.rate") ]
	L223.GlobalIntTechShutdownProfit_elec <- subset_inttechs( L223.GlobalTechShutdownProfit_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalIntTechShutdownProfit_elec ) == 0 ) rm( L223.GlobalIntTechShutdownProfit_elec )
	L223.GlobalTechShutdownProfit_elec <- subset_techs( L223.GlobalTechShutdownProfit_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalTechShutdownProfit_elec ) == 0 ) rm( L223.GlobalTechShutdownProfit_elec )
	}
if( any( !is.na( L223.globaltech_retirement$shutdown.rate ) & L223.globaltech_retirement$profit.shutdown == 0 ) ){
	printlog( "L223.GlobalTechShutdown_elec: Global tech lifetime and shutdown rate" )
	L223.GlobalTechShutdown_elec <- L223.globaltech_retirement[
	     !is.na( L223.globaltech_retirement$shutdown.rate ) & L223.globaltech_retirement$profit.shutdown == 1,
	      c( names_GlobalTechYr, "lifetime", "shutdown.rate") ]
	L223.GlobalIntTechShutdown_elec <- subset_inttechs( L223.GlobalTechShutdown_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalIntTechShutdown_elec ) == 0 ) rm( L223.GlobalIntTechShutdown_elec )
	L223.GlobalTechShutdown_elec <- subset_techs( L223.GlobalTechShutdown_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalTechShutdown_elec ) == 0 ) rm( L223.GlobalTechShutdown_elec )
	}
if( any( !is.na( L223.globaltech_retirement$half.life ) & L223.globaltech_retirement$profit.shutdown == 1 ) ){
	printlog( "L223.GlobalTechSCurveProfit_elec: Global tech lifetime, s-curve retirement function, and profit shutdown" )
	L223.GlobalTechSCurveProfit_elec <- L223.globaltech_retirement[
	     !is.na( L223.globaltech_retirement$half.life ) & L223.globaltech_retirement$profit.shutdown == 1,
	      c( names_GlobalTechYr, "lifetime", "steepness", "half.life" ) ]
	L223.GlobalIntTechSCurveProfit_elec <- subset_inttechs( L223.GlobalTechSCurveProfit_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalIntTechSCurveProfit_elec ) == 0 ) rm( L223.GlobalIntTechSCurveProfit_elec )
	L223.GlobalTechSCurveProfit_elec <- subset_techs( L223.GlobalTechSCurveProfit_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalTechSCurveProfit_elec ) == 0 ) rm( L223.GlobalTechSCurveProfit_elec )
	}
if( any( !is.na( L223.globaltech_retirement$half.life ) & L223.globaltech_retirement$profit.shutdown == 0 ) ){
	printlog( "L223.GlobalTechSCurve_elec: Global tech lifetime and s-curve retirement function" )
	L223.GlobalTechSCurve_elec <- L223.globaltech_retirement[
	     !is.na( L223.globaltech_retirement$half.life ) & L223.globaltech_retirement$profit.shutdown == 1,
	      c( names_GlobalTechYr, "lifetime", "steepness", "half.life" ) ]
	L223.GlobalIntTechSCurve_elec <- subset_inttechs( L223.GlobalTechSCurve_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalIntTechSCurve_elec ) == 0 ) rm( L223.GlobalIntTechSCurve_elec )
	L223.GlobalTechSCurve_elec <- subset_techs( L223.GlobalTechSCurve_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalTechSCurve_elec ) == 0 ) rm( L223.GlobalTechSCurve_elec )
	}
if( any( is.na( L223.globaltech_retirement$shutdown.rate ) & is.na( L223.globaltech_retirement$half.life ) & L223.globaltech_retirement$profit.shutdown == 1 ) ){
	printlog( "L223.GlobalTechLifetimeProfit_elec: Global tech lifetime and profit shutdown decider" )
	L223.GlobalTechLifetimeProfit_elec <- L223.globaltech_retirement[
	     is.na( L223.globaltech_retirement$shutdown.rate ) & is.na( L223.globaltech_retirement$half.life ) & L223.globaltech_retirement$profit.shutdown == 1,
	      c( names_GlobalTechYr, "lifetime" ) ]
	L223.GlobalIntTechLifetimeProfit_elec <- subset_inttechs( L223.GlobalTechLifetimeProfit_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalIntTechLifetimeProfit_elec ) == 0 ) rm( L223.GlobalIntTechLifetimeProfit_elec )
	L223.GlobalTechLifetimeProfit_elec <- subset_techs( L223.GlobalTechLifetimeProfit_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalTechLifetimeProfit_elec ) == 0 ) rm( L223.GlobalTechLifetimeProfit_elec )
	}
if( any( is.na( L223.globaltech_retirement$shutdown.rate ) & is.na( L223.globaltech_retirement$half.life ) & L223.globaltech_retirement$profit.shutdown == 0 ) ){
	printlog( "L223.GlobalTechLifetime_elec: Global tech lifetime" )
	L223.GlobalTechLifetime_elec <- L223.globaltech_retirement[
	     is.na( L223.globaltech_retirement$shutdown.rate ) & is.na( L223.globaltech_retirement$half.life ) & L223.globaltech_retirement$profit.shutdown == 0,
	      c( names_GlobalTechYr, "lifetime" ) ]
	L223.GlobalIntTechLifetime_elec <- subset_inttechs( L223.GlobalTechLifetime_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalIntTechLifetime_elec ) == 0 ) rm( L223.GlobalIntTechLifetime_elec )
	L223.GlobalTechLifetime_elec <- subset_techs( L223.GlobalTechLifetime_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalTechLifetime_elec ) == 0 ) rm( L223.GlobalTechLifetime_elec )
	}

#2d. Calibration and region-specific data
L223.in_EJ_R_elec_F_tech_Yh <- interpolate_and_melt( L1231.in_EJ_R_elec_F_tech_Yh, model_base_years )
L223.in_EJ_R_elec_F_tech_Yh <- add_region_name( L223.in_EJ_R_elec_F_tech_Yh )
L223.in_EJ_R_elec_F_tech_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( paste( L223.in_EJ_R_elec_F_tech_Yh$sector, L223.in_EJ_R_elec_F_tech_Yh$fuel, L223.in_EJ_R_elec_F_tech_Yh$technology ),
             paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ),
      c( "supplysector", "subsector", "technology" ) ]
L223.in_EJ_R_elec_F_tech_Yh <- subset( L223.in_EJ_R_elec_F_tech_Yh, calibrated_techs$calibration[
      match( paste( sector, fuel, technology ), paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ) ] == "input" )

L223.out_EJ_R_elec_F_tech_Yh <- interpolate_and_melt( L1231.out_EJ_R_elec_F_tech_Yh, model_base_years )
L223.out_EJ_R_elec_F_tech_Yh <- add_region_name( L223.out_EJ_R_elec_F_tech_Yh )
L223.out_EJ_R_elec_F_tech_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( paste( L223.out_EJ_R_elec_F_tech_Yh$sector, L223.out_EJ_R_elec_F_tech_Yh$fuel, L223.out_EJ_R_elec_F_tech_Yh$technology ),
             paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ),
      c( "supplysector", "subsector", "technology" ) ]
L223.fixout_EJ_R_elec_F_tech_Yh <- subset( L223.out_EJ_R_elec_F_tech_Yh, calibrated_techs$calibration[
      match( paste( sector, fuel, technology ), paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ) ] == "fixed output" )
L223.calout_EJ_R_elec_F_tech_Yh <- subset( L223.out_EJ_R_elec_F_tech_Yh, calibrated_techs$calibration[
      match( paste( sector, fuel, technology ), paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ) ] == "output" )

printlog( "L223.StubTechCalInput_elec: calibrated input of electricity generation technologies")
#Note that there is no need to specify which stub technologies are intermittent
L223.StubTechCalInput_elec <- L223.in_EJ_R_elec_F_tech_Yh[ names_StubTechYr ]
L223.StubTechCalInput_elec$minicam.energy.input <- A23.globaltech_eff$minicam.energy.input[ 
      match( paste( L223.StubTechCalInput_elec$subsector, L223.StubTechCalInput_elec$stub.technology ),
             paste( A23.globaltech_eff$subsector, A23.globaltech_eff$technology ) ) ]
L223.StubTechCalInput_elec$calibrated.value <- round( L223.in_EJ_R_elec_F_tech_Yh$value, digits_calOutput )
L223.StubTechCalInput_elec$year.share.weight <- L223.StubTechCalInput_elec$year
L223.StubTechCalInput_elec <- set_subsector_shrwt( L223.StubTechCalInput_elec, value.name = "calibrated.value" )
L223.StubTechCalInput_elec$share.weight <- ifelse( L223.StubTechCalInput_elec$calibrated.value > 0, 1, 0 )

printlog( "L223.StubTechFixOut_elec: fixed output of electricity generation technologies")
L223.StubTechFixOut_elec <- L223.fixout_EJ_R_elec_F_tech_Yh[ names_StubTechYr ]
L223.StubTechFixOut_elec$fixedOutput <- round( L223.fixout_EJ_R_elec_F_tech_Yh$value, digits_calOutput )
L223.StubTechFixOut_elec$year.share.weight <- L223.StubTechFixOut_elec$year
L223.StubTechFixOut_elec$subsector.share.weight <- 0
L223.StubTechFixOut_elec$share.weight <- 0

printlog( "L223.StubTechProd_elec: calibrated output of electricity generation technologies" )
L223.StubTechProd_elec <- L223.calout_EJ_R_elec_F_tech_Yh[ names_StubTechYr ]
L223.StubTechProd_elec$calOutputValue <- round( L223.calout_EJ_R_elec_F_tech_Yh$value, digits_calOutput )
L223.StubTechProd_elec$year.share.weight <- L223.StubTechProd_elec$year
L223.StubTechProd_elec <- set_subsector_shrwt( L223.StubTechProd_elec, value.name="calOutputValue" )
L223.StubTechProd_elec$share.weight <- ifelse( L223.StubTechProd_elec$calOutputValue > 0, 1, 0 )

printlog( "L223.StubTechEff_elec: calibrated efficiencies of electricity generation technologies" )
L223.eff_R_elec_F_tech_Yh <- interpolate_and_melt( L1231.eff_R_elec_F_tech_Yh, model_base_years )
L223.eff_R_elec_F_tech_Yh <- add_region_name( L223.eff_R_elec_F_tech_Yh )
L223.eff_R_elec_F_tech_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( paste( L223.eff_R_elec_F_tech_Yh$sector, L223.eff_R_elec_F_tech_Yh$fuel, L223.eff_R_elec_F_tech_Yh$technology ),
             paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ),
      c( "supplysector", "subsector", "technology" ) ]

L223.StubTechEff_elec <- L223.eff_R_elec_F_tech_Yh[ names_StubTechYr ]
L223.StubTechEff_elec$minicam.energy.input <- A23.globaltech_eff$minicam.energy.input[ 
      match( paste( L223.StubTechEff_elec$subsector, L223.StubTechEff_elec$stub.technology ),
             paste( A23.globaltech_eff$subsector, A23.globaltech_eff$technology ) ) ]
L223.StubTechEff_elec$efficiency <- round( L223.eff_R_elec_F_tech_Yh$value, digits_calOutput )  #use level of rounding of calibrated output
L223.StubTechEff_elec$market.name <- L223.StubTechEff_elec$region

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L223.Supplysector_elec, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L223.Supplysector_elec",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_electricity.xml" ) 
write_mi_data( L223.ElecReserve, "ElecReserve", "ENERGY_LEVEL2_DATA", "L223.ElecReserve", "ENERGY_XML_BATCH", "batch_electricity.xml" ) 
write_mi_data( L223.SubsectorLogit_elec, "SubsectorLogit", "ENERGY_LEVEL2_DATA", "L223.SubsectorLogit_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" ) 
if( exists( "L223.SubsectorShrwt_elec" ) ){
	write_mi_data( L223.SubsectorShrwt_elec, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L223.SubsectorShrwt_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.SubsectorShrwtFllt_elec" ) ){
	write_mi_data( L223.SubsectorShrwtFllt_elec, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L223.SubsectorShrwtFllt_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" ) 
	}
write_mi_data( L223.SubsectorShrwt_nuc, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L223.SubsectorShrwt_nuc", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.SubsectorShrwt_renew, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L223.SubsectorShrwt_renew", "ENERGY_XML_BATCH", "batch_electricity.xml" )
if( exists( "L223.SubsectorInterp_elec" ) ) {
	write_mi_data( L223.SubsectorInterp_elec, "SubsectorInterp", "ENERGY_LEVEL2_DATA", "L223.SubsectorInterp_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.SubsectorInterpTo_elec" ) ) {
	write_mi_data( L223.SubsectorInterpTo_elec, "SubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L223.SubsectorInterpTo_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
write_mi_data( L223.StubTech_elec, "StubTech", "ENERGY_LEVEL2_DATA", "L223.StubTech_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalIntTechEff_elec, "GlobalIntTechEff", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechEff_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalTechEff_elec, "GlobalTechEff", "ENERGY_LEVEL2_DATA", "L223.GlobalTechEff_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalTechCapital_elec, "GlobalTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalIntTechCapital_elec, "GlobalIntTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechCapital_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalTechOMfixed_elec, "GlobalTechOMfixed", "ENERGY_LEVEL2_DATA", "L223.GlobalTechOMfixed_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalIntTechOMfixed_elec, "GlobalIntTechOMfixed", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechOMfixed_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalTechOMvar_elec, "GlobalTechOMvar", "ENERGY_LEVEL2_DATA", "L223.GlobalTechOMvar_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalIntTechOMvar_elec, "GlobalIntTechOMvar", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechOMvar_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalTechShrwt_elec, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L223.GlobalTechShrwt_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalIntTechShrwt_elec, "GlobalIntTechShrwt", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechShrwt_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalTechCapture_elec, "GlobalTechCapture", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapture_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalIntTechBackup_elec, "GlobalIntTechBackup", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechBackup_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
if( exists( "L223.GlobalTechShutdownProfit_elec" ) ) {
	write_mi_data( L223.GlobalTechShutdownProfit_elec, "GlobalTechShutdownProfit", "ENERGY_LEVEL2_DATA", "L223.GlobalTechShutdownProfit_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalIntTechShutdownProfit_elec" ) ) {
	write_mi_data( L223.GlobalIntTechShutdownProfit_elec, "GlobalIntTechShutdownProfit", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechShutdownProfit_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalTechShutdown_elec" ) ) {
	write_mi_data( L223.GlobalTechShutdown_elec, "GlobalTechShutdown", "ENERGY_LEVEL2_DATA", "L223.GlobalTechShutdown_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalIntTechShutdown_elec" ) ) {
	write_mi_data( L223.GlobalIntTechShutdown_elec, "GlobalIntTechShutdown", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechShutdown_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalTechSCurveProfit_elec" ) ) {
	write_mi_data( L223.GlobalTechSCurveProfit_elec, "GlobalTechSCurveProfit", "ENERGY_LEVEL2_DATA", "L223.GlobalTechSCurveProfit_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalIntTechSCurveProfit_elec" ) ) {
	write_mi_data( L223.GlobalIntTechSCurveProfit_elec, "GlobalIntTechSCurveProfit", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechSCurveProfit_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalTechSCurve_elec" ) ) {
	write_mi_data( L223.GlobalTechSCurve_elec, "GlobalTechSCurve", "ENERGY_LEVEL2_DATA", "L223.GlobalTechSCurve_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalIntTechSCurve_elec" ) ) {
	write_mi_data( L223.GlobalIntTechSCurve_elec, "GlobalIntTechSCurve", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechSCurve_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalTechLifetimeProfit_elec" ) ) {
	write_mi_data( L223.GlobalTechLifetimeProfit_elec, "GlobalTechLifetimeProfit", "ENERGY_LEVEL2_DATA", "L223.GlobalTechLifetimeProfit_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalIntTechLifetimeProfit_elec" ) ) {
	write_mi_data( L223.GlobalIntTechLifetimeProfit_elec, "GlobalIntTechLifetimeProfit", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechLifetimeProfit_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalTechLifetime_elec" ) ) {
	write_mi_data( L223.GlobalTechLifetime_elec, "GlobalTechLifetime", "ENERGY_LEVEL2_DATA", "L223.GlobalTechLifetime_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalIntTechLifetime_elec" ) ) {
	write_mi_data( L223.GlobalIntTechLifetime_elec, "GlobalIntTechLifetime", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechLifetime_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
write_mi_data( L223.StubTechCalInput_elec, "StubTechCalInput", "ENERGY_LEVEL2_DATA", "L223.StubTechCalInput_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.StubTechFixOut_elec, "StubTechFixOut", "ENERGY_LEVEL2_DATA", "L223.StubTechFixOut_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.StubTechProd_elec, "StubTechProd", "ENERGY_LEVEL2_DATA", "L223.StubTechProd_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.StubTechEff_elec, "StubTechEff", "ENERGY_LEVEL2_DATA", "L223.StubTechEff_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_electricity.xml", "ENERGY_XML_FINAL", "electricity.xml", "", xml_tag="outFile" )

logstop()


