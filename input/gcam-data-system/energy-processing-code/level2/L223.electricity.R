
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
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_ccs_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
fuel_energy_input <- readdata( "ENERGY_MAPPINGS", "fuel_energy_input" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
A23.sector <- readdata( "ENERGY_ASSUMPTIONS", "A23.sector" )
A23.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A23.subsector_logit" )
A23.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A23.subsector_shrwt" )
A23.subsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A23.subsector_interp" )
A23.subsector_interp_R <- readdata("ENERGY_ASSUMPTIONS","A23.subsector_interp_R")
A23.subsector_shrwt_nuc_R <- readdata( "ENERGY_ASSUMPTIONS", "A23.subsector_shrwt_nuc_R" )
A23.subsector_shrwt_renew_R <- readdata( "ENERGY_ASSUMPTIONS", "A23.subsector_shrwt_renew_R" )
A23.globalinttech <- readdata( "ENERGY_ASSUMPTIONS", "A23.globalinttech" )
A23.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_shrwt" )
A23.globaltech_interp <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_interp" )
A23.globaltech_keyword <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_keyword" )
A23.globaltech_eff <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_eff" )
A23.globaltech_capacity_factor <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_capacity_factor" )
A23.globaltech_capital <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_capital" )
A23.globaltech_capital_adv <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_capital_adv" )
A23.globaltech_capital_low <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_capital_low" )
A23.globaltech_OMfixed <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_OMfixed" )
A23.globaltech_OMvar <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_OMvar" )
A23.globaltech_retirement <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_retirement" )
A23.globaltech_co2capture <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_co2capture" )
L114.RsrcCurves_EJ_R_wind <- readdata( "ENERGY_LEVEL1_DATA", "L114.RsrcCurves_EJ_R_wind" )
L118.out_EJ_R_elec_hydro_Yfut <- readdata( "ENERGY_LEVEL1_DATA", "L118.out_EJ_R_elec_hydro_Yfut" )
L119.Irradiance_rel_R <- readdata( "ENERGY_LEVEL1_DATA", "L119.Irradiance_rel_R" )
L1231.in_EJ_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.in_EJ_R_elec_F_tech_Yh" )
L1231.out_EJ_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.out_EJ_R_elec_F_tech_Yh" )
L1231.eff_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.eff_R_elec_F_tech_Yh" )
L102.gdp_mil90usd_GCAM3_ctry_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.gdp_mil90usd_GCAM3_ctry_Y" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Supplysector information
printlog( "L223.Supplysector_elec: Supply sector information for electricity sector" )
L223.SectorLogitTables <- get_logit_fn_tables( A23.sector, names_SupplysectorLogitType,
    base.header="Supplysector_", include.equiv.table=T, write.all.regions=T )
L223.Supplysector_elec <- write_to_all_regions( A23.sector, names_Supplysector )

printlog( "L223.ElecReserve: Electricity reserve margin and average grid capacity factor" )
L223.ElecReserve <- write_to_all_regions( A23.sector, names_ElecReserve )

# 2b. Subsector information
printlog( "L223.SubsectorLogit_elec: Subsector logit exponents of electricity sector" )
L223.SubsectorLogitTables <- get_logit_fn_tables( A23.subsector_logit, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=T )
L223.SubsectorLogit_elec <- write_to_all_regions( A23.subsector_logit, names_SubsectorLogit )

printlog( "L223.SubsectorShrwt_elec and L223.SubsectorShrwtFllt_elec: Subsector shareweights of electricity sector" )
if( any( !is.na( A23.subsector_shrwt$year ) ) ){
	L223.SubsectorShrwt_elec <- write_to_all_regions( A23.subsector_shrwt[ !is.na( A23.subsector_shrwt$year ), ], names_SubsectorShrwt )
	}
if( any( !is.na( A23.subsector_shrwt$year.fillout ) ) ){
	L223.SubsectorShrwtFllt_elec <- write_to_all_regions( A23.subsector_shrwt[ !is.na( A23.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
	}

printlog( "L223.SubsectorShrwt_nuc: Subsector shareweights of nuclear electricity" )
# Start out with the list of ISO matched to region_GCAM3
L223.SubsectorShrwt_nuc_ctry <- iso_GCAM_regID[ c( "iso", "region_GCAM3", R ) ]
X_nuc_shrwt_years <- names( A23.subsector_shrwt_nuc_R )[ grepl( "X[0-9]{4}", names( A23.subsector_shrwt_nuc_R ) ) ]
nuc_shrwt_years <- as.numeric( substr( X_nuc_shrwt_years, 2, 5 ) )
L223.SubsectorShrwt_nuc_ctry[ X_nuc_shrwt_years ] <- A23.subsector_shrwt_nuc_R[
      match( L223.SubsectorShrwt_nuc_ctry$region_GCAM3,
             A23.subsector_shrwt_nuc_R$region_GCAM3 ),
      X_nuc_shrwt_years ]

#Where country-level shareweights are provided, use those
L223.SubsectorShrwt_nuc_ctry[ L223.SubsectorShrwt_nuc_ctry$iso %in% A23.subsector_shrwt_nuc_R$iso, X_nuc_shrwt_years ] <- 
      A23.subsector_shrwt_nuc_R[ match( 
         L223.SubsectorShrwt_nuc_ctry$iso[ L223.SubsectorShrwt_nuc_ctry$iso %in% A23.subsector_shrwt_nuc_R$iso ],
         A23.subsector_shrwt_nuc_R$iso ),
      X_nuc_shrwt_years ]
      
# Use GDP by country as a weighting factor in going from country-level shareweights to region-level shareweights
L223.SubsectorShrwt_nuc_ctry$weight <- L102.gdp_mil90usd_GCAM3_ctry_Y[[X_final_historical_year]][
      match( L223.SubsectorShrwt_nuc_ctry$iso, L102.gdp_mil90usd_GCAM3_ctry_Y$iso ) ]
L223.SubsectorShrwt_nuc_ctry <- na.omit( L223.SubsectorShrwt_nuc_ctry )
L223.SubsectorShrwt_nuc_ctry[ X_nuc_shrwt_years ] <-
      L223.SubsectorShrwt_nuc_ctry[ X_nuc_shrwt_years ] * L223.SubsectorShrwt_nuc_ctry$weight
L223.SubsectorShrwt_nuc_R <- aggregate( L223.SubsectorShrwt_nuc_ctry[ c( X_nuc_shrwt_years, "weight" ) ],
      by=as.list( L223.SubsectorShrwt_nuc_ctry[ R ] ), sum )
L223.SubsectorShrwt_nuc_R[ X_nuc_shrwt_years ] <- 
      L223.SubsectorShrwt_nuc_R[ X_nuc_shrwt_years ] / L223.SubsectorShrwt_nuc_R$weight

# Interpolate to model time periods, and add columns specifying the final format
L223.SubsectorShrwt_nuc <- interpolate_and_melt(
      L223.SubsectorShrwt_nuc_R, model_future_years[ model_future_years >= min( nuc_shrwt_years ) & model_future_years <= max( nuc_shrwt_years ) ],
      value.name = "share.weight" )
L223.SubsectorShrwt_nuc[ c( supp, subs ) ] <- A23.subsector_shrwt_nuc_R[ 1, c( supp, subs ) ]
L223.SubsectorShrwt_nuc <- add_region_name( L223.SubsectorShrwt_nuc )[ names_SubsectorShrwt ]

printlog( "L223.SubsectorShrwt_renew: Near term subsector shareweights of renewable technologies" )
#First, melt the table with near-term shareweights from GCAM 3.0 regions
L223.SubsectorShrwt_renew_GCAM3 <- melt( A23.subsector_shrwt_renew_R, id.vars = grep( "X[0-9]{4}", names( A23.subsector_shrwt_renew_R ), invert = T ) )
L223.SubsectorShrwt_renew_GCAM3$year <- substring( L223.SubsectorShrwt_renew_GCAM3$variable, 2, 5 )
names( L223.SubsectorShrwt_renew_GCAM3 )[ names( L223.SubsectorShrwt_renew_GCAM3 ) == "value" ] <- "share.weight"

#Build a table with all combinations of GCAM regions, electricity technologies, and years
L223.SubsectorShrwt_renew <- data.frame(
      GCAM_region_ID = rep( GCAM_region_names[[R]], times = length( unique( L223.SubsectorShrwt_renew_GCAM3$subsector ) ) ),
      supplysector = unique( L223.SubsectorShrwt_renew_GCAM3$supplysector ),
      subsector = sort( rep( unique( L223.SubsectorShrwt_renew_GCAM3$subsector ), times = nrow( GCAM_region_names ) ) ) )
L223.SubsectorShrwt_renew <- repeat_and_add_vector( L223.SubsectorShrwt_renew, Y, unique( L223.SubsectorShrwt_renew_GCAM3$year ) )

printlog( "Using an approximate match between current regions and GCAM 3.0 regions" )
L223.SubsectorShrwt_renew$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L223.SubsectorShrwt_renew[[R]], iso_GCAM_regID[[R]] ) ]
L223.SubsectorShrwt_renew$share.weight <- L223.SubsectorShrwt_renew_GCAM3$share.weight[
      match( vecpaste( L223.SubsectorShrwt_renew[ c( "region_GCAM3", "subsector", "year" ) ] ),
             vecpaste( L223.SubsectorShrwt_renew_GCAM3[ c( "region_GCAM3", "subsector", "year" ) ] ) ) ]
L223.SubsectorShrwt_renew[ is.na( L223.SubsectorShrwt_renew ) ] <- 0
L223.SubsectorShrwt_renew <- add_region_name( L223.SubsectorShrwt_renew )[ names_SubsectorShrwt ]

printlog( "L223.SubsectorInterp_elec and L223.SubsectorInterpTo_elec: Subsector shareweight interpolation of electricity sector" )
if( any( is.na( A23.subsector_interp$to.value ) ) ){
	L223.SubsectorInterp_elec <- write_to_all_regions( A23.subsector_interp[ is.na( A23.subsector_interp$to.value ), ], names_SubsectorInterp )
	}
if( any( !is.na( A23.subsector_interp$to.value ) ) ){
	L223.SubsectorInterpTo_elec <- write_to_all_regions( A23.subsector_interp[ !is.na( A23.subsector_interp$to.value ), ], names_SubsectorInterpTo )
	}

printlog( "Adjust subsector interp rules regionally" )
# Any global interp rules that match by region + sector + subsector name will be replaced by a regionally specific
# interp rule.
L223.SubsectorInterp_elec <- L223.SubsectorInterp_elec[ vecpaste( L223.SubsectorInterp_elec[, names_Subsector ] ) %!in%
                                                        vecpaste( A23.subsector_interp_R[, names_Subsector ] ), ]
L223.SubsectorInterp_elec <- rbind( L223.SubsectorInterp_elec, A23.subsector_interp_R[, names( L223.SubsectorInterp_elec ) ] )
L223.SubsectorInterp_elec <- set_years( L223.SubsectorInterp_elec )
L223.SubsectorInterpTo_elec <- L223.SubsectorInterpTo_elec[ vecpaste( L223.SubsectorInterpTo_elec[, names_Subsector ] ) %!in%
                                                            vecpaste( A23.subsector_interp_R[, names_Subsector ] ), ]
L223.SubsectorInterpTo_elec <- rbind( L223.SubsectorInterpTo_elec,
                                      A23.subsector_interp_R[ !is.na( A23.subsector_interp_R$to.value ), names( L223.SubsectorInterpTo_elec ) ] )
L223.SubsectorInterpTo_elec <- set_years( L223.SubsectorInterpTo_elec )

# 2c. Technology information
printlog( "L223.StubTech_elec: Identification of stub technologies of electricity generation" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L223.StubTech_elec <- write_to_all_regions( A23.globaltech_shrwt, names_Tech )
names( L223.StubTech_elec ) <- names_StubTech

#Efficiencies of global technologies
printlog( "L223.GlobalTechEff_elec: Energy inputs and coefficients of global electricity generation technologies" )
L223.globaltech_eff.melt <- interpolate_and_melt( A23.globaltech_eff, c( model_base_years, model_future_years ), value.name="efficiency", digits = digits_efficiency, rule=3 )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L223.globaltech_eff.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_eff.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechEff_elec <- L223.globaltech_eff.melt[ names_GlobalTechEff ]

L223.GlobalIntTechEff_elec <- subset_inttechs( L223.GlobalTechEff_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechEff_elec <- subset_techs( L223.GlobalTechEff_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

#Capacity factor of global technologies
printlog( "L223.GlobalTechCapFac_elec: Capacity factor of global electricity generation technologies" )
L223.globaltech_capfac.melt <- interpolate_and_melt( A23.globaltech_capacity_factor, c( model_base_years, model_future_years ), value.name="capacity.factor", digits = digits_efficiency, rule=2 )
L223.globaltech_capfac.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_capfac.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechCapFac_elec <- L223.globaltech_capfac.melt[ names_GlobalTechCapFac ]
L223.GlobalIntTechCapFac_elec <- subset_inttechs( L223.GlobalTechCapFac_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechCapFac_elec <- subset_techs( L223.GlobalTechCapFac_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

#Costs of global technologies
printlog( "L223.GlobalTechCapital_elec: Capital costs of global electricity generation technologies" )
L223.globaltech_capital.melt <- interpolate_and_melt( A23.globaltech_capital, c( model_base_years, model_future_years ), value.name="capital.overnight", digits = digits_capital, rule=3 )
L223.globaltech_capital.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_capital.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechCapital_elec <- L223.globaltech_capital.melt[ names_GlobalTechCapital ]
L223.GlobalIntTechCapital_elec <- subset_inttechs( L223.GlobalTechCapital_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechCapital_elec <- subset_techs( L223.GlobalTechCapital_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

#Costs of global technologies - advanced
printlog( "L223.GlobalTechCapital_elec_adv: Capital costs of global electricity generation technologies - advanced case" )
L223.globaltech_capital_adv.melt <- interpolate_and_melt( A23.globaltech_capital_adv, c( model_base_years, model_future_years ), value.name="capital.overnight", digits = digits_capital, rule=3 )
L223.globaltech_capital_adv.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_capital_adv.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechCapital_elec_adv <- L223.globaltech_capital_adv.melt[ names_GlobalTechCapital ]
L223.GlobalIntTechCapital_elec_adv <- subset_inttechs( L223.GlobalTechCapital_elec_adv, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechCapital_elec_adv <- subset_techs( L223.GlobalTechCapital_elec_adv, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

L223.GlobalIntTechCapital_sol_adv <- subset( L223.GlobalIntTechCapital_elec_adv, L223.GlobalIntTechCapital_elec_adv$subsector.name %in% c( "solar", "rooftop_pv") )
L223.GlobalTechCapital_sol_adv <- subset( L223.GlobalTechCapital_elec_adv, L223.GlobalTechCapital_elec_adv$subsector.name %in% c( "solar", "rooftop_pv")) 

L223.GlobalIntTechCapital_wind_adv <- subset( L223.GlobalIntTechCapital_elec_adv, L223.GlobalIntTechCapital_elec_adv$subsector.name == "wind" )
L223.GlobalTechCapital_wind_adv <- subset( L223.GlobalTechCapital_elec_adv, L223.GlobalTechCapital_elec_adv$subsector.name == "wind") 

L223.GlobalTechCapital_geo_adv <- subset( L223.GlobalTechCapital_elec_adv, L223.GlobalTechCapital_elec_adv$subsector.name == "geothermal") 

L223.GlobalTechCapital_nuc_adv <- subset( L223.GlobalTechCapital_elec_adv, L223.GlobalTechCapital_elec_adv$subsector.name == "nuclear") 

#Costs of global technologies - low
printlog( "L223.GlobalTechCapital_elec_low: Capital costs of global electricity generation technologies - lowanced case" )
L223.globaltech_capital_low.melt <- interpolate_and_melt( A23.globaltech_capital_low, c( model_base_years, model_future_years ), value.name="capital.overnight", digits = digits_capital, rule=3 )
L223.globaltech_capital_low.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_capital_low.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechCapital_elec_low <- L223.globaltech_capital_low.melt[ names_GlobalTechCapital ]
L223.GlobalIntTechCapital_elec_low <- subset_inttechs( L223.GlobalTechCapital_elec_low, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechCapital_elec_low <- subset_techs( L223.GlobalTechCapital_elec_low, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

L223.GlobalIntTechCapital_sol_low <- subset( L223.GlobalIntTechCapital_elec_low, L223.GlobalIntTechCapital_elec_low$subsector.name %in% c( "solar", "rooftop_pv") )
L223.GlobalTechCapital_sol_low <- subset( L223.GlobalTechCapital_elec_low, L223.GlobalTechCapital_elec_low$subsector.name %in% c( "solar", "rooftop_pv")) 

L223.GlobalIntTechCapital_wind_low <- subset( L223.GlobalIntTechCapital_elec_low, L223.GlobalIntTechCapital_elec_low$subsector.name == "wind" )
L223.GlobalTechCapital_wind_low <- subset( L223.GlobalTechCapital_elec_low, L223.GlobalTechCapital_elec_low$subsector.name == "wind") 

L223.GlobalTechCapital_geo_low <- subset( L223.GlobalTechCapital_elec_low, L223.GlobalTechCapital_elec_low$subsector.name == "geothermal") 

L223.GlobalTechCapital_nuc_low <- subset( L223.GlobalTechCapital_elec_low, L223.GlobalTechCapital_elec_low$subsector.name == "nuclear") 

printlog( "L223.GlobalTechOMfixed_elec: Fixed O&M costs of global electricity generation technologies" )
L223.globaltech_OMfixed.melt <- interpolate_and_melt( A23.globaltech_OMfixed, c( model_base_years, model_future_years ), value.name="OM.fixed", digits = digits_OM, rule=3 )
L223.globaltech_OMfixed.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_OMfixed.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechOMfixed_elec <- L223.globaltech_OMfixed.melt[ names_GlobalTechOMfixed ]
L223.GlobalIntTechOMfixed_elec <- subset_inttechs( L223.GlobalTechOMfixed_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechOMfixed_elec <- subset_techs( L223.GlobalTechOMfixed_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

printlog( "L223.GlobalTechOMvar_elec: Variable O&M costs of global electricity generation technologies" )
L223.globaltech_OMvar.melt <- interpolate_and_melt( A23.globaltech_OMvar, c( model_base_years, model_future_years ), value.name="OM.var", digits = digits_OM, rule=3 )
L223.globaltech_OMvar.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_OMvar.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechOMvar_elec <- L223.globaltech_OMvar.melt[ names_GlobalTechOMvar ]
L223.GlobalIntTechOMvar_elec <- subset_inttechs( L223.GlobalTechOMvar_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechOMvar_elec <- subset_techs( L223.GlobalTechOMvar_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

#Shareweights of global technologies
printlog( "L223.GlobalTechShrwt_elec: Shareweights of global electricity generation technologies" )
L223.globaltech_shrwt.melt <- interpolate_and_melt( A23.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L223.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechShrwt_elec <- L223.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]
L223.GlobalIntTechShrwt_elec <- subset_inttechs( L223.GlobalTechShrwt_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.GlobalTechShrwt_elec <- subset_techs( L223.GlobalTechShrwt_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

#Interpolation rules
#TODO: we should subset_inttechs only at the moment there are none
L223.GlobalTechInterp_elec <- set_years( A23.globaltech_interp )
names( L223.GlobalTechInterp_elec )[ names( L223.GlobalTechInterp_elec ) %in% c( "supplysector", "subsector" ) ] <- c( "sector.name", "subsector.name" )

printlog( "L223.PrimaryRenewKeyword_elec: Keywords of primary renewable electric generation technologies" )
L223.AllKeyword_elec <- repeat_and_add_vector( A23.globaltech_keyword, Y, c( model_base_years, model_future_years ) )
L223.AllKeyword_elec[ c( "sector.name", "subsector.name" ) ] <- L223.AllKeyword_elec[ c( "supplysector", "subsector" ) ]
L223.PrimaryRenewKeyword_elec <- L223.AllKeyword_elec[ !is.na( L223.AllKeyword_elec$primary.renewable ), c( names_GlobalTechYr, "primary.renewable" ) ]
L223.PrimaryRenewKeywordInt_elec <- subset_inttechs( L223.PrimaryRenewKeyword_elec,
      inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
L223.PrimaryRenewKeyword_elec <- subset_techs( L223.PrimaryRenewKeyword_elec,
      inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )

printlog( "L223.AvgFossilEffKeyword_elec: Keywords of fossil/bio electric generation technologies" )
L223.AvgFossilEffKeyword_elec <- L223.AllKeyword_elec[ !is.na( L223.AllKeyword_elec$average.fossil.efficiency ),
      c( names_GlobalTechYr, "average.fossil.efficiency" ) ]

#CO2 capture rates of global technologies
printlog( "L223.GlobalTechCapture_elec: CO2 capture fractions from global electricity generation technologies" )
## No need to consider historical periods or intermittent technologies here
L223.globaltech_co2capture.melt <- interpolate_and_melt( A23.globaltech_co2capture, model_future_years, value.name="remove.fraction", digits = digits_remove.fraction )
L223.globaltech_co2capture.melt[ c( "sector.name", "subsector.name" ) ] <- L223.globaltech_co2capture.melt[ c( "supplysector", "subsector" ) ]
L223.GlobalTechCapture_elec <- L223.globaltech_co2capture.melt[ c( names_GlobalTechYr, "remove.fraction" ) ]
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
      repeat_and_add_vector( subset( L223.globaltech_retirement, year > max(model_base_years ) ), "year", model_future_years ) )

#Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
# All of these options have different headers, and all are allowed
if( any( !is.na( L223.globaltech_retirement$shutdown.rate ) ) ){
	printlog( "L223.GlobalTechShutdown_elec: Global tech lifetime and shutdown rate" )
	L223.GlobalTechShutdown_elec <- L223.globaltech_retirement[
	     !is.na( L223.globaltech_retirement$shutdown.rate ),
	      c( names_GlobalTechYr, "lifetime", "shutdown.rate") ]
	L223.GlobalIntTechShutdown_elec <- subset_inttechs( L223.GlobalTechShutdown_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalIntTechShutdown_elec ) == 0 ) rm( L223.GlobalIntTechShutdown_elec )
	L223.GlobalTechShutdown_elec <- subset_techs( L223.GlobalTechShutdown_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalTechShutdown_elec ) == 0 ) rm( L223.GlobalTechShutdown_elec )
	}
if( any( !is.na( L223.globaltech_retirement$half.life ) ) ){
	printlog( "L223.GlobalTechSCurve_elec: Global tech lifetime and s-curve retirement function" )
	L223.GlobalTechSCurve_elec <- L223.globaltech_retirement[
	     !is.na( L223.globaltech_retirement$half.life ),
	      c( names_GlobalTechYr, "lifetime", "steepness", "half.life" ) ]
	L223.GlobalIntTechSCurve_elec <- subset_inttechs( L223.GlobalTechSCurve_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalIntTechSCurve_elec ) == 0 ) rm( L223.GlobalIntTechSCurve_elec )
	L223.GlobalTechSCurve_elec <- subset_techs( L223.GlobalTechSCurve_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalTechSCurve_elec ) == 0 ) rm( L223.GlobalTechSCurve_elec )
	}
if( any( is.na( L223.globaltech_retirement$shutdown.rate ) & is.na( L223.globaltech_retirement$half.life ) ) ){
	printlog( "L223.GlobalTechLifetime_elec: Global tech lifetime" )
	L223.GlobalTechLifetime_elec <- L223.globaltech_retirement[
	     is.na( L223.globaltech_retirement$shutdown.rate ) & is.na( L223.globaltech_retirement$half.life ),
	      c( names_GlobalTechYr, "lifetime" ) ]
	L223.GlobalIntTechLifetime_elec <- subset_inttechs( L223.GlobalTechLifetime_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalIntTechLifetime_elec ) == 0 ) rm( L223.GlobalIntTechLifetime_elec )
	L223.GlobalTechLifetime_elec <- subset_techs( L223.GlobalTechLifetime_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalTechLifetime_elec ) == 0 ) rm( L223.GlobalTechLifetime_elec )
	}
if( any( !is.na( L223.globaltech_retirement$median.shutdown.point ) ) ){
	printlog( "L223.GlobalTechProfitShutdown_elec: Global tech lifetime and shutdown rate" )
	L223.GlobalTechProfitShutdown_elec <- L223.globaltech_retirement[
	     !is.na( L223.globaltech_retirement$median.shutdown.point ),
	      c( names_GlobalTechYr, "median.shutdown.point", "profit.shutdown.steepness") ]
	L223.GlobalIntTechProfitShutdown_elec <- subset_inttechs( L223.GlobalTechProfitShutdown_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalIntTechProfitShutdown_elec ) == 0 ) rm( L223.GlobalIntTechProfitShutdown_elec )
	L223.GlobalTechProfitShutdown_elec <- subset_techs( L223.GlobalTechProfitShutdown_elec, inttech.table = A23.globalinttech, sector.name="sector.name", subsector.name="subsector.name" )
	if( nrow( L223.GlobalTechProfitShutdown_elec ) == 0 ) rm( L223.GlobalTechProfitShutdown_elec )
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

printlog( "NOTE: Fixed output is assumed to apply in all historical years, regardless of final calibration year")
L223.out_EJ_R_elec_F_tech_Yh <- interpolate_and_melt( L1231.out_EJ_R_elec_F_tech_Yh, model_years[ model_years %in% historical_years ] )
L223.out_EJ_R_elec_F_tech_Yh <- add_region_name( L223.out_EJ_R_elec_F_tech_Yh )
L223.out_EJ_R_elec_F_tech_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( paste( L223.out_EJ_R_elec_F_tech_Yh$sector, L223.out_EJ_R_elec_F_tech_Yh$fuel, L223.out_EJ_R_elec_F_tech_Yh$technology ),
             paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ),
      c( "supplysector", "subsector", "technology" ) ]
L223.fixout_EJ_R_elec_F_tech_Yh <- subset( L223.out_EJ_R_elec_F_tech_Yh, calibrated_techs$calibration[
      match( paste( sector, fuel, technology ), paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ) ] == "fixed output" )
L223.calout_EJ_R_elec_F_tech_Yh <- subset( L223.out_EJ_R_elec_F_tech_Yh,
      calibrated_techs$calibration[
          match( paste( sector, fuel, technology ), paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ) ] == "output" &
          year %in% model_base_years )

printlog( "L223.StubTechCalInput_elec: calibrated input of electricity generation technologies")
#Note that there is no need to specify which stub technologies are intermittent
L223.StubTechCalInput_elec <- L223.in_EJ_R_elec_F_tech_Yh[ names_StubTechYr ]
L223.StubTechCalInput_elec$minicam.energy.input <- A23.globaltech_eff$minicam.energy.input[ 
      match( paste( L223.StubTechCalInput_elec$subsector, L223.StubTechCalInput_elec$stub.technology ),
             paste( A23.globaltech_eff$subsector, A23.globaltech_eff$technology ) ) ]
L223.StubTechCalInput_elec$calibrated.value <- round( L223.in_EJ_R_elec_F_tech_Yh$value, digits_calOutput )
L223.StubTechCalInput_elec$share.weight.year <- L223.StubTechCalInput_elec$year
L223.StubTechCalInput_elec <- set_subsector_shrwt( L223.StubTechCalInput_elec, value.name = "calibrated.value" )
L223.StubTechCalInput_elec$tech.share.weight <- ifelse( L223.StubTechCalInput_elec$calibrated.value > 0, 1, 0 )

printlog( "L223.StubTechFixOut_elec: fixed output of electricity generation technologies")
L223.StubTechFixOut_elec <- L223.fixout_EJ_R_elec_F_tech_Yh[ names_StubTechYr ]
L223.StubTechFixOut_elec$fixedOutput <- round( L223.fixout_EJ_R_elec_F_tech_Yh$value, digits_calOutput )
L223.StubTechFixOut_elec$share.weight.year <- L223.StubTechFixOut_elec$year
L223.StubTechFixOut_elec$subsector.share.weight <- 0
L223.StubTechFixOut_elec$share.weight <- 0

#Adding in future hydropower generation here
printlog( "L223.StubTechFixOut_hydro: fixed output of future hydropower")
L223.StubTechFixOut_hydro <- interpolate_and_melt( L118.out_EJ_R_elec_hydro_Yfut,
      model_future_years[ !model_future_years %in% historical_years ], value = "fixedOutput",
      digits = digits_calOutput )
L223.StubTechFixOut_hydro <- add_region_name( L223.StubTechFixOut_hydro )
L223.StubTechFixOut_hydro[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( paste( L223.StubTechFixOut_hydro$sector, L223.StubTechFixOut_hydro$fuel ),
             paste( calibrated_techs$sector, calibrated_techs$fuel ) ),
      c( "supplysector", "subsector", "technology" ) ]
L223.StubTechFixOut_hydro$share.weight.year <- L223.StubTechFixOut_hydro$year
L223.StubTechFixOut_hydro$subs.share.weight <- 0
L223.StubTechFixOut_hydro$tech.share.weight <- 0
L223.StubTechFixOut_hydro <- L223.StubTechFixOut_hydro[ names_StubTechFixOut ]

printlog( "L223.StubTechProd_elec: calibrated output of electricity generation technologies" )
L223.StubTechProd_elec <- L223.calout_EJ_R_elec_F_tech_Yh[ names_StubTechYr ]
L223.StubTechProd_elec$calOutputValue <- round( L223.calout_EJ_R_elec_F_tech_Yh$value, digits_calOutput )
L223.StubTechProd_elec$share.weight.year <- L223.StubTechProd_elec$year
L223.StubTechProd_elec <- set_subsector_shrwt( L223.StubTechProd_elec, value.name="calOutputValue" )
L223.StubTechProd_elec$share.weight <- ifelse( L223.StubTechProd_elec$calOutputValue > 0, 1, 0 )

printlog( "L223.StubTechEff_elec: calibrated efficiencies of electricity generation technologies" )
printlog( "NOTE: Electric sector efficiencies are assumed to apply for all historical years, regardless of final calibration year" )
L223.eff_R_elec_F_tech_Yh <- interpolate_and_melt( L1231.eff_R_elec_F_tech_Yh, model_years[ model_years %in% historical_years ] )
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

# Regional adjustments to wind to include a "base price" for the wind resource supply
# We will have these total levelized cost reconsile by adjusting the capacity factor
printlog("L223.StubTechCapFactor_elec: regional adjustments to wind capacity factors")
L223.StubTechCapFactor_elec <- L114.RsrcCurves_EJ_R_wind[, c( "GCAM_region_ID", "base.price" ) ]
# Gather the global assumptions for wind cost in the base year from which we will make
# regional adjustments
L223.names_GlobalIntTechCapital <- names_GlobalTechCapital[ !( names_GlobalTechCapital %in% c( "year" ) ) ]
L223.names_StubTechCapital <- L223.names_GlobalIntTechCapital
L223.names_StubTechCapital[ L223.names_StubTechCapital %in% names_GlobalTech ] <- names_StubTech[ names_StubTech != "region" ]
L223.names_GlobalIntTechCapital <- sub( "technology", "intermittent.technology", L223.names_GlobalIntTechCapital )
L223.StubTechCapFactor_elec[, L223.names_StubTechCapital ] <-
    subset( L223.GlobalIntTechCapital_elec, intermittent.technology == "wind" & year == wind_base_cost_year,
        select=L223.names_GlobalIntTechCapital )
L223.StubTechCapFactor_elec[, c( "OM.var" ) ] <-
    subset( L223.GlobalIntTechOMvar_elec, intermittent.technology == "wind" & year == wind_base_cost_year,
        select=c( "OM.var" ) )
L223.StubTechCapFactor_elec[, c( "input.OM.fixed", "OM.fixed" ) ] <-
    subset( L223.GlobalIntTechOMfixed_elec, intermittent.technology == "wind" & year == wind_base_cost_year,
        select=c( "input.OM.fixed", "OM.fixed" ) )
# Calculate a new capacity factor to match the regional base.price
L223.StubTechCapFactor_elec$capacity.factor <- with( L223.StubTechCapFactor_elec, ( capital.overnight * fixed.charge.rate +
    OM.fixed ) / ( conv_kwh_GJ * conv_year_hours ) / ( base.price - ( OM.var / ( 1000 * conv_kwh_GJ ) ) ) )
L223.StubTechCapFactor_elec$capacity.factor <- round( L223.StubTechCapFactor_elec$capacity.factor, digits_capacity_factor )
L223.StubTechCapFactor_elec <- add_region_name( L223.StubTechCapFactor_elec )
# This fixes the capacity factor for all future years and is inconsistent if future capacity factors are assumed to change.
L223.StubTechCapFactor_elec <- repeat_and_add_vector( L223.StubTechCapFactor_elec, Y, model_years )
L223.StubTechCapFactor_elec <- L223.StubTechCapFactor_elec[, names_StubTechCapFactor ]
# The same capacity factor should apply to wind with storage as well.
L223.StubTechCapFactor_elec.storage <- L223.StubTechCapFactor_elec
L223.StubTechCapFactor_elec.storage$stub.technology <- "wind_storage"
L223.StubTechCapFactor_elec <- rbind( L223.StubTechCapFactor_elec, L223.StubTechCapFactor_elec.storage )

# Regional adjustment for solar capacity factors.  We will use relative total and direct irradiance
# to scale the capacity factors for central PV and CSP, and distributed rooftop PV respectively.
printlog( "Regional capacity factor adjustment for solar technologies" )
L223.StubTechCapFactor_solar <- subset( L223.GlobalIntTechCapFac_elec, subsector.name %in% c( "solar", "rooftop_pv" ) )
# get PV_storage and CSP_storage capacity factors to adjust
L223.StubTechCapFactor_solar_storage <- subset( L223.GlobalTechCapFac_elec, subsector.name %in% c( "solar" ) )
names( L223.StubTechCapFactor_solar_storage )[ names( L223.StubTechCapFactor_solar_storage ) == "technology" ] <- "intermittent.technology"
L223.StubTechCapFactor_solar <- rbind(L223.StubTechCapFactor_solar, L223.StubTechCapFactor_solar_storage)
L223.StubTechCapFactor_solar <- merge( L223.StubTechCapFactor_solar, L119.Irradiance_rel_R )
L223.StubTechCapFactor_solar[ grep( 'PV', L223.StubTechCapFactor_solar$intermittent.technology, ignore.case=TRUE ), c( "capacity.factor" ) ] <-
    L223.StubTechCapFactor_solar[ grep( 'PV', L223.StubTechCapFactor_solar$intermittent.technology, ignore.case=TRUE ), c( "capacity.factor" ) ] * 
    L223.StubTechCapFactor_solar[ grep( 'PV', L223.StubTechCapFactor_solar$intermittent.technology, ignore.case=TRUE ), "irradiance_avg_rel" ]
L223.StubTechCapFactor_solar[ grep( 'CSP', L223.StubTechCapFactor_solar$intermittent.technology ), c( "capacity.factor" ) ] <-
    L223.StubTechCapFactor_solar[ grep( 'CSP', L223.StubTechCapFactor_solar$intermittent.technology ), c( "capacity.factor" ) ] * 
    L223.StubTechCapFactor_solar[ grep( 'CSP', L223.StubTechCapFactor_solar$intermittent.technology ), "dni_avg_rel" ]
names( L223.StubTechCapFactor_solar )[ names( L223.StubTechCapFactor_solar ) == "intermittent.technology" ] <- "stub.technology"
L223.StubTechCapFactor_solar[ L223.StubTechCapFactor_solar$capacity.factor > 0.85, "capacity.factor" ] <- 0.85
L223.StubTechCapFactor_solar <- add_region_name( L223.StubTechCapFactor_solar )
names(L223.StubTechCapFactor_solar)[names(L223.StubTechCapFactor_solar) %in% c( "sector.name", "subsector.name" )] <- c( "supplysector", "subsector" )
L223.StubTechCapFactor_elec <- rbind( L223.StubTechCapFactor_elec,
    L223.StubTechCapFactor_solar[, names( L223.StubTechCapFactor_elec ) ] )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table in names ( L223.SectorLogitTables) ) {
write_mi_data( L223.SectorLogitTables[[ curr_table ]]$data, L223.SectorLogitTables[[ curr_table ]]$header,
    "ENERGY_LEVEL2_DATA", paste0("L223.", L223.SectorLogitTables[[ curr_table ]]$header ), "ENERGY_XML_BATCH",
    "batch_electricity.xml" )
}
write_mi_data( L223.Supplysector_elec, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L223.Supplysector_elec",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_electricity.xml" ) 
write_mi_data( L223.ElecReserve, "ElecReserve", "ENERGY_LEVEL2_DATA", "L223.ElecReserve", "ENERGY_XML_BATCH", "batch_electricity.xml" ) 
for( curr_table in names ( L223.SubsectorLogitTables ) ) {
write_mi_data( L223.SubsectorLogitTables[[ curr_table ]]$data, L223.SubsectorLogitTables[[ curr_table ]]$header,
    "ENERGY_LEVEL2_DATA", paste0("L223.", L223.SubsectorLogitTables[[ curr_table ]]$header ), "ENERGY_XML_BATCH",
    "batch_electricity.xml" )
}
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
write_mi_data( L223.GlobalTechCapFac_elec, "GlobalTechCapFac", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapFac_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalIntTechCapFac_elec, "GlobalIntTechCapFac", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechCapFac_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalTechCapital_elec, "GlobalTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalIntTechCapital_elec, "GlobalIntTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechCapital_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalTechOMfixed_elec, "GlobalTechOMfixed", "ENERGY_LEVEL2_DATA", "L223.GlobalTechOMfixed_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalIntTechOMfixed_elec, "GlobalIntTechOMfixed", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechOMfixed_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalTechOMvar_elec, "GlobalTechOMvar", "ENERGY_LEVEL2_DATA", "L223.GlobalTechOMvar_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalIntTechOMvar_elec, "GlobalIntTechOMvar", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechOMvar_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalTechShrwt_elec, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L223.GlobalTechShrwt_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalTechInterp_elec, "GlobalTechInterp", "ENERGY_LEVEL2_DATA", "L223.GlobalTechInterp_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalIntTechShrwt_elec, "GlobalIntTechShrwt", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechShrwt_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.PrimaryRenewKeyword_elec, "PrimaryRenewKeyword", "ENERGY_LEVEL2_DATA", "L223.PrimaryRenewKeyword_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.PrimaryRenewKeywordInt_elec, "PrimaryRenewKeywordInt", "ENERGY_LEVEL2_DATA", "L223.PrimaryRenewKeywordInt_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.AvgFossilEffKeyword_elec, "AvgFossilEffKeyword", "ENERGY_LEVEL2_DATA", "L223.AvgFossilEffKeyword_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalTechCapture_elec, "GlobalTechCapture", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapture_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.GlobalIntTechBackup_elec, "GlobalIntTechBackup", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechBackup_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.StubTechCapFactor_elec, "StubTechCapFactor", "ENERGY_LEVEL2_DATA", "L223.StubTechCapFactor_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
if( exists( "L223.GlobalTechShutdown_elec" ) ) {
	write_mi_data( L223.GlobalTechShutdown_elec, "GlobalTechShutdown", "ENERGY_LEVEL2_DATA", "L223.GlobalTechShutdown_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalIntTechShutdown_elec" ) ) {
	write_mi_data( L223.GlobalIntTechShutdown_elec, "GlobalIntTechShutdown", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechShutdown_elec",
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
if( exists( "L223.GlobalTechLifetime_elec" ) ) {
	write_mi_data( L223.GlobalTechLifetime_elec, "GlobalTechLifetime", "ENERGY_LEVEL2_DATA", "L223.GlobalTechLifetime_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalIntTechLifetime_elec" ) ) {
	write_mi_data( L223.GlobalIntTechLifetime_elec, "GlobalIntTechLifetime", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechLifetime_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalTechProfitShutdown_elec" ) ) {
	write_mi_data( L223.GlobalTechProfitShutdown_elec, "GlobalTechProfitShutdown", "ENERGY_LEVEL2_DATA", "L223.GlobalTechProfitShutdown_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
if( exists( "L223.GlobalIntTechProfitShutdown_elec" ) ) {
	write_mi_data( L223.GlobalIntTechProfitShutdown_elec, "GlobalIntTechProfitShutdown", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechProfitShutdown_elec",
	               "ENERGY_XML_BATCH", "batch_electricity.xml" )
	}
write_mi_data( L223.StubTechCalInput_elec, "StubTechCalInput", "ENERGY_LEVEL2_DATA", "L223.StubTechCalInput_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.StubTechFixOut_elec, "StubTechFixOut", "ENERGY_LEVEL2_DATA", "L223.StubTechFixOut_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.StubTechFixOut_hydro, "StubTechFixOut", "ENERGY_LEVEL2_DATA", "L223.StubTechFixOut_hydro", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.StubTechProd_elec, "StubTechProd", "ENERGY_LEVEL2_DATA", "L223.StubTechProd_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )
write_mi_data( L223.StubTechEff_elec, "StubTechEff", "ENERGY_LEVEL2_DATA", "L223.StubTechEff_elec", "ENERGY_XML_BATCH", "batch_electricity.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_electricity.xml", "ENERGY_XML_FINAL", "electricity.xml", "", xml_tag="outFile" )

write_mi_data( L223.GlobalTechCapital_sol_adv, "GlobalTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_sol_adv", "ENERGY_XML_BATCH", "batch_solar_adv.xml" )
write_mi_data( L223.GlobalIntTechCapital_sol_adv, "GlobalIntTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechCapital_sol_adv", "ENERGY_XML_BATCH", "batch_solar_adv.xml" )
insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_solar_adv.xml", "ENERGY_XML_FINAL", "solar_adv.xml", "", xml_tag="outFile" )

write_mi_data( L223.GlobalTechCapital_wind_adv, "GlobalTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_wind_adv", "ENERGY_XML_BATCH", "batch_wind_adv.xml" )
write_mi_data( L223.GlobalIntTechCapital_wind_adv, "GlobalIntTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechCapital_wind_adv", "ENERGY_XML_BATCH", "batch_wind_adv.xml" )
insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_wind_adv.xml", "ENERGY_XML_FINAL", "wind_adv.xml", "", xml_tag="outFile" )

write_mi_data( L223.GlobalTechCapital_geo_adv, "GlobalTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_geo_adv", "ENERGY_XML_BATCH", "batch_geo_tech_adv.xml" )
insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_geo_tech_adv.xml", "ENERGY_XML_FINAL", "geo_tech_adv.xml", "", xml_tag="outFile" )

write_mi_data( L223.GlobalTechCapital_nuc_adv, "GlobalTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_nuc_adv", "ENERGY_XML_BATCH", "batch_nuclear_adv.xml" )
insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_nuclear_adv.xml", "ENERGY_XML_FINAL", "nuclear_adv.xml", "", xml_tag="outFile" )

write_mi_data( L223.GlobalTechCapital_sol_low, "GlobalTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_sol_low", "ENERGY_XML_BATCH", "batch_solar_low.xml" )
write_mi_data( L223.GlobalIntTechCapital_sol_low, "GlobalIntTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechCapital_sol_low", "ENERGY_XML_BATCH", "batch_solar_low.xml" )
insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_solar_low.xml", "ENERGY_XML_FINAL", "solar_low.xml", "", xml_tag="outFile" )

write_mi_data( L223.GlobalTechCapital_wind_low, "GlobalTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_wind_low", "ENERGY_XML_BATCH", "batch_wind_low.xml" )
write_mi_data( L223.GlobalIntTechCapital_wind_low, "GlobalIntTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalIntTechCapital_wind_low", "ENERGY_XML_BATCH", "batch_wind_low.xml" )
insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_wind_low.xml", "ENERGY_XML_FINAL", "wind_low.xml", "", xml_tag="outFile" )

write_mi_data( L223.GlobalTechCapital_geo_low, "GlobalTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_geo_low", "ENERGY_XML_BATCH", "batch_geo_low.xml" )
insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_geo_low.xml", "ENERGY_XML_FINAL", "geo_low.xml", "", xml_tag="outFile" )

write_mi_data( L223.GlobalTechCapital_nuc_low, "GlobalTechCapital", "ENERGY_LEVEL2_DATA", "L223.GlobalTechCapital_nuc_low", "ENERGY_XML_BATCH", "batch_nuclear_low.xml" )
insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_nuclear_low.xml", "ENERGY_XML_FINAL", "nuclear_low.xml", "", xml_tag="outFile" )

logstop()


