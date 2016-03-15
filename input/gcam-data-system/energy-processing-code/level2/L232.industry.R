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
logstart( "L232.industry.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Model input for aggregate industrial sectors" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_ind_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
fuel_energy_input <- readdata( "ENERGY_MAPPINGS", "fuel_energy_input" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
A23.chp_elecratio <- readdata( "ENERGY_ASSUMPTIONS", "A23.chp_elecratio" )
A32.sector <- readdata( "ENERGY_ASSUMPTIONS", "A32.sector" )
A32.subsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A32.subsector_interp" )
A32.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A32.subsector_logit" )
A32.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A32.subsector_shrwt" )
A32.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A32.globaltech_coef" )
A32.globaltech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A32.globaltech_cost" )
A32.globaltech_eff <- readdata( "ENERGY_ASSUMPTIONS", "A32.globaltech_eff" )
A32.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A32.globaltech_shrwt" )
A32.globaltech_interp <- readdata( "ENERGY_ASSUMPTIONS", "A32.globaltech_interp" )
A32.nonenergy_Cseq <- readdata( "ENERGY_ASSUMPTIONS", "A32.nonenergy_Cseq" )
A32.fuelprefElasticity <- readdata( "ENERGY_ASSUMPTIONS", "A32.fuelprefElasticity" )
#####A32.aeei <- readdata( "ENERGY_ASSUMPTIONS", "A32.aeei" ) 
A32.demand <- readdata( "ENERGY_ASSUMPTIONS", "A32.demand" )
L123.in_EJ_R_indchp_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.in_EJ_R_indchp_F_Yh")
L1322.in_EJ_R_indenergy_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1322.in_EJ_R_indenergy_F_Yh" )
L1322.in_EJ_R_indfeed_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1322.in_EJ_R_indfeed_F_Yh" )
A32.inc_elas_output <- readdata( "SOCIO_ASSUMPTIONS", "A32.inc_elas_output" )
L101.Pop_thous_GCAM3_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_GCAM3_R_Y" )
L102.pcgdp_thous90USD_GCAM3_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_GCAM3_R_Y" )
L102.pcgdp_thous90USD_Scen_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_Scen_R_Y" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
L232.heat_techs <- unique( calibrated_techs[ grepl( "industry", calibrated_techs$sector ) & calibrated_techs$fuel == "heat", s_s_t ] )
L232.rm_heat_techs_R <- repeat_and_add_vector( L232.heat_techs, R, A_regions[[R]][ A_regions$heat == 0 ] )
L232.rm_heat_techs_R <- add_region_name( L232.rm_heat_techs_R )

# 2a. Supplysector information
printlog( "L232.Supplysector_ind: Supply sector information for industry sector" )
L232.SectorLogitTables <- get_logit_fn_tables( A32.sector, names_SupplysectorLogitType,
    base.header="Supplysector_", include.equiv.table=T, write.all.regions=T )
L232.Supplysector_ind <- write_to_all_regions( A32.sector, names_Supplysector )

printlog( "L232.FinalEnergyKeyword_ind: Supply sector keywords for industry sector" )
L232.FinalEnergyKeyword_ind <- na.omit( write_to_all_regions( A32.sector, names_FinalEnergyKeyword ) )

# 2b. Subsector information
printlog( "L232.SubsectorLogit_ind: Subsector logit exponents of industry sector" )
L232.SubsectorLogitTables <- get_logit_fn_tables( A32.subsector_logit, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=T )
for( curr_table in names( L232.SubsectorLogitTables ) ) {
    if( curr_table != "EQUIV_TABLE" ) {
        L232.SubsectorLogitTables[[ curr_table ]]$data <- L232.SubsectorLogitTables[[ curr_table ]]$data[
            vecpaste( L232.SubsectorLogitTables[[ curr_table ]]$data[ c( "region", "subsector" ) ] ) %!in%
            vecpaste( L232.rm_heat_techs_R[ c( "region", "subsector" ) ] ), ]
    }
}
L232.SubsectorLogit_ind <- write_to_all_regions( A32.subsector_logit, names_SubsectorLogit )
#Remove non-existent heat subsectors from each region
L232.SubsectorLogit_ind <- L232.SubsectorLogit_ind[
      vecpaste( L232.SubsectorLogit_ind[ c( "region", "subsector" ) ] ) %!in% vecpaste( L232.rm_heat_techs_R[ c( "region", "subsector" ) ] ), ]

printlog( "L232.SubsectorShrwt_ind and L232.SubsectorShrwtFllt_ind: Subsector shareweights of industry sector" )
if( any( !is.na( A32.subsector_shrwt$year ) ) ){
	L232.SubsectorShrwt_ind <- write_to_all_regions( A32.subsector_shrwt[ !is.na( A32.subsector_shrwt$year ), ], names_SubsectorShrwt )
	#Remove non-existent heat subsectors from each region
	L232.SubsectorShrwt_ind <- L232.SubsectorShrwt_ind[
      vecpaste( L232.SubsectorShrwt_ind[ c( "region", "subsector" ) ] ) %!in% vecpaste( L232.rm_heat_techs_R[ c( "region", "subsector" ) ] ), ]
	}
if( any( !is.na( A32.subsector_shrwt$year.fillout ) ) ){
	L232.SubsectorShrwtFllt_ind <- write_to_all_regions( A32.subsector_shrwt[ !is.na( A32.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
	#Remove non-existent heat technologies from each region
	L232.SubsectorShrwtFllt_ind <- L232.SubsectorShrwtFllt_ind[
      vecpaste( L232.SubsectorShrwtFllt_ind[ c( "region", "subsector" ) ] ) %!in% vecpaste( L232.rm_heat_techs_R[ c( "region", "subsector" ) ] ), ]
	}

printlog( "L232.SubsectorInterp_ind and L232.SubsectorInterpTo_ind: Subsector shareweight interpolation of industry sector" )
if( any( is.na( A32.subsector_interp$to.value ) ) ){
	L232.SubsectorInterp_ind <- write_to_all_regions( A32.subsector_interp[ is.na( A32.subsector_interp$to.value ), ], names_SubsectorInterp )
	#Remove non-existent heat technologies from each region
	L232.SubsectorInterp_ind <- L232.SubsectorInterp_ind[
      vecpaste( L232.SubsectorInterp_ind[ c( "region", "subsector" ) ] ) %!in% vecpaste( L232.rm_heat_techs_R[ c( "region", "subsector" ) ] ), ]
	}
if( any( !is.na( A32.subsector_interp$to.value ) ) ){
	L232.SubsectorInterpTo_ind <- write_to_all_regions( A32.subsector_interp[ !is.na( A32.subsector_interp$to.value ), ], names_SubsectorInterpTo )
	#Remove non-existent heat technologies from each region
	L232.SubsectorInterpTo_ind <- L232.SubsectorInterpTo_ind[
      vecpaste( L232.SubsectorInterpTo_ind[ c( "region", "subsector" ) ] ) %!in% vecpaste( L232.rm_heat_techs_R[ c( "region", "subsector" ) ] ), ]
	}

# 2c. Technology information
printlog( "L232.StubTech_ind: Identification of stub technologies of industrial sector" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L232.StubTech_ind <- write_to_all_regions( A32.globaltech_shrwt, names_Tech )
names( L232.StubTech_ind ) <- names_StubTech
#Remove non-existent heat technologies from each region
L232.StubTech_ind <- L232.StubTech_ind[
      vecpaste( L232.StubTech_ind[ c( "region", "stub.technology" ) ] ) %!in% vecpaste( L232.rm_heat_techs_R[ c( "region", "technology" ) ] ), ]

printlog( "L232.GlobalTechShrwt_ind: Shareweights of global industrial sector technologies" )
L232.globaltech_shrwt.melt <- interpolate_and_melt( A32.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L232.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L232.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L232.GlobalTechShrwt_ind <- L232.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]

printlog( "L232.StubTechInterp_ind: Shareweight interpolation of global industrial sector technologies" )
L232.StubTechInterp_ind <- write_to_all_regions( A32.globaltech_interp, names_TechInterp )
names( L232.StubTechInterp_ind )[ names( L232.StubTechInterp_ind ) == "technology" ] <- "stub.technology"

printlog( "L232.GlobalTechEff_ind: Energy inputs and coefficients of global industrial energy use and feedstocks technologies" )
L232.globaltech_eff.melt <- interpolate_and_melt( A32.globaltech_eff, c( model_base_years, model_future_years ), value.name="efficiency", digits = digits_efficiency )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L232.globaltech_eff.melt[ c( "sector.name", "subsector.name" ) ] <- L232.globaltech_eff.melt[ c( "supplysector", "subsector" ) ]
L232.GlobalTechEff_ind <- L232.globaltech_eff.melt[ names_GlobalTechEff ]

#Coefficients on global industry sector technologies (not energy-use or feedstocks)
printlog( "L232.GlobalTechCoef_ind: Energy inputs and coefficients of global industry technologies" )
L232.globaltech_coef.melt <- interpolate_and_melt( A32.globaltech_coef, c( model_base_years, model_future_years ), value.name="coefficient" )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L232.globaltech_coef.melt[ c( "sector.name", "subsector.name" ) ] <- L232.globaltech_coef.melt[ c( "supplysector", "subsector" ) ]
L232.GlobalTechCoef_ind <- L232.globaltech_coef.melt[ names_GlobalTechCoef ]

#Secondary outputs of cogen technologies: these are input as a ratio
printlog( "L232.GlobalTechSecOut_ind: Secondary output ratios of industrial cogeneration technologies" )
L232.GlobalTechSecOut_ind <- subset( L232.globaltech_eff.melt, !is.na( secondary.output ) )
L232.GlobalTechSecOut_ind$elec_ratio <- A23.chp_elecratio$elec_ratio[ match( L232.GlobalTechSecOut_ind$subsector, A23.chp_elecratio$fuel ) ]
L232.GlobalTechSecOut_ind$output.ratio <- round( L232.GlobalTechSecOut_ind$elec_ratio / L232.GlobalTechSecOut_ind$efficiency, digits_efficiency )

#NOTE: holding the output ratio constant over time in future periods
L232.GlobalTechSecOut_ind$output.ratio[ L232.GlobalTechSecOut_ind$year %in% model_future_years ] <-
      L232.GlobalTechSecOut_ind$output.ratio[ L232.GlobalTechSecOut_ind$year %in% max( model_base_years ) ]
L232.GlobalTechSecOut_ind <- L232.GlobalTechSecOut_ind[ names_GlobalTechSecOut ]

#Costs of global technologies
printlog( "L232.GlobalTechCost_ind: Capital costs of global industrial technologies" )
L232.globaltech_cost.melt <- interpolate_and_melt( A32.globaltech_cost, c( model_base_years, model_future_years ), value.name="input.cost" )
L232.globaltech_cost.melt[ c( "sector.name", "subsector.name" ) ] <- L232.globaltech_cost.melt[ c( "supplysector", "subsector" ) ]
L232.GlobalTechCost_ind <- L232.globaltech_cost.melt[ names_GlobalTechCost ]

#Carbon capture from feedstock carbon sequestration
printlog( "L232.GlobalTechCSeq_ind: CO2 capture fractions from global electricity generation technologies" )
## No need to consider historical periods or intermittent technologies here
L232.globaltech_Cseq.melt <- repeat_and_add_vector( A32.nonenergy_Cseq, Y, c( model_base_years, model_future_years ) )
L232.globaltech_Cseq.melt[ c( "sector.name", "subsector.name" ) ] <- L232.globaltech_Cseq.melt[ c( "supplysector", "subsector" ) ]
L232.GlobalTechCSeq_ind <- L232.globaltech_Cseq.melt[ names_GlobalTechCSeq ]

#Calibration and region-specific data
printlog( "L232.StubTechCalInput_indenergy: calibrated input of industrial energy use technologies (including cogen)")
L232.in_EJ_R_indenergy_F_Yh <- interpolate_and_melt( rbind( L1322.in_EJ_R_indenergy_F_Yh, L123.in_EJ_R_indchp_F_Yh ), model_base_years )
L232.in_EJ_R_indenergy_F_Yh <- add_region_name( L232.in_EJ_R_indenergy_F_Yh )
L232.in_EJ_R_indenergy_F_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( vecpaste( L232.in_EJ_R_indenergy_F_Yh[ S_F ] ),
             vecpaste( calibrated_techs[ S_F ] ) ),
      c( "supplysector", "subsector", "technology" ) ]

L232.StubTechCalInput_indenergy <- L232.in_EJ_R_indenergy_F_Yh[ names_StubTechYr ]
L232.StubTechCalInput_indenergy$minicam.energy.input <- A32.globaltech_eff$minicam.energy.input[ 
      match( vecpaste( L232.StubTechCalInput_indenergy[ c( "subsector", "stub.technology" ) ] ),
             vecpaste( A32.globaltech_eff[ c( "subsector", "technology" ) ] ) ) ]
L232.StubTechCalInput_indenergy$calibrated.value <- round( L232.in_EJ_R_indenergy_F_Yh$value, digits_calOutput )
L232.StubTechCalInput_indenergy$share.weight.year <- L232.StubTechCalInput_indenergy$year
L232.StubTechCalInput_indenergy <- set_subsector_shrwt( L232.StubTechCalInput_indenergy, value.name = "calibrated.value" )
L232.StubTechCalInput_indenergy$tech.share.weight <- ifelse( L232.StubTechCalInput_indenergy$calibrated.value > 0, 1, 0 )
L232.StubTechCalInput_indenergy <- L232.StubTechCalInput_indenergy[ names_StubTechCalInput ]

printlog( "L232.StubTechCalInput_indfeed: calibrated input of industrial feedstock technologies")
L232.in_EJ_R_indfeed_F_Yh <- interpolate_and_melt( L1322.in_EJ_R_indfeed_F_Yh, model_base_years )
L232.in_EJ_R_indfeed_F_Yh <- add_region_name( L232.in_EJ_R_indfeed_F_Yh )
L232.in_EJ_R_indfeed_F_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( vecpaste( L232.in_EJ_R_indfeed_F_Yh[ S_F ] ),
             vecpaste( calibrated_techs[ S_F ] ) ),
      c( "supplysector", "subsector", "technology" ) ]

L232.StubTechCalInput_indfeed <- L232.in_EJ_R_indfeed_F_Yh[ names_StubTechYr ]
L232.StubTechCalInput_indfeed$minicam.energy.input <- A32.globaltech_eff$minicam.energy.input[ 
      match( vecpaste( L232.StubTechCalInput_indfeed[ c( "subsector", "stub.technology" ) ] ),
             vecpaste( A32.globaltech_eff[ c( "subsector", "technology" ) ] ) ) ]
L232.StubTechCalInput_indfeed$calibrated.value <- round( L232.in_EJ_R_indfeed_F_Yh$value, digits_calOutput )
L232.StubTechCalInput_indfeed$share.weight.year <- L232.StubTechCalInput_indfeed$year
L232.StubTechCalInput_indfeed <- set_subsector_shrwt( L232.StubTechCalInput_indfeed, value.name = "calibrated.value" )
L232.StubTechCalInput_indfeed$tech.share.weight <- ifelse( L232.StubTechCalInput_indfeed$calibrated.value > 0, 1, 0 )

printlog( "L232.StubTechProd_industry: calibrated output of industrial sector" )
#First, calculate service output by technology, for energy-use and feedstocks
L232.out_EJ_R_ind_serv_F_Yh <- rbind( L232.in_EJ_R_indenergy_F_Yh, L232.in_EJ_R_indfeed_F_Yh )
L232.out_EJ_R_ind_serv_F_Yh$efficiency <- L232.globaltech_eff.melt$efficiency[
      match( vecpaste( L232.out_EJ_R_ind_serv_F_Yh[ c( supp, subs, "stub.technology", Y ) ] ),
             vecpaste( L232.globaltech_eff.melt[ c( s_s_t, Y ) ] ) ) ]
L232.out_EJ_R_ind_serv_F_Yh$calOutputValue <- round( L232.out_EJ_R_ind_serv_F_Yh$value * L232.out_EJ_R_ind_serv_F_Yh$efficiency, digits_calOutput )

#Aggregate service output by region. This is the output of the industrial sector in each region.
L232.StubTechProd_industry <- aggregate( L232.out_EJ_R_ind_serv_F_Yh[ "calOutputValue" ],
      by=as.list( L232.out_EJ_R_ind_serv_F_Yh[ c( "region", R, Y ) ] ), sum )
L232.industry_names <- A32.globaltech_shrwt[ A32.globaltech_shrwt[[supp]] == "industry", s_s_t ]
L232.StubTechProd_industry[ c( supp, subs, "stub.technology" ) ] <- L232.industry_names[ rep( 1, times = nrow( L232.StubTechProd_industry ) ), ]
L232.StubTechProd_industry$share.weight.year <- L232.StubTechProd_industry[[Y]]
L232.StubTechProd_industry$subs.share.weight <- ifelse( L232.StubTechProd_industry$calOutputValue > 0, 1, 0 )
L232.StubTechProd_industry$tech.share.weight <- L232.StubTechProd_industry$subs.share.weight
L232.StubTechProd_industry <- L232.StubTechProd_industry[ names_StubTechProd ]

printlog( "L232.StubTechCoef_industry: calibrated output of industrial sector" )
#Next, aggregate service output by sector to calculate the portion of each input
L232.StubTechCoef_industry_base <- aggregate( L232.out_EJ_R_ind_serv_F_Yh[ "calOutputValue" ],
      by=as.list( L232.out_EJ_R_ind_serv_F_Yh[ c( "region", R, supp, Y ) ] ), sum )
L232.StubTechCoef_industry_base$output_tot <- L232.StubTechProd_industry$calOutputValue[
      match( vecpaste( L232.StubTechCoef_industry_base[ c( "region", Y ) ] ),
             vecpaste( L232.StubTechProd_industry[ c( "region", Y ) ] ) ) ]
L232.StubTechCoef_industry_base$coefficient <- L232.StubTechCoef_industry_base$calOutputValue / L232.StubTechCoef_industry_base$output_tot
names( L232.StubTechCoef_industry_base )[ names( L232.StubTechCoef_industry_base ) == supp ] <- input
L232.StubTechCoef_industry_base[ c( supp, subs, "stub.technology" ) ] <- L232.industry_names[ rep( 1, times = nrow( L232.StubTechCoef_industry_base ) ), ]
L232.StubTechCoef_industry_base$market.name <- L232.StubTechCoef_industry_base$region

#This set of coefficients covers only the base years; the first "future" period will default to the global tech coefficient
# Instead, interpolate the coefficients to these global default values in a specified period
L232.tech_coef <- repeat_and_add_vector( A32.globaltech_coef, R, GCAM_region_names[[R]] )
L232.tech_coef <- add_region_name( L232.tech_coef )
L232.tech_coef[[ X_final_model_base_year ]] <- L232.StubTechCoef_industry_base$coefficient[
      match( paste( L232.tech_coef$region, L232.tech_coef[[input]], max( model_base_years ) ),
             paste( L232.StubTechCoef_industry_base$region, L232.StubTechCoef_industry_base[[input]], L232.StubTechCoef_industry_base[[Y]] ) ) ] 
L232.tech_coef[ X_indcoef_conv_year ] <- L232.tech_coef[ "X2100" ]
L232.tech_coef <- gcam_interp( L232.tech_coef, c( max( model_base_years ), model_future_years, indcoef_conv_year ) )
L232.StubTechCoef_industry_fut <- interpolate_and_melt( L232.tech_coef, model_future_years, value.name = "coefficient", digits = digits_coefficient )
L232.StubTechCoef_industry_fut$stub.technology <- L232.StubTechCoef_industry_fut$technology
L232.StubTechCoef_industry_fut$market.name <- L232.StubTechCoef_industry_fut$region

#Combine the base year and future year coefficients
L232.StubTechCoef_industry <- rbind( L232.StubTechCoef_industry_base[ names_StubTechCoef ], L232.StubTechCoef_industry_fut[ names_StubTechCoef ] )
L232.StubTechCoef_industry$coefficient <- round( L232.StubTechCoef_industry$coefficient, digits_coefficient )

printlog( "L232.FuelPrefElast_indenergy: fuel preference elasticities of industrial energy use" )
#First, calculate the fuel shares allocated to each fuel
L232.indenergy_fuel_shares <- aggregate( L232.in_EJ_R_indenergy_F_Yh[ "value" ],
      by=as.list( L232.in_EJ_R_indenergy_F_Yh[ c( "region", R, supp, subs, Y ) ] ), sum )
L232.indenergy_fuel_totals <- aggregate( L232.indenergy_fuel_shares[ "value" ],
      by=as.list( L232.indenergy_fuel_shares[ c( "region", R, supp, Y ) ] ), sum )
L232.indenergy_fuel_shares$total <- L232.indenergy_fuel_totals$value[
      match( vecpaste( L232.indenergy_fuel_shares[ c( R, supp, Y ) ] ),
             vecpaste( L232.indenergy_fuel_totals[ c( R, supp, Y ) ] ) ) ]
L232.indenergy_fuel_shares$share <- L232.indenergy_fuel_shares$value / L232.indenergy_fuel_shares$total
L232.indenergy_fuel_shares <- subset( L232.indenergy_fuel_shares, year == max( model_base_years ) )

#Set fuel preference elasticities as indicated by exogenous rules
L232.indenergy_fuel_shares$fuelprefElasticity <- 0
for( j in 1:nrow( L232.indenergy_fuel_shares ) ){
	for( i in 1:nrow( A32.fuelprefElasticity ) ){
		L232.indenergy_fuel_shares$fuelprefElasticity[j][
		    L232.indenergy_fuel_shares$subsector[j] == A32.fuelprefElasticity$subsector[i] &
		    A32.fuelprefElasticity$criteria[i] == "greater than" &
		    L232.indenergy_fuel_shares$share[j] > A32.fuelprefElasticity$share[i] ] <-
		    	A32.fuelprefElasticity$fuelprefElasticity[i]
	}
}

for( j in 1:nrow( L232.indenergy_fuel_shares ) ){
	for( i in 1:nrow( A32.fuelprefElasticity ) ){
		L232.indenergy_fuel_shares$fuelprefElasticity[j][
		    L232.indenergy_fuel_shares$subsector[j] == A32.fuelprefElasticity$subsector[i] &
		    A32.fuelprefElasticity$criteria[i] == "lesser than" &
		    L232.indenergy_fuel_shares$share[j] < A32.fuelprefElasticity$share[i] ] <-
		    	A32.fuelprefElasticity$fuelprefElasticity[i]
	}
}

#The fuel preference elasticities only matter in future periods. Fill out from the first future model time period
L232.indenergy_fuel_shares$year.fillout <- min( model_future_years )
L232.FuelPrefElast_indenergy <- L232.indenergy_fuel_shares[
      L232.indenergy_fuel_shares$fuelprefElasticity !=0, names_FuelPrefElasticity ]

printlog( "L232.PerCapitaBased_ind: per-capita based flag for industry final demand" )
L232.PerCapitaBased_ind <- data.frame(
      region = GCAM_region_names$region, energy.final.demand = A32.demand$energy.final.demand, perCapitaBased = A32.demand$perCapitaBased )

printlog( "L232.PriceElasticity_ind: price elasticity of industry final demand" )
#Price elasticities are only applied to future periods. Application in base years will cause solution failure
L232.PriceElasticity_ind <- data.frame(
      region = rep( GCAM_region_names$region, times = length( model_future_years ) ),
      energy.final.demand = A32.demand$energy.final.demand,
      year = sort( rep( model_future_years, times = nrow( GCAM_region_names ) ) ),
      price.elasticity = A32.demand$price.elasticity )

printlog( "L232.BaseService_ind: base-year service output of industry final demand" )
#Base service is equal to the output of the industry supplysector
L232.BaseService_ind <- data.frame(
      region = L232.StubTechProd_industry$region,
      energy.final.demand = A32.demand$energy.final.demand,
      year = L232.StubTechProd_industry$year,
      base.service = L232.StubTechProd_industry$calOutputValue )

printlog( "L232.IncomeElasticity_ind_scen: income elasticity of industry (scenario-specific)" )
#Combine GCAM 3.0 with the SSPs, and subset only the relevant years
L102.pcgdp_thous90USD_GCAM3_R_Y[[Scen]] <- "GCAM3"
L232.pcgdp_thous90USD_ALL_R_Y <- rbind(
      L102.pcgdp_thous90USD_Scen_R_Y,
      L102.pcgdp_thous90USD_GCAM3_R_Y )[ c( Scen_R, X_final_model_base_year, X_model_future_years ) ]

# Per-capita GDP ratios, which are used in the equation for demand growth
X_elast_years <- c( X_final_model_base_year, X_model_future_years )
L232.pcgdpRatio_ALL_R_Y <- L232.pcgdp_thous90USD_ALL_R_Y[ c( Scen_R, X_model_future_years ) ]
L232.pcgdpRatio_ALL_R_Y[ X_elast_years[ 2:length( X_elast_years ) ] ] <-
      L232.pcgdp_thous90USD_ALL_R_Y[ X_elast_years[ 2:length( X_elast_years ) ] ] /
      L232.pcgdp_thous90USD_ALL_R_Y[ X_elast_years[ 1:( length( X_elast_years ) - 1 ) ] ]

#Calculate the industrial output as the base-year industrial output times the GDP ratio raised to the income elasticity
# The income elasticity is looked up based on the prior year's output
L232.Output_ind <- add_region_name( L232.pcgdpRatio_ALL_R_Y[ c( Scen_R ) ] )
L232.Output_ind[[X_final_model_base_year ]] <- L232.BaseService_ind$base.service[
      match( paste( L232.Output_ind$region, max( model_base_years ) ),
             paste( L232.BaseService_ind$region, L232.BaseService_ind$year ) ) ] * conv_bil_thous /
      L101.Pop_thous_GCAM3_R_Y[[ X_final_model_base_year ]][
         match( L232.Output_ind[[R]], L101.Pop_thous_GCAM3_R_Y[[R]] ) ]

#At each time, the output is equal to the prior period's output times the GDP ratio, raised to the elasticity
# that corresponds to the output that was observed in the prior time period. This method prevents (ideally) runaway
# industrial production.
for( i in 2:( length( X_elast_years ) ) ){
	L232.Output_ind[ X_elast_years[i] ] <-
	   L232.Output_ind[ X_elast_years[i-1] ] *
	   L232.pcgdpRatio_ALL_R_Y[ X_elast_years[i] ] ^
	   approx( x = A32.inc_elas_output$pc.output_GJ,
	           y = A32.inc_elas_output$inc_elas,
	           xout = L232.Output_ind[[ X_elast_years[i-1] ]], rule = 2 )$y
}	

#Now that we have industrial output, we can back out the appropriate income elasticities
L232.IncElas_ind <- L232.Output_ind[ c( Scen, R, X_model_future_years ) ]
for( i in 1:length( X_model_future_years ) ){
	L232.IncElas_ind[ X_model_future_years[i] ] <-
	approx( x = A32.inc_elas_output$pc.output_GJ,
	           y = A32.inc_elas_output$inc_elas,
	           xout = L232.Output_ind[[ X_model_future_years[i] ]], rule = 2 )$y
}

L232.IncomeElasticity_ind <- interpolate_and_melt(
      L232.IncElas_ind, model_future_years, value.name = "income.elasticity", digits = digits_IncElas_ind )
L232.IncomeElasticity_ind <- add_region_name( L232.IncomeElasticity_ind )
L232.IncomeElasticity_ind$energy.final.demand <- A32.demand$energy.final.demand
#These will be written out as separate tables using a for loop

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table in names ( L232.SectorLogitTables) ) {
write_mi_data( L232.SectorLogitTables[[ curr_table ]]$data, L232.SectorLogitTables[[ curr_table ]]$header,
    "ENERGY_LEVEL2_DATA", paste0("L232.", L232.SectorLogitTables[[ curr_table ]]$header ), "ENERGY_XML_BATCH",
    "batch_industry.xml" )
}
write_mi_data( L232.Supplysector_ind, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L232.Supplysector_ind",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_industry.xml" ) 
for( curr_table in names ( L232.SubsectorLogitTables ) ) {
write_mi_data( L232.SubsectorLogitTables[[ curr_table ]]$data, L232.SubsectorLogitTables[[ curr_table ]]$header,
    "ENERGY_LEVEL2_DATA", paste0("L232.", L232.SubsectorLogitTables[[ curr_table ]]$header ), "ENERGY_XML_BATCH",
    "batch_industry.xml" )
}
write_mi_data( L232.SubsectorLogit_ind, "SubsectorLogit", "ENERGY_LEVEL2_DATA", "L232.SubsectorLogit_ind", "ENERGY_XML_BATCH", "batch_industry.xml" ) 
write_mi_data( L232.FinalEnergyKeyword_ind, "FinalEnergyKeyword", "ENERGY_LEVEL2_DATA", "L232.FinalEnergyKeyword_ind", "ENERGY_XML_BATCH", "batch_industry.xml" ) 
if( exists( "L232.SubsectorShrwt_ind" ) ){
	write_mi_data( L232.SubsectorShrwt_ind, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L232.SubsectorShrwt_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
	}
if( exists( "L232.SubsectorShrwtFllt_ind" ) ){
	write_mi_data( L232.SubsectorShrwtFllt_ind, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L232.SubsectorShrwtFllt_ind",
	               "ENERGY_XML_BATCH", "batch_industry.xml" ) 
	}
if( exists( "L232.SubsectorInterp_ind" ) ) {
	write_mi_data( L232.SubsectorInterp_ind, "SubsectorInterp", "ENERGY_LEVEL2_DATA", "L232.SubsectorInterp_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
	}
if( exists( "L232.SubsectorInterpTo_ind" ) ) {
	write_mi_data( L232.SubsectorInterpTo_ind, "SubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L232.SubsectorInterpTo_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
	}
write_mi_data( L232.StubTech_ind, "StubTech", "ENERGY_LEVEL2_DATA", "L232.StubTech_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.GlobalTechShrwt_ind, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L232.GlobalTechShrwt_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.StubTechInterp_ind, "StubTechInterp", "ENERGY_LEVEL2_DATA", "L232.StubTechInterp_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.GlobalTechEff_ind, "GlobalTechEff", "ENERGY_LEVEL2_DATA", "L232.GlobalTechEff_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.GlobalTechCoef_ind, "GlobalTechCoef", "ENERGY_LEVEL2_DATA", "L232.GlobalTechCoef_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.GlobalTechCost_ind, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L232.GlobalTechCost_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.GlobalTechSecOut_ind, "GlobalTechSecOut", "ENERGY_LEVEL2_DATA", "L232.GlobalTechSecOut_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.GlobalTechCSeq_ind, "GlobalTechCSeq", "ENERGY_LEVEL2_DATA", "L232.GlobalTechCSeq_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.StubTechCalInput_indenergy, "StubTechCalInput", "ENERGY_LEVEL2_DATA", "L232.StubTechCalInput_indenergy", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.StubTechCalInput_indfeed, "StubTechCalInput", "ENERGY_LEVEL2_DATA", "L232.StubTechCalInput_indfeed", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.StubTechProd_industry, "StubTechProd", "ENERGY_LEVEL2_DATA", "L232.StubTechProd_industry", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.StubTechCoef_industry, "StubTechCoef", "ENERGY_LEVEL2_DATA", "L232.StubTechCoef_industry", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.FuelPrefElast_indenergy, "FuelPrefElast", "ENERGY_LEVEL2_DATA", "L232.FuelPrefElast_indenergy", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.PerCapitaBased_ind, "PerCapitaBased", "ENERGY_LEVEL2_DATA", "L232.PerCapitaBased_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.PriceElasticity_ind, "PriceElasticity", "ENERGY_LEVEL2_DATA", "L232.PriceElasticity_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )
write_mi_data( L232.BaseService_ind, "BaseService", "ENERGY_LEVEL2_DATA", "L232.BaseService_ind", "ENERGY_XML_BATCH", "batch_industry.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_industry.xml", "ENERGY_XML_FINAL", "industry.xml", "", xml_tag="outFile" )

#Income elasticities by scenario
for( i in 1:length( unique( L232.IncomeElasticity_ind[[ Scen ]] ) ) ){
  scenarios <- unique( L232.IncomeElasticity_ind[[ Scen ]] )
  scen_strings <- tolower( scenarios )
  objectname <- paste0( "L232.IncomeElasticity_ind_", scen_strings[i] )
  object <- subset( L232.IncomeElasticity_ind, scenario == scenarios[i] )[ names_IncomeElasticity ] 
  assign( objectname, object )
  batchXMLstring <- paste0( "batch_industry_incelas_",
                            scen_strings[i],
                            ".xml" )
  write_mi_data( object, "IncomeElasticity", "ENERGY_LEVEL2_DATA", objectname, "ENERGY_XML_BATCH", batchXMLstring )
  XMLstring <- sub( "batch_", "", batchXMLstring )
  insert_file_into_batchxml( "ENERGY_XML_BATCH", batchXMLstring, "ENERGY_XML_FINAL", XMLstring, "", xml_tag="outFile" )
}


logstop()
