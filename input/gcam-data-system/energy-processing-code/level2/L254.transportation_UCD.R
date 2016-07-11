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
logstart( "L254.transportation_UCD.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Model input for UC Davis based transportation sector" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_trn_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
UCD_techs <- readdata( "ENERGY_MAPPINGS", "UCD_techs" )
A54.demand <- readdata( "ENERGY_ASSUMPTIONS", "A54.demand" )
if( trn_SSP == "SSP1" ){
  A54.demand <- readdata( "ENERGY_ASSUMPTIONS", "A54.demand_ssp1" )
}
A54.incelas_R <- readdata( "ENERGY_ASSUMPTIONS", "A54.incelas_R" )
A54.sector <- readdata( "ENERGY_ASSUMPTIONS", "A54.sector" )
A54.tranSubsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A54.tranSubsector_logit" )
A54.tranSubsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A54.tranSubsector_shrwt" )
A54.tranSubsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A54.tranSubsector_interp" )
A54.tranSubsector_VOTT <- readdata( "ENERGY_ASSUMPTIONS", "A54.tranSubsector_VOTT" )
if( trn_SSP == "SSP1" ){
  A54.tranSubsector_VOTT <- readdata( "ENERGY_ASSUMPTIONS", "A54.tranSubsector_VOTT_ssp1" )
}
A54.globaltech_passthru <- readdata( "ENERGY_ASSUMPTIONS", "A54.globaltech_passthru" )
A54.globaltech_nonmotor <- readdata( "ENERGY_ASSUMPTIONS", "A54.globaltech_nonmotor" )
A54.globaltranTech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A54.globaltranTech_shrwt" )
A54.globaltranTech_interp <- readdata( "ENERGY_ASSUMPTIONS", "A54.globaltranTech_interp" )
A54.globaltranTech_retire <- readdata( "ENERGY_ASSUMPTIONS", "A54.globaltranTech_retire" )
L154.in_EJ_R_trn_m_sz_tech_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L154.in_EJ_R_trn_m_sz_tech_F_Yh" )
L154.cost_usdvkm_R_trn_m_sz_tech_F_Y <- readdata( "ENERGY_LEVEL1_DATA", "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y" )
L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y <- readdata( "ENERGY_LEVEL1_DATA", "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y" )
L154.loadfactor_R_trn_m_sz_tech_F_Y <- readdata( "ENERGY_LEVEL1_DATA", "L154.loadfactor_R_trn_m_sz_tech_F_Y" )
L154.speed_kmhr_R_trn_m_sz_tech_F_Y <- readdata( "ENERGY_LEVEL1_DATA", "L154.speed_kmhr_R_trn_m_sz_tech_F_Y" )
L154.out_mpkm_R_trn_nonmotor_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L154.out_mpkm_R_trn_nonmotor_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
## NOTE: Due to asymmetrical nature of the transportation sectors in the various regions, we can't simply write
## generic information to all regions. Instead, technology information is read from the global UCD transportation
## technology database, and supplysector and subsector attributes are matched in from lookup tables
printlog( "Transportation sectors are built from the technology level up" )
printlog( "L254.StubTranTech: Transportation stub technologies (built from technologies with coefficients in the UCD database)" )
L254.StubTranTech <- L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y[ c( R, UCD_techID ) ]
L254.StubTranTech <- add_region_name( L254.StubTranTech )
L254.StubTranTech[ s_tS_tT ] <- UCD_techs[
     match( vecpaste( L254.StubTranTech[ UCD_techID ] ),
            vecpaste( UCD_techs[ UCD_techID ] ) ),
     s_tS_tT ]
L254.StubTranTech$stub.technology <- L254.StubTranTech$tranTechnology
L254.StubTranTech <- L254.StubTranTech[ names_StubTranTech ]

#Write the pass-through technologies to all regions
L254.StubTech_passthru <- write_to_all_regions( A54.globaltech_passthru, c( "region", s_tS_t, input ) )

#Of the pass-throughs, subset only the ones whose "input" actually exists in the given region OR
# ones whose input is in the list of pass-through technologies.
L254.StubTech_passthru <- subset( L254.StubTech_passthru,
      paste( region, minicam.energy.input ) %in%
      paste( L254.StubTranTech$region, L254.StubTranTech$supplysector ) |
      minicam.energy.input %in%
      A54.globaltech_passthru$supplysector )
L254.StubTech_passthru$stub.technology <- L254.StubTech_passthru$technology
L254.StubTech_passthru <- L254.StubTech_passthru[ names_StubTranTech ]

#Write the non-motorized technologies to all regions
L254.StubTech_nonmotor <- write_to_all_regions( A54.globaltech_nonmotor, c( "region", s_tS_t ) )
L254.StubTech_nonmotor$stub.technology <- L254.StubTech_nonmotor$technology
L254.StubTech_nonmotor <- L254.StubTech_nonmotor[ names_StubTranTech ]

printlog( "L254.Supplysector_trn: Supply sector information for transportation sector" )
#Writing the generic supplysector table to all regions may generate combinations that don't apply
L254.SectorLogitTables <- get_logit_fn_tables( A54.sector, names_SupplysectorLogitType, base.header="Supplysector_",
    include.equiv.table=T, write.all.regions=T )
for( curr_table in names( L254.SectorLogitTables ) ) {
    if( curr_table != "EQUIV_TABLE" ) {
        L254.SectorLogitTables[[ curr_table ]]$data <- L254.SectorLogitTables[[ curr_table ]]$data[
            vecpaste( L254.SectorLogitTables[[ curr_table ]]$data[ c( "region", "supplysector" ) ] ) %in%
            c( vecpaste( L254.StubTranTech[ c( "region", "supplysector" ) ] ),
               vecpaste( L254.StubTech_passthru[ c( "region", "supplysector" ) ] ),
               vecpaste( L254.StubTech_nonmotor[ c( "region", "supplysector" ) ] ) ), ]
    }
}
L254.Supplysector_trn <- write_to_all_regions( A54.sector, names_Supplysector )

#Subset only the combinations of region and supplysector that are available in the stub technology table
L254.Supplysector_trn <- L254.Supplysector_trn[
      vecpaste( L254.Supplysector_trn[ c( "region", "supplysector" ) ] ) %in%
      c( vecpaste( L254.StubTranTech[ c( "region", "supplysector" ) ] ),
         vecpaste( L254.StubTech_passthru[ c( "region", "supplysector" ) ] ),
         vecpaste( L254.StubTech_nonmotor[ c( "region", "supplysector" ) ] ) ), ]

printlog( "L254.FinalEnergyKeyword_trn: Supply sector keywords for transportation sector" )
L254.FinalEnergyKeyword_trn <- L254.Supplysector_trn
L254.FinalEnergyKeyword_trn$final.energy <- A54.sector$final.energy[
      match( L254.FinalEnergyKeyword_trn$supplysector, A54.sector$supplysector ) ]
L254.FinalEnergyKeyword_trn <- na.omit( L254.FinalEnergyKeyword_trn[ names_FinalEnergyKeyword ] )

# 2b. Subsector information
printlog( "L254.tranSubsectorLogit: Subsector logit exponents of transportation sector" )
L254.SubsectorLogitTables <- get_logit_fn_tables( A54.tranSubsector_logit, names_tranSubsectorLogitType,
    base.header="tranSubsector_", include.equiv.table=F, write.all.regions=T )
for( curr_table in names( L254.SubsectorLogitTables ) ) {
    if( curr_table != "EQUIV_TABLE" ) {
        L254.SubsectorLogitTables[[ curr_table ]]$data <- L254.SubsectorLogitTables[[ curr_table ]]$data[
            vecpaste( L254.SubsectorLogitTables[[ curr_table ]]$data[ names_tranSubsector ] ) %in%
            c( vecpaste( L254.StubTranTech[ names_tranSubsector ] ),
               vecpaste( L254.StubTech_passthru[ names_tranSubsector ] ),
               vecpaste( L254.StubTech_nonmotor[ names_tranSubsector ] ) ), ]
    }
}
L254.tranSubsectorLogit <- write_to_all_regions( A54.tranSubsector_logit, names_tranSubsectorLogit )
L254.tranSubsectorLogit <- L254.tranSubsectorLogit[
      vecpaste( L254.tranSubsectorLogit[ names_tranSubsector ] ) %in%
      c( vecpaste( L254.StubTranTech[ names_tranSubsector ] ),
         vecpaste( L254.StubTech_passthru[ names_tranSubsector ] ),
         vecpaste( L254.StubTech_nonmotor[ names_tranSubsector ] ) ), ]

printlog( "L254.tranSubsectorShrwt and L254.tranSubsectorShrwtFllt: Subsector shareweights of transportation sector" )
if( any( !is.na( A54.tranSubsector_shrwt$year ) ) ){
	L254.tranSubsectorShrwt <- write_to_all_regions( A54.tranSubsector_shrwt[ !is.na( A54.tranSubsector_shrwt$year ), ], names_tranSubsectorShrwt )
	L254.tranSubsectorShrwt <- L254.tranSubsectorShrwt[
	   vecpaste( L254.tranSubsectorShrwt[ names_tranSubsector ] ) %in%
	   c( vecpaste( L254.StubTranTech[ names_tranSubsector ] ),
          vecpaste( L254.StubTech_passthru[ names_tranSubsector ] ),
          vecpaste( L254.StubTech_nonmotor[ names_tranSubsector ] ) ), ]
	}
if( any( !is.na( A54.tranSubsector_shrwt$year.fillout ) ) ){
	L254.tranSubsectorShrwtFllt <- write_to_all_regions( A54.tranSubsector_shrwt[ !is.na( A54.tranSubsector_shrwt$year.fillout ), ], names_tranSubsectorShrwtFllt )
	L254.tranSubsectorShrwtFllt <- L254.tranSubsectorShrwtFllt[
	   vecpaste( L254.tranSubsectorShrwtFllt[ names_tranSubsector ] ) %in%
	   c( vecpaste( L254.StubTranTech[ names_tranSubsector ] ),
          vecpaste( L254.StubTech_passthru[ names_tranSubsector ] ),
          vecpaste( L254.StubTech_nonmotor[ names_tranSubsector ] ) ), ]
	}

printlog( "L254.tranSubsectorInterp and L254.tranSubsectorInterpTo: Subsector shareweight interpolation of transportation sector" )
if( any( is.na( A54.tranSubsector_interp$to.value ) ) ){
	L254.tranSubsectorInterp <- write_to_all_regions( A54.tranSubsector_interp[ is.na( A54.tranSubsector_interp$to.value ), ], names_tranSubsectorInterp )
	L254.tranSubsectorInterp <- L254.tranSubsectorInterp[
	   vecpaste( L254.tranSubsectorInterp[ names_tranSubsector ] ) %in%
	   c( vecpaste( L254.StubTranTech[ names_tranSubsector ] ),
          vecpaste( L254.StubTech_passthru[ names_tranSubsector ] ),
          vecpaste( L254.StubTech_nonmotor[ names_tranSubsector ] ) ), ]
	}
if( any( !is.na( A54.tranSubsector_interp$to.value ) ) ){
	L254.tranSubsectorInterpTo <- write_to_all_regions( A54.tranSubsector_interp[ !is.na( A54.tranSubsector_interp$to.value ), ], names_tranSubsectorInterpTo )
	L254.tranSubsectorInterpTo <- L254.tranSubsectorInterpTo[
	   vecpaste( L254.tranSubsectorInterpTo[ names_tranSubsector ] ) %in%
	   c( vecpaste( L254.StubTranTech[ names_tranSubsector ] ),
          vecpaste( L254.StubTech_passthru[ names_tranSubsector ] ),
          vecpaste( L254.StubTech_nonmotor[ names_tranSubsector ] ) ), ]
	}

printlog( "L254.tranSubsectorSpeed: speeds of transportation modes (not including pass-through sectors)" )
L254.tranSubsectorSpeed <- interpolate_and_melt( L154.speed_kmhr_R_trn_m_sz_tech_F_Y, years = model_years, value.name = "speed", digits = digits_speed )
L254.tranSubsectorSpeed <- add_region_name( L254.tranSubsectorSpeed )
L254.tranSubsectorSpeed[ s_tS_tT ] <- UCD_techs[
      match( vecpaste( L254.tranSubsectorSpeed[ UCD_techID ] ),
             vecpaste( UCD_techs[ UCD_techID ] ) ),
      s_tS_tT ]
L254.tranSubsectorSpeed <- L254.tranSubsectorSpeed[ names_tranSubsectorSpeed ]

#This does not include the pass-through tranSectors
# Pass-through tranSubsectors for which time value is added are assigned a sector from which to get their speed.
printlog( "L254.tranSubsectorSpeed_passthru: speeds of pass-through transportation subsectors" )
L254.tranSubsectorSpeed_passthru <- write_to_all_regions(
      subset( A54.tranSubsector_VOTT, !is.na( speed.source ) ),
      c( names_tranSubsector, "speed.source" ) )
L254.tranSubsectorSpeed_passthru <- repeat_and_add_vector( L254.tranSubsectorSpeed_passthru, Y, model_years )
L254.tranSubsectorSpeed_passthru$speed <- L254.tranSubsectorSpeed$speed[
      match( vecpaste( L254.tranSubsectorSpeed_passthru[ c( "region", "speed.source", Y ) ] ),
             vecpaste( L254.tranSubsectorSpeed[ c( "region", "supplysector", Y ) ] ) ) ]

#Drop any missing values, which indicate region x passthru combinations that don't apply
L254.tranSubsectorSpeed_passthru <- na.omit( L254.tranSubsectorSpeed_passthru )[ names_tranSubsectorSpeed ]          

printlog( "L254.tranSubsectorSpeed_noVOTT: speeds of transportation subsectors whose time value is not considered" )
printlog( "NOTE: This step should be unnecessary. Currently there is no model default value for speed, and a subsector" )
printlog( "with no speed read in will cause a model crash, even for modes such as freight where time value is not modeled")
#Start with all observed subsectors in the transportation module
L254.tranSubsectorSpeed_noVOTT <- rbind(
      unique( L254.StubTranTech[ names_tranSubsector ] ),
      unique( L254.StubTech_passthru[ names_tranSubsector ] ) )
#Subset only those whose speeds have not already been specified
L254.tranSubsectorSpeed_noVOTT <- L254.tranSubsectorSpeed_noVOTT[
      !vecpaste( L254.tranSubsectorSpeed_noVOTT[ names_tranSubsector ] ) %in%
      c( vecpaste( L254.tranSubsectorSpeed[ names_tranSubsector ] ),
         vecpaste( L254.tranSubsectorSpeed_passthru[ names_tranSubsector ] ),
         vecpaste( L254.StubTech_nonmotor[ names_tranSubsector ] ) ),
      names_tranSubsector ]
#Repeat by the number of model time periods
L254.tranSubsectorSpeed_noVOTT <- repeat_and_add_vector( L254.tranSubsectorSpeed_noVOTT, Y, model_years )
#Write in a default value for speed
L254.tranSubsectorSpeed_noVOTT$speed <- 1

printlog( "L254.tranSubsectorSpeed_nonmotor: speeds of non-motorized transportation subsectors" )
L254.tranSubsectorSpeed_nonmotor <- repeat_and_add_vector( A54.globaltech_nonmotor, Y, model_years )
L254.tranSubsectorSpeed_nonmotor <- write_to_all_regions( L254.tranSubsectorSpeed_nonmotor, c( names_tranSubsector, Y, "speed" ) )

printlog( "L254.tranSubsectorVOTT: Value of time in transit parameterization" )
printlog( "NOTE: These are currently considered time- and region-independent characteristics" )
L254.tranSubsectorVOTT <- write_to_all_regions(
      subset( A54.tranSubsector_VOTT, addTimeValue == 1 ),
      c( names_tranSubsector, "addTimeValue", "time.value.multiplier" ) )
L254.tranSubsectorVOTT$year.fillout <- min( model_years )
L254.tranSubsectorVOTT <- L254.tranSubsectorVOTT[
      vecpaste( L254.tranSubsectorVOTT[ names_tranSubsector ] ) %in%
      c( vecpaste( L254.StubTranTech[ names_tranSubsector ] ),
         vecpaste( L254.StubTech_passthru[ names_tranSubsector ] ),
         vecpaste( L254.StubTech_nonmotor[ names_tranSubsector ] ) ),
      names_tranSubsectorVOTT ]

printlog( "L254.tranSubsectorFuelPref: Subsector preferences that are tied to GDP (unrelated to time value)" )
L254.tranSubsectorFuelPref <- write_to_all_regions(
      subset( A54.tranSubsector_VOTT, fuelprefElasticity != 0 ),
      c( names_tranSubsector, "fuelprefElasticity" ) )
L254.tranSubsectorFuelPref$year.fillout <- min( model_years )
L254.tranSubsectorFuelPref <- L254.tranSubsectorFuelPref[
      vecpaste( L254.tranSubsectorFuelPref[ names_tranSubsector ] ) %in%
      c( vecpaste( L254.StubTranTech[ names_tranSubsector ] ),
         vecpaste( L254.StubTech_passthru[ names_tranSubsector ] ),
         vecpaste( L254.StubTech_nonmotor[ names_tranSubsector ] ) ),
      names_tranSubsectorFuelPref ]

# 2c. Technology information - global technologies (i.e. not tranTechnologies)
printlog( "L254.GlobalTechShrwt_passthru: Shareweights of global transportation sector technologies (not tranTechnologies)" )
L254.globaltech_passthru <- repeat_and_add_vector( A54.globaltech_passthru, Y, model_years )
L254.globaltech_passthru[ c( "sector.name", "subsector.name" ) ] <- L254.globaltech_passthru[ c( "supplysector", "tranSubsector" ) ]
L254.GlobalTechShrwt_passthru <- L254.globaltech_passthru[ c( names_GlobalTechYr, "share.weight" ) ]

printlog( "L254.GlobalTechShrwt_nonmotor: Shareweights of non-motorized global transportation sector technologies (not tranTechnologies)" )
L254.globaltech_nonmotor <- repeat_and_add_vector( A54.globaltech_nonmotor, Y, model_years )
L254.globaltech_nonmotor[ c( "sector.name", "subsector.name" ) ] <- L254.globaltech_nonmotor[ c( "supplysector", "tranSubsector" ) ]
L254.GlobalTechShrwt_nonmotor <- L254.globaltech_nonmotor[ c( names_GlobalTechYr, "share.weight" ) ]

printlog( "L254.GlobalTechCoef_passthru: Coefficients of global transportation sector technologies (not tranTechnologies)" )
L254.GlobalTechCoef_passthru <- L254.globaltech_passthru[ names_GlobalTechCoef ]

printlog( "L254.GlobalRenewTech_nonmotor: Renewable inputs to non-motorized transportation technologies" )
L254.GlobalRenewTech_nonmotor <- L254.globaltech_nonmotor[ names_GlobalRenewTech ]

# 2d. Technology information - global tranTechnologies
printlog( "L254.GlobalTranTechInterp: Shareweight interpolation of global tranTechnologies" )
L254.GlobalTranTechInterp <- set_years( A54.globaltranTech_interp )
L254.GlobalTranTechInterp[ c( "sector.name", "subsector.name" ) ] <- L254.GlobalTranTechInterp[ c( "supplysector", "tranSubsector" ) ]
L254.GlobalTranTechInterp <- L254.GlobalTranTechInterp[ names_GlobalTranTechInterp ]

printlog( "L254.GlobalTranTechShrwt: Shareweights of global tranTechnologies" )
L254.GlobalTranTechShrwt <- interpolate_and_melt(
      A54.globaltranTech_shrwt, model_years, value.name = "share.weight", digits = digits_shrwt )
L254.GlobalTranTechShrwt[ c( "sector.name", "subsector.name" ) ] <- L254.GlobalTranTechShrwt[ c( "supplysector", "tranSubsector" ) ]
L254.GlobalTranTechShrwt <- L254.GlobalTranTechShrwt[ names_GlobalTranTechShrwt ]

printlog( "L254.GlobalTranTechSCurve: Retirement of global tranTechnologies" )
L254.GlobalTranTechSCurve <- set_years( A54.globaltranTech_retire )
#Copy the final year forward to all future time periods
L254.GlobalTranTechSCurve <- rbind(
      subset( L254.GlobalTranTechSCurve, year < max( year ) ),
      repeat_and_add_vector(
          subset( L254.GlobalTranTechSCurve, year == max( year ) ),
      Y, model_years[ model_years >= max( L254.GlobalTranTechSCurve$year ) ] ) )
L254.GlobalTranTechSCurve[ c( "sector.name", "subsector.name" ) ] <- L254.GlobalTranTechSCurve[ c( "supplysector", "tranSubsector" ) ]
L254.GlobalTranTechSCurve <- L254.GlobalTranTechSCurve[ names_GlobalTranTechSCurve ]

# 2e. Calibration and region-specific data
printlog( "L254.StubTranTechCalInput: calibrated input of tranTechnologies")
L254.StubTranTechCalInput <- interpolate_and_melt(
      L154.in_EJ_R_trn_m_sz_tech_F_Yh, model_base_years, value.name = "calibrated.value", digits = digits_calOutput )
L254.StubTranTechCalInput <- add_region_name( L254.StubTranTechCalInput )
L254.StubTranTechCalInput[ c( "supplysector", "tranSubsector", "stub.technology", "minicam.energy.input" ) ] <- UCD_techs[
      match( vecpaste( L254.StubTranTechCalInput[ UCD_techID ] ),
             vecpaste( UCD_techs[ UCD_techID ] ) ),
      c( s_tS_tT, input ) ]
L254.StubTranTechCalInput$share.weight.year <- L254.StubTranTechCalInput$year
L254.StubTranTechCalInput <- set_subsector_shrwt( L254.StubTranTechCalInput, subsector.name = "tranSubsector", value.name = "calibrated.value" )
L254.StubTranTechCalInput$tech.share.weight <- ifelse( L254.StubTranTechCalInput$calibrated.value > 0, 1, 0 )
L254.StubTranTechCalInput <- L254.StubTranTechCalInput[ names_StubTranTechCalInput ]

printlog( "L254.StubTranTechLoadFactor: tranTechnology load factors (all periods)")
L254.StubTranTechLoadFactor <- interpolate_and_melt(
      L154.loadfactor_R_trn_m_sz_tech_F_Y, model_years, value.name = "loadFactor", digits = digits_LoadFactor )
L254.StubTranTechLoadFactor <- add_region_name( L254.StubTranTechLoadFactor )
L254.StubTranTechLoadFactor[ c( "supplysector", "tranSubsector", "stub.technology" ) ] <- UCD_techs[
      match( vecpaste( L254.StubTranTechLoadFactor[ UCD_techID ] ),
             vecpaste( UCD_techs[ UCD_techID ] ) ),
      s_tS_tT ]
L254.StubTranTechLoadFactor <- L254.StubTranTechLoadFactor[ names_StubTranTechLoadFactor ]

printlog( "L254.StubTranTechCost: tranTechnology costs (all periods)")
L254.StubTranTechCost <- interpolate_and_melt(
      L154.cost_usdvkm_R_trn_m_sz_tech_F_Y, model_years, value.name = "input.cost.05USD" )
L254.StubTranTechCost$input.cost <- round(
      L254.StubTranTechCost$input.cost.05USD / conv_1990_2005_USD,
      digits = digits_cost )
L254.StubTranTechCost <- add_region_name( L254.StubTranTechCost )
L254.StubTranTechCost[ c( "supplysector", "tranSubsector", "stub.technology" ) ] <- UCD_techs[
      match( vecpaste( L254.StubTranTechCost[ UCD_techID ] ),
             vecpaste( UCD_techs[ UCD_techID ] ) ),
      s_tS_tT ]
L254.StubTranTechCost$minicam.non.energy.input <- "non-energy"
L254.StubTranTechCost <- L254.StubTranTechCost[ names_StubTranTechCost ]

printlog( "L254.StubTranTechCoef: tranTechnology coefficients (intensities; all periods)")
L254.StubTranTechCoef <- interpolate_and_melt(
      L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y, model_years, value.name = "coefficient.MJvkm" )
L254.StubTranTechCoef$coefficient <- round(
      L254.StubTranTechCoef$coefficient.MJvkm * conv_MJ_btu,
      digits = digits_coefficient )
L254.StubTranTechCoef <- add_region_name( L254.StubTranTechCoef )
L254.StubTranTechCoef[ c( "supplysector", "tranSubsector", "stub.technology", "minicam.energy.input" ) ] <- UCD_techs[
      match( vecpaste( L254.StubTranTechCoef[ UCD_techID ] ),
             vecpaste( UCD_techs[ UCD_techID ] ) ),
      c( s_tS_tT, input ) ]
#Currently, the market names for the fuels will be the same as the region
L254.StubTranTechCoef$market.name <- L254.StubTranTechCoef$region
L254.StubTranTechCoef <- L254.StubTranTechCoef[ names_StubTranTechCoef ]

printlog( "L254.StubTechCalInput_passthru: calibrated input of passthrough technologies" )
#First, need to calculate the service output for all tranTechnologies (= calInput * loadFactor * unit_conversion / (coef * unit conversion ) )
L254.StubTranTechOutput <- L254.StubTranTechCalInput[ !grepl( "share", names( L254.StubTranTechCalInput ) ) ]
L254.StubTranTechOutput$loadFactor <- L254.StubTranTechLoadFactor$loadFactor[
      match( vecpaste( L254.StubTranTechOutput[ c( "region", supp, "tranSubsector", "stub.technology", Y ) ] ),
             vecpaste( L254.StubTranTechLoadFactor[ c( "region", supp, "tranSubsector", "stub.technology", Y ) ] ) ) ]
L254.StubTranTechOutput$coefficient <- L254.StubTranTechCoef$coefficient[
      match( vecpaste( L254.StubTranTechOutput[ c( "region", supp, "tranSubsector", "stub.technology", Y ) ] ),
             vecpaste( L254.StubTranTechCoef[ c( "region", supp, "tranSubsector", "stub.technology", Y ) ] ) ) ]
L254.StubTranTechOutput <- within( L254.StubTranTechOutput,
      output <- calibrated.value * loadFactor * conv_EJ_GJ / ( coefficient * conv_btu_kJ ) )

#The next step is to bind rows with all pass-through technologies on to this table
#Write all possible pass-through technologies to all regions
L254.StubTechCalInput_passthru <- repeat_and_add_vector(
      A54.globaltech_passthru, Y, model_base_years )
L254.StubTechCalInput_passthru <- write_to_all_regions(
      L254.StubTechCalInput_passthru, c( names_tranSubsector, tech, Y, input ) )
names( L254.StubTechCalInput_passthru )[ names( L254.StubTechCalInput_passthru ) == "technology" ] <- "stub.technology"

#Subset only the passthrough technologies that are applicable in each region
L254.StubTechCalInput_passthru <- L254.StubTechCalInput_passthru[
      vecpaste( L254.StubTechCalInput_passthru[ names_StubTranTech ] ) %in%
      vecpaste( L254.StubTech_passthru[ names_StubTranTech ] ), ]

#Start with a 0 value for output, and bind this to the table of output by tranTechnology (using only columns whose names match)
L254.StubTechCalInput_passthru$output <- 0
L254.StubTechCalInput_passthru <- rbind(
      L254.StubTranTechOutput[ names( L254.StubTechCalInput_passthru ) ],
      L254.StubTechCalInput_passthru )

for( i in 1:nrow( L254.StubTechCalInput_passthru ) ){
	L254.StubTechCalInput_passthru[i, "output"] <- aggregate_passthroughs( L254.StubTechCalInput_passthru, i )
}

#Then, remove the technologies that are not pass-through sectors, and rename "output" to "calibrated.value"
L254.StubTechCalInput_passthru <- L254.StubTechCalInput_passthru[
      vecpaste( L254.StubTechCalInput_passthru[ names_StubTranTech ] ) %in%
      vecpaste( L254.StubTech_passthru[ names_StubTranTech ] ), ]
names( L254.StubTechCalInput_passthru )[ names( L254.StubTechCalInput_passthru ) == "output" ] <- "calibrated.value"
L254.StubTechCalInput_passthru$share.weight.year <- L254.StubTechCalInput_passthru$year
L254.StubTechCalInput_passthru$subs.share.weight <- ifelse( L254.StubTechCalInput_passthru$calibrated.value > 0, 1, 0 )
L254.StubTechCalInput_passthru$tech.share.weight <- ifelse( L254.StubTechCalInput_passthru$calibrated.value > 0, 1, 0 )

#2f. non-motorized transportation - service output
printlog( "L254.StubTechProd_nonmotor: service output of non-motorized transportation technologies" )
L254.StubTechProd_nonmotor <- interpolate_and_melt(
      L154.out_mpkm_R_trn_nonmotor_Yh, model_base_years, value.name = "calOutputValue", digits = digits_mpkm )
L254.StubTechProd_nonmotor <- add_region_name( L254.StubTechProd_nonmotor )
L254.StubTechProd_nonmotor[ s_tS_t ] <- A54.globaltech_nonmotor[
      match( L254.StubTechProd_nonmotor$mode,
             A54.globaltech_nonmotor$tranSubsector ),
      s_tS_t ]
L254.StubTechProd_nonmotor$stub.technology <- L254.StubTechProd_nonmotor$technology
#There is no need to match shareweights to the calOutputValue because no region should ever have a 0 here
L254.StubTechProd_nonmotor <- L254.StubTechProd_nonmotor[ c( names_StubTranTechYr, "calOutputValue" ) ]

printlog( "L254.PerCapitaBased_trn: per-capita based flag for transportation final demand" )
L254.PerCapitaBased_trn <- write_to_all_regions( A54.demand, names_PerCapitaBased )

printlog( "L254.PriceElasticity_trn: price elasticity of transportation final demand" )
#Price elasticities are only applied to future periods. Application in base years will cause solution failure
L254.PriceElasticity_trn <- repeat_and_add_vector( A54.demand, Y, model_future_years )
L254.PriceElasticity_trn <- write_to_all_regions( L254.PriceElasticity_trn, names_PriceElasticity )

printlog( "L254.IncomeElasticity_trn: income elasticity of transportation final demand" )
#Income elasticities are only applied to future periods
L254.IncomeElasticity_trn <- repeat_and_add_vector( A54.demand, Y, model_future_years )
L254.IncomeElasticity_trn <- write_to_all_regions( L254.IncomeElasticity_trn, names_IncomeElasticity )

printlog( "L254.BaseService_trn: base-year service output of transportation final demand" )
L254.AllTechOutput <- rbind(
      L254.StubTranTechOutput[ names_StubTranTechYr ],
      L254.StubTechProd_nonmotor[ names_StubTranTechYr ] )
L254.AllTechOutput$base.service <- c(
      L254.StubTranTechOutput$output,
      L254.StubTechProd_nonmotor$calOutputValue )
L254.AllTechOutput$energy.final.demand <- A54.sector$energy.final.demand[
      match( L254.AllTechOutput$supplysector, A54.sector$supplysector ) ]
L254.BaseService_trn <- aggregate( L254.AllTechOutput[ "base.service" ],
      by=as.list( L254.AllTechOutput[ c( names_EnergyFinalDemand, "year" ) ] ), sum )
      
# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table in names ( L254.SectorLogitTables) ) {
write_mi_data( L254.SectorLogitTables[[ curr_table ]]$data, L254.SectorLogitTables[[ curr_table ]]$header,
    "ENERGY_LEVEL2_DATA", paste0("L254.", L254.SectorLogitTables[[ curr_table ]]$header ), "ENERGY_XML_BATCH",
    paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
}
write_mi_data( L254.Supplysector_trn, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L254.Supplysector_trn",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file=paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) ) 
write_mi_data( L254.FinalEnergyKeyword_trn, "FinalEnergyKeyword", "ENERGY_LEVEL2_DATA", "L254.FinalEnergyKeyword_trn", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) ) 
for( curr_table in names ( L254.SubsectorLogitTables ) ) {
write_mi_data( L254.SubsectorLogitTables[[ curr_table ]]$data, L254.SubsectorLogitTables[[ curr_table ]]$header,
    "ENERGY_LEVEL2_DATA", paste0("L254.", L254.SubsectorLogitTables[[ curr_table ]]$header ), "ENERGY_XML_BATCH",
    paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
}
write_mi_data( L254.tranSubsectorLogit, "tranSubsectorLogit", "ENERGY_LEVEL2_DATA", "L254.tranSubsectorLogit", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) ) 
if( exists( "L254.SubsectorShrwt_trn" ) ){
	write_mi_data( L254.tranSubsectorShrwt, "tranSubsectorShrwt", "ENERGY_LEVEL2_DATA", "L254.tranSubsectorShrwt", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
	}
if( exists( "L254.tranSubsectorShrwtFllt" ) ){
	write_mi_data( L254.tranSubsectorShrwtFllt, "tranSubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L254.tranSubsectorShrwtFllt",
	               "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) ) 
	}
if( exists( "L254.tranSubsectorInterp" ) ) {
	write_mi_data( L254.tranSubsectorInterp, "tranSubsectorInterp", "ENERGY_LEVEL2_DATA", "L254.tranSubsectorInterp", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
	}
if( exists( "L254.tranSubsectorInterpTo" ) ) {
	write_mi_data( L254.tranSubsectorInterpTo, "tranSubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L254.tranSubsectorInterpTo", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
	}
write_mi_data( L254.tranSubsectorSpeed, "tranSubsectorSpeed", "ENERGY_LEVEL2_DATA", "L254.tranSubsectorSpeed", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.tranSubsectorSpeed_passthru, "tranSubsectorSpeed", "ENERGY_LEVEL2_DATA", "L254.tranSubsectorSpeed_passthru", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.tranSubsectorSpeed_noVOTT, "tranSubsectorSpeed", "ENERGY_LEVEL2_DATA", "L254.tranSubsectorSpeed_noVOTT", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.tranSubsectorSpeed_nonmotor, "tranSubsectorSpeed", "ENERGY_LEVEL2_DATA", "L254.tranSubsectorSpeed_nonmotor", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.tranSubsectorVOTT, "tranSubsectorVOTT", "ENERGY_LEVEL2_DATA", "L254.tranSubsectorVOTT", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.tranSubsectorFuelPref, "tranSubsectorFuelPref", "ENERGY_LEVEL2_DATA", "L254.tranSubsectorFuelPref", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.StubTranTech, "StubTranTech", "ENERGY_LEVEL2_DATA", "L254.StubTranTech", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.StubTech_passthru, "StubTranTech", "ENERGY_LEVEL2_DATA", "L254.StubTech_passthru", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.StubTech_nonmotor, "StubTranTech", "ENERGY_LEVEL2_DATA", "L254.StubTech_nonmotor", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.GlobalTechShrwt_passthru, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L254.GlobalTechShrwt_passthru", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.GlobalTechShrwt_nonmotor, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L254.GlobalTechShrwt_nonmotor", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.GlobalTechCoef_passthru, "GlobalTechCoef", "ENERGY_LEVEL2_DATA", "L254.GlobalTechCoef_passthru", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.GlobalRenewTech_nonmotor, "GlobalRenewTech", "ENERGY_LEVEL2_DATA", "L254.GlobalRenewTech_nonmotor", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.GlobalTranTechInterp, "GlobalTranTechInterp", "ENERGY_LEVEL2_DATA", "L254.GlobalTranTechInterp", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.GlobalTranTechShrwt, "GlobalTranTechShrwt", "ENERGY_LEVEL2_DATA", "L254.GlobalTranTechShrwt", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.GlobalTranTechSCurve, "GlobalTranTechSCurve", "ENERGY_LEVEL2_DATA", "L254.GlobalTranTechSCurve", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.StubTranTechCalInput, "StubTranTechCalInput", "ENERGY_LEVEL2_DATA", "L254.StubTranTechCalInput", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.StubTranTechLoadFactor, "StubTranTechLoadFactor", "ENERGY_LEVEL2_DATA", "L254.StubTranTechLoadFactor", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.StubTranTechCost, "StubTranTechCost", "ENERGY_LEVEL2_DATA", "L254.StubTranTechCost", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.StubTranTechCoef, "StubTranTechCoef", "ENERGY_LEVEL2_DATA", "L254.StubTranTechCoef", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.StubTechCalInput_passthru, "StubTranTechCalInput", "ENERGY_LEVEL2_DATA", "L254.StubTechCalInput_passthru", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.StubTechProd_nonmotor, "StubTranTechProd", "ENERGY_LEVEL2_DATA", "L254.StubTechProd_nonmotor", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.PerCapitaBased_trn, "PerCapitaBased", "ENERGY_LEVEL2_DATA", "L254.PerCapitaBased_trn", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.PriceElasticity_trn, "PriceElasticity", "ENERGY_LEVEL2_DATA", "L254.PriceElasticity_trn", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.IncomeElasticity_trn, "IncomeElasticity", "ENERGY_LEVEL2_DATA", "L254.IncomeElasticity_trn", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )
write_mi_data( L254.BaseService_trn, "BaseService", "ENERGY_LEVEL2_DATA", "L254.BaseService_trn", "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ) )

insert_file_into_batchxml( "ENERGY_XML_BATCH", paste0( "batch_transportation_UCD_", trn_SSP, ".xml" ), "ENERGY_XML_FINAL", paste0( "transportation_UCD_", trn_SSP, ".xml" ), "", xml_tag="outFile" )

logstop()
