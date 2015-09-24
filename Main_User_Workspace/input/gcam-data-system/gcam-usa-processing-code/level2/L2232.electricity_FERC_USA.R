if( !exists( "GCAMUSAPROC_DIR" ) ){
    if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
        GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "L2232.electricity_FERC_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for GCAM USA electricity sectors with demand resolved at the grid region level" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )

#NOTE: this code file only builds the electric sector model input if the demand is being resolved at the level of the grid regions
if( use_regional_elec_markets ){	
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
A23.sector <- readdata( "ENERGY_ASSUMPTIONS", "A23.sector" )
A232.structure <- readdata( "GCAMUSA_ASSUMPTIONS", "A232.structure" )
L123.in_EJ_state_ownuse_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.in_EJ_state_ownuse_elec" )
L123.out_EJ_state_ownuse_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.out_EJ_state_ownuse_elec" )
L126.in_EJ_state_td_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L126.in_EJ_state_td_elec" )
L126.out_EJ_state_td_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L126.out_EJ_state_td_elec" )
L132.out_EJ_state_indchp_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L132.out_EJ_state_indchp_F" )
L223.Supplysector_elec <- readdata( "ENERGY_LEVEL2_DATA", "L223.Supplysector_elec", skip = 4 )
L1232.out_EJ_sR_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L1232.out_EJ_sR_elec" )
L223.StubTechMarket_backup_USA <- readdata( "GCAMUSA_LEVEL2_DATA", "L223.StubTechMarket_backup_USA", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Perform computations
grid_regions <- sort( unique( states_subregions$grid_region ) )

printlog( "PART 1: THE USA REGION" )
# Remove the USA electricity sector, and replace with electricity trade
printlog( "L2232.DeleteSupplysector_USAelec: Removing the electricity sectors of the USA region (incl. net_ownuse)" )
L2232.DeleteSupplysector_USAelec <- data.frame( region = "USA", supplysector = c( "electricity", "electricity_net_ownuse" ) )
write_mi_data( L2232.DeleteSupplysector_USAelec, "DeleteSupplysector", "GCAMUSA_LEVEL2_DATA", "L2232.DeleteSupplysector_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )

printlog( "L2232.Supplysector_USAelec: supplysector for electricity sector in the USA region, including logit exponent between grid regions" )
#All of the supplysector information is the same as before, except the logit exponent
A232.USAstructure <- subset( A232.structure, region == "USA" )
L2232.Supplysector_USAelec <- data.frame(
      A232.USAstructure[ names( A232.structure ) %in% names_Supplysector ],
      logit.year.fillout = min( model_base_years ), logit.exponent = A232.USAstructure$subsector.logit,
      logit.type = A232.USAstructure$subsector.logit.type )
L2232.SectorLogitTables_USAelec <- get_logit_fn_tables( L2232.Supplysector_USAelec, names_SupplysectorLogitType,
      base.header="Supplysector_", include.equiv.table=T, write.all.regions=F )
L2232.Supplysector_USAelec <- L2232.Supplysector_USAelec[, names_Supplysector ]

#No need to read in subsector logit exponents, which are applied to the technology competition
printlog( "L2232.SubsectorShrwtFllt_USAelec: subsector (grid region) shareweights in USA electricity trade" )
L2232.SubsectorShrwtFllt_USAelec <- repeat_and_add_vector( A232.USAstructure[ names( A232.USAstructure ) %in% names_Subsector ], "grid_region", grid_regions )
for( i in 1:length( grid_regions ) ){
	L2232.SubsectorShrwtFllt_USAelec$subsector[i] <- sub( "grid_region", L2232.SubsectorShrwtFllt_USAelec$grid_region[i], L2232.SubsectorShrwtFllt_USAelec$subsector[i] )
}
L2232.SubsectorShrwtFllt_USAelec$year.fillout <- min( model_base_years )
L2232.SubsectorShrwtFllt_USAelec$share.weight <- 1
L2232.SubsectorShrwtFllt_USAelec <- L2232.SubsectorShrwtFllt_USAelec[ names_SubsectorShrwtFllt ]

printlog( "L2232.SubsectorInterp_USAelec: subsector (grid region) shareweights in USA electricity" )
printlog( "NOTE: this just carries the base year shareweights forward; regions that don't export in the base year don't export at all" )
L2232.SubsectorInterp_USAelec <- data.frame(
      L2232.SubsectorShrwtFllt_USAelec[ names_Subsector ],
      apply.to = "share-weight", from.year = max( model_base_years ), to.year = max( model_years ),
      interpolation.function = "fixed" )

# NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
L2232.SubsectorLogit_USAelec <- data.frame(
      L2232.SubsectorShrwtFllt_USAelec[ names_Subsector ],
      logit.year.fillout = min( model_base_years ), logit.exponent = A232.USAstructure$technology.logit,
      logit.type=A232.USAstructure$technology.logit.type )
L2232.SubsectorLogitTables_USAelec <- get_logit_fn_tables( L2232.SubsectorLogit_USAelec, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )
L2232.SubsectorLogit_USAelec <- L2232.SubsectorLogit_USAelec[, names_SubsectorLogit ]

printlog( "L2232.TechShrwt_USAelec: technology shareweights, USA region" )
L2232.TechShrwt_USAelec <- repeat_and_add_vector( A232.USAstructure[ names_Tech ], "grid_region", grid_regions )
for( i in 1:length( grid_regions ) ){
	L2232.TechShrwt_USAelec$subsector[i] <- sub( "grid_region", L2232.TechShrwt_USAelec$grid_region[i], L2232.TechShrwt_USAelec$subsector[i] )
	L2232.TechShrwt_USAelec$technology[i] <- sub( "grid_region", L2232.TechShrwt_USAelec$grid_region[i], L2232.TechShrwt_USAelec$technology[i] )
}
L2232.TechShrwt_USAelec <- repeat_and_add_vector( L2232.TechShrwt_USAelec, Y, model_years )
L2232.TechShrwt_USAelec$share.weight <- 1
L2232.TechShrwt_USAelec <- L2232.TechShrwt_USAelec[ c( names_TechYr, "share.weight", "grid_region" ) ]

printlog( "L2232.TechCoef_USAelec: technology coefficients and market names, USA region")
L2232.TechCoef_USAelec <- L2232.TechShrwt_USAelec
L2232.TechCoef_USAelec[[input]] <- A232.USAstructure[[input]]
L2232.TechCoef_USAelec$coefficient <- 1
L2232.TechCoef_USAelec$market.name <- L2232.TechCoef_USAelec$grid_region
L2232.TechCoef_USAelec <- L2232.TechCoef_USAelec[ names_TechCoef ]

printlog( "Compiling flows of electricity in each FERC region: generation, cogeneration, ownuse, and consumption by all sectors" )
L2232.out_EJ_sR_elec.melt <- interpolate_and_melt( L1232.out_EJ_sR_elec, model_base_years, value.name = "generation" )
L2232.out_EJ_sR_elec.melt$region <- "USA"
L2232.elec_flows_FERC <- subset( L2232.TechShrwt_USAelec, year %in% model_base_years )
L2232.elec_flows_FERC$generation <- L2232.out_EJ_sR_elec.melt$generation[
      match( vecpaste( L2232.elec_flows_FERC[ c( "grid_region", Y ) ] ),
             vecpaste( L2232.out_EJ_sR_elec.melt[ c( "grid_region", Y ) ] ) ) ]

printlog( "Cogeneration is not included in the grid region totals; need to add it here for balance" )
L2232.out_EJ_state_indchp_F.melt <- interpolate_and_melt( L132.out_EJ_state_indchp_F, model_base_years, value.name = "cogeneration" )
L2232.out_EJ_state_indchp_F.melt$region <- states_subregions$grid_region[
      match( L2232.out_EJ_state_indchp_F.melt$state, states_subregions$state ) ]
L2232.out_EJ_sR_indchp_F.melt <- aggregate( L2232.out_EJ_state_indchp_F.melt["cogeneration"],
      by=as.list( L2232.out_EJ_state_indchp_F.melt[ c( reg, Y ) ] ), sum )
L2232.elec_flows_FERC$cogeneration <- L2232.out_EJ_sR_indchp_F.melt$cogeneration[
      match( vecpaste( L2232.elec_flows_FERC[ c( "grid_region", Y ) ] ),
             vecpaste( L2232.out_EJ_sR_indchp_F.melt[ c( "region", Y ) ] ) ) ]

#Subtract own use of electricity prior to calculating net exports
printlog( "Calculating own use in each FERC region" )
L2232.net_EJ_state_ownuse_elec <- L123.in_EJ_state_ownuse_elec
L2232.net_EJ_state_ownuse_elec[ X_historical_years ] <- L123.in_EJ_state_ownuse_elec[ X_historical_years ] - L123.out_EJ_state_ownuse_elec[ X_historical_years ]

L2232.net_EJ_state_ownuse_elec.melt <- interpolate_and_melt( L2232.net_EJ_state_ownuse_elec, model_base_years, value.name = "ownuse" )
L2232.net_EJ_state_ownuse_elec.melt$grid_region <- states_subregions$grid_region[
      match( L2232.net_EJ_state_ownuse_elec.melt$state, states_subregions$state ) ]
L2232.net_EJ_sR_ownuse_elec.melt <- aggregate( L2232.net_EJ_state_ownuse_elec.melt["ownuse"],
      by=as.list( L2232.net_EJ_state_ownuse_elec.melt[ c( "grid_region", Y ) ] ), sum )
L2232.elec_flows_FERC$ownuse <- L2232.net_EJ_sR_ownuse_elec.melt$ownuse[
      match( vecpaste( L2232.elec_flows_FERC[ c( "grid_region", Y ) ] ),
             vecpaste( L2232.net_EJ_sR_ownuse_elec.melt[ c( "grid_region", Y ) ] ) ) ]

printlog( "Indicating the sum of all demands in each FERC region (equal to the input to the elect_td sectors)" )
L2232.in_EJ_state_td_elec.melt <- interpolate_and_melt( L126.in_EJ_state_td_elec, model_base_years, value.name = "consumption" )
L2232.in_EJ_state_td_elec.melt$grid_region <- states_subregions$grid_region[
      match( L2232.in_EJ_state_td_elec.melt$state, states_subregions$state ) ]
L2232.in_EJ_sR_td_elec.melt <- aggregate( L2232.in_EJ_state_td_elec.melt[ "consumption" ],
      by=as.list( L2232.in_EJ_state_td_elec.melt[ c( "grid_region", Y ) ] ), sum )

L2232.elec_flows_FERC$consumption <- L2232.in_EJ_sR_td_elec.melt$consumption[
      match( vecpaste( L2232.elec_flows_FERC[ c( "grid_region", Y ) ] ),
             vecpaste( L2232.in_EJ_sR_td_elec.melt[ c( "grid_region", Y ) ] ) ) ]

printlog( "Calculating net exports: generation + cogeneration - ownuse - consumption" )
L2232.elec_flows_FERC$net.exports <- with( L2232.elec_flows_FERC, generation + cogeneration - ownuse - consumption )

printlog( "Split net exports into gross imports and exports")
L2232.elec_flows_FERC$imports <- with( L2232.elec_flows_FERC, pmax( 0, -1 * net.exports ) )
L2232.elec_flows_FERC$exports <- with( L2232.elec_flows_FERC, pmax( 0, net.exports ) )

#Calculate consumption from domestic sources: total consumption minus imports
L2232.elec_flows_FERC$net.supply <- with( L2232.elec_flows_FERC, consumption - imports )

printlog( "L2232.Production_exports_USAelec: calibrated exports of electricity from grid regions to shared USA region" )
L2232.Production_exports_USAelec <- L2232.elec_flows_FERC[ names_TechYr ]
L2232.Production_exports_USAelec$calOutputValue <- round( L2232.elec_flows_FERC$exports, digits_calOutput )
L2232.Production_exports_USAelec$share.weight.year <- L2232.Production_exports_USAelec$year
L2232.Production_exports_USAelec <- set_subsector_shrwt( L2232.Production_exports_USAelec )
L2232.Production_exports_USAelec$tech.share.weight <- ifelse( L2232.Production_exports_USAelec$calOutputValue == 0, 0, 1 )

printlog( "PART 2: THE FERC REGIONS" )
printlog( "Some of the information read in about these regions is in the primary electricity_USA code file" )
printlog( "L2232.Supplysector_elec_FERC: supplysector information for electricity passthru sectors in the FERC regions" )
A232.FERCstructure <- repeat_and_add_vector( subset( A232.structure, region == "grid_region" ), "region", grid_regions )
for( i in 1:nrow( A232.FERCstructure ) ){
	for( j in 2:ncol( A232.FERCstructure ) ){
        if( grepl( 'grid_region', A232.FERCstructure[i,j] ) ) {
            A232.FERCstructure[i,j] <- sub( "grid_region", A232.FERCstructure$region[i], A232.FERCstructure[i,j] )
        }
	}
}
L2232.Supplysector_elec_FERC <- A232.FERCstructure[ names( A232.FERCstructure  ) %in% names_Supplysector ]
L2232.Supplysector_elec_FERC$logit.year.fillout <- min( model_base_years )
L2232.Supplysector_elec_FERC$logit.exponent <- A232.FERCstructure$subsector.logit
L2232.Supplysector_elec_FERC$logit.type <- A232.FERCstructure$subsector.logit.type
L2232.SectorLogitTables_elec_FERC <- get_logit_fn_tables( L2232.Supplysector_elec_FERC, names_SupplysectorLogitType,
    base.header="Supplysector_", include.equiv.table=F, write.all.regions=F )
L2232.Supplysector_elec_FERC <- L2232.Supplysector_elec_FERC[, names_Supplysector ]

printlog( "L2232.ElecReserve_FERC: electricity reserve margin and avg grid capacity factor in the FERC regions" )
L2232.ElecReserve_FERC <- repeat_and_add_vector( subset( A23.sector, supplysector == "electricity" ), "region", grid_regions )[ names_ElecReserve ]

printlog( "L2232.SubsectorShrwtFllt_elec_FERC: subsector (grid region) shareweights in USA electricity" )
L2232.SubsectorShrwtFllt_elec_FERC <- A232.FERCstructure[ names_Subsector ]
L2232.SubsectorShrwtFllt_elec_FERC$year.fillout <- min( model_base_years )
L2232.SubsectorShrwtFllt_elec_FERC$share.weight <- 1

printlog( "L2232.SubsectorInterp_elec_FERC: subsector (grid region) shareweights in USA electricity" )
L2232.SubsectorInterp_elec_FERC <- data.frame(
      L2232.SubsectorShrwtFllt_elec_FERC[ names_Subsector ],
      apply.to = "share-weight", from.year = max( model_base_years ), to.year = max( model_years ),
      interpolation.function = "fixed" )

# NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
L2232.SubsectorLogit_elec_FERC <- data.frame(
      L2232.SubsectorShrwtFllt_elec_FERC[ names_Subsector ],
      logit.year.fillout = min( model_base_years ), logit.exponent = A232.FERCstructure$technology.logit,
      logit.type=A232.FERCstructure$technology.logit.type )
L2232.SubsectorLogitTables_elec_FERC <- get_logit_fn_tables( L2232.SubsectorLogit_elec_FERC, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )
L2232.SubsectorLogit_elec_FERC <- L2232.SubsectorLogit_elec_FERC[, names_SubsectorLogit ]

printlog( "L2232.TechShrwt_elec_FERC: technology shareweights, USA region" )
L2232.TechShrwt_elec_FERC <- repeat_and_add_vector( A232.FERCstructure[ names_Tech ], Y, model_years )
L2232.TechShrwt_elec_FERC$share.weight <- 1

printlog( "L2232.TechCoef_elec_FERC: technology coefficients and market names")
L2232.TechCoef_elec_FERC <- repeat_and_add_vector( A232.FERCstructure, Y, model_years )[ c( names_TechYr, input, "market.name" ) ]
#Own use will be done separately; extract it from the table here
L2232.TechCoef_elecownuse_FERC <- subset( L2232.TechCoef_elec_FERC, supplysector == "electricity_net_ownuse" )
L2232.TechCoef_elec_FERC <- subset( L2232.TechCoef_elec_FERC, supplysector != "electricity_net_ownuse" )
L2232.TechCoef_elec_FERC$coefficient <- 1
L2232.TechCoef_elec_FERC <- L2232.TechCoef_elec_FERC[ names_TechCoef ]

printlog( "L2232.TechCoef_elecownuse_FERC: own use coefficients in the grid regions" )
L2232.elec_flows_FERC$ownuse_coef <- with( L2232.elec_flows_FERC, ( generation + cogeneration ) / (generation + cogeneration - ownuse ) )
L2232.TechCoef_elecownuse_FERC$coefficient <- L2232.elec_flows_FERC$ownuse_coef[
      match( vecpaste( L2232.TechCoef_elecownuse_FERC[ c( reg, Y ) ] ),
             vecpaste( L2232.elec_flows_FERC[ c( "grid_region", Y ) ] ) ) ]
L2232.TechCoef_elecownuse_FERC$coefficient[ L2232.TechCoef_elecownuse_FERC$year %in% model_future_years ] <-
      L2232.TechCoef_elecownuse_FERC$coefficient[ L2232.TechCoef_elecownuse_FERC$year == max( model_base_years ) ]
L2232.TechCoef_elecownuse_FERC <- L2232.TechCoef_elecownuse_FERC[ names_TechCoef ]

printlog( "L2232.Production_imports_FERC: calibrated electricity imports (from USA region)" )
L2232.Production_imports_FERC <- subset( L2232.TechCoef_elec_FERC, year %in% model_base_years & market.name == "USA" )[ names_TechYr ]
L2232.Production_imports_FERC$calOutputValue <-
      round( L2232.elec_flows_FERC$imports[
          match( vecpaste( L2232.Production_imports_FERC[ c( reg, Y ) ] ),
                 vecpaste( L2232.elec_flows_FERC[ c( "grid_region", Y ) ] ) ) ],
      digits_calOutput )
L2232.Production_imports_FERC$share.weight.year <- L2232.Production_imports_FERC$year
L2232.Production_imports_FERC <- set_subsector_shrwt( L2232.Production_imports_FERC )
L2232.Production_imports_FERC$tech.share.weight <- ifelse( L2232.Production_imports_FERC$calOutputValue == 0, 0, 1 )

printlog( "L2232.Production_elec_gen_FERC: calibrated net electricity generation (from within grid region)" )
L2232.Production_elec_gen_FERC <- subset( L2232.TechCoef_elec_FERC, year %in% model_base_years & market.name != "USA" )[ names_TechYr ]
L2232.Production_elec_gen_FERC$calOutputValue <- round(
      L2232.elec_flows_FERC$net.supply[
          match( vecpaste( L2232.Production_elec_gen_FERC[ c( reg, Y ) ] ),
                 vecpaste( L2232.elec_flows_FERC[ c( "grid_region", Y ) ] ) ) ],
      digits_calOutput )
L2232.Production_elec_gen_FERC$share.weight.year <- L2232.Production_elec_gen_FERC$year
L2232.Production_elec_gen_FERC <- set_subsector_shrwt( L2232.Production_elec_gen_FERC )
L2232.Production_elec_gen_FERC$tech.share.weight <- ifelse( L2232.Production_elec_gen_FERC$calOutputValue == 0, 0, 1 )

printlog( "PART 3: THE STATES" )
printlog( "Resetting the electric sector market to the grid regions (for backup calculations)" )
printlog( "L2232.StubTechElecMarket_backup_USA_FERC: electric sector name" )
L2232.StubTechElecMarket_backup_USA <- L223.StubTechMarket_backup_USA[ names_StubTechYr ]
L2232.StubTechElecMarket_backup_USA$electric.sector.market <- states_subregions$grid_region[
      match( L2232.StubTechElecMarket_backup_USA$region, states_subregions$state ) ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table in names ( L2232.SectorLogitTables_USAelec ) ) {
write_mi_data( L2232.SectorLogitTables_USAelec[[ curr_table ]]$data, L2232.SectorLogitTables_USAelec[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L2232.", L2232.SectorLogitTables_USAelec[[ curr_table ]]$header, "_USA" ), "GCAMUSA_XML_BATCH",
    "batch_electricity_USA.xml" )
}
write_mi_data( L2232.Supplysector_USAelec, "Supplysector", "GCAMUSA_LEVEL2_DATA", "L2232.Supplysector_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L2232.SubsectorShrwtFllt_USAelec, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L2232.SubsectorShrwtFllt_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L2232.SubsectorInterp_USAelec, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L2232.SubsectorInterp_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
for( curr_table in names ( L2232.SubsectorLogitTables_USAelec ) ) {
write_mi_data( L2232.SubsectorLogitTables_USAelec[[ curr_table ]]$data, L2232.SubsectorLogitTables_USAelec[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L2232.", L2232.SubsectorLogitTables_USAelec[[ curr_table ]]$header, "_USA" ), "GCAMUSA_XML_BATCH",
    "batch_electricity_USA.xml" )
}
write_mi_data( L2232.SubsectorLogit_USAelec, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L2232.SubsectorLogit_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L2232.TechShrwt_USAelec, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L2232.TechShrwt_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L2232.TechCoef_USAelec, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L2232.TechCoef_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L2232.Production_exports_USAelec, "Production", "GCAMUSA_LEVEL2_DATA", "L2232.Production_exports_USAelec", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )

for( curr_table in names ( L2232.SectorLogitTables_elec_FERC ) ) {
write_mi_data( L2232.SectorLogitTables_elec_FERC[[ curr_table ]]$data, L2232.SectorLogitTables_elec_FERC[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L2232.", L2232.SectorLogitTables_elec_FERC[[ curr_table ]]$header, "_FERC" ), "GCAMUSA_XML_BATCH",
    "batch_electricity_USA.xml" )
}
write_mi_data( L2232.Supplysector_elec_FERC, "Supplysector", "GCAMUSA_LEVEL2_DATA", "L2232.Supplysector_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L2232.ElecReserve_FERC, "ElecReserve", "GCAMUSA_LEVEL2_DATA", "L2232.ElecReserve_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L2232.SubsectorShrwtFllt_elec_FERC, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L2232.SubsectorShrwtFllt_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L2232.SubsectorInterp_elec_FERC, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L2232.SubsectorInterp_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
for( curr_table in names ( L2232.SubsectorLogitTables_elec_FERC ) ) {
write_mi_data( L2232.SubsectorLogitTables_elec_FERC[[ curr_table ]]$data, L2232.SubsectorLogitTables_elec_FERC[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L2232.", L2232.SubsectorLogitTables_elec_FERC[[ curr_table ]]$header, "_FERC" ), "GCAMUSA_XML_BATCH",
    "batch_electricity_USA.xml" )
}
write_mi_data( L2232.SubsectorLogit_elec_FERC, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L2232.SubsectorLogit_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L2232.TechShrwt_elec_FERC, "TechShrwt", "GCAMUSA_LEVEL2_DATA", "L2232.TechShrwt_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L2232.TechCoef_elec_FERC, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L2232.TechCoef_elec_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L2232.TechCoef_elecownuse_FERC, "TechCoef", "GCAMUSA_LEVEL2_DATA", "L2232.TechCoef_elecownuse_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )

write_mi_data( L2232.Production_imports_FERC, "Production", "GCAMUSA_LEVEL2_DATA", "L2232.Production_imports_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
write_mi_data( L2232.Production_elec_gen_FERC, "Production", "GCAMUSA_LEVEL2_DATA", "L2232.Production_elec_gen_FERC", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )

write_mi_data( L2232.StubTechElecMarket_backup_USA, "StubTechElecMarket", "GCAMUSA_LEVEL2_DATA", "L2232.StubTechElecMarket_backup_USA", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml", "GCAMUSA_XML_FINAL", "electricity_USA.xml", "", xml_tag="outFile" )

} #close out from use_regional_elec_markets

logstop()
