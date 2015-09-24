# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu processing scripts, please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "L202.an_input.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for animal production (including inputs to animal produciton)" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_agRsrc <- readdata( "AGLU_ASSUMPTIONS", "A_agRsrc" )
A_agSubRsrc <- readdata( "AGLU_ASSUMPTIONS", "A_agSubRsrc" )
A_agRsrcCurves <- readdata( "AGLU_ASSUMPTIONS", "A_agRsrcCurves" )
A_an_input_supplysector <- readdata( "AGLU_ASSUMPTIONS", "A_an_input_supplysector" )
A_an_input_subsector <- readdata( "AGLU_ASSUMPTIONS", "A_an_input_subsector" )
A_an_input_technology <- readdata( "AGLU_ASSUMPTIONS", "A_an_input_technology" )
A_an_supplysector <- readdata( "AGLU_ASSUMPTIONS", "A_an_supplysector" )
A_an_subsector <- readdata( "AGLU_ASSUMPTIONS", "A_an_subsector" )
A_an_technology <- readdata( "AGLU_ASSUMPTIONS", "A_an_technology" )
L107.an_Prod_Mt_R_C_Sys_Fd_Y <- readdata( "AGLU_LEVEL1_DATA", "L107.an_Prod_Mt_R_C_Sys_Fd_Y" )
L107.an_FeedIO_R_C_Sys_Fd_Y <- readdata( "AGLU_LEVEL1_DATA", "L107.an_FeedIO_R_C_Sys_Fd_Y" )
L107.an_Feed_Mt_R_C_Sys_Fd_Y <- readdata( "AGLU_LEVEL1_DATA", "L107.an_Feed_Mt_R_C_Sys_Fd_Y" )
L108.ag_Feed_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L108.ag_Feed_Mt_R_C_Y" )
L109.an_ALL_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L109.an_ALL_Mt_R_C_Y" )
L132.ag_an_For_Prices <- readdata( "AGLU_LEVEL1_DATA", "L132.ag_an_For_Prices" )

# -----------------------------------------------------------------------------
# 2. Build tables
#Base table for resources
printlog( "Melting and adding region names to Level1 data tables" )
L202.an_Prod_Mt_R_C_Sys_Fd_Y.melt <- interpolate_and_melt( L107.an_Prod_Mt_R_C_Sys_Fd_Y, model_base_years )
L202.an_FeedIO_R_C_Sys_Fd_Y.melt <- interpolate_and_melt( L107.an_FeedIO_R_C_Sys_Fd_Y, model_base_years )
L202.an_Feed_Mt_R_C_Sys_Fd_Y.melt <- interpolate_and_melt( L107.an_Feed_Mt_R_C_Sys_Fd_Y, model_base_years )
L202.ag_Feed_Mt_R_C_Y.melt <- interpolate_and_melt( L108.ag_Feed_Mt_R_C_Y, model_base_years )

L202.an_Prod_Mt_R_C_Sys_Fd_Y.melt <- add_region_name( L202.an_Prod_Mt_R_C_Sys_Fd_Y.melt )
L202.an_FeedIO_R_C_Sys_Fd_Y.melt <- add_region_name( L202.an_FeedIO_R_C_Sys_Fd_Y.melt )
L202.an_Feed_Mt_R_C_Sys_Fd_Y.melt <- add_region_name( L202.an_Feed_Mt_R_C_Sys_Fd_Y.melt )
L202.ag_Feed_Mt_R_C_Y.melt <- add_region_name( L202.ag_Feed_Mt_R_C_Y.melt )
L202.an_ALL_Mt_R_C_Y <- add_region_name( L109.an_ALL_Mt_R_C_Y )

printlog( "L202.RenewRsrc: generic resource attributes" )
L202.RenewRsrc <- write_to_all_regions_ag( A_agRsrc, names_RenewRsrc )
L202.RenewRsrc$market[ L202.RenewRsrc$market == "regional" ] <- L202.RenewRsrc$region[ L202.RenewRsrc$market == "regional" ]

printlog( "L202.RenewRsrcPrice: resource prices" )
L202.RenewRsrcPrice <- L202.RenewRsrc[ c( "region", "renewresource" ) ]
L202.RenewRsrcPrice$year <- min( model_base_years )
L202.RenewRsrcPrice$price <- 1

printlog( "L202.RenewRsrcCalProd: Calibrated production of resources" )
L202.RenewRsrcCalProd <- write_to_all_regions_ag( A_agSubRsrc, names = names_SubRenewRsrc )
L202.RenewRsrcCalProd <- repeat_and_add_vector( L202.RenewRsrcCalProd, Y, model_base_years )

#Paste in cal-production from Animal products table, where appropriate.
L202.RenewRsrcCalProd_an <- L202.RenewRsrcCalProd[ L202.RenewRsrcCalProd$sub.renewable.resource %in% L202.an_ALL_Mt_R_C_Y[[C]], ]
L202.RenewRsrcCalProd_an$cal.production <- round( L202.an_ALL_Mt_R_C_Y$Prod_Mt[
      match( vecpaste( L202.RenewRsrcCalProd_an[ c( reg, SubRenewRsrc, Y ) ] ),
             vecpaste( L202.an_ALL_Mt_R_C_Y[ c( reg, C, Y ) ] ) ) ],
      digits_calOutput )

#Paste in cal-production from Feed table, where appropriate.
L202.RenewRsrcCalProd_feed <- L202.RenewRsrcCalProd[ L202.RenewRsrcCalProd$sub.renewable.resource %in% L202.ag_Feed_Mt_R_C_Y.melt[[C]], ]
L202.RenewRsrcCalProd_feed$cal.production <- round( L202.ag_Feed_Mt_R_C_Y.melt$value[
      match( vecpaste( L202.RenewRsrcCalProd_feed[ c( reg, SubRenewRsrc, Y ) ] ),
             vecpaste( L202.ag_Feed_Mt_R_C_Y.melt[ c( reg, C, Y ) ] ) ) ],
      digits_calOutput )

#Rbind the two tables
L202.RenewRsrcCalProd <- rbind( L202.RenewRsrcCalProd_an, L202.RenewRsrcCalProd_feed )[ names_RenewRsrcCalProd ]

printlog( "L202.maxSubResource: maximum amount of resource production allowed in any period" )
#Compute the maxsubresource as the maximum of all base periods, for each region and resource
L202.maxSubResource <- aggregate( L202.RenewRsrcCalProd$cal.production, by=as.list( L202.RenewRsrcCalProd[ names_SubRenewRsrc ] ), max )
names( L202.maxSubResource )[ names( L202.maxSubResource ) == "x" ] <- "maxSubResource"
L202.maxSubResource <- L202.maxSubResource[ names_maxSubResource ]

printlog( "L202.RenewRsrcCurves" )
L202.RenewRsrcCurves <- write_to_all_regions_ag( A_agRsrcCurves, names_RenewRsrcCurves )

printlog( "L202.Supplysector_in: generic supplysector info for inputs to animal production" )
L202.SectorLogitTables_in <- get_logit_fn_tables( A_an_input_supplysector, names_SupplysectorLogitType,
    base.header="Supplysector_", include.equiv.table=T, write.all.regions=T )
L202.Supplysector_in <- write_to_all_regions_ag( A_an_input_supplysector, names_Supplysector )

printlog( "L202.SubsectorAll_in: generic subsector info for inputs to animal production technologies" )
L202.SubsectorLogitTables_in <- get_logit_fn_tables( A_an_input_subsector, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=T )
L202.SubsectorAll_in <- write_to_all_regions_ag( A_an_input_subsector, names_SubsectorAll )

printlog( "L202.StubTech_in: identification of stub technologies for inputs to animal production" )
L202.StubTech_in <- write_to_all_regions_ag( A_an_input_technology, names_Tech )
names( L202.StubTech_in ) <- gsub( "technology", "stub.technology", names( L202.StubTech_in ) )

printlog( "L202.StubTechInterp_in: generic technology info for inputs to animal production" )
L202.StubTechInterp_in <- write_to_all_regions_ag( A_an_input_technology, names_TechInterp )
names( L202.StubTechInterp_in ) <- gsub( "technology", "stub.technology", names( L202.StubTechInterp_in ) )

printlog( "L202.GlobalTechCoef_in: coefficients for inputs to animal production" )
L202.GlobalTechCoef_in <- repeat_and_add_vector( A_an_input_technology, Y, c( model_base_years, model_future_years ) )
L202.GlobalTechCoef_in[ c( "sector.name", "subsector.name" ) ] <- L202.GlobalTechCoef_in[ c( supp, subs ) ]
L202.GlobalTechCoef_in <- L202.GlobalTechCoef_in[ names_GlobalTechCoef ]

printlog( "L202.StubTechProd_in: base year output of the inputs (feed types) to animal production" )
L202.StubTechProd_in <- write_to_all_regions_ag( A_an_input_technology, names_Tech )
L202.StubTechProd_in$stub.technology <- L202.StubTechProd_in$technology
L202.StubTechProd_in <- repeat_and_add_vector( L202.StubTechProd_in, Y, model_base_years )
L202.StubTechProd_in$calOutputValue <- round( L202.ag_Feed_Mt_R_C_Y.melt$value[
      match( vecpaste( L202.StubTechProd_in[ c( reg, tech, Y ) ] ),
             vecpaste( L202.ag_Feed_Mt_R_C_Y.melt[ c( reg, C, Y ) ] ) ) ],
      digits_calOutput )
#Subsector and technology shareweights (subsector requires the year as well)
L202.StubTechProd_in$share.weight.year <- L202.StubTechProd_in$year
L202.StubTechProd_in$subs.share.weight <- ifelse( L202.StubTechProd_in$calOutputValue > 0, 1, 0 )
L202.StubTechProd_in$tech.share.weight <- ifelse( L202.StubTechProd_in$calOutputValue > 0, 1, 0 )
L202.StubTechProd_in <- L202.StubTechProd_in[ names_StubTechProd]

printlog( "L202.Supplysector_an: generic animal production supplysector info" )
L202.SectorLogitTables_an <- get_logit_fn_tables( A_an_supplysector, names_SupplysectorLogitType,
    base.header="Supplysector_", include.equiv.table=F, write.all.regions=T )
L202.Supplysector_an <- write_to_all_regions_ag( A_an_supplysector, names_Supplysector )

printlog( "L202.SubsectorAll_an: generic animal production subsector info" )
L202.SubsectorLogitTables_an <- get_logit_fn_tables( A_an_subsector, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=T )
L202.SubsectorAll_an <- write_to_all_regions_ag( A_an_subsector, names_SubsectorAll )

printlog( "L202.StubTech_an: identification of stub technologies for animal production" )
L202.StubTech_an <- write_to_all_regions_ag( A_an_technology, names_Tech )
names( L202.StubTech_an ) <- gsub( "technology", "stub.technology", names( L202.StubTech_an ) )

printlog( "L202.StubTechInterp_an: shareweight interpolation for animal production technologies" )
L202.StubTechInterp_an <- write_to_all_regions_ag( A_an_technology, names_TechInterp )
names( L202.StubTechInterp_an ) <- gsub( "technology", "stub.technology", names( L202.StubTechInterp_an ) )

printlog( "L202.StubTechProd_an: animal production by technology and region" )
L202.StubTechProd_an <- write_to_all_regions_ag( A_an_technology, names_Tech )
L202.StubTechProd_an$stub.technology <- L202.StubTechProd_an$technology
L202.StubTechProd_an <- repeat_and_add_vector( L202.StubTechProd_an, Y, model_base_years )
L202.StubTechProd_an$calOutputValue <- round( L202.an_Prod_Mt_R_C_Sys_Fd_Y.melt$value[
      match( vecpaste( L202.StubTechProd_an[ c( reg, supp, subs, tech, Y ) ] ),
             vecpaste( L202.an_Prod_Mt_R_C_Sys_Fd_Y.melt[ c( reg, C, Sys, Fd, Y ) ] ) ) ],
      digits_calOutput )
#Subsector and technology shareweights (subsector requires the year as well)
L202.StubTechProd_an$share.weight.year <- L202.StubTechProd_an$year
L202.StubTechProd_an$subs.share.weight <- ifelse( L202.StubTechProd_an$calOutputValue > 0, 1, 0 )
L202.StubTechProd_an$tech.share.weight <- ifelse( L202.StubTechProd_an$calOutputValue > 0, 1, 0 )
L202.StubTechProd_an <- L202.StubTechProd_an[ names_StubTechProd]

#Note that some subsectors here have multiple technologies, so shareweights should be derived from aggregation
L202.an_subs_sw <- aggregate( L202.StubTechProd_an[ "calOutputValue" ],
      by=as.list( L202.StubTechProd_an[ c( reg, supp, subs, Y ) ] ), sum)
L202.an_subs_sw$share.weight <- ifelse( L202.an_subs_sw$calOutputValue > 0, 1, 0 )

#Over-ride the shareweights in the production table
L202.StubTechProd_an$subs.share.weight <- L202.an_subs_sw$share.weight[
      match( vecpaste( L202.StubTechProd_an[ c( reg, supp, subs, Y ) ] ),
             vecpaste( L202.an_subs_sw[ c( reg, supp, subs, Y ) ] ) ) ]

printlog( "L202.StubTechCoef_an: animal production input-output coefficients by technology and region" )
L202.StubTechCoef_an <- write_to_all_regions_ag( A_an_technology, c( names_Tech, input, "market.name" ) )
L202.StubTechCoef_an$stub.technology <- L202.StubTechCoef_an$technology
L202.StubTechCoef_an <- repeat_and_add_vector( L202.StubTechCoef_an, Y, c( model_base_years, model_future_years ) )
L202.StubTechCoef_an$coefficient <- round( L202.an_FeedIO_R_C_Sys_Fd_Y.melt$value[
      match( vecpaste( L202.StubTechCoef_an[ c( reg, supp, subs, input, Y ) ] ),
             vecpaste( L202.an_FeedIO_R_C_Sys_Fd_Y.melt[ c( reg, C, Sys, Fd, Y ) ] ) ) ],
      digits_calOutput )

#For values beyond the coefficient time series, use the final available year
final_coef_year <- max( L202.an_FeedIO_R_C_Sys_Fd_Y.melt$year )
L202.StubTechCoef_an$coefficient[ L202.StubTechCoef_an$year > max( final_coef_year ) ] <-    
      L202.StubTechCoef_an$coefficient[ L202.StubTechCoef_an$year == max( final_coef_year ) ]
L202.StubTechCoef_an <- L202.StubTechCoef_an[ names_StubTechCoef ]

#Supplemental calculation of non-input cost of animal production
printlog( "Calculating non-feed costs of animal production based on US commodity prices and feed costs" )
#First, calculate the weighted average price across the different feed types (supplysectors)
Index_region <- GCAM_region_names$region[ 1 ]
L202.ag_Feed_P_share_R_C <- L202.StubTechProd_in[
      L202.StubTechProd_in$region == Index_region & L202.StubTechProd_in$year == max( model_base_years ),
      c( names_StubTech, "calOutputValue" ) ]
L202.ag_Feed_Mt_R_F <- aggregate( L202.ag_Feed_P_share_R_C[ "calOutputValue" ],
      by=as.list( L202.ag_Feed_P_share_R_C[ c( reg, supp ) ] ), sum )
L202.ag_Feed_P_share_R_C$output_supplysector <- L202.ag_Feed_Mt_R_F$calOutputValue[
      match( vecpaste( L202.ag_Feed_P_share_R_C[ c( reg, supp ) ] ),
             vecpaste( L202.ag_Feed_Mt_R_F[ c( reg, supp ) ] ) ) ]
L202.ag_Feed_P_share_R_C$share_Fd <- L202.ag_Feed_P_share_R_C$calOutputValue / L202.ag_Feed_P_share_R_C$output_supplysector
L202.ag_Feed_P_share_R_C$price <- L132.ag_an_For_Prices$calPrice[
      match( L202.ag_Feed_P_share_R_C$stub.technology, L132.ag_an_For_Prices$GCAM_commodity ) ]
L202.ag_Feed_P_share_R_C$price[ L202.ag_Feed_P_share_R_C$stub.technology %in% A_agRsrcCurves$sub.renewable.resource ] <-
      A_agRsrcCurves$extractioncost[
          match( paste( L202.ag_Feed_P_share_R_C$stub.technology[ L202.ag_Feed_P_share_R_C$stub.technology %in% A_agRsrcCurves$sub.renewable.resource ], "grade 2" ), 
                 paste( A_agRsrcCurves$sub.renewable.resource, A_agRsrcCurves$grade ) ) ]
L202.ag_Feed_P_share_R_C$wtd_price <- L202.ag_Feed_P_share_R_C$share_Fd * L202.ag_Feed_P_share_R_C$price
L202.ag_FeedCost_USDkg_R_F <- aggregate( L202.ag_Feed_P_share_R_C[ "wtd_price" ],
      by=as.list( L202.ag_Feed_P_share_R_C[ c( reg, supp ) ] ), sum )

#Calculate the total cost of all inputs, for each animal commodity, first matching in the feed quantity and the price
L202.an_FeedCost_R_C_Sys_Fd <- subset( L202.an_Prod_Mt_R_C_Sys_Fd_Y.melt, year == max( model_base_years ) & region == Index_region )
L202.an_FeedCost_R_C_Sys_Fd$Feed_Mt <- L202.an_Feed_Mt_R_C_Sys_Fd_Y.melt$value[
      match( vecpaste( L202.an_FeedCost_R_C_Sys_Fd[ R_C_Sys_Fd_Y ] ),
             vecpaste( L202.an_Feed_Mt_R_C_Sys_Fd_Y.melt[ R_C_Sys_Fd_Y ] ) ) ]
L202.an_FeedCost_R_C_Sys_Fd$FeedPrice_USDkg <- L202.ag_FeedCost_USDkg_R_F$wtd_price[
      match( vecpaste( L202.an_FeedCost_R_C_Sys_Fd[ c( reg, Fd ) ] ),
             vecpaste( L202.ag_FeedCost_USDkg_R_F[ c( reg, supp ) ] ) ) ]

#Multiply price by quantity to calculate feed expenditure, and aggregat expenditure and production to get weighted avg cost
L202.an_FeedCost_R_C_Sys_Fd$FeedCost_bilUSD <- L202.an_FeedCost_R_C_Sys_Fd$Feed_Mt * L202.an_FeedCost_R_C_Sys_Fd$FeedPrice_USDkg
L202.an_FeedCost_R_C <- aggregate( L202.an_FeedCost_R_C_Sys_Fd[ c( "value", "FeedCost_bilUSD" ) ],
      by=as.list( L202.an_FeedCost_R_C_Sys_Fd[ R_C ] ), sum )
L202.an_FeedCost_R_C$FeedCost_USDkg <- L202.an_FeedCost_R_C$FeedCost_bilUSD / L202.an_FeedCost_R_C$value
L202.an_FeedCost_R_C$CommodityPrice_USDkg <- L132.ag_an_For_Prices$calPrice[
      match( L202.an_FeedCost_R_C[[C]], L132.ag_an_For_Prices[[C]]) ]
L202.an_FeedCost_R_C$nonFeedCost <- 
      pmax( min_an_noninput_cost, ( L202.an_FeedCost_R_C$CommodityPrice_USDkg - L202.an_FeedCost_R_C$FeedCost_USDkg ) )

printlog( "L202.GlobalTechCost_an: costs of animal production technologies")
L202.GlobalTechCost_an <- repeat_and_add_vector( A_an_technology, Y, c( model_base_years, model_future_years ) )
L202.GlobalTechCost_an[ c( "sector.name", "subsector.name" ) ] <- L202.GlobalTechCost_an[ c( supp, subs ) ]
L202.GlobalTechCost_an$minicam.non.energy.input <- "non-energy"
L202.GlobalTechCost_an$input.cost <- round( L202.an_FeedCost_R_C$nonFeedCost[
      match( L202.GlobalTechCost_an$supplysector, L202.an_FeedCost_R_C[[C]] ) ],
      digits_calPrice )
L202.GlobalTechCost_an <- L202.GlobalTechCost_an[ names_GlobalTechCost ]

printlog( "L202.GlobalRenewTech_imp_an: generic technology info for animal imports" )
L202.GlobalRenewTech_imp_an <- data.frame(
      sector.name = rep( A_an_supplysector$supplysector, times = length( c( model_base_years, model_future_years ) ) ),
      subsector.name = "Imports", technology = "Imports", renewable.input = "renewable",
      year = sort( rep( c( model_base_years, model_future_years ), times = length( A_an_supplysector$supplysector ) ) ) )
L202.GlobalRenewTech_imp_an <- write_to_all_regions_ag( L202.GlobalRenewTech_imp_an, names_GlobalRenewTech )

printlog( "L202.StubTechFixOut_imp_an: animal imports for net importing regions in all periods" )
L202.StubTechFixOut_imp_an <- data.frame( supplysector = A_an_supplysector$supplysector, subsector = "Imports", stub.technology = "Imports" )
#Write to all regions before repeating by years so that future year values will copy correctly
L202.StubTechFixOut_imp_an <- write_to_all_regions_ag( L202.StubTechFixOut_imp_an, names_StubTech )
L202.StubTechFixOut_imp_an <- repeat_and_add_vector( L202.StubTechFixOut_imp_an, Y, c( model_base_years, model_future_years ) )
L202.StubTechFixOut_imp_an$fixedOutput <- pmax( 0, round( -1 * L202.an_ALL_Mt_R_C_Y$NetExp_Mt[
      match( vecpaste( L202.StubTechFixOut_imp_an[ c( reg, supp, Y ) ] ),
             vecpaste( L202.an_ALL_Mt_R_C_Y[ c( reg, C_Y ) ] ) ) ],
      digits_calOutput ) )
L202.StubTechFixOut_imp_an$share.weight.year <- L202.StubTechFixOut_imp_an$year
L202.StubTechFixOut_imp_an$subs.share.weight <- 0
L202.StubTechFixOut_imp_an$tech.share.weight <- 0
L202.StubTechFixOut_imp_an <- L202.StubTechFixOut_imp_an[ names_StubTechFixOut ]

#For values beyond the final base year, copy the final base year forward
#NOTE: This is complicated. Currently the base-service read in to the model is not used in years after the final calibration year.
# If actual historical values are used here in historical years after the final calibration year, the model will not solve as the
# supply of the Exports_* markets will be set while the demand will be purely inelastic, carried forward with no changes from the
# final calibration year. This could theoretically be overcome in most instances by reading in time- and region-specific income
# elasticities that return the correct values for each historical year after the final calibration year. This is not done, and is not
# recommended, as any regions that switch between imports and exports will have elasiticies of +/- Inf; the historical data can not be
# represented. If the model code is changed to allow the base-service to be prescribed beyond the final calibration year, then the following line may be used:
#final_an_exp_year <- max( L202.an_ALL_Mt_R_C_Y$year )

final_an_exp_year <- max( model_base_years )
L202.StubTechFixOut_imp_an$fixedOutput[ L202.StubTechFixOut_imp_an$year > max( final_an_exp_year ) ] <-    
      L202.StubTechFixOut_imp_an$fixedOutput[ L202.StubTechFixOut_imp_an$year == max( final_an_exp_year ) ]
L202.StubTechFixOut_imp_an <- L202.StubTechFixOut_imp_an[ names_StubTechFixOut ]

#Remove any regions for which agriculture and land use are not modeled
L202.RenewRsrc <- subset( L202.RenewRsrc, !region %in% no_aglu_regions )
L202.RenewRsrcPrice <- subset( L202.RenewRsrcPrice, !region %in% no_aglu_regions )
L202.RenewRsrcCalProd <- subset( L202.RenewRsrcCalProd, !region %in% no_aglu_regions )
L202.maxSubResource <- subset( L202.maxSubResource, !region %in% no_aglu_regions )
L202.RenewRsrcCurves <- subset( L202.RenewRsrcCurves, !region %in% no_aglu_regions )
for( curr_table in names( L202.SectorLogitTables_in ) ) {
    if( curr_table != "EQUIV_TABLE" ) {
        L202.SectorLogitTables_in[[ curr_table ]]$data <- subset( L202.SectorLogitTables_in[[ curr_table ]]$data,
            !region %in% no_aglu_regions )
    }
}
L202.Supplysector_in <- subset( L202.Supplysector_in, !region %in% no_aglu_regions )
for( curr_table in names( L202.SubsectorLogitTables_in ) ) {
    if( curr_table != "EQUIV_TABLE" ) {
        L202.SubsectorLogitTables_in[[ curr_table ]]$data <- subset( L202.SubsectorLogitTables_in[[ curr_table ]]$data,
            !region %in% no_aglu_regions )
    }
}
L202.SubsectorAll_in <- subset( L202.SubsectorAll_in, !region %in% no_aglu_regions )
L202.StubTech_in <- subset( L202.StubTech_in, !region %in% no_aglu_regions )
L202.StubTechInterp_in <- subset( L202.StubTechInterp_in, !region %in% no_aglu_regions )
L202.StubTechProd_in <- subset( L202.StubTechProd_in, !region %in% no_aglu_regions )
for( curr_table in names( L202.SectorLogitTables_an ) ) {
    if( curr_table != "EQUIV_TABLE" ) {
        L202.SectorLogitTables_an[[ curr_table ]]$data <- subset( L202.SectorLogitTables_an[[ curr_table ]]$data,
            !region %in% no_aglu_regions )
    }
}
L202.Supplysector_an <- subset( L202.Supplysector_an, !region %in% no_aglu_regions )
for( curr_table in names( L202.SubsectorLogitTables_an) ) {
    if( curr_table != "EQUIV_TABLE" ) {
        L202.SubsectorLogitTables_an[[ curr_table ]]$data <- subset( L202.SubsectorLogitTables_an[[ curr_table ]]$data,
            !region %in% no_aglu_regions )
    }
}
L202.SubsectorAll_an <- subset( L202.SubsectorAll_an, !region %in% no_aglu_regions )
L202.StubTech_an <- subset( L202.StubTech_an, !region %in% no_aglu_regions )
L202.StubTechInterp_an <- subset( L202.StubTechInterp_an, !region %in% no_aglu_regions )
L202.StubTechProd_an <- subset( L202.StubTechProd_an, !region %in% no_aglu_regions )
L202.StubTechCoef_an <- subset( L202.StubTechCoef_an, !region %in% no_aglu_regions )
L202.StubTechFixOut_imp_an <- subset( L202.StubTechFixOut_imp_an, !region %in% no_aglu_regions )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L202.RenewRsrc, IDstring="RenewRsrc", domain="AGLU_LEVEL2_DATA", fn="L202.RenewRsrc",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_an_input.xml" ) 
write_mi_data( L202.RenewRsrcPrice, "RenewRsrcPrice", "AGLU_LEVEL2_DATA", "L202.RenewRsrcPrice", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.maxSubResource, "maxSubResource", "AGLU_LEVEL2_DATA", "L202.maxSubResource", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.RenewRsrcCurves, "RenewRsrcCurves", "AGLU_LEVEL2_DATA", "L202.RenewRsrcCurves", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
for( curr_table in names ( L202.SectorLogitTables_in ) ) {
write_mi_data( L202.SectorLogitTables_in[[ curr_table ]]$data, L202.SectorLogitTables_in[[ curr_table ]]$header,
    "AGLU_LEVEL2_DATA", paste0("L202.", L202.SectorLogitTables_in[[ curr_table ]]$header, "_in" ), "AGLU_XML_BATCH",
    "batch_an_input.xml" )
}
write_mi_data( L202.Supplysector_in, "Supplysector", "AGLU_LEVEL2_DATA", "L202.Supplysector_in", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
for( curr_table in names ( L202.SubsectorLogitTables_in ) ) {
write_mi_data( L202.SubsectorLogitTables_in[[ curr_table ]]$data, L202.SubsectorLogitTables_in[[ curr_table ]]$header,
    "AGLU_LEVEL2_DATA", paste0("L202.", L202.SubsectorLogitTables_in[[ curr_table ]]$header, "_in" ), "AGLU_XML_BATCH",
    "batch_an_input.xml" )
}
write_mi_data( L202.SubsectorAll_in, "SubsectorAll", "AGLU_LEVEL2_DATA", "L202.SubsectorAll_in", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.StubTech_in, "StubTech", "AGLU_LEVEL2_DATA", "L202.StubTech_in", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.StubTechInterp_in, "StubTechInterp", "AGLU_LEVEL2_DATA", "L202.StubTechInterp_in", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.GlobalTechCoef_in, "GlobalTechCoef", "AGLU_LEVEL2_DATA", "L202.GlobalTechCoef_in", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.StubTechProd_in, "StubTechProd", "AGLU_LEVEL2_DATA", "L202.StubTechProd_in", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
for( curr_table in names ( L202.SectorLogitTables_an ) ) {
write_mi_data( L202.SectorLogitTables_an[[ curr_table ]]$data, L202.SectorLogitTables_an[[ curr_table ]]$header,
    "AGLU_LEVEL2_DATA", paste0("L202.", L202.SectorLogitTables_an[[ curr_table ]]$header, "_an" ), "AGLU_XML_BATCH",
    "batch_an_input.xml" )
}
write_mi_data( L202.Supplysector_an, "Supplysector", "AGLU_LEVEL2_DATA", "L202.Supplysector_an", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
for( curr_table in names ( L202.SubsectorLogitTables_an ) ) {
write_mi_data( L202.SubsectorLogitTables_an[[ curr_table ]]$data, L202.SubsectorLogitTables_an[[ curr_table ]]$header,
    "AGLU_LEVEL2_DATA", paste0("L202.", L202.SubsectorLogitTables_an[[ curr_table ]]$header, "_an" ), "AGLU_XML_BATCH",
    "batch_an_input.xml" )
}
write_mi_data( L202.SubsectorAll_an, "SubsectorAll", "AGLU_LEVEL2_DATA", "L202.SubsectorAll_an", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.StubTech_an, "StubTech", "AGLU_LEVEL2_DATA", "L202.StubTech_an", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.StubTechInterp_an, "StubTechInterp", "AGLU_LEVEL2_DATA", "L202.StubTechInterp_an", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.StubTechProd_an, "StubTechProd", "AGLU_LEVEL2_DATA", "L202.StubTechProd_an", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.StubTechCoef_an, "StubTechCoef", "AGLU_LEVEL2_DATA", "L202.StubTechCoef_an", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.GlobalTechCost_an, "GlobalTechCost", "AGLU_LEVEL2_DATA", "L202.GlobalTechCost_an", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.GlobalRenewTech_imp_an, "GlobalRenewTech", "AGLU_LEVEL2_DATA", "L202.GlobalRenewTech_imp_an", "AGLU_XML_BATCH", "batch_an_input.xml" ) 
write_mi_data( L202.StubTechFixOut_imp_an, "StubTechFixOut", "AGLU_LEVEL2_DATA", "L202.StubTechFixOut_imp_an", "AGLU_XML_BATCH", "batch_an_input.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_an_input.xml", "AGLU_XML_FINAL", "an_input.xml", "", xml_tag="outFile" )

logstop()
