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
logstart( "L203.demand_input.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for demands of all aglu commodities (crop, animal, forest)" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_demand_supplysector <- readdata( "AGLU_ASSUMPTIONS", "A_demand_supplysector" )
A_demand_subsector <- readdata( "AGLU_ASSUMPTIONS", "A_demand_subsector" )
A_demand_technology <- readdata( "AGLU_ASSUMPTIONS", "A_demand_technology" )
A_fuelprefElasticity_ssp1 <- readdata( "AGLU_ASSUMPTIONS", "A_fuelprefElasticity_ssp1" )
L101.ag_Food_Pcal_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L101.ag_Food_Pcal_R_C_Y" )
L101.ag_kcalg_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L101.ag_kcalg_R_C_Y" )
L105.an_Food_Pcal_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L105.an_Food_Pcal_R_C_Y" )
L105.an_kcalg_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L105.an_kcalg_R_C_Y" )
L109.ag_ALL_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L109.ag_ALL_Mt_R_C_Y" )
L109.an_ALL_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L109.an_ALL_Mt_R_C_Y" )
L110.For_ALL_bm3_R_Y <- readdata( "AGLU_LEVEL1_DATA", "L110.For_ALL_bm3_R_Y" )
L134.pcFood_kcald_R_Dmnd_Y <- readdata( "AGLU_LEVEL1_DATA", "L134.pcFood_kcald_R_Dmnd_Y" )
L134.pcFood_kcald_R_Dmnd_Y_ssp1 <- readdata( "AGLU_LEVEL1_DATA", "L134.pcFood_kcald_R_Dmnd_Y_ssp1" )
L134.pcFood_kcald_R_Dmnd_Y_ssp2 <- readdata( "AGLU_LEVEL1_DATA", "L134.pcFood_kcald_R_Dmnd_Y_ssp2" )
L134.pcFood_kcald_R_Dmnd_Y_ssp3 <- readdata( "AGLU_LEVEL1_DATA", "L134.pcFood_kcald_R_Dmnd_Y_ssp3" )
L134.pcFood_kcald_R_Dmnd_Y_ssp4 <- readdata( "AGLU_LEVEL1_DATA", "L134.pcFood_kcald_R_Dmnd_Y_ssp4" )
L134.pcFood_kcald_R_Dmnd_Y_ssp5 <- readdata( "AGLU_LEVEL1_DATA", "L134.pcFood_kcald_R_Dmnd_Y_ssp5" )
L101.Pop_thous_R_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_R_Yh" )
L102.pcgdp_thous90USD_SSP_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_SSP_R_Y" )

# -----------------------------------------------------------------------------
# 2. Build tables
# NOTE: This is somewhat complicated. Unlike demands in the energy system, the aglu demands are treated as exogenous
# in all historical periods, regardless of which model base years are used. Processing data for all historical years
# that are also model years (i.e. not just the model base years)
aglu_demand_calyears <- historical_years[ historical_years %in% model_years ]
X_aglu_demand_calyears <- paste0( "X", aglu_demand_calyears )
aglu_demand_futureyears <- c( max( aglu_demand_calyears  ), model_years[ !model_years %in% aglu_demand_calyears ] )
X_aglu_demand_futureyears <- paste0( "X", aglu_demand_futureyears )

L203.ag_Food_Pcal_R_C_Y.melt <- interpolate_and_melt( L101.ag_Food_Pcal_R_C_Y, aglu_demand_calyears )
L203.ag_kcalg_R_C_Y.melt <- interpolate_and_melt( L101.ag_kcalg_R_C_Y, aglu_demand_calyears )
L203.an_Food_Pcal_R_C_Y.melt <- interpolate_and_melt( L105.an_Food_Pcal_R_C_Y, aglu_demand_calyears )
L203.an_kcalg_R_C_Y.melt <- interpolate_and_melt( L105.an_kcalg_R_C_Y, aglu_demand_calyears )
L203.pcFood_kcald_R_Dmnd_Y.melt <- interpolate_and_melt( L134.pcFood_kcald_R_Dmnd_Y, aglu_demand_futureyears )
L203.pcFood_kcald_R_Dmnd_Y_ssp1.melt <- interpolate_and_melt( L134.pcFood_kcald_R_Dmnd_Y_ssp1, aglu_demand_futureyears )
L203.pcFood_kcald_R_Dmnd_Y_ssp2.melt <- interpolate_and_melt( L134.pcFood_kcald_R_Dmnd_Y_ssp2, aglu_demand_futureyears )
L203.pcFood_kcald_R_Dmnd_Y_ssp3.melt <- interpolate_and_melt( L134.pcFood_kcald_R_Dmnd_Y_ssp3, aglu_demand_futureyears )
L203.pcFood_kcald_R_Dmnd_Y_ssp4.melt <- interpolate_and_melt( L134.pcFood_kcald_R_Dmnd_Y_ssp4, aglu_demand_futureyears )
L203.pcFood_kcald_R_Dmnd_Y_ssp5.melt <- interpolate_and_melt( L134.pcFood_kcald_R_Dmnd_Y_ssp5, aglu_demand_futureyears )
L203.Pop_thous_R_Yh <- interpolate_and_melt( L101.Pop_thous_R_Yh, aglu_demand_calyears )
L203.pcgdp_thous90USD_SSP_R_Y.melt <- interpolate_and_melt( L102.pcgdp_thous90USD_SSP_R_Y, c( model_base_years, model_future_years ) )

#Adding lookup vectors to level1 output tables
printlog( "Adding region names to Level1 data tables" )
L203.ag_Food_Pcal_R_C_Y.melt <- add_region_name( L203.ag_Food_Pcal_R_C_Y.melt )
L203.ag_kcalg_R_C_Y.melt  <- add_region_name( L203.ag_kcalg_R_C_Y.melt )
L203.an_Food_Pcal_R_C_Y.melt <- add_region_name( L203.an_Food_Pcal_R_C_Y.melt )
L203.an_kcalg_R_C_Y.melt  <- add_region_name( L203.an_kcalg_R_C_Y.melt )
L203.ag_ALL_Mt_R_C_Y <- add_region_name( L109.ag_ALL_Mt_R_C_Y )
L203.an_ALL_Mt_R_C_Y <- add_region_name( L109.an_ALL_Mt_R_C_Y )
L203.For_ALL_bm3_R_Y <- add_region_name( L110.For_ALL_bm3_R_Y )
L203.pcFood_kcald_R_Dmnd_Y.melt <- add_region_name( L203.pcFood_kcald_R_Dmnd_Y.melt )
L203.pcFood_kcald_R_Dmnd_Y_ssp1.melt <- add_region_name( L203.pcFood_kcald_R_Dmnd_Y_ssp1.melt )
L203.pcFood_kcald_R_Dmnd_Y_ssp2.melt <- add_region_name( L203.pcFood_kcald_R_Dmnd_Y_ssp2.melt )
L203.pcFood_kcald_R_Dmnd_Y_ssp3.melt <- add_region_name( L203.pcFood_kcald_R_Dmnd_Y_ssp3.melt )
L203.pcFood_kcald_R_Dmnd_Y_ssp4.melt <- add_region_name( L203.pcFood_kcald_R_Dmnd_Y_ssp4.melt )
L203.pcFood_kcald_R_Dmnd_Y_ssp5.melt <- add_region_name( L203.pcFood_kcald_R_Dmnd_Y_ssp5.melt )
L203.Pop_thous_R_Yh <- add_region_name( L203.Pop_thous_R_Yh )
L203.pcgdp_thous90USD_SSP_R_Y.melt <- add_region_name( L203.pcgdp_thous90USD_SSP_R_Y.melt )

printlog( "L203.Supplysector_demand: generic info for demand sectors" )
L203.SectorLogitTables <- get_logit_fn_tables( A_demand_supplysector, names_SupplysectorLogitType,
    base.header="Supplysector_", include.equiv.table=T, write.all.regions=T )
L203.Supplysector_demand <- write_to_all_regions_ag( A_demand_supplysector, names_Supplysector )

printlog( "L203.SubsectorAll_demand: generic info for demand subsectors" )
L203.SubsectorLogitTables <- get_logit_fn_tables( A_demand_subsector, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=T )
L203.SubsectorAll_demand <- write_to_all_regions_ag( A_demand_subsector, names_SubsectorAll )

printlog( "L203.StubTech_demand: identification of stub technologies for demands" )
L203.StubTech_demand <- write_to_all_regions_ag( A_demand_technology, names_Tech )
names( L203.StubTech_demand ) <- gsub( "technology", "stub.technology", names( L203.StubTech_demand ) )

printlog( "L203.GlobalTechCoef_demand: input names of demand technologies" )
L203.GlobalTechCoef_demand <- repeat_and_add_vector( A_demand_technology, Y, c( model_base_years, model_future_years ) )
L203.GlobalTechCoef_demand[ c( "sector.name", "subsector.name" ) ] <- L203.GlobalTechCoef_demand[ c( "supplysector", "subsector" ) ]
L203.GlobalTechCoef_demand <- L203.GlobalTechCoef_demand[ names_GlobalTechCoef ]

printlog( "L203.GlobalTechShrwt_demand: shareweights of demand technologies" )
L203.GlobalTechShrwt_demand <- L203.GlobalTechCoef_demand[ names_GlobalTechYr ]
L203.GlobalTechShrwt_demand$share.weight <- 1

#Calibrated food and nonfood demands of crops and meat
#Create table of regions, technologies, and all base years
# NOTE: Easiest if the model base years are subsetted from a full table as a last step in the construction of each of these tables
A_demand_technology_R <- write_to_all_regions_ag( A_demand_technology, c( names_Tech, "minicam.energy.input", "market.name" ) )
A_demand_technology_R$stub.technology <- A_demand_technology_R$technology
A_demand_technology_R_Yh <- repeat_and_add_vector( A_demand_technology_R, Y, aglu_demand_calyears )
A_demand_technology_R_Y <- repeat_and_add_vector( A_demand_technology_R, Y, c( model_base_years, model_future_years ) )

printlog( "L203.StubTechProd_food_crop: crop food demand by technology and region" )
L203.StubTechProd_food_crop <- subset( A_demand_technology_R_Yh, supplysector == "FoodDemand_Crops" )
L203.StubTechProd_food_crop$calOutputValue <- round( L203.ag_Food_Pcal_R_C_Y.melt$value[
      match( vecpaste( L203.StubTechProd_food_crop[ c( "region", "technology", Y ) ] ),
             vecpaste( L203.ag_Food_Pcal_R_C_Y.melt[ c( "region", C, Y ) ] ) ) ],
      digits_calOutput )
#Subsector and technology shareweights (subsector requires the year as well)
L203.StubTechProd_food_crop$share.weight.year <- L203.StubTechProd_food_crop$year
L203.StubTechProd_food_crop$subs.share.weight <- ifelse( L203.StubTechProd_food_crop$calOutputValue > 0, 1, 0 )
L203.StubTechProd_food_crop$tech.share.weight <- ifelse( L203.StubTechProd_food_crop$calOutputValue > 0, 1, 0 )
L203.StubTechProd_food_crop <- L203.StubTechProd_food_crop[ names_StubTechProd]

printlog( "L203.StubTechProd_food_meat: meat food demand by technology and region" )
L203.StubTechProd_food_meat <- subset( A_demand_technology_R_Yh, supplysector == "FoodDemand_Meat" )
L203.StubTechProd_food_meat$calOutputValue <- round( L203.an_Food_Pcal_R_C_Y.melt$value[
      match( vecpaste( L203.StubTechProd_food_meat[ c( "region", "technology", Y ) ] ),
             vecpaste( L203.an_Food_Pcal_R_C_Y.melt[ c( "region", C, Y ) ] ) ) ],
      digits_calOutput )
#Subsector and technology shareweights (subsector requires the year as well)
L203.StubTechProd_food_meat$share.weight.year <- L203.StubTechProd_food_meat$year
L203.StubTechProd_food_meat$subs.share.weight <- ifelse( L203.StubTechProd_food_meat$calOutputValue > 0, 1, 0 )
L203.StubTechProd_food_meat$tech.share.weight <- ifelse( L203.StubTechProd_food_meat$calOutputValue > 0, 1, 0 )
L203.StubTechProd_food_meat <- L203.StubTechProd_food_meat[ names_StubTechProd]

printlog( "L203.StubTechProd_nonfood_crop: crop nonfood demand by technology and region" )
L203.StubTechProd_nonfood_crop <- subset( A_demand_technology_R_Yh, supplysector == "NonFoodDemand_Crops" )
L203.StubTechProd_nonfood_crop$calOutputValue <- round( L203.ag_ALL_Mt_R_C_Y$OtherUses_Mt[
      match( vecpaste( L203.StubTechProd_nonfood_crop[ c( "region", "technology", Y ) ] ),
             vecpaste( L203.ag_ALL_Mt_R_C_Y[ c( "region", C, Y ) ] ) ) ],
      digits_calOutput )
#Subsector and technology shareweights (subsector requires the year as well)
L203.StubTechProd_nonfood_crop$share.weight.year <- L203.StubTechProd_nonfood_crop$year
L203.StubTechProd_nonfood_crop$subs.share.weight <- ifelse( L203.StubTechProd_nonfood_crop$calOutputValue > 0, 1, 0 )
L203.StubTechProd_nonfood_crop$tech.share.weight <- ifelse( L203.StubTechProd_nonfood_crop$calOutputValue > 0, 1, 0 )
L203.StubTechProd_nonfood_crop <- L203.StubTechProd_nonfood_crop[ names_StubTechProd]

printlog( "L203.StubTechProd_nonfood_meat: meat nonfood demand by technology and region" )
L203.StubTechProd_nonfood_meat <- subset( A_demand_technology_R_Yh, supplysector == "NonFoodDemand_Meat" )
L203.StubTechProd_nonfood_meat$calOutputValue <- round( L203.an_ALL_Mt_R_C_Y$OtherUses_Mt[
      match( vecpaste( L203.StubTechProd_nonfood_meat[ c( "region", "technology", Y ) ] ),
             vecpaste( L203.an_ALL_Mt_R_C_Y[ c( "region", C, Y ) ] ) ) ],
      digits_calOutput )
#Subsector and technology shareweights (subsector requires the year as well)
L203.StubTechProd_nonfood_meat$share.weight.year <- L203.StubTechProd_nonfood_meat$year
L203.StubTechProd_nonfood_meat$subs.share.weight <- ifelse( L203.StubTechProd_nonfood_meat$calOutputValue > 0, 1, 0 )
L203.StubTechProd_nonfood_meat$tech.share.weight <- ifelse( L203.StubTechProd_nonfood_meat$calOutputValue > 0, 1, 0 )
L203.StubTechProd_nonfood_meat <- L203.StubTechProd_nonfood_meat[ names_StubTechProd]

printlog( "L203.StubTechProd_For: Forest product demand by technology and region" )
L203.StubTechProd_For <- subset( A_demand_technology_R_Yh, supplysector == "NonFoodDemand_Forest" )
L203.StubTechProd_For$calOutputValue <- round( L203.For_ALL_bm3_R_Y$Cons_bm3[
      match( vecpaste( L203.StubTechProd_For[ c( "region", Y ) ] ),
             vecpaste( L203.For_ALL_bm3_R_Y[ c( "region", Y ) ] ) ) ],
      digits_calOutput )
#Subsector and technology shareweights (subsector requires the year as well)
L203.StubTechProd_For$share.weight.year <- L203.StubTechProd_For$year
L203.StubTechProd_For$subs.share.weight <- ifelse( L203.StubTechProd_For$calOutputValue > 0, 1, 0 )
L203.StubTechProd_For$tech.share.weight <- ifelse( L203.StubTechProd_For$calOutputValue > 0, 1, 0 )
L203.StubTechProd_For <- L203.StubTechProd_For[ names_StubTechProd]

printlog( "L203.StubTechFixOut_exp: animal exports for net exporting regions in all periods" )
L203.StubTechFixOut_exp <- subset( A_demand_technology_R_Y, supplysector == "Exports_Meat" )
L203.StubTechFixOut_exp$fixedOutput <- pmax( 0, round( L203.an_ALL_Mt_R_C_Y$NetExp_Mt[
      match( vecpaste( L203.StubTechFixOut_exp[ c( "region", "technology", Y ) ] ),
             vecpaste( L203.an_ALL_Mt_R_C_Y[ c( "region", C, Y ) ] ) ) ],
      digits_calOutput ) )
L203.StubTechFixOut_exp$fixedOutput[ L203.StubTechFixOut_exp$year > max( model_base_years ) ] <-
      L203.StubTechFixOut_exp$fixedOutput[ L203.StubTechFixOut_exp$year == max( model_base_years ) ]
L203.StubTechFixOut_exp$share.weight.year <- L203.StubTechFixOut_exp$year
L203.StubTechFixOut_exp$subs.share.weight <- 0
L203.StubTechFixOut_exp$tech.share.weight <- 0
L203.StubTechFixOut_exp <- L203.StubTechFixOut_exp[ names_StubTechFixOut]

printlog( "L203.StubCalorieContent_crop: caloric content of food crops (incl secondary products)" )
L203.StubCalorieContent_crop <- subset( A_demand_technology_R_Y, supplysector == "FoodDemand_Crops" )
L203.StubCalorieContent_crop$efficiency <- round( L203.ag_kcalg_R_C_Y.melt$value[
      match( vecpaste( L203.StubCalorieContent_crop[ c( "region", "technology", Y ) ] ),
             vecpaste( L203.ag_kcalg_R_C_Y.melt[ c( "region", C, Y ) ] ) ) ],
      digits_calOutput )
L203.StubCalorieContent_crop$efficiency[ L203.StubCalorieContent_crop$year > max( model_base_years ) ] <- 
      L203.StubCalorieContent_crop$efficiency[ L203.StubCalorieContent_crop$year == max( model_base_years) ]
L203.StubCalorieContent_crop <- L203.StubCalorieContent_crop[ names_StubTechCalorieContent ]

printlog( "L203.StubCalorieContent_meat: caloric content of meat commodities" )
L203.StubCalorieContent_meat <- subset( A_demand_technology_R_Y, supplysector == "FoodDemand_Meat" )
L203.StubCalorieContent_meat$efficiency <- round( L203.an_kcalg_R_C_Y.melt$value[
      match( vecpaste( L203.StubCalorieContent_meat[ c( "region", "technology", Y ) ] ),
             vecpaste( L203.an_kcalg_R_C_Y.melt[ c( "region", C, Y ) ] ) ) ],
      digits_calOutput )
L203.StubCalorieContent_meat$efficiency[ L203.StubCalorieContent_meat$year > max( model_base_years ) ] <- 
      L203.StubCalorieContent_meat$efficiency[ L203.StubCalorieContent_meat$year == max( model_base_years) ]
L203.StubCalorieContent_meat <- L203.StubCalorieContent_meat[ names_StubTechCalorieContent ]

#FINAL DEMANDS
printlog( "L203.PerCapitaBased: final demand attributes that do not vary by time period" )
L203.PerCapitaBased <- write_to_all_regions_ag( A_demand_supplysector, names_PerCapitaBased )

printlog( "L203.BaseService: base service of final demands" )
Prod_colnames <- c( "region", "supplysector", "year", "calOutputValue" )
L203.StubTechProd_exp <- subset( L203.StubTechFixOut_exp, year %in% model_base_years )
L203.StubTechProd_exp$calOutputValue <- L203.StubTechProd_exp$fixedOutput
L203.StubTechProd_all <- rbind( L203.StubTechProd_food_crop[ Prod_colnames ], L203.StubTechProd_food_meat[ Prod_colnames ],
      L203.StubTechProd_nonfood_crop[ Prod_colnames ], L203.StubTechProd_nonfood_meat[ Prod_colnames ],
      L203.StubTechProd_For[ Prod_colnames ], L203.StubTechProd_exp[ Prod_colnames ] )
L203.BaseService <- aggregate( L203.StubTechProd_all$calOutputValue,
      by=as.list( L203.StubTechProd_all[ c( "region", "supplysector", "year" ) ] ), sum )
names( L203.BaseService ) <- names_BaseService

printlog( "L203.IncomeElasticity: Income elasticities" )
printlog( "NOTE: food demands are taken as given in historical validation runs" )
#Note: these steps contain processing normally considered level 1, but are done here because unlike other quantities computed in level 1,
# the income elasticities depend on the timestep length chosen, and as such are dependent on the modeltime, which is not a level 1 attribute
printlog( "Step 1: Building historical estimates of changes in per-capita food demands by region and demand type")
L203.Food_pckcald_R.melt <- subset( L203.BaseService, energy.final.demand %in% c( "FoodDemand_Crops", "FoodDemand_Meat" ) )
L203.Food_pckcald_R.melt$population <- L203.Pop_thous_R_Yh$value[
      match( vecpaste( L203.Food_pckcald_R.melt[ c( "region", "year" ) ] ),
             vecpaste( L203.Pop_thous_R_Yh[ c( "region", "year" ) ] ) ) ]
L203.Food_pckcald_R.melt$pckcald <- L203.Food_pckcald_R.melt$base.service * conv_Pcal_Mcal * conv_days_year / L203.Food_pckcald_R.melt$population

#Cast so that years are columns (with X pasted in front), and compute a table of ratios
L203.Food_pckcald_R.melt$Xyear <- paste0( "X", L203.Food_pckcald_R.melt$year )
L203.Food_pckcald_R_Yh <- dcast( L203.Food_pckcald_R.melt, region + energy.final.demand ~ Xyear, value.var = "pckcald" )
L203.pcFoodRatio_R_Yh <- L203.Food_pckcald_R_Yh
L203.pcFoodRatio_R_Yh[ X_aglu_demand_calyears[ 2:length( X_aglu_demand_calyears ) ] ] <-
      L203.Food_pckcald_R_Yh[ X_aglu_demand_calyears[ 2:length( X_aglu_demand_calyears ) ] ] / 
      L203.Food_pckcald_R_Yh[ X_aglu_demand_calyears[ 1:( length( X_aglu_demand_calyears ) - 1 ) ] ]
L203.pcFoodRatio_R_Yh[ X_aglu_demand_calyears[1] ] <- 1

printlog( "Step 2: Calculating future changes (ratios) in caloric demands by region and demand type" )
#Cast table of future diet change and compute ratios
L203.pcFood_kcald_R_Dmnd_Y.melt$energy.final.demand[ L203.pcFood_kcald_R_Dmnd_Y.melt$GCAM_demand == "crops" ] <- "FoodDemand_Crops"
L203.pcFood_kcald_R_Dmnd_Y.melt$energy.final.demand[ L203.pcFood_kcald_R_Dmnd_Y.melt$GCAM_demand == "meat" ] <- "FoodDemand_Meat"
L203.pcFood_kcald_R_Dmnd_Yfut <- dcast( L203.pcFood_kcald_R_Dmnd_Y.melt, region + energy.final.demand ~ variable )
L203.pcFoodRatio_R_Dmnd_Yfut <- L203.pcFood_kcald_R_Dmnd_Yfut
L203.pcFoodRatio_R_Dmnd_Yfut[ X_aglu_demand_futureyears[ 2:length( X_aglu_demand_futureyears ) ] ] <-
      L203.pcFood_kcald_R_Dmnd_Yfut[ X_aglu_demand_futureyears[ 2:length( X_aglu_demand_futureyears ) ] ] / 
      L203.pcFood_kcald_R_Dmnd_Yfut[ X_aglu_demand_futureyears[ 1:( length( X_aglu_demand_futureyears ) - 1 ) ] ]
L203.pcFoodRatio_R_Dmnd_Yfut[ X_aglu_demand_futureyears[1] ] <- L203.pcFoodRatio_R_Yh[ X_aglu_demand_futureyears[1] ]

L203.pcFood_kcald_R_Dmnd_Y_ssp1.melt$energy.final.demand[ L203.pcFood_kcald_R_Dmnd_Y_ssp1.melt$GCAM_demand == "crops" ] <- "FoodDemand_Crops"
L203.pcFood_kcald_R_Dmnd_Y_ssp1.melt$energy.final.demand[ L203.pcFood_kcald_R_Dmnd_Y_ssp1.melt$GCAM_demand == "meat" ] <- "FoodDemand_Meat"
L203.pcFood_kcald_R_Dmnd_Yfut_ssp1 <- dcast( L203.pcFood_kcald_R_Dmnd_Y_ssp1.melt, region + energy.final.demand ~ variable )
L203.pcFoodRatio_R_Dmnd_Yfut_ssp1 <- L203.pcFood_kcald_R_Dmnd_Yfut_ssp1
L203.pcFoodRatio_R_Dmnd_Yfut_ssp1[ X_aglu_demand_futureyears[ 2:length( X_aglu_demand_futureyears ) ] ] <-
  L203.pcFoodRatio_R_Dmnd_Yfut_ssp1[ X_aglu_demand_futureyears[ 2:length( X_aglu_demand_futureyears ) ] ] / 
  L203.pcFoodRatio_R_Dmnd_Yfut_ssp1[ X_aglu_demand_futureyears[ 1:( length( X_aglu_demand_futureyears ) - 1 ) ] ]
L203.pcFoodRatio_R_Dmnd_Yfut_ssp1[ X_aglu_demand_futureyears[1] ] <- L203.pcFoodRatio_R_Yh[ X_aglu_demand_futureyears[1] ]

L203.pcFood_kcald_R_Dmnd_Y_ssp2.melt$energy.final.demand[ L203.pcFood_kcald_R_Dmnd_Y_ssp2.melt$GCAM_demand == "crops" ] <- "FoodDemand_Crops"
L203.pcFood_kcald_R_Dmnd_Y_ssp2.melt$energy.final.demand[ L203.pcFood_kcald_R_Dmnd_Y_ssp2.melt$GCAM_demand == "meat" ] <- "FoodDemand_Meat"
L203.pcFood_kcald_R_Dmnd_Yfut_ssp2 <- dcast( L203.pcFood_kcald_R_Dmnd_Y_ssp2.melt, region + energy.final.demand ~ variable )
L203.pcFoodRatio_R_Dmnd_Yfut_ssp2 <- L203.pcFood_kcald_R_Dmnd_Yfut_ssp2
L203.pcFoodRatio_R_Dmnd_Yfut_ssp2[ X_aglu_demand_futureyears[ 2:length( X_aglu_demand_futureyears ) ] ] <-
  L203.pcFoodRatio_R_Dmnd_Yfut_ssp2[ X_aglu_demand_futureyears[ 2:length( X_aglu_demand_futureyears ) ] ] / 
  L203.pcFoodRatio_R_Dmnd_Yfut_ssp2[ X_aglu_demand_futureyears[ 1:( length( X_aglu_demand_futureyears ) - 1 ) ] ]
L203.pcFoodRatio_R_Dmnd_Yfut_ssp2[ X_aglu_demand_futureyears[1] ] <- L203.pcFoodRatio_R_Yh[ X_aglu_demand_futureyears[1] ]

L203.pcFood_kcald_R_Dmnd_Y_ssp3.melt$energy.final.demand[ L203.pcFood_kcald_R_Dmnd_Y_ssp3.melt$GCAM_demand == "crops" ] <- "FoodDemand_Crops"
L203.pcFood_kcald_R_Dmnd_Y_ssp3.melt$energy.final.demand[ L203.pcFood_kcald_R_Dmnd_Y_ssp3.melt$GCAM_demand == "meat" ] <- "FoodDemand_Meat"
L203.pcFood_kcald_R_Dmnd_Yfut_ssp3 <- dcast( L203.pcFood_kcald_R_Dmnd_Y_ssp3.melt, region + energy.final.demand ~ variable )
L203.pcFoodRatio_R_Dmnd_Yfut_ssp3 <- L203.pcFood_kcald_R_Dmnd_Yfut_ssp3
L203.pcFoodRatio_R_Dmnd_Yfut_ssp3[ X_aglu_demand_futureyears[ 2:length( X_aglu_demand_futureyears ) ] ] <-
  L203.pcFoodRatio_R_Dmnd_Yfut_ssp3[ X_aglu_demand_futureyears[ 2:length( X_aglu_demand_futureyears ) ] ] / 
  L203.pcFoodRatio_R_Dmnd_Yfut_ssp3[ X_aglu_demand_futureyears[ 1:( length( X_aglu_demand_futureyears ) - 1 ) ] ]
L203.pcFoodRatio_R_Dmnd_Yfut_ssp3[ X_aglu_demand_futureyears[1] ] <- L203.pcFoodRatio_R_Yh[ X_aglu_demand_futureyears[1] ]

L203.pcFood_kcald_R_Dmnd_Y_ssp4.melt$energy.final.demand[ L203.pcFood_kcald_R_Dmnd_Y_ssp4.melt$GCAM_demand == "crops" ] <- "FoodDemand_Crops"
L203.pcFood_kcald_R_Dmnd_Y_ssp4.melt$energy.final.demand[ L203.pcFood_kcald_R_Dmnd_Y_ssp4.melt$GCAM_demand == "meat" ] <- "FoodDemand_Meat"
L203.pcFood_kcald_R_Dmnd_Yfut_ssp4 <- dcast( L203.pcFood_kcald_R_Dmnd_Y_ssp4.melt, region + energy.final.demand ~ variable )
L203.pcFoodRatio_R_Dmnd_Yfut_ssp4 <- L203.pcFood_kcald_R_Dmnd_Yfut_ssp4
L203.pcFoodRatio_R_Dmnd_Yfut_ssp4[ X_aglu_demand_futureyears[ 2:length( X_aglu_demand_futureyears ) ] ] <-
  L203.pcFoodRatio_R_Dmnd_Yfut_ssp4[ X_aglu_demand_futureyears[ 2:length( X_aglu_demand_futureyears ) ] ] / 
  L203.pcFoodRatio_R_Dmnd_Yfut_ssp4[ X_aglu_demand_futureyears[ 1:( length( X_aglu_demand_futureyears ) - 1 ) ] ]
L203.pcFoodRatio_R_Dmnd_Yfut_ssp4[ X_aglu_demand_futureyears[1] ] <- L203.pcFoodRatio_R_Yh[ X_aglu_demand_futureyears[1] ]

L203.pcFood_kcald_R_Dmnd_Y_ssp5.melt$energy.final.demand[ L203.pcFood_kcald_R_Dmnd_Y_ssp5.melt$GCAM_demand == "crops" ] <- "FoodDemand_Crops"
L203.pcFood_kcald_R_Dmnd_Y_ssp5.melt$energy.final.demand[ L203.pcFood_kcald_R_Dmnd_Y_ssp5.melt$GCAM_demand == "meat" ] <- "FoodDemand_Meat"
L203.pcFood_kcald_R_Dmnd_Yfut_ssp5 <- dcast( L203.pcFood_kcald_R_Dmnd_Y_ssp5.melt, region + energy.final.demand ~ variable )
L203.pcFoodRatio_R_Dmnd_Yfut_ssp5 <- L203.pcFood_kcald_R_Dmnd_Yfut_ssp5
L203.pcFoodRatio_R_Dmnd_Yfut_ssp5[ X_aglu_demand_futureyears[ 2:length( X_aglu_demand_futureyears ) ] ] <-
  L203.pcFoodRatio_R_Dmnd_Yfut_ssp5[ X_aglu_demand_futureyears[ 2:length( X_aglu_demand_futureyears ) ] ] / 
  L203.pcFoodRatio_R_Dmnd_Yfut_ssp5[ X_aglu_demand_futureyears[ 1:( length( X_aglu_demand_futureyears ) - 1 ) ] ]
L203.pcFoodRatio_R_Dmnd_Yfut_ssp5[ X_aglu_demand_futureyears[1] ] <- L203.pcFoodRatio_R_Yh[ X_aglu_demand_futureyears[1] ]

printlog( "Step 3: Merging the historical and future caloric demand trajectories" )
L203.pcFoodRatio_R_Dmnd_Y <- L203.pcFoodRatio_R_Yh
L203.pcFoodRatio_R_Dmnd_Y[ X_aglu_demand_futureyears ] <- L203.pcFoodRatio_R_Dmnd_Yfut[ X_aglu_demand_futureyears ]

L203.pcFoodRatio_R_Dmnd_Y_ssp1 <- L203.pcFoodRatio_R_Yh
L203.pcFoodRatio_R_Dmnd_Y_ssp1[ X_aglu_demand_futureyears ] <- L203.pcFoodRatio_R_Dmnd_Yfut_ssp1[ X_aglu_demand_futureyears ]

L203.pcFoodRatio_R_Dmnd_Y_ssp2 <- L203.pcFoodRatio_R_Yh
L203.pcFoodRatio_R_Dmnd_Y_ssp2[ X_aglu_demand_futureyears ] <- L203.pcFoodRatio_R_Dmnd_Yfut_ssp2[ X_aglu_demand_futureyears ]

L203.pcFoodRatio_R_Dmnd_Y_ssp3 <- L203.pcFoodRatio_R_Yh
L203.pcFoodRatio_R_Dmnd_Y_ssp3[ X_aglu_demand_futureyears ] <- L203.pcFoodRatio_R_Dmnd_Yfut_ssp3[ X_aglu_demand_futureyears ]

L203.pcFoodRatio_R_Dmnd_Y_ssp4 <- L203.pcFoodRatio_R_Yh
L203.pcFoodRatio_R_Dmnd_Y_ssp4[ X_aglu_demand_futureyears ] <- L203.pcFoodRatio_R_Dmnd_Yfut_ssp4[ X_aglu_demand_futureyears ]

L203.pcFoodRatio_R_Dmnd_Y_ssp5 <- L203.pcFoodRatio_R_Yh
L203.pcFoodRatio_R_Dmnd_Y_ssp5[ X_aglu_demand_futureyears ] <- L203.pcFoodRatio_R_Dmnd_Yfut_ssp5[ X_aglu_demand_futureyears ]

printlog( "Step 4: Calculating the per-capita GDP trajectories over the same time period" )
printlog( "NOTE: only computing elasticities based on the specified GDP scenario" )
L203.pcgdp_thous90USD_R_Y.melt <- L203.pcgdp_thous90USD_SSP_R_Y.melt[
       L203.pcgdp_thous90USD_SSP_R_Y.melt[[Scen]] == diet_gdpScen & L203.pcgdp_thous90USD_SSP_R_Y.melt[[Y]] %in% model_years, ]
L203.pcgdp_usd_R_Y <- dcast( L203.pcgdp_thous90USD_R_Y.melt, region ~ variable )
L203.pcgdpRatio_R_Y <- L203.pcgdp_usd_R_Y
L203.pcgdpRatio_R_Y[ X_model_years[ 2:length( X_model_years ) ] ] <-
      L203.pcgdp_usd_R_Y[ X_model_years[ 2:length( X_model_years ) ] ] / 
      L203.pcgdp_usd_R_Y[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]
L203.pcgdpRatio_R_Y[ X_model_years[1] ] <- 1

L203.pcgdp_thous90USD_R_Y_SSP1.melt <- L203.pcgdp_thous90USD_SSP_R_Y.melt[
  L203.pcgdp_thous90USD_SSP_R_Y.melt[[Scen]] == "SSP1" & L203.pcgdp_thous90USD_SSP_R_Y.melt[[Y]] %in% model_years, ]
L203.pcgdp_usd_R_Y_SSP1 <- dcast( L203.pcgdp_thous90USD_R_Y_SSP1.melt, region ~ variable )
L203.pcgdpRatio_R_Y_SSP1 <- L203.pcgdp_usd_R_Y_SSP1
L203.pcgdpRatio_R_Y_SSP1[ X_model_years[ 2:length( X_model_years ) ] ] <-
  L203.pcgdpRatio_R_Y_SSP1[ X_model_years[ 2:length( X_model_years ) ] ] / 
  L203.pcgdpRatio_R_Y_SSP1[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]
L203.pcgdpRatio_R_Y_SSP1[ X_model_years[1] ] <- 1

L203.pcgdp_thous90USD_R_Y_SSP2.melt <- L203.pcgdp_thous90USD_SSP_R_Y.melt[
  L203.pcgdp_thous90USD_SSP_R_Y.melt[[Scen]] == "SSP2" & L203.pcgdp_thous90USD_SSP_R_Y.melt[[Y]] %in% model_years, ]
L203.pcgdp_usd_R_Y_SSP2 <- dcast( L203.pcgdp_thous90USD_R_Y_SSP2.melt, region ~ variable )
L203.pcgdpRatio_R_Y_SSP2 <- L203.pcgdp_usd_R_Y_SSP2
L203.pcgdpRatio_R_Y_SSP2[ X_model_years[ 2:length( X_model_years ) ] ] <-
  L203.pcgdpRatio_R_Y_SSP2[ X_model_years[ 2:length( X_model_years ) ] ] / 
  L203.pcgdpRatio_R_Y_SSP2[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]
L203.pcgdpRatio_R_Y_SSP2[ X_model_years[1] ] <- 1

L203.pcgdp_thous90USD_R_Y_SSP3.melt <- L203.pcgdp_thous90USD_SSP_R_Y.melt[
  L203.pcgdp_thous90USD_SSP_R_Y.melt[[Scen]] == "SSP3" & L203.pcgdp_thous90USD_SSP_R_Y.melt[[Y]] %in% model_years, ]
L203.pcgdp_usd_R_Y_SSP3 <- dcast( L203.pcgdp_thous90USD_R_Y_SSP3.melt, region ~ variable )
L203.pcgdpRatio_R_Y_SSP3 <- L203.pcgdp_usd_R_Y_SSP3
L203.pcgdpRatio_R_Y_SSP3[ X_model_years[ 2:length( X_model_years ) ] ] <-
  L203.pcgdpRatio_R_Y_SSP3[ X_model_years[ 2:length( X_model_years ) ] ] / 
  L203.pcgdpRatio_R_Y_SSP3[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]
L203.pcgdpRatio_R_Y_SSP3[ X_model_years[1] ] <- 1

L203.pcgdp_thous90USD_R_Y_SSP4.melt <- L203.pcgdp_thous90USD_SSP_R_Y.melt[
  L203.pcgdp_thous90USD_SSP_R_Y.melt[[Scen]] == "SSP4" & L203.pcgdp_thous90USD_SSP_R_Y.melt[[Y]] %in% model_years, ]
L203.pcgdp_usd_R_Y_SSP4 <- dcast( L203.pcgdp_thous90USD_R_Y_SSP4.melt, region ~ variable )
L203.pcgdpRatio_R_Y_SSP4 <- L203.pcgdp_usd_R_Y_SSP4
L203.pcgdpRatio_R_Y_SSP4[ X_model_years[ 2:length( X_model_years ) ] ] <-
  L203.pcgdpRatio_R_Y_SSP4[ X_model_years[ 2:length( X_model_years ) ] ] / 
  L203.pcgdpRatio_R_Y_SSP4[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]
L203.pcgdpRatio_R_Y_SSP4[ X_model_years[1] ] <- 1

L203.pcgdp_thous90USD_R_Y_SSP5.melt <- L203.pcgdp_thous90USD_SSP_R_Y.melt[
  L203.pcgdp_thous90USD_SSP_R_Y.melt[[Scen]] == "SSP5" & L203.pcgdp_thous90USD_SSP_R_Y.melt[[Y]] %in% model_years, ]
L203.pcgdp_usd_R_Y_SSP5 <- dcast( L203.pcgdp_thous90USD_R_Y_SSP5.melt, region ~ variable )
L203.pcgdpRatio_R_Y_SSP5 <- L203.pcgdp_usd_R_Y_SSP5
L203.pcgdpRatio_R_Y_SSP5[ X_model_years[ 2:length( X_model_years ) ] ] <-
  L203.pcgdpRatio_R_Y_SSP5[ X_model_years[ 2:length( X_model_years ) ] ] / 
  L203.pcgdpRatio_R_Y_SSP5[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]
L203.pcgdpRatio_R_Y_SSP5[ X_model_years[1] ] <- 1

printlog( "Step 5: Solving for the income elasticities in each time period" )
L203.IncElas_R_Dmnd_Y <- L203.pcFoodRatio_R_Dmnd_Y
L203.IncElas_R_Dmnd_Y[ X_model_years ] <-
      round( log( L203.pcFoodRatio_R_Dmnd_Y [ X_model_years ] ) / 
             log( L203.pcgdpRatio_R_Y[ match( L203.pcFoodRatio_R_Dmnd_Y$region, L203.pcgdpRatio_R_Y$region ), X_model_years ] ),
             digits_IncElas )
L203.IncElas_R_Dmnd_Y[ is.na( L203.IncElas_R_Dmnd_Y ) ] <- 0

L203.IncElas_R_Dmnd_Y_SSP1 <- L203.pcFoodRatio_R_Dmnd_Y_ssp1
L203.IncElas_R_Dmnd_Y_SSP1[ X_model_years ] <-
  round( log( L203.pcFoodRatio_R_Dmnd_Y_ssp1[ X_model_years ] ) / 
           log( L203.pcgdpRatio_R_Y_SSP1[ match( L203.pcFoodRatio_R_Dmnd_Y_ssp1$region, L203.pcgdpRatio_R_Y_SSP1$region ), X_model_years ] ),
         digits_IncElas )
L203.IncElas_R_Dmnd_Y_SSP1[ is.na( L203.IncElas_R_Dmnd_Y_SSP1 ) ] <- 0

L203.IncElas_R_Dmnd_Y_SSP2 <- L203.pcFoodRatio_R_Dmnd_Y_ssp2
L203.IncElas_R_Dmnd_Y_SSP2[ X_model_years ] <-
  round( log( L203.pcFoodRatio_R_Dmnd_Y_ssp2[ X_model_years ] ) / 
           log( L203.pcgdpRatio_R_Y_SSP2[ match( L203.pcFoodRatio_R_Dmnd_Y_ssp2$region, L203.pcgdpRatio_R_Y_SSP2$region ), X_model_years ] ),
         digits_IncElas )
L203.IncElas_R_Dmnd_Y_SSP2[ is.na( L203.IncElas_R_Dmnd_Y_SSP2 ) ] <- 0

L203.IncElas_R_Dmnd_Y_SSP3 <- L203.pcFoodRatio_R_Dmnd_Y_ssp3
L203.IncElas_R_Dmnd_Y_SSP3[ X_model_years ] <-
  round( log( L203.pcFoodRatio_R_Dmnd_Y_ssp3 [ X_model_years ] ) / 
           log( L203.pcgdpRatio_R_Y_SSP3[ match( L203.pcFoodRatio_R_Dmnd_Y_ssp3$region, L203.pcgdpRatio_R_Y_SSP3$region ), X_model_years ] ),
         digits_IncElas )
L203.IncElas_R_Dmnd_Y_SSP3[ is.na( L203.IncElas_R_Dmnd_Y_SSP3 ) ] <- 0

L203.IncElas_R_Dmnd_Y_SSP4 <- L203.pcFoodRatio_R_Dmnd_Y_ssp4
L203.IncElas_R_Dmnd_Y_SSP4[ X_model_years ] <-
  round( log( L203.pcFoodRatio_R_Dmnd_Y_ssp4[ X_model_years ] ) / 
           log( L203.pcgdpRatio_R_Y_SSP4[ match( L203.pcFoodRatio_R_Dmnd_Y_ssp4$region, L203.pcgdpRatio_R_Y_SSP4$region ), X_model_years ] ),
         digits_IncElas )
L203.IncElas_R_Dmnd_Y_SSP4[ is.na( L203.IncElas_R_Dmnd_Y_SSP4 ) ] <- 0

L203.IncElas_R_Dmnd_Y_SSP5 <- L203.pcFoodRatio_R_Dmnd_Y_ssp5
L203.IncElas_R_Dmnd_Y_SSP5[ X_model_years ] <-
  round( log( L203.pcFoodRatio_R_Dmnd_Y_ssp5[ X_model_years ] ) / 
           log( L203.pcgdpRatio_R_Y_SSP5[ match( L203.pcFoodRatio_R_Dmnd_Y_ssp5$region, L203.pcgdpRatio_R_Y_SSP5$region ), X_model_years ] ),
         digits_IncElas )
L203.IncElas_R_Dmnd_Y_SSP5[ is.na( L203.IncElas_R_Dmnd_Y_SSP5 ) ] <- 0

printlog( "Step 6: Converting to appropriate formats")
L203.IncomeElasticity <- interpolate_and_melt( L203.IncElas_R_Dmnd_Y, model_future_years, value.name = "income.elasticity" )
L203.IncomeElasticity <- L203.IncomeElasticity[ names_IncomeElasticity ]

L203.IncomeElasticity_SSP1 <- interpolate_and_melt( L203.IncElas_R_Dmnd_Y_SSP1, model_future_years, value.name = "income.elasticity" )
L203.IncomeElasticity_SSP1 <- L203.IncomeElasticity_SSP1[ names_IncomeElasticity ]
L203.IncomeElasticity_SSP1$income.elasticity[ L203.IncomeElasticity_SSP1$income.elasticity > 1 ] <- 1
L203.IncomeElasticity_SSP1$income.elasticity[ L203.IncomeElasticity_SSP1$income.elasticity < 0 ] <- 0

L203.IncomeElasticity_SSP2 <- interpolate_and_melt( L203.IncElas_R_Dmnd_Y_SSP2, model_future_years, value.name = "income.elasticity" )
L203.IncomeElasticity_SSP2 <- L203.IncomeElasticity_SSP2[ names_IncomeElasticity ]
L203.IncomeElasticity_SSP2$income.elasticity[ L203.IncomeElasticity_SSP2$income.elasticity > 1 ] <- 1
L203.IncomeElasticity_SSP2$income.elasticity[ L203.IncomeElasticity_SSP2$income.elasticity < 0 ] <- 0

L203.IncomeElasticity_SSP3 <- interpolate_and_melt( L203.IncElas_R_Dmnd_Y_SSP3, model_future_years, value.name = "income.elasticity" )
L203.IncomeElasticity_SSP3 <- L203.IncomeElasticity_SSP3[ names_IncomeElasticity ]
L203.IncomeElasticity_SSP3$income.elasticity[ L203.IncomeElasticity_SSP3$income.elasticity > 1 ] <- 1
L203.IncomeElasticity_SSP3$income.elasticity[ L203.IncomeElasticity_SSP3$income.elasticity < 0 ] <- 0

L203.IncomeElasticity_SSP4 <- interpolate_and_melt( L203.IncElas_R_Dmnd_Y_SSP4, model_future_years, value.name = "income.elasticity" )
L203.IncomeElasticity_SSP4 <- L203.IncomeElasticity_SSP4[ names_IncomeElasticity ]
L203.IncomeElasticity_SSP4$income.elasticity[ L203.IncomeElasticity_SSP4$income.elasticity > 1 ] <- 1
L203.IncomeElasticity_SSP4$income.elasticity[ L203.IncomeElasticity_SSP4$income.elasticity < 0 ] <- 0

L203.IncomeElasticity_SSP5 <- interpolate_and_melt( L203.IncElas_R_Dmnd_Y_SSP5, model_future_years, value.name = "income.elasticity" )
L203.IncomeElasticity_SSP5 <- L203.IncomeElasticity_SSP5[ names_IncomeElasticity ]
L203.IncomeElasticity_SSP5$income.elasticity[ L203.IncomeElasticity_SSP5$income.elasticity > 1 ] <- 1
L203.IncomeElasticity_SSP5$income.elasticity[ L203.IncomeElasticity_SSP5$income.elasticity < 0 ] <- 0

printlog( "L203.PriceElasticity: Price elasticities" )
L203.PriceElasticity <- write_to_all_regions_ag( A_demand_supplysector, c( names_EnergyFinalDemand, "price.elasticity" ) )
#Price elasticities are only read for future periods
L203.PriceElasticity <- repeat_and_add_vector( L203.PriceElasticity, Y, model_future_years )
#Set the USA price elasticity to a region-specific value
L203.PriceElasticity$price.elasticity[ L203.PriceElasticity$region == "USA" &
      L203.PriceElasticity$energy.final.demand == "FoodDemand_Meat" ] <- food_meat_P_elas_USA
L203.PriceElasticity <- L203.PriceElasticity[ names_PriceElasticity ]

#fuel preference elasticity
printlog( "L203.FuelPrefElast_ssp1: Fuel preference elasticities for meat in SSP1" )
A_fuelprefElasticity_ssp1$year.fillout <- min( model_base_years )
L203.FuelPrefElast_ssp1 <- write_to_all_regions_ag( A_fuelprefElasticity_ssp1, names_FuelPrefElasticity )

#Remove any regions for which agriculture and land use are not modeled
## ALSO SUBSET THE CALIBRATION TABLES TO ONLY THE MODEL BASE YEARS
for( curr_table in names( L203.SectorLogitTables ) ) {
    if( curr_table != "EQUIV_TABLE" ) {
        L203.SectorLogitTables[[ curr_table ]]$data <- subset( L203.SectorLogitTables[[ curr_table ]]$data,
            !region %in% no_aglu_regions )
    }
}
L203.Supplysector_demand         <- subset( L203.Supplysector_demand, !region %in% no_aglu_regions )
for( curr_table in names( L203.SubsectorLogitTables ) ) {
    if( curr_table != "EQUIV_TABLE" ) {
        L203.SubsectorLogitTables[[ curr_table ]]$data <- subset( L203.SubsectorLogitTables[[ curr_table ]]$data,
            !region %in% no_aglu_regions )
    }
}
L203.SubsectorAll_demand         <- subset( L203.SubsectorAll_demand, !region %in% no_aglu_regions )
L203.StubTech_demand             <- subset( L203.StubTech_demand, !region %in% no_aglu_regions )
L203.StubTechProd_food_crop      <- subset( L203.StubTechProd_food_crop, !region %in% no_aglu_regions & year %in% model_base_years )
L203.StubTechProd_food_meat      <- subset( L203.StubTechProd_food_meat, !region %in% no_aglu_regions & year %in% model_base_years )
L203.StubTechProd_nonfood_crop   <- subset( L203.StubTechProd_nonfood_crop, !region %in% no_aglu_regions & year %in% model_base_years )
L203.StubTechProd_nonfood_meat   <- subset( L203.StubTechProd_nonfood_meat, !region %in% no_aglu_regions & year %in% model_base_years )
L203.StubTechProd_For            <- subset( L203.StubTechProd_For, !region %in% no_aglu_regions & year %in% model_base_years )
L203.StubTechFixOut_exp          <- subset( L203.StubTechFixOut_exp, !region %in% no_aglu_regions )
L203.StubCalorieContent_crop     <- subset( L203.StubCalorieContent_crop, !region %in% no_aglu_regions )
L203.StubCalorieContent_meat     <- subset( L203.StubCalorieContent_meat, !region %in% no_aglu_regions )
L203.PerCapitaBased              <- subset( L203.PerCapitaBased, !region %in% no_aglu_regions )
L203.BaseService                 <- subset( L203.BaseService, !region %in% no_aglu_regions & year %in% model_base_years )
L203.IncomeElasticity            <- subset( L203.IncomeElasticity, !region %in% no_aglu_regions )
L203.IncomeElasticity_SSP1       <- subset( L203.IncomeElasticity_SSP1, !region %in% no_aglu_regions )
L203.IncomeElasticity_SSP2       <- subset( L203.IncomeElasticity_SSP2, !region %in% no_aglu_regions )
L203.IncomeElasticity_SSP3       <- subset( L203.IncomeElasticity_SSP3, !region %in% no_aglu_regions )
L203.IncomeElasticity_SSP4       <- subset( L203.IncomeElasticity_SSP4, !region %in% no_aglu_regions )
L203.IncomeElasticity_SSP5       <- subset( L203.IncomeElasticity_SSP5, !region %in% no_aglu_regions )
L203.PriceElasticity             <- subset( L203.PriceElasticity, !region %in% no_aglu_regions )
L203.FuelPrefElast_ssp1          <- subset( L203.FuelPrefElast_ssp1, !region %in% no_aglu_regions )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

for( curr_table in names ( L203.SectorLogitTables) ) {
write_mi_data( L203.SectorLogitTables[[ curr_table ]]$data, L203.SectorLogitTables[[ curr_table ]]$header,
    "AGLU_LEVEL2_DATA", paste0("L203.", L203.SectorLogitTables[[ curr_table ]]$header ), "AGLU_XML_BATCH",
    "batch_demand_input.xml" )
}
write_mi_data( L203.Supplysector_demand, IDstring="Supplysector", domain="AGLU_LEVEL2_DATA", fn="L203.Supplysector_demand",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_demand_input.xml" )
for( curr_table in names ( L203.SubsectorLogitTables ) ) {
write_mi_data( L203.SubsectorLogitTables[[ curr_table ]]$data, L203.SubsectorLogitTables[[ curr_table ]]$header,
    "AGLU_LEVEL2_DATA", paste0("L203.", L203.SubsectorLogitTables[[ curr_table ]]$header ), "AGLU_XML_BATCH",
    "batch_demand_input.xml" )
}
write_mi_data( L203.SubsectorAll_demand, "SubsectorAll", "AGLU_LEVEL2_DATA", "L203.SubsectorAll_demand", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.StubTech_demand, "StubTech", "AGLU_LEVEL2_DATA", "L203.StubTech_demand", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.GlobalTechCoef_demand, "GlobalTechCoef", "AGLU_LEVEL2_DATA", "L203.GlobalTechCoef_demand", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.GlobalTechShrwt_demand, "GlobalTechShrwt", "AGLU_LEVEL2_DATA", "L203.GlobalTechShrwt_demand", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.StubTechProd_food_crop, "StubTechProd", "AGLU_LEVEL2_DATA", "L203.StubTechProd_food_crop", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.StubTechProd_food_meat, "StubTechProd", "AGLU_LEVEL2_DATA", "L203.StubTechProd_food_meat", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.StubTechProd_nonfood_crop, "StubTechProd", "AGLU_LEVEL2_DATA", "L203.StubTechProd_nonfood_crop", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.StubTechProd_nonfood_meat, "StubTechProd", "AGLU_LEVEL2_DATA", "L203.StubTechProd_nonfood_meat", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.StubTechProd_For, "StubTechProd", "AGLU_LEVEL2_DATA", "L203.StubTechProd_For", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.StubTechFixOut_exp, "StubTechFixOut", "AGLU_LEVEL2_DATA", "L203.StubTechFixOut_exp", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.StubCalorieContent_crop, "StubCalorieContent", "AGLU_LEVEL2_DATA", "L203.StubCalorieContent_crop", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.StubCalorieContent_meat, "StubCalorieContent", "AGLU_LEVEL2_DATA", "L203.StubCalorieContent_meat", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.PerCapitaBased, "PerCapitaBased", "AGLU_LEVEL2_DATA", "L203.PerCapitaBased", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.BaseService, "BaseService", "AGLU_LEVEL2_DATA", "L203.BaseService", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.IncomeElasticity, "IncomeElasticity", "AGLU_LEVEL2_DATA", "L203.IncomeElasticity", "AGLU_XML_BATCH", "batch_demand_input.xml" )
write_mi_data( L203.PriceElasticity, "PriceElasticity", "AGLU_LEVEL2_DATA", "L203.PriceElasticity", "AGLU_XML_BATCH", "batch_demand_input.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_demand_input.xml", "AGLU_XML_FINAL", "demand_input.xml", "", xml_tag="outFile" )

write_mi_data( L203.FuelPrefElast_ssp1, "FuelPrefElast", "AGLU_LEVEL2_DATA", "L203.FuelPrefElast_ssp1", "AGLU_XML_BATCH", "batch_food_SSP1.xml" )
write_mi_data( L203.IncomeElasticity_SSP1, "IncomeElasticity", "AGLU_LEVEL2_DATA", "L203.IncomeElasticity_SSP1", "AGLU_XML_BATCH", "batch_food_SSP1.xml" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_food_SSP1.xml", "AGLU_XML_FINAL", "food_SSP1.xml", "", xml_tag="outFile" )

write_mi_data( L203.IncomeElasticity_SSP2, "IncomeElasticity", "AGLU_LEVEL2_DATA", "L203.IncomeElasticity_SSP2", "AGLU_XML_BATCH", "batch_food_SSP2.xml" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_food_SSP2.xml", "AGLU_XML_FINAL", "food_SSP2.xml", "", xml_tag="outFile" )

write_mi_data( L203.IncomeElasticity_SSP3, "IncomeElasticity", "AGLU_LEVEL2_DATA", "L203.IncomeElasticity_SSP3", "AGLU_XML_BATCH", "batch_food_SSP3.xml" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_food_SSP3.xml", "AGLU_XML_FINAL", "food_SSP3.xml", "", xml_tag="outFile" )

write_mi_data( L203.IncomeElasticity_SSP4, "IncomeElasticity", "AGLU_LEVEL2_DATA", "L203.IncomeElasticity_SSP4", "AGLU_XML_BATCH", "batch_food_SSP4.xml" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_food_SSP4.xml", "AGLU_XML_FINAL", "food_SSP4.xml", "", xml_tag="outFile" )

write_mi_data( L203.IncomeElasticity_SSP5, "IncomeElasticity", "AGLU_LEVEL2_DATA", "L203.IncomeElasticity_SSP5", "AGLU_XML_BATCH", "batch_food_SSP5.xml" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_food_SSP5.xml", "AGLU_XML_FINAL", "food_SSP5.xml", "", xml_tag="outFile" )

logstop()
