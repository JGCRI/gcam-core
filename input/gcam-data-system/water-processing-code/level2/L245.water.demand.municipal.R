# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "WATERPROC_DIR" ) ){
    if( Sys.getenv( "WATERPROC" ) != "" ){
        WATERPROC_DIR <- Sys.getenv( "WATERPROC" )
    } else {
        stop("Could not determine location of water data system. Please set the R var WATERPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(WATERPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(WATERPROC_DIR,"/../_common/headers/WATER_header.R",sep=""))
logstart( "L245.water.demand.municipal.R" )
printlog( "Genereate municipal water sector input file" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A03.sector <- readdata( "WATER_ASSUMPTIONS", "A03.sector" )
A45.sector <- readdata( "WATER_ASSUMPTIONS", "A45.sector" )
A45.tech_cost <- readdata( "WATER_ASSUMPTIONS", "A45.tech_cost" )
A45.demand <- readdata( "WATER_ASSUMPTIONS", "A45.demand" )
L145.municipal_water_R_W_Yh_km3 <- readdata( "WATER_LEVEL1_DATA", "L145.municipal_water_R_W_Yh_km3" )
L145.municipal_water_cost_R_75USD_m3 <- readdata( "WATER_LEVEL1_DATA", "L145.municipal_water_cost_R_75USD_m3" )
L145.municipal_water_eff_R_Y <- readdata( "WATER_LEVEL1_DATA", "L145.municipal_water_eff_R_Y" )

# -------------------------------------------------------------------------------

#2. Build tables

# Create a complete assumptions table to simplify processing below since there is only one
# sector/subsector/tech anyway
L245.assumptions_all <- merge( A45.sector, A45.tech_cost )
L245.assumptions_all <- cbind( L245.assumptions_all, A45.demand )
L245.assumptions_all$logit.year.fillout <- model_years[1]
L245.assumptions_all <- merge( L245.assumptions_all, GCAM_region_names )

printlog( "L245.Supplysector: Sector information" )
L245.Supplysector <- L245.assumptions_all[, names_Supplysector ]
L245.SectorLogitTables <- get_logit_fn_tables(
    L245.assumptions_all[, names_SupplysectorLogitType ],
    names_SupplysectorLogitType, base.header="Supplysector_",
    include.equiv.table=T, write.all.regions=F )

printlog( "L453.SubsectorLogit: Subsector logit exponents" )
L245.SubsectorLogit <- L245.assumptions_all[, c( names_SubsectorLogit, "logit.type" ) ]
L245.SubsectorLogitTables <- get_logit_fn_tables( L245.SubsectorLogit, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )
L245.SubsectorLogit <- L245.SubsectorLogit[, names_SubsectorLogit ]

printlog( "L245.SubsectorShrwtFllt: subsector share weights (just 1 due to no competition)" )
L245.SubsectorShrwtFllt <- L245.assumptions_all[, names_Subsector ]
L245.SubsectorShrwtFllt$year.fillout <- model_years[1]
L245.SubsectorShrwtFllt$share.weight <- 1
L245.SubsectorShrwtFllt <- L245.SubsectorShrwtFllt[, names_SubsectorShrwtFllt ]

printlog( "L245.TechShrwt: technology share weights (just 1 due to no competition)" )
L245.TechShrwt <- L245.assumptions_all[, names_Tech ]
L245.TechShrwt$share.weight <- 1
L245.TechShrwt <- L245.TechShrwt[ rep( 1:nrow( L245.TechShrwt ), times=length( model_years ) ), ]
L245.TechShrwt[[Y]] <- model_years[ sort( rep( 1:length( model_years ), times=nrow( L245.assumptions_all ) ) ) ]
L245.TechShrwt <- L245.TechShrwt[, names_TechShrwt ]

printlog( "L245.TechCoef: pass-through demands for withdrawals, some fraction is consumed" )
L245.TechCoef <- merge( L245.assumptions_all[, c( R, names_Tech ) ], L145.municipal_water_eff_R_Y )
L245.TechCoef$minicam.energy.input <- water_C
L245.TechCoef$market.name <- L245.TechCoef[[reg]]
L245.TechCoef.W <- L245.TechCoef
L245.TechCoef.W$minicam.energy.input <- water_W
L245.TechCoef.W$coefficient <- 1
L245.TechCoef <- rbind( L245.TechCoef, L245.TechCoef.W )
L245.TechCoef[[water_sector]] <- "Municipal"
L245.TechCoef$minicam.energy.input <- get_water_inputs_for_mapping( L245.TechCoef, A03.sector, water_type.col="minicam.energy.input" )
L245.TechCoef <- L245.TechCoef[, names_TechCoef ]

printlog( "L245.TechCost: Municipal water non-energy cost" )
L245.TechCost <- merge( L245.assumptions_all[, c( R, names_Tech, "minicam.non.energy.input" ) ], L145.municipal_water_cost_R_75USD_m3 )
# Set the coef for all years
L245.orig_num_rows <- nrow( L245.TechCost )
L245.TechCost <- L245.TechCost[ rep( 1:nrow( L245.TechCost ), times=length( model_years ) ), ]
L245.TechCost[[Y]] <- model_years[ sort( rep( 1:length( model_years ), times=L245.orig_num_rows ) ) ]
L245.TechCost <- L245.TechCost[, names_TechCost ]

printlog( "L245.PerCapitaBased: Set the final demand is per-capita based" )
L245.PerCapitaBased <- L245.assumptions_all[, names_PerCapitaBased ]

printlog( "L245.BaseService: Set the final demand base service" )
L245.BaseService <- merge( L245.assumptions_all[, c( R, names_EnergyFinalDemand ) ],
    interpolate_and_melt( L145.municipal_water_R_W_Yh_km3, model_base_years, value.name="base.service" ) )
L245.BaseService <- L245.BaseService[, names_BaseService ]

printlog( "L245.IncomeElasticity: Set the final demand income elasticity" )
L245.IncomeElasticity <- L245.assumptions_all[, c( names_EnergyFinalDemand, "income.elasticity" )]
L245.IncomeElasticity <- L245.IncomeElasticity[ rep( 1:nrow( L245.IncomeElasticity ), times=length( future_years ) ), ]
L245.IncomeElasticity[[Y]] <- future_years[ sort( rep( 1:length( future_years ), times=nrow( L245.assumptions_all ) ) ) ]
L245.IncomeElasticity <- L245.IncomeElasticity[, names_IncomeElasticity ]

printlog( "L245.PriceElasticity: Set the final demand price elasticity" )
L245.PriceElasticity <- L245.assumptions_all[, c( names_EnergyFinalDemand, "price.elasticity" )]
L245.PriceElasticity <- L245.PriceElasticity[ rep( 1:nrow( L245.PriceElasticity ), times=length( future_years ) ), ]
L245.PriceElasticity[[Y]] <- future_years[ sort( rep( 1:length( future_years ), times=nrow( L245.assumptions_all ) ) ) ]
L245.PriceElasticity <- L245.PriceElasticity[, names_PriceElasticity ]

printlog( "L245.aeei: Set the final demand efficiency improvement rate" )
L245.aeei <- L245.assumptions_all[, c( names_EnergyFinalDemand, "aeei" )]
L245.aeei <- L245.aeei[ rep( 1:nrow( L245.aeei ), times=length( future_years ) ), ]
L245.aeei[[Y]] <- future_years[ sort( rep( 1:length( future_years ), times=nrow( L245.assumptions_all ) ) ) ]
L245.aeei <- L245.aeei[, names_aeei ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table in names ( L245.SectorLogitTables ) ) {
write_mi_data( L245.SectorLogitTables[[ curr_table ]]$data, L245.SectorLogitTables[[ curr_table ]]$header,
    "WATER_LEVEL2_DATA", paste0("L245.", L245.SectorLogitTables[[ curr_table ]]$header ), "WATER_XML_BATCH",
    "batch_water_demand_municipal.xml" )
}
write_mi_data( L245.Supplysector, "Supplysector", "WATER_LEVEL2_DATA", "L245.Supplysector", "WATER_XML_BATCH", "batch_water_demand_municipal.xml" )
for( curr_table in names ( L245.SubsectorLogitTables ) ) {
write_mi_data( L245.SubsectorLogitTables[[ curr_table ]]$data, L245.SubsectorLogitTables[[ curr_table ]]$header,
    "WATER_LEVEL2_DATA", paste0("L245.", L245.SubsectorLogitTables[[ curr_table ]]$header ), "WATER_XML_BATCH",
    "batch_water_demand_municipal.xml" )
}
write_mi_data( L245.SubsectorLogit, "SubsectorLogit", "WATER_LEVEL2_DATA", "L245.SubsectorLogit", "WATER_XML_BATCH", "batch_water_demand_municipal.xml" )
write_mi_data( L245.SubsectorShrwtFllt, "SubsectorShrwtFllt", "WATER_LEVEL2_DATA", "L245.SubsectorShrwtFllt", "WATER_XML_BATCH", "batch_water_demand_municipal.xml" )
write_mi_data( L245.TechShrwt, "TechShrwt", "WATER_LEVEL2_DATA", "L245.TechShrwt", "WATER_XML_BATCH", "batch_water_demand_municipal.xml" )
write_mi_data( L245.TechCoef, "TechCoef", "WATER_LEVEL2_DATA", "L245.TechCoef", "WATER_XML_BATCH", "batch_water_demand_municipal.xml" )
write_mi_data( L245.TechCost, "TechCost", "WATER_LEVEL2_DATA", "L245.TechCost", "WATER_XML_BATCH", "batch_water_demand_municipal.xml" )
write_mi_data( L245.PerCapitaBased, "PerCapitaBased", "WATER_LEVEL2_DATA", "L245.PerCapitaBased", "WATER_XML_BATCH", "batch_water_demand_municipal.xml" )
write_mi_data( L245.BaseService, "BaseService", "WATER_LEVEL2_DATA", "L245.BaseService", "WATER_XML_BATCH", "batch_water_demand_municipal.xml" )
write_mi_data( L245.IncomeElasticity, "IncomeElasticity", "WATER_LEVEL2_DATA", "L245.IncomeElasticity", "WATER_XML_BATCH", "batch_water_demand_municipal.xml" )
write_mi_data( L245.PriceElasticity, "PriceElasticity", "WATER_LEVEL2_DATA", "L245.PriceElasticity", "WATER_XML_BATCH", "batch_water_demand_municipal.xml" )
write_mi_data( L245.aeei, "aeei", "WATER_LEVEL2_DATA", "L245.aeei", "WATER_XML_BATCH", "batch_water_demand_municipal.xml" )
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_demand_municipal.xml", "WATER_XML_FINAL", "water_demand_municipal.xml", "", "outFile" )

logstop()
