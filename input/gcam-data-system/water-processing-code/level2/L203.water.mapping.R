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
logstart( "L203.water.mapping.R" )
printlog( "Genereate water mapping sector input files to group demands by sectors and potentially apply losses" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L165.ag_IrrEff_R <- readdata("AGLU_LEVEL1_DATA", "L165.ag_IrrEff_R")
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A03.sector <- readdata( "WATER_ASSUMPTIONS", "A03.sector" )

# -------------------------------------------------------------------------------

#2. Build tables
# Make a table with all of the possible mapping sectors with shares
L203.mapping_nonirr <- A03.sector[ A03.sector[[water_sector]] %in% nonirr_water_sectors, ]
L203.mapping_nonirr[[AEZ]] <- NA
L203.mapping_irr <- A03.sector[ A03.sector[[water_sector]] %in% irr_water_sector, ]
L203.mapping_irr.orig_size <- nrow( L203.mapping_irr )
L203.mapping_irr <- L203.mapping_irr[ rep( 1:L203.mapping_irr.orig_size, times=length( AEZs ) ), ]
# use just AEZ numbers to avoid pasting AEZAEZ in get_water_inputs_for_mapping
L203.mapping_irr[[AEZ]] <- sort( rep( 1:length( AEZs ), times=L203.mapping_irr.orig_size ) )
L203.mapping_all <- rbind( L203.mapping_nonirr, L203.mapping_irr )
L203.mapping_all$coefficient <- 1
L203.mapping_all[[water_type]] <- water_C
L203.mapping_all.W <- L203.mapping_all
L203.mapping_all.W[[water_type]] <- water_W
L203.mapping_all <- rbind( L203.mapping_all, L203.mapping_all.W )
L203.mapping_all[[supp]] <- get_water_inputs_for_mapping( L203.mapping_all, A03.sector )
L203.mapping_all <- merge( L203.mapping_all, GCAM_region_names )
L203.mapping_all$logit.year.fillout <- model_years[1]
L203.mapping_all[[subs]] <- L203.mapping_all[[supp]]
L203.mapping_all[[tech]] <- L203.mapping_all[[supp]]
# remove non-existant region + AEZ combos
L203.R_AEZ_nonexist <- paste( L125.R_AEZ_nonexist[[R]], gsub( "AEZ0?", "", L125.R_AEZ_nonexist[[AEZ]] ) )
L203.mapping_all <- L203.mapping_all[
    paste( L203.mapping_all[[R]], L203.mapping_all[[AEZ]] ) %!in%
    L203.R_AEZ_nonexist, ]
L203.mapping_all <- L203.mapping_all[ order( L203.mapping_all[[R]] ), ]
# map in converyance losses for irrigation water withdrawals
# The loss is given as an efficiency, we will read it into the model as coefficient
L203.mapping_all[ L203.mapping_all[[water_sector]] == irr_water_sector & L203.mapping_all[[water_type]] == water_W, "coefficient" ] <- 1.0 /

    L165.ag_IrrEff_R[ match(
        L203.mapping_all[ L203.mapping_all[[water_sector]] == irr_water_sector & L203.mapping_all[[water_type]] == water_W, R ],
        L165.ag_IrrEff_R[[R]] ), "conveyance.eff" ]

printlog( "L203.Supplysector: Sector information" )
L203.Supplysector <- L203.mapping_all[, names_Supplysector ]
L203.SectorLogitTables <- get_logit_fn_tables(
    L203.mapping_all[, names_SupplysectorLogitType ],
    names_SupplysectorLogitType, base.header="Supplysector_",
    include.equiv.table=T, write.all.regions=F )

printlog( "L203.SubsectorLogit: Subsector logit exponents of mapping sector" )
L203.SubsectorLogit <- L203.mapping_all[, c( names_SubsectorLogit, "logit.type" ) ]
L203.SubsectorLogitTables <- get_logit_fn_tables( L203.SubsectorLogit, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )
L203.SubsectorLogit <- L203.SubsectorLogit[, names_SubsectorLogit ]

printlog( "L203.SubsectorShrwtFllt: subsector share weights to 1 (no competition)" )
L203.SubsectorShrwtFllt <- L203.mapping_all[, names_Subsector ]
L203.SubsectorShrwtFllt$share.weight <- 1
L203.SubsectorShrwtFllt$year.fillout <- model_years[1]
L203.SubsectorShrwtFllt <- L203.SubsectorShrwtFllt[, names_SubsectorShrwtFllt ]

printlog( "L203.TechShrwt: Pass-through technology to the water resource, no competition" )
L203.TechShrwt <- L203.mapping_all[, names_Tech ]
L203.TechShrwt$year <- model_years[1]
L203.TechShrwt$share.weight <- 1

printlog( "L203.TechCoef: Pass-through technology to the water resource" )
L203.TechCoef <- L203.mapping_all[, c( names_Tech, water_type, "coefficient" ) ]
L203.TechCoef$minicam.energy.input <- L203.TechCoef[[water_type]]
L203.TechCoef$year <- model_years[1]
L203.TechCoef$market.name <- L203.TechCoef[[reg]]
L203.TechCoef <- L203.TechCoef[, names_TechCoef ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table in names ( L203.SectorLogitTables ) ) {
write_mi_data( L203.SectorLogitTables[[ curr_table ]]$data, L203.SectorLogitTables[[ curr_table ]]$header,
    "WATER_LEVEL2_DATA", paste0("L203.", L203.SectorLogitTables[[ curr_table ]]$header ), "WATER_XML_BATCH",
    "batch_water_mapping.xml" )
}
write_mi_data( L203.Supplysector, "Supplysector", "WATER_LEVEL2_DATA", "L203.Supplysector", "WATER_XML_BATCH", "batch_water_mapping.xml" )
for( curr_table in names ( L203.SubsectorLogitTables ) ) {
write_mi_data( L203.SubsectorLogitTables[[ curr_table ]]$data, L203.SubsectorLogitTables[[ curr_table ]]$header,
    "WATER_LEVEL2_DATA", paste0("L203.", L203.SubsectorLogitTables[[ curr_table ]]$header ), "WATER_XML_BATCH",
    "batch_water_mapping.xml" )
}
write_mi_data( L203.SubsectorLogit, "SubsectorLogit", "WATER_LEVEL2_DATA", "L203.SubsectorLogit", "WATER_XML_BATCH", "batch_water_mapping.xml" )
write_mi_data( L203.SubsectorShrwtFllt, "SubsectorShrwtFllt", "WATER_LEVEL2_DATA", "L203.SubsectorShrwtFllt", "WATER_XML_BATCH", "batch_water_mapping.xml" )
write_mi_data( L203.TechShrwt, "TechShrwt", "WATER_LEVEL2_DATA", "L203.TechShrwt", "WATER_XML_BATCH", "batch_water_mapping.xml" )
write_mi_data( L203.TechCoef, "TechCoef", "WATER_LEVEL2_DATA", "L203.TechCoef", "WATER_XML_BATCH", "batch_water_mapping.xml" )
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_mapping.xml", "WATER_XML_FINAL", "water_mapping.xml", "", "outFile" )

logstop()
