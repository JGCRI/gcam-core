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
logstart( "L261.carbon_storage_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for GCAM USA energy electricity sectors (1 region)" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_rsrc_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
L161.Cstorage_FERC <- readdata( "GCAMUSA_LEVEL1_DATA", "L161.Cstorage_FERC" )
L261.DepRsrc <- readdata( "ENERGY_LEVEL2_DATA", "L261.DepRsrc", skip = 4 )
L261.Supplysector_C <- readdata( "ENERGY_LEVEL2_DATA", "L261.Supplysector_C", skip = 4 )
L261.SubsectorLogit_C <- readdata( "ENERGY_LEVEL2_DATA", "L261.SubsectorLogit_C", skip = 4, must.exist = F )
L261.SubsectorShrwtFllt_C <- readdata( "ENERGY_LEVEL2_DATA", "L261.SubsectorShrwtFllt_C", skip = 4, must.exist = F )
L261.StubTech_C <- readdata( "ENERGY_LEVEL2_DATA", "L261.StubTech_C", skip = 4 )
L261.GlobalTechCoef_C <- readdata( "ENERGY_LEVEL2_DATA", "L261.GlobalTechCoef_C", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Carbon storage onshore resources are modeled in the FERC regions with non-zero storage curves" )
C_grid_regions <- sort( unique( L161.Cstorage_FERC$grid_region ) )
noC_grid_regions <- sort( unique( states_subregions$grid_region[ states_subregions$grid_region %!in% C_grid_regions ] ) )

printlog( "L261.DeleteDepRsrc_USAC: delete onshore carbon storage in the USA" )
L261.DeleteDepRsrc_USAC <- L261.DepRsrc[ L261.DepRsrc$region == "USA", c( "region", "depresource" ) ]

printlog( "L261.DeleteSubsector_USAC: delete onshore carbon storage subsector of carbon storage sector in the USA" )
printlog( "NOTE: leaving the offshore here so that the USA hydrogen sector has a carbon storage market" )
L261.DeleteSubsector_USAC <- subset( L261.SubsectorShrwtFllt_C[ names_Subsector ], region == "USA" & subsector %in% L261.DepRsrc$depresource  )

grid_Cstorage_nonexist <- paste( noC_grid_regions, L261.DeleteDepRsrc_USAC$depresource[1] )

printlog( "L261.DepRsrc_FERC: onshore storage in the FERC regions" )
L261.DepRsrc_FERC <- repeat_and_add_vector( subset( L261.DepRsrc, region == "USA" ), "region", C_grid_regions )
L261.DepRsrc_FERC$market <- L261.DepRsrc_FERC$region

#make an object with the grid regions that don't have any resources, if there are any

printlog( "L261.DepRsrcCurves_FERC: onshore storage supply curves in the FERC regions" )
L261.DepRsrcCurves_FERC <- data.frame(
      region = L161.Cstorage_FERC$grid_region,
      depresource = L261.DepRsrc_FERC$depresource[1],
      subresource = L261.DepRsrc_FERC$depresource[1],
      grade = L161.Cstorage_FERC$grade,
      available = round( L161.Cstorage_FERC$MtC, digits_depresource ),
      extractioncost = round( L161.Cstorage_FERC$Cost_1990USDtC, digits_cost ) )

printlog( "L261.Supplysector_C_USA: supplysector info in the states" )
L261.Supplysector_C_USA <- write_to_all_states( subset( L261.Supplysector_C, region == "USA" ), names_Supplysector)
L261.SectorLogitTables_C_USA <- read_logit_fn_tables( "ENERGY_LEVEL2_DATA", "L261.Supplysector_", skip=4, include.equiv.table=T )
for( curr_table_name in names( L261.SectorLogitTables_C_USA ) ) {
    if( substr( curr_table_name, 6, 16 ) != "EQUIV_TABLE" && nrow( L261.SectorLogitTables_C_USA[[ curr_table_name ]] ) > 0 ) {
        L261.SectorLogitTables_C_USA[[ curr_table_name ]] <- write_to_all_states(
            subset( L261.SectorLogitTables_C_USA[[ curr_table_name ]], region == "USA" ), names_SupplysectorLogitType )
    }
}

printlog( "L261.SubsectorLogit_C: subsector logit info in the states" )
L261.SubsectorLogit_C_USA <- write_to_all_states( subset( L261.SubsectorLogit_C, region == "USA" ), names_SubsectorLogit )
#NOTE: This table contains logit values in states where no C storage resources may exist at the grid level
L261.SubsectorLogit_C_USA$grid_region <- states_subregions$grid_region[
      match( L261.SubsectorLogit_C_USA$region, states_subregions$state ) ]
L261.SubsectorLogit_C_USA <- subset( L261.SubsectorLogit_C_USA, !paste( grid_region, subsector ) %in% grid_Cstorage_nonexist )
L261.SubsectorLogitTables_C_USA <- read_logit_fn_tables( "ENERGY_LEVEL2_DATA", "L261.SubsectorLogit_", skip=4, include.equiv.table=F )
for( curr_table_name in names( L261.SubsectorLogitTables_C_USA ) ) {
    if( substr( curr_table_name, 6, 16 ) != "EQUIV_TABLE" && nrow( L261.SubsectorLogitTables_C_USA[[ curr_table_name ]] ) > 0 ) {
        L261.SubsectorLogitTables_C_USA[[ curr_table_name ]] <- write_to_all_states(
            subset( L261.SubsectorLogitTables_C_USA[[ curr_table_name ]], region == "USA" ), names_SubsectorLogitType )
        L261.SubsectorLogitTables_C_USA[[ curr_table_name ]]$grid_region <- states_subregions$grid_region[
            match( L261.SubsectorLogitTables_C_USA[[ curr_table_name ]]$region, states_subregions$state ) ]
        L261.SubsectorLogitTables_C_USA[[ curr_table_name ]] <- subset( L261.SubsectorLogitTables_C_USA[[ curr_table_name ]],
            !paste( grid_region, subsector ) %in% grid_Cstorage_nonexist )
    }
}

printlog( "L261.SubsectorShrwtFllt_C_USA: subsector shareweight info in the states" )
L261.SubsectorShrwtFllt_C_USA <- write_to_all_states( subset( L261.SubsectorShrwtFllt_C, region == "USA" ), names_SubsectorShrwtFllt)
#NOTE: This table contains shareweight values in states where no C storage resources may exist at the grid level
L261.SubsectorShrwtFllt_C_USA$grid_region <- states_subregions$grid_region[
      match( L261.SubsectorShrwtFllt_C_USA$region, states_subregions$state ) ]
L261.SubsectorShrwtFllt_C_USA <- subset( L261.SubsectorShrwtFllt_C_USA, !paste( grid_region, subsector ) %in% grid_Cstorage_nonexist )

printlog( "L261.StubTech_C_USA: stub technology info for the states" )
L261.StubTech_C_USA <- write_to_all_states( subset( L261.StubTech_C, region == "USA" ), names_StubTech )
L261.StubTech_C_USA$grid_region <- states_subregions$grid_region[
      match( L261.StubTech_C_USA$region, states_subregions$state ) ]
L261.StubTech_C_USA <- subset( L261.StubTech_C_USA, !paste( grid_region, stub.technology ) %in% grid_Cstorage_nonexist )[ names_StubTech ]

printlog( "L261.StubTechMarket_C_USA: stub technology market names for the grid regions" )
L261.StubTechMarket_C_USA <- repeat_and_add_vector( L261.StubTech_C_USA, Y, model_years )
L261.StubTechMarket_C_USA[[input]] <- L261.GlobalTechCoef_C[[input]][
      match( vecpaste( L261.StubTechMarket_C_USA[ c( supp, subs, "stub.technology" ) ] ),
             vecpaste( L261.GlobalTechCoef_C[ c( "sector.name", "subsector.name", tech ) ] ) ) ]
L261.StubTechMarket_C_USA$market.name <- states_subregions$grid_region[
      match( L261.StubTechMarket_C_USA $region, states_subregions$state ) ]

#Offshore carbon storage is from the USA market
L261.StubTechMarket_C_USA$market.name[ L261.StubTechMarket_C_USA[[input]] %!in% L261.DepRsrc_FERC$depresource ] <- "USA"

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L261.DeleteDepRsrc_USAC, "DeleteDepRsrc", "GCAMUSA_LEVEL2_DATA", "L261.DeleteDepRsrc_USAC", "GCAMUSA_XML_BATCH", "batch_Cstorage_USA.xml" )
write_mi_data( L261.DeleteSubsector_USAC, "DeleteSubsector", "GCAMUSA_LEVEL2_DATA", "L261.DeleteSubsector_USAC", "GCAMUSA_XML_BATCH", "batch_Cstorage_USA.xml" )
write_mi_data( L261.DepRsrc_FERC, "DepRsrc", "GCAMUSA_LEVEL2_DATA", "L261.DepRsrc_FERC", "GCAMUSA_XML_BATCH", "batch_Cstorage_USA.xml" )
write_mi_data( L261.DepRsrcCurves_FERC, "DepRsrcCurves", "GCAMUSA_LEVEL2_DATA", "L261.DepRsrcCurves_FERC", "GCAMUSA_XML_BATCH", "batch_Cstorage_USA.xml" )
for( curr_table in names ( L261.SectorLogitTables_C_USA ) ) {
curr_header <- substr( curr_table, 6, nchar( curr_table ) )
write_mi_data( L261.SectorLogitTables_C_USA[[ curr_table ]], curr_header, "GCAMUSA_LEVEL2_DATA", paste0( "L261.", curr_header, "_C_USA" ),
    "GCAMUSA_XML_BATCH", "batch_Cstorage_USA.xml" )
}
write_mi_data( L261.Supplysector_C_USA, "Supplysector", "GCAMUSA_LEVEL2_DATA", "L261.Supplysector_C_USA", "GCAMUSA_XML_BATCH", "batch_Cstorage_USA.xml" )
for( curr_table in names ( L261.SubsectorLogitTables_C_USA ) ) {
curr_header <- substr( curr_table, 6, nchar( curr_table ) )
write_mi_data( L261.SubsectorLogitTables_C_USA[[ curr_table ]], curr_header, "GCAMUSA_LEVEL2_DATA", paste0( "L261.", curr_header, "_C_USA" ),
    "GCAMUSA_XML_BATCH", "batch_Cstorage_USA.xml" )
}
write_mi_data( L261.SubsectorLogit_C_USA, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L261.SubsectorLogit_C_USA", "GCAMUSA_XML_BATCH", "batch_Cstorage_USA.xml" )
write_mi_data( L261.SubsectorShrwtFllt_C_USA, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L261.SubsectorShrwtFllt_C_USA", "GCAMUSA_XML_BATCH", "batch_Cstorage_USA.xml" )
write_mi_data( L261.StubTech_C_USA, "StubTech", "GCAMUSA_LEVEL2_DATA", "L261.StubTech_C_USA", "GCAMUSA_XML_BATCH", "batch_Cstorage_USA.xml" )
write_mi_data( L261.StubTechMarket_C_USA, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L261.StubTechMarket_C_USA", "GCAMUSA_XML_BATCH", "batch_Cstorage_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_Cstorage_USA.xml", "GCAMUSA_XML_FINAL", "Cstorage_USA.xml", "", xml_tag="outFile" )

logstop()
