# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
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
logstart( "L261.Cstorage.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Carbon storage supply curves and sectors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A61.rsrc_info <- readdata( "ENERGY_ASSUMPTIONS", "A61.rsrc_info")
A61.sector <- readdata( "ENERGY_ASSUMPTIONS", "A61.sector")
A61.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A61.subsector_shrwt")
A61.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A61.globaltech_coef")
A61.globaltech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A61.globaltech_cost")
A61.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A61.globaltech_shrwt")
L161.RsrcCurves_MtC_R <- readdata( "ENERGY_LEVEL1_DATA", "L161.RsrcCurves_MtC_R" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Output unit, price unit, market
#Repeat and add region vector to resource assumptions table (use ID to ensure correct region ordering)
L261.rsrc_info <- repeat_and_add_vector( A61.rsrc_info, "GCAM_region_ID", GCAM_region_names$GCAM_region_ID )
L261.rsrc_info <- add_region_name( L261.rsrc_info )

#Reset regional markets to the names of the specific regions
L261.rsrc_info$market[ L261.rsrc_info$market == "regional" ] <- L261.rsrc_info$region[ L261.rsrc_info$market == "regional" ]

#Split different types of resources into separate tables
L261.dep_rsrc_info <- subset( L261.rsrc_info, resource_type == "depresource" )
L261.unlim_rsrc_info <- subset( L261.rsrc_info, resource_type == "unlimited-resource" )

printlog( "L261.DepRsrc: output unit, price unit, and market for depletable resources" )
L261.DepRsrc <- data.frame(
      region = L261.dep_rsrc_info$region,
      depresource = L261.dep_rsrc_info$resource,
      output.unit = L261.dep_rsrc_info$output.unit,
      price.unit = L261.dep_rsrc_info$price.unit,
      market = L261.dep_rsrc_info$market)

printlog( "L261.UnlimitRsrc: output unit, price unit, and market for unlimited resources" )
L261.UnlimitRsrc <- data.frame(
      region = L261.unlim_rsrc_info$region,
      unlimited.resource = L261.unlim_rsrc_info$resource,
      output.unit = L261.unlim_rsrc_info$output.unit,
      price.unit = L261.unlim_rsrc_info$price.unit,
      market = L261.unlim_rsrc_info$market,
      capacity.factor = L261.unlim_rsrc_info$capacity.factor )
      
# 2b. Resource supply curves
printlog( "L261.DepRsrcCurves_C: supply curves of carbon storage resources")
L261.DepRsrcCurves_C <- add_region_name( L161.RsrcCurves_MtC_R )
L261.DepRsrcCurves_C <- convert_rsrc_to_L2( L261.DepRsrcCurves_C, "depresource" )

#2c. Carbon storage sector information
printlog( "L261.Supplysector_C: Carbon storage supplysector information" )
L261.Supplysector_C <- write_to_all_regions( A61.sector, names_Supplysector )

# 2d. Subsector information
printlog( "L261.SubsectorShrwtFllt_C: Subsector shareweights of energy transformation sectors" )
L261.SubsectorShrwtFllt_C <- write_to_all_regions( A61.subsector_shrwt, names_SubsectorShrwtFllt )

#2e. Technology information
printlog( "L261.StubTech_C: Identification of stub technologies of energy transformation" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L261.StubTech_C <- write_to_all_regions( A61.globaltech_shrwt, names_Tech )
names( L261.StubTech_C ) <- names_StubTech

#Coefficients of global technologies
printlog( "L261.GlobalTechCoef_C: Energy inputs and coefficients of global technologies for carbon storage" )
L261.globaltech_coef.melt <- interpolate_and_melt( A61.globaltech_coef, c( model_base_years, model_future_years ), value.name="coefficient", digits = digits_coefficient )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L261.globaltech_coef.melt[ c( "sector.name", "subsector.name" ) ] <- L261.globaltech_coef.melt[ c( "supplysector", "subsector" ) ]
L261.GlobalTechCoef_C <- L261.globaltech_coef.melt[ names_GlobalTechCoef ]

#Costs of global technologies
printlog( "L261.GlobalTechCost_C: Costs of global technologies for carbon storage" )
L261.globaltech_cost.melt <- interpolate_and_melt( A61.globaltech_cost, c( model_base_years, model_future_years ), value.name="input.cost", digits = digits_cost )
L261.globaltech_cost.melt[ c( "sector.name", "subsector.name" ) ] <- L261.globaltech_cost.melt[ c( "supplysector", "subsector" ) ]
L261.GlobalTechCost_C <- L261.globaltech_cost.melt[ names_GlobalTechCost ]

#Shareweights of global technologies
printlog( "L261.GlobalTechShrwt_C: Shareweights of global technologies for energy transformation" )
L261.globaltech_shrwt.melt <- interpolate_and_melt( A61.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L261.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L261.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L261.GlobalTechShrwt_C <- L261.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L261.DepRsrc, IDstring="DepRsrc", domain="ENERGY_LEVEL2_DATA", fn="L261.DepRsrc", batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_Cstorage.xml" ) 
write_mi_data( L261.UnlimitRsrc, "UnlimitRsrc", "ENERGY_LEVEL2_DATA", "L261.UnlimitRsrc", "ENERGY_XML_BATCH", "batch_Cstorage.xml" ) 
write_mi_data( L261.DepRsrcCurves_C, "DepRsrcCurves", "ENERGY_LEVEL2_DATA", "L261.DepRsrcCurves_C", "ENERGY_XML_BATCH", "batch_Cstorage.xml" ) 
write_mi_data( L261.Supplysector_C, "Supplysector", "ENERGY_LEVEL2_DATA", "L261.Supplysector_C", "ENERGY_XML_BATCH", "batch_Cstorage.xml" ) 
write_mi_data( L261.SubsectorShrwtFllt_C, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L261.SubsectorShrwtFllt_C", "ENERGY_XML_BATCH", "batch_Cstorage.xml" ) 
write_mi_data( L261.StubTech_C, "StubTech", "ENERGY_LEVEL2_DATA", "L261.StubTech_C", "ENERGY_XML_BATCH", "batch_Cstorage.xml" ) 
write_mi_data( L261.GlobalTechCoef_C, "GlobalTechCoef", "ENERGY_LEVEL2_DATA", "L261.GlobalTechCoef_C", "ENERGY_XML_BATCH", "batch_Cstorage.xml" ) 
write_mi_data( L261.GlobalTechCost_C, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L261.GlobalTechCost_C", "ENERGY_XML_BATCH", "batch_Cstorage.xml" ) 
write_mi_data( L261.GlobalTechShrwt_C, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L261.GlobalTechShrwt_C", "ENERGY_XML_BATCH", "batch_Cstorage.xml" ) 

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_Cstorage.xml", "ENERGY_XML_FINAL", "Cstorage.xml", "", xml_tag="outFile" )

logstop()