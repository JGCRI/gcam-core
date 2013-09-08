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
logstart( "L221.en_supply.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Primary fuel handling sectors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A21.sector <- readdata( "ENERGY_ASSUMPTIONS", "A21.sector" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
A21.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A21.subsector_logit" )
A21.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A21.subsector_shrwt" )
A21.subsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A21.subsector_interp" )
A21.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A21.globaltech_coef" )
A21.globaltech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A21.globaltech_cost" )
A21.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A21.globaltech_shrwt" )
A21.globaltech_keyword <- readdata( "ENERGY_ASSUMPTIONS", "A21.globaltech_keyword" )
A21.tradedtech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A21.tradedtech_coef" )
A21.tradedtech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A21.tradedtech_cost" )
A21.tradedtech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A21.tradedtech_shrwt" )
L111.Prod_EJ_R_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L111.Prod_EJ_R_F_Yh" )
L121.in_EJ_R_TPES_unoil_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L121.in_EJ_R_TPES_unoil_Yh" )
L121.in_EJ_R_TPES_crude_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L121.in_EJ_R_TPES_crude_Yh" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Supplysector information
printlog( "L221.Supplysector_en: Supply sector information for upstream energy handling sectors" )
L221.Supplysector_en <- write_to_all_regions( A21.sector, names_Supplysector, has.traded=T )

# 2b. Subsector information
printlog( "L221.SubsectorLogit_en: Subsector logit exponents of upstream energy handling sectors" )
L221.SubsectorLogit_en <- write_to_all_regions( A21.subsector_logit, names_SubsectorLogit, has.traded=T )

printlog( "L221.SubsectorShrwt_en and L221.SubsectorShrwtFllt_en: Subsector shareweights of upstream energy handling sectors" )
if( any( !is.na( A21.subsector_shrwt$year ) ) ){
	L221.SubsectorShrwt_en <- write_to_all_regions( A21.subsector_shrwt[ !is.na( A21.subsector_shrwt$year ), ], names_SubsectorShrwt, has.traded=T )
	}
if( any( !is.na( A21.subsector_shrwt$year.fillout ) ) ){
	L221.SubsectorShrwtFllt_en <- write_to_all_regions( A21.subsector_shrwt[ !is.na( A21.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt, has.traded=T )
	}

printlog( "L221.SubsectorInterp_en and L221.SubsectorInterpTo_en: Subsector shareweight interpolation of upstream energy handling sectors" )
if( any( is.na( A21.subsector_interp$to.value ) ) ){
	L221.SubsectorInterp_en <- write_to_all_regions( A21.subsector_interp[ is.na( A21.subsector_interp$to.value ), ], names_SubsectorInterp, has.traded=T )
	}
if( any( !is.na( A21.subsector_interp$to.value ) ) ){
	L221.SubsectorInterpTo_en <- write_to_all_regions( A21.subsector_interp[ !is.na( A21.subsector_interp$to.value ), ], names_SubsectorInterpTo, has.traded=T )
	}

# 2c. Technology information
#Identification of stub technologies (assume those in global tech shareweight table include all techs)
printlog( "L221.StubTech_en: Identification of stub technologies of upstream energy handling sectors" )
L221.StubTech_en <- write_to_all_regions( A21.globaltech_shrwt, names_Tech, has.traded=F )
names( L221.StubTech_en ) <- names_StubTech

#Drop stub technologies for biomassOil techs that do not exist
L221.rm_biomassOil_techs <- A21.globaltech_shrwt[ A21.globaltech_shrwt$supplysector == "regional biomassOil", s_s_t ]
L221.rm_biomassOil_techs_R <- repeat_and_add_vector( L221.rm_biomassOil_techs, R, GCAM_region_names[[R]] )
L221.rm_biomassOil_techs_R <- add_region_name( L221.rm_biomassOil_techs_R )
L221.rm_biomassOil_techs_R <- subset( L221.rm_biomassOil_techs_R, paste( region, technology ) %!in% paste( A_regions$region, A_regions$biomassOil_tech ) )
L221.StubTech_en <- L221.StubTech_en[
      vecpaste( L221.StubTech_en[ c( "region", "stub.technology" ) ] ) %!in% vecpaste( L221.rm_biomassOil_techs_R[ c( "region", "technology" ) ] ), ]

#Coefficients of global technologies
printlog( "L221.GlobalTechCoef_en: Energy inputs and coefficients of global technologies for upstream energy handling" )
L221.globaltech_coef.melt <- interpolate_and_melt( A21.globaltech_coef, c( model_base_years, model_future_years ), value.name="coefficient" )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L221.globaltech_coef.melt[ c( "sector.name", "subsector.name" ) ] <- L221.globaltech_coef.melt[ c( "supplysector", "subsector" ) ]
L221.GlobalTechCoef_en <- L221.globaltech_coef.melt[ names_GlobalTechCoef ]

#Costs of global technologies
printlog( "L221.GlobalTechCost_en: Costs of global technologies for upstream energy handling" )
L221.globaltech_cost.melt <- interpolate_and_melt( A21.globaltech_cost, c( model_base_years, model_future_years ), value.name="input.cost" )
L221.globaltech_cost.melt[ c( "sector.name", "subsector.name" ) ] <- L221.globaltech_cost.melt[ c( "supplysector", "subsector" ) ]
L221.GlobalTechCost_en <- L221.globaltech_cost.melt[ names_GlobalTechCost ]

#Shareweights of global technologies
printlog( "L221.GlobalTechShrwt_en: Shareweights of global technologies for upstream energy handling" )
L221.globaltech_shrwt.melt <- interpolate_and_melt( A21.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L221.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L221.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L221.GlobalTechShrwt_en <- L221.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]

#Keywords of global technologies
printlog( "L221.PrimaryConsKeyword_en: Primary energy consumption keywords" )
L221.PrimaryConsKeyword_en <- repeat_and_add_vector( A21.globaltech_keyword, Y, c( model_base_years, model_future_years ) )
L221.PrimaryConsKeyword_en[ c( "sector.name", "subsector.name" ) ] <- L221.PrimaryConsKeyword_en[ c( "supplysector", "subsector" ) ]
L221.PrimaryConsKeyword_en <- L221.PrimaryConsKeyword_en[ c( names_GlobalTechYr, "primary.consumption" ) ]

#Coefficients of traded technologies
printlog( "L221.TechCoef_en_Traded: Energy inputs, coefficients, and market names of traded technologies for upstream energy handling" )
L221.tradedtech_coef.melt <- interpolate_and_melt( A21.tradedtech_coef, c( model_base_years, model_future_years ), value.name="coefficient" )
L221.TechCoef_en_Traded <- write_to_all_regions( L221.tradedtech_coef.melt, names_TechCoef, has.traded = T, apply.to = "all", set.market = T )

#Costs of traded technologies
printlog( "L221.TechCost_en_Traded: Costs of traded technologies for upstream energy handling" )
L221.tradedtech_cost.melt <- interpolate_and_melt( A21.tradedtech_cost, c( model_base_years, model_future_years ), value.name="input.cost" )
L221.TechCost_en_Traded <- write_to_all_regions( L221.tradedtech_cost.melt, names_TechCost, has.traded = T, apply.to = "all", set.market = F )

#Shareweights of traded technologies
printlog( "L221.TechShrwt_en_Traded: Shareweights of traded technologies for upstream energy handling" )
L221.tradedtech_shrwt.melt <- interpolate_and_melt( A21.tradedtech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L221.TechShrwt_en_Traded <- write_to_all_regions( L221.tradedtech_shrwt.melt, c( names_TechYr, "share.weight" ), has.traded = T, apply.to = "all", set.market = F )

#2b. Calibration and region-specific data
printlog( "L221.StubTechCoef_unoil: Coefficient and market name of stub technologies for importing traded unconventional oil" )
L221.StubTechCoef_unoil <- subset( L221.globaltech_coef.melt, minicam.energy.input %in% L221.TechShrwt_en_Traded$supplysector )
L221.StubTechCoef_unoil$stub.technology <- L221.StubTechCoef_unoil$technology
L221.StubTechCoef_unoil <- write_to_all_regions( L221.StubTechCoef_unoil, names_StubTechCoef[ names_StubTechCoef != "market.name" ] )
L221.StubTechCoef_unoil$market.name <- GCAM_region_names$region[1]

L221.Prod_EJ_R_unoil_Yh.melt <- melt( L111.Prod_EJ_R_F_Yh[ grep( "unconventional", L111.Prod_EJ_R_F_Yh$fuel ),
      c( "GCAM_region_ID", X_model_base_years ) ], id.vars = "GCAM_region_ID" )
L221.Prod_EJ_R_unoil_Yh.melt <- add_region_name( L221.Prod_EJ_R_unoil_Yh.melt )
L221.Prod_EJ_R_unoil_Yh.melt$year <- substr( L221.Prod_EJ_R_unoil_Yh.melt$variable, 2, 5 )

printlog( "L221.Production_unoil: Calibrated production of unconventional oil" )
L221.Production_unoil <- subset( L221.TechCoef_en_Traded, supplysector == "traded unconventional oil" & year %in% model_base_years )
L221.Production_unoil$calOutputValue <- round(
      L221.Prod_EJ_R_unoil_Yh.melt$value[
         match( paste( L221.Production_unoil$market.name, L221.Production_unoil$year ),
                paste( L221.Prod_EJ_R_unoil_Yh.melt$region, L221.Prod_EJ_R_unoil_Yh.melt$year ) ) ],
      digits_calOutput )
L221.Production_unoil <- L221.Production_unoil[ c( names_TechYr, "calOutputValue" ) ]
L221.Production_unoil$calOutputValue[ is.na( L221.Production_unoil$calOutputValue ) ] <- 0             
L221.Production_unoil$year.share.weight <- L221.Production_unoil$year
L221.Production_unoil$subsector.share.weight <- ifelse( L221.Production_unoil$calOutputValue > 0, 1, 0 )
L221.Production_unoil$share.weight <- ifelse( L221.Production_unoil$calOutputValue >0, 1, 0 )

#Unconventional oil demand
L221.in_EJ_R_TPES_unoil_Yh.melt <- melt( L121.in_EJ_R_TPES_unoil_Yh, id.vars = R_S_F )
L221.in_EJ_R_TPES_unoil_Yh.melt <- add_region_name( L221.in_EJ_R_TPES_unoil_Yh.melt )
L221.in_EJ_R_TPES_unoil_Yh.melt$year <- substr( L221.in_EJ_R_TPES_unoil_Yh.melt$variable, 2, 5 )

printlog( "L221.StubTechProd_oil_unoil: Calibrated demand of unconventional oil" )
L221.StubTechProd_oil_unoil <- subset( L221.StubTech_en, supplysector == "regional oil" & subsector == "unconventional oil" )
L221.StubTechProd_oil_unoil <- repeat_and_add_vector( L221.StubTechProd_oil_unoil, "year", model_base_years )
L221.StubTechProd_oil_unoil$calOutputValue <- round(
      L221.in_EJ_R_TPES_unoil_Yh.melt$value[
         match( paste( L221.StubTechProd_oil_unoil$region, L221.StubTechProd_oil_unoil$year ),
                paste( L221.in_EJ_R_TPES_unoil_Yh.melt$region, L221.in_EJ_R_TPES_unoil_Yh.melt$year ) ) ],
      digits_calOutput )
L221.StubTechProd_oil_unoil$year.share.weight <- L221.StubTechProd_oil_unoil$year
L221.StubTechProd_oil_unoil$subsector.share.weight <- ifelse( L221.StubTechProd_oil_unoil$calOutputValue > 0, 1, 0 )
L221.StubTechProd_oil_unoil$share.weight <- ifelse( L221.StubTechProd_oil_unoil$calOutputValue > 0, 1, 0 )

#Crude oil demand
L221.in_EJ_R_TPES_crude_Yh.melt <- melt( L121.in_EJ_R_TPES_crude_Yh, id.vars = R_S_F )
L221.in_EJ_R_TPES_crude_Yh.melt <- add_region_name( L221.in_EJ_R_TPES_crude_Yh.melt )
L221.in_EJ_R_TPES_crude_Yh.melt$year <- substr( L221.in_EJ_R_TPES_crude_Yh.melt$variable, 2, 5 )

printlog( "L221.StubTechProd_oil_crude: Calibrated demand of crude oil" )
L221.StubTechProd_oil_crude <- subset( L221.StubTech_en, supplysector == "regional oil" & subsector == "crude oil" )
L221.StubTechProd_oil_crude <- repeat_and_add_vector( L221.StubTechProd_oil_crude, "year", model_base_years )
L221.StubTechProd_oil_crude$calOutputValue <- round(
      L221.in_EJ_R_TPES_crude_Yh.melt$value[
         match( paste( L221.StubTechProd_oil_crude$region, L221.StubTechProd_oil_crude$year ),
                paste( L221.in_EJ_R_TPES_crude_Yh.melt$region, L221.in_EJ_R_TPES_crude_Yh.melt$year ) ) ],
      digits_calOutput )
L221.StubTechProd_oil_crude$year.share.weight <- L221.StubTechProd_oil_crude$year
L221.StubTechProd_oil_crude$subsector.share.weight <- ifelse( L221.StubTechProd_oil_crude$calOutputValue > 0, 1, 0 )
L221.StubTechProd_oil_crude$share.weight <- ifelse( L221.StubTechProd_oil_crude$calOutputValue > 0, 1, 0 )

printlog( "L221.StubTechShrwt_bio: region-specific technology shareweights for biomassOil passthrough sector")
L221.globaltech_shrwt_bio <- interpolate_and_melt( subset( A21.globaltech_shrwt, supplysector == "regional biomassOil" ),
      c( model_base_years, model_future_years ), value.name="share.weight" )
L221.StubTechShrwt_bio <- write_to_all_regions( L221.globaltech_shrwt_bio, c( "region", names( L221.globaltech_shrwt_bio ) ) )
L221.StubTechShrwt_bio$stub.technology <- L221.StubTechShrwt_bio$technology
L221.StubTechShrwt_bio$share.weight <- ifelse(
      vecpaste( L221.StubTechShrwt_bio[ c( "region", "stub.technology" ) ] ) %in% vecpaste( A_regions[ c( "region", "biomassOil_tech" ) ] ),
      1, 0 )
L221.StubTechShrwt_bio <- L221.StubTechShrwt_bio[ c( names_StubTechYr, "share.weight" ) ]
L221.StubTechShrwt_bio <- subset( L221.StubTechShrwt_bio,
      vecpaste( L221.StubTechShrwt_bio[ c( "stub.technology", "year", "share.weight" ) ] ) != 
      vecpaste( L221.globaltech_shrwt_bio[ c( "technology", "year", "share.weight" ) ] ) )

###For regions with no agricultural and land use sector (Taiwan), need to remove the passthrough supplysectors for first-gen biofuels
ag_en <- c( "regional corn for ethanol", "regional sugar for ethanol", "regional biomassOil" )
L221.Supplysector_en <- L221.Supplysector_en[ vecpaste( L221.Supplysector_en[ c( "region", supp ) ] ) %!in% paste( no_aglu_regions, ag_en ), ]
L221.SubsectorLogit_en <- L221.SubsectorLogit_en[ vecpaste( L221.SubsectorLogit_en[ c( "region", supp ) ] ) %!in% paste( no_aglu_regions, ag_en ), ]
if( exists( "L221.SubsectorShrwt_en" ) ){
	L221.SubsectorShrwt_en <- L221.SubsectorShrwt_en[ vecpaste( L221.SubsectorShrwt_en[ c( "region", supp ) ] ) %!in% paste( no_aglu_regions, ag_en ), ]
}
if( exists( "L221.SubsectorShrwtFllt_en" ) ){
	L221.SubsectorShrwtFllt_en <- L221.SubsectorShrwtFllt_en[ vecpaste( L221.SubsectorShrwtFllt_en[ c( "region", supp ) ] ) %!in% paste( no_aglu_regions, ag_en ), ]
}
if( exists( "L221.SubsectorInterp_en" ) ) {
	L221.SubsectorInterp_en <- L221.SubsectorInterp_en[ vecpaste( L221.SubsectorInterp_en[ c( "region", supp ) ] ) %!in% paste( no_aglu_regions, ag_en ), ]
}
if( exists( "L221.SubsectorInterpTo_en" ) ) {
	L221.SubsectorInterpTo_en <- L221.SubsectorInterpTo_en[ vecpaste( L221.SubsectorInterpTo_en[ c( "region", supp ) ] ) %!in% paste( no_aglu_regions, ag_en ), ]
}
L221.StubTech_en <- L221.StubTech_en[ vecpaste( L221.StubTech_en[ c( "region", supp ) ] ) %!in% paste( no_aglu_regions, ag_en ), ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L221.Supplysector_en, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L221.Supplysector_en",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_en_supply.xml" ) 
write_mi_data( L221.SubsectorLogit_en, "SubsectorLogit", "ENERGY_LEVEL2_DATA", "L221.SubsectorLogit_en", "ENERGY_XML_BATCH", "batch_en_supply.xml" ) 
if( exists( "L221.SubsectorShrwt_en" ) ){
	write_mi_data( L221.SubsectorShrwt_en, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L221.SubsectorShrwt_en", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
	}
if( exists( "L221.SubsectorShrwtFllt_en" ) ){
	write_mi_data( L221.SubsectorShrwtFllt_en, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L221.SubsectorShrwtFllt_en", "ENERGY_XML_BATCH", "batch_en_supply.xml" ) 
	}
if( exists( "L221.SubsectorInterp_en" ) ) {
	write_mi_data( L221.SubsectorInterp_en, "SubsectorInterp", "ENERGY_LEVEL2_DATA", "L221.SubsectorInterp_en", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
	}
if( exists( "L221.SubsectorInterpTo_en" ) ) {
	write_mi_data( L221.SubsectorInterpTo_en, "SubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L221.SubsectorInterpTo_en", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
	}
write_mi_data( L221.StubTech_en, "StubTech", "ENERGY_LEVEL2_DATA", "L221.StubTech_en", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
write_mi_data( L221.GlobalTechCoef_en, "GlobalTechCoef", "ENERGY_LEVEL2_DATA", "L221.GlobalTechCoef_en", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
write_mi_data( L221.GlobalTechCost_en, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L221.GlobalTechCost_en", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
write_mi_data( L221.GlobalTechShrwt_en, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L221.GlobalTechShrwt_en", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
write_mi_data( L221.PrimaryConsKeyword_en, "PrimaryConsKeyword", "ENERGY_LEVEL2_DATA", "L221.PrimaryConsKeyword_en", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
write_mi_data( L221.TechCoef_en_Traded, "TechCoef", "ENERGY_LEVEL2_DATA", "L221.TechCoef_en_Traded", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
write_mi_data( L221.TechCost_en_Traded, "TechCost", "ENERGY_LEVEL2_DATA", "L221.TechCost_en_Traded", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
write_mi_data( L221.TechShrwt_en_Traded, "TechShrwt", "ENERGY_LEVEL2_DATA", "L221.TechShrwt_en_Traded", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
write_mi_data( L221.StubTechCoef_unoil, "StubTechCoef", "ENERGY_LEVEL2_DATA", "L221.StubTechCoef_unoil", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
write_mi_data( L221.Production_unoil, "Production", "ENERGY_LEVEL2_DATA", "L221.Production_unoil", "ENERGY_XML_BATCH", "batch_en_supply.xml" ) 
write_mi_data( L221.StubTechProd_oil_unoil, "StubTechProd", "ENERGY_LEVEL2_DATA", "L221.StubTechProd_oil_unoil", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
write_mi_data( L221.StubTechProd_oil_crude, "StubTechProd", "ENERGY_LEVEL2_DATA", "L221.StubTechProd_oil_crude", "ENERGY_XML_BATCH", "batch_en_supply.xml" )
write_mi_data( L221.StubTechShrwt_bio, "StubTechShrwt", "ENERGY_LEVEL2_DATA", "L221.StubTechShrwt_bio", "ENERGY_XML_BATCH", "batch_en_supply.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_en_supply.xml", "ENERGY_XML_FINAL", "en_supply.xml", "", xml_tag="outFile" )

logstop()


