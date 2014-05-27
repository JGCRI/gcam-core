# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "EMISSPROC_DIR" ) ){
    if( Sys.getenv( "EMISSIONSPROC" ) != "" ){
        EMISSPROC_DIR <- Sys.getenv( "EMISSIONSPROC" )
    } else {
        stop("Could not determine location of emissions data system. Please set the R var EMISSPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
logstart( "L231.proc_sector.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Urban and industrial processes sectors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_region <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
A31.rsrc_info <- readdata( "EMISSIONS_ASSUMPTIONS", "A31.rsrc_info" )
A31.sector <- readdata( "EMISSIONS_ASSUMPTIONS", "A31.sector" )
A31.subsector_logit <- readdata( "EMISSIONS_ASSUMPTIONS", "A31.subsector_logit" )
A31.subsector_shrwt <- readdata( "EMISSIONS_ASSUMPTIONS", "A31.subsector_shrwt" )
A31.subsector_interp <- readdata( "EMISSIONS_ASSUMPTIONS", "A31.subsector_interp" )
A31.globaltech_shrwt <- readdata( "EMISSIONS_ASSUMPTIONS", "A31.globaltech_shrwt" )
A31.globaltech_eff <- readdata( "EMISSIONS_ASSUMPTIONS", "A31.globaltech_eff" )
A31.globaltech_cost <- readdata( "EMISSIONS_ASSUMPTIONS", "A31.globaltech_cost" )
A31.globaltech_coef <- readdata( "EMISSIONS_ASSUMPTIONS", "A31.globaltech_coef" )
Ind.globaltech_eff <- readdata( "ENERGY_ASSUMPTIONS", "A32.globaltech_eff" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
L1322.in_EJ_R_indfeed_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1322.in_EJ_R_indfeed_F_Yh" )
L1322.in_EJ_R_indenergy_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1322.in_EJ_R_indenergy_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#First find sectors
L231.urban <- subset( GCAM_sector_tech, GCAM_sector_tech$sector == "urban processes" )
L231.ind <- subset( GCAM_sector_tech, GCAM_sector_tech$sector == "industrial processes" )

# 2a. Supplysector information
printlog( "L231.FinalDemand_urb: Final demand information for urban processes sector" )
L231.FinalDemand_urb <- data.frame( region = A_region$region )
L231.FinalDemand_urb$energy.final.demand <- "urban processes"
L231.FinalDemand_urb$perCapitaBased <- 1
L231.FinalDemand_urb$income.elasticity <- 0
L231.FinalDemand_urb <- repeat_and_add_vector( L231.FinalDemand_urb, "year", model_base_years )
L231.FinalDemand_urb$base.service <- 0.004
L231.FinalDemand_urb$aeei <- 0

printlog( "L231.Supplysector_ind: Supply sector information for urban & industrial processes sectors" )
L231.Supplysector_urb_ind <- write_to_all_regions( A31.sector, names_Supplysector )

# 2b. Subsector information
printlog( "L231.SubsectorLogit_urb_ind: Subsector logit exponents of urban & industrial processes sectors" )
L231.SubsectorLogit_urb_ind <- write_to_all_regions( A31.subsector_logit, names_SubsectorLogit )

printlog( "L231.SubsectorShrwt_urb_ind and L231.SubsectorShrwtFllt_urb_ind: Subsector shareweights of urban & industrial processes sectors" )
if( any( !is.na( A31.subsector_shrwt$year ) ) ){
	L231.SubsectorShrwt_urb_ind <- write_to_all_regions( A31.subsector_shrwt[ !is.na( A31.subsector_shrwt$year ), ], names_SubsectorShrwt )
	}
if( any( !is.na( A31.subsector_shrwt$year.fillout ) ) ){
	L231.SubsectorShrwtFllt_urb_ind <- write_to_all_regions( A31.subsector_shrwt[ !is.na( A31.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
	}

printlog( "L231.SubsectorInterp_urb_ind and L231.SubsectorInterpTo_urb_ind: Subsector shareweight interpolation of urban & industrial processes sector" )
if( any( is.na( A31.subsector_interp$to.value ) ) ){
	L231.SubsectorInterp_urb_ind <- write_to_all_regions( A31.subsector_interp[ is.na( A31.subsector_interp$to.value ), ], names_SubsectorInterp )
	}
if( any( !is.na( A31.subsector_interp$to.value ) ) ){
	L231.SubsectorInterpTo_urb_ind <- write_to_all_regions( A31.subsector_interp[ !is.na( A31.subsector_interp$to.value ), ], names_SubsectorInterpTo )
	}

# 2c. Technology information
printlog( "L231.StubTech_urb_ind: Identification of stub technologies of urban & industrial processes sectors" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L231.StubTech_urb_ind <- write_to_all_regions( A31.globaltech_shrwt, names_Tech )
names( L231.StubTech_urb_ind ) <- names_StubTech

printlog( "L231.GlobalTechShrwt_urb_ind: Shareweights of global urban & industrial processes sector technologies" )
L231.globaltech_shrwt.melt <- interpolate_and_melt( A31.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L231.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L231.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L231.GlobalTechShrwt_urb_ind <- L231.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]

printlog( "L231.GlobalTechEff_urb_ind: Energy inputs and coefficients of global urban & industrial processes technologies" )
L231.globaltech_eff.melt <- interpolate_and_melt( A31.globaltech_eff, c( model_base_years, model_future_years ), value.name="efficiency", digits = digits_efficiency )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L231.globaltech_eff.melt[ c( "sector.name", "subsector.name" ) ] <- L231.globaltech_eff.melt[ c( "supplysector", "subsector" ) ]
L231.GlobalTechEff_urb_ind <- L231.globaltech_eff.melt[ names_GlobalTechEff ]

#Coefficients on global industry sector technologies (not energy-use or feedstocks)
printlog( "L231.GlobalTechCoef_urb_ind: Energy inputs and coefficients of global urban & industrial processes technologies" )
L231.globaltech_coef.melt <- interpolate_and_melt( A31.globaltech_coef, c( model_base_years, model_future_years ), value.name="coefficient" )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L231.globaltech_coef.melt[ c( "sector.name", "subsector.name" ) ] <- L231.globaltech_coef.melt[ c( "supplysector", "subsector" ) ]
L231.GlobalTechCoef_urb_ind <- L231.globaltech_coef.melt[ names_GlobalTechCoef ]

#Costs of global technologies
printlog( "L231.GlobalTechCost_urb_ind: Capital costs of global urban & industrial processes technologies" )
L231.globaltech_cost.melt <- interpolate_and_melt( A31.globaltech_cost, c( model_base_years, model_future_years ), value.name="input.cost" )
L231.globaltech_cost.melt[ c( "sector.name", "subsector.name" ) ] <- L231.globaltech_cost.melt[ c( "supplysector", "subsector" ) ]
L231.GlobalTechCost_urb_ind <- L231.globaltech_cost.melt[ names_GlobalTechCost ]

#Calibration and region-specific data
printlog( "L231.StubTechCalInput_calvalue: calibrated input of urban & industrial processes technologies")
L231.RegionalTechCalValue_urb_ind <- L231.GlobalTechCost_urb_ind[ names( L231.GlobalTechCost_urb_ind ) %!in% c( "minicam.non.energy.input", "input.cost" )]
L231.RegionalTechCalValue_urb_ind <- repeat_and_add_vector( L231.RegionalTechCalValue_urb_ind, "region", A_region$region )
L231.RegionalTechCalValue_urb_ind$minicam.energy.input <- "misc emissions sources"
L231.RegionalTechCalValue_urb_ind$calibrated.value <- 0.001
L231.RegionalTechCalValue_urb_ind <- L231.RegionalTechCalValue_urb_ind[ c( "region", names_GlobalTechYr, "minicam.energy.input", "calibrated.value" ) ]
L231.RegionalTechCalValue_urb_ind <- subset( L231.RegionalTechCalValue_urb_ind, L231.RegionalTechCalValue_urb_ind$year %in% model_base_years )

# 2d. Resource Infomation
#Interpolate to specified historical years, as necessary
L231.rsrc_info <- gcam_interp( A31.rsrc_info, model_base_years )

#Repeat and add region vector to resource assumptions table (use ID to ensure correct region ordering)
L231.rsrc_info <- repeat_and_add_vector( A31.rsrc_info, "GCAM_region_ID", GCAM_region_names$GCAM_region_ID )
L231.rsrc_info <- add_region_name( L231.rsrc_info )

#Reset regional markets to the names of the specific regions
L231.rsrc_info$market[ L231.rsrc_info$market == "regional" ] <- L231.rsrc_info$region[ L231.rsrc_info$market == "regional" ]

#Split different types of resources into separate tables
L231.unlim_rsrc_info <- subset( L231.rsrc_info, resource_type == "unlimited-resource" )

printlog( "L231.UnlimitRsrc: output unit, price unit, and market for unlimited resources" )
L231.UnlimitRsrc <- data.frame(
      region = L231.unlim_rsrc_info$region,
      unlimited.resource = L231.unlim_rsrc_info$resource,
      output.unit = L231.unlim_rsrc_info$output.unit,
      price.unit = L231.unlim_rsrc_info$price.unit,
      market = L231.unlim_rsrc_info$market,
      capacity.factor = L231.unlim_rsrc_info$capacity.factor )
      
printlog( "L231.UnlimitRsrcPrice: prices for unlimited resources" )
L231.unlimit_rsrc_price.melt <- interpolate_and_melt(
      L231.unlim_rsrc_info[ names( L231.unlim_rsrc_info ) %in% c( "region", "resource", X_historical_years ) ],
      model_base_years )
L231.UnlimitRsrcPrice <- data.frame(
      region = L231.unlimit_rsrc_price.melt$region,
      unlimited.resource = L231.unlimit_rsrc_price.melt$resource,
      year = L231.unlimit_rsrc_price.melt$year,
      price = L231.unlimit_rsrc_price.melt$value )
      
printlog( "L231.IndCoef: coefficient on industrial processes as an input to the industry sector" )
# Coefficient = 0.008 / change in industry output from 1990 (0.008 is the sum of calvalue)
L1322.in_EJ_R_indenergy_F_Yh$sector <- "industrial energy use"
L1322.in_EJ_R_indfeed_F_Yh$sector <- "industrial feedstocks"
L231.IndBaseService <- rbind( L1322.in_EJ_R_indenergy_F_Yh, L1322.in_EJ_R_indfeed_F_Yh)
L231.IndBaseService.melt <- melt( L231.IndBaseService, id.vars=c( "GCAM_region_ID", "sector", "fuel" ) )
Ind.globaltech_eff.melt <- interpolate_and_melt( Ind.globaltech_eff, c( model_base_years, model_future_years ), value.name="efficiency", digits = digits_efficiency )
L231.IndBaseService.melt$efficiency <- Ind.globaltech_eff.melt$efficiency[ match( vecpaste(L231.IndBaseService.melt[ c( "sector", "fuel", "variable" )]), vecpaste( Ind.globaltech_eff.melt[ c( "supplysector", "subsector", "variable" )]) )]
L231.IndBaseService.melt$service <- L231.IndBaseService.melt$value * L231.IndBaseService.melt$efficiency
L231.IndBaseService.melt <- na.omit( L231.IndBaseService.melt )
L231.IndBaseService.melt <- aggregate( L231.IndBaseService.melt$service, by=as.list( L231.IndBaseService.melt[ c( "GCAM_region_ID", "variable" )]), sum)
names( L231.IndBaseService.melt )[ names( L231.IndBaseService.melt ) == "x" ] <- "ind_output"

L231.IndCoef <- L231.IndBaseService.melt
L231.IndCoef$ind_proc_input <- 0.008
L231.IndCoef$coefficient <- L231.IndCoef$ind_proc_input / L231.IndCoef$ind_output
L231.IndCoef <- add_region_name( L231.IndCoef )
L231.IndCoef$supplysector <- "industry"
L231.IndCoef$subsector <- "industry"
L231.IndCoef$technology <- "industry"
L231.IndCoef$minicam.energy.input <- "industrial processes"
L231.IndCoef$year <- as.numeric( substr( L231.IndCoef$variable, 2, 5 ) )
L231.IndCoef <- L231.IndCoef[ c( names_TechYr, "minicam.energy.input", "coefficient" ) ]

#Add in future years
L231.IndCoef.fby <- subset( L231.IndCoef, L231.IndCoef$year == final_historical_year )
L231.IndCoef.fy <- repeat_and_add_vector( L231.IndCoef.fby, "fyear", future_years )
L231.IndCoef.fy$year <- L231.IndCoef.fy$fyear
L231.IndCoef.fy <- L231.IndCoef.fy[ names( L231.IndCoef.fy ) != "fyear" ]

#Bind together
L231.IndCoef <- rbind( L231.IndCoef, L231.IndCoef.fy )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L231.UnlimitRsrc, "UnlimitRsrc", "EMISSIONS_LEVEL2_DATA", "L231.UnlimitRsrc", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" ) 
write_mi_data( L231.UnlimitRsrcPrice, "UnlimitRsrcPrice", "EMISSIONS_LEVEL2_DATA", "L231.UnlimitRsrcPrice", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" ) 
write_mi_data( L231.FinalDemand_urb, "FinalDemandInfo", "EMISSIONS_LEVEL2_DATA", "L231.FinalDemand_urb", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" )
write_mi_data( L231.Supplysector_urb_ind, IDstring="Supplysector", domain="EMISSIONS_LEVEL2_DATA", fn="L231.Supplysector_urb_ind",
               batch_XML_domain="EMISSIONS_XML_BATCH", batch_XML_file="batch_ind_urb_processing_sectors.xml" )  
write_mi_data( L231.SubsectorLogit_urb_ind, "SubsectorLogit", "EMISSIONS_LEVEL2_DATA", "L231.SubsectorLogit_urb_ind", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" ) 
if( exists( "L231.SubsectorShrwt_urb_ind" ) ){
	write_mi_data( L231.SubsectorShrwt_ind, "SubsectorShrwt", "EMISSIONS_LEVEL2_DATA", "L231.SubsectorShrwt_urb_ind", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" )
	}
if( exists( "L231.SubsectorShrwtFllt_urb_ind" ) ){
	write_mi_data( L231.SubsectorShrwtFllt_urb_ind, "SubsectorShrwtFllt", "EMISSIONS_LEVEL2_DATA", "L231.SubsectorShrwtFllt_urb_ind",
	               "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" ) 
	}
if( exists( "L231.SubsectorInterp_urb_ind" ) ) {
	write_mi_data( L231.SubsectorInterp_urb_ind, "SubsectorInterp", "EMISSIONS_LEVEL2_DATA", "L231.SubsectorInterp_urb_ind", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" )
	}
if( exists( "L231.SubsectorInterpTo_urb_ind" ) ) {
	write_mi_data( L231.SubsectorInterpTo_urb_ind, "SubsectorInterpTo", "EMISSIONS_LEVEL2_DATA", "L231.SubsectorInterpTo_urb_ind", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" )
	}
write_mi_data( L231.StubTech_urb_ind, "StubTech", "EMISSIONS_LEVEL2_DATA", "L231.StubTech_urb_ind", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" )
write_mi_data( L231.GlobalTechShrwt_urb_ind, "GlobalTechShrwt", "EMISSIONS_LEVEL2_DATA", "L231.GlobalTechShrwt_urb_ind", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" )
write_mi_data( L231.GlobalTechEff_urb_ind, "GlobalTechEff", "EMISSIONS_LEVEL2_DATA", "L231.GlobalTechEff_urb_ind", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" )
write_mi_data( L231.GlobalTechCoef_urb_ind, "GlobalTechCoef", "EMISSIONS_LEVEL2_DATA", "L231.GlobalTechCoef_urb_ind", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" )
write_mi_data( L231.GlobalTechCost_urb_ind, "GlobalTechCost", "EMISSIONS_LEVEL2_DATA", "L231.GlobalTechCost_urb_ind", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" )
write_mi_data( L231.RegionalTechCalValue_urb_ind, "StubTechCalInputIndUrb", "EMISSIONS_LEVEL2_DATA", "L231.RegionalTechCalValue_urb_ind", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" )
write_mi_data( L231.IndCoef, "StubTechCoefIndUrb", "EMISSIONS_LEVEL2_DATA", "L231.IndCoef", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" )

insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml", "EMISSIONS_XML_FINAL", "ind_urb_processing_sectors.xml", "", xml_tag="outFile" )

logstop()
