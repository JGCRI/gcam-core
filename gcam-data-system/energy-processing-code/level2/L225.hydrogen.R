
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
logstart( "L225.hydrogen.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Hydrogen" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_ccs_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A25.sector <- readdata( "ENERGY_ASSUMPTIONS", "A25.sector" )
A25.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A25.subsector_logit" )
A25.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A25.subsector_shrwt" )
A25.globaltech_eff <- readdata( "ENERGY_ASSUMPTIONS", "A25.globaltech_eff" )
A25.globaltech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A25.globaltech_cost" )
A25.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A25.globaltech_shrwt" )
A25.globaltech_keyword <- readdata( "ENERGY_ASSUMPTIONS", "A25.globaltech_keyword" )
A25.globaltech_co2capture <- readdata( "ENERGY_ASSUMPTIONS", "A25.globaltech_co2capture" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Supplysector information
printlog( "L225.Supplysector_h2: Supply sector information for hydrogen sectors" )
L225.Supplysector_h2 <- write_to_all_regions( A25.sector, names_Supplysector )

# 2b. Subsector information
printlog( "L225.SubsectorLogit_h2: Subsector logit exponents of hydrogen sectors" )
L225.SubsectorLogit_h2 <- write_to_all_regions( A25.subsector_logit, names_SubsectorLogit )

printlog( "L225.SubsectorShrwt_h2 and L225.SubsectorShrwtFllt_h2: Subsector shareweights of hydrogen sectors" )
if( any( !is.na( A25.subsector_shrwt$year ) ) ){
	L225.SubsectorShrwt_h2 <- write_to_all_regions( A25.subsector_shrwt[ !is.na( A25.subsector_shrwt$year ), ], names_SubsectorShrwt )
	}
if( any( !is.na( A25.subsector_shrwt$year.fillout ) ) ){
	L225.SubsectorShrwtFllt_h2 <- write_to_all_regions( A25.subsector_shrwt[ !is.na( A25.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
	}

if( exists( "A25.subsector_interp" ) ){
	printlog( "L225.SubsectorInterp_h2 and L225.SubsectorInterpTo_h2: Subsector shareweight interpolation of hydrogen sectors" )
	if( any( is.na( A25.subsector_interp$to.value ) ) ){
		L225.SubsectorInterp_h2 <- write_to_all_regions( A25.subsector_interp[ is.na( A25.subsector_interp$to.value ), ], names_SubsectorInterp )
	}
	if( any( !is.na( A25.subsector_interp$to.value ) ) ){
		L225.SubsectorInterpTo_h2 <- write_to_all_regions( A25.subsector_interp[ !is.na( A25.subsector_interp$to.value ), ], names_SubsectorInterpTo )
	}	
}

# 2c. Technology information
printlog( "L225.StubTech_h2: Identification of stub technologies of hydrogen" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L225.StubTech_h2 <- write_to_all_regions( A25.globaltech_shrwt, names_Tech )
names( L225.StubTech_h2 ) <- names_StubTech

#Efficiencies of global technologies
printlog( "L225.GlobalTechEff_h2: Energy inputs and efficiencies of global technologies for hydrogen" )
L225.globaltech_eff.melt <- interpolate_and_melt( A25.globaltech_eff, c( model_base_years, model_future_years ), value.name="efficiency", digits = digits_efficiency )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L225.globaltech_eff.melt[ c( "sector.name", "subsector.name" ) ] <- L225.globaltech_eff.melt[ c( "supplysector", "subsector" ) ]
L225.GlobalTechEff_h2 <- L225.globaltech_eff.melt[ names_GlobalTechEff ]

#Costs of global technologies
printlog( "L225.GlobalTechCost_h2: Costs of global technologies for hydrogen" )
L225.globaltech_cost.melt <- interpolate_and_melt( A25.globaltech_cost, c( model_base_years, model_future_years ), value.name="input.cost", digits = digits_cost )
L225.globaltech_cost.melt[ c( "sector.name", "subsector.name" ) ] <- L225.globaltech_cost.melt[ c( "supplysector", "subsector" ) ]
L225.GlobalTechCost_h2 <- L225.globaltech_cost.melt[ names_GlobalTechCost ]

#Shareweights of global technologies
printlog( "L225.GlobalTechShrwt_h2: Shareweights of global technologies for hydrogen" )
L225.globaltech_shrwt.melt <- interpolate_and_melt( A25.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L225.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L225.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L225.GlobalTechShrwt_h2 <- L225.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]

printlog( "L225.PrimaryRenewKeyword_h2: Keywords of primary renewable electric generation technologies" )
L225.AllKeyword_h2 <- repeat_and_add_vector( A25.globaltech_keyword, Y, c( model_base_years, model_future_years ) )
L225.AllKeyword_h2[ c( "sector.name", "subsector.name" ) ] <- L225.AllKeyword_h2[ c( "supplysector", "subsector" ) ]
L225.PrimaryRenewKeyword_h2 <- L225.AllKeyword_h2[ !is.na( L225.AllKeyword_h2$primary.renewable ), c( names_GlobalTechYr, "primary.renewable" ) ]

printlog( "L225.AvgFossilEffKeyword_h2: Keywords of fossil/bio electric generation technologies" )
L225.AvgFossilEffKeyword_h2 <- L225.AllKeyword_h2[ !is.na( L225.AllKeyword_h2$average.fossil.efficiency ),
      c( names_GlobalTechYr, "average.fossil.efficiency" ) ]

printlog( "L225.GlobalTechCapture_h2: CO2 capture fractions from global fertilizer production technologies with CCS" )
## No need to consider historical periods or intermittent technologies here
L225.globaltech_co2capture.melt <- interpolate_and_melt( A25.globaltech_co2capture, model_future_years, value.name="remove.fraction", digits = digits_remove.fraction )
L225.globaltech_co2capture.melt[ c( "sector.name", "subsector.name" ) ] <- L225.globaltech_co2capture.melt[ c( "supplysector", "subsector" ) ]
L225.GlobalTechCapture_h2 <- L225.globaltech_co2capture.melt[ c( names_GlobalTechYr, "remove.fraction" ) ]
L225.GlobalTechCapture_h2$storage.market <- CO2.storage.market

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L225.Supplysector_h2, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L225.Supplysector_h2",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_hydrogen.xml" ) 
write_mi_data( L225.SubsectorLogit_h2, "SubsectorLogit", "ENERGY_LEVEL2_DATA", "L225.SubsectorLogit_h2", "ENERGY_XML_BATCH", "batch_hydrogen.xml" ) 
if( exists( "L225.SubsectorShrwt_h2" ) ){
	write_mi_data( L225.SubsectorShrwt_h2, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L225.SubsectorShrwt_h2", "ENERGY_XML_BATCH", "batch_hydrogen.xml" )
	}
if( exists( "L225.SubsectorShrwtFllt_h2" ) ){
	write_mi_data( L225.SubsectorShrwtFllt_h2, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L225.SubsectorShrwtFllt_h2",
	               "ENERGY_XML_BATCH", "batch_hydrogen.xml" ) 
	}
if( exists( "L225.SubsectorInterp_h2" ) ) {
	write_mi_data( L225.SubsectorInterp_h2, "SubsectorInterp", "ENERGY_LEVEL2_DATA", "L225.SubsectorInterp_h2", "ENERGY_XML_BATCH", "batch_hydrogen.xml" )
	}
if( exists( "L225.SubsectorInterpTo_h2" ) ) {
	write_mi_data( L225.SubsectorInterpTo_h2, "SubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L225.SubsectorInterpTo_h2", "ENERGY_XML_BATCH", "batch_hydrogen.xml" )
	}
write_mi_data( L225.StubTech_h2, "StubTech", "ENERGY_LEVEL2_DATA", "L225.StubTech_h2", "ENERGY_XML_BATCH", "batch_hydrogen.xml" )
write_mi_data( L225.GlobalTechEff_h2, "GlobalTechEff", "ENERGY_LEVEL2_DATA", "L225.GlobalTechEff_h2", "ENERGY_XML_BATCH", "batch_hydrogen.xml" )
write_mi_data( L225.GlobalTechCost_h2, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L225.GlobalTechCost_h2", "ENERGY_XML_BATCH", "batch_hydrogen.xml" )
write_mi_data( L225.GlobalTechShrwt_h2, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L225.GlobalTechShrwt_h2", "ENERGY_XML_BATCH", "batch_hydrogen.xml" )
write_mi_data( L225.PrimaryRenewKeyword_h2, "PrimaryRenewKeyword", "ENERGY_LEVEL2_DATA", "L225.PrimaryRenewKeyword", "ENERGY_XML_BATCH", "batch_hydrogen.xml" )
write_mi_data( L225.AvgFossilEffKeyword_h2, "AvgFossilEffKeyword", "ENERGY_LEVEL2_DATA", "L225.AvgFossilEffKeyword_h2", "ENERGY_XML_BATCH", "batch_hydrogen.xml" )
write_mi_data( L225.GlobalTechCapture_h2, "GlobalTechCapture", "ENERGY_LEVEL2_DATA", "L225.GlobalTechCapture_h2", "ENERGY_XML_BATCH", "batch_hydrogen.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_hydrogen.xml", "ENERGY_XML_FINAL", "hydrogen.xml", "", xml_tag="outFile" )

logstop()


