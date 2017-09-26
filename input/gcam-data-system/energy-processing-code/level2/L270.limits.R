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
logstart( "L270.limits.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Constraints on bioliquids and bioenergy" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A23.globaltech_eff <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_eff" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#Limit bioliquids for feedstocks and electricity 
#Note: we do this by requiring a certain fraction of inputs to those technologies to come from oil
printlog( "L270.CreditOutput: Secondary output of oil credits" )
L270.CreditOutput <- data.frame(sector.name = "refining",
                                subsector.name = "oil refining",
                                technology = "oil refining")
L270.CreditOutput <- repeat_and_add_vector(L270.CreditOutput, "year", model_years)
L270.CreditOutput$res.secondary.output <- "oil-credits"
L270.CreditOutput$output.ratio <- 1

printlog( "L270.CreditInput_elec: minicam-energy-input of oil credits for electricity techs" )
L270.globaltech_eff.melt <- interpolate_and_melt( A23.globaltech_eff, c( model_base_years, model_future_years ), value.name="efficiency", digits = digits_efficiency, rule=3 )

L270.CreditInput_elec <- data.frame(sector.name = c("electricity", "electricity", "electricity"),
                                subsector.name = c("refined liquids", "refined liquids", "refined liquids"),
                                technology = c("refined liquids (steam/CT)", "refined liquids (CC)", "refined liquids (CC CCS)"))
L270.CreditInput_elec <- repeat_and_add_vector(L270.CreditInput_elec, "year", model_years)
L270.CreditInput_elec$minicam.energy.input <- "oil-credits"
L270.CreditInput_elec$efficiency <- L270.globaltech_eff.melt$efficiency[ match( vecpaste(L270.CreditInput_elec[c("subsector.name", "technology", "year")]),
                                                                                vecpaste(L270.globaltech_eff.melt[c("subsector", "technology", "year")]))]
L270.CreditInput_elec$coefficient <- oilFract_Elec / L270.CreditInput_elec$efficiency
L270.CreditInput_elec$efficiency <- NULL

printlog( "L270.CreditInput_feedstocks: minicam-energy-input of oil credits for feedstock techs" )
L270.CreditInput_feedstocks <- data.frame(sector.name = "industrial feedstocks",
                                    subsector.name = "refined liquids",
                                    technology = "refined liquids")
L270.CreditInput_feedstocks <- repeat_and_add_vector(L270.CreditInput_feedstocks, "year", model_years)
L270.CreditInput_feedstocks$minicam.energy.input <- "oil-credits"
L270.CreditInput_feedstocks$coefficient <- oilFract_Feedstocks

printlog( "L270.CreditMkt: Market for oil credits" )
L270.CreditMkt <- GCAM_region_names[c("region")]
L270.CreditMkt$policy.portfolio.standard <- "oil-credits"
L270.CreditMkt$market <- "global"
L270.CreditMkt$policyType <- "RES"
L270.CreditMkt <- repeat_and_add_vector(L270.CreditMkt, "year", model_future_years)
L270.CreditMkt$constraint <- 1

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L270.CreditOutput, "GlobalTechRESSecOut", "ENERGY_LEVEL2_DATA", "L270.CreditOutput", "ENERGY_XML_BATCH", "batch_liquids_limits.xml" ) 
write_mi_data( L270.CreditInput_elec, "GlobalTechCoef", "ENERGY_LEVEL2_DATA", "L270.CreditInput_elec", "ENERGY_XML_BATCH", "batch_liquids_limits.xml" ) 
write_mi_data( L270.CreditInput_feedstocks, "GlobalTechCoef", "ENERGY_LEVEL2_DATA", "L270.CreditInput_feedstocks", "ENERGY_XML_BATCH", "batch_liquids_limits.xml" ) 
write_mi_data( L270.CreditMkt, "PortfolioStd", "ENERGY_LEVEL2_DATA", "L270.CreditMkt", "ENERGY_XML_BATCH", "batch_liquids_limits.xml" ) 

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_liquids_limits.xml", "ENERGY_XML_FINAL", "liquids_limits.xml", "", xml_tag="outFile" )

logstop()
