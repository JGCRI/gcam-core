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
L102.gdp_mil90usd_GCAM3_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.gdp_mil90usd_GCAM3_R_Y" )
L102.gdp_mil90usd_Scen_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.gdp_mil90usd_Scen_R_Y" )
L102.gdp_mil90usd_Scen_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.gdp_mil90usd_Scen_R_Y" )
L221.GlobalTechCoef_en <- readdata( "ENERGY_LEVEL2_DATA", "L221.GlobalTechCoef_en", skip=4 )

# Policy assumptions: TODO where to put these
NEG_EMISS_POLICY_NAME <- "negative_emiss_budget"
NEG_EMISS_GDP_BUDGET_PCT <- 0.01
NEG_EMISS_MARKT_GLOBAL <- TRUE

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
L270.CreditMkt$price.unit <- "1975$/GJ"
L270.CreditMkt$output.unit <- "EJ"

# Create the negative emissions GDP budget constraint limits

printlog( "L270.GDP: Create a usable GDP by region + scenario + year" )
L270.GDP <- L102.gdp_mil90usd_GCAM3_R_Y
L270.GDP[[Scen]] <- "GCAM3"
L270.GDP <- rbind( L270.GDP, L102.gdp_mil90usd_Scen_R_Y )
L270.GDP <- add_region_name( L270.GDP )
L270.GDP[[R]] <- NULL
L270.GDP <- melt(L270.GDP, id.vars=c("scenario", "region"), value.name="gdp", variable.name="year")
L270.GDP$year <- as.integer(sub('^X', '', L270.GDP$year))
L270.GDP <- subset(L270.GDP, year %in% model_years)

printlog( "L270.CTaxInput: Create ctax-input for all biomass" )
L270.CTaxInput <- L221.GlobalTechCoef_en
L270.CTaxInput <- subset( L270.CTaxInput, grepl('(biomass|ethanol)', sector.name) )
L270.CTaxInput$ctax.input <- NEG_EMISS_POLICY_NAME
L270.CTaxInput$fuel.name <- L270.CTaxInput$sector.name
L270.CTaxInput <- L270.CTaxInput[, c( names_GlobalTechYr, "ctax.input", "fuel.name" ) ]

printlog( "L270.NegEmissFinalDemand: Create negative emissions final demand")
L270.NegEmissFinalDemand <- data.frame( region=GCAM_region_names$region, negative.emissions.final.demand="CO2", policy.name=NEG_EMISS_POLICY_NAME )

printlog( "L270.NegEmissBudget: Create the budget for paying for net negative emissions" )
L270.NegEmissBudget <- L270.GDP
# no dollar year or unit conversions since emissions already match
L270.NegEmissBudget$constraint <- L270.NegEmissBudget$gdp * NEG_EMISS_GDP_BUDGET_PCT
L270.NegEmissBudget$policy.portfolio.standard <- NEG_EMISS_POLICY_NAME
L270.NegEmissBudget$policyType <- "tax"
L270.NegEmissBudget$market <- L270.NegEmissBudget$region
L270.NegEmissBudget$price.unit <- "%"
L270.NegEmissBudget$output.unit <- "mil 1990$"
# constrain in only years which could include a carbon price
L270.NegEmissBudget <- subset( L270.NegEmissBudget , year >= 2020 )

L270.NegEmissBudgetMaxPrice <- data.frame(region=GCAM_region_names$region,
                                          policy.portfolio.standard=NEG_EMISS_POLICY_NAME,
                                          max.price= 1.0)

# split out SSPs so that we can generate SPA policies
# NOTE: SPA policies *must* be regional no matter the value of NEG_EMISS_MARKT_GLOBAL
L270.NegEmissBudget_SPA <- subset( L270.NegEmissBudget, grepl('^SSP\\d', scenario) )
# SSP 2 and 3 share SPA
L270.NegEmissBudget_SPA <- subset( L270.NegEmissBudget_SPA, scenario != "SSP3" )
L270.NegEmissBudget_SPA[ L270.NegEmissBudget_SPA$scenario == "SSP2", "scenario" ] <- "SSP23"
L270.NegEmissBudget_SPA$scenario <- sub( 'SSP', 'spa', L270.NegEmissBudget_SPA$scenario )
# Copy Final Demand data.frame as well to ensure it is regional
L270.NegEmissFinalDemand_SPA <- L270.NegEmissFinalDemand

if( NEG_EMISS_MARKT_GLOBAL ) {
    printlog( "Adjust inputs for global market" )
    # when the negative emissions budget is global we need to aggregate
    # the constraint accross regions
    L270.NegEmissBudget.agg <- L270.NegEmissBudget[, c(Scen, Y, "constraint") ]
    L270.NegEmissBudget.agg <- aggregate( constraint ~ scenario + year, L270.NegEmissBudget.agg, FUN=sum )
    names( L270.NegEmissBudget.agg )[names( L270.NegEmissBudget.agg ) == "constraint" ] <- "g.constraint"
    L270.NegEmissBudget <- merge(L270.NegEmissBudget, L270.NegEmissBudget.agg)
    L270.NegEmissBudget$constraint <- L270.NegEmissBudget$g.constraint

    # set the market global
    L270.NegEmissBudget$market <- "global"

    # the negative emissions final demand must only be included in just one region (could be any)
    L270.NegEmissFinalDemand <- L270.NegEmissFinalDemand[1, ]
}

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L270.CreditOutput, "GlobalTechRESSecOut", "ENERGY_LEVEL2_DATA", "L270.CreditOutput", "ENERGY_XML_BATCH", "batch_liquids_limits.xml" ) 
write_mi_data( L270.CreditInput_elec, "GlobalTechCoef", "ENERGY_LEVEL2_DATA", "L270.CreditInput_elec", "ENERGY_XML_BATCH", "batch_liquids_limits.xml" ) 
write_mi_data( L270.CreditInput_feedstocks, "GlobalTechCoef", "ENERGY_LEVEL2_DATA", "L270.CreditInput_feedstocks", "ENERGY_XML_BATCH", "batch_liquids_limits.xml" ) 
write_mi_data( L270.CreditMkt, "PortfolioStd", "ENERGY_LEVEL2_DATA", "L270.CreditMkt", "ENERGY_XML_BATCH", "batch_liquids_limits.xml" ) 

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_liquids_limits.xml", "ENERGY_XML_FINAL", "liquids_limits.xml", "", xml_tag="outFile" )

for( scn in unique(L270.NegEmissBudget[[Scen]] ) ) {
    curr_batch <- paste0( "batch_negative_emissions_budget_", scn, ".xml" )
    write_mi_data( L270.CTaxInput, "GlobalTechCTaxInput", "ENERGY_LEVEL2_DATA", "L270.CTaxInput", "ENERGY_XML_BATCH", curr_batch )
    write_mi_data( L270.NegEmissFinalDemand, "NegEmissFinalDemand", "ENERGY_LEVEL2_DATA", "L270.NegEmissFinalDemand", "ENERGY_XML_BATCH", curr_batch )
    write_mi_data( L270.NegEmissBudgetMaxPrice, "PortfolioStdMaxPrice", "ENERGY_LEVEL2_DATA", "L270.NegEmissBudgetMaxPrice", "ENERGY_XML_BATCH", curr_batch )
    curr_budget <- subset(L270.NegEmissBudget, scenario == scn)
    curr_budget <- curr_budget[, c("region", "policy.portfolio.standard", "market", "policyType", "year", "constraint", "price.unit", "output.unit" ) ]
    curr_fn <- paste0( "L270.NegEmissBudget_", scn )
    write_mi_data( curr_budget, "PortfolioStd", "ENERGY_LEVEL2_DATA", curr_fn, "ENERGY_XML_BATCH", curr_batch )
    curr_xml <- paste0( "negative_emissions_budget_", scn, ".xml" )
    insert_file_into_batchxml( "ENERGY_XML_BATCH", curr_batch, "ENERGY_XML_FINAL", curr_xml, "", xml_tag="outFile" )
}

for( scn in unique(L270.NegEmissBudget_SPA[[Scen]] ) ) {
    curr_batch <- paste0( "batch_negative_emissions_budget_", scn, ".xml" )
    write_mi_data( L270.CTaxInput, "GlobalTechCTaxInput", "ENERGY_LEVEL2_DATA", "L270.CTaxInput", "ENERGY_XML_BATCH", curr_batch )
    write_mi_data( L270.NegEmissFinalDemand_SPA, "NegEmissFinalDemand", "ENERGY_LEVEL2_DATA", "L270.NegEmissFinalDemand_SPA", "ENERGY_XML_BATCH", curr_batch )
    write_mi_data( L270.NegEmissBudgetMaxPrice, "PortfolioStdMaxPrice", "ENERGY_LEVEL2_DATA", "L270.NegEmissBudgetMaxPrice", "ENERGY_XML_BATCH", curr_batch )
    curr_budget <- subset(L270.NegEmissBudget_SPA, scenario == scn)
    curr_budget <- curr_budget[, c("region", "policy.portfolio.standard", "market", "policyType", "year", "constraint", "price.unit", "output.unit" ) ]
    curr_fn <- paste0( "L270.NegEmissBudget_", scn )
    write_mi_data( curr_budget, "PortfolioStd", "ENERGY_LEVEL2_DATA", curr_fn, "ENERGY_XML_BATCH", curr_batch )
    curr_xml <- paste0( "negative_emissions_budget_", scn, ".xml" )
    insert_file_into_batchxml( "ENERGY_XML_BATCH", curr_batch, "ENERGY_XML_FINAL", curr_xml, "", xml_tag="outFile" )
}

logstop()
