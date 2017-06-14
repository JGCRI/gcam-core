# xml.R

#' create_xml
#'
#' The basis to define how to convert data to an XML file.  This method
#' simple requires the name to save the XML file as and optionally the
#' model interface "header" file that defines the transformation lookup
#' to go from tabular data to hierarchical.  The result of this should be
#' used in a dplyr pipeline with one or more calls to \code{\link{add_xml_data}}
#' to add the data to convert and finally ending with \code{\link{run_xml_conversion}}
#' to run the conversion.
#'
#' @param xml_file The name to save the XML file to
#' @param mi_header The model interface "header".  This will default to the one
#' included in this package
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @export
create_xml <- function(xml_file, mi_header = NULL) {
  if(is.null(mi_header)) {
    mi_header <- system.file("extdata/mi_headers", "ModelInterface_headers.txt",
                             package = "gcamdata")
  }

  xml_obj <- list(xml_file = xml_file,
                  mi_header = mi_header,
                  data_tables = list())
  xml_obj <- add_flags(xml_obj, FLAG_XML)

  xml_obj
}

#' add_xml_data
#'
#' Add a table to include for conversion to XML.  We need the tibble to convert
#' and a header tag which can be looked up in the header file to convert the
#' tibble.  This method is meant to be included in a pipeline between calls of
#' \code{\link{create_xml}} and \code{\link{run_xml_conversion}}.
#'
#' @param dot The current state of the pipeline started from \code{create_xml}
#' @param data The tibble of data to add to the conversion
#' @param header The header tag to can be looked up in the header file to
#' convert \code{data}
#' @param column_order_lookup A tag that can be used to look up \code{LEVEL2_DATA_NAMES}
#' to reorder the columns of data before XML conversion to ensure the correspond
#' with the ModelInterface header.  Note by default the \code{header} is used and if
#' given \code{NULL} no column reordering will be done.
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @author PP March 2017
add_xml_data <- function(dot, data, header, column_order_lookup = header) {
  # Users can skip column reordering by passing NULL as the argument value.
  if(!is.null(column_order_lookup)) {
    data <- data[, LEVEL2_DATA_NAMES[[column_order_lookup]] ]
  }

  curr_table <- list(data = data, header = header)
  dot$data_tables[[length(dot$data_tables)+1]] <- curr_table

  dot
}

# Note: we have put the definition of run_xml_conversion inside of the "closure"
# make_run_xml_conversion so that we may stash the XML_WARNING_GIVEN flag in an
# environment that can only be reached by run_xml_conversion.
make_run_xml_conversion <- function() {
  XML_WARNING_GIVEN <- FALSE
  function(dot) {
    use_java <- getOption("gcamdata.use_java")
    if(!isTRUE(use_java) && !XML_WARNING_GIVEN) {
      message("Skipping XML conversion as global option gcamdata.use_java is not TRUE")
      # set the flag to avoid repeating the warning.
      XML_WARNING_GIVEN <<- TRUE
    } else if(isTRUE(use_java)) {
      java_cp <- system.file("extdata/ModelInterface", "CSVToXML.jar",
                             package = "gcamdata")
      cmd <- c(
        "java",
        "-cp", java_cp,
        "-Xmx1g", # TODO: memory limits?
        "ModelInterface.ModelGUI2.csvconv.CSVToXMLMain",
        "-", # Read from STDIN
        dot$mi_header,
        dot$xml_file
      )
      conv_pipe <- pipe(paste(cmd, collapse=" "), open = "w")
      on.exit(close(conv_pipe))

      for(i in seq_along(dot$data_tables)) {
        table <- dot$data_tables[[i]]
        cat("INPUT_TABLE", file = conv_pipe, sep = "\n")
        cat("Variable ID", file = conv_pipe, sep = "\n")
        cat(table$header, file = conv_pipe, sep = "\n")
        cat("", file = conv_pipe, sep = "\n")
        utils::write.table(table$data, file = conv_pipe, sep=",", row.names = FALSE, col.names = TRUE, quote = FALSE)
        cat("", file = conv_pipe, sep = "\n")
      }
    }

    dot
  }
}

#' run_xml_conversion
#'
#' Run the CSV to XML conversion using the model interface tool.  This method
#' should be the final call in a pipeline started with \code{\link{create_xml}}
#' and one or more calls to \code{\link{add_xml_data}}.
#'
#' Not that this method relies on Java to run the conversion.  To avoid errors for
#' users who do not have Java installed it will check the global option
#' \code{gcamdata.use_java} before attempting to run the conversion.  If the flag
#' is set to \code{FALSE} a warning will be issued and the conversion skipped.
#' To enable the use of Java a user can set \code{options(gcamdata.use_java=TRUE)}
#'
#' @param dot The current state of the pipeline started from \code{create_xml}
#' @return The argument passed in unmodified in case a user wanted run the
#' conversion again at a later time.
#' @author PP March 2017
run_xml_conversion <- make_run_xml_conversion()

# Note: the methods below explicitly name XML tags as expected by GCAM and/or
# the model interface headers thus will need to be maintained to be consistent.

#' add_rename_landnode_xml
#'
#' Add a table to an XML pipeline that instructs the ModelInterface to rename
#' LandNodeX to LandNode.  Such a table is necessary to help work around
#' limitations in the XML processing that node names of the same name can not be
#' nested with in each other: LandNode/LandNode thus instead we say
#' LandNode1/LandNode2 and rename as the last step.  Therefore in most cases a
#' user should add this table near the end of the XML pipeline.
#' @param dot The current state of the pipeline started from \code{create_xml}
#' @return A "data structure" to hold the various parts needed to run the model
#' interface CSV to XML conversion.
#' @author Pralit Patel
add_rename_landnode_xml <- function(dot) {
  land_name_table <- tibble(from=paste0("LandNode", seq(1,5)),to="LandNode")

  add_xml_data(dot, land_name_table, "NodeRename", NULL)
}

#' generate_level2_data_names
#'
#' Generate the list of "level 2 data names" or really a list of column orderings
#' keyed by the ModelInterface header so that we can ensure tables being sent to be
#' converted to XML by the ModelInterface have their columns arranged in the order
#' the ModelInterface is expecting them.
#' @return A list object where [[header]] <- character vector of column names
#' corresponding to header.
#' @author Pralit Patel
generate_level2_data_names <- function() {
  #Column names of tables that are read in to the model interface
  #These correspond to the headers of the same name in the headers/ModelInterface_headers.txt file
  level2_data_names <- list()

  # Socioeconomics
  level2_data_names[["Pop"]] <- c( "region", "year", "totalPop" )

  #Resources
  level2_data_names[["DepRsrc"]] <- c( "region", "depresource", "output.unit", "price.unit", "market" )
  level2_data_names[["RenewRsrc"]] <- c( "region", "renewresource", "output.unit", "price.unit", "market" )
  level2_data_names[["UnlimitRsrc"]] <- c( "region", "unlimited.resource", "output.unit", "price.unit", "market", "capacity.factor" )
  level2_data_names[["DepRsrcPrice"]] <- c( "region", "depresource", "year", "price" )
  level2_data_names[["RenewRsrcPrice"]] <- c( "region", "renewresource", "year", "price" )
  level2_data_names[["UnlimitRsrcPrice"]] <- c( "region", "unlimited.resource", "year", "price" )

  #Subresources
  level2_data_names[["SubDepRsrc"]] <- c( "region", "depresource", "subresource" )
  level2_data_names[["SubRenewRsrc"]] <- c( "region", "renewresource", "sub.renewable.resource" )
  level2_data_names[["SmthRenewRsrc"]] <- c( "region", "renewresource", "smooth.renewable.subresource" )
  level2_data_names[["DepRsrcCalProd"]] <- c( level2_data_names[["SubDepRsrc"]], "year", "cal.production" )
  level2_data_names[["RenewRsrcCalProd"]] <- c( level2_data_names[["SubRenewRsrc"]], "year", "cal.production" )
  level2_data_names[["maxSubResource"]] <- c( level2_data_names[["SubRenewRsrc"]], "year.fillout", "maxSubResource" ) #only applicable for renewable resources
  level2_data_names[["DepRsrcCurves"]] <- c( level2_data_names[["SubDepRsrc"]], "grade", "available", "extractioncost" )
  level2_data_names[["RenewRsrcCurves"]] <- c( level2_data_names[["SubRenewRsrc"]], "grade", "available", "extractioncost" )
  level2_data_names[["SmthRenewRsrcCurves"]] <- c( level2_data_names[["SmthRenewRsrc"]], "year.fillout", "maxSubResource", "mid.price", "curve.exponent" )
  level2_data_names[["DepRsrcTechChange"]] <- c( level2_data_names[["SubDepRsrc"]], "year.fillout", "techChange" )
  level2_data_names[["RenewRsrcTechChange"]] <- c( level2_data_names[["SubRenewRsrc"]], "year.fillout", "techChange" )
  level2_data_names[["SmthRenewRsrcTechChange"]] <- c( level2_data_names[["SmthRenewRsrc"]], "year.fillout", "techChange" )

  #Supplysectors
  level2_data_names[["Supplysector"]] <- c( "region", "supplysector", "output.unit", "input.unit", "price.unit", "logit.year.fillout", "logit.exponent" )
  level2_data_names[["FinalEnergyKeyword"]] <- c( "region", "supplysector", "final.energy" )
  level2_data_names[["SupplysectorPrice"]] <- c( "region", "supplysector", "year", "price" )
  level2_data_names[["SupplysectorLogitType"]] <- c( "region", "supplysector", "logit.type" )
  level2_data_names[["PassThroughSector"]] <- c( "region", "pass.through.sector", "marginal.revenue.sector", "marginal.revenue.market" )

  #Subsectors
  level2_data_names[["Subsector"]] <- c( "region", "supplysector", "subsector")
  level2_data_names[["SubsectorAll"]] <- c( level2_data_names[["Subsector"]], "logit.year.fillout", "logit.exponent", "year.fillout", "share.weight", "apply.to","from.year", "to.year", "interpolation.function" )
  level2_data_names[["SubsectorLogit"]] <- c( level2_data_names[["Subsector"]], "logit.year.fillout", "logit.exponent" )
  level2_data_names[["SubsectorLogitType"]] <- c( level2_data_names[["Subsector"]], "logit.type" )
  level2_data_names[["SubsectorShrwt"]] <- c( level2_data_names[["Subsector"]], "year", "share.weight" )
  level2_data_names[["SubsectorShrwtFllt"]] <- c( level2_data_names[["Subsector"]], "year.fillout", "share.weight" )
  level2_data_names[["SubsectorInterp"]] <- c( level2_data_names[["Subsector"]], "apply.to","from.year", "to.year", "interpolation.function" )
  level2_data_names[["SubsectorInterpTo"]] <- c( level2_data_names[["Subsector"]], "apply.to","from.year", "to.year", "to.value", "interpolation.function" )
  level2_data_names[["FuelPrefElasticity"]] <- c( level2_data_names[["Subsector"]], "year.fillout", "fuelprefElasticity" )

  #Technologies
  level2_data_names[["Tech"]] <- c( "region", "supplysector", "subsector", "technology" )
  level2_data_names[["TechInterp"]] <- c( level2_data_names[["Tech"]], "apply.to", "from.year", "to.year", "interpolation.function" )
  level2_data_names[["TechInterpTo"]] <- c( level2_data_names[["Tech"]], "apply.to","from.year", "to.year", "to.value", "interpolation.function" )
  level2_data_names[["TechYr"]] <- c( level2_data_names[["Tech"]], "year" )
  level2_data_names[["Production"]] <- c( level2_data_names[["TechYr"]], "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight" )
  level2_data_names[["TechShrwt"]] <- c( level2_data_names[["TechYr"]], "share.weight" )
  level2_data_names[["CalInput"]] <- c( level2_data_names[["TechYr"]], "minicam.energy.input", "calibrated.value", "share.weight.year", "subs.share.weight", "tech.share.weight" )
  #Market name is specified for efficiencies or coefficients
  level2_data_names[["TechCoef"]] <- c( level2_data_names[["TechYr"]], "minicam.energy.input", "coefficient", "market.name" )
  level2_data_names[["TechEff"]] <- c( level2_data_names[["TechYr"]], "minicam.energy.input", "efficiency", "market.name" )
  level2_data_names[["TechCost"]] <- c( level2_data_names[["TechYr"]], "minicam.non.energy.input", "input.cost" )
  level2_data_names[["CarbonCapture"]] <- c( level2_data_names[["TechYr"]], "storage.market", "remove.fraction" )
  level2_data_names[["ImportTech"]] <- c( level2_data_names[["TechYr"]] ) #indicates a renewable input; nothing is specified in the table however
  level2_data_names[["TechCapital"]] <- c( level2_data_names[["TechYr"]], "input.capital", "capital.overnight", "fixed.charge.rate", "capacity.factor" )
  level2_data_names[["TechOMfixed"]] <- c( level2_data_names[["TechYr"]], "input.OM.fixed", "OM.fixed", "capacity.factor" )
  level2_data_names[["TechOMvar"]] <- c( level2_data_names[["TechYr"]], "input.OM.var", "OM.var" )
  level2_data_names[["TechSCurve"]] <- c( level2_data_names[["TechYr"]], "lifetime", "steepness", "half.life" )

  #Global technologies
  level2_data_names[["GlobalTech"]] <- c( "sector.name", "subsector.name", "technology" )
  level2_data_names[["GlobalTechInterp"]] <- c( "sector.name", "subsector.name", "technology", "apply.to","from.year", "to.year", "interpolation.function" )
  level2_data_names[["GlobalTechInterpTo"]] <- c( "sector.name", "subsector.name", "technology", "apply.to","from.year", "to.year", "to.value", "interpolation.function" )
  level2_data_names[["GlobalTechYr"]] <- c( level2_data_names[["GlobalTech"]], "year" )
  level2_data_names[["GlobalTechInput"]] <- c( level2_data_names[["GlobalTechYr"]], "minicam.energy.input" )
  level2_data_names[["GlobalTechCoef"]] <- c( level2_data_names[["GlobalTechYr"]], "minicam.energy.input", "coefficient" )
  level2_data_names[["GlobalTechEff"]] <- c( level2_data_names[["GlobalTechYr"]], "minicam.energy.input", "efficiency" )
  level2_data_names[["GlobalTechCost"]] <- c( level2_data_names[["GlobalTechYr"]], "minicam.non.energy.input", "input.cost" )
  level2_data_names[["GlobalTechCapital"]] <- c( level2_data_names[["GlobalTechYr"]], "input.capital", "capital.overnight", "fixed.charge.rate", "capacity.factor" )
  level2_data_names[["GlobalTechOMfixed"]] <- c( level2_data_names[["GlobalTechYr"]], "input.OM.fixed", "OM.fixed", "capacity.factor" )
  level2_data_names[["GlobalTechOMvar"]] <- c( level2_data_names[["GlobalTechYr"]], "input.OM.var", "OM.var" )
  level2_data_names[["GlobalTechBackup"]] <- c( level2_data_names[["GlobalTechYr"]], "electric.sector.name", "trial.market.name", "backup.capital.cost",
                                                "backup.capacity.factor", "capacity.limit", "minicam.energy.input", "minicam.non.energy.input", "flag" )
  level2_data_names[["GlobalCarbonCapture"]] <- c( level2_data_names[["GlobalTechYr"]], "storage.market", "remove.fraction" )
  level2_data_names[["GlobalRenewTech"]] <- c( level2_data_names[["GlobalTechYr"]], "renewable.input" )
  level2_data_names[["GlobalTechSecOut"]] <- c( level2_data_names[["GlobalTechYr"]], "secondary.output", "output.ratio" )
  level2_data_names[["GlobalTechCSeq"]] <- c( level2_data_names[["GlobalTechYr"]], "remove.fraction", "target.gas" )
  level2_data_names[["GlobalTechShutdown"]] <- c( level2_data_names[["GlobalTechYr"]], "lifetime", "shutdown.rate" )
  level2_data_names[["GlobalTechSCurve"]] <- c( level2_data_names[["GlobalTechYr"]], "lifetime", "steepness", "half.life" )

  #Stub technologies
  level2_data_names[["StubTech"]] <- c( "region", "supplysector", "subsector", "stub.technology" )
  level2_data_names[["StubTechInterp"]] <- c( level2_data_names[["StubTech"]], "apply.to", "from.year", "to.year", "interpolation.function" )
  level2_data_names[["StubTechYr"]] <- c( level2_data_names[["StubTech"]], "year" )
  level2_data_names[["StubTechProd"]] <- c( level2_data_names[["StubTechYr"]], "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight" )
  level2_data_names[["StubTechCoef"]] <- c( level2_data_names[["StubTechYr"]], "minicam.energy.input", "coefficient", "market.name" )
  level2_data_names[["StubTechCoef_NM"]] <- c( level2_data_names[["StubTechYr"]], "minicam.energy.input", "coefficient" )
  level2_data_names[["StubTechEff"]] <- c( level2_data_names[["StubTechYr"]], "minicam.energy.input", "efficiency", "market.name" )
  level2_data_names[["StubTechCalInput"]] <- c( level2_data_names[["StubTechYr"]], "minicam.energy.input", "calibrated.value", "share.weight.year", "subs.share.weight", "tech.share.weight" )
  level2_data_names[["StubTechCalorieContent"]] <- c( level2_data_names[["StubTechEff"]] )
  level2_data_names[["StubTechCapital"]] <- c( level2_data_names[["StubTechYr"]], "input.capital", "capital.overnight", "fixed.charge.rate", "capacity.factor" )
  level2_data_names[["StubTechCost"]] <- c( level2_data_names[["StubTechYr"]], "minicam.non.energy.input", "input.cost" )
  level2_data_names[["StubTechFixOut"]] <- c( level2_data_names[["StubTechYr"]], "fixedOutput", "share.weight.year", "subs.share.weight", "tech.share.weight" )
  level2_data_names[["StubTechCapFactor"]] <- c( level2_data_names[["StubTechYr"]], "input.capital", "capacity.factor.capital", "input.OM.fixed", "capacity.factor.OM" )
  level2_data_names[["StubTechMarket"]] <- c( level2_data_names[["StubTechYr"]], "minicam.energy.input", "market.name" )
  level2_data_names[["StubTechFractSecOut"]] <- c( level2_data_names[["StubTechYr"]], "fractional.secondary.output", "output.ratio" )
  level2_data_names[["StubTechNonCO2"]] <- c( level2_data_names[["StubTechYr"]], "Non.CO2" )

  #Agricultural sectors, subsectors, and technologies
  level2_data_names[["AgSupplySector"]] <- c( "region", "AgSupplySector", "output.unit", "input.unit", "price.unit", "calPrice", "market", "logit.year.fillout", "logit.exponent" )
  level2_data_names[["AgSupplySectorLogitType"]] <- c( "region", "AgSupplySector", "logit.type" )
  level2_data_names[["AgSupplySubsector"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "logit.year.fillout", "logit.exponent" )
  level2_data_names[["AgSupplySubsectorLogitType"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "logit.type" )
  level2_data_names[["AgSupplySubsectorAll"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "logit.year.fillout", "logit.exponent", "year.fillout", "share.weight" )
  level2_data_names[["AgTech"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
  level2_data_names[["AgTechInterp"]] <- c( level2_data_names[["AgTech"]], "apply.to", "from.year", "to.year", "interpolation.function" )
  level2_data_names[["AgTechYr"]] <- c( level2_data_names[["AgTech"]], "year" )
  level2_data_names[["AgTechShrwt"]] <- c( level2_data_names[["AgTechYr"]], "share.weight" )
  level2_data_names[["AgProduction"]] <- c( level2_data_names[["AgTechYr"]], "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight" )
  level2_data_names[["AgHAtoCL"]] <- c( level2_data_names[["AgTechYr"]], "harvests.per.year" )
  level2_data_names[["AgYield"]] <- c( level2_data_names[["AgTechYr"]], "yield" )
  level2_data_names[["AgCoef"]] <- c( level2_data_names[["AgTechYr"]], "minicam.energy.input", "coefficient", "market.name" )
  level2_data_names[["AgProdChange"]] <- c( level2_data_names[["AgTechYr"]], "AgProdChange" )
  level2_data_names[["AgCost"]] <- c( level2_data_names[["AgTechYr"]], "nonLandVariableCost" )
  level2_data_names[["AgCoef"]] <- c( level2_data_names[["AgTechYr"]], "minicam.energy.input", "coefficient" )
  level2_data_names[["AgRES"]] <- c( level2_data_names[["AgTechYr"]], "res.secondary.output", "output.ratio" )
  level2_data_names[["AgConstraint"]] <- c( level2_data_names[["AgTechYr"]], "input.tax", "coefficient" )
  level2_data_names[["UnmgdTech"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology" )
  level2_data_names[["AgResBio"]] <- c( level2_data_names[["AgTechYr"]], "residue.biomass.production", "mass.conversion", "harvest.index", "eros.ctrl", "mass.to.energy", "water.content" )

  #Demands
  level2_data_names[["EnergyFinalDemand"]] <- c( "region", "energy.final.demand" )
  level2_data_names[["PerCapitaBased"]] <- c( level2_data_names[["EnergyFinalDemand"]], "perCapitaBased" )
  level2_data_names[["BaseService"]] <- c( level2_data_names[["EnergyFinalDemand"]], "year", "base.service" )
  level2_data_names[["PriceElasticity"]] <- c( level2_data_names[["EnergyFinalDemand"]], "year", "price.elasticity" )
  level2_data_names[["IncomeElasticity"]] <- c( level2_data_names[["EnergyFinalDemand"]], "year", "income.elasticity" )
  level2_data_names[["aeei"]] <- c( "region", "energy.final.demand", "year", "aeei" )

  #Land types
  level2_data_names[["LN0_Land"]] <- c( "region", "LandAllocatorRoot", "year.fillout", "landAllocation" )
  level2_data_names[["LN0_Logit"]] <- c( "region", "LandAllocatorRoot", "logit.year.fillout", "logit.exponent" )
  level2_data_names[["LN0_LogitType"]] <- c( "region", "LandAllocatorRoot", "logit.type" )
  level2_data_names[["LN0_SoilTimeScale"]] <- c( "region", "LandAllocatorRoot", "soilTimeScale" )
  level2_data_names[["LN1_ValueLogit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "unManagedLandValue", "logit.year.fillout", "logit.exponent" )
  level2_data_names[["LN1_LogitType"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "logit.type" )
  level2_data_names[["LN1_HistUnmgdAllocation"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "UnmanagedLandLeaf", "year", "allocation" )
  level2_data_names[["LN1_UnmgdAllocation"]] <- level2_data_names[["LN1_HistUnmgdAllocation"]]
  level2_data_names[["LN1_UnmgdCarbon"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                               "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
  level2_data_names[["LN2_Logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "logit.year.fillout", "logit.exponent" )
  level2_data_names[["LN2_LogitType"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "logit.type" )
  level2_data_names[["LN2_HistUnmgdAllocation"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "UnmanagedLandLeaf", "year", "allocation" )
  level2_data_names[["LN2_UnmgdAllocation"]] <- level2_data_names[["LN2_HistUnmgdAllocation"]]
  level2_data_names[["LN2_HistMgdAllocation"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandLeaf", "year", "allocation" )
  level2_data_names[["LN2_MgdAllocation"]] <- level2_data_names[["LN2_HistMgdAllocation"]]
  level2_data_names[["LN2_UnmgdCarbon"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                               "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
  level2_data_names[["LN2_MgdCarbon"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                             "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
  level2_data_names[["LN3_Logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "logit.year.fillout", "logit.exponent" )
  level2_data_names[["LN3_LogitType"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "logit.type" )
  level2_data_names[["LN3_NodeGhostShare"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "year", "ghost.unnormalized.share" )
  level2_data_names[["LN3_LeafGhostShare"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "year", "ghost.unnormalized.share" )
  level2_data_names[["LN3_LeafIsGhostShareRel"]]<- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "is.ghost.share.relative" )
  level2_data_names[["LN3_HistUnmgdAllocation"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "UnmanagedLandLeaf", "year", "allocation" )
  level2_data_names[["LN3_UnmgdAllocation"]] <- level2_data_names[["LN3_HistUnmgdAllocation"]]
  level2_data_names[["LN3_HistMgdAllocation"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "year", "allocation" )
  level2_data_names[["LN3_MgdAllocation"]] <- level2_data_names[["LN3_HistMgdAllocation"]]
  level2_data_names[["LN3_UnmgdCarbon"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                               "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
  level2_data_names[["LN3_MgdCarbon"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                             "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
  level2_data_names[["LN3_NewTech"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "year.fillout", "isNewTechnology" )
  level2_data_names[["LN4_Logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "logit.year.fillout", "logit.exponent" )
  level2_data_names[["LN4_LogitType"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "logit.type" )
  level2_data_names[["LN4_HistUnmgdAllocation"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "UnmanagedLandLeaf", "year", "allocation" )
  level2_data_names[["LN4_UnmgdAllocation"]] <- level2_data_names[["LN4_HistUnmgdAllocation"]]
  level2_data_names[["LN4_HistMgdAllocation"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandLeaf", "year", "allocation" )
  level2_data_names[["LN4_MgdAllocation"]] <- level2_data_names[["LN4_HistMgdAllocation"]]
  level2_data_names[["LN4_UnmgdCarbon"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                               "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
  level2_data_names[["LN4_MgdCarbon"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                             "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
  level2_data_names[["LN4_LeafGhostShare"]]<- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandLeaf", "year", "ghost.unnormalized.share" )
  level2_data_names[["LN4_NodeGhostShare"]]<- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "year", "ghost.unnormalized.share" )
  level2_data_names[["LN4_NodeIsGhostShareRel"]]<- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "is.ghost.share.relative" )

  level2_data_names[["LN5_Logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "logit.year.fillout", "logit.exponent" )
  level2_data_names[["LN5_LogitType"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "logit.type" )
  level2_data_names[["LN5_HistUnmgdAllocation"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "UnmanagedLandLeaf", "year", "allocation" )
  level2_data_names[["LN5_UnmgdAllocation"]] <- level2_data_names[["LN5_HistUnmgdAllocation"]]
  level2_data_names[["LN5_HistMgdAllocation"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "LandLeaf", "year", "allocation" )
  level2_data_names[["LN5_MgdAllocation"]] <- level2_data_names[["LN5_HistMgdAllocation"]]
  level2_data_names[["LN5_UnmgdCarbon"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "UnmanagedLandLeaf", "hist.veg.carbon.density",
                                               "hist.soil.carbon.density", "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
  level2_data_names[["LN5_MgdCarbon"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                             "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
  level2_data_names[["LN5_LeafGhostShare"]]<- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "LandLeaf", "year", "ghost.unnormalized.share" )

  level2_data_names[["Pop"]] <- c( "region", "year", "totalPop" )
  level2_data_names[["GlobalIntTechCoef"]] <- c( "sector.name", "subsector.name", "technology", "year", "minicam.energy.input", "coefficient" )
  level2_data_names[["GDPCtrlMaxRes"]] <- c( "region", "depresource", "Non.CO2", "ctrl.name", "max.reduction" )
  level2_data_names[["GenericBaseService"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input", "year", "base.service" )
  level2_data_names[["HDDCDD"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "year", "degree.days" )
  level2_data_names[["ResEmissCoef"]] <- c( "region", "depresource", "Non.CO2", "emiss.coef" )
  level2_data_names[["GlobalIntTechCapital"]] <- c( "sector.name", "subsector.name", "intermittent.technology", "year", "input.capital", "capital.overnight", "fixed.charge.rate", "capacity.factor" )
  level2_data_names[["LN1_Delete"]] <- c( "region", "LandAllocatorRoot", "LandNode1" )
  level2_data_names[["InputEmissCoeff"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "emiss.coef" )
  level2_data_names[["StubTranTechCoef"]] <- c( "region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.energy.input", "coefficient", "market.name" )
  level2_data_names[["LN2_Logit_absolute-cost-logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "logit.type" )
  level2_data_names[["GlobalTranTechSCurve"]] <- c( "sector.name", "subsector.name", "tranTechnology", "year", "lifetime", "steepness", "half.life" )
  level2_data_names[["LN3_NoEmissCarbon"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "no.emiss.carbon.calc", "extra" )
  level2_data_names[["ResBio"]] <- c( level2_data_names[["TechYr"]], "residue.biomass.production", "mass.conversion", "harvest.index", "eros.ctrl", "mass.to.energy", "water.content" )
  level2_data_names[["AvgFossilEffKeyword"]] <- c( "sector.name", "subsector.name", "technology", "year", "average.fossil.efficiency" )
  level2_data_names[["GlobalTranTechShrwt"]] <- c( "sector.name", "subsector.name", "tranTechnology", "year", "share.weight" )
  level2_data_names[["MAC"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "mac.control", "tax", "mac.reduction", "EPA_region" )
  level2_data_names[["StubTranTechProd"]] <- c( "region", "supplysector", "tranSubsector", "stub.technology", "year", "calOutputValue" )
  level2_data_names[["DemandFunction_flsp"]] <- c( "region", "gcam.consumer", "nodeInput", "prodDmdFnType" )
  level2_data_names[["LN3_NodeCarbon"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "node.carbon.calc", "extra" )
  level2_data_names[["RenewRsrcMkt"]] <- c( "region", "renewresource", "market" )
  level2_data_names[["SubsectorLogit_absolute-cost-logit"]] <- c( "region", "supplysector", "subsector", "logit.type" )
  level2_data_names[["GlobalTranTechInterp"]] <- c( "sector.name", "subsector.name", "tranTechnology", "apply.to", "from.year", "to.year", "interpolation.function" )
  level2_data_names[["AgResBioCurve"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "residue.biomass.production", "price", "fract.harvested" )
  level2_data_names[["StubTechSecOut"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "secondary.output.name", "secondary.output" )
  level2_data_names[["PolicyPortfolioStd"]] <- c( "region", "policy.portfolio.standard", "market", "policyType", "year", "constraint" )
  level2_data_names[["AgSupplySubsector_absolute-cost-logit"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "logit.type" )
  level2_data_names[["GlobalTechShrwt"]] <- c( "sector.name", "subsector.name", "technology", "year", "share.weight" )
  level2_data_names[["tranSubsector_absolute-cost-logit"]] <- c( "region", "supplysector", "tranSubsector", "logit.type" )
  level2_data_names[["InputEmissionsUnmgd"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology", "year", "Non.CO2", "input.emissions" )
  level2_data_names[["LN0_Logit_relative-cost-logit"]] <- c( "region", "LandAllocatorRoot", "logit.type" )
  level2_data_names[["DeleteSubsector"]] <- level2_data_names[["Subsector"]]
  level2_data_names[["StubCalorieContent"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input", "efficiency", "market.name" )
  level2_data_names[["StubTechSecPmult"]] <- c( level2_data_names[["StubTechYr"]], "secondary.output", "pMultiplier" )
  level2_data_names[["ItemName"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology", "year", "itemName" )
  level2_data_names[["GlobalIntTechProfitShutdown"]] <- c( "sector.name", "subsector.name", "intermittent.technology", "year", "median.shutdown.point", "steepness" )
  level2_data_names[["MAC_NC"]] <- level2_data_names[["MAC"]]
  level2_data_names[["DepRsrcEnvironCost"]] <- c( "region", "depresource", "subresource", "year.fillout", "environCost" )
  level2_data_names[["PrimaryRenewKeyword"]] <- c( "sector.name", "subsector.name", "technology", "year", "primary.renewable" )
  level2_data_names[["Satiation_flsp"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "satiation.level" )
  level2_data_names[["StubTranTechCost"]] <- c( "region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.non.energy.input", "input.cost" )
  level2_data_names[["ElecReserve"]] <- c( "region", "supplysector", "electricity.reserve.margin", "average.grid.capacity.factor" )
  level2_data_names[["PrimaryRenewKeywordInt"]] <- c( "sector.name", "subsector.name", "technology", "year", "primary.renewable" )
  level2_data_names[["OutputEmFactUnmgd"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology", "year", "Non.CO2", "emiss.coef" )
  level2_data_names[["AgGDPCtrlSteep"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "ctrl.name", "steepness" )
  level2_data_names[["ResBioCurve"]] <- c( level2_data_names[["TechYr"]], "residue.biomass.production", "price", "fract.harvested" )
  level2_data_names[["Intgains_scalar"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "internal.gains.scalar" )
  level2_data_names[["GrdRenewRsrcMax"]] <- c( "region", "renewresource", "sub.renewable.resource", "year.fillout", "maxSubResource" )
  level2_data_names[["DeleteThermalService"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "supplysector" )
  level2_data_names[["LN5_Logit_absolute-cost-logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "logit.type" )
  level2_data_names[["LN4_Logit_absolute-cost-logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "logit.type" )
  level2_data_names[["TechSecOut"]] <- c( level2_data_names[["TechYr"]], "secondary.output", "output.ratio" )
  level2_data_names[["LN3_Logit_relative-cost-logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "logit.type" )
  level2_data_names[["GlobalIntTechEff"]] <- c( "sector.name", "subsector.name", "intermittent.technology", "year", "minicam.energy.input", "efficiency", "type" )
  level2_data_names[["AgSupplySubsector_relative-cost-logit"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "logit.type" )
  level2_data_names[["PassThroughTech"]] <- c( level2_data_names[["Subsector"]], "pass.through.technology" )
  level2_data_names[["CalorieContent"]] <- c( level2_data_names[["TechYr"]], "minicam.energy.input", "efficiency" )
  level2_data_names[["LN5_Logit_relative-cost-logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "logit.type" )
  level2_data_names[["ReadInControl"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "future.emiss.coeff.name", "future.emiss.coeff.year", "emiss.coeff" )
  level2_data_names[["TechIntGainOutputRatio"]] <- c( "region", "supplysector", "subsector", "technology", "year", "internal.gains.output.ratio", "internal.gains.market.name" )
  level2_data_names[["StubTechIntGainOutputRatio"]] <- c( "region", "supplysector", "subsector", "technology", "year", "internal.gains.output.ratio", "internal.gains.market.name" )
  level2_data_names[["LN3_Logit_absolute-cost-logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "logit.type" )
  level2_data_names[["tranSubsector_relative-cost-logit"]] <- c( "region", "supplysector", "tranSubsector", "logit.type" )
  level2_data_names[["StubTechCalInputIndUrb"]] <- c( "region", "sector.name", "subsector.name", "technology", "year", "minicam.energy.input", "calibrated.value" )
  level2_data_names[["GrdRenewRsrcCurves"]] <- c( "region", "renewresource", "sub.renewable.resource", "grade", "available", "extractioncost" )
  level2_data_names[["GlobalTechProfitShutdown"]] <- c( "sector.name", "subsector.name", "technology", "year", "median.shutdown.point", "profit.shutdown.steepness" )
  level2_data_names[["PPPConvert"]] <- c( "region", "constRatio", "PPPConvert" )
  level2_data_names[["SatiationAdder"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "satiation.adder" )
  level2_data_names[["GlobalTechLifetime"]] <- c( "sector.name", "subsector.name", "technology", "year", "lifetime" )
  level2_data_names[["StubTechShrwt"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "share.weight" )
  level2_data_names[["hector"]] <- c( "hector.end.year", "emissions.switch.year", "hector.ini.file", "carbon.model.start.year" )
  level2_data_names[["DelEmCtrl"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "ctrl.name" )
  level2_data_names[["StubResBioCurve"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "residue.biomass.production", "price", "fract.harvested" )
  level2_data_names[["DeleteInput"]] <- c( "region", "supplysector", "subsector", "technology", "year", "minicam.energy.input" )
  level2_data_names[["GDPCtrlSteep"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "ctrl.name", "steepness" )
  level2_data_names[["DeleteConsumer"]] <- c( "region", "gcam.consumer" )
  level2_data_names[["tranSubsectorInterp"]] <- c( "region", "supplysector", "tranSubsector", "apply.to", "from.year", "to.year", "interpolation.function" )
  level2_data_names[["GlobalIntTechEff"]] <- c( "sector.name", "subsector.name", "intermittent.technology", "year", "lifetime", "steepness", "half.life" )
  level2_data_names[["StubTechEmissUnits"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "emissions.unit" )
  level2_data_names[["StubTranTech"]] <- c( "region", "supplysector", "tranSubsector", "stub.technology" )
  level2_data_names[["LN5_NodeGhostShare"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "year", "ghost.unnormalized.share" )
  level2_data_names[["StubTranTechLoadFactor"]] <- c( "region", "supplysector", "tranSubsector", "stub.technology", "year", "loadFactor" )
  level2_data_names[["tranSubsectorVOTT"]] <- c( "region", "supplysector", "tranSubsector", "addTimeValue", "year.fillout", "time.value.multiplier" )
  level2_data_names[["AgMACTC"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "mac.control", "tax", "mac.reduction", "tech.change" )
  level2_data_names[["SubsectorLogit_relative-cost-logit"]] <- c( "region", "supplysector", "subsector", "logit.type" )
  level2_data_names[["StubTranTechCalInput"]] <- c( "region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.energy.input", "calibrated.value", "share.weight.year", "subs.share.weight", "tech.share.weight" )
  level2_data_names[["MAGICC"]] <- c( "last.historical.year", "bc.unit.forcing", "default.emiss.file", "carbon.model.start.year" )
  level2_data_names[["StbTechOutputEmissions"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "input.emissions" )
  level2_data_names[["OutputEmissCoeffAg"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "emiss.coef" )
  level2_data_names[["ThermalServiceSatiation"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "satiation.level" )
  level2_data_names[["GDPCtrlSteepRes"]] <- c( "region", "depresource", "Non.CO2", "ctrl.name", "steepness" )
  level2_data_names[["AgSupplySector_absolute-cost-logit"]] <- c( "region", "AgSupplySector", "logit.type" )
  level2_data_names[["tranSubsectorLogit"]] <- c( "region", "supplysector", "tranSubsector", "logit.year.fillout", "logit.exponent" )
  level2_data_names[["GlobalResBio"]] <- c( "sector.name", "subsector.name", "technology", "year", "residue.biomass.production", "mass.conversion", "harvest.index", "eros.ctrl", "mass.to.energy", "water.content" )
  level2_data_names[["StubTechCoefIndUrb"]] <- c( "region", "supplysector", "subsector", "technology", "year", "minicam.energy.input", "coefficient" )
  level2_data_names[["TechLifetime"]] <- c( level2_data_names[["TechYr"]], "lifetime" )
  level2_data_names[["ModelTimeInterYears"]] <- c( "inter.year.timestep", "inter.year" )
  level2_data_names[["PrimaryConsKeyword"]] <- c( "sector.name", "subsector.name", "technology", "year", "primary.consumption" )
  level2_data_names[["DeleteFinalDemand"]] <- level2_data_names[["EnergyFinalDemand"]]
  level2_data_names[["GlobalIntTechBackup"]] <- c( "sector.name", "subsector.name", "technology", "year", "electric.sector.name", "trial.market.name", "backup.capital.cost", "backup.capacity.factor", "capacity.limit", "minicam.energy.input", "minicam.non.energy.input", "flag" )
  level2_data_names[["tranSubsectorInterpTo"]] <- c( "region", "supplysector", "tranSubsector", "apply.to", "from.year", "to.year", "to.value", "interpolation.function" )
  level2_data_names[["OutputEmissionsUnmgd"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology", "year", "Non.CO2", "input.emissions" )
  level2_data_names[["GlobalPassThroughTech"]] <- c( "sector.name", "subsector.name", "technology" )
  level2_data_names[["AgMAC"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "mac.control", "tax", "mac.reduction", "EPA_region" )
  level2_data_names[["OutputEmissionsAg"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "input.emissions" )
  level2_data_names[["DeleteSupplysector"]] <- c( "region", "supplysector" )
  level2_data_names[["ModelTime"]] <- c( "start.year.timestep", "start.year", "final.calibration.year", "end.year" )
  level2_data_names[["AgGDPCtrlMax"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "ctrl.name", "max.reduction" )
  level2_data_names[["tranSubsectorShrwt"]] <- c( "region", "supplysector", "tranSubsector", "year", "share.weight" )
  level2_data_names[["GlobalIntTechShrwt"]] <- c( "sector.name", "subsector.name", "intermittent.technology", "year", "share.weight" )
  level2_data_names[["LN2_Logit_relative-cost-logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "logit.type" )
  level2_data_names[["LaborProductivity"]] <- c( "region", "year", "laborproductivity" )
  level2_data_names[["tranSubsectorShrwtFllt"]] <- c( "region", "supplysector", "tranSubsector", "year.fillout", "share.weight" )
  level2_data_names[["DeleteGenericService"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input", "supplysector" )
  level2_data_names[["ShellConductance"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "year", "shell.conductance", "shell.year", "floor.to.surface.ratio" )
  level2_data_names[["AgMkt"]] <- c( "region", "AgSupplySector", "market" )
  level2_data_names[["AgSupplySector_relative-cost-logit"]] <- c( "region", "AgSupplySector", "logit.type" )
  level2_data_names[["InputEmissions"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "input.emissions" )
  level2_data_names[["InterestRate"]] <- c( "region", "interest.rate" )
  level2_data_names[["StubTechFractProd"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "fractional.secondary.output", "price", "fraction.produced" )
  level2_data_names[["SmthRenewRsrcCurvesGdpElast"]] <- c( "region", "renewresource", "smooth.renewable.subresource", "year.fillout", "maxSubResource", "mid.price", "curve.exponent", "gdpSupplyElast" )
  level2_data_names[["GenericServiceSatiation"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input", "satiation.level" )
  level2_data_names[["MAC_NoBelowZero"]] <- level2_data_names[["MAC"]]
  level2_data_names[["TechSecOutPMult"]] <- c( level2_data_names[["TechSecOut"]], "pMultiplier" )
  level2_data_names[["BaseGDP"]] <- c( "region", "baseGDP" )
  level2_data_names[["ThermalServiceSatiationAdder"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "satiation.adder" )
  level2_data_names[["PriceExp_IntGains"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "price.exp.year.fillout", "price.exponent", "internal.gains.market.name", "internal.gains.unit" )
  level2_data_names[["OutputEmissCoeff"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "emiss.coeff" )
  level2_data_names[["FinalDemandInfo"]] <- c( "region", "energy.final.demand", "perCapitaBased", "income.elasticity", "year", "base.service", "aeei" )
  level2_data_names[["GlobalTechIntGainOutputRatio"]] <- c( level2_data_names[["GlobalTechYr"]], "internal.gains.output.ratio", "internal.gains.market.name" )
  level2_data_names[["StubTechElecMarket"]] <- c( level2_data_names[["StubTechYr"]], "electric.sector.market" )
  level2_data_names[["InputEmFactUnmgd"]] <- c( "region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology", "year", "Non.CO2", "emiss.coef" )
  level2_data_names[["LN4_Logit_relative-cost-logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "logit.type" )
  level2_data_names[["Floorspace"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "year", "base.building.size" )
  level2_data_names[["FuelPrefElast"]] <- c( "region", "supplysector", "subsector", "year.fillout", "fuelprefElasticity" )
  level2_data_names[["tranSubsectorSpeed"]] <- c( "region", "supplysector", "tranSubsector", "year", "speed" )
  level2_data_names[["CarbonCoef"]] <- c( "region", "PrimaryFuelCO2Coef.name", "PrimaryFuelCO2Coef" )
  level2_data_names[["DemandFunction_serv"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "prodDmdFnType" )
  level2_data_names[["LaborForceFillout"]] <- c( "region", "year.fillout", "laborforce" )
  level2_data_names[["GlobalIntTechLifetime"]] <- c( "sector.name", "subsector.name", "intermittent.technology", "year", "lifetime" )
  level2_data_names[["OutputEmissions"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "input.emissions" )
  level2_data_names[["DeleteRenewRsrc"]] <- c( "region", "renewresource" )
  level2_data_names[["MACTC"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "mac.control", "tax", "mac.reduction", "tech.change" )
  level2_data_names[["TechFixOut"]] <- c( level2_data_names[["TechYr"]], "fixedOutput" )
  level2_data_names[["AgInputTax"]] <- c( level2_data_names[["AgTech"]], "input.tax", "coefficient" )
  level2_data_names[["LaborForce"]] <- c( "region", "year", "laborforce" )
  level2_data_names[["GlobalIntTechShutdown"]] <- c( "sector.name", "subsector.name", "intermittent.technology", "year", "lifetime", "shutdown.rate" )
  level2_data_names[["ThermalBaseService"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "year", "base.service" )
  level2_data_names[["DeleteUnlimitRsrc"]] <- c( "region", "unlimited.resource" )
  level2_data_names[["GlobalIntTechOMfixed"]] <- c( "sector.name", "subsector.name", "intermittent.technology", "year", "input.OM.fixed", "OM.fixed", "capacity.factor" )
  level2_data_names[["GlobalIntTechOMvar"]] <- c( "sector.name", "subsector.name", "intermittent.technology", "year", "input.OM.var", "OM.var" )
  level2_data_names[["tranSubsectorFuelPref"]] <- c( "region", "supplysector", "tranSubsector", "year.fillout", "fuelprefElasticity" )
  level2_data_names[["Supplysector_relative-cost-logit"]] <- c( "region", "supplysector", "logit.type" )
  level2_data_names[["GlobalTechCapture"]] <- c( "sector.name", "subsector.name", "technology", "year", "remove.fraction", "storage.market" )
  level2_data_names[["SectorUseTrialMarket"]] <- c( "region", "supplysector", "use.trial.market" )
  level2_data_names[["GenericServiceTechChange"]] <- c( "region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input", "year.fillout", "tech.change" )
  level2_data_names[["LN1_Logit_absolute-cost-logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "logit.type" )
  level2_data_names[["Supplysector_absolute-cost-logit"]] <- c( "region", "supplysector", "logit.type" )
  level2_data_names[["LN0_Logit_absolute-cost-logit"]] <- c( "region", "LandAllocatorRoot", "logit.type" )
  level2_data_names[["SubregionalShares"]] <- c( "region", "gcam.consumer", "pop.year.fillout", "inc.year.fillout", "subregional.population.share", "subregional.income.share" )
  level2_data_names[["ResMAC"]] <- c( "region", "depresource", "Non.CO2", "mac.control", "tax", "mac.reduction", "tech.change" )
  level2_data_names[["DeleteDepRsrc"]] <- c( "region", "depresource" )
  level2_data_names[["StubTechSecMarket"]] <- c( level2_data_names[["StubTechYr"]], "secondary.output", "market.name" )
  level2_data_names[["GDPCtrlMax"]] <- c( "region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "ctrl.name", "max.reduction" )
  level2_data_names[["DeleteStubTech"]] <- level2_data_names[["StubTech"]]
  level2_data_names[["LN1_Logit_relative-cost-logit"]] <- c( "region", "LandAllocatorRoot", "LandNode1", "logit.type" )


  level2_data_names
}

#' Aa list of column orderings keyed by the ModelInterface header so that we can
#' ensure tables being sent to be converted to XML by the ModelInterface have their
#' columns arranged in the order the ModelInterface is expecting them.
#' @author Pralit Patel
LEVEL2_DATA_NAMES <- generate_level2_data_names()
