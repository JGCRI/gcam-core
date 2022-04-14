library(usethis)
library(devtools)

# We could potentially use drake to speed up the process of updating the package
# data which otherwise requires multiple runs of driver.  However, given drake
# is optional we default to not use it.
USE_DRIVER_DRAKE <- FALSE

# Note: the methods below explicitly name XML tags as expected by GCAM and/or
# the model interface headers thus will need to be maintained to be consistent.

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
  # Column names of tables that are read in to the model interface
  # These correspond to the headers of the same name in the headers/ModelInterface_headers.txt file
  level2_data_names <- list()

  # Modeltime
  level2_data_names[["ModelTime"]] <- c("start.year.timestep", "start.year", "final.calibration.year", "end.year", "carbon.model.start.year")
  level2_data_names[["ModelTimeInterYears"]] <- c("inter.year.timestep", "inter.year", "dummy.tag")

  # Climate
  level2_data_names[["MAGICC"]] <- c("last.historical.year", "bc.unit.forcing", "default.emiss.file")
  level2_data_names[["hector"]] <- c("hector.end.year", "emissions.switch.year", "hector.ini.file")
  level2_data_names[["DeleteMAGICC"]] <- c("delete")
  level2_data_names[["DeleteHector"]] <- c("delete")
  level2_data_names[["NoClimateModel"]] <- c("no.climate.model")

  # Socioeconomics
  level2_data_names[["Pop"]] <- c("region", "year", "totalPop")
  level2_data_names[["BaseGDP"]] <- c("region", "baseGDP")
  level2_data_names[["LaborForce"]] <- c("region", "year", "laborforce")
  level2_data_names[["LaborForceFillout"]] <- c("region", "year.fillout", "laborforce")
  level2_data_names[["LaborProductivity"]] <- c("region", "year", "laborproductivity")
  level2_data_names[["PPPConvert"]] <- c("region", "constRatio", "PPPConvert")
  level2_data_names[["InterestRate"]] <- c("region", "interest.rate")

  # Carbon Coef
  level2_data_names[["CarbonCoef"]] <- c("region", "PrimaryFuelCO2Coef.name", "PrimaryFuelCO2Coef")

  # Resources
  level2_data_names[["Rsrc"]] <- c("region", "resource", "output.unit", "price.unit", "market")
  level2_data_names[["RenewRsrc"]] <- c("region", "renewresource", "output.unit", "price.unit", "market")
  level2_data_names[["UnlimitRsrc"]] <- c("region", "unlimited.resource", "output.unit", "price.unit", "market")
  level2_data_names[["RsrcPrice"]] <- c("region", "resource", "year", "price")
  level2_data_names[["RenewRsrcPrice"]] <- c("region", "renewresource", "year", "price")
  level2_data_names[["UnlimitRsrcPrice"]] <- c("region", "unlimited.resource", "year", "price")
  level2_data_names[["RenewRsrcMkt"]] <- c("region", "renewresource", "market")
  level2_data_names[["DeleteRsrc"]] <- c("region", "resource")
  level2_data_names[["DeleteRenewRsrc"]] <- c("region", "renewresource")
  level2_data_names[["DeleteUnlimitRsrc"]] <- c("region", "unlimited.resource")


  # Subresources
  level2_data_names[["SubRsrc"]] <- c("region", "resource", "subresource")
  level2_data_names[["SubRenewRsrc"]] <- c("region", "renewresource", "sub.renewable.resource")
  level2_data_names[["SmthRenewRsrc"]] <- c("region", "renewresource", "smooth.renewable.subresource")
  level2_data_names[["RsrcCalProd"]] <- c(level2_data_names[["SubRsrc"]], "year", "cal.production")
  level2_data_names[["RenewRsrcCalProd"]] <- c(level2_data_names[["SubRenewRsrc"]], "year", "cal.production")
  level2_data_names[["maxSubResource"]] <- c(level2_data_names[["SubRenewRsrc"]], "year.fillout", "maxSubResource") #only applicable for renewable resources
  level2_data_names[["RsrcCurves"]] <- c(level2_data_names[["SubRsrc"]], "grade", "available", "extractioncost")
  level2_data_names[["RenewRsrcCurves"]] <- c(level2_data_names[["SubRenewRsrc"]], "grade", "available", "extractioncost")
  level2_data_names[["SmthRenewRsrcCurves"]] <- c(level2_data_names[["SmthRenewRsrc"]], "year.fillout", "maxSubResource", "mid.price", "curve.exponent")
  level2_data_names[["GrdRenewRsrcCurves"]] <- c("region", "renewresource", "sub.renewable.resource", "grade", "available", "extractioncost")
  level2_data_names[["GrdRenewRsrcMax"]] <- c("region", "renewresource", "sub.renewable.resource", "year.fillout", "maxSubResource")
  level2_data_names[["GrdRenewRsrcMaxNoFillOut"]] <- c("region", "renewresource", "sub.renewable.resource", "year", "maxSubResource")
  level2_data_names[["RsrcTechChange"]] <- c(level2_data_names[["SubRsrc"]], "year.fillout", "techChange")
  level2_data_names[["RenewRsrcTechChange"]] <- c(level2_data_names[["SubRenewRsrc"]], "year.fillout", "techChange")
  level2_data_names[["SmthRenewRsrcTechChange"]] <- c(level2_data_names[["SmthRenewRsrc"]], "year.fillout", "techChange")
  level2_data_names[["SubresourcePriceAdder"]] <- c(level2_data_names[["SubRsrc"]], "year", "price.adder")
  level2_data_names[["SmthRenewRsrcCurvesGdpElast"]] <- c("region", "renewresource", "smooth.renewable.subresource", "year.fillout", "maxSubResource", "mid.price", "curve.exponent", "gdpSupplyElast")
  level2_data_names[["ReserveCalReserve"]] <- c("region", "resource", "reserve.subresource", "year", "cal.reserve")
  level2_data_names[["ResSubresourceProdLifetime"]] <- c("region", "resource", "reserve.subresource", "avg.prod.lifetime")

  # Supplysectors
  level2_data_names[["Supplysector"]] <- c("region", "supplysector", "output.unit", "input.unit", "price.unit", "logit.year.fillout", "logit.exponent")
  level2_data_names[["FinalEnergyKeyword"]] <- c("region", "supplysector", "final.energy")
  level2_data_names[["SupplysectorPrice"]] <- c("region", "supplysector", "year", "price")
  level2_data_names[["Supplysector_absolute-cost-logit"]] <- c("region", "supplysector")
  level2_data_names[["Supplysector_relative-cost-logit"]] <- c("region", "supplysector")
  level2_data_names[["PassThroughSector"]] <- c("region", "pass.through.sector", "marginal.revenue.sector", "marginal.revenue.market")
  level2_data_names[["ElecReserve"]] <- c("region", "supplysector", "electricity.reserve.margin", "average.grid.capacity.factor")
  level2_data_names[["SectorUseTrialMarket"]] <- c("region", "supplysector", "use.trial.market")
  level2_data_names[["DeleteSupplysector"]] <- c("region", "supplysector")

  # Subsectors
  level2_data_names[["Subsector"]] <- c("region", "supplysector", "subsector")
  level2_data_names[["SubsectorAll"]] <- c(level2_data_names[["Subsector"]], "logit.year.fillout", "logit.exponent", "year.fillout", "share.weight", "apply.to", "from.year", "to.year", "interpolation.function")
  level2_data_names[["SubsectorAllTo"]] <- c(level2_data_names[["Subsector"]], "logit.year.fillout", "logit.exponent", "year.fillout", "share.weight", "apply.to", "from.year", "to.year", "to.value", "interpolation.function")
  level2_data_names[["SubsectorLogit"]] <- c(level2_data_names[["Subsector"]], "logit.year.fillout", "logit.exponent")
  level2_data_names[["SubsectorLogit_absolute-cost-logit"]] <- c("region", "supplysector", "subsector")
  level2_data_names[["SubsectorLogit_relative-cost-logit"]] <- c("region", "supplysector", "subsector")
  level2_data_names[["SubsectorShrwt"]] <- c(level2_data_names[["Subsector"]], "year", "share.weight")
  level2_data_names[["SubsectorShrwtFllt"]] <- c(level2_data_names[["Subsector"]], "year.fillout", "share.weight")
  level2_data_names[["SubsectorInterp"]] <- c(level2_data_names[["Subsector"]], "apply.to", "from.year", "to.year", "interpolation.function")
  level2_data_names[["SubsectorInterpTo"]] <- c(level2_data_names[["Subsector"]], "apply.to", "from.year", "to.year", "to.value", "interpolation.function")
  level2_data_names[["FuelPrefElast"]] <- c(level2_data_names[["Subsector"]], "year.fillout", "fuelprefElasticity")
  level2_data_names[["tranSubsectorFuelPref"]] <- c("region", "supplysector", "tranSubsector", "year.fillout", "fuelprefElasticity")
  level2_data_names[["tranSubsectorInterp"]] <- c("region", "supplysector", "tranSubsector", "apply.to", "from.year", "to.year", "interpolation.function")
  level2_data_names[["tranSubsectorInterpTo"]] <- c("region", "supplysector", "tranSubsector", "apply.to", "from.year", "to.year", "to.value", "interpolation.function")
  level2_data_names[["tranSubsectorLogit"]] <- c("region", "supplysector", "tranSubsector", "logit.year.fillout", "logit.exponent")
  level2_data_names[["tranSubsectorShrwt"]] <- c("region", "supplysector", "tranSubsector", "year", "share.weight")
  level2_data_names[["tranSubsectorShrwtFllt"]] <- c("region", "supplysector", "tranSubsector", "year.fillout", "share.weight")
  level2_data_names[["tranSubsectorSpeed"]] <- c("region", "supplysector", "tranSubsector", "year", "speed")
  level2_data_names[["tranSubsectorVOTT"]] <- c("region", "supplysector", "tranSubsector", "addTimeValue", "year.fillout", "time.value.multiplier")
  level2_data_names[["tranSubsector_absolute-cost-logit"]] <- c("region", "supplysector", "tranSubsector")
  level2_data_names[["tranSubsector_relative-cost-logit"]] <- c("region", "supplysector", "tranSubsector")
  level2_data_names[["DeleteSubsector"]] <- level2_data_names[["Subsector"]]

  # Technologies
  level2_data_names[["Tech"]] <- c("region", "supplysector", "subsector", "technology")
  level2_data_names[["TechInterp"]] <- c(level2_data_names[["Tech"]], "apply.to", "from.year", "to.year", "interpolation.function")
  level2_data_names[["TechInterpTo"]] <- c(level2_data_names[["Tech"]], "apply.to", "from.year", "to.year", "to.value", "interpolation.function")
  level2_data_names[["TechYr"]] <- c(level2_data_names[["Tech"]], "year")
  level2_data_names[["Production"]] <- c(level2_data_names[["TechYr"]], "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight")
  level2_data_names[["TechShrwt"]] <- c(level2_data_names[["TechYr"]], "share.weight")
  level2_data_names[["CalInput"]] <- c(level2_data_names[["TechYr"]], "minicam.energy.input", "calibrated.value", "share.weight.year", "subs.share.weight", "tech.share.weight")
  # Market name is specified for efficiencies or coefficients
  level2_data_names[["TechCoef"]] <- c(level2_data_names[["TechYr"]], "minicam.energy.input", "coefficient", "market.name")
  level2_data_names[["TechEff"]] <- c(level2_data_names[["TechYr"]], "minicam.energy.input", "efficiency", "market.name")
  level2_data_names[["TechCost"]] <- c(level2_data_names[["TechYr"]], "minicam.non.energy.input", "input.cost")
  level2_data_names[["TechPmult"]] <- c(level2_data_names[["TechYr"]], "pMult")
  level2_data_names[["CarbonCapture"]] <- c(level2_data_names[["TechYr"]], "storage.market", "remove.fraction")
  level2_data_names[["ImportTech"]] <- c(level2_data_names[["TechYr"]]) #indicates a renewable input; nothing is specified in the table however
  level2_data_names[["TechCapital"]] <- c(level2_data_names[["TechYr"]], "input.capital", "capital.overnight", "fixed.charge.rate")
  level2_data_names[["TechOMfixed"]] <- c(level2_data_names[["TechYr"]], "input.OM.fixed", "OM.fixed")
  level2_data_names[["TechOMvar"]] <- c(level2_data_names[["TechYr"]], "input.OM.var", "OM.var")
  level2_data_names[["TechSCurve"]] <- c(level2_data_names[["TechYr"]], "lifetime", "steepness", "half.life")
  level2_data_names[["TechFixOut"]] <- c(level2_data_names[["TechYr"]], "fixedOutput")
  level2_data_names[["TechIntGainOutputRatio"]] <- c("region", "supplysector", "subsector", "technology", "year", "internal.gains.output.ratio", "internal.gains.market.name")
  level2_data_names[["TechLifetime"]] <- c(level2_data_names[["TechYr"]], "lifetime")
  level2_data_names[["TechSecOut"]] <- c(level2_data_names[["TechYr"]], "secondary.output", "output.ratio")
  level2_data_names[["TechSecOutPMult"]] <- c(level2_data_names[["TechSecOut"]], "pMultiplier")
  level2_data_names[["PassThroughTech"]] <- c(level2_data_names[["Subsector"]], "pass.through.technology")
  level2_data_names[["CalorieContent"]] <- c(level2_data_names[["TechYr"]], "minicam.energy.input", "efficiency")
  level2_data_names[["DeleteInput"]] <- c("region", "supplysector", "subsector", "technology", "year", "minicam.energy.input")
  level2_data_names[["ResTechShrwt"]] <- c("region", "resource", "subresource", "technology", "year", "share.weight")
  level2_data_names[["ResTechCoef"]] <- c("region", "resource", "subresource", "technology", "year", "minicam.energy.input", "coefficient")
  level2_data_names[["DeleteResTechInput"]] <- c("region", "resource", "subresource", "technology", "year", "minicam.energy.input")
  level2_data_names[["ResReserveTechCost"]] <- c("region", "resource", "reserve.subresource", "resource.reserve.technology", "year", "minicam.non.energy.input", "input.cost")
  level2_data_names[["ResReserveTechLifetime"]] <- c("region", "resource", "reserve.subresource", "resource.reserve.technology", "year", "lifetime")
  level2_data_names[["ResReserveTechProfitShutdown"]] <- c("region", "resource", "reserve.subresource", "resource.reserve.technology", "year", "median.shutdown.point", "profit.shutdown.steepness")
  level2_data_names[["ResReserveTechDeclinePhase"]] <- c("region", "resource", "reserve.subresource", "resource.reserve.technology", "year", "decline.phase.percent")
  level2_data_names[["ResReserveTechCoef"]] <- c("region", "resource", "reserve.subresource", "resource.reserve.technology", "year", "minicam.energy.input", "coefficient")
  level2_data_names[["FuelPrefElastTech"]] <- c(level2_data_names[["Tech"]], "year.fillout", "fuelprefElasticity")

  # Global technologies
  level2_data_names[["GlobalTech"]] <- c("sector.name", "subsector.name", "technology")
  level2_data_names[["GlobalTechInterp"]] <- c("sector.name", "subsector.name", "technology", "apply.to", "from.year", "to.year", "interpolation.function")
  level2_data_names[["GlobalTechInterpTo"]] <- c("sector.name", "subsector.name", "technology", "apply.to", "from.year", "to.year", "to.value", "interpolation.function")
  level2_data_names[["GlobalTechShrwt"]] <- c("sector.name", "subsector.name", "technology", "year", "share.weight")
  level2_data_names[["GlobalTechYr"]] <- c(level2_data_names[["GlobalTech"]], "year")
  level2_data_names[["GlobalTechCapFac"]] <- c(level2_data_names[["GlobalTechYr"]], "capacity.factor")
  level2_data_names[["GlobalTechInput"]] <- c(level2_data_names[["GlobalTechYr"]], "minicam.energy.input")
  level2_data_names[["GlobalTechCoef"]] <- c(level2_data_names[["GlobalTechYr"]], "minicam.energy.input", "coefficient")

  level2_data_names[["GlobalTechEff"]] <- c(level2_data_names[["GlobalTechYr"]], "minicam.energy.input", "efficiency")
  level2_data_names[["GlobalTechCost"]] <- c(level2_data_names[["GlobalTechYr"]], "minicam.non.energy.input", "input.cost")

  level2_data_names[["GlobalTechCapital"]] <- c(level2_data_names[["GlobalTechYr"]], "input.capital", "capital.overnight", "fixed.charge.rate")
  level2_data_names[["GlobalTechCapitalOnly"]] <- c(level2_data_names[["GlobalTechYr"]], "input.capital", "capital.overnight")
  level2_data_names[["GlobalTechFCROnly"]] <- c(level2_data_names[["GlobalTechYr"]], "input.capital", "fixed.charge.rate")
  level2_data_names[["GlobalTechOMfixed"]] <- c(level2_data_names[["GlobalTechYr"]], "input.OM.fixed", "OM.fixed")
  level2_data_names[["GlobalTechOMvar"]] <- c(level2_data_names[["GlobalTechYr"]], "input.OM.var", "OM.var")
  level2_data_names[["GlobalTechBackup"]] <- c(level2_data_names[["GlobalTechYr"]], "electric.sector.name", "trial.market.name", "backup.capital.cost",
                                               "backup.capacity.factor", "capacity.limit", "minicam.energy.input", "minicam.non.energy.input", "flag")
  level2_data_names[["GlobalCarbonCapture"]] <- c(level2_data_names[["GlobalTechYr"]], "storage.market", "remove.fraction")
  level2_data_names[["GlobalRenewTech"]] <- c(level2_data_names[["GlobalTechYr"]], "renewable.input")
  level2_data_names[["GlobalTechSecOut"]] <- c(level2_data_names[["GlobalTechYr"]], "secondary.output", "output.ratio")
  level2_data_names[["GlobalTechRESSecOut"]] <- c(level2_data_names[["GlobalTechYr"]], "res.secondary.output", "output.ratio")
  level2_data_names[["GlobalTechCSeq"]] <- c(level2_data_names[["GlobalTechYr"]], "remove.fraction", "target.gas")
  level2_data_names[["GlobalTechLifetime"]] <- c("sector.name", "subsector.name", "technology", "year", "lifetime")
  level2_data_names[["GlobalTechShutdown"]] <- c(level2_data_names[["GlobalTechYr"]], "lifetime", "shutdown.rate")
  level2_data_names[["GlobalTechSCurve"]] <- c(level2_data_names[["GlobalTechYr"]], "lifetime", "steepness", "half.life")
  level2_data_names[["GlobalTechProfitShutdown"]] <- c("sector.name", "subsector.name", "technology", "year", "median.shutdown.point", "profit.shutdown.steepness")
  level2_data_names[["GlobalTechCapture"]] <- c("sector.name", "subsector.name", "technology", "year", "remove.fraction", "storage.market")
  level2_data_names[["GlobalIntTechShrwt"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "share.weight")
  level2_data_names[["GlobalIntTechCoef"]] <- c("sector.name", "subsector.name", "technology", "year", "minicam.energy.input", "coefficient")
  level2_data_names[["GlobalIntTechEff"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "minicam.energy.input", "efficiency", "type")
  level2_data_names[["GlobalIntTechCapFac"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "capacity.factor")
  level2_data_names[["GlobalIntTechCapital"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "input.capital", "capital.overnight", "fixed.charge.rate")
  level2_data_names[["GlobalIntTechCapitalOnly"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "input.capital", "capital.overnight")
  level2_data_names[["GlobalIntTechFCROnly"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "input.capital", "fixed.charge.rate")
  level2_data_names[["GlobalIntTechOMfixed"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "input.OM.fixed", "OM.fixed")
  level2_data_names[["GlobalIntTechOMvar"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "input.OM.var", "OM.var")
  level2_data_names[["GlobalIntTechLifetime"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "lifetime")
  level2_data_names[["GlobalIntTechShutdown"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "lifetime", "shutdown.rate")
  level2_data_names[["GlobalIntTechProfitShutdown"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "median.shutdown.point", "steepness")
  level2_data_names[["GlobalIntTechSCurve"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "lifetime", "steepness", "half.life")
  level2_data_names[["GlobalIntTechBackup"]] <- c("sector.name", "subsector.name", "technology", "year", "electric.sector.name", "trial.market.name", "backup.capital.cost", "backup.capacity.factor", "capacity.limit", "minicam.energy.input", "minicam.non.energy.input", "flag")
  level2_data_names[["GlobalTranTechShrwt"]] <- c("sector.name", "subsector.name", "tranTechnology", "year", "share.weight")
  level2_data_names[["GlobalTranTechInterp"]] <- c("sector.name", "subsector.name", "tranTechnology", "apply.to", "from.year", "to.year", "interpolation.function")
  level2_data_names[["GlobalTranTechSCurve"]] <- c("sector.name", "subsector.name", "tranTechnology", "year", "lifetime", "steepness", "half.life")
  level2_data_names[["GlobalPassThroughTech"]] <- c("sector.name", "subsector.name", "technology")
  level2_data_names[["GlobalResBio"]] <- c("sector.name", "subsector.name", "technology", "year", "residue.biomass.production", "mass.conversion", "harvest.index", "eros.ctrl", "mass.to.energy", "water.content")
  level2_data_names[["GlobalTechIntGainOutputRatio"]] <- c(level2_data_names[["GlobalTechYr"]], "internal.gains.output.ratio", "internal.gains.market.name")
  level2_data_names[["AvgFossilEffKeyword"]] <- c(level2_data_names[["GlobalTechYr"]], "average.fossil.efficiency")
  level2_data_names[["PrimaryConsKeyword"]] <- c(level2_data_names[["GlobalTechYr"]], "primary.consumption")
  level2_data_names[["PrimaryConsKeywordff"]] <- c("region", "supplysector", "subsector", "technology", "year", "primary.consumption")
  level2_data_names[["PrimaryRenewKeyword"]] <- c(level2_data_names[["GlobalTechYr"]], "primary.renewable")
  level2_data_names[["PrimaryRenewKeywordInt"]] <- c(level2_data_names[["GlobalTechYr"]], "primary.renewable")
  level2_data_names[["GlobalTechCTaxInput"]] <- c(level2_data_names[["GlobalTechYr"]], "ctax.input", "fuel.C.coef")
  level2_data_names[["DeleteGlobalTechInput"]] <- level2_data_names[["GlobalTechInput"]]
  level2_data_names[["GlobalIntTechCost"]] <- c("sector.name", "subsector.name", "intermittent.technology", "year", "minicam.non.energy.input", "input.cost")

  # Stub technologies
  level2_data_names[["StubTech"]] <- c("region", "supplysector", "subsector", "stub.technology")
  level2_data_names[["StubTechYr"]] <- c(level2_data_names[["StubTech"]], "year")
  level2_data_names[["StubTechInterp"]] <- c(level2_data_names[["StubTech"]], "apply.to", "from.year", "to.year", "interpolation.function")
  level2_data_names[["StubTechShrwt"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "share.weight")
  level2_data_names[["StubTechCoef"]] <- c(level2_data_names[["StubTechYr"]], "minicam.energy.input", "coefficient", "market.name")
  level2_data_names[["StubTechCoef_NM"]] <- c(level2_data_names[["StubTechYr"]], "minicam.energy.input", "coefficient")
  level2_data_names[["StubTechEff"]] <- c(level2_data_names[["StubTechYr"]], "minicam.energy.input", "efficiency", "market.name")
  level2_data_names[["StubTechCalorieContent"]] <- c(level2_data_names[["StubTechEff"]])
  level2_data_names[["StubTechProd"]] <- c(level2_data_names[["StubTechYr"]], "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight")
  level2_data_names[["StubTechCalInput"]] <- c(level2_data_names[["StubTechYr"]], "minicam.energy.input", "calibrated.value", "share.weight.year", "subs.share.weight", "tech.share.weight")
  level2_data_names[["StubTechCapital"]] <- c(level2_data_names[["StubTechYr"]], "input.capital", "capital.overnight", "fixed.charge.rate")
  level2_data_names[["StubTechCost"]] <- c(level2_data_names[["StubTechYr"]], "minicam.non.energy.input", "input.cost")
  level2_data_names[["StubTechFixOut"]] <- c(level2_data_names[["StubTechYr"]], "fixedOutput", "share.weight.year", "subs.share.weight", "tech.share.weight")
  level2_data_names[["StubTechCapFactor"]] <- c(level2_data_names[["StubTechYr"]], "capacity.factor")
  level2_data_names[["StubTechMarket"]] <- c(level2_data_names[["StubTechYr"]], "minicam.energy.input", "market.name")
  level2_data_names[["StubCalorieContent"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input", "efficiency")
  level2_data_names[["StubResBioCurve"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "residue.biomass.production", "price", "fract.harvested")
  level2_data_names[["StubTechCalInputIndUrb"]] <- c("region", "sector.name", "subsector.name", "technology", "year", "minicam.energy.input", "calibrated.value")
  level2_data_names[["StubTechCoefIndUrb"]] <- c("region", "supplysector", "subsector", "technology", "year", "minicam.energy.input", "coefficient")
  level2_data_names[["StubTechElecMarket"]] <- c(level2_data_names[["StubTechYr"]], "electric.sector.market")
  level2_data_names[["StubTechFractSecOut"]] <- c(level2_data_names[["StubTechYr"]], "fractional.secondary.output", "output.ratio")
  level2_data_names[["StubTechFractProd"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "fractional.secondary.output", "price", "fraction.produced")
  level2_data_names[["StubTechFractCalPrice"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "fractional.secondary.output", "calPrice")
  level2_data_names[["StubTechIntGainOutputRatio"]] <- c("region", "supplysector", "subsector", "technology", "year", "internal.gains.output.ratio", "internal.gains.market.name")
  level2_data_names[["StubTechSecOut"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "secondary.output", "output.ratio")
  level2_data_names[["StubTechSecMarket"]] <- c(level2_data_names[["StubTechYr"]], "secondary.output", "market.name")
  level2_data_names[["StubTechSecPmult"]] <- c(level2_data_names[["StubTechYr"]], "secondary.output", "pMultiplier")
  level2_data_names[["StubTranTech"]] <- c("region", "supplysector", "tranSubsector", "stub.technology")
  level2_data_names[["StubTranTechCalInput"]] <- c("region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.energy.input", "calibrated.value", "share.weight.year", "subs.share.weight", "tech.share.weight")
  level2_data_names[["StubTranTechCoef"]] <- c("region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.energy.input", "coefficient", "market.name")
  level2_data_names[["StubTranTechCost"]] <- c("region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.non.energy.input", "input.cost")
  level2_data_names[["StubTranTechLoadFactor"]] <- c("region", "supplysector", "tranSubsector", "stub.technology", "year", "loadFactor")
  level2_data_names[["StubTranTechProd"]] <- c("region", "supplysector", "tranSubsector", "stub.technology", "year", "calOutputValue")
  level2_data_names[["DeleteStubTech"]] <- level2_data_names[["StubTech"]]
  level2_data_names[["StubTechSCurve"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "lifetime", "steepness", "half.life")
  level2_data_names[["StubTechLifetime"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "lifetime")
  level2_data_names[["StubTechProfitShutdown"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "median.shutdown.point", "profit.shutdown.steepness")
  level2_data_names[["DeleteStubTechMinicamEnergyInput"]] <- c(level2_data_names[["StubTechYr"]], "minicam.energy.input")
  level2_data_names[["StubTechEffFlag"]] <- c(level2_data_names[["StubTechYr"]], "minicam.energy.input", "efficiency", "market.name", "flag")

  # Agricultural sectors, subsectors, and technologies
  level2_data_names[["AgSupplySector"]] <- c("region", "AgSupplySector", "output.unit", "input.unit", "price.unit", "calPrice", "market", "logit.year.fillout", "logit.exponent")
  level2_data_names[["AgMkt"]] <- c("region", "AgSupplySector", "market")
  level2_data_names[["AgSupplySector_absolute-cost-logit"]] <- c("region", "AgSupplySector")
  level2_data_names[["AgSupplySector_relative-cost-logit"]] <- c("region", "AgSupplySector")
  level2_data_names[["AgSupplySubsector"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "logit.year.fillout", "logit.exponent")
  level2_data_names[["AgSupplySubsector_absolute-cost-logit"]] <- c("region", "AgSupplySector", "AgSupplySubsector")
  level2_data_names[["AgSupplySubsector_relative-cost-logit"]] <- c("region", "AgSupplySector", "AgSupplySubsector")
  level2_data_names[["AgSupplySubsectorAll"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "logit.year.fillout", "logit.exponent", "year.fillout", "share.weight")
  level2_data_names[["AgTech"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology")
  level2_data_names[["AgTechInterp"]] <- c(level2_data_names[["AgTech"]], "apply.to", "from.year", "to.year", "interpolation.function")
  level2_data_names[["AgTechYr"]] <- c(level2_data_names[["AgTech"]], "year")
  level2_data_names[["AgTechShrwt"]] <- c(level2_data_names[["AgTechYr"]], "share.weight")
  level2_data_names[["AgProduction"]] <- c(level2_data_names[["AgTechYr"]], "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight")
  level2_data_names[["AgHAtoCL"]] <- c(level2_data_names[["AgTechYr"]], "harvests.per.year")
  level2_data_names[["AgYield"]] <- c(level2_data_names[["AgTechYr"]], "yield")
  level2_data_names[["AgNonEnergyCost"]] <- c(level2_data_names[["AgTechYr"]], "minicam.non.energy.input", "input.cost")
  level2_data_names[["AgPriceConversion"]] <- c(level2_data_names[["AgTechYr"]], "minicam.energy.input", "price.unit.conversion")
  level2_data_names[["AgProdChange"]] <- c(level2_data_names[["AgTechYr"]], "AgProdChange")
  level2_data_names[["AgCost"]] <- c(level2_data_names[["AgTechYr"]], "nonLandVariableCost")
  level2_data_names[["AgCoef"]] <- c(level2_data_names[["AgTechYr"]], "minicam.energy.input", "coefficient")
  level2_data_names[["AgRES"]] <- c(level2_data_names[["AgTechYr"]], "res.secondary.output", "output.ratio")
  level2_data_names[["AgResBioCurve"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "residue.biomass.production", "price", "fract.harvested")
  level2_data_names[["ResBio"]] <- c(level2_data_names[["TechYr"]], "residue.biomass.production", "mass.conversion", "harvest.index", "eros.ctrl", "mass.to.energy", "water.content")
  level2_data_names[["ResBioCurve"]] <- c(level2_data_names[["TechYr"]], "residue.biomass.production", "price", "fract.harvested")
  level2_data_names[["AgConstraint"]] <- c(level2_data_names[["AgTechYr"]], "input.tax", "coefficient")
  level2_data_names[["UnmgdTech"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology")
  level2_data_names[["AgResBio"]] <- c(level2_data_names[["AgTechYr"]], "residue.biomass.production", "mass.conversion", "harvest.index", "eros.ctrl", "mass.to.energy", "water.content")
  level2_data_names[["AgInputTax"]] <- c(level2_data_names[["AgTech"]], "input.tax", "coefficient")
  level2_data_names[["ItemName"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology", "year", "itemName")

  # Demands
  level2_data_names[["EnergyFinalDemand"]] <- c("region", "energy.final.demand")
  level2_data_names[["PerCapitaBased"]] <- c(level2_data_names[["EnergyFinalDemand"]], "perCapitaBased")
  level2_data_names[["BaseService"]] <- c(level2_data_names[["EnergyFinalDemand"]], "year", "base.service")
  level2_data_names[["PriceElasticity"]] <- c(level2_data_names[["EnergyFinalDemand"]], "year", "price.elasticity")
  level2_data_names[["IncomeElasticity"]] <- c(level2_data_names[["EnergyFinalDemand"]], "year", "income.elasticity")
  level2_data_names[["aeei"]] <- c("region", "energy.final.demand", "year", "aeei")
  level2_data_names[["FinalDemandInfo"]] <- c("region", "energy.final.demand", "perCapitaBased", "income.elasticity", "year", "base.service", "aeei")
  level2_data_names[["SubregionalShares"]] <- c("region", "gcam.consumer", "pop.year.fillout", "inc.year.fillout", "subregional.population.share", "subregional.income.share")
  level2_data_names[["DemandFunction_food"]] <- c("region", "gcam.consumer", "nodeInput", "prodDmdFnType")
  level2_data_names[["DemandFunction_flsp"]] <- c("region", "gcam.consumer", "nodeInput", "prodDmdFnType")
  level2_data_names[["DemandFunction_serv"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "prodDmdFnType")
  level2_data_names[["Floorspace"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "year", "base.building.size")
  level2_data_names[["SatiationAdder"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "satiation.adder")
  level2_data_names[["Satiation_flsp"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "satiation.level")
  level2_data_names[["ShellConductance"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "year", "shell.conductance", "shell.year", "floor.to.surface.ratio")
  level2_data_names[["PriceExp_IntGains"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "price.exp.year.fillout", "price.exponent", "internal.gains.market.name", "internal.gains.unit")
  level2_data_names[["ThermalBaseService"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "year", "base.service")
  level2_data_names[["ThermalServiceSatiation"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "satiation.level")
  level2_data_names[["ThermalServiceSatiationAdder"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "satiation.adder")
  level2_data_names[["Intgains_scalar"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "internal.gains.scalar")
  level2_data_names[["HDDCDD"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "year", "degree.days")
  level2_data_names[["GenericBaseService"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input", "year", "base.service")
  level2_data_names[["GenericServiceSatiation"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input", "satiation.level")
  level2_data_names[["GenericServiceTechChange"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input", "year.fillout", "tech.change")
  level2_data_names[["DeleteFinalDemand"]] <- level2_data_names[["EnergyFinalDemand"]]
  level2_data_names[["DeleteConsumer"]] <- c("region", "gcam.consumer")
  level2_data_names[["DeleteThermalService"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "supplysector")
  level2_data_names[["DeleteGenericService"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input", "supplysector")
  level2_data_names[["BldNodes"]] <- c("region", "gcam.consumer", "nodeInput", "building.node.input")
  level2_data_names[["DemandStapleParams"]] <- c("region", "gcam.consumer", "nodeInput", "staples.food.demand.input", "scale.param", "self.price.elasticity", "cross.price.elasticity", "income.elasticity", "income.max.term", "price.received")
  level2_data_names[["DemandNonStapleParams"]] <- c("region", "gcam.consumer", "nodeInput", "non.staples.food.demand.input", "scale.param", "self.price.elasticity", "income.elasticity")
  level2_data_names[["DemandStapleRegBias"]] <- c("region", "gcam.consumer", "nodeInput", "staples.food.demand.input", "regional.bias.year", "regional.bias")
  level2_data_names[["DemandNonStapleRegBias"]] <- c("region", "gcam.consumer", "nodeInput", "non.staples.food.demand.input", "regional.bias.year", "regional.bias")
  level2_data_names[["StapleBaseService"]] <- c("region", "gcam.consumer", "nodeInput", "staples.food.demand.input", "year", "base.service")
  level2_data_names[["NonStapleBaseService"]] <- c("region", "gcam.consumer", "nodeInput", "non.staples.food.demand.input", "year", "base.service")
  level2_data_names[["GompFnParam"]] <- c("region","gcam.consumer","nodeInput","building.node.input","habitable.land","base.pcFlsp","unadjust.satiation","land.density.param","b.param","income.param","bias.adjust.param")

  # Land types
  level2_data_names[["LN0_Land"]] <- c("region", "LandAllocatorRoot", "year.fillout", "landAllocation")
  level2_data_names[["LN0_Logit"]] <- c("region", "LandAllocatorRoot", "logit.year.fillout", "logit.exponent")
  level2_data_names[["LN0_Logit_absolute-cost-logit"]] <- c("region", "LandAllocatorRoot")
  level2_data_names[["LN0_Logit_relative-cost-logit"]] <- c("region", "LandAllocatorRoot")
  level2_data_names[["LN0_SoilTimeScale"]] <- c("region", "LandAllocatorRoot", "soilTimeScale")
  level2_data_names[["LN1_ValueLogit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "unManagedLandValue", "logit.year.fillout", "logit.exponent")
  level2_data_names[["LN1_Logit_absolute-cost-logit"]] <- c("region", "LandAllocatorRoot", "LandNode1")
  level2_data_names[["LN1_Logit_relative-cost-logit"]] <- c("region", "LandAllocatorRoot", "LandNode1")
  level2_data_names[["LN1_HistUnmgdAllocation"]] <- c("region", "LandAllocatorRoot", "LandNode1", "UnmanagedLandLeaf", "year", "allocation")
  level2_data_names[["LN1_UnmgdAllocation"]] <- level2_data_names[["LN1_HistUnmgdAllocation"]]
  level2_data_names[["LN1_UnmgdCarbon"]] <- c("region", "LandAllocatorRoot", "LandNode1", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                              "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
  level2_data_names[["LN2_Logit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "logit.year.fillout", "logit.exponent")
  level2_data_names[["LN2_Logit_absolute-cost-logit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2")
  level2_data_names[["LN2_Logit_relative-cost-logit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2")
  level2_data_names[["LN2_HistUnmgdAllocation"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "UnmanagedLandLeaf", "year", "allocation")
  level2_data_names[["LN2_UnmgdAllocation"]] <- level2_data_names[["LN2_HistUnmgdAllocation"]]
  level2_data_names[["LN2_HistMgdAllocation"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandLeaf", "year", "allocation")
  level2_data_names[["LN2_MgdAllocation"]] <- level2_data_names[["LN2_HistMgdAllocation"]]
  level2_data_names[["LN2_UnmgdCarbon"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                              "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
  level2_data_names[["LN2_MgdCarbon"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                            "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
  level2_data_names[["LN3_Logit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "logit.year.fillout", "logit.exponent")
  level2_data_names[["LN3_Logit_absolute-cost-logit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3")
  level2_data_names[["LN3_Logit_relative-cost-logit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3")
  level2_data_names[["LN3_NodeGhostShare"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "year", "ghost.unnormalized.share")
  level2_data_names[["LN3_Leaf"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf")
  level2_data_names[["LN3_LeafGhostShare"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "year", "ghost.unnormalized.share")
  level2_data_names[["LN3_LeafIsGhostShareRel"]]<- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "is.ghost.share.relative")
  level2_data_names[["LN3_HistUnmgdAllocation"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "UnmanagedLandLeaf", "year", "allocation")
  level2_data_names[["LN3_UnmgdAllocation"]] <- level2_data_names[["LN3_HistUnmgdAllocation"]]
  level2_data_names[["LN3_HistMgdAllocation"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "year", "allocation")
  level2_data_names[["LN3_MgdAllocation"]] <- level2_data_names[["LN3_HistMgdAllocation"]]
  level2_data_names[["LN3_UnmgdCarbon"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                              "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
  level2_data_names[["LN3_MgdCarbon"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                            "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
  level2_data_names[["LN3_NoEmissCarbon"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "no.emiss.carbon.calc", "extra")
  level2_data_names[["LN3_NodeCarbon"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "node.carbon.calc", "extra")
  level2_data_names[["LN3_NewTech"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "year.fillout", "isNewTechnology")
  level2_data_names[["LN4_Logit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "logit.year.fillout", "logit.exponent")
  level2_data_names[["LN4_Logit_absolute-cost-logit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4")
  level2_data_names[["LN4_Logit_relative-cost-logit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4")
  level2_data_names[["LN4_HistUnmgdAllocation"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "UnmanagedLandLeaf", "year", "allocation")
  level2_data_names[["LN4_UnmgdAllocation"]] <- level2_data_names[["LN4_HistUnmgdAllocation"]]
  level2_data_names[["LN4_HistMgdAllocation"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandLeaf", "year", "allocation")
  level2_data_names[["LN4_MgdAllocation"]] <- level2_data_names[["LN4_HistMgdAllocation"]]
  level2_data_names[["LN4_UnmgdCarbon"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                              "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
  level2_data_names[["LN4_MgdCarbon"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                            "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
  level2_data_names[["LN4_LeafGhostShare"]]<- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandLeaf", "year", "ghost.unnormalized.share")
  level2_data_names[["LN4_NodeGhostShare"]]<- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "year", "ghost.unnormalized.share")
  level2_data_names[["LN4_NodeIsGhostShareRel"]]<- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "is.ghost.share.relative")

  level2_data_names[["LN5_Logit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "logit.year.fillout", "logit.exponent")
  level2_data_names[["LN5_Logit_absolute-cost-logit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5")
  level2_data_names[["LN5_Logit_relative-cost-logit"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5")
  level2_data_names[["LN5_HistUnmgdAllocation"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "UnmanagedLandLeaf", "year", "allocation")
  level2_data_names[["LN5_UnmgdAllocation"]] <- level2_data_names[["LN5_HistUnmgdAllocation"]]
  level2_data_names[["LN5_HistMgdAllocation"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "LandLeaf", "year", "allocation")
  level2_data_names[["LN5_MgdAllocation"]] <- level2_data_names[["LN5_HistMgdAllocation"]]
  level2_data_names[["LN5_UnmgdCarbon"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "UnmanagedLandLeaf", "hist.veg.carbon.density",
                                              "hist.soil.carbon.density", "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
  level2_data_names[["LN5_MgdCarbon"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                            "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
  level2_data_names[["LN5_NodeGhostShare"]] <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "year", "ghost.unnormalized.share")
  level2_data_names[["LN5_LeafGhostShare"]]<- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "LandLeaf", "year", "ghost.unnormalized.share")
  level2_data_names[["LN1_Delete"]] <- c("region", "LandAllocatorRoot", "LandNode1")
  level2_data_names[["LandRootNegEmissMkt"]] <- c("region", "LandAllocatorRoot", "negative.emiss.market")
  level2_data_names[["LandLeafNegEmissMkt"]] <- c("region", "LandAllocatorRoot", "LandNode" ,"LandLeaf", "negative.emiss.market")

  # NonCO2
  level2_data_names[["ResEmissCoef"]] <- c("region", "resource", "subresource", "technology", "year", "Non.CO2", "emiss.coef")
  level2_data_names[["GDPCtrlMaxRes"]] <- c("region", "resource", "subresource", "technology", "year", "Non.CO2", "ctrl.name", "max.reduction")
  level2_data_names[["GDPCtrlSteepRes"]] <- c("region", "resource", "subresource", "technology", "year", "Non.CO2", "ctrl.name", "steepness")
  level2_data_names[["ResMAC"]] <- c("region", "resource", "subresource", "technology", "year", "Non.CO2", "mac.control", "tax", "mac.reduction", "market.name")
  level2_data_names[["ResMACTC"]] <- c("region", "resource", "subresource", "technology", "year", "Non.CO2", "mac.control", "tech.change.year", "tech.change")
  level2_data_names[["ResMACPhaseIn"]] <- c("region", "resource", "subresource", "technology", "year", "Non.CO2", "mac.control", "mac.phase.in.time")
  level2_data_names[["StubTechNonCO2"]] <- c(level2_data_names[["StubTechYr"]], "Non.CO2")
  level2_data_names[["StubTechEmissUnits"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "emissions.unit")
  level2_data_names[["InputEmissCoeff"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "emiss.coef", "input.name")
  level2_data_names[["OutputEmissCoeff"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "emiss.coeff")
  level2_data_names[["InputEmissions"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "input.emissions", "input.name")
  level2_data_names[["OutputEmissions"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "input.emissions")
  level2_data_names[["StbTechOutputEmissions"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "input.emissions")
  level2_data_names[["ReadInControl"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "future.emiss.coeff.name", "future.emiss.coeff.year", "emiss.coeff")
  level2_data_names[["ResReadInControl"]] <- c("region", "resource", "subresource", "technology", "year", "Non.CO2", "future.emiss.coeff.name", "future.emiss.coeff.year", "emiss.coef")
  level2_data_names[["GDPCtrlMax"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "ctrl.name", "max.reduction")
  level2_data_names[["GDPCtrlSteep"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "ctrl.name", "steepness")
  level2_data_names[["DelEmCtrl"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "ctrl.name")
  level2_data_names[["MAC"]] <- c("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "mac.control", "tax", "mac.reduction", "market.name")
  level2_data_names[["MACTC"]] <- c(level2_data_names[["StubTechNonCO2"]], "mac.control", "tech.change.year", "tech.change")
  level2_data_names[["MACPhaseIn"]] <- c(level2_data_names[["StubTechNonCO2"]], "mac.control", "mac.phase.in.time")
  level2_data_names[["MAC_NC"]] <- level2_data_names[["MAC"]]
  level2_data_names[["MAC_NoBelowZero"]] <- level2_data_names[["MAC"]]
  level2_data_names[["InputEmFactUnmgd"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology", "year", "Non.CO2", "emiss.coef", "input.name")
  level2_data_names[["OutputEmFactUnmgd"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology", "year", "Non.CO2", "emiss.coef")
  level2_data_names[["OutputEmissCoeffAg"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "emiss.coef")
  level2_data_names[["InputEmissionsUnmgd"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology", "year", "Non.CO2", "input.emissions", "input.name")
  level2_data_names[["OutputEmissionsAg"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "input.emissions")
  level2_data_names[["OutputEmissionsUnmgd"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology", "year", "Non.CO2", "input.emissions")
  level2_data_names[["AgGDPCtrlMax"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "ctrl.name", "max.reduction")
  level2_data_names[["AgGDPCtrlSteep"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "ctrl.name", "steepness")
  level2_data_names[["AgMAC"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "mac.control", "tax", "mac.reduction", "market.name")
  level2_data_names[["AgMACTC"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "mac.control", "tech.change.year", "tech.change")
  level2_data_names[["AgMACPhaseIn"]] <- c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year", "Non.CO2", "mac.control", "mac.phase.in.time")
  level2_data_names[["TrnInputEmissCoeff"]] <- c("region", "supplysector", "tranSubsector", "stub.technology", "year", "Non.CO2", "emiss.coef", "input.name")
  level2_data_names[["LinearCtrlInc"]] <- c("region", "supplysector", "tranSubsector","stub.technology", "year", "Non.CO2", "linear.control", "start.year", "end.year", "final.emissions.coefficient")
  level2_data_names[["EF_Retrofit"]] <- c("region", "supplysector", "subsector", "stub.technology", "retrofit_vintage", "Non.CO2", "linear.control", "start.year", "end.year", "final.emissions.coefficient")
  level2_data_names[["RetrofitOff"]] <- c("region", "supplysector", "subsector", "stub.technology", "period", "Non.CO2", "linear.control", "start.year", "end.year", "disable.em.control")
  level2_data_names[["EF_NSPS"]] <- c("region", "supplysector", "subsector", "stub.technology", "period", "Non.CO2", "emiss.coef")
  level2_data_names[["DeleteGDPControl"]] <-  c("region", "supplysector", "subsector", "stub.technology", "period", "Non.CO2", "gdp.control")

  # Policy
  level2_data_names[["PortfolioStd"]] <- c("region", "policy.portfolio.standard", "market", "policyType", "year", "constraint", "price.unit", "output.unit")
  level2_data_names[["PortfolioStdConstraint"]] <- c("region", "policy.portfolio.standard", "market", "policyType", "year", "constraint")
  level2_data_names[["PortfolioStdMinPrice"]] <- c("region", "policy.portfolio.standard", "min.price")
  level2_data_names[["PortfolioStdMaxPrice"]] <- c("region", "policy.portfolio.standard", "max.price")

  level2_data_names
}

#' LEVEL2_DATA_NAMES
#'
#' A list of column orderings keyed by the ModelInterface header so that we can
#' ensure tables being sent to be converted to XML by the ModelInterface have their
#' columns arranged in the order the ModelInterface is expecting them.
#' @author Pralit Patel
LEVEL2_DATA_NAMES <- generate_level2_data_names()
# Save these objects for use as internal package data
usethis::use_data(LEVEL2_DATA_NAMES, overwrite = TRUE, internal = TRUE)
rm(list = c("LEVEL2_DATA_NAMES"))

# It is frequently the case that we need to refresh the LEVEL2_DATA_NAMES in order to have
# a successful driver() run which is required to update the following so we will re-load
# the package now so the updated LEVEL2_DATA_NAMES can take effect.
devtools::load_all()

if(USE_DRIVER_DRAKE) {
  # do an initial call to ensure all targets are up to date
  driver_drake()
}

#' GCAM_DATA_MAP
#'
#' There are two levels of information available from the GCAM data system:
#' chunk dependencies, which are available for "free", i.e. with a fast query to
#' each chunk on the part of \link{\code{chunk_inputs}} and \link{\code{chunk_outputs}},
#' and detailed information on data object-level dependencies. This function is
#' used to generate this latter data, i.e. a tibble of chunk-output-precursor information,
#' which is used by \link{\code{dstrace}} and various other graphing and diagnostic utilities.
#' @author BBL
# Note: the above text is not used for package documentation and is instead
# replicated in data.R for that purpose.
if(USE_DRIVER_DRAKE) {
  # we will need to drake "plan" to construct the GCAM_DATA_MAP from cache
  # note: calling driver_drake with return_plan_only = TRUE does not actually run the driver
  gcamdata_plan <- driver_drake(return_plan_only = TRUE)
  GCAM_DATA_MAP <- create_datamap_from_cache(gcamdata_plan)
} else {
  GCAM_DATA_MAP <- driver(return_data_map_only = TRUE)
}
# Save these objects as external data (i.e. requires explicit call to `data()` to load)
usethis::use_data(GCAM_DATA_MAP, overwrite = TRUE, internal = FALSE)

prebuilt_data_names <- c(
  # outputs of module_emissions_L102.nonco2_ceds_R_S_Y
  "L102.ceds_GFED_nonco2_tg_R_S_F",
  "L102.ceds_int_shipping_nonco2_tg_S_F",

  # outputs of module_energy_LA101.en_bal_IEA
  "L101.en_bal_EJ_R_Si_Fi_Yh_full",
  "L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
  "L101.in_EJ_ctry_trn_Fi_Yh",
  "L101.in_EJ_ctry_bld_Fi_Yh",

  # output of module_energy_LA111.rsrc_fos_Prod
  "L111.RsrcCurves_EJ_R_Ffos",

  # output of module_energy_LA118.hydro
  "L118.out_EJ_R_elec_hydro_Yfut",

  # outputs of module_energy_LA121.liquids
  "L121.in_EJ_R_unoil_F_Yh",
  "L121.in_EJ_R_TPES_crude_Yh",
  "L121.in_EJ_R_TPES_unoil_Yh",
  "L121.share_R_TPES_biofuel_tech",
  "L121.BiomassOilRatios_kgGJ_R_C"
)

#' PREBUILT_DATA
#'
#' A list of prebuilt data objects. These are used when the proprietary IEA
#' energy data files are not available, and thus
#' \code{\link{module_energy_LA100.IEA_downscale_ctry}} is not able to run.
#' Its immediate downstream dependencies (currently, four chunks) then use the
#' prebuilt versions of their outputs stored in this object.
#' @author BBL
# Note: the above text is not used for package documentation and is instead
# replicated in data.R for that purpose.
if(USE_DRIVER_DRAKE) {
  PREBUILT_DATA <- load_from_cache(prebuilt_data_names)
} else {
  PREBUILT_DATA <- driver(write_outputs = FALSE,
                          write_xml = FALSE,
                          return_data_names = prebuilt_data_names)
}
# Save these objects as external data (i.e. requires explicit call to `data()` to load)
usethis::use_data(PREBUILT_DATA, overwrite = TRUE, internal = FALSE)

