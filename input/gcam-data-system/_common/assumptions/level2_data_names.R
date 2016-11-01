#Column names of tables that are read in to the model interface
#These correspond to the headers of the same name in the headers/ModelInterface_headers.txt file
# These are for generic categories; module-specific level2 data names are found in data header files of each module
#Resources
names_DepRsrc <- c( "region", "depresource", "output.unit", "price.unit", "market" )
names_RenewRsrc <- c( "region", "renewresource", "output.unit", "price.unit", "market" )
names_UnlimitRsrc <- c( "region", "unlimited.resource", "output.unit", "price.unit", "market", "capacity.factor" )
names_DepRsrcPrice <- c( "region", "depresource", "year", "price" )
names_RenewRsrcPrice <- c( "region", "renewresource", "year", "price" )
names_UnlimitRsrcPrice <- c( "region", "unlimited.resource", "year", "price" )

#Subresources
names_SubDepRsrc <- c( "region", "depresource", "subresource" )
names_SubRenewRsrc <- c( "region", "renewresource", "sub.renewable.resource" )
names_SmthRenewRsrc <- c( "region", "renewresource", "smooth.renewable.subresource" )
names_DepRsrcCalProd <- c( names_SubDepRsrc, "year", "cal.production" )
names_RenewRsrcCalProd <- c( names_SubRenewRsrc, "year", "cal.production" )
names_maxSubResource <- c( names_SubRenewRsrc, "year.fillout", "maxSubResource" ) #only applicable for renewable resources
names_DepRsrcCurves <- c( names_SubDepRsrc, "grade", "available", "extractioncost" )
names_RenewRsrcCurves <- c( names_SubRenewRsrc, "grade", "available", "extractioncost" )
names_SmthRenewRsrcCurves <- c( names_SmthRenewRsrc, "year.fillout", "maxSubResource", "mid.price", "curve.exponent" )
names_DepRsrcTechChange <- c( names_SubDepRsrc, "year.fillout", "techChange" )
names_RenewRsrcTechChange <- c( names_SubRenewRsrc, "year.fillout", "techChange" )
names_SmthRenewRsrcTechChange <- c( names_SmthRenewRsrc, "year.fillout", "techChange" )

#Supplysectors
names_Supplysector <- c( "region", "supplysector", "output.unit", "input.unit", "price.unit", "logit.year.fillout", "logit.exponent" )
names_FinalEnergyKeyword <- c( "region", "supplysector", "final.energy" )
names_SupplysectorPrice <- c( "region", "supplysector", "year", "price" )
names_SupplysectorLogitType <- c( "region", "supplysector", "logit.type" )
names_PassThroughSector <- c( "region", "pass.through.sector", "marginal.revenue.sector", "marginal.revenue.market" )

#Subsectors
names_Subsector <- c( "region", "supplysector", "subsector")
names_SubsectorAll <- c( names_Subsector, "logit.year.fillout", "logit.exponent", "year.fillout", "share.weight", "apply.to","from.year", "to.year", "interpolation.function" )
names_SubsectorLogit <- c( names_Subsector, "logit.year.fillout", "logit.exponent" )
names_SubsectorLogitType <- c( names_Subsector, "logit.type" )
names_SubsectorShrwt <- c( names_Subsector, "year", "share.weight" )
names_SubsectorShrwtFllt <- c( names_Subsector, "year.fillout", "share.weight" )
names_SubsectorInterp <- c( names_Subsector, "apply.to","from.year", "to.year", "interpolation.function" )
names_SubsectorInterpTo <- c( names_Subsector, "apply.to","from.year", "to.year", "to.value", "interpolation.function" )
names_FuelPrefElasticity <- c( names_Subsector, "year.fillout", "fuelprefElasticity" )

#Technologies
names_Tech <- c( "region", "supplysector", "subsector", "technology" )
names_TechInterp <- c( names_Tech, "apply.to", "from.year", "to.year", "interpolation.function" )
names_TechInterpTo <- c( names_Tech, "apply.to","from.year", "to.year", "to.value", "interpolation.function" )
names_TechYr <- c( names_Tech, "year" )
names_Production <- c( names_TechYr, "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight" )
names_TechShrwt <- c( names_TechYr, "share.weight" )
names_CalInput <- c( names_TechYr, input, "calibrated.value", "share.weight.year", "subs.share.weight", "tech.share.weight" )
#Market name is specified for efficiencies or coefficients
names_TechCoef <- c( names_TechYr, input, "coefficient", "market.name" )
names_TechEff <- c( names_TechYr, input, "efficiency", "market.name" )
names_TechCost <- c( names_TechYr, "minicam.non.energy.input", "input.cost" )
names_CarbonCapture <- c( names_TechYr, "storage.market", "remove.fraction" )
names_ImportTech <- c( names_TechYr ) #indicates a renewable input; nothing is specified in the table however
names_FixedOutput <- c( names_TechYr, "fixedOutput" )
names_TechCapital <- c( names_TechYr, "input.capital", "capital.overnight", "fixed.charge.rate", "capacity.factor" )
names_TechOMfixed <- c( names_TechYr, "input.OM.fixed", "OM.fixed", "capacity.factor" )
names_TechOMvar <- c( names_TechYr, "input.OM.var", "OM.var" )
names_TechSCurve <- c( names_TechYr, "lifetime", "steepness", "half.life" )

#Global technologies
names_GlobalTech <- c( "sector.name", "subsector.name", "technology" )
names_GlobalTechInterp <- c( "sector.name", "subsector.name", "technology", "apply.to","from.year", "to.year", "interpolation.function" )
names_GlobalTechInterpTo <- c( "sector.name", "subsector.name", "technology", "apply.to","from.year", "to.year", "to.value", "interpolation.function" )
names_GlobalTechYr <- c( names_GlobalTech, "year" )
names_GlobalTechInput <- c( names_GlobalTechYr, input )
names_GlobalTechCoef <- c( names_GlobalTechYr, input, "coefficient" )
names_GlobalTechEff <- c( names_GlobalTechYr, input, "efficiency" )
names_GlobalTechCost <- c( names_GlobalTechYr, "minicam.non.energy.input", "input.cost" )
names_GlobalTechCapital <- c( names_GlobalTechYr, "input.capital", "capital.overnight", "fixed.charge.rate", "capacity.factor" )
names_GlobalTechOMfixed <- c( names_GlobalTechYr, "input.OM.fixed", "OM.fixed", "capacity.factor" )
names_GlobalTechOMvar <- c( names_GlobalTechYr, "input.OM.var", "OM.var" )
names_GlobalTechBackup <- c( names_GlobalTechYr, "electric.sector.name", "trial.market.name", "backup.capital.cost",
                             "backup.capacity.factor", "capacity.limit", "minicam.energy.input", "minicam.non.energy.input", "flag" )
names_GlobalCarbonCapture <- c( names_GlobalTechYr, "storage.market", "remove.fraction" )
names_GlobalRenewTech <- c( names_GlobalTechYr, "renewable.input" )
names_GlobalTechSecOut <- c( names_GlobalTechYr, "secondary.output", "output.ratio" )
names_GlobalTechCSeq <- c( names_GlobalTechYr, "remove.fraction", "target.gas" )
names_GlobalTechShutdown <- c( names_GlobalTechYr, "lifetime", "shutdown.rate" )
names_GlobalTechSCurve <- c( names_GlobalTechYr, "lifetime", "steepness", "half.life" )

#Stub technologies
names_StubTech <- c( "region", "supplysector", "subsector", "stub.technology" )
names_StubTechInterp <- c( names_StubTech, "apply.to", "from.year", "to.year", "interpolation.function" )
names_StubTechYr <- c( names_StubTech, "year" )
names_StubTechProd <- c( names_StubTechYr, "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight" )
names_StubTechCoef <- c( names_StubTechYr, input, "coefficient", "market.name" )
names_StubTechCoef_NM <- c( names_StubTechYr, input, "coefficient" )
names_StubTechEff <- c( names_StubTechYr, input, "efficiency", "market.name" )
names_StubTechCalInput <- c( names_StubTechYr, input, "calibrated.value", "share.weight.year", "subs.share.weight", "tech.share.weight" )
names_StubTechCalorieContent <- c( names_StubTechEff )
names_StubTechCapital <- c( names_StubTechYr, "input.capital", "capital.overnight", "fixed.charge.rate", "capacity.factor" )
names_StubTechCost <- c( names_StubTechYr, "minicam.non.energy.input", "input.cost" )
names_StubTechFixOut <- c( names_StubTechYr, "fixedOutput", "share.weight.year", "subs.share.weight", "tech.share.weight" )
names_StubTechCapFactor <- c( names_StubTechYr, "input.capital", "capacity.factor.capital", "input.OM.fixed", "capacity.factor.OM" )
names_StubTechMarket <- c( names_StubTechYr, input, "market.name" )
names_StubTechFractSecOut <- c( names_StubTechYr, "fractional.secondary.output", "output.ratio" )
names_StubTechNonCO2 <- c( names_StubTechYr, "Non.CO2" )

#Agricultural sectors, subsectors, and technologies
names_AgSupplySector <- c( "region", "AgSupplySector", "output.unit", "input.unit", "price.unit", "calPrice", "market", "logit.year.fillout", "logit.exponent" )
names_AgSupplySectorLogitType <- c( "region", "AgSupplySector", "logit.type" )
names_AgSupplySubsector <- c( "region", "AgSupplySector", "AgSupplySubsector", "logit.year.fillout", "logit.exponent" )
names_AgSupplySubsectorLogitType <- c( "region", "AgSupplySector", "AgSupplySubsector", "logit.type" )
names_AgSupplySubsectorAll <- c( "region", "AgSupplySector", "AgSupplySubsector", "logit.year.fillout", "logit.exponent", "year.fillout", "share.weight" )
names_AgTech <- c( "region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
names_AgTechInterp <- c( names_AgTech, "apply.to", "from.year", "to.year", "interpolation.function" )
names_AgTechYr <- c( names_AgTech, "year" )
names_AgTechShrwt <- c( names_AgTechYr, "share.weight" )
names_AgProduction <- c( names_AgTechYr, "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight" )
names_AgHAtoCL <- c( names_AgTechYr, "harvests.per.year" )
names_AgYield <- c( names_AgTechYr, "yield" )
names_AgCoef <- c( names_AgTechYr, input, "coefficient", "market.name" )
names_AgProdChange <- c( names_AgTechYr, "AgProdChange" )
names_AgCost <- c( names_AgTechYr, "nonLandVariableCost" )
names_AgCoef <- c( names_AgTechYr, input, "coefficient" )
names_AgRES <- c( names_AgTechYr, "res.secondary.output", "output.ratio" )
names_AgConstraint <- c( names_AgTechYr, "input.tax", "coefficient" )
names_UnmgdTech <- c( "region", "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology" )
names_AgResBio <- c( names_AgTechYr, "residue.biomass.production", "mass.conversion", "harvest.index", "eros.ctrl", "mass.to.energy", "water.content" )

#Demands
names_EnergyFinalDemand <- c( "region", "energy.final.demand" )
names_PerCapitaBased <- c( names_EnergyFinalDemand, "perCapitaBased" )
names_BaseService <- c( names_EnergyFinalDemand, "year", "base.service" )
names_PriceElasticity <- c( names_EnergyFinalDemand, "year", "price.elasticity" )
names_IncomeElasticity <- c( names_EnergyFinalDemand, "year", "income.elasticity" )
names_aeei <- c( "region", "energy.final.demand", "year", "aeei" )

#Land types
names_LN0_Land <- c( "region", "LandAllocatorRoot", "year.fillout", "landAllocation" )
names_LN0_Logit <- c( "region", "LandAllocatorRoot", "logit.year.fillout", "logit.exponent" )
names_LN0_LogitType <- c( "region", "LandAllocatorRoot", "logit.type" )
names_LN0_SoilTimeScale <- c( "region", "LandAllocatorRoot", "soilTimeScale" )
names_LN1_ValueLogit <- c( "region", "LandAllocatorRoot", "LandNode1", "unManagedLandValue", "logit.year.fillout", "logit.exponent" )
names_LN1_LogitType <- c( "region", "LandAllocatorRoot", "LandNode1", "logit.type" )
names_LN1_HistUnmgdAllocation <- c( "region", "LandAllocatorRoot", "LandNode1", "UnmanagedLandLeaf", "year", "allocation" )
names_LN1_UnmgdAllocation <- names_LN1_HistUnmgdAllocation
names_LN1_UnmgdCarbon <- c( "region", "LandAllocatorRoot", "LandNode1", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
      "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
names_LN2_Logit <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "logit.year.fillout", "logit.exponent" )
names_LN2_LogitType <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "logit.type" )
names_LN2_HistUnmgdAllocation <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "UnmanagedLandLeaf", "year", "allocation" )
names_LN2_UnmgdAllocation <- names_LN2_HistUnmgdAllocation
names_LN2_HistMgdAllocation <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandLeaf", "year", "allocation" )
names_LN2_MgdAllocation <- names_LN2_HistMgdAllocation
names_LN2_UnmgdCarbon <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
      "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
names_LN2_MgdCarbon <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
      "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
names_LN3_Logit <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "logit.year.fillout", "logit.exponent" )
names_LN3_LogitType <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "logit.type" )
names_LN3_DefaultShare <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "year", "default.share" )
names_LN3_HistUnmgdAllocation <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "UnmanagedLandLeaf", "year", "allocation" )
names_LN3_UnmgdAllocation <- names_LN3_HistUnmgdAllocation
names_LN3_HistMgdAllocation <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "year", "allocation" )
names_LN3_MgdAllocation <- names_LN3_HistMgdAllocation
names_LN3_UnmgdCarbon <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
      "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
names_LN3_MgdCarbon <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
      "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
names_LN3_NewTech <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf", "year.fillout", "isNewTechnology" )
names_LN4_Logit <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "logit.year.fillout", "logit.exponent" )
names_LN4_LogitType <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "logit.type" )
names_LN4_DefaultShare <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "year", "default.share" )
names_LN4_HistUnmgdAllocation <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "UnmanagedLandLeaf", "year", "allocation" )
names_LN4_UnmgdAllocation <- names_LN4_HistUnmgdAllocation
names_LN4_HistMgdAllocation <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandLeaf", "year", "allocation" )
names_LN4_MgdAllocation <- names_LN4_HistMgdAllocation
names_LN4_UnmgdCarbon <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                            "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
names_LN4_MgdCarbon <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                          "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
names_LN4_NewTech <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandLeaf", "newTechStartYear", "isNewTechnology", "ghost.share.leaf" )
names_LN4_NewNode <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "isNewTechnology", "ghost.share.node" )

names_LN5_Logit <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "logit.year.fillout", "logit.exponent" )
names_LN5_LogitType <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "logit.type" )
names_LN5_DefaultShare <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "year", "default.share" )
names_LN5_HistUnmgdAllocation <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "UnmanagedLandLeaf", "year", "allocation" )
names_LN5_UnmgdAllocation <- names_LN5_HistUnmgdAllocation
names_LN5_HistMgdAllocation <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "LandLeaf", "year", "allocation" )
names_LN5_MgdAllocation <- names_LN5_HistMgdAllocation
names_LN5_UnmgdCarbon <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "UnmanagedLandLeaf", "hist.veg.carbon.density",
                            "hist.soil.carbon.density", "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
names_LN5_MgdCarbon <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                          "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density" )
names_LN5_NewTech <- c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandNode4", "LandNode5", "LandLeaf", "newTechStartYear", "isNewTechnology", "ghost.share.leaf" )
