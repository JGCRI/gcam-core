# Column names of tables that are read in to the model interface
# These correspond to the headers of the same name in the headers/ModelInterface_headers.txt file
# These are for generic categories; module-specific level2 data names are found in data header files of each module

R <- "region"
Y <- "year"
input <- "minicam.energy.input"

# Resources
aglu.NAMES_DEPRSRC          <- c(R, "depresource", "output.unit", "price.unit", "market")
aglu.NAMES_RENEWRSRC        <- c(R, "renewresource", "output.unit", "price.unit", "market")
aglu.NAMES_UNLIMITRSRC      <- c(R, "unlimited.resource", "output.unit", "price.unit", "market", "capacity.factor")
aglu.NAMES_DEPRSRCPRICE	    <- c(R, "depresource", Y, "price")
aglu.NAMES_RENEWRSRCPRICE	  <- c(R, "renewresource", Y, "price")
aglu.NAMES_UNLIMITRSRCPRICE	<- c(R, "unlimited.resource", Y, "price")

# Subresources
aglu.NAMES_SUBDEPRSRC              <- c(R, "depresource", "subresource")
aglu.NAMES_SUBRENEWRSRC            <- c(R, "renewresource", "sub.renewable.resource")
aglu.NAMES_SMTHRENEWRSRC           <- c(R, "renewresource", "smooth.renewable.subresource")
aglu.NAMES_DEPRSRCCALPROD          <- c(aglu.NAMES_SUBDEPRSRC, Y, "cal.production")
aglu.NAMES_RENEWRSRCCALPROD        <- c(aglu.NAMES_SUBRENEWRSRC, Y, "cal.production")
aglu.NAMES_MAXSUBRESOURCE          <- c(aglu.NAMES_SMTHRENEWRSRC, "year.fillout", "maxSubResource") #only applicable for renewable resources
aglu.NAMES_DEPRSRCCURVES           <- c(aglu.NAMES_SUBDEPRSRC, "grade", "available", "extractioncost")
aglu.NAMES_RENEWRSRCCURVES         <- c(aglu.NAMES_SUBRENEWRSRC, "grade", "available", "extractioncost")
aglu.NAMES_SMTHRENEWRSRCCURVES     <- c(aglu.NAMES_SMTHRENEWRSRC, "year.fillout", "maxSubResource", "mid.price", "curve.exponent")
aglu.NAMES_DEPRSRCTECHCHANGE       <- c(aglu.NAMES_SUBDEPRSRC, "year.fillout", "techChange")
aglu.NAMES_RENEWRSRCTECHCHANGE     <- c(aglu.NAMES_SUBRENEWRSRC, "year.fillout", "techChange")
aglu.NAMES_SMTHRENEWRSRCTECHCHANGE <- c(aglu.NAMES_SMTHRENEWRSRC, "year.fillout", "techChange")

# Supplysectors
aglu.NAMES_SUPPLYSECTOR          <- c(R, "supplysector", "output.unit", "input.unit", "price.unit", "logit.year.fillout", "logit.exponent")
aglu.NAMES_FINALENERGYKEYWORD    <- c(R, "supplysector", "final.energy")
aglu.NAMES_SUPPLYSECTORPRICE     <- c(R, "supplysector", Y, "price")
aglu.NAMES_SUPPLYSECTORLOGITTYPE <- c(R, "supplysector", "logit.type")
aglu.NAMES_PASSTHROUGHSECTOR     <- c(R, "pass.through.sector", "marginal.revenue.sector", "marginal.revenue.market")

# Subsectors
aglu.NAMES_SUBSECTOR          <- c(R, "supplysector", "subsector")
aglu.NAMES_SUBSECTORALL       <- c(aglu.NAMES_SUBSECTOR, "logit.year.fillout", "logit.exponent", "year.fillout", "share.weight", "apply.to", "from.year", "to.year", "interpolation.function")
aglu.NAMES_SUBSECTORLOGIT     <- c(aglu.NAMES_SUBSECTOR, "logit.year.fillout", "logit.exponent")
aglu.NAMES_SUBSECTORLOGITTYPE <- c(aglu.NAMES_SUBSECTOR, "logit.type")
aglu.NAMES_SUBSECTORSHRWT     <- c(aglu.NAMES_SUBSECTOR, Y, "share.weight")
aglu.NAMES_SUBSECTORSHRWTFLLT <- c(aglu.NAMES_SUBSECTOR, "year.fillout", "share.weight")
aglu.NAMES_SUBSECTORINTERP    <- c(aglu.NAMES_SUBSECTOR, "apply.to", "from.year", "to.year", "interpolation.function")
aglu.NAMES_SUBSECTORINTERPTO  <- c(aglu.NAMES_SUBSECTOR, "apply.to", "from.year", "to.year", "to.value", "interpolation.function")
aglu.NAMES_FUELPREFELASTICITY <- c(aglu.NAMES_SUBSECTOR, "year.fillout", "fuelprefElasticity")

# Technologies
aglu.NAMES_TECH         <- c(R, "supplysector", "subsector", "technology")
aglu.NAMES_TECHINTERP   <- c(aglu.NAMES_TECH, "apply.to", "from.year", "to.year", "interpolation.function")
aglu.NAMES_TECHINTERPTO <- c(aglu.NAMES_TECH, "apply.to", "from.year", "to.year", "to.value", "interpolation.function")
aglu.NAMES_TECHYR       <- c(aglu.NAMES_TECH, Y)
aglu.NAMES_PRODUCTION   <- c(aglu.NAMES_TECHYR, "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight")
aglu.NAMES_TECHSHRWT    <- c(aglu.NAMES_TECHYR, "share.weight")
aglu.NAMES_CALINPUT     <- c(aglu.NAMES_TECHYR, input, "calibrated.value", "share.weight.year", "subs.share.weight", "tech.share.weight")

# Market name is specified for efficiencies or coefficients
aglu.NAMES_TECHCOEF      <- c(aglu.NAMES_TECHYR, input, "coefficient", "market.name")
aglu.NAMES_TECHEFF       <- c(aglu.NAMES_TECHYR, input, "efficiency", "market.name")
aglu.NAMES_TECHCOST      <- c(aglu.NAMES_TECHYR, "minicam.non.energy.input", "input.cost")
aglu.NAMES_CARBONCAPTURE <- c(aglu.NAMES_TECHYR, "storage.market", "remove.fraction")
aglu.NAMES_IMPORTTECH    <- c(aglu.NAMES_TECHYR) # indicates a renewable input; nothing is specified in the table however
aglu.NAMES_FIXEDOUTPUT   <- c(aglu.NAMES_TECHYR, "fixedOutput")
aglu.NAMES_TECHCAPITAL   <- c(aglu.NAMES_TECHYR, "input.capital", "capital.overnight", "fixed.charge.rate", "capacity.factor")
aglu.NAMES_TECHOMFIXED   <- c(aglu.NAMES_TECHYR, "input.OM.fixed", "OM.fixed", "capacity.factor")
aglu.NAMES_TECHOMVAR     <- c(aglu.NAMES_TECHYR, "input.OM.var", "OM.var")
aglu.NAMES_TECHSCURVE    <- c(aglu.NAMES_TECHYR, "lifetime", "steepness", "half.life")

# Global technologies
aglu.NAMES_GLOBALTECH          <- c("sector.name", "subsector.name", "technology")
aglu.NAMES_GLOBALTECHINTERP    <- c("sector.name", "subsector.name", "technology", "apply.to", "from.year", "to.year", "interpolation.function")
aglu.NAMES_GLOBALTECHINTERPTO  <- c("sector.name", "subsector.name", "technology", "apply.to", "from.year", "to.year", "to.value", "interpolation.function")
aglu.NAMES_GLOBALTECHYR        <- c(aglu.NAMES_GLOBALTECH, Y)
aglu.NAMES_GLOBALTECHINPUT     <- c(aglu.NAMES_GLOBALTECHYR, input)
aglu.NAMES_GLOBALTECHCOEF      <- c(aglu.NAMES_GLOBALTECHYR, input, "coefficient")
aglu.NAMES_GLOBALTECHEFF       <- c(aglu.NAMES_GLOBALTECHYR, input, "efficiency")
aglu.NAMES_GLOBALTECHCOST      <- c(aglu.NAMES_GLOBALTECHYR, "minicam.non.energy.input", "input.cost")
aglu.NAMES_GLOBALTECHCAPITAL   <- c(aglu.NAMES_GLOBALTECHYR, "input.capital", "capital.overnight", "fixed.charge.rate", "capacity.factor")
aglu.NAMES_GLOBALTECHOMFIXED   <- c(aglu.NAMES_GLOBALTECHYR, "input.OM.fixed", "OM.fixed", "capacity.factor")
aglu.NAMES_GLOBALTECHOMVAR     <- c(aglu.NAMES_GLOBALTECHYR, "input.OM.var", "OM.var")
aglu.NAMES_GLOBALTECHBACKUP    <- c(aglu.NAMES_GLOBALTECHYR, "electric.sector.name", "trial.market.name", "backup.capital.cost",
                                    "backup.capacity.factor", "capacity.limit", "minicam.energy.input", "minicam.non.energy.input", "flag")
aglu.NAMES_GLOBALCARBONCAPTURE <- c(aglu.NAMES_GLOBALTECHYR, "storage.market", "remove.fraction")
aglu.NAMES_GLOBALRENEWTECH     <- c(aglu.NAMES_GLOBALTECHYR, "renewable.input")
aglu.NAMES_GLOBALTECHSECOUT    <- c(aglu.NAMES_GLOBALTECHYR, "secondary.output", "output.ratio")
aglu.NAMES_GLOBALTECHCSEQ      <- c(aglu.NAMES_GLOBALTECHYR, "remove.fraction", "target.gas")
aglu.NAMES_GLOBALTECHSHUTDOWN  <- c(aglu.NAMES_GLOBALTECHYR, "lifetime", "shutdown.rate")
aglu.NAMES_GLOBALTECHSCURVE    <- c(aglu.NAMES_GLOBALTECHYR, "lifetime", "steepness", "half.life")

# Stub technologies
aglu.NAMES_STUBTECH          <- c(R, "supplysector", "subsector", "stub.technology")
aglu.NAMES_STUBTECHINTERP    <- c(aglu.NAMES_STUBTECH, "apply.to", "from.year", "to.year", "interpolation.function")
aglu.NAMES_STUBTECHYR        <- c(aglu.NAMES_STUBTECH, Y)
aglu.NAMES_STUBTECHPROD      <- c(aglu.NAMES_STUBTECHYR, "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight")
aglu.NAMES_STUBTECHCOEF      <- c(aglu.NAMES_STUBTECHYR, input, "coefficient", "market.name")
aglu.NAMES_STUBTECHCOEF_NM   <- c(aglu.NAMES_STUBTECHYR, input, "coefficient")
aglu.NAMES_STUBTECHEFF       <- c(aglu.NAMES_STUBTECHYR, input, "efficiency", "market.name")
aglu.NAMES_STUBTECHCALINPUT  <- c(aglu.NAMES_STUBTECHYR, input, "calibrated.value", "share.weight.year", "subs.share.weight", "tech.share.weight")
aglu.NAMES_STUBTECHCALORIECONTENT <- c(aglu.NAMES_STUBTECHEFF)
aglu.NAMES_STUBTECHCAPITAL   <- c(aglu.NAMES_STUBTECHYR, "input.capital", "capital.overnight", "fixed.charge.rate", "capacity.factor")
aglu.NAMES_STUBTECHCOST      <- c(aglu.NAMES_STUBTECHYR, "minicam.non.energy.input", "input.cost")
aglu.NAMES_STUBTECHFIXOUT    <- c(aglu.NAMES_STUBTECHYR, "fixedOutput", "share.weight.year", "subs.share.weight", "tech.share.weight")
aglu.NAMES_STUBTECHCAPFACTOR <- c(aglu.NAMES_STUBTECHYR, "input.capital", "capacity.factor.capital", "input.OM.fixed", "capacity.factor.OM")
aglu.NAMES_STUBTECHMARKET    <- c(aglu.NAMES_STUBTECHYR, input, "market.name")
aglu.NAMES_STUBTECHFRACTSECOUT <- c(aglu.NAMES_STUBTECHYR, "fractional.secondary.output", "output.ratio")
aglu.NAMES_STUBTECHNONCO2    <- c(aglu.NAMES_STUBTECHYR, "Non.CO2")

# Agricultural sectors, subsectors, and technologies
aglu.NAMES_AGSUPPLYSECTOR             <- c(R, "AgSupplySector", "output.unit", "input.unit", "price.unit", "calPrice", "market", "logit.year.fillout", "logit.exponent")
aglu.NAMES_AGSUPPLYSECTORLOGITTYPE    <- c(R, "AgSupplySector", "logit.type")
aglu.NAMES_AGSUPPLYSUBSECTOR          <- c(R, "AgSupplySector", "AgSupplySubsector", "logit.year.fillout", "logit.exponent")
aglu.NAMES_AGSUPPLYSUBSECTORLOGITTYPE <- c(R, "AgSupplySector", "AgSupplySubsector", "logit.type")
aglu.NAMES_AGSUPPLYSUBSECTORALL       <- c(R, "AgSupplySector", "AgSupplySubsector", "logit.year.fillout", "logit.exponent", "year.fillout", "share.weight")
aglu.NAMES_AGTECH       <- c(R, "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology")
aglu.NAMES_AGTECHINTERP <- c(aglu.NAMES_AGTECH, "apply.to", "from.year", "to.year", "interpolation.function")
aglu.NAMES_AGTECHYR     <- c(aglu.NAMES_AGTECH, Y)
aglu.NAMES_AGTECHSHRWT  <- c(aglu.NAMES_AGTECHYR, "share.weight")
aglu.NAMES_AGPRODUCTION <- c(aglu.NAMES_AGTECHYR, "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight")
aglu.NAMES_AGHATOCL     <- c(aglu.NAMES_AGTECHYR, "harvests.per.year")
aglu.NAMES_AGYIELD      <- c(aglu.NAMES_AGTECHYR, "yield")
aglu.NAMES_AGCOEF       <- c(aglu.NAMES_AGTECHYR, input, "coefficient", "market.name")
aglu.NAMES_AGPRODCHANGE <- c(aglu.NAMES_AGTECHYR, "AgProdChange")
aglu.NAMES_AGCOST       <- c(aglu.NAMES_AGTECHYR, "nonLandVariableCost")
aglu.NAMES_AGCOEF       <- c(aglu.NAMES_AGTECHYR, input, "coefficient")
aglu.NAMES_AGRES        <- c(aglu.NAMES_AGTECHYR, "res.secondary.output", "output.ratio")
aglu.NAMES_AGCONSTRAINT <- c(aglu.NAMES_AGTECHYR, "input.tax", "coefficient")
aglu.NAMES_UNMGDTECH    <- c(R, "AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology")
aglu.NAMES_AGRESBIO     <- c(aglu.NAMES_AGTECHYR, "residue.biomass.production", "mass.conversion", "harvest.index", "eros.ctrl", "mass.to.energy", "water.content")

# Demands
aglu.NAMES_ENERGYFINALDEMAND <- c(R, "energy.final.demand")
aglu.NAMES_PERCAPITABASED    <- c(aglu.NAMES_ENERGYFINALDEMAND, "perCapitaBased")
aglu.NAMES_BASESERVICE       <- c(aglu.NAMES_ENERGYFINALDEMAND, Y, "base.service")
aglu.NAMES_PRICEELASTICITY   <- c(aglu.NAMES_ENERGYFINALDEMAND, Y, "price.elasticity")
aglu.NAMES_INCOMEELASTICITY  <- c(aglu.NAMES_ENERGYFINALDEMAND, Y, "income.elasticity")
aglu.NAMES_AEEI              <- c(R, "energy.final.demand", Y, "aeei")

# Land types
aglu.NAMES_LN0_LAND                <- c(R, "LandAllocatorRoot", "year.fillout", "landAllocation")
aglu.NAMES_LN0_LOGIT               <- c(R, "LandAllocatorRoot", "logit.year.fillout", "logit.exponent")
aglu.NAMES_LN0_LOGITTYPE           <- c(R, "LandAllocatorRoot", "logit.type")
aglu.NAMES_LN0_SOILTIMESCALE       <- c(R, "LandAllocatorRoot", "soilTimeScale")
aglu.NAMES_LN1_VALUELOGIT          <- c(R, "LandAllocatorRoot", "LandNode1", "unManagedLandValue", "logit.year.fillout", "logit.exponent")
aglu.NAMES_LN1_LOGITTYPE           <- c(R, "LandAllocatorRoot", "LandNode1", "logit.type")
aglu.NAMES_LN1_HISTUNMGDALLOCATION <- c(R, "LandAllocatorRoot", "LandNode1", "UnmanagedLandLeaf", Y, "allocation")
aglu.NAMES_LN1_UNMGDALLOCATION     <- aglu.NAMES_LN1_HISTUNMGDALLOCATION
aglu.NAMES_LN1_UNMGDCARBON         <- c(R, "LandAllocatorRoot", "LandNode1", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                        "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
aglu.NAMES_LN2_LOGIT               <- c(R, "LandAllocatorRoot", "LandNode1", "LandNode2", "logit.year.fillout", "logit.exponent")
aglu.NAMES_LN2_LOGITTYPE           <- c(R, "LandAllocatorRoot", "LandNode1", "LandNode2", "logit.type")
aglu.NAMES_LN2_HISTUNMGDALLOCATION <- c(R, "LandAllocatorRoot", "LandNode1", "LandNode2", "UnmanagedLandLeaf", Y, "allocation")
aglu.NAMES_LN2_UNMGDALLOCATION     <- aglu.NAMES_LN2_HISTUNMGDALLOCATION
aglu.NAMES_LN2_HISTMGDALLOCATION   <- c(R, "LandAllocatorRoot", "LandNode1", "LandNode2", "LandLeaf", Y, "allocation")
aglu.NAMES_LN2_MGDALLOCATION       <- aglu.NAMES_LN2_HISTMGDALLOCATION
aglu.NAMES_LN2_UNMGDCARBON         <- c(R, "LandAllocatorRoot", "LandNode1", "LandNode2", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                        "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
aglu.NAMES_LN2_MGDCARBON           <- c(R, "LandAllocatorRoot", "LandNode1", "LandNode2", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                        "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
ln13 <- c("LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3")
aglu.NAMES_LN3_LOGIT               <- c(R, ln13, "logit.year.fillout", "logit.exponent")
aglu.NAMES_LN3_LOGITTYPE           <- c(R, ln13, "logit.type")
aglu.NAMES_LN3_NODEGHOSTSHARE      <- c(R, ln13, Y, "ghost.unnormalized.share")
aglu.NAMES_LN3_LEAFGHOSTSHARE      <- c(R, ln13, "LandLeaf", Y, "ghost.unnormalized.share")
aglu.NAMES_LN3_LEAFISGHOSTSHAREREL <- c(R, ln13, "LandLeaf", "is.ghost.share.relative")
aglu.NAMES_LN3_HISTUNMGDALLOCATION <- c(R, ln13, "UnmanagedLandLeaf", Y, "allocation")
aglu.NAMES_LN3_UNMGDALLOCATION     <- aglu.NAMES_LN3_HISTUNMGDALLOCATION
aglu.NAMES_LN3_HISTMGDALLOCATION   <- c(R, ln13, "LandLeaf", Y, "allocation")
aglu.NAMES_LN3_MGDALLOCATION       <- aglu.NAMES_LN3_HISTMGDALLOCATION
aglu.NAMES_LN3_UNMGDCARBON         <- c(R, ln13, "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                        "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
aglu.NAMES_LN3_MGDCARBON           <- c(R, ln13, "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                        "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
aglu.NAMES_LN3_NEWTECH             <- c(R, ln13, "LandLeaf", "year.fillout", "isNewTechnology")
aglu.NAMES_LN4_LOGIT               <- c(R, ln13, "LandNode4", "logit.year.fillout", "logit.exponent")
aglu.NAMES_LN4_LOGITTYPE           <- c(R, ln13, "LandNode4", "logit.type")
aglu.NAMES_LN4_HISTUNMGDALLOCATION <- c(R, ln13, "LandNode4", "UnmanagedLandLeaf", Y, "allocation")
aglu.NAMES_LN4_UNMGDALLOCATION     <- aglu.NAMES_LN4_HISTUNMGDALLOCATION
aglu.NAMES_LN4_HISTMGDALLOCATION   <- c(R, ln13, "LandNode4", "LandLeaf", Y, "allocation")
aglu.NAMES_LN4_MGDALLOCATION       <- aglu.NAMES_LN4_HISTMGDALLOCATION
aglu.NAMES_LN4_UNMGDCARBON         <- c(R, ln13, "LandNode4", "UnmanagedLandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                        "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
aglu.NAMES_LN4_MGDCARBON           <- c(R, ln13, "LandNode4", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                        "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
aglu.NAMES_LN4_LEAFGHOSTSHARE	     <- c(R, ln13, "LandNode4", "LandLeaf", Y, "ghost.unnormalized.share")
aglu.NAMES_LN4_NODEGHOSTSHARE	     <- c(R, ln13, "LandNode4", Y, "ghost.unnormalized.share")
aglu.NAMES_LN4_NODEISGHOSTSHAREREL <- c(R, ln13, "LandNode4", "is.ghost.share.relative")

aglu.NAMES_LN5_LOGIT               <- c(R, ln13, "LandNode4", "LandNode5", "logit.year.fillout", "logit.exponent")
aglu.NAMES_LN5_LOGITTYPE           <- c(R, ln13, "LandNode4", "LandNode5", "logit.type")
aglu.NAMES_LN5_HISTUNMGDALLOCATION <- c(R, ln13, "LandNode4", "LandNode5", "UnmanagedLandLeaf", Y, "allocation")
aglu.NAMES_LN5_UNMGDALLOCATION     <- aglu.NAMES_LN5_HISTUNMGDALLOCATION
aglu.NAMES_LN5_HISTMGDALLOCATION   <- c(R, ln13, "LandNode4", "LandNode5", "LandLeaf", Y, "allocation")
aglu.NAMES_LN5_MGDALLOCATION       <- aglu.NAMES_LN5_HISTMGDALLOCATION
aglu.NAMES_LN5_UNMGDCARBON         <- c(R, ln13, "LandNode4", "LandNode5", "UnmanagedLandLeaf", "hist.veg.carbon.density",
                                        "hist.soil.carbon.density", "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
aglu.NAMES_LN5_MGDCARBON           <- c(R, ln13, "LandNode4", "LandNode5", "LandLeaf", "hist.veg.carbon.density", "hist.soil.carbon.density",
                                        "veg.carbon.density", "soil.carbon.density", "mature.age.year.fillout", "mature.age", "min.veg.carbon.density", "min.soil.carbon.density")
aglu.NAMES_LN5_LEAFGHOSTSHARE	     <- c(R, ln13, "LandNode4", "LandNode5", "LandLeaf", Y, "ghost.unnormalized.share")

rm(input, ln13, R, Y)  # don't want these hanging around namespace
