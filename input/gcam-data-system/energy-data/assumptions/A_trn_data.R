# UC Davis transportation sector objects
#Currently the UCD database's base year is 2005 (there is no other base year).
# This is used for energy disaggregation in all historical years.
UCD_en_year <- 2005
X_UCD_en_year <- paste0( "X", UCD_en_year )

UCD_years <- seq( 2005, 2095, 15 )
X_UCD_years <- paste0( "X", UCD_years )
pre_UCD_years <- historical_years[ historical_years < min( UCD_years ) ]
X_pre_UCD_years <- paste0( "X", pre_UCD_years )
UCD_first_year <- min( UCD_years )
X_UCD_first_year <- paste0( "X", UCD_first_year )
hist_nonUCD_years <- historical_years[ !historical_years %in% c( pre_UCD_years , UCD_years ) ]

UCD_modeID <- c( "UCD_sector", "mode" )
UCD_sizeID <- c( "UCD_sector", "mode", "size.class" )
UCD_techID <- c( "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel" )
UCD_Cat_F <- c( "UCD_category", "fuel" )
UCD_R_Cat_F <- c( "UCD_region", UCD_Cat_F )

s_tS_tT <- c( "supplysector", "tranSubsector", "tranTechnology" )
s_tS_t <- c( "supplysector", "tranSubsector", "technology" )

discount_rate_veh <- 0.1   #Consumer discount rate for vehicle purchases
nper_amort_veh <- 10    #Number of periods (years) over which vehicle capital payments are amortized
fcr_veh <- discount_rate_veh + discount_rate_veh / ( ( ( 1 + discount_rate_veh ) ^ nper_amort_veh ) - 1 )
min_weight_EJ <- 1e-8    #Minimum weighting in EJ, for country-level aggregation.

digits_speed <- 1
digits_LoadFactor <- 2
digits_mpkm <- 0
# reduce rounding in detailed USA transport for compatability w model
digits_trnUSA_default <- 1

#Level2 data names (specific to the transportation sector)
#Transportation subsectors and technologies
names_tranSubsector <- c( "region", "supplysector", "tranSubsector" )
names_tranSubsectorLogit <- c( names_tranSubsector, "logit.year.fillout", "logit.exponent" )
names_tranSubsectorLogitType <- c( names_tranSubsector, "logit.type" )
names_tranSubsectorShrwt <- c( names_tranSubsector, "year", "share.weight" )
names_tranSubsectorShrwtFllt <- c( names_tranSubsector, "year.fillout", "share.weight" )
names_tranSubsectorInterp <- c( names_tranSubsector, "apply.to","from.year", "to.year", "interpolation.function" )
names_tranSubsectorInterpTo <- c( names_tranSubsector, "apply.to","from.year", "to.year", "to.value", "interpolation.function" )
names_tranSubsectorSpeed <- c( names_tranSubsector, "year", "speed" )
names_tranSubsectorVOTT <- c( names_tranSubsector, "addTimeValue", "year.fillout", "time.value.multiplier" )
names_tranSubsectorFuelPref <- c( names_tranSubsector, "year.fillout", "fuelprefElasticity" )

#Stub technologies
names_StubTranTech <- c( "region", "supplysector", "tranSubsector", "stub.technology" )
names_StubTranTechYr <- c( names_StubTranTech, "year" )
names_StubTranTechCalInput <- c( names_StubTranTechYr, input, "calibrated.value", "share.weight.year", "subs.share.weight", "tech.share.weight" )
names_StubTranTechLoadFactor <- c( names_StubTranTechYr, "loadFactor" )
names_StubTranTechCost <- c( names_StubTranTechYr, "minicam.non.energy.input", "input.cost" )
names_StubTranTechCoef <- c( names_StubTranTechYr, input, "coefficient", "market.name" )

#Global technologies
names_GlobalTranTech <- c( "sector.name", "subsector.name", "tranTechnology" )
names_GlobalTranTechYr <- c( names_GlobalTranTech, "year")
names_GlobalTranTechShrwt <- c( names_GlobalTranTechYr, "share.weight" )
names_GlobalTranTechInterp <- c( names_GlobalTranTech, "apply.to","from.year", "to.year", "interpolation.function" )
names_GlobalTranTechSCurve <- c( names_GlobalTranTechYr, "lifetime", "steepness", "half.life" )

#Function specific to the transportation data processing
#aggregate_passthroughs: a function for calibrating the inputs to the pass-through technologies of the heavily nested transportation sector
aggregate_passthroughs <- function(data, index) {
	input_at_index <- data[index, "minicam.energy.input"]
	input_sector_indices <- row.names(data[data$region == data[index, "region"] & data$year == data[index, "year"] & data$supplysector == input_at_index,])
	if(length(input_sector_indices) > 0) {
		sum <- 0
		for(new_index in input_sector_indices) {
			sum <- sum + aggregate_passthroughs(data, new_index)
		}
		data[index, "output"] <- sum
	}
	return(data[index, "output"])
}
