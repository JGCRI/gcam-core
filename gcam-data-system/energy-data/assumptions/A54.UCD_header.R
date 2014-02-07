# UC Davis transportation sector objects
#Currently the UCD database's base year is 2005 (there is no other base year).
# This is used for energy disaggregation in all historical years.
UCD_en_year <- 2005
X_UCD_en_year <- paste0( "X", UCD_en_year )

UCD_years <- seq( 2005, 2095, 15 )
X_UCD_years <- paste0( "X", UCD_years )

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





