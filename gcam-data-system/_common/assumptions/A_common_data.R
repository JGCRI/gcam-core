#------------------------------------------------------------------------------------
#Historical years for data write-out
historical_years <- 1971:2010
X_historical_years <- paste( "X", historical_years, sep = "" )
X_final_historical_year <- X_historical_years[ length( X_historical_years ) ]

#Future years, where applicable
future_years <- seq( 2015, 2100, 5 )
X_future_years <- paste0( "X", future_years )

#Default logit exponents
default_subsector_logit <- -3
default_tech_logit <- -6

#Column headers referred to throughout the processing
Scen <- "scenario"
R <- "GCAM_region_ID"
Y <- "year"
Scen_R <- c( Scen, R )
R_Y <- c( R, Y )
reg <- "region"
supp <- "supplysector"
subs <- "subsector"
tech <- "technology"
input <- "minicam.energy.input"
RenewRsrc <- "renewresource"
SubRenewRsrc <- "sub.renewable.resource"

agsupp <- "AgSupplySector"
agsubs <- "AgSupplySubsector"
agtech <- "AgProductionTechnology"
