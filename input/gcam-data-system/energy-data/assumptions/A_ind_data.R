
min_in_EJ_ind <- 1e-3 #used to avoid negative/zero energy when disaggregating detailed industries (cement, fertilizer)
NH3_H_frac <- 3/17 #stiochiometric mass ratio. portion of NH3 that is H (by mass)
H_energy_GJtH2 <- 120 #energy content (LHV) of hydrogen gas

#Set a year to which to converge the feedstock:energy ratios in generic industrial production function coefficients
indcoef_conv_year <- 2150
X_indcoef_conv_year <- paste0( "X", indcoef_conv_year )

digits_IncElas_ind <- 3

max_indService_GJcap <- 75
