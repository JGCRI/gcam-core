# Level 1 column headers
Xyr <- "Xyear"
R_S <- c( "GCAM_region_ID", "sector" )
R_S_F <- c( "GCAM_region_ID", "sector", "fuel")
R_S_F_tech <- c( R_S_F, "technology" )
R_F <- c( "GCAM_region_ID", "fuel" )
R_F_Y <- c( "GCAM_region_ID", "fuel", "year" )
R_F_Xyr <- c( "GCAM_region_ID", "fuel", "Xyear" )
R_Y <- c( "GCAM_region_ID", "year" )
R_Xyr <- c( "GCAM_region_ID", "Xyear" )
S_F <- c( "sector", "fuel" )
S_F_tech <- c( "sector", "fuel", "technology" )
RG3_subrsrc_grd <- c( "region_GCAM3", "subresource", "grade" )
RG3_F <- c( "region_GCAM3", "fuel" )
s_s_t <- c( "supplysector", "subsector", "technology" )
s_s_t_i <- c( "supplysector", "subsector", "technology", "minicam.energy.input" )

digits_calOutput <- 7
digits_coefficient <- 7
digits_cost <- 4
digits_efficiency <- 3
digits_shrwt <- 4

#Range of PMultipliers to use for secondary output feedcrops from biodiesel and corn ethanol
max_bioliquid_Pmult <- 0.80
