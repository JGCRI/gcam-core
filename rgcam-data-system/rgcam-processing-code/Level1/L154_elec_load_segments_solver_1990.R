# L154_elec_load_segments_solver_1990.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L154_elec_load_segments_solver_1990.R" )
printlog( "Elec load segments solver for 1990" )

# -----------------------------------------------------------------------------
# 1. Read files
USA_demand_fraction <- readdata( "USA_demand_fraction" )
USA_elec_segments <- readdata( "USA_elec_segments" )
L107_in_EJ_sR_elec_F <- readdata( "L107_in_EJ_sR_elec_F" )
L153_sR_elec_supply <- readdata( "L153_sR_elec_supply" )
L152_sR_elec_demand <- readdata( "L152_sR_elec_demand" )
L153_sR_elec_segments <- readdata( "L153_sR_elec_segments" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Initialize Variables
L154_sR_elec_supply <- L153_sR_elec_supply
L154_sR_elec_demand <- L152_sR_elec_demand
L154_sR_elec_segments <- L153_sR_elec_segments
#Initialize Variables
L154_sR_elec_supply$objective1990 <- 1
L154_segment_list <- c("base load generation", "intermediate generation", "subpeak generation", "peak generation")
L154_subregion_list <- unique(L154_sR_elec_supply$GCAM_subregion)

#Function to check demands and supplys by segment and L154_region
check_elec_segments <- function (gen_fraction, L154_region, segment, fuel = "gas") {
#set fraction as specified
L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == fuel & L154_sR_elec_supply$segment == segment,
							names(L154_sR_elec_supply) == "fraction1990"] <- gen_fraction
L154_sR_elec_supply$gen1990 <- L154_sR_elec_supply$X1990*L154_sR_elec_supply$fraction1990

#Aggregate all technologies to get total subregional supply by generation segment
L154_sR_elec_segments_tmp <- aggregate(L154_sR_elec_supply[,9:10 ],
							list(subregion = L154_sR_elec_supply$GCAM_subregion, segment = L154_sR_elec_supply$segment),sum)
L154_sR_elec_segments[L154_sR_elec_segments$subregion == L154_region & L154_sR_elec_segments$segment == segment, names(L154_sR_elec_segments) == "gen1990"] <- 
							L154_sR_elec_segments_tmp[L154_sR_elec_segments_tmp$subregion == L154_region & L154_sR_elec_segments_tmp$segment == segment, names(L154_sR_elec_segments_tmp) == "gen1990"]

#Check that supply meets demand
L154_sR_elec_segments$check1990 <- L154_sR_elec_segments$demand1990 - L154_sR_elec_segments$gen1990
L154_sR_elec_segments$check2005 <- L154_sR_elec_segments$demand2005 - L154_sR_elec_segments$gen2005
L154_sR_elec_segments$pct1990 <- L154_sR_elec_segments$check1990/L154_sR_elec_segments$gen1990
L154_sR_elec_segments$pct2005 <- L154_sR_elec_segments$check2005/L154_sR_elec_segments$gen2005

check <- abs(L154_sR_elec_segments[L154_sR_elec_segments$subregion == L154_region & L154_sR_elec_segments$segment == segment,names(L154_sR_elec_segments) == "check1990"])
check
}

#Loop through each subregion
for (j in 1:length(L154_subregion_list)){
L154_region <- L154_subregion_list[j]

#First, if subregion is largely hydro, move some hydro to intermediate
L154_hydro_frac <- L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$segment == "base load generation" & L154_sR_elec_supply$GCAM_fuel == "hydro",
							names(L154_sR_elec_supply) == "X1990"] / L154_sR_elec_segments[L154_sR_elec_segments$subregion == L154_region & 
							L154_sR_elec_segments$segment == "base load generation",names(L154_sR_elec_segments) == "tot_demand1990"]
if (L154_hydro_frac > 0.4) {
	L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "hydro" & 
		L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"] <- 0.75*L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region 
		& L154_sR_elec_supply$GCAM_fuel == "hydro" & L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"]
	L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "hydro" & 
		L154_sR_elec_supply$segment == L154_segment_list[2],names(L154_sR_elec_supply) == "fraction1990"] <- 1 - L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region 
		& L154_sR_elec_supply$GCAM_fuel == "hydro" & L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"]
	}

#Loop to solve for generation fractions for each generation segment
for (i in 1:4) {
if (i != 4) {
	L154_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-4, L154_region, L154_segment_list[i])
	L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- L154_solved_fraction$minimum
	L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "objective1990"] <- L154_solved_fraction$objective
	} else {
	L154_nonpeak <- sum(L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & L154_sR_elec_supply$segment != L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] )
	L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- 1 - L154_nonpeak

		}
	}
	
	
	
#If there wasn't enough gas to solve, make more adjustments#
	if (L154_nonpeak > 1)  {
	#Change coal shares
	L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & 
		L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"] <- 0.05*L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region 
		& L154_sR_elec_supply$GCAM_fuel == "gas" & L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"]
	L154_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-4, L154_region, L154_segment_list[1], "coal")
	L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "coal" & 
		L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"] <- L154_solved_fraction$minimum
	L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "coal" & 
		L154_sR_elec_supply$segment == L154_segment_list[2],names(L154_sR_elec_supply) == "fraction1990"] <- .77*(1- L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region 
		& L154_sR_elec_supply$GCAM_fuel == "coal" & L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"])
	L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "coal" & 
		L154_sR_elec_supply$segment == L154_segment_list[3],names(L154_sR_elec_supply) == "fraction1990"] <- .23*(1- L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region 
		& L154_sR_elec_supply$GCAM_fuel == "coal" & L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"])
	for (i in 1:4) {
		if (i != 4) {
		L154_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-4, L154_region, L154_segment_list[i])
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- L154_solved_fraction$minimum
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "objective1990"] <- L154_solved_fraction$objective
		} else {
		L154_nonpeak <- sum(L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & L154_sR_elec_supply$segment != L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] )
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- 1 - L154_nonpeak
			}
		}
	}
	
#If we are still off, try adjusting oil consumption
if (L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & L154_sR_elec_supply$segment == L154_segment_list[3],
							names(L154_sR_elec_supply) == "objective1990"] > .0001)  {				
		for (i in 3:4) {
		if (i != 4) {
		L154_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-5, L154_region, L154_segment_list[i], "refined liquids")
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "refined liquids" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- L154_solved_fraction$minimum
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "refined liquids" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "objective1990"] <- L154_solved_fraction$objective
		} else {
		L154_nonpeak <- sum(L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "refined liquids" & L154_sR_elec_supply$segment != L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] )
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "refined liquids" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- 1 - L154_nonpeak
			}
		}
	}

#Additional adjustments for subregions that were off in 1990
if (L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & L154_sR_elec_supply$segment == L154_segment_list[3],
							names(L154_sR_elec_supply) == "objective1990"] > .0001 ||
	L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & L154_sR_elec_supply$segment == L154_segment_list[2],
							names(L154_sR_elec_supply) == "objective1990"] > .0001)
{
		#Reset Gas Shares
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & 
			L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"] <- 0.25
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & 
			L154_sR_elec_supply$segment == L154_segment_list[2],names(L154_sR_elec_supply) == "fraction1990"] <- 0.25
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & 
			L154_sR_elec_supply$segment == L154_segment_list[3],names(L154_sR_elec_supply) == "fraction1990"] <- 0.25
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "gas" & 
			L154_sR_elec_supply$segment == L154_segment_list[4],names(L154_sR_elec_supply) == "fraction1990"] <- 0.25
	#If a L154_region had a lot of oil, move some to intermediate
	L154_oil_frac <- L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$segment == "base load generation" & L154_sR_elec_supply$GCAM_fuel == "refined liquids",
							names(L154_sR_elec_supply) == "X1990"] / L154_sR_elec_segments[L154_sR_elec_segments$subregion == L154_region & 
							L154_sR_elec_segments$segment == "base load generation",names(L154_sR_elec_segments) == "tot_demand1990"]
	if (L154_oil_frac > 0.2) {
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "refined liquids" & 
			L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"] <- 0.35
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "refined liquids" & 
			L154_sR_elec_supply$segment == L154_segment_list[2],names(L154_sR_elec_supply) == "fraction1990"] <- 0.25
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "refined liquids" & 
			L154_sR_elec_supply$segment == L154_segment_list[3],names(L154_sR_elec_supply) == "fraction1990"] <- 0.20
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "refined liquids" & 
			L154_sR_elec_supply$segment == L154_segment_list[4],names(L154_sR_elec_supply) == "fraction1990"] <- 0.20
	}
	
	#Split hydro between intermediate and base
	L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "hydro" & 
		L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"] <- 0.75*L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region 
		& L154_sR_elec_supply$GCAM_fuel == "hydro" & L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"]
	L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "hydro" & 
		L154_sR_elec_supply$segment == L154_segment_list[2],names(L154_sR_elec_supply) == "fraction1990"] <- 1 - L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region 
		& L154_sR_elec_supply$GCAM_fuel == "hydro" & L154_sR_elec_supply$segment == L154_segment_list[1],names(L154_sR_elec_supply) == "fraction1990"]
		
	#If coal is > 85% of generation, put some coal in peak (Make sure the fractions below sum to 1!)
		L154_coal_frac <- L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$segment == "base load generation" & L154_sR_elec_supply$GCAM_fuel == "coal",
							names(L154_sR_elec_supply) == "X1990"] / L154_sR_elec_segments[L154_sR_elec_segments$subregion == L154_region & 
							L154_sR_elec_segments$segment == "base load generation",names(L154_sR_elec_segments) == "tot_demand1990"]
		nuc_frac <- L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$segment == "base load generation" & L154_sR_elec_supply$GCAM_fuel == "nuclear",
							names(L154_sR_elec_supply) == "X1990"] / L154_sR_elec_segments[L154_sR_elec_segments$subregion == L154_region & 
							L154_sR_elec_segments$segment == "base load generation",names(L154_sR_elec_segments) == "tot_demand1990"]
	if (L154_coal_frac + nuc_frac > 0.9) {
	for (i in 1:4) {
		if (i != 4) {
		L154_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-4, L154_region, L154_segment_list[i], "coal")
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "coal" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- L154_solved_fraction$minimum
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "coal" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "objective1990"] <- L154_solved_fraction$objective
		} else {
		L154_nonpeak <- sum(L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "coal" & L154_sR_elec_supply$segment != L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] )
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "coal" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- 1 - L154_nonpeak
			}
		}
	}
	
	if (L154_coal_frac > 0.5) {
	for (i in 1:3) {
		if (i != 3) {
		L154_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-4, L154_region, L154_segment_list[i], "coal")
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "coal" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- L154_solved_fraction$minimum
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "coal" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "objective1990"] <- L154_solved_fraction$objective
		} else {
		L154_nonpeak <- sum(L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "coal" & L154_sR_elec_supply$segment != L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] )
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "coal" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- 1 - L154_nonpeak
			}
		}
	}
	
	if (L154_hydro_frac > 0.5) {
	for (i in 1:4) {
		if (i != 4) {
		L154_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-4, L154_region, L154_segment_list[i], "hydro")
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "hydro" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- L154_solved_fraction$minimum
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "hydro" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "objective1990"] <- L154_solved_fraction$objective
		} else {
		L154_nonpeak <- sum(L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "hydro" & L154_sR_elec_supply$segment != L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] )
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "hydro" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- 1 - L154_nonpeak
			}
		}
	}
		
	for (i in 1:4) {
		if (i != 4) {
		L154_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-5, L154_region, L154_segment_list[i], "refined liquids")
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "refined liquids" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- L154_solved_fraction$minimum
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "refined liquids" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "objective1990"] <- L154_solved_fraction$objective
		} else {
		L154_nonpeak <- sum(L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "refined liquids" & L154_sR_elec_supply$segment != L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] )
		L154_sR_elec_supply[L154_sR_elec_supply$GCAM_subregion == L154_region & L154_sR_elec_supply$GCAM_fuel == "refined liquids" & L154_sR_elec_supply$segment == L154_segment_list[i],
							names(L154_sR_elec_supply) == "fraction1990"] <- 1 - L154_nonpeak
			}
		}
	
	}
}	
#Recalculate demands

##Total Subregional Supply By Elec Generation Segment and Technology using US level fractions by technology
L154_sR_elec_supply$gen1990 <- L154_sR_elec_supply$X1990*L154_sR_elec_supply$fraction1990
L154_sR_elec_supply$gen2005 <- L154_sR_elec_supply$X2005*L154_sR_elec_supply$fraction2005

##Aggregate all technologies to get total subregional supply by generation segment
L154_sR_elec_segments <- aggregate(L154_sR_elec_supply[,9:10 ],
								list(subregion = L154_sR_elec_supply$GCAM_subregion, segment = L154_sR_elec_supply$segment),sum)
L154_sR_elec_segments <- L154_sR_elec_segments[order(L154_sR_elec_segments$subregion),]

##Total Demand By Elec Segment
L154_sR_elec_demand <- aggregate(L154_sR_elec_segments[,3:4], list(subregion = L154_sR_elec_segments$subregion),sum)
L154_sR_elec_demand <- L154_sR_elec_demand[rep(1:nrow(L154_sR_elec_demand),times=4),]
L154_sR_elec_demand <- L154_sR_elec_demand[order(L154_sR_elec_demand$subregion),]
L154_sR_elec_segments$tot_demand1990 <-L154_sR_elec_demand$gen1990
L154_sR_elec_segments$tot_demand2005 <-L154_sR_elec_demand$gen2005
L154_sR_elec_segments$demand_fraction <- USA_demand_fraction$Demand_Fraction[match(L154_sR_elec_segments$segment,USA_demand_fraction$Vsegment)]
L154_sR_elec_segments$demand1990 <- L154_sR_elec_segments$demand_fraction*L154_sR_elec_segments$tot_demand1990
L154_sR_elec_segments$demand2005 <- L154_sR_elec_segments$demand_fraction*L154_sR_elec_segments$tot_demand2005

##Check that supply meets demand
L154_sR_elec_segments$check1990 <- L154_sR_elec_segments$demand1990 - L154_sR_elec_segments$gen1990
L154_sR_elec_segments$check2005 <- L154_sR_elec_segments$demand2005 - L154_sR_elec_segments$gen2005
L154_sR_elec_segments$pct1990 <- L154_sR_elec_segments$check1990/L154_sR_elec_segments$gen1990
L154_sR_elec_segments$pct2005 <- L154_sR_elec_segments$check2005/L154_sR_elec_segments$gen2005

#L154_sR_elec_supply is only elect output. Before writing out, add elec input
L154_sR_elec_supply_in <-merge(L107_in_EJ_sR_elec_F, USA_elec_segments, by.x = "GCAM_fuel", by.y="fuel")
L154_sR_elec_supply_in <- L154_sR_elec_supply_in[,c("GCAM_subregion", "GCAM_fuel","technology","segment","X1990","X2005","fraction1990", "fraction2005")]
L154_sR_elec_supply_in <- L154_sR_elec_supply_in[order(L154_sR_elec_supply_in$GCAM_subregion),]
names( L154_sR_elec_supply_in )[ names( L154_sR_elec_supply_in ) == "X1990" ] <- "tot_inX1990"
names( L154_sR_elec_supply_in )[ names( L154_sR_elec_supply_in ) == "X2005" ] <- "tot_inX2005"
L154_sR_elec_supply <- cbind(L154_sR_elec_supply[,1:4],L154_sR_elec_supply_in[,names(L154_sR_elec_supply_in) == "tot_inX1990" | 
							names(L154_sR_elec_supply_in) == "tot_inX2005"],L154_sR_elec_supply[,5:ncol(L154_sR_elec_supply)])
L154_sR_elec_supply$in1990 <- L154_sR_elec_supply$tot_inX1990*L154_sR_elec_supply$fraction1990
L154_sR_elec_supply$in2005 <- L154_sR_elec_supply$tot_inX2005*L154_sR_elec_supply$fraction2005
L154_sR_elec_supply <- L154_sR_elec_supply[,names(L154_sR_elec_supply) != "objective1990" & names(L154_sR_elec_supply) != "objective2005"]

#Add ID vector to Supply table
L154_sR_elec_supply$ID_sR_T_Y<-paste(L154_sR_elec_supply$GCAM_subregion,L154_sR_elec_supply$technology,sep="")

#write.table(L154_sR_elec_segments,file="Rdata_out/L154_sR_elec_segments.csv",sep=",",col.names=TRUE,row.names=FALSE)
#write.table(L154_sR_elec_supply,file="Rdata_out/L154_sR_elec_supply.csv",sep=",",col.names=TRUE,row.names=FALSE)

#TODO: Check for negative generation numbers and write out a warning if they happen

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L154_sR_elec_segments <- c( "Electricity demand by load segment","Unit = EJ" )
comments.L154_sR_elec_supply<- c( "Electricity supply / fuel / year","Unit = EJ" )
#comments.L152_sR_elec_demand<- c( "Electricity demand / fuel / year","Unit = EJ" )

#write tables as CSV files
writedata( L154_sR_elec_segments,fn="L154_sR_elec_segments", comments=comments.L154_sR_elec_segments )
writedata( L154_sR_elec_supply,fn="L154_sR_elec_supply", comments=comments.L154_sR_elec_supply )
#writedata( L152_sR_elec_demand,fn="L152_sR_elec_demand", comments=comments.L152_sR_elec_demand )

# Every script should finish with this line
logstop()

