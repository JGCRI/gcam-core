# L153_elec_load_segments_solver_2005.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L153_elec_load_segments_solver_2005.R" )
printlog( "Elec load segments solver for 2005" )

# -----------------------------------------------------------------------------
# 1. Read files

USA_demand_fraction <- readdata( "USA_demand_fraction" )
L152_sR_elec_supply <- readdata( "L152_sR_elec_supply" )
L152_sR_elec_demand <- readdata( "L152_sR_elec_demand" )
L152_sR_elec_segments <- readdata( "L152_sR_elec_segments" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Initialize Variables
L153_sR_elec_supply <- L152_sR_elec_supply
L153_sR_elec_demand <- L152_sR_elec_demand
L153_sR_elec_segments <- L152_sR_elec_segments
L153_sR_elec_supply$objective2005 <- 1
L153_segment_list <- c("base load generation", "intermediate generation", "subpeak generation", "peak generation")
L153_subregion_list <- unique(L153_sR_elec_supply$GCAM_subregion)

#Function to check demands and supplys by segment and L153_region
check_elec_segments <- function (gen_fraction, L153_region, segment, fuel = "gas") {
#set fraction as specified
L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == fuel & L153_sR_elec_supply$segment == segment,
							names(L153_sR_elec_supply) == "fraction2005"] <- gen_fraction
L153_sR_elec_supply$gen2005 <- L153_sR_elec_supply$X2005*L153_sR_elec_supply$fraction2005

#Aggregate all technologies to get total subregional supply by generation segment
L153_sR_elec_segments_tmp <- aggregate(L153_sR_elec_supply[,9:10 ],
							list(subregion = L153_sR_elec_supply$GCAM_subregion, segment = L153_sR_elec_supply$segment),sum)
L153_sR_elec_segments[L153_sR_elec_segments$subregion == L153_region & L153_sR_elec_segments$segment == segment, names(L153_sR_elec_segments) == "gen2005"] <- 
							L153_sR_elec_segments_tmp[L153_sR_elec_segments_tmp$subregion == L153_region & L153_sR_elec_segments_tmp$segment == segment, names(L153_sR_elec_segments_tmp) == "gen2005"]

#Check that supply meets demand
L153_sR_elec_segments$check1990 <- L153_sR_elec_segments$demand1990 - L153_sR_elec_segments$gen1990
L153_sR_elec_segments$check2005 <- L153_sR_elec_segments$demand2005 - L153_sR_elec_segments$gen2005
L153_sR_elec_segments$pct1990 <- L153_sR_elec_segments$check1990/L153_sR_elec_segments$gen1990
L153_sR_elec_segments$pct2005 <- L153_sR_elec_segments$check2005/L153_sR_elec_segments$gen2005

check <- abs(L153_sR_elec_segments[L153_sR_elec_segments$subregion == L153_region & L153_sR_elec_segments$segment == segment,names(L153_sR_elec_segments) == "check2005"])
check
}

#Loop through each subregion
for (j in 1:length(L153_subregion_list)){
L153_region <- L153_subregion_list[j]

#First, if subregion is largely hydro, move some hydro to intermediate
L153_hydro_frac <- L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$segment == "base load generation" & L153_sR_elec_supply$GCAM_fuel == "hydro",
							names(L153_sR_elec_supply) == "X2005"] / L153_sR_elec_segments[L153_sR_elec_segments$subregion == L153_region & 
							L153_sR_elec_segments$segment == "base load generation",names(L153_sR_elec_segments) == "tot_demand2005"]
if (L153_hydro_frac > 0.4) {
	L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "hydro" & 
		L153_sR_elec_supply$segment == L153_segment_list[1],names(L153_sR_elec_supply) == "fraction2005"] <- 0.75*L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region 
		& L153_sR_elec_supply$GCAM_fuel == "hydro" & L153_sR_elec_supply$segment == L153_segment_list[1],names(L153_sR_elec_supply) == "fraction2005"]
	L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "hydro" & 
		L153_sR_elec_supply$segment == L153_segment_list[2],names(L153_sR_elec_supply) == "fraction2005"] <- 1 - L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region 
		& L153_sR_elec_supply$GCAM_fuel == "hydro" & L153_sR_elec_supply$segment == L153_segment_list[1],names(L153_sR_elec_supply) == "fraction2005"]
}

#Loop to solve for generation fractions for each generation segment
for (i in 1:4) {
if (i != 4) {
	L153_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-4, L153_region, L153_segment_list[i])
	L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment == L153_segment_list[i],
							names(L153_sR_elec_supply) == "fraction2005"] <- L153_solved_fraction$minimum
	L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment == L153_segment_list[i],
							names(L153_sR_elec_supply) == "objective2005"] <- L153_solved_fraction$objective
	} else {
	L153_nonpeak <- sum(L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment != L153_segment_list[i],
							names(L153_sR_elec_supply) == "fraction2005"] )
	L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment == L153_segment_list[i],
							names(L153_sR_elec_supply) == "fraction2005"] <- 1 - L153_nonpeak

	}}
	
	
	
#If there wasn't enough gas to solve, make more adjustments#

	if (L153_nonpeak > 1)  {
	#If a L153_region had a lot of oil, move some to intermediate
		L153_oil_frac <- L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$segment == "base load generation" & L153_sR_elec_supply$GCAM_fuel == "refined liquids",
							names(L153_sR_elec_supply) == "X2005"] / L153_sR_elec_segments[L153_sR_elec_segments$subregion == L153_region & 
							L153_sR_elec_segments$segment == "base load generation",names(L153_sR_elec_segments) == "tot_demand2005"]
		if (L153_oil_frac > 0.09) {
		L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "refined liquids" & 
			L153_sR_elec_supply$segment == L153_segment_list[2],names(L153_sR_elec_supply) == "fraction2005"] <- 0.5
		L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "refined liquids" & 
			L153_sR_elec_supply$segment == L153_segment_list[3],names(L153_sR_elec_supply) == "fraction2005"] <- 0.25
		L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "refined liquids" & 
			L153_sR_elec_supply$segment == L153_segment_list[4],names(L153_sR_elec_supply) == "fraction2005"] <- 0.25
			
			for (i in 1:4) {

				if (i != 4) {
			L153_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-4, L153_region, L153_segment_list[i])
			L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment == L153_segment_list[i],
							names(L153_sR_elec_supply) == "fraction2005"] <- L153_solved_fraction$minimum
			L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment == L153_segment_list[i],
							names(L153_sR_elec_supply) == "objective2005"] <- L153_solved_fraction$objective
				} else {
				L153_nonpeak <- sum(L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment != L153_segment_list[i],
							names(L153_sR_elec_supply) == "fraction2005"] )
				L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment == L153_segment_list[i],
							names(L153_sR_elec_supply) == "fraction2005"] <- 1 - L153_nonpeak
				}
			}
	     }
	}
	
	
	if (L153_nonpeak > 1)  {
	#!--------------------------------------------
	#Change coal shares
	L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & 
		L153_sR_elec_supply$segment == L153_segment_list[1],names(L153_sR_elec_supply) == "fraction2005"] <- 0.05*L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region 
		& L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment == L153_segment_list[1],names(L153_sR_elec_supply) == "fraction2005"]
	L153_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-4, L153_region, L153_segment_list[1], "coal")
	L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "coal" & 
		L153_sR_elec_supply$segment == L153_segment_list[1],names(L153_sR_elec_supply) == "fraction2005"] <- L153_solved_fraction$minimum
	L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "coal" & 
		L153_sR_elec_supply$segment == L153_segment_list[2],names(L153_sR_elec_supply) == "fraction2005"] <- .77*(1- L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region 
		& L153_sR_elec_supply$GCAM_fuel == "coal" & L153_sR_elec_supply$segment == L153_segment_list[1],names(L153_sR_elec_supply) == "fraction2005"])
	L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "coal" & 
		L153_sR_elec_supply$segment == L153_segment_list[3],names(L153_sR_elec_supply) == "fraction2005"] <- .23*(1- L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region 
		& L153_sR_elec_supply$GCAM_fuel == "coal" & L153_sR_elec_supply$segment == L153_segment_list[1],names(L153_sR_elec_supply) == "fraction2005"])
	for (i in 1:4) {

		if (i != 4) {
		L153_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-4, L153_region, L153_segment_list[i])
		L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment == L153_segment_list[i],
							names(L153_sR_elec_supply) == "fraction2005"] <- L153_solved_fraction$minimum
		L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment == L153_segment_list[i],
							names(L153_sR_elec_supply) == "objective2005"] <- L153_solved_fraction$objective
		} else {
		L153_nonpeak <- sum(L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment != L153_segment_list[i],
							names(L153_sR_elec_supply) == "fraction2005"] )
		L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment == L153_segment_list[i],
							names(L153_sR_elec_supply) == "fraction2005"] <- 1 - L153_nonpeak
		}
	}
	}
	
#If we are still off, try adjusting oil consumption
if (L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "gas" & L153_sR_elec_supply$segment == L153_segment_list[3],
							names(L153_sR_elec_supply) == "objective2005"] > .0001)  {
							
		for (i in 3:4) {

		if (i != 4) {
		L153_solved_fraction <- optimize(check_elec_segments, c(0,1), tol=1e-5, L153_region, L153_segment_list[i], "refined liquids")
		L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "refined liquids" & L153_sR_elec_supply$segment == L153_segment_list[i],
							names(L153_sR_elec_supply) == "fraction2005"] <- L153_solved_fraction$minimum
		L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "refined liquids" & L153_sR_elec_supply$segment == L153_segment_list[i],
							names(L153_sR_elec_supply) == "objective2005"] <- L153_solved_fraction$objective
		} else {
		L153_nonpeak <- sum(L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "refined liquids" & L153_sR_elec_supply$segment != L153_segment_list[i],
							names(L153_sR_elec_supply) == "fraction2005"] )
		L153_sR_elec_supply[L153_sR_elec_supply$GCAM_subregion == L153_region & L153_sR_elec_supply$GCAM_fuel == "refined liquids" & L153_sR_elec_supply$segment == L153_segment_list[i],
							names(L153_sR_elec_supply) == "fraction2005"] <- 1 - L153_nonpeak
		}
	}
}

}	
#Recalculate demands

##Total Subregional Supply By Elec Generation Segment and Technology using US level fractions by technology
L153_sR_elec_supply$gen1990 <- L153_sR_elec_supply$X1990*L153_sR_elec_supply$fraction1990
L153_sR_elec_supply$gen2005 <- L153_sR_elec_supply$X2005*L153_sR_elec_supply$fraction2005

##Aggregate all technologies to get total subregional supply by generation segment
L153_sR_elec_segments <- aggregate(L153_sR_elec_supply[,9:10 ],
								list(subregion = L153_sR_elec_supply$GCAM_subregion, segment = L153_sR_elec_supply$segment),sum)
L153_sR_elec_segments <- L153_sR_elec_segments[order(L153_sR_elec_segments$subregion),]

##Total Demand By Elec Segment
L153_sR_elec_demand <- aggregate(L153_sR_elec_segments[,3:4], list(subregion = L153_sR_elec_segments$subregion),sum)
L153_sR_elec_demand <- L153_sR_elec_demand[rep(1:nrow(L153_sR_elec_demand),times=4),]
L153_sR_elec_demand <- L153_sR_elec_demand[order(L153_sR_elec_demand$subregion),]
L153_sR_elec_segments$tot_demand1990 <-L153_sR_elec_demand$gen1990
L153_sR_elec_segments$tot_demand2005 <-L153_sR_elec_demand$gen2005
L153_sR_elec_segments$demand_fraction <- USA_demand_fraction$Demand_Fraction[match(L153_sR_elec_segments$segment,USA_demand_fraction$Vsegment)]
L153_sR_elec_segments$demand1990 <- L153_sR_elec_segments$demand_fraction*L153_sR_elec_segments$tot_demand1990
L153_sR_elec_segments$demand2005 <- L153_sR_elec_segments$demand_fraction*L153_sR_elec_segments$tot_demand2005

##Check that supply meets demand
L153_sR_elec_segments$check1990 <- L153_sR_elec_segments$demand1990 - L153_sR_elec_segments$gen1990
L153_sR_elec_segments$check2005 <- L153_sR_elec_segments$demand2005 - L153_sR_elec_segments$gen2005
L153_sR_elec_segments$pct1990 <- L153_sR_elec_segments$check1990/L153_sR_elec_segments$gen1990
L153_sR_elec_segments$pct2005 <- L153_sR_elec_segments$check2005/L153_sR_elec_segments$gen2005



#example of converting a table of quantities to percentages of total
#tmp <- L153_sR_elec_segments[ 1:20, 1:10]
#numerical_columns <- c( "gen1990", "gen2005" )
#tmp_fracs <- data.frame( tmp[ , 1:2 ],
#      sweep( tmp[ numerical_columns ], 2, colSums( tmp[ numerical_columns ] ), "/" ) )
      
#check to make sure that it worked
#apply( tmp_fracs[ numerical_columns ], 2, sum )

# Do we need this? L153_sR_elec_supply already has fractions by tech and subregion    

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L153_sR_elec_segments <- c( "Electricity demand by load segment","Unit = EJ" )
comments.L153_sR_elec_supply<- c( "Electricity supply / fuel / year","Unit = EJ" )
#comments.L152_sR_elec_demand<- c( "Electricity demand / fuel / year","Unit = EJ" )

#write tables as CSV files
writedata( L153_sR_elec_segments,fn="L153_sR_elec_segments", comments=comments.L153_sR_elec_segments )
writedata( L153_sR_elec_supply,fn="L153_sR_elec_supply", comments=comments.L153_sR_elec_supply )
#writedata( L152_sR_elec_demand,fn="L152_sR_elec_demand", comments=comments.L152_sR_elec_demand )

# Every script should finish with this line
logstop()

