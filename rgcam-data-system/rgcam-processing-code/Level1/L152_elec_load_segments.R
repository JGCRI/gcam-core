# L152_elec_load_segments.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L152_elec_load_segments.R" )
printlog( "Elec load segments" )

# -----------------------------------------------------------------------------
# 1. Read files

USA_elec_segments <- readdata( "USA_elec_segments" )
USA_demand_fraction <- readdata( "USA_demand_fraction" )
L107_out_EJ_sR_elec_F <- readdata( "L107_out_EJ_sR_elec_F" )

# -----------------------------------------------------------------------------
# 2. Perform computations

#USA_generation_to_demand<-read.table("Inventory Data/Rdata_in/USA_generation_to_demand.csv",header=T,sep=',',na.strings="..")
#USA_demand_fraction is the fraction of demand supplied by baseload generation, int. generation, etc. 
#These fractions are calculated from the ratio of values in Y12:Y15 in Electric Sector Inputs:gen_cal_input

#Aggregate state level generation up to subregion
USA_elec_segments<-USA_elec_segments[order(USA_elec_segments$fuel) & USA_elec_segments$fuel != "",]
#USA_generation_to_demand<-USA_generation_to_demand[USA_generation_to_demand$X != "",]

L152_sR_elec_supply <-merge(L107_out_EJ_sR_elec_F, USA_elec_segments, by.x = "GCAM_fuel", by.y="fuel")
L152_sR_elec_supply <- L152_sR_elec_supply[,c("GCAM_subregion", "GCAM_fuel","technology","segment","X1990","X2005","fraction1990", "fraction2005")]
L152_sR_elec_supply <- L152_sR_elec_supply[order(L152_sR_elec_supply$GCAM_subregion),]


#Total Subregional Supply By Elec Generation Segment and Technology using US level fractions by technology
L152_sR_elec_supply$gen1990 <- L152_sR_elec_supply$X1990*L152_sR_elec_supply$fraction1990
L152_sR_elec_supply$gen2005 <- L152_sR_elec_supply$X2005*L152_sR_elec_supply$fraction2005

#Aggregate all technologies to get total subregional supply by generation segment
L152_sR_elec_segments <- aggregate(L152_sR_elec_supply[,names(L152_sR_elec_supply) == "gen1990" | names(L152_sR_elec_supply) == "gen2005" ],
								list(subregion = L152_sR_elec_supply$GCAM_subregion, segment = L152_sR_elec_supply$segment),sum)
L152_sR_elec_segments <- L152_sR_elec_segments[order(L152_sR_elec_segments$subregion),]

#Total Demand By Elec Segment
L152_sR_elec_demand <- aggregate(L152_sR_elec_segments[,3:4], list(subregion = L152_sR_elec_segments$subregion),sum)
L152_sR_elec_demand <- L152_sR_elec_demand[rep(1:nrow(L152_sR_elec_demand),times=4),]
L152_sR_elec_demand <- L152_sR_elec_demand[order(L152_sR_elec_demand$subregion),]
L152_sR_elec_segments$tot_demand1990 <-L152_sR_elec_demand$gen1990
L152_sR_elec_segments$tot_demand2005 <-L152_sR_elec_demand$gen2005
L152_sR_elec_segments$demand_fraction <- USA_demand_fraction$Demand_Fraction[match(L152_sR_elec_segments$segment,USA_demand_fraction$Vsegment)]
L152_sR_elec_segments$demand1990 <- L152_sR_elec_segments$demand_fraction*L152_sR_elec_segments$tot_demand1990
L152_sR_elec_segments$demand2005 <- L152_sR_elec_segments$demand_fraction*L152_sR_elec_segments$tot_demand2005

#Check that supply meets demand
L152_sR_elec_segments$check1990 <- L152_sR_elec_segments$demand1990 - L152_sR_elec_segments$gen1990
L152_sR_elec_segments$check2005 <- L152_sR_elec_segments$demand2005 - L152_sR_elec_segments$gen2005
L152_sR_elec_segments$pct1990 <- L152_sR_elec_segments$check1990/L152_sR_elec_segments$gen1990
L152_sR_elec_segments$pct2005 <- L152_sR_elec_segments$check2005/L152_sR_elec_segments$gen2005

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L152_sR_elec_segments <- c( "Electricity demand by load segment","Unit = EJ" )
comments.L152_sR_elec_supply <- c( "Electricity supply / fuel / year","Unit = EJ" )
comments.L152_sR_elec_demand <- c( "Electricity demand / fuel / year","Unit = EJ" )

#write tables as CSV files
writedata( L152_sR_elec_segments,fn="L152_sR_elec_segments", comments=comments.L152_sR_elec_segments )
writedata( L152_sR_elec_supply,fn="L152_sR_elec_supply", comments=comments.L152_sR_elec_supply )
writedata( L152_sR_elec_demand,fn="L152_sR_elec_demand", comments=comments.L152_sR_elec_demand )

# Every script should finish with this line
logstop()

