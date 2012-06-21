# L107_Subregions_elec.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L107_Subregions_elec.R" )
printlog( "Sum electricity into subregions" )

# -----------------------------------------------------------------------------
# 1. Read files

states_subregions <- readdata( "states_subregions" )
L103_in_EJ_state_elec_F <- readdata( "L103_in_EJ_state_elec_F" )
L103_out_EJ_state_elec_F <- readdata( "L103_out_EJ_state_elec_F" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# aggregate input tables
L107_in_EJ_state_elec_F <- L103_in_EJ_state_elec_F
L107_in_EJ_state_elec_F$GCAM_subregion<-states_subregions$subregion_FERC[match(L107_in_EJ_state_elec_F$state,states_subregions$state)]
L107_in_EJ_sR_elec_F<-aggregate(L107_in_EJ_state_elec_F[,4:11],list(GCAM_subregion = L107_in_EJ_state_elec_F$GCAM_subregion,GCAM_sector = L107_in_EJ_state_elec_F$GCAM_sector,GCAM_fuel = L107_in_EJ_state_elec_F$GCAM_fuel),sum)
L107_in_EJ_sR_elec_F<-L107_in_EJ_sR_elec_F[order(L107_in_EJ_sR_elec_F$GCAM_subregion,L107_in_EJ_sR_elec_F$GCAM_sector,L107_in_EJ_sR_elec_F$GCAM_fuel),]

# aggregate output tables
L107_out_EJ_state_elec_F <- L103_out_EJ_state_elec_F
L107_out_EJ_state_elec_F$GCAM_subregion <- states_subregions$subregion_FERC[match(L107_out_EJ_state_elec_F$state,states_subregions$state)]
L107_out_EJ_sR_elec_F<-aggregate(L107_out_EJ_state_elec_F[,4:11],list(GCAM_subregion = L107_out_EJ_state_elec_F$GCAM_subregion,GCAM_sector = L107_out_EJ_state_elec_F$GCAM_sector,GCAM_fuel = L107_out_EJ_state_elec_F$GCAM_fuel),sum)
L107_out_EJ_sR_elec_F<-L107_out_EJ_sR_elec_F[order(L107_out_EJ_sR_elec_F$GCAM_subregion,L107_out_EJ_sR_elec_F$GCAM_sector,L107_out_EJ_sR_elec_F$GCAM_fuel),]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L107_in_EJ_sR_elec_F <- c( "Electricity input by subregion / fuel / year","Unit = EJ" )
comments.L107_out_EJ_sR_elec_F <- c( "Electricity output by subregion / fuel / year","Unit = EJ" )

#write tables as CSV files
writedata( L107_in_EJ_sR_elec_F,fn="L107_in_EJ_sR_elec_F", comments=comments.L107_in_EJ_sR_elec_F )
writedata( L107_out_EJ_sR_elec_F,fn="L107_out_EJ_sR_elec_F", comments=comments.L107_out_EJ_sR_elec_F )

# Every script should finish with this line
logstop()

