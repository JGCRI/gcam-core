# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "GCAMUSAPROC_DIR" ) ){
    if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
        GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "LA122.Refining.R" )
printlog( "Refining sector inputs and outputs, industrial sector inputs, and industrial cogeneration output" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
L122.in_EJ_R_refining_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L122.in_EJ_R_refining_F_Yh" )
L122.out_EJ_R_refining_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L122.out_EJ_R_refining_F_Yh" )
L101.inEIA_EJ_state_S_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L101.inEIA_EJ_state_S_F" )

EIA_biodiesel_Mgal.yr <- readdata( "GCAMUSA_LEVEL0_DATA", "EIA_biodiesel_Mgal.yr" )



# -----------------------------------------------------------------------------

# 2. Perform computations

#CRUDE OIL REFINING

printlog( "NOTE: using SEDS crude oil input to industry as basis for allocation of crude oil refining to states" )

#Crude oil consumption by industry is the energy used at refineries (input - output)

L122.net_EJ_state_cor_crude <- subset( L101.inEIA_EJ_state_S_F, sector == "industry" & fuel == "crude oil" )

L122.net_EJ_state_cor_crude$sector <- "oil refining"



#Calculate the percentages in each state

L122.pct_state_cor <- data.frame(
      L122.net_EJ_state_cor_crude[ state_S_F ],

      sweep( L122.net_EJ_state_cor_crude[ X_historical_years ], 2,
             colSums( L122.net_EJ_state_cor_crude[ X_historical_years ] ), "/" ) )



printlog( "Crude oil refining output by state" )

#Apportion the national total to the states
L122.out_EJ_state_cor <- apportion_to_states(
      nation_data = subset( L122.out_EJ_R_refining_F_Yh, GCAM_region_ID == USA_regID & sector == "oil refining" ),
      state_share_data = L122.pct_state_cor,
      match_vectors = "sector" )


printlog( "Crude oil refining inputs by state and fuel" )

#Inputs to crude oil refining - same method of portional allocations, but with multiple fuels

L122.pct_state_cor_repF <- repeat_and_add_vector( L122.pct_state_cor, "fuel",
      unique(L122.in_EJ_R_refining_F_Yh$fuel[ L122.in_EJ_R_refining_F_Yh$sector == "oil refining"] ) )

L122.in_EJ_state_cor_F <- apportion_to_states(
      nation_data = subset( L122.in_EJ_R_refining_F_Yh, GCAM_region_ID == USA_regID & sector == "oil refining" ),
      state_share_data = L122.pct_state_cor_repF,
      match_vectors = S_F )

#BIOMASS LIQUIDS

printlog( "NOTE: using SEDS biofuel transformation-related losses to disaggregate ethanol production to states" )
#Calculate the percentages of net energy losses from biofuel production in each state
L122.net_EJ_state_btl <- subset( L101.inEIA_EJ_state_S_F, sector == "corn ethanol" )
L122.pct_state_btle <- data.frame(
      L122.net_EJ_state_btl[ state_S_F ],
      sweep( L122.net_EJ_state_btl[ X_historical_years ], 2,
             colSums( L122.net_EJ_state_btl[ X_historical_years ] ), "/" ) )
L122.pct_state_btle[ is.na( L122.pct_state_btle ) ] <- 0


printlog( "Corn ethanol output by state" )
L122.out_EJ_state_btle <- apportion_to_states(
      nation_data = subset( L122.out_EJ_R_refining_F_Yh, GCAM_region_ID == USA_regID & sector == "corn ethanol" ),
      state_share_data = L122.pct_state_btle,
      match_vectors = "sector" )
      

printlog( "Corn ethanol inputs by state and fuel" )

#Repeat percentage-wise table by number of fuel inputs
L122.pct_state_btle_repF <- repeat_and_add_vector( L122.pct_state_btle, "fuel",
      unique(L122.in_EJ_R_refining_F_Yh$fuel[ L122.in_EJ_R_refining_F_Yh$sector == "corn ethanol"] ) )
L122.in_EJ_state_btle_F <- apportion_to_states(
      nation_data = subset( L122.in_EJ_R_refining_F_Yh, GCAM_region_ID == USA_regID & sector == "corn ethanol" ),
      state_share_data = L122.pct_state_btle_repF,
      match_vectors = S_F )


printlog( "Biodiesel output by state" )
printlog( "NOTE: SEDS does not cover biodiesel; using a separate EIA database for disaggregating this to states")

#Build table of percentages by historical year
EIA_biodiesel_Mgal.yr$pct <- EIA_biodiesel_Mgal.yr$Mgal.yr / sum( EIA_biodiesel_Mgal.yr$Mgal.yr )
L122.pct_state_btlbd <- data.frame( state = states, sector = "biodiesel", fuel = "biomass oil" )
L122.pct_state_btlbd[ X_historical_years ] <- EIA_biodiesel_Mgal.yr$pct[
      match( L122.pct_state_btlbd$state, EIA_biodiesel_Mgal.yr$state ) ]
L122.pct_state_btlbd[ is.na( L122.pct_state_btlbd ) ] <- 0

#Apportion to the states
L122.out_EJ_state_btlbd <- apportion_to_states(
      nation_data = subset( L122.out_EJ_R_refining_F_Yh, GCAM_region_ID == USA_regID & sector == "biodiesel" ),
      state_share_data = L122.pct_state_btlbd,
      match_vectors = "sector" )

printlog( "Biodiesel inputs by state and fuel" )

L122.pct_state_btlbd_repF <- repeat_and_add_vector( L122.pct_state_btlbd, "fuel",
      unique( L122.in_EJ_R_refining_F_Yh$fuel[ L122.in_EJ_R_refining_F_Yh$sector == "biodiesel" ] ) )
L122.in_EJ_state_btlbd_F <- apportion_to_states(
      nation_data = subset( L122.in_EJ_R_refining_F_Yh, GCAM_region_ID == USA_regID & sector == "biodiesel" ),
      state_share_data = L122.pct_state_btlbd_repF,
      match_vectors = S_F )


#rbind the tables of inputs and outputs of all refineries by state in the base years

L122.in_EJ_state_refining_F <- rbind( L122.in_EJ_state_cor_F, L122.in_EJ_state_btle_F, L122.in_EJ_state_btlbd_F )

L122.out_EJ_state_refining_F <- rbind( L122.out_EJ_state_cor, L122.out_EJ_state_btle, L122.out_EJ_state_btlbd )





# -----------------------------------------------------------------------------

# 3. Output

#Add comments for each table

comments.L122.in_EJ_state_refining_F <- c( "Refinery energy inputs by state, technology, and fuel","Unit = EJ" )

comments.L122.out_EJ_state_refining_F <- c( "Refinery output by state and technology","Unit = EJ" )



#write tables as CSV files

writedata( L122.in_EJ_state_refining_F, domain="GCAMUSA_LEVEL1_DATA", fn="L122.in_EJ_state_refining_F", comments=comments.L122.in_EJ_state_refining_F )

writedata( L122.out_EJ_state_refining_F, domain="GCAMUSA_LEVEL1_DATA", fn="L122.out_EJ_state_refining_F", comments=comments.L122.out_EJ_state_refining_F )



# Every script should finish with this line

logstop()

