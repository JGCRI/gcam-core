
if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "L144.det_glbl_bld.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical building sector data" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
A44.HouseholdSize <- readdata( "ENERGY_ASSUMPTIONS", "A44.HouseholdSize" )
flsp_mapping <- readdata( "ENERGY_MAPPINGS", "flsp_mapping" )
IEA_PCResFloorspace <- readdata( "ENERGY_LEVEL0_DATA", "IEA_PCResFloorspace" )
Odyssee_ResFloorspacePerHouse <- readdata( "ENERGY_LEVEL0_DATA", "Odyssee_ResFloorspacePerHouse" )
L100.Pop_thous_ctry_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L100.Pop_thous_ctry_Yh" )
L142.in_EJ_R_bld_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L142.in_EJ_R_bld_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
## FLOORSPACE CALCULATION
#First, fill out the household size to all relevant years
Odyssee_flsp_years <- 1980:2009
X_Odyssee_flsp_years <- paste0( "X", Odyssee_flsp_years )
L144.HouseholdSize <- gcam_interp( A44.HouseholdSize, Odyssee_flsp_years, rule = 2 )    #use rule = 2 to fill out 

#Drop the Odyssee countries that we have estimates for in the IEA dataset (IEA assumed to be better as it presents per-capita flsp)
L144.Odyssee_phflsp_Yh <- subset( Odyssee_ResFloorspacePerHouse, !iso %in% IEA_PCResFloorspace$iso )
L144.Odyssee_pcflsp_Yh <- L144.Odyssee_phflsp_Yh
L144.Odyssee_pcflsp_Yh[ X_Odyssee_flsp_years ] <- L144.Odyssee_phflsp_Yh[ X_Odyssee_flsp_years ] /
      L144.HouseholdSize[rep( 1, times = nrow( L144.Odyssee_phflsp_Yh ) ), X_Odyssee_flsp_years ]

IEA_flsp_years <- 1980:2004
X_IEA_flsp_years <- paste0( "X", IEA_flsp_years )
L144.OECD_pcflsp_Yh <- rbind( L144.Odyssee_pcflsp_Yh[ c( "iso", X_IEA_flsp_years ) ], IEA_PCResFloorspace[ c( "iso", X_IEA_flsp_years ) ] )
L144.OECD_pcflsp_Yh <- L144.OECD_pcflsp_Yh[ order( L144.OECD_pcflsp_Yh$iso ), ]
#Drop any countries with all missing values
L144.OECD_pcflsp_Yh <- subset( L144.OECD_pcflsp_Yh, rowSums( L144.OECD_pcflsp_Yh[ X_IEA_flsp_years ], na.rm = T ) != 0 )

#Fill out missing values in specified countries
L144.OECD_pcflsp_Yh$X1980[ L144.OECD_pcflsp_Yh$iso == "usa" ] <- 49.5       #Derived from RECS; see RGCAM data system for documentation
L144.OECD_pcflsp_Yh$X1990[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ] <- L144.OECD_pcflsp_Yh$X1991[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ]
L144.OECD_pcflsp_Yh$X1990[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ] <- L144.OECD_pcflsp_Yh$X1992[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ]
L144.OECD_pcflsp_Yh$X1990[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ] <- L144.OECD_pcflsp_Yh$X1995[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ]
L144.OECD_pcflsp_Yh$X1990[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ] <- L144.OECD_pcflsp_Yh$X1996[ is.na( L144.OECD_pcflsp_Yh$X1990 ) ]

#Calculate average 1980-1990 growth rates for countries with 1980 data. Apply this to the 1990 data to return estimated 1980 floorspace
growthrate_1980_1990 <- sum( L144.OECD_pcflsp_Yh$X1990[ !is.na( L144.OECD_pcflsp_Yh$X1980 ) ] ) / sum( L144.OECD_pcflsp_Yh$X1980[ !is.na( L144.OECD_pcflsp_Yh$X1980 ) ] )
L144.OECD_pcflsp_Yh$X1980[ is.na( L144.OECD_pcflsp_Yh$X1980 ) ] <- L144.OECD_pcflsp_Yh$X1990[ is.na( L144.OECD_pcflsp_Yh$X1980 ) ] / growthrate_1980_1990

#Fill out australia and belgium 2004 data
L144.OECD_pcflsp_Yh$X2004[ L144.OECD_pcflsp_Yh$iso == "aus" ] <- L144.OECD_pcflsp_Yh$X2004[ L144.OECD_pcflsp_Yh$iso == "usa" ] / 
      L144.OECD_pcflsp_Yh$X1998[ L144.OECD_pcflsp_Yh$iso == "usa" ] * L144.OECD_pcflsp_Yh$X1998[ L144.OECD_pcflsp_Yh$iso == "aus" ]
L144.OECD_pcflsp_Yh$X2004[ L144.OECD_pcflsp_Yh$iso == "bel" ] <- L144.OECD_pcflsp_Yh$X2004[ L144.OECD_pcflsp_Yh$iso == "fra" ] / 
      L144.OECD_pcflsp_Yh$X2001[ L144.OECD_pcflsp_Yh$iso == "fra" ] * L144.OECD_pcflsp_Yh$X2001[ L144.OECD_pcflsp_Yh$iso == "bel" ]

#Interpolate and extrapolate the time series to all historical years
interp_years_early <- 1981:1989
L144.OECD_pcflsp_Yh <- gcam_interp( L144.OECD_pcflsp_Yh, interp_years_early )
interp_years_later <- 1991:2003
L144.OECD_pcflsp_Yh <- gcam_interp( L144.OECD_pcflsp_Yh, interp_years_later )

#Time series doesn't span entire "historical" range; need to extrapolate

#Apply these estimates of per-capita floorspace to remaining countries in the world, using mapping assumptions

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L144.OECD_pcflsp_Yh <- c( "Building energy consumption by GCAM region / fuel / historical year","Unit = EJ" )

#write tables as CSV files
writedata( L144.OECD_pcflsp_Yh, domain="ENERGY_LEVEL1_DATA", fn="L144.OECD_pcflsp_Yh", comments=comments.L144.OECD_pcflsp_Yh )

# Every script should finish with this line
logstop()
