if( !exists( "SOCIOPROC_DIR" ) ){
    if( Sys.getenv( "SOCIOPROC" ) != "" ){
        SOCIOPROC_DIR <- Sys.getenv( "SOCIOPROC" )
    } else {
        stop("Could not determine location of socioeconomics processing scripts, please set the R var SOCIOPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
logstart( "L100.Population_downscale_ctry.R" )
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
printlog( "Historical (from 1500) population by country" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
socioeconomics_ctry <- readdata( "SOCIO_MAPPINGS", "socioeconomics_ctry" )
Maddison_population <- readdata( "SOCIO_LEVEL0_DATA", "Maddison_population" )
SSP_database_v9 <- readdata( "SOCIO_LEVEL0_DATA", "SSP_database_v9" )
UN_popTot <- readdata( "SOCIO_LEVEL0_DATA", "UN_popTot" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Part 1: Downscaling Maddison population data to all countries with modern iso codes")
#First, subset the years that will be used in this analysis (many are missing in the database, and filling them out would be tedious)
X_Maddison_years <- names( Maddison_population[ grepl( "X[0-9]{4}", names( Maddison_population ) ) ] )
Maddison_years <- as.numeric( substr( X_Maddison_years, 2, 5 ) )

#Downscale countries that split over time
L100.Maddison_population <- subset( Maddison_population, !is.na( Maddison_population$Country ) )
L100.Maddison_population$iso <- socioeconomics_ctry$iso[ match( L100.Maddison_population$Country, socioeconomics_ctry$Maddison_ctry ) ]
#Czechoslovakia
iso_cze <- socioeconomics_ctry$iso[ !is.na( socioeconomics_ctry$Downscale_from ) & socioeconomics_ctry$Downscale_from == "Czechoslovakia" ]
L100.Maddison_population <- downscale_Maddison_country( L100.Maddison_population, "Czechoslovakia", iso_cze, 1950 )
#USSR
iso_ussr <- socioeconomics_ctry$iso[ !is.na( socioeconomics_ctry$Downscale_from ) & socioeconomics_ctry$Downscale_from == "Total Former USSR" ]
L100.Maddison_population <- downscale_Maddison_country( L100.Maddison_population, "Total Former USSR", iso_ussr, 1950 )
#Yugoslavia
iso_yug <- socioeconomics_ctry$iso[ !is.na( socioeconomics_ctry$Downscale_from ) & socioeconomics_ctry$Downscale_from == "Yugoslavia" ]
L100.Maddison_population <- downscale_Maddison_country( L100.Maddison_population, "Yugoslavia", iso_yug, 1950 )

#Subset only the rows with assigned iso codes
L100.Pop_Maddison <- subset( L100.Maddison_population, !is.na( L100.Maddison_population$iso ) )

#Interpolate any of the desired historical years that are not in the dataset
L100.Pop_Maddison <- gcam_interp( L100.Pop_Maddison, c( Maddison_historical_years, UN_historical_years[1] ) )

#For countries with 1820 as the first available year, set the estimate in 1800 equal to the 1820 estimate
L100.Pop_Maddison$X1800 <- ifelse( is.na( L100.Pop_Maddison$X1800 ), L100.Pop_Maddison$X1820, L100.Pop_Maddison$X1800 )

#Subset only the years for which we want population data over this timeframe. Fill out missing values based on global total
L100.Pop_Maddison_Yhh <- L100.Pop_Maddison[ c( "iso", X_Maddison_historical_years, X_UN_historical_years[1] ) ]
L100.Pop_Maddison_glbl_nomissing <- colSums( L100.Pop_Maddison_Yhh[ c( X_Maddison_historical_years, X_UN_historical_years[1] ) ], na.rm = T )
L100.Pop_Maddison_glbl <- gcam_interp( subset( L100.Maddison_population, Country == "World Total" ), c( Maddison_historical_years, UN_historical_years[1] ) )
L100.Pop_Maddison_glbl <- L100.Pop_Maddison_glbl[ c( X_Maddison_historical_years, X_UN_historical_years[1] ) ]
L100.Pop_Maddison_missing <- data.frame( iso = "all missing", L100.Pop_Maddison_glbl - L100.Pop_Maddison_glbl_nomissing )

#Calculate the shares in each time peiod and multiply through. Do this with a for loop that starts right and moves left
for( i in ( length( X_Maddison_historical_years ) + 1 ): 2 ){
	L100.Pop_Maddison_Yhh[[i]][is.na( L100.Pop_Maddison_Yhh[[i]] ) ] <- L100.Pop_Maddison_missing[[i]] *
	L100.Pop_Maddison_Yhh[[i+1]][is.na( L100.Pop_Maddison_Yhh[[i]] ) ] /
	sum( L100.Pop_Maddison_Yhh[[i+1]][ is.na( L100.Pop_Maddison_Yhh[[i]] ) ] )
}

printlog( "Calculating population ratios from the start of the UN population database")
#Calculate population ratios from the first UN nation at the level of the country
L100.PopRatio_Maddison_Yhh <- L100.Pop_Maddison_Yhh[ c( "iso", X_Maddison_historical_years ) ]
L100.PopRatio_Maddison_Yhh[ X_Maddison_historical_years ] <- L100.Pop_Maddison_Yhh[ X_Maddison_historical_years ] /
      L100.Pop_Maddison_Yhh[[X_UN_historical_years[1]]]

printlog( "Part 2: Applying these ratios to the historical UN population data")
UN_popTot$iso <- tolower( UN_popTot$Country )
UN_popTot$XYear <- paste0( "X", UN_popTot$Year )
L100.Pop_thous_ctry_Yh <- dcast( subset( UN_popTot, Scenario == "EST" ), iso ~ XYear, value.var = "Value" )
#Reset XEA to Taiwan
L100.Pop_thous_ctry_Yh$iso[ L100.Pop_thous_ctry_Yh$iso == "xea" ] <- "twn"      
L100.Pop_thous_ctry_Yh[ X_Maddison_historical_years ] <- L100.Pop_thous_ctry_Yh[[ X_UN_historical_years[1] ]] * L100.PopRatio_Maddison_Yhh[
      match( L100.Pop_thous_ctry_Yh$iso, L100.PopRatio_Maddison_Yhh$iso ),
      X_Maddison_historical_years ]
      
#This leaves some missing values worth changing. Manually set Serbia and Montenegro, and use Indonesia population ratio for East Timor
L100.Pop_thous_ctry_Yh[ L100.Pop_thous_ctry_Yh$iso == "mne", X_Maddison_historical_years ] <-
      L100.Pop_thous_ctry_Yh[ L100.Pop_thous_ctry_Yh$iso == "mne", X_UN_historical_years[1] ] *
      L100.PopRatio_Maddison_Yhh[ L100.PopRatio_Maddison_Yhh$iso == "scg", X_Maddison_historical_years ]
L100.Pop_thous_ctry_Yh[ L100.Pop_thous_ctry_Yh$iso == "srb", X_Maddison_historical_years ] <-
      L100.Pop_thous_ctry_Yh[ L100.Pop_thous_ctry_Yh$iso == "srb", X_UN_historical_years[1] ] *
      L100.PopRatio_Maddison_Yhh[ L100.PopRatio_Maddison_Yhh$iso == "scg", X_Maddison_historical_years ]
L100.Pop_thous_ctry_Yh[ L100.Pop_thous_ctry_Yh$iso == "tls", X_Maddison_historical_years ] <-
      L100.Pop_thous_ctry_Yh[ L100.Pop_thous_ctry_Yh$iso == "tls", X_UN_historical_years[1] ] *
      L100.PopRatio_Maddison_Yhh[ L100.PopRatio_Maddison_Yhh$iso == "idn", X_Maddison_historical_years ]
      
#At this point the remaining mismatched countries are small, so set to zero
L100.Pop_thous_ctry_Yh[ is.na( L100.Pop_thous_ctry_Yh ) ] <- 0          
L100.Pop_thous_ctry_Yh <- L100.Pop_thous_ctry_Yh[ c( "iso", X_Maddison_historical_years, X_UN_historical_years ) ]

printlog( "Part 3: Downscaling SSP scenarios to the modern day iso level" )
X_final_historical_year <- X_UN_historical_years[ length( X_UN_historical_years ) ]

#Subset the SSP scenarios from the appropriate model and drop the version name from the scenario name
L100.pop_mil_SSP_ctry_Yfut <- subset( SSP_database_v9, MODEL == pop_model & VARIABLE == "Population" )
L100.pop_mil_SSP_ctry_Yfut$iso <- tolower( L100.pop_mil_SSP_ctry_Yfut$REGION )
L100.pop_mil_SSP_ctry_Yfut[[Scen]] <- substr( L100.pop_mil_SSP_ctry_Yfut$SCENARIO, 1, 4 )

#Romania is called "rou" in the SSP database. reset this
L100.pop_mil_SSP_ctry_Yfut$iso[ L100.pop_mil_SSP_ctry_Yfut$iso == "rou" ] <- "rom"

#Compute ratios to final historical year in the SSP scenarios
X_SSP_years <- c( X_final_historical_year, X_future_years )
L100.popRatio_SSP_ctry_Yfut <- L100.pop_mil_SSP_ctry_Yfut[ c( Scen, "iso", X_future_years ) ]
L100.popRatio_SSP_ctry_Yfut[ X_future_years ] <- L100.pop_mil_SSP_ctry_Yfut[ X_future_years ] /  L100.pop_mil_SSP_ctry_Yfut[[ X_final_historical_year ]]

#Multiply these ratios by the country-level population data from the UN
L100.Pop_thous_ctry_Yfut <- L100.Pop_thous_ctry_Yh[ c( "iso", X_final_historical_year ) ]
L100.Pop_thous_SSP_ctry_Yfut <- repeat_and_add_vector( L100.Pop_thous_ctry_Yfut, Scen, unique( L100.popRatio_SSP_ctry_Yfut[[Scen]] ) )
L100.Pop_thous_SSP_ctry_Yfut[X_future_years ] <- L100.Pop_thous_SSP_ctry_Yfut[[ X_final_historical_year ]] * L100.popRatio_SSP_ctry_Yfut[
      match(vecpaste( L100.Pop_thous_SSP_ctry_Yfut[ c( Scen, "iso" ) ] ), vecpaste( L100.popRatio_SSP_ctry_Yfut[ c( Scen, "iso" ) ] ) ),
      X_future_years ]

#For countries not covered in the SSPs, set the future population equal to its value in the base year
L100.Pop_thous_SSP_ctry_Yfut[ is.na( L100.Pop_thous_SSP_ctry_Yfut[[ X_future_years[1] ]] ), X_future_years ] <-
      L100.Pop_thous_SSP_ctry_Yfut[[ X_final_historical_year ]][ is.na( L100.Pop_thous_SSP_ctry_Yfut[[ X_future_years[1] ]] ) ]
L100.Pop_thous_SSP_ctry_Yfut <- L100.Pop_thous_SSP_ctry_Yfut[ c( Scen, "iso", X_future_years ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L100.Pop_thous_ctry_Yh <- c( "Population by country over the historical time period","Unit = thous persons" )
comments.L100.Pop_thous_SSP_ctry_Yfut <- c( "Population by SSP and country for future time periods","Unit = thous persons" )

writedata( L100.Pop_thous_ctry_Yh, domain="SOCIO_LEVEL1_DATA", fn="L100.Pop_thous_ctry_Yh", comments=comments.L100.Pop_thous_ctry_Yh )
writedata( L100.Pop_thous_SSP_ctry_Yfut, domain="SOCIO_LEVEL1_DATA", fn="L100.Pop_thous_SSP_ctry_Yfut", comments=comments.L100.Pop_thous_SSP_ctry_Yfut )

# Every script should finish with this line
logstop()
