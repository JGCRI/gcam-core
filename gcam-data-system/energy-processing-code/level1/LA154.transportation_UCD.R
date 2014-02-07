
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
logstart( "LA154.transportation_UCD.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Processing of UC Davis transportation database to GCAM region / sector / mode / size class / technology / fuel / historical year" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A54.UCD_header", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
calibrated_techs_trn_agg <- readdata( "ENERGY_MAPPINGS", "calibrated_techs_trn_agg" )
enduse_fuel_aggregation <- readdata( "ENERGY_MAPPINGS", "enduse_fuel_aggregation" )
UCD_ctry <- readdata( "ENERGY_MAPPINGS", "UCD_ctry" )
UCD_techs <- readdata( "ENERGY_MAPPINGS", "UCD_techs" )
UCD_transportation_database <- readdata( "ENERGY_LEVEL0_DATA", "UCD_transportation_database" )
L101.in_EJ_ctry_trn_Fi_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L101.in_EJ_ctry_trn_Fi_Yh" )
L131.in_EJ_R_Senduse_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L131.in_EJ_R_Senduse_F_Yh" )
L100.Pop_thous_ctry_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L100.Pop_thous_ctry_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Part 1: downscaling country-level transportation energy data to UCD transportation technologies" )
printlog( "NOTE: We are currently aggregating IEA's data on rail and road due to inconsistencies (e.g. no rail in the Middle East)" )
printlog( "To changes this, modify the mappings in ", file_fqn( "ENERGY_MAPPINGS", "calibrated_techs_trn_agg" ) )
L154.in_EJ_ctry_trn_Fi_Yh <- L101.in_EJ_ctry_trn_Fi_Yh
L154.in_EJ_ctry_trn_Fi_Yh$sector <- sub( "in_", "", L101.in_EJ_ctry_trn_Fi_Yh$sector )
L154.in_EJ_ctry_trn_Fi_Yh$UCD_category <- calibrated_techs_trn_agg$UCD_category[
      match( L154.in_EJ_ctry_trn_Fi_Yh$sector, calibrated_techs_trn_agg$sector ) ]
L154.in_EJ_ctry_trn_Fi_Yh <- aggregate( L154.in_EJ_ctry_trn_Fi_Yh[ X_historical_years ],
      by=as.list( L154.in_EJ_ctry_trn_Fi_Yh[ c( "iso", "UCD_category", "fuel" ) ] ),
      sum, na.rm = T )

printlog( "Aggregating UCD transportation database by the general categories used for the IEA transportation data" )
printlog( "These will be used to compute shares for allocation of energy to mode/technology/fuel within category/fuel")
L154.in_PJ_Rucd_trn_m_sz_tech_F <- subset( UCD_transportation_database, variable == "energy" )
L154.in_PJ_Rucd_trn_m_sz_tech_F[ UCD_Cat_F ] <- UCD_techs[
      match( vecpaste( L154.in_PJ_Rucd_trn_m_sz_tech_F[ UCD_techID ] ),
             vecpaste( UCD_techs[ UCD_techID ] ) ),
      UCD_Cat_F ]
L154.in_PJ_Rucd_trn_Cat_F <- aggregate( L154.in_PJ_Rucd_trn_m_sz_tech_F[ X_UCD_en_year ],
      by=as.list( L154.in_PJ_Rucd_trn_m_sz_tech_F[ UCD_R_Cat_F ] ),
      sum )

#Match these energy quantities back into the complete table for computation of shares
L154.in_PJ_Rucd_trn_m_sz_tech_F$UCD_Cat_total <- L154.in_PJ_Rucd_trn_Cat_F[[ X_UCD_en_year ]][
      match( vecpaste( L154.in_PJ_Rucd_trn_m_sz_tech_F[ UCD_R_Cat_F ] ),
             vecpaste( L154.in_PJ_Rucd_trn_Cat_F[ UCD_R_Cat_F ] ) ) ]
L154.in_PJ_Rucd_trn_m_sz_tech_F$share_UCD_Cat <-
      L154.in_PJ_Rucd_trn_m_sz_tech_F[[ X_UCD_en_year ]] /
      L154.in_PJ_Rucd_trn_m_sz_tech_F$UCD_Cat_total

#Zero out missing values where region / category / fuel are zero
L154.in_PJ_Rucd_trn_m_sz_tech_F$share_UCD_Cat[
      is.na( L154.in_PJ_Rucd_trn_m_sz_tech_F$share_UCD_Cat ) ] <- 0
      
printlog( "Writing out the UC Davis mode/technology/fuel shares within categroy/fuel at the country level" )
## NOTE: There is bound to be a better way to do this. Didn't want to use a for loop tho
L154.share_ctry_trn_m_sz_tech_F <- repeat_and_add_vector( L154.in_PJ_Rucd_trn_m_sz_tech_F,
      "iso", sort( unique( L101.in_EJ_ctry_trn_Fi_Yh$iso ) ) )
L154.share_ctry_trn_m_sz_tech_F <- subset( L154.share_ctry_trn_m_sz_tech_F,
      paste( UCD_region, iso ) %in%
      paste( UCD_ctry$UCD_region, UCD_ctry$iso ) )
      
printlog( "Multiplying historical energy by country/category/fuel by shares of country/mode/tech/fuel within country/category/fuel" )
L154.in_EJ_ctry_trn_m_sz_tech_F <- L154.share_ctry_trn_m_sz_tech_F[ c( "iso", UCD_techID, UCD_Cat_F ) ]
L154.in_EJ_ctry_trn_m_sz_tech_F[ X_historical_years ] <-  L154.share_ctry_trn_m_sz_tech_F$share_UCD_Cat *
      L154.in_EJ_ctry_trn_Fi_Yh[
         match( vecpaste( L154.in_EJ_ctry_trn_m_sz_tech_F[ c( "iso", UCD_Cat_F ) ] ),
                vecpaste( L154.in_EJ_ctry_trn_Fi_Yh[ c( "iso", UCD_Cat_F ) ] ) ),
         X_historical_years ]

#Re-set missing values to 0. These are combinations not available in the data from IEA.
L154.in_EJ_ctry_trn_m_sz_tech_F[ X_historical_years ][
      is.na( L154.in_EJ_ctry_trn_m_sz_tech_F[ X_historical_years ] ) ] <- 0

printlog( "Aggregating by GCAM region. These still need to be scaled to match processing of IEA energy balances")
L154.in_EJ_ctry_trn_m_sz_tech_F[[R]] <- iso_GCAM_regID[[R]][
      match( L154.in_EJ_ctry_trn_m_sz_tech_F$iso, iso_GCAM_regID$iso ) ]
L154.in_EJ_R_trn_m_sz_tech_F <- aggregate( L154.in_EJ_ctry_trn_m_sz_tech_F[ X_historical_years ],
      by=as.list( L154.in_EJ_ctry_trn_m_sz_tech_F[ c( R, UCD_techID, "fuel" ) ] ),
      sum )

printlog( "Aggregating by fuel to calculate scalers")
L154.in_EJ_R_trn_F_Yh_unscaled <- aggregate( L154.in_EJ_R_trn_m_sz_tech_F[ X_historical_years ],
      by=as.list( L154.in_EJ_R_trn_m_sz_tech_F[ R_F ] ), sum )
L154.in_EJ_R_Strn_F_Yh <- subset( L131.in_EJ_R_Senduse_F_Yh, grepl( "trn", sector ) )

#Need to use the "aggregate" fuels
L154.in_EJ_R_Strn_F_Yh$fuel <- enduse_fuel_aggregation$trn[
      match( L154.in_EJ_R_Strn_F_Yh$fuel, enduse_fuel_aggregation$fuel ) ]
L154.in_EJ_R_trn_F_Yh <- aggregate( L154.in_EJ_R_Strn_F_Yh[ X_historical_years ],
      by=as.list( L154.in_EJ_R_Strn_F_Yh[ R_F ] ), sum )
L154.scalers_R_trn_F_Yh <- L154.in_EJ_R_trn_F_Yh_unscaled
L154.scalers_R_trn_F_Yh[ X_historical_years ] <- L154.in_EJ_R_trn_F_Yh[
      match( vecpaste( L154.scalers_R_trn_F_Yh[ R_F ] ),
             vecpaste( L154.in_EJ_R_trn_F_Yh[ R_F ] ) ),
      X_historical_years ] /
      L154.in_EJ_R_trn_F_Yh_unscaled[ X_historical_years ]

printlog( "Missing values (NA and NaN) are set to zero. Then the scalers can be multiplied by original estimates" )
L154.scalers_R_trn_F_Yh[ X_historical_years ][ is.na( L154.scalers_R_trn_F_Yh[ X_historical_years ] ) ] <- 0

#Multiplying scalers by original estimates
L154.in_EJ_R_trn_m_sz_tech_F_Yh <- L154.in_EJ_R_trn_m_sz_tech_F
L154.in_EJ_R_trn_m_sz_tech_F_Yh[ X_historical_years ] <- L154.in_EJ_R_trn_m_sz_tech_F[ X_historical_years ] *
      L154.scalers_R_trn_F_Yh[
         match( vecpaste( L154.in_EJ_R_trn_m_sz_tech_F_Yh[ R_F ] ),
                vecpaste( L154.scalers_R_trn_F_Yh[ R_F ] ) ),
         X_historical_years ]

printlog( "Post-scaling missing values indicate energy that is being dropped, and that the database needs to be modified. Checking" )
if( any( !complete.cases( L154.in_EJ_R_trn_m_sz_tech_F_Yh ) ) ){
	tmp <- L154.in_EJ_R_trn_m_sz_tech_F_Yh[ !complete.cases( L154.in_EJ_R_trn_m_sz_tech_F_Yh ), ]
	tmp[ X_historical_years ][ !is.na( tmp[ X_historical_years ] ) ] <- ""
	print( tmp )
	printlog( "Energy is being dropped due to zeroes in the UCD database. Might want to add new techs to the UC Davis database")
	L154.in_EJ_R_trn_m_sz_tech_F_Yh[ is.na( L154.in_EJ_R_trn_m_sz_tech_F_Yh ) ] <- 0
}

printlog( "Part 2: Downscaling of parameters in the UCD database to the country level")
printlog( "2a: Merging of non-fuel costs to assign each technology with a single cost per vkm" )
#NOTE: because "$" is recognized as an operator in R, it can not be used in grepl() to return the costs. Using exact unit names
L154.UCD_trn_cost_data <- subset( UCD_transportation_database, unit %in% c( "2005$/veh/yr", "2005$/veh", "2005$/vkt" ) )
L154.UCD_trn_cost_data.melt <- melt( L154.UCD_trn_cost_data, measure.vars = X_UCD_years, variable_name = "Xyear" )

#Where the unit is $/vkt, the various cost components are ready for aggregation
#Where the unit is $/veh, convert to $/veh/yr using an exogenous fixed charge rate
L154.UCD_trn_cost_data.melt$value[ L154.UCD_trn_cost_data.melt$unit == "2005$/veh" ] <- 
      L154.UCD_trn_cost_data.melt$value[ L154.UCD_trn_cost_data.melt$unit == "2005$/veh" ] * fcr_veh
L154.UCD_trn_cost_data.melt$unit[ L154.UCD_trn_cost_data.melt$unit == "2005$/veh" ] <- "2005$/veh/yr"

#Next, match in the number of km per vehicle per year in order to calculate a levelized cost (per vkm)
L154.UCD_trn_vkm_veh <- subset( UCD_transportation_database, variable == "annual travel per vehicle" )
L154.UCD_trn_cost_data.melt$vkm.veh <- L154.UCD_trn_vkm_veh[[ X_UCD_en_year ]][
      match( vecpaste( L154.UCD_trn_cost_data.melt[ c( "UCD_region", "UCD_sector", "mode", "size.class" ) ] ),
             vecpaste( L154.UCD_trn_vkm_veh[ c( "UCD_region", "UCD_sector", "mode", "size.class" ) ] ) ) ]
L154.UCD_trn_cost_data.melt$value[ L154.UCD_trn_cost_data.melt$unit == "2005$/veh/yr" ] <-
      L154.UCD_trn_cost_data.melt$value[ L154.UCD_trn_cost_data.melt$unit == "2005$/veh/yr" ] /
      L154.UCD_trn_cost_data.melt$vkm.veh[ L154.UCD_trn_cost_data.melt$unit == "2005$/veh/yr" ]
L154.UCD_trn_cost_data.melt$unit[ L154.UCD_trn_cost_data.melt$unit == "2005$/veh/yr" ] <- "2005$/vkt"      
      
L154.UCD_trn_cost_data_agg.melt <- aggregate( L154.UCD_trn_cost_data.melt[ "value" ],
      by=as.list( L154.UCD_trn_cost_data.melt[ c( "UCD_region", UCD_techID, "unit", "Xyear" ) ] ),
      sum )
L154.UCD_trn_cost_data_agg.melt$variable <- "non-fuel costs"
L154.UCD_trn_cost_data_agg <- cast( L154.UCD_trn_cost_data_agg.melt, UCD_region + UCD_sector + mode + size.class +
      UCD_technology + UCD_fuel + variable + unit ~ Xyear, value = "value" )

L154.UCD_trn_data <- rbind(
      subset( UCD_transportation_database, variable %in% c( "intensity", "load factor", "speed" ) ),
      L154.UCD_trn_cost_data_agg )

#Write this out to all years and melt (this step takes a long time)
L154.UCD_trn_data_Y <- gcam_interp( L154.UCD_trn_data,
      years = c( historical_years, future_years ), rule = 2 )
L154.UCD_trn_data_Y.melt <- melt( L154.UCD_trn_data_Y,
      measure.vars = c( X_historical_years, X_future_years ),
      variable_name = "Xyear" )

#Split into different tables, one per variable
L154.UCD_trn_intensity_Y.melt <- subset( L154.UCD_trn_data_Y.melt, variable == "intensity" )
L154.UCD_trn_load_Y.melt <- subset( L154.UCD_trn_data_Y.melt, variable == "load factor" )
L154.UCD_trn_speed_Y.melt <- subset( L154.UCD_trn_data_Y.melt, variable == "speed" )
L154.UCD_trn_nonfuel_Y.melt <- subset( L154.UCD_trn_data_Y.melt, variable == "non-fuel costs" )

#Aggregate the country-level energy consumption by sector and mode. First need to add in the future years for matching purposes
L154.in_EJ_ctry_trn_m_sz_tech_F_Y <- L154.in_EJ_ctry_trn_m_sz_tech_F
L154.in_EJ_ctry_trn_m_sz_tech_F_Y[ X_future_years ] <- L154.in_EJ_ctry_trn_m_sz_tech_F[ X_final_historical_year ]
L154.in_EJ_ctry_trn_m_Y <- aggregate( L154.in_EJ_ctry_trn_m_sz_tech_F_Y[ c( X_historical_years, X_future_years ) ],
      by=as.list( L154.in_EJ_ctry_trn_m_sz_tech_F_Y[ c( "iso", "UCD_sector", "mode" ) ] ), sum )
#This table will be used for the energy weights, applied to each country/sector/mode
L154.in_EJ_ctry_trn_m_Y.melt <- melt( L154.in_EJ_ctry_trn_m_Y,
      measure.vars = c( X_historical_years, X_future_years ),
      variable_name = "Xyear" )

#Also melt the energy table, which has a full technology list. This will be used for matching in parameters.
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt <- melt( L154.in_EJ_ctry_trn_m_sz_tech_F_Y,
      measure.vars = c( X_historical_years, X_future_years ),
      variable_name = "Xyear" )

#Add in the UCD region for matching in the derived variables
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$UCD_region <- UCD_ctry$UCD_region[
      match( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$iso, UCD_ctry$iso ) ]

#The energy weights will be replaced by the energy weights of each mode, as many techs have 0 consumption in the base year
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$value <- NULL
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$weight_EJ <- pmax(
      L154.in_EJ_ctry_trn_m_Y.melt$value[
         match( vecpaste( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt[ c( "iso", "UCD_sector", "mode", "Xyear" ) ] ),
                vecpaste( L154.in_EJ_ctry_trn_m_Y.melt[ c( "iso", "UCD_sector", "mode", "Xyear" ) ] ) ) ],
      min_weight_EJ )     #Using a floor on the weighting factor to avoid having zero weights for any countries

#Next, match in the derived variables, specific to each individual country/sector/mode/size.class/tech/fuel
#Intensity
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$intensity_MJvkm <- L154.UCD_trn_intensity_Y.melt$value[
      match( vecpaste( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt[ c( "UCD_region", UCD_techID, "Xyear" ) ] ),
             vecpaste( L154.UCD_trn_intensity_Y.melt[ c( "UCD_region", UCD_techID, "Xyear" ) ] ) ) ]

#Load factor
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$loadfactor <- L154.UCD_trn_load_Y.melt$value[
      match( vecpaste( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt[ c( "UCD_region", UCD_techID, "Xyear" ) ] ),
             vecpaste( L154.UCD_trn_load_Y.melt[ c( "UCD_region", UCD_techID, "Xyear" ) ] ) ) ]

#Non-fuel costs
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$cost_usdvkm <- L154.UCD_trn_nonfuel_Y.melt$value[
      match( vecpaste( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt[ c( "UCD_region", UCD_techID, "Xyear" ) ] ),
             vecpaste( L154.UCD_trn_nonfuel_Y.melt[ c( "UCD_region", UCD_techID, "Xyear" ) ] ) ) ]

#NOTE - where costs are not read in, missing values will be returned. These can be set to 0
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$cost_usdvkm[ is.na( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$cost_usdvkm ) ] <- 0

#Speed - note that this is matched by the mode and (for some) size class. Match size class first
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$speed_kmhr <- L154.UCD_trn_speed_Y.melt$value[
      match( vecpaste( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt[ c( "UCD_region", UCD_sizeID, "Xyear" ) ] ),
             vecpaste( L154.UCD_trn_speed_Y.melt[ c( "UCD_region", UCD_sizeID, "Xyear" ) ] ) ) ]

#For the missing values, match using the mode ID
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$speed_kmhr[ is.na( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$speed_kmhr ) ] <-
      L154.UCD_trn_speed_Y.melt$value[
          match( vecpaste( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt[
                           is.na( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$speed_kmhr ),
                           c( "UCD_region", UCD_modeID, "Xyear" ) ] ),
                 vecpaste( L154.UCD_trn_speed_Y.melt[ c( "UCD_region", UCD_modeID, "Xyear" ) ] ) ) ]

#For the remaining missing values, set to a default. This should be techs without the time value added (freight)
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$speed_kmhr[ is.na( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt$speed_kmhr ) ] <- 1

#Then, calculate the weighted volumes. No need to convert units because these will be aggregated and divided by weights
#Tvkm = trillion vehicle kilometers
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt <- within( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt, Tvkm <- weight_EJ / intensity_MJvkm )
#Tpkm = trillion passenger (and tonne) kilometers
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt <- within( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt, Tpkm <- Tvkm * loadfactor )
#Tusd = trillion US dollars
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt <- within( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt, Tusd <- Tvkm * cost_usdvkm )
#Thr = trillion hours
L154.ALL_ctry_trn_m_sz_tech_F_Y.melt <- within( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt, Thr <- Tvkm / speed_kmhr )

#Aggregate by GCAM region ID
L154.ALL_R_trn_m_sz_tech_F_Y.melt <- aggregate( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt[
      c( "weight_EJ", "Tvkm", "Tpkm", "Tusd", "Thr" ) ],
      by=as.list( L154.ALL_ctry_trn_m_sz_tech_F_Y.melt[ c( R, UCD_techID, "Xyear" ) ] ),
      sum )
      
#Reverse the calculations to calculate the weighted average of each derived variable
L154.ALL_R_trn_m_sz_tech_F_Y.melt <- within( L154.ALL_R_trn_m_sz_tech_F_Y.melt, intensity_MJvkm <- weight_EJ / Tvkm )
L154.ALL_R_trn_m_sz_tech_F_Y.melt <- within( L154.ALL_R_trn_m_sz_tech_F_Y.melt, loadfactor <- Tpkm / Tvkm )
L154.ALL_R_trn_m_sz_tech_F_Y.melt <- within( L154.ALL_R_trn_m_sz_tech_F_Y.melt, cost_usdvkm <- Tusd / Tvkm )
L154.ALL_R_trn_m_sz_tech_F_Y.melt <- within( L154.ALL_R_trn_m_sz_tech_F_Y.melt, speed_kmhr <- Tvkm / Thr )

#Build the final data frames, casting by year
L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y <- cast( L154.ALL_R_trn_m_sz_tech_F_Y.melt,
      GCAM_region_ID + UCD_sector + mode + size.class + UCD_technology + UCD_fuel ~ Xyear,
      value = "intensity_MJvkm" )
L154.loadfactor_R_trn_m_sz_tech_F_Y <- cast( L154.ALL_R_trn_m_sz_tech_F_Y.melt,
      GCAM_region_ID + UCD_sector + mode + size.class + UCD_technology + UCD_fuel ~ Xyear,
      value = "loadfactor" )
L154.cost_usdvkm_R_trn_m_sz_tech_F_Y <- cast( L154.ALL_R_trn_m_sz_tech_F_Y.melt,
      GCAM_region_ID + UCD_sector + mode + size.class + UCD_technology + UCD_fuel ~ Xyear,
      value = "cost_usdvkm" )
L154.speed_kmhr_R_trn_m_sz_tech_F_Y <- cast( L154.ALL_R_trn_m_sz_tech_F_Y.melt,
      GCAM_region_ID + UCD_sector + mode + size.class + UCD_technology + UCD_fuel ~ Xyear,
      value = "speed_kmhr" )

printlog( "Part 3: Downscaling of non-motorized transport to the country level, using population" )
L154.out_bpkm_Rucd_trn_nonmotor <- subset( UCD_transportation_database, mode %in% c( "Walk", "Cycle" ) & variable == "service output" )
L100.Pop_thous_ctry_Yh$UCD_region <- UCD_ctry$UCD_region[
      match( L100.Pop_thous_ctry_Yh$iso, UCD_ctry$iso ) ]
L154.Pop_thous_Rucd <- aggregate( L100.Pop_thous_ctry_Yh[ X_UCD_en_year ],
      by=as.list( L100.Pop_thous_ctry_Yh[ "UCD_region" ] ), sum )
L154.out_bpkm_Rucd_trn_nonmotor$Pop_thous <- L154.Pop_thous_Rucd[[ X_UCD_en_year ]][
      match( L154.out_bpkm_Rucd_trn_nonmotor$UCD_region, L154.Pop_thous_Rucd$UCD_region ) ]
L154.out_bpkm_Rucd_trn_nonmotor$pkm_percap <- L154.out_bpkm_Rucd_trn_nonmotor[[X_UCD_en_year]] * conv_bil_thous /
      L154.out_bpkm_Rucd_trn_nonmotor$Pop_thous

#Compute the nonmotorized service output at the country level, using the historical population
L154.out_mpkm_ctry_trn_nonmotor <- repeat_and_add_vector(
      L100.Pop_thous_ctry_Yh, "mode", c( "Walk", "Cycle" ) )
L154.out_mpkm_ctry_trn_nonmotor[ X_historical_years ] <- L154.out_mpkm_ctry_trn_nonmotor[ X_historical_years ] *
      conv_thous_mil *
      L154.out_bpkm_Rucd_trn_nonmotor$pkm_percap[
         match( vecpaste( L154.out_mpkm_ctry_trn_nonmotor[ c( "UCD_region", "mode" ) ] ),
                vecpaste( L154.out_bpkm_Rucd_trn_nonmotor[ c( "UCD_region", "mode" ) ] ) ) ]

#Aggregate by GCAM region and write it out
L154.out_mpkm_ctry_trn_nonmotor[[R]] <- iso_GCAM_regID[[R]][
      match( L154.out_mpkm_ctry_trn_nonmotor$iso, iso_GCAM_regID$iso ) ]
L154.out_mpkm_R_trn_nonmotor_Yh <- aggregate( L154.out_mpkm_ctry_trn_nonmotor[ X_historical_years ],
      by=as.list( L154.out_mpkm_ctry_trn_nonmotor[ c( R, "mode" ) ] ), sum )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L154.in_EJ_R_trn_m_sz_tech_F_Yh <- c( "Transportation energy consumption by GCAM region / mode / size class / technology / fuel / historical year","Unit = EJ" )
comments.L154.in_EJ_ctry_trn_m_sz_tech_F <- c( "Unscaled transportation energy consumption by country / mode / size class / technology / fuel / historical year","Unit = EJ" )
comments.L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y <- c( "Vehicle energy intensity by GCAM region / mode / size class / technology / fuel / year","Unit = MJ / vkm" )
comments.L154.loadfactor_R_trn_m_sz_tech_F_Y <- c( "Load factors by GCAM region / mode / size class / technology / fuel / year","Unit = pers / veh and tonnes / veh" )
comments.L154.cost_usdvkm_R_trn_m_sz_tech_F_Y <- c( "Non-fuel cost by GCAM region / mode / size class / technology / fuel / year","Unit = 2005USD / vkm" )
comments.L154.speed_kmhr_R_trn_m_sz_tech_F_Y <- c( "Vehicle speed by GCAM region / mode / size class / technology / fuel / year","Unit = km / hr" )
comments.L154.out_mpkm_R_trn_nonmotor_Yh <- c( "Service output by GCAM region / non-motorized transport mode / year","Unit = million pass-km" )

#write tables as CSV files
writedata( L154.in_EJ_R_trn_m_sz_tech_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L154.in_EJ_R_trn_m_sz_tech_F_Yh", comments=comments.L154.in_EJ_R_trn_m_sz_tech_F_Yh )
writedata( L154.in_EJ_ctry_trn_m_sz_tech_F, domain="ENERGY_LEVEL1_DATA", fn="L154.in_EJ_ctry_trn_m_sz_tech_F", comments=comments.L154.in_EJ_ctry_trn_m_sz_tech_F )
writedata( L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y, domain="ENERGY_LEVEL1_DATA", fn="L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
      comments=comments.L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y )
writedata( L154.loadfactor_R_trn_m_sz_tech_F_Y, domain="ENERGY_LEVEL1_DATA", fn="L154.loadfactor_R_trn_m_sz_tech_F_Y",
      comments=comments.L154.loadfactor_R_trn_m_sz_tech_F_Y )
writedata( L154.cost_usdvkm_R_trn_m_sz_tech_F_Y, domain="ENERGY_LEVEL1_DATA", fn="L154.cost_usdvkm_R_trn_m_sz_tech_F_Y",
      comments=comments.L154.cost_usdvkm_R_trn_m_sz_tech_F_Y )
writedata( L154.speed_kmhr_R_trn_m_sz_tech_F_Y, domain="ENERGY_LEVEL1_DATA", fn="L154.speed_kmhr_R_trn_m_sz_tech_F_Y",
      comments=comments.L154.speed_kmhr_R_trn_m_sz_tech_F_Y )
writedata( L154.out_mpkm_R_trn_nonmotor_Yh, domain="ENERGY_LEVEL1_DATA", fn="L154.out_mpkm_R_trn_nonmotor_Yh",
      comments=comments.L154.out_mpkm_R_trn_nonmotor_Yh )

# Every script should finish with this line
logstop()
