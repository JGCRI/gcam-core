# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu processing scripts, please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "LB134.Diet_Rfao.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Future diet: income elasticities on food demands" )

# -----------------------------------------------------------------------------
# 1. Read data
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO2050_items_cal <- readdata( "AGLU_MAPPINGS", "FAO2050_items_cal" )
FAO2050_Diet <- readdata( "AGLU_LEVEL0_DATA", "FAO2050_Diet" )
L100.FAO_ag_Food_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_ag_Food_t" )
L100.FAO_an_Food_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_an_Food_t" )
L101.ag_Food_Pcal_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L101.ag_Food_Pcal_R_C_Y" )
L105.an_Food_Pcal_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L105.an_Food_Pcal_R_C_Y" )
L101.Pop_thous_R_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_R_Yh" )
L102.pcgdp_thous90USD_Scen_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_Scen_R_Y")

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Building historical time series of per-capita caloric demands" )
#Historical time series of ag and animal product consumption
L134.ag_Food_Pcal_R_Y <- aggregate( L101.ag_Food_Pcal_R_C_Y[ X_AGLU_historical_years ], by=as.list( L101.ag_Food_Pcal_R_C_Y[ R ] ), sum )
L134.ag_Food_Pcal_R_Y$GCAM_demand <- "crops"
L134.an_Food_Pcal_R_Y <- aggregate( L105.an_Food_Pcal_R_C_Y[ X_AGLU_historical_years ], by=as.list( L105.an_Food_Pcal_R_C_Y[ R ] ), sum )
L134.an_Food_Pcal_R_Y$GCAM_demand <- "meat"
L134.Food_Pcal_R_Dmnd_Y <- rbind( L134.ag_Food_Pcal_R_Y[ c( R, "GCAM_demand", X_AGLU_historical_years ) ],
      L134.an_Food_Pcal_R_Y[ c( R, "GCAM_demand", X_AGLU_historical_years ) ] )

#Divide by population to calculate the historical per-capita food demands, in kcal per person per day
L134.pcFood_kcald_R_Dmnd_Y <- L134.Food_Pcal_R_Dmnd_Y
L134.pcFood_kcald_R_Dmnd_Y[ X_AGLU_historical_years ] <- L134.Food_Pcal_R_Dmnd_Y[ X_AGLU_historical_years ] * conv_days_year * conv_Pcal_Mcal /
      L101.Pop_thous_R_Yh[
         match( L134.pcFood_kcald_R_Dmnd_Y[[R]], L101.Pop_thous_R_Yh[[R]] ),
         X_AGLU_historical_years ]

#Extrapolate this to the future periods based on the FAO projections
L134.FAO_ag_Food_t <- aggregate( L100.FAO_ag_Food_t[ X_final_historical_year ], by=as.list( L100.FAO_ag_Food_t[ "iso" ] ), sum )
L134.FAO_ag_Food_t$GCAM_demand <- "crops"
L134.FAO_an_Food_t <- aggregate( L100.FAO_an_Food_t[ X_final_historical_year ], by=as.list( L100.FAO_an_Food_t[ "iso" ] ), sum )
L134.FAO_an_Food_t$GCAM_demand <- "meat"
L134.Food_t_ctry_Dmnd_Y <- rbind( L134.FAO_ag_Food_t[ c( "iso", "GCAM_demand", X_final_historical_year ) ],
      L134.FAO_an_Food_t[ c( "iso", "GCAM_demand", X_final_historical_year ) ] )
L134.Food_t_ctry_Dmnd_Y$FAO2050_reg <- AGLU_ctry$FAO2050_reg[ match( L134.Food_t_ctry_Dmnd_Y$iso, AGLU_ctry$iso ) ]

#Calculate the future demand ratios by FAO2050 region for each demand type
#Drop unnecessary composite regions from FAO diet table
L134.Diet_Rfao_Cfao_Yfao <- FAO2050_Diet[ FAO2050_Diet$FAO2050_reg %in% L134.Food_t_ctry_Dmnd_Y$FAO2050_reg, ]

printlog( "Calculating FAO2050 diet for specified diet years, using linear interpolation" )
#Use linear interpolation to convert FAO2050 model time periods to GCAM "diet years"
L134.Diet_Rfao_Cfao_Yfao <- gcam_interp( L134.Diet_Rfao_Cfao_Yfao, diet_years )

#Add vectors for caloric content and demand category
L134.Diet_Rfao_Cfao_Yfao$GCAM_demand<-FAO2050_items_cal$GCAM_demand[
      match( L134.Diet_Rfao_Cfao_Yfao$FAO2050_item, FAO2050_items_cal$FAO2050_item)]
L134.Diet_Rfao_Cfao_Yfao$kcalkg <- FAO2050_items_cal$kcalkg[ match( L134.Diet_Rfao_Cfao_Yfao$FAO2050_item, FAO2050_items_cal$FAO2050_item ) ]
L134.Diet_Rfao_Cfao_Yfao$conv_d <- FAO2050_items_cal$conv_d[ match( L134.Diet_Rfao_Cfao_Yfao$FAO2050_item, FAO2050_items_cal$FAO2050_item ) ]

#Build new table with only diet years subsetted
#Multiply through by caloric contents to get all measures in kcal/pers/d
printlog( "Deriving diet in kcal/pers/d, by FAO region and food categories" )
L134.Diet_kcald_Rfao_Cfao_Y <- L134.Diet_Rfao_Cfao_Yfao[ c( "FAO2050_reg", "FAO2050_item", "GCAM_demand" ) ]
L134.Diet_kcald_Rfao_Cfao_Y[ X_diet_years ] <-
      L134.Diet_Rfao_Cfao_Yfao[ X_diet_years ] * L134.Diet_Rfao_Cfao_Yfao$kcalkg / L134.Diet_Rfao_Cfao_Yfao$conv_d

#Aggregate by GCAM demand and FAO region
printlog( "Aggregating by GCAM food categories and computing future diet ratios by FAO2050 region" )
L134.Diet_kcald_Rfao_Dmnd_Y <- aggregate( L134.Diet_kcald_Rfao_Cfao_Y[ X_diet_years ],
      by=as.list( L134.Diet_kcald_Rfao_Cfao_Y[ c( "FAO2050_reg", "GCAM_demand" ) ] ), sum )
L134.Diet_kcald_Rfao_Dmnd_Y$GCAM_demand[ L134.Diet_kcald_Rfao_Dmnd_Y$GCAM_demand == "total" ] <- "crops"
L134.Diet_kcald_Rfao_Dmnd_Y[ L134.Diet_kcald_Rfao_Dmnd_Y$GCAM_demand == "crops", X_diet_years ] <-
      L134.Diet_kcald_Rfao_Dmnd_Y[ L134.Diet_kcald_Rfao_Dmnd_Y$GCAM_demand == "crops", X_diet_years ] -
      L134.Diet_kcald_Rfao_Dmnd_Y[ L134.Diet_kcald_Rfao_Dmnd_Y$GCAM_demand == "meat", X_diet_years ]      
L134.DietRatio_Rfao_Dmnd_Y <- L134.Diet_kcald_Rfao_Dmnd_Y
L134.DietRatio_Rfao_Dmnd_Y[ X_diet_years ] <- L134.Diet_kcald_Rfao_Dmnd_Y[ X_diet_years ] / L134.Diet_kcald_Rfao_Dmnd_Y[[ X_diet_years[1] ]]

#Multiply these ratios by the starting values at the country level
L134.Food_t_ctry_Dmnd_Y[ X_diet_years ] <- L134.Food_t_ctry_Dmnd_Y[[ X_final_historical_year ]] * L134.DietRatio_Rfao_Dmnd_Y[
      match( vecpaste( L134.Food_t_ctry_Dmnd_Y[ c( "FAO2050_reg", "GCAM_demand" ) ] ),
             vecpaste( L134.DietRatio_Rfao_Dmnd_Y[ c( "FAO2050_reg", "GCAM_demand" ) ] ) ),
      X_diet_years ]
      
#Match in GCAM regions, aggregate, and compute ratios from final historical year
L134.Food_t_ctry_Dmnd_Y[[R]] <- iso_GCAM_regID[[R]][ match( L134.Food_t_ctry_Dmnd_Y$iso, iso_GCAM_regID$iso ) ]
L134.Food_t_R_Dmnd_Y <- aggregate( L134.Food_t_ctry_Dmnd_Y[ X_diet_years ], by=as.list( L134.Food_t_ctry_Dmnd_Y[ c( R, "GCAM_demand" ) ] ), sum )
L134.FoodRatio_R_Dmnd_Y <- L134.Food_t_R_Dmnd_Y
L134.FoodRatio_R_Dmnd_Y[ X_diet_years ] <- L134.Food_t_R_Dmnd_Y[ X_diet_years ] / L134.Food_t_R_Dmnd_Y[[ X_final_historical_year ]]

#Write this to a full table with no missing values for regions that do not actually exist
L134.FoodRatio_R_Dmnd_Y <- translate_to_full_table( L134.FoodRatio_R_Dmnd_Y,
      R, unique( iso_GCAM_regID[[R]] ),
      "GCAM_demand", unique( L134.FoodRatio_R_Dmnd_Y$GCAM_demand ),
      datacols = X_diet_years,
      na.value = 1 )

#Multiply ratios by the caloric demands in the final historical year
L134.pcFood_kcald_R_Dmnd_Y[ X_diet_years ] <- L134.pcFood_kcald_R_Dmnd_Y[[ X_final_historical_year ]] * L134.FoodRatio_R_Dmnd_Y[
      match( vecpaste( L134.pcFood_kcald_R_Dmnd_Y[ c( R, "GCAM_demand" ) ] ),
             vecpaste( L134.FoodRatio_R_Dmnd_Y[ c( R, "GCAM_demand" ) ] ) ),
      X_diet_years ]

printlog( "Extending the projected diets to all years, assuming convergence year and demand levels")
L134.pcFood_kcald_R_Dmnd_Y[[ X_diet_convergence_year ]][ L134.pcFood_kcald_R_Dmnd_Y$GCAM_demand == "crops" ] <- convergence_kcald_crops
L134.pcFood_kcald_R_Dmnd_Y[[ X_diet_convergence_year ]][ L134.pcFood_kcald_R_Dmnd_Y$GCAM_demand == "meat" ] <- convergence_kcald_meat

L134.pcFood_kcald_R_Dmnd_Y <- gcam_interp( L134.pcFood_kcald_R_Dmnd_Y, c( historical_years, future_years ) )
L134.pcFood_kcald_R_Dmnd_Y <- L134.pcFood_kcald_R_Dmnd_Y[ c( R, "GCAM_demand", X_historical_years, X_future_years ) ]

printlog( "Alternative diet scenarios for the SSPs" )
L134.pcFood_kcald_R_Dmnd_Y_low <- L134.pcFood_kcald_R_Dmnd_Y[ c( R, "GCAM_demand", X_historical_years, "X2050" ) ]
L134.pcFood_kcald_R_Dmnd_Y_low$X2050 <- L134.pcFood_kcald_R_Dmnd_Y_low$X2050 * low_waste_mult
L134.pcFood_kcald_R_Dmnd_Y_low$X2050[ L134.pcFood_kcald_R_Dmnd_Y_low$GCAM_demand == "meat" ] <- L134.pcFood_kcald_R_Dmnd_Y_low$X2050[ L134.pcFood_kcald_R_Dmnd_Y_low$GCAM_demand == "meat" ] * low_meat_mult
L134.pcFood_kcald_R_Dmnd_Y_low$X2100 <- L134.pcFood_kcald_R_Dmnd_Y_low$X2050
L134.pcFood_kcald_R_Dmnd_Y_low <- gcam_interp( L134.pcFood_kcald_R_Dmnd_Y_low, c( historical_years, future_years ) )
L134.pcFood_kcald_R_Dmnd_Y_low <- L134.pcFood_kcald_R_Dmnd_Y_low[ c( R, "GCAM_demand", X_historical_years, X_future_years ) ]

L134.pcFood_kcald_R_Dmnd_Y_high <- L134.pcFood_kcald_R_Dmnd_Y[ c( R, "GCAM_demand", X_historical_years, "X2050" ) ]
L134.pcFood_kcald_R_Dmnd_Y_high$X2050 <- L134.pcFood_kcald_R_Dmnd_Y_high$X2050 * high_waste_mult
L134.pcFood_kcald_R_Dmnd_Y_high$X2050[ L134.pcFood_kcald_R_Dmnd_Y_high$GCAM_demand == "meat" ] <- L134.pcFood_kcald_R_Dmnd_Y_high$X2050[ L134.pcFood_kcald_R_Dmnd_Y_high$GCAM_demand == "meat" ] * high_meat_mult
L134.pcFood_kcald_R_Dmnd_Y_high$X2100 <- L134.pcFood_kcald_R_Dmnd_Y_high$X2050
L134.pcFood_kcald_R_Dmnd_Y_high <- gcam_interp( L134.pcFood_kcald_R_Dmnd_Y_high, c( historical_years, future_years ) )
L134.pcFood_kcald_R_Dmnd_Y_high <- L134.pcFood_kcald_R_Dmnd_Y_high[ c( R, "GCAM_demand", X_historical_years, X_future_years ) ]

L134.pcgdp_2010 <- subset( L102.pcgdp_thous90USD_Scen_R_Y, L102.pcgdp_thous90USD_Scen_R_Y$scenario == "SSP4" )
L134.pcgdp_2010 <- L134.pcgdp_2010[ names( L134.pcgdp_2010) %in% c( "GCAM_region_ID", "X2010" ) ]
L134.pcgdp_2010$X2010 <- L134.pcgdp_2010$X2010 * conv_1990_2010_USD
L134.high_reg <- L134.pcgdp_2010$GCAM_region_ID[ L134.pcgdp_2010$X2010 > hi_growth_pcgdp ]
L134.low_reg <- L134.pcgdp_2010$GCAM_region_ID[ L134.pcgdp_2010$X2010 < lo_growth_pcgdp ]

L134.pcFood_kcald_R_Dmnd_Y_ssp4_lo <- subset( L134.pcFood_kcald_R_Dmnd_Y_low, L134.pcFood_kcald_R_Dmnd_Y_low$GCAM_region_ID %in% L134.low_reg )
L134.pcFood_kcald_R_Dmnd_Y_ssp4_med <- subset( L134.pcFood_kcald_R_Dmnd_Y, L134.pcFood_kcald_R_Dmnd_Y$GCAM_region_ID %!in% c( L134.low_reg, L134.high_reg) )
L134.pcFood_kcald_R_Dmnd_Y_ssp4_hi <- subset( L134.pcFood_kcald_R_Dmnd_Y_high, L134.pcFood_kcald_R_Dmnd_Y_high$GCAM_region_ID %in% L134.high_reg )
L134.pcFood_kcald_R_Dmnd_Y_ssp4 <- rbind( L134.pcFood_kcald_R_Dmnd_Y_ssp4_lo, L134.pcFood_kcald_R_Dmnd_Y_ssp4_med, L134.pcFood_kcald_R_Dmnd_Y_ssp4_hi)
# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L134.pcFood_kcald_R_Dmnd_Y <- c( "Per-capita food demands by region / demand type / year (historical and future)","Unit: kcal / person / day" )
comments.L134.pcFood_kcald_R_Dmnd_Y_low <- c( "Low Per-capita food demands by region / demand type / year (historical and future)","Unit: kcal / person / day" )
comments.L134.pcFood_kcald_R_Dmnd_Y_high <- c( "High Per-capita food demands by region / demand type / year (historical and future)","Unit: kcal / person / day" )
comments.L134.pcFood_kcald_R_Dmnd_Y_ssp4 <- c( "SSP4 Per-capita food demands by region / demand type / year (historical and future)","Unit: kcal / person / day" )

writedata( L134.pcFood_kcald_R_Dmnd_Y, domain="AGLU_LEVEL1_DATA", fn="L134.pcFood_kcald_R_Dmnd_Y", comments=comments.L134.pcFood_kcald_R_Dmnd_Y )
writedata( L134.pcFood_kcald_R_Dmnd_Y_low, domain="AGLU_LEVEL1_DATA", fn="L134.pcFood_kcald_R_Dmnd_Y_low", comments=comments.L134.pcFood_kcald_R_Dmnd_Y_low )
writedata( L134.pcFood_kcald_R_Dmnd_Y_high, domain="AGLU_LEVEL1_DATA", fn="L134.pcFood_kcald_R_Dmnd_Y_high", comments=comments.L134.pcFood_kcald_R_Dmnd_Y_high )
writedata( L134.pcFood_kcald_R_Dmnd_Y_ssp4, domain="AGLU_LEVEL1_DATA", fn="L134.pcFood_kcald_R_Dmnd_Y_ssp4", comments=comments.L134.pcFood_kcald_R_Dmnd_Y_ssp4 )

# Every script should finish with this line
logstop()
