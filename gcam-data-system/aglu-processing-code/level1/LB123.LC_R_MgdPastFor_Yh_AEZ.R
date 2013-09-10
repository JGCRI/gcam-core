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
logstart( "LB123.LC_R_MgdPastFor_Yh_AEZ.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Managed forest and pasture cover by region / crop / historical year / AEZ" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L102.ag_Prod_Mt_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L102.ag_Prod_Mt_R_C_AEZ" )
L102.ag_HA_bm2_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L102.ag_HA_bm2_R_C_AEZ" )
L108.ag_Feed_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L108.ag_Feed_Mt_R_C_Y" )
L110.For_ALL_bm3_R_Y <- readdata( "AGLU_LEVEL1_DATA", "L110.For_ALL_bm3_R_Y" )
L120.LC_bm2_R_LT_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_R_LT_Yh_AEZ" )
L121.VegC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.VegC_kgm2_R_LT_AEZ" )
L121.MatureAge_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.MatureAge_R_LT_AEZ" )
L101.Pop_thous_R_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_R_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Part 1: Managed pasture production and land cover" )
#Calculate yields of pasture by AEZ from the GTAP data on FodderGrass ( hay ) production. Assume the global average by AEZ in all regions.
printlog( "NOTE: Using global average hay yields by AEZ to estimate production of grass in all pastures" )
#Production
L123.ag_Prod_Mt_R_Grass_AEZ <- subset( L102.ag_Prod_Mt_R_C_AEZ, GCAM_commodity=="FodderGrass" )
L123.ag_Prod_Mt_Grass_AEZ <- aggregate( L123.ag_Prod_Mt_R_Grass_AEZ[ AEZs ], by=as.list( L123.ag_Prod_Mt_R_Grass_AEZ[ C ] ), sum )

#Harvested area
L123.ag_HA_bm2_R_Grass_AEZ <- subset( L102.ag_HA_bm2_R_C_AEZ, GCAM_commodity=="FodderGrass" )
L123.ag_HA_bm2_Grass_AEZ <- aggregate( L123.ag_HA_bm2_R_Grass_AEZ[ AEZs ], by=as.list( L123.ag_HA_bm2_R_Grass_AEZ[ C ] ), sum )

#Yield
L123.ag_Yield_kgm2_Past_AEZ <- L123.ag_Prod_Mt_Grass_AEZ
L123.ag_Yield_kgm2_Past_AEZ[ AEZs ] <- L123.ag_Prod_Mt_Grass_AEZ[ AEZs ] / L123.ag_HA_bm2_Grass_AEZ[ AEZs ]
L123.ag_Yield_kgm2_Past_AEZ[ is.na( L123.ag_Yield_kgm2_Past_AEZ ) ] <- 0

#Calculate bottom-up estimate of pasture production by region and AEZ ( yield times land area )
printlog( "Calculating total pasture grass production by region and AEZ" )
L123.LC_bm2_R_Past_Y_AEZ <- subset( L120.LC_bm2_R_LT_Yh_AEZ, Land_Type=="Pasture", select = c( R_LT_AEZ, X_AGLU_historical_years ) )
L123.ag_Yield_kgm2_Past_AEZ.melt <- melt( L123.ag_Yield_kgm2_Past_AEZ, id.vars = C, variable_name = "AEZ" ) 

L123.ag_potentialProd_Mt_R_Past_Y_AEZ <- L123.LC_bm2_R_Past_Y_AEZ
L123.ag_potentialProd_Mt_R_Past_Y_AEZ[ X_AGLU_historical_years ] <- L123.LC_bm2_R_Past_Y_AEZ[ X_AGLU_historical_years ] * L123.ag_Yield_kgm2_Past_AEZ.melt$value[
      match( L123.LC_bm2_R_Past_Y_AEZ$AEZ, L123.ag_Yield_kgm2_Past_AEZ.melt$AEZ ) ]

#Use this "potential production" to disaggregate actual pastureland production to AEZs
printlog( "NOTE: using pasture grass production by region and AEZ to disaggregate regional pasture consumption to AEZ" )
L123.ag_potentialProd_Mt_R_Past_Y <- aggregate( L123.ag_potentialProd_Mt_R_Past_Y_AEZ[ X_AGLU_historical_years ], by=as.list( L123.ag_potentialProd_Mt_R_Past_Y_AEZ[ R_LT ] ), sum )
L123.ag_PastureProdfrac_R_Y_AEZ <- L123.ag_potentialProd_Mt_R_Past_Y_AEZ[ R_LT_AEZ ]
L123.ag_PastureProdfrac_R_Y_AEZ[ X_AGLU_historical_years ] <- L123.ag_potentialProd_Mt_R_Past_Y_AEZ[ X_AGLU_historical_years ] / L123.ag_potentialProd_Mt_R_Past_Y[
      match( L123.ag_potentialProd_Mt_R_Past_Y_AEZ[[R]], L123.ag_potentialProd_Mt_R_Past_Y[[R]] ),
      X_AGLU_historical_years ]
#For regions that do not exist, return NaNs to 0
L123.ag_PastureProdfrac_R_Y_AEZ[ is.na( L123.ag_PastureProdfrac_R_Y_AEZ ) ] <- 0
L123.ag_Prod_Mt_R_Past_Y_AEZ <- L123.ag_PastureProdfrac_R_Y_AEZ[ R_LT_AEZ ]
names( L123.ag_Prod_Mt_R_Past_Y_AEZ )[ names( L123.ag_Prod_Mt_R_Past_Y_AEZ) == LT ] <- C
L123.ag_Prod_Mt_R_Past_Y_AEZ[ X_AGLU_historical_years ] <- L123.ag_PastureProdfrac_R_Y_AEZ[ X_AGLU_historical_years ] * L108.ag_Feed_Mt_R_C_Y[
      match( vecpaste( L123.ag_Prod_Mt_R_Past_Y_AEZ[ R_C ] ), vecpaste( L108.ag_Feed_Mt_R_C_Y[ R_C ] ) ),
      X_AGLU_historical_years ]

#Calculate land requirements.
printlog( "Calculating pasture land required to produce grass consumed in pastures. This is managed pasture" )
L123.LC_bm2_R_MgdPast_Y_AEZ <- L123.ag_Prod_Mt_R_Past_Y_AEZ[ R_C_AEZ ]
L123.LC_bm2_R_MgdPast_Y_AEZ[ X_AGLU_historical_years ] <- L123.ag_Prod_Mt_R_Past_Y_AEZ[ X_AGLU_historical_years ] / L123.ag_Yield_kgm2_Past_AEZ.melt$value[
      match( L123.ag_Prod_Mt_R_Past_Y_AEZ$AEZ, L123.ag_Yield_kgm2_Past_AEZ.melt$AEZ ) ]
L123.LC_bm2_R_MgdPast_Y_AEZ[ is.na(L123.LC_bm2_R_MgdPast_Y_AEZ ) ] <- 0
names( L123.LC_bm2_R_MgdPast_Y_AEZ )[ names( L123.LC_bm2_R_MgdPast_Y_AEZ) == C ] <- LT

#Where managed pasture is greater than assumed threshold percentage of total pasture, reduce the managed pasture land.
#Output is unaffected so these regions have higher yields.
printlog( "Applying maximum percentage of any region/AEZs pasture that is allowed to be in production (managed)" )
printlog( "NOTE: In region/AEZs where applicable, this threshold will result in increased pasture yields" )
L123.LC_MgdPastFrac_Y_AEZ <- L123.LC_bm2_R_MgdPast_Y_AEZ[ R_LT_AEZ ]
L123.LC_MgdPastFrac_Y_AEZ[ X_AGLU_historical_years ] <-
      L123.LC_bm2_R_MgdPast_Y_AEZ[ X_AGLU_historical_years ] / L123.LC_bm2_R_Past_Y_AEZ[ X_AGLU_historical_years ]
L123.LC_MgdPastFrac_Y_AEZ[ is.na( L123.LC_MgdPastFrac_Y_AEZ) ] <- 0
L123.LC_MgdPastFrac_Y_AEZ_adj <- L123.LC_MgdPastFrac_Y_AEZ
L123.LC_MgdPastFrac_Y_AEZ_adj[ X_AGLU_historical_years ][ L123.LC_MgdPastFrac_Y_AEZ_adj[ X_AGLU_historical_years ] > max_MgdPast_frac ] <- max_MgdPast_frac

#Recalculate managed pasture land, adjusted by assumed maximum portion that can be managed
L123.LC_bm2_R_MgdPast_Y_AEZ_adj <- L123.LC_bm2_R_Past_Y_AEZ[ R_LT_AEZ ]
L123.LC_bm2_R_MgdPast_Y_AEZ_adj[ X_AGLU_historical_years ] <-
      L123.LC_bm2_R_Past_Y_AEZ[ X_AGLU_historical_years ] * L123.LC_MgdPastFrac_Y_AEZ_adj[ X_AGLU_historical_years ]

#Calculate pasture yield
L123.ag_Yield_kgm2_R_Past_Y_AEZ <- L123.ag_Prod_Mt_R_Past_Y_AEZ[ R_C_AEZ ]
L123.ag_Yield_kgm2_R_Past_Y_AEZ[ X_AGLU_historical_years ] <-
      L123.ag_Prod_Mt_R_Past_Y_AEZ[ X_AGLU_historical_years ] / L123.LC_bm2_R_MgdPast_Y_AEZ_adj [ X_AGLU_historical_years ]
L123.ag_Yield_kgm2_R_Past_Y_AEZ[ is.na( L123.ag_Yield_kgm2_R_Past_Y_AEZ ) ] <- 0

#Multiply "managed" shares in the earliest available year by prior pasture land cover pathway to get historical managed pasture
printlog( "Building managed pasture land use history" )
printlog( "NOTE: Assuming same managed:unmanaged pasture ratio in early land cover years" )
L123.LC_bm2_R_Past_Yhh_AEZ <- subset( L120.LC_bm2_R_LT_Yh_AEZ, Land_Type=="Pasture", select = c( R_LT_AEZ, X_preAGLU_years ) )
L123.LC_bm2_R_MgdPast_Yh_AEZ <- L123.LC_bm2_R_MgdPast_Y_AEZ_adj
L123.LC_bm2_R_MgdPast_Yh_AEZ[ X_preAGLU_years ] <- L123.LC_MgdPastFrac_Y_AEZ_adj[[ X_AGLU_historical_years[1] ]] *
      L123.LC_bm2_R_Past_Yhh_AEZ[ X_preAGLU_years ]
L123.LC_bm2_R_MgdPast_Yh_AEZ <- L123.LC_bm2_R_MgdPast_Yh_AEZ[ c( R_LT_AEZ, X_land_cover_years ) ]

#FORESTRY
#Carbon densities are divided by mature age to get net primary productivity, and used to derive exogenous yields for separating managed/unmanaged forest
#calculate veg mass of each AEZ based on above-ground carbon content of each AEZ. Assume 288 kgC/m3 of wood.
printlog( "Part 2: Managed forest production and land cover" )
printlog( "NOTE: Using average vegetation carbon densities and mature ages to estimate annual forest biomass production" )
L123.For_VegVolume_m3m2_R_AEZ <- subset( L121.VegC_kgm2_R_LT_AEZ, Land_Type=="Forest" )
L123.For_VegVolume_m3m2_R_AEZ[ AEZs ] <- L121.VegC_kgm2_R_LT_AEZ[
      L121.VegC_kgm2_R_LT_AEZ$Land_Type=="Forest", AEZs ] / AvgWoodDensity_kgCm3

#calculate biomass production ( yield ) as veg volume divided by the mature age
L123.For_MatureAge_R_AEZ <- L121.MatureAge_R_LT_AEZ[ L121.MatureAge_R_LT_AEZ[[LT]] == "Forest", ]
L123.For_Yield_m3m2_R_AEZ <- data.frame( L123.For_VegVolume_m3m2_R_AEZ[ R ],
      GCAM_commodity = L123.For_VegVolume_m3m2_R_AEZ[[LT]],
      L123.For_VegVolume_m3m2_R_AEZ[ AEZs ] / L123.For_MatureAge_R_AEZ[ AEZs ] )
L123.For_Yield_m3m2_R_AEZ[ is.na( L123.For_Yield_m3m2_R_AEZ ) ] <- 0

#disaggregate logging to AEZ on the basis of biomass production rates.
printlog( "NOTE: Using forest biomass production by AEZ to disaggregate regional wood production to AEZs" )
L123.LC_bm2_R_For_Y_AEZ <- L120.LC_bm2_R_LT_Yh_AEZ[ L120.LC_bm2_R_LT_Yh_AEZ[[LT]] == "Forest", c( R_LT_AEZ, X_AGLU_historical_years ) ]
L123.For_Yield_m3m2_R_AEZ.melt <- melt( L123.For_Yield_m3m2_R_AEZ, id.vars = R_C, variable_name = "AEZ" )

L123.For_potentialProd_bm3_R_Y_AEZ <- L123.LC_bm2_R_For_Y_AEZ
names( L123.For_potentialProd_bm3_R_Y_AEZ )[ names( L123.For_potentialProd_bm3_R_Y_AEZ ) == LT ] <- C
L123.For_potentialProd_bm3_R_Y_AEZ[ X_AGLU_historical_years ] <- L123.LC_bm2_R_For_Y_AEZ[ X_AGLU_historical_years ] * L123.For_Yield_m3m2_R_AEZ.melt$value[
      match( vecpaste( L123.LC_bm2_R_For_Y_AEZ[ R_AEZ ] ), vecpaste( L123.For_Yield_m3m2_R_AEZ.melt[ R_AEZ ]) ) ]

#Cast regional forestry output into the same format as these tables
L110.For_ALL_bm3_R_Y$Xyear <- paste( "X", L110.For_ALL_bm3_R_Y[[Y]], sep = "" )
L123.For_ALL_bm3_R_Y <- cast( L110.For_ALL_bm3_R_Y, GCAM_region_ID + GCAM_commodity ~ Xyear, value = "Prod_bm3" )

#Forest output by AEZ = Regional forest output * AEZ-wise forest biomass production fraction
L123.For_potentialProd_bm3_R_Y <- aggregate( L123.For_potentialProd_bm3_R_Y_AEZ[ X_AGLU_historical_years ], by=as.list( L123.For_potentialProd_bm3_R_Y_AEZ[ R_C ] ), sum )
L123.For_Prodfrac_R_Y_AEZ <- L123.For_potentialProd_bm3_R_Y_AEZ[ R_C_AEZ ]
L123.For_Prodfrac_R_Y_AEZ[ X_AGLU_historical_years ] <- L123.For_potentialProd_bm3_R_Y_AEZ[ X_AGLU_historical_years ] / L123.For_potentialProd_bm3_R_Y[
      match( L123.For_potentialProd_bm3_R_Y_AEZ[[R]], L123.For_potentialProd_bm3_R_Y[[R]] ),
      X_AGLU_historical_years ]
#For regions that do not exist, return NaNs to 0
L123.For_Prodfrac_R_Y_AEZ[ is.na( L123.For_Prodfrac_R_Y_AEZ ) ] <- 0
L123.For_Prod_bm3_R_Y_AEZ <- L123.For_Prodfrac_R_Y_AEZ[ R_C_AEZ]
L123.For_Prod_bm3_R_Y_AEZ[ X_AGLU_historical_years ] <- L123.For_Prodfrac_R_Y_AEZ[ X_AGLU_historical_years ] * L123.For_ALL_bm3_R_Y[
      match( L123.For_Prodfrac_R_Y_AEZ[[R]], L123.For_ALL_bm3_R_Y[[R]] ),
      X_AGLU_historical_years ]
      
#Get rid of values less than 1e-6
L123.For_Prod_bm3_R_Y_AEZ[ X_AGLU_historical_years ][ L123.For_Prod_bm3_R_Y_AEZ[ X_AGLU_historical_years ] < 1e-6 ] <- 0

#Calculate land cover of "managed" forest as the output divided by the yield ( net primary production ), in each AEZ and region
L123.LC_bm2_R_MgdFor_Y_AEZ <- L123.For_Prod_bm3_R_Y_AEZ[ R_C_AEZ ]
names( L123.LC_bm2_R_MgdFor_Y_AEZ )[ names( L123.LC_bm2_R_MgdFor_Y_AEZ ) == C ] <- LT
L123.LC_bm2_R_MgdFor_Y_AEZ[ X_AGLU_historical_years ] <- L123.For_Prod_bm3_R_Y_AEZ[ X_AGLU_historical_years ] / L123.For_Yield_m3m2_R_AEZ.melt$value[
      match( vecpaste( L123.For_Prod_bm3_R_Y_AEZ[ R_AEZ ] ), vecpaste( L123.For_Yield_m3m2_R_AEZ.melt[ R_AEZ ] ) ) ]
L123.LC_bm2_R_MgdFor_Y_AEZ[ is.na( L123.LC_bm2_R_MgdFor_Y_AEZ ) ] <- 0

#Use historical population ratios to estimate managed forest in the pre-AGLU years
printlog( "Building managed forest land use history" )
printlog( "NOTE: Scaling historical managed forest land area with historical population" )
L123.PopRatio_R_Yhh <- L101.Pop_thous_R_Yh[ c( R, X_preAGLU_years ) ]
L123.PopRatio_R_Yhh[ X_preAGLU_years ] <- L101.Pop_thous_R_Yh[ X_preAGLU_years ] / L101.Pop_thous_R_Yh[[ X_AGLU_historical_years[1] ]]

L123.LC_bm2_R_MgdFor_Yh_AEZ <- L123.LC_bm2_R_MgdFor_Y_AEZ
L123.LC_bm2_R_MgdFor_Yh_AEZ[ X_preAGLU_years ] <- L123.LC_bm2_R_MgdFor_Y_AEZ[[ X_AGLU_historical_years[1] ]] * L123.PopRatio_R_Yhh[
      match( L123.LC_bm2_R_MgdFor_Y_AEZ[[R]], L123.PopRatio_R_Yhh[[R]] ),
      X_preAGLU_years ]
L123.LC_bm2_R_MgdFor_Yh_AEZ <- L123.LC_bm2_R_MgdFor_Yh_AEZ[ c( R_LT_AEZ, X_land_cover_years ) ]

#Where managed forest is greater than assumed maximum percentage of total forest, reduce the managed forest land.
#Output is unaffected so these regions have higher yields.
printlog( "Applying maximum percentage of any region/AEZs forest that is allowed to be in production (managed)" )
printlog( "NOTE: In region/AEZs where applicable, this threshold will result in increased forest yields" )
L123.LC_bm2_R_For_Yh_AEZ <- L120.LC_bm2_R_LT_Yh_AEZ[ L120.LC_bm2_R_LT_Yh_AEZ[[LT]] == "Forest", c( R_LT_AEZ, X_land_cover_years ) ]
L123.LC_MgdForFrac_Yh_AEZ <- L123.LC_bm2_R_MgdFor_Yh_AEZ[ R_LT_AEZ ]
L123.LC_MgdForFrac_Yh_AEZ[ X_land_cover_years ] <-
      L123.LC_bm2_R_MgdFor_Yh_AEZ[ X_land_cover_years ] / L123.LC_bm2_R_For_Yh_AEZ[ X_land_cover_years ]
L123.LC_MgdForFrac_Yh_AEZ[ is.na( L123.LC_MgdForFrac_Yh_AEZ) ] <- 0
L123.LC_MgdForFrac_Yh_AEZ_adj <- L123.LC_MgdForFrac_Yh_AEZ
L123.LC_MgdForFrac_Yh_AEZ_adj[ X_land_cover_years ][ L123.LC_MgdForFrac_Yh_AEZ_adj[ X_land_cover_years ] > max_MgdFor_frac ] <- max_MgdFor_frac

#Recalculate managed forest land, adjusted by assumed maximum portion that can be managed
L123.LC_bm2_R_MgdFor_Yh_AEZ <- L123.LC_bm2_R_For_Yh_AEZ[ R_LT_AEZ ]
L123.LC_bm2_R_MgdFor_Yh_AEZ[ X_land_cover_years ] <-
      L123.LC_bm2_R_For_Yh_AEZ[ X_land_cover_years ] * L123.LC_MgdForFrac_Yh_AEZ_adj[ X_land_cover_years ]

#Recalculate forestry yield
L123.For_Yield_m3m2_R_AEZ <- L123.For_Prod_bm3_R_Y_AEZ[ R_C_AEZ ]
L123.For_Yield_m3m2_R_AEZ[ X_AGLU_historical_years ] <-
      L123.For_Prod_bm3_R_Y_AEZ[ X_AGLU_historical_years ] / L123.LC_bm2_R_MgdFor_Yh_AEZ [ X_AGLU_historical_years ]
L123.For_Yield_m3m2_R_AEZ[ is.na( L123.For_Yield_m3m2_R_AEZ ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L123.ag_Prod_Mt_R_Past_Y_AEZ <- c( "Pasture production by GCAM region / year / AEZ","Unit = Mt" )
comments.L123.ag_Yield_kgm2_R_Past_Y_AEZ <- c( "Pasture yield by GCAM region / year / AEZ","Unit = kg/m2" )
comments.L123.LC_bm2_R_MgdPast_Yh_AEZ <- c( "Managed pasture land cover by GCAM region / historical year / AEZ","Unit = bm2" )
comments.L123.For_Prod_bm3_R_Y_AEZ <- c( "Forest production by GCAM region / year / AEZ","Unit = bm3" )
comments.L123.For_Yield_m3m2_R_AEZ <- c( "Forest yield by GCAM region / year / AEZ","Unit = m3/m2" )
comments.L123.LC_bm2_R_MgdFor_Yh_AEZ <- c( "Managed forest land cover by GCAM region / historical year / AEZ","Unit = bm2" )

writedata( L123.ag_Prod_Mt_R_Past_Y_AEZ, domain="AGLU_LEVEL1_DATA", fn="L123.ag_Prod_Mt_R_Past_Y_AEZ", comments=comments.L123.ag_Prod_Mt_R_Past_Y_AEZ )
writedata( L123.ag_Yield_kgm2_R_Past_Y_AEZ, domain="AGLU_LEVEL1_DATA", fn="L123.ag_Yield_kgm2_R_Past_Y_AEZ", comments=comments.L123.ag_Yield_kgm2_R_Past_Y_AEZ )
writedata( L123.LC_bm2_R_MgdPast_Yh_AEZ, domain="AGLU_LEVEL1_DATA", fn="L123.LC_bm2_R_MgdPast_Yh_AEZ", comments=comments.L123.LC_bm2_R_MgdPast_Yh_AEZ )
writedata( L123.For_Prod_bm3_R_Y_AEZ, domain="AGLU_LEVEL1_DATA", fn="L123.For_Prod_bm3_R_Y_AEZ", comments=comments.L123.For_Prod_bm3_R_Y_AEZ )
writedata( L123.For_Yield_m3m2_R_AEZ, domain="AGLU_LEVEL1_DATA", fn="L123.For_Yield_m3m2_R_AEZ", comments=comments.L123.For_Yield_m3m2_R_AEZ )
writedata( L123.LC_bm2_R_MgdFor_Yh_AEZ, domain="AGLU_LEVEL1_DATA", fn="L123.LC_bm2_R_MgdFor_Yh_AEZ", comments=comments.L123.LC_bm2_R_MgdFor_Yh_AEZ )

# Every script should finish with this line
logstop()
