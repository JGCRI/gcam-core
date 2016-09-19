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
logstart( "LB123.LC_R_MgdPastFor_Yh_GLU.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Managed forest and pasture cover by region / crop / historical year / GLU" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L102.ag_Prod_Mt_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L102.ag_Prod_Mt_R_C_GLU" )
L102.ag_HA_bm2_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L102.ag_HA_bm2_R_C_GLU" )
L108.ag_Feed_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L108.ag_Feed_Mt_R_C_Y" )
L110.For_ALL_bm3_R_Y <- readdata( "AGLU_LEVEL1_DATA", "L110.For_ALL_bm3_R_Y" )
L120.LC_bm2_R_LT_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_R_LT_Yh_GLU" )
L121.CarbonContent_kgm2_R_LT_GLU <- readdata( "AGLU_LEVEL1_DATA", "L121.CarbonContent_kgm2_R_LT_GLU" )
L101.Pop_thous_R_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_R_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Part 1: Managed pasture production and land cover" )
#Calculate yields of pasture by GLU from the GTAP/LDS data on FodderGrass ( hay ) production. Assume the global average in all regions.
printlog( "NOTE: Using global average hay yields to estimate production of grass in all pastures" )
#Production
L123.ag_Yield_kgm2_Past <- sum( L102.ag_Prod_Mt_R_C_GLU$value[ L102.ag_Prod_Mt_R_C_GLU[[C]] == "FodderGrass" ] ) /
  sum( L102.ag_HA_bm2_R_C_GLU$value[ L102.ag_HA_bm2_R_C_GLU[[C]] == "FodderGrass" ] )

#Calculate bottom-up estimate of pasture production by region and GLU ( yield times land area )
printlog( "Calculating total pasture grass production by region and GLU" )
L123.ag_potentialProd_Mt_R_Past_Y_GLU <- L120.LC_bm2_R_LT_Yh_GLU[
  L120.LC_bm2_R_LT_Yh_GLU[[LT]] =="Pasture", c( R_LT_GLU, X_AGLU_historical_years ) ]
L123.ag_potentialProd_Mt_R_Past_Y_GLU[ X_AGLU_historical_years ] <-
  L123.ag_potentialProd_Mt_R_Past_Y_GLU[ X_AGLU_historical_years ] * L123.ag_Yield_kgm2_Past

#Use this "potential production" to disaggregate actual pastureland production to GLUs
printlog( "NOTE: using pasture grass production by region and GLU to disaggregate regional pasture consumption to GLU" )
L123.ag_potentialProd_Mt_R_Past_Y <- aggregate( L123.ag_potentialProd_Mt_R_Past_Y_GLU[ X_AGLU_historical_years ],
                                                by = L123.ag_potentialProd_Mt_R_Past_Y_GLU[ R_LT ], sum )
L123.ag_PastureProdfrac_R_Y_GLU <- L123.ag_potentialProd_Mt_R_Past_Y_GLU[ R_LT_GLU ]
L123.ag_PastureProdfrac_R_Y_GLU[ X_AGLU_historical_years ] <- L123.ag_potentialProd_Mt_R_Past_Y_GLU[ X_AGLU_historical_years ] /
  L123.ag_potentialProd_Mt_R_Past_Y[
      match( L123.ag_potentialProd_Mt_R_Past_Y_GLU[[R]], L123.ag_potentialProd_Mt_R_Past_Y[[R]] ),
      X_AGLU_historical_years ]

#Drop NaNs for regions/years with no managed pasture
L123.ag_PastureProdfrac_R_Y_GLU[ is.na( L123.ag_PastureProdfrac_R_Y_GLU ) ] <- 0
L123.ag_Prod_Mt_R_Past_Y_GLU <- L123.ag_PastureProdfrac_R_Y_GLU[ R_LT_GLU ]
names( L123.ag_Prod_Mt_R_Past_Y_GLU )[ names( L123.ag_Prod_Mt_R_Past_Y_GLU) == LT ] <- C
L123.ag_Prod_Mt_R_Past_Y_GLU[ X_AGLU_historical_years ] <- L123.ag_PastureProdfrac_R_Y_GLU[ X_AGLU_historical_years ] *
  L108.ag_Feed_Mt_R_C_Y[
      match( vecpaste( L123.ag_Prod_Mt_R_Past_Y_GLU[ R_C ] ), vecpaste( L108.ag_Feed_Mt_R_C_Y[ R_C ] ) ),
      X_AGLU_historical_years ]

#Calculate land requirements.
printlog( "Calculating pasture land required to produce grass consumed in pastures. This is managed pasture" )
L123.LC_bm2_R_MgdPast_Y_GLU <- L123.ag_Prod_Mt_R_Past_Y_GLU[ R_C_GLU ]
L123.LC_bm2_R_MgdPast_Y_GLU[ X_AGLU_historical_years ] <- L123.ag_Prod_Mt_R_Past_Y_GLU[ X_AGLU_historical_years ] / L123.ag_Yield_kgm2_Past
names( L123.LC_bm2_R_MgdPast_Y_GLU )[ names( L123.LC_bm2_R_MgdPast_Y_GLU) == C ] <- LT

#Where managed pasture is greater than assumed threshold percentage of total pasture, reduce the managed pasture land.
#Output is unaffected so these regions have higher yields.
printlog( "Applying maximum percentage of any region/GLUs pasture that is allowed to be in production (managed)" )
printlog( "NOTE: In region/GLUs where applicable, this threshold will result in increased pasture yields" )
L123.LC_bm2_R_Past_Y_GLU <- L120.LC_bm2_R_LT_Yh_GLU[
  L120.LC_bm2_R_LT_Yh_GLU[[LT]] =="Pasture", c( R_LT_GLU, X_AGLU_historical_years ) ]
L123.LC_MgdPastFrac_Y_GLU <- L123.LC_bm2_R_MgdPast_Y_GLU[ R_LT_GLU ]
L123.LC_MgdPastFrac_Y_GLU[ X_AGLU_historical_years ] <- L123.LC_bm2_R_MgdPast_Y_GLU[ X_AGLU_historical_years ] /
  L123.LC_bm2_R_Past_Y_GLU[
    match( vecpaste( L123.LC_MgdPastFrac_Y_GLU[ R_LT_GLU ] ),
           vecpaste( L123.LC_bm2_R_Past_Y_GLU[ R_LT_GLU ] ) ),
    X_AGLU_historical_years ]
L123.LC_MgdPastFrac_Y_GLU[ is.na( L123.LC_MgdPastFrac_Y_GLU) ] <- 0
L123.LC_MgdPastFrac_Y_GLU_adj <- L123.LC_MgdPastFrac_Y_GLU
L123.LC_MgdPastFrac_Y_GLU_adj[ X_AGLU_historical_years ][ 
  L123.LC_MgdPastFrac_Y_GLU_adj[ X_AGLU_historical_years ] > max_MgdPast_frac ] <- max_MgdPast_frac

#Recalculate managed pasture land, adjusted by assumed maximum portion that can be managed
L123.LC_bm2_R_MgdPast_Y_GLU_adj <- L123.LC_bm2_R_Past_Y_GLU
L123.LC_bm2_R_MgdPast_Y_GLU_adj[ X_AGLU_historical_years ] <- L123.LC_bm2_R_MgdPast_Y_GLU_adj[ X_AGLU_historical_years ] *
  L123.LC_MgdPastFrac_Y_GLU_adj[
    match( vecpaste( L123.LC_bm2_R_MgdPast_Y_GLU_adj[ R_LT_GLU ] ),
           vecpaste( L123.LC_MgdPastFrac_Y_GLU_adj[ R_LT_GLU ] ) ),
    X_AGLU_historical_years ]

#Calculate pasture yield (after adjustments)
L123.ag_Yield_kgm2_R_Past_Y_GLU <- L123.ag_Prod_Mt_R_Past_Y_GLU
L123.ag_Yield_kgm2_R_Past_Y_GLU[ X_AGLU_historical_years ] <- L123.ag_Yield_kgm2_R_Past_Y_GLU[ X_AGLU_historical_years ] /
  L123.LC_bm2_R_MgdPast_Y_GLU_adj[
    match( vecpaste( L123.ag_Yield_kgm2_R_Past_Y_GLU[ R_C_GLU ] ),
           vecpaste( L123.LC_bm2_R_MgdPast_Y_GLU_adj[ R_LT_GLU ] ) ),
    X_AGLU_historical_years ]
L123.ag_Yield_kgm2_R_Past_Y_GLU[ is.na( L123.ag_Yield_kgm2_R_Past_Y_GLU ) ] <- L123.ag_Yield_kgm2_Past

#Multiply "managed" shares in the earliest available year by prior pasture land cover pathway to get historical managed pasture
printlog( "Building managed pasture land use history" )
printlog( "NOTE: Assuming same managed:unmanaged pasture ratio in early land cover years" )
L123.LC_bm2_R_Past_Yhh_GLU <- subset( L120.LC_bm2_R_LT_Yh_GLU, Land_Type=="Pasture", select = c( R_LT_GLU, X_preAGLU_years ) )
L123.LC_bm2_R_MgdPast_Yh_GLU <- L123.LC_bm2_R_MgdPast_Y_GLU_adj
L123.LC_bm2_R_MgdPast_Yh_GLU[ X_preAGLU_years ] <- L123.LC_MgdPastFrac_Y_GLU_adj[[ X_AGLU_historical_years[1] ]] *
      L123.LC_bm2_R_Past_Yhh_GLU[
        match( vecpaste( L123.LC_MgdPastFrac_Y_GLU_adj[ R_LT_GLU ] ),
               vecpaste( L123.LC_bm2_R_Past_Yhh_GLU[ R_LT_GLU ] ) ),
        X_preAGLU_years ]
L123.LC_bm2_R_MgdPast_Yh_GLU <- L123.LC_bm2_R_MgdPast_Yh_GLU[ c( R_LT_GLU, X_land_cover_years ) ]

#FORESTRY
#Carbon densities are divided by mature age to get net primary productivity, and used to derive exogenous yields for separating managed/unmanaged forest
#calculate veg mass of each GLU based on above-ground carbon content of each GLU.
printlog( "Part 2: Managed forest production and land cover" )
printlog( "NOTE: Using average vegetation carbon densities and mature ages to estimate annual forest biomass production" )
L123.For_Yield_m3m2_R_GLU <- L121.CarbonContent_kgm2_R_LT_GLU[ L121.CarbonContent_kgm2_R_LT_GLU[[LT]] =="Forest", ]
L123.For_Yield_m3m2_R_GLU$VegVolume_m3m2 <- L123.For_Yield_m3m2_R_GLU$veg_c / AvgWoodDensity_kgCm3
L123.For_Yield_m3m2_R_GLU$Yield_m3m2 <- with( L123.For_Yield_m3m2_R_GLU, VegVolume_m3m2 / mature.age )

#disaggregate logging to GLU on the basis of annual biomass production rates
printlog( "NOTE: Using forest biomass production (annual yield) by GLU to disaggregate regional wood production to GLUs" )
L123.For_potentialProd_bm3_R_Y_GLU <- L120.LC_bm2_R_LT_Yh_GLU[
  L120.LC_bm2_R_LT_Yh_GLU[[LT]] == "Forest", c( R_LT_GLU, X_AGLU_historical_years ) ]
names( L123.For_potentialProd_bm3_R_Y_GLU )[ names( L123.For_potentialProd_bm3_R_Y_GLU ) == LT ] <- C
L123.For_potentialProd_bm3_R_Y_GLU[ X_AGLU_historical_years ] <- L123.For_potentialProd_bm3_R_Y_GLU[ X_AGLU_historical_years ] *
  L123.For_Yield_m3m2_R_GLU$Yield_m3m2[
      match( vecpaste( L123.For_potentialProd_bm3_R_Y_GLU[ R_GLU ] ),
             vecpaste( L123.For_Yield_m3m2_R_GLU[ R_GLU ] ) ) ]

#Cast regional forestry output into the same format as these tables
L110.For_ALL_bm3_R_Y$Xyear <- paste0( "X", L110.For_ALL_bm3_R_Y[[Y]] )
L123.For_ALL_bm3_R_Y <- dcast( L110.For_ALL_bm3_R_Y, GCAM_region_ID + GCAM_commodity ~ Xyear, value.var = "Prod_bm3" )

#Forest output by GLU = Regional forest output * GLU-wise forest biomass production fraction
L123.For_potentialProd_bm3_R_Y <- aggregate( L123.For_potentialProd_bm3_R_Y_GLU[ X_AGLU_historical_years ],
                                             by = L123.For_potentialProd_bm3_R_Y_GLU[ R_C ], sum )
L123.For_Prodfrac_R_Y_GLU <- L123.For_potentialProd_bm3_R_Y_GLU[ R_C_GLU ]
L123.For_Prodfrac_R_Y_GLU[ X_AGLU_historical_years ] <- L123.For_potentialProd_bm3_R_Y_GLU[ X_AGLU_historical_years ] /
  L123.For_potentialProd_bm3_R_Y[
      match( L123.For_potentialProd_bm3_R_Y_GLU[[R]],
             L123.For_potentialProd_bm3_R_Y[[R]] ),
      X_AGLU_historical_years ]

printlog( "Calculating logging production as the regional total times the GLU-wise production fractions" )
L123.For_Prod_bm3_R_Y_GLU <- L123.For_Prodfrac_R_Y_GLU[ R_C_GLU]
L123.For_Prod_bm3_R_Y_GLU[ X_AGLU_historical_years ] <- L123.For_Prodfrac_R_Y_GLU[ X_AGLU_historical_years ] * L123.For_ALL_bm3_R_Y[
      match( L123.For_Prodfrac_R_Y_GLU[[R]],
             L123.For_ALL_bm3_R_Y[[R]] ),
      X_AGLU_historical_years ]

#Calculate land cover of "managed" forest as the output divided by the yield ( net primary production ), in each GLU and region
L123.LC_bm2_R_MgdFor_Y_GLU <- L123.For_Prod_bm3_R_Y_GLU[ R_C_GLU ]
names( L123.LC_bm2_R_MgdFor_Y_GLU )[ names( L123.LC_bm2_R_MgdFor_Y_GLU ) == C ] <- LT
L123.LC_bm2_R_MgdFor_Y_GLU[ X_AGLU_historical_years ] <- L123.For_Prod_bm3_R_Y_GLU[ X_AGLU_historical_years ] / L123.For_Yield_m3m2_R_GLU$Yield_m3m2[
      match( vecpaste( L123.For_Prod_bm3_R_Y_GLU[ R_GLU ] ),
             vecpaste( L123.For_Yield_m3m2_R_GLU[ R_GLU ] ) ) ]

#Use historical population ratios to estimate managed forest in the pre-AGLU years
printlog( "Building managed forest land use history" )
printlog( "NOTE: Scaling historical managed forest land area with historical population" )
L123.PopRatio_R_Yhh <- L101.Pop_thous_R_Yh[ c( R, X_preAGLU_years ) ]
L123.PopRatio_R_Yhh[ X_preAGLU_years ] <- L101.Pop_thous_R_Yh[ X_preAGLU_years ] / L101.Pop_thous_R_Yh[[ X_AGLU_historical_years[1] ]]

L123.LC_bm2_R_MgdFor_Yh_GLU <- L123.LC_bm2_R_MgdFor_Y_GLU
L123.LC_bm2_R_MgdFor_Yh_GLU[ X_preAGLU_years ] <- L123.LC_bm2_R_MgdFor_Y_GLU[[ X_AGLU_historical_years[1] ]] * L123.PopRatio_R_Yhh[
      match( L123.LC_bm2_R_MgdFor_Y_GLU[[R]], L123.PopRatio_R_Yhh[[R]] ),
      X_preAGLU_years ]
L123.LC_bm2_R_MgdFor_Yh_GLU <- L123.LC_bm2_R_MgdFor_Yh_GLU[ c( R_LT_GLU, X_land_cover_years ) ]

#Where managed forest is greater than assumed maximum percentage of total forest, reduce the managed forest land.
#Output is unaffected so these regions have higher yields.
printlog( "Applying maximum percentage of any region/GLUs forest that is allowed to be in production (managed)" )
printlog( "NOTE: In region/GLUs where applicable, this threshold will result in increased forest yields" )
L123.LC_bm2_R_For_Yh_GLU <- L120.LC_bm2_R_LT_Yh_GLU[ L120.LC_bm2_R_LT_Yh_GLU[[LT]] == "Forest", c( R_LT_GLU, X_land_cover_years ) ]
L123.LC_MgdForFrac_Yh_GLU <- L123.LC_bm2_R_MgdFor_Yh_GLU[ R_LT_GLU ]
L123.LC_MgdForFrac_Yh_GLU[ X_land_cover_years ] <- L123.LC_bm2_R_MgdFor_Yh_GLU[ X_land_cover_years ] /
  L123.LC_bm2_R_For_Yh_GLU[
    match( vecpaste( L123.LC_bm2_R_MgdFor_Yh_GLU[ R_LT_GLU ] ),
           vecpaste( L123.LC_bm2_R_For_Yh_GLU[ R_LT_GLU ] ) ),
    X_land_cover_years ]
#Re-set missing values to zeroes
L123.LC_MgdForFrac_Yh_GLU[ is.na( L123.LC_MgdForFrac_Yh_GLU) ] <- 0
L123.LC_MgdForFrac_Yh_GLU_adj <- L123.LC_MgdForFrac_Yh_GLU
L123.LC_MgdForFrac_Yh_GLU_adj[ X_land_cover_years ][ L123.LC_MgdForFrac_Yh_GLU_adj[ X_land_cover_years ] > max_MgdFor_frac ] <- max_MgdFor_frac

#Recalculate managed forest land, adjusted by assumed maximum portion that can be managed
L123.LC_bm2_R_MgdFor_Yh_GLU <- L123.LC_bm2_R_For_Yh_GLU[ R_LT_GLU ]
L123.LC_bm2_R_MgdFor_Yh_GLU[ X_land_cover_years ] <- L123.LC_bm2_R_For_Yh_GLU[ X_land_cover_years ] * 
  L123.LC_MgdForFrac_Yh_GLU_adj[
    match( vecpaste( L123.LC_bm2_R_For_Yh_GLU[ R_LT_GLU ] ),
           vecpaste( L123.LC_MgdForFrac_Yh_GLU_adj[ R_LT_GLU ] ) ),
    X_land_cover_years ]

#Recalculate forestry yield
L123.For_Yield_m3m2_R_GLU <- L123.For_Prod_bm3_R_Y_GLU[ R_C_GLU ]
L123.For_Yield_m3m2_R_GLU[ X_AGLU_historical_years ] <- L123.For_Prod_bm3_R_Y_GLU[ X_AGLU_historical_years ] /
  L123.LC_bm2_R_MgdFor_Yh_GLU[
    match( vecpaste( L123.For_Prod_bm3_R_Y_GLU[ R_GLU ]),
           vecpaste( L123.LC_bm2_R_MgdFor_Yh_GLU[ R_GLU ] ) ),
    X_AGLU_historical_years ]
#Set missing values to an assumed minimum forestry yield, from the data where available
L123.For_Yield_m3m2_R_GLU[ is.na( L123.For_Yield_m3m2_R_GLU ) ] <- min( L123.For_Yield_m3m2_R_GLU[[X_final_historical_year]], na.rm = T)

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L123.ag_Prod_Mt_R_Past_Y_GLU <- c( "Pasture production by GCAM region / year / GLU","Unit = Mt" )
comments.L123.ag_Yield_kgm2_R_Past_Y_GLU <- c( "Pasture yield by GCAM region / year / GLU","Unit = kg/m2" )
comments.L123.LC_bm2_R_MgdPast_Yh_GLU <- c( "Managed pasture land cover by GCAM region / historical year / GLU","Unit = bm2" )
comments.L123.For_Prod_bm3_R_Y_GLU <- c( "Forest production by GCAM region / year / GLU","Unit = bm3" )
comments.L123.For_Yield_m3m2_R_GLU <- c( "Forest yield by GCAM region / year / GLU","Unit = m3/m2" )
comments.L123.LC_bm2_R_MgdFor_Yh_GLU <- c( "Managed forest land cover by GCAM region / historical year / GLU","Unit = bm2" )

writedata( L123.ag_Prod_Mt_R_Past_Y_GLU, domain="AGLU_LEVEL1_DATA", fn="L123.ag_Prod_Mt_R_Past_Y_GLU", comments=comments.L123.ag_Prod_Mt_R_Past_Y_GLU )
writedata( L123.ag_Yield_kgm2_R_Past_Y_GLU, domain="AGLU_LEVEL1_DATA", fn="L123.ag_Yield_kgm2_R_Past_Y_GLU", comments=comments.L123.ag_Yield_kgm2_R_Past_Y_GLU )
writedata( L123.LC_bm2_R_MgdPast_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L123.LC_bm2_R_MgdPast_Yh_GLU", comments=comments.L123.LC_bm2_R_MgdPast_Yh_GLU )
writedata( L123.For_Prod_bm3_R_Y_GLU, domain="AGLU_LEVEL1_DATA", fn="L123.For_Prod_bm3_R_Y_GLU", comments=comments.L123.For_Prod_bm3_R_Y_GLU )
writedata( L123.For_Yield_m3m2_R_GLU, domain="AGLU_LEVEL1_DATA", fn="L123.For_Yield_m3m2_R_GLU", comments=comments.L123.For_Yield_m3m2_R_GLU )
writedata( L123.LC_bm2_R_MgdFor_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L123.LC_bm2_R_MgdFor_Yh_GLU", comments=comments.L123.LC_bm2_R_MgdFor_Yh_GLU )

# Every script should finish with this line
logstop()
