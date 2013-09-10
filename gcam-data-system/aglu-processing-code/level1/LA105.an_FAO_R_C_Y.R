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
logstart( "LA105.an_FAO_R_C_Y.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Animal product data from FAO, assigned to GCAM region / commodity / year" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_an_items_cal_SUA <- readdata( "AGLU_MAPPINGS", "FAO_an_items_cal_SUA" )
FAO_an_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_an_items_PRODSTAT" )
L100.FAO_an_Food_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_an_Food_t" )
L100.FAO_an_Prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_an_Prod_t" )

# -----------------------------------------------------------------------------
# 2. Perform computations

#add lookup vectors for iso codes and then GCAM regions
L100.FAO_an_Food_t$iso <- AGLU_ctry$iso[ match( L100.FAO_an_Food_t$countries, AGLU_ctry$FAO_country ) ]
L100.FAO_an_Prod_t$iso <- AGLU_ctry$iso[ match( L100.FAO_an_Prod_t$countries, AGLU_ctry$FAO_country ) ]
L100.FAO_an_Food_t$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L100.FAO_an_Food_t$iso, iso_GCAM_regID$iso ) ]
L100.FAO_an_Prod_t$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L100.FAO_an_Prod_t$iso, iso_GCAM_regID$iso ) ]

#add lookup vector for GCAM commodity names
L100.FAO_an_Food_t$GCAM_commodity <- FAO_an_items_cal_SUA$GCAM_commodity[ match( L100.FAO_an_Food_t$item, FAO_an_items_cal_SUA$item ) ]
L100.FAO_an_Prod_t$GCAM_commodity <- FAO_an_items_cal_SUA$GCAM_commodity[ match( L100.FAO_an_Prod_t$item, FAO_an_items_cal_SUA$item ) ]

#add lookup vector for calorie conversions
L100.FAO_an_Food_t$Mcal_t <- FAO_an_items_cal_SUA$Mcal_t[ match( L100.FAO_an_Food_t$item, FAO_an_items_cal_SUA$item ) ]

#build table for consumption in terms of calories
L105.FAO_an_Food_Mcal <- L100.FAO_an_Food_t
L105.FAO_an_Food_Mcal[ X_AGLU_historical_years ] <- L100.FAO_an_Food_t[ X_AGLU_historical_years ] *  L100.FAO_an_Food_t$Mcal_t

#Compile animal production by GCAM commodity and country (iso)
L105.an_Prod_t_ctry_C_Y <- aggregate( L100.FAO_an_Prod_t[ X_AGLU_historical_years ], by=as.list( L100.FAO_an_Prod_t[ c( "iso", C ) ] ), sum )

#build tables collapsed by GCAM region and animal product name
L105.an_Food_t_R_C_Y <- aggregate( L100.FAO_an_Food_t[ X_AGLU_historical_years ], by=as.list( L100.FAO_an_Food_t[ R_C ] ), sum )
L105.an_Food_Mcal_R_C_Y <- aggregate( L105.FAO_an_Food_Mcal[ X_AGLU_historical_years ], by=as.list( L105.FAO_an_Food_Mcal[ R_C ] ), sum )
L105.an_Prod_t_R_C_Y <- aggregate( L100.FAO_an_Prod_t[ X_AGLU_historical_years ], by=as.list( L100.FAO_an_Prod_t[ R_C ] ), sum )

#Convert to desired units (Mt, Pcal, and bm2)
printlog( "Converting mass to Mt, energy to Pcal, and area to bm2 (thous km2)" )
L105.an_Food_Mt_R_C_Y_prelim <- cbind( L105.an_Food_t_R_C_Y[ R_C ], L105.an_Food_t_R_C_Y[ X_AGLU_historical_years ] * conv_t_Mt )
L105.an_Food_Pcal_R_C_Y_prelim <- cbind( L105.an_Food_Mcal_R_C_Y[ R_C ], L105.an_Food_Mcal_R_C_Y[ X_AGLU_historical_years ] * conv_Mcal_Pcal )
L105.an_Prod_Mt_R_C_Y_prelim <- cbind( L105.an_Prod_t_R_C_Y[ R_C ], L105.an_Prod_t_R_C_Y[ X_AGLU_historical_years ] * conv_t_Mt )
L105.an_Prod_Mt_ctry_C_Y_prelim <- cbind( L105.an_Prod_t_ctry_C_Y[ c( "iso", C) ], L105.an_Prod_t_ctry_C_Y[ X_AGLU_historical_years ] * conv_t_Mt )

#Calculate Mt to Pcal conversion for each region and animal type
L105.an_kcalg_R_C_Y_prelim <- cbind( L105.an_Food_Mt_R_C_Y_prelim[ R_C ],
      L105.an_Food_Pcal_R_C_Y_prelim[ X_AGLU_historical_years ] / L105.an_Food_Mt_R_C_Y_prelim[ X_AGLU_historical_years ] )

#Translate these to full tables, where no values can be missing
L105.an_Food_Mt_R_C_Y <- translate_to_full_table( L105.an_Food_Mt_R_C_Y_prelim,
      R, unique( iso_GCAM_regID$GCAM_region_ID ),
      C, unique( L105.an_Food_Mt_R_C_Y_prelim$GCAM_commodity ) )
L105.an_Food_Pcal_R_C_Y <- translate_to_full_table( L105.an_Food_Pcal_R_C_Y_prelim,
      R, unique( iso_GCAM_regID$GCAM_region_ID ),
      C, unique( L105.an_Food_Pcal_R_C_Y_prelim$GCAM_commodity ) )
L105.an_kcalg_R_C_Y <- translate_to_full_table( L105.an_kcalg_R_C_Y_prelim,
      R, unique( iso_GCAM_regID$GCAM_region_ID ),
      C, unique( L105.an_kcalg_R_C_Y_prelim$GCAM_commodity ), na.value = 1 )
L105.an_Prod_Mt_R_C_Y <- translate_to_full_table( L105.an_Prod_Mt_R_C_Y_prelim,
      R, unique( iso_GCAM_regID$GCAM_region_ID ),
      C, unique( L105.an_Prod_Mt_R_C_Y_prelim$GCAM_commodity ) )
L105.an_Prod_Mt_ctry_C_Y <- translate_to_full_table( L105.an_Prod_Mt_ctry_C_Y_prelim,
      "iso", unique( iso_GCAM_regID$iso ),
      C, unique( L105.an_Prod_Mt_ctry_C_Y_prelim$GCAM_commodity ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L105.an_Food_Mt_R_C_Y <- c( "Animal consumption by GCAM region / commodity / year", "Unit = Mt" )
comments.L105.an_Food_Pcal_R_C_Y <- c( "Animal consumption by GCAM region / commodity / year", "Unit = Pcal" )
comments.L105.an_kcalg_R_C_Y <- c( "Average caloric content of animal products by GCAM region / commodity / year", "Unit = kcal/g" )
comments.L105.an_Prod_Mt_R_C_Y <- c( "Animal production by GCAM region / commodity / year", "Unit = Mt" )
comments.L105.an_Prod_Mt_ctry_C_Y <- c( "Animal production by country / commodity / year", "Unit = Mt" )

#write tables as CSV files
writedata( L105.an_Food_Mt_R_C_Y, domain="AGLU_LEVEL1_DATA", fn="L105.an_Food_Mt_R_C_Y", comments=comments.L105.an_Food_Mt_R_C_Y )
writedata( L105.an_Food_Pcal_R_C_Y, domain="AGLU_LEVEL1_DATA", fn="L105.an_Food_Pcal_R_C_Y", comments=comments.L105.an_Food_Pcal_R_C_Y )
writedata( L105.an_kcalg_R_C_Y, domain="AGLU_LEVEL1_DATA", fn="L105.an_kcalg_R_C_Y", comments=comments.L105.an_kcalg_R_C_Y )
writedata( L105.an_Prod_Mt_R_C_Y, domain="AGLU_LEVEL1_DATA", fn="L105.an_Prod_Mt_R_C_Y", comments=comments.L105.an_Prod_Mt_R_C_Y )
writedata( L105.an_Prod_Mt_ctry_C_Y, domain="AGLU_LEVEL1_DATA", fn="L105.an_Prod_Mt_ctry_C_Y", comments=comments.L105.an_Prod_Mt_ctry_C_Y )

# Every script should finish with this line
logstop()           
