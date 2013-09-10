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
logstart( "LA101.ag_FAO_R_C_Y.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Primary agricultural good data from FAO, assigned to GCAM region / commodity / year" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
FAO_ag_items_cal_SUA <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_cal_SUA" )
L100.FAO_ag_Food_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_ag_Food_t" )
L100.FAO_ag_HA_ha <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_ag_HA_ha" )
L100.FAO_ag_Prod_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_ag_Prod_t" )

# -----------------------------------------------------------------------------

# 2. Perform computations
L101.FAO_ag_Food_t <- L100.FAO_ag_Food_t[ c( "iso", "item", X_AGLU_historical_years ) ]
L101.FAO_ag_HA_ha <- L100.FAO_ag_HA_ha[ c( "iso", "item", X_AGLU_historical_years ) ]
L101.FAO_ag_Prod_t <- L100.FAO_ag_Prod_t[ c( "iso", "item", X_AGLU_historical_years ) ]
printlog( "Dividing USA Alfalfa by 4 for consistency with USDA" )
L101.FAO_ag_Prod_t[ L101.FAO_ag_Prod_t$iso == "usa" &
                             L101.FAO_ag_Prod_t$item == "Alfalfa for forage and silage", X_AGLU_historical_years ] <-
      L101.FAO_ag_Prod_t[ L101.FAO_ag_Prod_t$iso == "usa" &
                             L101.FAO_ag_Prod_t$item == "Alfalfa for forage and silage", X_AGLU_historical_years ] / 4      

#add lookup vector for GCAM regions
printlog( "Adding region and crop lookup vectors to FAO tables" )
L101.FAO_ag_Food_t$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L101.FAO_ag_Food_t$iso, iso_GCAM_regID$iso ) ]
L101.FAO_ag_HA_ha$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L101.FAO_ag_HA_ha$iso, iso_GCAM_regID$iso ) ]
L101.FAO_ag_Prod_t$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L101.FAO_ag_Prod_t$iso, iso_GCAM_regID$iso ) ]

#add lookup vectors for GCAM crop names
L101.FAO_ag_Food_t$GCAM_commodity <- FAO_ag_items_cal_SUA$GCAM_commodity[ match( L101.FAO_ag_Food_t$item, FAO_ag_items_cal_SUA$item ) ]
L101.FAO_ag_HA_ha$GCAM_commodity <- FAO_ag_items_PRODSTAT$GCAM_commodity[ match( L101.FAO_ag_HA_ha$item, FAO_ag_items_PRODSTAT$item ) ]
L101.FAO_ag_Prod_t$GCAM_commodity <- FAO_ag_items_PRODSTAT$GCAM_commodity[ match( L101.FAO_ag_Prod_t$item, FAO_ag_items_PRODSTAT$item ) ]

#add lookup vectors for Mcal conversions for consumption tables
printlog( "Computing FAO ag commodity consumption in terms of calories" )
L101.FAO_ag_Food_t$Mcal_t <- FAO_ag_items_cal_SUA$Mcal_t[ match( L101.FAO_ag_Food_t$item, FAO_ag_items_cal_SUA$item ) ]

#build table for food consumption in Mcal
L101.FAO_ag_Food_Mcal_SUA <- L101.FAO_ag_Food_t
L101.FAO_ag_Food_Mcal_SUA[ X_AGLU_historical_years ]  <- L101.FAO_ag_Food_t[ X_AGLU_historical_years ] * L101.FAO_ag_Food_t$Mcal_t

#build tables collapsed by GCAM region and crop name
printlog( "Collapsing FAO ag commodity data into GCAM regions and commodities" )
L101.ag_Food_t_R_C_Y <- aggregate( L101.FAO_ag_Food_t[ X_AGLU_historical_years ],
      by=as.list( L101.FAO_ag_Food_t[ R_C ] ), sum )
L101.ag_Food_Mcal_R_C_Y <- aggregate( L101.FAO_ag_Food_Mcal_SUA[ X_AGLU_historical_years ],
      by=as.list( L101.FAO_ag_Food_Mcal_SUA[ R_C ] ), sum )
L101.ag_HA_ha_R_C_Y <- aggregate( L101.FAO_ag_HA_ha[ X_AGLU_historical_years ], by=as.list( L101.FAO_ag_HA_ha[ R_C ] ), sum )
L101.ag_Prod_t_R_C_Y <- aggregate( L101.FAO_ag_Prod_t[ X_AGLU_historical_years ], by=as.list( L101.FAO_ag_Prod_t[ R_C ] ), sum )

#Convert to desired units (Mt, Pcal, and bm2)
printlog( "Converting mass to Mt, energy to Pcal, and area to bm2 (thous km2)" )
#Specifying these as preliminary tables because they do not necessarily include all region x commodity combinations
L101.ag_Food_Mt_R_C_Y_prelim <- data.frame( L101.ag_Food_t_R_C_Y[ R_C ], L101.ag_Food_t_R_C_Y[ X_AGLU_historical_years ] * conv_t_Mt )
L101.ag_Food_Pcal_R_C_Y_prelim <- data.frame( L101.ag_Food_Mcal_R_C_Y[ R_C ], L101.ag_Food_Mcal_R_C_Y[ X_AGLU_historical_years ] * conv_Mcal_Pcal )
L101.ag_HA_bm2_R_C_Y_prelim <- data.frame( L101.ag_HA_ha_R_C_Y[ R_C ], L101.ag_HA_ha_R_C_Y[ X_AGLU_historical_years ] * conv_Ha_bm2 )
L101.ag_Prod_Mt_R_C_Y_prelim <- data.frame( L101.ag_Prod_t_R_C_Y[ R_C ], L101.ag_Prod_t_R_C_Y[ X_AGLU_historical_years ] * conv_t_Mt )

#Calculate average caloric content of consumed commodities (kcal/g)
printlog("Calculating weighted average caloric content of food commodities, in kcal/g")
L101.ag_kcalg_R_C_Y_prelim <- data.frame( L101.ag_Food_Pcal_R_C_Y_prelim[ R_C ],
   L101.ag_Food_Pcal_R_C_Y_prelim[ X_AGLU_historical_years ] / L101.ag_Food_Mt_R_C_Y_prelim[ X_AGLU_historical_years ] )

#Translate these to full tables, where no values can be missing
L101.ag_Food_Mt_R_C_Y <- translate_to_full_table( L101.ag_Food_Mt_R_C_Y_prelim,
      R, unique( iso_GCAM_regID$GCAM_region_ID ),
      C, unique( L101.ag_Food_Mt_R_C_Y_prelim$GCAM_commodity ) )
L101.ag_Food_Pcal_R_C_Y <- translate_to_full_table( L101.ag_Food_Pcal_R_C_Y_prelim,
      R, unique( iso_GCAM_regID$GCAM_region_ID ),
      C, unique( L101.ag_Food_Pcal_R_C_Y_prelim$GCAM_commodity ) )
L101.ag_kcalg_R_C_Y <- translate_to_full_table( L101.ag_kcalg_R_C_Y_prelim,
      R, unique( iso_GCAM_regID$GCAM_region_ID ),
      C, unique( L101.ag_kcalg_R_C_Y_prelim$GCAM_commodity ), na.value = 1 )
L101.ag_HA_bm2_R_C_Y <- translate_to_full_table( L101.ag_HA_bm2_R_C_Y_prelim,
      R, unique( iso_GCAM_regID$GCAM_region_ID ),
      C, unique( L101.ag_HA_bm2_R_C_Y_prelim$GCAM_commodity ) )
L101.ag_Prod_Mt_R_C_Y <- translate_to_full_table( L101.ag_Prod_Mt_R_C_Y_prelim,
      R, unique( iso_GCAM_regID$GCAM_region_ID ),
      C, unique( L101.ag_Prod_Mt_R_C_Y_prelim$GCAM_commodity ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L101.ag_Food_Mt_R_C_Y <- c( "Food consumption by GCAM region / commodity / year","Unit = Mt" )
comments.L101.ag_Food_Pcal_R_C_Y <- c( "Food consumption by GCAM region / commodity / year","Unit = Pcal" )
comments.L101.ag_kcalg_R_C_Y <- c( "Weighted average commodity caloric content by GCAM region / commodity / year","Unit = kcal/g" )
comments.L101.ag_HA_bm2_R_C_Y <- c( "Harvested area by GCAM region / commodity (cotton separated) / year","Unit = bm2 (thous km2)" )
comments.L101.ag_Prod_Mt_R_C_Y <- c( "Agricultural production by GCAM region / commodity (cotton separated) / year","Unit = Mt" )

#write tables as CSV files
writedata( L101.ag_Food_Mt_R_C_Y, domain="AGLU_LEVEL1_DATA", fn="L101.ag_Food_Mt_R_C_Y", comments=comments.L101.ag_Food_Mt_R_C_Y )
writedata( L101.ag_Food_Pcal_R_C_Y, domain="AGLU_LEVEL1_DATA", fn="L101.ag_Food_Pcal_R_C_Y", comments=comments.L101.ag_Food_Pcal_R_C_Y )
writedata( L101.ag_kcalg_R_C_Y, domain="AGLU_LEVEL1_DATA", fn="L101.ag_kcalg_R_C_Y", comments=comments.L101.ag_kcalg_R_C_Y )
writedata( L101.ag_HA_bm2_R_C_Y, domain="AGLU_LEVEL1_DATA", fn="L101.ag_HA_bm2_R_C_Y", comments=comments.L101.ag_HA_bm2_R_C_Y )
writedata( L101.ag_Prod_Mt_R_C_Y, domain="AGLU_LEVEL1_DATA", fn="L101.ag_Prod_Mt_R_C_Y", comments=comments.L101.ag_Prod_Mt_R_C_Y )

# Every script should finish with this line
logstop()
