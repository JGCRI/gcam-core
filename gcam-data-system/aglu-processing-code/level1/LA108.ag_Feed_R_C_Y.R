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
logstart( "LA108.ag_Feed_R_C_Y.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Agricultural feed production by GCAM region / commodity / year" )

# -----------------------------------------------------------------------------
# 1. Read data
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
FAO_ag_items_cal_SUA <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_cal_SUA" )
L100.FAO_ag_Feed_t <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_ag_Feed_t" )
L104.ag_Prod_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L104.ag_Prod_Mt_R_C_Y" )
L107.an_Feed_Mt_R_C_Sys_Fd_Y <- readdata( "AGLU_LEVEL1_DATA", "L107.an_Feed_Mt_R_C_Sys_Fd_Y" )

# -----------------------------------------------------------------------------
# 2. Perform computations

#FEEDCROPS
printlog( "Part 1: Feedcrops" )
#Compile regional feedcrop demands by crop type from FAO balances. Import data, add identifier vectors, and aggregate.
printlog( "Aggregating FAO feedcrop consumption by GCAM region, commodity, and year" )
L100.FAO_ag_Feed_t$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L100.FAO_ag_Feed_t$iso, iso_GCAM_regID$iso ) ]
L100.FAO_ag_Feed_t$GCAM_commodity <- FAO_ag_items_cal_SUA$GCAM_commodity[ match( L100.FAO_ag_Feed_t$item, FAO_ag_items_cal_SUA$item ) ]
L108.ag_Feed_t_R_Cnf_Y <- aggregate( L100.FAO_ag_Feed_t[ X_AGLU_historical_years ], by=as.list( L100.FAO_ag_Feed_t[ R_C ] ), sum )

#Convert to desired units ( Mt ), and write it out to a table with all combinations of region x commodity
printlog( "Converting to Mt and writing out all combinations of region x commodity" )
L108.ag_Feed_Mt_R_Cnf_Y_prelim <- cbind( L108.ag_Feed_t_R_Cnf_Y[ R_C ], L108.ag_Feed_t_R_Cnf_Y[ X_AGLU_historical_years ] * conv_t_Mt )
L108.ag_Feed_Mt_R_Cnf_Y <- translate_to_full_table( L108.ag_Feed_Mt_R_Cnf_Y_prelim,
      R, unique( iso_GCAM_regID$GCAM_region_ID ),
      C, unique( L108.ag_Feed_Mt_R_Cnf_Y_prelim$GCAM_commodity ) )
             
#Aggregate FAO feed data by region in order to calculate proportions to assign to each crop
printlog( "Calculating feedcrop shares by crop within each region" )
L108.ag_Feed_Mt_R_Y <- aggregate( L108.ag_Feed_Mt_R_Cnf_Y[ X_AGLU_historical_years ], by=as.list( L108.ag_Feed_Mt_R_Cnf_Y[ R ] ), sum )
L108.ag_Feedfrac_R_Cnf_Y <- L108.ag_Feed_Mt_R_Cnf_Y
L108.ag_Feedfrac_R_Cnf_Y[ X_AGLU_historical_years ] <- L108.ag_Feed_Mt_R_Cnf_Y[ X_AGLU_historical_years ] / L108.ag_Feed_Mt_R_Y[
      match( L108.ag_Feedfrac_R_Cnf_Y$GCAM_region_ID, L108.ag_Feed_Mt_R_Y$GCAM_region_ID ),
      X_AGLU_historical_years ]
L108.ag_Feedfrac_R_Cnf_Y[ is.na( L108.ag_Feedfrac_R_Cnf_Y ) ] <- 0

#Compile regional total feedcrop demands from IMAGE data
printlog( "Multiplying feedcrop shares by IMAGE-based total regional feedcrop demands" )
L108.ag_Feed_Mt_R_F_Y <- aggregate( L107.an_Feed_Mt_R_C_Sys_Fd_Y[ X_AGLU_historical_years ],
     by=as.list( L107.an_Feed_Mt_R_C_Sys_Fd_Y[ R_Fd ] ), sum )
L108.ag_Feed_Mt_R_FeedCrop_Y <- subset( L108.ag_Feed_Mt_R_F_Y, feed=="FeedCrops" )
L108.ag_Feed_Mt_R_Cnf_Y_adj <- L108.ag_Feedfrac_R_Cnf_Y
L108.ag_Feed_Mt_R_Cnf_Y_adj[ X_AGLU_historical_years ] <- L108.ag_Feedfrac_R_Cnf_Y[ X_AGLU_historical_years ] * L108.ag_Feed_Mt_R_F_Y[
      match( L108.ag_Feed_Mt_R_Cnf_Y_adj$GCAM_region_ID, L108.ag_Feed_Mt_R_F_Y$GCAM_region_ID ),
      X_AGLU_historical_years ]

#FODDERHERB/RESIDUE
printlog( "Part 2: Calculating FodderHerb and Residue balances by region and year" )
#Calculate FodderHerb production by region, and fodderherb_residue demands by region
L108.ag_Prod_Mt_R_FodderHerb_Y <- subset( L104.ag_Prod_Mt_R_C_Y, GCAM_commodity=="FodderHerb" )
L108.ag_Feed_Mt_R_FodderHerbResidue_Y <- subset( L108.ag_Feed_Mt_R_F_Y, feed=="FodderHerb_Residue" )

#Calculate the residual in each region (production minus demand). Split positive and negative into separate tables for computation of each country's share of each
L108.ag_Residual_Mt_R_FodderHerbResidue_Y <- L108.ag_Feed_Mt_R_FodderHerbResidue_Y
L108.ag_Residual_Mt_R_FodderHerbResidue_Y[ X_AGLU_historical_years ] <- L108.ag_Prod_Mt_R_FodderHerb_Y[ X_AGLU_historical_years ] -
      L108.ag_Feed_Mt_R_FodderHerbResidue_Y[ X_AGLU_historical_years ]
#Positive residuals = regions whose fodderherb production exceeds the demands for fodderherb/residue
L108.ag_PosResidual_Mt_R_FodderHerbResidue_Y <- L108.ag_Residual_Mt_R_FodderHerbResidue_Y
L108.ag_PosResidual_Mt_R_FodderHerbResidue_Y[ X_AGLU_historical_years ][ L108.ag_PosResidual_Mt_R_FodderHerbResidue_Y[ X_AGLU_historical_years ]  < 0 ] <- 0
#Negative residuals = regions whose fodderherb production is less than demands for fodderherb/residue
L108.ag_NegResidual_Mt_R_FodderHerbResidue_Y <- L108.ag_Residual_Mt_R_FodderHerbResidue_Y
L108.ag_NegResidual_Mt_R_FodderHerbResidue_Y[ X_AGLU_historical_years ][ L108.ag_NegResidual_Mt_R_FodderHerbResidue_Y[ X_AGLU_historical_years ]  > 0 ] <- 0

#Aggregate the global totals of residual (net), and individual regions' shares of positive and negative residuals
L108.ag_Residual_Mt_glbl_FodderHerbResidue_Y <- aggregate( L108.ag_Residual_Mt_R_FodderHerbResidue_Y[ X_AGLU_historical_years ],
      by=as.list( L108.ag_Residual_Mt_R_FodderHerbResidue_Y[ Fd ] ), sum )
L108.ag_PosResidual_share_R_FodderHerbResidue_Y <- sweep( L108.ag_PosResidual_Mt_R_FodderHerbResidue_Y[ X_AGLU_historical_years ], 2,
      colSums( L108.ag_PosResidual_Mt_R_FodderHerbResidue_Y[ X_AGLU_historical_years ] ), "/" )
L108.ag_NegResidual_share_R_FodderHerbResidue_Y <- sweep( L108.ag_NegResidual_Mt_R_FodderHerbResidue_Y[ X_AGLU_historical_years ], 2,
      colSums( L108.ag_NegResidual_Mt_R_FodderHerbResidue_Y[ X_AGLU_historical_years ] ), "/" )

#Calculate the global net residual, splitting positive and negative into separate data frames
L108.ag_PosResidual_Mt_glbl_FodderHerbResidue_Y <- L108.ag_Residual_Mt_glbl_FodderHerbResidue_Y
L108.ag_PosResidual_Mt_glbl_FodderHerbResidue_Y[ X_AGLU_historical_years ][ L108.ag_PosResidual_Mt_glbl_FodderHerbResidue_Y[ X_AGLU_historical_years ] < 0 ] <- 0
L108.ag_NegResidual_Mt_glbl_FodderHerbResidue_Y <- L108.ag_Residual_Mt_glbl_FodderHerbResidue_Y
L108.ag_NegResidual_Mt_glbl_FodderHerbResidue_Y[ X_AGLU_historical_years ][ L108.ag_NegResidual_Mt_glbl_FodderHerbResidue_Y[ X_AGLU_historical_years ] > 0 ] <- 0
L108.ag_NegResidual_Mt_glbl_FodderHerbResidue_Y[ X_AGLU_historical_years ] <- L108.ag_NegResidual_Mt_glbl_FodderHerbResidue_Y[ X_AGLU_historical_years ] * -1

printlog( "NOTE: When global FodderHerb production exceeds FodderHerb_Residue demand, the excess supply is mapped to other net uses" )
printlog( "NOTE: Global non-food uses are apportioned to regions according to relative shares of excess supply" )
printlog( "NOTE: When global FodderHerb production is less than FodderHerb_Residue demand, the excess demand is supplied by Residue" )
printlog( "NOTE: Global Residue production is apportioned to regions according to relative shares of excess demand" )

#Multiply the global net total by the sum of individual regions' contributions to each flow
#Positive residuals go to other net uses
L108.ag_OtherUses_Mt_R_FodderHerb_Y <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID$GCAM_region_ID ) ),
      GCAM_commodity = "FodderHerb",
      L108.ag_PosResidual_share_R_FodderHerbResidue_Y[ X_AGLU_historical_years ] *
      L108.ag_PosResidual_Mt_glbl_FodderHerbResidue_Y[ rep( 1, times = length( unique( iso_GCAM_regID$GCAM_region_ID ) ) ), X_AGLU_historical_years ] )

#Negative residuals go to residue
L108.ag_Feed_Mt_R_Residue_Y <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID$GCAM_region_ID ) ),
      GCAM_commodity = "Residue",
      L108.ag_NegResidual_share_R_FodderHerbResidue_Y[ X_AGLU_historical_years ] *
      L108.ag_NegResidual_Mt_glbl_FodderHerbResidue_Y[ rep( 1, times = length( unique( iso_GCAM_regID$GCAM_region_ID ) ) ), X_AGLU_historical_years ] )

#Feed from FodderHerb = FodderHerb_Residue minus Residue
L108.ag_Feed_Mt_R_FodderHerb_Y <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID$GCAM_region_ID ) ),
      GCAM_commodity = "FodderHerb",
      L108.ag_Feed_Mt_R_FodderHerbResidue_Y[ X_AGLU_historical_years ] - L108.ag_Feed_Mt_R_Residue_Y[ X_AGLU_historical_years ] )
      
#PASTURE & FODDERGRASS
printlog( "Part 3: Calculating Pasture and FodderGrass inputs by region and year" )
#Calculate regional FodderGrass production
L108.ag_Prod_Mt_R_FodderGrass_Y <- subset( L104.ag_Prod_Mt_R_C_Y, GCAM_commodity=="FodderGrass" )

#Calculate regional demands of grass (Pasture_FodderGrass)
L108.ag_Feed_Mt_R_PastFodderGrass_Y <- subset( L108.ag_Feed_Mt_R_F_Y, feed == "Pasture_FodderGrass" )

#Pasture demand is equal to Pasture_FodderGrass demand minus FodderGrass production within each region
L108.ag_Feed_Mt_R_Past_Y <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID$GCAM_region_ID ) ),
      GCAM_commodity = "Pasture",
      L108.ag_Feed_Mt_R_PastFodderGrass_Y[ X_AGLU_historical_years ] - L108.ag_Prod_Mt_R_FodderGrass_Y[ X_AGLU_historical_years ] )

#Where pasture demands are negative, set pasture to zero and treat this quantity of foddergrass demand as an other use
L108.ag_OtherUses_Mt_R_FodderGrass_Y <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID$GCAM_region_ID ) ),
      GCAM_commodity = "FodderGrass",
      L108.ag_Feed_Mt_R_Past_Y[ X_AGLU_historical_years ] * -1 )
L108.ag_OtherUses_Mt_R_FodderGrass_Y[ X_AGLU_historical_years ][ L108.ag_OtherUses_Mt_R_FodderGrass_Y[ X_AGLU_historical_years ] < 0 ] <- 0      
L108.ag_Feed_Mt_R_Past_Y[ X_AGLU_historical_years ][L108.ag_Feed_Mt_R_Past_Y[ X_AGLU_historical_years ] < 0 ] <- 0

#FodderGrass used as feed = FodderGrass production - other uses
L108.ag_Feed_Mt_R_FodderGrass_Y <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID$GCAM_region_ID ) ),
      GCAM_commodity = "FodderGrass",
      L108.ag_Prod_Mt_R_FodderGrass_Y[ X_AGLU_historical_years ] - L108.ag_OtherUses_Mt_R_FodderGrass_Y[ X_AGLU_historical_years ] )

#SCAVENGING & OTHER
printlog( "Part 4: Scavenging and other inputs" )
#regional demands of scavenging_other determine the supplies; no calculations are needed here
L108.ag_Feed_Mt_R_ScvgOthr_Y <- subset( L108.ag_Feed_Mt_R_F_Y, feed=="Scavenging_Other" )
names( L108.ag_Feed_Mt_R_ScvgOthr_Y )[ names( L108.ag_Feed_Mt_R_ScvgOthr_Y )==Fd ] <- C

#Merge all feed sources into a single table
printlog( "Combining all feed tables into a single table for export" )
L108.ag_Feed_Mt_R_C_Y <- rbind( L108.ag_Feed_Mt_R_Cnf_Y_adj, L108.ag_Feed_Mt_R_FodderHerb_Y, L108.ag_Feed_Mt_R_Residue_Y,
      L108.ag_Feed_Mt_R_FodderGrass_Y, L108.ag_Feed_Mt_R_Past_Y, L108.ag_Feed_Mt_R_ScvgOthr_Y )

#Write out the net exports of FodderHerb
L108.ag_NetExp_Mt_R_FodderHerb_Y <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID$GCAM_region_ID ) ),
      GCAM_commodity = "FodderHerb",
      L108.ag_Prod_Mt_R_FodderHerb_Y[ X_AGLU_historical_years ] - L108.ag_Feed_Mt_R_FodderHerb_Y[ X_AGLU_historical_years ] -
         L108.ag_OtherUses_Mt_R_FodderHerb_Y[ X_AGLU_historical_years ] )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L108.ag_Feed_Mt_R_C_Y <- c( "Feed use by GCAM region / commodity / year","Unit = Mt" )
comments.L108.ag_NetExp_Mt_R_FodderHerb_Y <- c( "Net exports of FodderHerb by GCAM region / year","Unit = Mt" )

#write tables as CSV files
writedata( L108.ag_Feed_Mt_R_C_Y, domain="AGLU_LEVEL1_DATA",fn="L108.ag_Feed_Mt_R_C_Y", comments=comments.L108.ag_Feed_Mt_R_C_Y )
writedata( L108.ag_NetExp_Mt_R_FodderHerb_Y, domain="AGLU_LEVEL1_DATA",fn="L108.ag_NetExp_Mt_R_FodderHerb_Y", comments=comments.L108.ag_NetExp_Mt_R_FodderHerb_Y )

# Every script should finish with this line
logstop()
