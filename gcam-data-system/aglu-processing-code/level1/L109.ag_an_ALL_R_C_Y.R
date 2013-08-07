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
logstart( "L109.ag_an_ALL_R_C_Y.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Primary agricultural good and animal product mass balances, by region / commodity / year" )

# -----------------------------------------------------------------------------
# 1. Read data from previous files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L101.ag_Food_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L101.ag_Food_Mt_R_C_Y" )
L104.ag_Prod_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L104.ag_Prod_Mt_R_C_Y" )
L105.an_Food_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L105.an_Food_Mt_R_C_Y" )
L105.an_Prod_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L105.an_Prod_Mt_R_C_Y" )
L106.ag_NetExp_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L106.ag_NetExp_Mt_R_C_Y" )
L106.an_NetExp_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L106.an_NetExp_Mt_R_C_Y" )
L108.ag_Feed_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L108.ag_Feed_Mt_R_C_Y" )
L108.ag_NetExp_Mt_R_FodderHerb_Y <- readdata( "AGLU_LEVEL1_DATA", "L108.ag_NetExp_Mt_R_FodderHerb_Y" )
L122.in_Mt_R_C_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L122.in_Mt_R_C_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Part 1: Primary agricultural goods" )
#Aggregate AEZs in agricultural production
L109.ag_NetExp_Mt_R_C_Y <- rbind( L106.ag_NetExp_Mt_R_C_Y, L108.ag_NetExp_Mt_R_FodderHerb_Y )

#Name the flows in each table
ag_Flow_cols <- c( "Prod_Mt", "NetExp_Mt", "Supply_Mt", "Food_Mt", "Feed_Mt", "Biofuels_Mt", "OtherUses_Mt" )
L104.ag_Prod_Mt_R_C_Y$flow <- "Prod_Mt"
L109.ag_NetExp_Mt_R_C_Y$flow <- "NetExp_Mt"
L101.ag_Food_Mt_R_C_Y$flow <- "Food_Mt"
L108.ag_Feed_Mt_R_C_Y$flow <- "Feed_Mt"
L122.in_Mt_R_C_Yh$flow <- "Biofuels_Mt"

#Rbind and recast so that the flows are the columns
L109.ag_ALL_Mt_R_C_Y_prelim <- rbind( L104.ag_Prod_Mt_R_C_Y, L109.ag_NetExp_Mt_R_C_Y, L101.ag_Food_Mt_R_C_Y, L108.ag_Feed_Mt_R_C_Y, L122.in_Mt_R_C_Yh )
L109.ag_ALL_Mt_R_C_Y <- recast( L109.ag_ALL_Mt_R_C_Y_prelim, GCAM_region_ID + GCAM_commodity + variable ~ flow, id.var = c( R_C, "flow" ) )

#set non-existent combinations to 0
L109.ag_ALL_Mt_R_C_Y[ is.na( L109.ag_ALL_Mt_R_C_Y ) ] <- 0
names( L109.ag_ALL_Mt_R_C_Y )[ names( L109.ag_ALL_Mt_R_C_Y ) == "variable" ] <- Y
L109.ag_ALL_Mt_R_C_Y$year <- as.numeric( gsub2( "X", "", L109.ag_ALL_Mt_R_C_Y$year ) )

#For any commodities (e.g. pasture, residue, scavenging) in feed but not in production tables, production = feed
Feed_commodities <- unique( L108.ag_Feed_Mt_R_C_Y$GCAM_commodity[ !L108.ag_Feed_Mt_R_C_Y$GCAM_commodity %in% L104.ag_Prod_Mt_R_C_Y$GCAM_commodity ] )
L109.ag_ALL_Mt_R_C_Y$Prod_Mt[ L109.ag_ALL_Mt_R_C_Y$GCAM_commodity %in% Feed_commodities ] <-
      L109.ag_ALL_Mt_R_C_Y$Feed_Mt[ L109.ag_ALL_Mt_R_C_Y$GCAM_commodity %in% Feed_commodities ]

#Calculate the domestic supply and other uses, and re-order the columns
L109.ag_ALL_Mt_R_C_Y$Supply_Mt <- L109.ag_ALL_Mt_R_C_Y$Prod_Mt - L109.ag_ALL_Mt_R_C_Y$NetExp_Mt
L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt <- L109.ag_ALL_Mt_R_C_Y$Supply_Mt - L109.ag_ALL_Mt_R_C_Y$Food_Mt - L109.ag_ALL_Mt_R_C_Y$Feed_Mt - L109.ag_ALL_Mt_R_C_Y$Biofuels_Mt
L109.ag_ALL_Mt_R_C_Y <- L109.ag_ALL_Mt_R_C_Y[ c( R_C_Y, ag_Flow_cols ) ]

if( any( L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt<0 ) ){
	printlog( "Adjusting regional crop mass balances to remove net negative other uses" )
	printlog( "NOTE: Assigning negative other net uses to imports, and adjusting global trade to maintain balances" )
	L109.ag_ALL_Mt_R_C_Y$NegOtherUses_Mt <- ifelse( L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt < 0, L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt, 0 )
	L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt_adj <- ifelse( L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt >= 0, L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt, 0 )
	L109.ag_ALL_Mt_R_C_Y$NetExp_Mt_adj <- L109.ag_ALL_Mt_R_C_Y$NetExp_Mt + L109.ag_ALL_Mt_R_C_Y$NegOtherUses_Mt
	
	printlog( "NOTE: Increases in global net exports are apportioned among regions with positive net exports, according to shares" )
	L109.ag_ALL_Mt_glbl_C_Y <- aggregate( L109.ag_ALL_Mt_R_C_Y[ c( "NegOtherUses_Mt", "OtherUses_Mt_adj", "NetExp_Mt_adj" ) ],
      by=as.list( L109.ag_ALL_Mt_R_C_Y[ C_Y ] ), sum )
	
	#Subset negative and positive "other uses" separately. Positive will be the new adjusted "Other uses"
	L109.ag_ALL_Mt_R_C_Y[ c( "GlobalOtherUses_Mt", "GlobalNetExpAdj" ) ] <- L109.ag_ALL_Mt_glbl_C_Y[
      match( vecpaste( L109.ag_ALL_Mt_R_C_Y[ C_Y ] ), vecpaste( L109.ag_ALL_Mt_glbl_C_Y[ C_Y ] ) ),
      c( "OtherUses_Mt_adj", "NegOtherUses_Mt" ) ]
	L109.ag_ALL_Mt_R_C_Y$NetExpAdjFrac <- ifelse( L109.ag_ALL_Mt_R_C_Y$GlobalOtherUses_Mt == 0, 0, 
	  L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt_adj / L109.ag_ALL_Mt_R_C_Y$GlobalOtherUses_Mt )
	L109.ag_ALL_Mt_R_C_Y$NetExp_Mt_adj2 <- L109.ag_ALL_Mt_R_C_Y$NetExp_Mt_adj - L109.ag_ALL_Mt_R_C_Y$NetExpAdjFrac *
      L109.ag_ALL_Mt_R_C_Y$GlobalNetExpAdj

	printlog( "Rebuilding primary agricultural product mass balance table" )
	L109.ag_ALL_Mt_R_C_Y_adj <- L109.ag_ALL_Mt_R_C_Y[ c( R_C_Y, ag_Flow_cols ) ]
	L109.ag_ALL_Mt_R_C_Y_adj$NetExp_Mt <- L109.ag_ALL_Mt_R_C_Y$NetExp_Mt_adj2
	L109.ag_ALL_Mt_R_C_Y_adj$Supply_Mt <- L109.ag_ALL_Mt_R_C_Y_adj$Prod_Mt - L109.ag_ALL_Mt_R_C_Y_adj$NetExp_Mt
	L109.ag_ALL_Mt_R_C_Y_adj[ ag_Flow_cols ] <- round( L109.ag_ALL_Mt_R_C_Y_adj[ ag_Flow_cols ], digits_calOutput )
	L109.ag_ALL_Mt_R_C_Y_adj$OtherUses_Mt <- L109.ag_ALL_Mt_R_C_Y_adj$Supply_Mt - L109.ag_ALL_Mt_R_C_Y_adj$Food_Mt -
      L109.ag_ALL_Mt_R_C_Y_adj$Biofuels_Mt - L109.ag_ALL_Mt_R_C_Y_adj$Feed_Mt
	L109.ag_ALL_Mt_R_C_Y <- L109.ag_ALL_Mt_R_C_Y_adj
} 

printlog ("Part 2: Animal commodities" )
an_Flow_cols <- c( "Prod_Mt", "NetExp_Mt", "Supply_Mt", "Food_Mt", "OtherUses_Mt" )
L105.an_Prod_Mt_R_C_Y$flow <- "Prod_Mt"
L106.an_NetExp_Mt_R_C_Y$flow <- "NetExp_Mt"
L105.an_Food_Mt_R_C_Y$flow <- "Food_Mt"

#Rbind and recast so that the flows are the columns
L109.an_ALL_Mt_R_C_Y_prelim <- rbind( L105.an_Prod_Mt_R_C_Y, L106.an_NetExp_Mt_R_C_Y, L105.an_Food_Mt_R_C_Y )
L109.an_ALL_Mt_R_C_Y <- recast( L109.an_ALL_Mt_R_C_Y_prelim, GCAM_region_ID + GCAM_commodity + variable ~ flow, id.var = c( R_C, "flow" ) )

names( L109.an_ALL_Mt_R_C_Y )[ names( L109.an_ALL_Mt_R_C_Y ) == "variable" ] <- Y
L109.an_ALL_Mt_R_C_Y$year <- as.numeric( gsub2( "X", "", L109.an_ALL_Mt_R_C_Y$year ) )

#Calculate the domestic supply and other uses, and re-order the columns
L109.an_ALL_Mt_R_C_Y$Supply_Mt <- L109.an_ALL_Mt_R_C_Y$Prod_Mt - L109.an_ALL_Mt_R_C_Y$NetExp_Mt
L109.an_ALL_Mt_R_C_Y$OtherUses_Mt <- L109.an_ALL_Mt_R_C_Y$Supply_Mt - L109.an_ALL_Mt_R_C_Y$Food_Mt
L109.an_ALL_Mt_R_C_Y <- L109.an_ALL_Mt_R_C_Y[ c( R_C_Y, an_Flow_cols ) ]

if( any( L109.an_ALL_Mt_R_C_Y$OtherUses_Mt<0 ) ){
	printlog( "Adjusting regional animal product mass balances to remove net negative other uses" )
	printlog( "NOTE: Assigning negative other net uses to imports, and adjusting global trade to maintain balances" )
	L109.an_ALL_Mt_R_C_Y$NegOtherUses_Mt <- ifelse( L109.an_ALL_Mt_R_C_Y$OtherUses_Mt < 0, L109.an_ALL_Mt_R_C_Y$OtherUses_Mt, 0 )
	L109.an_ALL_Mt_R_C_Y$OtherUses_Mt_adj <- ifelse( L109.an_ALL_Mt_R_C_Y$OtherUses_Mt >= 0, L109.an_ALL_Mt_R_C_Y$OtherUses_Mt, 0 )
	L109.an_ALL_Mt_R_C_Y$NetExp_Mt_adj <- L109.an_ALL_Mt_R_C_Y$NetExp_Mt + L109.an_ALL_Mt_R_C_Y$NegOtherUses_Mt
	
	printlog( "NOTE: Increases in global net exports are apportioned among regions with positive net exports, according to shares" )
	L109.an_ALL_Mt_glbl_C_Y <- aggregate( L109.an_ALL_Mt_R_C_Y[ c( "NegOtherUses_Mt", "OtherUses_Mt_adj", "NetExp_Mt_adj" ) ],
      by=as.list( L109.an_ALL_Mt_R_C_Y[ C_Y ] ), sum )
	
	#Subset negative and positive "other uses" separately. Positive will be the new adjusted "Other uses"
	L109.an_ALL_Mt_R_C_Y[ c( "GlobalOtherUses_Mt", "GlobalNetExpAdj" ) ] <- L109.an_ALL_Mt_glbl_C_Y[
      match( vecpaste( L109.an_ALL_Mt_R_C_Y[ C_Y ] ), vecpaste( L109.an_ALL_Mt_glbl_C_Y[ C_Y ] ) ),
      c( "OtherUses_Mt_adj", "NegOtherUses_Mt" ) ]
	L109.an_ALL_Mt_R_C_Y$NetExpAdjFrac <- ifelse( L109.an_ALL_Mt_R_C_Y$GlobalOtherUses_Mt == 0, 0, 
	  L109.an_ALL_Mt_R_C_Y$OtherUses_Mt_adj / L109.an_ALL_Mt_R_C_Y$GlobalOtherUses_Mt )
	L109.an_ALL_Mt_R_C_Y$NetExp_Mt_adj2 <- L109.an_ALL_Mt_R_C_Y$NetExp_Mt_adj - L109.an_ALL_Mt_R_C_Y$NetExpAdjFrac *
      L109.an_ALL_Mt_R_C_Y$GlobalNetExpAdj

	printlog( "Rebuilding animal product mass balance table" )
	L109.an_ALL_Mt_R_C_Y_adj <- L109.an_ALL_Mt_R_C_Y[ c( R_C_Y, an_Flow_cols ) ]
	L109.an_ALL_Mt_R_C_Y_adj$NetExp_Mt <- L109.an_ALL_Mt_R_C_Y$NetExp_Mt_adj2
	L109.an_ALL_Mt_R_C_Y_adj$Supply_Mt <- L109.an_ALL_Mt_R_C_Y_adj$Prod_Mt - L109.an_ALL_Mt_R_C_Y_adj$NetExp_Mt
	L109.an_ALL_Mt_R_C_Y_adj[ an_Flow_cols ] <- round( L109.an_ALL_Mt_R_C_Y_adj[ an_Flow_cols ], digits_calOutput )
	L109.an_ALL_Mt_R_C_Y_adj$OtherUses_Mt <- L109.an_ALL_Mt_R_C_Y_adj$Supply_Mt - L109.an_ALL_Mt_R_C_Y_adj$Food_Mt
	L109.an_ALL_Mt_R_C_Y <- L109.an_ALL_Mt_R_C_Y_adj
} 

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L109.ag_ALL_Mt_R_C_Y <- c( "Primary agriculture good mass balances by GCAM region / commodity / year","Unit = Mt" )
comments.L109.an_ALL_Mt_R_C_Y <- c( "Animal product mass balances by GCAM region / commodity / year","Unit = Mt" )

#write tables as CSV files
writedata( L109.ag_ALL_Mt_R_C_Y, domain="AGLU_LEVEL1_DATA",fn="L109.ag_ALL_Mt_R_C_Y", comments=comments.L109.ag_ALL_Mt_R_C_Y )
writedata( L109.an_ALL_Mt_R_C_Y, domain="AGLU_LEVEL1_DATA",fn="L109.an_ALL_Mt_R_C_Y", comments=comments.L109.an_ALL_Mt_R_C_Y )

# Every script should finish with this line
logstop()
