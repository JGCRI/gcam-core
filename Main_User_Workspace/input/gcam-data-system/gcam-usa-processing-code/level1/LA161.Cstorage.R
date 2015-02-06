# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "GCAMUSAPROC_DIR" ) ){
    if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
        GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "LA161.Cstorage.R" )
printlog( "CO2 storage capacities and costs by state" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
Dooley_CCS_USA <- readdata( "GCAMUSA_LEVEL0_DATA", "Dooley_CCS_USA" )

# -----------------------------------------------------------------------------
# 2. Perform computations
L161.Csupply_state <- Dooley_CCS_USA
L161.Csupply_state$grid_region <- states_subregions$grid_region[ match( L161.Csupply_state$state, states_subregions$state ) ]
L161.Csupply_state$Cost_1990USDtC <- L161.Csupply_state$Cost_2005USDtCO2 / conv_1990_2005_USD / conv_CO2_C
L161.Csupply_state$MtC <- L161.Csupply_state$CO2_Mt * conv_CO2_C

printlog( "NOTE: there may be grid regions (e.g. AK, HI) that do not have CO2 storage points. These are not assigned onshore CO2 storage curves" )
C_grid_regions <- sort( unique( L161.Csupply_state$grid_region ) ) 

printlog( "The construction of supply curves takes place within each grid region. Doing this in a for loop" )
printlog( "In each region, there are four cost points assigned, for each quartile of the supply curve")
region_data <- list()
region_quartiles <- list()
for( i in 1:length( C_grid_regions ) ){
	region_data[[i]] <- subset( L161.Csupply_state, grid_region == C_grid_regions[i] )
	region_data[[i]][["Cumul_MtC"]] <- region_data[[i]][["MtC"]][1]
	for( j in 2:nrow( region_data[[i]] ) ){
		region_data[[i]][["Cumul_MtC"]][j] <- region_data[[i]][["Cumul_MtC"]][j-1] + region_data[[i]][["MtC"]][j]
	}
	#only take the 2nd to 5th points in the quartiles (25%, 50%, 75%, 100%)
	# use "type=1" to indicate that we want to return only exact matches to the data in the table.
	region_quartiles[[i]] <- subset( region_data[[i]], Cumul_MtC %in% quantile( Cumul_MtC, type = 1 )[ 2:5] )
	region_quartiles[[i]][["grade"]] <- paste( "grade", 1:4, sep = " " )
	region_quartiles[[i]]["MtC"] <- region_quartiles[[i]][["Cumul_MtC"]][1]
	#from the cumulative totals, return to the marginal quantities associated with each grade
	for( k in 2:4 ){
		region_quartiles[[i]][["MtC"]][k] <- region_quartiles[[i]][["Cumul_MtC"]][k] - region_quartiles[[i]][["Cumul_MtC"]][k-1]
	}
}

#Return from the list to a data frame with all necessary data
L161.Cstorage_FERC <- do.call( rbind, region_quartiles )[ c( "grid_region", "grade", "Cost_1990USDtC", "MtC" ) ]

printlog( "Setting a minimum cost of 0 on CO2 storage and transport projects" )
L161.Cstorage_FERC$Cost_1990USDtC <- pmax( L161.Cstorage_FERC$Cost_1990USDtC, 0 )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L161.Cstorage_FERC <- c( "CO2 storage curves by FERC regions and grades","MtC and 1990 USD per tC" )

#write tables as CSV files
writedata( L161.Cstorage_FERC, domain="GCAMUSA_LEVEL1_DATA", fn="L161.Cstorage_FERC", comments=comments.L161.Cstorage_FERC )

# Every script should finish with this line
logstop()
