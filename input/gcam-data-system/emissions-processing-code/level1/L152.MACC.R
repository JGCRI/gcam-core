# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "EMISSPROC_DIR" ) ){
    if( Sys.getenv( "EMISSIONSPROC" ) != "" ){
        EMISSPROC_DIR <- Sys.getenv( "EMISSIONSPROC" )
    } else {
        stop("Could not determine location of emissions data system. Please set the R var EMISSPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
logstart( "L152.MACC.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Marginal abatement cost curves" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
EPA_MACC_baselines_MtCO2e <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_MACC_baselines_MtCO2e" )
if( EPA_MACC_year == 2020 ) EPA_MACC_MtCO2e <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_MACC_2020_MtCO2e" )
if( EPA_MACC_year == 2030 ) EPA_MACC_MtCO2e <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_MACC_2030_MtCO2e" )
if( !EPA_MACC_year %in% c( 2020, 2030 ) ) stop( "MAC curve year needs to be either 2020 or 2030" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Replacing all & with and" )
EPA_MACC_baselines_MtCO2e$Process <- sub( "\\&", "and", EPA_MACC_baselines_MtCO2e$Process )
EPA_MACC_MtCO2e$Process <- sub( "\\&", "and", EPA_MACC_MtCO2e$Process )
#Make the region names consistent too
EPA_MACC_baselines_MtCO2e$EPA_region <- sub( "\\&", "and", EPA_MACC_baselines_MtCO2e$EPA_region )
EPA_MACC_baselines_MtCO2e$EPA_region[ grepl( "World", EPA_MACC_baselines_MtCO2e$EPA_region ) ] <- "Global"

printlog( "Converting from 2010$/tCO2e to 1990$/tC" )
L152.EPA_MACC_MtCO2e <- melt( EPA_MACC_MtCO2e, variable.name = "cost_2010USD_tCO2e", value.name = "reduction_MtCO2e" )
L152.EPA_MACC_MtCO2e$cost_2010USD_tCO2e <- sub( "\\.", "\\-", L152.EPA_MACC_MtCO2e$cost_2010USD_tCO2e )
L152.EPA_MACC_MtCO2e$cost_2010USD_tCO2e <- as.numeric( sub( "X", "", L152.EPA_MACC_MtCO2e$cost_2010USD_tCO2e ) )
L152.EPA_MACC_MtCO2e$cost_1990USD_tCe <- round( L152.EPA_MACC_MtCO2e$cost_2010USD_tCO2e * conv_C_CO2 / conv_1990_2010_USD, 0 )

printlog( "Merging aluminum and magnesium baseline and abatement")
EPA_MACC_baselines_MtCO2e$Process[ grepl( "Aluminum", EPA_MACC_baselines_MtCO2e$Process ) ] <- "Aluminum and Magnesium Production"
EPA_MACC_baselines_MtCO2e$Process[ grepl( "Magnesium", EPA_MACC_baselines_MtCO2e$Process ) ] <- "Aluminum and Magnesium Production"
EPA_MACC_baselines_MtCO2e <- aggregate( EPA_MACC_baselines_MtCO2e[ X_EPA_MACC_year ],
      by=as.list( EPA_MACC_baselines_MtCO2e[ EPA_MACC_names ] ), sum )

L152.EPA_MACC_MtCO2e$Process[ grepl( "Aluminum", L152.EPA_MACC_MtCO2e$Process ) ] <- "Aluminum and Magnesium Production"
L152.EPA_MACC_MtCO2e$Process[ grepl( "Magnesium", L152.EPA_MACC_MtCO2e$Process ) ] <- "Aluminum and Magnesium Production"
L152.EPA_MACC_MtCO2e <- aggregate( L152.EPA_MACC_MtCO2e[ "reduction_MtCO2e" ],
      by=as.list( L152.EPA_MACC_MtCO2e[ c( EPA_MACC_names, "cost_1990USD_tCe" ) ] ), sum )

printlog( "Matching in the baseline emissions quantities to calculate abatement percentages" )
L152.EPA_MACC_MtCO2e$baseline_MtCO2e <- EPA_MACC_baselines_MtCO2e[[ X_EPA_MACC_year ]][
      match( vecpaste( L152.EPA_MACC_MtCO2e[ EPA_MACC_names ] ),
             vecpaste( EPA_MACC_baselines_MtCO2e[ EPA_MACC_names ] ) ) ]
L152.EPA_MACC_MtCO2e$reduction_pct <- L152.EPA_MACC_MtCO2e$reduction_MtCO2e / L152.EPA_MACC_MtCO2e$baseline_MtCO2e

printlog( "NOTE: dropping MAC curves in regions where the sector/process does not exist (i.e., the baseline is zero)" )
L152.EPA_MACC_MtCO2e <- subset( L152.EPA_MACC_MtCO2e, !is.na( L152.EPA_MACC_MtCO2e$reduction_pct ) )

printlog( "Writing out only the specified tax levels" )
L152.MAC_pct_R_S_Proc_EPA.melt <- repeat_and_add_vector(
      unique( L152.EPA_MACC_MtCO2e[ EPA_MACC_names ] ),
      "tax_1990USD_tCe", MAC_taxes )
L152.MAC_pct_R_S_Proc_EPA.melt <- L152.MAC_pct_R_S_Proc_EPA.melt[
  order( L152.MAC_pct_R_S_Proc_EPA.melt[[ "Process" ]], L152.MAC_pct_R_S_Proc_EPA.melt[[ "EPA_region" ]] ), ]

# Match in the reduction for the given tax levels
L152.MAC_pct_R_S_Proc_EPA.melt$reduction <- L152.EPA_MACC_MtCO2e$reduction_pct[
      match( vecpaste( L152.MAC_pct_R_S_Proc_EPA.melt[ c( EPA_MACC_names, "tax_1990USD_tCe" ) ] ),
             vecpaste( L152.EPA_MACC_MtCO2e[ c( EPA_MACC_names, "cost_1990USD_tCe" ) ] ) ) ]
if( any( is.na( L152.MAC_pct_R_S_Proc_EPA.melt$reduction ) ) ) stop( "Need to set the tax levels equal to values available in the converted EPA values" )
L152.MAC_pct_R_S_Proc_EPA <- dcast( L152.MAC_pct_R_S_Proc_EPA.melt, Sector + Process + EPA_region ~ tax_1990USD_tCe, value.var = "reduction" )

# The for loop below will interpolate if the chosen MAC tax levels are not among the list of converted factors. This takes ~10 minutes to run and adds zero value.
# Better to choose tax levels that are among the EPA's 176 provided values, even if the post-conversion numbers look strangely precise
# for( i in 1:nrow( L152.MAC_pct_R_S_Proc_EPA.melt ) ){
#	L152.MAC_pct_R_S_Proc_EPA.melt$reduction[i] <- approx(
#      x = L152.EPA_MACC_MtCO2e$cost_1990USD_tCe[ vecpaste( L152.EPA_MACC_MtCO2e[ EPA_MACC_names ] ) == vecpaste( L152.MAC_pct_R_S_Proc_EPA.melt[ i, EPA_MACC_names ] ) ],
#      y = L152.EPA_MACC_MtCO2e$reduction_pct[ vecpaste( L152.EPA_MACC_MtCO2e[ EPA_MACC_names ] ) == vecpaste( L152.MAC_pct_R_S_Proc_EPA.melt[ i, EPA_MACC_names ] ) ],
#      xout = L152.MAC_pct_R_S_Proc_EPA.melt$tax_1990USD_tCe[i], rule = 2 )$y
#}

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L152.MAC_pct_R_S_Proc_EPA <- c( "Marginal abatement cost curves by EPA region / EPA sector / process", "Unit = %" )

#write tables as CSV files
writedata( L152.MAC_pct_R_S_Proc_EPA, domain="EMISSIONS_LEVEL1_DATA", fn="L152.MAC_pct_R_S_Proc_EPA", comments=comments.L152.MAC_pct_R_S_Proc_EPA )

# Every script should finish with this line
logstop()
