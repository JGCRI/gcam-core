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
logstart( "L151.ctrl_R_en_S_T.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Maximum reduction of air pollutants for energy technologies by GCAM technology" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
A51.min_coeff <- readdata( "EMISSIONS_ASSUMPTIONS", "A51.min_coeff" )
L111.nonghg_tgej_R_en_S_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L111.nonghg_tgej_R_en_S_F_Yh" )
L114.bcoc_tgej_R_en_S_F_2000 <- readdata( "EMISSIONS_LEVEL1_DATA", "L114.bcoc_tgej_R_en_S_F_2000" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compute max emissions reduction for SO2, CO, NOx, NMVOC" )
#First, set up min coeff data frame
L151.min_coeff <- melt( A51.min_coeff, id.vars=c( "supplysector", "subsector", "stub.technology" ), variable.name = "Non.CO2" )
L151.min_coeff <- na.omit( L151.min_coeff )

#Then, map in relevant information
L111.nonghg_tgej_R_en_S_F_Yh <- L111.nonghg_tgej_R_en_S_F_Yh[ names( L111.nonghg_tgej_R_en_S_F_Yh ) %!in% c( "X2009", "X2010" )]
L151.nonghg_ctrl_R_en_S_T <- melt( L111.nonghg_tgej_R_en_S_F_Yh,
                                   id.vars=c( "GCAM_region_ID", "supplysector", "subsector", "stub.technology", "Non.CO2" ),
                                   measure.vars = "X2005",   # subsetting 2005 only
                                   variable.name = Y, value.name = "curr_coeff" )
L151.nonghg_ctrl_R_en_S_T$min_coeff <- L151.min_coeff$value[
  match( vecpaste( L151.nonghg_ctrl_R_en_S_T[ c( "supplysector", "subsector", "stub.technology", "Non.CO2" ) ] ),
         vecpaste( L151.min_coeff[ c( "supplysector", "subsector", "stub.technology", "Non.CO2" )] ) ) ]
L151.nonghg_ctrl_R_en_S_T <- na.omit( L151.nonghg_ctrl_R_en_S_T )

#Finally, compute maximum reduction as percentage
L151.nonghg_ctrl_R_en_S_T$max_reduction <- with( L151.nonghg_ctrl_R_en_S_T, 100 * ( curr_coeff - min_coeff ) / curr_coeff )
L151.nonghg_ctrl_R_en_S_T <- na.omit( L151.nonghg_ctrl_R_en_S_T )
L151.nonghg_ctrl_R_en_S_T$max_reduction[ L151.nonghg_ctrl_R_en_S_T$max_reduction > 100 ] <- 100
L151.nonghg_ctrl_R_en_S_T$max_reduction[ L151.nonghg_ctrl_R_en_S_T$max_reduction < 0 ] <- 0

printlog( "Compute max emissions reduction for BC & OC" )
#Then, map in relevant information
L151.bcoc_ctrl_R_en_S_T <- L114.bcoc_tgej_R_en_S_F_2000
L151.bcoc_ctrl_R_en_S_T[[Y]] <- "X2000"
names( L151.bcoc_ctrl_R_en_S_T )[ names( L151.bcoc_ctrl_R_en_S_T ) == "X2000" ] <- "curr_coeff"
L151.bcoc_ctrl_R_en_S_T$min_coeff <- L151.min_coeff$value[
  match( vecpaste(L151.bcoc_ctrl_R_en_S_T[ c( "supplysector", "subsector", "stub.technology", "Non.CO2" ) ] ),
         vecpaste( L151.min_coeff[ c( "supplysector", "subsector", "stub.technology", "Non.CO2" )] ) ) ]
L151.bcoc_ctrl_R_en_S_T <- na.omit( L151.bcoc_ctrl_R_en_S_T )

#Finally, compute maximum reduction as percentage
L151.bcoc_ctrl_R_en_S_T$max_reduction <- with( L151.bcoc_ctrl_R_en_S_T, 100 * ( curr_coeff - min_coeff ) / curr_coeff )
L151.bcoc_ctrl_R_en_S_T <- na.omit( L151.bcoc_ctrl_R_en_S_T )
L151.bcoc_ctrl_R_en_S_T$max_reduction[ L151.bcoc_ctrl_R_en_S_T$max_reduction > 100 ] <- 100
L151.bcoc_ctrl_R_en_S_T$max_reduction[ L151.bcoc_ctrl_R_en_S_T$max_reduction < 0 ] <- 0

printlog( "Combine dataframes and remove unnecessary columns")
L151.nonghg_ctrl_R_en_S_T <- rbind( L151.nonghg_ctrl_R_en_S_T, L151.bcoc_ctrl_R_en_S_T )
L151.nonghg_ctrl_R_en_S_T <- L151.nonghg_ctrl_R_en_S_T[ names( L151.nonghg_ctrl_R_en_S_T ) %!in% c( "curr_coeff", "min_coeff", "year" )]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L151.nonghg_ctrl_R_en_S_T <- c( "Maximum reduction by region / sector / gas", "Unit = %" )

#write tables as CSV files
writedata( L151.nonghg_ctrl_R_en_S_T, domain="EMISSIONS_LEVEL1_DATA", fn="L151.nonghg_ctrl_R_en_S_T", comments=comments.L151.nonghg_ctrl_R_en_S_T )

# Every script should finish with this line
logstop()
