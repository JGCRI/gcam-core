# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "WATERPROC_DIR" ) ){
    if( Sys.getenv( "WATERPROC" ) != "" ){
        WATERPROC_DIR <- Sys.getenv( "WATERPROC" )
    } else {
        stop("Could not determine location of water data system. Please set the R var WATERPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(WATERPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(WATERPROC_DIR,"/../_common/headers/WATER_header.R",sep=""))
logstart( "L132.water.demand.manufacturing.R" )
printlog( "Calculate water demand coefficients for manufacturing by water type." )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
L1322.in_EJ_R_indenergy_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1322.in_EJ_R_indenergy_F_Yh" )
L1322.in_EJ_R_indfeed_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1322.in_EJ_R_indfeed_F_Yh" )
manufacturing_water_mapping <- readdata( "WATER_MAPPINGS", "manufacturing_water_mapping" )
manufacturing_water_data <- readdata( "WATER_LEVEL0_DATA", "manufacturing_water_data" )
manufacturing_water_ratios <- readdata( "WATER_LEVEL0_DATA", "manufacturing_water_ratios" )

# -----------------------------------------------------------------------------

# 2. Process data
printlog( "Aggregate total industrial energy use to the manufacturing water continent regions" )
L132.manufacture_content_energy <- rbind( L1322.in_EJ_R_indenergy_F_Yh, L1322.in_EJ_R_indfeed_F_Yh )
L132.manufacture_content_energy <- merge( L132.manufacture_content_energy, GCAM_region_names )
L132.manufacture_content_energy <- merge( L132.manufacture_content_energy, manufacturing_water_mapping )
L132.manufacture_content_energy <- aggregate( X1995 ~ continent, L132.manufacture_content_energy, FUN=sum )

printlog( "Merge manufacturing water demands by continent regions" )
L132.manufacture_content_energy <- merge( L132.manufacture_content_energy, manufacturing_water_data )
# Note withdrawals is in million m^3 and energy is in EJ.  We want the coefficient in km^3/EJ or m^3/GJ so we
# divide by 1e3
L132.manufacture_content_energy[[water_W]] <- L132.manufacture_content_energy$withdrawals * manufacturing_water_ratios$self.to.total.ratio / L132.manufacture_content_energy$X1995 / 1e3
L132.manufacture_content_energy[[water_C]] <- L132.manufacture_content_energy[[water_W]] * manufacturing_water_ratios$cons.to.with.ratio
L132.manufacture_content_energy <- melt( L132.manufacture_content_energy, id.vars="continent", measure.vars=c( water_W, water_C ),
    variable.name=water_type, value.name="coefficient" )

print( "Map coefficients back to GCAM regions" )
L132.water_coef_manufacturing_R_W_m3_GJ <- merge( manufacturing_water_mapping, L132.manufacture_content_energy )
L132.water_coef_manufacturing_R_W_m3_GJ <- merge( L132.water_coef_manufacturing_R_W_m3_GJ, GCAM_region_names )
L132.water_coef_manufacturing_R_W_m3_GJ <- L132.water_coef_manufacturing_R_W_m3_GJ[, c( R, water_type, "coefficient" ) ]

# 3. Output

#Add comments for each table
comments.L132.water_coef_manufacturing_R_W_m3_GJ <- c( "Manufacturing energy water coefficients by region ID / water type","Unit = m^3 / GJ" )

#write tables as CSV files
writedata( L132.water_coef_manufacturing_R_W_m3_GJ, domain="WATER_LEVEL1_DATA", fn="L132.water_coef_manufacturing_R_W_m3_GJ", comments=comments.L132.water_coef_manufacturing_R_W_m3_GJ )

# Every script should finish with this line
logstop()
