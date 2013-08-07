if( !exists( "SOCIOPROC_DIR" ) ){
    if( Sys.getenv( "SOCIOPROC" ) != "" ){
        SOCIOPROC_DIR <- Sys.getenv( "SOCIOPROC" )
    } else {
        stop("Could not determine location of socioeconomics processing scripts, please set the R var SOCIOPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
logstart( "L100.OECD_GDP_hist.R" )
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
printlog( "Historical GDP downscaled to modern country" )

# --------------------------------------------------	---------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
socioeconomics_ctry <- readdata( "SOCIO_MAPPINGS", "socioeconomics_ctry" )
GDPhist <- readdata( "SOCIO_LEVEL0_DATA", "GDPhist" )
UN_popTot <- readdata( "SOCIO_LEVEL0_DATA", "UN_popTot" )

# -----------------------------------------------------------------------------
# 2. Perform computations
L100.pop_thous_ctry_Yh <- subset( UN_popTot, Year %in% historical_years & Scenario == "EST" )
L100.pop_thous_ctry_Yh$iso <- tolower( L100.pop_thous_ctry_Yh$Country )

#Reset East Asia non-specified ("xea") to Taiwan ("twn")
L100.pop_thous_ctry_Yh$iso[ L100.pop_thous_ctry_Yh$iso == "xea" ] <- "twn"

#Aggregate population by the GDP database regions
L100.pop_thous_ctry_Yh$GDP_reg <- socioeconomics_ctry$GDP_reg[ match( L100.pop_thous_ctry_Yh$iso, socioeconomics_ctry$iso ) ]
L100.pop_thous_ctry_Yh$Xyear <- paste0( "X", L100.pop_thous_ctry_Yh$Year )
L100.pop_thous_Rgdp_Yh <- aggregate( L100.pop_thous_ctry_Yh[ "Value" ], by=as.list( L100.pop_thous_ctry_Yh[ c( "GDP_reg", "Xyear" ) ] ), sum )
names( L100.pop_thous_Rgdp_Yh )[ names( L100.pop_thous_Rgdp_Yh ) == "Value" ] <- "pop_thous"

#Melt the GDP table and match GDP by region into the population table
L100.gdp_milusd_Rgdp_Yh.melt <- melt( GDPhist, id.vars = c( "Country", "iso" ), variable_name = "Xyear" )
L100.pop_thous_Rgdp_Yh$gdp_mil <- L100.gdp_milusd_Rgdp_Yh.melt$value[
      match( vecpaste( L100.pop_thous_Rgdp_Yh[ c( "GDP_reg", "Xyear" ) ] ),
             vecpaste( L100.gdp_milusd_Rgdp_Yh.melt[ c( "iso", "Xyear" ) ] ) ) ]

#Calculate the per-capita GDP by year and OECD region
L100.pop_thous_Rgdp_Yh$pcgdp_thous <- L100.pop_thous_Rgdp_Yh$gdp_mil / L100.pop_thous_Rgdp_Yh$pop_thous

#Match in the per-capita GDP to each country, and multiply by population to calculate total GDP
L100.pop_thous_ctry_Yh$pcgdp_thous <- L100.pop_thous_Rgdp_Yh$pcgdp_thous[
      match( vecpaste( L100.pop_thous_ctry_Yh[ c( "GDP_reg", "Xyear" ) ] ),
             vecpaste( L100.pop_thous_Rgdp_Yh[ c( "GDP_reg", "Xyear" ) ] ) ) ]
L100.pop_thous_ctry_Yh$gdp_milusd <- L100.pop_thous_ctry_Yh$Value * L100.pop_thous_ctry_Yh$pcgdp_thous

#Cast so that years are columns
L100.gdp_mil90usd_ctry_Yh <- cast( L100.pop_thous_ctry_Yh, iso ~ Xyear, value = "gdp_milusd" )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L100.gdp_mil90usd_ctry_Yh <- c( "Historical GDP downscaled to country (iso)","Unit = million 1990 US dollars" )

writedata( L100.gdp_mil90usd_ctry_Yh, domain="SOCIO_LEVEL1_DATA", fn="L100.gdp_mil90usd_ctry_Yh", comments=comments.L100.gdp_mil90usd_ctry_Yh )

# Every script should finish with this line
logstop()
