# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "MODELTIMEPROC_DIR" ) ){
    if( Sys.getenv( "MODELTIMEPROC" ) != "" ){
        MODELTIMEPROC_DIR <- Sys.getenv( "MODELTIMEPROC" )
    } else {
        stop("Could not determine location of modeltime data system. Please set the R var MODELTIMEPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(MODELTIMEPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(MODELTIMEPROC_DIR,"/../_common/headers/MODELTIME_header.R",sep=""))
logstart( "L200.modeltime.R" )
adddep(paste(MODELTIMEPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(MODELTIMEPROC_DIR,"/../_common/headers/MODELTIME_header.R",sep=""))
printlog( "GCAM model time" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "L200.ModelTime: Specify start year, starting timestep length, end year, and final calibration year")
#Calculate the read-in timesteps in the model
GCAM_years <- c( model_base_years, model_future_years )
GCAM_timesteps <- GCAM_years[ 2:length( GCAM_years ) ] - GCAM_years[ 1:length( GCAM_years ) - 1 ]

L200.ModelTime <- data.frame(
      start.year.timestep = GCAM_timesteps[ 1 ],
      start.year = min( model_base_years ),
      final.calibration.year = max( model_base_years ),
      end.year = max( model_future_years ) )
      
printlog( "L200.ModelTimeInterYears: Specify variable timesteps")
#build a true/false vector that is true for every period where the timestep length changes. start with all false values.
changeyears <- is.na( GCAM_timesteps )
for ( i in 2:length( GCAM_timesteps) ){
	changeyears[i] <- ifelse( GCAM_timesteps[i] != GCAM_timesteps[i-1], T, F )
}
GCAM_interyears <- GCAM_years[ changeyears ]
GCAM_interyear.timesteps <- GCAM_timesteps[ changeyears ]

L200.ModelTimeInterYears <- data.frame(
      inter.year.timestep = GCAM_interyear.timesteps,
      inter.year = GCAM_interyears )

#Magicc model data
printlog( "L200.MAGICC: Input parameters from GCAM to MAGICC" )
L200.MAGICC <- data.frame( 
      last.historical.year = Magicc_last_historical_year,
      bc.unit.forcing = Magicc_bc_unit_forcing,
      carbon.model.start.year = Magicc_C_start_year )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L200.ModelTime, IDstring="ModelTime", domain="MODELTIME_LEVEL2_DATA", fn="L200.ModelTime", batch_XML_domain="MODELTIME_XML_BATCH", batch_XML_file="batch_modeltime.xml" ) 
if( nrow( L200.ModelTimeInterYears ) != 0 ){
  write_mi_data( L200.ModelTimeInterYears, "ModelTimeInterYears", "MODELTIME_LEVEL2_DATA", "L200.ModelTimeInterYears", "MODELTIME_XML_BATCH", "batch_modeltime.xml" )
}
write_mi_data( L200.MAGICC, "MAGICC", "MODELTIME_LEVEL2_DATA", "L200.MAGICC", "MODELTIME_XML_BATCH", "batch_modeltime.xml" ) 

insert_file_into_batchxml( "MODELTIME_XML_BATCH", "batch_modeltime.xml", "MODELTIME_XML_FINAL", "modeltime.xml", "", xml_tag="outFile" )

logstop()
