# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu data system. Please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "LA100.0_LDS_preprocessing.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Re-setting column names of LDS output data files for consistency with GCAM data processing system" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )

LDSfilepath <- readdomainpathmap()["AGLU_LDS_DATA"][[1]]
LDSfiles.list <- list.files( LDSfilepath )
LDSfiles.list <- sub( ".csv", "", LDSfiles.list )
LDSfiles <- list()
for( i in LDSfiles.list ){
  index <- which( LDSfiles.list == i )
  LDSfiles[[ index ]] <- readdata( "AGLU_LDS_DATA", i)
}
names( LDSfiles ) <- LDSfiles.list

# -----------------------------------------------------------------------------
# Change names
for( i in 1:length( LDSfiles ) ){
  names( LDSfiles[[i]] ) <- sub( "ctry_iso", "iso", names( LDSfiles[[i]] ) )
  names( LDSfiles[[i]] ) <- sub( "reglr_iso", "GTAP_region", names( LDSfiles[[i]] ) )
  names( LDSfiles[[i]] ) <- sub( "glu_code", GLU, names( LDSfiles[[i]] ) )
  names( LDSfiles[[i]] ) <- sub( "land_type", "land_code", names( LDSfiles[[i]] ) )
  names( LDSfiles[[i]] ) <- sub( "SAGE_crop", "GTAP_crop", names( LDSfiles[[i]] ) )
  names( LDSfiles[[i]] ) <- sub( "mirca_crop", "MIRCA_crop", names( LDSfiles[[i]] ) )
  names( LDSfiles[[i]] ) <- sub( "use_sector", "GTAP_use", names( LDSfiles[[i]] ) )

  #Replace numerical GLU code with concatenated GLU and the 3-digit code  
  if( GLU %in% names( LDSfiles[[i]] ) ){
    LDSfiles[[i]][[GLU]] <- paste( GLU, sprintf( "%03d", LDSfiles[[i]][[GLU]] ), sep = GLU_name_delimiter )
  }
  
  #Need to re-set Taiwan to mainland China, as the current version of AgLU (and pre-2015 versions of FAOSTAT) doesn't disaggregate Taiwan
  if( "iso" %in% names( LDSfiles[[i]] ) ){
    if( names( LDSfiles )[i] != "Pot_veg_carbon_Mg_per_ha" ){
      LDSfiles[[i]][["iso"]][ LDSfiles[[i]][["iso"]] == "twn" ] <- "chn"
      agg.vectors <- names( LDSfiles[[i]] )[ names( LDSfiles[[i]] ) != "value" ]
      LDSfiles[[i]] <- aggregate( LDSfiles[[i]][ "value" ],
                                  by = LDSfiles[[i]][ agg.vectors ], sum )
    }
    #Drop Taiwan from the carbon contents
    if( names( LDSfiles )[i] == "Pot_veg_carbon_Mg_per_ha" ){
      LDSfiles[[i]] <- subset( LDSfiles[[i]], iso != "twn" )
    } 
  }
}

#Next, the production and harvested area tables have values <1 clipped, resulting in some country/glu/crops present in one but not the other
# For now these will simply be dropped; in the future, we may want to add a digit of rounding in the lds
LDSfiles[["LDS_ag_HA_ha"]] <- LDSfiles[["LDS_ag_HA_ha"]][
      vecpaste( LDSfiles[["LDS_ag_HA_ha"]][ c( "iso", GLU, "GTAP_crop" ) ] ) %in%
      vecpaste( LDSfiles[["LDS_ag_prod_t"]][ c( "iso", GLU, "GTAP_crop" ) ] ), ]
LDSfiles[["LDS_ag_prod_t"]] <- LDSfiles[["LDS_ag_prod_t"]][
      vecpaste( LDSfiles[["LDS_ag_prod_t"]][ c( "iso", GLU, "GTAP_crop" ) ] ) %in%
      vecpaste( LDSfiles[["LDS_ag_HA_ha"]][ c( "iso", GLU, "GTAP_crop" ) ] ), ]

# -----------------------------------------------------------------------------
# 3. Output
#write tables as CSV files
for( i in 1:length( LDSfiles ) ){
  objectname <- paste0( "L100.", names( LDSfiles )[i] )
  object <- LDSfiles[[i]]
  assign( objectname, c )
  writedata( object,domain="AGLU_LEVEL1_DATA",fn=objectname )
}

# Every script should finish with this line
logstop()
