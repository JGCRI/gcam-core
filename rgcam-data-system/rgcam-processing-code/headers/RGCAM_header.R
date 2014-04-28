# The GCAM R header file
# Ben Bond-Lamberty August 2011, updated February 2012

# This file should be source'd by any R script involved in the processing of GCAM input data
# It provides various facilities, including logging, file I/O and definition of common settings

# -----------------------------------------------------------------------------
# Load required libraries
libs <- c( "ggplot2", "reshape" )
for( i in libs ) {
	if( !require( i, character.only=T ) ) {
		cat( "Couldn't load", i, "; trying to download it...\n" )
		install.packages( i )
	}
	library( i, character.only=T )
}

# -----------------------------------------------------------------------------
# Global settings (in CAPITALS)
# TODO: check build target. If it's "clean", or something like that, reset everything
# This first group of settings is protected--we don't want it re-set every time
# this header is read.
if( !exists( "GCAM_SOURCE_FN" ) ) {		# i.e. #ifndef
	GCAM_SOURCE_FN 		<- c( "?" ) 	# name of currently executing source file (stack structure)
	GCAM_LOG_SAVE	 	<- c( FALSE )	# whether current log is also being saved to file (stack structure)
	GCAM_SOURCE_RD 		<- 0			# recursion depth, an index into above structures
	DEPENDENCIES 		<- list()		# dependencies (i.e. what files scripts read)
}

GCAM_DATA_COMMENT 		<- "#"								# Comment character for files
XML_TEMPLATE_FILENAME 	<- "batch_rgcam_template.xml"		# XML template file name
GCAM_HEADERS_RGCAM 		<- "headers_rgcam.txt"				# RGCAM header file name
PATH_FROM_MI 			<- "./"
DEPENDENCY_FOLDER 		<- "rgcam-dependencies"				# Dependency data folder
FILEPATHMAP 			<- "rgcam-processing-code/headers/filemapping.csv"	# List of file mappings


# -----------------------------------------------------------------------------
# printlog: time-stamped output
# params: msg (message, can be many items); ts (add timestamp?), cr (print CR?)
printlog <- function( msg, ..., ts=TRUE, cr=TRUE ) {
	if( ts ) cat( date(), GCAM_SOURCE_FN[ GCAM_SOURCE_RD ], "[", GCAM_SOURCE_RD, "]", ": " )
	cat( msg, ... )
	if( cr ) cat( "\n")
}

# -----------------------------------------------------------------------------
# logstart: start a new log (to screen and optionally file)
# params: fn (name of source file being executed), savelog (whether to write to disk)
logstart <- function( fn, savelog=T ) {
	GCAM_SOURCE_RD <<- GCAM_SOURCE_RD + 1		# push
	GCAM_SOURCE_FN[ GCAM_SOURCE_RD ]  <<- fn
	GCAM_LOG_SAVE[ GCAM_SOURCE_RD ] <<- savelog
	if( savelog ) sink( paste( "rgcam-processing-code/logs/", fn, ".log", sep="" ), split=T )
	printlog( "-----" )
	printlog( "Starting", fn )
	
	DEPENDENCIES[[ fn ]] <<- c( NULL ) # Create a new entry in the dependency list
}

# -----------------------------------------------------------------------------
# logstop: stop the current log (to screen and optionally file)
logstop <- function() {

	fn <- GCAM_SOURCE_FN[ GCAM_SOURCE_RD ]
	if( !file.exists( DEPENDENCY_FOLDER ) ) {
		printlog( "Creating", DEPENDENCY_FOLDER )
		dir.create( DEPENDENCY_FOLDER )
	}
	printlog( "Writing dependency information for", fn, "..." )
	cat( DEPENDENCIES[[ fn ]], file=paste( DEPENDENCY_FOLDER, "/", fn, ".txt", sep="" ), sep="\n" )
	
	printlog( "All done with", fn )
	if( GCAM_SOURCE_RD > 0 ) {
		if( GCAM_LOG_SAVE[ GCAM_SOURCE_RD ] ) sink()
		GCAM_SOURCE_RD <<- GCAM_SOURCE_RD - 1		# pop
	} else {
		printlog( "WARNING: Attempt to close a non-open log file")
	}
}

# -----------------------------------------------------------------------------
# readfilepathmap: read the file path map and return a list
# The file path map is simply a central (location below) mapping of all files
# and where they're to be found.
readfilepathmap <- function() {
	fn <- FILEPATHMAP
	fpm <- tryCatch( {
		 read.csv( fn, comment.char="#" )
#	}, warning=function( war ) {
#		warning( "Warning in read of", fn )
#		warning( war )
#		printlog( as.character( war ) )
	}, error=function( err ) {
		printlog( "Error in read of", fn )
		printlog( as.character( err ) )
		stop( err )		# can't recover from this
#	}, finally={

	} )	# end tryCatch

	if( length( fpm ) > 0 ) {
#		printlog( "All done reading", fn )
	} else {
		printlog( "Error: zero rows read from", fn )
		stop()
	}
	
	mylist <- list( NULL )		# make a lookup list of file names and paths
	for( i in 1:nrow( fpm ) ) 
		mylist[ as.character( fpm[ i, 1 ] ) ] <- as.character( fpm[ i, 2 ] )
	return( mylist )
}

# -----------------------------------------------------------------------------
# file_fqn: given a filename, return path+name (the fully qualified name)
# Default extension is ".csv" but this can be overridden
# This is the normal way for callers to get fqns for their files
file_fqn <- function( fn, extension=".csv" ) {
	map <- readfilepathmap()
	if( fn %in% names( map ) ) {
		return( paste( map[ fn ], "/", fn, extension, sep="" ) )
	} else {
		printlog( "Couldn't find", fn, "in file path map" )
		stop()
	}
}


# -----------------------------------------------------------------------------
# readdata: read an arbitrary data file
# params: fn (filename )
# TODO: error handling (probably using try/catch)
readdata <- function( fn, ... ) {

	myfn <- file_fqn( fn )

	# Update dependency list, if necessary
	deps <- DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]]
	if( !( fn %in% deps ) ) {
		DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]] <<- c( deps, myfn )
	}
	
	printlog( "Reading", myfn, cr=F )
	if( !file.exists( myfn ) ) {
		printlog( "WARNING: file", myfn, "does not appear to exist" )
	}
	x <- ( read.csv( myfn, na.strings="..", stringsAsFactors=F,
						comment.char=GCAM_DATA_COMMENT,	# Our comment signal
						... ) )	
	printlog( "...OK.", nrow( x ), "rows,", ncol( x ), "cols", ts=F )
	return( x )			
}

# -----------------------------------------------------------------------------
# readmap: read a mapping
# params: fn (filename )
# TODO: what's going on here with the paste? check callers
readmap <- function( fn, ... ) {
	myfn <- paste( fn, sep="" ) #"rgcam-mappings/", 
	return( readdata( myfn, ... ) )				
}

# -----------------------------------------------------------------------------
# writedata: write an arbitrary data file
# params: x (data frame), fn (filename, optional), fn_sfx (filename suffix; optional), comments (optional)
writedata <- function( x, fn=GCAM_SOURCE_FN, fn_sfx=NULL, comments=NULL, ... ) {
	if( length( fn_sfx ) ) {
		myfn <- paste( fn, "_", fn_sfx, sep="" )
	}
	myfn <- file_fqn( fn )
	
	printlog( "Writing", myfn, "w/", length( comments ), "comments" )

	tryCatch( {
		# Write the comments, if any, then the data
		cat( paste( GCAM_DATA_COMMENT, myfn ), file=myfn, sep="\n" )
		cat( paste( GCAM_DATA_COMMENT, "Written by", GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ), file=myfn, sep="\n", append=T )
		cat( paste( GCAM_DATA_COMMENT, date() ), file=myfn, sep="\n", append=T )
		for( i in 1:length( comments ) ) {
			cat( paste( GCAM_DATA_COMMENT, "\"", comments[ i ], "\"" ), file=myfn, sep="\n", append=T, ... )
		}
		w <- getOption( "warn" )
		options( warn=-1 )		# suppress the warning about columns names and appending
		write.table( x, file=myfn, sep=",", row.names=F, col.names=T, append=T, ... )
		options( warn=w )

		}, error=function( err ) {
			printlog( "Error in write of", fn )
			printlog( as.character( err ) )
			stop( err )		# can't recover from this
		}
	) # tryCatch
}

# -----------------------------------------------------------------------------
#Interpolation function

gcam_interp <- function( d, years, pattern= "X[0-9]{4}" ) {
	
	yearcols <- grep( pattern, names( d ) )
	x<-as.numeric( substr( names( d )[ yearcols ], 2, 5 ) )
	#diet_years <- c( 2005,2020,2035,2050)
	
	for( i in 1:nrow( d ) ) {
		y <- d[ i, yearcols ]
		li <- approx( x, y, xout=years )
		for( j in 1:length( years ) ) {
			d[ i, paste( "X", years[ j ], sep="" ) ] <- li$y[ j ]
		}
	}
	return( d )
}

# -----------------------------------------------------------------------------
#Function "is not an element of" (opposite of %in%)
'%!in%' <- function( x, y ) !( '%in%'( x, y ) )

# -----------------------------------------------------------------------------
# append_xml_line: insert a line at the end of an XML file
# Note: not actually at the end, but before last "/command"
# XML_tag: the tag to use
# valuetext: value to write for this XML tag
# XML_file: file we're working with
# searchtext: we can optionally eliminate previous references, based on this
append_xml_file <- function( XML_tag, valuetext, XML_file, searchtext=NA ) {
    printlog( "Inserting", valuetext, "into", XML_file, "using tag", XML_tag )
    contents <- scan( XML_file, what=character(), sep="\n" )   # read in file
    
    # If line(s) already exists in which 'myfn' occurs, nuke 'em
    if( !is.na( searchtext ) ) {
      founds <- which( grepl( searchtext, contents ) )
      if( length( founds ) ) {
        printlog( searchtext, "found in", XML_file, founds, "; deleting" )
        contents <- contents[ -founds ]  # delete line(s)    
      }
    }
    # OK, now insert our line at 'end' (before last line with insertpointstring below)
    insertpointstring <- "/command"
    if( sum( grepl( insertpointstring, contents ) ) == 0 ) {
       # uh oh
       printlog( insertpointstring, "not found in", XML_file )
       stop( "Batch XML file doesn't appear to be formatted correctly" )
    } 
    insertpt <-  which( grepl( insertpointstring, contents ) )
    if( length( insertpt ) > 1 ) {
      insertpt <- insertpt[ length( insertpt ) ]  # go to last occurrence       
    }
    cat( contents[ 1:insertpt-1 ], file=XML_file, sep="\n", append=F )
    fileline <- paste( "\t\t<", XML_tag, ">", valuetext, "</", XML_tag, ">\n", sep="" )
    cat( fileline, file=XML_file, append=T )
    cat( contents[ insertpt:length( contents ) ], file=XML_file, sep="\n", append=T )

}

# -----------------------------------------------------------------------------
# write_mi_xml: insert a line into batch_XML_file, consisting of
#                   <xml_tag> out_XML_file </xml_tag>
# The template batch file is specified at the beginning of the data build 
# <command name="CSV file">
#	 <headerFile>../rgcam-processing-code/Level2/headers_rgcam.txt</headerFile>
#	 <outFile>../rgcam-data/Level2/xml/For_Past_bio_input_hi.xml</outFile>
# </command>
insert_file_into_batchxml <- function( batch_XML_file, fn, fn_extension, xml_tag=NA ) {
    if( is.na( xml_tag ) ) {
      stop( "Missing xml_tag in insert_file_into_batchxml" )
    }
    mi_file <- file_fqn( batch_XML_file, extension="" )
    print(mi_file)
    myfn <- file_fqn( fn, extension="" )
    if( fn_extension != "" ) {
      myfn <- paste( myfn, fn_extension, sep="." )
    }
    
    if( !file.exists( mi_file ) ) {
      mi_template_file <- file_fqn( XML_TEMPLATE_FILENAME, extension="" )
      printlog( "Creating", batch_XML_file, "from", mi_template_file )
      file.copy( mi_template_file, mi_file )
      
      # We now have a brand-new batch XML file created from the template
      # Insert the header file reference into it
      valuetext <- paste( PATH_FROM_MI, file_fqn( GCAM_HEADERS_RGCAM, extension=""), sep="/"  )
      headertag <- "headerFile"
      append_xml_file( headertag, valuetext, mi_file, searchtext=headertag )
    }
    
    valuetext <- paste( PATH_FROM_MI, myfn, sep="/" )   # this is the between-tag text
    append_xml_file( xml_tag, valuetext=valuetext, mi_file, searchtext=valuetext )
}


# -----------------------------------------------------------------------------
# write_mi_data: write a data file to be read by modelInterface, and add data filename to batch XML file
# params: x (data frame), IDstring (table ID), fn (filename, optional), addToXML (boolean, optional)
# TODO: error handling (probably using try/catch)
# TODO: add in the capability to append the node rename table to the CSV
write_mi_data <- function( x, IDstring, fn=GCAM_SOURCE_FN[ GCAM_SOURCE_RD ], batch_XML_file=NA, node_rename=F, ... ) {
	
	myfn <- file_fqn( fn )
	
	printlog( "Writing", myfn )
	
	write( "INPUT_TABLE", myfn, append=F )
	write( "Variable ID", myfn, append=T )
	write( IDstring, myfn, append=T )
	write( "", myfn, append=T )
	options( warn=-1 )    # disable column name warning
	write.table( x, myfn, sep=",", row.names=F, col.names=T, append=T, quote=F )
	options( warn=0 )
	
	if( !is.na( batch_XML_file ) ) {    # caller wants to add this info to XML file
		insert_file_into_batchxml( batch_XML_file, fn, "csv", "csvFile" ) 
	}
	
	if( node_rename ) {		# the ModelInterface can't handle identically-named nodes, so we've given them different names
							# Now, we want to write rename instructions so that the MI cleans things up correctly		
		printlog( "Writing node renaming instructions..." )
		write( "\nINPUT_TABLE", myfn, append=T )
		write( "Variable ID", myfn, append=T )
		write( "11000", myfn, append=T )
		write( "", myfn, append=T )
		write( "from,to", myfn, append=T )
		write( "LandNode1,LandNode", myfn, append=T )
		write( "LandNode2,LandNode", myfn, append=T )
		write( "LandNode3,LandNode", myfn, append=T )		
	}
}


# -----------------------------------------------------------------------------
# avg_FAObaseyears: short function to average years (in columns) into model base periods (as rows)
# this depends on RGCAM_data.R (the 'data' header) already having been read
avg_FAObaseyears <- function( d, field ) {

	printlog( "Averaging FAO base years for", match.call()[[ 2 ]] )	# name of first parameter
	#Start with table of GCAM regions by commodities
	d_new <- d[ c( "GCAM_region_ID", "GCAM_commodity" ) ]
	
	#Repeat by number of model base years, and paste in sorted vector of model base years
	d_new <- d_new[ rep( 1:nrow( d_new ), times=length( model_base_years ) ), ]
	d_new$year <- sort( rep( model_base_years, length.out=nrow( d_new ) ) )

	#Create vector for data to be mapped in, and map in each model base year separately
	d_new[ d_new$year==model_base_years[ 1 ], field ] <- rowMeans( d[ names( d ) %in% X_base_years_per1_FAO ] )
	d_new[ d_new$year==model_base_years[ 2 ], field ] <- rowMeans( d[ names( d ) %in% X_base_years_per2_FAO ] )
	return( d_new )
}

# -----------------------------------------------------------------------------
# repeat_and_add_vector: function for repeating a dataframe in order to add a new vector
repeat_and_add_vector <- function( data, vector, vector_values ) {
     data_new <- data[ rep( 1:nrow( data ), times = length( vector_values ) ), ]
     data_new[[vector]] <- sort( rep( vector_values, length.out = nrow( data_new ) ) )
     return( data_new )
	 }

