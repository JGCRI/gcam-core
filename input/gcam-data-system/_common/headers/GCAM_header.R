# The GCAM R header file
# Ben Bond-Lamberty August 2011, updated February 2012

# This file should be source'd by any R script involved in the processing of GCAM input data
# It provides various facilities, including logging, file I/O and definition of common settings

# -----------------------------------------------------------------------------
# Load required libraries
libs <- c( "reshape2", "stringr" )
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

MODULE_PROC_ROOT		<- ""   #Module processing code root folder should be set in module-specific header file
GCAM_DATA_COMMENT 		<- "#"							# Comment character for files
XML_TEMPLATE_FILENAME 	<- "batch_xml_template.xml"		                               # XML template file name
GCAM_HEADERS_MI 		<- "ModelInterface_headers.txt"				               # csv to xml header file name
PATH_FROM_MI 			<- ""		                                       # Path from Model Interface
DOMAINPATHMAP 			<- paste( MODULE_PROC_ROOT, "../_common/mappings/domainmapping.csv", sep="" )    # List of domain (groups of files) mappings


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
	logpath <- paste( MODULE_PROC_ROOT, "/logs/", sep = "")
	if( savelog ) sink( paste( logpath, fn, ".log", sep = "" ), split=T )
	printlog( "-----" )
	printlog( "Starting", fn )
	
	DEPENDENCIES[[ fn ]] <<- c( NULL ) # Create a new entry in the dependency list
}

# -----------------------------------------------------------------------------
# writemakefiledepend: write dependency information for inclusion in makefile
writemakefiledepend <- function(outfile) {
	of_parts <- unlist(strsplit(outfile,"\\."))
	if(length(of_parts) == 1) {
	    of_root <- of_parts[1]
	    of_ext  <- ""
	} else {
	    of_root = paste(of_parts[1:length(of_parts)-1], collapse=".")
	    of_ext  = of_parts[-1]
	}
        fn <- GCAM_SOURCE_FN[ GCAM_SOURCE_RD ]
	dfile <- paste(of_root, ".d", sep="")
	write(paste(outfile,": \\",sep=""),file=dfile)
	cat("  ", DEPENDENCIES[[ fn ]], file=dfile, append=TRUE, sep=" \\\n  ") 
}


# -----------------------------------------------------------------------------
# logstop: stop the current log (to screen and optionally file)
logstop <- function() {

	fn <- GCAM_SOURCE_FN[ GCAM_SOURCE_RD ]

	printlog( "Writing dependency information for", fn, "..." )
	logfile <- paste( MODULE_PROC_ROOT, "/logs/", fn, ".log", sep="" )
	writemakefiledepend(logfile)
	
	printlog( "All done with", fn )
	if( GCAM_SOURCE_RD > 0 ) {
		if( GCAM_LOG_SAVE[ GCAM_SOURCE_RD ] ) sink()
		GCAM_SOURCE_RD <<- GCAM_SOURCE_RD - 1		# pop
	} else {
		printlog( "WARNING: Attempt to close a non-open log file")
	}
}

# -----------------------------------------------------------------------------
# readdomainpathmap: read the domain path map and return a list
# The file path map is simply a central (location below) mapping of all files
# and where they're to be found.
readdomainpathmap <- function() {
	fn <- DOMAINPATHMAP
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
#		print( "All done reading", fn )
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
# file_fqn: given a domain name and a file name, return path+name (the fully qualified name)
# Default extension is ".csv" but this can be overridden
# This is the normal way for callers to get fqns for their files
file_fqn <- function( domain, fn, extension=".csv" ) {
	map <- readdomainpathmap()
	if( domain %in% names( map ) ) {
		return( paste( map[ domain ], "/", fn, extension, sep="" ) )
	} else {
		printlog( "Couldn't find domain", domain, "in domain path map" )
		stop()
	}
}



# -----------------------------------------------------------------------------
# readdata: read an arbitrary data file
# params: fn (filename )
# TODO: error handling (probably using try/catch)
readdata <- function( domain="none", fn="none", extension=".csv", na.strings="", must.exist=TRUE, ... ) {

	if( domain=="none" | fn=="none" ) {
		printlog( "ERROR: no domain/file specified", fn )
		stop( "Error: you need to specify both a filename and domain" )
	}

	myfn <- file_fqn( domain, fn, extension )

	printlog( "Reading", myfn, cr=F )

	if( file.exists( myfn ) ) {
	
		x <- ( read.csv( myfn, na.strings=na.strings, stringsAsFactors=F,
							comment.char=GCAM_DATA_COMMENT,	# Our comment signal
							... ) )	
		printlog( "...OK.", nrow( x ), "rows,", ncol( x ), "cols", ts=F )

		# Update dependency list, if necessary
		deps <- DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]]
		if( !( fn %in% deps ) ) {
			DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]] <<- c( deps, myfn )
		}
	
		return( x )
		
	} else {
		printlog( "...does not exist" )
		stopifnot( !must.exist )
		return( NULL )
	}
}

# -----------------------------------------------------------------------------
# sourcedata: read a .R file used to store data
# params: domain (location), fn (filename )
# TODO: error handling (probably using try/catch)
sourcedata <- function( domain="none", fn="none", extension=".R", ... ) {

	if( domain=="none" | fn=="none" ) {
		printlog( "ERROR: no domain/file specified", fn )
		stop( "Error: you need to specify both a filename and domain" )
	}

	myfn <- file_fqn( domain, fn, extension )

	# Update dependency list, if necessary
	deps <- DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]]
	if( !( fn %in% deps ) ) {
		DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]] <<- c( deps, myfn )
	}
	
	printlog( "Reading", myfn, cr=F )
	if( !file.exists( myfn ) ) {
		printlog( "WARNING: file", myfn, "does not appear to exist" )
	}
	source( myfn )	
	printlog( "...OK.", ts=F )
}

# -----------------------------------------------------------------------------
# adddep: Adds a fully qualified name to the dependency list
# params: fqn (filename )
# TODO: error handling (probably using try/catch)
adddep <- function( fqn, ... ) {

	# Update dependency list, if necessary
	deps <- DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]]
	if( !( fqn %in% deps ) ) {
		DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]] <<- c( deps, fqn )
	}
}

# -----------------------------------------------------------------------------
# writedata: write an arbitrary data file
# params: x (data frame), fn (filename, optional), fn_sfx (filename suffix; optional), comments (optional)
writedata <- function( x, domain="none", fn=GCAM_SOURCE_FN, fn_sfx=NULL, comments=NULL, ... ) {

	if( domain=="none" ) {
		printlog( "ERROR: no domain specified for", fn )
		stop( "Error: you need to specify both a filename and domain" )
	}

	if( length( fn_sfx ) ) {
		myfn <- paste( fn, "_", fn_sfx, sep="" )
	}
	myfn <- file_fqn( domain, fn )
	
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

gcam_interp <- function( d, years, pattern= "X[0-9]{4}",rule=1) {
	yearcols <- grep( pattern, names( d ) )
	x<-as.numeric( substr( names( d )[ yearcols ], 2, 5 ) )
	d.out=matrix(data=NA,nrow=length(d[,1]),ncol=length(years))
	colnames(d.out)=paste("X", years,sep="");
    if(rule == 3) {
        # rule 3 means we will extrapolate data using a exponential growth/decay
        # formulation.  Note that this only will only apply to extrapolation
        # and any interpolations we still rely on the approx method (aka rule=1)
        do.exp.extrapolate <- TRUE
        rule <- 1
        stopifnot(any( "improvement.max" %in% colnames(d)))
        stopifnot(any( "improvement.rate" %in% colnames(d)))
        shadow.colname <- colnames(d)[grep('improvement.shadow.', colnames(d))]
        shadow.colvalues <- d[,sub( 'improvement.shadow.', '', shadow.colname)]
        shadow.rows <- match( d[, shadow.colname], shadow.colvalues )
    } else {
        do.exp.extrapolate <- FALSE
    }
    for(i in 1:nrow(d)) {
		y <- d[i , yearcols ]; 
        d.out[i, paste( "X", years, sep="" )]=approx( x, y, xout=years,rule=rule)$y;
    }
    if(do.exp.extrapolate) {
        rowstoprocess <- 1:nrow(d)
        while(length(rowstoprocess) > 0 ) {
            i <- rowstoprocess[1]
            rowstoprocess <- rowstoprocess[-1]
            if(is.na(shadow.rows[i]) || shadow.rows[i] %!in% rowstoprocess) {
                # get the last year which is not NA
                extrap.base.year <- years[max(which(!is.na(d.out[i, paste0("X", years)])))]
                extrap.Xbase.year <- paste0("X", extrap.base.year)

                # get the years that need to be extrapolated
                extrap.years <- years[years > extrap.base.year]
                extrap.Xyears <- paste0("X", extrap.years)
                if(length(extrap.years) == 0) {
                    # All values were provided, no extrapolation to do for this row so just
                    # skip to the next
                    next
                }

                # get the base value to extrapolate from
                extrap.value <- d.out[i, extrap.Xbase.year]

                if(!is.na(shadow.rows[i])) {
                    # if this row has a shadow row then it's value is the same as that row
                    # plus the improvements calculated from the difference of the value in
                    # extrap.base.year from the shadow row
                    d.out[i,extrap.Xyears] <- d.out[shadow.rows[i],extrap.Xyears]
                    extrap.value <- extrap.value - d.out[shadow.rows[i],extrap.Xbase.year]
                } else {
                    d.out[i,extrap.Xyears] <- 0
                }
                d.out[i,extrap.Xyears] <- d.out[i,extrap.Xyears] + extrap.value * d[i,"improvement.max"] + (extrap.value - extrap.value * d[i, "improvement.max"]) *
                    (1-d[i,"improvement.rate"]) ^ (extrap.years - extrap.base.year)
            } else {
                # this row shadows another row which has yet to he processed so add it back
                # to the end of the list 
                rowstoprocess <- c(rowstoprocess, i)
            }
        }
	}
	d[ , paste( "X", years, sep="" ) ] =d.out[,paste( "X", years, sep="" )]
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
#	 <headerFile>../aglu-processing-code/Level2/headers_aglu.txt</headerFile>
#	 <outFile>../aglu-data/Level2/xml/For_Past_bio_input_hi.xml</outFile>
# </command>
insert_file_into_batchxml <- function( batch_XML_domain="none", batch_XML_file, domain="none", fn, fn_extension, xml_tag=NA ) {

	if( batch_XML_domain=="none" | domain=="none" ) {
		printlog( "ERROR: no domain specified for", fn )
		stop( "Error: you need to specify a domain for batch XML file and for CSV file" )
	}

    if( is.na( xml_tag ) ) {
      stop( "Missing xml_tag in insert_file_into_batchxml" )
    }
    
    mi_file <- file_fqn( batch_XML_domain, batch_XML_file, extension="" )
    print(mi_file)
    myfn <- file_fqn( domain, fn, extension="" )
    if( fn_extension != "" ) {
      myfn <- paste( myfn, fn_extension, sep="." )
    }
    
    if( !file.exists( mi_file ) ) {
      mi_template_file <- file_fqn( "COMMON_HEADERS", XML_TEMPLATE_FILENAME, extension="" )
      printlog( "Creating", batch_XML_file, "from", mi_template_file )
      file.copy( mi_template_file, mi_file )
      
      # We now have a brand-new batch XML file created from the template
      # Insert the header file reference into it
      valuetext <- paste( PATH_FROM_MI, file_fqn( "COMMON_HEADERS", GCAM_HEADERS_MI, extension=""), sep=""  )
      headertag <- "headerFile"
      append_xml_file( headertag, valuetext, mi_file, searchtext=headertag )
    }
    
    valuetext <- paste( PATH_FROM_MI, myfn, sep="" )   # this is the between-tag text
    append_xml_file( xml_tag, valuetext=valuetext, mi_file, searchtext=valuetext )
}


# -----------------------------------------------------------------------------
# write_mi_data: write a data file to be read by modelInterface, and add data filename to batch XML file
# params: x (data frame), IDstring (table ID), fn (filename, optional), addToXML (boolean, optional)
# TODO: error handling (probably using try/catch)
# TODO: add in the capability to append the node rename table to the CSV
write_mi_data <- function( x, IDstring, domain="none", fn=GCAM_SOURCE_FN[ GCAM_SOURCE_RD ], batch_XML_domain = "none", batch_XML_file=NA, node_rename=F, ... ) {

	if( batch_XML_domain == "none" | domain=="none" ) {
		printlog( "ERROR: no domain specified for", fn )
		stop( "Error: you need to specify a domain for both batch XML file and CSV file" )
	}

	myfn <- file_fqn( domain, fn )
	
	printlog( "Writing", myfn )
	
	write( "INPUT_TABLE", myfn, append=F )
	write( "Variable ID", myfn, append=T )
	write( IDstring, myfn, append=T )
	write( "", myfn, append=T )
	options( warn=-1 )    # disable column name warning
	write.table( x, myfn, sep=",", row.names=F, col.names=T, append=T, quote=F )
	options( warn=0 )
	
	if( !is.na( batch_XML_file ) ) {    # caller wants to add this info to XML file
		insert_file_into_batchxml( batch_XML_domain, batch_XML_file, domain, fn, "csv", "csvFile" ) 
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
		write( "LandNode4,LandNode", myfn, append=T )		
	}
}

# -----------------------------------------------------------------------------
# add_region_name: function for creating a new vector with a region name, for a given region ID
add_region_name <- function( data ) {
     data_new <- data
     data_new["region"] <- GCAM_region_names$region[
          match( data[["GCAM_region_ID"]], GCAM_region_names$GCAM_region_ID ) ]
     return( data_new )
	 }

# -----------------------------------------------------------------------------
# repeat_and_add_vector: function for repeating a dataframe in order to add a new vector
repeat_and_add_vector <- function( data, vector, vector_values ) {
     data_new <- data[ rep( 1:nrow( data ), times = length( vector_values ) ), ]
     data_new[[vector]] <- sort( rep( vector_values, length.out = nrow( data_new ) ) )
     return( data_new )
	 }

# -----------------------------------------------------------------------------
# set_years: function for converting text descriptions of years to numerical values
set_years <- function( data ) {
     data_new <- data
     data_new[data_new == "start-year"] <- min( model_base_years )
     data_new[data_new == "final-calibration-year" ] <- max( model_base_years )
     data_new[data_new == "final-historical-year" ] <- max( historical_years )
     data_new[data_new == "initial-future-year" ] <- min( model_future_years )
     data_new[data_new == "initial-nonhistorical-year" ] <- min( model_years[ model_years > max( historical_years ) ] )
     data_new[data_new == "end-year" ] <- max( model_future_years )
     return( data_new )
	 }

# -----------------------------------------------------------------------------
#vecpaste: this is a function for pasting together any number of variables to be used as unique identifiers in a lookup
vecpaste <- function (x) {
     y <- x[[1]]
     if (length(x) > 1) {
         for (i in 2:length(x)) {
             y <- paste(y, x[[i]] )
         }
     }
     y
 }

# -----------------------------------------------------------------------------
# interpolate_and_melt: melt a table with years as columns and interpolate to all specified years
interpolate_and_melt <- function( data, years, value.name="value", digits = NA, rule=1 ){
	data_new <- gcam_interp( data, years, rule=rule )
	if( any(is.na( data_new[, paste0('X', years) ] ) ) ) stop( "Provided years in input data do not span range of desired output years" )
	data_new.melt <- melt( data_new, id.vars = grep( "X[0-9]{4}", names( data_new ), invert = T ) )
	if( !is.na( digits ) ) data_new.melt$value <- round( data_new.melt$value, digits )
	names( data_new.melt )[ names( data_new.melt ) == "value" ] <- value.name
	data_new.melt$year <- as.numeric( substr( data_new.melt$variable, 2, 5 ) )
	data_new.melt <- data_new.melt[ order( data_new.melt$year ), ]
	data_new.melt <- subset( data_new.melt, year %in% years )
	return( data_new.melt ) 
}

# -----------------------------------------------------------------------------
#translate_to_full_table: function to write out all possible combinations of variables
translate_to_full_table <- function( data, var1, var1_values, var2, var2_values, var3 = NA, var3_values = NA, datacols = X_AGLU_historical_years, na.value = 0 ){
	data_new <- data.frame(
	     rep( sort( var1_values ), times = length( var2_values ) ),
	     sort( rep( var2_values, times = length( var1_values ) ) ) )
	names( data_new ) <- c( var1, var2 )
	if( is.na( var3 ) ){
		data_new[ datacols ] <- data[
			match( vecpaste( data_new[ c( var1, var2 ) ] ), vecpaste( data[ c( var1, var2 ) ] ) ),
			datacols ]
	}
	if( !is.na( var3 ) ){
		data_new <- repeat_and_add_vector( data_new, var3, var3_values )
		data_new[ datacols ] <- data[
			match( vecpaste( data_new[ c( var1, var2, var3 ) ] ), vecpaste( data[ c( var1, var2, var3 ) ] ) ),
			datacols ]
	}
	data_new[ is.na( data_new ) ] <- na.value
	return( data_new )
}

# -----------------------------------------------------------------------------
# set_subsector_shrwt: calculate subsector shareweights in calibration periods, where subsectors may have multiple technologies
set_subsector_shrwt <- function( data,
  value.name="calOutputValue", region.name="region", sector.name="supplysector", subsector.name="subsector", year.name="year",
  result.column.name="subs.share.weight" ){
	data_aggregated <- aggregate( data[value.name],
	      by=list( region = data[[region.name]], sector = data[[sector.name]], subsector = data[[subsector.name]], year = data[[year.name]] ),
	      FUN=sum )
	data_new <- data
	data_new[[result.column.name]] <- ifelse( data_aggregated[[value.name]][
	      match( paste( data_new[[region.name]], data_new[[sector.name]], data_new[[subsector.name]], data_new[[year.name]] ),
	             paste( data_aggregated$region, data_aggregated$sector, data_aggregated$subsector, data_aggregated$year ) ) ] > 0, 1, 0 )
	return( data_new ) 	
}

# Gets a list of all valid logit types.
# WARNING: this needs to be kept in sync with the model and headers
get_logit_types <- function() {
    return( c( "relative-cost-logit", "absolute-cost-logit" ) )
}

# -----------------------------------------------------------------------------
# Generates a list of tables that sets the appropriate discrete choice function to use.
# The data has to be partitioned into multiple tables, one for each logit.type.  The returned
# list has fore each element containing two variables: 1) The header for the table 2) The data for the table
# If requested an additional table will be added for an EQUIV_TABLE so that the tables that contain the logit
# exponents do not themselves have to know what logit.type they are using and thus do not need to be
# partitioned.
# data: The data to partition by logit.type
# names: The column names to use out of data
# default.logit.type: The default logit function to use if the user did not specify (NA value)
# base.header: The base header that is used for the logit type tables which will get pasted with
#              what the logit.type for each table.
# include.equiv.table: If the EQUIV_TABLE should be included as well.
# write.all.regions: If each table should be written to all regions
# ...: Any additional params to be passed to write.all.regions
get_logit_fn_tables <- function( data, names, default.logit.type="relative-cost-logit", base.header,
                                 include.equiv.table, write.all.regions, ... )
{
    # Set the logit type to the default logit if the user did not explicitly set one.
    data[ is.na( data$logit.type ), "logit.type" ] <- default.logit.type
    # Note it is safer to create tables for all valid logit types rather than just the
    # ones included in unique( data$logit.type ) even if it results in an empty table
    # since if we switch all from one type to the other and do not clean out the level
    # 2 CSV and batch file we will be left with both defined for all rows which is bad.
    all_logit_types <- get_logit_types()

    # Create the EQUIV_TABLE table which allows the Model Interface to be ambiguous about what the
    # actual logit type is when setting the logit exponent.
    tables <- list()
    if( include.equiv.table ) {
        tables[[ "EQUIV_TABLE" ]]$header <- "EQUIV_TABLE"
        tables[[ "EQUIV_TABLE" ]]$data <- data.frame( group.name=c("LogitType"), tag1=c("dummy-logit-tag"),
            tag2=c("relative-cost-logit"), tag3=c("absolute-cost-logit"), stringsAsFactors=FALSE )
    }

    # Loop through each of the logit types that were found in data and create a table for it
    # using the appropriate header name and writing to all regions if requested.
    for( curr_logit_type in all_logit_types ) {
        tables[[ curr_logit_type ]]$header <- paste0( base.header, curr_logit_type )
        curr.data <- data[ data$logit.type == curr_logit_type, ]
        if( write.all.regions ) {
            if( nrow( curr.data ) > 0 ) {
                curr.data <- write_to_all_regions( curr.data, names, ... )
            } else {
                curr.data <- cbind( curr.data, data.frame( region=character(0) ) )
            }
        }
        tables[[ curr_logit_type ]]$data <- curr.data[, names ]
    }

    return(tables)
}

# Read all logit function type tables given a base table name and return them in a list
# where the full table name -> the read data.
# domain: The data domain to read the table from
# table.name: The base table name to generate all logit type table names
# skip: Whether to skip any lines (such as head information) when reading the table
# include.equiv.table: Whether to include the EQUIV_TABLE in the list of results
read_logit_fn_tables <- function( domain, table.name, skip=0, include.equiv.table=F ) {
    all_logit_types <- get_logit_types()
    all_table_names <- paste0( table.name, all_logit_types )
    if( include.equiv.table ) {
        # need to include the appropraite processing code number to append
        proc_number <- regmatches( table.name, regexpr( '^L[[:digit:]]+.', table.name ) )
        all_table_names <- c( paste0( proc_number, "EQUIV_TABLE" ), all_table_names )
    }
    ret_tables <- list()
    for( curr_table_name in all_table_names ) {
        ret_tables[[ curr_table_name ]] <- readdata( domain, curr_table_name, skip=skip )
    }
    return( ret_tables )
}

#add_agtech_names: function to use the commodity and GLU names to create agsupplysectors, subsectors, technologies
add_agtech_names <- function( data ){
  data[[agsupp]] <- data[[C]]
  data[[agsubs]] <- paste( data[[C]], data[[GLU]], sep = crop_GLU_delimiter )
  data[[agtech]] <- data[[agsubs]]
  if( irr %in% names( data ) ){
    data[[agtech]] <- paste( data[[agsubs]], data[[irr]], sep = irr_delimiter )
  }	
  return( data )
}

write_to_all_regions <- function( data, names, has.traded=F, apply.to = "selected", set.market = F ){
  if ( "logit.year.fillout" %in% names ) data$logit.year.fillout <- "start-year"
  if ( "price.exp.year.fillout" %in% names ) data$price.exp.year.fillout <- "start-year"
  data_new <- set_years( data )
  data_new <- repeat_and_add_vector( data_new, "GCAM_region_ID", GCAM_region_names$GCAM_region_ID )
  data_new <- add_region_name( data_new )
  if ("market.name" %in% names ) data_new$market.name <- data_new$region
  if( has.traded==T){
    if( set.market==T){
      data_new$market.name <- data_new$region
    }
    data_new <- set_traded_names( data_new, apply.to )
  }
  return( data_new[ names ] ) 
}

# -----------------------------------------------------------------------------
# set_traded_names: convert names of traded secondary goods to be contained within region 1, with region appended to subsector and tech names
set_traded_names <- function( data, apply.to="selected" ) {
  data_new <- data
  if( apply.to=="selected" ){
    if( "subsector" %in% names( data ) ){
      data_new$subsector[data$traded == 1] <- paste( data$region[data$traded == 1], data$subsector[data$traded == 1], sep = " " )
    } 
    if( "technology" %in% names( data ) ){
      data_new$technology[data$traded == 1] <- paste( data$region[data$traded == 1], data$technology[data$traded == 1], sep = " " )
    } 
    data_new$region[data$traded == 1] <- GCAM_region_names$region[1]
    return( data_new )
  }
  if(apply.to=="all" ){
    if( "subsector" %in% names( data ) ){
      data_new$subsector <- paste( data$region, data$subsector, sep = " " )
    } 
    if( "technology" %in% names( data ) ){
      data_new$technology <- paste( data$region, data$technology, sep = " " )
    } 
    data_new$region <- GCAM_region_names$region[1]
    return( data_new )
  }
}




