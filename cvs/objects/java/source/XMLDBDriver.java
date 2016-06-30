/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/

import java.util.Properties;
import java.io.FileInputStream;
import java.io.PipedOutputStream;
import java.io.IOException;
import java.io.File;

/**
 * A helper class which gives GCAM a simple interface for adding or appending
 * XML to a BaseX database.  GCAM can create an instance of this class
 * giving the database location and the name of the document to add.  It
 * can then receive the data and stream it into the database by reading a buffer
 * via the receiveDataFromGCAM method.  When all data has been sent the finish
 * method is called which will wait until the BaseX has finished adding all of
 * the data.  Users can optionally configure this class to:
 *  - Apply a filter on the XML as it is stremed into the database.
 *  - Run a Model Interface batch file to exectue queries afer a GCAM scenario has
 *    finished running.
 *  - Create the database in memory so that they can be queries and discarded without
 *    the performance penalty to writing thmm to disk.
 *
 * @author Pralit Patel
 */
public class XMLDBDriver {
    /**
     * The stream that will read XML from GCAM to write to the DB (or filter if configured).
     */
    private final PipedOutputStream mReadFromGCAMStream = new PipedOutputStream();

    /**
     * The class that will handle writing database to the BaseX DB.
     */
    private WriteLocalBaseXDB mWriteDB = null;

    /**
     * A class that can be used to filter GCAM output XML before it reaches the DB.
     * If null output will be writen to the database unfiltered.
     */
    private FilterOutput mFilterOutput = null;

    /**
     * A class that can be used to run queries against the BaseX DB.
     * If null no queries will be run. These will wait to run until finalizeAndClose
     * is called so that we can ensure that all data has been appended to the database
     * and GCASM has cleared out some memory.
     */
    private RunQueries mRunQueries = null;

    /**
     * The buffer size to use in all of these streams passing around data.
     * We use a buffer size of 1 MB which seems large enough to keep the DB continuously
     * fed with data to write.
     */
    public static final int BUFFER_SIZE = 1024 * 1024;

    /**
     * Constructor which will open the DB and get ready to receive XML to put
     * into the DB.
     * @param aDBLocation The location of the database to open.
     * @param aDocName A unique document name to use to store the XML in the DB.
     */
    public XMLDBDriver( final String aDBLocation, final String aDocName ) {
        Properties config = new Properties();
        try {
            File configFile = new File( "XMLDBDriver.properties" );
            if( configFile.exists() ) {
                config.loadFromXML( new FileInputStream( configFile ) );
            }

            // always open the database optionally in memory (off by default)
            boolean inMemDB = Boolean.parseBoolean( config.getProperty( "in-memory", "false" ) );
            mWriteDB = new WriteLocalBaseXDB( aDBLocation, aDocName, inMemDB );

            // optionally filter output using an XSLT style script (off by default)
            String filterScript = config.getProperty( "filter-script", "" );
            mFilterOutput = filterScript.isEmpty() ? null : new FilterOutput( filterScript );

            // optionally run a batch batch query (off by default)
            String batchFile = config.getProperty( "batch-queries", "" );
            mRunQueries = batchFile.isEmpty() ? null : new RunQueries( batchFile );

            // connect up the XML streams so that it passes from:
            // GCAM -> Filter (if it exists) -> DB
            if( mFilterOutput != null ) {
                mFilterOutput.setInputStream( mReadFromGCAMStream );
                mWriteDB.setInputStream( mFilterOutput.getOutputStream() );
            }
            else {
                mWriteDB.setInputStream( mReadFromGCAMStream );
            }

            // start the workers for filtering and storing data however not
            // running queries which will wait until finalizeAndClose.
            if( mFilterOutput != null ) {
                mFilterOutput.start();
            }
            mWriteDB.start();
        }
        catch( Exception error ) {
            error.printStackTrace();
            try {
                mReadFromGCAMStream.close();
            }
            catch( IOException ioError ) {
                // ignore
            }
            cancelWrite();
        }
    }

    /**
     * Cancel writing the data to the DB presumably because there was
     * an error.
     */
    private void cancelWrite() {
        // stop any workers processing data if they have even been started.
        if( mFilterOutput != null ) {
            mFilterOutput.cancel();
            mFilterOutput = null;
        }
        if( mWriteDB != null ) {
            mWriteDB.cancel();
            mWriteDB = null;
        }
    }

    /**
     * Notify that no more XML from GCAM will be sent.  This method
     * will wait until all of the data has been stored to the DB before
     * returning.
     */
    public void finish() {
        try {
            mReadFromGCAMStream.close();
        }
        catch( IOException ioError ) {
            // ignore
        }

        // This will block until the database is done storing
        // the data sent from GCAM.
        if( mFilterOutput != null ) {
            mFilterOutput.finish();
        }
        if( mWriteDB != null ) {
            mWriteDB.finish();
        }

        // note we keep the database open in case additional processing
        // will occur.
    }

    /**
     * Run any final processing that the user may have requested then close
     * the database out.  At this point GCAM has cleaned out it's memory and
     * will not be appending any more data to the scenariothis would be a good
     * time to run queries if requested and then close the database.
     */
    public void finalizeAndClose() {
        // error checking if an earlier error has already closed the database.
        if( mWriteDB == null ) {
            if( mRunQueries != null ) {
                System.err.println( "ERROR: Database has already been closed.  Could not run batch queries." );
            }
            // else there were no queries to run and the earlier error was already printed so no need to print again
            return;
        }

        // If the user wanted to run some queries now is the time to run them.
        if( mRunQueries != null ) {
            mRunQueries.start( mWriteDB.getDBLocation(), mWriteDB.getContext() );
            // finish will blcok until the queries have funished running.
            mRunQueries.finish();
        }

        mWriteDB.close();
    }

    /**
     * Receives data from GCAM in a buffer intending to be sent from
     * a stream.
     * @param aBuffer The raw XML data from GCAM.
     * @param aLength The amount of data that was sent in the buffer.
     * @return An error flag set to true if an error occurred.
     */
    public boolean receiveDataFromGCAM( byte[] aBuffer, int aLength ) {
        boolean hadError = false;
        try {
            mReadFromGCAMStream.write( aBuffer, 0, aLength );
        }
        catch ( IOException ioError ) {
            ioError.printStackTrace();
            cancelWrite();
            hadError = true;
        }
        finally {
            return hadError;
        }
    }

    /**
     * Updates the existing document by inserting the given data after the
     * given location.
     * @param aData The XML data to insert.
     * @param aLocation An XPath that describes where to insert the data after.
     * @return True if the data was successfully updated, false otherwise.
     */
    public boolean appendData( final String aData, final String aLocation ) {
        if( mWriteDB == null ) {
            System.err.println( "ERROR: Could not append data as database has already been closed." );
            return false;
        }
        else {
            return mWriteDB.appendData( aData, aLocation );
        }
    }

    /**
     * Provide a main method to run the driver as a stand alone tool apart from GCAM by reading
     * the XML from a file instead.
     * This may be useful for a number of reasons:
     *  - Test out a filter script to ensure it behaves as expected
     *  - Accumulate exported runs into a database
     *  - Simply run queries on an exported XML file (likely using an in-memory DB)
     *
     * @param aArgs The command line arguments are required (the properties file is loaded as always):
     *  java -jar XMLDBDriver.jar --db-path [Path to DB]
     *                            --doc-name [The unique name to call the document in the DB]
     *                            --xml [The exported GCAM results XML file to load]
     */
    public static void main( String[] aArgs ) throws Exception {
        // Parse command line arguments
        String error = null;
        String dbPath = null;
        String docName = null;
        String xmlFile = null;
        for( int i = 0; i < aArgs.length && error == null; ) {
            if( ( i + 1 ) >= aArgs.length ) {
                error = "Insufficient number of arguments.";
            }
            else if( aArgs[ i ].equals( "--db-path" ) ) {
                dbPath = aArgs[ i + 1 ];
            }
            else if( aArgs[ i ].equals( "--doc-name" ) ) {
                docName = aArgs[ i + 1 ];
            }
            else if( aArgs[ i ].equals( "--xml" ) ) {
                xmlFile = aArgs[ i + 1 ];
            }
            else {
                error = "Uknown argument: "+aArgs[ i ];
            }
            i += 2;
        }
        // ensure all of the required flags were parsed
        // do not override an already existing error message
        if( error == null ) {
            if( dbPath == null ) {
                error = "Flag --db-path was not set";
            }
            else if( docName == null ) {
                error = "Flag --doc-name was not set";
            }
            else if( xmlFile == null ) {
                error = "Flag --xml was not set";
            }
        }
        // Check if we have an error message for any reason, if so print it and exit
        if( error != null ) {
            System.err.println( "ERROR: "+error );
            System.err.println( "Usage: java -cp XMLDBDriver.jar XMLDBDriver --db-path [Path to DB]" );
            System.err.println( "            --doc-name [The unique name to call the document in the DB" );
            System.err.println( "            --xml [The exported GCAM results XML file to load]" );
        }
        else {
            // Run the XMLDBDriver by mimicing the sequence of method calls GCAM would make
            XMLDBDriver driver = new XMLDBDriver( dbPath, docName );

            // copy the XML file through processing streams via receiveDataFromGCAM
            FileInputStream xmlRead = new FileInputStream( xmlFile );
            byte[] buffer = new byte[ XMLDBDriver.BUFFER_SIZE ];
            int read = 0;
            while( ( read = xmlRead.read( buffer ) ) != -1 ) {
                boolean hadError = driver.receiveDataFromGCAM( buffer, read );
                if( hadError ) {
                    // There was an error in set up.  Those messages have already been
                    // printed so we just need to stop trying to send data.
                    break;
                }
            }
            xmlRead.close();

            // wait for the XML to be finished processing and stored
            driver.finish();
            // run any potential queries and close the DB
            driver.finalizeAndClose();
        }
    }
}

