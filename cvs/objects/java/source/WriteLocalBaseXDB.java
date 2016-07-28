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

import java.io.PipedOutputStream;
import java.io.PipedInputStream;
import java.io.IOException;
import java.io.File;

import org.basex.core.Context;
import org.basex.core.MainOptions;
import org.basex.core.cmd.Check;
import org.basex.core.cmd.Add;
import org.basex.core.cmd.Close;
import org.basex.query.QueryProcessor;

/**
 * A helper class which gives GCAM a simple interface for adding or appending
 * XML to a local BaseX database.  The XMLDBDriver will set up the interactions
 * and notify when to close as results may be filtered before reaching the database
 * and run queries after GCAM has cleaned up the Scenario.
 * @author Pralit Patel
 */
public class WriteLocalBaseXDB implements Runnable {
    /**
     * The database context needed to run commands on the DB.
     */
    private Context mContext = null;

    /**
     * The thread on which writing to the DB will take place.
     */
    private final Thread mWorkerThread = new Thread( this );

    /**
     * The command which executes adding the XML to the database.
     * We keep a reference here in case we need to cancel the
     * command incase something went wrong.
     */
    private Add mAddCommand = null;

    /**
     * The stream that will transfer the XML read from GCAM and write it to the DB.
     */
    private final PipedInputStream mWriteToDBStream = new PipedInputStream( XMLDBDriver.BUFFER_SIZE );

    /**
     * The location of the database to write the XML to.
     */
    private final String mDBLocation;

    /**
     * A unique name to call the document to be added into the DB.
     */
    private final String mDocName;

    /**
     * Constructor which will open the DB and get ready to receive XML to put
     * into the DB.
     * @param aDBLocation The location of the database to open.
     * @param aDocName A unique document name to use to store the XML in the DB.
     * @param aInMemoryDB If the database is to be stored in memory only and never
     *                    written to disk.  This would only be useful if the users
     *                    were going to run queries on it after since as soon as the
     *                    database is closed the data is lost.
     * @param aOpenDBWait A timeout in seconds to wait for user intervention before
     *                    attempting to write to a DB which appears to be open. A
     *                    negative value indicates to wait indefinately.
     */
    public WriteLocalBaseXDB( final String aDBLocation, final String aDocName,
                              final boolean aInMemoryDB, final int aOpenDBWait ) throws Exception
    {
        // Set the DB location and doc name.
        mDBLocation = aDBLocation;
        mDocName = aDocName;

	if ( aInMemoryDB ) {
	    System.out.println("Opening in-memory database");
	}
	
        // Open the database
        openDB( aInMemoryDB, aOpenDBWait );
    }

    /**
     * Get the file location for this database.
     * @return The DB location this database was opened with.
     */
    public String getDBLocation() {
        return mDBLocation;
    }

    /**
     * Get the database context of this DB.  This context could be used to
     * run queries on.
     * @return The database context.
     */
    public Context getContext() {
        return mContext;
    }

    /**
     * Opens the database.  We will "Check" the database which will open it
     * if it already exists or create a new one otherwise.
     * @param aInMemoryDB If the databse is to be stored in memory only.
     * @param aOpenDBWait A timeout in seconds to wait for user intervention before
     *                    attempting to write to a DB which appears to be open. A
     *                    negative value indicates to wait indefinately.
     */
    private void openDB( final boolean aInMemoryDB, final int aOpenDBWait ) throws Exception {
        // Figure out the path to the database
        // First try the unix file separator
        int indexOfFileSep = mDBLocation.lastIndexOf( '/' );
        String path = null;
        String containerName = null;
        if( indexOfFileSep == -1 ) {
            // that wasn't found so try the windows
            indexOfFileSep = mDBLocation.lastIndexOf( '\\' );
            if( indexOfFileSep == -1 ) {
                // still not found, maybe there was no path so just assume current directory
                path = ".";
                containerName = mDBLocation;
            }
        }

        // Parse out the path and the container name from the DB location.
        if( indexOfFileSep != -1 ) {
            path = mDBLocation.substring( 0, indexOfFileSep );
            // TODO: worry about invalid characters in the DB name?
            containerName = mDBLocation.substring( indexOfFileSep + 1 );
        }

        // The db Context will check the org.basex.DBPATH property when it is created
        // and use it as the base path for finding all collections/containers
        // The path may be a relative path so we must convert it to absolute here.
        System.setProperty( "org.basex.DBPATH", new File( path ).getAbsolutePath() );
        mContext = new Context();

        // Set some default behaviors
        // TODO: experiment with these to see which gives us the best performance
        // Turn off all indexing
        mContext.options.set( MainOptions.ATTRINDEX, false );
        mContext.options.set( MainOptions.FTINDEX, false );
        mContext.options.set( MainOptions.UPDINDEX, false );

        // Strip whitespace for internal representation
        mContext.options.set( MainOptions.CHOP, true );
        // Write data to disk as data is added to the DB rather than keeping it all
        // in memory. This is useful for large documents such as from GCAM.
        mContext.options.set( MainOptions.ADDCACHE, true );
        // Use the internal BaseX XML parser which is faster than the Java default.
        mContext.options.set( MainOptions.INTPARSE, true );
        // Open the database in memory if requested.
        mContext.options.set( MainOptions.MAINMEM, aInMemoryDB );

        // Check if the database is "pinned" or open already in which case
        // ask the user to close it before procceeding.
        if( mContext.pinned( containerName ) ) {
            // Rings bell on most terminals
            System.out.print((char)7);
            System.out.println( "The database "+containerName+" appears to be open.");
            // If the timeout is less than 0 we will wait indefinately.
            if( aOpenDBWait < 0 ) {
                System.out.println( "Please close it and press return to continue.." );
                System.in.read();
            } else {
                //BufferedReader in = new BufferedReader( new InputStreamReader( System.in ) );
                final long startTime = System.currentTimeMillis();
                System.out.println( "Please close it and press return to continue (waiting "+aOpenDBWait+" seconds).." );
                while( ( System.currentTimeMillis() - startTime ) < aOpenDBWait * 1000
                       && System.in.available() == 0 )
                {
                    // busy wait
                }
            }
            System.out.println( "Attempting to write again." );
        }

        // The Check command will open the database if it already exists or
        // create a new one otherwise.
        new Check( containerName ).execute( mContext );
    }

    /**
     * Connect the PipedInputStream to the given PipedOutputStream to recieve the
     * XML data through.  Note that the data may be filtered before arriving here.
     * @param aOutputStream The PipedOutputStream to connect to.
     */
    public void setInputStream( PipedOutputStream aOutputStream ) throws IOException {
        // Only need to connect one end of the pipe and the other will
        // automatically be connected as well.
        aOutputStream.connect( mWriteToDBStream );
    }

    /**
     * Start the worker thread for adding the data as GCAM will start sending
     * it soon.
     */
    public void start() {
        // Start the add on a new thread which will add the XML as GCAM
        // produces it.
        mWorkerThread.start();
    }

    /**
     * Cancel writing the data to the DB presumably because there was
     * an error.
     */
    public void cancel() {
        // Set the flag that the thread should stop.
        mWorkerThread.interrupt();

        // Only need to stop the add command if it has started
        if( mAddCommand != null ) {
            // Signal the command that it should stop.
            mAddCommand.stop();
        }
    }

    /**
     * Notify that no more XML from GCAM will be sent.  This method
     * will wait until all of the data has been stored to the DB before
     * returning.
     */
    public void finish() {
        try {
            // This will block until the database is done storing
            // the data sent from GCAM.
            mWorkerThread.join();
        }
        catch( InterruptedException interruptError ) {
            interruptError.printStackTrace();
        }
    }

    /**
     * GCAM is done potentially adding any more data and any potential queries
     * have been run so we can go ahead and close the database.
     */
    public void close() {
        try {
            // close the database
            if( mContext != null ) {
                new Close().execute( mContext );
                mContext = null;
            }
        }
        catch( Exception error ) {
            error.printStackTrace();
        }
    }

    /**
     * Sets up the Add command to add the XML to the database as it becomes
     * available from GCAM.  The run function implements the Runnable interface
     * so that it may run on a new thread.  We must do this off of the main
     * thread to avoid deadlock.
     */
    public void run() {
        try {
            mAddCommand = new Add( mDocName );
            mAddCommand.setInput( mWriteToDBStream );
            mAddCommand.execute( mContext );
        }
        catch( Exception error ) {
            error.printStackTrace();
        }
        finally {
            try {
                mWriteToDBStream.close();
            }
            catch( IOException ioError ) {
                // ignore
            }
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
        if( mContext == null ) {
            // Failed to open the database, an error would have already been printed.
            return false;
        }

        boolean noError = true;

        // Add the document name to get the full location path
        String fullLocation = "collection()[matches(document-uri(.), '" + mDocName + "')]" + aLocation;

        // Construct the insert statement
        String insertStatement = "insert node (" + aData + ") after " + fullLocation;

        // Run the statement
        QueryProcessor queryProc = new QueryProcessor( insertStatement, mContext );
        try {
            // not expecting anything to be in the results
            queryProc.execute();
        }
        catch( Exception queryError ) {
            queryError.printStackTrace();
            noError = false;
        }
        finally {
            queryProc.close();
        }
        return noError;
    }
}

