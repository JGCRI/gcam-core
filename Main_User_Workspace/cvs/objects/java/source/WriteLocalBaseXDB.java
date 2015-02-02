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
 * XML to a local BaseX database.  GCAM can create an instance of this class
 * giving the database location and the name of the document to add/update.  It
 * can then receive the data and stream it into the database by reading a buffer
 * via the receiveDataFromGCAM method.  When all data has been sent the finish
 * method is called which will wait until the BaseX has finished adding all of
 * the data.  A user can then also call the appendData method giving additional
 * XML to append to an existing document.
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
     * The stream that will read XML from GCAM to write to the DB.
     */
    private final PipedOutputStream mReadFromGCAMStream = new PipedOutputStream();

    /**
     * The stream that will transfer the XML read from GCAM and write it to the DB.
     * We use a buffer size of 1 MB which seems large enough to keep the DB continuously
     * fed with data to write.
     */
    private final PipedInputStream mWriteToDBStream = new PipedInputStream( 1024 * 1024 );

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
     * @param aAppendOnly If the database is being opened only to append to an
     *                    existing document and not adding a new one.
     */
    public WriteLocalBaseXDB( final String aDBLocation, final String aDocName, final boolean aAppendOnly ) {
        // Set the DB location and doc name.
        mDBLocation = aDBLocation;
        mDocName = aDocName;
        try {
            // Open the database
            openDB();

            if( !aAppendOnly ) {
                // Only need to connect one end of the pipe and the other will
                // automatically be connected as well.
                mReadFromGCAMStream.connect( mWriteToDBStream );

                // Start the add on a new thread which will add the XML as GCAM
                // produces it.
                mWorkerThread.start();
            }
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
            mContext = null;
        }
    }

    /**
     * Opens the database.  We will "Check" the database which will open it
     * if it already exists or create a new one otherwise.
     */
    private void openDB() throws Exception {
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

        // The Check command will open the database if it already exists or
        // create a new one otherwise.
        new Check( containerName ).execute( mContext );
    }

    /**
     * Cancel writing the data to the DB presumably because there was
     * an error.
     */
    private void cancelWrite() {
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
            mReadFromGCAMStream.close();
        }
        catch( IOException ioError ) {
            // ignore
        }

        try {
            // This will block until the database is done storing
            // the data sent from GCAM.
            mWorkerThread.join();
        }
        catch( InterruptedException interruptError ) {
            interruptError.printStackTrace();
        }

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
            // this call will close the input stream too
            try {
                mReadFromGCAMStream.close();
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
            // Failed to open the database.
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
            try {
                new Close().execute( mContext );
            }
            catch( Exception closeError ) {
                // ignore
            }
            mContext = null;
        }
        return noError;
    }
}

