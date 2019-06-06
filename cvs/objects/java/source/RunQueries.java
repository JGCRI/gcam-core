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
import java.util.List;
import java.util.ArrayList;
import org.basex.core.Context;

import ModelInterface.InterfaceMain;
import ModelInterface.ModelGUI2.xmldb.XMLDB;

/**
 * A helper class to give a simple interface to launching the ModelInterface
 * in batch mode to run queries.  We must utilize the already open database's
 * context to be able to make these queries especially if the database is in
 * memory only.  Note that GCAM will wait until after the scenario has cleaned
 * up before launching these so we can be sure all data including cost information
 * has been added and memory has been freed.
 * @author Pralit Patel
 */
public class RunQueries implements Runnable {
    /**
     * The thread on which filtering will take place.
     * Note we could probably get away with just running this on the main
     * thread as the ModelInterface will run each query on worker threads
     * anyway.
     */
    private final Thread mWorkerThread = new Thread( this );

    /**
     * The batch file to run in the ModelInterface.
     * This batch file will have all of the necessary information for running
     * the queries.  The DB location fields will be ignored however since we
     * will just be connecting to the already opened DB.
     */
    private final String mBatchFile;
    private final String mLogFile;

    /**
     * Constructor which simply stores the batch file location. We will wait until
     * start to load the ModelInterface and begin running.
     * @param aBatchFile The ModelInterface batch file to run.
     * @param aLogFile Optional file into which to direct ModelInterface's stdout.
     */
    public RunQueries( final String aBatchFile, final String aLogFile ) {
        mBatchFile = aBatchFile;
        mLogFile = aLogFile;
    }

    /**
     * Start the worker thread to run the queries.
     * Note we will open the XMLDB before starting the batch processing to ensure
     * the ModelInterface uses the given DB location and context.
     * @param aDBLocation The location of the database to query.
     * @param aContext A database context that has already been opened.
     */
    public void start( final String aDBLocation, Context aContext ) {
        if( aContext == null ) {
            System.err.println( "ERROR: Database context is null and must have already closed.  Could not run queries." );
            return;
        }
        try {
            XMLDB.openDatabase( aContext );
            // process the queries on the worker thread
            mWorkerThread.start();
        }
        catch( Exception error ) {
            error.printStackTrace();
        }
    }

    /**
     * Wait for the queries to finish running.
     */
    public void finish() {
        try {
            // This will block until the ModelInterface is done
            mWorkerThread.join();
            XMLDB.closeDatabase();
        }
        catch( InterruptedException interruptError ) {
            interruptError.printStackTrace();
        }
    }

    /**
     * The run function for the worker thread that will call the ModelInterface in
     * batch mode.
     */
    public void run() {
        List<String> args = new ArrayList<String>();
        args.add("-b");
        args.add(mBatchFile);
        if ( !mLogFile.isEmpty() ) {
            args.add("-l");
            args.add(mLogFile);
        }
        // Run the ModelInterface in batch mode
        System.out.println("Running batch file: " + mBatchFile);
        InterfaceMain.main( args.toArray(new String[0]) );
    }
}

