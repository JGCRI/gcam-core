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

import java.io.FileInputStream;
import java.io.PipedOutputStream;
import java.io.PipedInputStream;
import java.io.IOException;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;

/**
 * A class that can filter/transform XML results as they are streamed to this
 * class and pass them on as a stream to an XML database to be stored.
 * @author Pralit Patel
 */
public class FilterOutput implements Runnable {
    /**
     * The thread on which filtering will take place.
     */
    private final Thread mWorkerThread = new Thread( this );

    /**
     * The Transformer implementation which processes the XML filtering 
     * it as it is streamed in and outputs the filtered results back out.
     */
    private final Transformer mTransformer;

    /**
     * The stream that will recieve data from GCAM for filtering.
     */
    private final PipedInputStream mSendThroughFilterStream = new PipedInputStream( XMLDBDriver.BUFFER_SIZE );

    /**
     * The stream that will transfer the filtered XML on to write it to the DB.
     */
    private final PipedOutputStream mSendToDBStream = new PipedOutputStream();

    /**
     * Constructor which will create the transformer and read the filter script.
     * @param aFilterScript The script which contains the rules to filter the XML data.
     */
    public FilterOutput( final String aFilterScript ) throws Exception {
        StreamSource filterScriptSource = new StreamSource( new FileInputStream( aFilterScript ) );
        // NOTE: we are loading a custom transformer here called Joost which implements STX = Streaming Transformations for XML
        // http://joost.sourceforge.net
        // This allows us to filter XML as it is generated instead of having to collect
        // and parse to DOM all of the GCAM data before filtering as is the case with XSLT.
        // If we watned to switch back to XSLT the only change necessary is to load the
        // default Transformer implemntation which can be accomplished by calling the
        // factors with no arguments: TransformerFactory.newInstance()
        mTransformer = TransformerFactory.newInstance( "net.sf.joost.trax.TransformerFactoryImpl",
                ClassLoader.getSystemClassLoader() ).newTransformer( filterScriptSource );
    }

    /**
     * Connect the PipedInputStream to the given PipedOutputStream to recieve the
     * XML data from GCAM.
     * @param aOutputStream The PipedOutputStream to connect to.
     */
    public void setInputStream( PipedOutputStream aOutputStream ) throws IOException {
        // Only need to connect one end of the pipe and the other will
        // automatically be connected as well.
        aOutputStream.connect( mSendThroughFilterStream );
    }

    /**
     * Get the stream of output that will come out of the filter to send into the
     * database.
     * @return The PipedOutputStream which can be used the get the filtered XML.
     */
    public PipedOutputStream getOutputStream() {
        return mSendToDBStream;
    }

    /**
     * Start the worker thread to filter the data as GCAM will start sending
     * it soon.
     */
    public void start() {
        // We need to consume the output from GCAM on a new thread to avoid
        // deadlock on the piped streams so the filter will work on it's own
        // thread.
        mWorkerThread.start();
    }

    /**
     * Cancel writing the data to the DB presumably because there was
     * an error.
     */
    public void cancel() {
        // Set the flag that the thread should stop.
        mWorkerThread.interrupt();

        try {
            // close the output stream too
            mSendToDBStream.close();
        }
        catch( IOException error ) {
            // ignore
        }
    }

    /**
     * Notify that no more XML from GCAM will be sent.  This method
     * will wait until all of the data has been filtered and send on
     * to the DB before returning.
     */
    public void finish() {
        try {
            // This will block until the Transformer is done
            mWorkerThread.join();
        }
        catch( InterruptedException interruptError ) {
            interruptError.printStackTrace();
        }
    }

    /**
     * Runs the transform process to filter the XML as it is produced by GCAM
     * and then sent on to the DB. The run function implements the Runnable interface
     * so that it may run on a new thread.  We must do this off of the main
     * thread to avoid deadlock.
     */
    public void run() {
        try {
            // wrap the piped streams into an interface usable by the transformer
            StreamSource readFromGCAMSource = new StreamSource( mSendThroughFilterStream );
            StreamResult sendToDBResult = new StreamResult( mSendToDBStream );
            // do the transformation/filter
            mTransformer.transform( readFromGCAMSource, sendToDBResult );
        }
        catch( Exception error ) {
            error.printStackTrace();
        }
        finally {
            try {
                mSendToDBStream.close();
            }
            catch( IOException ioError ) {
                // ignore
            }
        }
    }
}

