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
import java.net.URI;

import org.basex.core.Context;
import org.basex.core.cmd.Check;
import org.basex.core.cmd.Add;
import org.basex.core.StaticOptions;
import org.basex.api.client.ClientSession;

/**
 * A helper class which gives GCAM a simple interface for adding or appending
 * XML to a remote BaseX database.  The XMLDBDriver will set up the interactions
 * and notify when to close as results may be filtered before reaching the database
 * and run queries after GCAM has cleaned up the Scenario.
 * @author Pralit Patel
 */
public class WriteRemoteBaseXDB extends WriteLocalBaseXDB {

    /**
     * The Session connected to the remote server.
     */
    ClientSession mSession;
    
    /**
     * Constructor which will open the DB and get ready to receive XML to put
     * into the DB.
     * @param aDBLocation The URI location of the database to open of the format
     *                    {@code basex://<username>:<password>@<host>:<port>/<DB Name>}
     * @param aDocName A unique document name to use to store the XML in the DB.
     * @param aInMemoryDB This option is ignored for remote DB as it will be controlled
     *                    entirely by the server's configuration.
     * @param aOpenDBWait This option is ignored for remote DB as it will be controlled
     *                    entirely by the server's configuration.
     */
    public WriteRemoteBaseXDB( final String aURIStr, final String aDocName,
                              final boolean aInMemoryDB, final int aOpenDBWait ) throws Exception
    {
        super(aURIStr, aDocName, false, 0);
    }

    /**
     * Opens a Client Session to the remote database.  We will "Check" the database which
     * will open it if it already exists or create a new one otherwise.
     * @param aInMemoryDB This option is ignored for remote DB as it will be controlled
     *                    entirely by the server's configuration.
     * @param aOpenDBWait This option is ignored for remote DB as it will be controlled
     *                    entirely by the server's configuration.
     */
    protected void openDB( final boolean aInMemoryDB, final int aOpenDBWait ) throws Exception {
        // we are expecting the DB location to be a URI of format:
        // basex://<username>:<password>@<host>:<port>/<DB Name>
        URI xmldbURI = new URI(getDBLocation());

        String auth = xmldbURI.getUserInfo();
        String[] authSplit = auth.split(":");
        String mUserName = authSplit[0];
        String mPassword = authSplit[1];
        String mHostName = xmldbURI.getHost();
        int mPortNumber = xmldbURI.getPort();
        String dbLocation = xmldbURI.getPath();

        // establish the ClientSession
        StaticOptions options = new StaticOptions(true);
        options.set(StaticOptions.HOST, mHostName);
        options.set(StaticOptions.PORT, mPortNumber);
        options.set(StaticOptions.USER, mUserName);
        options.set(StaticOptions.PASSWORD, mPassword);
        mContext = new Context(options);
        mSession = new ClientSession(mHostName, mPortNumber, mUserName, mPassword);


        // "Check" that the DB is open in the remote session
        mSession.execute(new Check( dbLocation ));
    }

    public void run() {
        try {
            // start "adding" XML data
            mSession.add(mDocName, mWriteToDBStream);
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
    public void close() {
        try {
            // close the database
            if( mContext != null ) {
                mSession.close();
                mSession = null;
                mContext = null;
            }
        }
        catch( Exception error ) {
            error.printStackTrace();
        }
    }
}

