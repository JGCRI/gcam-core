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

import java.net.URLClassLoader;
import java.net.URL;
import java.net.URI;
import java.io.File;
import java.io.FilenameFilter;
import java.util.List;
import java.util.ArrayList;

/**
 * A custom class loader which simply expands any wildcard jar specifications
 * in the java.class.path in accordance with the Java spec.
 * Such a class loader is necessary since while Java should do this automatically
 * for us it turns out that it is not done when the JVM is launched via JNI.
 * This class loader will simply do the wild card expansion and rely on the
 * URLClassLoader to do the real work.
 *
 * @author Pralit Patel
 */
public class WildcardExpandingClassLoader extends URLClassLoader {
    /**
     * Constructor which will expand any wildcard jar specifications then call
     * the constructor or the base URLClassLoader with this updated list of jars.
     * @param aParentClassLoader The system class loader which we want to replace
     *                           where aParentClassLoader's unexpanded URLs are now
     *                           expanded.
     */
    public WildcardExpandingClassLoader( ClassLoader aParentClassLoader ) {
        // cut the parent class loader out of the loop by copying it's search path
        // and expanding the wildcard definitions then pointing directly to it's
        // parent class loader
        super( expandWildcardClasspath( ((URLClassLoader)aParentClassLoader).getURLs() ), aParentClassLoader.getParent() );
    }

    /**
     * A helper function to expand wildcard jar definitions since the JNI api can not
     * be relied upon to do so for us.
     * @param aOriginalURLs The original URLs which may not have had wildcards expanded.
     * @return An array of the URLs from aOriginalURLs except the unexpaned wildcards
     *         have been expaneded.
     */
    private static URL[] expandWildcardClasspath( URL[] aOriginalURLs ) {
        List<URL> ret = new ArrayList<URL>();
        for( URL currURL : aOriginalURLs ) {
            if( currURL.getFile().endsWith( "*" ) ) {
                // This URL needs to be expanded
                try {
                    File currFile = new File( currURL.getFile() );
                    // Search the parent path for any files that end in .jar
                    File[] expandedJars = currFile.getParentFile().listFiles(
                        new FilenameFilter() {
                            public boolean accept( File aDir, String aName ) {
                                return aName.endsWith( ".jar" );
                            }
                        } );
                    // Add the additional jars to the new search path
                    if( expandedJars != null ) {
                        for( File currJar : expandedJars ) {
                            ret.add( currJar.toURI().toURL() );
                        }
                    } else {
                        // could not expand due to some error, we can try to
                        // proceed with out these jars
                        System.out.println( "WARNING: could not expand classpath at: "+currFile.toString() );
                    }
                } catch( Exception e ) {
                    // could not expand due to some error, we can try to
                    // proceed with out these jars
                    e.printStackTrace();
                }
            }
            else {
                // Just use this unmodified
                ret.add( currURL );
            }
        }
        return ret.toArray( new URL[ 0 ] );
    }
}
