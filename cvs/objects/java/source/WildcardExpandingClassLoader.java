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
import java.net.URLDecoder;
import java.net.MalformedURLException;
import java.io.File;
import java.io.FilenameFilter;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * A custom class loader which simply expands any wildcard jar specifications
 * in the java.class.path in accordance with the Java spec.
 * Such a class loader is necessary since while Java should do this automatically
 * for us it turns out that it is not done when the JVM is launched via JNI.
 * This class loader will simply do the wild card expansion and rely on the
 * URLClassLoader to do the real work.  Note since Java 9 the URLClassLoader
 * is no longer the default so we will also manually lookup the classpath as well.
 *
 * @author Pralit Patel
 */
public class WildcardExpandingClassLoader extends URLClassLoader {
    /**
     * A pattern that looks for BaseX library names.
     */
    final static String BASEX_LIB_MATCH = "^.*[Bb][Aa][Ss][Ee][Xx].*$";
    
    /**
     * A pattern that looks for a "prefered" BaseX version if we find
     * duplicate versions of the library.
     */
    final static String PREFERED_BASEX_VER = "^.*[Bb][Aa][Ss][Ee][Xx]-9.5.0.*$";

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
        super( expandWildcardClasspath(), aParentClassLoader.getParent() );
    }

    /**
     * A helper function to expand wildcard jar definitions since the JNI api can not
     * be relied upon to do so for us.
     * @param aOriginalURLs The original URLs which may not have had wildcards expanded.
     * @return An array of the URLs from aOriginalURLs except the unexpaned wildcards
     *         have been expaneded.
     */
    private static URL[] expandWildcardClasspath() {
        List<URL> ret = new ArrayList<URL>();
        int numBaseXJars = 0;
        String classpath = System.getProperty("java.class.path");
        String[] classpathEntries = classpath.split(System.getProperty("path.separator"));
        for( String currCP : classpathEntries ) {
            File classpathFile = new File(currCP);
            URI uri = classpathFile.toURI();
            URL currURL = null;
            try {
                currURL = uri.toURL();
            } catch (MalformedURLException e) {
                System.out.println("Ignoring classpath entry: " + currCP);
            }
            if( currCP.endsWith( "*" ) ) {
                // This URL needs to be expanded
                try {
                    File currFile = new File( URLDecoder.decode( currURL.getFile(), "UTF-8" ) );
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
                            if( currJar.getName().matches(BASEX_LIB_MATCH) ) {
                                ++numBaseXJars;
                            }
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
                if( currURL.getFile().matches(BASEX_LIB_MATCH) ) {
                    ++numBaseXJars;
                }
            }
        }
        // we've had trouble finding multiple jars of the BaseX of different versions
        // so if we find more than we will accept the one that matches the "prefered" version
        // which is hard coded to the version used when this workspace was created
        if( numBaseXJars > 1 ) {
            for( Iterator<URL> it = ret.iterator(); it.hasNext(); ) {
                URL currURL = it.next();
                if( currURL.getFile().matches(BASEX_LIB_MATCH) && !currURL.getFile().matches(PREFERED_BASEX_VER) ) {
                    it.remove();
                    --numBaseXJars;
                }
            }
        }
        if( numBaseXJars == 0 ) {
            System.out.println( "WARNING: did not recongnize any BaseX jars in classpath.  This may indicate missing jars or duplicate version mismatch.");
        }
        return ret.toArray( new URL[ 0 ] );
    }
}
