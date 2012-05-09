/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
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
*/
package ModelInterface.ConfigurationEditor.utils;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 *  This class allows the user to fetch a read-in string from a
 *  resource bundle based on a string key. This class is auto-generated
 *  by Eclipse. Do not modify this class.
 *  
 * @author Josh Lurz
 */
public class Messages {
    /**
     * The name of the resource bundle file.
     */
    private static final String BUNDLE_NAME = "ModelInterface.ConfigurationEditor.utils.messages"; //$NON-NLS-1$
    
    /**
     * The resource bundle containing all strings.
     */
    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
            .getBundle(BUNDLE_NAME);

    /**
     * Default empty constructor.
     *
     */
    private Messages() {
        super();
    }

    /**
     * Get a string from the resource bundle.
     * @param aKey Key for which to fetch the string.
     * @return The resource associated with the string.
     */
    public static String getString(final String aKey) {
        try {
            return RESOURCE_BUNDLE.getString(aKey);
        } catch (final MissingResourceException aException) {
            return '!' + aKey + '!';
        }
    }
}
