package ModelInterface.ConfigurationEditor.src.utils;

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
    private static final String BUNDLE_NAME = "ModelInterface.ConfigurationEditor.src.utils.messages"; //$NON-NLS-1$
    
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
