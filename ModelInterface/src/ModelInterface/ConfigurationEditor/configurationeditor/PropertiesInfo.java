package ModelInterface.ConfigurationEditor.configurationeditor;

/**
 * 
 * @author Josh Lurz
 *
 * Class which contains static information about the location of the 
 * property file and specific properties.
 */
public class PropertiesInfo {
    /**
     * Constructor
     */
    PropertiesInfo(){
        super();
        // Empty constructor
    }
    /**
     * The name of the properties file.
     */
    public static final String PROPERTY_FILE = "configuration_editor.properties"; //$NON-NLS-1$
    
    /**
     * The name of the executable path property.
     */
    public static final String EXE_PATH = "executable-path"; //$NON-NLS-1$
    
    /**
     * The name of the attribute which stores the configuration template path.
     */
    public static final String CONF_TMPL = "template-path"; //$NON-NLS-1$
    
    /**
     * The name of the property which stores the location of the log configuration file.
     */
    public static final String LOG_CONF = "log-conf-path"; //$NON-NLS-1$
    
    /**
     * The name of the attribute which stores the most recently opened file.
     */
    static public final String RECENT_FILE = "most-recent-file"; //$NON-NLS-1$
}
