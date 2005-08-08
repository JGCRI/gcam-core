/**
 * 
 */
package ModelInterface.ConfigurationEditor.src.configurationeditor;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import ModelInterface.ConfigurationEditor.src.utils.Messages;

/**
 * Main class which instantiates and displays the ConfigurationEditor class.
 * @author Josh Lurz
 * 
 */
public class Main {

    /**
     * Prevent the main class from being instantiated as it 
     * only contains a static method.
     */
    private Main() {
        super();
    }
    
    /**
     * @param aArgs Arguments to the main method,
     */
    public static void main(final String[] aArgs) {

        // Setup the main application.
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
            // Failed to set the look and feel. Allow the user to continue
            // the windows just won't look right.
            Logger.global.log(Level.WARNING,
                    Messages.getString("Main.0")); //$NON-NLS-1$
        }
        SwingUtilities.invokeLater(new Runnable() {
            synchronized public void run() {
                // Create the configuration editor main window and show it.
                final ConfigurationEditor mainWindow = new ConfigurationEditor();
                mainWindow.pack();
                mainWindow.setVisible(true);
            }
        });
    }

}
