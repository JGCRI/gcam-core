/**
 * 
 */
package configurationeditor;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.SwingUtilities;
import javax.swing.UIManager;

/**
 * Main class which instantiates and displays the ConfigurationEditor class.
 * @author Josh Lurz
 * 
 */
public class Main {

    /**
     * @param args
     */
    public static void main(String[] args) {
        // Setup the main application.
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
            Logger.global.log(Level.WARNING,
                    Messages.getString("Main.0")); //$NON-NLS-1$
        }
        // Create the configuration editor main window and show it.
        final ConfigurationEditor mainWindow = new ConfigurationEditor();
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                mainWindow.pack();
                mainWindow.setVisible(true);
            }
        });
        mainWindow.askForInitialAction();
    }

}
