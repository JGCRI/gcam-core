/**
 * 
 */
package ModelInterface.ConfigurationEditor.actions;


import java.awt.Container;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.Properties;

import javax.swing.AbstractAction;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;

import org.w3c.dom.Document;

import ModelInterface.ConfigurationEditor.utils.Messages;
import ModelInterface.ConfigurationEditor.utils.FileUtils;

import ModelInterface.ConfigurationEditor.configurationeditor.ConfigurationEditor;
import ModelInterface.ConfigurationEditor.configurationeditor.PropertiesInfo;

/**
 * This class defines the action which occurs when the user attempts to create a
 * new configuration file.
 * @author Josh Lurz
 * 
 */
public class NewAction extends AbstractAction {
    /**
     * Identifier used for serializing.
     */
    private static final long serialVersionUID = 4782184657425041505L;
    
    /**
     * Constructor which sets the name of the Action.
     */
    public NewAction() {
        super("New"); //$NON-NLS-1$
    }

    /**
     * Create a new document. A new document is created by loading the 
     * configuration template file and setting the current document path to null.
     * Create a completely new document results in too many uninitialized preferences.
     * The path to the configuration template is stored in the properties file.
     * 
     * @param aEvent
     *            The event which triggered the action.
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(final ActionEvent aEvent) {
        // Find the root window.
        // TODO: Improve this to not use instance of.
        ConfigurationEditor parentEditor = null;
        if (aEvent.getSource() instanceof ConfigurationEditor) {
            parentEditor = (ConfigurationEditor) aEvent.getSource();
        } else {
            Container parentContainer = ((JComponent) aEvent.getSource())
                    .getParent();
            if (parentContainer instanceof JPopupMenu) {
                parentEditor = (ConfigurationEditor) ((JComponent) ((JPopupMenu) parentContainer)
                        .getInvoker()).getTopLevelAncestor();
            } else if (parentContainer instanceof JToolBar) {
                parentEditor = (ConfigurationEditor) ((JToolBar) parentContainer)
                        .getTopLevelAncestor();
            } else {
                // Unknown type.
                assert (false);
            }
        }
    	
        // Check if the file should be saved before creating a new one.
        if (!FileUtils.askForSave(parentEditor)) {
            // The user does not want to continue.
            return;
        }
        
        // Get the path to the configuration template from the preferences.
        final Properties props = FileUtils.getInitializedProperties(parentEditor);
        final String currentFile = props.getProperty(PropertiesInfo.CONF_TMPL);
        
        // Check if the configuration template path has been initialized.
        if( currentFile == null) {
            final String errorMessage = Messages.getString("NewAction.1"); //$NON-NLS-1$
            final String errorTitle =  Messages.getString("NewAction.2"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(parentEditor, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
            return;
        }
        final File newConfFile = new File(currentFile);
        
        // Attempt to parse the document.
        final Document loadedDocument = FileUtils.loadDocument(parentEditor, newConfFile, ConfigurationEditor.ROOT_ELEMENT_NAME);
        // Clear the loaded document's saved file name because it would be the template
        // file's location.
        FileUtils.setDocumentFile(loadedDocument, null);
        
        // Set the document into the model.
        parentEditor.setDocument(loadedDocument);
    }

}
