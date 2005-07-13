/**
 * 
 */
package actions;


import java.awt.event.ActionEvent;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;
import javax.xml.parsers.DocumentBuilder;

import org.w3c.dom.Document;

import utils.DOMUtils;
import utils.Messages;
import utils.FileUtils;

import configurationeditor.ConfigurationEditor;
import configurationeditor.PropertiesInfo;

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
     * A reference to the top level editor from which this action is receiving
     * commands.
     */
    private final transient ConfigurationEditor mParentEditor;
    
    /**
     * Constructor which sets the name of the Action and stores the parent editor.
     * @param aParentEditor
     *            The top level editor.
     */
    public NewAction(ConfigurationEditor aParentEditor) {
        super("New"); //$NON-NLS-1$
        mParentEditor = aParentEditor;
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
        // Check if the file should be saved before creating a new one.
        if (!FileUtils.askForSave(mParentEditor)) {
            // The user does not want to continue.
            return;
        }
        // Create the document builder.
        final DocumentBuilder docBuilder = DOMUtils.getDocumentBuilder(mParentEditor);
        
        // Return early if we couldn't create a document builder. An error
        // message will have been printed by the FileUtils function.
        if(docBuilder == null){
        	return;
        }
        
        // Get the path to the configuration template from the preferences.
        final Properties props = FileUtils.getInitializedProperties(mParentEditor);
        final String currentFile = props.getProperty(PropertiesInfo.CONF_TMPL);
        
        // Check if the configuration template path has been initialized.
        if( currentFile == null) {
            final String errorMessage = Messages.getString("NewAction.1"); //$NON-NLS-1$
            final String errorTitle =  Messages.getString("NewAction.2"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(mParentEditor, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
            return;
        }
        
        // Attempt to parse the document.
        Document loadedDocument = null;
        try {
            loadedDocument = docBuilder.parse(currentFile);
        } catch (Exception e) {
           // Unexpected error parsing the document.
            Logger.global.log(Level.SEVERE, e.getStackTrace().toString());
            final String errorMessage = Messages.getString("NewAction.3") //$NON-NLS-1$
                    + e.getMessage() + "."; //$NON-NLS-1$
            final String errorTitle = Messages.getString("NewAction.5"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(mParentEditor, errorMessage,
                    errorTitle, JOptionPane.ERROR_MESSAGE);
            return;
        }
        
        // Check if this is a valid configuration document.
        // This is admittedly not a complete check which 
        // would require schema validation.
        if(!loadedDocument.getDocumentElement().getNodeName().equals(ConfigurationEditor.ROOT_ELEMENT_NAME)) {
            // This isn't a configuration document.
            final String errorMessage = "Configuration template file is not valid.";
            final String errorTitle = "Invalid Document";
            Logger.global.log(Level.WARNING, errorMessage);
            JOptionPane.showMessageDialog(mParentEditor, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE);
            return;
        }
        // Set the document into the model.
        mParentEditor.setDocument(loadedDocument);
        
        // Set the file containing the document to blank.
        FileUtils.setDocumentFile(loadedDocument, null);

        // Put up a message telling the user that a new file was created,
        // otherwise there is no feedback for this action.
        Logger.global.log(Level.INFO, Messages.getString("NewAction.6")); //$NON-NLS-1$
        final String message = Messages.getString("NewAction.7"); //$NON-NLS-1$
        final String messageTitle = Messages.getString("NewAction.8"); //$NON-NLS-1$
        JOptionPane.showMessageDialog(mParentEditor, message, messageTitle,
                JOptionPane.INFORMATION_MESSAGE);
    }

}
