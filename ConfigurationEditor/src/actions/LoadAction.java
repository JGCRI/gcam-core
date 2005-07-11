/**
 * 
 */
package actions;


import java.awt.event.ActionEvent;
import java.io.File;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.xml.parsers.DocumentBuilder;

import org.w3c.dom.Document;

import utils.DOMUtils;
import utils.Messages;
import utils.FileUtils;
import utils.XMLFileFilter;

import configurationeditor.ConfigurationEditor;

/**
 * This action creates a file chooser so the user can select a new document
 * and loads it into the XML tree. Errors are handled by displaying dialog
 * boxes to the user.
 * @author Josh Lurz
 */
public class LoadAction extends AbstractAction {
    /**
     * Identifier used for serializing.
     */
    private static final long serialVersionUID = -1377726687700388463L;

    /**
     * The name of the attribute which stores the most recently opened file.
     */
    private final String mRecentFileProperty = "most-recent-file"; //$NON-NLS-1$
    
    /**
     * A reference to the top level editor from which this action is receiving
     * commands.
     */
    private ConfigurationEditor mParentEditor = null;

    /**
     * Constructor which sets the name of the Action and stores the parent editor.
     * @param aParentEditor
     *            The top level editor.
     */
    public LoadAction(ConfigurationEditor aParentEditor) {
        super("Load"); //$NON-NLS-1$
        mParentEditor = aParentEditor;
    }

    /**
     * Method called when an action is performed which triggers
     * the load action.
     * 
     * The method checks first if the file needs to be saved and then
     * queries the user for a new file to load. The file is loaded
     * and set as the document of the ConfigurationEditor.
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent aEvent) {
        // Check if the file should be saved before loading a new one.
        if (!FileUtils.askForSave(mParentEditor)) {
            // The user does not want to continue.
            return;
        }
        
        // Find the most recent file the user opened from the properties.
        Properties props = FileUtils.getInitializedProperties(mParentEditor);
        String recentFile = props.getProperty(mRecentFileProperty);
        
        // Ask the user for a file to load.
        JFileChooser chooser = new JFileChooser(recentFile);
        chooser.setFileFilter(new XMLFileFilter());

        int rv = chooser.showOpenDialog(mParentEditor);
        File currentFile = null;
        if (rv == JFileChooser.APPROVE_OPTION) {
            currentFile = chooser.getSelectedFile();
        } else {
            // Leave the existing document.
            return;
        }
        // Create the document builder.
        DocumentBuilder docBuilder = DOMUtils.getDocumentBuilder(mParentEditor);
        
        // Return early if we couldn't create a document builder. An error
        // message will have been printed by the FileUtils function.
        if(docBuilder == null){
        	return;
        }
        
        // Attempt to parse the document.
        Document loadedDocument = null;
        try {
            loadedDocument = docBuilder.parse(currentFile);
        } catch (Exception e) {
           // Unexpected error parsing the document.
            Logger.global.log(Level.SEVERE, e.getStackTrace().toString());
            String errorMessage = Messages.getString("LoadAction.1") //$NON-NLS-1$
                    + e.getMessage() + "."; //$NON-NLS-1$
            String errorTitle = Messages.getString("LoadAction.3"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(mParentEditor, errorMessage,
                    errorTitle, JOptionPane.ERROR_MESSAGE);
            return;
        }
        
        // Check if the root element is a configuration element.
        // TODO: Unhardcode configuration.
        if(!loadedDocument.getDocumentElement().getNodeName().equals("Configuration")) { //$NON-NLS-1$
            String errorTitle = Messages.getString("LoadAction.5"); //$NON-NLS-1$
            String errorMessage = Messages.getString("LoadAction.6"); //$NON-NLS-1$
            Logger.global.log(Level.SEVERE, errorMessage);
            JOptionPane.showMessageDialog(mParentEditor, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE);
            return;
        }
        // Set the document into the model.
        mParentEditor.setDocument(loadedDocument);
        // TODO: The parent model doesn't update immediately.
        
        // Set the current file into the configuration editor.
        FileUtils.setDocumentFile(loadedDocument, currentFile);
        
        // Save the file as the most recent document.
        props.setProperty(mRecentFileProperty, currentFile.getAbsolutePath());
        FileUtils.saveProperties(props);
    }
}
