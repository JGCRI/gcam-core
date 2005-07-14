/**
 * 
 */
package actions;


import guihelpers.XMLFileFilter;

import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.AbstractAction;
import javax.swing.JComponent;

import org.w3c.dom.Document;

import utils.FileUtils;
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
     * Constructor which sets the name of the Action.
     */
    public LoadAction() {
        super("Load"); //$NON-NLS-1$
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
    public void actionPerformed(final ActionEvent aEvent) {
        // Find the root window. 
    	// TODO: Improve this to not use instance of.
    	ConfigurationEditor parentEditor = null;
    	if(aEvent.getSource() instanceof ConfigurationEditor){
    		parentEditor = (ConfigurationEditor)aEvent.getSource();
    	}
    	else {
    		parentEditor = (ConfigurationEditor)((JComponent)aEvent.getSource()).getTopLevelAncestor();
    	}
    	
    	// Check if the file should be saved before loading a new one.
        if (!FileUtils.askForSave(parentEditor)) {
            // The user does not want to continue.
            return;
        }
        
        // Ask the user to select the file.
        final File currentFile = FileUtils.selectFile(parentEditor, new XMLFileFilter());
        
        // Check if the user cancelled the action.
        if(currentFile == null){
        	return;
        }
        
        // Load the document.
        final Document loadedDocument = FileUtils.loadDocument(parentEditor, currentFile, ConfigurationEditor.ROOT_ELEMENT_NAME);

        // Set the document into the model.
        parentEditor.setDocument(loadedDocument);
    }
}
