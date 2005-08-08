/**
 * 
 */
package ModelInterface.ConfigurationEditor.src.actions;


import ModelInterface.ConfigurationEditor.src.guihelpers.XMLFileFilter;

import java.awt.event.ActionEvent;
import java.io.File;
import javax.swing.AbstractAction;
import org.w3c.dom.Document;

import ModelInterface.ConfigurationEditor.src.utils.DOMUtils;
import ModelInterface.ConfigurationEditor.src.utils.FileUtils;

import ModelInterface.ConfigurationEditor.src.configurationeditor.ConfigurationEditor;

/**
 * Class which implements the save and save as actions.
 * @author Josh Lurz
 * 
 */

public class SaveAction extends AbstractAction {
    /**
     * Identifier used for serializing.
     */
    private static final long serialVersionUID = 7682523793575123303L;
    /**
     * A reference to the top level editor from which this action is receiving
     * commands.
     */
    private transient final ConfigurationEditor mParentEditor;

    /**
     * Constructor which sets the name of the Action and stores the parent editor.
     * @param aParentEditor
     *            The top level editor.
     */
    public SaveAction(ConfigurationEditor aParentEditor) {
        super("Save"); //$NON-NLS-1$
        mParentEditor = aParentEditor;
    }

    /**
     * Method called when the save action is activated which queries the user 
     * for a filename to save the configuration document to if necessary and 
     * performs the save.
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(final ActionEvent aEvent) {
        if (aEvent.getActionCommand().equals("Save")) { //$NON-NLS-1$
        	// Get the document file from the editor.
        	final File confFile = FileUtils.getDocumentFile(mParentEditor.getDocument());
            // If there is not a file name, call the save as method.
            if (confFile == null) {
                doSaveAs();
            } else {
                doSave();
            }
        } else if (aEvent.getActionCommand().equals("SaveAs")) { //$NON-NLS-1$
            doSaveAs();
        } else {
        	// Should not get another action command here.
        	assert(false);
        }
    }
    /**
     * Function which performs the save once a filename has been set.
     * 
     */
    private void doSave() {
        // Save the document.
        final Document curr = mParentEditor.getDocument();
        assert(curr != null);
        
        // Serialize the document to the file.
        DOMUtils.serialize(curr, mParentEditor);
    }

    /**
     * Perform the save as action.
     */
    private void doSaveAs() {
    	final File currFile = FileUtils.selectFile(mParentEditor, new XMLFileFilter(), null, true);
    	if(currFile != null){
            // TODO: Overwrite warning.
        	// Set the file into the editor where doSave will find it.
        	FileUtils.setDocumentFile(mParentEditor.getDocument(), currFile);
            doSave();
        }
    }
}
