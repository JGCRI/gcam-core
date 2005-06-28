/**
 * 
 */
package actions;

import interfaceutils.Util;
import interfaceutils.XMLFileFilter;

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.JFileChooser;

import org.w3c.dom.Document;

import configurationeditor.ConfigurationEditor;

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
    private ConfigurationEditor mParentEditor = null;

    /**
     * Constructor which sets the name of the Action and stores the parent editor.
     * @param aParentEditor
     *            The top level editor.
     */
    public SaveAction(ConfigurationEditor aParentEditor) {
        super("Save"); //$NON-NLS-1$
        mParentEditor = aParentEditor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent aEvent) {
        if (aEvent.getActionCommand().equals("Save")) { //$NON-NLS-1$
        	// Get the document file from the editor.
        	File confFile = Util.getDocumentFile(mParentEditor.getDocument());
            // If there is not a file name, call the save as method.
            if (confFile == null) {
                doSaveAs();
            } else {
                doSave();
            }
        } else if (aEvent.getActionCommand().equals("SaveAs")) { //$NON-NLS-1$
            doSaveAs();
        }
    }
    /**
     * Function which performs the save once a filename has been set.
     * 
     */
    private void doSave() {
        Logger.global.entering("doSave", "Entering"); //$NON-NLS-1$ //$NON-NLS-2$
        // Save the document.
        Document curr = mParentEditor.getDocument();
        assert(curr != null);
        
        // Serialize the document to the file.
        Util.serializeDocument(curr, mParentEditor);
    }

    /**
     * Perform the save as action.
     */
    private void doSaveAs() {
        Logger.global.entering("DoSaveAs", "Entering"); //$NON-NLS-1$ //$NON-NLS-2$
        JFileChooser chooser = new JFileChooser();
        chooser.setFileFilter(new XMLFileFilter());
        int rv = chooser.showSaveDialog(mParentEditor);
        if (rv == JFileChooser.APPROVE_OPTION) {
        	File currFile = chooser.getSelectedFile();
        	// Set the file into the editor where doSave will find it.
        	Util.setDocumentFile(mParentEditor.getDocument(), currFile);
            doSave();
        }
    }
}
