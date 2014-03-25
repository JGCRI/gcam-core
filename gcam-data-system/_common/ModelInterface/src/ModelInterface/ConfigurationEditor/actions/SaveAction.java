/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2012 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
*/
package ModelInterface.ConfigurationEditor.actions;


import ModelInterface.ConfigurationEditor.guihelpers.XMLFileFilter;

import java.awt.event.ActionEvent;
import java.io.File;
import javax.swing.AbstractAction;
import org.w3c.dom.Document;

import ModelInterface.ConfigurationEditor.utils.DOMUtils;
import ModelInterface.ConfigurationEditor.utils.FileUtils;

import ModelInterface.ConfigurationEditor.configurationeditor.ConfigurationEditor;

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
