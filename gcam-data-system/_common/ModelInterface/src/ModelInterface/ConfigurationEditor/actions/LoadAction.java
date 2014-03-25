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
import javax.swing.JComponent;

import org.w3c.dom.Document;

import ModelInterface.ConfigurationEditor.utils.FileUtils;
import ModelInterface.ConfigurationEditor.configurationeditor.ConfigurationEditor;

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
        final File currentFile = FileUtils.selectFile(parentEditor, new XMLFileFilter(), null, false);
        
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
