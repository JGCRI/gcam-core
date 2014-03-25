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


import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.AbstractAction;

import ModelInterface.ConfigurationEditor.utils.FileUtils;

import ModelInterface.ConfigurationEditor.configurationeditor.ConfigurationEditor;

/**
 * This action object is called whenever the user calls quit. This can be called
 * by clicking the quit button, selecting it from the window, or closing the
 * window. The action will determine if a save is needed and dispatch it if
 * needed, and offers the user an opportunity to cancel the quit.
 * TODO: Can this use the generic interface utils window listener?
 * 
 * @author Josh Lurz
 */
public class QuitAction extends AbstractAction implements WindowListener {
    /**
     * Identifier used for serializing.
     */
    private static final long serialVersionUID = -8436856241489036508L;

    /**
     * A reference to the top level editor from which this action is receiving
     * commands.
     */
    private final transient ConfigurationEditor mParentEditor;

    /**
     * Constructor which sets the name of the Action and stores the parent
     * editor.
     * 
     * @param aParentEditor
     *            The top level editor.
     */
    public QuitAction(ConfigurationEditor aParentEditor) {
        super("Quit"); //$NON-NLS-1$
        mParentEditor = aParentEditor;
    }

    /**
     * Method called when the quit action is activated which calls
     * the internal doQuit method to check if the file should be saved
     * before quitting.
     * 
     * @param aEvent The event received.
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(final ActionEvent aEvent) {
    	// Call the internal quit method.
    	doQuit();
    }

    /**
     * Function which checks if the file needs to be saved, and if it does asks
     * the user whether they would like to. This uses the same logic as save as.
     * 
     */
    private void doQuit() {
        // Check if the file should be saved before loading a new one.
        if (!FileUtils.askForSave(mParentEditor)) {
            // The user does not want to continue.
            return;
        }
        // The file is not modified or has been saved, let them exit.
        mParentEditor.dispose();
    }

    /**
     * Method called when the window is closing which allows
     * the user to choose whether to save the file before
     * quitting.
     * @param aEvent The window event received.
     */
    public void windowClosing(final WindowEvent aEvent) {
        // Call the internal helper method.
        doQuit();
    }

	/**
	 * Method called when a window is opened, implemented to do nothing.
	 * 
	 * @param aEvent
	 *            The window event received.
	 */
	public void windowOpened(final WindowEvent aEvent) {
		// Do nothing
	}
	
	/**
	 * Method called when a window is closed, implemented to do nothing.
	 * 
	 * @param aEvent
	 *            The window event received.
	 */
	public void windowClosed(final WindowEvent aEvent) {
		// Do nothing
	}

	/**
	 * Method called when a window is iconified, implemented to do nothing.
	 * 
	 * @param aEvent
	 *            The window event received.
	 */
	public void windowIconified(final WindowEvent aEvent) {
		// Do nothing
	}

	/**
	 * Method called when a window is deiconified, implemented to do
	 * nothing.
	 * 
	 * @param aEvent
	 *            The window event received.
	 */
	public void windowDeiconified(final WindowEvent aEvent) {
		// Do nothing
	}

	/**
	 * Method called when a window is activated, implemented to do nothing.
	 * 
	 * @param aEvent
	 *            The window event received.
	 */
	public void windowActivated(final WindowEvent aEvent) {
		// Do nothing
	}

	/**
	 * Method called when a window is deactivated, implemented to do
	 * nothing.
	 * 
	 * @param aEvent
	 *            The window event received.
	 */
	public void windowDeactivated(final WindowEvent aEvent) {
		// Do nothing
	}
}
