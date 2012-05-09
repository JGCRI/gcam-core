/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
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
package ModelInterface.ConfigurationEditor.guihelpers;

import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.w3c.dom.Document;

import ModelInterface.ConfigurationEditor.utils.DOMUtils;
import ModelInterface.ConfigurationEditor.utils.FileUtils;

import ModelInterface.ConfigurationEditor.configurationeditor.DOMDocumentEditor;

/**
 * This class implements a window close listener which ensures the user is
 * allowed to save the batch file before exiting.
 * 
 * @author Josh Lurz
 */
public class WindowCloseListener implements WindowListener {
    /**
     * Method called when the window is closing which checks if the document
     * needs saving and queries the user.
     * 
     * @see java.awt.event.WindowListener#windowClosing(java.awt.event.WindowEvent)
     * @param aEvent
     *            The window event received.
     */
    public void windowClosing(final WindowEvent aEvent) {
        // TODO: This is pretty bad.
        final DOMDocumentEditor sourceEditor = (DOMDocumentEditor)((JDialog)aEvent.getWindow()).getContentPane();
        // Get the document from the editor.
        final Document document = sourceEditor.getDocument();
        // Check if the document needs to be saved.
        if (document != null && FileUtils.isDirty(document)) {
            // Check if the user must be asked before the file is saved.
            if (sourceEditor.askBeforeSaving()) {
                final String message = "Would you like to save the current file?";
                final int returnValue = JOptionPane.showConfirmDialog(aEvent.getWindow(),
                        message,
                        "Save Dialog", JOptionPane.YES_NO_CANCEL_OPTION); //$NON-NLS-1$
                // The user wants to save.
                if (returnValue == JOptionPane.YES_OPTION) {
                    DOMUtils.serialize(document, aEvent.getWindow());
                } else if (returnValue == JOptionPane.CANCEL_OPTION) {
                    // They closed the dialog, they want to continue.
                    return;
                }
            } else {
                // Save the document without the user's intervention.
                DOMUtils.serialize(document, aEvent.getWindow());
            }
        }
        aEvent.getWindow().dispose();
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
     * Method called when a window is deiconified, implemented to do nothing.
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
     * Method called when a window is deactivated, implemented to do nothing.
     * 
     * @param aEvent
     *            The window event received.
     */
    public void windowDeactivated(final WindowEvent aEvent) {
        // Do nothing
    }
}
