/*
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
        if (FileUtils.isDirty(document)) {
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
