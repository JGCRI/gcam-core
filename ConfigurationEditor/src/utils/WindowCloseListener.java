/*
 */
package utils;

import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JOptionPane;

import org.w3c.dom.Document;

import configurationeditor.DOMDocumentEditor;

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
    public void windowClosing(WindowEvent aEvent) {
        DOMDocumentEditor sourceEditor = ((DOMDocumentEditor) aEvent
                .getWindow());
        // Get the document from the editor.
        Document document = sourceEditor.getDocument();
        // Check if the document needs to be saved.
        if (FileUtils.isDirty(document)) {
            // Check if the user must be asked before the file is saved.
            if (sourceEditor.askBeforeSaving()) {
                final String message = "Would you like to save the current file?";
                int rv = JOptionPane.showConfirmDialog(aEvent.getWindow(),
                        message,
                        "Save Dialog", JOptionPane.YES_NO_CANCEL_OPTION); //$NON-NLS-1$
                // The user wants to save.
                if (rv == JOptionPane.YES_OPTION) {
                    DOMUtils.serializeDocument(document, aEvent.getWindow());
                } else if (rv == JOptionPane.NO_OPTION) {
                    // They don't want to save, so allow the quit.
                } else {
                    // They closed the dialog, they want to continue.
                    return;
                }
            } else {
                // Save the document without the user's intervention.
                DOMUtils.serializeDocument(document, aEvent.getWindow());
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
    public void windowOpened(WindowEvent aEvent) {
        // Do nothing
    }

    /**
     * Method called when a window is closed, implemented to do nothing.
     * 
     * @param aEvent
     *            The window event received.
     */
    public void windowClosed(WindowEvent aEvent) {
        // Do nothing
    }

    /**
     * Method called when a window is iconified, implemented to do nothing.
     * 
     * @param aEvent
     *            The window event received.
     */
    public void windowIconified(WindowEvent aEvent) {
        // Do nothing
    }

    /**
     * Method called when a window is deiconified, implemented to do nothing.
     * 
     * @param aEvent
     *            The window event received.
     */
    public void windowDeiconified(WindowEvent aEvent) {
        // Do nothing
    }

    /**
     * Method called when a window is activated, implemented to do nothing.
     * 
     * @param aEvent
     *            The window event received.
     */
    public void windowActivated(WindowEvent aEvent) {
        // Do nothing
    }

    /**
     * Method called when a window is deactivated, implemented to do nothing.
     * 
     * @param aEvent
     *            The window event received.
     */
    public void windowDeactivated(WindowEvent aEvent) {
        // Do nothing
    }
}
