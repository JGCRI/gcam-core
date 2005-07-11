/*
 */
package utils;


import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JOptionPane;

import org.w3c.dom.Document;

import configurationeditor.DOMDocumentEditor;

/**
 * This class implements a window close listener which ensures
 * the user is allowed to save the batch file before exiting.
 * @author Josh Lurz
 */
public class WindowCloseListener implements WindowListener {

	/* (non-Javadoc)
	 * @see java.awt.event.WindowListener#windowOpened(java.awt.event.WindowEvent)
	 */
	public void windowOpened(WindowEvent aEvent) {
        // Ignore window opened event.
	}

	/* (non-Javadoc)
	 * @see java.awt.event.WindowListener#windowClosing(java.awt.event.WindowEvent)
	 */
	public void windowClosing(WindowEvent aEvent) {
        // Get the document from the editor.
        Document document = ((DOMDocumentEditor)aEvent.getWindow()).getDocument();
        // Check if the document needs to be saved.
        if(FileUtils.isDirty(document)) {
            final String message = "Would you like to save the current file?";
            int rv = JOptionPane.showConfirmDialog(aEvent.getWindow(), message,
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
        }
        aEvent.getWindow().dispose();
	}

	/* (non-Javadoc)
	 * @see java.awt.event.WindowListener#windowClosed(java.awt.event.WindowEvent)
	 */
	public void windowClosed(WindowEvent arg0) {
        // Ignore window closed event.
	}

	/* (non-Javadoc)
	 * @see java.awt.event.WindowListener#windowIconified(java.awt.event.WindowEvent)
	 */
	public void windowIconified(WindowEvent arg0) {
        // Ignore window iconified event.
	}

	/* (non-Javadoc)
	 * @see java.awt.event.WindowListener#windowDeiconified(java.awt.event.WindowEvent)
	 */
	public void windowDeiconified(WindowEvent arg0) {
        // Ignore window deiconified event.
	}

	/* (non-Javadoc)
	 * @see java.awt.event.WindowListener#windowActivated(java.awt.event.WindowEvent)
	 */
	public void windowActivated(WindowEvent arg0) {
        // Ignore window activated event.
	}

	/* (non-Javadoc)
	 * @see java.awt.event.WindowListener#windowDeactivated(java.awt.event.WindowEvent)
	 */
	public void windowDeactivated(WindowEvent arg0) {
        // Ignore window deactivated event.
	}

}
