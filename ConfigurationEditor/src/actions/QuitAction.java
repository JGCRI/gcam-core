/**
 * 
 */
package actions;


import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.AbstractAction;

import utils.Util;

import configurationeditor.ConfigurationEditor;

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
    private ConfigurationEditor mParentEditor = null;

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

    /*
     * (non-Javadoc) @param aEvent The event received.
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent aEvent) {
        // Determine which action to take.
        if (aEvent.getActionCommand().equals("Quit")) { //$NON-NLS-1$
            // Call the internal quit method.
            doQuit();
        } else {
            // We should have had the Quit action.
            assert (false);
        }
    }

    /**
     * Function which checks if the file needs to be saved, and if it does asks
     * the user whether they would like to. This uses the same logic as save as.
     * 
     */
    private void doQuit() {
        // Check if the file should be saved before loading a new one.
        if (!Util.askForSave(mParentEditor)) {
            // The user does not want to continue.
            return;
        }
        // The file is not modified or has been saved, let them exit.
        mParentEditor.dispose();
    }

    public void windowOpened(WindowEvent arg0) {
        // Ignore window opened event.
    }

    public void windowClosing(WindowEvent aWindowEvent) {
        // Call the internal helper method.
        doQuit();
    }

    public void windowClosed(WindowEvent arg0) {
        // Ignore window closed event.
    }

    public void windowIconified(WindowEvent arg0) {
        // Ignore window iconified event.
    }

    public void windowDeiconified(WindowEvent arg0) {
        // Ignore window deiconified event.
    }

    public void windowActivated(WindowEvent arg0) {
        // Ignore window activated event.
    }

    public void windowDeactivated(WindowEvent arg0) {
        // Ignore window deactivated event.
    }
}
