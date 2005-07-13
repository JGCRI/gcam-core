/**
 * 
 */
package actions;


import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.AbstractAction;

import utils.FileUtils;

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
