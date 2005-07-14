/**
 * 
 */
package actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JDialog;
import configurationeditor.LogEditor;

/**
 * This class displays a dialog for editing log settings.
 * 
 * @author Josh Lurz
 */
public class EditLogSettingsAction extends AbstractAction {

    /**
     * Identifier used for serializing.
     */
    private static final long serialVersionUID = 1257512799098654257L;

    /**
     * Constructor
     */
    public EditLogSettingsAction() {
        super("Edit Log Settings..."); //$NON-NLS-1$
    }

    /**
     * Method called when an action is performed on the dispatching object.
     * 
     * @param aEvent
     *            The event received.
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(final ActionEvent aEvent) {
        // TODO: Get a location for this dialog.
        final JDialog editLogsDialog = new LogEditor(null);
        editLogsDialog.pack();
        editLogsDialog.setVisible(true);
    }

}
