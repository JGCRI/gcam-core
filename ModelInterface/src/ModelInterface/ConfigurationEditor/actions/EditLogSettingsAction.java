/**
 * 
 */
package ModelInterface.ConfigurationEditor.actions;

import ModelInterface.ConfigurationEditor.guihelpers.WindowCloseListener;

import java.awt.Frame;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JComponent;
import javax.swing.JDialog;
import ModelInterface.ConfigurationEditor.configurationeditor.LogEditor;

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
        // Initialize the log editor.
        final LogEditor logEditor = new LogEditor();
        if (!logEditor.initialize()) {
            return;
        }
        // TODO: This is pretty nasty.
        final Frame parent = (Frame) ((JComponent) aEvent.getSource())
                .getTopLevelAncestor();
        final JDialog editLogsDialog = new JDialog(parent, "Log Settings", true);
        editLogsDialog.addWindowListener(new WindowCloseListener());
        editLogsDialog.setContentPane(logEditor);
        editLogsDialog.pack();
        editLogsDialog.setVisible(true);
    }

}
