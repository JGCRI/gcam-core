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
