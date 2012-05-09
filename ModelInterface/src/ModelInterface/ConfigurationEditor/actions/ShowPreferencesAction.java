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
package ModelInterface.ConfigurationEditor.actions;

import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Properties;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;

import ModelInterface.ConfigurationEditor.configurationeditor.PropertiesInfo;
import ModelInterface.ConfigurationEditor.guicomponents.PropertiesTextField;
import ModelInterface.ConfigurationEditor.utils.FileUtils;
import ModelInterface.ConfigurationEditor.utils.Messages;

/**
 * Class which defines the action which occurs when the user selects edit
 * preferences. The class displays a window to the user which shows a set of
 * preferences for the configuration editor which the user can modify. The
 * preferences are stored in a Preferences object. The action reads from the
 * preferences file each time the options are displayed. The file is stored when
 * the dialog is closed.
 * 
 * @author Josh Lurz
 */
public class ShowPreferencesAction extends AbstractAction {
    /**
     * Identifier used for serializing.
     */
    private static final long serialVersionUID = -5299525135551340917L;

    /**
     * A reference to the top level editor from which this action is receiving
     * commands.
     */
    private final transient Frame mParentFrame;

    /**
     * Constructor which sets the name of the Action and stores the parent
     * editor.
     * 
     * @param aParentFrame
     *            The top level window.
     */
    public ShowPreferencesAction(Frame aParentFrame) {
        super("Show Preferences..."); //$NON-NLS-1$
        mParentFrame = aParentFrame;
    }

    /**
     * Method called when an action is performed.
     * 
     * @param aEvent
     *            The event.
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(final ActionEvent aEvent) {
        // Initialize the properties object before setting up the input fields.
        final JDialog preferenceDialog = createPreferencesDialog();
        preferenceDialog.pack();
        preferenceDialog.setVisible(true);
    }

    /**
     * This method creates and initializes the preferences dialog frame.
     * 
     * @return The preferences dialog.
     */
    private JDialog createPreferencesDialog() {
        final JDialog prefDialog = new JDialog(mParentFrame, Messages
                    .getString("ShowPreferencesAction.3"), true); //$NON-NLS-1$
        prefDialog.setContentPane(createPreferencesDialogFrame(prefDialog));
        return prefDialog;
    }

    /**
     * This method initializes the preferences dialog.
     * @param aParentDialog The dialog which is creating this frame.
     * @return The preferences dialog frame.
     */
    private JPanel createPreferencesDialogFrame(final JDialog aParentDialog) {
        final JPanel prefDialog = new JPanel(new GridBagLayout());
        // Now setup the layout constraints. The components will be layed
        // out on a 3x4 grid with the field panels in the left column
        // and the ok and cancel buttons in the 2nd and 3rd columns in
        // the last row.
        final GridBagConstraints cons = new GridBagConstraints();

        // Set that the panels should grow to fit the cells.
        cons.fill = GridBagConstraints.BOTH;

        // Position the panels in the 1st column.
        cons.gridx = 0;

        // Add panels in increasing rows.
        cons.gridy = GridBagConstraints.RELATIVE;

        // Add 5 units of spacing on the y axis.
        cons.ipady = 5;

        // Put a border of 5 around the entire panel.
        cons.insets = new Insets(5, 5, 5, 5);

        // Weight the panels so they grow when the window is resized
        // horizontally
        // but not vertically.
        cons.weightx = 1;
        
        // Get the properties object.
        final Properties props = FileUtils.getInitializedProperties(prefDialog);
        // Add the fields to select the executable.
        final JPanel exeSelectFields = new PropertiesTextField(
                "Executable File", PropertiesInfo.EXE_PATH, props, "exe");
        prefDialog.add(exeSelectFields, cons);

        // Add the fields to select the configuration template file.
        final JPanel templateFields = new PropertiesTextField(
                "Configuration Template", PropertiesInfo.CONF_TMPL,
                props, "xml");
        prefDialog.add(templateFields, cons);

        // Add the fields to select the log configuration file.
        final JPanel logSelectFields = new PropertiesTextField(
                "Log File Location", PropertiesInfo.LOG_CONF, props,
                "xml");
        prefDialog.add(logSelectFields, cons);

        // Add ok and cancel buttons.
        final JButton okButton = new JButton(Messages
                .getString("ShowPreferencesAction.19")); //$NON-NLS-1$
        okButton.setToolTipText(Messages.getString("ShowPreferencesAction.10")); //$NON-NLS-1$

        // Add a listener which will save the preferences and close the
        // window.
        okButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent aEvent) {
                aParentDialog.dispose();
            }
        });
        // Set that the buttons should remain their default size
        // and be in the lower right corner of the grid.
        cons.fill = GridBagConstraints.NONE;
        cons.gridy = 4;
        cons.gridx = 2;

        // Don't allow the buttons to take up extra space.
        cons.weightx = 0;

        // Anchor the button to the north west side of the cell.
        cons.anchor = GridBagConstraints.NORTHWEST;

        prefDialog.add(okButton, cons);
        // Add a cancel button.
        final JButton cancelButton = new JButton();
        cancelButton.setText(Messages.getString("ShowPreferencesAction.18")); //$NON-NLS-1$
        cancelButton.setToolTipText(Messages
                .getString("ShowPreferencesAction.13")); //$NON-NLS-1$
        // Add a listener which will save the preferences and close the
        // window.
        cancelButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent aEvent) {
                aParentDialog.dispose();
            }
        });
        // The cancel button should be directly to the right
        // of the ok button.
        cons.gridx = GridBagConstraints.RELATIVE;
        prefDialog.add(cancelButton, cons);
        return prefDialog;
    }
}
