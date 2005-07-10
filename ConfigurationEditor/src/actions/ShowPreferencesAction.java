/**
 * 
 */
package actions;


import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import utils.PropertiesTextField;
import utils.Util;
import configurationeditor.ConfigurationEditor;
import configurationeditor.PropertiesInfo;

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
    private ConfigurationEditor mParentEditor = null;

    /**
     * The preferences dialog.
     */
    private JDialog mPreferenceDialog = null;

    /**
     * The content frame of the preferences dialog.
     */
    private JPanel mPreferencesDialogFrame = null;

    /**
     * Properties object.
     */
    private Properties mProperties = null;

    /**
     * The cancel button.
     */
    private JButton mCancelButton = null;

    /**
     * The OK button.
     */
    private JButton mOKButton = null;

    /**
     * Constructor which sets the name of the Action and stores the parent
     * editor.
     * 
     * @param aParentEditor
     *            The top level editor.
     */
    public ShowPreferencesAction(ConfigurationEditor aParentEditor) {
        super("ShowPreferencesAction"); //$NON-NLS-1$
        mParentEditor = aParentEditor;
    }

    /**
     * Method called when an action is performed.
     * 
     * @param aEvent
     *            The event.
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent aEvent) {
        // Initialize the properties object before setting up the input fields.
        initializeProperties();
        JDialog preferenceDialog = getPreferenceDialog();
        preferenceDialog.pack();
        preferenceDialog.setVisible(true);
    }

    /**
     * Initialize the internal properties object which reads and stores
     * preferences in a file.
     * 
     */
    private void initializeProperties() {
        // Get the executable path from the properties file.
        // Properties shouldn't already be initialized.
        assert (mProperties == null);
        mProperties = new Properties();

        try {
            FileInputStream inputStream = new FileInputStream(
                    PropertiesInfo.PROPERTY_FILE);
            mProperties.loadFromXML(inputStream);
            inputStream.close();
        } catch (FileNotFoundException e) {
            // The preferences file did not exist, this is not an
            // error. A preferences file will be created when they
            // are saved.
            return;
        } catch (IOException e) {
            // The preferences file exists but it can't be read.
            String errorMessage = Messages.getString("ShowPreferencesAction.2") + e.getMessage() + "."; //$NON-NLS-1$ //$NON-NLS-2$
            String errorTitle = Messages.getString("ShowPreferencesAction.0"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(mParentEditor, errorMessage,
                    errorTitle, JOptionPane.ERROR_MESSAGE);
            Logger.global.log(Level.SEVERE, errorMessage);
            return;
        }
    }

    /**
     * This method initializes the preferences dialog frame.
     * 
     * @return The preferences dialog.
     */
    private JDialog getPreferenceDialog() {
        if (mPreferenceDialog == null) {
            mPreferenceDialog = new JDialog(mParentEditor, Messages
                    .getString("ShowPreferencesAction.3"), true); //$NON-NLS-1$
            mPreferenceDialog.setContentPane(getPreferencesDialogFrame());
        }
        return mPreferenceDialog;
    }

    /**
     * This method initializes the preferences dialog.
     * 
     * @return The preferences dialog frame.
     */
    private JPanel getPreferencesDialogFrame() {
        if (mPreferencesDialogFrame == null) {
            mPreferencesDialogFrame = new JPanel(new GridBagLayout());
            // Now setup the layout constraints. The components will be layed
            // out on a 3x4 grid with the field panels in the left column
            // and the ok and cancel buttons in the 2nd and 3rd columns in
            // the last row.
            GridBagConstraints cons = new GridBagConstraints();

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

            // Add the fields to select the executable.
            JPanel exeSelectionFields = new PropertiesTextField(
                    "Executable File", PropertiesInfo.EXE_PATH_PROPERTY,
                    mProperties, "exe");
            mPreferencesDialogFrame.add(exeSelectionFields, cons);

            // Add the fields to select the configuration template file.
            JPanel confTemplateSelectionFields = new PropertiesTextField(
                    "Configuration Template",
                    PropertiesInfo.CONFIGURATION_TEMPLATE_PROPERTY,
                    mProperties, "xml");
            mPreferencesDialogFrame.add(confTemplateSelectionFields, cons);

            // Add the fields to select the log configuration file.
            JPanel logSelectionFields = new PropertiesTextField(
                    "Log File Location", PropertiesInfo.LOG_CONF_PROPERTY,
                    mProperties, "xml");
            mPreferencesDialogFrame.add(logSelectionFields, cons);

            // Add ok and cancel buttons.
            mOKButton = new JButton();
            mOKButton.setText(Messages.getString("ShowPreferencesAction.19")); //$NON-NLS-1$
            mOKButton.setToolTipText(Messages
                    .getString("ShowPreferencesAction.10")); //$NON-NLS-1$

            // Add a listener which will save the preferences and close the
            // window.
            mOKButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent aEvent) {
                    Util.saveProperties(mProperties);
                    getPreferenceDialog().dispose();
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

            mPreferencesDialogFrame.add(mOKButton, cons);
            // Add a cancel button.
            mCancelButton = new JButton();
            mCancelButton.setText(Messages
                    .getString("ShowPreferencesAction.18")); //$NON-NLS-1$
            mCancelButton.setToolTipText(Messages
                    .getString("ShowPreferencesAction.13")); //$NON-NLS-1$
            // Add a listener which will save the preferences and close the
            // window.
            mCancelButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent aEvent) {
                    getPreferenceDialog().dispose();
                }
            });
            // The cancel button should be directly to the right
            // of the ok button.
            cons.gridx = GridBagConstraints.RELATIVE;
            mPreferencesDialogFrame.add(mCancelButton, cons);
        }
        return mPreferencesDialogFrame;
    }
}
