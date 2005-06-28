/**
 * 
 */
package actions;


import interfaceutils.ExeFileFilter;
import interfaceutils.Util;
import interfaceutils.XMLFileFilter;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import com.zookitec.layout.ComponentEF;
import com.zookitec.layout.ContainerEF;
import com.zookitec.layout.ExplicitConstraints;
import com.zookitec.layout.ExplicitLayout;
import com.zookitec.layout.GroupEF;

import configurationeditor.ConfigurationEditor;

/** 
 * Class which defines the action which occurs when the user selects edit preferences.
 * The class displays a window to the user which shows a set of preferences for
 * the configuration editor which the user can modify. The preferences are
 * stored in a Preferences object. The action reads from the preferences file
 * each time the options are displayed. The file is stored when the dialog is
 * closed.
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
     * The label for the configuration template field.
     */
    private JLabel mConfLabel = null;
    
    /**
     * The field containing the path to the executable.
     * This must be a member variable so it can be accessed
     * inside the Action for the select button.
     */
    private JTextField mConfigurationTemplatePathField = null;
    
    /**
     * The button which selects the configuration template file.
     */
    private JButton mConfSelectButton = null;
    /**
     * The label for the executable path.
     */
    private JLabel mExePathLabel = null;
    
    /**
     * The field containing the path to the configuration template.
     * This must be a member variable so it can be accessed
     * inside the Action for the select button.
     */
    private JTextField mExePathField = null;
    
    /**
     * The button which selects the executable
     */
    private JButton mExeSelectButton = null;
    
    /**
     * The cancel button.
     */
    private JButton mCancelButton;
    
    /**
     * The OK button.
     */
    private JButton mOKButton;
    
    /**
     * The name of the attribute which stores the configuration template path.
     * This field is currently duplicated in NewAction.
     */
    private final String mConfigurationTemplateProperty = "template-path"; //$NON-NLS-1$
    
    /**
     * Constructor which sets the name of the Action and stores the parent editor.
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
     * @param aEvent The event.
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
        assert(mProperties == null);
        mProperties = new Properties();
        
        try {
            FileInputStream inputStream = new FileInputStream(ConfigurationEditor.PROPERTY_FILE_NAME);
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
            JOptionPane.showMessageDialog(mParentEditor, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE);
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
            mPreferenceDialog = new JDialog(mParentEditor, Messages.getString("ShowPreferencesAction.3"), true ); //$NON-NLS-1$
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
            mPreferencesDialogFrame = new JPanel(new ExplicitLayout());
            addExeSelectionFields();
            addConfFileSelectionFields();
            addOKCancelButtons();
            
            // Set the preferred width and height.
            // Use the width of the configuration items as preferred width,
            // this is not perfect.
            // TODO: Use the maximum width of all rows.
            Component widthComponents[] = { mExePathLabel, mConfigurationTemplatePathField, mConfSelectButton };
            double width = GroupEF.preferredWidthSum(widthComponents).getValue((ExplicitLayout)mPreferencesDialogFrame.getLayout());
            // Add extra for buffer space between items.
            width += 20;
            
            // TODO: Use max's of everything.
            Component heightComponents[] = { mExeSelectButton, mConfSelectButton, mOKButton };
            double height = GroupEF.preferredHeightSum(heightComponents).getValue((ExplicitLayout)mPreferencesDialogFrame.getLayout());
            // Add extra buffer space between items.
            height += 20;
            mPreferencesDialogFrame.setPreferredSize(new Dimension((int)Math.round(width), (int)Math.round(height)));
            
        }
        return mPreferencesDialogFrame;
    }
    
    /**
	 * Adds a label, text field and button to the preferences
	 * frame which are used for selecting the executable.
	 */
	private void addExeSelectionFields() {
		// Add a label for the path to the executable.
        mExePathLabel = new JLabel();
        mExePathLabel.setText(Messages.getString("ShowPreferencesAction.4")); //$NON-NLS-1$
		mPreferencesDialogFrame.add(mExePathLabel,
				new ExplicitConstraints(mExePathLabel, 
						                ContainerEF.left(mPreferencesDialogFrame).add(5),
						                ContainerEF.top(mPreferencesDialogFrame).add(5)));

		// Add a text field.
		mExePathField = new JTextField();

		// Try and get the existing path.
		String currPath = mProperties.getProperty(ConfigurationEditor.EXE_PATH_PROPERTY_NAME);
		mExePathField.setText(currPath);

		mExePathField.setPreferredSize(new Dimension(400, 20));
		mPreferencesDialogFrame.add(mExePathField,
									new ExplicitConstraints(mExePathField, 
								    ComponentEF.right(mExePathLabel).add(5),
								    ContainerEF.top(mPreferencesDialogFrame).add(5)));

		// Add a button to select the file.
		mExeSelectButton = new JButton();
        mExeSelectButton.setText(Messages.getString("ShowPreferencesAction.7")); //$NON-NLS-1$
        mExeSelectButton.setToolTipText(Messages
				.getString("ShowPreferencesAction.6")); //$NON-NLS-1$

		// Add a listener which will launch the file chooser.
        mExeSelectButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent aEvent) {
						JFileChooser exeFileChooser = new JFileChooser();
						exeFileChooser.setFileFilter(new ExeFileFilter());
						int rv = exeFileChooser.showDialog(mParentEditor,
								Messages.getString("ShowPreferencesAction.8")); //$NON-NLS-1$
						if (rv == JFileChooser.APPROVE_OPTION) {
							File selectedFile = exeFileChooser
									.getSelectedFile();
							if (selectedFile != null) {
								// Set the text field and the property.
								String selectedPath = selectedFile
										.getAbsolutePath();
								mExePathField.setText(selectedPath);
								mProperties.setProperty(ConfigurationEditor.EXE_PATH_PROPERTY_NAME,
										selectedPath);
							}
						}
					}
				});
		mPreferencesDialogFrame.add(mExeSelectButton,
									new ExplicitConstraints(mExeSelectButton, 
					                ComponentEF.right(mExePathField).add(5),
					                ContainerEF.top(mPreferencesDialogFrame).add(5)));
	}

	/**
	 * Adds a label, text field and button to the preferences
	 * frame which are used for selecting the executable.
	 */
	private void addConfFileSelectionFields() {
		{
			// Add a label for the path to the configuration template.
			mConfLabel = new JLabel();
            mConfLabel.setText(Messages.getString("ShowPreferencesAction.14")); //$NON-NLS-1$
			mPreferencesDialogFrame.add(mConfLabel,
										new ExplicitConstraints(mConfLabel, 
							            ContainerEF.left(mPreferencesDialogFrame).add(5),
							            ComponentEF.bottom(mExeSelectButton).add(5)));

			// Add a text field.
			mConfigurationTemplatePathField = new JTextField();

			// Try and get the existing path.
			String currConfPath = mProperties
					.getProperty(mConfigurationTemplateProperty);
			if (currConfPath != null) {
				mConfigurationTemplatePathField.setText(currConfPath);
			}

			mConfigurationTemplatePathField.setPreferredSize(new Dimension(400,
					20));
			mPreferencesDialogFrame.add(mConfigurationTemplatePathField,
										new ExplicitConstraints(mConfigurationTemplatePathField, 
										ComponentEF.right(mExePathLabel).add(5),
                                        ComponentEF.bottom(mExeSelectButton).add(5)));
			// Add a button to select the file.
			mConfSelectButton = new JButton();
            mConfSelectButton.setText(Messages.getString("ShowPreferencesAction.15")); //$NON-NLS-1$
            mConfSelectButton
					.setToolTipText(Messages.getString("ShowPreferencesAction.16")); //$NON-NLS-1$

			// Add a listener which will launch the file chooser.
            mConfSelectButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent aEvent) {
					JFileChooser confTemplateFileChooser = new JFileChooser();
					confTemplateFileChooser.setFileFilter(new XMLFileFilter());
					int rv = confTemplateFileChooser.showDialog(mParentEditor, Messages.getString("ShowPreferencesAction.17")); //$NON-NLS-1$
					if (rv == JFileChooser.APPROVE_OPTION) {
						File selectedFile = confTemplateFileChooser
								.getSelectedFile();
						if (selectedFile != null) {
							// Set the text field and the property.
							String selectedPath = selectedFile
									.getAbsolutePath();
							mConfigurationTemplatePathField
									.setText(selectedPath);
							mProperties.setProperty(
									mConfigurationTemplateProperty,
									selectedPath);
						}
					}
				}
			});
			mPreferencesDialogFrame.add(mConfSelectButton,
										new ExplicitConstraints(mConfSelectButton, 
							            ComponentEF.right(mExePathField).add(5),
                                        ComponentEF.bottom(mExeSelectButton).add(5)));
		}
	}

	/**
	 * Add OK and Cancel buttons to the preferences from which will
	 * save or discard any changes to the fields.
	 *
	 */
	private void addOKCancelButtons() {
		// Add a cancel button.
		mCancelButton = new JButton();
        mCancelButton.setText(Messages.getString("ShowPreferencesAction.18")); //$NON-NLS-1$
        mCancelButton.setToolTipText(Messages
				.getString("ShowPreferencesAction.13")); //$NON-NLS-1$

		// Add a listener which will save the preferences and close the window.
        mCancelButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent aEvent) {
						getPreferenceDialog().dispose();
					}
				});
		mPreferencesDialogFrame.add(mCancelButton,
				new ExplicitConstraints(mCancelButton, 
                        ContainerEF.right(mPreferencesDialogFrame).subtract(ComponentEF.width(mCancelButton).add(5)),
			            ContainerEF.bottom(mPreferencesDialogFrame).subtract(ComponentEF.height(mCancelButton).add(5))));
        
        // Add an OK button after adding the cancel button so it can position
        // itself based on the cancel button.
        mOKButton = new JButton();
        mOKButton.setText(Messages.getString("ShowPreferencesAction.19")); //$NON-NLS-1$
        mOKButton.setToolTipText(Messages.getString("ShowPreferencesAction.10")); //$NON-NLS-1$

        // Add a listener which will save the preferences and close the
        // window.
        mOKButton.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent aEvent) {
                        Util.saveProperties(mProperties);
                        getPreferenceDialog().dispose();
                    }
                });
        mPreferencesDialogFrame.add(mOKButton,
                    new ExplicitConstraints(mOKButton, 
                        ComponentEF.left(mCancelButton).subtract(ComponentEF.width(mOKButton).add(5)),
                        ContainerEF.bottom(mPreferencesDialogFrame).subtract(ComponentEF.height(mOKButton).add(5))));
	}
}
