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
package ModelInterface.ConfigurationEditor.guicomponents;

import ModelInterface.ConfigurationEditor.guihelpers.XMLFileFilter;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.File;
import javax.swing.filechooser.FileFilter;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import ModelInterface.ConfigurationEditor.utils.ExeFileFilter;
import ModelInterface.ConfigurationEditor.utils.FileUtils;

/**
 * @author Josh Lurz
 * 
 */
public class PropertiesTextField extends JPanel {

	/**
	 * Serial version unique identifier.
	 */
	private static final long serialVersionUID = 723135424936504399L;

	/**
	 * The label to use for the text field.
	 */
	private final transient String mLabelText;

	/**
	 * The identifier of the property in the properties map.
	 */
	private final transient String mPropertyID;

	/**
	 * The properties map to set and query.
	 */
	private final transient Properties mProperties;

	/**
	 * The file type of the item being selected. If this is null, a select
	 * button is not added.
	 */
	private final transient String mFileType;

	/**
	 * Constructor.
	 * 
	 * @param aLabelText
	 *            Label to use for the text field.
	 * @param aPropertyID
	 *            The identifier of the item in the properties.
	 * @param aProperties
	 *            A set of properties to set and query for the item.
	 * @param aFileType
	 *            The file type of the item being selected. Null if this is not
	 *            a type of file.
	 */
	public PropertiesTextField(String aLabelText, String aPropertyID,
			Properties aProperties, String aFileType) {
		super();
		mLabelText = aLabelText;
		mPropertyID = aPropertyID;
		mProperties = aProperties;
		mFileType = aFileType;
		initialize();
	}

	/**
	 * Initialize the UI items, set their initial values and add listeners.
	 */
	private void initialize() {
		setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		// Add a label for the field.
		final JLabel label = new JLabel(mLabelText + ":", SwingConstants.RIGHT);
		label.setPreferredSize(new Dimension(150, 20));
		add(label);

		// Add a horizontal gap between the label and the text field.
		add(Box.createRigidArea(new Dimension(5, 0)));

		// Add a text field.
		final JTextField textField = new JTextField();

		// Try and get the existing value of the property.
		assert (mProperties != null);
		assert (mPropertyID != null && mPropertyID.length() > 0);
		textField.setText(mProperties.getProperty(mPropertyID));

		textField.setPreferredSize(new Dimension(400, 20));

		// Add a focus listener so when the user clicks off the field the
		// value is saved.
		textField.addFocusListener(new FocusListener() {
			public void focusGained(FocusEvent aEvent) {
				// Ignore event.
			}

			/**
			 * Method to save the value of the text field when the user clicks
			 * away from the text field.
			 */
			public void focusLost(FocusEvent aEvent) {
				mProperties.setProperty(mPropertyID, textField.getText());
			}
		});

		add(textField);

		// Add a horizontal gap between the text field and the button.
		add(Box.createRigidArea(new Dimension(5, 0)));

		// If the file type is null the user does not want a file selection
		// button.
		if (mFileType != null) {
			// Add a button to select the file.
			final JButton selectButton = new JButton();
			selectButton.setText("Select...");
			selectButton.setToolTipText("Select the value for the property.");

			// Add a listener which will launch the file chooser.
			final JPanel parent = this;
			selectButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent aEvent) {
					// Select the type of file filter to use.
					FileFilter filter = null;
					if (mFileType.equals("exe")) {
						filter = new ExeFileFilter();
					} else if (mFileType.equals("xml")) {
						filter = new XMLFileFilter();
					} else {
						Logger.global.log(Level.SEVERE, "Unknown filter type.");
					}
					final File selectedFile = FileUtils.selectFile(parent,
							filter, textField.getText(), false);
					if (selectedFile != null) {
						// Set the text field and the property.
						final String selectedPath = selectedFile
								.getAbsolutePath();
						textField.setText(selectedPath);
						mProperties.setProperty(mPropertyID, selectedPath);
					}
				}
			});
			add(selectButton);
		}
	}
}
