/**
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
