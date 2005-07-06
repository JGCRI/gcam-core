/**
 * 
 */
package interfaceutils;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.File;
import java.util.Properties;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

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
    private String mLabelText = null;

    /**
     * The identifier of the property in the properties map.
     */
    private String mPropertyID = null;

    /**
     * The properties map to set and query.
     */
    private Properties mProperties = null;

    /**
     * Constructor.
     * @param aLabelText Label to use for the text field.
     * @param aPropertyID The identifier of the item in the properties.
     * @param aProperties A set of properties to set and query for the item.
     */
    public PropertiesTextField(String aLabelText, String aPropertyID,
            Properties aProperties) {
        super();
        mLabelText = aLabelText;
        mPropertyID = aPropertyID;
        mProperties = aProperties;
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
        add(Box.createRigidArea(new Dimension(5,0)));
        
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
             * Method to save the value of the text field when
             * the user clicks away from the text field.
             */
            public void focusLost(FocusEvent aEvent) {
                mProperties.setProperty(mPropertyID, textField.getText());
            }
        });

        add(textField);
        
        // Add a horizontal gap between the text field and the button.
        add(Box.createRigidArea(new Dimension(5,0)));
        
        // Add a button to select the file.
        final JButton selectButton = new JButton();
        selectButton.setText("Select...");
        selectButton.setToolTipText("Select the value for the property.");
        
        // Add a listener which will launch the file chooser.
        final JPanel parent = this;
        selectButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent aEvent) {
                JFileChooser fileChooser = new JFileChooser();
                fileChooser.setFileFilter(new ExeFileFilter());
                int rv = fileChooser.showDialog(parent,
                        mLabelText);
                if (rv == JFileChooser.APPROVE_OPTION) {
                    File selectedFile = fileChooser.getSelectedFile();
                    if (selectedFile != null) {
                        // Set the text field and the property.
                        String selectedPath = selectedFile.getAbsolutePath();
                        textField.setText(selectedPath);
                        mProperties.setProperty(mPropertyID, selectedPath);
                    }
                }
            }
        });
        add(selectButton);
    }
}
