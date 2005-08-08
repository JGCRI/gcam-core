package configurationeditor;

import guicomponents.DOMButtonModel;
import guicomponents.DOMListPanelFactory;
import guicomponents.DOMTextFieldFactory;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.KeyEvent;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.BevelBorder;

import utils.Messages;

/**
 * 
 */

/**
 * Panel which contains the information for modifying the most 
 * common configuration parameters.
 * @author Josh Lurz
 */
public class MainOptionsPanel extends AbstractEditorPanel {

    /**
     * Unique identifier for serializing.
     */
    private static final long serialVersionUID = 7827035762215557234L;

    /**
     * Constructor
     */
    public MainOptionsPanel() {
        super();
        initialize();
    }
    
    /**
     * This method initializes the main options panel.
     */
    private void initialize() {
        setLayout(new GridBagLayout());
        setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));

        // Now setup the layout constraints.
        final GridBagConstraints cons = new GridBagConstraints();

        // Layout the items top to bottom, left to right.
        // Position the checkboxes in the first column.
        cons.gridx = 0;

        // Position components on the left side of their cells.
        cons.anchor = GridBagConstraints.WEST;

        // Add panels in increasing rows.
        cons.gridy = 0;

        // Put a border of 5 around the entire panel.
        cons.insets = new Insets(5, 5, 5, 5);

        add(createCalibrationCheckBox(), cons);

        cons.gridy = GridBagConstraints.RELATIVE;
        add(createCalcCostCheckBox(), cons);

        // Now add the labels in column 1.
        cons.gridx = 1;
        cons.gridy = 0;

        // Create a label for the scenario name field.
        final JLabel scenNameLabel = new JLabel(Messages
                .getString("ConfigurationEditor.112")); //$NON-NLS-1$
        add(scenNameLabel, cons);

        // Put the labels in increasing rows.
        cons.gridy = GridBagConstraints.RELATIVE;

        // Create a label for the input file field.
        final JLabel inputFileLabel = new JLabel(Messages
                .getString("ConfigurationEditor.115")); //$NON-NLS-1$
        add(inputFileLabel, cons);

        // Create a label for the output file field.
        final JLabel outputFileLabel = new JLabel(Messages
                .getString("ConfigurationEditor.118")); //$NON-NLS-1$
        add(outputFileLabel, cons);

        // Add the text fields in column 2.
        cons.gridx = 2;
        cons.gridy = 0;

        // Allow the text field to resize with the items.
        cons.weightx = 1;
        cons.fill = GridBagConstraints.HORIZONTAL;

        // Create a text field factory and add it as a listener on the editor.
        final DOMTextFieldFactory textFieldFactory = new DOMTextFieldFactory();
        addPropertyChangeListener("document-replaced", textFieldFactory);

        // Add the scenario name field.
        final JTextField scenNameField = textFieldFactory.createTextField(
                "Strings", "scenarioName"); //$NON-NLS-1$ //$NON-NLS-2$
        scenNameField.setPreferredSize(new Dimension(200, 20));
        add(scenNameField, cons);

        // Put the fields in increasing rows.
        cons.gridy = GridBagConstraints.RELATIVE;

        // Add the input field.
        final JTextField inputFileField = textFieldFactory.createTextField(
                "Files", "xmlInputFileName"); //$NON-NLS-1$ //$NON-NLS-2$
        inputFileField.setPreferredSize(new Dimension(200, 20));
        add(inputFileField, cons);

        // Add the output file field.
        final JTextField outputFileField = textFieldFactory.createTextField(
                "Files", "xmlOutputFileName"); //$NON-NLS-1$ //$NON-NLS-2$
        outputFileField.setPreferredSize(new Dimension(200, 20));
        add(outputFileField, cons);

        // Put the list panel in column 3.
        cons.gridx = 3;
        cons.gridy = 0;
        // Make the panel absorb 4 rows.
        cons.gridheight = 4;
        cons.weightx = 1;
        // Make the panel take up the entire height.
        cons.fill = GridBagConstraints.BOTH;
        // Make the panel absorb vertical space.
        cons.weighty = 1;

        // Create the list panel factory and add it as a listener on the
        // current editor.
        final DOMListPanelFactory listFactory = new DOMListPanelFactory();
        addPropertyChangeListener("document-replaced", listFactory);

        final JPanel addOnPanel = listFactory
                .createDOMFileListPanel(
                        "/" + ConfigurationEditor.ROOT_ELEMENT_NAME + "/ScenarioComponents", ConfigurationEditor.ELEMENT_NAME, Messages.getString("ConfigurationEditor.151"), true); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        add(addOnPanel, cons);
    }
    
    /**
     * This method initializes the do calibration checkbox.
     * 
     * @return The do calibration checkbox.
     */
    private JCheckBox createCalibrationCheckBox() {
        final JCheckBox calCheckBox = new JCheckBox(Messages
                .getString("ConfigurationEditor.13")); //$NON-NLS-1$
        final String parentXPath = "/" + ConfigurationEditor.ROOT_ELEMENT_NAME
                + "/Bools";
        final DOMButtonModel model = new DOMButtonModel(null, parentXPath,
                ConfigurationEditor.ELEMENT_NAME, "CalibrationActive", false);
        calCheckBox.setModel(model); //$NON-NLS-1$
        
        // Add the model as a property change listener so it will receive
        // document changed notifications.
        addPropertyChangeListener("document-replaced", model);
        
        calCheckBox.setMnemonic(KeyEvent.VK_C);
        calCheckBox
                .setToolTipText(Messages.getString("ConfigurationEditor.14")); //$NON-NLS-1$
        return calCheckBox;
    }

    /**
     * This method initializes the run consts curves check box.
     * 
     * @return The run cost curves check box.
     */
    private JCheckBox createCalcCostCheckBox() {
        final JCheckBox calcCosts = new JCheckBox(Messages
                .getString("ConfigurationEditor.128")); //$NON-NLS-1$

        final String parentXPath = "/" + ConfigurationEditor.ROOT_ELEMENT_NAME
                + "/Bools";
        
        final DOMButtonModel model = new DOMButtonModel(null, parentXPath, ConfigurationEditor.ELEMENT_NAME,
                "createCostCurve", false);
        calcCosts.setModel(model); //$NON-NLS-1$
        
        // Add the model as a property change listener so it will receive
        // document changed notifications.
        addPropertyChangeListener("document-replaced", model);
        
        calcCosts.setMnemonic(KeyEvent.VK_O);
        calcCosts.setToolTipText(Messages.getString("ConfigurationEditor.129")); //$NON-NLS-1$
        return calcCosts;
    }
}
