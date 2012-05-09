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
package ModelInterface.ConfigurationEditor.configurationeditor;

import ModelInterface.ConfigurationEditor.guicomponents.DOMButtonModel;
import ModelInterface.ConfigurationEditor.guicomponents.DOMListPanelFactory;
import ModelInterface.ConfigurationEditor.guicomponents.DOMTextFieldFactory;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.BevelBorder;

import ModelInterface.ConfigurationEditor.utils.Messages;

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
    
    @Override
    public void propertyChange(final PropertyChangeEvent aEvent) {
        super.propertyChange(aEvent);

        // If the document was replaced, enable or disable all the
        // labels depending on whether the new document is null.
        if (aEvent.getPropertyName() == "document-replaced") {
            for (int i = 0; i < getComponentCount(); ++i) {
                final Component curr = getComponent(i);
                curr.setEnabled(aEvent.getNewValue() != null);
            }
        }
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
