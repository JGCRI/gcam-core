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
package ModelInterface.ConfigurationEditor.guihelpers;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import ModelInterface.ConfigurationEditor.utils.DOMUtils;

/**
 * Displays a dialog to the user which allow them to select a new node name and
 * value.
 * 
 * @author Josh Lurz
 * 
 */
public class NewNodeDialog extends JDialog {
	/**
	 * 
	 */
	private static final long serialVersionUID = -8042747643539685447L;

	/**
	 * Document mDocument The document used to create new nodes.
	 */
	private final transient Document mDocument;

	/**
	 * The selected node, null if the user pressed cancel or did not select one.
	 */
	private transient Node mSelectedNode = null;

	/**
	 * The element name of value nodes.
	 */
	private static final String VALUE_NAME = "Value";

	/**
	 * Constructor
	 * 
	 * @param aParent
	 *            Parent frame to center the dialog on.
	 * @param aDocument
	 *            DOM document used to create new nodes.
	 */
	public NewNodeDialog(Frame aParent, Document aDocument) {
		super(aParent, "Create a New Item", true);
		mDocument = aDocument;
		initialize();
		pack();
	}

	/**
	 * Get the top level UI frame.
	 * @return The top level UI frame.
	 */
	private JDialog getTopLevelUI(){
		return this;
	}
	
	/**
	 * Initialize the dialog.
	 */
	private void initialize() {
		// Use a box layout to put everything on one line.
		setLayout(new BoxLayout(this.getContentPane(), BoxLayout.X_AXIS));
		// Add a label for the name.
		add(Box.createRigidArea(new Dimension(5, 30)));
		add(new JLabel("Name"));
		add(Box.createRigidArea(new Dimension(5, 30)));
		
		// Add a text field for the name.
		final JTextField nameField = new JTextField();
		nameField.setPreferredSize(new Dimension(200, 10));
		add(nameField);
		add(Box.createRigidArea(new Dimension(10, 30)));

		// Add a label for the value.
		add(new JLabel("Value"));
		add(Box.createRigidArea(new Dimension(5, 30)));

		// Add a text field for the value.
		final JTextField valueField = new JTextField();
		valueField.setPreferredSize(new Dimension(200, 10));
		add(valueField);
		add(Box.createRigidArea(new Dimension(10, 30)));

		// Add OK and cancel buttons.
		{
            final JButton okButton = new JButton("OK");
			okButton.setMnemonic(KeyEvent.VK_O);
			// Accelerator?
			// Add an action listener which will create a new node with
			// the specified name and value.
			okButton.addActionListener(new ActionListener() {
				/**
				 * Method called when the OK button is clicked which will create
				 * the new node.
				 * 
				 * @param aEvent
				 *            The action event.
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent aEvent) {
					// Check if the name and value are not null.
                    final String name = nameField.getText();
                    final String value = valueField.getText();
                    if(name == null || value == null) {
                        final String message = "Cannot create node with null name or value.";
                        final String title = "Invalid entries";
                        JOptionPane.showMessageDialog(getTopLevelUI(), message, title, JOptionPane.ERROR_MESSAGE);
                    }
                    else {
                        final Node newNode = mDocument
								.createElement(VALUE_NAME);
						// Set the name attribute.
						DOMUtils.setNameAttrValue(newNode, name);
						// Set the text content as the value.
						newNode.setTextContent(value);
						// Save the new node as the selected item.
						mSelectedNode = newNode;
						// Close the window.
						setVisible(false);
					}
				}
			});
			// Add the ok button to the dialog.
			add(okButton);
			add(Box.createRigidArea(new Dimension(5, 30)));
		}
		{
			// Add a cancel button.
            final JButton cancelButton = new JButton("Cancel");
			cancelButton.setMnemonic(KeyEvent.VK_C);
			// Accelerator?
			// Add an action listener which will close the dialog.
			cancelButton.addActionListener(new ActionListener() {
				/**
				 * Method called when the cancel button is clicked which will
				 * close the dialog.
				 * 
				 * @param aEvent
				 *            The action event.
				 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
				 */
				public void actionPerformed(ActionEvent aEvent) {
					// Close the window.
					setVisible(false);
				}
			});
			// Add the cancel button to the dialog.
			add(cancelButton);
			add(Box.createRigidArea(new Dimension(5, 30)));
		}
	}

	/**
	 * Get the selected item.
	 * 
	 * @return The currently selected node, null if there is not one.
	 */
	public Object getSelectedValue() {
		return mSelectedNode;
	}

}
