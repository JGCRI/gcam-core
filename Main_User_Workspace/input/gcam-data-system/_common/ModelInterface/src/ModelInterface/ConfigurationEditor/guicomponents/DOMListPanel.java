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
package ModelInterface.ConfigurationEditor.guicomponents;

import ModelInterface.ConfigurationEditor.guihelpers.ButtonSetEnabler;
import ModelInterface.ConfigurationEditor.guihelpers.XMLFileFilter;
import ModelInterface.ConfigurationEditor.guihelpers.ButtonSetEnabler.ButtonType;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import org.w3c.dom.Document;

import ModelInterface.ConfigurationEditor.utils.Messages;
import ModelInterface.ConfigurationEditor.utils.FileUtils;

/**
 * A panel which contains a list based on an underlying DOM tree, and buttons to
 * add, remove, move up, or move down entries in the list.
 * 
 * @author Josh Lurz
 */
public class DOMListPanel extends JPanel {
	/**
	 * Unique Identifier required for serialization.
	 */
	private static final long serialVersionUID = 216029089953286071L;

	/**
	 * The label to title this panel with.
	 */
	private transient final String mPanelLabel;

	/**
	 * The name of elements of this list.
	 */
	private transient final String mElementName;

	/**
	 * The name of the element which contains this list.
	 */
	private transient final String mContainerName;

	/**
	 * Whether the children of this list are leaves of the DOM tree, if not they
	 * have children with child nodes. This determines whether the items in the
	 * list are files or nodes.
	 */
	private transient final boolean mLeafChildren;
	
	/**
	 * The list of files.
	 */
	private transient JList mList = null;

	/**
	 * Constructor which calls the default JPanel constructor. This will
	 * implicitly use a FlowLayoutManager and turn on double-bufferring.
	 * 
	 * @param aElementName
	 *            The name of elements in the list.
	 * @param aContainerName
	 *            The name of the container element for the list.
	 * @param aPanelLabel
	 *            The label to put at the top of the panel.
	 * @param aLeafChildren
	 *            Whether the elements of this list are leaves. Leaf lists will
	 *            add new files as new items, node lists will add elements as
	 *            new items.
	 */
	public DOMListPanel(String aElementName, String aContainerName,
			String aPanelLabel, boolean aLeafChildren) {
		super(new GridBagLayout());
		assert (aElementName != null);
		mElementName = aElementName;
		mContainerName = aContainerName;
		mPanelLabel = aPanelLabel;
		mLeafChildren = aLeafChildren;
		editorInitialize();
	}

	/**
	 * Set the underlying document.
	 * 
	 * @param aDocument
	 *            The new document.
	 */
	public void setDocument(final Document aDocument) {
		// TODO: This is ugly.
		((DOMListModel) getList(null).getModel()).setDocument(aDocument);
	}

    /**
     * Set whether the list panel is enabled.
     * @param aEnabled Whether the panel is enabled.
     */
    @Override
    public void setEnabled(final boolean aEnabled){
        super.setEnabled(aEnabled);
        
        // Disable all the child components.
        for (int i = 0; i < getComponentCount(); ++i) {
            final Component curr = getComponent(i);
            curr.setEnabled(aEnabled);
        }
    }
    
	/**
	 * Initialize the panel before viewing.
	 * 
	 */
	private final void editorInitialize() {
		setBorder(BorderFactory.createEtchedBorder());
		// Setup the frame.

		// Create a grid bag constraint.
		final GridBagConstraints cons = new GridBagConstraints();

		// Create a border around the entire panel.
		cons.insets = new Insets(5, 5, 5, 5);

		// Use internal spacing between components.
		cons.ipadx = 5;
		cons.ipady = 5;

		// Start at (0,0).
		cons.gridx = 0;
		cons.gridy = 0;
		cons.gridwidth = 2;
		// Don't fill empty space.
		cons.fill = GridBagConstraints.NONE;

		// Put the label in the center.
		cons.anchor = GridBagConstraints.CENTER;

		// Add a label at the top.
		final JLabel panelLabel = new JLabel(mPanelLabel);
		add(panelLabel, cons);

		// Put the scroll pane at (0,1)
		cons.gridx = 0;
		cons.gridy = 1;

		// Make the scroll pane absorb 4 cells.
		cons.gridheight = 4;

		// The scroll pane should absorb extra space.
		cons.weightx = 1;
		cons.weighty = 1;
		cons.fill = GridBagConstraints.BOTH;

		// Center the scroll pane.
		cons.anchor = GridBagConstraints.CENTER;

		// Create the list.
		final ButtonSetEnabler buttonEnabler = new ButtonSetEnabler();
		final JList list = getList(buttonEnabler);

		// Add the list scroll pane.
		add(createListScrollPane(list), cons);

		// Don't allow buttons to absorb space.
		cons.fill = GridBagConstraints.NONE;
		cons.weightx = 0;
		cons.weighty = 0;

		// Only take up one cell.
		cons.gridheight = 1;
		cons.gridwidth = 1;

		// Add the up button.
		// Put the up button at (3,3)
		cons.gridx = 3;
		cons.gridy = 3;

		// Put the down button in the lower left side of
		// the cell.
		cons.anchor = GridBagConstraints.CENTER;
		add(createUpButton(buttonEnabler), cons);

		// Add the down button.
		// Put the down button at (4,4) in the upper left side
		// of the cell.
		cons.anchor = GridBagConstraints.CENTER;
		cons.gridy = 4;
		add(createDownButton(buttonEnabler), cons);

		// Add the add button.
		// Put the add button at (0,4)
		// Center the add and delete button.
		cons.anchor = GridBagConstraints.CENTER;
		cons.gridx = 0;
		cons.gridy = 5;

		add(createAddButton(buttonEnabler), cons);

		// Add the delete button.
		// Put the delete button at (1,4)
		cons.gridx = 1;
		add(createDeleteButton(buttonEnabler), cons);
	}

	/**
	 * Create and initialize the list.
	 * 
	 * @return The list.
	 * @param aEnabler
	 *            A button enabler.
	 */
	public final JList getList(final ButtonSetEnabler aEnabler) {
		if (mList == null) {
			mList = new JList();
			mList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			mList.setToolTipText(Messages.getString("DOMListPanel.0")); //$NON-NLS-1$
			mList.setModel(createListModel(mList, aEnabler));
			mList.addListSelectionListener(aEnabler);
		}
		return mList;
	}

	/**
	 * Create and initialize the list model.
	 * 
	 * @param aList
	 *            The list containing this model.
	 * @param aEnabler
	 *            The button set enabler.
	 * @return The list model.
	 */
	private final ListModel createListModel(final JList aList,
			final ButtonSetEnabler aEnabler) {
		final ListModel listModel = new DOMListModel(aList, mContainerName,
				mElementName, mLeafChildren);
		listModel.addListDataListener(aEnabler);
		return listModel;
	}

	/**
	 * Initialize and get the list scroll pane.
	 * 
	 * @param aList
	 *            The list to contain in the scroll pane.
	 * @return The list scroll pane.
	 */
	private final JScrollPane createListScrollPane(final JList aList) {
		final JScrollPane scrollPane = new JScrollPane();
		scrollPane.setPreferredSize(new Dimension(100, 200));
		scrollPane.setViewportView(aList);
		return scrollPane;
	}

	/**
	 * This method creates and initializes the add button.
	 * 
	 * @param aEnabler
	 *            The controller of the buttons enabling.
	 * @return The add button.
	 */
	private final JButton createAddButton(final ButtonSetEnabler aEnabler) {
		final JButton addButton = new JButton(Messages
				.getString("DOMListPanel.1")); //$NON-NLS-1$
		addButton.setToolTipText(Messages.getString("DOMListPanel.2"));

		// Setting a preferred size so add and delete are the same size.
		addButton.setPreferredSize(new Dimension(80, 25));

		// Add the button to the button enabler so it can be controlled.
		aEnabler.addControlledButton(addButton, ButtonType.ADD);

		// Decide how new items will be added.
		if (mLeafChildren) {
			// Add an action listener which will prompt the user to select a
			// file and to add it.
			addButton.addActionListener(new AddFileButtonActionListener());
		} else {
			// Add an action listener which will prompt the user to type a
			// new item and add it.
			addButton.addActionListener(new AddItemButtonActionListener());
		}

		return addButton;
	}

	/**
	 * This method initializes the delete button.
	 * 
	 * @param aEnabler
	 *            The controller of the buttons enabling.
	 * @return The delete button.
	 */
	private final JButton createDeleteButton(final ButtonSetEnabler aEnabler) {
		final JButton deleteButton = new JButton(Messages
				.getString("DOMListPanel.4")); //$NON-NLS-1$
		deleteButton.setToolTipText(Messages.getString("DOMListPanel.3"));

		// Setting a preferred size so add and delete are the same size.
		deleteButton.setPreferredSize(new Dimension(80, 25));

		// Add the button to the button enabler so it can be controlled.
		aEnabler.addControlledButton(deleteButton, ButtonType.DELETE);

		// Add an action listener which will perform the deletion
		// on the list.
		deleteButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent aEvent) {
				final DOMListModel model = (DOMListModel) mList.getModel();
				model.removeElement(mList.getSelectedValue());
				mList.setSelectedIndex(model.getSize() - 1);
			}
		});
		return deleteButton;
	}

	/**
	 * This method creates and initializes the up button.
	 * 
	 * @param aEnabler
	 *            The controller of the buttons enabling.
	 * @return The up button.
	 */
	private final JButton createUpButton(final ButtonSetEnabler aEnabler) {
		final JButton upButton = new JButton(Messages
				.getString("DOMListPanel.5")); //$NON-NLS-1$
		upButton.setToolTipText(Messages.getString("DOMListPanel.6")); //$NON-NLS-1$

		// Set the preferred size so the up and down buttons are the same size.
		upButton.setPreferredSize(new Dimension(70, 25));

		// Add the button to the button enabler so it can be controlled.
		aEnabler.addControlledButton(upButton, ButtonType.UP);

		// Add an action listener which will move the item up in the list.
		upButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent aEvent) {
				final DOMListModel model = (DOMListModel) mList.getModel();
				final int newPos = model.moveElementBack(mList
						.getSelectedValue());
				mList.setSelectedIndex(newPos);
			}
		});
		return upButton;
	}

	/**
	 * This method creates and initializes the down button.
	 * 
	 * @param aEnabler
	 *            The controller of the buttons enabling.
	 * @return The down button.
	 */
	private final JButton createDownButton(final ButtonSetEnabler aEnabler) {
		final JButton downButton = new JButton(Messages
				.getString("DOMListPanel.7")); //$NON-NLS-1$
		downButton.setToolTipText(Messages.getString("DOMListPanel.8")); //$NON-NLS-1$

		// Set the preferred size here so the up and down button have the same
		// size.
		downButton.setPreferredSize(new Dimension(70, 25));
		
		// Add the button to the controller.
		aEnabler.addControlledButton(downButton, ButtonType.DOWN);
		
		downButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent aEvent) {
				final DOMListModel model = (DOMListModel) mList.getModel();
				final int newPos = model.moveElementForward(mList
						.getSelectedValue());
				mList.setSelectedIndex(newPos);
			}
		});
		return downButton;
	}

	/**
	 * An internal class which implements an action listener which adds items to
	 * the list.
	 * 
	 * @author Josh Lurz
	 * 
	 */
	private final class AddItemButtonActionListener implements ActionListener {
		/**
		 * Method called when an action is performed.
		 * 
		 * @param aEvent
		 *            The event causing this action.
		 */
		public void actionPerformed(final ActionEvent aEvent) {
			final String message = Messages.getString("DOMListPanel.9"); //$NON-NLS-1$
			final String newItem = JOptionPane.showInputDialog(
					getTopLevelAncestor(), message, Messages
							.getString("DOMListPanel.10"), //$NON-NLS-1$
					JOptionPane.PLAIN_MESSAGE);
			// Check if the value cannot be added because it is
			// not unique and report an error to the user.
			// TODO: Consolidate duplicate code between this and the other
			// action listener
			final DOMListModel model = (DOMListModel) mList.getModel();
			if (model.contains(newItem)) {
				final String errorTitle = Messages.getString("DOMListPanel.11"); //$NON-NLS-1$
				final String errorMessage = Messages
						.getString("DOMListPanel.12"); //$NON-NLS-1$
				// TODO: Find a better container for the error message.
				JOptionPane.showMessageDialog(getTopLevelAncestor(),
						errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE);
			} else {
				model.addElement(newItem);
			}

			mList.setSelectedIndex(model.getSize() - 1);
		}
	}

	/**
	 * An internal class which implements an action listener which adds files to
	 * the list.
	 * 
	 * @author Josh Lurz
	 * 
	 */
	private final class AddFileButtonActionListener implements ActionListener {
		/**
		 * Method called when an action is performed.
		 * 
		 * @param aEvent
		 *            The event causing this action.
		 */
		public void actionPerformed(final ActionEvent aEvent) {
			// Create a file chooser and add an XML filter.
			// This uses a chooser explicitly instead of the helper function
			// so that it can allow the user to select multiple files.
			final JFileChooser chooser = new JFileChooser();
			chooser.setFileFilter(new XMLFileFilter());
			chooser.setMultiSelectionEnabled(true);
			
			// Show the file chooser.
			final int returnValue = chooser
					.showOpenDialog(getTopLevelAncestor());
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				// Get the list of selected files.
				final File[] newFiles = FileUtils.getSelectedFiles(chooser);
				final DOMListModel model = (DOMListModel) mList.getModel();
				for (int i = 0; i < newFiles.length; i++) {
					try {
						final String newFile = newFiles[i].getCanonicalPath();
						// Check if the value cannot be added because it is
						// not unique and report an error to the user.

						if (model.contains(newFile)) {
							final String errorTitle = Messages
									.getString("DOMListPanel.13"); //$NON-NLS-1$
							final String errorMessage = Messages
									.getString("DOMListPanel.14"); //$NON-NLS-1$
							JOptionPane.showMessageDialog(
									getTopLevelAncestor(), errorMessage,
									errorTitle, JOptionPane.ERROR_MESSAGE);
						} else {
							model.addElement(newFile);
						}
					} catch (IOException aException) {
						// TODO: Error dialog.
						Logger.global.throwing("actionPerformed",
								"AddFileButtonActionListener", aException);
					}
				}
				// What thread are we on?
				mList.setSelectedIndex(model.getSize() - 1);
			}
		}
	}
}
