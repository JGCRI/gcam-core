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
package ModelInterface.ConfigurationEditor.guihelpers;

import ModelInterface.ConfigurationEditor.guicomponents.DOMListModel;

import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.SwingUtilities;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

/**
 * This class selectively enables and disables a set of buttons related to a
 * list based on whether the action they represent is currently possible to
 * execute.
 * 
 * @author Josh Lurz
 * 
 */
public class ButtonSetEnabler implements ListSelectionListener,
		ListDataListener {
	/**
	 * An enum specifying value button types to add.
	 */
	public enum ButtonType {
		/**
		 * Add button.
		 */
		ADD,
		/**
		 * Delete button.
		 */
		DELETE,
		/**
		 * Up button.
		 */
		UP,
		/**
		 * Down button.
		 */
		DOWN
	}

	/**
	 * The number of buttons contained.
	 */
	private static final int NUM_BUTTONS = 4;

	/**
	 * An array of buttons to update in the order specified by the button enum.
	 */
	private transient final JButton[] mButtons;

	/**
	 * Constructor.
	 */
	public ButtonSetEnabler() {
		super();
		mButtons = new JButton[NUM_BUTTONS];
	}

	/**
	 * Add a button which will be controlled. If there is already a button with
	 * that type it will be overridden.
	 * 
	 * @param aButton
	 *            Button to add.
	 * @param aType
	 *            Type of button to add.
	 */
	public void addControlledButton(final JButton aButton,
			final ButtonType aType) {
		mButtons[aType.ordinal()] = aButton;
	}

	/**
	 * Value changed listener method which calls updateButtonStates.
	 * 
	 * @param aEvent
	 *            The event received.
	 * @see javax.swing.event.ListSelectionListener#valueChanged(javax.swing.event.ListSelectionEvent)
	 */
	public void valueChanged(final ListSelectionEvent aEvent) {
		final JList source = (JList) aEvent.getSource();
		updateButtonStates(source);
	}

	/**
	 * Method called when an interval is added to the list.
	 * 
	 * @param aEvent
	 *            The event received.
	 */
	public void intervalAdded(final ListDataEvent aEvent) {
		final JList source = ((DOMListModel) aEvent.getSource())
				.getParentList();
		updateButtonStates(source);
	}

	/**
	 * Method called when an interval is removed from the list.
	 * 
	 * @param aEvent
	 *            The event received.
	 */
	public void intervalRemoved(final ListDataEvent aEvent) {
		final JList source = ((DOMListModel) aEvent.getSource())
				.getParentList();
		updateButtonStates(source);
	}

	/**
	 * Method called when the contents of a list change.
	 * 
	 * @param aEvent
	 *            The event received.
	 */
	public void contentsChanged(final ListDataEvent aEvent) {
		final JList source = ((DOMListModel) aEvent.getSource())
				.getParentList();
		updateButtonStates(source);
	}

	/**
	 * Conditionally enable and disable the four buttons based on the state of
	 * the list.
	 * 
	 * Individually enables the four buttons based on whether the action linked
	 * to the button is currently possible.
	 * <ul>
	 * <li>The add button is enabled when there is a parent in the DOM tree to
	 * add to.</li>
	 * <li>The delete button is enabled when the list has a positive size and a
	 * list element is selected.</li>
	 * <li>The up button is enabled if the current index is greater than zero.</li>
	 * <li>The down button is enabled if the index is set and not the end of
	 * the list.</li>
	 * </ul>
	 * 
	 * @param aList
	 *            The list causing the update.
	 */
	private void updateButtonStates(final JList aList) {
			// Check if this is necessary.
			SwingUtilities.invokeLater(new Runnable() {
				synchronized public void run() {
					// Enable the add button if there is a root node.
					mButtons[ButtonType.ADD.ordinal()]
							.setEnabled(((DOMListModel) aList.getModel())
									.canAddElements());

					final int listSize = aList.getModel().getSize();
					final int index = aList.getSelectedIndex();

					// Enable the delete button if the size is greater than zero
					// and
					// an
					// element is selected.
					mButtons[ButtonType.DELETE.ordinal()]
							.setEnabled(index != -1 && listSize > 0);

					// Enable the up button if the selected element is greater
					// than
					// 0.
					mButtons[ButtonType.UP.ordinal()].setEnabled(index > 0);

					// Enable the down button if the selected element is not the
					// last.
					mButtons[ButtonType.DOWN.ordinal()].setEnabled(index != -1
							&& index < listSize - 1);
				}
			});
		}
	}
